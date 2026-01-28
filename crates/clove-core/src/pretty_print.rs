use crate::ast::{Form, FormKind, InterpolatedPart, MapItem};
use crate::reader::Reader;
use crate::types::TypeHintStyle;
use std::borrow::Cow;
use std::sync::Arc;

const INLINE_DEPTH_LIMIT: usize = 3;

pub type ForeignFormatter = dyn Fn(Option<&str>, &str) -> Option<String> + Send + Sync + 'static;

#[derive(Clone)]
pub struct PrettyOptions {
    pub indent_width: usize,
    pub max_inline_chars: usize,
    pub foreign_formatter: Option<Arc<ForeignFormatter>>,
}

impl Default for PrettyOptions {
    fn default() -> Self {
        Self {
            indent_width: 2,
            max_inline_chars: 120,
            foreign_formatter: None,
        }
    }
}

pub fn pretty_print_source(source: &str, options: PrettyOptions) -> Result<String, String> {
    let mut reader = Reader::new(source);
    let forms = reader.read_all().map_err(|e| e.to_string())?;
    let owned_forms;
    let formatted_forms = if options.foreign_formatter.is_some() {
        owned_forms = preformat_foreign_blocks(&forms, &options);
        &owned_forms
    } else {
        &forms
    };
    Ok(pretty_print_forms(formatted_forms, &options))
}

pub fn pretty_print_forms(forms: &[Form], options: &PrettyOptions) -> String {
    let mut out = String::new();
    for (idx, form) in forms.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        write_form(form, 0, options, &mut out);
        if !out.ends_with('\n') {
            out.push('\n');
        }
    }
    out
}

pub fn pretty_print_form(form: &Form, options: &PrettyOptions) -> String {
    let mut out = String::new();
    write_form(form, 0, options, &mut out);
    out
}

pub(crate) fn preformat_foreign_blocks(forms: &[Form], options: &PrettyOptions) -> Vec<Form> {
    if options.foreign_formatter.is_none() {
        return forms.to_vec();
    }
    forms
        .iter()
        .map(|form| preformat_foreign_form(form, options))
        .collect()
}

pub(crate) fn preformat_foreign_form(form: &Form, options: &PrettyOptions) -> Form {
    let kind = match &form.kind {
        FormKind::List(items) => FormKind::List(preformat_foreign_blocks(items, options)),
        FormKind::ShortFn(items) => FormKind::ShortFn(preformat_foreign_blocks(items, options)),
        FormKind::Vector(items) => FormKind::Vector(preformat_foreign_blocks(items, options)),
        FormKind::Set(items) => FormKind::Set(preformat_foreign_blocks(items, options)),
        FormKind::InterpolatedString(parts) => FormKind::InterpolatedString(
            parts
                .iter()
                .map(|part| match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => {
                        InterpolatedPart::Expr(preformat_foreign_form(expr, options))
                    }
                })
                .collect(),
        ),
        FormKind::InterpolatedRegex { parts, delim } => FormKind::InterpolatedRegex {
            parts: parts
                .iter()
                .map(|part| match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => {
                        InterpolatedPart::Expr(preformat_foreign_form(expr, options))
                    }
                })
                .collect(),
            delim: *delim,
        },
        FormKind::Map(entries) => FormKind::Map(
            entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => MapItem::KeyValue(
                        preformat_foreign_form(k, options),
                        preformat_foreign_form(v, options),
                    ),
                    MapItem::Spread(expr) => MapItem::Spread(preformat_foreign_form(expr, options)),
                })
                .collect(),
        ),
        FormKind::ForeignRaw { tag, code } => match &options.foreign_formatter {
            Some(fmt) => {
                let normalized = normalize_foreign_code(code, tag.as_deref(), options.indent_width);
                let new_code = fmt(tag.as_deref(), normalized.as_ref())
                    .unwrap_or_else(|| normalized.into_owned());
                FormKind::ForeignRaw {
                    tag: tag.clone(),
                    code: new_code,
                }
            }
            None => FormKind::ForeignRaw {
                tag: tag.clone(),
                code: code.clone(),
            },
        },
        FormKind::ForeignBlock { tag, code } => match &options.foreign_formatter {
            Some(fmt) => {
                let normalized =
                    normalize_foreign_code(code, Some(tag.as_str()), options.indent_width);
                let new_code = fmt(Some(tag.as_str()), normalized.as_ref())
                    .unwrap_or_else(|| normalized.into_owned());
                FormKind::ForeignBlock {
                    tag: tag.clone(),
                    code: new_code,
                }
            }
            None => FormKind::ForeignBlock {
                tag: tag.clone(),
                code: code.clone(),
            },
        },
        FormKind::ForeignSymbol { tag, path } => FormKind::ForeignSymbol {
            tag: tag.clone(),
            path: path.clone(),
        },
        _ => form.kind.clone(),
    };
    let mut out = Form::new(kind, form.span);
    out.type_hint = form.type_hint.clone();
    out
}

fn write_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    if let Some(dot_chain) = render_dot_chain(form, options, 0) {
        out.push_str(&dot_chain);
        append_type_hint(form, out);
        return;
    }
    if let Some(deref) = render_deref_expr(form, options, 0) {
        out.push_str(&deref);
        append_type_hint(form, out);
        return;
    }
    if let Some(indexed) = render_index_expr(form, options, 0) {
        out.push_str(&indexed);
        append_type_hint(form, out);
        return;
    }
    match &form.kind {
        FormKind::Symbol(s) => out.push_str(s),
        FormKind::Keyword(k) => {
            out.push(':');
            out.push_str(k);
        }
        FormKind::Int(n) => out.push_str(&n.to_string()),
        FormKind::Float(n) => out.push_str(&n.to_string()),
        FormKind::String(s) => push_string_literal(out, s),
        FormKind::InterpolatedString(parts) => {
            write_interpolated_string(parts, indent, options, out);
        }
        FormKind::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        FormKind::Nil => out.push_str("nil"),
        FormKind::Duration(d) => out.push_str(&d.to_string()),
        FormKind::ShortFn(body) => write_short_fn(body, indent, options, out),
        FormKind::Regex { pattern, delim } => match delim {
            crate::ast::RegexDelim::Slash => {
                out.push('/');
                push_regex_content(out, pattern);
                out.push('/');
            }
            crate::ast::RegexDelim::Hash => {
                out.push_str("#\"");
                push_regex_content(out, pattern);
                out.push('"');
            }
            crate::ast::RegexDelim::HashSlash => {
                out.push_str("#/");
                push_regex_content(out, pattern);
                out.push('/');
            }
        },
        FormKind::InterpolatedRegex { parts, delim } => {
            write_interpolated_regex(parts, delim, indent, options, out);
        }
        FormKind::List(_) => write_list(form, indent, options, out),
        FormKind::Vector(_) => write_vector(form, indent, options, out),
        FormKind::Set(_) => write_set(form, indent, options, out),
        FormKind::Map(_) => write_map(form, indent, options, out),
        FormKind::ForeignRaw { tag, code } => {
            write_foreign_with_prefix(tag.as_deref(), code, true, indent, options, out);
        }
        FormKind::ForeignBlock { tag, code } => {
            write_foreign_with_prefix(Some(tag.as_str()), code, false, indent, options, out);
        }
        FormKind::ForeignSymbol { tag, path } => {
            out.push('$');
            if let Some(t) = tag {
                out.push_str(t);
                out.push(':');
            }
            out.push_str(path);
        }
    }
    append_type_hint(form, out);
}

fn write_list(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() == 2 {
        if let Some(head) = list_head_symbol(items) {
            if head == "quote" {
                out.push('\'');
                write_form(&items[1], indent, options, out);
                return;
            }
        }
    }
    if items.is_empty() {
        out.push_str("()");
        return;
    }
    if let Some(head) = list_head_symbol(items) {
        if head == "defn" {
            if let Some(inline) = inline_small_defn(items, options, 0) {
                out.push_str(&inline);
                return;
            }
        }
    }
    let head_symbol = list_head_symbol(items);
    if matches!(head_symbol, Some("deref")) && items.len() == 2 {
        write_deref_form(form, indent, options, out);
        return;
    }
    if let Some(head) = head_symbol {
        if is_thread_macro_head(head) {
            write_thread_form(form, indent, options, out);
            return;
        }
        if is_let_like(head) {
            write_let_like(form, indent, options, out);
            return;
        }
        match head {
            "if" if items.len() >= 3 => {
                write_if_form(form, indent, options, out);
                return;
            }
            "when" if items.len() >= 2 => {
                write_when_form(form, indent, options, out);
                return;
            }
            "deref" if items.len() == 2 => {
                write_deref_form(form, indent, options, out);
                return;
            }
            "fn" if items.len() >= 2 => {
                write_fn_form(form, indent, options, out);
                return;
            }
            "match" if items.len() >= 4 && (items.len() - 2) % 2 == 0 => {
                write_match_form(form, indent, options, out);
                return;
            }
            "cond" if items.len() >= 3 && (items.len() - 1) % 2 == 0 => {
                write_cond_form(form, indent, options, out);
                return;
            }
            "defn" if items.len() >= 3 => {
                write_defn_form(form, indent, options, out);
                return;
            }
            "deftype" if items.len() >= 3 => {
                write_deftype_form(form, indent, options, out);
                return;
            }
            "defenum" if items.len() >= 3 => {
                write_defenum_form(form, indent, options, out);
                return;
            }
            "try" if items.len() >= 2 => {
                write_try_form(form, indent, options, out);
                return;
            }
            "finally" if items.len() >= 2 => {
                write_finally_form("finally", form, indent, options, out);
                return;
            }
            "catch" if items.len() >= 2 => {
                write_finally_form("catch", form, indent, options, out);
                return;
            }
            "fin" if items.len() >= 2 => {
                write_finally_form("fin", form, indent, options, out);
                return;
            }
            "err" if items.len() >= 2 => {
                write_finally_form("err", form, indent, options, out);
                return;
            }
            _ => {}
        }
    }
    write_head_list(form, indent, options, out);
}

fn write_head_list(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        out.push_str("()");
        return;
    }
    out.push('(');
    let child_indent = indent + options.indent_width;
    write_form(&items[0], child_indent, options, out);
    if items.len() == 1 {
        out.push(')');
        return;
    }
    if items.len() == 2 {
        if let FormKind::Map(_) = items[1].kind {
            out.push(' ');
            let map_indent = current_column(out);
            write_form(&items[1], map_indent, options, out);
            out.push(')');
            return;
        }
    }
    let force_multiline = matches!(items.first().map(|f| &f.kind), Some(FormKind::Vector(_)));
    let mut inline_mode = !force_multiline;
    for item in items.iter().skip(1) {
        if inline_mode {
            if let Some(inline) = inline_form(item, options, 0) {
                out.push(' ');
                out.push_str(&inline);
                continue;
            }
            inline_mode = false;
        }
        out.push('\n');
        write_indent(child_indent, out);
        write_form(item, child_indent, options, out);
    }
    out.push(')');
}

fn write_short_fn(body: &[Form], indent: usize, options: &PrettyOptions, out: &mut String) {
    if body.is_empty() {
        out.push_str("#()");
        return;
    }
    let complex = short_fn_has_complex_body(body);
    if complex {
        out.push('#');
        let list_form = Form {
            kind: FormKind::List(body.to_vec()),
            span: body[0].span,
            type_hint: None,
        };
        write_form(&list_form, indent, options, out);
        return;
    }
    out.push_str("#(");
    let child_indent = indent + options.indent_width;
    write_form(&body[0], child_indent, options, out);
    if body.len() == 1 {
        if out.ends_with('\n') {
            write_indent(indent, out);
        }
        out.push(')');
        return;
    }
    let mut inline_mode = !complex;
    for item in body.iter().skip(1) {
        if inline_mode {
            if let Some(inline) = inline_form(item, options, 0) {
                out.push(' ');
                out.push_str(&inline);
                continue;
            }
            inline_mode = false;
        }
        out.push('\n');
        write_indent(child_indent, out);
        write_form(item, child_indent, options, out);
    }
    out.push(')');
}

fn write_let_like(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        out.push_str("()");
        return;
    }
    out.push('(');
    let child_indent = indent + options.indent_width;
    write_form(&items[0], child_indent, options, out);
    if items.len() == 1 {
        out.push(')');
        return;
    }
    out.push(' ');
    write_binding_vector(&items[1], indent, options, out);
    for form in items.iter().skip(2) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(form, child_indent, options, out);
    }
    out.push(')');
}

fn write_if_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, out);
        return;
    }
    out.push('(');
    out.push_str("if");
    let child_indent = block_child_indent(indent, "if".len(), options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push(' ');
        out.push_str(&inline);
    } else {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, out);
    }
    for branch in items.iter().skip(2) {
        out.push('\n');
        write_indent(child_indent, out);
        if is_control_form(branch) {
            write_form(branch, child_indent, options, out);
            continue;
        }
        if let Some(inline) = inline_form(branch, options, 0) {
            out.push_str(&inline);
        } else {
            write_form(branch, child_indent, options, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_finally_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    for body in items.iter().skip(1) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(body, child_indent, options, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_when_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, out);
        return;
    }
    out.push('(');
    out.push_str("when");
    let child_indent = block_child_indent(indent, "when".len(), options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push(' ');
        out.push_str(&inline);
    } else {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, out);
    }
    for body in items.iter().skip(2) {
        out.push('\n');
        write_indent(child_indent, out);
        if let Some(inline) = inline_form(body, options, 0) {
            out.push_str(&inline);
        } else {
            write_form(body, child_indent, options, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_try_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, out);
        return;
    }
    out.push('(');
    out.push_str("try");
    let child_indent = block_child_indent(indent, "try".len(), options, out);
    for body in items.iter().skip(1) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(body, child_indent, options, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_thread_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, out);
        return;
    }
    let head = list_head_symbol(items).unwrap_or("");
    out.push('(');
    out.push_str(head);
    let child_indent = indent + options.indent_width * 2;
    out.push(' ');
    if let Some(inline) = inline_fn_args_vector_text(&items[1], options) {
        out.push_str(&inline);
    } else {
        write_fn_args_vector(&items[1], child_indent, options, out);
    }
    for item in items.iter().skip(2) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(item, child_indent, options, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_match_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 4 {
        write_head_list(form, indent, options, out);
        return;
    }
    out.push('(');
    out.push_str("match");
    let clause_indent = block_child_indent(indent, "match".len(), options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push(' ');
        out.push_str(&inline);
    } else {
        out.push('\n');
        write_indent(clause_indent, out);
        write_form(&items[1], clause_indent, options, out);
    }
    let mut clauses: Vec<(&Form, &Form)> = Vec::new();
    for idx in (2..items.len()).step_by(2) {
        if idx + 1 >= items.len() {
            break;
        }
        clauses.push((&items[idx], &items[idx + 1]));
    }
    out.push('\n');
    let pattern_strings = collect_inline_strings(
        clauses.iter().map(|(pat, _)| *pat),
        options,
        INLINE_DEPTH_LIMIT,
    );
    let value_column = pattern_strings
        .as_ref()
        .map(|strings| clause_indent + strings.iter().map(|s| s.len()).max().unwrap_or(0) + 2);
    for (idx, (pattern, expr)) in clauses.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        write_indent(clause_indent, out);
        if let (Some(strings), Some(col)) = (&pattern_strings, value_column) {
            let pat_text = &strings[idx];
            out.push_str(pat_text);
            let current_col = clause_indent + pat_text.len();
            let mut pad = if col > current_col {
                col - current_col
            } else {
                2
            };
            if pad < 2 {
                pad = 2;
            }
            for _ in 0..pad {
                out.push(' ');
            }
            write_form(expr, col, options, out);
        } else {
            write_form(pattern, clause_indent, options, out);
            if out.ends_with('\n') {
                write_indent(clause_indent + options.indent_width, out);
            } else {
                out.push(' ');
            }
            write_form(expr, clause_indent, options, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_cond_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, out);
        return;
    }
    out.push('(');
    out.push_str("cond");
    let clause_indent = block_child_indent(indent, "cond".len(), options, out);
    let mut clauses: Vec<(&Form, &Form)> = Vec::new();
    for idx in (1..items.len()).step_by(2) {
        if idx + 1 >= items.len() {
            break;
        }
        clauses.push((&items[idx], &items[idx + 1]));
    }
    out.push('\n');
    let mut max_test_len = 0usize;
    for (test, _) in clauses.iter() {
        let len = formatted_last_line_len(test, options);
        if len > max_test_len {
            max_test_len = len;
        }
    }
    let value_column = if max_test_len > 0 {
        Some(clause_indent + max_test_len + 2)
    } else {
        None
    };
    for (idx, (test, expr)) in clauses.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        write_indent(clause_indent, out);
        write_form(test, clause_indent, options, out);
        if let Some(col) = value_column {
            let mut needs_newline = out.ends_with('\n');
            let current_col = current_column(out);
            if !needs_newline && current_col > col {
                needs_newline = true;
            }
            if needs_newline {
                if !out.ends_with('\n') {
                    out.push('\n');
                }
                write_indent(col, out);
            } else {
                let mut pad = if col > current_col {
                    col - current_col
                } else {
                    2
                };
                if pad < 2 {
                    pad = 2;
                }
                for _ in 0..pad {
                    out.push(' ');
                }
            }
            write_form(expr, col, options, out);
        } else {
            if out.ends_with('\n') {
                write_indent(clause_indent + options.indent_width, out);
            } else {
                out.push(' ');
            }
            write_form(expr, clause_indent, options, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn formatted_last_line_len(form: &Form, options: &PrettyOptions) -> usize {
    let mut buf = String::new();
    write_form(form, 0, options, &mut buf);
    let trimmed = buf.trim_end_matches('\n');
    trimmed.rsplit('\n').next().unwrap_or("").len()
}

fn write_defn_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str("defn ");
    let child_indent = block_child_indent(indent, "defn ".len(), options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, out);
    }
    let mut had_doc_or_meta = false;
    while idx < items.len() && matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_)) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
        had_doc_or_meta = true;
    }
    if idx >= items.len() {
        if out.ends_with('\n') {
            write_indent(indent, out);
        }
        out.push(')');
        return;
    }
    if matches!(items[idx].kind, FormKind::List(_)) {
        while idx < items.len() {
            out.push('\n');
            write_indent(child_indent, out);
            write_form(&items[idx], child_indent, options, out);
            idx += 1;
        }
    } else {
        let mut inlined_params = false;
        if !had_doc_or_meta {
            if let Some(inline) = inline_fn_args_vector_text(&items[idx], options) {
                let current_col = current_column(out);
                if current_col + 1 + inline.len() <= options.max_inline_chars {
                    out.push(' ');
                    out.push_str(&inline);
                    idx += 1;
                    inlined_params = true;
                }
            }
        }
        if !inlined_params {
            out.push('\n');
            write_indent(child_indent, out);
            write_fn_args_vector(&items[idx], child_indent, options, out);
            idx += 1;
        }
        while idx < items.len() {
            out.push('\n');
            write_indent(child_indent, out);
            write_form(&items[idx], child_indent, options, out);
            idx += 1;
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_fn_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    out.push('(');
    out.push_str("fn");
    let child_indent = block_child_indent(indent, "fn".len(), options, out);
    let mut idx = 1;
    if idx < items.len() {
        if let Some(inline) = inline_fn_args_vector_text(&items[idx], options) {
            out.push(' ');
            out.push_str(&inline);
            idx += 1;
        } else {
            out.push('\n');
            write_indent(child_indent, out);
            write_fn_args_vector(&items[idx], child_indent, options, out);
            idx += 1;
        }
    }
    while idx < items.len() {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_deref_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return write_form(form, indent, options, out),
    };
    out.push('@');
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push_str(&inline);
    } else {
        let child_indent = indent + options.indent_width;
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, out);
    }
}

fn write_deftype_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    write_block_definition(form, indent, options, out, "deftype");
}

fn write_defenum_form(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str("defenum");
    out.push(' ');
    let child_indent = block_child_indent(indent, "defenum".len() + 1, options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, out);
    }
    if idx < items.len() && matches!(items[idx].kind, FormKind::String(_)) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
    }
    if idx < items.len() && matches!(items[idx].kind, FormKind::Map(_)) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
    }
    while idx + 1 < items.len() && matches!(items[idx].kind, FormKind::Keyword(_)) {
        let option_form = &items[idx];
        let value_form = &items[idx + 1];
        out.push('\n');
        write_indent(child_indent, out);
        write_form(option_form, child_indent, options, out);
        out.push(' ');
        let value_indent = current_column(out);
        write_form(value_form, value_indent, options, out);
        idx += 2;
    }
    while idx < items.len() {
        let member = &items[idx];
        if let FormKind::Symbol(sym) = &member.kind {
            if !sym.starts_with('*') {
                if let Some(payload) = items.get(idx + 1) {
                    if matches!(payload.kind, FormKind::Map(_)) {
                        out.push('\n');
                        write_indent(child_indent, out);
                        write_form(member, child_indent, options, out);
                        out.push(' ');
                        let payload_indent = current_column(out);
                        write_form(payload, payload_indent, options, out);
                        idx += 2;
                        continue;
                    }
                }
            }
        }
        out.push('\n');
        write_indent(child_indent, out);
        write_form(member, child_indent, options, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_block_definition(
    form: &Form,
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
    head: &str,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str(head);
    out.push(' ');
    let child_indent = block_child_indent(indent, head.len() + 1, options, out);
    if let Some(inline) = inline_form(&items[1], options, 0) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, out);
    }
    while idx < items.len() && matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_)) {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
    }
    while idx < items.len() {
        out.push('\n');
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_binding_vector(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    match &form.kind {
        FormKind::Vector(items) => {
            if items.is_empty() {
                out.push_str("[]");
                return;
            }
            if write_aligned_bindings(items, indent, options, out) {
                return;
            }
            out.push('[');
            let pair_indent = current_column(out);
            let mut idx = 0;
            let mut first = true;
            while idx + 1 < items.len() {
                if !first {
                    out.push('\n');
                    write_indent(pair_indent, out);
                }
                write_form(&items[idx], pair_indent, options, out);
                let value = &items[idx + 1];
                if let Some(inline) = inline_form(value, options, 0) {
                    if current_column(out) + 1 + inline.len() <= options.max_inline_chars {
                        out.push(' ');
                        out.push_str(&inline);
                    } else {
                        out.push('\n');
                        let value_indent = pair_indent + options.indent_width;
                        write_indent(value_indent, out);
                        write_form(value, value_indent, options, out);
                    }
                } else {
                    out.push('\n');
                    let value_indent = pair_indent + options.indent_width;
                    write_indent(value_indent, out);
                    write_form(value, value_indent, options, out);
                }
                idx += 2;
                first = false;
            }
            if idx < items.len() {
                out.push('\n');
                write_indent(pair_indent, out);
                write_form(&items[idx], pair_indent, options, out);
            }
            out.push(']');
        }
        _ => write_form(form, indent, options, out),
    }
}

fn write_aligned_bindings(
    items: &[Form],
    _indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) -> bool {
    if items.len() % 2 != 0 {
        return false;
    }
    let mut pairs: Vec<(String, &Form)> = Vec::new();
    let mut max_len = 0;
    let mut idx = 0;
    while idx < items.len() {
        if let Some(name) = inline_form(&items[idx], options, 0) {
            max_len = max_len.max(name.len());
            let value = &items[idx + 1];
            pairs.push((name, value));
        } else {
            return false;
        }
        idx += 2;
    }
    if pairs.is_empty() {
        return false;
    }
    out.push('[');
    let pair_indent = current_column(out);
    for (idx, (name, value)) in pairs.iter().enumerate() {
        if idx == 0 {
            out.push_str(name);
        } else {
            out.push('\n');
            write_indent(pair_indent, out);
            out.push_str(name);
        }
        let pad = max_len.saturating_sub(name.len()) + 2;
        for _ in 0..pad {
            out.push(' ');
        }
        let value_indent = current_column(out);
        write_form(value, value_indent, options, out);
    }
    out.push(']');
    true
}

fn write_vector(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => unreachable!(),
    };
    if let Some(inline) = inline_vector(form, options, 0) {
        out.push_str(&inline);
        return;
    }
    out.push('[');
    if items.is_empty() {
        out.push(']');
        return;
    }
    let item_indent = indent + 1;
    write_form(&items[0], item_indent, options, out);
    for item in items.iter().skip(1) {
        out.push('\n');
        write_indent(item_indent, out);
        write_form(item, item_indent, options, out);
    }
    out.push(']');
}

fn write_fn_args_vector(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    if let Some(inline) = inline_fn_args_vector_text(form, options) {
        out.push_str(&inline);
        return;
    }
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => {
            write_form(form, indent, options, out);
            return;
        }
    };
    out.push('[');
    if items.is_empty() {
        out.push(']');
        return;
    }
    let item_indent = indent + options.indent_width;
    write_form(&items[0], item_indent, options, out);
    for item in items.iter().skip(1) {
        out.push('\n');
        write_indent(item_indent, out);
        write_form(item, item_indent, options, out);
    }
    out.push(']');
    append_type_hint(form, out);
}

fn write_set(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let items = match &form.kind {
        FormKind::Set(items) => items,
        _ => unreachable!(),
    };
    if let Some(inline) = inline_set(form, options, 0) {
        out.push_str(&inline);
        return;
    }
    out.push_str("#{");
    if items.is_empty() {
        out.push('}');
        return;
    }
    let item_indent = indent + options.indent_width;
    write_form(&items[0], item_indent, options, out);
    for item in items.iter().skip(1) {
        out.push('\n');
        write_indent(item_indent, out);
        write_form(item, item_indent, options, out);
    }
    out.push('}');
}

fn render_map_shorthand_key(key: &Form, value: &Form) -> Option<String> {
    if key.span.index != value.span.index {
        return None;
    }
    if value.type_hint.is_some() {
        return None;
    }
    match (&key.kind, &value.kind) {
        (FormKind::Keyword(k), FormKind::Symbol(v))
            if k == v && !is_map_shorthand_special_key(k) =>
        {
            Some(format!(":{}", k))
        }
        _ => None,
    }
}

fn is_map_shorthand_special_key(key: &str) -> bool {
    matches!(key, "keys" | "as")
}

fn write_map(form: &Form, indent: usize, options: &PrettyOptions, out: &mut String) {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => unreachable!(),
    };
    if let Some(inline) = inline_map(form, options, 0) {
        out.push_str(&inline);
        return;
    }
    if write_aligned_map(entries, indent, options, out) {
        return;
    }
    if entries.is_empty() {
        out.push_str("{}");
        return;
    }
    out.push('{');
    let entry_indent = indent + 1;
    for (idx, entry) in entries.iter().enumerate() {
        if idx == 0 {
            // nothing
        } else {
            out.push('\n');
            write_indent(entry_indent, out);
        }
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v) {
                    out.push_str(&format!("{},", base));
                } else {
                    write_form(k, entry_indent, options, out);
                    out.push(' ');
                    write_form(v, entry_indent, options, out);
                }
            }
            MapItem::Spread(expr) => {
                out.push('*');
                out.push(' ');
                write_form(expr, entry_indent, options, out);
            }
        }
    }
    out.push('}');
}

fn write_aligned_map(
    entries: &[MapItem],
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) -> bool {
    if entries.is_empty() {
        return false;
    }
    let mut key_strings: Vec<String> = Vec::new();
    for entry in entries {
        match entry {
            MapItem::KeyValue(k, v) => {
                if render_map_shorthand_key(k, v).is_some() {
                    return false;
                }
                if let Some(text) = inline_form(k, options, 0) {
                    key_strings.push(text);
                } else {
                    return false;
                }
            }
            MapItem::Spread(_) => return false,
        }
    }
    if key_strings.is_empty() {
        return false;
    }
    let max_key_len = key_strings.iter().map(|s| s.len()).max().unwrap_or(0);
    out.push('{');
    let entry_indent = indent + 1;
    for (idx, entry) in entries.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
            write_indent(entry_indent, out);
        }
        if let MapItem::KeyValue(_, v) = entry {
            let key_text = &key_strings[idx];
            out.push_str(key_text);
            let pad = max_key_len.saturating_sub(key_text.len()) + 1;
            for _ in 0..pad {
                out.push(' ');
            }
            let value_indent = current_column(out);
            write_form(v, value_indent, options, out);
        }
    }
    out.push('}');
    true
}

fn write_foreign_with_prefix(
    tag: Option<&str>,
    code: &str,
    is_dollar: bool,
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) {
    if is_dollar {
        out.push('$');
    }
    if let Some(t) = tag {
        if !(is_dollar && t == "rb") {
            out.push_str(t);
        }
    }
    out.push('{');
    if !code.contains('\n') {
        out.push_str(code);
        out.push('}');
        return;
    }
    out.push('\n');
    let inner_indent = indent + options.indent_width;
    let normalized = normalize_foreign_code(code, tag, options.indent_width);
    for line in normalized.split('\n') {
        if line.trim().is_empty() {
            out.push('\n');
            continue;
        }
        write_indent(inner_indent, out);
        out.push_str(line);
        out.push('\n');
    }
    write_indent(indent, out);
    out.push('}');
}

fn normalize_foreign_code<'a>(
    code: &'a str,
    tag: Option<&str>,
    indent_width: usize,
) -> Cow<'a, str> {
    if !code.contains('\n') {
        return Cow::Borrowed(code);
    }
    let dedented = dedent_foreign_block(code);
    if is_ruby_tag(tag) {
        Cow::Owned(fallback_ruby_indent(&dedented, indent_width))
    } else {
        Cow::Owned(dedented)
    }
}

fn dedent_foreign_block(code: &str) -> String {
    let mut lines: Vec<String> = code
        .split('\n')
        .map(|line| line.trim_end_matches('\r').to_string())
        .collect();
    trim_edge_blank_lines(&mut lines);
    let mut base_indent: Option<usize> = None;
    for line in &lines {
        if line.trim().is_empty() {
            continue;
        }
        if base_indent.is_some() {
            break;
        }
        base_indent = Some(leading_indent_width(line));
    }
    let base_indent = base_indent.unwrap_or(0);
    if base_indent == 0 {
        return lines.join("\n");
    }
    for line in &mut lines {
        if line.trim().is_empty() {
            line.clear();
            continue;
        }
        let indent = leading_indent_width(line);
        if indent >= base_indent {
            line.drain(0..base_indent);
        }
    }
    lines.join("\n")
}

fn leading_indent_width(line: &str) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|b| matches!(**b, b' ' | b'\t'))
        .count()
}

fn trim_edge_blank_lines(lines: &mut Vec<String>) {
    while matches!(lines.first(), Some(line) if line.trim().is_empty()) {
        lines.remove(0);
    }
    while matches!(lines.last(), Some(line) if line.trim().is_empty()) {
        lines.pop();
    }
}

fn is_ruby_tag(tag: Option<&str>) -> bool {
    match tag {
        Some(t) => t == "rb" || t == "ruby",
        None => true,
    }
}

fn fallback_ruby_indent(code: &str, indent_width: usize) -> String {
    let mut output = Vec::new();
    let mut level: usize = 0;
    let unit = indent_width.max(1);
    let mut last_blank = false;
    for raw_line in code.split('\n') {
        if raw_line.trim().is_empty() {
            if !last_blank {
                output.push(String::new());
                last_blank = true;
            }
            continue;
        }
        last_blank = false;
        let trimmed_leading = raw_line.trim_start();
        let line_no_cr = trimmed_leading.trim_end_matches('\r');
        let stripped = strip_ruby_comment(line_no_cr);
        let before = ruby_indent_adjust_before(stripped);
        level = level.saturating_sub(before);
        let mut buf = String::new();
        for _ in 0..level * unit {
            buf.push(' ');
        }
        buf.push_str(line_no_cr.trim_start());
        output.push(buf);
        let after = ruby_indent_adjust_after(stripped);
        level += after;
    }
    output.join("\n")
}

fn strip_ruby_comment(line: &str) -> &str {
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for (idx, ch) in line.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        match ch {
            '\\' => escape = true,
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '#' if !in_single && !in_double => return &line[..idx],
            _ => {}
        }
    }
    line
}

fn ruby_indent_adjust_before(line: &str) -> usize {
    let mut trimmed = line.trim_start();
    if trimmed.is_empty() {
        return 0;
    }
    let mut count = 0;
    while trimmed.starts_with('}') {
        count += 1;
        trimmed = &trimmed[1..];
        trimmed = trimmed.trim_start();
    }
    if let Some(keyword) = ruby_first_keyword(trimmed) {
        if matches!(
            keyword,
            "end" | "else" | "elsif" | "when" | "rescue" | "ensure"
        ) {
            count += 1;
        }
    }
    count
}

fn ruby_indent_adjust_after(line: &str) -> usize {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return 0;
    }
    let mut count = 0;
    if let Some(keyword) = ruby_first_keyword(trimmed) {
        if matches!(
            keyword,
            "class"
                | "module"
                | "def"
                | "case"
                | "begin"
                | "if"
                | "unless"
                | "while"
                | "until"
                | "for"
                | "loop"
        ) {
            count += 1;
        }
        if matches!(keyword, "else" | "elsif" | "when" | "rescue" | "ensure") {
            count += 1;
        }
    }
    if ruby_line_has_do_block(trimmed) {
        count += 1;
    }
    let brace_delta = ruby_brace_delta(trimmed);
    if brace_delta > 0 {
        count += brace_delta as usize;
    }
    count
}

fn ruby_first_keyword(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    if trimmed.is_empty() {
        return None;
    }
    let mut end = trimmed.len();
    for (idx, ch) in trimmed.char_indices() {
        if ch.is_alphanumeric() || ch == '_' {
            continue;
        }
        end = idx;
        break;
    }
    if end == 0 {
        return None;
    }
    Some(&trimmed[..end])
}

fn ruby_line_has_do_block(line: &str) -> bool {
    let stripped = strip_ruby_comment(line).trim_end();
    if stripped.ends_with(" do") {
        return true;
    }
    if stripped.contains(" do |") {
        return true;
    }
    if let Some(idx) = stripped.find(" do") {
        let before = stripped[..idx].chars().last();
        if matches!(before, Some(ch) if ch.is_whitespace() || ch == ')' || ch == ']') {
            return true;
        }
    }
    false
}

fn ruby_brace_delta(line: &str) -> i32 {
    let mut delta = 0;
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for ch in line.chars() {
        if escape {
            escape = false;
            continue;
        }
        match ch {
            '\\' => escape = true,
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '{' if !in_single && !in_double => delta += 1,
            '}' if !in_single && !in_double => delta -= 1,
            _ => {}
        }
    }
    delta
}

fn write_indent(width: usize, out: &mut String) {
    for _ in 0..width {
        out.push(' ');
    }
}

fn push_string_content(buf: &mut String, value: &str) {
    let mut prev_was_cr = false;
    let mut chars = value.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                buf.push_str("\\\\");
                prev_was_cr = false;
            }
            '"' => {
                buf.push_str("\\\"");
                prev_was_cr = false;
            }
            '\n' => {
                buf.push_str("\\n");
                prev_was_cr = false;
            }
            '\r' => {
                buf.push_str("\\r");
                prev_was_cr = true;
            }
            '\t' => {
                buf.push_str("\\t");
                prev_was_cr = false;
            }
            '#' if matches!(chars.peek(), Some('{')) => {
                buf.push_str("\\#");
                prev_was_cr = false;
            }
            other => {
                buf.push(other);
                prev_was_cr = false;
            }
        }
    }
    if prev_was_cr {
        buf.push_str("\\n");
    }
}

fn push_regex_content(buf: &mut String, value: &str) {
    let mut chars = value.chars().peekable();
    let mut escaped = false;
    while let Some(ch) = chars.next() {
        if escaped {
            buf.push(ch);
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            buf.push(ch);
            continue;
        }
        if ch == '#' && matches!(chars.peek(), Some('{')) {
            buf.push_str("\\#");
            continue;
        }
        buf.push(ch);
    }
}

fn push_string_literal(out: &mut String, value: &str) {
    let mut buf = String::new();
    buf.push('"');
    push_string_content(&mut buf, value);
    buf.push('"');
    out.push_str(&buf);
}

fn write_interpolated_string(
    parts: &[InterpolatedPart],
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) {
    out.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_string_content(out, text),
            InterpolatedPart::Expr(expr) => {
                out.push_str("#{");
                let rendered = render_interpolation_expr(expr, indent, options);
                out.push_str(&rendered);
                out.push('}');
            }
        }
    }
    out.push('"');
}

fn write_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &crate::ast::RegexDelim,
    indent: usize,
    options: &PrettyOptions,
    out: &mut String,
) {
    let (prefix, suffix) = match delim {
        crate::ast::RegexDelim::Slash => ("/", "/"),
        crate::ast::RegexDelim::Hash => ("#\"", "\""),
        crate::ast::RegexDelim::HashSlash => ("#/", "/"),
    };
    out.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_regex_content(out, text),
            InterpolatedPart::Expr(expr) => {
                out.push_str("#{");
                let rendered = render_interpolation_expr(expr, indent, options);
                out.push_str(&rendered);
                out.push('}');
            }
        }
    }
    out.push_str(suffix);
}

fn render_interpolation_expr(expr: &Form, indent: usize, options: &PrettyOptions) -> String {
    if let Some(inline) = inline_form(expr, options, 0) {
        return inline;
    }
    let mut buf = String::new();
    write_form(expr, indent, options, &mut buf);
    buf
}

fn inline_interpolated_string(
    parts: &[InterpolatedPart],
    options: &PrettyOptions,
    depth: usize,
) -> Option<String> {
    let mut buf = String::new();
    buf.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_string_content(&mut buf, text),
            InterpolatedPart::Expr(expr) => {
                let inner = inline_form(expr, options, depth + 1)?;
                buf.push_str("#{");
                buf.push_str(&inner);
                buf.push('}');
            }
        }
    }
    buf.push('"');
    Some(buf)
}

fn inline_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &crate::ast::RegexDelim,
    options: &PrettyOptions,
    depth: usize,
) -> Option<String> {
    let (prefix, suffix) = match delim {
        crate::ast::RegexDelim::Slash => ("/", "/"),
        crate::ast::RegexDelim::Hash => ("#\"", "\""),
        crate::ast::RegexDelim::HashSlash => ("#/", "/"),
    };
    let mut buf = String::new();
    buf.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_regex_content(&mut buf, text),
            InterpolatedPart::Expr(expr) => {
                let inner = inline_form(expr, options, depth + 1)?;
                buf.push_str("#{");
                buf.push_str(&inner);
                buf.push('}');
            }
        }
    }
    buf.push_str(suffix);
    Some(buf)
}

fn current_column(out: &str) -> usize {
    out.rfind('\n')
        .map(|idx| out.len() - idx - 1)
        .unwrap_or(out.len())
}

fn append_type_hint(form: &Form, out: &mut String) {
    let Some(hint) = form.type_hint.as_ref() else {
        return;
    };
    if matches!(&form.kind, FormKind::Symbol(sym) if symbol_has_inline_type_hint(sym)) {
        return;
    }
    match hint.style {
        TypeHintStyle::Postfix => {
            out.push(':');
            out.push(' ');
        }
        TypeHintStyle::Return => {
            out.push(' ');
            out.push_str("-> ");
        }
    }
    out.push_str(&hint.kind.describe());
}

fn append_type_hint_inline(form: &Form, mut base: String) -> String {
    append_type_hint(form, &mut base);
    base
}

fn symbol_has_inline_type_hint(sym: &str) -> bool {
    let start = match sym.find('<') {
        Some(idx) if idx > 0 => idx,
        _ => return false,
    };
    let mut depth = 0usize;
    for (offset, ch) in sym[start..].char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return start + offset + 1 == sym.len();
                }
            }
            _ => {}
        }
    }
    false
}

fn block_child_indent(indent: usize, head_len: usize, options: &PrettyOptions, out: &str) -> usize {
    let head_start = current_column(out).saturating_sub(head_len + 1);
    let inline_indent = head_start + options.indent_width;
    let base_indent = indent + options.indent_width;
    inline_indent.max(base_indent)
}

fn inline_form(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    if depth > INLINE_DEPTH_LIMIT {
        return None;
    }
    if let Some(dot_chain) = render_dot_chain(form, options, depth) {
        return Some(append_type_hint_inline(form, dot_chain));
    }
    if let Some(deref) = render_deref_expr(form, options, depth) {
        return Some(append_type_hint_inline(form, deref));
    }
    if let Some(indexed) = render_index_expr(form, options, depth) {
        return Some(append_type_hint_inline(form, indexed));
    }
    let base = match &form.kind {
        FormKind::Symbol(s) => Some(s.clone()),
        FormKind::Keyword(k) => Some(format!(":{}", k)),
        FormKind::Int(n) => Some(n.to_string()),
        FormKind::Float(n) => Some(n.to_string()),
        FormKind::String(s) => {
            let mut buf = String::new();
            push_string_literal(&mut buf, s);
            Some(buf)
        }
        FormKind::InterpolatedString(parts) => inline_interpolated_string(parts, options, depth),
        FormKind::InterpolatedRegex { parts, delim } => {
            inline_interpolated_regex(parts, delim, options, depth)
        }
        FormKind::Bool(b) => Some(b.to_string()),
        FormKind::Nil => Some("nil".into()),
        FormKind::Duration(d) => Some(d.to_string()),
        FormKind::ShortFn(body) => {
            let mut parts = Vec::new();
            for f in body {
                if let Some(inline) = inline_form(f, options, depth + 1) {
                    parts.push(inline);
                } else {
                    return None;
                }
            }
            Some(format!("#({})", parts.join(" ")))
        }
        FormKind::Regex { pattern, delim } => Some(match delim {
            crate::ast::RegexDelim::Slash => format!("/{}/", pattern),
            crate::ast::RegexDelim::Hash => format!("#\"{}\"", pattern),
            crate::ast::RegexDelim::HashSlash => format!("#/{}/", pattern),
        }),
        FormKind::List(_) => inline_list(form, options, depth + 1),
        FormKind::Vector(_) => inline_vector(form, options, depth + 1),
        FormKind::Set(_) => inline_set(form, options, depth + 1),
        FormKind::Map(_) => inline_map(form, options, depth + 1),
        FormKind::ForeignRaw { tag, code } => {
            inline_foreign_with_prefix(tag.as_deref(), code, true)
        }
        FormKind::ForeignBlock { tag, code } => {
            inline_foreign_with_prefix(Some(tag.as_str()), code, false)
        }
        FormKind::ForeignSymbol { tag, path } => {
            let mut buf = String::new();
            buf.push('$');
            if let Some(t) = tag {
                buf.push_str(t);
                buf.push(':');
            }
            buf.push_str(path);
            Some(buf)
        }
    };
    base.map(|value| append_type_hint_inline(form, value))
}

fn inline_list(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        return Some("()".into());
    }
    if matches!(items.first().map(|f| &f.kind), Some(FormKind::Vector(_))) {
        return None;
    }
    if let Some(head) = list_head_symbol(items) {
        if is_thread_macro_head(head) {
            return None;
        }
        if head == "fn" {
            if let Some(inline) = inline_small_fn(items, options, depth) {
                return Some(inline);
            }
            return None;
        }
        if head == "defn" {
            if let Some(inline) = inline_small_defn(items, options, depth) {
                return Some(inline);
            }
            return None;
        }
        if head == "quote" && items.len() == 2 {
            let quoted = inline_form(&items[1], options, depth + 1)?;
            let buf = format!("'{}", quoted);
            return Some(buf);
        }
        if head == "if" {
            if let Some(inline) = inline_small_if(items, options, depth) {
                return Some(inline);
            }
            return None;
        }
        if is_block_head(head) {
            return None;
        }
    }
    if items.len() > 5
        && items.iter().skip(1).any(|item| {
            matches!(
                item.kind,
                FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
            )
        })
    {
        return None;
    }
    let mut parts = Vec::new();
    for item in items {
        if let Some(inline) = inline_form(item, options, depth + 1) {
            parts.push(inline);
        } else {
            return None;
        }
    }
    Some(format!("({})", parts.join(" ")))
}

fn inline_vector(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        return Some("[]".into());
    }
    if items.len() > 4 {
        return None;
    }
    if items.len() > 3
        && items.iter().any(|item| {
            matches!(
                item.kind,
                FormKind::List(_) | FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
            )
        })
    {
        return None;
    }
    let mut parts = Vec::new();
    for item in items {
        if let Some(inline) = inline_form(item, options, depth + 1) {
            parts.push(inline);
        } else {
            return None;
        }
    }
    Some(format!("[{}]", parts.join(" ")))
}

fn inline_set(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::Set(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        return Some("#{}".into());
    }
    if items.len() > 6 {
        return None;
    }
    let mut parts = Vec::new();
    for item in items {
        if let Some(inline) = inline_form(item, options, depth + 1) {
            parts.push(inline);
        } else {
            return None;
        }
    }
    Some(format!("#{{{}}}", parts.join(" ")))
}

fn inline_map(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => unreachable!(),
    };
    if entries.is_empty() {
        return Some("{}".into());
    }
    let has_shorthand = entries.iter().any(|entry| match entry {
        MapItem::KeyValue(k, v) => render_map_shorthand_key(k, v).is_some(),
        MapItem::Spread(_) => false,
    });
    if !has_shorthand && entries.len() > 3 {
        return None;
    }
    let mut parts = Vec::new();
    for entry in entries {
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v) {
                    parts.push(format!("{},", base));
                } else {
                    if !is_simple_map_value(v, options, depth) {
                        return None;
                    }
                    let key = inline_form(k, options, depth + 1)?;
                    let value = inline_form(v, options, depth + 1)?;
                    parts.push(format!("{} {}", key, value));
                }
            }
            MapItem::Spread(_expr) => return None,
        }
    }
    Some(format!("{{{}}}", parts.join(" ")))
}

fn inline_fn_arg_form(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    if depth > INLINE_DEPTH_LIMIT {
        return None;
    }
    match &form.kind {
        FormKind::Vector(_) => inline_vector_args(form, options, depth),
        FormKind::Map(_) => inline_map_relaxed(form, options, depth),
        _ => inline_form(form, options, depth),
    }
}

fn inline_fn_args_vector_text(form: &Form, options: &PrettyOptions) -> Option<String> {
    let inline = inline_vector_args(form, options, 0)?;
    Some(append_type_hint_inline(form, inline))
}

fn inline_vector_args(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => return None,
    };
    if items.len() > 8 {
        return None;
    }
    let mut parts = Vec::new();
    for item in items {
        parts.push(inline_fn_arg_form(item, options, depth + 1)?);
    }
    let buf = format!("[{}]", parts.join(" "));
    if buf.len() <= options.max_inline_chars {
        Some(buf)
    } else {
        None
    }
}

fn inline_map_relaxed(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => return None,
    };
    if entries.is_empty() {
        return Some("{}".into());
    }
    let has_shorthand = entries.iter().any(|entry| match entry {
        MapItem::KeyValue(k, v) => render_map_shorthand_key(k, v).is_some(),
        MapItem::Spread(_) => false,
    });
    if !has_shorthand && entries.len() > 4 {
        return None;
    }
    let mut parts = Vec::new();
    for entry in entries {
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v) {
                    parts.push(format!("{},", base));
                } else {
                    let key = inline_fn_arg_form(k, options, depth + 1)?;
                    let value = inline_fn_arg_form(v, options, depth + 1)?;
                    parts.push(format!("{} {}", key, value));
                }
            }
            MapItem::Spread(_expr) => return None,
        }
    }
    let buf = format!("{{{}}}", parts.join(" "));
    if buf.len() <= options.max_inline_chars {
        Some(buf)
    } else {
        None
    }
}

fn short_fn_has_complex_body(body: &[Form]) -> bool {
    if let Some(head) = short_fn_head_symbol(body) {
        if is_complex_head(head) {
            return true;
        }
    }
    body.iter().any(form_has_complex_control)
}

fn fn_body_is_complex(items: &[Form]) -> bool {
    if items.len() != 3 {
        return true;
    }
    if !matches!(items[1].kind, FormKind::Vector(_)) {
        return true;
    }
    form_has_complex_control(&items[2])
}

fn form_has_complex_control(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => list_has_complex_head(items),
        FormKind::ShortFn(body) => short_fn_has_complex_body(body),
        _ => false,
    }
}

fn short_fn_head_symbol(body: &[Form]) -> Option<&str> {
    body.first().and_then(|form| match &form.kind {
        FormKind::Symbol(s) => Some(s.as_str()),
        _ => None,
    })
}

fn list_has_complex_head(items: &[Form]) -> bool {
    list_head_symbol(items)
        .map(is_complex_head)
        .unwrap_or(false)
}

fn is_complex_head(head: &str) -> bool {
    matches!(
        head,
        "when"
            | "let"
            | "let*"
            | "loop"
            | "binding"
            | "do"
            | "doseq"
            | "each"
            | "for"
            | "cond"
            | "case"
            | "match"
            | "if"
            | "try"
            | "catch"
            | "finally"
            | "err"
            | "fin"
    )
}

fn is_simple_map_value(form: &Form, options: &PrettyOptions, depth: usize) -> bool {
    match &form.kind {
        FormKind::ShortFn(body) => {
            if short_fn_has_complex_body(body) {
                return false;
            }
            inline_form(form, options, depth + 1)
                .map(|s| s.len() <= options.max_inline_chars)
                .unwrap_or(false)
        }
        FormKind::List(items) => {
            if let Some(head) = list_head_symbol(items) {
                if head == "fn" {
                    if fn_body_is_complex(items) {
                        return false;
                    }
                    if let Some(text) = inline_small_fn(items, options, depth + 1) {
                        return text.len() <= options.max_inline_chars;
                    }
                }
            }
            is_inline_scalar(form)
        }
        FormKind::Vector(_) | FormKind::Set(_) => inline_form(form, options, depth + 1)
            .map(|s| s.len() <= options.max_inline_chars)
            .unwrap_or(false),
        _ => is_inline_scalar(form),
    }
}

fn inline_small_fn(items: &[Form], options: &PrettyOptions, depth: usize) -> Option<String> {
    if items.len() != 3 {
        return None;
    }
    if !matches!(items[1].kind, FormKind::Vector(_)) {
        return None;
    }
    let params = inline_fn_args_vector_text(&items[1], options)?;
    let body = inline_form(&items[2], options, depth + 1)?;
    let buf = format!("(fn {} {})", params, body);
    if buf.len() <= options.max_inline_chars {
        Some(buf)
    } else {
        None
    }
}

fn inline_small_defn(items: &[Form], options: &PrettyOptions, depth: usize) -> Option<String> {
    if items.len() != 4 {
        return None;
    }
    let name = inline_form(&items[1], options, depth + 1)?;
    let params = inline_fn_args_vector_text(&items[2], options)?;
    let body = inline_form(&items[3], options, depth + 1)?;
    let buf = format!("(defn {} {} {})", name, params, body);
    if buf.len() <= options.max_inline_chars {
        Some(buf)
    } else {
        None
    }
}

fn inline_small_if(items: &[Form], options: &PrettyOptions, depth: usize) -> Option<String> {
    if items.len() != 4 {
        return None;
    }
    let cond = inline_form(&items[1], options, depth)?;
    let then = inline_form(&items[2], options, depth)?;
    let else_expr = inline_form(&items[3], options, depth)?;
    let buf = format!("(if {} {} {})", cond, then, else_expr);
    if buf.len() <= options.max_inline_chars.saturating_mul(2) {
        Some(buf)
    } else {
        None
    }
}

fn inline_foreign_with_prefix(tag: Option<&str>, code: &str, is_dollar: bool) -> Option<String> {
    if code.contains('\n') {
        return None;
    }
    let mut buf = String::new();
    if is_dollar {
        buf.push('$');
    }
    if let Some(t) = tag {
        if !(is_dollar && t == "rb") {
            buf.push_str(t);
        }
    }
    buf.push('{');
    buf.push_str(code);
    buf.push('}');
    Some(buf)
}

fn render_dot_chain(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    render_special_chain("clove.syntax::dot", ".", form, options, depth)
}

fn render_deref_expr(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() != 2 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "deref" {
        return None;
    }
    let target = inline_form(&items[1], options, depth + 1)?;
    Some(format!("@{}", target))
}

fn render_index_expr(form: &Form, options: &PrettyOptions, depth: usize) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 3 || items.len() > 4 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    let target = inline_form(&items[1], options, depth + 1)?;
    let default = match items.get(3) {
        Some(form) => Some(inline_form(form, options, depth + 1)?),
        None => None,
    };
    let mut buf = String::new();
    buf.push_str(&target);
    buf.push('[');
    match head {
        crate::reader::INDEX_GET_SYM => {
            let path = inline_form(&items[2], options, depth + 1)?;
            buf.push_str(&path);
        }
        crate::reader::INDEX_GET_IN_SYM => match &items[2].kind {
            FormKind::Vector(path_items) => {
                for (idx, item) in path_items.iter().enumerate() {
                    if idx > 0 {
                        buf.push(' ');
                    }
                    let part = inline_form(item, options, depth + 1)?;
                    buf.push_str(&part);
                }
            }
            _ => return None,
        },
        crate::reader::INDEX_GET_MANY_SYM => match &items[2].kind {
            FormKind::Vector(path_items) => {
                let parts = collect_index_many_parts(path_items);
                for (idx, part) in parts.iter().enumerate() {
                    if idx > 0 {
                        buf.push(' ');
                    }
                    let text = inline_form(part.form, options, depth + 1)?;
                    buf.push_str(&text);
                    if part.comma_after {
                        buf.push(',');
                    }
                }
            }
            _ => return None,
        },
        _ => return None,
    }
    if let Some(default) = default {
        buf.push_str(" || ");
        buf.push_str(&default);
    }
    buf.push(']');
    Some(buf)
}

fn render_special_chain(
    sym: &str,
    marker: &str,
    form: &Form,
    options: &PrettyOptions,
    depth: usize,
) -> Option<String> {
    let list_items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if list_items.len() < 2 {
        return None;
    }
    if !matches!(&list_items[0].kind, FormKind::Symbol(s) if s == sym) {
        return None;
    }
    let mut parts = Vec::new();
    for item in list_items.iter().skip(1) {
        if let Some(inline) = inline_form(item, options, depth + 1) {
            parts.push(inline);
        } else {
            return None;
        }
    }
    Some(parts.join(marker))
}

struct IndexPart<'a> {
    form: &'a Form,
    comma_after: bool,
}

fn collect_index_many_parts<'a>(path_items: &'a [Form]) -> Vec<IndexPart<'a>> {
    let mut parts = Vec::new();
    if path_items.is_empty() {
        return parts;
    }
    let last_group = path_items.len().saturating_sub(1);
    for (group_idx, item) in path_items.iter().enumerate() {
        match &item.kind {
            FormKind::Vector(inner) | FormKind::List(inner) => {
                if inner.is_empty() {
                    continue;
                }
                let last_inner = inner.len().saturating_sub(1);
                for (idx, inner_item) in inner.iter().enumerate() {
                    let comma_after = idx == last_inner && group_idx < last_group;
                    parts.push(IndexPart {
                        form: inner_item,
                        comma_after,
                    });
                }
            }
            _ => {
                let comma_after = group_idx < last_group;
                parts.push(IndexPart {
                    form: item,
                    comma_after,
                });
            }
        }
    }
    parts
}

fn collect_inline_strings<'a>(
    forms: impl Iterator<Item = &'a Form>,
    options: &PrettyOptions,
    depth: usize,
) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    for form in forms {
        if let Some(inline) = inline_form(form, options, depth) {
            parts.push(inline);
        } else {
            return None;
        }
    }
    Some(parts)
}

fn list_head_symbol(items: &[Form]) -> Option<&str> {
    if let Some(Form {
        kind: FormKind::Symbol(sym),
        ..
    }) = items.first()
    {
        Some(sym.as_str())
    } else {
        None
    }
}

fn is_let_like(sym: &str) -> bool {
    matches!(sym, "let" | "loop" | "binding")
}

fn is_control_form(form: &Form) -> bool {
    matches!(
        &form.kind,
        FormKind::List(items)
            if matches!(
                list_head_symbol(items),
                Some("if" | "when" | "match" | "cond" | "try" | "catch" | "finally" | "err" | "fin")
            )
    )
}

fn is_block_head(head: &str) -> bool {
    matches!(
        head,
        "let"
            | "let*"
            | "loop"
            | "binding"
            | "go"
            | "go-loop"
            | "async-scope"
            | "async::scope"
            | "scope-loop"
            | "async::scope-loop"
            | "when"
            | "fn"
            | "defn"
            | "defmacro"
            | "def"
            | "macro"
            | "do"
            | "cond"
            | "case"
            | "try"
            | "catch"
            | "err"
            | "fin"
    )
}

fn is_thread_macro_head(head: &str) -> bool {
    matches!(head, "->" | "->>" | "as->" | "cond->" | "cond->>")
}

fn is_inline_scalar(form: &Form) -> bool {
    match form.kind {
        FormKind::Symbol(_)
        | FormKind::Keyword(_)
        | FormKind::Int(_)
        | FormKind::Float(_)
        | FormKind::String(_)
        | FormKind::Bool(_)
        | FormKind::Nil
        | FormKind::Duration(_)
        | FormKind::Regex { .. }
        | FormKind::ForeignSymbol { .. } => true,
        _ => false,
    }
}
