use crate::ast::{Form, FormKind, InterpolatedPart, MapItem};
use crate::string_escape::escape_string_fragment;
use crate::types::TypeHintStyle;

/// Convert a form into a source string for a given foreign tag (best-effort).
pub fn form_to_string(form: &Form, tag: &str) -> String {
    let mut out = match &form.kind {
        FormKind::Int(n) => n.to_string(),
        FormKind::Float(n) => n.to_string(),
        FormKind::String(s) => format!("\"{}\"", escape_string_fragment(s)),
        FormKind::InterpolatedString(parts) => format_interpolated_string(parts, tag),
        FormKind::Bool(b) => {
            if *b {
                "true".into()
            } else {
                "false".into()
            }
        }
        FormKind::Nil => "nil".into(),
        FormKind::Duration(d) => d.to_string(),
        FormKind::Keyword(k) => format!(":{}", k),
        FormKind::Symbol(s) => s.clone(),
        FormKind::ShortFn(items) => {
            let parts: Vec<String> = items.iter().map(|f| form_to_string(f, tag)).collect();
            format!("#({})", parts.join(" "))
        }
        FormKind::Regex { pattern, delim } => match delim {
            crate::ast::RegexDelim::Slash => format!("/{}/", escape_regex_fragment(pattern)),
            crate::ast::RegexDelim::Hash => format!("#\"{}\"", escape_regex_fragment(pattern)),
            crate::ast::RegexDelim::HashSlash => format!("#/{}/", escape_regex_fragment(pattern)),
        },
        FormKind::InterpolatedRegex { parts, delim } => {
            format_interpolated_regex(parts, delim, tag)
        }
        FormKind::List(items) => match items.split_first() {
            None => String::new(),
            Some((head, rest)) if rest.is_empty() => form_to_string(head, tag),
            Some((left, rest)) if rest.len() == 2 => {
                if let FormKind::Symbol(op) = &rest[0].kind {
                    if is_infix_op(op) {
                        let lhs = form_to_string(left, tag);
                        let rhs = form_to_string(&rest[1], tag);
                        format!("({}) {} ({})", lhs, op, rhs)
                    } else {
                        let head_str = form_to_string(left, tag);
                        let args: Vec<String> =
                            rest.iter().map(|f| form_to_string(f, tag)).collect();
                        format!("{}({})", head_str, args.join(", "))
                    }
                } else {
                    let head_str = form_to_string(left, tag);
                    let args: Vec<String> = rest.iter().map(|f| form_to_string(f, tag)).collect();
                    format!("{}({})", head_str, args.join(", "))
                }
            }
            Some((head, rest))
                if rest
                    .iter()
                    .all(|f| matches!(&f.kind, FormKind::Symbol(s) if s.starts_with('.'))) =>
            {
                let head_str = form_to_string(head, tag);
                let chain: String = rest
                    .iter()
                    .map(|f| match &f.kind {
                        FormKind::Symbol(s) => s.clone(),
                        _ => unreachable!(),
                    })
                    .collect();
                format!("({}){}", head_str, chain)
            }
            Some((head, rest)) => {
                let head_str = form_to_string(head, tag);
                let args: Vec<String> = rest.iter().map(|f| form_to_string(f, tag)).collect();
                format!("{}({})", head_str, args.join(", "))
            }
        },
        FormKind::Vector(items) => {
            let parts: Vec<String> = items.iter().map(|f| form_to_string(f, tag)).collect();
            format!("[{}]", parts.join(", "))
        }
        FormKind::Map(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        format!("{} => {}", form_to_string(k, tag), form_to_string(v, tag))
                    }
                    MapItem::Spread(expr) => format!("*{}", form_to_string(expr, tag)),
                })
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        FormKind::Set(items) => {
            let parts: Vec<String> = items.iter().map(|f| form_to_string(f, tag)).collect();
            format!("set([{}])", parts.join(", "))
        }
        FormKind::ForeignBlock { code, .. } => code.clone(),
        FormKind::ForeignRaw { code, .. } => code.clone(),
        FormKind::ForeignSymbol { path, .. } => path.clone(),
    };
    if tag.is_empty() {
        if let Some(hint) = form.type_hint.as_ref() {
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
    }
    out
}

fn format_interpolated_string(parts: &[InterpolatedPart], tag: &str) -> String {
    let mut buf = String::new();
    buf.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => buf.push_str(&escape_string_fragment(text)),
            InterpolatedPart::Expr(expr) => {
                buf.push_str("#{");
                buf.push_str(&form_to_string(expr, tag));
                buf.push('}');
            }
        }
    }
    buf.push('"');
    buf
}

fn format_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &crate::ast::RegexDelim,
    tag: &str,
) -> String {
    let (prefix, suffix) = match delim {
        crate::ast::RegexDelim::Slash => ("/", "/"),
        crate::ast::RegexDelim::Hash => ("#\"", "\""),
        crate::ast::RegexDelim::HashSlash => ("#/", "/"),
    };
    let mut buf = String::new();
    buf.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => buf.push_str(&escape_regex_fragment(text)),
            InterpolatedPart::Expr(expr) => {
                buf.push_str("#{");
                buf.push_str(&form_to_string(expr, tag));
                buf.push('}');
            }
        }
    }
    buf.push_str(suffix);
    buf
}

fn escape_regex_fragment(text: &str) -> String {
    let mut out = String::new();
    let mut chars = text.chars().peekable();
    let mut escaped = false;
    while let Some(ch) = chars.next() {
        if escaped {
            out.push(ch);
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            out.push(ch);
            continue;
        }
        if ch == '#' && matches!(chars.peek(), Some('{')) {
            out.push('\\');
            out.push('#');
            continue;
        }
        out.push(ch);
    }
    out
}

fn is_infix_op(op: &str) -> bool {
    matches!(
        op,
        "+" | "-"
            | "*"
            | "/"
            | "%"
            | ">"
            | "<"
            | ">="
            | "<="
            | "=="
            | "!="
            | "==="
            | "!=="
            | "&&"
            | "||"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_with_single_item_has_no_call_parens() {
        let span = crate::ast::Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let atom = Form::new(FormKind::Symbol("1+2".into()), span);
        let list = Form::new(FormKind::List(vec![atom]), span);
        assert_eq!(form_to_string(&list, "rb"), "1+2");
    }

    #[test]
    fn list_with_args_keeps_call_shape() {
        let span = crate::ast::Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let head = Form::new(FormKind::Symbol("f".into()), span);
        let arg = Form::new(FormKind::Int(1), span);
        let list = Form::new(FormKind::List(vec![head, arg]), span);
        assert_eq!(form_to_string(&list, "rb"), "f(1)");
    }

    #[test]
    fn list_with_dot_symbols_becomes_method_chain() {
        let span = crate::ast::Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let expr = Form::new(FormKind::Symbol("1+1".into()), span);
        let dot = Form::new(FormKind::Symbol(".to_f".into()), span);
        let list = Form::new(FormKind::List(vec![expr, dot]), span);
        assert_eq!(form_to_string(&list, "rb"), "(1+1).to_f");
    }

    #[test]
    fn list_with_infix_op_is_rendered_infix() {
        let span = crate::ast::Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let rand = Form::new(FormKind::Symbol("rand".into()), span);
        let gt = Form::new(FormKind::Symbol(">".into()), span);
        let num = Form::new(FormKind::Float(0.5), span);
        let list = Form::new(FormKind::List(vec![rand, gt, num]), span);
        assert_eq!(form_to_string(&list, "rb"), "(rand) > (0.5)");
    }
}
