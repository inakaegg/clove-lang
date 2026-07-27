use crate::ast::{Form, FormKind, InterpolatedPart, MapItem, RegexDelim};

pub fn form_to_source(form: &Form) -> String {
    form_to_source_inner(form, false)
}

/// `in_quote` は「このフォームが quote の内側にあるか」。内側では `__apply` は
/// コンパイラが入れたものではなくユーザーが書いたデータなので、落としてはいけない。
fn form_to_source_inner(form: &Form, in_quote: bool) -> String {
    let form_to_source = |f: &Form| form_to_source_inner(f, in_quote);
    match &form.kind {
        FormKind::Int(n) => n.to_string(),
        FormKind::Float(n) => format_float(*n),
        FormKind::String(s) => format!("\"{}\"", escape_string_fragment(s)),
        FormKind::InterpolatedString(parts) => format_interpolated_string(parts, in_quote),
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
            let parts: Vec<String> = items.iter().map(form_to_source).collect();
            format!("#({})", parts.join(" "))
        }
        FormKind::Regex { pattern, delim } => match delim {
            RegexDelim::Slash => format!("/{}/", escape_regex_fragment(pattern)),
            RegexDelim::Hash => format!("#\"{}\"", escape_regex_fragment(pattern)),
            RegexDelim::HashSlash => format!("#/{}/", escape_regex_fragment(pattern)),
        },
        FormKind::InterpolatedRegex { parts, delim } => {
            format_interpolated_regex(parts, delim, in_quote)
        }
        FormKind::List(items) => {
            // コンパイラは通常の呼び出しを `(__apply f args..)` へ下ろす。ソースとして
            // 見せるときは元の `(f args..)` に戻す。内部形式を利用者へ出さない。
            // quote の内側は下ろされていないので触らない。
            let items = if in_quote {
                items.as_slice()
            } else {
                strip_internal_apply(items)
            };
            // quote の中身はデータ。ここから先は書き換えない。
            let inner_quote = in_quote
                || matches!(items.first().map(|f| &f.kind), Some(FormKind::Symbol(s)) if s == "quote");
            let parts: Vec<String> = items
                .iter()
                .map(|item| form_to_source_inner(item, inner_quote))
                .collect();
            format!("({})", parts.join(" "))
        }
        FormKind::Vector(items) => {
            let parts: Vec<String> = items.iter().map(form_to_source).collect();
            format!("[{}]", parts.join(" "))
        }
        FormKind::Map(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        format!("{} {}", form_to_source(k), form_to_source(v))
                    }
                    MapItem::Spread(expr) => format!("*{}", form_to_source(expr)),
                })
                .collect();
            format!("{{{}}}", parts.join(" "))
        }
        FormKind::Set(items) => {
            let parts: Vec<String> = items.iter().map(form_to_source).collect();
            format!("#{{{}}}", parts.join(" "))
        }
        FormKind::ForeignBlock { tag, code } => format!("${}{{{}}}", tag, code),
        FormKind::ForeignRaw { tag, code } => match tag.as_deref() {
            Some(t) if !t.is_empty() => format!("${}:{}", t, code),
            _ => format!("${}", code),
        },
        FormKind::ForeignSymbol { tag, path } => match tag.as_deref() {
            Some(t) if !t.is_empty() => format!("${}:{}", t, path),
            _ => format!("${}", path),
        },
    }
}

/// `(__apply f args..)` から `__apply` を落とす。それ以外はそのまま。
fn strip_internal_apply(items: &[Form]) -> &[Form] {
    match items.split_first() {
        Some((head, rest))
            if !rest.is_empty()
                && matches!(&head.kind, FormKind::Symbol(name) if name == crate::compiler::APPLY_SYM) =>
        {
            rest
        }
        _ => items,
    }
}

fn format_interpolated_string(parts: &[InterpolatedPart], in_quote: bool) -> String {
    let mut buf = String::new();
    buf.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => buf.push_str(&escape_string_fragment(text)),
            InterpolatedPart::Expr(expr) => {
                buf.push_str("#{");
                buf.push_str(&form_to_source_inner(expr, in_quote));
                buf.push('}');
            }
        }
    }
    buf.push('"');
    buf
}

fn format_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &RegexDelim,
    in_quote: bool,
) -> String {
    let (prefix, suffix) = match delim {
        RegexDelim::Slash => ("/", "/"),
        RegexDelim::Hash => ("#\"", "\""),
        RegexDelim::HashSlash => ("#/", "/"),
    };
    let mut buf = String::new();
    buf.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => buf.push_str(&escape_regex_fragment(text)),
            InterpolatedPart::Expr(expr) => {
                buf.push_str("#{");
                buf.push_str(&form_to_source_inner(expr, in_quote));
                buf.push('}');
            }
        }
    }
    buf.push_str(suffix);
    buf
}

fn escape_string_fragment(text: &str) -> String {
    let mut escaped = format!("{:?}", text);
    if escaped.starts_with('"') && escaped.ends_with('"') && escaped.len() >= 2 {
        escaped = escaped[1..escaped.len() - 1].to_string();
    }
    escaped.replace("#{", "\\#{")
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

fn format_float(n: f64) -> String {
    if n.fract() == 0.0 {
        format!("{:.1}", n)
    } else {
        n.to_string()
    }
}
