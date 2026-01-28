use crate::ast::{Form, FormKind, InterpolatedPart, MapItem};
use crate::fn_meta::{self, SubjectPos};
use crate::form_source::form_to_source;
use crate::reader::Reader;
use crate::string_escape::escape_string_fragment;
use crate::symbols::builtin_alias_target;

const EXAMPLE_DELIMITERS: [&str; 4] = [" ; => ", "; =>", " => ", "=>"];

#[derive(Clone, Debug)]
pub struct ExampleParts {
    pub expr_src: String,
    pub expected_src: String,
    pub tail: String,
}

pub fn split_example(example: &str) -> Option<ExampleParts> {
    let trimmed = example.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (idx, delimiter) = find_delimiter(trimmed)?;
    let expr_raw = &trimmed[..idx];
    let rest_raw = &trimmed[idx + delimiter.len()..];
    let expr_src = strip_inline_comment(expr_raw);
    let expected_src = strip_inline_comment(rest_raw);
    if expr_src.is_empty() || expected_src.is_empty() {
        return None;
    }
    let tail = trimmed[idx..].to_string();
    Some(ExampleParts {
        expr_src,
        expected_src,
        tail,
    })
}

pub fn try_gen_oop_example(example: &str, origin: Option<&str>) -> Option<String> {
    let parts = split_example(example)?;
    let form = parse_single_form(&parts.expr_src).ok()?;
    let config = RewriteConfig {
        origin: origin.unwrap_or(""),
    };
    let rewritten = rewrite_form(&form, config, false);
    if !rewritten.converted {
        return None;
    }
    Some(format!("{}{}", rewritten.src, parts.tail))
}

pub fn gen_oop_examples(examples: &[String], origin: Option<&str>) -> Vec<String> {
    let mut out = Vec::new();
    for example in examples {
        if let Some(converted) = try_gen_oop_example(example, origin) {
            out.push(converted);
        }
    }
    out
}

fn find_delimiter(example: &str) -> Option<(usize, &'static str)> {
    for delimiter in EXAMPLE_DELIMITERS {
        if let Some(idx) = find_delimiter_outside_string(example, delimiter) {
            return Some((idx, delimiter));
        }
    }
    None
}

fn find_delimiter_outside_string(example: &str, delimiter: &str) -> Option<usize> {
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in example.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if ch == '"' {
            in_string = !in_string;
            continue;
        }
        if !in_string && example[idx..].starts_with(delimiter) {
            return Some(idx);
        }
    }
    None
}

fn parse_single_form(src: &str) -> Result<Form, String> {
    let mut reader = Reader::new(src);
    let mut forms = reader.read_all().map_err(|err| err.to_string())?;
    if forms.len() != 1 {
        return Err(format!("expected single form, got {}", forms.len()));
    }
    Ok(forms.remove(0))
}

fn strip_inline_comment(raw: &str) -> String {
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in raw.char_indices() {
        if ch == '"' && !escape {
            in_string = !in_string;
        }
        if ch == '\\' && !escape {
            escape = true;
            continue;
        }
        if ch == ';' && !in_string {
            return raw[..idx].trim().to_string();
        }
        escape = false;
    }
    raw.trim().to_string()
}

#[derive(Clone, Copy)]
struct RewriteConfig<'a> {
    origin: &'a str,
}

#[derive(Clone)]
struct RewriteResult {
    src: String,
    converted: bool,
}

fn rewrite_form(form: &Form, config: RewriteConfig<'_>, in_quote: bool) -> RewriteResult {
    match &form.kind {
        FormKind::List(items) => rewrite_list(form, items, config, in_quote),
        FormKind::Vector(items) => {
            let mut converted = false;
            let parts: Vec<String> = items
                .iter()
                .map(|item| {
                    let rewritten = rewrite_form(item, config, in_quote);
                    converted |= rewritten.converted;
                    rewritten.src
                })
                .collect();
            RewriteResult {
                src: format!("[{}]", parts.join(" ")),
                converted,
            }
        }
        FormKind::Set(items) => {
            let mut converted = false;
            let parts: Vec<String> = items
                .iter()
                .map(|item| {
                    let rewritten = rewrite_form(item, config, in_quote);
                    converted |= rewritten.converted;
                    rewritten.src
                })
                .collect();
            RewriteResult {
                src: format!("#{{{}}}", parts.join(" ")),
                converted,
            }
        }
        FormKind::Map(entries) => {
            let mut converted = false;
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        let key = rewrite_form(k, config, in_quote);
                        let value = rewrite_form(v, config, in_quote);
                        converted |= key.converted || value.converted;
                        format!("{} {}", key.src, value.src)
                    }
                    MapItem::Spread(expr) => {
                        let rewritten = rewrite_form(expr, config, in_quote);
                        converted |= rewritten.converted;
                        format!("*{}", rewritten.src)
                    }
                })
                .collect();
            RewriteResult {
                src: format!("{{{}}}", parts.join(" ")),
                converted,
            }
        }
        FormKind::ShortFn(items) => {
            let mut converted = false;
            let parts: Vec<String> = items
                .iter()
                .map(|item| {
                    let rewritten = rewrite_form(item, config, in_quote);
                    converted |= rewritten.converted;
                    rewritten.src
                })
                .collect();
            RewriteResult {
                src: format!("#({})", parts.join(" ")),
                converted,
            }
        }
        FormKind::InterpolatedString(parts) => {
            let mut converted = false;
            let mut out = String::from("\"");
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        out.push_str(&escape_string_fragment(text));
                    }
                    InterpolatedPart::Expr(expr) => {
                        let rewritten = rewrite_form(expr, config, in_quote);
                        converted |= rewritten.converted;
                        out.push_str("#{");
                        out.push_str(&rewritten.src);
                        out.push('}');
                    }
                }
            }
            out.push('"');
            RewriteResult {
                src: out,
                converted,
            }
        }
        _ => RewriteResult {
            src: form_to_source(form),
            converted: false,
        },
    }
}

fn rewrite_list(
    form: &Form,
    items: &[Form],
    config: RewriteConfig<'_>,
    in_quote: bool,
) -> RewriteResult {
    if items.is_empty() {
        return RewriteResult {
            src: "()".to_string(),
            converted: false,
        };
    }
    let head_symbol = match &items[0].kind {
        FormKind::Symbol(sym) => Some(sym.as_str()),
        _ => None,
    };
    if let Some(sym) = head_symbol {
        if sym == "quote" || sym == "comment" {
            return RewriteResult {
                src: form_to_source(form),
                converted: false,
            };
        }
        if !in_quote && !is_special_form(sym) {
            if let Some(rewritten) = rewrite_call(sym, &items[1..], config) {
                return rewritten;
            }
        }
    }
    let next_in_quote = in_quote || matches!(head_symbol, Some("quote"));
    let mut converted = false;
    let parts: Vec<String> = items
        .iter()
        .map(|item| {
            let rewritten = rewrite_form(item, config, next_in_quote);
            converted |= rewritten.converted;
            rewritten.src
        })
        .collect();
    RewriteResult {
        src: format!("({})", parts.join(" ")),
        converted,
    }
}

fn rewrite_call(sym: &str, args: &[Form], config: RewriteConfig<'_>) -> Option<RewriteResult> {
    let subject_pos = resolve_subject_pos(sym, config.origin)?;
    let receiver_index = match subject_pos {
        SubjectPos::Fixed(pos) => pos,
        SubjectPos::Last => args.len(),
    };
    if receiver_index == 0 || receiver_index > args.len() {
        return None;
    }
    let mut converted = true;
    let mut args_vec: Vec<Form> = args.to_vec();
    let receiver = args_vec.remove(receiver_index - 1);
    let receiver_src = rewrite_form(&receiver, config, false);
    converted |= receiver_src.converted;
    let mut arg_srcs = Vec::with_capacity(args_vec.len());
    for arg in args_vec {
        let rewritten = rewrite_form(&arg, config, false);
        converted |= rewritten.converted;
        arg_srcs.push(rewritten.src);
    }
    let args_joined = arg_srcs.join(" ");
    let src = if args_joined.is_empty() {
        format!("{}.{}()", receiver_src.src, sym)
    } else {
        format!("{}.{}({})", receiver_src.src, sym, args_joined)
    };
    Some(RewriteResult { src, converted })
}

fn resolve_subject_pos(sym: &str, origin: &str) -> Option<SubjectPos> {
    if let Some(meta) = fn_meta::get(sym) {
        return meta.subject_pos.clone();
    }
    if let Some(target) = builtin_alias_target(sym) {
        if let Some(meta) = fn_meta::get(target) {
            return meta.subject_pos.clone();
        }
    }
    if !sym.contains("::") && !origin.is_empty() && origin != "special form" && origin != "macro" {
        let qualified = format!("{}::{}", origin, sym);
        if let Some(meta) = fn_meta::get(&qualified) {
            return meta.subject_pos.clone();
        }
    }
    None
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "def"
            | "def-"
            | "defn"
            | "defn-"
            | "-def"
            | "-defn"
            | "method"
            | "deftype"
            | "defenum"
            | "for"
            | "condp"
            | "with-redefs"
            | "with-open"
            | "doto"
            | "describe"
            | "describe-type"
            | "infer-type"
            | "enum-members"
            | "let"
            | "if"
            | "do"
            | "where"
            | "fn"
            | "loop"
            | "set!"
            | "redef"
            | "and"
            | "or"
            | "when"
            | "when-not"
            | "if-not"
            | "when-let"
            | "if-let"
            | "if-some"
            | "cond"
            | "cond->"
            | "cond->>"
            | "->"
            | "->>"
            | "as->"
            | "some->"
            | "some->>"
            | "throw"
            | "recur"
            | "try"
            | "catch"
            | "finally"
            | "err"
            | "fin"
            | "p"
            | "comment"
            | "while"
            | "doseq"
            | "each"
            | "dotimes"
            | "match"
            | "break"
            | "current-ns"
            | "repl"
            | "debug"
            | "use"
            | "use-syntax"
            | "ns-map"
            | "nav"
            | "lookup"
            | "create-ns"
            | "refer"
            | "resolve"
            | "load-file"
            | "load-string"
            | "delay"
            | "eval"
            | "with-dyn"
            | "go-loop"
            | "async-scope"
            | "async::scope"
            | "scope-loop"
            | "async::scope-loop"
    )
}
