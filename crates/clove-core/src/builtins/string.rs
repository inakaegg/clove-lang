use regex::Regex;

use crate::ast::Vector;
use crate::ast::{FnArity, Value};
use crate::builtins::{as_number, def_builtin, err, seq_items, sorted_map_get};
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::to_key_value_checked;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::seq::SeqHandle;
use crate::types::TypeKind;

pub(crate) fn install(env: &mut Env) {
    register_string_metas();
    // --- string namespace ---
    def_builtin!(env, "join", FnArity::range(1, 2), |args| {
        match args {
            [coll] => join_strings("", coll),
            [coll, sep] => {
                let sep_str = crate::builtins::expect_string(sep, "join", 2)?;
                join_strings(&sep_str, coll)
            }
            _ => err("join expects (coll) or (coll sep)"),
        }
    });
    define_string_alias(env, "join", "join");

    def_builtin!(env, "split", FnArity::range(2, 3), |args| {
        let (s_val, pat_val, limit_val) = match args {
            [s, pat] => (s, pat, None),
            [s, pat, n] => (s, pat, Some(n)),
            _ => return err("split expects string, pattern string, optional limit"),
        };
        let s = crate::builtins::expect_string(s_val, "split", 1)?;
        let regex = match pat_val {
            Value::String(pat) => build_regex(pat)?,
            Value::Regex(re) => re.regex.clone(),
            other => {
                return Err(crate::builtins::type_mismatch_arg(
                    "string or regex",
                    "split",
                    2,
                    other,
                ))
            }
        };
        let limit = limit_val
            .map(|n| crate::builtins::as_number(n))
            .transpose()?
            .map(|(n, _)| n.max(0.0) as usize);
        split_with_regex(&s, &regex, limit)
    });
    define_string_alias(env, "split", "split");

    def_builtin!(env, "str-replace", FnArity::exact(3), |args| {
        let [s, from, to] = args else {
            return err("str-replace expects string, match string, replacement string");
        };
        let s = crate::builtins::expect_string(s, "str-replace", 1)?;
        let from = crate::builtins::expect_string(from, "str-replace", 2)?;
        let to = crate::builtins::expect_string(to, "str-replace", 3)?;
        Ok(Value::String(s.replace(&from, &to)))
    });
    define_string_alias(env, "replace", "str-replace");

    def_builtin!(env, "replace-first", FnArity::exact(3), |args| {
        let [s_val, pat_val, to_val] = args else {
            return err("replace-first expects string, pattern string/regex, replacement string");
        };
        let s = crate::builtins::expect_string(s_val, "replace-first", 1)?;
        let to = crate::builtins::expect_string(to_val, "replace-first", 3)?;
        match pat_val {
            Value::String(from) => Ok(Value::String(s.replacen(from, &to, 1))),
            Value::Regex(pattern) => Ok(Value::String(
                pattern.regex.replace(&s, to.as_str()).into_owned(),
            )),
            other => Err(crate::builtins::type_mismatch_arg(
                "string or regex",
                "replace-first",
                2,
                other,
            )),
        }
    });
    define_string_alias(env, "replace-first", "replace-first");

    def_builtin!(env, "re-find", FnArity::exact(2), |args| {
        match args {
            [pattern, Value::String(text)] => {
                let regex = resolve_regex(pattern)?;
                Ok(regex
                    .captures(text)
                    .map(|caps| capture_to_value(&caps))
                    .unwrap_or(Value::Nil))
            }
            [_, text] => Err(crate::builtins::type_mismatch_arg(
                "string", "re-find", 2, text,
            )),
            _ => err("re-find expects regex and string"),
        }
    });

    def_builtin!(env, "re-matches", FnArity::exact(2), |args| {
        match args {
            [pattern, Value::String(text)] => {
                let regex = resolve_regex(pattern)?;
                Ok(regex
                    .captures(text)
                    .and_then(|caps| {
                        caps.get(0).and_then(|full| {
                            if full.start() == 0 && full.end() == text.len() {
                                Some(capture_to_value(&caps))
                            } else {
                                None
                            }
                        })
                    })
                    .unwrap_or(Value::Nil))
            }
            [_, text] => Err(crate::builtins::type_mismatch_arg(
                "string",
                "re-matches",
                2,
                text,
            )),
            _ => err("re-matches expects regex and string"),
        }
    });

    def_builtin!(env, "upper-case", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "upper-case", 1)?;
        Ok(Value::String(s.to_uppercase()))
    });
    define_string_alias(env, "upper-case", "upper-case");

    def_builtin!(env, "lower-case", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "lower-case", 1)?;
        Ok(Value::String(s.to_lowercase()))
    });
    define_string_alias(env, "lower-case", "lower-case");

    def_builtin!(env, "trim", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "trim", 1)?;
        Ok(Value::String(s.trim().to_string()))
    });
    define_string_alias(env, "trim", "trim");

    def_builtin!(env, "triml", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "triml", 1)?;
        Ok(Value::String(s.trim_start().to_string()))
    });
    define_string_alias(env, "triml", "triml");

    def_builtin!(env, "trimr", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "trimr", 1)?;
        Ok(Value::String(s.trim_end().to_string()))
    });
    define_string_alias(env, "trimr", "trimr");

    def_builtin!(env, "blank?", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "blank?", 1)?;
        Ok(Value::Bool(s.trim().is_empty()))
    });
    define_string_alias(env, "blank?", "blank?");

    def_builtin!(env, "string::includes?", FnArity::exact(2), |args| {
        let s = crate::builtins::expect_string(&args[0], "includes?", 1)?;
        let substr = crate::builtins::expect_string(&args[1], "includes?", 2)?;
        Ok(Value::Bool(s.contains(&substr)))
    });
    define_string_alias(env, "includes?", "string::includes?");

    def_builtin!(env, "starts-with?", FnArity::exact(2), |args| {
        let s = crate::builtins::expect_string(&args[0], "starts-with?", 1)?;
        let prefix = crate::builtins::expect_string(&args[1], "starts-with?", 2)?;
        Ok(Value::Bool(s.starts_with(&prefix)))
    });
    define_string_alias(env, "starts-with?", "starts-with?");

    def_builtin!(env, "ends-with?", FnArity::exact(2), |args| {
        let s = crate::builtins::expect_string(&args[0], "ends-with?", 1)?;
        let suffix = crate::builtins::expect_string(&args[1], "ends-with?", 2)?;
        Ok(Value::Bool(s.ends_with(&suffix)))
    });
    define_string_alias(env, "ends-with?", "ends-with?");

    def_builtin!(env, "split-lines", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "split-lines", 1)?;
        Ok(Value::Vector(
            s.lines()
                .map(|l| Value::String(l.to_string()))
                .collect::<Vector<_>>(),
        ))
    });
    define_string_alias(env, "split-lines", "split-lines");

    def_builtin!(env, "lines", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "lines", 1)?;
        if s.is_empty() {
            return Ok(Value::Vector(Vector::new()));
        }
        Ok(Value::Vector(
            s.split_inclusive('\n')
                .map(|l| Value::String(l.to_string()))
                .collect::<Vector<_>>(),
        ))
    });
    define_string_alias(env, "lines", "lines");

    def_builtin!(env, "reverse-str", FnArity::exact(1), |args| match args {
        [v] => {
            let s = crate::builtins::expect_string(v, "reverse", 1)?;
            Ok(Value::String(s.chars().rev().collect()))
        }
        _ => err("reverse expects string"),
    });
    define_string_alias(env, "reverse", "reverse-str");

    def_builtin!(env, "capitalize", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "capitalize", 1)?;
        let mut chars = s.chars();
        if let Some(first) = chars.next() {
            let mut out = String::new();
            out.extend(first.to_uppercase());
            out.push_str(&chars.as_str().to_lowercase());
            Ok(Value::String(out))
        } else {
            Ok(Value::String(String::new()))
        }
    });
    define_string_alias(env, "capitalize", "capitalize");

    def_builtin!(env, "trim-newline", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "trim-newline", 1)?;
        let mut out = s.clone();
        if out.ends_with('\n') {
            out.pop();
            if out.ends_with('\r') {
                out.pop();
            }
        }
        Ok(Value::String(out))
    });
    define_string_alias(env, "trim-newline", "trim-newline");

    def_builtin!(env, "escape", FnArity::exact(2), |args| {
        let s = crate::builtins::expect_string(&args[0], "escape", 1)?;
        let mapping = match &args[1] {
            Value::Map(_) | Value::SortedMap(_) => &args[1],
            other => {
                return Err(crate::builtins::type_mismatch_arg(
                    "map", "escape", 2, other,
                ))
            }
        };
        let mut out = String::new();
        for ch in s.chars() {
            let key = Value::String(ch.to_string());
            let found = match mapping {
                Value::Map(map) => map.get(&to_key_value_checked(&key)?).cloned(),
                Value::SortedMap(map) => sorted_map_get(map, &to_key_value_checked(&key)?)?,
                Value::MutMap(handle) => {
                    let map = handle
                        .lock()
                        .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                    map.get(&to_key_value_checked(&key)?).cloned()
                }
                _ => None,
            };
            if let Some(repl) = found {
                match repl {
                    Value::String(text) => out.push_str(&text),
                    other => out.push_str(&other.to_string()),
                }
            } else {
                out.push(ch);
            }
        }
        Ok(Value::String(out))
    });
    define_string_alias(env, "escape", "escape");

    def_builtin!(env, "index-of", FnArity::range(2, 3), |args| match args {
        [Value::String(s), Value::String(substr)] => {
            Ok(index_of(s, substr, 0).map_or(Value::Nil, |i| Value::Int(i as i64)))
        }
        [Value::String(s), Value::String(substr), n] => {
            let start = as_number(n)?.0.max(0.0) as usize;
            Ok(index_of(s, substr, start).map_or(Value::Nil, |i| Value::Int(i as i64)))
        }
        [Value::String(_s), other, ..] => Err(crate::builtins::type_mismatch_arg(
            "string", "index-of", 2, other,
        )),
        [s, _, ..] => Err(crate::builtins::type_mismatch_arg(
            "string", "index-of", 1, s,
        )),
        _ => err("index-of expects string, substring, optional start"),
    });
    define_string_alias(env, "index-of", "index-of");

    def_builtin!(
        env,
        "last-index-of",
        FnArity::range(2, 3),
        |args| match args {
            [Value::String(s), Value::String(substr)] => {
                Ok(last_index_of(s, substr, None).map_or(Value::Nil, |i| Value::Int(i as i64)))
            }
            [Value::String(s), Value::String(substr), n] => {
                let end = as_number(n)?.0.max(0.0) as usize;
                Ok(
                    last_index_of(s, substr, Some(end))
                        .map_or(Value::Nil, |i| Value::Int(i as i64)),
                )
            }
            [Value::String(_s), other, ..] => Err(crate::builtins::type_mismatch_arg(
                "string",
                "last-index-of",
                2,
                other,
            )),
            [s, _, ..] => Err(crate::builtins::type_mismatch_arg(
                "string",
                "last-index-of",
                1,
                s,
            )),
            _ => err("last-index-of expects string, substring, optional end"),
        }
    );
    define_string_alias(env, "last-index-of", "last-index-of");

    def_builtin!(env, "re-pattern", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "re-pattern", 1)?;
        let rv = crate::ast::RegexValue::new(s)?;
        Ok(Value::Regex(rv))
    });
    define_string_alias(env, "re-pattern", "re-pattern");
    // alias: regex (String -> Regex)
    def_builtin!(env, "regex", FnArity::exact(1), |args| {
        let s = crate::builtins::expect_string(&args[0], "regex", 1)?;
        let rv = crate::ast::RegexValue::new(s)?;
        Ok(Value::Regex(rv))
    });
    define_string_alias(env, "regex", "regex");

    def_builtin!(env, "re-seq", FnArity::exact(2), |args| match args {
        [pattern, Value::String(text)] => {
            let regex = resolve_regex(pattern)?;
            let mut items = Vec::new();
            for caps in regex.captures_iter(text) {
                items.push(capture_to_value(&caps));
            }
            Ok(Value::Seq(SeqHandle::from_iter(items.into_iter())))
        }
        [_, text] => Err(crate::builtins::type_mismatch_arg(
            "string", "re-seq", 2, text,
        )),
        _ => err("re-seq expects regex and string"),
    });
    define_string_alias(env, "re-seq", "re-seq");

    def_builtin!(env, "re-matcher", FnArity::range(1, 2), |args| match args {
        [Value::String(s)] => {
            let rv = crate::ast::RegexValue::new(s.clone())?;
            Ok(Value::Regex(rv))
        }
        [pattern, Value::String(_text)] => {
            // simple matcher proxy; return compiled regex
            let regex = resolve_regex(pattern)?;
            Ok(Value::Regex(crate::ast::RegexValue::new(regex.as_str())?))
        }
        [_, text] => Err(crate::builtins::type_mismatch_arg(
            "string",
            "re-matcher",
            2,
            text,
        )),
        _ => err("re-matcher expects pattern or pattern and text"),
    });
    define_string_alias(env, "re-matcher", "re-matcher");
}

fn define_string_alias(env: &mut Env, local: &str, source: &str) {
    if let Some(value) = env.get(source) {
        env.define_builtin(&format!("string::{}", local), value.clone());
        env.define_builtin(&format!("str::{}", local), value);
    } else {
        panic!("missing builtin '{}' for string namespace", source);
    }
}

fn join_strings(sep: &str, coll: &Value) -> Result<Value, CloveError> {
    let seq = seq_items(coll)?;
    let mut out = Vec::with_capacity(seq.len());
    for v in seq {
        match v {
            Value::String(s) => out.push(s),
            other => out.push(other.to_string()),
        }
    }
    Ok(Value::String(out.join(sep)))
}

fn build_regex(pat: &str) -> Result<Regex, CloveError> {
    Regex::new(pat).map_err(|e| CloveError::runtime(format!("invalid regex: {}", e)))
}

fn split_with_regex(s: &str, re: &Regex, limit: Option<usize>) -> Result<Value, CloveError> {
    let parts: Vec<String> = match limit {
        Some(0) => vec![s.to_string()],
        Some(n) if n > 0 => re.splitn(s, n).map(|p| p.to_string()).collect(),
        _ => re.split(s).map(|p| p.to_string()).collect(),
    };
    Ok(Value::Vector(
        parts.into_iter().map(Value::String).collect::<Vector<_>>(),
    ))
}

fn resolve_regex(value: &Value) -> Result<Regex, CloveError> {
    match value {
        Value::Regex(re) => Ok(re.regex.clone()),
        Value::String(pat) => build_regex(pat),
        _ => err("regex pattern must be regex literal or string"),
    }
}

fn capture_to_value(captures: &regex::Captures) -> Value {
    if captures.len() == 1 {
        return Value::String(
            captures
                .get(0)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default(),
        );
    }
    let mut items = Vector::new();
    for idx in 0..captures.len() {
        match captures.get(idx) {
            Some(m) => items.push_back(Value::String(m.as_str().to_string())),
            None => items.push_back(Value::Nil),
        }
    }
    Value::Vector(items)
}

fn index_of(s: &str, substr: &str, start: usize) -> Option<usize> {
    if start >= s.len() {
        return None;
    }
    s[start..].find(substr).map(|idx| idx + start)
}

fn last_index_of(s: &str, substr: &str, end: Option<usize>) -> Option<usize> {
    let slice_end = end.unwrap_or_else(|| s.len());
    if slice_end == 0 || substr.is_empty() && slice_end == 0 {
        return None;
    }
    let capped_end = slice_end.min(s.len());
    s[..capped_end].rfind(substr)
}

fn register_string_metas() {
    fn str_type() -> TypeKind {
        TypeKind::Str
    }
    fn bool_type() -> TypeKind {
        TypeKind::Bool
    }
    fn regex_type() -> TypeKind {
        TypeKind::Named("clove::core::Regex".into())
    }
    fn vec_str() -> TypeKind {
        TypeKind::vector(TypeKind::Str)
    }
    fn map_str_str() -> TypeKind {
        TypeKind::map(TypeKind::Str, TypeKind::Str)
    }

    let mut join_meta = FnMeta::new("string", "join");
    join_meta.arglist.push("[coll]".into());
    join_meta.arglist.push("[coll sep]".into());
    join_meta.doc = Some("Join collection of strings, optionally using separator.".into());
    join_meta.overloads.push(FnOverload {
        arg_types: vec![vec_str()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    join_meta.overloads.push(FnOverload {
        arg_types: vec![vec_str(), str_type()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    join_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(join_meta);

    let mut split_meta = FnMeta::new("string", "split");
    split_meta.arglist.push("[text pattern]".into());
    split_meta.arglist.push("[text pattern limit]".into());
    split_meta.doc = Some("Split text using regex pattern, optionally limiting results.".into());
    split_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), regex_type()],
        rest: None,
        ret_type: vec_str(),
        special_op: None,
    });
    split_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), regex_type(), TypeKind::Int],
        rest: None,
        ret_type: vec_str(),
        special_op: None,
    });
    split_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(split_meta);

    let mut str_replace_meta = FnMeta::new("string", "str-replace");
    str_replace_meta
        .arglist
        .push("[text pattern replacement]".into());
    str_replace_meta.doc = Some("Replace all matches of pattern with replacement.".into());
    str_replace_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), str_type(), str_type()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    str_replace_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(str_replace_meta);

    let mut replace_first_meta = FnMeta::new("string", "replace-first");
    replace_first_meta
        .arglist
        .push("[text pattern replacement]".into());
    replace_first_meta.doc = Some("Replace first occurrence of pattern with replacement.".into());
    replace_first_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), str_type(), str_type()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    replace_first_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), regex_type(), str_type()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    replace_first_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(replace_first_meta);

    let mut re_find_meta = FnMeta::new("string", "re-find");
    re_find_meta.arglist.push("[pattern text]".into());
    re_find_meta.doc = Some("Return first regex match or nil.".into());
    re_find_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, str_type()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    re_find_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(re_find_meta);

    let mut re_matches_meta = FnMeta::new("string", "re-matches");
    re_matches_meta.arglist.push("[pattern text]".into());
    re_matches_meta.doc = Some("Return text if entire string matches pattern; else nil.".into());
    re_matches_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, str_type()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    re_matches_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(re_matches_meta);

    macro_rules! simple_str_fn {
        ($name:expr, $doc:expr, $ret:expr) => {{
            let mut meta = FnMeta::new("string", $name);
            meta.arglist.push("[text]".into());
            meta.doc = Some($doc.into());
            meta.overloads.push(FnOverload {
                arg_types: vec![str_type()],
                rest: None,
                ret_type: $ret,
                special_op: None,
            });
            meta.subject_pos = Some(SubjectPos::Fixed(1));
            fn_meta::register(meta);
        }};
    }

    simple_str_fn!("upper-case", "Convert text to upper case.", str_type());
    simple_str_fn!("lower-case", "Convert text to lower case.", str_type());
    simple_str_fn!("trim", "Trim whitespace from both ends.", str_type());
    simple_str_fn!("triml", "Trim whitespace from the left side.", str_type());
    simple_str_fn!("trimr", "Trim whitespace from the right side.", str_type());
    simple_str_fn!(
        "blank?",
        "Return true if string is empty or whitespace.",
        bool_type()
    );
    simple_str_fn!("split-lines", "Split string into lines.", vec_str());
    simple_str_fn!(
        "lines",
        "Split string into lines, keeping line endings.",
        vec_str()
    );
    simple_str_fn!(
        "reverse-str",
        "Reverse the characters of the string.",
        str_type()
    );
    simple_str_fn!("capitalize", "Capitalize the first character.", str_type());
    simple_str_fn!(
        "trim-newline",
        "Trim trailing newline or carriage return.",
        str_type()
    );
    simple_str_fn!("re-pattern", "Compile string into regex.", regex_type());
    simple_str_fn!("regex", "Alias for re-pattern.", regex_type());

    macro_rules! str2_bool_fn {
        ($name:expr, $doc:expr) => {{
            let mut meta = FnMeta::new("string", $name);
            meta.arglist.push("[text substr]".into());
            meta.doc = Some($doc.into());
            meta.overloads.push(FnOverload {
                arg_types: vec![str_type(), str_type()],
                rest: None,
                ret_type: bool_type(),
                special_op: None,
            });
            meta.subject_pos = Some(SubjectPos::Fixed(1));
            fn_meta::register(meta);
        }};
    }

    str2_bool_fn!("includes?", "Return true when text contains substring.");
    str2_bool_fn!(
        "starts-with?",
        "Return true when text starts with substring."
    );
    str2_bool_fn!("ends-with?", "Return true when text ends with substring.");

    let mut escape_meta = FnMeta::new("string", "escape");
    escape_meta.arglist.push("[text replacements]".into());
    escape_meta.doc = Some("Escape characters using replacements map.".into());
    escape_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), map_str_str()],
        rest: None,
        ret_type: str_type(),
        special_op: None,
    });
    escape_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(escape_meta);

    let mut index_of_meta = FnMeta::new("string", "index-of");
    index_of_meta.arglist.push("[text substr]".into());
    index_of_meta
        .arglist
        .push("[text substr from-index]".into());
    index_of_meta.doc = Some("Return index of substring or nil starting at optional index.".into());
    index_of_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), str_type()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    index_of_meta.overloads.push(FnOverload {
        arg_types: vec![str_type(), str_type(), TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    index_of_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(index_of_meta);

    let mut re_seq_meta = FnMeta::new("string", "re-seq");
    re_seq_meta.arglist.push("[pattern text]".into());
    re_seq_meta.doc = Some("Return vector of all regex matches in text.".into());
    re_seq_meta.overloads.push(FnOverload {
        arg_types: vec![regex_type(), str_type()],
        rest: None,
        ret_type: vec_str(),
        special_op: None,
    });
    re_seq_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(re_seq_meta);

    let mut re_matcher_meta = FnMeta::new("string", "re-matcher");
    re_matcher_meta.arglist.push("[pattern text]".into());
    re_matcher_meta
        .arglist
        .push("[pattern matcher text]".into());
    re_matcher_meta.doc = Some("Create or reuse regex matcher for text.".into());
    re_matcher_meta.overloads.push(FnOverload {
        arg_types: vec![regex_type(), str_type()],
        rest: None,
        ret_type: regex_type(),
        special_op: None,
    });
    re_matcher_meta.overloads.push(FnOverload {
        arg_types: vec![regex_type(), regex_type(), str_type()],
        rest: None,
        ret_type: regex_type(),
        special_op: None,
    });
    re_matcher_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(re_matcher_meta);
}
