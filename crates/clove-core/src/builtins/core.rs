use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap as StdHashMap;
use std::convert::TryFrom;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use crate::ast::{is_mut_collection, FnArity, HashMap, Key, SortedMap, SortedSet, Value, Vector};
use crate::builtins::{
    as_number, def_builtin, err, map_like_to_hashmap, seq_items, sorted_map_from_entries,
    sorted_map_from_map, sorted_map_from_pairs, sorted_map_from_sorted_map, sorted_map_get,
    sorted_map_insert_mut, sorted_set_contains, sorted_set_from_items,
};
use crate::docs;
use crate::dynamic_vars;
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::{call_callable, current_file_name, form_to_value, to_key_value_checked};
use crate::fmt_config::load_fmt_config_path;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::fn_type;
use crate::form_source;
use crate::formatter::FormatOptions;
use crate::pretty_print::{pretty_print_source, PrettyOptions};
use crate::settings::REPL_ON_ERROR_VAR;
use crate::string_escape::escape_string_fragment;
use crate::symbol_meta;
use crate::symbols::canonical_symbol_name;
use crate::type_registry;
use crate::types::TypeKind;
use im::HashSet;
use once_cell::sync::Lazy;
use rand::seq::SliceRandom;
use rand::Rng;

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static META_STORE: Lazy<Mutex<StdHashMap<Value, Value>>> =
    Lazy::new(|| Mutex::new(StdHashMap::new()));
static DOC_STORE: Lazy<Mutex<StdHashMap<Value, String>>> =
    Lazy::new(|| Mutex::new(StdHashMap::new()));
static FLUSH_META_KEY: Lazy<Key> = Lazy::new(|| Key::Keyword("flush".into()));

fn write_dynamic(target: &str, content: String) -> Result<Value, CloveError> {
    if let Some(writer) = dynamic_vars::current_value(target) {
        return call_callable(writer, vec![Value::String(content)]);
    }
    Err(CloveError::runtime(format!(
        "dynamic var '{}' is not bound",
        target
    )))
}

fn flush_current_out() -> Result<Value, CloveError> {
    if let Some(writer) = dynamic_vars::current_value("*out*") {
        if let Some(handler) = writer_flush_callable(&writer) {
            call_callable(handler, Vec::new())?;
            return Ok(Value::Nil);
        }
    }
    flush_stdout_now()?;
    Ok(Value::Nil)
}

fn writer_flush_callable(writer: &Value) -> Option<Value> {
    match meta_lookup(writer) {
        Value::Map(meta_map) => meta_map.get(&*FLUSH_META_KEY).cloned(),
        _ => None,
    }
}

fn flush_stdout_now() -> Result<(), CloveError> {
    std::io::stdout()
        .flush()
        .map_err(|e| CloveError::runtime(format!("failed to flush stdout: {}", e)))
}

fn flush_stderr_now() -> Result<(), CloveError> {
    std::io::stderr()
        .flush()
        .map_err(|e| CloveError::runtime(format!("failed to flush stderr: {}", e)))
}

fn attach_flush_handler(writer: &Value, flush_fn: Value) {
    let mut meta = HashMap::new();
    meta.insert((*FLUSH_META_KEY).clone(), flush_fn);
    meta_set(writer.clone(), Value::Map(meta));
}

fn oop_seg_name(value: &Value) -> Result<String, CloveError> {
    match value {
        Value::String(s) => Ok(s.clone()),
        Value::Symbol(s) => Ok(s.trim_start_matches(':').to_string()),
        other => err(format!(
            "oop-seg expects name as string or symbol, got {}",
            other.type_name()
        )),
    }
}

fn oop_seg_lookup(target: &Value, name: &str) -> Result<Value, CloveError> {
    let key_kw = Key::Keyword(name.to_string());
    let key_str = Key::String(name.to_string());
    let key_sym = Key::Symbol(name.to_string());
    match target {
        Value::Map(map) => {
            if map.contains_key(&key_kw) {
                return Ok(map.get(&key_kw).cloned().unwrap_or(Value::Nil));
            }
            if map.contains_key(&key_str) {
                return Ok(map.get(&key_str).cloned().unwrap_or(Value::Nil));
            }
            if map.contains_key(&key_sym) {
                return Ok(map.get(&key_sym).cloned().unwrap_or(Value::Nil));
            }
        }
        Value::SortedMap(map) => {
            if let Some(val) = sorted_map_get(map, &key_kw)? {
                return Ok(val);
            }
            if let Some(val) = sorted_map_get(map, &key_str)? {
                return Ok(val);
            }
            if let Some(val) = sorted_map_get(map, &key_sym)? {
                return Ok(val);
            }
        }
        Value::Set(set) => {
            let kw = Value::Symbol(format!(":{}", name));
            if set.contains(&kw) {
                return Ok(kw);
            }
            let string_val = Value::String(name.to_string());
            if set.contains(&string_val) {
                return Ok(string_val);
            }
            let sym = Value::Symbol(name.to_string());
            if set.contains(&sym) {
                return Ok(sym);
            }
        }
        Value::SortedSet(set) => {
            let kw = Value::Symbol(format!(":{}", name));
            if sorted_set_contains(set, &kw)? {
                return Ok(kw);
            }
            let string_val = Value::String(name.to_string());
            if sorted_set_contains(set, &string_val)? {
                return Ok(string_val);
            }
            let sym = Value::Symbol(name.to_string());
            if sorted_set_contains(set, &sym)? {
                return Ok(sym);
            }
        }
        other => {
            return err(format!(
                "oop-seg expects map/set target, got {}",
                other.type_name()
            ))
        }
    }
    err(format!("oop-seg could not resolve segment '{}'", name))
}

pub(crate) fn install(env: &mut Env) {
    register_core_metas();
    let stdout_writer = Value::native_fn(FnArity::exact(1), |args| match args {
        [Value::String(s)] => {
            print!("{}", s);
            Ok(Value::Nil)
        }
        [other] => Err(CloveError::type_mismatch("string", other.type_name())),
        _ => err("*out* expects one string"),
    });
    let stderr_writer = Value::native_fn(FnArity::exact(1), |args| match args {
        [Value::String(s)] => {
            eprint!("{}", s);
            Ok(Value::Nil)
        }
        [other] => Err(CloveError::type_mismatch("string", other.type_name())),
        _ => err("*err* expects one string"),
    });
    attach_flush_handler(
        &stdout_writer,
        Value::native_fn(FnArity::exact(0), |_| {
            flush_stdout_now()?;
            Ok(Value::Nil)
        }),
    );
    attach_flush_handler(
        &stderr_writer,
        Value::native_fn(FnArity::exact(0), |_| {
            flush_stderr_now()?;
            Ok(Value::Nil)
        }),
    );
    env.define_builtin("*out*", stdout_writer.clone());
    env.define_builtin("*err*", stderr_writer.clone());
    dynamic_vars::set_root_value("*out*", stdout_writer);
    dynamic_vars::set_root_value("*err*", stderr_writer);
    def_builtin!(env, "current-file", FnArity::exact(0), |_args| {
        let Some(name) = current_file_name() else {
            return Ok(Value::Nil);
        };
        if name.starts_with('<') && name.ends_with('>') {
            return Ok(Value::Nil);
        }
        Ok(Value::String(name))
    });

    // --- IO / String ---
    def_builtin!(env, "println", FnArity::at_least(0), |args| {
        let rendered: Vec<String> = args.iter().map(render_display_arg).collect();
        let content = format!("{}\n", rendered.join(" "));
        write_dynamic("*out*", content)
    });
    def_builtin!(env, "print", FnArity::at_least(0), |args| {
        let rendered: Vec<String> = args.iter().map(render_display_arg).collect();
        let content = rendered.join(" ");
        write_dynamic("*out*", content)
    });
    alias(env, "puts", "println");
    def_builtin!(env, "pr-str", FnArity::at_least(0), |args| {
        let rendered: Vec<String> = args.iter().map(|v| v.to_string()).collect();
        Ok(Value::String(rendered.join(" ")))
    });
    def_builtin!(env, "prn", FnArity::at_least(0), |args| {
        let rendered: Vec<String> = args.iter().map(|v| v.to_string()).collect();
        let content = format!("{}\n", rendered.join(" "));
        write_dynamic("*out*", content)
    });
    def_builtin!(env, "pp-str", FnArity::range(1, 2), |args| {
        let (value, opts) = match args {
            [value] => (value, None),
            [value, opts] => (value, Some(opts)),
            _ => return err("pp-str expects a value and optional opts map"),
        };
        let config = parse_pp_config(opts, "pp-str", 2)?;
        let rendered = pp_str_value(value, &config)?;
        Ok(Value::String(rendered))
    });
    def_builtin!(env, "pp", FnArity::range(1, 2), |args| {
        let (value, opts) = match args {
            [value] => (value, None),
            [value, opts] => (value, Some(opts)),
            _ => return err("pp expects a value and optional opts map"),
        };
        let config = parse_pp_config(opts, "pp", 2)?;
        let rendered = pp_str_value(value, &config)?;
        let content = format!("{}\n", rendered);
        write_dynamic("*out*", content)?;
        Ok(value.clone())
    });
    alias(env, "pprint", "pp");
    alias(env, "pprint-str", "pp-str");
    def_builtin!(env, "flush", FnArity::exact(0), |_args| flush_current_out());
    def_builtin!(env, "pvalues", FnArity::at_least(0), |args| {
        Ok(Value::Vector(Vector::from(args.to_vec())))
    });
    def_builtin!(env, "str", FnArity::at_least(0), |args| {
        let rendered: Vec<String> = args.iter().map(render_str_arg).collect();
        Ok(Value::String(rendered.join("")))
    });
    def_builtin!(env, "slurp", FnArity::exact(1), |args| {
        match args {
            [Value::String(path)] => slurp_to_string(path).map(Value::String),
            _ => err("slurp expects a path string"),
        }
    });
    def_builtin!(env, "read-string", FnArity::exact(1), |args| {
        match args {
            [Value::String(src)] => {
                let mut reader = crate::reader::Reader::new_with_options(
                    src,
                    crate::reader::ReaderOptions::language_defaults(vec![]),
                );
                let forms = reader.read_all()?;
                let first = forms
                    .into_iter()
                    .next()
                    .ok_or_else(|| CloveError::runtime("read-string found no form"))?;
                form_to_value(&first)
            }
            _ => err("read-string expects a string"),
        }
    });
    def_builtin!(env, "read", FnArity::range(0, 1), |args| {
        let source_text = match args {
            [] => {
                let mut buf = String::new();
                std::io::stdin()
                    .read_line(&mut buf)
                    .map_err(|e| CloveError::runtime(e.to_string()))?;
                if buf.ends_with('\n') {
                    buf.pop();
                    if buf.ends_with('\r') {
                        buf.pop();
                    }
                }
                buf
            }
            [Value::String(s)] => s.clone(),
            [other] => return Err(CloveError::type_mismatch("string", other.type_name())),
            _ => return err("read expects zero or one argument"),
        };
        let mut reader = crate::reader::Reader::new_with_options(
            &source_text,
            crate::reader::ReaderOptions::language_defaults(vec![]),
        );
        let forms = reader.read_all()?;
        if let Some(form) = forms.into_iter().next() {
            form_to_value(&form)
        } else {
            Ok(Value::Nil)
        }
    });
    def_builtin!(env, "oop-seg", FnArity::exact(2), |args| {
        match args {
            [target, name] => {
                let name = oop_seg_name(name)?;
                oop_seg_lookup(target, &name)
            }
            _ => err("oop-seg expects target and name"),
        }
    });
    def_builtin!(env, "symbol", FnArity::exact(1), |args| match args {
        [Value::Symbol(s)] | [Value::String(s)] => {
            let raw = s.trim_start_matches(':');
            Ok(Value::Symbol(canonical_symbol_name(raw).into_owned()))
        }
        [other] => Err(CloveError::type_mismatch(
            "string or symbol",
            other.type_name(),
        )),
        _ => err("symbol expects one argument"),
    });
    def_builtin!(env, "keyword", FnArity::range(1, 2), |args| match args {
        [Value::Symbol(s)] | [Value::String(s)] =>
            Ok(Value::Symbol(format!(":{}", s.trim_start_matches(':')))),
        [Value::Symbol(ns), Value::Symbol(name)]
        | [Value::String(ns), Value::Symbol(name)]
        | [Value::Symbol(ns), Value::String(name)]
        | [Value::String(ns), Value::String(name)] => {
            let ns_part = ns.trim_start_matches(':');
            let name_part = name.trim_start_matches(':');
            let sep = if name_part.starts_with('/') { "" } else { "/" };
            Ok(Value::Symbol(format!(":{}{}{}", ns_part, sep, name_part)))
        }
        [_, _] => Err(CloveError::type_mismatch(
            "string or symbol",
            "non-string or non-symbol",
        )),
        [other] => Err(CloveError::type_mismatch(
            "string or symbol",
            other.type_name(),
        )),
        _ => err("keyword expects one or two arguments"),
    });
    def_builtin!(env, "name", FnArity::exact(1), |args| match args {
        [Value::Symbol(s)] => Ok(Value::String(s.trim_start_matches(':').to_string())),
        [Value::String(s)] => Ok(Value::String(s.clone())),
        [other] => Err(CloveError::type_mismatch(
            "symbol, keyword, or string",
            other.type_name(),
        )),
        _ => err("name expects one argument"),
    });
    def_builtin!(env, "gensym", FnArity::range(0, 1), |args| {
        let prefix = match args {
            [] => "G__".to_string(),
            [Value::String(s)] | [Value::Symbol(s)] => s.clone(),
            [other] => {
                return Err(CloveError::type_mismatch(
                    "string or symbol",
                    other.type_name(),
                ))
            }
            _ => return err("gensym expects zero or one argument"),
        };
        let id = GENSYM_COUNTER.fetch_add(1, AtomicOrdering::SeqCst);
        let name = format!("{}{}", canonical_symbol_name(&prefix), id);
        Ok(Value::Symbol(name))
    });
    def_builtin!(env, "meta", FnArity::exact(1), |args| match args {
        [v] => Ok(meta_lookup(v)),
        _ => err("meta expects one argument"),
    });
    def_builtin!(env, "doc", FnArity::exact(1), |args| match args {
        [Value::Symbol(name)] | [Value::String(name)] =>
            Ok(doc_from_name(name).map(Value::String).unwrap_or(Value::Nil)),
        [value] => Ok(doc_from_value(value)
            .map(Value::String)
            .unwrap_or(Value::Nil)),
        _ => err("doc expects one argument"),
    });
    def_builtin!(env, "source", FnArity::exact(1), |args| match args {
        [Value::Symbol(name)] | [Value::String(name)] => Ok(source_from_name(name)
            .map(Value::String)
            .unwrap_or(Value::Nil)),
        [value] => Ok(source_from_value(value)
            .map(Value::String)
            .unwrap_or(Value::Nil)),
        _ => err("source expects one argument"),
    });
    def_builtin!(env, "with-meta", FnArity::exact(2), |args| match args {
        [v, m] => {
            meta_set(v.clone(), m.clone());
            Ok(v.clone())
        }
        _ => err("with-meta expects value and meta map"),
    });
    def_builtin!(env, "vary-meta", FnArity::at_least(2), |args| match args {
        [v, f, extra @ ..] => {
            let current = meta_lookup(v);
            let mut call_args = Vec::with_capacity(1 + extra.len());
            call_args.push(current);
            call_args.extend_from_slice(extra);
            let new_meta = call_callable(f.clone(), call_args)?;
            meta_set(v.clone(), new_meta);
            Ok(v.clone())
        }
        _ => err("vary-meta expects value, function, and optional args"),
    });
    def_builtin!(env, "spit", FnArity::exact(2), |args| {
        match args {
            [Value::String(path), Value::String(content)] => {
                fs::write(path, content)
                    .map_err(|e| CloveError::runtime(format!("spit failed: {}", e)))?;
                Ok(Value::String(path.clone()))
            }
            _ => err("spit expects path string and content string"),
        }
    });
    def_builtin!(env, "subs", FnArity::range(2, 3), |args| {
        match args {
            [Value::String(s), start] => {
                let (st_val, _) = as_number(start)?;
                let st = st_val as usize;
                Ok(Value::String(s.chars().skip(st).collect()))
            }
            [Value::String(s), start, end] => {
                let (st_val, _) = as_number(start)?;
                let (en_val, _) = as_number(end)?;
                let st = st_val as usize;
                let en = en_val as usize;
                Ok(Value::String(
                    s.chars().skip(st).take(en.saturating_sub(st)).collect(),
                ))
            }
            _ => err("subs expects (string start) or (string start end)"),
        }
    });

    // --- Collections Construction ---
    def_builtin!(env, "list", FnArity::at_least(0), |args| {
        Ok(Value::List(Vector::from(args.to_vec())))
    });
    def_builtin!(env, "vector", FnArity::at_least(0), |args| {
        Ok(Value::Vector(Vector::from(args.to_vec())))
    });
    def_builtin!(env, "hash-map", FnArity::at_least(0), |args| {
        if args.len() % 2 != 0 {
            return err("hash-map expects even number of key/value args");
        }
        let mut map = HashMap::new();
        let mut iter = args.iter();
        while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
            map.insert(to_key_value_checked(k)?, v.clone());
        }
        Ok(Value::Map(map))
    });
    def_builtin!(env, "sorted-map-by", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((cmp, rest)) => {
                let map = build_sorted_map(cmp.clone(), rest, "sorted-map-by", 2)?;
                Ok(Value::SortedMap(map))
            }
            None => err("sorted-map-by expects comparator and key/value args"),
        }
    });
    def_builtin!(env, "hash-set", FnArity::at_least(0), |args| {
        let mut set = HashSet::new();
        for v in args {
            if is_mut_collection(v) {
                return err("cannot put mutable collection into set; use (imut x)");
            }
            set.insert(v.clone());
        }
        Ok(Value::Set(set))
    });
    def_builtin!(env, "set", FnArity::at_least(0), |args| {
        let mut set = HashSet::new();
        for v in args {
            if is_mut_collection(v) {
                return err("cannot put mutable collection into set; use (imut x)");
            }
            set.insert(v.clone());
        }
        Ok(Value::Set(set))
    });
    def_builtin!(env, "sorted-set-by", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((cmp, rest)) => {
                let set = build_sorted_set(cmp.clone(), rest)?;
                Ok(Value::SortedSet(set))
            }
            None => err("sorted-set-by expects comparator and values"),
        }
    });
    def_builtin!(env, "find", FnArity::exact(2), |args| match args {
        [Value::Map(map), key] => {
            let lookup = to_key_value_checked(key)?;
            if let Some(val) = map.get(&lookup) {
                Ok(Value::Vector(Vector::from(vec![key.clone(), val.clone()])))
            } else {
                Ok(Value::Nil)
            }
        }
        [Value::SortedMap(map), key] => {
            let lookup = to_key_value_checked(key)?;
            if let Some(val) = sorted_map_get(map, &lookup)? {
                Ok(Value::Vector(Vector::from(vec![key.clone(), val])))
            } else {
                Ok(Value::Nil)
            }
        }
        [Value::MutMap(handle), key] => {
            let map = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
            let lookup = to_key_value_checked(key)?;
            if let Some(val) = map.get(&lookup) {
                Ok(Value::Vector(Vector::from(vec![key.clone(), val.clone()])))
            } else {
                Ok(Value::Nil)
            }
        }
        _ => err("find expects map and key"),
    });
    def_builtin!(env, "key", FnArity::exact(1), |args| match args {
        [Value::Vector(v)] if v.len() >= 2 => Ok(v[0].clone()),
        [Value::List(v)] if v.len() >= 2 => Ok(v[0].clone()),
        [other] => Err(CloveError::type_mismatch("map entry", other.type_name(),)),
        _ => err("key expects one argument"),
    });
    def_builtin!(env, "val", FnArity::exact(1), |args| match args {
        [Value::Vector(v)] if v.len() >= 2 => Ok(v[1].clone()),
        [Value::List(v)] if v.len() >= 2 => Ok(v[1].clone()),
        [other] => Err(CloveError::type_mismatch("map entry", other.type_name(),)),
        _ => err("val expects one argument"),
    });
    def_builtin!(env, "hash", FnArity::exact(1), |args| match args {
        [value] => {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            Ok(Value::Int(hasher.finish() as i64))
        }
        _ => err("hash expects one argument"),
    });

    def_builtin!(env, "type", FnArity::exact(1), |args| {
        Ok(Value::Symbol(type_registry::type_of_value(&args[0])))
    });
    alias(env, "type-of", "type");
    def_builtin!(env, "instance?", FnArity::exact(2), |args| match args {
        [type_val, value] => {
            let expected = normalize_type_name_from_value(type_val)?;
            Ok(Value::Bool(type_registry::conforms_named(&expected, value)))
        }
        _ => err("instance? expects type and value"),
    });
    def_builtin!(env, "tagged?", FnArity::exact(2), |args| match args {
        [type_val, value] => {
            let expected = normalize_type_name_from_value(type_val)?;
            let kind = TypeKind::named(expected);
            Ok(Value::Bool(type_registry::matches_type_expr(&kind, value)))
        }
        _ => err("tagged? expects type and value"),
    });
    def_builtin!(env, "describe", FnArity::exact(1), |args| {
        type_registry::describe_value_arg(&args[0], None)
    });
    alias(env, "describe-type", "describe");
    def_builtin!(env, "fn-type", FnArity::exact(1), |args| match args {
        [value] => Ok(fn_type::fn_type_string(value)
            .map(Value::String)
            .unwrap_or(Value::Nil)),
        _ => err("fn-type expects one argument"),
    });
    def_builtin!(env, "enum-members", FnArity::exact(1), |args| {
        match type_registry::describe_value_arg(&args[0], None)? {
            Value::Map(map) => Ok(map
                .get(&Key::Keyword("members".into()))
                .cloned()
                .unwrap_or(Value::Nil)),
            Value::SortedMap(map) => {
                Ok(sorted_map_get(&map, &Key::Keyword("members".into()))?.unwrap_or(Value::Nil))
            }
            other => Ok(other),
        }
    });
    def_builtin!(env, "all-types", FnArity::exact(0), |_args| {
        let names = type_registry::list_all_types();
        let mut set = HashSet::new();
        for name in names {
            set.insert(Value::Symbol(format!(":{}", name)));
        }
        Ok(Value::Set(set))
    });

    env.define_builtin(REPL_ON_ERROR_VAR, Value::Bool(false));

    // --- Measurement utilities ---
    def_builtin!(env, "time", FnArity::exact(1), |args| {
        let callable = args[0].clone();
        let start = Instant::now();
        let value = call_callable(callable, vec![])?;
        let elapsed = start.elapsed();
        Ok(Value::Map(measurement_result(value, 1, elapsed)))
    });

    def_builtin!(env, "bench", FnArity::exact(2), |args| {
        let iterations = match &args[0] {
            Value::Int(n) if *n > 0 => *n as u64,
            Value::Int(_) => return err("bench iterations must be positive"),
            other => {
                return Err(CloveError::type_mismatch("int", other.type_name()));
            }
        };
        let callable = args[1].clone();
        let start = Instant::now();
        let mut last = Value::Nil;
        for _ in 0..iterations {
            last = call_callable(callable.clone(), vec![])?;
        }
        let elapsed = start.elapsed();
        Ok(Value::Map(measurement_result(last, iterations, elapsed)))
    });

    def_builtin!(env, "runtime-error", FnArity::at_least(1), |args| {
        let rendered: Vec<String> = args
            .iter()
            .map(|v| match v {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            })
            .collect();
        err(rendered.join(" "))
    });

    def_builtin!(env, "compare", FnArity::exact(2), |args| {
        let ordering = compare_values(&args[0], &args[1])?;
        let result = match ordering {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        };
        Ok(Value::Int(result))
    });
    def_builtin!(env, "compare-desc", FnArity::exact(2), |args| {
        let ordering = compare_values(&args[1], &args[0])?;
        let result = match ordering {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        };
        Ok(Value::Int(result))
    });
    let default_compare = env.get("compare").expect("compare builtin missing");
    let desc_compare = env
        .get("compare-desc")
        .expect("compare-desc builtin missing");
    let sorted_map_compare = default_compare.clone();
    def_builtin!(env, "sorted-map", FnArity::at_least(0), |args| {
        let map = build_sorted_map(sorted_map_compare.clone(), args, "sorted-map", 1)?;
        Ok(Value::SortedMap(map))
    });
    let sorted_map_desc_compare = desc_compare.clone();
    def_builtin!(env, "sorted-map-desc", FnArity::at_least(0), |args| {
        let map = build_sorted_map(sorted_map_desc_compare.clone(), args, "sorted-map-desc", 1)?;
        Ok(Value::SortedMap(map))
    });
    let sorted_set_compare = default_compare.clone();
    def_builtin!(env, "sorted-set", FnArity::at_least(0), |args| {
        let set = build_sorted_set(sorted_set_compare.clone(), args)?;
        Ok(Value::SortedSet(set))
    });
    let sorted_set_desc_compare = desc_compare.clone();
    def_builtin!(env, "sorted-set-desc", FnArity::at_least(0), |args| {
        let set = build_sorted_set(sorted_set_desc_compare.clone(), args)?;
        Ok(Value::SortedSet(set))
    });
    def_builtin!(env, "min-key", FnArity::at_least(2), |args| {
        if args.len() < 2 {
            return err("min-key expects at least key function and one value");
        }
        let key_fn = args[0].clone();
        let mut best_val = args[1].clone();
        let mut best_key = call_callable(key_fn.clone(), vec![best_val.clone()])?;
        for candidate in &args[2..] {
            let candidate_key = call_callable(key_fn.clone(), vec![candidate.clone()])?;
            if compare_values(&candidate_key, &best_key)? == Ordering::Less {
                best_key = candidate_key;
                best_val = candidate.clone();
            }
        }
        Ok(best_val)
    });
    def_builtin!(env, "max-key", FnArity::at_least(2), |args| {
        if args.len() < 2 {
            return err("max-key expects at least key function and one value");
        }
        let key_fn = args[0].clone();
        let mut best_val = args[1].clone();
        let mut best_key = call_callable(key_fn.clone(), vec![best_val.clone()])?;
        for candidate in &args[2..] {
            let candidate_key = call_callable(key_fn.clone(), vec![candidate.clone()])?;
            if compare_values(&candidate_key, &best_key)? == Ordering::Greater {
                best_key = candidate_key;
                best_val = candidate.clone();
            }
        }
        Ok(best_val)
    });
    def_builtin!(env, "rand", FnArity::range(0, 1), |args| {
        let mut rng = rand::thread_rng();
        match args {
            [] => Ok(Value::Float(rng.gen::<f64>())),
            [n] => {
                let (upper, _) = as_number(n)?;
                Ok(Value::Float(rng.gen::<f64>() * upper))
            }
            _ => err("rand expects zero or one numeric argument"),
        }
    });
    def_builtin!(env, "shuffle", FnArity::exact(1), |args| match args {
        [coll] => {
            let mut rng = rand::thread_rng();
            let mut items: Vec<Value> = seq_items(coll)?.into_iter().collect();
            items.shuffle(&mut rng);
            Ok(Value::Vector(Vector::from(items)))
        }
        _ => err("shuffle expects one collection"),
    });
    def_builtin!(env, "format", FnArity::at_least(1), |args| {
        let tmpl = match args.first() {
            Some(Value::String(s)) => s.clone(),
            Some(other) => {
                return Err(CloveError::type_mismatch(
                    "format string",
                    other.type_name(),
                ))
            }
            None => return err("format expects at least one argument"),
        };
        let rendered = format_template(&tmpl, &args[1..])?;
        Ok(Value::String(rendered))
    });
    def_builtin!(env, "printf", FnArity::at_least(1), |args| {
        let tmpl = match args.first() {
            Some(Value::String(s)) => s.clone(),
            Some(other) => {
                return Err(CloveError::type_mismatch(
                    "format string",
                    other.type_name(),
                ))
            }
            None => return err("printf expects at least one argument"),
        };
        let rendered = format_template(&tmpl, &args[1..])?;
        print!("{}", rendered);
        Ok(Value::Nil)
    });
    def_builtin!(env, "replace", FnArity::exact(2), |args| match args {
        [Value::Map(mapping), coll] => {
            let items = crate::builtins::seq_items(coll)?;
            match coll {
                Value::Map(map_coll) => {
                    let mut new_map = HashMap::new();
                    for (k, v) in map_coll {
                        let new_key = mapping
                            .get(&k)
                            .map(|repl| to_key_value_checked(repl))
                            .transpose()?
                            .unwrap_or_else(|| k.clone());
                        new_map.insert(new_key, v.clone());
                    }
                    Ok(Value::Map(new_map))
                }
                Value::SortedMap(map_coll) => {
                    let mut out = SortedMap {
                        comparator: map_coll.comparator.clone(),
                        entries: Vec::new(),
                    };
                    for (k, v) in &map_coll.entries {
                        let new_key = mapping
                            .get(k)
                            .map(|repl| to_key_value_checked(repl))
                            .transpose()?
                            .unwrap_or_else(|| k.clone());
                        sorted_map_insert_mut(&mut out, new_key, v.clone())?;
                    }
                    Ok(Value::SortedMap(out))
                }
                _ => {
                    let mut out = Vector::new();
                    for item in items {
                        let key = to_key_value_checked(&item)?;
                        if let Some(repl) = mapping.get(&key) {
                            out.push_back(repl.clone());
                        } else {
                            out.push_back(item);
                        }
                    }
                    Ok(Value::Vector(out))
                }
            }
        }
        [Value::SortedMap(mapping), coll] => {
            let items = crate::builtins::seq_items(coll)?;
            match coll {
                Value::Map(map_coll) => {
                    let mut new_map = HashMap::new();
                    for (k, v) in map_coll {
                        let new_key = if let Some(repl) = sorted_map_get(mapping, &k)? {
                            to_key_value_checked(&repl)?
                        } else {
                            k.clone()
                        };
                        new_map.insert(new_key, v.clone());
                    }
                    Ok(Value::Map(new_map))
                }
                Value::SortedMap(map_coll) => {
                    let mut out = SortedMap {
                        comparator: map_coll.comparator.clone(),
                        entries: Vec::new(),
                    };
                    for (k, v) in &map_coll.entries {
                        let new_key = if let Some(repl) = sorted_map_get(mapping, k)? {
                            to_key_value_checked(&repl)?
                        } else {
                            k.clone()
                        };
                        sorted_map_insert_mut(&mut out, new_key, v.clone())?;
                    }
                    Ok(Value::SortedMap(out))
                }
                _ => {
                    let mut out = Vector::new();
                    for item in items {
                        let key = to_key_value_checked(&item)?;
                        if let Some(repl) = sorted_map_get(mapping, &key)? {
                            out.push_back(repl);
                        } else {
                            out.push_back(item);
                        }
                    }
                    Ok(Value::Vector(out))
                }
            }
        }
        _ => err("replace expects map and collection"),
    });

    // std placeholder (standard library will be overridden by Clove code)
    def_builtin!(env, "std::prelude-version", FnArity::exact(0), |_args| Ok(
        Value::String("builtin-std-placeholder".into())
    ));
}

fn slurp_to_string(path: &str) -> Result<String, CloveError> {
    if path.starts_with("http://") || path.starts_with("https://") {
        let res = reqwest::blocking::get(path)
            .and_then(|r| r.error_for_status())
            .and_then(|r| r.text())
            .map_err(|e| CloveError::runtime(format!("slurp failed: {}", e)))?;
        Ok(res)
    } else {
        fs::read_to_string(path).map_err(|e| CloveError::runtime(format!("slurp failed: {}", e)))
    }
}

fn render_str_arg(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Symbol(s) => s.trim_start_matches(':').to_string(),
        _ => value.to_string(),
    }
}

fn render_display_arg(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        _ => value.to_string(),
    }
}

#[derive(Clone, Copy)]
struct PpLimits {
    max_depth: usize,
    max_items: usize,
    max_string: usize,
    max_bytes: usize,
}

impl Default for PpLimits {
    fn default() -> Self {
        Self {
            max_depth: 6,
            max_items: 200,
            max_string: 4096,
            max_bytes: 65536,
        }
    }
}

#[derive(Clone, Copy)]
struct PpConfig {
    indent_width: Option<usize>,
    max_inline_chars: Option<usize>,
    limits: PpLimits,
}

impl Default for PpConfig {
    fn default() -> Self {
        Self {
            indent_width: None,
            max_inline_chars: None,
            limits: PpLimits::default(),
        }
    }
}

fn parse_pp_config(
    opts: Option<&Value>,
    op: &str,
    arg_index: usize,
) -> Result<PpConfig, CloveError> {
    let Some(opts_val) = opts else {
        return Ok(PpConfig::default());
    };
    if matches!(opts_val, Value::Nil) {
        return Ok(PpConfig::default());
    }
    let map = map_like_to_hashmap(opts_val, op, arg_index)?;
    let mut config = PpConfig::default();
    if let Some(indent) = map_get_usize(&map, "indent", op)? {
        config.indent_width = Some(indent);
    }
    if let Some(width) = map_get_usize(&map, "width", op)? {
        config.max_inline_chars = Some(width);
    }
    if let Some(max_depth) = map_get_usize(&map, "max-depth", op)? {
        config.limits.max_depth = max_depth;
    }
    if let Some(max_items) = map_get_usize(&map, "max-items", op)? {
        config.limits.max_items = max_items;
    }
    if let Some(max_string) = map_get_usize(&map, "max-string", op)? {
        config.limits.max_string = max_string;
    }
    if let Some(max_bytes) = map_get_usize(&map, "max-bytes", op)? {
        config.limits.max_bytes = max_bytes;
    }
    Ok(config)
}

fn map_get_usize(
    map: &HashMap<Key, Value>,
    key: &str,
    op: &str,
) -> Result<Option<usize>, CloveError> {
    let value = map
        .get(&Key::Keyword(key.to_string()))
        .or_else(|| map.get(&Key::String(key.to_string())))
        .or_else(|| map.get(&Key::Symbol(key.to_string())));
    let Some(value) = value else {
        return Ok(None);
    };
    match value {
        Value::Int(n) if *n >= 0 => usize::try_from(*n)
            .map(Some)
            .map_err(|_| CloveError::runtime(format!("{} option {} is too large", op, key))),
        _ => Err(CloveError::runtime(format!(
            "{} option {} expects a non-negative int",
            op, key
        ))),
    }
}

fn pp_str_value(value: &Value, config: &PpConfig) -> Result<String, CloveError> {
    let source = render_value_limited(value, &config.limits, 0);
    let pretty_opts = resolve_pretty_options(config.indent_width, config.max_inline_chars)?;
    let formatted = pretty_print_source(&source, pretty_opts).unwrap_or(source);
    Ok(trim_trailing_newlines(&formatted))
}

fn resolve_pretty_options(
    indent_width: Option<usize>,
    max_inline_chars: Option<usize>,
) -> Result<PrettyOptions, CloveError> {
    let mut options = FormatOptions::default();
    let cwd = std::env::current_dir()
        .map_err(|e| CloveError::runtime(format!("failed to get cwd: {}", e)))?;
    if let Some(path) = find_fmt_config(&cwd) {
        let loaded = load_fmt_config_path(&path).map_err(|e| CloveError::runtime(e.to_string()))?;
        loaded
            .config
            .apply(&mut options)
            .map_err(|e| CloveError::runtime(e.to_string()))?;
    }
    if let Some(indent) = indent_width {
        options.indent_width = indent.max(1);
    }
    if let Some(width) = max_inline_chars {
        options.max_inline_chars = width.max(40);
    }
    Ok(options.as_pretty_options())
}

fn find_fmt_config(start_dir: &Path) -> Option<PathBuf> {
    for dir in start_dir.ancestors() {
        let primary = dir.join("clovefmt.toml");
        if primary.is_file() {
            return Some(primary);
        }
        let hidden = dir.join(".clovefmt.toml");
        if hidden.is_file() {
            return Some(hidden);
        }
    }
    None
}

fn trim_trailing_newlines(text: &str) -> String {
    text.trim_end_matches('\n').to_string()
}

fn render_value_limited(value: &Value, limits: &PpLimits, depth: usize) -> String {
    match value {
        Value::String(s) => render_string_limited(s, limits),
        Value::List(items) => render_list_limited(items, limits, depth),
        Value::Vector(items) => render_vector_limited(items, limits, depth),
        Value::Map(map) => render_map_limited(map, limits, depth),
        Value::SortedMap(map) => render_sorted_map_limited(map, limits, depth),
        Value::Set(set) => render_set_limited(set, limits, depth),
        Value::SortedSet(set) => render_sorted_set_limited(set, limits, depth),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::Atom(_)
        | Value::Chan(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_)
        | Value::Agent(_)
        | Value::Delay(_)
        | Value::Seq(_)
        | Value::Foreign(_)
        | Value::ForeignCallable { .. } => render_non_literal(value, limits),
        _ => value.to_string(),
    }
}

fn render_string_limited(value: &str, limits: &PpLimits) -> String {
    let truncated = truncate_text(value, limits.max_string);
    format!("\"{}\"", escape_string_fragment(&truncated))
}

fn render_non_literal(value: &Value, limits: &PpLimits) -> String {
    let rendered = value.to_string();
    render_string_limited(&rendered, limits)
}

fn truncate_text(value: &str, max_len: usize) -> String {
    let mut iter = value.chars();
    let mut out = String::new();
    for _ in 0..max_len {
        match iter.next() {
            Some(ch) => out.push(ch),
            None => return out,
        }
    }
    if iter.next().is_some() {
        out.push_str("...");
    }
    out
}

fn render_list_limited(items: &Vector<Value>, limits: &PpLimits, depth: usize) -> String {
    render_sequence_limited("(", ")", items, limits, depth, is_byte_sequence(items))
}

fn render_vector_limited(items: &Vector<Value>, limits: &PpLimits, depth: usize) -> String {
    render_sequence_limited("[", "]", items, limits, depth, is_byte_sequence(items))
}

fn render_sequence_limited(
    open: &str,
    close: &str,
    items: &Vector<Value>,
    limits: &PpLimits,
    depth: usize,
    is_bytes: bool,
) -> String {
    if depth >= limits.max_depth {
        return format!("{}...{}", open, close);
    }
    let mut parts = Vec::new();
    let limit = if is_bytes {
        limits.max_items.min(limits.max_bytes)
    } else {
        limits.max_items
    };
    let mut count = 0;
    for item in items {
        if count >= limit {
            break;
        }
        parts.push(render_value_limited(item, limits, depth + 1));
        count += 1;
    }
    if items.len() > count {
        parts.push("...".to_string());
    }
    format!("{}{}{}", open, parts.join(" "), close)
}

fn render_set_limited(set: &HashSet<Value>, limits: &PpLimits, depth: usize) -> String {
    if depth >= limits.max_depth {
        return "#{...}".to_string();
    }
    let limit = limits.max_items;
    let mut parts: Vec<String> = if set.len() <= limit {
        set.iter()
            .map(|item| render_value_limited(item, limits, depth + 1))
            .collect()
    } else {
        let mut out = Vec::new();
        for (idx, item) in set.iter().enumerate() {
            if idx >= limit {
                break;
            }
            out.push(render_value_limited(item, limits, depth + 1));
        }
        out
    };
    parts.sort();
    if set.len() > limit {
        parts.push("...".to_string());
    }
    format!("#{{{}}}", parts.join(" "))
}

fn render_sorted_set_limited(set: &SortedSet, limits: &PpLimits, depth: usize) -> String {
    if depth >= limits.max_depth {
        return "#{...}".to_string();
    }
    let mut parts = Vec::new();
    let limit = limits.max_items;
    for (idx, item) in set.entries.iter().enumerate() {
        if idx >= limit {
            break;
        }
        parts.push(render_value_limited(item, limits, depth + 1));
    }
    if set.entries.len() > limit {
        parts.push("...".to_string());
    }
    format!("#{{{}}}", parts.join(" "))
}

fn render_map_limited(map: &HashMap<Key, Value>, limits: &PpLimits, depth: usize) -> String {
    if depth >= limits.max_depth {
        return "{:__more__ true}".to_string();
    }
    let mut parts = Vec::new();
    let limit = limits.max_items;
    for (idx, (key, value)) in map.iter().enumerate() {
        if idx >= limit {
            break;
        }
        parts.push(format!(
            "{} {}",
            format_key(key),
            render_value_limited(value, limits, depth + 1)
        ));
    }
    if map.len() > limit {
        parts.push(":__more__ true".to_string());
    }
    format!("{{{}}}", parts.join(" "))
}

fn render_sorted_map_limited(map: &SortedMap, limits: &PpLimits, depth: usize) -> String {
    if depth >= limits.max_depth {
        return "{:__more__ true}".to_string();
    }
    let mut parts = Vec::new();
    let limit = limits.max_items;
    for (idx, (key, value)) in map.entries.iter().enumerate() {
        if idx >= limit {
            break;
        }
        parts.push(format!(
            "{} {}",
            format_key(key),
            render_value_limited(value, limits, depth + 1)
        ));
    }
    if map.entries.len() > limit {
        parts.push(":__more__ true".to_string());
    }
    format!("{{{}}}", parts.join(" "))
}

fn format_key(k: &Key) -> String {
    match k {
        Key::Keyword(s) => format!(":{}", s),
        Key::Symbol(s) => format!(":{}", s),
        Key::String(s) => format!("\"{}\"", escape_string_fragment(s)),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

fn is_byte_sequence(items: &Vector<Value>) -> bool {
    items
        .iter()
        .all(|item| matches!(item, Value::Int(n) if (0..=255).contains(n)))
}

fn format_template(tmpl: &str, args: &[Value]) -> Result<String, CloveError> {
    let mut out = String::new();
    let chars: Vec<char> = tmpl.chars().collect();
    let mut idx = 0;
    let mut arg_idx = 0;
    while idx < chars.len() {
        let ch = chars[idx];
        if ch == '%' {
            if idx + 1 >= chars.len() {
                return err("unterminated format specifier");
            }
            let spec = chars[idx + 1];
            match spec {
                '%' => {
                    out.push('%');
                    idx += 2;
                    continue;
                }
                's' | 'd' => {
                    let val = args
                        .get(arg_idx)
                        .ok_or_else(|| CloveError::runtime("format missing arguments"))?;
                    let rendered = if spec == 's' {
                        render_str_arg(val)
                    } else {
                        val.to_string()
                    };
                    out.push_str(&rendered);
                    arg_idx += 1;
                    idx += 2;
                    continue;
                }
                _ => return err("unknown format specifier"),
            }
        } else if ch == '{' {
            if idx + 1 < chars.len() && chars[idx + 1] == '{' {
                out.push('{');
                idx += 2;
                continue;
            }
            let debug = if idx + 3 < chars.len()
                && chars[idx + 1] == ':'
                && chars[idx + 2] == '?'
                && chars[idx + 3] == '}'
            {
                idx += 4;
                true
            } else if idx + 1 < chars.len() && chars[idx + 1] == '}' {
                idx += 2;
                false
            } else {
                return err("format placeholder must be {} or {:?}");
            };
            let val = args
                .get(arg_idx)
                .ok_or_else(|| CloveError::runtime("format missing arguments"))?;
            out.push_str(&format_value(val, debug));
            arg_idx += 1;
            continue;
        } else if ch == '}' {
            if idx + 1 < chars.len() && chars[idx + 1] == '}' {
                out.push('}');
                idx += 2;
                continue;
            }
            return err("unmatched '}' in format string");
        } else {
            out.push(ch);
            idx += 1;
        }
    }
    if arg_idx < args.len() {
        return Err(CloveError::runtime("format received too many arguments"));
    }
    Ok(out)
}

pub(crate) fn meta_lookup(value: &Value) -> Value {
    if let Some(stored) = META_STORE.lock().unwrap().get(value).cloned() {
        return stored;
    }
    match value {
        Value::Lambda { meta: Some(m), .. } | Value::MultiLambda { meta: Some(m), .. } => {
            Value::Map(m.clone())
        }
        _ => Value::Nil,
    }
}

pub(crate) fn meta_set(value: Value, meta: Value) {
    let mut guard = META_STORE.lock().unwrap();
    guard.insert(value, meta);
}

pub(crate) fn doc_lookup(value: &Value) -> Option<String> {
    DOC_STORE.lock().unwrap().get(value).cloned()
}

pub(crate) fn doc_set(value: Value, doc: String) {
    let mut guard = DOC_STORE.lock().unwrap();
    guard.insert(value, doc);
}

fn doc_from_value(value: &Value) -> Option<String> {
    if let Some(doc) = doc_lookup(value) {
        return normalize_doc(Some(doc));
    }
    match value {
        Value::Lambda { doc, .. } | Value::MultiLambda { doc, .. } => normalize_doc(doc.clone()),
        _ => None,
    }
}

fn doc_from_name(name: &str) -> Option<String> {
    if let Some(meta) = symbol_meta::get(name) {
        if let Some(doc) = normalize_doc(meta.doc) {
            return Some(doc);
        }
    }
    if let Some(meta) = fn_meta::get(name) {
        if let Some(doc) = normalize_doc(meta.doc) {
            return Some(doc);
        }
    }
    if let Some(entry) = type_registry::get_type_entry(name) {
        let doc = match entry {
            type_registry::TypeEntry::Primitive(meta) => meta.doc,
            type_registry::TypeEntry::Product(meta) => meta.doc,
            type_registry::TypeEntry::Sum(meta) => meta.doc,
            type_registry::TypeEntry::Alias(meta) => meta.doc,
        };
        if let Some(doc) = normalize_doc(doc) {
            return Some(doc);
        }
    }
    docs::find_doc_entry(name).and_then(|entry| docs::format_doc_entry(entry))
}

fn source_from_name(name: &str) -> Option<String> {
    symbol_meta::get(name).and_then(|meta| meta.source)
}

fn source_from_value(value: &Value) -> Option<String> {
    match value {
        Value::Lambda {
            params, rest, body, ..
        } => Some(format!(
            "(fn {} {})",
            format_arglist(params, rest.as_ref()),
            format_body_source(body)
        )),
        Value::MultiLambda { clauses, .. } => {
            let parts: Vec<String> = clauses
                .iter()
                .map(|clause| {
                    format!(
                        "({} {})",
                        format_arglist(&clause.params, clause.rest.as_ref()),
                        format_body_source(&clause.body)
                    )
                })
                .collect();
            Some(format!("(fn {})", parts.join(" ")))
        }
        _ => None,
    }
}

fn format_arglist(params: &[String], rest: Option<&String>) -> String {
    let mut parts = Vec::new();
    parts.extend(params.iter().cloned());
    if let Some(rest_name) = rest {
        parts.push("&".into());
        parts.push(rest_name.clone());
    }
    format!("[{}]", parts.join(" "))
}

fn format_body_source(body: &[crate::ast::Form]) -> String {
    if body.is_empty() {
        "nil".into()
    } else {
        body.iter()
            .map(form_source::form_to_source)
            .collect::<Vec<_>>()
            .join(" ")
    }
}

fn normalize_doc(doc: Option<String>) -> Option<String> {
    doc.and_then(|text| {
        if text.trim().is_empty() {
            None
        } else {
            Some(text)
        }
    })
}

fn format_value(value: &Value, debug: bool) -> String {
    if debug {
        value.to_string()
    } else {
        match value {
            Value::String(s) => s.clone(),
            Value::Symbol(s) => s.clone(),
            other => other.to_string(),
        }
    }
}

fn measurement_result(last_value: Value, runs: u64, elapsed: Duration) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    let elapsed_ms = elapsed.as_secs_f64() * 1_000.0;
    map.insert(Key::Keyword("elapsed-ms".into()), Value::Float(elapsed_ms));
    map.insert(Key::Keyword("result".into()), last_value);
    map.insert(
        Key::Keyword("runs".into()),
        Value::Int(runs.min(i64::MAX as u64) as i64),
    );
    if runs > 0 {
        let avg_ms = elapsed_ms / runs as f64;
        map.insert(Key::Keyword("avg-ms".into()), Value::Float(avg_ms));
    }
    map
}

fn compare_values(left: &Value, right: &Value) -> Result<Ordering, CloveError> {
    use Value::*;
    match (left, right) {
        (Nil, Nil) => Ok(Ordering::Equal),
        (Nil, _) => Ok(Ordering::Less),
        (_, Nil) => Ok(Ordering::Greater),
        (Int(a), Int(b)) => Ok(a.cmp(b)),
        (Int(a), Float(b)) => compare_floats(*a as f64, *b),
        (Float(a), Int(b)) => compare_floats(*a, *b as f64),
        (Float(a), Float(b)) => compare_floats(*a, *b),
        (String(a), String(b)) => Ok(a.cmp(b)),
        (Bool(a), Bool(b)) => Ok(a.cmp(b)),
        (Symbol(a), Symbol(b)) => Ok(a.cmp(b)),
        _ => Err(CloveError::runtime(format!(
            "compare not supported for {} and {}",
            left.type_name(),
            right.type_name()
        ))),
    }
}

fn compare_floats(a: f64, b: f64) -> Result<Ordering, CloveError> {
    a.partial_cmp(&b)
        .ok_or_else(|| CloveError::runtime("cannot compare NaN values"))
}

fn alias(env: &mut Env, new: &str, existing: &str) {
    if let Some(value) = env.get(existing) {
        env.define_builtin(new, value);
    }
}

fn normalize_type_name(name: &str) -> String {
    let name = name.strip_prefix(':').unwrap_or(name);
    if name == "String" {
        return "Str".into();
    }
    if let Some(prefix) = name.strip_suffix("::String") {
        return format!("{}::Str", prefix);
    }
    if let Some(stripped) = name.strip_prefix("clove::") {
        return stripped.to_string();
    }
    name.to_string()
}

fn normalize_type_name_from_value(value: &Value) -> Result<String, CloveError> {
    match value {
        Value::Symbol(s) | Value::String(s) => Ok(normalize_type_name(s)),
        other => Err(CloveError::type_mismatch(
            "string or symbol",
            other.type_name(),
        )),
    }
}

fn build_sorted_map(
    comparator: Value,
    args: &[Value],
    op: &str,
    arg_offset: usize,
) -> Result<SortedMap, CloveError> {
    if args.is_empty() {
        return Ok(SortedMap {
            comparator: Box::new(comparator),
            entries: Vec::new(),
        });
    }
    if args.len() == 1 {
        match &args[0] {
            Value::Map(map) => return sorted_map_from_map(comparator, map),
            Value::SortedMap(map) => return sorted_map_from_sorted_map(comparator, map),
            Value::List(_)
            | Value::Vector(_)
            | Value::Set(_)
            | Value::SortedSet(_)
            | Value::Seq(_) => {
                let entries = seq_items(&args[0])?;
                let entries_vec: Vec<Value> = entries.iter().cloned().collect();
                return sorted_map_from_entries(comparator, &entries_vec, op);
            }
            _ => {}
        }
    }
    let entry_list = args
        .iter()
        .all(|item| matches!(item, Value::List(_) | Value::Vector(_)));
    if entry_list {
        return sorted_map_from_entries(comparator, args, op);
    }
    sorted_map_from_pairs(comparator, args, op, arg_offset)
}

fn build_sorted_set(comparator: Value, args: &[Value]) -> Result<SortedSet, CloveError> {
    if args.is_empty() {
        return sorted_set_from_items(comparator, &[]);
    }
    if args.len() == 1 {
        match &args[0] {
            Value::List(_)
            | Value::Vector(_)
            | Value::Set(_)
            | Value::SortedSet(_)
            | Value::Seq(_) => {
                let items = seq_items(&args[0])?;
                let items_vec: Vec<Value> = items.iter().cloned().collect();
                return sorted_set_from_items(comparator, &items_vec);
            }
            _ => {}
        }
    }
    sorted_set_from_items(comparator, args)
}

fn register_core_metas() {
    fn symbol_type() -> TypeKind {
        TypeKind::Named("clove::core::Symbol".into())
    }
    fn function_type() -> TypeKind {
        TypeKind::Named("clove::core::Function".into())
    }
    fn list_type() -> TypeKind {
        TypeKind::Named("clove::core::List".into())
    }
    fn set_type() -> TypeKind {
        TypeKind::Named("clove::core::Set".into())
    }
    fn vec_any() -> TypeKind {
        TypeKind::vector(TypeKind::Any)
    }
    fn map_any() -> TypeKind {
        TypeKind::map(TypeKind::Any, TypeKind::Any)
    }

    let mut println_meta = FnMeta::new("core", "println");
    println_meta.arglist.push("[& values]".into());
    println_meta.doc = Some("Print values with spaces and newline.".into());
    println_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    println_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(println_meta);

    let mut print_meta = FnMeta::new("core", "print");
    print_meta.arglist.push("[& values]".into());
    print_meta.doc = Some("Print values without automatic newline.".into());
    print_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    print_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(print_meta);

    let mut pr_str_meta = FnMeta::new("core", "pr-str");
    pr_str_meta.arglist.push("[& values]".into());
    pr_str_meta.doc = Some("Return the printed representation of all values as a string.".into());
    pr_str_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Str,
        special_op: None,
    });
    pr_str_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pr_str_meta);

    let mut prn_meta = FnMeta::new("core", "prn");
    prn_meta.arglist.push("[& values]".into());
    prn_meta.doc = Some("Print the representations of values followed by newline.".into());
    prn_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    prn_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(prn_meta);

    let mut pp_str_meta = FnMeta::new("core", "pp-str");
    pp_str_meta.arglist.push("[x]".into());
    pp_str_meta.arglist.push("[x opts]".into());
    pp_str_meta.doc = Some(
        "Return the pretty-printed readable representation of x as a string. opts can set :indent/:width and limits.".into(),
    );
    pp_str_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    pp_str_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    pp_str_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pp_str_meta);

    let mut pp_meta = FnMeta::new("core", "pp");
    pp_meta.arglist.push("[x]".into());
    pp_meta.arglist.push("[x opts]".into());
    pp_meta.doc = Some("Pretty-print x to *out* and return x. opts are the same as pp-str.".into());
    pp_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    pp_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    pp_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pp_meta);

    let mut pprint_str_meta = FnMeta::new("core", "pprint-str");
    pprint_str_meta.arglist.push("[x]".into());
    pprint_str_meta.arglist.push("[x opts]".into());
    pprint_str_meta.doc = Some("Alias of pp-str.".into());
    pprint_str_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    pprint_str_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    pprint_str_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pprint_str_meta);

    let mut pprint_meta = FnMeta::new("core", "pprint");
    pprint_meta.arglist.push("[x]".into());
    pprint_meta.arglist.push("[x opts]".into());
    pprint_meta.doc = Some("Alias of pp.".into());
    pprint_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    pprint_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    pprint_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pprint_meta);

    let mut flush_meta = FnMeta::new("core", "flush");
    flush_meta.arglist.push("[]".into());
    flush_meta.doc = Some("Flush the output stream bound to *out*.".into());
    flush_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    flush_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(flush_meta);

    let mut pvalues_meta = FnMeta::new("core", "pvalues");
    pvalues_meta.arglist.push("[& values]".into());
    pvalues_meta.doc = Some("Return values collected into a vector.".into());
    pvalues_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: vec_any(),
        special_op: None,
    });
    pvalues_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pvalues_meta);

    let mut str_meta = FnMeta::new("core", "str");
    str_meta.arglist.push("[& values]".into());
    str_meta.doc = Some("Concatenate values into a single string.".into());
    str_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Str,
        special_op: None,
    });
    str_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(str_meta);

    let mut read_string_meta = FnMeta::new("core", "read-string");
    read_string_meta.arglist.push("[source]".into());
    read_string_meta.doc = Some("Read a single form from the given string.".into());
    read_string_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    read_string_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(read_string_meta);

    let mut read_meta = FnMeta::new("core", "read");
    read_meta.arglist.push("[]".into());
    read_meta.arglist.push("[source]".into());
    read_meta.doc = Some("Read one form from STDIN or from the provided string.".into());
    read_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    read_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    read_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(read_meta);

    let mut slurp_meta = FnMeta::new("core", "slurp");
    slurp_meta.arglist.push("[path]".into());
    slurp_meta.doc = Some("Read the entire file contents as a UTF-8 string.".into());
    slurp_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    slurp_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(slurp_meta);

    let mut spit_meta = FnMeta::new("core", "spit");
    spit_meta.arglist.push("[path content]".into());
    spit_meta.doc = Some("Write the given string content to the file path. Returns path.".into());
    spit_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str, TypeKind::Str],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    spit_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(spit_meta);

    let mut type_meta = FnMeta::new("core", "type");
    type_meta.arglist.push("[value]".into());
    type_meta.doc = Some("Return the symbolic type name of the value.".into());
    type_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    type_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(type_meta);

    let mut instance_meta = FnMeta::new("core", "instance?");
    instance_meta.arglist.push("[type value]".into());
    instance_meta.doc = Some(
        "Return true when value conforms to the given type name, validating required fields for deftype values.".into(),
    );
    instance_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    instance_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(instance_meta);

    let mut tagged_meta = FnMeta::new("core", "tagged?");
    tagged_meta.arglist.push("[type value]".into());
    tagged_meta.doc = Some(
        "Return true when value carries a matching :type tag (does not validate required fields)."
            .into(),
    );
    tagged_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    tagged_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(tagged_meta);

    let mut symbol_meta = FnMeta::new("core", "symbol");
    symbol_meta.arglist.push("[name]".into());
    symbol_meta.doc = Some("Construct a canonical symbol from a string or symbol.".into());
    symbol_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    symbol_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type()],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    symbol_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(symbol_meta);

    let mut keyword_meta = FnMeta::new("core", "keyword");
    keyword_meta.arglist.push("[name]".into());
    keyword_meta.arglist.push("[ns name]".into());
    keyword_meta.doc = Some("Create a keyword (interned symbol prefixed with ':').".into());
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type()],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str, TypeKind::Str],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str, symbol_type()],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type(), TypeKind::Str],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type(), symbol_type()],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    keyword_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(keyword_meta);

    let mut name_meta = FnMeta::new("core", "name");
    name_meta.arglist.push("[value]".into());
    name_meta.doc = Some("Return the name part of a symbol/keyword or the string itself.".into());
    name_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type()],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    name_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    name_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(name_meta);

    let mut gensym_meta = FnMeta::new("core", "gensym");
    gensym_meta.arglist.push("[]".into());
    gensym_meta.arglist.push("[prefix]".into());
    gensym_meta.doc =
        Some("Generate a unique symbol, optionally prefixed by a string or symbol.".into());
    gensym_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    gensym_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    gensym_meta.overloads.push(FnOverload {
        arg_types: vec![symbol_type()],
        rest: None,
        ret_type: symbol_type(),
        special_op: None,
    });
    gensym_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(gensym_meta);

    let mut meta_meta = FnMeta::new("core", "meta");
    meta_meta.arglist.push("[value]".into());
    meta_meta.doc = Some("Return metadata map attached to value (or nil).".into());
    meta_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    meta_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta_meta);

    let mut doc_meta = FnMeta::new("core", "doc");
    doc_meta.arglist.push("[target]".into());
    doc_meta.doc = Some("Return docstring for symbol or value (or nil).".into());
    doc_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    doc_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(doc_meta);

    let mut source_meta = FnMeta::new("core", "source");
    source_meta.arglist.push("[target]".into());
    source_meta.doc = Some("Return source string for symbol or value (or nil).".into());
    source_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    source_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(source_meta);

    let mut with_meta_meta = FnMeta::new("core", "with-meta");
    with_meta_meta.arglist.push("[value meta]".into());
    with_meta_meta.doc = Some("Attach metadata map to value and return value.".into());
    with_meta_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, map_any()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    with_meta_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(with_meta_meta);

    let mut vary_meta_meta = FnMeta::new("core", "vary-meta");
    vary_meta_meta.arglist.push("[value f & args]".into());
    vary_meta_meta.doc = Some("Update value metadata by applying function to current meta.".into());
    vary_meta_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, function_type()],
        rest: Some(vec_any()),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    vary_meta_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(vary_meta_meta);

    let mut subs_meta = FnMeta::new("core", "subs");
    subs_meta.arglist.push("[s start]".into());
    subs_meta.arglist.push("[s start end]".into());
    subs_meta.doc =
        Some("Return substring of s between start (inclusive) and optional end.".into());
    subs_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    subs_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str, TypeKind::Int, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    subs_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(subs_meta);

    let mut list_meta = FnMeta::new("core", "list");
    list_meta.arglist.push("[& values]".into());
    list_meta.doc = Some("Construct a list (linked persistent vector) from the values.".into());
    list_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: list_type(),
        special_op: None,
    });
    list_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(list_meta);

    let mut vector_meta = FnMeta::new("core", "vector");
    vector_meta.arglist.push("[& values]".into());
    vector_meta.doc = Some("Construct a persistent vector from the values.".into());
    vector_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: vec_any(),
        special_op: None,
    });
    vector_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(vector_meta);

    let mut hash_map_meta = FnMeta::new("core", "hash-map");
    hash_map_meta.arglist.push("[& keyvals]".into());
    hash_map_meta.doc = Some("Construct a map from key/value arguments.".into());
    hash_map_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: map_any(),
        special_op: None,
    });
    hash_map_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(hash_map_meta);

    let mut sorted_map_meta = FnMeta::new("core", "sorted-map");
    sorted_map_meta.arglist.push("[& keyvals]".into());
    sorted_map_meta.arglist.push("[coll-or-entries]".into());
    sorted_map_meta.doc = Some(
        "Construct a sorted map using core::compare; accepts key/value pairs or a collection of entries."
            .into(),
    );
    sorted_map_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(sorted_map_meta);

    let mut sorted_map_desc_meta = FnMeta::new("core", "sorted-map-desc");
    sorted_map_desc_meta.arglist.push("[& keyvals]".into());
    sorted_map_desc_meta
        .arglist
        .push("[coll-or-entries]".into());
    sorted_map_desc_meta.doc = Some(
        "Construct a sorted map using core::compare-desc; accepts key/value pairs or a collection of entries."
            .into(),
    );
    sorted_map_desc_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_desc_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_desc_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(sorted_map_desc_meta);

    let mut sorted_map_by_meta = FnMeta::new("core", "sorted-map-by");
    sorted_map_by_meta.arglist.push("[cmp & keyvals]".into());
    sorted_map_by_meta
        .arglist
        .push("[cmp coll-or-entries]".into());
    sorted_map_by_meta.doc = Some(
        "Construct a sorted map using comparator cmp; accepts key/value pairs or a collection of entries."
            .into(),
    );
    sorted_map_by_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: Some(vec_any()),
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_by_meta.overloads.push(FnOverload {
        arg_types: vec![function_type(), TypeKind::Any],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    sorted_map_by_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(sorted_map_by_meta);

    let mut hash_set_meta = FnMeta::new("core", "hash-set");
    hash_set_meta.arglist.push("[& values]".into());
    hash_set_meta.doc = Some("Construct a persistent set from the values.".into());
    hash_set_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: set_type(),
        special_op: None,
    });
    hash_set_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(hash_set_meta.clone());

    let mut set_meta = FnMeta::new("core", "set");
    set_meta.arglist = hash_set_meta.arglist.clone();
    set_meta.doc = hash_set_meta.doc.clone();
    set_meta.overloads = hash_set_meta.overloads.clone();
    set_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(set_meta);

    let mut sorted_set_meta = FnMeta::new("core", "sorted-set");
    sorted_set_meta.arglist.push("[& values]".into());
    sorted_set_meta.arglist.push("[coll]".into());
    sorted_set_meta.doc =
        Some("Construct a sorted set using core::compare; accepts values or a collection.".into());
    sorted_set_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(sorted_set_meta);

    let mut sorted_set_desc_meta = FnMeta::new("core", "sorted-set-desc");
    sorted_set_desc_meta.arglist.push("[& values]".into());
    sorted_set_desc_meta.arglist.push("[coll]".into());
    sorted_set_desc_meta.doc = Some(
        "Construct a sorted set using core::compare-desc; accepts values or a collection.".into(),
    );
    sorted_set_desc_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_desc_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_desc_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(sorted_set_desc_meta);

    let mut sorted_set_by_meta = FnMeta::new("core", "sorted-set-by");
    sorted_set_by_meta.arglist.push("[cmp & values]".into());
    sorted_set_by_meta.arglist.push("[cmp coll]".into());
    sorted_set_by_meta.doc =
        Some("Construct a sorted set using comparator cmp; accepts values or a collection.".into());
    sorted_set_by_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: Some(vec_any()),
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_by_meta.overloads.push(FnOverload {
        arg_types: vec![function_type(), TypeKind::Any],
        rest: None,
        ret_type: set_type(),
        special_op: None,
    });
    sorted_set_by_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(sorted_set_by_meta);

    let mut find_meta = FnMeta::new("core", "find");
    find_meta.arglist.push("[map key]".into());
    find_meta.doc = Some("Return map entry (key/value vector) for key or nil.".into());
    find_meta.overloads.push(FnOverload {
        arg_types: vec![map_any(), TypeKind::Any],
        rest: None,
        ret_type: vec_any(),
        special_op: None,
    });
    find_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(find_meta);

    let mut val_meta = FnMeta::new("core", "val");
    val_meta.arglist.push("[entry]".into());
    val_meta.doc = Some("Return value from a key/value entry vector or list.".into());
    val_meta.overloads.push(FnOverload {
        arg_types: vec![vec_any()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    val_meta.overloads.push(FnOverload {
        arg_types: vec![list_type()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    val_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(val_meta);

    let mut key_meta = FnMeta::new("core", "key");
    key_meta.arglist.push("[entry]".into());
    key_meta.doc = Some("Return key from a key/value entry vector or list.".into());
    key_meta.overloads.push(FnOverload {
        arg_types: vec![vec_any()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    key_meta.overloads.push(FnOverload {
        arg_types: vec![list_type()],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    key_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(key_meta);

    let mut hash_meta = FnMeta::new("core", "hash");
    hash_meta.arglist.push("[value]".into());
    hash_meta.doc = Some("Return hash of the value.".into());
    hash_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    hash_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(hash_meta);

    let mut describe_meta = FnMeta::new("core", "describe");
    describe_meta.arglist.push("[value]".into());
    describe_meta.doc = Some("Return detailed type info map for the value.".into());
    describe_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    describe_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(describe_meta.clone());

    let mut fn_type_meta = FnMeta::new("core", "fn-type");
    fn_type_meta.arglist.push("[f]".into());
    fn_type_meta.doc = Some("Return inferred signature for a function value.".into());
    fn_type_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: None,
        ret_type: TypeKind::Str,
        special_op: None,
    });
    fn_type_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(fn_type_meta);

    let mut enum_members_meta = FnMeta::new("core", "enum-members");
    enum_members_meta.arglist.push("[enum-val]".into());
    enum_members_meta.doc = Some("Return members list for enum description maps.".into());
    enum_members_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    enum_members_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(enum_members_meta);

    let mut all_types_meta = FnMeta::new("core", "all-types");
    all_types_meta.arglist.push("[]".into());
    all_types_meta.doc = Some("Return set of all registered type names.".into());
    all_types_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: set_type(),
        special_op: None,
    });
    all_types_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(all_types_meta);

    let mut time_meta = FnMeta::new("core", "time");
    time_meta.arglist.push("[callable]".into());
    time_meta.doc = Some("Invoke callable, returning map with elapsed time and result.".into());
    time_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    time_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(time_meta);

    let mut bench_meta = FnMeta::new("core", "bench");
    bench_meta.arglist.push("[n callable]".into());
    bench_meta.doc = Some("Invoke callable n times and return timing statistics map.".into());
    bench_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, function_type()],
        rest: None,
        ret_type: map_any(),
        special_op: None,
    });
    bench_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(bench_meta);

    let mut runtime_error_meta = FnMeta::new("core", "runtime-error");
    runtime_error_meta.arglist.push("[& messages]".into());
    runtime_error_meta.doc = Some("Raise a runtime error built from the provided messages.".into());
    runtime_error_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(vec_any()),
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    runtime_error_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(runtime_error_meta);

    let mut compare_meta = FnMeta::new("core", "compare");
    compare_meta.arglist.push("[x y]".into());
    compare_meta.doc = Some("Compare two values returning -1,0,1 ordering.".into());
    compare_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    compare_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(compare_meta);

    let mut compare_desc_meta = FnMeta::new("core", "compare-desc");
    compare_desc_meta.arglist.push("[x y]".into());
    compare_desc_meta.doc = Some("Compare two values in descending order.".into());
    compare_desc_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    compare_desc_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(compare_desc_meta);

    let mut min_key_meta = FnMeta::new("core", "min-key");
    min_key_meta.arglist.push("[key-fn value & more]".into());
    min_key_meta.doc = Some("Return value whose key-fn result is minimal.".into());
    min_key_meta.overloads.push(FnOverload {
        arg_types: vec![function_type(), TypeKind::Any],
        rest: Some(vec_any()),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    min_key_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(min_key_meta);

    let mut max_key_meta = FnMeta::new("core", "max-key");
    max_key_meta.arglist.push("[key-fn value & more]".into());
    max_key_meta.doc = Some("Return value whose key-fn result is maximal.".into());
    max_key_meta.overloads.push(FnOverload {
        arg_types: vec![function_type(), TypeKind::Any],
        rest: Some(vec_any()),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    max_key_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(max_key_meta);

    let mut rand_meta = FnMeta::new("core", "rand");
    rand_meta.arglist.push("[]".into());
    rand_meta.arglist.push("[upper]".into());
    rand_meta.doc = Some("Return random float in [0,1) or scaled by upper.".into());
    rand_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    rand_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    rand_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(rand_meta);

    let mut shuffle_meta = FnMeta::new("core", "shuffle");
    shuffle_meta.arglist.push("[coll]".into());
    shuffle_meta.doc =
        Some("Return a new vector with collection elements randomly permuted.".into());
    shuffle_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: vec_any(),
        special_op: None,
    });
    shuffle_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(shuffle_meta);

    let mut format_meta = FnMeta::new("core", "format");
    format_meta.arglist.push("[template & args]".into());
    format_meta.doc = Some("Return formatted string from template and arguments.".into());
    format_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: Some(vec_any()),
        ret_type: TypeKind::Str,
        special_op: None,
    });
    format_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(format_meta);

    let mut printf_meta = FnMeta::new("core", "printf");
    printf_meta.arglist.push("[template & args]".into());
    printf_meta.doc = Some("Print formatted text using template and arguments.".into());
    printf_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Str],
        rest: Some(vec_any()),
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    printf_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(printf_meta);

    let mut replace_meta = FnMeta::new("core", "replace");
    replace_meta.arglist.push("[mapping coll]".into());
    replace_meta.doc = Some("Replace keys in mapping with mapped values within collection.".into());
    replace_meta.overloads.push(FnOverload {
        arg_types: vec![map_any(), TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    replace_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(replace_meta);

    let mut oop_seg_meta = FnMeta::new("core", "oop-seg");
    oop_seg_meta.arglist.push("[target name]".into());
    oop_seg_meta.doc = Some(
        "Resolve a dot-segment fallback against map/set keys (:name, \"name\", 'name), erroring when missing.".into(),
    );
    oop_seg_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    oop_seg_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(oop_seg_meta);
}
