use crate::ast::{FnArity, HashMap, Key, Value, Vector};
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use std::collections::HashMap as StdHashMap;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "cli::argv", FnArity::exact(0), |_args| {
        Ok(Value::Vector(std::env::args().map(Value::String).collect()))
    });
    def_builtin!(env, "cli::env", FnArity::exact(0), |_args| {
        let mut map = HashMap::new();
        for (k, v) in std::env::vars() {
            map.insert(Key::String(k), Value::String(v));
        }
        Ok(Value::Map(map))
    });
    def_builtin!(
        env,
        "cli::env-get",
        FnArity::range(1, 2),
        |args| match args {
            [Value::String(k)] => Ok(std::env::var(k).map(Value::String).unwrap_or(Value::Nil)),
            [Value::String(k), default] => Ok(std::env::var(k)
                .map(Value::String)
                .unwrap_or(default.clone())),
            _ => err("cli::env-get expects key string and optional default"),
        }
    );
    def_builtin!(env, "cli::exit", FnArity::range(0, 1), |args| {
        let code = match args {
            [] => 0,
            [Value::Int(n)] => *n as i32,
            [Value::Float(f)] => *f as i32,
            _ => return err("cli::exit expects optional int code"),
        };
        std::process::exit(code);
    });
    def_builtin!(
        env,
        "cli::parse-opts",
        FnArity::range(2, 2),
        |args| match args {
            [argv, spec] => parse_opts(argv, spec),
            _ => err("cli::parse-opts expects argv vector and spec vector"),
        }
    );
    def_builtin!(
        env,
        "cli::parse-opts-adv",
        FnArity::range(2, 2),
        |args| match args {
            [argv, spec] => parse_opts_adv(argv, spec),
            _ => err("cli::parse-opts-adv expects argv vector and spec vector"),
        }
    );
}

#[derive(Default)]
struct OptSpec {
    id: String,
    short: Option<String>,
    long: Option<String>,
    takes_arg: bool,
    default: Option<Value>,
    required: bool,
}

fn parse_opts(argv_val: &Value, spec_val: &Value) -> Result<Value, CloveError> {
    let argv = match argv_val {
        Value::Vector(v) | Value::List(v) => v.clone(),
        _ => return err("argv must be vector/list of strings"),
    };
    let specs_raw = match spec_val {
        Value::Vector(v) | Value::List(v) => v,
        _ => return err("spec must be vector/list"),
    };
    let mut specs = Vec::new();
    for s in specs_raw {
        let mut spec = OptSpec::default();
        if let Value::Map(m) = s {
            if let Some(v) = m.get(&Key::Keyword("id".into())) {
                if let Value::Symbol(id) | Value::String(id) = v {
                    spec.id = id.trim_start_matches(':').to_string();
                }
            }
            if let Some(Value::String(short)) = m.get(&Key::Keyword("short".into())) {
                spec.short = Some(short.clone());
            }
            if let Some(Value::String(long)) = m.get(&Key::Keyword("long".into())) {
                spec.long = Some(long.clone());
            }
            if let Some(Value::Bool(b)) = m.get(&Key::Keyword("arg?".into())) {
                spec.takes_arg = *b;
            }
            if let Some(default) = m.get(&Key::Keyword("default".into())) {
                spec.default = Some(default.clone());
            }
            if let Some(Value::Bool(b)) = m.get(&Key::Keyword("required?".into())) {
                spec.required = *b;
            }
        }
        if spec.id.is_empty() {
            return err("each spec needs :id keyword");
        }
        specs.push(spec);
    }

    let mut opts = HashMap::new();
    let mut errors = Vec::new();
    let mut positionals = Vec::new();

    // set defaults
    for s in &specs {
        if let Some(d) = &s.default {
            opts.insert(Key::Keyword(s.id.clone()), d.clone());
        }
    }

    let mut iter = argv.into_iter().peekable();
    while let Some(arg) = iter.next() {
        let s = match arg {
            Value::String(ref s) => s.clone(),
            Value::Symbol(ref s) => s.clone(),
            _ => {
                errors.push(format!("invalid arg {:?}", arg));
                continue;
            }
        };
        if !s.starts_with('-') || s == "-" {
            positionals.push(Value::String(s));
            continue;
        }
        // support --key=value / -k=value
        let (flag, attached) = if let Some(idx) = s.find('=') {
            (s[..idx].to_string(), Some(s[idx + 1..].to_string()))
        } else {
            (s.clone(), None)
        };
        let matched = specs.iter().find(|spec| {
            spec.short.as_deref() == Some(flag.as_str())
                || spec.long.as_deref() == Some(flag.as_str())
        });
        if let Some(spec) = matched {
            if spec.takes_arg {
                if let Some(val) = attached {
                    opts.insert(Key::Keyword(spec.id.clone()), Value::String(val));
                } else if let Some(next) = iter.next() {
                    match next {
                        Value::String(ref ns) | Value::Symbol(ref ns) => {
                            opts.insert(Key::Keyword(spec.id.clone()), Value::String(ns.clone()));
                        }
                        other => {
                            errors.push(format!("option {} needs argument, got {}", flag, other));
                        }
                    }
                } else {
                    errors.push(format!("option {} needs argument", flag));
                }
            } else {
                opts.insert(Key::Keyword(spec.id.clone()), Value::Bool(true));
            }
        } else {
            errors.push(format!("unknown option {}", flag));
        }
    }

    // required? check
    for spec in &specs {
        if spec.required && !opts.contains_key(&Key::Keyword(spec.id.clone())) {
            errors.push(format!("required option missing: {}", spec.id));
        }
    }

    let mut out = HashMap::new();
    out.insert(Key::Keyword("options".into()), Value::Map(opts));
    out.insert(
        Key::Keyword("args".into()),
        Value::Vector(positionals.into()),
    );
    out.insert(
        Key::Keyword("errors".into()),
        Value::Vector(errors.into_iter().map(Value::String).collect()),
    );
    Ok(Value::Map(out))
}

#[derive(Clone, Copy)]
enum CoerceKind {
    Int,
    Float,
    Bool,
    String,
}

#[derive(Default)]
struct AdvOptSpec {
    id: String,
    short: Option<String>,
    long: Option<String>,
    aliases: Vec<String>,
    takes_arg: bool,
    default: Option<Value>,
    required: bool,
    coerce: Option<CoerceKind>,
    collect: bool,
}

fn parse_opts_adv(argv_val: &Value, spec_val: &Value) -> Result<Value, CloveError> {
    let argv = match argv_val {
        Value::Vector(v) | Value::List(v) => v.clone(),
        _ => return err("argv must be vector/list of strings"),
    };
    let specs_raw = match spec_val {
        Value::Vector(v) | Value::List(v) => v,
        _ => return err("spec must be vector/list"),
    };
    let mut specs = Vec::new();
    for s in specs_raw {
        let mut spec = AdvOptSpec::default();
        if let Value::Map(m) = s {
            if let Some(v) = m.get(&Key::Keyword("id".into())) {
                if let Some(id) = parse_id_value(v) {
                    spec.id = id;
                }
            }
            if let Some(v) = m.get(&Key::Keyword("short".into())) {
                spec.short = Some(expect_string_value(v, "short")?);
            }
            if let Some(v) = m.get(&Key::Keyword("long".into())) {
                spec.long = Some(expect_string_value(v, "long")?);
            }
            if let Some(Value::Bool(b)) = m.get(&Key::Keyword("arg?".into())) {
                spec.takes_arg = *b;
            }
            if let Some(default) = m.get(&Key::Keyword("default".into())) {
                spec.default = Some(default.clone());
            }
            if let Some(Value::Bool(b)) = m.get(&Key::Keyword("required?".into())) {
                spec.required = *b;
            }
            if let Some(v) = m.get(&Key::Keyword("alias".into())) {
                spec.aliases = parse_aliases(v)?;
            }
            if let Some(v) = m.get(&Key::Keyword("coerce".into())) {
                spec.coerce = Some(parse_coerce_kind(v)?);
            }
            if let Some(Value::Bool(b)) = m.get(&Key::Keyword("collect?".into())) {
                spec.collect = *b;
            } else if let Some(Value::Bool(b)) = m.get(&Key::Keyword("collect".into())) {
                spec.collect = *b;
            }
        }
        if spec.id.is_empty() {
            return err("each spec needs :id keyword");
        }
        if spec.collect {
            if let Some(default) = &spec.default {
                match default {
                    Value::Vector(_) | Value::List(_) => {}
                    _ => {
                        return err("collect option default must be vector/list");
                    }
                }
            }
        }
        specs.push(spec);
    }

    let mut flag_map = StdHashMap::new();
    for (idx, spec) in specs.iter().enumerate() {
        for flag in spec
            .short
            .iter()
            .chain(spec.long.iter())
            .chain(spec.aliases.iter())
        {
            if flag_map.insert(flag.clone(), idx).is_some() {
                return err(format!("duplicate option flag {}", flag));
            }
        }
    }

    let mut opts = HashMap::new();
    let mut errors = Vec::new();
    let mut positionals = Vec::new();

    // set defaults
    for s in &specs {
        if let Some(d) = &s.default {
            opts.insert(Key::Keyword(s.id.clone()), d.clone());
        }
    }

    let mut iter = argv.into_iter().peekable();
    while let Some(arg) = iter.next() {
        let s = match arg {
            Value::String(ref s) => s.clone(),
            Value::Symbol(ref s) => s.clone(),
            _ => {
                errors.push(format!("invalid arg {:?}", arg));
                continue;
            }
        };
        if s == "--" {
            for rest in iter {
                match rest {
                    Value::String(rs) | Value::Symbol(rs) => {
                        positionals.push(Value::String(rs));
                    }
                    other => {
                        errors.push(format!("invalid arg {:?}", other));
                    }
                }
            }
            break;
        }
        if !s.starts_with('-') || s == "-" {
            positionals.push(Value::String(s));
            continue;
        }

        if let Some(bundle) = try_parse_short_bundle(&s, &specs, &flag_map) {
            match bundle {
                Ok(flags) => {
                    for spec in flags {
                        set_option_value(&mut opts, spec, Value::Bool(true));
                    }
                    continue;
                }
                Err(()) => {}
            }
        }

        // support --key=value / -k=value
        let (flag, attached) = if let Some(idx) = s.find('=') {
            (s[..idx].to_string(), Some(s[idx + 1..].to_string()))
        } else {
            (s.clone(), None)
        };
        let matched = flag_map.get(&flag).and_then(|idx| specs.get(*idx));
        if let Some(spec) = matched {
            if spec.takes_arg {
                let raw = if let Some(val) = attached {
                    Some(val)
                } else if let Some(next) = iter.next() {
                    match next {
                        Value::String(ref ns) | Value::Symbol(ref ns) => Some(ns.clone()),
                        other => {
                            errors.push(format!("option {} needs argument, got {}", flag, other));
                            None
                        }
                    }
                } else {
                    errors.push(format!("option {} needs argument", flag));
                    None
                };
                if let Some(raw) = raw {
                    match coerce_value(spec.coerce, &raw) {
                        Ok(val) => set_option_value(&mut opts, spec, val),
                        Err(msg) => errors.push(format!("option {} {}", flag, msg)),
                    }
                }
            } else {
                set_option_value(&mut opts, spec, Value::Bool(true));
            }
        } else {
            errors.push(format!("unknown option {}", flag));
        }
    }

    // required? check
    for spec in &specs {
        if spec.required && !opts.contains_key(&Key::Keyword(spec.id.clone())) {
            errors.push(format!("required option missing: {}", spec.id));
        }
    }

    let mut out = HashMap::new();
    out.insert(Key::Keyword("options".into()), Value::Map(opts));
    out.insert(
        Key::Keyword("args".into()),
        Value::Vector(positionals.into()),
    );
    out.insert(
        Key::Keyword("errors".into()),
        Value::Vector(errors.into_iter().map(Value::String).collect()),
    );
    Ok(Value::Map(out))
}

fn parse_id_value(value: &Value) -> Option<String> {
    match value {
        Value::Symbol(id) | Value::String(id) => Some(id.trim_start_matches(':').to_string()),
        _ => None,
    }
}

fn expect_string_value(value: &Value, label: &str) -> Result<String, CloveError> {
    match value {
        Value::String(s) | Value::Symbol(s) => Ok(s.clone()),
        _ => err(format!("{} must be string/symbol", label)),
    }
}

fn parse_aliases(value: &Value) -> Result<Vec<String>, CloveError> {
    match value {
        Value::String(s) | Value::Symbol(s) => Ok(vec![s.clone()]),
        Value::Vector(items) | Value::List(items) => {
            let mut out = Vec::new();
            for item in items {
                out.push(expect_string_value(item, "alias")?);
            }
            Ok(out)
        }
        _ => err("alias must be string/symbol or list"),
    }
}

fn parse_coerce_kind(value: &Value) -> Result<CoerceKind, CloveError> {
    let raw = match value {
        Value::String(s) | Value::Symbol(s) => s.trim_start_matches(':').to_string(),
        _ => return err("coerce must be keyword/symbol or string"),
    };
    match raw.as_str() {
        "int" => Ok(CoerceKind::Int),
        "float" => Ok(CoerceKind::Float),
        "bool" => Ok(CoerceKind::Bool),
        "string" => Ok(CoerceKind::String),
        _ => err(format!("unknown coerce kind {}", raw)),
    }
}

fn coerce_value(kind: Option<CoerceKind>, raw: &str) -> Result<Value, String> {
    match kind.unwrap_or(CoerceKind::String) {
        CoerceKind::String => Ok(Value::String(raw.to_string())),
        CoerceKind::Int => raw
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| format!("invalid int value {}", raw)),
        CoerceKind::Float => raw
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| format!("invalid float value {}", raw)),
        CoerceKind::Bool => {
            let lower = raw.to_ascii_lowercase();
            match lower.as_str() {
                "true" | "1" => Ok(Value::Bool(true)),
                "false" | "0" => Ok(Value::Bool(false)),
                _ => Err(format!("invalid bool value {}", raw)),
            }
        }
    }
}

fn set_option_value(opts: &mut HashMap<Key, Value>, spec: &AdvOptSpec, value: Value) {
    let key = Key::Keyword(spec.id.clone());
    if spec.collect {
        let mut current = match opts.get(&key).cloned() {
            Some(Value::Vector(v)) | Some(Value::List(v)) => v,
            Some(other) => Vector::from(vec![other]),
            None => Vector::new(),
        };
        current.push_back(value);
        opts.insert(key, Value::Vector(current));
    } else {
        opts.insert(key, value);
    }
}

fn try_parse_short_bundle<'a>(
    arg: &str,
    specs: &'a [AdvOptSpec],
    flag_map: &StdHashMap<String, usize>,
) -> Option<Result<Vec<&'a AdvOptSpec>, ()>> {
    if !arg.starts_with('-') || arg.starts_with("--") || arg.len() <= 2 || arg.contains('=') {
        return None;
    }
    let mut out = Vec::new();
    for ch in arg.chars().skip(1) {
        let flag = format!("-{}", ch);
        match flag_map.get(&flag).and_then(|idx| specs.get(*idx)) {
            Some(spec) if !spec.takes_arg => out.push(spec),
            _ => return Some(Err(())),
        }
    }
    Some(Ok(out))
}
