use crate::ast::{FnArity, Key, Value};
use crate::builtins::{def_builtin, err, expect_string, map_like_to_hashmap, type_mismatch_arg};
use crate::error::CloveError;
use crate::eval::current_file_name;
use crate::runtime::{runtime_context_required, RuntimeCtx};
use std::fs;
use std::path::{Component, Path, PathBuf};

pub(crate) fn install(env: &mut crate::env::Env) {
    define_all(env, "path::join", FnArity::at_least(1), |args| {
        join_paths(args)
    });
    define_all(env, "path::cwd", FnArity::exact(0), |_args| {
        let cwd = resolve_base_dir()?;
        Ok(Value::String(cwd.to_string_lossy().to_string()))
    });
    define_all(env, "path::home-dir", FnArity::exact(0), |_args| {
        let home = resolve_home_dir();
        Ok(home
            .map(|p| Value::String(p.to_string_lossy().to_string()))
            .unwrap_or(Value::Nil))
    });
    define_all(env, "path::temp-dir", FnArity::exact(0), |_args| {
        Ok(Value::String(
            std::env::temp_dir().to_string_lossy().to_string(),
        ))
    });
    define_all(
        env,
        "path::basename",
        FnArity::exact(1),
        |args| match args {
            [p] => path_op("path::basename", 1, p, |p| {
                Ok(Value::String(
                    p.file_name()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_default(),
                ))
            }),
            _ => err("path::basename expects path"),
        },
    );
    define_all(env, "path::dirname", FnArity::exact(1), |args| match args {
        [p] => path_op("path::dirname", 1, p, |p| {
            Ok(Value::String(
                p.parent()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_else(|| "".to_string()),
            ))
        }),
        _ => err("path::dirname expects path"),
    });
    define_all(env, "path::extname", FnArity::exact(1), |args| match args {
        [p] => path_op("path::extname", 1, p, |p| {
            Ok(Value::String(
                p.extension()
                    .map(|s| format!(".{}", s.to_string_lossy()))
                    .unwrap_or_default(),
            ))
        }),
        _ => err("path::extname expects path"),
    });
    define_all(
        env,
        "path::normalize",
        FnArity::exact(1),
        |args| match args {
            [p] => path_op("path::normalize", 1, p, |p| {
                Ok(Value::String(
                    normalize_path(p).to_string_lossy().to_string(),
                ))
            }),
            _ => err("path::normalize expects path"),
        },
    );
    define_all(
        env,
        "path::canonicalize",
        FnArity::exact(1),
        |args| match args {
            [p] => path_op("path::canonicalize", 1, p, |p| {
                let target = if p.is_absolute() {
                    p.to_path_buf()
                } else {
                    resolve_base_dir()?.join(p)
                };
                let canonical =
                    fs::canonicalize(&target).map_err(|e| CloveError::runtime(e.to_string()))?;
                Ok(Value::String(canonical.to_string_lossy().to_string()))
            }),
            _ => err("path::canonicalize expects path"),
        },
    );
    define_all(
        env,
        "path::absolute?",
        FnArity::exact(1),
        |args| match args {
            [p] => path_op("path::absolute?", 1, p, |p| {
                Ok(Value::Bool(p.is_absolute()))
            }),
            _ => err("path::absolute? expects path"),
        },
    );
    define_all(
        env,
        "path::relative?",
        FnArity::exact(1),
        |args| match args {
            [p] => path_op("path::relative?", 1, p, |p| {
                Ok(Value::Bool(p.is_relative()))
            }),
            _ => err("path::relative? expects path"),
        },
    );
    define_all(env, "path::resolve", FnArity::exact(1), |args| match args {
        [p] => path_op("path::resolve", 1, p, |p| {
            let resolved = if p.is_absolute() {
                p.to_path_buf()
            } else {
                resolve_base_dir()?.join(p)
            };
            Ok(Value::String(
                normalize_path(&resolved).to_string_lossy().to_string(),
            ))
        }),
        _ => err("path::resolve expects path"),
    });
    define_all(env, "path::source-dir", FnArity::exact(0), |_args| {
        let Some(name) = current_file_name() else {
            return Ok(Value::Nil);
        };
        if name.starts_with('<') && name.ends_with('>') {
            return Ok(Value::Nil);
        }
        let dir = Path::new(&name)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();
        Ok(Value::String(dir))
    });
    define_all(
        env,
        "path::sanitize",
        FnArity::range(1, 2),
        |args| match args {
            [value] => sanitize_filename_value(value, None),
            [value, opts] => sanitize_filename_value(value, Some(opts)),
            _ => err("path::sanitize expects string and optional opts map"),
        },
    );
    define_alias(env, "path::sanitize-filename", "path::sanitize");
    define_all(
        env,
        "path::sanitize-path",
        FnArity::range(1, 2),
        |args| match args {
            [value] => sanitize_relative_path_value(value, None),
            [value, opts] => sanitize_relative_path_value(value, Some(opts)),
            _ => err("path::sanitize-path expects string and optional opts map"),
        },
    );
    define_all(
        env,
        "path::slugify",
        FnArity::range(1, 2),
        |args| match args {
            [value] => slugify_value(value, None),
            [value, opts] => slugify_value(value, Some(opts)),
            _ => err("path::slugify expects string and optional opts map"),
        },
    );
}

fn define_all(
    env: &mut crate::env::Env,
    name: &str,
    arity: FnArity,
    func: impl Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync + Copy + 'static,
) {
    def_builtin!(env, name, arity, |args| func(args));
}

fn define_alias(env: &mut crate::env::Env, alias: &str, target: &str) {
    if let Some(value) = env.get(target) {
        env.define_builtin(alias, value);
    }
}

fn resolve_base_dir() -> Result<PathBuf, CloveError> {
    if let Some(result) = RuntimeCtx::try_with_current(|ctx| Ok(ctx.working_dir().to_path_buf())) {
        return result;
    }
    if runtime_context_required() {
        return Err(CloveError::runtime("runtime context is not available"));
    }
    std::env::current_dir().map_err(|e| CloveError::runtime(e.to_string()))
}

fn resolve_home_dir() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("USERPROFILE").map(PathBuf::from))
}

fn path_op<F>(op: &str, arg_index: usize, value: &Value, f: F) -> Result<Value, CloveError>
where
    F: Fn(&Path) -> Result<Value, CloveError>,
{
    let path = crate::builtins::expect_path_buf(value, op, arg_index)?;
    f(&path)
}

fn join_paths(args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("path::join expects at least 1 argument");
    }
    let mut buf = PathBuf::new();
    for (idx, arg) in args.iter().enumerate() {
        buf.push(crate::builtins::expect_path_buf(
            arg,
            "path::join",
            idx + 1,
        )?);
    }
    Ok(Value::String(
        normalize_path(&buf).to_string_lossy().to_string(),
    ))
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut components = Vec::new();
    for comp in path.components() {
        match comp {
            Component::CurDir => {}
            Component::ParentDir => {
                if let Some(last) = components.last() {
                    if *last != Component::RootDir {
                        components.pop();
                        continue;
                    }
                }
                components.push(comp);
            }
            other => components.push(other),
        }
    }
    let mut out = PathBuf::new();
    for comp in components {
        out.push(comp.as_os_str());
    }
    out
}

struct SanitizeOpts {
    replacement: String,
    max_len: usize,
    lower: bool,
    windows: bool,
}

fn parse_sanitize_opts(
    op: &str,
    arg_index: usize,
    opts_val: Option<&Value>,
    default_lower: bool,
    default_replacement: &str,
) -> Result<SanitizeOpts, CloveError> {
    let mut opts = SanitizeOpts {
        replacement: default_replacement.to_string(),
        max_len: 255,
        lower: default_lower,
        windows: true,
    };
    let Some(opts_val) = opts_val else {
        return Ok(opts);
    };
    if matches!(opts_val, Value::Nil) {
        return Ok(opts);
    }
    let map = map_like_to_hashmap(opts_val, op, arg_index)?;
    if let Some(val) = map.get(&Key::Keyword("replacement".into())) {
        opts.replacement = match val {
            Value::String(s) => s.clone(),
            other => return Err(type_mismatch_arg("string", op, arg_index, other)),
        };
    }
    if let Some(val) = map.get(&Key::Keyword("max-len".into())) {
        match val {
            Value::Int(n) if *n >= 0 => {
                opts.max_len =
                    usize::try_from(*n).map_err(|_| CloveError::runtime("max-len is too large"))?;
            }
            Value::Int(_) => {
                return Err(CloveError::runtime("max-len must be non-negative"));
            }
            other => return Err(type_mismatch_arg("int", op, arg_index, other)),
        }
    }
    if let Some(val) = map.get(&Key::Keyword("lower?".into())) {
        opts.lower = match val {
            Value::Bool(b) => *b,
            other => return Err(type_mismatch_arg("bool", op, arg_index, other)),
        };
    }
    if let Some(val) = map.get(&Key::Keyword("windows?".into())) {
        opts.windows = match val {
            Value::Bool(b) => *b,
            other => return Err(type_mismatch_arg("bool", op, arg_index, other)),
        };
    }
    Ok(opts)
}

fn sanitize_filename_value(value: &Value, opts: Option<&Value>) -> Result<Value, CloveError> {
    let input = expect_string(value, "path::sanitize", 1)?;
    let opts = parse_sanitize_opts("path::sanitize", 2, opts, false, "_")?;
    Ok(Value::String(sanitize_filename_str(&input, &opts)))
}

fn sanitize_relative_path_value(value: &Value, opts: Option<&Value>) -> Result<Value, CloveError> {
    let input = expect_string(value, "path::sanitize-path", 1)?;
    let opts = parse_sanitize_opts("path::sanitize-path", 2, opts, false, "_")?;
    Ok(Value::String(sanitize_path_str(&input, &opts)))
}

fn slugify_value(value: &Value, opts: Option<&Value>) -> Result<Value, CloveError> {
    let input = expect_string(value, "path::slugify", 1)?;
    let opts = parse_sanitize_opts("path::slugify", 2, opts, true, "-")?;
    Ok(Value::String(slugify_str(&input, &opts)))
}

fn sanitize_filename_str(input: &str, opts: &SanitizeOpts) -> String {
    let mut out = String::new();
    for ch in input.chars() {
        if is_forbidden_filename_char(ch, opts.windows) {
            out.push_str(&opts.replacement);
        } else {
            out.push(ch);
        }
    }
    let mut text = if opts.lower { out.to_lowercase() } else { out };
    text = text.trim().to_string();
    text = trim_end_dots(text);
    text = normalize_empty_filename(text);
    if opts.windows && is_windows_reserved_name(&text) {
        text = format!("_{}", text);
    }
    text = truncate_to_char_len(&text, opts.max_len);
    text = trim_end_dots_spaces(text);
    text = normalize_empty_filename(text);
    if opts.windows && is_windows_reserved_name(&text) {
        text = format!("_{}", text);
    }
    if opts.max_len > 0 && text.chars().count() > opts.max_len {
        text = truncate_to_char_len(&text, opts.max_len);
    }
    if text.is_empty() {
        "_".to_string()
    } else {
        text
    }
}

fn sanitize_path_str(input: &str, opts: &SanitizeOpts) -> String {
    let mut parts = Vec::new();
    for raw in input.split(|ch| ch == '/' || ch == '\\') {
        if raw.is_empty() || raw == "." {
            continue;
        }
        if raw == ".." {
            if !parts.is_empty() {
                parts.pop();
            }
            continue;
        }
        let sanitized = sanitize_filename_str(raw, opts);
        parts.push(sanitized);
    }
    if parts.is_empty() {
        "_".to_string()
    } else {
        parts.join("/")
    }
}

fn slugify_str(input: &str, opts: &SanitizeOpts) -> String {
    let sep = if opts.replacement.is_empty() {
        "-".to_string()
    } else {
        opts.replacement.clone()
    };
    let mut out = String::new();
    let mut pending_sep = false;
    for ch in input.chars() {
        if ch.is_alphanumeric() {
            if pending_sep && !out.is_empty() {
                out.push_str(&sep);
            }
            out.push(ch);
            pending_sep = false;
        } else {
            pending_sep = true;
        }
    }
    let mut text = if opts.lower { out.to_lowercase() } else { out };
    text = trim_separator(text, &sep);
    if text.chars().count() > opts.max_len {
        text = truncate_to_char_len(&text, opts.max_len);
        text = trim_separator(text, &sep);
    }
    if text.is_empty() {
        "_".to_string()
    } else {
        text
    }
}

fn is_forbidden_filename_char(ch: char, windows: bool) -> bool {
    if ch == '/' || ch == '\\' || ch == '\0' {
        return true;
    }
    if windows {
        if ch.is_control() {
            return true;
        }
        matches!(ch, '<' | '>' | ':' | '"' | '|' | '?' | '*')
    } else {
        false
    }
}

fn is_windows_reserved_name(name: &str) -> bool {
    let trimmed = name.trim_end_matches(|ch| ch == ' ' || ch == '.');
    if trimmed.is_empty() {
        return false;
    }
    let base = trimmed.split('.').next().unwrap_or(trimmed);
    let upper = base.to_ascii_uppercase();
    matches!(upper.as_str(), "CON" | "PRN" | "AUX" | "NUL")
        || (upper.starts_with("COM")
            && upper.len() == 4
            && matches!(upper.chars().nth(3), Some('1'..='9')))
        || (upper.starts_with("LPT")
            && upper.len() == 4
            && matches!(upper.chars().nth(3), Some('1'..='9')))
}

fn normalize_empty_filename(text: String) -> String {
    if text.is_empty() || text == "." || text == ".." {
        "_".to_string()
    } else {
        text
    }
}

fn trim_end_dots(mut text: String) -> String {
    while text.ends_with('.') {
        text.pop();
    }
    text
}

fn trim_end_dots_spaces(text: String) -> String {
    text.trim_end_matches(|ch| ch == '.' || ch == ' ')
        .to_string()
}

fn trim_separator(mut text: String, sep: &str) -> String {
    if sep.is_empty() {
        return text;
    }
    loop {
        if let Some(rest) = text.strip_prefix(sep) {
            text = rest.to_string();
        } else {
            break;
        }
    }
    loop {
        if let Some(rest) = text.strip_suffix(sep) {
            text = rest.to_string();
        } else {
            break;
        }
    }
    text
}

fn truncate_to_char_len(input: &str, max_len: usize) -> String {
    if max_len == 0 {
        return String::new();
    }
    let mut out = String::new();
    for (idx, ch) in input.chars().enumerate() {
        if idx >= max_len {
            break;
        }
        out.push(ch);
    }
    out
}
