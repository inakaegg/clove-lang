use crate::ast::{FnArity, HashMap, Key, Value};
use crate::builtins::shared::key_to_string;
use crate::builtins::{def_builtin, err, map_like_to_hashmap, type_mismatch_arg};
use crate::env::Env;
use crate::error::CloveError;
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;

pub(crate) fn install(env: &mut Env) {
    def_builtin!(env, "process::sh", FnArity::at_least(1), |args| sh(
        false, args
    ));
    def_builtin!(env, "process::sh!", FnArity::at_least(1), |args| sh(
        true, args
    ));
    def_builtin!(env, "process::sh-line", FnArity::range(1, 2), |args| {
        sh_line(false, args)
    });
    def_builtin!(env, "process::run", FnArity::range(1, 2), |args| run(args));
    def_builtin!(env, "process::which", FnArity::exact(1), |args| which(args));
    define_alias(env, "sh", "process::sh");
    define_alias(env, "sh!", "process::sh!");
    define_alias(env, "sh-line", "process::sh-line");
}

fn define_alias(env: &mut Env, alias: &str, target: &str) {
    if let Some(value) = env.get(target) {
        env.define_builtin(alias, value);
    }
}

fn sh(raise: bool, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("process::sh expects command");
    }
    let (cmd, arg_list, opts) = parse_args(args)?;
    let mut command = Command::new(cmd);
    command.args(arg_list);
    if let Some(ref dir) = opts.dir {
        command.current_dir(dir);
    }
    if !opts.env.is_empty() {
        command.envs(opts.env.iter().map(|(k, v)| (k, v)));
    }
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    if opts.stdin.is_some() {
        command.stdin(Stdio::piped());
    }

    let mut child = command
        .spawn()
        .map_err(|e| CloveError::runtime(format!("process spawn failed: {}", e)))?;

    if let Some(input) = opts.stdin {
        if let Some(stdin) = child.stdin.as_mut() {
            stdin
                .write_all(input.as_bytes())
                .map_err(|e| CloveError::runtime(format!("process stdin error: {}", e)))?;
        }
    }

    let mut stdout = String::new();
    let mut stderr = String::new();

    if opts.stream {
        let mut out_handle = child.stdout.take().unwrap();
        let mut err_handle = child.stderr.take().unwrap();
        let out_thread = thread::spawn(move || read_streaming(&mut out_handle, true));
        let err_thread = thread::spawn(move || read_streaming(&mut err_handle, false));
        stdout = out_thread
            .join()
            .unwrap_or_else(|_| Ok(String::new()))
            .map_err(|e| CloveError::runtime(e))?;
        stderr = err_thread
            .join()
            .unwrap_or_else(|_| Ok(String::new()))
            .map_err(|e| CloveError::runtime(e))?;
    } else {
        if let Some(mut out) = child.stdout.take() {
            out.read_to_string(&mut stdout)
                .map_err(|e| CloveError::runtime(format!("process read stdout error: {}", e)))?;
        }
        if let Some(mut err) = child.stderr.take() {
            err.read_to_string(&mut stderr)
                .map_err(|e| CloveError::runtime(format!("process read stderr error: {}", e)))?;
        }
    }

    let status = child
        .wait()
        .map_err(|e| CloveError::runtime(format!("process wait error: {}", e)))?;
    let code = status.code().unwrap_or(-1);

    if raise && code != 0 {
        return err(format!("command failed (exit {}): {}", code, stderr));
    }

    let mut out_map = HashMap::new();
    let status_val = Value::Int(code as i64);
    out_map.insert(Key::Keyword("status".into()), status_val.clone());
    out_map.insert(Key::Keyword("exit".into()), status_val);
    out_map.insert(Key::Keyword("out".into()), Value::String(stdout));
    out_map.insert(Key::Keyword("err".into()), Value::String(stderr));
    Ok(Value::Map(out_map))
}

fn run(args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("process::run expects command vector");
    }
    if args.len() > 2 {
        return err("process::run expects command vector and optional opts map");
    }
    let (cmd, arg_list) = parse_command_value(&args[0])?;
    let opts = if args.len() == 2 {
        match &args[1] {
            Value::Map(_) | Value::SortedMap(_) => {
                Some(map_like_to_hashmap(&args[1], "process::run", 2)?)
            }
            _ => return err("process::run expects opts map as second argument"),
        }
    } else {
        None
    };
    let mut run_opts = RunOpts::default();
    if let Some(map) = opts.as_ref() {
        parse_run_opts(&mut run_opts, map)?;
    }

    let mut command = Command::new(cmd);
    command.args(arg_list);
    if let Some(ref dir) = run_opts.cwd {
        command.current_dir(dir);
    }
    if run_opts.clear_env {
        command.env_clear();
    }
    if !run_opts.env.is_empty() {
        command.envs(run_opts.env.iter().map(|(k, v)| (k, v)));
    }
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    if run_opts.stdin.is_some() {
        command.stdin(Stdio::piped());
    }

    let mut child = command
        .spawn()
        .map_err(|e| CloveError::runtime(format!("process spawn failed: {}", e)))?;

    if let Some(input) = run_opts.stdin {
        if let Some(stdin) = child.stdin.as_mut() {
            stdin
                .write_all(&input)
                .map_err(|e| CloveError::runtime(format!("process stdin error: {}", e)))?;
        }
    }

    let output = child
        .wait_with_output()
        .map_err(|e| CloveError::runtime(format!("process wait error: {}", e)))?;
    let code = output.status.code().unwrap_or(-1);

    let out_val = match run_opts.out_enc {
        OutputEncoding::Bytes => bytes_to_value(&output.stdout),
        OutputEncoding::Text => Value::String(String::from_utf8_lossy(&output.stdout).to_string()),
    };
    let err_val = match run_opts.err_enc {
        OutputEncoding::Bytes => bytes_to_value(&output.stderr),
        OutputEncoding::Text => Value::String(String::from_utf8_lossy(&output.stderr).to_string()),
    };

    let mut out_map = HashMap::new();
    let status_val = Value::Int(code as i64);
    out_map.insert(Key::Keyword("status".into()), status_val.clone());
    out_map.insert(Key::Keyword("exit".into()), status_val);
    out_map.insert(Key::Keyword("out".into()), out_val);
    out_map.insert(Key::Keyword("err".into()), err_val);
    Ok(Value::Map(out_map))
}

fn which(args: &[Value]) -> Result<Value, CloveError> {
    let name = match args {
        [Value::String(s)] | [Value::Symbol(s)] => s.clone(),
        [other] => {
            return Err(type_mismatch_arg(
                "command string",
                "process::which",
                1,
                other,
            ))
        }
        _ => return err("process::which expects command string"),
    };
    if let Some(found) = find_in_path(&name) {
        return Ok(Value::String(found));
    }
    Ok(Value::Nil)
}

fn sh_line(raise: bool, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("process::sh-line expects command line");
    }
    if args.len() > 2 {
        return err("process::sh-line expects command line and optional opts map");
    }
    let line = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => return err("command line must be string"),
    };
    let parts = shell_words::split(line)
        .map_err(|e| CloveError::runtime(format!("sh-line parse error: {}", e)))?;
    if parts.is_empty() {
        return err("process::sh-line expects command");
    }
    let mut call_args: Vec<Value> = parts.into_iter().map(Value::String).collect();
    if args.len() == 2 {
        match &args[1] {
            Value::Map(_) | Value::SortedMap(_) => call_args.push(args[1].clone()),
            _ => return err("process::sh-line expects opts map as second argument"),
        }
    }
    sh(raise, &call_args)
}

fn read_streaming(reader: &mut dyn Read, to_stdout: bool) -> Result<String, String> {
    let mut buf = [0u8; 1024];
    let mut collected = Vec::new();
    loop {
        match reader.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                let chunk = &buf[..n];
                collected.extend_from_slice(chunk);
                if to_stdout {
                    let _ = std::io::stdout().write_all(chunk);
                    let _ = std::io::stdout().flush();
                } else {
                    let _ = std::io::stderr().write_all(chunk);
                    let _ = std::io::stderr().flush();
                }
            }
            Err(e) => return Err(e.to_string()),
        }
    }
    String::from_utf8(collected).map_err(|e| e.to_string())
}

#[derive(Default)]
struct RunOpts {
    cwd: Option<String>,
    env: std::collections::HashMap<String, String>,
    clear_env: bool,
    stdin: Option<Vec<u8>>,
    out_enc: OutputEncoding,
    err_enc: OutputEncoding,
}

#[derive(Clone, Copy)]
enum OutputEncoding {
    Text,
    Bytes,
}

impl Default for OutputEncoding {
    fn default() -> Self {
        OutputEncoding::Text
    }
}

struct ShOpts {
    dir: Option<String>,
    env: std::collections::HashMap<String, String>,
    stream: bool,
    stdin: Option<String>,
}

fn parse_args(args: &[Value]) -> Result<(String, Vec<String>, ShOpts), CloveError> {
    let cmd = match &args[0] {
        Value::String(s) => s.clone(),
        Value::Symbol(s) => s.clone(),
        _ => return err("command must be string"),
    };
    let mut rest = Vec::new();
    let mut opts_map: Option<HashMap<Key, Value>> = None;
    for arg in &args[1..] {
        match arg {
            Value::Map(_) | Value::SortedMap(_) => {
                opts_map = Some(map_like_to_hashmap(arg, "process::sh", 2)?)
            }
            Value::String(s) => rest.push(s.clone()),
            Value::Symbol(s) => rest.push(s.clone()),
            _ => return err("arguments must be strings or opts map"),
        }
    }
    let mut opts = ShOpts {
        dir: None,
        env: std::collections::HashMap::new(),
        stream: false,
        stdin: None,
    };
    if let Some(m) = opts_map.as_ref() {
        if let Some(v) = m.get(&Key::Keyword("dir".into())) {
            if let Value::String(s) = v {
                opts.dir = Some(s.clone());
            }
        }
        if let Some(v) = m.get(&Key::Keyword("env".into())) {
            if let Value::Map(_) | Value::SortedMap(_) = v {
                let env_map = map_like_to_hashmap(v, "process::sh", 2)?;
                for (k, v) in env_map {
                    let key = key_to_string(&k);
                    let val = match v {
                        Value::String(s) => s.clone(),
                        Value::Int(n) => n.to_string(),
                        Value::Float(f) => f.to_string(),
                        Value::Bool(b) => b.to_string(),
                        Value::Nil => "".to_string(),
                        _ => return err("env values must be scalar"),
                    };
                    opts.env.insert(key, val);
                }
            }
        }
        if let Some(Value::Bool(b)) = m.get(&Key::Keyword("stream?".into())) {
            opts.stream = *b;
        }
        if let Some(Value::String(s)) = m.get(&Key::Keyword("stdin".into())) {
            opts.stdin = Some(s.clone());
        }
    }
    Ok((cmd, rest, opts))
}

fn parse_command_value(value: &Value) -> Result<(String, Vec<String>), CloveError> {
    match value {
        Value::Vector(items) | Value::List(items) => {
            if items.is_empty() {
                return err("process::run expects non-empty command vector");
            }
            let mut iter = items.iter();
            let cmd = match iter.next() {
                Some(Value::String(s)) | Some(Value::Symbol(s)) => s.clone(),
                Some(other) => {
                    return Err(type_mismatch_arg(
                        "command string",
                        "process::run",
                        1,
                        other,
                    ))
                }
                None => return err("process::run expects non-empty command vector"),
            };
            let mut args = Vec::new();
            for item in iter {
                match item {
                    Value::String(s) | Value::Symbol(s) => args.push(s.clone()),
                    other => {
                        return Err(type_mismatch_arg(
                            "command arg string",
                            "process::run",
                            1,
                            other,
                        ))
                    }
                }
            }
            Ok((cmd, args))
        }
        Value::String(s) | Value::Symbol(s) => Ok((s.clone(), Vec::new())),
        other => Err(type_mismatch_arg(
            "command vector",
            "process::run",
            1,
            other,
        )),
    }
}

fn parse_run_opts(opts: &mut RunOpts, map: &HashMap<Key, Value>) -> Result<(), CloveError> {
    if let Some(v) = map.get(&Key::Keyword("cwd".into())) {
        if let Value::String(s) = v {
            opts.cwd = Some(s.clone());
        }
    }
    if let Some(v) = map.get(&Key::Keyword("clear-env".into())) {
        if let Value::Bool(b) = v {
            opts.clear_env = *b;
        }
    }
    if let Some(v) = map.get(&Key::Keyword("env".into())) {
        if let Value::Map(_) | Value::SortedMap(_) = v {
            let env_map = map_like_to_hashmap(v, "process::run", 2)?;
            for (k, v) in env_map {
                let key = key_to_string(&k);
                let val = match v {
                    Value::String(s) => s.clone(),
                    Value::Int(n) => n.to_string(),
                    Value::Float(f) => f.to_string(),
                    Value::Bool(b) => b.to_string(),
                    Value::Nil => "".to_string(),
                    _ => return err("env values must be scalar"),
                };
                opts.env.insert(key, val);
            }
        }
    }
    if let Some(v) = map.get(&Key::Keyword("in".into())) {
        opts.stdin = Some(value_to_bytes(v)?);
    }
    if let Some(v) = map.get(&Key::Keyword("out-enc".into())) {
        opts.out_enc = parse_output_encoding(v);
    }
    if let Some(v) = map.get(&Key::Keyword("err-enc".into())) {
        opts.err_enc = parse_output_encoding(v);
    }
    Ok(())
}

fn parse_output_encoding(value: &Value) -> OutputEncoding {
    match value {
        Value::Symbol(s) if s == ":bytes" => OutputEncoding::Bytes,
        Value::String(s) if s.eq_ignore_ascii_case("bytes") => OutputEncoding::Bytes,
        _ => OutputEncoding::Text,
    }
}

fn value_to_bytes(value: &Value) -> Result<Vec<u8>, CloveError> {
    match value {
        Value::String(s) => Ok(s.as_bytes().to_vec()),
        Value::Vector(items) | Value::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                match item {
                    Value::Int(n) if (0..=255).contains(n) => out.push(*n as u8),
                    other => {
                        return Err(type_mismatch_arg(
                            "byte (0-255) int",
                            "process::run",
                            1,
                            other,
                        ))
                    }
                }
            }
            Ok(out)
        }
        Value::Nil => Ok(Vec::new()),
        other => Err(type_mismatch_arg(
            "stdin string or bytes",
            "process::run",
            1,
            other,
        )),
    }
}

fn bytes_to_value(bytes: &[u8]) -> Value {
    let mut vec = crate::ast::Vector::new();
    for b in bytes {
        vec.push_back(Value::Int(*b as i64));
    }
    Value::Vector(vec)
}

fn find_in_path(name: &str) -> Option<String> {
    let path = Path::new(name);
    if path.is_absolute() || name.contains(std::path::MAIN_SEPARATOR) {
        return path.is_file().then(|| path.to_string_lossy().to_string());
    }
    let Some(raw) = std::env::var_os("PATH") else {
        return None;
    };
    let exts = path_exts(name);
    for dir in std::env::split_paths(&raw) {
        for ext in &exts {
            let candidate = if ext.is_empty() {
                dir.join(name)
            } else {
                dir.join(format!("{}{}", name, ext))
            };
            if candidate.is_file() {
                return Some(candidate.to_string_lossy().to_string());
            }
        }
    }
    None
}

fn path_exts(name: &str) -> Vec<String> {
    let mut exts = vec![String::new()];
    if cfg!(windows) && Path::new(name).extension().is_none() {
        if let Ok(raw) = std::env::var("PATHEXT") {
            for ext in raw.split(';') {
                if ext.is_empty() {
                    continue;
                }
                let norm = if ext.starts_with('.') {
                    ext.to_string()
                } else {
                    format!(".{}", ext)
                };
                exts.push(norm);
            }
        }
    }
    exts
}
