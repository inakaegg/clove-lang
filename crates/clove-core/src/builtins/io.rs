use crate::ast::Vector;
use crate::ast::{FnArity, Value};
use crate::builtins::{def_builtin, err, expect_path_buf, type_mismatch_arg};
use crate::env::Env;
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::io_reader::{reader_from_value, reader_value, ReaderHandle};
use crate::runtime::{runtime_context_required, RuntimeCtx};
use crate::seq::{SeqEngine, SeqHandle};
use crate::types::TypeKind;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

pub(crate) fn install(env: &mut Env) {
    def_builtin!(
        env,
        "io::slurp-bytes",
        FnArity::exact(1),
        |args| match args {
            [path] => slurp_bytes(path),
            _ => err("slurp-bytes expects path"),
        }
    );
    define_alias(env, "slurp-bytes", "io::slurp-bytes");
    def_builtin!(
        env,
        "io::spit-bytes",
        FnArity::exact(2),
        |args| match args {
            [path, bytes] => spit_bytes(path, bytes),
            _ => err("spit-bytes expects path and byte vector"),
        }
    );
    define_alias(env, "spit-bytes", "io::spit-bytes");
    def_builtin!(env, "io::line-seq", FnArity::exact(1), |args| match args {
        [path] => {
            let pb = expect_path_buf(path, "line-seq", 1)?;
            let file = File::open(&pb)
                .map_err(|e| CloveError::runtime(format!("line-seq failed: {}", e)))?;
            let reader = BufReader::new(file);
            Ok(Value::Seq(SeqHandle::new(Box::new(LineSeqEngine {
                reader,
            }))))
        }
        _ => err("line-seq expects path"),
    });
    define_alias(env, "line-seq", "io::line-seq");
    def_builtin!(
        env,
        "io::open-reader",
        FnArity::exact(1),
        |args| match args {
            [path] => open_reader(path),
            _ => err("open-reader expects path"),
        }
    );
    define_alias(env, "open-reader", "io::open-reader");
    def_builtin!(env, "io::close", FnArity::exact(1), |args| match args {
        [reader] => close_reader(reader),
        _ => err("close expects reader"),
    });
    define_alias(env, "close", "io::close");
    def_builtin!(env, "io::read-line", FnArity::exact(0), |_args| read_line());
    define_alias(env, "read-line", "io::read-line");
    def_builtin!(env, "io::read-all", FnArity::exact(0), |_args| read_all());
    define_alias(env, "read-all", "io::read-all");
    def_builtin!(
        env,
        "io::resource-url",
        FnArity::exact(1),
        |args| match args {
            [name] => resource_url(name),
            _ => err("resource-url expects name"),
        }
    );
    define_alias(env, "resource-url", "io::resource-url");
    def_builtin!(
        env,
        "io::resource-bytes",
        FnArity::exact(1),
        |args| match args {
            [name] => resource_bytes(name),
            _ => err("resource-bytes expects name"),
        }
    );
    define_alias(env, "resource-bytes", "io::resource-bytes");
    def_builtin!(
        env,
        "io::resource->tempfile",
        FnArity::exact(1),
        |args| match args {
            [name] => resource_tempfile(name),
            _ => err("resource->tempfile expects name"),
        }
    );
    define_alias(env, "resource->tempfile", "io::resource->tempfile");
    install_fn_meta();
}

fn define_alias(env: &mut Env, alias: &str, target: &str) {
    if let Some(value) = env.get(target) {
        env.define_builtin(alias, value);
    }
}

fn install_fn_meta() {
    fn named(name: &str) -> TypeKind {
        TypeKind::named(name.to_string())
    }
    fn path_ty() -> TypeKind {
        TypeKind::union(vec![TypeKind::Str, named("core::Symbol")])
    }
    fn list_ty() -> TypeKind {
        named("core::List")
    }
    fn seq_ty() -> TypeKind {
        named("core::Seq")
    }
    fn reader_ty() -> TypeKind {
        named("core::Foreign")
    }
    fn byte_seq_ty() -> TypeKind {
        TypeKind::union(vec![TypeKind::vector(TypeKind::Int), list_ty()])
    }
    fn maybe_str_ty() -> TypeKind {
        TypeKind::union(vec![TypeKind::Str, TypeKind::Nil])
    }
    fn overload(args: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> FnOverload {
        FnOverload {
            arg_types: args,
            rest,
            ret_type: ret,
            special_op: None,
        }
    }
    fn register(name: &str, arglist: &[&str], doc: &str, overloads: Vec<FnOverload>) {
        let (ns, local) = match name.split_once("::") {
            Some((ns, local)) => (ns, local),
            None => ("core", name),
        };
        let mut meta = FnMeta::new(ns, local);
        for arg in arglist {
            meta.arglist.push((*arg).into());
        }
        meta.doc = Some(doc.to_string());
        meta.overloads = overloads;
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }
    fn register_aliases(name: &str, arglist: &[&str], doc: &str, overloads: Vec<FnOverload>) {
        register(name, arglist, doc, overloads.clone());
        register(&format!("io::{}", name), arglist, doc, overloads.clone());
        register(&format!("std::{}", name), arglist, doc, overloads);
    }

    register_aliases(
        "slurp-bytes",
        &["[path]"],
        "Read the file or URL at path into a vector of bytes.",
        vec![overload(
            vec![path_ty()],
            None,
            TypeKind::vector(TypeKind::Int),
        )],
    );
    register_aliases(
        "spit-bytes",
        &["[path bytes]"],
        "Write the byte vector/list to the file at path. Returns path.",
        vec![overload(vec![path_ty(), byte_seq_ty()], None, path_ty())],
    );
    register_aliases(
        "line-seq",
        &["[path]"],
        "Return a lazy seq of lines from the file at path.",
        vec![overload(vec![path_ty()], None, seq_ty())],
    );
    register_aliases(
        "open-reader",
        &["[path]"],
        "Open a file reader handle.",
        vec![overload(vec![path_ty()], None, reader_ty())],
    );
    register_aliases(
        "close",
        &["[reader]"],
        "Close a reader handle.",
        vec![overload(vec![reader_ty()], None, TypeKind::Nil)],
    );
    register_aliases(
        "read-line",
        &["[]"],
        "Read a line from stdin, stripping the trailing newline.",
        vec![overload(vec![], None, TypeKind::Str)],
    );
    register_aliases(
        "read-all",
        &["[]"],
        "Read all remaining stdin input into a string.",
        vec![overload(vec![], None, TypeKind::Str)],
    );
    register_aliases(
        "resource-url",
        &["[name]"],
        "Resolve resource name to a resource or file URL string.",
        vec![overload(vec![path_ty()], None, maybe_str_ty())],
    );
    register_aliases(
        "resource-bytes",
        &["[name]"],
        "Read resource bytes by name. Returns nil when not found.",
        vec![overload(
            vec![path_ty()],
            None,
            TypeKind::union(vec![byte_seq_ty(), TypeKind::Nil]),
        )],
    );
    register_aliases(
        "resource->tempfile",
        &["[name]"],
        "Resolve resource name to a file path, writing to a temp file if needed.",
        vec![overload(vec![path_ty()], None, maybe_str_ty())],
    );
}

fn slurp_bytes(path: &Value) -> Result<Value, CloveError> {
    let path_str = match path {
        Value::String(s) => s.clone(),
        Value::Symbol(s) => s.clone(),
        other => {
            return Err(type_mismatch_arg(
                "path string or symbol",
                "slurp-bytes",
                1,
                other,
            ))
        }
    };
    if path_str.starts_with("http://") || path_str.starts_with("https://") {
        let resp = reqwest::blocking::get(&path_str)
            .and_then(|r| r.error_for_status())
            .map_err(|e| CloveError::runtime(format!("slurp-bytes failed: {}", e)))?;
        let bytes = resp
            .bytes()
            .map_err(|e| CloveError::runtime(format!("slurp-bytes failed: {}", e)))?;
        Ok(bytes_to_value(&bytes))
    } else {
        let data = fs::read(&path_str)
            .map_err(|e| CloveError::runtime(format!("slurp-bytes failed: {}", e)))?;
        Ok(bytes_to_value(&data))
    }
}

fn bytes_to_value(data: &[u8]) -> Value {
    let mut vec = Vector::new();
    for b in data {
        vec.push_back(Value::Int(*b as i64));
    }
    Value::Vector(vec)
}

fn spit_bytes(path: &Value, bytes: &Value) -> Result<Value, CloveError> {
    let path_buf = expect_path_buf(path, "spit-bytes", 1)?;
    let data = match bytes {
        Value::Vector(v) | Value::List(v) => {
            let mut out = Vec::with_capacity(v.len());
            for item in v {
                match item {
                    Value::Int(n) if *n >= 0 && *n <= 255 => out.push(*n as u8),
                    _ => return Err(type_mismatch_arg("byte (0-255) int", "spit-bytes", 2, item)),
                }
            }
            out
        }
        other => {
            return Err(type_mismatch_arg(
                "byte vector/list",
                "spit-bytes",
                2,
                other,
            ))
        }
    };
    fs::write(&path_buf, data)
        .map_err(|e| CloveError::runtime(format!("spit-bytes failed: {}", e)))?;
    Ok(path.clone())
}

fn open_reader(path: &Value) -> Result<Value, CloveError> {
    let pb = expect_path_buf(path, "open-reader", 1)?;
    let file =
        File::open(&pb).map_err(|e| CloveError::runtime(format!("open-reader failed: {}", e)))?;
    let reader = BufReader::new(file);
    Ok(reader_value(ReaderHandle::new(reader)))
}

fn close_reader(reader: &Value) -> Result<Value, CloveError> {
    let Some(handle) = reader_from_value(reader) else {
        return Err(type_mismatch_arg("reader handle", "close", 1, reader));
    };
    handle
        .close()
        .map_err(|e| CloveError::runtime(format!("close failed: {}", e)))?;
    Ok(Value::Nil)
}

fn read_line() -> Result<Value, CloveError> {
    let stdin = io::stdin();
    let mut buf = String::new();
    let mut handle = stdin.lock();
    handle
        .read_line(&mut buf)
        .map_err(|e| CloveError::runtime(e.to_string()))?;
    if buf.ends_with('\n') {
        buf.pop();
        if buf.ends_with('\r') {
            buf.pop();
        }
    }
    Ok(Value::String(buf))
}

fn read_all() -> Result<Value, CloveError> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|e| CloveError::runtime(e.to_string()))?;
    Ok(Value::String(buf))
}

enum ResourceLocation {
    Embedded { name: String, bytes: Vec<u8> },
    File(PathBuf),
}

fn resource_url(name: &Value) -> Result<Value, CloveError> {
    let name = expect_resource_name(name, "resource-url", 1)?;
    match resolve_resource(&name)? {
        Some(ResourceLocation::Embedded { name, .. }) => {
            Ok(Value::String(format!("resource://{}", name)))
        }
        Some(ResourceLocation::File(path)) => {
            Ok(Value::String(format!("file://{}", path.to_string_lossy())))
        }
        None => Ok(Value::Nil),
    }
}

fn resource_bytes(name: &Value) -> Result<Value, CloveError> {
    let name = expect_resource_name(name, "resource-bytes", 1)?;
    match resolve_resource(&name)? {
        Some(ResourceLocation::Embedded { bytes, .. }) => Ok(bytes_to_value(&bytes)),
        Some(ResourceLocation::File(path)) => {
            let data = fs::read(&path)
                .map_err(|e| CloveError::runtime(format!("resource-bytes failed: {}", e)))?;
            Ok(bytes_to_value(&data))
        }
        None => Ok(Value::Nil),
    }
}

fn resource_tempfile(name: &Value) -> Result<Value, CloveError> {
    let name = expect_resource_name(name, "resource->tempfile", 1)?;
    match resolve_resource(&name)? {
        Some(ResourceLocation::Embedded { name, bytes }) => {
            let path = write_temp_resource(&name, &bytes)?;
            Ok(Value::String(path.to_string_lossy().to_string()))
        }
        Some(ResourceLocation::File(path)) => Ok(Value::String(path.to_string_lossy().to_string())),
        None => Ok(Value::Nil),
    }
}

fn expect_resource_name(value: &Value, op: &str, arg_index: usize) -> Result<String, CloveError> {
    match value {
        Value::String(s) | Value::Symbol(s) => Ok(s.clone()),
        other => Err(type_mismatch_arg(
            "resource name string",
            op,
            arg_index,
            other,
        )),
    }
}

fn resolve_resource(name: &str) -> Result<Option<ResourceLocation>, CloveError> {
    if let Some((name, bytes)) = resolve_embedded_resource(name)? {
        return Ok(Some(ResourceLocation::Embedded { name, bytes }));
    }
    if let Some(path) = resolve_file_resource(name)? {
        return Ok(Some(ResourceLocation::File(path)));
    }
    Ok(None)
}

fn resolve_embedded_resource(name: &str) -> Result<Option<(String, Vec<u8>)>, CloveError> {
    let trimmed = name.trim_start_matches("./");
    let candidates = if trimmed != name {
        vec![name, trimmed]
    } else {
        vec![name]
    };
    for candidate in candidates {
        if let Some(result) =
            RuntimeCtx::try_with_current(|ctx| Ok(ctx.embedded_resource(candidate)))
        {
            let bytes = result?;
            if let Some(bytes) = bytes {
                return Ok(Some((candidate.to_string(), bytes)));
            }
        }
    }
    Ok(None)
}

fn resolve_file_resource(name: &str) -> Result<Option<PathBuf>, CloveError> {
    let raw = Path::new(name);
    if raw.is_absolute() && raw.exists() {
        return Ok(Some(raw.to_path_buf()));
    }
    if let Some(current) = current_file_dir() {
        let candidate = current.join(raw);
        if candidate.exists() {
            return Ok(Some(candidate));
        }
    }
    let base = resolve_base_dir()?;
    let candidate = base.join(raw);
    if candidate.exists() {
        return Ok(Some(candidate));
    }
    Ok(None)
}

fn current_file_dir() -> Option<PathBuf> {
    let name = crate::eval::current_file_name()?;
    if name.starts_with('<') && name.ends_with('>') {
        return None;
    }
    Path::new(&name).parent().map(|p| p.to_path_buf())
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

fn write_temp_resource(name: &str, bytes: &[u8]) -> Result<PathBuf, CloveError> {
    let mut dir = std::env::temp_dir();
    let ext = Path::new(name)
        .extension()
        .and_then(|s| s.to_str())
        .map(|s| format!(".{}", s))
        .unwrap_or_default();
    for _ in 0..10 {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let file_name = format!("clove-resource-{}-{}{}", std::process::id(), stamp, ext);
        dir.push(file_name);
        if !dir.exists() {
            fs::write(&dir, bytes)
                .map_err(|e| CloveError::runtime(format!("resource->tempfile failed: {}", e)))?;
            return Ok(dir);
        }
        dir.pop();
    }
    Err(CloveError::runtime(
        "resource->tempfile failed: too many attempts",
    ))
}

struct LineSeqEngine {
    reader: BufReader<File>,
}

impl SeqEngine for LineSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        let mut buf = String::new();
        match self.reader.read_line(&mut buf) {
            Ok(0) => Ok(None),
            Ok(_) => {
                if buf.ends_with('\n') {
                    buf.pop();
                    if buf.ends_with('\r') {
                        buf.pop();
                    }
                }
                Ok(Some(Value::String(buf)))
            }
            Err(e) => Err(CloveError::runtime(e.to_string())),
        }
    }
}
