use crate::ast::Vector;
use crate::ast::{FnArity, Value};
use crate::builtins::shared::{serde_to_value, value_to_serde};
use crate::builtins::{def_builtin, err, expect_path_buf, type_mismatch_arg};
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::io_reader::reader_from_value;
use crate::seq::{SeqEngine, SeqHandle};
use crate::types::TypeKind;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "json::parse", FnArity::exact(1), |args| match args {
        [Value::String(s)] => parse_json(s),
        _ => err("json::parse expects string"),
    });
    def_builtin!(
        env,
        "json::generate",
        FnArity::exact(1),
        |args| match args {
            [v] => generate_json(v, false),
            _ => err("json::generate expects value"),
        }
    );
    def_builtin!(
        env,
        "json::generate-pretty",
        FnArity::exact(1),
        |args| match args {
            [v] => generate_json(v, true),
            _ => err("json::generate-pretty expects value"),
        }
    );
    def_builtin!(
        env,
        "json::read-file",
        FnArity::exact(1),
        |args| match args {
            [Value::String(path)] => {
                let content = fs::read_to_string(path)
                    .map_err(|e| CloveError::runtime(format!("json::read-file failed: {}", e)))?;
                parse_json(&content)
            }
            _ => err("json::read-file expects path string"),
        }
    );
    def_builtin!(
        env,
        "json::read-file-seq",
        FnArity::exact(1),
        |args| match args {
            [Value::String(path)] =>
                read_json_seq(&Value::String(path.clone()), "json::read-file-seq"),
            _ => err("json::read-file-seq expects path string"),
        }
    );
    def_builtin!(
        env,
        "json::read-seq",
        FnArity::exact(1),
        |args| match args {
            [value] => read_json_seq(value, "json::read-seq"),
            _ => err("json::read-seq expects path or reader"),
        }
    );
    def_builtin!(
        env,
        "json::write-file",
        FnArity::exact(2),
        |args| match args {
            [Value::String(path), v] => {
                let json = generate_json(v, false)?;
                let s = match json {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                if let Some(parent) = Path::new(path).parent() {
                    if !parent.as_os_str().is_empty() {
                        fs::create_dir_all(parent).map_err(|e| {
                            CloveError::runtime(format!("json::write-file failed: {}", e))
                        })?;
                    }
                }
                fs::write(path, s)
                    .map_err(|e| CloveError::runtime(format!("json::write-file failed: {}", e)))?;
                Ok(Value::String(path.clone()))
            }
            _ => err("json::write-file expects path and value"),
        }
    );
    install_fn_meta();
}

pub(crate) fn read_json_file_value(path: &str) -> Result<Value, CloveError> {
    let content = fs::read_to_string(path)
        .map_err(|e| CloveError::runtime(format!("json::read-file failed: {}", e)))?;
    parse_json(&content)
}

fn read_json_seq(value: &Value, op: &str) -> Result<Value, CloveError> {
    match value {
        Value::String(_) | Value::Symbol(_) => {
            let pb = expect_path_buf(value, op, 1)?;
            let file =
                File::open(&pb).map_err(|e| CloveError::runtime(format!("{op} failed: {}", e)))?;
            let reader = BufReader::new(file);
            Ok(Value::Seq(SeqHandle::new(Box::new(JsonSeqEngine::new(
                reader,
            )))))
        }
        _ => {
            if let Some(handle) = reader_from_value(value) {
                let reader = BufReader::new(handle);
                Ok(Value::Seq(SeqHandle::new(Box::new(JsonSeqEngine::new(
                    reader,
                )))))
            } else {
                Err(type_mismatch_arg("path or reader", op, 1, value))
            }
        }
    }
}

fn parse_json(s: &str) -> Result<Value, CloveError> {
    let parsed: serde_json::Value = serde_json::from_str(s)
        .map_err(|e| CloveError::runtime(format!("json parse error: {}", e)))?;
    serde_to_value(&parsed)
}

fn generate_json(v: &Value, pretty: bool) -> Result<Value, CloveError> {
    let serde_val = value_to_serde(v)?;
    let out = if pretty {
        serde_json::to_string_pretty(&serde_val)
    } else {
        serde_json::to_string(&serde_val)
    }
    .map_err(|e| CloveError::runtime(format!("json generate error: {}", e)))?;
    Ok(Value::String(out))
}

enum JsonSeqMode {
    Array,
    Object,
}

struct JsonSeqEngine<R>
where
    R: Read + Send + 'static,
{
    reader: ByteReader<R>,
    mode: Option<JsonSeqMode>,
    done: bool,
}

impl<R> JsonSeqEngine<R>
where
    R: Read + Send + 'static,
{
    fn new(reader: R) -> Self {
        Self {
            reader: ByteReader::new(reader),
            mode: None,
            done: false,
        }
    }

    fn init(&mut self) -> Result<(), CloveError> {
        if self.mode.is_some() || self.done {
            return Ok(());
        }
        let head = self
            .reader
            .read_non_ws()?
            .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
        match head {
            b'[' => {
                self.mode = Some(JsonSeqMode::Array);
                let next = self.reader.read_non_ws()?.ok_or_else(|| {
                    CloveError::runtime("json::read-file-seq failed: unexpected EOF")
                })?;
                if next == b']' {
                    self.done = true;
                } else {
                    self.reader.unread(next);
                }
            }
            b'{' => {
                self.mode = Some(JsonSeqMode::Object);
                let next = self.reader.read_non_ws()?.ok_or_else(|| {
                    CloveError::runtime("json::read-file-seq failed: unexpected EOF")
                })?;
                if next == b'}' {
                    self.done = true;
                } else {
                    self.reader.unread(next);
                }
            }
            _ => {
                return Err(CloveError::runtime(
                    "json::read-file-seq expects top-level array or object",
                ))
            }
        }
        Ok(())
    }

    fn next_array_item(&mut self) -> Result<Option<Value>, CloveError> {
        if self.done {
            return Ok(None);
        }
        let raw = self.read_value_raw()?;
        let value = parse_json(&raw)?;
        let delim = self
            .reader
            .read_non_ws()?
            .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
        match delim {
            b',' => Ok(Some(value)),
            b']' => {
                self.done = true;
                Ok(Some(value))
            }
            _ => Err(CloveError::runtime(
                "json::read-file-seq failed: expected ',' or ']'",
            )),
        }
    }

    fn next_object_item(&mut self) -> Result<Option<Value>, CloveError> {
        if self.done {
            return Ok(None);
        }
        let raw_key = self.read_value_raw()?;
        if !raw_key.starts_with('"') {
            return Err(CloveError::runtime(
                "json::read-file-seq failed: object key must be string",
            ));
        }
        let key: String = serde_json::from_str(&raw_key)
            .map_err(|e| CloveError::runtime(format!("json::read-file-seq failed: {}", e)))?;
        let colon = self
            .reader
            .read_non_ws()?
            .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
        if colon != b':' {
            return Err(CloveError::runtime(
                "json::read-file-seq failed: expected ':' after key",
            ));
        }
        let raw_value = self.read_value_raw()?;
        let value = parse_json(&raw_value)?;
        let delim = self
            .reader
            .read_non_ws()?
            .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
        let entry = Value::Vector(Vector::from(vec![Value::String(key), value]));
        match delim {
            b',' => Ok(Some(entry)),
            b'}' => {
                self.done = true;
                Ok(Some(entry))
            }
            _ => Err(CloveError::runtime(
                "json::read-file-seq failed: expected ',' or '}'",
            )),
        }
    }

    fn read_value_raw(&mut self) -> Result<String, CloveError> {
        let first = self
            .reader
            .read_non_ws()?
            .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
        let mut buf = Vec::new();
        let mut depth = 0usize;
        let mut in_string = false;
        let mut escape = false;
        let mut started_struct = false;
        let mut started_string = false;

        match first {
            b'{' | b'[' => {
                depth = 1;
                started_struct = true;
            }
            b'"' => {
                in_string = true;
                started_string = true;
            }
            _ => {}
        }
        buf.push(first);

        loop {
            let b = self
                .reader
                .next_byte()?
                .ok_or_else(|| CloveError::runtime("json::read-file-seq failed: unexpected EOF"))?;
            if in_string {
                buf.push(b);
                if escape {
                    escape = false;
                    continue;
                }
                if b == b'\\' {
                    escape = true;
                    continue;
                }
                if b == b'"' {
                    in_string = false;
                    if started_string {
                        break;
                    }
                }
                continue;
            }

            if started_struct {
                if b == b'"' {
                    in_string = true;
                    buf.push(b);
                    continue;
                }
                match b {
                    b'{' | b'[' => {
                        depth += 1;
                    }
                    b'}' | b']' => {
                        if depth > 0 {
                            depth -= 1;
                        }
                    }
                    _ => {}
                }
                buf.push(b);
                if depth == 0 {
                    break;
                }
                continue;
            }

            if b.is_ascii_whitespace() || b == b',' || b == b']' || b == b'}' {
                if b == b',' || b == b']' || b == b'}' {
                    self.reader.unread(b);
                }
                break;
            }
            buf.push(b);
        }

        String::from_utf8(buf)
            .map_err(|_| CloveError::runtime("json::read-file-seq failed: invalid utf-8"))
    }
}

impl<R> SeqEngine for JsonSeqEngine<R>
where
    R: Read + Send + 'static,
{
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        self.init()?;
        if self.done {
            return Ok(None);
        }
        match self.mode {
            Some(JsonSeqMode::Array) => self.next_array_item(),
            Some(JsonSeqMode::Object) => self.next_object_item(),
            None => Ok(None),
        }
    }
}

struct ByteReader<R>
where
    R: Read + Send + 'static,
{
    reader: R,
    buf: [u8; 8192],
    pos: usize,
    len: usize,
    pending: Vec<u8>,
}

impl<R> ByteReader<R>
where
    R: Read + Send + 'static,
{
    fn new(reader: R) -> Self {
        Self {
            reader,
            buf: [0; 8192],
            pos: 0,
            len: 0,
            pending: Vec::new(),
        }
    }

    fn next_byte(&mut self) -> Result<Option<u8>, CloveError> {
        if let Some(b) = self.pending.pop() {
            return Ok(Some(b));
        }
        if self.pos >= self.len {
            self.len = self
                .reader
                .read(&mut self.buf)
                .map_err(|e| CloveError::runtime(format!("json::read-file-seq failed: {}", e)))?;
            self.pos = 0;
            if self.len == 0 {
                return Ok(None);
            }
        }
        let b = self.buf[self.pos];
        self.pos += 1;
        Ok(Some(b))
    }

    fn unread(&mut self, b: u8) {
        self.pending.push(b);
    }

    fn read_non_ws(&mut self) -> Result<Option<u8>, CloveError> {
        loop {
            match self.next_byte()? {
                Some(b) if b.is_ascii_whitespace() => continue,
                other => return Ok(other),
            }
        }
    }
}

fn install_fn_meta() {
    fn json_ty() -> TypeKind {
        TypeKind::named("core::Json".to_string())
    }
    fn seq_ty() -> TypeKind {
        TypeKind::named("core::Seq".to_string())
    }
    fn reader_ty() -> TypeKind {
        TypeKind::named("core::Foreign".to_string())
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

    register(
        "json::parse",
        &["[json]"],
        "Parse JSON string into value.",
        vec![overload(vec![TypeKind::Str], None, json_ty())],
    );
    register(
        "json::generate",
        &["[value]"],
        "Generate JSON string from value.",
        vec![overload(vec![TypeKind::Any], None, TypeKind::Str)],
    );
    register(
        "json::generate-pretty",
        &["[value]"],
        "Generate pretty JSON string from value.",
        vec![overload(vec![TypeKind::Any], None, TypeKind::Str)],
    );
    register(
        "json::read-file",
        &["[path]"],
        "Read JSON file into value.",
        vec![overload(vec![TypeKind::Str], None, json_ty())],
    );
    register(
        "json::read-file-seq",
        &["[path]"],
        "Read JSON file lazily. Returns elements for arrays or [key value] pairs for objects.",
        vec![overload(vec![TypeKind::Str], None, seq_ty())],
    );
    register(
        "json::read-seq",
        &["[path-or-reader]"],
        "Read JSON lazily. Accepts a path or reader handle.",
        vec![overload(
            vec![TypeKind::union(vec![TypeKind::Str, reader_ty()])],
            None,
            seq_ty(),
        )],
    );
    register(
        "json::write-file",
        &["[path value]"],
        "Write value as JSON into file. Returns path.",
        vec![overload(
            vec![TypeKind::Str, TypeKind::Any],
            None,
            TypeKind::Str,
        )],
    );
}
