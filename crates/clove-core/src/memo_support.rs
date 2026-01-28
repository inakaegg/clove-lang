use crate::ast::{
    DurationValue, Form, FormKind, HashMap, Key, LocalDefn, MapItem, RegexValue, Value, Vector,
};
use crate::error::CloveError;
use im::HashSet as ImHashSet;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static TMP_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug)]
pub enum SerializationError {
    Unsupported(&'static str),
}

impl SerializationError {
    pub fn unsupported(msg: &'static str) -> Self {
        SerializationError::Unsupported(msg)
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(tag = "type", content = "value")]
enum SerializableValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    Bytes(SerializableBytes),
    Vector(Vec<SerializableValue>),
    List(Vec<SerializableValue>),
    Map(Vec<(SerializableKey, SerializableValue)>),
    Set(Vec<SerializableValue>),
    Symbol(String),
    Regex(String),
    Duration(i128),
}

#[derive(Serialize, Deserialize, Clone)]
struct SerializableBytes {
    data: String,
    list: bool,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(tag = "type", content = "value")]
enum SerializableKey {
    Keyword(String),
    Symbol(String),
    String(String),
    Number(i64),
    Bool(bool),
}

#[derive(Serialize, Deserialize)]
struct DiskEntry {
    saved_at_ms: i64,
    ttl_ms: Option<i64>,
    value: SerializableValue,
    value_repr: Option<String>,
}

pub(crate) struct LoadedEntry {
    pub value: Value,
    pub saved_at_ms: i64,
}

pub(crate) struct PersistentStore {
    dir: PathBuf,
    ttl_ms: Option<i64>,
    args_warned: AtomicBool,
    value_warned: AtomicBool,
}

impl PersistentStore {
    pub fn new(dir: PathBuf, ttl_ms: Option<i64>) -> Self {
        Self {
            dir,
            ttl_ms,
            args_warned: AtomicBool::new(false),
            value_warned: AtomicBool::new(false),
        }
    }

    pub fn dir(&self) -> &Path {
        &self.dir
    }

    pub fn try_load(&self, key: &str) -> Result<Option<LoadedEntry>, CloveError> {
        let path = self.entry_path(key);
        let data = match fs::read_to_string(&path) {
            Ok(data) => data,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(err) => {
                return Err(CloveError::runtime(format!(
                    "failed to read memo cache: {}",
                    err
                )))
            }
        };
        let mut entry: DiskEntry = match serde_json::from_str(&data) {
            Ok(entry) => entry,
            Err(err) => {
                eprintln!(
                    "memo: removing corrupted cache entry '{}': {}",
                    path.display(),
                    err
                );
                let _ = fs::remove_file(&path);
                return Ok(None);
            }
        };
        if let Some(ttl_ms) = entry.ttl_ms {
            let age = now_ms().saturating_sub(entry.saved_at_ms);
            if age > ttl_ms {
                let _ = fs::remove_file(&path);
                return Ok(None);
            }
        }
        if entry.ttl_ms != self.ttl_ms {
            entry.ttl_ms = self.ttl_ms;
            entry.saved_at_ms = now_ms();
            self.write_entry(key, &entry)?;
        }
        let saved_at = entry.saved_at_ms;
        match entry.value.into_value() {
            Ok(val) => Ok(Some(LoadedEntry {
                value: val,
                saved_at_ms: saved_at,
            })),
            Err(err) => {
                eprintln!(
                    "memo: removing unreadable cache entry '{}': {}",
                    path.display(),
                    err
                );
                let _ = fs::remove_file(&path);
                Ok(None)
            }
        }
    }

    pub fn save(&self, key: &str, value: &Value) -> Result<(), CloveError> {
        let serializable = match serialize_value(value) {
            Ok(serializable) => serializable,
            Err(SerializationError::Unsupported(msg)) => {
                self.warn_value_unserializable(msg);
                return Ok(());
            }
        };
        let entry = DiskEntry {
            saved_at_ms: now_ms(),
            ttl_ms: self.ttl_ms,
            value: serializable,
            value_repr: value_repr_for(value),
        };
        self.write_entry(key, &entry)
    }

    pub fn warn_args_unserializable(&self, reason: &str) {
        warn_once(
            &self.args_warned,
            &format!(
                "memo: some arguments could not be serialized for store '{}'; falling back to memory cache only ({})",
                self.dir.display(),
                reason
            ),
        );
    }

    pub fn warn_value_unserializable(&self, reason: &str) {
        warn_once(
            &self.value_warned,
            &format!(
                "memo: return value could not be serialized for store '{}'; result kept in memory only ({})",
                self.dir.display(),
                reason
            ),
        );
    }

    fn entry_path(&self, key: &str) -> PathBuf {
        self.dir.join(format!("{}.json", key))
    }

    fn write_entry(&self, key: &str, entry: &DiskEntry) -> Result<(), CloveError> {
        ensure_parent(&self.dir)?;
        let tmp_name = format!(
            "{}.tmp-{}-{}",
            key,
            std::process::id(),
            TMP_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let tmp_path = self.dir.join(tmp_name);
        let bytes = serde_json::to_vec(entry).map_err(|err| {
            CloveError::runtime(format!("failed to serialize memo entry: {}", err))
        })?;
        {
            let mut file = std::fs::File::create(&tmp_path)
                .map_err(|e| CloveError::runtime(format!("failed to write memo cache: {}", e)))?;
            file.write_all(&bytes)
                .map_err(|e| CloveError::runtime(format!("failed to write memo cache: {}", e)))?;
            file.sync_all()
                .map_err(|e| CloveError::runtime(format!("failed to sync memo cache: {}", e)))?;
        }
        fs::rename(&tmp_path, self.entry_path(key))
            .map_err(|e| CloveError::runtime(format!("failed to finalize memo cache entry: {}", e)))
    }
}

pub(crate) fn canonical_key_for_args(args: &[Value]) -> Result<String, SerializationError> {
    let serializable = SerializableValue::Vector(
        args.iter()
            .map(serialize_value)
            .collect::<Result<Vec<_>, _>>()?,
    );
    Ok(hash_serialized(&serializable))
}

fn serialize_value(value: &Value) -> Result<SerializableValue, SerializationError> {
    match value {
        Value::Int(n) => Ok(SerializableValue::Int(*n)),
        Value::Float(f) => Ok(SerializableValue::Float(*f)),
        Value::String(s) => Ok(SerializableValue::String(s.clone())),
        Value::Bool(b) => Ok(SerializableValue::Bool(*b)),
        Value::Nil => Ok(SerializableValue::Nil),
        Value::Vector(items) => {
            if let Some(bytes) = values_to_bytes(items) {
                Ok(SerializableValue::Bytes(SerializableBytes {
                    data: base64_encode(&bytes),
                    list: false,
                }))
            } else {
                Ok(SerializableValue::Vector(
                    items
                        .iter()
                        .map(serialize_value)
                        .collect::<Result<Vec<_>, _>>()?,
                ))
            }
        }
        Value::List(items) => {
            if let Some(bytes) = values_to_bytes(items) {
                Ok(SerializableValue::Bytes(SerializableBytes {
                    data: base64_encode(&bytes),
                    list: true,
                }))
            } else {
                Ok(SerializableValue::List(
                    items
                        .iter()
                        .map(serialize_value)
                        .collect::<Result<Vec<_>, _>>()?,
                ))
            }
        }
        Value::SortedMap(map) => {
            let mut pairs: Vec<(SerializableKey, SerializableValue)> = map
                .entries
                .iter()
                .map(|(k, v)| Ok((SerializableKey::from_key(k), serialize_value(v)?)))
                .collect::<Result<_, SerializationError>>()?;
            pairs.sort_by(|(left, _), (right, _)| left.sort_key().cmp(&right.sort_key()));
            Ok(SerializableValue::Map(pairs))
        }
        Value::Map(map) => {
            let mut pairs: Vec<(SerializableKey, SerializableValue)> = map
                .iter()
                .map(|(k, v)| Ok((SerializableKey::from_key(k), serialize_value(v)?)))
                .collect::<Result<_, SerializationError>>()?;
            pairs.sort_by(|(left, _), (right, _)| left.sort_key().cmp(&right.sort_key()));
            Ok(SerializableValue::Map(pairs))
        }
        Value::SortedSet(set) => {
            let mut items: Vec<SerializableValue> = set
                .entries
                .iter()
                .map(serialize_value)
                .collect::<Result<Vec<_>, _>>()?;
            items.sort_by(|a, b| canonical_json(a).cmp(&canonical_json(b)));
            Ok(SerializableValue::Set(items))
        }
        Value::Set(set) => {
            let mut items: Vec<SerializableValue> = set
                .iter()
                .map(serialize_value)
                .collect::<Result<Vec<_>, _>>()?;
            items.sort_by(|a, b| canonical_json(a).cmp(&canonical_json(b)));
            Ok(SerializableValue::Set(items))
        }
        Value::Symbol(s) => Ok(SerializableValue::Symbol(s.clone())),
        Value::Regex(re) => Ok(SerializableValue::Regex(re.pattern.clone())),
        Value::Duration(d) => Ok(SerializableValue::Duration(d.as_nanos())),
        Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_) => {
            Err(SerializationError::unsupported(
                "transient collections cannot be serialized for memo cache",
            ))
        }
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => {
            Err(SerializationError::unsupported(
                "mutable collections cannot be serialized for memo cache",
            ))
        }
        Value::Atom(_)
        | Value::Chan(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_)
        | Value::Agent(_)
        | Value::Delay(_)
        | Value::NativeBuf { .. }
        | Value::Seq(_) => Err(SerializationError::unsupported(
            "seq or concurrency handles cannot be serialized for memo cache",
        )),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. } => Err(SerializationError::unsupported(
            "functions cannot be serialized for memo cache",
        )),
        Value::Foreign(_) | Value::ForeignCallable { .. } => Err(SerializationError::unsupported(
            "foreign values cannot be serialized for memo cache",
        )),
    }
}

impl SerializableValue {
    fn into_value(self) -> Result<Value, String> {
        match self {
            SerializableValue::Int(n) => Ok(Value::Int(n)),
            SerializableValue::Float(f) => Ok(Value::Float(f)),
            SerializableValue::String(s) => Ok(Value::String(s)),
            SerializableValue::Bool(b) => Ok(Value::Bool(b)),
            SerializableValue::Nil => Ok(Value::Nil),
            SerializableValue::Bytes(bytes) => {
                let decoded = base64_decode(&bytes.data)?;
                Ok(bytes_to_value(decoded, bytes.list))
            }
            SerializableValue::Vector(items) => Ok(Value::Vector(Vector::from(
                items
                    .into_iter()
                    .map(|v| v.into_value())
                    .collect::<Result<Vec<_>, _>>()?,
            ))),
            SerializableValue::List(items) => Ok(Value::List(Vector::from(
                items
                    .into_iter()
                    .map(|v| v.into_value())
                    .collect::<Result<Vec<_>, _>>()?,
            ))),
            SerializableValue::Map(entries) => {
                let mut map = HashMap::new();
                for (k, v) in entries {
                    map.insert(k.into_key(), v.into_value()?);
                }
                Ok(Value::Map(map))
            }
            SerializableValue::Set(items) => {
                let mut set = ImHashSet::new();
                for item in items {
                    set.insert(item.into_value()?);
                }
                Ok(Value::Set(set))
            }
            SerializableValue::Symbol(s) => Ok(Value::Symbol(s)),
            SerializableValue::Regex(pattern) => {
                let regex = RegexValue::new(pattern).map_err(|e| e.to_string())?;
                Ok(Value::Regex(regex))
            }
            SerializableValue::Duration(nanos) => {
                let duration = DurationValue::from_nanos(nanos).map_err(|e| e.to_string())?;
                Ok(Value::Duration(duration))
            }
        }
    }
}

fn values_to_bytes(values: &Vector<Value>) -> Option<Vec<u8>> {
    let mut out = Vec::with_capacity(values.len());
    for item in values {
        match item {
            Value::Int(n) if (0..=255).contains(n) => out.push(*n as u8),
            _ => return None,
        }
    }
    Some(out)
}

fn byte_len(values: &Vector<Value>) -> Option<usize> {
    for item in values {
        match item {
            Value::Int(n) if (0..=255).contains(n) => {}
            _ => return None,
        }
    }
    Some(values.len())
}

fn value_repr_for(value: &Value) -> Option<String> {
    match value {
        Value::Vector(items) => byte_len(items).map(|len| format!("<bytes {}B>", len)),
        Value::List(items) => byte_len(items).map(|len| format!("<bytes {}B>", len)),
        _ => None,
    }
    .or_else(|| Some(value.to_string()))
}

fn bytes_to_value(bytes: Vec<u8>, list: bool) -> Value {
    let mut vec = Vector::new();
    for b in bytes {
        vec.push_back(Value::Int(b as i64));
    }
    if list {
        Value::List(vec)
    } else {
        Value::Vector(vec)
    }
}

fn base64_encode(bytes: &[u8]) -> String {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    if bytes.is_empty() {
        return String::new();
    }
    let mut out = String::with_capacity(((bytes.len() + 2) / 3) * 4);
    let mut i = 0;
    while i < bytes.len() {
        let b0 = bytes[i];
        let b1 = if i + 1 < bytes.len() { bytes[i + 1] } else { 0 };
        let b2 = if i + 2 < bytes.len() { bytes[i + 2] } else { 0 };
        let n = ((b0 as u32) << 16) | ((b1 as u32) << 8) | (b2 as u32);
        let i0 = ((n >> 18) & 0x3f) as usize;
        let i1 = ((n >> 12) & 0x3f) as usize;
        let i2 = ((n >> 6) & 0x3f) as usize;
        let i3 = (n & 0x3f) as usize;
        out.push(TABLE[i0] as char);
        out.push(TABLE[i1] as char);
        if i + 1 < bytes.len() {
            out.push(TABLE[i2] as char);
        } else {
            out.push('=');
        }
        if i + 2 < bytes.len() {
            out.push(TABLE[i3] as char);
        } else {
            out.push('=');
        }
        i += 3;
    }
    out
}

fn base64_decode(input: &str) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    let mut buf = [0u8; 4];
    let mut len = 0;
    for b in input.bytes() {
        if b.is_ascii_whitespace() {
            continue;
        }
        let val = match b {
            b'A'..=b'Z' => Some(b - b'A'),
            b'a'..=b'z' => Some(26 + (b - b'a')),
            b'0'..=b'9' => Some(52 + (b - b'0')),
            b'+' => Some(62),
            b'/' => Some(63),
            b'=' => Some(64),
            _ => None,
        }
        .ok_or_else(|| "invalid base64 char".to_string())?;
        buf[len] = val;
        len += 1;
        if len == 4 {
            let pad = buf.iter().rev().take_while(|v| **v == 64).count();
            if pad > 2 {
                return Err("invalid base64 padding".to_string());
            }
            if (buf[0] == 64) || (buf[1] == 64) || (pad == 2 && buf[2] != 64) {
                return Err("invalid base64 padding".to_string());
            }
            let v0 = buf[0] as u32;
            let v1 = buf[1] as u32;
            let v2 = if buf[2] == 64 { 0 } else { buf[2] as u32 };
            let v3 = if buf[3] == 64 { 0 } else { buf[3] as u32 };
            let n = (v0 << 18) | (v1 << 12) | (v2 << 6) | v3;
            out.push(((n >> 16) & 0xff) as u8);
            if pad < 2 {
                out.push(((n >> 8) & 0xff) as u8);
            }
            if pad < 1 {
                out.push((n & 0xff) as u8);
            }
            len = 0;
        }
    }
    if len != 0 {
        return Err("invalid base64 length".to_string());
    }
    Ok(out)
}

impl SerializableKey {
    fn from_key(key: &Key) -> Self {
        match key {
            Key::Keyword(s) => SerializableKey::Keyword(s.clone()),
            Key::Symbol(s) => SerializableKey::Symbol(s.clone()),
            Key::String(s) => SerializableKey::String(s.clone()),
            Key::Number(n) => SerializableKey::Number(*n),
            Key::Bool(b) => SerializableKey::Bool(*b),
        }
    }

    fn into_key(self) -> Key {
        match self {
            SerializableKey::Keyword(s) => Key::Keyword(s),
            SerializableKey::Symbol(s) => Key::Symbol(s),
            SerializableKey::String(s) => Key::String(s),
            SerializableKey::Number(n) => Key::Number(n),
            SerializableKey::Bool(b) => Key::Bool(b),
        }
    }

    fn sort_key(&self) -> String {
        match self {
            SerializableKey::Keyword(s) => format!(":{}", s),
            SerializableKey::Symbol(s) => format!("::{}", s),
            SerializableKey::String(s) => format!("\"{}\"", s),
            SerializableKey::Number(n) => format!("{}", n),
            SerializableKey::Bool(b) => format!("{}", b),
        }
    }
}

fn canonical_json(value: &SerializableValue) -> String {
    serde_json::to_string(value).unwrap_or_default()
}

fn hash_serialized(value: &SerializableValue) -> String {
    let json = serde_json::to_vec(value).unwrap_or_default();
    let mut hasher = Sha256::new();
    hasher.update(&json);
    format!("{:x}", hasher.finalize())
}

fn ensure_parent(dir: &Path) -> Result<(), CloveError> {
    if dir.exists() {
        return Ok(());
    }
    fs::create_dir_all(dir)
        .map_err(|e| CloveError::runtime(format!("cannot create cache dir: {}", e)))
}

fn warn_once(flag: &AtomicBool, msg: &str) {
    if !flag.swap(true, Ordering::SeqCst) {
        eprintln!("{}", msg);
    }
}

pub(crate) fn now_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|dur| dur.as_millis() as i64)
        .unwrap_or(0)
}

pub(crate) fn default_cache_root() -> PathBuf {
    if let Ok(custom) = std::env::var("CLOVE_CACHE_DIR") {
        if !custom.is_empty() {
            return PathBuf::from(custom);
        }
    }
    let home = home_dir().unwrap_or_else(|| PathBuf::from("."));
    home.join(".clove").join("cache")
}

pub(crate) fn resolve_store_path(name: &str) -> PathBuf {
    if name.starts_with("~/") {
        if let Some(home) = home_dir() {
            return home.join(name.trim_start_matches("~/"));
        }
    }
    let path = Path::new(name);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        default_cache_root().join(path)
    }
}

fn home_dir() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("USERPROFILE").map(PathBuf::from))
}

pub(crate) fn derive_lambda_store_name(value: &Value) -> Option<String> {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            ..
        } => {
            let mut hasher = Sha256::new();
            hash_clause(&mut hasher, params, rest, body);
            hash_local_defns(&mut hasher, local_defns);
            Some(format!("{:x}", hasher.finalize()))
        }
        Value::MultiLambda { clauses, .. } => {
            if clauses.is_empty() {
                return None;
            }
            let mut hasher = Sha256::new();
            for clause in clauses {
                hash_clause(&mut hasher, &clause.params, &clause.rest, &clause.body);
                hash_local_defns(&mut hasher, &clause.local_defns);
            }
            Some(format!("{:x}", hasher.finalize()))
        }
        _ => None,
    }
}

fn hash_clause(hasher: &mut Sha256, params: &[String], rest: &Option<String>, body: &[Form]) {
    for param in params {
        hasher.update(param.as_bytes());
    }
    if let Some(rest) = rest {
        hasher.update(rest.as_bytes());
    }
    for form in body {
        hasher.update(form_signature(form).as_bytes());
    }
}

fn hash_local_defns(hasher: &mut Sha256, local_defns: &[LocalDefn]) {
    for defn in local_defns {
        hasher.update(defn.name.as_bytes());
        for clause in &defn.clauses {
            hash_clause(hasher, &clause.params, &clause.rest, &clause.body);
            hash_local_defns(hasher, &clause.local_defns);
        }
    }
}

fn form_signature(form: &Form) -> String {
    match &form.kind {
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            let mut sig = String::new();
            sig.push('(');
            for item in items {
                sig.push_str(&form_signature(item));
                sig.push('|');
            }
            sig.push(')');
            sig
        }
        FormKind::Map(entries) => {
            let mut sig = String::new();
            sig.push('{');
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        sig.push_str(&form_signature(k));
                        sig.push_str(&form_signature(v));
                    }
                    MapItem::Spread(expr) => {
                        sig.push('*');
                        sig.push_str(&form_signature(expr));
                    }
                }
            }
            sig.push('}');
            sig
        }
        other => format!("{:?}", other),
    }
}

pub(crate) fn ttl_value_ms(ttl: &Value) -> Result<Option<i64>, CloveError> {
    match ttl {
        Value::Nil => Ok(None),
        Value::Int(n) => {
            if *n < 0 {
                Err(CloveError::runtime("ttl must be non-negative"))
            } else {
                Ok(Some(n.saturating_mul(1000)))
            }
        }
        Value::Float(f) => {
            if *f < 0.0 {
                Err(CloveError::runtime("ttl must be non-negative"))
            } else {
                let millis = f * 1000.0;
                if millis > i64::MAX as f64 {
                    Err(CloveError::runtime("ttl is too large"))
                } else {
                    Ok(Some(millis.round() as i64))
                }
            }
        }
        Value::Duration(d) => d.to_millis_i64().map(Some),
        other => Err(CloveError::runtime(format!(
            "invalid ttl value: {}",
            other.type_name()
        ))),
    }
}
