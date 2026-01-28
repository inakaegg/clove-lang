use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use once_cell::sync::Lazy;
use serde::Deserialize;

use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload};
use crate::type_registry::{self, AliasMeta};
use crate::types::TypeKind;

static LOADED_FILES: Lazy<RwLock<HashSet<PathBuf>>> = Lazy::new(|| RwLock::new(HashSet::new()));
static OPAQUE_TYPES: Lazy<RwLock<HashSet<String>>> = Lazy::new(|| RwLock::new(HashSet::new()));

#[derive(Debug, Default)]
pub struct MetaLoadReport {
    pub loaded_files: Vec<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct MetaFile {
    #[serde(default)]
    schema: Option<u32>,
    #[serde(default)]
    types: Vec<MetaType>,
    #[serde(default)]
    fns: Vec<MetaFn>,
}

#[derive(Debug, Deserialize)]
struct MetaType {
    name: String,
    kind: String,
    #[serde(default)]
    doc: Option<String>,
}

#[derive(Debug, Deserialize)]
struct MetaFn {
    name: String,
    #[serde(rename = "type")]
    type_expr: Option<String>,
    #[serde(default)]
    overloads: Vec<String>,
    #[serde(default)]
    doc: Option<String>,
}

pub fn is_opaque_type(name: &str) -> bool {
    let guard = OPAQUE_TYPES.read().unwrap();
    guard.contains(name)
}

pub fn load_meta_from_dirs(dirs: &[PathBuf]) -> Result<MetaLoadReport, CloveError> {
    let mut pending = VecDeque::new();
    let mut seen_dirs = HashSet::new();
    for dir in dirs {
        if !dir.is_dir() {
            continue;
        }
        if seen_dirs.insert(dir.clone()) {
            pending.push_back(dir.clone());
        }
    }
    let mut meta_files = Vec::new();
    while let Some(dir) = pending.pop_front() {
        let entries = match fs::read_dir(&dir) {
            Ok(entries) => entries,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                continue;
            }
            if is_meta_file(&path) {
                meta_files.push(path);
            }
        }
    }
    let mut loaded_files = Vec::new();
    let mut errors = Vec::new();
    for path in meta_files {
        let canonical = fs::canonicalize(&path).unwrap_or(path.clone());
        let already_loaded = LOADED_FILES
            .read()
            .map(|guard| guard.contains(&canonical))
            .unwrap_or(false);
        if already_loaded {
            continue;
        }
        match load_meta_file(&canonical) {
            Ok(()) => {
                if let Ok(mut guard) = LOADED_FILES.write() {
                    guard.insert(canonical.clone());
                }
                loaded_files.push(canonical);
            }
            Err(err) => errors.push(err.to_string()),
        }
    }
    if errors.is_empty() {
        Ok(MetaLoadReport { loaded_files })
    } else {
        Err(CloveError::runtime(format!(
            "failed to load plugin meta:\n{}",
            errors.join("\n")
        )))
    }
}

fn is_meta_file(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
        return false;
    };
    name.ends_with(".meta.json")
}

fn load_meta_file(path: &Path) -> Result<(), CloveError> {
    let source = fs::read_to_string(path).map_err(|err| {
        CloveError::runtime(format!(
            "{}: failed to read meta file: {}",
            path.display(),
            err
        ))
    })?;
    let meta: MetaFile = serde_json::from_str(&source).map_err(|err| {
        CloveError::runtime(format!(
            "{}: meta file is not valid JSON: {}",
            path.display(),
            err
        ))
    })?;
    if let Some(schema) = meta.schema {
        if schema != 1 {
            return Err(CloveError::runtime(format!(
                "{}: unsupported meta schema {}",
                path.display(),
                schema
            )));
        }
    }
    for ty in meta.types {
        register_meta_type(path, ty)?;
    }
    for func in meta.fns {
        register_meta_fn(path, func)?;
    }
    Ok(())
}

fn register_meta_type(path: &Path, meta: MetaType) -> Result<(), CloveError> {
    match meta.kind.as_str() {
        "opaque" => {
            register_opaque_type(&meta.name);
            register_opaque_alias(path, &meta.name, meta.doc)?;
            Ok(())
        }
        other => Err(CloveError::runtime(format!(
            "{}: unknown type kind '{}'",
            path.display(),
            other
        ))),
    }
}

fn register_opaque_type(name: &str) {
    if let Ok(mut guard) = OPAQUE_TYPES.write() {
        guard.insert(name.to_string());
    }
}

fn register_opaque_alias(path: &Path, name: &str, doc: Option<String>) -> Result<(), CloveError> {
    let (ns, local) = split_fqn(name);
    let meta = AliasMeta {
        namespace: ns.to_string(),
        name: local.to_string(),
        doc,
        target: TypeKind::Any,
    };
    type_registry::register_alias(meta).map_err(|err| {
        CloveError::runtime(format!(
            "{}: failed to register type '{}': {}",
            path.display(),
            name,
            err
        ))
    })
}

fn register_meta_fn(path: &Path, meta: MetaFn) -> Result<(), CloveError> {
    let mut overloads = Vec::new();
    if !meta.overloads.is_empty() {
        for expr in meta.overloads {
            overloads.push(parse_overload(path, &meta.name, &expr)?);
        }
    } else if let Some(expr) = meta.type_expr {
        overloads.push(parse_overload(path, &meta.name, &expr)?);
    } else {
        return Err(CloveError::runtime(format!(
            "{}: function '{}' missing type",
            path.display(),
            meta.name
        )));
    }
    if fn_meta::get(&meta.name).is_some() {
        return Ok(());
    }
    let (ns, local) = split_fqn(&meta.name);
    let mut fn_meta = FnMeta::new(ns, local);
    fn_meta.overloads = overloads;
    fn_meta.doc = meta.doc;
    fn_meta::register(fn_meta);
    Ok(())
}

fn parse_overload(path: &Path, name: &str, expr: &str) -> Result<FnOverload, CloveError> {
    let ty = TypeKind::parse(expr).map_err(|err| {
        CloveError::runtime(format!(
            "{}: invalid type for '{}': {}",
            path.display(),
            name,
            err
        ))
    })?;
    match ty {
        TypeKind::Function { params, rest, ret } => Ok(FnOverload {
            arg_types: params,
            rest: rest.map(|boxed| *boxed),
            ret_type: *ret,
            special_op: None,
        }),
        other => Err(CloveError::runtime(format!(
            "{}: function '{}' expects function type, got {}",
            path.display(),
            name,
            other.describe()
        ))),
    }
}

fn split_fqn(name: &str) -> (&str, &str) {
    match name.rsplit_once("::") {
        Some((ns, local)) if !ns.is_empty() && !local.is_empty() => (ns, local),
        _ => ("user", name),
    }
}

#[cfg(test)]
pub fn clear_meta_for_tests() {
    if let Ok(mut guard) = LOADED_FILES.write() {
        guard.clear();
    }
    if let Ok(mut guard) = OPAQUE_TYPES.write() {
        guard.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_meta_dir(label: &str) -> PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        std::env::temp_dir().join(format!("clove-meta-test-{}-{}", label, stamp))
    }

    #[test]
    fn load_meta_reports_invalid_function_type_with_path() {
        let temp_dir = temp_meta_dir("invalid-fn");
        fs::create_dir_all(&temp_dir).expect("create temp dir");
        let meta_path = temp_dir.join("bad.meta.json");
        let meta = r#"
{
  "schema": 1,
  "fns": [
    {"name": "dummy::bad", "type": "Int"}
  ]
}
"#;
        fs::write(&meta_path, meta).expect("write meta");
        clear_meta_for_tests();
        let err = load_meta_from_dirs(&[temp_dir]).expect_err("expected error");
        let msg = err.to_string();
        let meta_path_str = meta_path.to_string_lossy();
        assert!(
            msg.contains(meta_path_str.as_ref()),
            "expected path in error, got: {}",
            msg
        );
        assert!(
            msg.contains("expects function type"),
            "expected function type error, got: {}",
            msg
        );
    }
}
