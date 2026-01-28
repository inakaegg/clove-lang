use once_cell::sync::Lazy;
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

use crate::doc_examples::gen_oop_examples;
use crate::options::EvalOptions;
use crate::package_registry::{clove_home, load_registry};
use crate::runtime::RuntimeCtx;
use crate::symbols::{canonical_symbol_name, doc_lookup_keys};

#[derive(Clone, Debug, Deserialize)]
struct RawDocEntry {
    name: String,
    #[serde(default)]
    signature: Option<String>,
    #[serde(default)]
    doc: Option<String>,
    #[serde(default)]
    origin: Option<String>,
    #[serde(default)]
    examples: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct DocEntry {
    pub name: String,
    pub canonical: String,
    pub signature: Option<String>,
    pub doc: Option<String>,
    pub origin: Option<String>,
    pub examples: Vec<String>,
    pub oop_examples: Vec<String>,
}

#[derive(Clone, Debug)]
struct DocStoreState {
    entries: &'static [DocEntry],
    index: HashMap<String, usize>,
}

const DOCS_FILE: &str = "clove-docs.json";
const LOCK_FILE: &str = "clove.lock.json";

static EXTRA_DOC_DIRS: Lazy<RwLock<Vec<PathBuf>>> = Lazy::new(|| RwLock::new(Vec::new()));

static DOC_STORE: Lazy<RwLock<DocStoreState>> = Lazy::new(|| RwLock::new(build_doc_store(&[])));
static DOC_RUNTIME: Lazy<Mutex<Arc<RuntimeCtx>>> =
    Lazy::new(|| Mutex::new(RuntimeCtx::new(EvalOptions::default(), &[])));

pub fn doc_entries() -> &'static [DocEntry] {
    DOC_STORE.read().map(|guard| guard.entries).unwrap_or(&[])
}

pub fn set_extra_doc_dirs(dirs: Vec<PathBuf>) {
    let normalized = normalize_doc_dirs(dirs);
    if let Ok(mut guard) = EXTRA_DOC_DIRS.write() {
        *guard = normalized.clone();
    }
    reload_doc_store(&normalized);
}

pub fn package_doc_dirs_from_roots(roots: &[PathBuf]) -> Vec<PathBuf> {
    let mut project_roots = HashSet::new();
    for root in roots {
        if let Some(project_root) = find_project_root(root) {
            project_roots.insert(project_root);
        }
    }
    if project_roots.is_empty() {
        return Vec::new();
    }
    let registry = load_registry(&clove_home()).ok();
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for project_root in project_roots {
        let lock_path = project_root.join(LOCK_FILE);
        let lock = match read_lock_file(&lock_path) {
            Some(lock) => lock,
            None => continue,
        };
        for (pkg_key, dep) in lock.deps {
            let mut candidates = Vec::new();
            let origin_path = PathBuf::from(&dep.origin_url);
            if origin_path.is_dir() {
                candidates.push(origin_path);
            }
            if let Some(registry) = registry.as_ref() {
                if let Some(entry) = registry.packages.get(&pkg_key) {
                    if let Some(install) = entry.installs.get(&dep.commit) {
                        if install.src.is_dir() {
                            candidates.push(install.src.clone());
                        } else if install.path.is_dir() {
                            candidates.push(install.path.clone());
                        }
                    }
                }
            }
            for root in candidates {
                let docs_dir = root.join("docs");
                let doc_file = docs_dir.join(DOCS_FILE);
                if doc_file.is_file() {
                    let canonical = fs::canonicalize(&docs_dir).unwrap_or(docs_dir.clone());
                    if seen.insert(canonical.clone()) {
                        out.push(canonical);
                    }
                }
            }
        }
    }
    out
}

pub fn find_doc_entry(symbol: &str) -> Option<&'static DocEntry> {
    let canonical = canonical_symbol_name(symbol);
    let store = DOC_STORE.read().ok()?;
    // 1) Prefer exact match of name and canonical
    if let Some(entry) = store
        .entries
        .iter()
        .find(|e| e.name == symbol || e.canonical == canonical)
    {
        return Some(entry);
    }

    // 2) Otherwise, search using the existing key priority
    for key in doc_lookup_keys(symbol) {
        if let Some(idx) = store.index.get(&key) {
            return store.entries.get(*idx);
        }
    }
    None
}

pub fn format_doc_entry(entry: &DocEntry) -> Option<String> {
    let mut parts = Vec::new();
    if let Some(doc) = entry
        .doc
        .as_ref()
        .map(|text| text.trim())
        .filter(|text| !text.is_empty())
    {
        parts.push(doc.to_string());
    }
    if let Some(section) = format_examples_section("Examples", &entry.examples) {
        parts.push(section);
    }
    if let Some(section) = format_examples_section("OOP Examples", &entry.oop_examples) {
        parts.push(section);
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("\n\n"))
    }
}

fn format_examples_section(title: &str, examples: &[String]) -> Option<String> {
    let lines: Vec<String> = examples
        .iter()
        .map(|ex| ex.trim())
        .filter(|ex| !ex.is_empty())
        .map(|ex| format!("  {}", ex))
        .collect();
    if lines.is_empty() {
        None
    } else {
        Some(format!("{}:\n{}", title, lines.join("\n")))
    }
}

#[derive(Clone, Debug, Deserialize)]
struct LockFile {
    #[serde(default)]
    deps: BTreeMap<String, LockDep>,
}

#[derive(Clone, Debug, Deserialize)]
struct LockDep {
    origin_url: String,
    commit: String,
}

fn read_lock_file(path: &Path) -> Option<LockFile> {
    let content = fs::read_to_string(path).ok()?;
    serde_json::from_str(&content).ok()
}

fn find_project_root(start_dir: &Path) -> Option<PathBuf> {
    let mut current = start_dir;
    loop {
        if current.join(LOCK_FILE).is_file() {
            return Some(current.to_path_buf());
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => return None,
        }
    }
}

fn normalize_doc_dirs(dirs: Vec<PathBuf>) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for dir in dirs {
        if !dir.is_dir() {
            continue;
        }
        let canonical = fs::canonicalize(&dir).unwrap_or(dir.clone());
        if seen.insert(canonical.clone()) {
            out.push(canonical);
        }
    }
    out
}

fn reload_doc_store(extra_dirs: &[PathBuf]) {
    let store = build_doc_store(extra_dirs);
    if let Ok(mut guard) = DOC_STORE.write() {
        *guard = store;
    }
}

fn build_doc_store(extra_dirs: &[PathBuf]) -> DocStoreState {
    let entries = build_doc_entries(extra_dirs);
    let mut index = HashMap::new();
    for (idx, entry) in entries.iter().enumerate() {
        for source in [&entry.canonical, &entry.name] {
            for key in doc_lookup_keys(source) {
                index.entry(key).or_insert(idx);
            }
        }
    }
    DocStoreState { entries, index }
}

fn build_doc_entries(extra_dirs: &[PathBuf]) -> &'static [DocEntry] {
    let json = include_str!("../../../data/clove_docs/clove-docs.json");
    let mut parsed: Vec<RawDocEntry> =
        serde_json::from_str(json).expect("failed to parse clove-docs.json");
    parsed.extend(load_docs_from_dirs(extra_dirs, DOCS_FILE));
    let entries = if RuntimeCtx::try_with_current(|_| Ok(())).is_some() {
        build_doc_entries_with_oop(parsed)
    } else {
        let guard = DOC_RUNTIME.lock().expect("failed to lock docs runtime");
        guard.with_current_ctx(|_| build_doc_entries_with_oop(parsed))
    };
    Box::leak(entries.into_boxed_slice())
}

fn build_doc_entries_with_oop(parsed: Vec<RawDocEntry>) -> Vec<DocEntry> {
    parsed
        .into_iter()
        .map(|entry| {
            let canonical = canonical_symbol_name(&entry.name).into_owned();
            let oop_examples = gen_oop_examples(&entry.examples, entry.origin.as_deref());
            DocEntry {
                name: entry.name,
                canonical,
                signature: entry.signature,
                doc: entry.doc,
                origin: entry.origin,
                examples: entry.examples,
                oop_examples,
            }
        })
        .collect()
}

fn load_docs_from_dirs(dirs: &[PathBuf], file_name: &str) -> Vec<RawDocEntry> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for dir in dirs {
        let path = dir.join(file_name);
        if !path.is_file() {
            continue;
        }
        let canonical = fs::canonicalize(&path).unwrap_or(path.clone());
        if !seen.insert(canonical.clone()) {
            continue;
        }
        let content = match fs::read_to_string(&canonical) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("failed to read {}: {}", canonical.display(), err);
                continue;
            }
        };
        let parsed: Vec<RawDocEntry> = match serde_json::from_str(&content) {
            Ok(parsed) => parsed,
            Err(err) => {
                eprintln!("failed to parse {}: {}", canonical.display(), err);
                continue;
            }
        };
        out.extend(parsed);
    }
    out
}
