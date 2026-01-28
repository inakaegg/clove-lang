use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::env::{new_ref, Env, EnvRef};

#[derive(Debug)]
pub enum NamespaceError {
    AlreadyBound {
        name: String,
        existing: PathBuf,
        requested: PathBuf,
    },
}

impl fmt::Display for NamespaceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NamespaceError::AlreadyBound {
                name,
                existing,
                requested,
            } => write!(
                f,
                "namespace '{}' is already defined in {}, cannot reuse {}",
                name,
                existing.display(),
                requested.display()
            ),
        }
    }
}

impl Error for NamespaceError {}

pub struct NamespaceStore {
    shared_env: EnvRef,
    namespaces: HashMap<String, NamespaceData>,
}

impl NamespaceStore {
    pub fn new(shared_env: EnvRef) -> Self {
        Self {
            shared_env,
            namespaces: HashMap::new(),
        }
    }

    pub fn ensure(&mut self, name: &str) -> &mut NamespaceData {
        let shared = self.shared_env.clone();
        self.namespaces
            .entry(name.to_string())
            .or_insert_with(|| NamespaceData::new(name, shared.clone()))
    }

    pub fn get(&self, name: &str) -> Option<&NamespaceData> {
        self.namespaces.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut NamespaceData> {
        self.namespaces.get_mut(name)
    }

    pub fn shared_env(&self) -> EnvRef {
        self.shared_env.clone()
    }

    pub fn names(&self) -> Vec<String> {
        self.namespaces.keys().cloned().collect()
    }

    pub fn namespace_for_file(&self, path: &Path) -> Option<String> {
        self.namespaces.iter().find_map(|(name, data)| {
            data.file_path
                .as_ref()
                .filter(|p| *p == path)
                .map(|_| name.clone())
        })
    }

    pub fn set_type_alias(&mut self, ns: &str, alias: &str, target: &str) {
        let entry = self.ensure(ns);
        entry
            .type_aliases
            .insert(alias.to_string(), target.to_string());
    }

    pub fn type_alias_target(&self, ns: &str, alias: &str) -> Option<String> {
        self.namespaces
            .get(ns)
            .and_then(|entry| entry.type_aliases.get(alias).cloned())
    }

    pub fn register_file(&mut self, name: &str, path: &Path) -> Result<(), NamespaceError> {
        let entry = self.ensure(name);
        if let Some(existing) = &entry.file_path {
            if existing != path {
                return Err(NamespaceError::AlreadyBound {
                    name: name.to_string(),
                    existing: existing.clone(),
                    requested: path.to_path_buf(),
                });
            }
        } else {
            entry.file_path = Some(path.to_path_buf());
        }
        Ok(())
    }
}

pub struct NamespaceData {
    env: EnvRef,
    pub file_path: Option<PathBuf>,
    pub root_dir: Option<PathBuf>,
    pub imported_symbols: HashSet<String>,
    pub imported_types: HashSet<String>,
    pub public_exports: HashMap<String, Value>,
    pub private_names: HashSet<String>,
    pub type_aliases: HashMap<String, String>,
    pub warned_path_mismatch: bool,
}

impl NamespaceData {
    fn new(_name: &str, shared_env: EnvRef) -> Self {
        let env = new_ref(Env::new_child(shared_env));
        Self {
            env,
            file_path: None,
            root_dir: None,
            imported_symbols: HashSet::new(),
            imported_types: HashSet::new(),
            public_exports: HashMap::new(),
            private_names: HashSet::new(),
            type_aliases: HashMap::new(),
            warned_path_mismatch: false,
        }
    }

    pub fn env(&self) -> EnvRef {
        self.env.clone()
    }

    pub fn set_root_if_missing(&mut self, dir: &Path) {
        if self.root_dir.is_none() {
            self.root_dir = Some(dir.to_path_buf());
        }
    }

    pub fn record_imported_aliases(&mut self, symbols: &[String]) {
        for symbol in symbols {
            self.imported_symbols.insert(symbol.clone());
        }
    }

    pub fn record_imported_types(&mut self, types: &[String]) {
        for name in types {
            self.imported_types.insert(name.clone());
        }
    }

    pub fn mark_private(&mut self, name: impl Into<String>) {
        self.private_names.insert(name.into());
    }
}
