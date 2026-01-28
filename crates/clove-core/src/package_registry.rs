use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageId {
    pub owner: String,
    pub pkg: String,
}

impl PackageId {
    pub fn parse(raw: &str) -> Result<Self, RegistryError> {
        let mut parts = raw.split('/');
        let owner = parts
            .next()
            .filter(|s| !s.is_empty())
            .ok_or_else(|| RegistryError::InvalidPackageId(raw.to_string()))?;
        let pkg = parts
            .next()
            .filter(|s| !s.is_empty())
            .ok_or_else(|| RegistryError::InvalidPackageId(raw.to_string()))?;
        if parts.next().is_some() {
            return Err(RegistryError::InvalidPackageId(raw.to_string()));
        }
        Ok(Self {
            owner: owner.to_string(),
            pkg: pkg.to_string(),
        })
    }

    pub fn key(&self) -> String {
        format!("{}/{}", self.owner, self.pkg)
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.owner, self.pkg)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Install {
    pub commit: String,
    pub path: PathBuf,
    pub src: PathBuf,
    #[serde(default)]
    pub installed_at: i64,
    #[serde(default)]
    pub rev_spec: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PackageEntry {
    #[serde(default)]
    pub origin_url: Option<String>,
    #[serde(default)]
    pub installs: HashMap<String, Install>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Registry {
    #[serde(default)]
    pub packages: HashMap<String, PackageEntry>,
}

impl Registry {
    pub fn resolve_src_roots(
        &self,
        overrides: &HashMap<String, String>,
    ) -> Result<Vec<PathBuf>, RegistryError> {
        let mut keys: Vec<&String> = self.packages.keys().collect();
        keys.sort();
        let mut roots = Vec::new();
        for (key, commit) in overrides {
            if !self.packages.contains_key(key) {
                return Err(RegistryError::MissingPackage(key.clone()));
            }
            let entry = self.packages.get(key).unwrap();
            if !entry.installs.contains_key(commit) {
                return Err(RegistryError::MissingInstall {
                    package: key.clone(),
                    commit: commit.clone(),
                });
            }
        }
        for key in keys {
            let entry = match self.packages.get(key) {
                Some(entry) => entry,
                None => continue,
            };
            let commit = overrides
                .get(key)
                .cloned()
                .or_else(|| pick_latest_install(&entry.installs));
            let commit = match commit {
                Some(commit) => commit,
                None => continue,
            };
            let install = match entry.installs.get(&commit) {
                Some(install) => install,
                None => continue,
            };
            roots.push(install.src.clone());
        }
        Ok(roots)
    }
}

#[derive(Debug)]
pub enum RegistryError {
    Io(std::io::Error),
    Json(serde_json::Error),
    InvalidPackageId(String),
    MissingPackage(String),
    MissingInstall { package: String, commit: String },
}

impl fmt::Display for RegistryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegistryError::Io(err) => write!(f, "io error: {}", err),
            RegistryError::Json(err) => write!(f, "json error: {}", err),
            RegistryError::InvalidPackageId(raw) => {
                write!(f, "invalid package id: {}", raw)
            }
            RegistryError::MissingPackage(pkg) => {
                write!(f, "package not installed: {}", pkg)
            }
            RegistryError::MissingInstall { package, commit } => {
                write!(
                    f,
                    "package {} does not have commit {} installed",
                    package, commit
                )
            }
        }
    }
}

fn pick_latest_install(installs: &HashMap<String, Install>) -> Option<String> {
    let mut best: Option<(&String, &Install)> = None;
    for (commit, install) in installs {
        match best {
            None => best = Some((commit, install)),
            Some((best_commit, best_install)) => {
                let cmp = install
                    .installed_at
                    .cmp(&best_install.installed_at)
                    .then_with(|| commit.cmp(best_commit));
                if cmp == std::cmp::Ordering::Greater {
                    best = Some((commit, install));
                }
            }
        }
    }
    best.map(|(commit, _)| commit.clone())
}

impl std::error::Error for RegistryError {}

impl From<std::io::Error> for RegistryError {
    fn from(err: std::io::Error) -> Self {
        RegistryError::Io(err)
    }
}

impl From<serde_json::Error> for RegistryError {
    fn from(err: serde_json::Error) -> Self {
        RegistryError::Json(err)
    }
}

pub fn clove_home() -> PathBuf {
    if let Some(custom) = std::env::var_os("CLOVE_HOME") {
        if !custom.is_empty() {
            return PathBuf::from(custom);
        }
    }
    let home = home_dir().unwrap_or_else(|| PathBuf::from("."));
    home.join(".clove")
}

pub fn registry_path(clove_home: &Path) -> PathBuf {
    clove_home.join("pkgs").join("registry.json")
}

pub fn load_registry(clove_home: &Path) -> Result<Registry, RegistryError> {
    let path = registry_path(clove_home);
    if !path.exists() {
        return Ok(Registry::default());
    }
    let content = fs::read_to_string(&path)?;
    let registry: Registry = serde_json::from_str(&content)?;
    Ok(registry)
}

pub fn save_registry(clove_home: &Path, registry: &Registry) -> Result<(), RegistryError> {
    let path = registry_path(clove_home);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(registry)?;
    fs::write(&path, format!("{}\n", json))?;
    Ok(())
}

fn home_dir() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("USERPROFILE").map(PathBuf::from))
}
