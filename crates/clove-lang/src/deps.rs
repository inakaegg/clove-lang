use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fmt::Write;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use clove_core::package_registry::{clove_home, load_registry, save_registry, PackageId, Registry};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::pkg::{
    ensure_origin_for_install, install_commit_with_registry, install_with_registry,
    origin_url_equivalent, resolve_commit, resolve_install_target,
};

const DEPS_FILE: &str = "clove.deps.json";
const LOCK_FILE: &str = "clove.lock.json";

#[derive(Debug, Deserialize, Serialize)]
struct DepsFile {
    #[serde(default)]
    deps: BTreeMap<String, DepSpec>,
}

#[derive(Debug, Deserialize, Serialize)]
struct DepSpec {
    origin: String,
    #[serde(default)]
    rev: Option<String>,
}

#[derive(Debug, Deserialize)]
struct PackageManifest {
    #[serde(default)]
    deps: BTreeMap<String, DepSpec>,
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub(crate) struct LockFile {
    #[serde(default)]
    pub(crate) deps: BTreeMap<String, LockDep>,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct LockDep {
    pub(crate) origin_url: String,
    pub(crate) commit: String,
    #[serde(default)]
    pub(crate) native_plugins: Vec<LockNativePlugin>,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct LockNativePlugin {
    pub(crate) rel_path: String,
    pub(crate) sha256: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) size: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) mtime_unix: Option<i64>,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SyncOptions {
    pub update: bool,
    pub offline: bool,
    pub force: bool,
}

#[derive(Debug)]
struct PendingDep {
    pkg_id: PackageId,
    origin: String,
    rev: Option<String>,
    requested_by: String,
}

#[derive(Debug)]
struct ResolvedDep {
    origin_url: String,
    commit: String,
    requested_by: String,
    rev: Option<String>,
}

pub fn sync_project(start_dir: &Path, options: SyncOptions) -> Result<PathBuf, String> {
    let project_root = find_project_root(start_dir)
        .ok_or_else(|| format!("{} not found in current or parent directories", DEPS_FILE))?;
    sync_project_at_root(&project_root, options)
}

pub fn sync_project_at_root(project_root: &Path, options: SyncOptions) -> Result<PathBuf, String> {
    let deps_path = project_root.join(DEPS_FILE);
    let lock_path = project_root.join(LOCK_FILE);
    let deps = read_deps_file(&deps_path)?;
    let existing_lock = if lock_path.exists() {
        read_lock_file(&lock_path)?
    } else {
        LockFile::default()
    };

    let clove_home = clove_home();
    let mut registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let mut queue = VecDeque::new();
    for (pkg_key, spec) in deps.deps.iter() {
        let pkg_id = PackageId::parse(pkg_key).map_err(|err| err.to_string())?;
        queue.push_back(PendingDep {
            pkg_id,
            origin: spec.origin.clone(),
            rev: spec.rev.clone(),
            requested_by: "project".to_string(),
        });
    }

    let mut resolved = HashMap::new();
    resolve_dependency_closure(
        &clove_home,
        &mut registry,
        &existing_lock,
        &mut queue,
        options,
        true,
        &mut resolved,
    )?;

    let mut next_lock = LockFile::default();
    for (pkg_key, entry) in resolved {
        let dep =
            lock_dep_from_registry(&registry, &pkg_key, entry.origin_url, entry.commit, None)?;
        next_lock.deps.insert(pkg_key, dep);
    }

    save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;
    write_lock_file(&lock_path, &next_lock)?;
    Ok(lock_path)
}

pub fn update_project_deps(
    start_dir: &Path,
    pkg_id: &PackageId,
    origin: &str,
    rev: Option<&str>,
) -> Result<PathBuf, String> {
    let project_root = find_project_root(start_dir).unwrap_or_else(|| start_dir.to_path_buf());
    let deps_path = project_root.join(DEPS_FILE);
    let mut deps = if deps_path.exists() {
        read_deps_file(&deps_path)?
    } else {
        DepsFile {
            deps: BTreeMap::new(),
        }
    };
    deps.deps.insert(
        pkg_id.key(),
        DepSpec {
            origin: origin.trim().to_string(),
            rev: rev.map(|s| s.to_string()),
        },
    );
    write_deps_file(&deps_path, &deps)?;
    Ok(project_root)
}

pub fn update_project_lock_with_closure(
    project_root: &Path,
    root_pkg: &PackageId,
    root_origin_url: &str,
    root_commit: &str,
    root_install_dir: &Path,
    force: bool,
) -> Result<PathBuf, String> {
    let lock_path = project_root.join(LOCK_FILE);
    let mut lock = if lock_path.exists() {
        read_lock_file(&lock_path)?
    } else {
        LockFile::default()
    };

    let clove_home = clove_home();
    let mut registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let mut queue = VecDeque::new();
    let mut resolved = HashMap::new();

    resolved.insert(
        root_pkg.key(),
        ResolvedDep {
            origin_url: root_origin_url.to_string(),
            commit: root_commit.to_string(),
            requested_by: "project".to_string(),
            rev: None,
        },
    );

    enqueue_manifest_deps(root_install_dir, &root_pkg.key(), &mut queue)?;
    resolve_dependency_closure(
        &clove_home,
        &mut registry,
        &LockFile::default(),
        &mut queue,
        SyncOptions {
            update: true,
            offline: false,
            force,
        },
        false,
        &mut resolved,
    )?;

    let mut updates = BTreeMap::new();
    let root_key = root_pkg.key();
    for (pkg_key, entry) in resolved {
        let override_dir = if pkg_key == root_key {
            Some(root_install_dir)
        } else {
            None
        };
        let dep = lock_dep_from_registry(
            &registry,
            &pkg_key,
            entry.origin_url,
            entry.commit,
            override_dir,
        )?;
        updates.insert(pkg_key, dep);
    }

    merge_lock_entries(&mut lock, updates, force)?;
    save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;
    write_lock_file(&lock_path, &lock)?;
    Ok(lock_path)
}

#[allow(dead_code)]
pub fn update_project_lock_entry(
    project_root: &Path,
    pkg_id: &PackageId,
    origin_url: &str,
    commit: &str,
) -> Result<PathBuf, String> {
    let lock_path = project_root.join(LOCK_FILE);
    let mut lock = if lock_path.exists() {
        read_lock_file(&lock_path)?
    } else {
        LockFile::default()
    };
    let clove_home = clove_home();
    let registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let mut updates = BTreeMap::new();
    let dep = lock_dep_from_registry(
        &registry,
        &pkg_id.key(),
        origin_url.to_string(),
        commit.to_string(),
        None,
    )?;
    updates.insert(pkg_id.key(), dep);
    merge_lock_entries(&mut lock, updates, false)?;
    write_lock_file(&lock_path, &lock)?;
    Ok(lock_path)
}

fn merge_lock_entries(
    lock: &mut LockFile,
    updates: BTreeMap<String, LockDep>,
    force: bool,
) -> Result<(), String> {
    for (pkg_key, dep) in updates {
        if let Some(existing) = lock.deps.get(&pkg_key) {
            if existing.commit != dep.commit
                || !origin_url_equivalent(&existing.origin_url, &dep.origin_url)
            {
                if !force {
                    return Err(format!(
                        "lock entry conflict for {} ({} @ {}) vs ({} @ {})",
                        pkg_key, existing.commit, existing.origin_url, dep.commit, dep.origin_url
                    ));
                }
            }
        }
        lock.deps.insert(pkg_key, dep);
    }
    Ok(())
}

fn resolve_dependency_closure(
    clove_home: &Path,
    registry: &mut Registry,
    existing_lock: &LockFile,
    queue: &mut VecDeque<PendingDep>,
    options: SyncOptions,
    use_existing_lock: bool,
    resolved: &mut HashMap<String, ResolvedDep>,
) -> Result<(), String> {
    while let Some(dep) = queue.pop_front() {
        let pkg_key = dep.pkg_id.key();
        let resolved_target = resolve_install_target(&dep.origin)?;
        let origin_url = resolved_target.url;

        if let Some(existing) = resolved.get(&pkg_key) {
            if !origin_url_equivalent(&existing.origin_url, &origin_url) {
                return Err(format!(
                    "dependency conflict for {}: origin mismatch {} (from {}) vs {} (from {})",
                    pkg_key,
                    existing.origin_url,
                    existing.requested_by,
                    origin_url,
                    dep.requested_by
                ));
            }
            if existing.rev.as_deref() == dep.rev.as_deref() {
                continue;
            }
            if options.offline && !origin_is_local_path(&origin_url) {
                return Err(format!(
                    "offline mode cannot resolve conflict for {} ({})",
                    pkg_key, origin_url
                ));
            }
            let rev_label = dep.rev.as_deref().unwrap_or("HEAD");
            let commit = resolve_commit(&origin_url, dep.rev.as_deref())?.ok_or_else(|| {
                format!("failed to resolve commit for {} ({})", pkg_key, rev_label)
            })?;
            if commit != existing.commit {
                return Err(format!(
                    "dependency conflict for {}: {} (rev {}) resolves to {}, but {} (rev {}) resolves to {}",
                    pkg_key,
                    existing.requested_by,
                    existing.rev.as_deref().unwrap_or("HEAD"),
                    existing.commit,
                    dep.requested_by,
                    rev_label,
                    commit
                ));
            }
            continue;
        }

        ensure_origin_for_install(registry, &dep.pkg_id, &origin_url, options.force)?;
        let install = install_dependency(
            clove_home,
            registry,
            &dep.pkg_id,
            &origin_url,
            dep.rev.as_deref(),
            existing_lock,
            options,
            use_existing_lock,
        )?;

        if let Some(existing) = resolved.get(&pkg_key) {
            if existing.commit != install.commit {
                return Err(format!(
                    "dependency conflict for {}: {} (rev {}) resolves to {}, but {} (rev {}) resolves to {}",
                    pkg_key,
                    existing.requested_by,
                    existing.rev.as_deref().unwrap_or("HEAD"),
                    existing.commit,
                    dep.requested_by,
                    dep.rev.as_deref().unwrap_or("HEAD"),
                    install.commit
                ));
            }
            continue;
        }

        resolved.insert(
            pkg_key.clone(),
            ResolvedDep {
                origin_url: origin_url.clone(),
                commit: install.commit.clone(),
                requested_by: dep.requested_by.clone(),
                rev: dep.rev.clone(),
            },
        );

        enqueue_manifest_deps(&install.install_dir, &pkg_key, queue)?;
    }
    Ok(())
}

fn install_dependency(
    clove_home: &Path,
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    rev: Option<&str>,
    existing_lock: &LockFile,
    options: SyncOptions,
    use_existing_lock: bool,
) -> Result<crate::pkg::InstallResult, String> {
    let mut use_lock = use_existing_lock && !options.update;
    let locked = if use_lock {
        existing_lock.deps.get(&pkg_id.key())
    } else {
        None
    };
    if let Some(entry) = locked {
        if !origin_url_equivalent(&entry.origin_url, origin_url) {
            if !options.force {
                return Err(format!(
                    "origin_url mismatch for {} (use --force)",
                    pkg_id.key()
                ));
            }
            use_lock = false;
        }
    } else {
        use_lock = false;
    }

    if use_lock {
        let locked = locked.expect("locked entry required");
        if options.offline && !origin_is_local_path(origin_url) {
            return Err(format!(
                "offline mode cannot install {} ({})",
                pkg_id.key(),
                origin_url
            ));
        }
        return install_commit_with_registry(
            clove_home,
            registry,
            pkg_id,
            origin_url,
            &locked.commit,
            rev,
            options.force,
            true,
        );
    }

    if options.offline && !origin_is_local_path(origin_url) {
        return Err(format!(
            "offline mode cannot update {} ({})",
            pkg_id.key(),
            origin_url
        ));
    }
    if let Some(commit) = resolve_commit(origin_url, rev)? {
        install_commit_with_registry(
            clove_home,
            registry,
            pkg_id,
            origin_url,
            &commit,
            rev,
            options.force,
            true,
        )
    } else {
        install_with_registry(
            clove_home,
            registry,
            pkg_id,
            origin_url,
            rev,
            options.force,
            true,
            false,
        )
    }
}

fn enqueue_manifest_deps(
    install_dir: &Path,
    parent_pkg_key: &str,
    queue: &mut VecDeque<PendingDep>,
) -> Result<(), String> {
    let deps = read_manifest_deps(install_dir)?;
    for (pkg_key, spec) in deps {
        let pkg_id = PackageId::parse(&pkg_key).map_err(|err| err.to_string())?;
        if pkg_id.key() == parent_pkg_key {
            continue;
        }
        queue.push_back(PendingDep {
            pkg_id,
            origin: spec.origin,
            rev: spec.rev,
            requested_by: parent_pkg_key.to_string(),
        });
    }
    Ok(())
}

fn read_manifest_deps(install_dir: &Path) -> Result<BTreeMap<String, DepSpec>, String> {
    let manifest_path = install_dir.join("clove-pkg.json");
    if !manifest_path.exists() {
        return Ok(BTreeMap::new());
    }
    let content = fs::read_to_string(&manifest_path)
        .map_err(|err| format!("failed to read {}: {}", manifest_path.display(), err))?;
    let manifest: PackageManifest = serde_json::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", manifest_path.display(), err))?;
    Ok(manifest.deps)
}

pub fn load_project_lock_overrides(start_dir: &Path) -> Result<HashMap<String, String>, String> {
    let project_root = match find_project_root(start_dir) {
        Some(root) => root,
        None => return Ok(HashMap::new()),
    };
    let lock_path = project_root.join(LOCK_FILE);
    if !lock_path.exists() {
        return Ok(HashMap::new());
    }
    let lock = read_lock_file(&lock_path)?;
    let mut overrides = HashMap::new();
    for (key, dep) in lock.deps {
        overrides.insert(key, dep.commit);
    }
    Ok(overrides)
}

pub fn load_project_plugin_dirs(start_dir: &Path) -> Result<Vec<PathBuf>, String> {
    let project_root = match find_project_root(start_dir) {
        Some(root) => root,
        None => return Ok(Vec::new()),
    };
    let lock_path = project_root.join(LOCK_FILE);
    if !lock_path.exists() {
        return Ok(Vec::new());
    }
    let lock = read_lock_file(&lock_path)?;
    if lock.deps.is_empty() {
        return Ok(Vec::new());
    }
    let clove_home = clove_home();
    let registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let platform = plugin_platform_tag();
    let mut dirs = Vec::new();
    let mut seen = HashSet::new();
    for (pkg_key, dep) in lock.deps {
        let entry = match registry.packages.get(&pkg_key) {
            Some(entry) => entry,
            None => continue,
        };
        let install = match entry.installs.get(&dep.commit) {
            Some(install) => install,
            None => continue,
        };
        let plugin_root = install.path.join("plugins");
        let platform_dir = plugin_root.join(&platform);
        if platform_dir.is_dir() {
            if seen.insert(platform_dir.clone()) {
                dirs.push(platform_dir);
            }
        } else if plugin_root.is_dir() && seen.insert(plugin_root.clone()) {
            dirs.push(plugin_root);
        }
    }
    Ok(dirs)
}

fn plugin_platform_tag() -> String {
    format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

pub fn find_project_root(start_dir: &Path) -> Option<PathBuf> {
    let mut current = start_dir.to_path_buf();
    loop {
        if current.join(DEPS_FILE).is_file() {
            return Some(current);
        }
        let parent = current.parent()?.to_path_buf();
        if parent == current {
            return None;
        }
        current = parent;
    }
}

fn origin_is_local_path(origin_url: &str) -> bool {
    Path::new(origin_url).exists()
}

fn lock_dep_from_registry(
    registry: &Registry,
    pkg_key: &str,
    origin_url: String,
    commit: String,
    install_dir_override: Option<&Path>,
) -> Result<LockDep, String> {
    let install_dir = match install_dir_override {
        Some(dir) => dir.to_path_buf(),
        None => resolve_install_dir(registry, pkg_key, &commit)?,
    };
    let native_plugins = scan_native_plugins(&install_dir)?;
    Ok(LockDep {
        origin_url,
        commit,
        native_plugins,
    })
}

fn resolve_install_dir(
    registry: &Registry,
    pkg_key: &str,
    commit: &str,
) -> Result<PathBuf, String> {
    let entry = registry
        .packages
        .get(pkg_key)
        .ok_or_else(|| format!("package not installed: {}", pkg_key))?;
    let install = entry.installs.get(commit).ok_or_else(|| {
        format!(
            "package {} does not have commit {} installed",
            pkg_key, commit
        )
    })?;
    Ok(install.path.clone())
}

fn scan_native_plugins(pkg_root: &Path) -> Result<Vec<LockNativePlugin>, String> {
    let plugin_root = pkg_root.join("plugins");
    if !plugin_root.is_dir() {
        return Ok(Vec::new());
    }
    let mut files = Vec::new();
    collect_native_plugin_files(&plugin_root, &mut files)?;
    let pkg_root_canon = fs::canonicalize(pkg_root).unwrap_or_else(|_| pkg_root.to_path_buf());
    let mut entries = Vec::new();
    for path in files {
        entries.push(native_plugin_entry(&path, &pkg_root_canon)?);
    }
    entries.sort_by(|a, b| a.rel_path.cmp(&b.rel_path));
    Ok(entries)
}

fn collect_native_plugin_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|err| format!("failed to read {}: {}", dir.display(), err))?;
    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to read {}: {}", dir.display(), err))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|err| format!("failed to stat {}: {}", path.display(), err))?;
        if file_type.is_dir() {
            collect_native_plugin_files(&path, out)?;
        } else if file_type.is_file() && is_native_plugin_file(&path) {
            out.push(path);
        }
    }
    Ok(())
}

fn is_native_plugin_file(path: &Path) -> bool {
    let Some(ext) = path.extension().and_then(|s| s.to_str()) else {
        return false;
    };
    if cfg!(target_os = "windows") {
        ext.eq_ignore_ascii_case("dll")
    } else if cfg!(target_os = "macos") {
        ext == "dylib"
    } else {
        ext == "so"
    }
}

fn native_plugin_entry(path: &Path, pkg_root: &Path) -> Result<LockNativePlugin, String> {
    let path_canon = fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    let rel = path_canon.strip_prefix(pkg_root).map_err(|_| {
        format!(
            "native plugin path is outside package root: {}",
            path.display()
        )
    })?;
    let rel_path = rel.to_string_lossy().into_owned();
    let sha256 = hash_file_sha256(&path_canon)?;
    let metadata = fs::metadata(&path_canon)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    let size = Some(metadata.len());
    let mtime_unix = metadata
        .modified()
        .ok()
        .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
        .map(|d| d.as_secs() as i64);
    Ok(LockNativePlugin {
        rel_path,
        sha256,
        size,
        mtime_unix,
    })
}

pub(crate) fn hash_file_sha256(path: &Path) -> Result<String, String> {
    let file = fs::File::open(path)
        .map_err(|err| format!("failed to open {}: {}", path.display(), err))?;
    let mut reader = io::BufReader::new(file);
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 8192];
    loop {
        let n = reader
            .read(&mut buf)
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
        if n == 0 {
            break;
        }
        hasher.update(&buf[..n]);
    }
    Ok(hex_lower(&hasher.finalize()))
}

fn hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        let _ = write!(&mut out, "{:02x}", byte);
    }
    out
}

fn read_deps_file(path: &Path) -> Result<DepsFile, String> {
    let content = fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    serde_json::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", path.display(), err))
}

pub(crate) fn read_lock_file(path: &Path) -> Result<LockFile, String> {
    let content = fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    serde_json::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", path.display(), err))
}

fn write_lock_file(path: &Path, lock: &LockFile) -> Result<(), String> {
    let json = serde_json::to_string_pretty(lock).map_err(|err| err.to_string())?;
    fs::write(path, format!("{}\n", json))
        .map_err(|err| format!("failed to write {}: {}", path.display(), err))?;
    Ok(())
}

fn write_deps_file(path: &Path, deps: &DepsFile) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)
                .map_err(|err| format!("failed to create {}: {}", parent.display(), err))?;
        }
    }
    let json = serde_json::to_string_pretty(deps).map_err(|err| err.to_string())?;
    fs::write(path, format!("{}\n", json))
        .map_err(|err| format!("failed to write {}: {}", path.display(), err))?;
    Ok(())
}
