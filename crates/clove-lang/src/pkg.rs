use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use clove_core::package_registry::{
    clove_home, load_registry, save_registry, Install, PackageEntry, PackageId, Registry,
};
use serde_json::{Map, Value};

use crate::deps;
use crate::plugin;

pub fn run_pkg(args: Vec<String>) -> Result<(), String> {
    let mut args = args;
    let sub = args.first().map(|s| s.as_str()).unwrap_or("");
    match sub {
        "install" => {
            args.remove(0);
            run_install(args)
        }
        "list" => {
            args.remove(0);
            run_list(args)
        }
        "update" => {
            args.remove(0);
            run_update(args)
        }
        "sync" => {
            args.remove(0);
            run_sync(args)
        }
        "uninstall" => {
            args.remove(0);
            run_uninstall(args)
        }
        "-h" | "--help" | "" => {
            print_pkg_help();
            Ok(())
        }
        other => Err(format!("unknown pkg subcommand: {}", other)),
    }
}

fn print_pkg_help() {
    println!("Usage:");
    println!("  clove pkg install <git-url> [--pkg owner/pkg] [--rev <ref>] [--force] [--project]");
    println!("  clove pkg list");
    println!("  clove pkg update <owner/pkg> [--rev <ref>] [--force]");
    println!("  clove pkg sync [--update] [--offline] [--force]");
    println!("  clove pkg uninstall <owner/pkg> [--commit <sha>] [--all]");
}

fn run_install(args: Vec<String>) -> Result<(), String> {
    let mut url: Option<String> = None;
    let mut pkg_id: Option<PackageId> = None;
    let mut rev: Option<String> = None;
    let mut force = false;
    let mut project = false;
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "--pkg" => {
                if idx + 1 >= args.len() {
                    return Err("--pkg requires a value".into());
                }
                pkg_id = Some(PackageId::parse(&args[idx + 1]).map_err(|err| err.to_string())?);
                idx += 2;
            }
            "--rev" => {
                if idx + 1 >= args.len() {
                    return Err("--rev requires a value".into());
                }
                rev = Some(args[idx + 1].clone());
                idx += 2;
            }
            "--force" => {
                force = true;
                idx += 1;
            }
            "--project" => {
                project = true;
                idx += 1;
            }
            "-h" | "--help" => {
                print_pkg_help();
                return Ok(());
            }
            other => {
                if url.is_none() {
                    url = Some(other.to_string());
                    idx += 1;
                } else {
                    return Err(format!("unknown argument: {}", other));
                }
            }
        }
    }

    let raw_url = url.ok_or_else(|| "git url is required".to_string())?;
    let resolved = resolve_install_target(&raw_url)?;
    let pkg_id = match pkg_id {
        Some(id) => id,
        None => resolved
            .inferred_pkg_id
            .ok_or_else(|| "--pkg is required for non-GitHub URLs".to_string())?,
    };

    let origin_url = resolved.url;
    let clove_home = clove_home();
    let mut registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    ensure_origin_for_install(&mut registry, &pkg_id, &origin_url, force)?;
    let result = install_with_registry(
        &clove_home,
        &mut registry,
        &pkg_id,
        &origin_url,
        rev.as_deref(),
        force,
        true,
        true,
    )?;
    save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;

    if result.reused {
        println!(
            "already installed {} at {} (commit {})",
            pkg_id,
            result.install_dir.to_string_lossy(),
            result.commit
        );
    } else {
        println!(
            "installed {} at {}",
            pkg_id,
            result.install_dir.to_string_lossy()
        );
    }

    if project {
        let cwd = std::env::current_dir().map_err(|err| err.to_string())?;
        let project_root = deps::update_project_deps(&cwd, &pkg_id, &raw_url, rev.as_deref())?;
        let lock_path = deps::update_project_lock_with_closure(
            &project_root,
            &pkg_id,
            &origin_url,
            &result.commit,
            &result.install_dir,
            force,
        )?;
        println!("updated lock -> {}", lock_path.display());
    }
    Ok(())
}

fn run_list(args: Vec<String>) -> Result<(), String> {
    if args.iter().any(|arg| arg == "-h" || arg == "--help") {
        print_pkg_help();
        return Ok(());
    }
    if !args.is_empty() {
        return Err("clove pkg list does not take arguments".into());
    }
    let clove_home = clove_home();
    let registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    if registry.packages.is_empty() {
        println!("(no packages installed)");
        return Ok(());
    }
    let mut keys: Vec<&String> = registry.packages.keys().collect();
    keys.sort();
    for key in keys {
        let entry = match registry.packages.get(key) {
            Some(entry) => entry,
            None => continue,
        };
        println!("{}", key);
        let mut commits: Vec<&String> = entry.installs.keys().collect();
        commits.sort();
        for commit in commits {
            let install = entry.installs.get(commit).unwrap();
            let rev = install
                .rev_spec
                .as_ref()
                .map(|s| format!(" (rev: {})", s))
                .unwrap_or_default();
            println!("  - {}{}", commit, rev);
        }
    }
    Ok(())
}

fn run_sync(args: Vec<String>) -> Result<(), String> {
    let mut options = deps::SyncOptions::default();
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "--update" => {
                options.update = true;
                idx += 1;
            }
            "--offline" => {
                options.offline = true;
                idx += 1;
            }
            "--force" => {
                options.force = true;
                idx += 1;
            }
            "-h" | "--help" => {
                print_pkg_help();
                return Ok(());
            }
            other => return Err(format!("unknown argument: {}", other)),
        }
    }
    let cwd = std::env::current_dir().map_err(|err| err.to_string())?;
    let lock_path = deps::sync_project(&cwd, options)?;
    println!("synced deps -> {}", lock_path.display());
    Ok(())
}

fn run_update(args: Vec<String>) -> Result<(), String> {
    let mut pkg_id: Option<PackageId> = None;
    let mut rev: Option<String> = None;
    let mut force = false;
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "--rev" => {
                if idx + 1 >= args.len() {
                    return Err("--rev requires a value".into());
                }
                rev = Some(args[idx + 1].clone());
                idx += 2;
            }
            "--force" => {
                force = true;
                idx += 1;
            }
            "-h" | "--help" => {
                print_pkg_help();
                return Ok(());
            }
            other => {
                if pkg_id.is_none() {
                    pkg_id = Some(PackageId::parse(other).map_err(|err| err.to_string())?);
                    idx += 1;
                } else {
                    return Err(format!("unknown argument: {}", other));
                }
            }
        }
    }

    let pkg_id = pkg_id.ok_or_else(|| "owner/pkg is required".to_string())?;

    let clove_home = clove_home();
    let mut registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let package_key = pkg_id.key();
    let entry = registry
        .packages
        .get(&package_key)
        .ok_or_else(|| format!("package not installed: {}", pkg_id))?;
    let origin_url = entry.origin_url.clone().ok_or_else(|| {
        format!(
            "origin_url is missing for {} (reinstall to record origin before update)",
            pkg_id
        )
    })?;
    let fallback_rev = pick_latest_commit(&entry.installs)
        .and_then(|commit| entry.installs.get(&commit))
        .and_then(|install| install.rev_spec.clone());
    let rev_spec = rev.or(fallback_rev);

    let result = install_with_registry(
        &clove_home,
        &mut registry,
        &pkg_id,
        &origin_url,
        rev_spec.as_deref(),
        force,
        true,
        false,
    )?;
    save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;

    if result.reused {
        println!(
            "already installed {} at {} (commit {})",
            pkg_id,
            result.install_dir.to_string_lossy(),
            result.commit
        );
    } else {
        println!(
            "updated {} at {}",
            pkg_id,
            result.install_dir.to_string_lossy()
        );
    }
    Ok(())
}

fn run_uninstall(args: Vec<String>) -> Result<(), String> {
    let mut pkg_id: Option<PackageId> = None;
    let mut commit: Option<String> = None;
    let mut all = false;
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "--commit" => {
                if idx + 1 >= args.len() {
                    return Err("--commit requires a value".into());
                }
                commit = Some(args[idx + 1].clone());
                idx += 2;
            }
            "--all" => {
                all = true;
                idx += 1;
            }
            "-h" | "--help" => {
                print_pkg_help();
                return Ok(());
            }
            other => {
                if pkg_id.is_none() {
                    pkg_id = Some(PackageId::parse(other).map_err(|err| err.to_string())?);
                    idx += 1;
                } else {
                    return Err(format!("unknown argument: {}", other));
                }
            }
        }
    }
    if all && commit.is_some() {
        return Err("--all cannot be used with --commit".into());
    }
    let pkg_id = pkg_id.ok_or_else(|| "owner/pkg is required".to_string())?;
    let clove_home = clove_home();
    let mut registry = load_registry(&clove_home).map_err(|err| err.to_string())?;
    let package_key = pkg_id.key();
    let entry = match registry.packages.get_mut(&package_key) {
        Some(entry) => entry,
        None => return Err(format!("package not installed: {}", pkg_id)),
    };

    if all {
        let install_paths: Vec<PathBuf> = entry
            .installs
            .values()
            .map(|install| install.path.clone())
            .collect();
        for path in install_paths {
            fs::remove_dir_all(&path)
                .map_err(|err| format!("failed to remove {}: {}", path.display(), err))?;
        }
        registry.packages.remove(&package_key);
        save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;
        println!("uninstalled all versions of {}", pkg_id);
        return Ok(());
    }

    let target_commit = match commit {
        Some(commit) => commit,
        None => pick_latest_commit(&entry.installs)
            .ok_or_else(|| "no installed commits to uninstall".to_string())?,
    };
    let install = entry
        .installs
        .remove(&target_commit)
        .ok_or_else(|| format!("commit not installed: {}", target_commit))?;
    fs::remove_dir_all(&install.path)
        .map_err(|err| format!("failed to remove {}: {}", install.path.display(), err))?;
    if entry.installs.is_empty() {
        registry.packages.remove(&package_key);
    }
    save_registry(&clove_home, &registry).map_err(|err| err.to_string())?;
    println!("uninstalled {} for {}", target_commit, pkg_id);
    Ok(())
}

pub(crate) struct InstallResult {
    pub(crate) commit: String,
    pub(crate) install_dir: PathBuf,
    pub(crate) reused: bool,
}

pub(crate) fn install_with_registry(
    clove_home: &Path,
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    rev_spec: Option<&str>,
    force: bool,
    allow_existing: bool,
    reuse_existing_commit: bool,
) -> Result<InstallResult, String> {
    let is_local_origin = origin_is_local_path(origin_url);
    let mut resolved_commit = None;
    if reuse_existing_commit && !force && !is_local_origin {
        if let Some(existing) = find_existing_commit(registry, pkg_id, rev_spec) {
            let install_dir = clove_home
                .join("pkgs")
                .join(&pkg_id.owner)
                .join(&pkg_id.pkg)
                .join(&existing);
            if install_dir.exists() {
                resolved_commit = Some(existing);
            }
        }
    }

    let resolved_commit = match resolved_commit {
        Some(commit) => Some(commit),
        None => resolve_commit(origin_url, rev_spec)?,
    };

    match resolved_commit {
        Some(commit) => {
            if is_local_origin && rev_spec.is_none() {
                install_with_registry_from_local(
                    clove_home,
                    registry,
                    pkg_id,
                    origin_url,
                    &commit,
                    rev_spec,
                    force,
                    allow_existing,
                )
            } else {
                install_commit_with_registry(
                    clove_home,
                    registry,
                    pkg_id,
                    origin_url,
                    &commit,
                    rev_spec,
                    force,
                    allow_existing,
                )
            }
        }
        None => install_with_registry_from_clone(
            clove_home,
            registry,
            pkg_id,
            origin_url,
            rev_spec,
            force,
            allow_existing,
        ),
    }
}

pub(crate) fn install_commit_with_registry(
    clove_home: &Path,
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    commit: &str,
    rev_spec: Option<&str>,
    force: bool,
    allow_existing: bool,
) -> Result<InstallResult, String> {
    let commit = commit.trim();
    if commit.is_empty() {
        return Err("commit is empty".into());
    }

    let install_dir = clove_home
        .join("pkgs")
        .join(&pkg_id.owner)
        .join(&pkg_id.pkg)
        .join(commit);
    if origin_is_local_path(origin_url) {
        let origin_path = Path::new(origin_url);
        let origin_canon =
            fs::canonicalize(origin_path).unwrap_or_else(|_| origin_path.to_path_buf());
        let install_canon = fs::canonicalize(&install_dir).unwrap_or_else(|_| install_dir.clone());
        if origin_canon == install_canon {
            return Ok(InstallResult {
                commit: commit.to_string(),
                install_dir,
                reused: true,
            });
        }
    }
    let existed = install_dir.exists();
    let mut reused = false;
    if existed {
        if force {
            fs::remove_dir_all(&install_dir)
                .map_err(|err| format!("failed to remove {}: {}", install_dir.display(), err))?;
        } else if allow_existing {
            reused = true;
        } else {
            return Err(format!(
                "install target already exists: {} (use --force)",
                install_dir.display()
            ));
        }
    }

    if !existed || force {
        let tmp_root = clove_home.join("pkgs").join("tmp");
        let tmp_dir = create_temp_dir(&tmp_root)?;
        let _tmp_guard = TempDirGuard::new(tmp_dir.clone());

        let tmp_dir_str = tmp_dir.to_string_lossy();
        run_git(&["clone", origin_url, tmp_dir_str.as_ref()])?;
        run_git_in(&tmp_dir, &["checkout", commit])?;
        fs::create_dir_all(&install_dir).map_err(|err| {
            format!(
                "failed to create install directory {}: {}",
                install_dir.display(),
                err
            )
        })?;
        copy_dir_all(&tmp_dir, &install_dir)?;
    }

    let src_dir = install_dir.join("src");
    if !src_dir.is_dir() {
        return Err(format!(
            "src directory not found in {}",
            install_dir.display()
        ));
    }

    let installed_at = if let Some(existing) = registry
        .packages
        .get(&pkg_id.key())
        .and_then(|entry| entry.installs.get(commit))
    {
        existing.installed_at
    } else {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|err| format!("failed to read time: {}", err))?
            .as_millis() as i64
    };
    write_manifest(&install_dir, pkg_id, commit, rev_spec, Some(origin_url))?;
    update_registry(
        registry,
        pkg_id,
        commit,
        &install_dir,
        &src_dir,
        rev_spec,
        installed_at,
    )?;
    if let Err(err) = plugin::maybe_generate_plugin_meta(&install_dir) {
        eprintln!("plugin meta generation skipped: {}", err);
    }
    Ok(InstallResult {
        commit: commit.to_string(),
        install_dir,
        reused,
    })
}

fn install_with_registry_from_local(
    clove_home: &Path,
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    commit: &str,
    rev_spec: Option<&str>,
    force: bool,
    allow_existing: bool,
) -> Result<InstallResult, String> {
    let origin_path = Path::new(origin_url);
    if !origin_path.is_dir() {
        return Err(format!(
            "local origin path is not a directory: {}",
            origin_path.display()
        ));
    }

    let install_dir = clove_home
        .join("pkgs")
        .join(&pkg_id.owner)
        .join(&pkg_id.pkg)
        .join(commit);
    let existed = install_dir.exists();
    if existed {
        if force {
            fs::remove_dir_all(&install_dir)
                .map_err(|err| format!("failed to remove {}: {}", install_dir.display(), err))?;
        } else if allow_existing {
            fs::remove_dir_all(&install_dir)
                .map_err(|err| format!("failed to remove {}: {}", install_dir.display(), err))?;
        } else {
            return Err(format!(
                "install target already exists: {} (use --force)",
                install_dir.display()
            ));
        }
    }

    if !install_dir.exists() {
        fs::create_dir_all(&install_dir).map_err(|err| {
            format!(
                "failed to create install directory {}: {}",
                install_dir.display(),
                err
            )
        })?;
    }
    copy_dir_all(origin_path, &install_dir)?;

    let src_dir = install_dir.join("src");
    if !src_dir.is_dir() {
        return Err(format!(
            "src directory not found in {}",
            install_dir.display()
        ));
    }

    let installed_at = if let Some(existing) = registry
        .packages
        .get(&pkg_id.key())
        .and_then(|entry| entry.installs.get(commit))
    {
        existing.installed_at
    } else {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|err| format!("failed to read time: {}", err))?
            .as_millis() as i64
    };
    write_manifest(&install_dir, pkg_id, commit, rev_spec, Some(origin_url))?;
    update_registry(
        registry,
        pkg_id,
        commit,
        &install_dir,
        &src_dir,
        rev_spec,
        installed_at,
    )?;
    if let Err(err) = plugin::maybe_generate_plugin_meta(&install_dir) {
        eprintln!("plugin meta generation skipped: {}", err);
    }
    Ok(InstallResult {
        commit: commit.to_string(),
        install_dir,
        reused: false,
    })
}

fn install_with_registry_from_clone(
    clove_home: &Path,
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    rev_spec: Option<&str>,
    force: bool,
    allow_existing: bool,
) -> Result<InstallResult, String> {
    let tmp_root = clove_home.join("pkgs").join("tmp");
    let tmp_dir = create_temp_dir(&tmp_root)?;
    let _tmp_guard = TempDirGuard::new(tmp_dir.clone());

    let tmp_dir_str = tmp_dir.to_string_lossy();
    run_git(&["clone", origin_url, tmp_dir_str.as_ref()])?;
    if let Some(rev_spec) = rev_spec {
        run_git_in(&tmp_dir, &["checkout", rev_spec])?;
    }
    let commit = capture_git(&tmp_dir, &["rev-parse", "HEAD"])?;
    let commit = commit.trim().to_string();
    if commit.is_empty() {
        return Err("git rev-parse returned empty commit".into());
    }

    let install_dir = clove_home
        .join("pkgs")
        .join(&pkg_id.owner)
        .join(&pkg_id.pkg)
        .join(&commit);
    let existed = install_dir.exists();
    let mut reused = false;
    if existed {
        if force {
            fs::remove_dir_all(&install_dir)
                .map_err(|err| format!("failed to remove {}: {}", install_dir.display(), err))?;
        } else if allow_existing {
            reused = true;
        } else {
            return Err(format!(
                "install target already exists: {} (use --force)",
                install_dir.display()
            ));
        }
    }
    if !existed || force {
        fs::create_dir_all(&install_dir).map_err(|err| {
            format!(
                "failed to create install directory {}: {}",
                install_dir.display(),
                err
            )
        })?;
        copy_dir_all(&tmp_dir, &install_dir)?;
    }

    let src_dir = install_dir.join("src");
    if !src_dir.is_dir() {
        return Err(format!(
            "src directory not found in {}",
            install_dir.display()
        ));
    }

    let installed_at = if let Some(existing) = registry
        .packages
        .get(&pkg_id.key())
        .and_then(|entry| entry.installs.get(&commit))
    {
        existing.installed_at
    } else {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|err| format!("failed to read time: {}", err))?
            .as_millis() as i64
    };
    write_manifest(&install_dir, pkg_id, &commit, rev_spec, Some(origin_url))?;
    update_registry(
        registry,
        pkg_id,
        &commit,
        &install_dir,
        &src_dir,
        rev_spec,
        installed_at,
    )?;
    if let Err(err) = plugin::maybe_generate_plugin_meta(&install_dir) {
        eprintln!("plugin meta generation skipped: {}", err);
    }
    Ok(InstallResult {
        commit,
        install_dir,
        reused,
    })
}

pub(crate) fn ensure_origin_for_install(
    registry: &mut Registry,
    pkg_id: &PackageId,
    origin_url: &str,
    force: bool,
) -> Result<(), String> {
    let entry = registry
        .packages
        .entry(pkg_id.key())
        .or_insert_with(PackageEntry::default);
    match entry.origin_url.as_ref() {
        None => {
            entry.origin_url = Some(origin_url.to_string());
        }
        Some(existing) => {
            if !origin_url_equivalent(existing, origin_url) {
                if force {
                    entry.origin_url = Some(origin_url.to_string());
                } else {
                    return Err(format!(
                        "package already installed from different origin: {} (use --force)",
                        existing
                    ));
                }
            }
        }
    }
    Ok(())
}

fn update_registry(
    registry: &mut Registry,
    pkg_id: &PackageId,
    commit: &str,
    install_dir: &Path,
    src_dir: &Path,
    rev_spec: Option<&str>,
    installed_at: i64,
) -> Result<(), String> {
    let entry = registry
        .packages
        .entry(pkg_id.key())
        .or_insert_with(PackageEntry::default);
    entry.installs.insert(
        commit.to_string(),
        Install {
            commit: commit.to_string(),
            path: install_dir.to_path_buf(),
            src: src_dir.to_path_buf(),
            installed_at,
            rev_spec: rev_spec.map(|s| s.to_string()),
        },
    );
    Ok(())
}

fn write_manifest(
    install_dir: &Path,
    pkg_id: &PackageId,
    commit: &str,
    rev_spec: Option<&str>,
    origin_url: Option<&str>,
) -> Result<(), String> {
    let package_key = pkg_id.key();
    let path = install_dir.join("clove-pkg.json");
    let mut manifest = if path.exists() {
        let content = fs::read_to_string(&path)
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
        serde_json::from_str(&content).unwrap_or_else(|_| Value::Object(Map::new()))
    } else {
        Value::Object(Map::new())
    };
    if !manifest.is_object() {
        manifest = Value::Object(Map::new());
    }
    let obj = manifest.as_object_mut().expect("manifest should be object");
    obj.insert("package".to_string(), Value::String(package_key));
    obj.insert("commit".to_string(), Value::String(commit.to_string()));
    match origin_url {
        Some(url) => {
            obj.insert("origin_url".to_string(), Value::String(url.to_string()));
        }
        None => {
            obj.insert("origin_url".to_string(), Value::Null);
        }
    }
    match rev_spec {
        Some(rev) => {
            obj.insert("rev_spec".to_string(), Value::String(rev.to_string()));
        }
        None => {
            obj.insert("rev_spec".to_string(), Value::Null);
        }
    }
    obj.insert("src".to_string(), Value::String("src".to_string()));

    let json = serde_json::to_string_pretty(&manifest).map_err(|err| err.to_string())?;
    fs::write(&path, format!("{}\n", json))
        .map_err(|err| format!("failed to write {}: {}", path.display(), err))?;
    Ok(())
}

fn pick_latest_commit(installs: &std::collections::HashMap<String, Install>) -> Option<String> {
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

fn find_existing_commit(
    registry: &Registry,
    pkg_id: &PackageId,
    rev_spec: Option<&str>,
) -> Option<String> {
    let entry = registry.packages.get(&pkg_id.key())?;
    if let Some(rev_spec) = rev_spec {
        for (commit, install) in entry.installs.iter() {
            if install.rev_spec.as_deref() == Some(rev_spec) {
                return Some(commit.clone());
            }
        }
        None
    } else {
        pick_latest_commit(&entry.installs)
    }
}

pub(crate) struct ResolvedTarget {
    pub(crate) url: String,
    pub(crate) inferred_pkg_id: Option<PackageId>,
}

pub(crate) fn resolve_install_target(raw: &str) -> Result<ResolvedTarget, String> {
    let trimmed = raw.trim();
    let path = Path::new(trimmed);
    if path.exists() {
        let canonical = fs::canonicalize(path)
            .map_err(|err| format!("failed to canonicalize {}: {}", path.display(), err))?;
        let manifest_root = if canonical.is_dir() {
            if canonical
                .file_name()
                .map(|name| name == ".git")
                .unwrap_or(false)
            {
                canonical.parent().unwrap_or(&canonical).to_path_buf()
            } else {
                canonical.clone()
            }
        } else {
            canonical.parent().unwrap_or(&canonical).to_path_buf()
        };
        let inferred_pkg_id = infer_pkg_from_manifest(&manifest_root)?;
        return Ok(ResolvedTarget {
            url: canonical.to_string_lossy().into_owned(),
            inferred_pkg_id,
        });
    }
    if trimmed.contains("://") {
        return Ok(ResolvedTarget {
            url: trimmed.to_string(),
            inferred_pkg_id: infer_pkg_from_url(trimmed),
        });
    }
    if let Some(rest) = trimmed.strip_prefix("github:") {
        let pkg_id = parse_owner_repo(rest)
            .ok_or_else(|| format!("invalid github shorthand: {}", trimmed))?;
        return Ok(ResolvedTarget {
            url: normalize_github_url(&pkg_id),
            inferred_pkg_id: Some(pkg_id),
        });
    }
    if let Some(pkg_id) = parse_owner_repo(trimmed) {
        return Ok(ResolvedTarget {
            url: normalize_github_url(&pkg_id),
            inferred_pkg_id: Some(pkg_id),
        });
    }
    Ok(ResolvedTarget {
        url: trimmed.to_string(),
        inferred_pkg_id: infer_pkg_from_url(trimmed),
    })
}

fn infer_pkg_from_manifest(path: &Path) -> Result<Option<PackageId>, String> {
    let manifest_path = path.join("clove-pkg.json");
    if !manifest_path.exists() {
        return Ok(None);
    }
    let content = fs::read_to_string(&manifest_path)
        .map_err(|err| format!("failed to read {}: {}", manifest_path.display(), err))?;
    let manifest: Value = serde_json::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", manifest_path.display(), err))?;
    let pkg = manifest
        .get("pkg")
        .and_then(|value| value.as_str())
        .ok_or_else(|| format!("{} missing 'pkg' field", manifest_path.display()))?;
    let pkg_id = PackageId::parse(pkg).map_err(|err| err.to_string())?;
    Ok(Some(pkg_id))
}

pub(crate) fn resolve_commit(
    origin_url: &str,
    rev_spec: Option<&str>,
) -> Result<Option<String>, String> {
    if Path::new(origin_url).exists() {
        let commit = resolve_local_commit(origin_url, rev_spec)?;
        return Ok(Some(commit));
    }
    resolve_remote_commit(origin_url, rev_spec)
}

fn resolve_local_commit(origin_url: &str, rev_spec: Option<&str>) -> Result<String, String> {
    let rev = rev_spec.unwrap_or("HEAD");
    let rev_arg = format!("{}^{{commit}}", rev);
    let output = capture_git(Path::new(origin_url), &["rev-parse", &rev_arg])?;
    let commit = output.trim().to_string();
    if commit.is_empty() {
        return Err("git rev-parse returned empty commit".into());
    }
    Ok(commit)
}

fn resolve_remote_commit(
    origin_url: &str,
    rev_spec: Option<&str>,
) -> Result<Option<String>, String> {
    if let Some(rev_spec) = rev_spec {
        if looks_like_commit(rev_spec) {
            return Ok(Some(rev_spec.to_string()));
        }
        if let Some(commit) = ls_remote_commit(origin_url, rev_spec)? {
            if let Some(tag_commit) = ls_remote_annotated_tag(origin_url, rev_spec)? {
                return Ok(Some(tag_commit));
            }
            return Ok(Some(commit));
        }
        if let Some(tag_commit) = ls_remote_annotated_tag(origin_url, rev_spec)? {
            return Ok(Some(tag_commit));
        }
        return Ok(None);
    }
    if let Some(commit) = ls_remote_commit(origin_url, "HEAD")? {
        return Ok(Some(commit));
    }
    Ok(None)
}

fn ls_remote_annotated_tag(origin_url: &str, rev_spec: &str) -> Result<Option<String>, String> {
    let tag_ref = if rev_spec.starts_with("refs/tags/") {
        format!("{}^{{}}", rev_spec)
    } else if rev_spec.contains("^{}") {
        rev_spec.to_string()
    } else {
        format!("refs/tags/{}^{{}}", rev_spec)
    };
    ls_remote_commit(origin_url, &tag_ref)
}

fn ls_remote_commit(origin_url: &str, pattern: &str) -> Result<Option<String>, String> {
    let output = capture_git_output(&["ls-remote", origin_url, pattern])?;
    let mut lines = output.lines();
    let line = match lines.next() {
        Some(line) => line,
        None => return Ok(None),
    };
    let mut parts = line.split_whitespace();
    let commit = match parts.next() {
        Some(commit) => commit.trim(),
        None => "",
    };
    if commit.is_empty() {
        Ok(None)
    } else {
        Ok(Some(commit.to_string()))
    }
}

fn looks_like_commit(rev: &str) -> bool {
    if rev.len() != 40 {
        return false;
    }
    rev.chars().all(|c| c.is_ascii_hexdigit())
}

fn normalize_github_url(pkg_id: &PackageId) -> String {
    format!("https://github.com/{}/{}.git", pkg_id.owner, pkg_id.pkg)
}

fn infer_pkg_from_url(url: &str) -> Option<PackageId> {
    let trimmed = url.trim_end_matches('/');
    if let Some(pos) = trimmed.find("github.com/") {
        let rest = &trimmed[pos + "github.com/".len()..];
        return parse_owner_repo(rest);
    }
    if let Some(pos) = trimmed.find("github.com:") {
        let rest = &trimmed[pos + "github.com:".len()..];
        return parse_owner_repo(rest);
    }
    None
}

pub(crate) fn origin_url_equivalent(left: &str, right: &str) -> bool {
    origin_url_identity(left) == origin_url_identity(right)
}

fn origin_url_identity(origin_url: &str) -> String {
    let trimmed = origin_url.trim().trim_end_matches('/');
    if Path::new(trimmed).exists() {
        if let Ok(canonical) = fs::canonicalize(trimmed) {
            return canonical.to_string_lossy().into_owned();
        }
    }
    if let Some(pkg_id) = infer_pkg_from_url(trimmed) {
        return format!("github:{}/{}", pkg_id.owner, pkg_id.pkg);
    }
    trimmed.to_string()
}

fn origin_is_local_path(origin_url: &str) -> bool {
    Path::new(origin_url).exists()
}

fn parse_owner_repo(raw: &str) -> Option<PackageId> {
    let mut path = raw.trim_start_matches('/');
    if let Some(stripped) = path.strip_suffix(".git") {
        path = stripped;
    }
    let mut parts = path.split('/');
    let owner = parts.next()?.trim();
    let repo = parts.next()?.trim();
    if owner.is_empty() || repo.is_empty() {
        return None;
    }
    PackageId::parse(&format!("{}/{}", owner, repo)).ok()
}

fn create_temp_dir(base: &Path) -> Result<PathBuf, String> {
    fs::create_dir_all(base)
        .map_err(|err| format!("failed to create temp root {}: {}", base.display(), err))?;
    let pid = std::process::id();
    for bump in 0..20 {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|err| format!("failed to read time: {}", err))?
            .as_nanos();
        let candidate = base.join(format!("{}-{}-{}", pid, nanos, bump));
        if !candidate.exists() {
            return Ok(candidate);
        }
    }
    Err("failed to create temp directory".into())
}

fn run_git(args: &[&str]) -> Result<(), String> {
    let status = Command::new("git")
        .args(args)
        .status()
        .map_err(|err| format!("failed to run git: {}", err))?;
    if !status.success() {
        return Err("git command failed".into());
    }
    Ok(())
}

fn run_git_in(dir: &Path, args: &[&str]) -> Result<(), String> {
    let status = Command::new("git")
        .args(args)
        .current_dir(dir)
        .status()
        .map_err(|err| format!("failed to run git: {}", err))?;
    if !status.success() {
        return Err("git command failed".into());
    }
    Ok(())
}

fn capture_git(dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()
        .map_err(|err| format!("failed to run git: {}", err))?;
    if !output.status.success() {
        return Err("git command failed".into());
    }
    String::from_utf8(output.stdout).map_err(|err| err.to_string())
}

fn capture_git_output(args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .map_err(|err| format!("failed to run git: {}", err))?;
    if !output.status.success() {
        return Err("git command failed".into());
    }
    String::from_utf8(output.stdout).map_err(|err| err.to_string())
}

fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), String> {
    let entries =
        fs::read_dir(src).map_err(|err| format!("failed to read {}: {}", src.display(), err))?;
    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to read entry: {}", err))?;
        let path = entry.path();
        let name = entry.file_name();
        if name == std::ffi::OsStr::new(".git") {
            continue;
        }
        let target = dst.join(&name);
        let file_type = entry
            .file_type()
            .map_err(|err| format!("failed to read file type: {}", err))?;
        if file_type.is_dir() {
            fs::create_dir_all(&target)
                .map_err(|err| format!("failed to create dir {}: {}", target.display(), err))?;
            copy_dir_all(&path, &target)?;
        } else {
            fs::copy(&path, &target).map_err(|err| {
                format!(
                    "failed to copy {} to {}: {}",
                    path.display(),
                    target.display(),
                    err
                )
            })?;
        }
    }
    Ok(())
}

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn resolve_install_target_accepts_owner_repo_shorthand() {
        let target = resolve_install_target("example/flappy").expect("resolve owner/repo");
        assert_eq!(target.url, "https://github.com/example/flappy.git");
        let pkg_id = target.inferred_pkg_id.expect("pkg id");
        assert_eq!(pkg_id.owner, "example");
        assert_eq!(pkg_id.pkg, "flappy");
    }

    #[test]
    fn resolve_install_target_accepts_github_prefix() {
        let target = resolve_install_target("github:example/flappy").expect("resolve github:");
        assert_eq!(target.url, "https://github.com/example/flappy.git");
        assert_eq!(target.inferred_pkg_id.unwrap().key(), "example/flappy");
    }

    #[test]
    fn resolve_install_target_prefers_existing_path() {
        let temp = TempDir::new().expect("temp dir");
        let local_path = temp.path().join("example").join("flappy");
        fs::create_dir_all(&local_path).expect("create local path");
        let target = resolve_install_target(local_path.to_string_lossy().as_ref())
            .expect("resolve local path");
        let canonical = fs::canonicalize(&local_path).expect("canonicalize");
        assert_eq!(target.url, canonical.to_string_lossy());
        assert!(target.inferred_pkg_id.is_none());
    }

    #[test]
    fn resolve_install_target_infers_pkg_from_manifest() {
        let temp = TempDir::new().expect("temp dir");
        let local_path = temp.path().join("example").join("flappy");
        fs::create_dir_all(&local_path).expect("create local path");
        let manifest_path = local_path.join("clove-pkg.json");
        fs::write(
            &manifest_path,
            r#"{"pkg":"example/flappy","ns_root":"example::flappy","deps":{}}"#,
        )
        .expect("write manifest");
        let target = resolve_install_target(local_path.to_string_lossy().as_ref())
            .expect("resolve local path");
        assert_eq!(target.inferred_pkg_id.unwrap().key(), "example/flappy");
    }
}
