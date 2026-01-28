use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Mutex, OnceLock};

use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use clove_lang::{create_runtime, default_engines, set_native_plugin_config, NativePluginConfig};
use sha2::{Digest, Sha256};
use tempfile::TempDir;

fn run_git(dir: &Path, args: &[&str]) -> Result<(), String> {
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

fn env_lock() -> std::sync::MutexGuard<'static, ()> {
    static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    match ENV_LOCK.get_or_init(|| Mutex::new(())).lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    }
}

fn setup_repo_with_versions(temp: &TempDir, repo_name: &str) -> (PathBuf, String, String) {
    let repo_dir = temp.path().join(repo_name);
    let src_dir = repo_dir.join("src").join("example").join("flappy");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        src_dir.join("core.clv"),
        "(ns example::flappy::core)\n(defn hello [] 1)\n",
    )
    .expect("write core.clv v0.1.0");

    run_git(&repo_dir, &["init"]).expect("git init");
    run_git(&repo_dir, &["config", "user.email", "test@example.com"]).expect("git email");
    run_git(&repo_dir, &["config", "user.name", "test"]).expect("git name");
    run_git(&repo_dir, &["add", "."]).expect("git add");
    run_git(&repo_dir, &["commit", "-m", "init"]).expect("git commit");
    run_git(&repo_dir, &["tag", "v0.1.0"]).expect("git tag v0.1.0");
    let v1_commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse v0.1.0")
        .trim()
        .to_string();

    fs::write(
        src_dir.join("core.clv"),
        "(ns example::flappy::core)\n(defn hello [] 2)\n",
    )
    .expect("write core.clv v0.2.0");
    run_git(&repo_dir, &["add", "."]).expect("git add v0.2.0");
    run_git(&repo_dir, &["commit", "-m", "v0.2.0"]).expect("git commit v0.2.0");
    run_git(&repo_dir, &["tag", "v0.2.0"]).expect("git tag v0.2.0");
    let v2_commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse v0.2.0")
        .trim()
        .to_string();

    (repo_dir, v1_commit, v2_commit)
}

fn setup_repo_with_versions_for(
    temp: &TempDir,
    repo_name: &str,
    owner: &str,
    pkg: &str,
) -> (PathBuf, String, String) {
    let repo_dir = temp.path().join(repo_name);
    let src_dir = repo_dir.join("src").join(owner).join(pkg);
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        src_dir.join("core.clv"),
        format!("(ns {}::{}::core)\n(defn hello [] 1)\n", owner, pkg),
    )
    .expect("write core.clv v0.1.0");

    run_git(&repo_dir, &["init"]).expect("git init");
    run_git(&repo_dir, &["config", "user.email", "test@example.com"]).expect("git email");
    run_git(&repo_dir, &["config", "user.name", "test"]).expect("git name");
    run_git(&repo_dir, &["add", "."]).expect("git add");
    run_git(&repo_dir, &["commit", "-m", "init"]).expect("git commit");
    run_git(&repo_dir, &["tag", "v0.1.0"]).expect("git tag v0.1.0");
    let v1_commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse v0.1.0")
        .trim()
        .to_string();

    fs::write(
        src_dir.join("core.clv"),
        format!("(ns {}::{}::core)\n(defn hello [] 2)\n", owner, pkg),
    )
    .expect("write core.clv v0.2.0");
    run_git(&repo_dir, &["add", "."]).expect("git add v0.2.0");
    run_git(&repo_dir, &["commit", "-m", "v0.2.0"]).expect("git commit v0.2.0");
    run_git(&repo_dir, &["tag", "v0.2.0"]).expect("git tag v0.2.0");
    let v2_commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse v0.2.0")
        .trim()
        .to_string();

    (repo_dir, v1_commit, v2_commit)
}

fn plugin_platform_tag() -> String {
    format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

fn native_plugin_filename(base: &str) -> String {
    if cfg!(target_os = "windows") {
        format!("{}.dll", base)
    } else if cfg!(target_os = "macos") {
        format!("lib{}.dylib", base)
    } else {
        format!("lib{}.so", base)
    }
}

fn setup_repo_with_plugin(
    temp: &TempDir,
    repo_name: &str,
    owner: &str,
    pkg: &str,
) -> (PathBuf, String) {
    let repo_dir = temp.path().join(repo_name);
    let src_dir = repo_dir.join("src").join(owner).join(pkg);
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        src_dir.join("core.clv"),
        format!("(ns {}::{}::core)\n(defn hello [] 1)\n", owner, pkg),
    )
    .expect("write core.clv");

    let plugin_dir = repo_dir.join("plugins").join(plugin_platform_tag());
    fs::create_dir_all(&plugin_dir).expect("create plugin dir");
    fs::write(plugin_dir.join("dummy"), "").expect("write dummy plugin marker");

    run_git(&repo_dir, &["init"]).expect("git init");
    run_git(&repo_dir, &["config", "user.email", "test@example.com"]).expect("git email");
    run_git(&repo_dir, &["config", "user.name", "test"]).expect("git name");
    run_git(&repo_dir, &["add", "."]).expect("git add");
    run_git(&repo_dir, &["commit", "-m", "init"]).expect("git commit");
    let commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse")
        .trim()
        .to_string();

    (repo_dir, commit)
}

fn setup_repo_with_native_plugin(
    temp: &TempDir,
    repo_name: &str,
    owner: &str,
    pkg: &str,
) -> (PathBuf, String, PathBuf) {
    let repo_dir = temp.path().join(repo_name);
    let src_dir = repo_dir.join("src").join(owner).join(pkg);
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        src_dir.join("core.clv"),
        format!("(ns {}::{}::core)\n(defn hello [] 1)\n", owner, pkg),
    )
    .expect("write core.clv");

    let plugin_dir = repo_dir.join("plugins").join(plugin_platform_tag());
    fs::create_dir_all(&plugin_dir).expect("create plugin dir");
    let plugin_name = native_plugin_filename("clove_test_plugin");
    let plugin_path = plugin_dir.join(&plugin_name);
    fs::write(&plugin_path, "native-plugin").expect("write native plugin");
    let rel_path = Path::new("plugins")
        .join(plugin_platform_tag())
        .join(&plugin_name);

    run_git(&repo_dir, &["init"]).expect("git init");
    run_git(&repo_dir, &["config", "user.email", "test@example.com"]).expect("git email");
    run_git(&repo_dir, &["config", "user.name", "test"]).expect("git name");
    run_git(&repo_dir, &["add", "."]).expect("git add");
    run_git(&repo_dir, &["commit", "-m", "init"]).expect("git commit");
    let commit = capture_git(&repo_dir, &["rev-parse", "HEAD"])
        .expect("rev-parse")
        .trim()
        .to_string();

    (repo_dir, commit, rel_path)
}

fn hash_file_sha256(path: &Path) -> String {
    let data = fs::read(path).expect("read plugin file");
    let mut hasher = Sha256::new();
    hasher.update(&data);
    let out = hasher.finalize();
    let mut s = String::with_capacity(out.len() * 2);
    for b in out {
        let _ = std::fmt::Write::write_fmt(&mut s, format_args!("{:02x}", b));
    }
    s
}

fn setup_repo_with_manifest(
    temp: &TempDir,
    repo_name: &str,
    owner: &str,
    pkg: &str,
    body: &str,
    deps: Option<BTreeMap<String, serde_json::Value>>,
) -> PathBuf {
    let repo_dir = temp.path().join(repo_name);
    let src_dir = repo_dir.join("src").join(owner).join(pkg);
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(src_dir.join("core.clv"), body).expect("write core.clv");

    if let Some(deps) = deps {
        let manifest = serde_json::json!({
            "pkg": format!("{}/{}", owner, pkg),
            "ns_root": format!("{}::{}", owner, pkg),
            "deps": deps,
        });
        fs::write(
            repo_dir.join("clove-pkg.json"),
            format!(
                "{}\n",
                serde_json::to_string_pretty(&manifest).expect("manifest json")
            ),
        )
        .expect("write clove-pkg.json");
    }

    run_git(&repo_dir, &["init"]).expect("git init");
    run_git(&repo_dir, &["config", "user.email", "test@example.com"]).expect("git email");
    run_git(&repo_dir, &["config", "user.name", "test"]).expect("git name");
    run_git(&repo_dir, &["add", "."]).expect("git add");
    run_git(&repo_dir, &["commit", "-m", "init"]).expect("git commit");

    repo_dir
}

#[test]
fn pkg_install_allows_require_from_registry() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install");

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call");
    assert_eq!(value.to_string(), "1");
}

#[test]
fn pkg_uses_latest_install_without_lock() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
    ])
    .expect("pkg install v0.2.0");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install v0.1.0");

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call v0.1.0");
    assert_eq!(value.to_string(), "1");
}

#[test]
fn pkg_uninstall_removes_latest_and_all() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install v0.1.0");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
    ])
    .expect("pkg install v0.2.0");

    clove_lang::pkg::run_pkg(vec!["uninstall".into(), "example/flappy".into()])
        .expect("pkg uninstall latest");

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call after uninstall latest");
    assert_eq!(value.to_string(), "1");

    clove_lang::pkg::run_pkg(vec![
        "uninstall".into(),
        "example/flappy".into(),
        "--all".into(),
    ])
    .expect("pkg uninstall --all");

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let err = ctx.eval_source("(require example::flappy::core)");
    assert!(err.is_err());
}

#[test]
fn pkg_update_works_without_url() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install v0.1.0");

    clove_lang::pkg::run_pkg(vec![
        "update".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
    ])
    .expect("pkg update v0.2.0");

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call v0.2.0");
    assert_eq!(value.to_string(), "2");
}

#[test]
fn pkg_install_rejects_different_origin_without_force() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_a, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo_a");
    let (repo_b, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo_b");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_a.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install repo_a");

    let err = clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_b.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ]);
    assert!(err.is_err());

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_b.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
        "--force".into(),
    ])
    .expect("pkg install repo_b --force");
}

#[test]
fn lock_override_takes_precedence_over_latest() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, v2_commit) = setup_repo_with_versions(&temp, "repo");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
    ])
    .expect("pkg install v0.2.0");

    clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.1.0".into(),
    ])
    .expect("pkg install v0.1.0");

    let mut overrides = HashMap::new();
    overrides.insert("example/flappy".to_string(), v2_commit);

    let mut opts = EvalOptions::default();
    opts.package_overrides = overrides;
    let ctx = RuntimeCtx::new(opts, &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call override");
    assert_eq!(value.to_string(), "2");
}

#[test]
fn deps_sync_generates_lock_and_overrides() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, v2_commit) = setup_repo_with_versions(&temp, "repo");
    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");
    let deps_path = project_root.join("clove.deps.json");
    let deps_json = format!(
        "{{\n  \"deps\": {{\n    \"example/flappy\": {{ \"origin\": \"{}\", \"rev\": \"v0.2.0\" }}\n  }}\n}}\n",
        repo_dir.to_string_lossy()
    );
    fs::write(&deps_path, deps_json).expect("write clove.deps.json");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec!["sync".into()]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg sync");

    let lock_path = project_root.join("clove.lock.json");
    let lock_text = fs::read_to_string(&lock_path).expect("read clove.lock.json");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("parse clove.lock.json");
    let locked = &lock_json["deps"]["example/flappy"];
    assert_eq!(locked["commit"].as_str(), Some(v2_commit.as_str()));
    let expected_origin = fs::canonicalize(&repo_dir).expect("canonicalize repo");
    assert_eq!(
        locked["origin_url"].as_str(),
        Some(expected_origin.to_string_lossy().as_ref())
    );

    let overrides =
        clove_lang::deps::load_project_lock_overrides(&project_root).expect("load overrides");
    let mut opts = EvalOptions::default();
    opts.package_overrides = overrides;
    let ctx = RuntimeCtx::new(opts, &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call override");
    assert_eq!(value.to_string(), "2");
}

#[test]
fn deps_sync_adds_native_plugin_dirs_from_lock() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);
    std::env::remove_var("CLOVE_PLUGIN_DIR");

    let (repo_dir, commit) = setup_repo_with_plugin(&temp, "repo", "example", "flappy");
    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");
    let deps_path = project_root.join("clove.deps.json");
    let deps_json = format!(
        "{{\n  \"deps\": {{\n    \"example/flappy\": {{ \"origin\": \"{}\", \"rev\": \"{}\" }}\n  }}\n}}\n",
        repo_dir.to_string_lossy(),
        commit
    );
    fs::write(&deps_path, deps_json).expect("write clove.deps.json");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec!["sync".into()]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg sync");

    let plugin_dirs =
        clove_lang::deps::load_project_plugin_dirs(&project_root).expect("load plugin dirs");
    let expected = clove_home
        .join("pkgs")
        .join("example")
        .join("flappy")
        .join(&commit)
        .join("plugins")
        .join(plugin_platform_tag());
    assert!(
        plugin_dirs.contains(&expected),
        "expected plugin dir in {:?}",
        plugin_dirs
    );

    set_native_plugin_config(NativePluginConfig {
        allow: true,
        plugin_dirs: Vec::new(),
        auto_plugin_dirs: plugin_dirs.clone(),
        base_dir: Some(project_root.clone()),
        exe_dir: None,
    });

    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let err = ctx
        .eval_source("(require-native \"dummy\")")
        .expect_err("require-native should fail");
    let msg = err.to_string();
    assert!(
        msg.contains(expected.to_string_lossy().as_ref()),
        "expected search paths to include {}",
        expected.display()
    );
}

#[test]
fn deps_sync_writes_native_plugin_hashes() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, commit, rel_path) =
        setup_repo_with_native_plugin(&temp, "repo_native", "example", "flappy");
    let project_root = temp.path().join("project_native");
    fs::create_dir_all(&project_root).expect("create project root");
    let deps_path = project_root.join("clove.deps.json");
    let deps_json = format!(
        "{{\n  \"deps\": {{\n    \"example/flappy\": {{ \"origin\": \"{}\", \"rev\": \"{}\" }}\n  }}\n}}\n",
        repo_dir.to_string_lossy(),
        commit
    );
    fs::write(&deps_path, deps_json).expect("write clove.deps.json");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec!["sync".into()]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg sync");

    let lock_path = project_root.join("clove.lock.json");
    let lock_text = fs::read_to_string(&lock_path).expect("read clove.lock.json");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("parse clove.lock.json");
    let plugins = lock_json["deps"]["example/flappy"]["native_plugins"]
        .as_array()
        .expect("native_plugins array");
    let rel_path_str = rel_path.to_string_lossy();
    let entry = plugins
        .iter()
        .find(|item| item["rel_path"].as_str() == Some(rel_path_str.as_ref()))
        .expect("plugin entry");

    let installed_plugin = clove_home
        .join("pkgs")
        .join("example")
        .join("flappy")
        .join(&commit)
        .join(&rel_path);
    let expected_hash = hash_file_sha256(&installed_plugin);
    assert_eq!(entry["sha256"].as_str(), Some(expected_hash.as_str()));
}

#[test]
fn pkg_install_project_updates_lock_and_requires() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, v2_commit) = setup_repo_with_versions(&temp, "repo");
    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
        "--project".into(),
    ]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg install --project");

    let lock_path = project_root.join("clove.lock.json");
    let lock_text = fs::read_to_string(&lock_path).expect("read clove.lock.json");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("parse clove.lock.json");
    let locked = &lock_json["deps"]["example/flappy"];
    assert_eq!(locked["commit"].as_str(), Some(v2_commit.as_str()));

    let overrides =
        clove_lang::deps::load_project_lock_overrides(&project_root).expect("load overrides");
    let mut opts = EvalOptions::default();
    opts.package_overrides = overrides;
    let ctx = RuntimeCtx::new(opts, &[]);
    let value = ctx
        .eval_source("(require example::flappy::core)\n(example::flappy::core::hello)")
        .expect("require and call override");
    assert_eq!(value.to_string(), "2");
}

#[test]
fn pkg_install_project_reuses_existing_without_tmp_clone() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_dir, _v1_commit, _v2_commit) = setup_repo_with_versions(&temp, "repo");
    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
        "--project".into(),
    ]);
    result.expect("pkg install --project");

    let tmp_root = clove_home.join("pkgs").join("tmp");
    if tmp_root.exists() {
        fs::remove_dir_all(&tmp_root).expect("remove tmp root");
    }

    let result = clove_lang::pkg::run_pkg(vec![
        "install".into(),
        repo_dir.to_string_lossy().into_owned(),
        "--pkg".into(),
        "example/flappy".into(),
        "--rev".into(),
        "v0.2.0".into(),
        "--project".into(),
    ]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg install --project again");

    assert!(!tmp_root.exists());
}

#[test]
fn pkg_sync_includes_transitive_dependencies() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let repo_b = setup_repo_with_manifest(
        &temp,
        "repo_b",
        "b",
        "util",
        "(ns b::util::core)\n(defn hello [] 7)\n",
        None,
    );
    let mut deps = BTreeMap::new();
    deps.insert(
        "b/util".to_string(),
        serde_json::json!({ "origin": repo_b.to_string_lossy() }),
    );
    let repo_a = setup_repo_with_manifest(
        &temp,
        "repo_a",
        "a",
        "app",
        "(ns a::app::core)\n(require b::util::core)\n(defn hello [] (b::util::core::hello))\n",
        Some(deps),
    );

    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");
    let deps_path = project_root.join("clove.deps.json");
    let deps_json = serde_json::json!({
        "deps": {
            "a/app": { "origin": repo_a.to_string_lossy() }
        }
    });
    fs::write(
        &deps_path,
        format!(
            "{}\n",
            serde_json::to_string_pretty(&deps_json).expect("deps json")
        ),
    )
    .expect("write clove.deps.json");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec!["sync".into()]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    result.expect("pkg sync");

    let lock_path = project_root.join("clove.lock.json");
    let lock_text = fs::read_to_string(&lock_path).expect("read clove.lock.json");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("parse clove.lock.json");
    assert!(lock_json["deps"]["a/app"]["commit"].is_string());
    assert!(lock_json["deps"]["b/util"]["commit"].is_string());

    let overrides =
        clove_lang::deps::load_project_lock_overrides(&project_root).expect("load overrides");
    let mut opts = EvalOptions::default();
    opts.package_overrides = overrides;
    let ctx = RuntimeCtx::new(opts, &[]);
    let value = ctx
        .eval_source("(require a::app::core)\n(a::app::core::hello)")
        .expect("require and call transitive");
    assert_eq!(value.to_string(), "7");
}

#[test]
fn pkg_sync_detects_transitive_conflict() {
    let _lock = env_lock();
    let temp = TempDir::new().expect("create temp dir");
    let clove_home = temp.path().join("clove_home");
    fs::create_dir_all(&clove_home).expect("create clove_home");
    std::env::set_var("CLOVE_HOME", &clove_home);

    let (repo_b, _v1_commit, _v2_commit) =
        setup_repo_with_versions_for(&temp, "repo_b", "b", "util");

    let mut deps_a = BTreeMap::new();
    deps_a.insert(
        "b/util".to_string(),
        serde_json::json!({ "origin": repo_b.to_string_lossy(), "rev": "v0.1.0" }),
    );
    let repo_a = setup_repo_with_manifest(
        &temp,
        "repo_a",
        "a",
        "app",
        "(ns a::app::core)\n(defn hello [] 1)\n",
        Some(deps_a),
    );

    let mut deps_c = BTreeMap::new();
    deps_c.insert(
        "b/util".to_string(),
        serde_json::json!({ "origin": repo_b.to_string_lossy(), "rev": "v0.2.0" }),
    );
    let repo_c = setup_repo_with_manifest(
        &temp,
        "repo_c",
        "c",
        "app",
        "(ns c::app::core)\n(defn hello [] 2)\n",
        Some(deps_c),
    );

    let project_root = temp.path().join("project");
    fs::create_dir_all(&project_root).expect("create project root");
    let deps_path = project_root.join("clove.deps.json");
    let deps_json = serde_json::json!({
        "deps": {
            "a/app": { "origin": repo_a.to_string_lossy() },
            "c/app": { "origin": repo_c.to_string_lossy() }
        }
    });
    fs::write(
        &deps_path,
        format!(
            "{}\n",
            serde_json::to_string_pretty(&deps_json).expect("deps json")
        ),
    )
    .expect("write clove.deps.json");

    let original_dir = std::env::current_dir().expect("current dir");
    std::env::set_current_dir(&project_root).expect("set current dir");
    let result = clove_lang::pkg::run_pkg(vec!["sync".into()]);
    std::env::set_current_dir(&original_dir).expect("restore current dir");
    let err = result.expect_err("pkg sync should fail");
    assert!(err.contains("dependency conflict"));
}
