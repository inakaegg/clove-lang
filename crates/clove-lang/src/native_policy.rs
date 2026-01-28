use std::path::{Component, Path, PathBuf};
use std::time::UNIX_EPOCH;

use clove_core::error::CloveError;
use clove_core::package_registry::clove_home;
use clove_core::runtime::RuntimeCtx;

use crate::deps;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum NativeAllowReason {
    FlagAllowAny,
    TrustedDirProject,
    TrustedDirUser,
    PkgHashVerified,
}

pub(crate) fn can_load_native_plugin(
    path: &Path,
    ctx: &RuntimeCtx,
    cli_allow_any: bool,
) -> Result<NativeAllowReason, CloveError> {
    if cli_allow_any {
        return Ok(NativeAllowReason::FlagAllowAny);
    }

    let clove_home = clove_home();
    let project_root = deps::find_project_root(ctx.working_dir());
    let project_root_for_msg = project_root.as_deref().unwrap_or_else(|| ctx.working_dir());
    let project_plugins = project_root_for_msg.join("plugins");
    let project_hidden_plugins = project_root_for_msg.join(".clove").join("plugins");
    let user_plugins = clove_home.join("plugins");

    let canonical = match path.canonicalize() {
        Ok(path) => path,
        Err(_) => {
            return Err(untrusted_error(
                &project_plugins,
                &project_hidden_plugins,
                &user_plugins,
            ))
        }
    };

    if let Some(root) = project_root.as_deref() {
        if is_under(&canonical, &root.join("plugins"))
            || is_under(&canonical, &root.join(".clove").join("plugins"))
        {
            return Ok(NativeAllowReason::TrustedDirProject);
        }
    }

    if is_under(&canonical, &user_plugins) {
        return Ok(NativeAllowReason::TrustedDirUser);
    }

    let pkgs_root = clove_home.join("pkgs");
    if let Some(pkgs_root_canon) = canonicalize_dir(&pkgs_root) {
        if canonical.starts_with(&pkgs_root_canon) {
            if let Some(location) = pkg_plugin_location(&canonical, &pkgs_root_canon) {
                verify_pkg_plugin_hash(&canonical, &location, project_root.as_deref())?;
                return Ok(NativeAllowReason::PkgHashVerified);
            }
        }
    }

    Err(untrusted_error(
        &project_plugins,
        &project_hidden_plugins,
        &user_plugins,
    ))
}

fn canonicalize_dir(path: &Path) -> Option<PathBuf> {
    let canon = path.canonicalize().ok()?;
    if canon.is_dir() {
        Some(canon)
    } else {
        None
    }
}

fn is_under(path: &Path, base: &Path) -> bool {
    let Some(base_canon) = canonicalize_dir(base) else {
        return false;
    };
    path.starts_with(&base_canon)
}

struct PkgPluginLocation {
    pkg_key: String,
    commit: String,
    rel_path: PathBuf,
}

fn pkg_plugin_location(path: &Path, pkgs_root: &Path) -> Option<PkgPluginLocation> {
    let rel = path.strip_prefix(pkgs_root).ok()?;
    let mut components = rel.components();
    let owner = component_to_string(components.next()?)?;
    let pkg = component_to_string(components.next()?)?;
    let commit = component_to_string(components.next()?)?;
    if owner.is_empty() || pkg.is_empty() || commit.is_empty() {
        return None;
    }
    let pkg_root = pkgs_root.join(&owner).join(&pkg).join(&commit);
    let rel_path = path.strip_prefix(&pkg_root).ok()?.to_path_buf();
    let mut rel_components = rel_path.components();
    let first = rel_components.next()?;
    if !matches!(first, Component::Normal(name) if name == "plugins") {
        return None;
    }
    Some(PkgPluginLocation {
        pkg_key: format!("{}/{}", owner, pkg),
        commit,
        rel_path,
    })
}

fn component_to_string(component: Component<'_>) -> Option<String> {
    match component {
        Component::Normal(value) => Some(value.to_string_lossy().into_owned()),
        _ => None,
    }
}

fn verify_pkg_plugin_hash(
    path: &Path,
    location: &PkgPluginLocation,
    project_root: Option<&Path>,
) -> Result<(), CloveError> {
    let project_root = project_root.ok_or_else(pkg_hash_error)?;
    let lock_path = project_root.join("clove.lock.json");
    if !lock_path.is_file() {
        return Err(pkg_hash_error());
    }
    let lock = deps::read_lock_file(&lock_path).map_err(|_| pkg_hash_error())?;
    let dep = lock
        .deps
        .get(&location.pkg_key)
        .ok_or_else(pkg_hash_error)?;
    if dep.commit != location.commit {
        return Err(pkg_hash_error());
    }
    let rel_path = location.rel_path.to_string_lossy();
    let entry = dep
        .native_plugins
        .iter()
        .find(|entry| entry.rel_path == rel_path);
    let entry = match entry {
        Some(entry) => entry,
        None => return Err(pkg_hash_error()),
    };
    let metadata = std::fs::metadata(path).map_err(|_| pkg_hash_error())?;
    if let Some(expected) = entry.size {
        if metadata.len() != expected {
            return Err(pkg_hash_error());
        }
    }
    if let Some(expected) = entry.mtime_unix {
        let actual = metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
            .map(|d| d.as_secs() as i64);
        if actual != Some(expected) {
            return Err(pkg_hash_error());
        }
    }
    let actual = deps::hash_file_sha256(path).map_err(|_| pkg_hash_error())?;
    if actual != entry.sha256 {
        return Err(pkg_hash_error());
    }
    Ok(())
}

fn untrusted_error(
    project_plugins: &Path,
    project_hidden_plugins: &Path,
    user_plugins: &Path,
) -> CloveError {
    CloveError::runtime(format!(
        "Native plugin loading is disabled. Pass `--allow-native-plugins`, or place the plugin under `{}`, `{}`, or `{}`.",
        project_plugins.display(),
        project_hidden_plugins.display(),
        user_plugins.display()
    ))
}

fn pkg_hash_error() -> CloveError {
    CloveError::runtime(
        "A native plugin under pkg does not match the sha256 recorded in the lockfile (possible tampering or replacement). Re-run `clove pkg install` to update the lock, or pass `--allow-native-plugins` explicitly."
            .to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use clove_core::options::EvalOptions;
    use sha2::{Digest, Sha256};
    use std::fs;
    use std::sync::{Mutex, OnceLock};
    use tempfile::TempDir;

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        match ENV_LOCK.get_or_init(|| Mutex::new(())).lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    fn hash_file(path: &Path) -> String {
        let data = fs::read(path).expect("read plugin");
        let mut hasher = Sha256::new();
        hasher.update(&data);
        let out = hasher.finalize();
        let mut s = String::with_capacity(out.len() * 2);
        for b in out {
            let _ = std::fmt::Write::write_fmt(&mut s, format_args!("{:02x}", b));
        }
        s
    }

    fn native_plugin_filename(name: &str) -> String {
        if cfg!(target_os = "windows") {
            format!("{}.dll", name)
        } else if cfg!(target_os = "macos") {
            format!("lib{}.dylib", name)
        } else {
            format!("lib{}.so", name)
        }
    }

    #[test]
    fn native_policy_allows_trusted_dirs_and_pkg_hash() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("tempdir");
        let clove_home_dir = temp.path().join("clove_home");
        fs::create_dir_all(&clove_home_dir).expect("create clove_home");
        std::env::set_var("CLOVE_HOME", &clove_home_dir);

        let project_root = temp.path().join("project");
        fs::create_dir_all(&project_root).expect("create project root");
        fs::write(project_root.join("clove.deps.json"), "{ \"deps\": {} }\n")
            .expect("write clove.deps.json");

        let mut opts = EvalOptions::default();
        opts.working_dir = Some(project_root.clone());
        let ctx = RuntimeCtx::new(opts, &[]);

        let plugin_name = native_plugin_filename("dummy_plugin");
        let project_plugin = project_root.join("plugins").join(&plugin_name);
        fs::create_dir_all(project_plugin.parent().unwrap()).expect("create project plugins");
        fs::write(&project_plugin, "dummy").expect("write project plugin");
        let reason = can_load_native_plugin(&project_plugin, &ctx, false).expect("project plugin");
        assert_eq!(reason, NativeAllowReason::TrustedDirProject);

        let hidden_plugin = project_root
            .join(".clove")
            .join("plugins")
            .join(&plugin_name);
        fs::create_dir_all(hidden_plugin.parent().unwrap()).expect("create hidden plugins");
        fs::write(&hidden_plugin, "dummy").expect("write hidden plugin");
        let reason =
            can_load_native_plugin(&hidden_plugin, &ctx, false).expect("hidden project plugin");
        assert_eq!(reason, NativeAllowReason::TrustedDirProject);

        let user_plugin = clove_home_dir.join("plugins").join(&plugin_name);
        fs::create_dir_all(user_plugin.parent().unwrap()).expect("create user plugins");
        fs::write(&user_plugin, "dummy").expect("write user plugin");
        let reason = can_load_native_plugin(&user_plugin, &ctx, false).expect("user plugin");
        assert_eq!(reason, NativeAllowReason::TrustedDirUser);

        let pkg_root = clove_home_dir
            .join("pkgs")
            .join("example")
            .join("flappy")
            .join("deadbeef");
        let pkg_plugin = pkg_root.join("plugins").join(&plugin_name);
        fs::create_dir_all(pkg_plugin.parent().unwrap()).expect("create pkg plugins");
        fs::write(&pkg_plugin, "hash-check").expect("write pkg plugin");

        let rel_path = Path::new("plugins").join(&plugin_name);
        let lock_json = serde_json::json!({
            "deps": {
                "example/flappy": {
                    "origin_url": "local",
                    "commit": "deadbeef",
                    "native_plugins": [{
                        "rel_path": rel_path.to_string_lossy(),
                        "sha256": hash_file(&pkg_plugin)
                    }]
                }
            }
        });
        fs::write(
            project_root.join("clove.lock.json"),
            format!("{}\n", serde_json::to_string_pretty(&lock_json).unwrap()),
        )
        .expect("write lock");

        let reason = can_load_native_plugin(&pkg_plugin, &ctx, false).expect("pkg plugin allowed");
        assert_eq!(reason, NativeAllowReason::PkgHashVerified);

        let other = temp.path().join("other").join(&plugin_name);
        fs::create_dir_all(other.parent().unwrap()).expect("create other dir");
        fs::write(&other, "nope").expect("write other plugin");
        let err = can_load_native_plugin(&other, &ctx, false).expect_err("untrusted");
        assert!(err
            .to_string()
            .contains("Native plugin loading is disabled"));
    }

    #[test]
    fn native_policy_rejects_pkg_hash_mismatch() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("tempdir");
        let clove_home_dir = temp.path().join("clove_home");
        fs::create_dir_all(&clove_home_dir).expect("create clove_home");
        std::env::set_var("CLOVE_HOME", &clove_home_dir);

        let project_root = temp.path().join("project");
        fs::create_dir_all(&project_root).expect("create project root");
        fs::write(project_root.join("clove.deps.json"), "{ \"deps\": {} }\n")
            .expect("write clove.deps.json");

        let mut opts = EvalOptions::default();
        opts.working_dir = Some(project_root.clone());
        let ctx = RuntimeCtx::new(opts, &[]);

        let plugin_name = native_plugin_filename("dummy_plugin");
        let pkg_root = clove_home_dir
            .join("pkgs")
            .join("example")
            .join("flappy")
            .join("deadbeef");
        let pkg_plugin = pkg_root.join("plugins").join(&plugin_name);
        fs::create_dir_all(pkg_plugin.parent().unwrap()).expect("create pkg plugins");
        fs::write(&pkg_plugin, "hash-check").expect("write pkg plugin");

        let rel_path = Path::new("plugins").join(&plugin_name);
        let lock_json = serde_json::json!({
            "deps": {
                "example/flappy": {
                    "origin_url": "local",
                    "commit": "deadbeef",
                    "native_plugins": [{
                        "rel_path": rel_path.to_string_lossy(),
                        "sha256": "deadbeef",
                    }]
                }
            }
        });
        fs::write(
            project_root.join("clove.lock.json"),
            format!("{}\n", serde_json::to_string_pretty(&lock_json).unwrap()),
        )
        .expect("write lock");

        let err = can_load_native_plugin(&pkg_plugin, &ctx, false).expect_err("hash mismatch");
        assert!(err
            .to_string()
            .contains("sha256 recorded in the lockfile"));
    }
}
