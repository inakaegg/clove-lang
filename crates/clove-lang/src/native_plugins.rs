use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

use clove_core::ast::{FnArity, Value};
use clove_core::error::CloveError;
use clove_core::runtime::RuntimeCtx;
use once_cell::sync::Lazy;

#[derive(Clone, Debug, Default)]
pub struct NativePluginConfig {
    pub allow: bool,
    pub plugin_dirs: Vec<PathBuf>,
    pub auto_plugin_dirs: Vec<PathBuf>,
    pub base_dir: Option<PathBuf>,
    pub exe_dir: Option<PathBuf>,
}

static CONFIG: Lazy<RwLock<NativePluginConfig>> =
    Lazy::new(|| RwLock::new(NativePluginConfig::default()));

pub fn set_native_plugin_config(config: NativePluginConfig) {
    if let Ok(mut guard) = CONFIG.write() {
        *guard = config;
    }
}

fn current_config() -> NativePluginConfig {
    CONFIG.read().map(|guard| guard.clone()).unwrap_or_default()
}

pub fn install(ctx: &RuntimeCtx) {
    #[cfg(not(target_family = "wasm"))]
    install_native(ctx);
    #[cfg(target_family = "wasm")]
    install_stub(ctx);
}

#[cfg(target_family = "wasm")]
fn install_stub(ctx: &RuntimeCtx) {
    let env = ctx.env();
    let mut writer = env.write().unwrap();
    let func = Value::native_fn_with_name("require-native", FnArity::exact(1), |_args| {
        Err(CloveError::runtime(
            "native plugins are not supported in this build",
        ))
    });
    writer.define_builtin("require-native", func);
}

#[cfg(not(target_family = "wasm"))]
fn install_native(ctx: &RuntimeCtx) {
    use crate::deps;
    use crate::native_policy;
    use clove_core::env::EnvRef;
    use clove_core::package_registry::clove_home;
    use clove_core::plugin_host::EnvHandleBox;
    use clove_plugin_api::PluginInitV1;
    use libloading::{Library, Symbol};
    use std::collections::HashSet;

    #[derive(Clone, Copy)]
    struct EnvHandlePtr(clove_plugin_api::EnvHandle);

    // EnvHandle is an immutable pointer; lifetime checks are delegated to the host side.
    unsafe impl Send for EnvHandlePtr {}
    unsafe impl Sync for EnvHandlePtr {}

    struct PluginManager {
        allow: bool,
        plugin_dirs: Vec<PathBuf>,
        auto_plugin_dirs: Vec<PathBuf>,
        base_dir: PathBuf,
        exe_dir: PathBuf,
        loaded_paths: HashSet<PathBuf>,
        libraries: Vec<Library>,
        env_handle: EnvHandlePtr,
        _env_handle_box: EnvHandleBox,
    }

    impl PluginManager {
        fn new(env: EnvRef, runtime_id: usize, config: NativePluginConfig) -> Self {
            let env_handle_box = EnvHandleBox::new(env, runtime_id);
            let env_handle = EnvHandlePtr(env_handle_box.handle());
            let base_dir = config
                .base_dir
                .clone()
                .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
            let exe_dir = config.exe_dir.clone().unwrap_or_else(|| base_dir.clone());
            Self {
                allow: config.allow,
                plugin_dirs: config.plugin_dirs,
                auto_plugin_dirs: config.auto_plugin_dirs,
                base_dir,
                exe_dir,
                loaded_paths: HashSet::new(),
                libraries: Vec::new(),
                env_handle,
                _env_handle_box: env_handle_box,
            }
        }

        fn load(&mut self, name: &str) -> Result<PathBuf, CloveError> {
            let search_dirs = self.search_dirs()?;
            let mut attempted = Vec::new();
            let mut found = None;
            for dir in search_dirs {
                for filename in plugin_filenames(name) {
                    let path = dir.join(filename);
                    attempted.push(path.clone());
                    if path.exists() {
                        found = Some(path);
                        break;
                    }
                }
                if found.is_some() {
                    break;
                }
            }
            let path = match found {
                Some(path) => path,
                None => {
                    return Err(CloveError::runtime(format!(
                        "native plugin '{}' not found (searched: {})",
                        name,
                        render_paths(&attempted)
                    )))
                }
            };
            if !self.allow {
                RuntimeCtx::with_current(|ctx| {
                    native_policy::can_load_native_plugin(&path, &ctx, self.allow).map(|_| ())
                })?;
            }
            let absolute = path.canonicalize().unwrap_or(path);
            if self.loaded_paths.contains(&absolute) {
                return Ok(absolute);
            }
            let lib = unsafe {
                Library::new(&absolute).map_err(|e| {
                    CloveError::runtime(format!(
                        "failed to load native plugin {}: {}",
                        absolute.display(),
                        e
                    ))
                })?
            };
            let init: Symbol<PluginInitV1> = unsafe {
                lib.get(b"clove_plugin_init_v1").map_err(|e| {
                    CloveError::runtime(format!(
                        "native plugin init symbol missing ({}): {}",
                        absolute.display(),
                        e
                    ))
                })?
            };
            let host = clove_core::plugin_host::host_api_v1();
            let code = unsafe { init(host as *const _, self.env_handle.0) };
            if code != 0 {
                return Err(CloveError::runtime(format!(
                    "native plugin init failed ({}): code {}",
                    absolute.display(),
                    code
                )));
            }
            self.loaded_paths.insert(absolute.clone());
            self.libraries.push(lib);
            Ok(absolute)
        }

        fn search_dirs(&self) -> Result<Vec<PathBuf>, CloveError> {
            let mut base_dirs = if !self.plugin_dirs.is_empty() {
                self.plugin_dirs.clone()
            } else if let Some(env_dirs) = env_plugin_dirs()? {
                env_dirs
            } else {
                default_plugin_dirs(&self.exe_dir, &self.base_dir)?
            };
            base_dirs.extend(self.auto_plugin_dirs.iter().cloned());
            let mut seen = HashSet::new();
            let mut dirs = Vec::new();
            for dir in base_dirs {
                let normalized = normalize_dir(&self.base_dir, &dir)?;
                if seen.insert(normalized.clone()) {
                    dirs.push(normalized);
                }
            }
            Ok(dirs)
        }
    }

    fn env_plugin_dirs() -> Result<Option<Vec<PathBuf>>, CloveError> {
        let raw = match std::env::var_os("CLOVE_PLUGIN_DIR") {
            Some(raw) => raw,
            None => return Ok(None),
        };
        let mut dirs: Vec<PathBuf> = std::env::split_paths(&raw).collect();
        dirs.retain(|dir| !dir.as_os_str().is_empty());
        if dirs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(dirs))
        }
    }

    fn default_plugin_dirs(exe_dir: &Path, base_dir: &Path) -> Result<Vec<PathBuf>, CloveError> {
        let mut dirs = Vec::new();
        dirs.push(exe_dir.join("plugins"));
        if let Some(project_root) = deps::find_project_root(base_dir) {
            dirs.push(project_root.join("plugins"));
            dirs.push(project_root.join(".clove").join("plugins"));
        }
        dirs.push(clove_home().join("plugins"));
        Ok(dirs)
    }

    fn normalize_dir(base_dir: &Path, dir: &Path) -> Result<PathBuf, CloveError> {
        if dir.is_absolute() {
            Ok(dir.to_path_buf())
        } else {
            Ok(base_dir.join(dir))
        }
    }

    fn plugin_filenames(name: &str) -> Vec<String> {
        let base = name.replace('-', "_");
        if cfg!(target_os = "windows") {
            vec![format!("{}.dll", base), format!("{}_plugin.dll", base)]
        } else if cfg!(target_os = "macos") {
            vec![
                format!("lib{}.dylib", base),
                format!("lib{}_plugin.dylib", base),
            ]
        } else {
            vec![format!("lib{}.so", base), format!("lib{}_plugin.so", base)]
        }
    }

    fn render_paths(paths: &[PathBuf]) -> String {
        let mut items: Vec<String> = paths.iter().map(|p| p.display().to_string()).collect();
        items.sort();
        items.join(", ")
    }

    fn plugin_namespace(name: &str) -> Option<String> {
        let stripped = name
            .strip_prefix("clove-")
            .or_else(|| name.strip_prefix("clove_"))
            .unwrap_or(name);
        if stripped.is_empty() {
            None
        } else {
            Some(stripped.to_string())
        }
    }

    let config = current_config();
    let runtime_id = ctx.runtime_id();
    let env = ctx.env();
    let manager = Arc::new(Mutex::new(PluginManager::new(
        env.clone(),
        runtime_id,
        config,
    )));
    let mut writer = env.write().unwrap();
    let manager_clone = manager.clone();
    let func =
        Value::native_fn_with_name(
            "require-native",
            FnArity::exact(1),
            move |args| match args {
                [Value::String(name)] => {
                    let mut loader = manager_clone
                        .lock()
                        .map_err(|_| CloveError::runtime("native plugin loader lock poisoned"))?;
                    let _path = loader.load(name)?;
                    if let Some(ns) = plugin_namespace(name) {
                        RuntimeCtx::with_current(|ctx| {
                            ctx.register_builtin_namespace(&ns);
                            Ok(Value::Nil)
                        })?;
                    }
                    Ok(Value::Bool(true))
                }
                _ => Err(CloveError::runtime(
                    "require-native expects plugin name string",
                )),
            },
        );
    writer.define_builtin("require-native", func);
}
