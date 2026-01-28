use std::collections::HashMap;
use std::env;
use std::path::PathBuf;

fn env_flag(name: &str) -> bool {
    let Ok(value) = env::var(name) else {
        return false;
    };
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

pub fn use_vm_from_env() -> bool {
    env_flag("CLOVE_USE_VM")
}

pub fn vm_profiler_from_env() -> bool {
    env_flag("CLOVE_VM_PROF")
}

#[derive(Clone, Debug)]
pub struct EvalOptions {
    pub auto_fallback: bool,
    pub use_vm: bool,
    pub vm_profiler: bool,
    pub no_std: bool,
    pub source_name: Option<String>,
    pub working_dir: Option<PathBuf>,
    pub apply_working_dir: bool,
    pub config_home: Option<PathBuf>,
    pub package_overrides: HashMap<String, String>,
}

impl Default for EvalOptions {
    fn default() -> Self {
        Self {
            auto_fallback: false,
            use_vm: use_vm_from_env(),
            vm_profiler: vm_profiler_from_env(),
            no_std: false,
            source_name: None,
            working_dir: None,
            apply_working_dir: true,
            config_home: None,
            package_overrides: HashMap::new(),
        }
    }
}
