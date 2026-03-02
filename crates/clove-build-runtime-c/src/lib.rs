#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryModel {
    ArenaRc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeConfig {
    pub memory_model: MemoryModel,
    pub allow_external_c_libs: bool,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            memory_model: MemoryModel::ArenaRc,
            allow_external_c_libs: true,
        }
    }
}

pub fn runtime_banner(config: &RuntimeConfig) -> String {
    format!(
        "phase2 C runtime (memory={:?}, external_c_libs={})",
        config.memory_model, config.allow_external_c_libs
    )
}

#[cfg(test)]
mod tests {
    use super::{runtime_banner, RuntimeConfig};

    #[test]
    fn banner_contains_memory_model() {
        let banner = runtime_banner(&RuntimeConfig::default());
        assert!(banner.contains("ArenaRc"));
    }
}
