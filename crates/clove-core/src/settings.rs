use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock};

pub const REPL_ON_ERROR_VAR: &str = "*repl-on-error*";
pub const MAIN_PACKAGE_ID: &str = "<main>";
pub const DOT_CHAIN_FEATURE: &str = "dot-chain";
pub const DOT_INDEXER_FEATURE: &str = "dot-indexer";
pub const INDEXER_FEATURE: &str = "indexer";
pub const FOREIGN_BLOCK_FEATURE: &str = "foreign-block";
pub const OOP_SYNTAX_FEATURE: &str = "oop-syntax";
pub const MAP_REFS_FEATURE: &str = "map-refs";

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SyntaxFeatureId {
    DotChain,
    DotIndexer,
    Indexer,
    ForeignBlocks,
    OopSyntax,
    MapRefs,
}

impl SyntaxFeatureId {
    pub fn key(self) -> &'static str {
        match self {
            SyntaxFeatureId::DotChain => DOT_CHAIN_FEATURE,
            SyntaxFeatureId::DotIndexer => DOT_INDEXER_FEATURE,
            SyntaxFeatureId::Indexer => INDEXER_FEATURE,
            SyntaxFeatureId::ForeignBlocks => FOREIGN_BLOCK_FEATURE,
            SyntaxFeatureId::OopSyntax => OOP_SYNTAX_FEATURE,
            SyntaxFeatureId::MapRefs => MAP_REFS_FEATURE,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuntimeFeatureId {
    ReplOnError,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FeatureToggle {
    Syntax(SyntaxFeatureId),
    Runtime(RuntimeFeatureId),
}

pub fn canonical_syntax_feature(sym: &str) -> Option<SyntaxFeatureId> {
    match sym {
        s if s == DOT_CHAIN_FEATURE => Some(SyntaxFeatureId::DotChain),
        "dotchain-syntax"
        | "dot-chain-syntax"
        | "dotchain"
        | "dot-chain"
        | "dot-pipeline"
        | "dot-pipeline-syntax"
        | "clove.syntax.dot-pipeline"
        | "clove.syntax.dot-chain" => Some(SyntaxFeatureId::DotChain),
        s if s == DOT_INDEXER_FEATURE => Some(SyntaxFeatureId::DotIndexer),
        "dot-indexer" | "dot-indexer-syntax" => Some(SyntaxFeatureId::DotIndexer),
        s if s == OOP_SYNTAX_FEATURE => Some(SyntaxFeatureId::OopSyntax),
        "oop" | "oop-syntax" | "method-chain" | "clove.syntax.oop" => {
            Some(SyntaxFeatureId::OopSyntax)
        }
        s if s == INDEXER_FEATURE => Some(SyntaxFeatureId::Indexer),
        "indexer"
        | "indexer-syntax"
        | "map-index"
        | "map-indexer"
        | "map-indexer-syntax"
        | "clove.syntax.map-index" => Some(SyntaxFeatureId::Indexer),
        s if s == MAP_REFS_FEATURE => Some(SyntaxFeatureId::MapRefs),
        "map-refs" | "map-refs-syntax" | "clove.syntax.map-refs" => Some(SyntaxFeatureId::MapRefs),
        s if s == FOREIGN_BLOCK_FEATURE => Some(SyntaxFeatureId::ForeignBlocks),
        "foreign" | "foreign-block" | "foreign-blocks" | "foreign-literal" | "foreign-literals"
        | "$rb" | "$py" => Some(SyntaxFeatureId::ForeignBlocks),
        _ => None,
    }
}

pub fn canonical_runtime_feature(sym: &str) -> Option<RuntimeFeatureId> {
    match sym {
        "repl-on-error" | "*repl-on-error*" => Some(RuntimeFeatureId::ReplOnError),
        _ => None,
    }
}

pub fn canonical_feature_toggle(sym: &str) -> Option<FeatureToggle> {
    if let Some(syntax) = canonical_syntax_feature(sym) {
        return Some(FeatureToggle::Syntax(syntax));
    }
    canonical_runtime_feature(sym).map(FeatureToggle::Runtime)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NamespaceOrigin {
    UserCode,
    StdLib,
    ExternalLib,
}

impl NamespaceOrigin {
    pub fn is_user_code(self) -> bool {
        matches!(self, NamespaceOrigin::UserCode)
    }
}

type PackageId = String;

const DEFAULT_SYNTAX_FEATURES: &[&str] = &[
    DOT_CHAIN_FEATURE,
    DOT_INDEXER_FEATURE,
    INDEXER_FEATURE,
    FOREIGN_BLOCK_FEATURE,
    OOP_SYNTAX_FEATURE,
    MAP_REFS_FEATURE,
];

#[derive(Clone)]
struct PackageFlags {
    syntax: HashMap<String, bool>,
    repl_on_error: bool,
}

impl PackageFlags {
    fn with_defaults() -> Self {
        let mut syntax = HashMap::new();
        for feature in DEFAULT_SYNTAX_FEATURES {
            syntax.insert((*feature).to_string(), true);
        }
        Self {
            syntax,
            repl_on_error: false,
        }
    }

    fn syntax_enabled(&self, feature: &str) -> bool {
        self.syntax.get(feature).copied().unwrap_or(false)
    }
}

impl Default for PackageFlags {
    fn default() -> Self {
        Self::with_defaults()
    }
}

#[derive(Default)]
struct SettingsData {
    pkg_flags: HashMap<PackageId, PackageFlags>,
    ns_origins: HashMap<String, NamespaceOrigin>,
    ns_packages: HashMap<String, PackageId>,
    loaded_pkg_config: HashSet<PackageId>,
}

#[derive(Clone, Default)]
pub struct RuntimeSettings {
    inner: Arc<RwLock<SettingsData>>,
}

impl PartialEq for RuntimeSettings {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for RuntimeSettings {}

impl Hash for RuntimeSettings {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state);
    }
}

impl RuntimeSettings {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(RwLock::new(SettingsData {
                pkg_flags: HashMap::new(),
                ns_origins: HashMap::new(),
                ns_packages: HashMap::new(),
                loaded_pkg_config: HashSet::new(),
            })),
        }
    }

    pub fn set_namespace_origin(&self, ns: &str, origin: NamespaceOrigin) {
        let mut guard = self.inner.write().unwrap();
        guard.ns_origins.insert(ns.to_string(), origin);
    }

    pub fn namespace_origin(&self, ns: Option<&str>) -> NamespaceOrigin {
        let guard = self.inner.read().unwrap();
        match ns {
            Some(name) => guard
                .ns_origins
                .get(name)
                .copied()
                .unwrap_or(NamespaceOrigin::UserCode),
            None => NamespaceOrigin::UserCode,
        }
    }

    pub fn set_namespace_package(&self, ns: &str, pkg_id: &str) {
        let mut guard = self.inner.write().unwrap();
        guard.ns_packages.insert(ns.to_string(), pkg_id.to_string());
    }

    pub fn namespace_package(&self, ns: &str) -> Option<String> {
        let guard = self.inner.read().unwrap();
        guard.ns_packages.get(ns).cloned()
    }

    pub fn package_for_namespace(&self, ns: Option<&str>) -> String {
        match ns {
            Some(name) => self
                .namespace_package(name)
                .unwrap_or_else(|| MAIN_PACKAGE_ID.to_string()),
            None => MAIN_PACKAGE_ID.to_string(),
        }
    }

    pub fn assign_feature_toggle(&self, feature: FeatureToggle, pkg_id: &str, enabled: bool) {
        let mut guard = self.inner.write().unwrap();
        let entry = guard
            .pkg_flags
            .entry(pkg_id.to_string())
            .or_insert_with(PackageFlags::default);
        match feature {
            FeatureToggle::Syntax(id) => {
                entry.syntax.insert(id.key().to_string(), enabled);
            }
            FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError) => {
                entry.repl_on_error = enabled;
            }
        }
    }

    pub fn feature_toggle_enabled(&self, feature: FeatureToggle, pkg_id: &str) -> bool {
        let guard = self.inner.read().unwrap();
        let default_flags = PackageFlags::default();
        let flags = guard.pkg_flags.get(pkg_id).unwrap_or(&default_flags);
        match feature {
            FeatureToggle::Syntax(id) => flags.syntax_enabled(id.key()),
            FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError) => flags.repl_on_error,
        }
    }

    pub fn repl_on_error_enabled_any(&self) -> bool {
        let guard = self.inner.read().unwrap();
        guard.pkg_flags.values().any(|flags| flags.repl_on_error)
    }

    pub fn mark_pkg_config_loaded(&self, pkg_id: &str) -> bool {
        let mut guard = self.inner.write().unwrap();
        guard.loaded_pkg_config.insert(pkg_id.to_string())
    }

    pub fn pkg_config_loaded(&self, pkg_id: &str) -> bool {
        let guard = self.inner.read().unwrap();
        guard.loaded_pkg_config.contains(pkg_id)
    }
}
