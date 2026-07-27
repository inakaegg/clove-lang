use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicBool, Ordering};
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

pub const SYNTAX_FEATURE_COUNT: usize = 6;

impl SyntaxFeatureId {
    /// Index into [`PackageFlags::syntax`]. Keeping the flags in a fixed array instead of
    /// a `HashMap<String, bool>` matters: these toggles are consulted while evaluating
    /// each form, and the map version rebuilt itself (allocating six strings) on every
    /// lookup.
    fn index(self) -> usize {
        match self {
            SyntaxFeatureId::DotChain => 0,
            SyntaxFeatureId::DotIndexer => 1,
            SyntaxFeatureId::Indexer => 2,
            SyntaxFeatureId::ForeignBlocks => 3,
            SyntaxFeatureId::OopSyntax => 4,
            SyntaxFeatureId::MapRefs => 5,
        }
    }

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

#[derive(Clone, Copy)]
struct PackageFlags {
    syntax: [bool; SYNTAX_FEATURE_COUNT],
    repl_on_error: bool,
}

impl PackageFlags {
    fn syntax_enabled(&self, id: SyntaxFeatureId) -> bool {
        self.syntax[id.index()]
    }

    fn set_syntax(&mut self, id: SyntaxFeatureId, enabled: bool) {
        self.syntax[id.index()] = enabled;
    }

    fn value(&self, feature: FeatureToggle) -> bool {
        match feature {
            FeatureToggle::Syntax(id) => self.syntax_enabled(id),
            FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError) => self.repl_on_error,
        }
    }
}

impl Default for PackageFlags {
    /// Every syntax feature is on unless a package turns it off.
    fn default() -> Self {
        Self {
            syntax: [true; SYNTAX_FEATURE_COUNT],
            repl_on_error: false,
        }
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
    /// Whether any package has overridden a feature toggle. Mirrors
    /// `SettingsData::pkg_flags` being non-empty so the hot path can skip the lock.
    has_pkg_flags: Arc<AtomicBool>,
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
            has_pkg_flags: Arc::new(AtomicBool::new(false)),
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
        let entry = guard.pkg_flags.entry(pkg_id.to_string()).or_default();
        match feature {
            FeatureToggle::Syntax(id) => entry.set_syntax(id, enabled),
            FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError) => {
                entry.repl_on_error = enabled;
            }
        }
        self.has_pkg_flags.store(true, Ordering::Relaxed);
    }

    /// Answer without knowing the package when no package has overridden anything.
    ///
    /// The caller would otherwise have to resolve the current namespace to a package id,
    /// which allocates a `String` — per feature check, per form. Scripts and most
    /// packages never override a toggle, so this is the common path.
    pub fn feature_toggle_default(&self, feature: FeatureToggle) -> Option<bool> {
        if self.has_pkg_flags.load(Ordering::Relaxed) {
            return None;
        }
        Some(PackageFlags::default().value(feature))
    }

    pub fn feature_toggle_enabled(&self, feature: FeatureToggle, pkg_id: &str) -> bool {
        let guard = self.inner.read().unwrap();
        guard
            .pkg_flags
            .get(pkg_id)
            .copied()
            .unwrap_or_default()
            .value(feature)
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
