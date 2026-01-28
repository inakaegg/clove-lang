use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::time::{Duration, Instant};

use clove_core::{
    ast::{Form, FormKind, Span},
    error::CloveError,
    form_to_string::form_to_string,
    parse_source_for_lsp,
    runtime::{lsp_namespace_info, parse_source_for_lsp_lenient, RequireSpec, RequireTarget},
    symbols::{canonical_symbol_name, doc_lookup_keys, symbol_aliases},
    types::TypeKind,
};
use ignore::WalkBuilder;
use serde_json::json;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic, DiagnosticSeverity,
    DocumentSymbol, Documentation, Location, MarkupContent, MarkupKind, Position, Range,
    SymbolKind as LspSymbolKind, TextEdit, Url,
};

use crate::builtin_stubs;
use crate::lsp_runtime::LspRuntime;

const NAMESPACE_MISS_TTL: Duration = Duration::from_secs(30);

pub(crate) struct DocumentData {
    pub(crate) text: String,
    pub(crate) language_id: Option<String>,
    pub(crate) source_path: Option<PathBuf>,
    pub(crate) workspace_root: Option<PathBuf>,
    pub(crate) ast: Option<Vec<Form>>, // Keep parsed forms for now (unused)
    pub(crate) symbols: SymbolIndex,
    pub(crate) map_shapes: HashMap<String, MapShape>,
    pub(crate) is_virtual: bool,
    pub(crate) is_open: bool,
    pub(crate) namespace: Option<String>,
    pub(crate) namespace_aliases: Vec<String>,
    pub(crate) namespace_span: Option<Range>,
    pub(crate) requires: Vec<RequireSpec>,
    pub(crate) parse_diagnostics: Vec<Diagnostic>,
    pub(crate) type_diagnostics: Vec<Diagnostic>,
    pub(crate) type_map: HashMap<usize, String>,
    pub(crate) hint_map: HashMap<usize, TypeKind>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RefactorPrefs {
    pub(crate) prefer_kw_chain: bool,
}

impl Default for RefactorPrefs {
    fn default() -> Self {
        Self {
            prefer_kw_chain: true,
        }
    }
}

impl Default for DocumentData {
    fn default() -> Self {
        Self {
            text: String::new(),
            language_id: None,
            source_path: None,
            workspace_root: None,
            ast: None,
            symbols: SymbolIndex::default(),
            map_shapes: HashMap::new(),
            is_virtual: false,
            is_open: false,
            namespace: None,
            namespace_aliases: Vec::new(),
            namespace_span: None,
            requires: Vec::new(),
            parse_diagnostics: Vec::new(),
            type_diagnostics: Vec::new(),
            type_map: HashMap::new(),
            hint_map: HashMap::new(),
        }
    }
}

pub(crate) struct DocumentStore {
    pub(crate) docs: HashMap<Url, DocumentData>,
    pub(crate) path_index: HashMap<PathBuf, Url>,
    pub(crate) indexed_files: HashSet<PathBuf>,
    pub(crate) scanned_dirs: HashSet<PathBuf>,
    pub(crate) indexed_roots: HashSet<PathBuf>,
    pub(crate) workspace_roots: Vec<PathBuf>,
    pub(crate) ns_to_uris: HashMap<String, Vec<Url>>,
    pub(crate) uri_to_namespaces: HashMap<Url, Vec<String>>,
    pub(crate) missing_namespaces: HashMap<String, Instant>,
    pub(crate) lsp_runtime: LspRuntime,
    pub(crate) refactor_prefs: RefactorPrefs,
    builtin_stub_uri: Option<Url>,
    builtin_stub_hash: Option<String>,
}

impl DocumentStore {
    fn new() -> Self {
        let mut store = Self {
            docs: HashMap::new(),
            path_index: HashMap::new(),
            indexed_files: HashSet::new(),
            scanned_dirs: HashSet::new(),
            indexed_roots: HashSet::new(),
            workspace_roots: Vec::new(),
            ns_to_uris: HashMap::new(),
            uri_to_namespaces: HashMap::new(),
            missing_namespaces: HashMap::new(),
            lsp_runtime: LspRuntime::new(),
            refactor_prefs: RefactorPrefs::default(),
            builtin_stub_uri: None,
            builtin_stub_hash: None,
        };
        store.load_builtin_stubs();
        store
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IndexMode {
    Full,
    Light,
}

impl DocumentStore {
    pub(crate) fn set_workspace_roots(&mut self, roots: Vec<PathBuf>) {
        let mut seen = HashSet::new();
        let mut normalized = Vec::new();
        for root in roots {
            let normalized_root = normalize_path(&root);
            if seen.insert(normalized_root.clone()) {
                normalized.push(normalized_root);
            }
        }
        self.workspace_roots = normalized;
        self.load_builtin_stubs();
    }

    pub(crate) fn refactor_prefs(&self) -> RefactorPrefs {
        self.refactor_prefs
    }

    pub(crate) fn set_refactor_prefs(&mut self, prefs: RefactorPrefs) {
        self.refactor_prefs = prefs;
    }

    pub(crate) fn scan_all_workspace_roots(&mut self) {
        let roots = self.workspace_roots.clone();
        for root in roots {
            self.scan_root_if_needed(&root);
        }
    }

    fn workspace_root_for(&self, path: &Path) -> Option<PathBuf> {
        let mut best_prefix: Option<PathBuf> = None;
        for root in &self.workspace_roots {
            if path.starts_with(root) {
                let root_norm = normalize_path(root);
                if best_prefix
                    .as_ref()
                    .map(|p| path_depth(&root_norm) > path_depth(p))
                    .unwrap_or(true)
                {
                    best_prefix = Some(root_norm);
                }
            }
        }
        let detected = detect_workspace_root(path).map(|p| normalize_path(&p));
        match (best_prefix, detected) {
            (Some(prefix), Some(det)) => {
                if path_depth(&det) > path_depth(&prefix) {
                    Some(det)
                } else {
                    Some(prefix)
                }
            }
            (Some(prefix), None) => Some(prefix),
            (None, Some(det)) => Some(det),
            (None, None) => None,
        }
    }

    pub(crate) fn open_or_update(&mut self, uri: Url, text: String) -> Vec<Diagnostic> {
        self.open_or_update_with_kind_and_language_and_mode(
            uri,
            text,
            false,
            None,
            IndexMode::Full,
            true,
        )
    }

    pub(crate) fn open_or_update_with_language(
        &mut self,
        uri: Url,
        text: String,
        language_id: Option<&str>,
    ) -> Vec<Diagnostic> {
        self.open_or_update_with_kind_and_language_and_mode(
            uri,
            text,
            false,
            language_id,
            IndexMode::Full,
            true,
        )
    }

    pub(crate) fn open_or_update_with_kind(
        &mut self,
        uri: Url,
        text: String,
        is_virtual: bool,
    ) -> Vec<Diagnostic> {
        self.open_or_update_with_kind_and_language_and_mode(
            uri,
            text,
            is_virtual,
            None,
            IndexMode::Full,
            false,
        )
    }

    pub(crate) fn open_or_update_light(&mut self, uri: Url, text: String) -> Vec<Diagnostic> {
        self.open_or_update_with_kind_and_language_and_mode(
            uri,
            text,
            false,
            None,
            IndexMode::Light,
            false,
        )
    }

    pub(crate) fn update_open_text(&mut self, uri: &Url, text: String) {
        let data = self.docs.entry(uri.clone()).or_default();
        data.text = text;
        data.is_open = true;
        data.ast = None;
        data.type_diagnostics.clear();
        data.type_map.clear();
        data.hint_map.clear();
        data.parse_diagnostics.clear();
    }

    pub(crate) fn downgrade_document(&mut self, uri: &Url) {
        let (text, is_virtual, language_id) = match self.docs.get(uri) {
            Some(data) => (data.text.clone(), data.is_virtual, data.language_id.clone()),
            None => return,
        };
        if text.is_empty() {
            if let Some(data) = self.docs.get_mut(uri) {
                data.is_open = false;
            }
            return;
        }
        let _ = self.open_or_update_with_kind_and_language_and_mode(
            uri.clone(),
            text,
            is_virtual,
            language_id.as_deref(),
            IndexMode::Light,
            false,
        );
    }

    fn open_or_update_with_kind_and_language_and_mode(
        &mut self,
        uri: Url,
        text: String,
        is_virtual: bool,
        language_id: Option<&str>,
        mode: IndexMode,
        is_open: bool,
    ) -> Vec<Diagnostic> {
        self.unregister_namespace(&uri);
        let mut namespace_for_registration = None;
        let mut namespace_aliases_for_registration = Vec::new();
        let mut requires_for_preload = Vec::new();
        let prior_language_id = self
            .docs
            .get(&uri)
            .and_then(|data| data.language_id.clone());
        let language_id = language_id
            .map(|value| value.to_string())
            .or(prior_language_id.clone());
        let normalized_path = uri.to_file_path().ok().map(|p| normalize_path(&p));
        let workspace_root = normalized_path
            .as_ref()
            .and_then(|path| self.workspace_root_for(path));
        let should_parse = should_parse_document(&uri, language_id.as_deref(), is_virtual);
        let diagnostics = {
            let data = self.docs.entry(uri.clone()).or_default();
            if is_open {
                data.text = text.clone();
            } else {
                data.text.clear();
            }
            data.language_id = language_id.clone();
            data.source_path = normalized_path.clone();
            data.workspace_root = workspace_root.clone();
            data.ast = None;
            data.symbols = SymbolIndex::default();
            data.is_virtual = is_virtual;
            data.is_open = is_open;
            data.namespace = None;
            data.namespace_aliases.clear();
            data.namespace_span = None;
            data.requires.clear();
            data.parse_diagnostics.clear();
            data.type_diagnostics.clear();
            data.type_map.clear();
            data.hint_map.clear();
            data.map_shapes.clear();
            if let Some(path) = normalized_path.clone() {
                self.path_index.insert(path.clone(), uri.clone());
                self.indexed_files.insert(path);
            }
            if !should_parse {
                let diagnostics = Vec::new();
                data.parse_diagnostics = diagnostics.clone();
                return diagnostics;
            }
            let diagnostics = match mode {
                IndexMode::Full => parse_and_index_full(
                    data,
                    &text,
                    uri.to_file_path()
                        .ok()
                        .and_then(|p| p.to_str().map(|s| s.to_string()))
                        .as_deref(),
                    workspace_root.as_deref(),
                    normalized_path.as_ref(),
                    &mut namespace_for_registration,
                    &mut namespace_aliases_for_registration,
                    &mut requires_for_preload,
                ),
                IndexMode::Light => parse_and_index_light(
                    data,
                    &text,
                    uri.to_file_path()
                        .ok()
                        .and_then(|p| p.to_str().map(|s| s.to_string()))
                        .as_deref(),
                    workspace_root.as_deref(),
                    normalized_path.as_ref(),
                    &mut namespace_for_registration,
                    &mut namespace_aliases_for_registration,
                    &mut requires_for_preload,
                ),
            };
            data.parse_diagnostics = diagnostics.clone();
            diagnostics
        };
        if !requires_for_preload.is_empty() {
            self.preload_requires(&requires_for_preload);
        }
        self.register_namespace(
            &uri,
            namespace_for_registration,
            &namespace_aliases_for_registration,
        );
        diagnostics
    }

    pub(crate) fn get(&self, uri: &Url) -> Option<&DocumentData> {
        self.docs.get(uri)
    }

    pub(crate) fn find_definitions(&self, name: &str, namespace: Option<&str>) -> Vec<Location> {
        let mut locations = Vec::new();
        for (uri, doc) in &self.docs {
            if let Some(infos) = doc.symbols.lookup(name) {
                for info in infos {
                    if let Some(ns) = namespace {
                        let mut ok = info.namespace.as_deref() == Some(ns);
                        if !ok && info.namespace_aliases.iter().any(|a| a == ns) {
                            ok = true;
                        }
                        if !ok {
                            continue;
                        }
                    }
                    locations.push(Location {
                        uri: uri.clone(),
                        range: info.range,
                    });
                }
            }
        }
        locations
    }

    pub(crate) fn find_type_definitions(
        &self,
        name: &str,
        namespace: Option<&str>,
    ) -> Vec<Location> {
        let mut locations = Vec::new();
        for (uri, doc) in &self.docs {
            if let Some(infos) = doc.symbols.lookup(name) {
                for info in infos.iter().filter(|info| info.is_type()) {
                    if let Some(ns) = namespace {
                        let mut ok = info.namespace.as_deref() == Some(ns);
                        if !ok && info.namespace_aliases.iter().any(|a| a == ns) {
                            ok = true;
                        }
                        if !ok {
                            continue;
                        }
                    }
                    locations.push(Location {
                        uri: uri.clone(),
                        range: info.range,
                    });
                }
            }
        }
        locations
    }

    pub(crate) fn find_enum_variant_location(
        &self,
        enum_name: &str,
        variant_name: &str,
        namespace: Option<&str>,
    ) -> Option<Location> {
        for (uri, doc) in &self.docs {
            if let Some(infos) = doc.symbols.lookup(enum_name) {
                for info in infos
                    .iter()
                    .filter(|info| matches!(info.kind, SymbolKind::Defenum))
                {
                    if let Some(ns) = namespace {
                        let mut ok = info.namespace.as_deref() == Some(ns);
                        if !ok && info.namespace_aliases.iter().any(|a| a == ns) {
                            ok = true;
                        }
                        if !ok {
                            continue;
                        }
                    }
                    if let Some(member) = info.enum_members.iter().find(|m| m.name == variant_name)
                    {
                        return Some(Location {
                            uri: uri.clone(),
                            range: member.range,
                        });
                    }
                }
            }
        }
        None
    }

    pub(crate) fn find_symbol_infos(&self, name: &str, namespace: Option<&str>) -> Vec<SymbolInfo> {
        let mut infos = Vec::new();
        for doc in self.docs.values() {
            if let Some(entries) = doc.symbols.lookup(name) {
                for info in entries {
                    let mut ok = namespace.is_none();
                    if let Some(ns) = namespace {
                        ok = info.namespace.as_deref() == Some(ns)
                            || info.namespace_aliases.iter().any(|alias| alias == ns);
                    }
                    if ok {
                        infos.push(info.clone());
                    }
                }
            }
        }
        infos
    }

    pub(crate) fn iter_type_symbols(&self) -> impl Iterator<Item = &SymbolInfo> {
        self.docs
            .values()
            .flat_map(|doc| doc.symbols.entries().iter().filter(|info| info.is_type()))
    }

    pub(crate) fn iter_user_symbols(&self) -> impl Iterator<Item = &SymbolInfo> {
        self.docs
            .values()
            .filter(|doc| !doc.is_virtual)
            .flat_map(|doc| doc.symbols.entries())
    }

    pub(crate) fn document_symbols(&self, uri: &Url) -> Option<Vec<DocumentSymbol>> {
        let doc = self.docs.get(uri)?;
        let mut items = Vec::new();
        for info in doc.symbols.entries() {
            let detail = info
                .params
                .as_ref()
                .map(|params| format!("({})", params.join(" ")));
            #[allow(deprecated)]
            items.push(DocumentSymbol {
                name: info.name.clone(),
                detail,
                kind: lsp_symbol_kind(&info.kind),
                tags: None,
                deprecated: None,
                range: info.range.clone(),
                selection_range: info.range.clone(),
                children: None,
            });
        }
        Some(items)
    }

    pub(crate) fn user_completion_items(
        &self,
        normalized_prefix: &str,
        replace_range: &Range,
    ) -> Vec<CompletionItem> {
        let mut seen = HashSet::new();
        let mut items = Vec::new();
        for info in self.iter_user_symbols() {
            if info.is_private {
                continue;
            }
            if !symbol_info_matches_prefix(info, normalized_prefix) {
                continue;
            }
            if !seen.insert(info.name.clone()) {
                continue;
            }
            items.push(build_user_completion_item(info, replace_range));
        }
        items
    }

    pub(crate) fn lookup_user_symbol_info(&self, name: &str) -> Option<SymbolInfo> {
        self.lookup_symbol_info_filtered(name, |doc| !doc.is_virtual)
    }

    pub(crate) fn lookup_map_shape(&self, name: &str) -> Option<MapShape> {
        for key in lookup_keys_for(name) {
            for doc in self.docs.values().filter(|doc| !doc.is_virtual) {
                if let Some(shape) = doc.map_shapes.get(&key) {
                    return Some(shape.clone());
                }
            }
        }
        None
    }

    pub(crate) fn find_map_shape_location(&self, name: &str, path: &[String]) -> Option<Location> {
        for key in lookup_keys_for(name) {
            for (uri, doc) in self.docs.iter().filter(|(_, doc)| !doc.is_virtual) {
                let Some(shape) = doc.map_shapes.get(&key) else {
                    continue;
                };
                let Some(field) = map_shape_field_for_path(shape, path) else {
                    continue;
                };
                return Some(Location {
                    uri: uri.clone(),
                    range: field.range.clone(),
                });
            }
        }
        None
    }

    pub(crate) fn lookup_symbol_info_filtered<F>(
        &self,
        name: &str,
        predicate: F,
    ) -> Option<SymbolInfo>
    where
        F: Fn(&DocumentData) -> bool,
    {
        for key in lookup_keys_for(name) {
            for doc in self.docs.values().filter(|doc| predicate(doc)) {
                if let Some(infos) = doc.symbols.lookup(&key) {
                    if let Some(info) = infos.first() {
                        return Some(info.clone());
                    }
                }
            }
        }
        None
    }

    pub(crate) fn ensure_workspace_symbols(&mut self, uri: &Url) {
        let roots = self.workspace_roots.clone();
        for root in roots {
            self.scan_root_if_needed(&root);
        }
        let path = match uri.to_file_path().ok() {
            Some(p) => p,
            None => return,
        };
        let normalized = normalize_path(&path);
        if let Some(root) = self.workspace_root_for(&normalized) {
            self.scan_root_if_needed(&root);
        }
    }

    fn scan_root_if_needed(&mut self, root: &Path) {
        let normalized_root = normalize_path(root);
        if !self.indexed_roots.insert(normalized_root.clone()) {
            return;
        }
        self.scan_directory(&normalized_root);
    }

    fn scan_directory(&mut self, root: &Path) {
        let normalized_root = normalize_path(root);
        let mut walker = WalkBuilder::new(&normalized_root);
        walker.standard_filters(true).follow_links(false);
        for result in walker.build() {
            let entry = match result {
                Ok(e) => e,
                Err(_) => continue,
            };
            let file_type = match entry.file_type() {
                Some(ft) => ft,
                None => continue,
            };
            let path = entry.path();
            if file_type.is_dir() {
                let dir_norm = normalize_path(path);
                let _ = self.scanned_dirs.insert(dir_norm);
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            if is_source_file(path) {
                self.index_file_if_needed(path);
            }
        }
    }

    fn index_file_if_needed(&mut self, path: &Path) {
        let normalized = normalize_path(path);
        if self.path_index.contains_key(&normalized) || self.indexed_files.contains(&normalized) {
            return;
        }
        let text = match fs::read_to_string(&normalized) {
            Ok(t) => t,
            Err(_) => return,
        };
        let uri = match Url::from_file_path(&normalized) {
            Ok(u) => u,
            Err(_) => return,
        };
        let _ = self.open_or_update_light(uri, text);
    }

    fn load_builtin_stubs(&mut self) {
        let hash = workspace_hash(&self.workspace_roots);
        if self.builtin_stub_hash.as_deref() == Some(hash.as_str()) {
            return;
        }
        if let Some(prev_uri) = self.builtin_stub_uri.take() {
            self.remove_document(&prev_uri);
        }
        let stub = self
            .lsp_runtime
            .with_current(|_ctx| builtin_stubs::ensure_stub_file(&hash));
        let Some(stub) = stub else {
            return;
        };
        if let Ok(uri) = Url::from_file_path(&stub.path) {
            let _ = self.open_or_update_with_kind(uri.clone(), stub.contents, true);
            self.builtin_stub_uri = Some(uri);
            self.builtin_stub_hash = Some(hash);
        }
    }

    fn remove_document(&mut self, uri: &Url) {
        self.unregister_namespace(uri);
        if let Ok(path) = uri.to_file_path() {
            let normalized = normalize_path(&path);
            if let Some(existing) = self.path_index.get(&normalized) {
                if existing == uri {
                    self.path_index.remove(&normalized);
                }
            }
            self.indexed_files.remove(&normalized);
        }
        self.docs.remove(uri);
    }

    pub(crate) fn with_lsp_runtime<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        self.lsp_runtime.with_current(|_ctx| f())
    }

    fn unregister_namespace(&mut self, uri: &Url) {
        let Some(keys) = self.uri_to_namespaces.remove(uri) else {
            return;
        };
        for key in keys {
            if let Some(uris) = self.ns_to_uris.get_mut(&key) {
                uris.retain(|u| u != uri);
                if uris.is_empty() {
                    self.ns_to_uris.remove(&key);
                }
            }
        }
    }

    fn register_namespace(&mut self, uri: &Url, namespace: Option<String>, aliases: &[String]) {
        if let Some(ns) = namespace.as_ref() {
            let entry = self.ns_to_uris.entry(ns.clone()).or_default();
            if !entry.iter().any(|u| u == uri) {
                entry.push(uri.clone());
            }
        }
        for alias in aliases {
            let entry = self.ns_to_uris.entry(alias.clone()).or_default();
            if !entry.iter().any(|u| u == uri) {
                entry.push(uri.clone());
            }
        }
        let mut stored: Vec<String> = Vec::new();
        if let Some(ns) = namespace {
            stored.push(ns);
        }
        stored.extend(aliases.iter().cloned());
        stored.sort();
        stored.dedup();
        if !stored.is_empty() {
            for key in &stored {
                self.missing_namespaces.remove(key);
            }
            self.uri_to_namespaces.insert(uri.clone(), stored);
        }
    }

    pub(crate) fn namespace_location(&self, ns: &str) -> Option<Location> {
        let candidates = namespace_lookup_keys(ns);
        let mut uris_opt = None;
        for key in candidates {
            if let Some(u) = self.ns_to_uris.get(&key) {
                uris_opt = Some(u);
                break;
            }
        }
        let uris = uris_opt?;
        let uri = uris.first()?;
        let range = self
            .docs
            .get(uri)
            .and_then(|d| d.namespace_span.clone())
            .unwrap_or(Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            });
        Some(Location {
            uri: uri.clone(),
            range,
        })
    }

    pub(crate) fn ensure_namespace_loaded(&mut self, ns: &str) -> bool {
        if namespace_known(self, ns) {
            return true;
        }
        if let Some(last) = self.missing_namespaces.get(ns) {
            if last.elapsed() <= NAMESPACE_MISS_TTL {
                return false;
            }
        }
        let mut roots: Vec<PathBuf> = self.workspace_roots.clone();
        for path in self.path_index.keys() {
            if let Some(det) = detect_workspace_root(path) {
                roots.push(normalize_path(&det));
            }
        }
        roots.extend(self.indexed_roots.iter().cloned());
        roots.sort_by(|a, b| path_depth(b).cmp(&path_depth(a)).then(a.cmp(b)));
        roots.dedup();
        let roots_snapshot = roots.clone();
        let rel = ns.replace("::", "/");
        let mut tried = Vec::new();
        for root in roots {
            for ext in ["clv", "clove"] {
                let candidate = root.join(&rel).with_extension(ext);
                tried.push(candidate.clone());
                if candidate.exists() {
                    self.index_file_if_needed(&candidate);
                    if namespace_known(self, ns) {
                        self.missing_namespaces.remove(ns);
                        return true;
                    }
                }
            }
        }
        if !tried.is_empty() {
            let roots_str: Vec<String> = roots_snapshot
                .iter()
                .map(|r| r.to_string_lossy().to_string())
                .collect();
            let tried_str: Vec<String> = tried
                .iter()
                .map(|c| c.to_string_lossy().to_string())
                .collect();
            eprintln!(
                "clove-lsp WARN: ensure_namespace_loaded failed ns={} roots={:?} candidates={:?}",
                ns, roots_str, tried_str
            );
        }
        self.missing_namespaces
            .insert(ns.to_string(), Instant::now());
        false
    }

    pub(crate) fn preload_requires(&mut self, specs: &[RequireSpec]) {
        for spec in specs {
            if let Some(ns) = require_target_namespace(spec) {
                let _ = self.ensure_namespace_loaded(&ns);
            }
        }
    }
}

fn infer_namespace_with_alias(
    mut namespace: Option<String>,
    workspace_root: Option<&Path>,
    normalized_path: Option<&PathBuf>,
) -> (Option<String>, Option<String>) {
    let mut inferred_alias = None;
    if namespace.is_none() {
        if let (Some(root), Some(path)) = (workspace_root, normalized_path) {
            if let Some(inferred) = infer_namespace_from_path(root, path) {
                namespace = Some(inferred.clone());
                inferred_alias = Some(inferred);
            }
        }
    } else if let (Some(root), Some(path)) = (workspace_root, normalized_path) {
        if let Some(inferred) = infer_namespace_from_path(root, path) {
            if namespace.as_deref() != Some(&inferred) {
                inferred_alias = Some(inferred);
            }
        }
    }
    (namespace, inferred_alias)
}

fn parse_and_index_full(
    data: &mut DocumentData,
    text: &str,
    path: Option<&str>,
    workspace_root: Option<&Path>,
    normalized_path: Option<&PathBuf>,
    namespace_for_registration: &mut Option<String>,
    namespace_aliases_for_registration: &mut Vec<String>,
    requires_for_preload: &mut Vec<RequireSpec>,
) -> Vec<Diagnostic> {
    match parse_source_for_lsp(text, path) {
        Ok(forms) => {
            let mut namespace = None;
            if let Ok(ns_info) = lsp_namespace_info(&forms) {
                namespace = ns_info.namespace;
                data.requires = ns_info.requires;
                data.namespace_span = ns_info.namespace_span.map(span_to_range);
            }
            let (namespace, inferred_alias) =
                infer_namespace_with_alias(namespace, workspace_root, normalized_path);
            data.namespace = namespace;
            if let Some(alias) = inferred_alias {
                data.namespace_aliases.push(alias);
            }
            *namespace_for_registration = data.namespace.clone();
            *namespace_aliases_for_registration = data.namespace_aliases.clone();
            data.symbols = index_forms(&forms, data.namespace.as_deref(), &data.namespace_aliases);
            data.map_shapes =
                collect_def_map_shapes(&forms, data.namespace.as_deref(), &data.namespace_aliases);
            let (type_map, type_diagnostics) = infer_type_map_with_diags(&forms);
            data.type_map = type_map;
            data.type_diagnostics = type_diagnostics;
            data.hint_map = infer_hint_map(&forms);
            data.ast = Some(forms);
            *requires_for_preload = data.requires.clone();
            Vec::new()
        }
        Err(err) => {
            let namespace = extract_namespace_from_text(text);
            let (namespace, inferred_alias) =
                infer_namespace_with_alias(namespace, workspace_root, normalized_path);
            data.namespace = namespace;
            if let Some(alias) = inferred_alias {
                data.namespace_aliases.push(alias);
            }
            *namespace_for_registration = data.namespace.clone();
            *namespace_aliases_for_registration = data.namespace_aliases.clone();
            if let Ok(forms) = parse_source_for_lsp_lenient(text, path) {
                if let Ok(ns_info) = lsp_namespace_info(&forms) {
                    data.requires = ns_info.requires;
                    data.namespace_span = ns_info.namespace_span.map(span_to_range);
                }
                data.symbols =
                    index_forms(&forms, data.namespace.as_deref(), &data.namespace_aliases);
                data.map_shapes = collect_def_map_shapes(
                    &forms,
                    data.namespace.as_deref(),
                    &data.namespace_aliases,
                );
                let (type_map, type_diagnostics) = infer_type_map_with_diags(&forms);
                data.type_map = type_map;
                data.type_diagnostics = type_diagnostics;
                data.hint_map = infer_hint_map(&forms);
                data.ast = Some(forms);
                *requires_for_preload = data.requires.clone();
            }
            vec![to_diagnostic(&err)]
        }
    }
}

fn parse_and_index_light(
    data: &mut DocumentData,
    text: &str,
    path: Option<&str>,
    workspace_root: Option<&Path>,
    normalized_path: Option<&PathBuf>,
    namespace_for_registration: &mut Option<String>,
    namespace_aliases_for_registration: &mut Vec<String>,
    requires_for_preload: &mut Vec<RequireSpec>,
) -> Vec<Diagnostic> {
    if let Ok(forms) = parse_source_for_lsp_lenient(text, path) {
        let mut namespace = None;
        if let Ok(ns_info) = lsp_namespace_info(&forms) {
            namespace = ns_info.namespace;
            data.requires = ns_info.requires;
            data.namespace_span = ns_info.namespace_span.map(span_to_range);
        }
        let (namespace, inferred_alias) =
            infer_namespace_with_alias(namespace, workspace_root, normalized_path);
        data.namespace = namespace;
        if let Some(alias) = inferred_alias {
            data.namespace_aliases.push(alias);
        }
        *namespace_for_registration = data.namespace.clone();
        *namespace_aliases_for_registration = data.namespace_aliases.clone();
        data.symbols = index_forms(&forms, data.namespace.as_deref(), &data.namespace_aliases);
        data.map_shapes =
            collect_def_map_shapes(&forms, data.namespace.as_deref(), &data.namespace_aliases);
        *requires_for_preload = data.requires.clone();
    } else {
        let namespace = extract_namespace_from_text(text);
        let (namespace, inferred_alias) =
            infer_namespace_with_alias(namespace, workspace_root, normalized_path);
        data.namespace = namespace;
        if let Some(alias) = inferred_alias {
            data.namespace_aliases.push(alias);
        }
        *namespace_for_registration = data.namespace.clone();
        *namespace_aliases_for_registration = data.namespace_aliases.clone();
    }
    Vec::new()
}

fn lsp_symbol_kind(kind: &SymbolKind) -> LspSymbolKind {
    match kind {
        SymbolKind::Def | SymbolKind::Defn => LspSymbolKind::FUNCTION,
        SymbolKind::Deftype => LspSymbolKind::STRUCT,
        SymbolKind::Defenum => LspSymbolKind::ENUM,
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    if let Ok(canon) = path.canonicalize() {
        canon
    } else {
        path.to_path_buf()
    }
}

fn path_depth(path: &Path) -> usize {
    path.components().count()
}

fn workspace_hash(roots: &[PathBuf]) -> String {
    if roots.is_empty() {
        return "no-workspace".to_string();
    }
    let mut items: Vec<String> = roots
        .iter()
        .map(|root| normalize_path(root).to_string_lossy().to_string())
        .collect();
    items.sort();
    let mut hash: u64 = 0xcbf29ce484222325;
    for item in items {
        for byte in item.as_bytes() {
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }
        hash ^= 0xff;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{:016x}", hash)
}

fn should_parse_document(uri: &Url, language_id: Option<&str>, is_virtual: bool) -> bool {
    if is_virtual {
        return true;
    }
    if matches!(language_id, Some("clove")) {
        return true;
    }
    if uri.scheme() != "file" {
        return false;
    }
    if let Ok(path) = uri.to_file_path() {
        if let Some(ext) = path.extension().and_then(|ext| ext.to_str()) {
            return matches!(ext, "clv" | "clove");
        }
    }
    false
}

fn extract_namespace_from_text(text: &str) -> Option<String> {
    for line in text.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with(';') || trimmed.is_empty() {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("(ns") {
            let remainder = rest.trim_start();
            let token = remainder
                .split_whitespace()
                .next()
                .unwrap_or("")
                .trim_end_matches(')');
            let token = token.trim_end_matches(|c| c == ')');
            if !token.is_empty() {
                return Some(token.to_string());
            }
        }
    }
    None
}

fn infer_namespace_from_path(root: &Path, path: &Path) -> Option<String> {
    let rel = path.strip_prefix(root).ok()?;
    let stem = rel.with_extension("");
    let mut parts = Vec::new();
    for component in stem.iter() {
        let s = component.to_str()?;
        if s.is_empty() {
            continue;
        }
        parts.push(s.replace('/', "::"));
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("::"))
    }
}

fn detect_workspace_root(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_file() {
        path.parent()?.to_path_buf()
    } else {
        path.to_path_buf()
    };
    loop {
        if has_workspace_marker(&current) {
            return Some(current);
        }
        if !current.pop() {
            break;
        }
    }
    None
}

fn has_workspace_marker(dir: &Path) -> bool {
    const MARKERS: &[&str] = &["Cargo.toml", "package.json", ".git"];
    MARKERS.iter().any(|marker| dir.join(marker).exists())
}

fn is_source_file(path: &Path) -> bool {
    const SOURCE_EXTS: &[&str] = &["clv", "clove"];
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| {
            let lower = ext.to_ascii_lowercase();
            SOURCE_EXTS
                .iter()
                .any(|candidate| candidate.eq_ignore_ascii_case(&lower))
        })
        .unwrap_or(false)
}

// Legacy indexer (unused but kept)
fn index_forms(
    forms: &[Form],
    namespace: Option<&str>,
    namespace_aliases: &[String],
) -> SymbolIndex {
    let mut index = SymbolIndex::default();
    for form in forms {
        if let FormKind::List(items) = &form.kind {
            if items.is_empty() {
                continue;
            }
            let head = match &items[0].kind {
                FormKind::Symbol(s) => s.as_str(),
                _ => continue,
            };
            let (kind, name_idx) = match head {
                "def" | "def-" => (SymbolKind::Def, 1),
                "defn" | "defn-" => (SymbolKind::Defn, 1),
                "deftype" => (SymbolKind::Deftype, 1),
                "defenum" => (SymbolKind::Defenum, 1),
                _ => continue,
            };
            let is_private = matches!(head, "def-" | "defn-");
            let name_form = items.get(name_idx);
            let name = match name_form.and_then(|f| match &f.kind {
                FormKind::Symbol(s) => Some(s.clone()),
                _ => None,
            }) {
                Some(n) => n,
                None => continue,
            };
            let mut next_idx = name_idx + 1;
            let require_tail = matches!(head, "def" | "def-" | "deftype");
            let (docstring, after_doc) = extract_docstring(items, next_idx, require_tail);
            next_idx = after_doc;
            let params = extract_params(items, next_idx);
            let arities = extract_arities(items, next_idx);
            let fields = if head == "deftype" {
                extract_deftype_fields(items, next_idx)
            } else {
                None
            };
            let mut enum_members = Vec::new();
            let mut enum_member_fields = Vec::new();
            let (qualified_only, defenum_start) = if head == "defenum" {
                extract_defenum_options(items, next_idx)
            } else {
                (false, next_idx)
            };
            if head == "defenum" {
                let mut member_idx = defenum_start;
                while member_idx < items.len() {
                    let member_form = &items[member_idx];
                    let member = match &member_form.kind {
                        FormKind::Symbol(sym) if !sym.starts_with('*') => sym.clone(),
                        _ => {
                            member_idx += 1;
                            continue;
                        }
                    };
                    let payload_form = items
                        .get(member_idx + 1)
                        .filter(|form| matches!(form.kind, FormKind::Map(_)));
                    let range = Range {
                        start: Position {
                            line: member_form.span.line.saturating_sub(1) as u32,
                            character: member_form.span.col.saturating_sub(1) as u32,
                        },
                        end: Position {
                            line: member_form.span.line.saturating_sub(1) as u32,
                            character: member_form.span.col.saturating_sub(1) as u32 + 1,
                        },
                    };
                    let info = EnumMemberInfo {
                        name: member,
                        range,
                    };
                    enum_members.push(info.clone());
                    let fields = payload_form.and_then(|form| match &form.kind {
                        FormKind::Map(entries) => extract_deftype_map_fields(entries),
                        _ => None,
                    });
                    enum_member_fields.push((info, fields));
                    member_idx += if payload_form.is_some() { 2 } else { 1 };
                }
            }
            let canonical = canonical_symbol_name(&name).into_owned();
            let range = Range {
                start: Position {
                    line: name_form.unwrap().span.line.saturating_sub(1) as u32,
                    character: name_form.unwrap().span.col.saturating_sub(1) as u32,
                },
                end: Position {
                    line: name_form.unwrap().span.line.saturating_sub(1) as u32,
                    character: name_form.unwrap().span.col.saturating_sub(1) as u32 + 1,
                },
            };
            index.insert(SymbolInfo {
                name,
                canonical,
                kind,
                range,
                docstring,
                params,
                fields,
                enum_members: enum_members.clone(),
                namespace: namespace.map(|s| s.to_string()),
                namespace_aliases: namespace_aliases.to_vec(),
                arities,
                is_private,
            });
            if head == "deftype" {
                if let Some(binding_form) = extract_deftype_from_binding(items, next_idx) {
                    if let FormKind::Symbol(sym) = &binding_form.kind {
                        let canonical = canonical_symbol_name(sym).into_owned();
                        let range = Range {
                            start: Position {
                                line: binding_form.span.line.saturating_sub(1) as u32,
                                character: binding_form.span.col.saturating_sub(1) as u32,
                            },
                            end: Position {
                                line: binding_form.span.line.saturating_sub(1) as u32,
                                character: binding_form.span.col.saturating_sub(1) as u32 + 1,
                            },
                        };
                        index.insert(SymbolInfo {
                            name: sym.clone(),
                            canonical,
                            kind: SymbolKind::Def,
                            range,
                            docstring: None,
                            params: None,
                            fields: None,
                            enum_members: Vec::new(),
                            namespace: namespace.map(|s| s.to_string()),
                            namespace_aliases: namespace_aliases.to_vec(),
                            arities: Vec::new(),
                            is_private: false,
                        });
                    }
                }
            }
            if head == "defenum" {
                if !qualified_only {
                    for (member, fields) in enum_member_fields {
                        let canonical = canonical_symbol_name(&member.name).into_owned();
                        index.insert(SymbolInfo {
                            name: member.name.clone(),
                            canonical,
                            kind: SymbolKind::Deftype,
                            range: member.range,
                            docstring: None,
                            params: None,
                            fields,
                            enum_members: Vec::new(),
                            namespace: namespace.map(|s| s.to_string()),
                            namespace_aliases: namespace_aliases.to_vec(),
                            arities: Vec::new(),
                            is_private: false,
                        });
                    }
                }
            }
        }
    }
    index
}

fn collect_def_map_shapes(
    forms: &[Form],
    namespace: Option<&str>,
    namespace_aliases: &[String],
) -> HashMap<String, MapShape> {
    let mut shapes = HashMap::new();
    for form in forms {
        if let Some((name, shape)) = extract_def_map_shape(form) {
            let canonical = canonical_symbol_name(&name).into_owned();
            let mut keys = symbol_aliases(&canonical);
            if !keys.iter().any(|k| k == &name) {
                keys.push(name.clone());
            }
            if !keys.iter().any(|k| k == &canonical) {
                keys.push(canonical);
            }
            if let Some(ns) = namespace {
                keys.push(format!("{}::{}", ns, name));
                keys.push(format!("{}/{}", ns, name));
            }
            for alias in namespace_aliases {
                keys.push(format!("{}::{}", alias, name));
                keys.push(format!("{}/{}", alias, name));
            }
            keys.sort();
            keys.dedup();
            for key in keys {
                shapes.insert(key, shape.clone());
            }
        }
    }
    shapes
}

fn extract_def_map_shape(form: &Form) -> Option<(String, MapShape)> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    let head = match items.first().map(|f| &f.kind) {
        Some(FormKind::Symbol(sym)) => sym.as_str(),
        _ => return None,
    };
    if !matches!(head, "def" | "def-") {
        return None;
    }
    let name_form = items.get(1)?;
    let name = match &name_form.kind {
        FormKind::Symbol(sym) => sym.clone(),
        _ => return None,
    };
    let value_form = def_value_form(items, 2)?;
    let shape = map_shape_from_form(value_form)?;
    Some((name, shape))
}

fn def_value_form(items: &[Form], start: usize) -> Option<&Form> {
    let mut pos = start;
    while pos < items.len() {
        if pos + 1 >= items.len() {
            return items.get(pos);
        }
        match &items[pos].kind {
            FormKind::String(_) | FormKind::Map(_) => pos += 1,
            _ => return items.get(pos),
        }
    }
    None
}

fn map_shape_from_form(form: &Form) -> Option<MapShape> {
    match &form.kind {
        FormKind::Map(entries) => Some(map_shape_from_entries(entries)),
        _ => None,
    }
}

fn map_shape_from_entries(entries: &[clove_core::ast::MapItem]) -> MapShape {
    let mut fields = Vec::new();
    for entry in entries {
        let (key, value) = match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => (k, v),
            clove_core::ast::MapItem::Spread(_) => continue,
        };
        let Some(name) = map_shape_key_name(key) else {
            continue;
        };
        let nested = match &value.kind {
            FormKind::Map(inner) => Some(Box::new(map_shape_from_entries(inner))),
            _ => None,
        };
        fields.push(MapShapeField {
            name,
            nested,
            range: span_to_range(key.span),
        });
    }
    MapShape { fields }
}

fn map_shape_key_name(form: &Form) -> Option<String> {
    match &form.kind {
        FormKind::Keyword(name) => Some(name.clone()),
        FormKind::Symbol(name) => Some(name.clone()),
        _ => None,
    }
}

fn map_shape_field_for_path<'a>(shape: &'a MapShape, path: &[String]) -> Option<&'a MapShapeField> {
    let mut current = shape;
    for (idx, segment) in path.iter().enumerate() {
        let field = current.fields.iter().find(|field| field.name == *segment)?;
        if idx + 1 == path.len() {
            return Some(field);
        }
        current = field.nested.as_deref()?;
    }
    None
}

pub(crate) fn extract_docstring(
    items: &[Form],
    idx: usize,
    require_tail: bool,
) -> (Option<String>, usize) {
    let mut pos = idx;
    let mut doc = None;
    while pos < items.len() {
        if require_tail && pos + 1 >= items.len() {
            break;
        }
        match &items[pos].kind {
            FormKind::String(s) => {
                if doc.is_none() {
                    doc = Some(s.clone());
                }
                pos += 1;
            }
            FormKind::Map(_) => {
                pos += 1;
            }
            _ => break,
        }
    }
    (doc, pos)
}

fn extract_params(items: &[Form], idx: usize) -> Option<Vec<String>> {
    let form = items.get(idx)?;
    match &form.kind {
        FormKind::Vector(entries) => Some(params_from_entries(entries)),
        FormKind::List(_) => {
            let mut best: Option<Vec<String>> = None;
            let mut best_has_rest = false;
            let mut best_len = 0usize;
            for form in items.iter().skip(idx) {
                let FormKind::List(variants) = &form.kind else {
                    continue;
                };
                let Some(Form {
                    kind: FormKind::Vector(entries),
                    ..
                }) = variants.first()
                else {
                    continue;
                };
                let has_rest = entries
                    .iter()
                    .any(|entry| matches!(&entry.kind, FormKind::Symbol(sym) if sym == "&"));
                let len = entries.len();
                let take = match best.as_ref() {
                    None => true,
                    Some(_) if has_rest && !best_has_rest => true,
                    Some(_) if has_rest == best_has_rest && len > best_len => true,
                    _ => false,
                };
                if take {
                    best = Some(params_from_entries(entries));
                    best_has_rest = has_rest;
                    best_len = len;
                }
            }
            best
        }
        _ => None,
    }
}

fn params_from_entries(entries: &[Form]) -> Vec<String> {
    entries
        .iter()
        .map(|entry| form_to_string(entry, ""))
        .collect::<Vec<_>>()
}

fn extract_defenum_options(items: &[Form], idx: usize) -> (bool, usize) {
    let mut qualified_only = false;
    let mut pos = idx;
    while let Some(form) = items.get(pos) {
        let option = match &form.kind {
            FormKind::Keyword(name) => name.as_str(),
            _ => break,
        };
        if option == "qualified-only" {
            if let Some(Form {
                kind: FormKind::Bool(value),
                ..
            }) = items.get(pos + 1)
            {
                qualified_only = *value;
                pos += 2;
                continue;
            }
        }
        break;
    }
    (qualified_only, pos)
}

fn extract_deftype_from_binding(items: &[Form], idx: usize) -> Option<&Form> {
    let mut pos = idx;
    while let Some(form) = items.get(pos) {
        match &form.kind {
            FormKind::Keyword(name) if name == "alias" => {
                pos += 2;
                continue;
            }
            FormKind::Keyword(name) if name == "from" => {
                return items.get(pos + 1);
            }
            _ => break,
        }
    }
    let form = items.get(pos)?;
    let FormKind::List(list_items) = &form.kind else {
        return None;
    };
    if matches!(
        list_items.first().map(|form| &form.kind),
        Some(FormKind::Symbol(sym)) if sym == "def"
    ) {
        return list_items.get(1);
    }
    None
}

fn extract_deftype_fields(items: &[Form], idx: usize) -> Option<Vec<TypeField>> {
    let form = items.get(idx)?;
    match &form.kind {
        FormKind::Keyword(name) if name == "alias" => None,
        FormKind::Keyword(name) if name == "from" => None,
        FormKind::List(items)
            if matches!(
                items.first().map(|form| &form.kind),
                Some(FormKind::Symbol(sym)) if sym == "def"
            ) =>
        {
            None
        }
        FormKind::Map(entries) => extract_deftype_map_fields(entries),
        FormKind::Vector(entries) | FormKind::List(entries) => extract_deftype_list_fields(entries),
        _ => {
            let tail = items.get(idx..)?;
            if tail.len() >= 2 {
                extract_deftype_list_fields(tail)
            } else {
                None
            }
        }
    }
}

fn extract_deftype_map_fields(entries: &[clove_core::ast::MapItem]) -> Option<Vec<TypeField>> {
    let mut fields = Vec::new();
    for entry in entries {
        let (key, value) = match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => (k, v),
            clove_core::ast::MapItem::Spread(_) => continue,
        };
        let key_name = match &key.kind {
            FormKind::Keyword(kw) => {
                if kw.starts_with(':') {
                    kw.clone()
                } else {
                    format!(":{}", kw)
                }
            }
            _ => continue,
        };
        let schema = form_to_string(value, "");
        fields.push(TypeField {
            name: key_name,
            schema,
        });
    }
    if fields.is_empty() {
        None
    } else {
        Some(fields)
    }
}

fn extract_deftype_list_fields(entries: &[Form]) -> Option<Vec<TypeField>> {
    if entries.is_empty() {
        return None;
    }
    if let Some(fields) = extract_deftype_typed_fields(entries) {
        return Some(fields);
    }
    if entries.len() % 2 != 0 {
        return None;
    }
    let mut fields = Vec::new();
    let mut idx = 0;
    while idx + 1 < entries.len() {
        let key = &entries[idx];
        let value = &entries[idx + 1];
        let name = match &key.kind {
            FormKind::Keyword(kw) => format!(":{}", kw),
            FormKind::Symbol(sym) => format!(":{}", sym.trim_end_matches(':')),
            _ => return None,
        };
        let schema = form_to_string(value, "");
        fields.push(TypeField { name, schema });
        idx += 2;
    }
    if fields.is_empty() {
        None
    } else {
        Some(fields)
    }
}

fn extract_deftype_typed_fields(entries: &[Form]) -> Option<Vec<TypeField>> {
    let mut fields = Vec::new();
    for entry in entries {
        let hint = entry.type_hint.as_ref()?;
        let name = match &entry.kind {
            FormKind::Keyword(kw) => format!(":{}", kw),
            FormKind::Symbol(sym) => format!(":{}", sym.trim_end_matches(':')),
            _ => return None,
        };
        fields.push(TypeField {
            name,
            schema: hint.kind.describe(),
        });
    }
    if fields.is_empty() {
        None
    } else {
        Some(fields)
    }
}

fn extract_arities(items: &[Form], idx: usize) -> Vec<usize> {
    let mut arities = Vec::new();
    for form in items.iter().skip(idx) {
        match &form.kind {
            FormKind::Vector(entries) => arities.push(entries.len()),
            FormKind::List(variants) => {
                if let Some(Form {
                    kind: FormKind::Vector(entries),
                    ..
                }) = variants.first()
                {
                    arities.push(entries.len());
                }
            }
            _ => {}
        }
    }
    arities.sort();
    arities.dedup();
    arities
}

#[derive(Clone, Debug)]
pub(crate) enum SymbolKind {
    Def,
    Defn,
    Deftype,
    Defenum,
}

#[derive(Clone, Debug)]
pub(crate) struct TypeField {
    pub(crate) name: String,
    pub(crate) schema: String,
}

#[derive(Clone, Debug)]
pub(crate) struct EnumMemberInfo {
    pub(crate) name: String,
    pub(crate) range: Range,
}

#[derive(Clone, Debug)]
pub(crate) struct SymbolInfo {
    pub(crate) name: String,
    pub(crate) canonical: String,
    #[allow(dead_code)]
    pub(crate) kind: SymbolKind,
    pub(crate) range: Range,
    pub(crate) docstring: Option<String>,
    pub(crate) params: Option<Vec<String>>,
    pub(crate) fields: Option<Vec<TypeField>>,
    pub(crate) enum_members: Vec<EnumMemberInfo>,
    pub(crate) namespace: Option<String>,
    pub(crate) namespace_aliases: Vec<String>,
    pub(crate) arities: Vec<usize>,
    pub(crate) is_private: bool,
}

impl SymbolInfo {
    pub(crate) fn is_type(&self) -> bool {
        matches!(self.kind, SymbolKind::Deftype | SymbolKind::Defenum)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct MapShapeField {
    pub(crate) name: String,
    pub(crate) nested: Option<Box<MapShape>>,
    pub(crate) range: Range,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct MapShape {
    pub(crate) fields: Vec<MapShapeField>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct SymbolIndex {
    by_name: HashMap<String, Vec<SymbolInfo>>,
    entries: Vec<SymbolInfo>,
}

impl SymbolIndex {
    fn insert(&mut self, info: SymbolInfo) {
        self.entries.push(info.clone());
        let mut keys = symbol_aliases(&info.canonical);
        if !info.name.is_empty() && !keys.iter().any(|k| k == &info.name) {
            keys.push(info.name.clone());
        }
        if let Some(ns) = &info.namespace {
            keys.push(format!("{}::{}", ns, info.name));
            keys.push(format!("{}/{}", ns, info.name));
        }
        for alias in &info.namespace_aliases {
            keys.push(format!("{}::{}", alias, info.name));
            keys.push(format!("{}/{}", alias, info.name));
        }
        keys.sort();
        keys.dedup();
        for key in keys {
            self.by_name.entry(key).or_default().push(info.clone());
        }
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&[SymbolInfo]> {
        self.by_name.get(name).map(|items| items.as_slice())
    }

    pub(crate) fn entries(&self) -> &[SymbolInfo] {
        &self.entries
    }
}

pub(crate) fn lookup_keys_for(symbol: &str) -> Vec<String> {
    let mut keys = doc_lookup_keys(symbol);
    keys.sort();
    keys.dedup();
    keys
}

fn symbol_variants(name: &str, canonical: &str) -> Vec<String> {
    let mut variants = Vec::new();
    variants.push(name.to_lowercase());
    variants.push(canonical.to_lowercase());
    if let Some((_, tail)) = canonical.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    if let Some((_, tail)) = name.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    variants.retain(|v| !v.is_empty());
    variants.sort();
    variants.dedup();
    variants
}

fn symbol_info_matches_prefix(info: &SymbolInfo, normalized: &str) -> bool {
    if normalized.is_empty() {
        return true;
    }
    symbol_variants(&info.name, &info.canonical)
        .iter()
        .any(|variant| variant.starts_with(normalized))
}

fn build_user_completion_item(info: &SymbolInfo, replace_range: &Range) -> CompletionItem {
    let mut item = CompletionItem::default();
    item.label = info.name.clone();
    item.kind = Some(CompletionItemKind::FUNCTION);
    item.detail = symbol_signature_label(info);
    item.documentation = user_documentation(info);
    item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
        range: replace_range.clone(),
        new_text: info.name.clone(),
    }));
    item.data = Some(json!({ "symbol": info.canonical.clone(), "source": "user" }));
    item
}

pub(crate) fn symbol_signature_label(info: &SymbolInfo) -> Option<String> {
    info.params.as_ref().map(|params| {
        if params.is_empty() {
            format!("{} []", info.name)
        } else {
            format!("{} [{}]", info.name, params.join(" "))
        }
    })
}

pub(crate) fn user_documentation(info: &SymbolInfo) -> Option<Documentation> {
    format_user_markdown(info).map(|markdown| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        })
    })
}

pub(crate) fn format_user_markdown(info: &SymbolInfo) -> Option<String> {
    let mut sections = Vec::new();
    if let Some(sig) = symbol_signature_label(info) {
        sections.push(format!("```clojure\n{}\n```", sig));
    }
    if let Some(doc) = info
        .docstring
        .as_ref()
        .map(|d| d.trim())
        .filter(|s| !s.is_empty())
    {
        sections.push(doc.to_string());
    }
    if info.is_type() {
        if let Some(fields) = &info.fields {
            let mut lines = Vec::new();
            for field in fields {
                if field.schema.is_empty() {
                    lines.push(format!("- `{}`", field.name));
                } else {
                    lines.push(format!("- `{}` {}", field.name, field.schema));
                }
            }
            if !lines.is_empty() {
                sections.push(format!("**Fields**\n{}", lines.join("\n")));
            }
        }
    }
    let aliases = symbol_aliases(&info.canonical)
        .into_iter()
        .filter(|alias| alias != &info.canonical)
        .collect::<Vec<_>>();
    if !aliases.is_empty() {
        sections.push(format!("_Aliases: {}_", aliases.join(", ")));
    }
    if sections.is_empty() {
        None
    } else {
        Some(sections.join("\n\n"))
    }
}

pub(crate) fn normalize_namespace(ns: &str) -> String {
    let mut norm = ns.to_string();
    while norm.contains("::::") {
        norm = norm.replace("::::", "::");
    }
    norm
}

pub(crate) fn namespace_lookup_keys(ns: &str) -> Vec<String> {
    vec![normalize_namespace(ns)]
}

pub(crate) fn namespace_known(store: &DocumentStore, ns: &str) -> bool {
    for key in namespace_lookup_keys(ns) {
        if store.ns_to_uris.contains_key(&key) {
            return true;
        }
    }
    false
}

pub(crate) fn require_target_namespace(spec: &RequireSpec) -> Option<String> {
    match &spec.target {
        RequireTarget::Namespace(ns) => Some(normalize_namespace(ns)),
        RequireTarget::FilePath(_) => None,
    }
}

pub(crate) fn require_target_namespace_for(
    spec: &RequireSpec,
    doc: &DocumentData,
) -> Option<String> {
    match &spec.target {
        RequireTarget::Namespace(ns) => Some(normalize_namespace(ns)),
        RequireTarget::FilePath(path) => resolve_require_file_namespace(path, doc),
    }
}

fn resolve_require_file_namespace(path: &str, doc: &DocumentData) -> Option<String> {
    let path = path.trim();
    if path.is_empty() || is_remote_require_path(path) {
        return None;
    }
    let resolved = resolve_require_file_path(path, doc)?;
    namespace_from_path(doc, &resolved)
}

fn is_remote_require_path(path: &str) -> bool {
    path.starts_with("http://") || path.starts_with("https://")
}

fn resolve_require_file_path(path: &str, doc: &DocumentData) -> Option<PathBuf> {
    let raw = Path::new(path);
    let mut bases = Vec::new();
    if raw.is_absolute() {
        bases.push(raw.to_path_buf());
    } else {
        if let Some(base) = doc.source_path.as_deref().and_then(|p| p.parent()) {
            bases.push(base.join(raw));
        }
        if let Some(root) = doc.workspace_root.as_deref() {
            bases.push(root.join(raw));
        }
    }
    let mut seen = HashSet::new();
    for base in bases {
        for candidate in expand_require_file_candidates(&base) {
            if !seen.insert(candidate.clone()) {
                continue;
            }
            if candidate.exists() {
                return Some(normalize_path(&candidate));
            }
        }
    }
    None
}

fn expand_require_file_candidates(base: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if base.extension().is_some() {
        out.push(base.to_path_buf());
        return out;
    }
    out.push(base.to_path_buf());
    let mut clv = base.to_path_buf();
    clv.set_extension("clv");
    out.push(clv);
    let mut clove = base.to_path_buf();
    clove.set_extension("clove");
    out.push(clove);
    out
}

fn namespace_from_path(doc: &DocumentData, path: &Path) -> Option<String> {
    if let Some(root) = doc.workspace_root.as_deref() {
        if let Some(ns) = infer_namespace_from_path(root, path) {
            return Some(ns);
        }
    }
    if let Some(base) = doc.source_path.as_deref().and_then(|p| p.parent()) {
        if let Ok(rel) = path.strip_prefix(base) {
            return namespace_from_relative_path(rel);
        }
    }
    namespace_from_relative_path(path)
}

fn namespace_from_relative_path(path: &Path) -> Option<String> {
    let stem = path.with_extension("");
    let mut parts = Vec::new();
    for component in stem.components() {
        let name = match component {
            Component::Normal(name) => name.to_str()?,
            Component::CurDir
            | Component::ParentDir
            | Component::RootDir
            | Component::Prefix(_) => {
                continue;
            }
        };
        if name.is_empty() {
            continue;
        }
        parts.push(name.replace('/', "::"));
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("::"))
    }
}

pub(crate) fn span_to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: span.line.saturating_sub(1) as u32,
            character: span.col.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.line.saturating_sub(1) as u32,
            character: span.col.saturating_sub(1) as u32 + 1,
        },
    }
}

pub(crate) fn make_diagnostic(
    range: Range,
    severity: DiagnosticSeverity,
    message: impl Into<String>,
) -> Diagnostic {
    Diagnostic {
        range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("clove-lsp".to_string()),
        message: message.into(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn to_diagnostic(err: &CloveError) -> Diagnostic {
    let span = err.span().unwrap_or(clove_core::ast::Span {
        line: 1,
        col: 1,
        index: 0,
    });
    Diagnostic {
        range: Range {
            start: Position {
                line: span.line.saturating_sub(1) as u32,
                character: span.col.saturating_sub(1) as u32,
            },
            end: Position {
                line: span.line.saturating_sub(1) as u32,
                character: span.col.saturating_sub(1) as u32 + 1,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("clove-lsp".to_string()),
        message: err.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn infer_type_map_with_diags(forms: &[Form]) -> (HashMap<usize, String>, Vec<Diagnostic>) {
    let mut out = HashMap::new();
    let mut diags = Vec::new();
    let result = clove_core::typing::infer::infer_forms_with_diags(forms);
    for (offset, ty) in result.type_map {
        out.insert(offset, ty.describe());
    }
    for diag in result.diags {
        diags.push(make_diagnostic(
            span_to_range(diag.span),
            DiagnosticSeverity::ERROR,
            diag.message,
        ));
    }
    (out, diags)
}

fn infer_hint_map(forms: &[Form]) -> HashMap<usize, TypeKind> {
    let mut out = HashMap::new();
    for form in forms {
        collect_hint_map(form, &mut out);
    }
    out
}

fn collect_hint_map(form: &Form, out: &mut HashMap<usize, TypeKind>) {
    if let Some(hint) = &form.type_hint {
        out.insert(form.span.index, hint.kind.clone());
    }
    match &form.kind {
        FormKind::List(items)
        | FormKind::Vector(items)
        | FormKind::Set(items)
        | FormKind::ShortFn(items) => {
            for item in items {
                collect_hint_map(item, out);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_hint_map(k, out);
                        collect_hint_map(v, out);
                    }
                    clove_core::ast::MapItem::Spread(expr) => {
                        collect_hint_map(expr, out);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_hint_map(expr, out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_hint_map(expr, out);
                }
            }
        }
        _ => {}
    }
}
