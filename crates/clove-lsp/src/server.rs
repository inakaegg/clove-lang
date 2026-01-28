//! LSP server based on tower-lsp.
//! Single-file implementation providing parse error notifications / hover / completion / definition.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use clove_core::{
    ast::{Form, FormKind, Span},
    fn_meta,
    package_registry::{clove_home, load_registry},
    plugin_meta,
    reader::{
        DOT_CHAIN_PLACEHOLDER_PREFIX, INDEX_GET_IN_SYM, INDEX_GET_SYM, INFIX_SYNTAX_SYM,
        MAP_REF_SYM, OOP_AS_SYM, OOP_BARE_SYM, OOP_DOT_STAGE_SYM, OOP_INDEX_SYM, OOP_LET_SYM,
        OOP_NIL_SAFE_SYM, OOP_SEG_SYM, OOP_SYNTAX_SYM,
    },
    runtime::{parse_source_for_lsp_lenient, RequireSpec, RequireTarget},
    symbols::{canonical_symbol_name, symbol_aliases},
    type_registry::{self, TypeEntry},
    types::TypeKind,
};
use serde::Deserialize;
use serde_json::json;
use tokio::sync::RwLock;
use tokio::time::sleep;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionContext, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionItem, CompletionItemKind,
    CompletionOptions, CompletionResponse, CompletionTextEdit, Diagnostic, DiagnosticSeverity,
    DidChangeConfigurationParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    InitializeResult, InlayHint, InlayHintKind, InlayHintParams, Location, MarkedString,
    MarkupContent, MarkupKind, MessageType, OneOf, ParameterInformation, ParameterLabel,
    PartialResultParams, Position, PrepareRenameResponse, Range, ReferenceParams, RenameOptions,
    RenameParams, ServerCapabilities, ServerInfo, SignatureHelp, SignatureHelpOptions,
    SignatureHelpParams, SignatureInformation, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentPositionParams, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Url, WorkDoneProgressParams, WorkspaceEdit,
};
use tower_lsp::{Client, LanguageServer};

use crate::docs::{all_docs, find_doc, signature_params_for, DocEntry};
use crate::document_store::{
    extract_docstring, format_user_markdown, make_diagnostic, namespace_known,
    namespace_lookup_keys, normalize_namespace, require_target_namespace_for, span_to_range,
    symbol_signature_label, user_documentation, DocumentData, DocumentStore, EnumMemberInfo,
    MapShape, RefactorPrefs, SymbolInfo, SymbolKind, TypeField,
};

const DID_CHANGE_DEBOUNCE: Duration = Duration::from_millis(200);

pub struct Backend {
    client: Option<Client>,
    store: Arc<RwLock<DocumentStore>>,
    debounce_tokens: Arc<RwLock<HashMap<Url, u64>>>,
}

#[derive(Clone, Debug)]
struct DefinitionAnchor {
    uri: Url,
    start: Position,
    namespace: Option<String>,
}

enum TokenAtPosition {
    Symbol { name: String, range: Range },
    Keyword { name: String, range: Range },
}

#[derive(Clone, Debug, Default, Deserialize)]
struct CloveSettings {
    refactor: Option<RefactorSettings>,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct RefactorSettings {
    access: Option<RefactorAccessSettings>,
}

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RefactorAccessSettings {
    prefer_keyword_chain: Option<bool>,
}

impl Backend {
    pub fn new() -> Self {
        Self {
            client: None,
            store: Arc::new(RwLock::new(DocumentStore::default())),
            debounce_tokens: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn with_client(&self, client: Client) -> Self {
        Self {
            client: Some(client),
            store: self.store.clone(),
            debounce_tokens: self.debounce_tokens.clone(),
        }
    }

    fn client(&self) -> &Client {
        self.client
            .as_ref()
            .expect("LanguageServer used without client")
    }

    async fn log_info(&self, msg: impl Into<String>) {
        let _ = self
            .client()
            .log_message(MessageType::INFO, msg.into())
            .await;
    }

    async fn recompute_and_publish_diagnostics(&self, uri: &Url) {
        recompute_and_publish_diagnostics_for(
            self.client().clone(),
            self.store.clone(),
            uri.clone(),
        )
        .await;
    }

    async fn schedule_debounced_change(&self, uri: Url, text: String) {
        let token = {
            let mut tokens = self.debounce_tokens.write().await;
            let entry = tokens.entry(uri.clone()).or_insert(0);
            *entry = entry.wrapping_add(1);
            *entry
        };
        let store = self.store.clone();
        let client = self.client().clone();
        let tokens = self.debounce_tokens.clone();
        tokio::spawn(async move {
            sleep(DID_CHANGE_DEBOUNCE).await;
            let current = {
                let guard = tokens.read().await;
                guard.get(&uri).cloned().unwrap_or(0)
            };
            if current != token {
                return;
            }
            {
                let mut store = store.write().await;
                store.open_or_update(uri.clone(), text);
            }
            recompute_and_publish_diagnostics_for(client, store, uri).await;
        });
    }
}

async fn recompute_and_publish_diagnostics_for(
    client: Client,
    store: Arc<RwLock<DocumentStore>>,
    uri: Url,
) {
    let diagnostics = {
        let mut store = store.write().await;
        store.ensure_workspace_symbols(&uri);
        let Some(doc) = store.get(&uri) else {
            return;
        };
        let requires = doc.requires.clone();
        store.preload_requires(&requires);
        let Some(doc) = store.get(&uri) else {
            return;
        };
        compute_diagnostics(&store, doc, DiagnosticsOptions::default())
    };
    let _ = client.publish_diagnostics(uri, diagnostics, None).await;
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.log_info("clove-lsp: initialize called").await;
        let roots = workspace_roots_from_initialize(&params);
        let doc_dirs = crate::docs::package_doc_dirs_from_roots(&roots);
        crate::docs::set_extra_doc_dirs(doc_dirs);
        {
            let mut store = self.store.write().await;
            store.set_workspace_roots(roots);
            if !store.workspace_roots.is_empty() {
                store.scan_all_workspace_roots();
            }
        }
        let plugin_dirs = plugin_dirs_from_initialize(&params);
        if !plugin_dirs.is_empty() {
            if let Err(err) = plugin_meta::load_meta_from_dirs(&plugin_dirs) {
                self.log_info(format!("clove-lsp: meta load failed: {}", err))
                    .await;
            }
        }
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            inlay_hint_provider: Some(OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: Some(vec!["(".into(), " ".into(), "/".into()]),
                ..CompletionOptions::default()
            }),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".into(), " ".into()]),
                retrigger_characters: None,
                work_done_progress_options: Default::default(),
            }),
            references_provider: Some(OneOf::Left(true)),
            definition_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            ..ServerCapabilities::default()
        };
        Ok(InitializeResult {
            capabilities,
            server_info: Some(ServerInfo {
                name: "clove-lsp".into(),
                version: None,
            }),
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.log_info("clove-lsp: initialized notification").await;
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let mut prefs = {
            let store = self.store.read().await;
            store.refactor_prefs()
        };
        let mut settings_value = params.settings;
        let root_settings = settings_value.clone();
        if let Some(clove_settings) = settings_value.get("clove") {
            settings_value = clove_settings.clone();
        }
        if let Ok(settings) = serde_json::from_value::<CloveSettings>(settings_value) {
            if let Some(access) = settings.refactor.and_then(|r| r.access) {
                if let Some(prefer_kw_chain) = access.prefer_keyword_chain {
                    prefs.prefer_kw_chain = prefer_kw_chain;
                }
            }
        }
        if let Some(prefer_kw_chain) = root_settings
            .get("refactor.access.preferKeywordChain")
            .and_then(|value| value.as_bool())
        {
            prefs.prefer_kw_chain = prefer_kw_chain;
        }
        let mut store = self.store.write().await;
        store.set_refactor_prefs(prefs);
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        self.log_info(format!(
            "clove-lsp: didOpen uri={} lang={}",
            params.text_document.uri, params.text_document.language_id
        ))
        .await;
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let language_id = params.text_document.language_id;
        {
            let mut store = self.store.write().await;
            store.open_or_update_with_language(uri.clone(), text, Some(language_id.as_str()));
            store.ensure_workspace_symbols(&uri);
        }
        self.recompute_and_publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(TextDocumentContentChangeEvent { text, .. }) = params.content_changes.last() {
            self.log_info(format!(
                "clove-lsp: didChange uri={} ({} bytes)",
                uri,
                text.len()
            ))
            .await;
            {
                let mut store = self.store.write().await;
                store.update_open_text(&uri, text.clone());
            }
            self.schedule_debounced_change(uri.clone(), text.clone())
                .await;
        } else {
            self.log_info(format!("clove-lsp: didChange uri={} (no changes)", uri))
                .await;
            self.recompute_and_publish_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            let mut store = self.store.write().await;
            store.open_or_update(uri.clone(), text);
        }
        self.recompute_and_publish_diagnostics(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let _ = self
            .client()
            .publish_diagnostics(uri.clone(), Vec::new(), None)
            .await;
        {
            let mut tokens = self.debounce_tokens.write().await;
            tokens.remove(&uri);
        }
        let mut store = self.store.write().await;
        store.downgrade_document(&uri);
    }

    async fn hover(&self, params: tower_lsp::lsp_types::HoverParams) -> Result<Option<Hover>> {
        self.log_info("clove-lsp: hover request").await;
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        {
            let mut store = self.store.write().await;
            store.ensure_workspace_symbols(&uri);
        }
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        if let Some(token) = type_token_at(&doc.text, position) {
            if let Some(markdown) = type_hover_markdown(&token.name, doc, &store) {
                let contents = HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                });
                return Ok(Some(Hover {
                    contents,
                    range: Some(token.range),
                }));
            }
        }

        if let Some(line) = line_at(&doc.text, position.line as usize) {
            let (start, end) = find_word_range(&line, position.character as usize);
            if start == end {
                return Ok(None);
            }
            let word = match slice_by_char_indices(&line, start, end) {
                Some(w) => w,
                None => return Ok(None),
            };
            let type_info = type_info_at(doc, position.line, start);
            let range = Range {
                start: Position {
                    line: position.line,
                    character: start as u32,
                },
                end: Position {
                    line: position.line,
                    character: end as u32,
                },
            };
            if word_has_type_punctuation(word) {
                let col = position.character as usize;
                if let Some(token) = type_token_in_word(&line, position.line, col, start, end) {
                    if let Some(markdown) = type_hover_markdown(&token.name, doc, &store) {
                        let contents = HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: markdown,
                        });
                        return Ok(Some(Hover {
                            contents,
                            range: Some(token.range),
                        }));
                    }
                }
            }
            if let Some(markdown) = enum_variant_hover_markdown(word, doc, &store) {
                let contents = HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                });
                return Ok(Some(Hover {
                    contents,
                    range: Some(range),
                }));
            }
            if let Some(entry) = find_doc(word) {
                if let Some(markdown) = format_doc_markdown(entry) {
                    let markdown = merge_hover_sections(markdown, type_info.as_deref());
                    let contents = HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: markdown,
                    });
                    return Ok(Some(Hover {
                        contents,
                        range: Some(range),
                    }));
                }
            } else if let Some(info) = lookup_symbol_info(word, doc, &store) {
                if let Some(markdown) = format_user_markdown(&info) {
                    let markdown = merge_hover_sections(markdown, type_info.as_deref());
                    let contents = HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: markdown,
                    });
                    return Ok(Some(Hover {
                        contents,
                        range: Some(range),
                    }));
                }
            } else if let Some(meta) = fn_meta::get(word) {
                if let Some(markdown) = format_fnmeta_markdown(&meta) {
                    let markdown = merge_hover_sections(markdown, type_info.as_deref());
                    let contents = HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: markdown,
                    });
                    return Ok(Some(Hover {
                        contents,
                        range: Some(range),
                    }));
                }
            }
            if let Some(type_markdown) = type_info {
                let contents = HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: type_markdown,
                });
                return Ok(Some(Hover {
                    contents,
                    range: Some(range),
                }));
            }
            let fallback =
                HoverContents::Scalar(MarkedString::String(format!("symbol: `{}`", word)));
            return Ok(Some(Hover {
                contents: fallback,
                range: Some(range),
            }));
        }
        Ok(None)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let mut hints = inlay_hints_for_doc(doc);
        let range = params.range;
        hints.retain(|hint| position_in_range(hint.position, &range));
        Ok(Some(hints))
    }

    async fn completion(
        &self,
        params: tower_lsp::lsp_types::CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        self.log_info(format!(
            "clove-lsp: completion request at line={} char={}",
            position.line, position.character
        ))
        .await;

        {
            let mut store = self.store.write().await;
            store.ensure_workspace_symbols(&uri);
        }
        let store = self.store.read().await;
        let context = store
            .get(&uri)
            .and_then(|doc| completion_context(&doc.text, position))
            .unwrap_or_else(|| CompletionContext::empty(position));

        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        if let Some(indexer_ctx) = indexer_completion_context(&doc.text, position) {
            let indexer_items = indexer_completion_items(&indexer_ctx, &store);
            if !indexer_items.is_empty() {
                return Ok(Some(CompletionResponse::Array(indexer_items)));
            }
        }
        let mut items =
            completion_items_for(&context.prefix, &context.range, &store, doc, context.mode);
        if matches!(context.mode, CompletionMode::Normal) {
            if let Some(fields) = type_field_candidates_at(doc, position, &store) {
                let field_items =
                    type_field_completion_items(&fields, &context.prefix, &context.range);
                if !field_items.is_empty() {
                    let mut combined = field_items;
                    combined.extend(items);
                    items = combined;
                }
            }
        }
        if items.is_empty() {
            return Ok(None);
        }
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn completion_resolve(&self, mut item: CompletionItem) -> Result<CompletionItem> {
        if let Some(symbol) = completion_item_symbol(&item) {
            if let Some(entry) = find_doc(&symbol) {
                if item.detail.is_none() {
                    item.detail = completion_detail(entry);
                }
                item.documentation = documentation_for_entry(entry);
            } else {
                let store = self.store.read().await;
                if let Some(info) = store.lookup_user_symbol_info(&symbol) {
                    if item.detail.is_none() {
                        item.detail = symbol_signature_label(&info);
                    }
                    if item.documentation.is_none() {
                        item.documentation = user_documentation(&info);
                    }
                }
            }
        }
        Ok(item)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let mut store = self.store.write().await;
        store.ensure_workspace_symbols(&uri);
        let doc = match store.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };
        if let Some(indexer_ctx) = indexer_segment_context(&doc.text, position) {
            let mut path = indexer_ctx.path.clone();
            path.push(indexer_ctx.segment);
            if let Some(loc) = store.find_map_shape_location(&indexer_ctx.base, &path) {
                return Ok(Some(GotoDefinitionResponse::Array(vec![loc])));
            }
        }
        if let Some(token) = type_token_at(&doc.text, position) {
            let lookup = normalize_type_name_for_lookup(&token.name);
            let mut locations = Vec::new();
            let candidates = resolve_symbol_candidates(&lookup, doc);
            for (ns, name) in candidates {
                let mut found = store.find_type_definitions(&name, ns.as_deref());
                if found.is_empty() {
                    let canon = canonical_symbol_name(&name).into_owned();
                    if canon != name {
                        found = store.find_type_definitions(&canon, ns.as_deref());
                    }
                }
                if !found.is_empty() {
                    locations = found;
                    break;
                }
            }
            if locations.is_empty() {
                locations = store.find_type_definitions(&lookup, None);
                if locations.is_empty() {
                    let canon = canonical_symbol_name(&lookup).into_owned();
                    if canon != lookup {
                        locations = store.find_type_definitions(&canon, None);
                    }
                }
            }
            if !locations.is_empty() {
                return Ok(Some(GotoDefinitionResponse::Array(locations)));
            }
        }
        let (mut result, word_for_ns) = {
            let mut result: Option<GotoDefinitionResponse> = None;
            let mut word_for_ns: Option<String> = None;
            let line = match line_at(&doc.text, position.line as usize) {
                Some(l) => l,
                None => return Ok(None),
            };
            let (start, end) = find_word_range(&line, position.character as usize);
            if start == end {
                return Ok(None);
            }
            let word = match slice_by_char_indices(&line, start, end) {
                Some(w) => w.to_string(),
                None => return Ok(None),
            };
            let mut word = word;
            if let Some(base) = oop_chain_base_symbol(&word) {
                word = base;
            }
            if word_has_type_punctuation(&word) {
                let col = position.character as usize;
                if let Some(token) = type_token_in_word(&line, position.line, col, start, end) {
                    let lookup = normalize_type_name_for_lookup(&token.name);
                    let mut locations = Vec::new();
                    let candidates = resolve_symbol_candidates(&lookup, doc);
                    for (ns, name) in candidates {
                        let mut found = store.find_type_definitions(&name, ns.as_deref());
                        if found.is_empty() {
                            let canon = canonical_symbol_name(&name).into_owned();
                            if canon != name {
                                found = store.find_type_definitions(&canon, ns.as_deref());
                            }
                        }
                        if !found.is_empty() {
                            locations = found;
                            break;
                        }
                    }
                    if locations.is_empty() {
                        locations = store.find_type_definitions(&lookup, None);
                        if locations.is_empty() {
                            let canon = canonical_symbol_name(&lookup).into_owned();
                            if canon != lookup {
                                locations = store.find_type_definitions(&canon, None);
                            }
                        }
                    }
                    if !locations.is_empty() {
                        result = Some(GotoDefinitionResponse::Array(locations));
                    }
                }
            }
            if result.is_none() {
                let canonical = canonical_symbol_name(&word).into_owned();
                let lookup_word = if canonical.is_empty() {
                    word.clone()
                } else {
                    canonical
                };
                word_for_ns = Some(lookup_word.clone());
                if lookup_word.is_empty() {
                    return Ok(None);
                }
                if let Some(loc) = enum_variant_definition_location(&lookup_word, doc, &store) {
                    result = Some(GotoDefinitionResponse::Array(vec![loc]));
                } else if let Some(ns_loc) = store.namespace_location(&lookup_word) {
                    result = Some(GotoDefinitionResponse::Array(vec![ns_loc]));
                } else if let Some(local) = find_local_definition(&uri, doc, position, &lookup_word)
                {
                    result = Some(GotoDefinitionResponse::Array(vec![local]));
                } else {
                    let candidates = resolve_symbol_candidates(&lookup_word, doc);
                    for (ns, lookup) in candidates {
                        let mut locations = store.find_definitions(&lookup, ns.as_deref());
                        if locations.is_empty() {
                            let canon_lookup = canonical_symbol_name(&lookup).into_owned();
                            if canon_lookup != lookup {
                                locations = store.find_definitions(&canon_lookup, ns.as_deref());
                            }
                        }
                        if !locations.is_empty() {
                            result = Some(GotoDefinitionResponse::Array(locations));
                            break;
                        }
                    }
                }
            }
            (result, word_for_ns)
        };
        if result.is_none() {
            if let Some(ns) = word_for_ns {
                if store.ensure_namespace_loaded(&ns) {
                    if let Some(ns_loc) = store.namespace_location(&ns) {
                        result = Some(GotoDefinitionResponse::Array(vec![ns_loc]));
                    }
                }
            }
        }
        Ok(result)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };
        let line = match line_at(&doc.text, position.line as usize) {
            Some(l) => l,
            None => return Ok(None),
        };
        let (start, end) = find_word_range(&line, position.character as usize);
        if start == end {
            return Ok(None);
        }
        let word = match slice_by_char_indices(&line, start, end) {
            Some(w) => w,
            None => return Ok(None),
        };
        let candidates = resolve_symbol_candidates(word, doc);
        let mut allowed_ns: Vec<Option<String>> = Vec::new();
        for (ns, _) in &candidates {
            if !allowed_ns.iter().any(|n| n.as_ref() == ns.as_ref()) {
                allowed_ns.push(ns.clone());
            }
        }
        let mut locations = Vec::new();
        for (doc_uri, data) in &store.docs {
            if data.is_virtual {
                continue;
            }
            let mut ns_ok = allowed_ns
                .iter()
                .any(|a| a.is_none() && data.namespace.is_none());
            if let Some(ns) = &data.namespace {
                if allowed_ns.iter().any(|a| a.as_deref() == Some(ns)) {
                    ns_ok = true;
                }
            }
            if !ns_ok {
                for alias in &data.namespace_aliases {
                    if allowed_ns.iter().any(|a| a.as_deref() == Some(alias)) {
                        ns_ok = true;
                        break;
                    }
                }
            }
            if !ns_ok {
                continue;
            }
            if data.text.is_empty() {
                let path = match doc_uri.to_file_path() {
                    Ok(p) => p,
                    Err(_) => continue,
                };
                let text = match fs::read_to_string(&path) {
                    Ok(t) => t,
                    Err(_) => continue,
                };
                for range in find_word_occurrences(&text, word) {
                    locations.push(Location {
                        uri: doc_uri.clone(),
                        range,
                    });
                }
                continue;
            }
            for range in find_word_occurrences(&data.text, word) {
                locations.push(Location {
                    uri: doc_uri.clone(),
                    range,
                });
            }
        }
        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let (name, range) = match token_at_position(doc, position) {
            Some(TokenAtPosition::Symbol { name, range }) => (name, range),
            Some(TokenAtPosition::Keyword { name, range }) => (name, range),
            None => return Ok(None),
        };
        if name.trim().is_empty() {
            return Ok(None);
        }
        Ok(Some(PrepareRenameResponse::Range(range)))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name.trim().to_string();
        if new_name.is_empty() || new_name.chars().any(|ch| ch.is_whitespace()) {
            return Ok(None);
        }
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let target = match token_at_position(doc, position) {
            Some(value) => value,
            None => return Ok(None),
        };
        match target {
            TokenAtPosition::Symbol { name: symbol, .. } => {
                let symbol_canon = canonical_symbol_name(&symbol).into_owned();
                let mut local_binding = None;
                if let Some(forms) = &doc.ast {
                    local_binding =
                        find_local_definition_in_forms(&doc.text, forms, position, &symbol);
                }
                let global_from_local = local_binding
                    .as_ref()
                    .and_then(|range| symbol_info_by_start(doc, range.start));
                if let Some(global_info) = global_from_local {
                    let anchor = DefinitionAnchor {
                        uri: uri.clone(),
                        start: global_info.range.start,
                        namespace: global_info.namespace.clone(),
                    };
                    return Ok(rename_global_symbol(
                        &store,
                        &symbol_canon,
                        &new_name,
                        Some(anchor),
                        doc,
                        &symbol,
                    ));
                }
                if let Some(binding_range) = local_binding {
                    return Ok(rename_local_symbol(
                        &uri,
                        doc,
                        &symbol_canon,
                        &new_name,
                        binding_range.start,
                    ));
                }
                let global_def = resolve_definition_location_for_rename(&store, doc, &symbol);
                if let Some(def_loc) = global_def {
                    let anchor = store.get(&def_loc.uri).map(|def_doc| DefinitionAnchor {
                        uri: def_loc.uri.clone(),
                        start: def_loc.range.start,
                        namespace: def_doc.namespace.clone(),
                    });
                    return Ok(rename_global_symbol(
                        &store,
                        &symbol_canon,
                        &new_name,
                        anchor,
                        doc,
                        &symbol,
                    ));
                }
            }
            TokenAtPosition::Keyword { name, .. } => {
                let new_keyword = match normalize_keyword_rename(&new_name) {
                    Some(value) => value,
                    None => return Ok(None),
                };
                return Ok(rename_keyword_in_document(&uri, doc, &name, &new_keyword));
            }
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let store = self.store.read().await;
        if let Some(symbols) = store.document_symbols(&uri) {
            if symbols.is_empty() {
                return Ok(None);
            }
            return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
        }
        Ok(None)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        {
            let mut store = self.store.write().await;
            store.ensure_workspace_symbols(&uri);
        }
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let context = match signature_context(&doc.text, position) {
            Some(ctx) => ctx,
            None => return Ok(None),
        };
        if let Some(entry) = find_doc(&context.head) {
            return Ok(Some(signature_help_from_doc(entry, context.argument_index)));
        }
        if let Some(info) = store.lookup_user_symbol_info(&context.head) {
            return Ok(Some(signature_help_from_symbol(
                &info,
                context.argument_index,
            )));
        }
        if let Some(meta) = fn_meta::get(&context.head) {
            return Ok(Some(signature_help_from_fnmeta(
                &meta,
                context.argument_index,
            )));
        }
        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.clone();
        {
            let mut store = self.store.write().await;
            store.ensure_workspace_symbols(&uri);
        }
        let store = self.store.read().await;
        let doc = match store.get(&uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };
        let prefs = store.refactor_prefs();
        let mut actions = code_actions_for_unbound(&params, doc, &store);
        actions.extend(code_actions_for_rewrite_access(&params, doc, prefs));
        actions.extend(code_actions_for_toggle_access(&params, doc, prefs));
        actions.extend(code_actions_for_rewrite_canonical_access(&params, doc));
        actions.extend(store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc)));
        if actions.is_empty() {
            return Ok(None);
        }
        let response: Vec<CodeActionOrCommand> = actions
            .into_iter()
            .map(CodeActionOrCommand::CodeAction)
            .collect();
        Ok(Some(response))
    }
}

#[derive(Clone, Debug)]
struct CompletionContext {
    range: Range,
    prefix: String,
    mode: CompletionMode,
}

#[derive(Clone, Debug)]
struct IndexerCompletionContext {
    range: Range,
    prefix: String,
    base: String,
    path: Vec<String>,
}

#[derive(Clone, Debug)]
struct IndexerSegmentContext {
    base: String,
    path: Vec<String>,
    segment: String,
}

impl CompletionContext {
    fn empty(position: Position) -> Self {
        Self {
            range: Range {
                start: position,
                end: position,
            },
            prefix: String::new(),
            mode: CompletionMode::Normal,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CompletionMode {
    Normal,
    Type,
}

fn completion_context(text: &str, position: Position) -> Option<CompletionContext> {
    if let Some(ctx) = type_completion_context(text, position) {
        return Some(ctx);
    }
    let line = line_at(text, position.line as usize)?;
    let (start, _) = find_word_range(&line, position.character as usize);
    let chars: Vec<char> = line.chars().collect();
    let idx = position.character as usize;
    let clamped = idx.min(chars.len());
    let mut prefix: String = chars[start..clamped].iter().collect();
    let mut range_start = start as u32;
    if let Some((offset, tail)) = split_oop_completion_prefix(&prefix) {
        range_start += offset as u32;
        prefix = tail.to_string();
    }
    let end_character = if idx > chars.len() {
        chars.len() as u32
    } else {
        position.character
    };
    Some(CompletionContext {
        range: Range {
            start: Position {
                line: position.line,
                character: range_start,
            },
            end: Position {
                line: position.line,
                character: end_character,
            },
        },
        prefix,
        mode: CompletionMode::Normal,
    })
}

fn split_oop_completion_prefix(prefix: &str) -> Option<(usize, &str)> {
    let mut last_dot = None;
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in prefix.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }
        if ch == '"' {
            in_string = true;
            continue;
        }
        if ch == '.' {
            last_dot = Some(idx);
        }
    }
    let dot = last_dot?;
    let next = dot + '.'.len_utf8();
    let tail = &prefix[next..];
    let offset = prefix[..next].chars().count();
    Some((offset, tail))
}

fn indexer_completion_context(text: &str, position: Position) -> Option<IndexerCompletionContext> {
    let line = line_at(text, position.line as usize)?;
    let col = position.character as usize;
    let (word_start, word_end) = find_word_range(&line, col);
    if word_start == word_end {
        return None;
    }
    let word = slice_by_char_indices(&line, word_start, word_end)?;
    if word.starts_with(':') || word.contains("::") {
        return None;
    }
    let chars: Vec<char> = word.chars().collect();
    let cursor = col.saturating_sub(word_start).min(chars.len());
    let first_colon = chars.iter().position(|ch| *ch == ':')?;
    if cursor <= first_colon {
        return None;
    }
    let mut last_colon = None;
    for idx in 0..cursor {
        if chars[idx] == ':' {
            last_colon = Some(idx);
        }
    }
    let last_colon = last_colon?;
    if last_colon == 0 {
        return None;
    }
    let base: String = chars[..first_colon].iter().collect();
    if base.is_empty() {
        return None;
    }
    let mut path = Vec::new();
    if last_colon > first_colon + 1 {
        let path_str: String = chars[first_colon + 1..last_colon].iter().collect();
        if !path_str.is_empty() {
            for seg in path_str.split(':') {
                if seg.is_empty() {
                    return None;
                }
                path.push(seg.to_string());
            }
        }
    }
    let segment_start = last_colon + 1;
    let mut segment_end = chars.len();
    for idx in cursor..chars.len() {
        if chars[idx] == ':' {
            segment_end = idx;
            break;
        }
    }
    let prefix: String = chars
        .iter()
        .skip(segment_start)
        .take(cursor.saturating_sub(segment_start))
        .collect();
    Some(IndexerCompletionContext {
        range: Range {
            start: Position {
                line: position.line,
                character: (word_start + segment_start) as u32,
            },
            end: Position {
                line: position.line,
                character: (word_start + segment_end) as u32,
            },
        },
        prefix,
        base,
        path,
    })
}

fn indexer_segment_context(text: &str, position: Position) -> Option<IndexerSegmentContext> {
    let line = line_at(text, position.line as usize)?;
    let col = position.character as usize;
    let (word_start, word_end) = find_word_range(&line, col);
    if word_start == word_end {
        return None;
    }
    let word = slice_by_char_indices(&line, word_start, word_end)?;
    if word.starts_with(':') || word.contains("::") {
        return None;
    }
    let chars: Vec<char> = word.chars().collect();
    let cursor = col.saturating_sub(word_start).min(chars.len());
    let first_colon = chars.iter().position(|ch| *ch == ':')?;
    if cursor <= first_colon {
        return None;
    }
    let base: String = chars[..first_colon].iter().collect();
    if base.is_empty() {
        return None;
    }
    let mut segments = Vec::new();
    let mut seg_start = first_colon + 1;
    let mut idx = seg_start;
    while idx <= chars.len() {
        if idx == chars.len() || chars[idx] == ':' {
            if seg_start < idx {
                segments.push((seg_start, idx));
            }
            seg_start = idx + 1;
        }
        idx += 1;
    }
    let mut segment_index = None;
    for (idx, (seg_start, seg_end)) in segments.iter().enumerate() {
        if cursor >= *seg_start && cursor <= *seg_end {
            segment_index = Some((idx, *seg_start, *seg_end));
            break;
        }
    }
    let Some((segment_index, seg_start, seg_end)) = segment_index else {
        return None;
    };
    let segment: String = chars
        .iter()
        .skip(seg_start)
        .take(seg_end - seg_start)
        .collect();
    if segment.is_empty() {
        return None;
    }
    let mut path = Vec::new();
    for (seg_start, seg_end) in segments.iter().take(segment_index) {
        let name: String = chars
            .iter()
            .skip(*seg_start)
            .take(seg_end - seg_start)
            .collect();
        if name.is_empty() {
            return None;
        }
        path.push(name);
    }
    Some(IndexerSegmentContext {
        base,
        path,
        segment,
    })
}

fn type_completion_context(text: &str, position: Position) -> Option<CompletionContext> {
    let line = line_at(text, position.line as usize)?;
    let col = position.character as usize;
    let (annot_start, annot_end) = find_type_annotation_bounds(&line, col)?;
    let (token_start, token_end) = type_token_bounds(&line, col, annot_start + 1, annot_end);
    let chars: Vec<char> = line.chars().collect();
    let clamped = col.min(chars.len());
    let prefix: String = chars
        .iter()
        .skip(token_start)
        .take(clamped.saturating_sub(token_start))
        .collect();
    Some(CompletionContext {
        range: Range {
            start: Position {
                line: position.line,
                character: token_start as u32,
            },
            end: Position {
                line: position.line,
                character: token_end as u32,
            },
        },
        prefix,
        mode: CompletionMode::Type,
    })
}

fn find_type_annotation_bounds(line: &str, col: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = line.chars().collect();
    let idx = col.min(chars.len());
    if let Some(bounds) = find_postfix_type_bounds(&chars, idx) {
        return Some(bounds);
    }
    find_inline_type_bounds(&chars, idx)
}

fn find_postfix_type_bounds(chars: &[char], idx: usize) -> Option<(usize, usize)> {
    let mut cursor = idx;
    while cursor > 0 {
        cursor -= 1;
        if chars[cursor] == ':' {
            if !is_annotation_colon(chars, cursor) {
                continue;
            }
            let start = cursor;
            let end = scan_type_end(chars, start + 1);
            if idx > start && idx <= end {
                return Some((start, end));
            }
        }
        if chars[cursor] == '>' && cursor > 0 && chars[cursor - 1] == '-' {
            let arrow_start = cursor - 1;
            if !is_annotation_arrow(chars, arrow_start) {
                continue;
            }
            let start = cursor;
            let end = scan_type_end(chars, start + 1);
            if idx > start && idx <= end {
                return Some((start, end));
            }
        }
    }
    None
}

fn find_inline_type_bounds(chars: &[char], idx: usize) -> Option<(usize, usize)> {
    let mut depth = 0usize;
    let mut start = None;
    let mut cursor = idx;
    while cursor > 0 {
        cursor -= 1;
        match chars[cursor] {
            '>' => depth += 1,
            '<' => {
                if depth == 0 {
                    if cursor == 0 || chars[cursor - 1].is_whitespace() {
                        continue;
                    }
                    start = Some(cursor);
                    break;
                }
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }
    }
    let start = start?;
    let mut depth = 0usize;
    let mut end = None;
    for (offset, ch) in chars.iter().enumerate().skip(start) {
        match ch {
            '<' => depth += 1,
            '>' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    end = Some(offset);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = end?;
    if idx <= start || idx > end {
        return None;
    }
    Some((start, end))
}

fn is_annotation_colon(chars: &[char], idx: usize) -> bool {
    if idx == 0 {
        return false;
    }
    if chars.get(idx + 1) == Some(&':') {
        return false;
    }
    let prev = chars[idx - 1];
    if prev.is_whitespace() || matches!(prev, '(' | '[' | '{' | ',' | ':') {
        return false;
    }
    true
}

fn is_annotation_arrow(chars: &[char], idx: usize) -> bool {
    if idx + 1 >= chars.len() || chars[idx] != '-' || chars[idx + 1] != '>' {
        return false;
    }
    if chars.get(idx + 2) == Some(&'>') {
        return false;
    }
    let Some(prev) = prev_non_space(chars, idx) else {
        return false;
    };
    !matches!(prev, '(' | '[' | '{' | ',')
}

fn prev_non_space(chars: &[char], mut idx: usize) -> Option<char> {
    while idx > 0 {
        idx -= 1;
        if !chars[idx].is_whitespace() {
            return Some(chars[idx]);
        }
    }
    None
}

fn next_non_space(chars: &[char], mut idx: usize) -> Option<char> {
    while idx < chars.len() {
        if !chars[idx].is_whitespace() {
            return Some(chars[idx]);
        }
        idx += 1;
    }
    None
}

fn next_is_arrow(chars: &[char], mut idx: usize) -> bool {
    while idx < chars.len() && chars[idx].is_whitespace() {
        idx += 1;
    }
    chars.get(idx) == Some(&'-') && chars.get(idx + 1) == Some(&'>')
}

fn scan_type_end(chars: &[char], start: usize) -> usize {
    let mut idx = start;
    let mut square_depth = 0usize;
    let mut curly_depth = 0usize;
    let mut angle_depth = 0usize;
    let mut last_op = true;
    while idx < chars.len() {
        let ch = chars[idx];
        if square_depth == 0 && curly_depth == 0 && angle_depth == 0 {
            if ch.is_whitespace() {
                if last_op {
                    idx += 1;
                    continue;
                }
                let next = next_non_space(chars, idx + 1);
                if matches!(next, Some('|')) || next_is_arrow(chars, idx + 1) {
                    idx += 1;
                    continue;
                }
                break;
            }
            if matches!(ch, ')' | ',' | ';') {
                break;
            }
            if ch == '-' && chars.get(idx + 1) == Some(&'>') {
                if chars.get(idx + 2) == Some(&'>') {
                    break;
                }
                last_op = true;
                idx += 2;
                continue;
            }
            if ch == '|' {
                last_op = true;
                idx += 1;
                continue;
            }
        }
        match ch {
            '[' => {
                square_depth += 1;
                last_op = false;
            }
            ']' => {
                if square_depth == 0 {
                    break;
                }
                square_depth = square_depth.saturating_sub(1);
                last_op = false;
            }
            '{' => {
                curly_depth += 1;
                last_op = false;
            }
            '}' => {
                if curly_depth == 0 {
                    break;
                }
                curly_depth = curly_depth.saturating_sub(1);
                last_op = false;
            }
            '<' => {
                angle_depth += 1;
                last_op = false;
            }
            '>' => {
                if angle_depth == 0 {
                    break;
                }
                angle_depth = angle_depth.saturating_sub(1);
                last_op = false;
            }
            _ => {
                last_op = false;
            }
        }
        idx += 1;
    }
    idx
}

fn type_token_bounds(line: &str, col: usize, start: usize, end: usize) -> (usize, usize) {
    let chars: Vec<char> = line.chars().collect();
    let mut min = start.min(chars.len());
    let mut max = end.min(chars.len());
    while min < max && chars[min].is_whitespace() {
        min += 1;
    }
    while max > min && chars[max - 1].is_whitespace() {
        max -= 1;
    }
    let mut left = col.min(chars.len());
    if left < min {
        left = min;
    }
    if left > max {
        left = max;
    }
    while left > min && is_type_name_char(chars[left - 1]) {
        left -= 1;
    }
    let mut right = col.min(chars.len());
    if right < min {
        right = min;
    }
    if right > max {
        right = max;
    }
    while right < max && is_type_name_char(chars[right]) {
        right += 1;
    }
    (left, right)
}

fn is_type_name_char(ch: char) -> bool {
    ch.is_alphanumeric() || matches!(ch, ':' | '-' | '_' | '.' | '!')
}

fn completion_items_for(
    prefix: &str,
    replace_range: &Range,
    store: &DocumentStore,
    doc: &DocumentData,
    mode: CompletionMode,
) -> Vec<CompletionItem> {
    let normalized = normalize_prefix(prefix);
    if matches!(mode, CompletionMode::Type) {
        return type_completion_items_for(&normalized, replace_range, store, doc);
    }
    if let Some(items) = enum_variant_completion_items(prefix, replace_range, store, doc) {
        return items;
    }
    let mut items = store.user_completion_items(&normalized, replace_range);
    let mut seen = HashSet::new();
    for item in &items {
        let key = canonical_symbol_name(&item.label).to_string();
        seen.insert(key);
    }
    let mut builtin_items = Vec::new();
    for entry in all_docs() {
        if entry_matches_prefix(entry, &normalized) {
            builtin_items.push(build_completion_item(entry, replace_range));
        }
        for alias in entry_aliases(entry) {
            if alias == entry.name {
                continue;
            }
            if normalized.is_empty() || alias.to_lowercase().starts_with(&normalized) {
                builtin_items.push(build_alias_completion_item(entry, &alias, replace_range));
            }
        }
    }
    for item in builtin_items {
        if items.len() >= 200 {
            break;
        }
        let key = canonical_symbol_name(&item.label).to_string();
        if seen.insert(key) {
            items.push(item);
        }
    }
    if items.len() >= 200 {
        items.truncate(200);
        return items;
    }
    let fnmeta_items = fnmeta_completion_items(&normalized, replace_range, &seen);
    for item in fnmeta_items {
        if items.len() >= 200 {
            break;
        }
        items.push(item);
    }
    items
}

fn type_field_candidates_at(
    doc: &DocumentData,
    position: Position,
    store: &DocumentStore,
) -> Option<Vec<TypeField>> {
    let offset = position_to_offset(&doc.text, position);
    let chars: Vec<char> = doc.text.chars().collect();
    let char_idx = offset_to_char_index(&doc.text, offset).min(chars.len());
    let map_start = find_enclosing_map_start(&chars, char_idx)?;
    let type_name = find_symbol_before(&chars, map_start)?;
    if let Some(infos) = doc.symbols.lookup(&type_name) {
        if let Some(info) = infos.iter().find(|info| info.is_type()) {
            return info.fields.clone();
        }
    }
    let info = lookup_type_symbol_info(&type_name, doc, store)?;
    if let Some(fields) = info.fields.clone() {
        return Some(fields);
    }
    registry_type_fields(&type_name, store)
}

fn find_enclosing_map_start(chars: &[char], mut idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    while idx > 0 {
        idx -= 1;
        match chars[idx] {
            '}' => depth += 1,
            '{' => {
                if depth == 0 {
                    return Some(idx);
                }
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }
    }
    None
}

fn find_symbol_before(chars: &[char], mut idx: usize) -> Option<String> {
    while idx > 0 && chars[idx - 1].is_whitespace() {
        idx -= 1;
    }
    let end = idx;
    let mut start = end;
    while start > 0 && is_word_char(chars[start - 1]) {
        start -= 1;
    }
    if start == end {
        return None;
    }
    Some(chars[start..end].iter().collect())
}

fn type_field_completion_items(
    fields: &[TypeField],
    prefix: &str,
    replace_range: &Range,
) -> Vec<CompletionItem> {
    let normalized = normalize_prefix(prefix);
    let mut items = Vec::new();
    for field in fields {
        if !field_matches_prefix(&field.name, &normalized) {
            continue;
        }
        let mut item = CompletionItem::default();
        item.label = field.name.clone();
        item.kind = Some(CompletionItemKind::FIELD);
        if !field.schema.is_empty() {
            item.detail = Some(field.schema.clone());
        }
        item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range.clone(),
            new_text: field.name.clone(),
        }));
        items.push(item);
    }
    items
}

fn indexer_completion_items(
    context: &IndexerCompletionContext,
    store: &DocumentStore,
) -> Vec<CompletionItem> {
    let Some(root) = store.lookup_map_shape(&context.base) else {
        return Vec::new();
    };
    let Some(shape) = map_shape_for_path(&root, &context.path) else {
        return Vec::new();
    };
    let normalized = normalize_prefix(&context.prefix);
    let mut items = Vec::new();
    for field in &shape.fields {
        if !indexer_field_matches_prefix(&field.name, &normalized) {
            continue;
        }
        let mut item = CompletionItem::default();
        item.label = format!(":{}", field.name);
        item.kind = Some(CompletionItemKind::FIELD);
        if field.nested.is_some() {
            item.detail = Some("map".to_string());
        }
        item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
            range: context.range.clone(),
            new_text: field.name.clone(),
        }));
        items.push(item);
    }
    items
}

fn map_shape_for_path<'a>(shape: &'a MapShape, path: &[String]) -> Option<&'a MapShape> {
    let mut current = shape;
    for segment in path {
        let field = current.fields.iter().find(|field| field.name == *segment)?;
        let next = field.nested.as_deref()?;
        current = next;
    }
    Some(current)
}

fn field_matches_prefix(field: &str, normalized_prefix: &str) -> bool {
    if normalized_prefix.is_empty() {
        return true;
    }
    let field_norm = field.trim_start_matches(':').to_lowercase();
    let prefix_norm = normalized_prefix.trim_start_matches(':');
    field_norm.starts_with(prefix_norm)
}

fn indexer_field_matches_prefix(field: &str, normalized_prefix: &str) -> bool {
    if normalized_prefix.is_empty() {
        return true;
    }
    field.to_lowercase().starts_with(normalized_prefix)
}

fn type_completion_items_for(
    normalized_prefix: &str,
    replace_range: &Range,
    store: &DocumentStore,
    doc: &DocumentData,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut seen = HashSet::new();
    for name in builtin_type_candidates(store) {
        if !type_name_variants(&name)
            .iter()
            .any(|variant| variant.starts_with(normalized_prefix))
        {
            continue;
        }
        if seen.insert(name.clone()) {
            items.push(build_type_completion_item(
                &name,
                None,
                replace_range,
                CompletionItemKind::CLASS,
                Some("builtin type".into()),
            ));
        }
    }
    for info in store.iter_type_symbols() {
        let name = format_type_completion_name(info, doc);
        if !type_name_variants(&name)
            .iter()
            .any(|variant| variant.starts_with(normalized_prefix))
        {
            continue;
        }
        if !seen.insert(name.clone()) {
            continue;
        }
        let kind = match info.kind {
            SymbolKind::Defenum => CompletionItemKind::ENUM,
            SymbolKind::Deftype => CompletionItemKind::STRUCT,
            _ => CompletionItemKind::CLASS,
        };
        let detail = Some(match info.kind {
            SymbolKind::Defenum => "enum type".to_string(),
            SymbolKind::Deftype => "type".to_string(),
            _ => "type".to_string(),
        });
        items.push(build_type_completion_item(
            &name,
            Some(info.canonical.clone()),
            replace_range,
            kind,
            detail,
        ));
    }
    items.truncate(200);
    items
}

fn build_type_completion_item(
    label: &str,
    canonical: Option<String>,
    replace_range: &Range,
    kind: CompletionItemKind,
    detail: Option<String>,
) -> CompletionItem {
    let mut item = CompletionItem::default();
    item.label = label.to_string();
    item.kind = Some(kind);
    item.detail = detail;
    item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
        range: replace_range.clone(),
        new_text: label.to_string(),
    }));
    if let Some(symbol) = canonical {
        item.data = Some(json!({ "symbol": symbol, "source": "user" }));
    }
    item
}

fn type_name_variants(name: &str) -> Vec<String> {
    let mut variants = Vec::new();
    let lower = name.to_lowercase();
    variants.push(lower.clone());
    if let Some((_, tail)) = name.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    variants.sort();
    variants.dedup();
    variants
}

fn builtin_type_candidates(store: &DocumentStore) -> Vec<String> {
    store.with_lsp_runtime(|| {
        let mut names = Vec::new();
        for entry in type_registry::list_all_types() {
            let display = display_registry_type_name(&entry);
            names.push(display);
        }
        names.push("Any".to_string());
        names.push("Integer".to_string());
        names.push("String".to_string());
        names.push("Sym".to_string());
        names.push("Keyword".to_string());
        names.push("Kw".to_string());
        names.push("Re".to_string());
        names.push("Vec".to_string());
        names.sort();
        names.dedup();
        names
    })
}

fn display_registry_type_name(name: &str) -> String {
    let name = name.strip_prefix("clove::").unwrap_or(name);
    if let Some(stripped) = name.strip_prefix("core::") {
        return stripped.to_string();
    }
    name.to_string()
}

fn format_type_completion_name(info: &SymbolInfo, doc: &DocumentData) -> String {
    let Some(ns) = &info.namespace else {
        return info.name.clone();
    };
    if doc.namespace.as_deref() == Some(ns) {
        return info.name.clone();
    }
    format!("{}::{}", ns, info.name)
}

fn completion_item_symbol(item: &CompletionItem) -> Option<String> {
    if let Some(data) = &item.data {
        if let Some(symbol) = data.get("symbol").and_then(|v| v.as_str()) {
            return Some(symbol.to_string());
        }
    }
    Some(item.label.clone())
}

#[derive(Debug)]
struct SignatureCallContext {
    head: String,
    argument_index: usize,
}

fn signature_context(text: &str, position: Position) -> Option<SignatureCallContext> {
    let offset = position_to_offset(text, position);
    let chars: Vec<char> = text.chars().collect();
    let char_idx = offset_to_char_index(text, offset);
    find_call_context(&chars, char_idx)
}

fn find_call_context(chars: &[char], cursor: usize) -> Option<SignatureCallContext> {
    if chars.is_empty() {
        return None;
    }
    let mut idx = cursor.min(chars.len());
    let mut depth: isize = 0;
    while idx > 0 {
        idx -= 1;
        match chars[idx] {
            ')' | ']' | '}' => depth += 1,
            '(' => {
                if depth == 0 {
                    let head_start = skip_whitespace(chars, idx + 1);
                    let head_end = read_symbol(chars, head_start);
                    if head_start >= head_end || head_end > chars.len() {
                        return None;
                    }
                    let head: String = chars[head_start..head_end].iter().collect();
                    let argument_index = count_arguments(chars, head_end, cursor);
                    return Some(SignatureCallContext {
                        head,
                        argument_index,
                    });
                } else {
                    depth -= 1;
                }
            }
            '[' | '{' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            '"' => {
                // skip string literals backwards
                idx = skip_string_literal_backwards(chars, idx);
            }
            ';' => {
                // skip line comment backwards
                idx = skip_line_comment_backwards(chars, idx);
            }
            _ => {}
        }
    }
    None
}

fn skip_whitespace(chars: &[char], mut idx: usize) -> usize {
    while idx < chars.len() && chars[idx].is_whitespace() {
        idx += 1;
    }
    idx
}

fn read_symbol(chars: &[char], mut idx: usize) -> usize {
    while idx < chars.len() && !is_symbol_boundary(chars[idx]) {
        idx += 1;
    }
    idx
}

fn is_symbol_boundary(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';')
}

fn count_arguments(chars: &[char], mut idx: usize, cursor: usize) -> usize {
    idx = skip_whitespace(chars, idx);
    let mut depth = 0usize;
    let mut arg_index = 0usize;
    let mut seen = false;
    while idx < cursor && idx < chars.len() {
        let ch = chars[idx];
        match ch {
            '(' | '[' | '{' => {
                depth += 1;
                seen = true;
            }
            ')' | ']' | '}' => {
                if depth > 0 {
                    depth -= 1;
                } else {
                    break;
                }
            }
            '"' => {
                seen = true;
                idx = skip_string_literal(chars, idx);
                continue;
            }
            ';' => {
                idx = skip_line_comment(chars, idx);
                continue;
            }
            _ if ch.is_whitespace() => {
                if depth == 0 && seen {
                    arg_index += 1;
                    seen = false;
                }
            }
            _ => {
                if depth == 0 && !seen {
                    seen = true;
                }
            }
        }
        idx += 1;
    }
    arg_index
}

fn skip_string_literal(chars: &[char], mut idx: usize) -> usize {
    idx += 1;
    while idx < chars.len() {
        if chars[idx] == '"' && chars.get(idx.saturating_sub(1)) != Some(&'\\') {
            return idx;
        }
        idx += 1;
    }
    idx
}

fn skip_string_literal_backwards(chars: &[char], mut idx: usize) -> usize {
    if chars[idx] != '"' {
        return idx;
    }
    if idx == 0 {
        return idx;
    }
    idx -= 1;
    while idx > 0 {
        if chars[idx] == '"' && chars.get(idx.saturating_sub(1)) != Some(&'\\') {
            return idx;
        }
        idx -= 1;
    }
    idx
}

fn skip_line_comment(chars: &[char], mut idx: usize) -> usize {
    while idx < chars.len() && chars[idx] != '\n' {
        idx += 1;
    }
    idx
}

fn skip_line_comment_backwards(chars: &[char], mut idx: usize) -> usize {
    while idx > 0 && chars[idx] != '\n' {
        idx -= 1;
    }
    idx
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut line = 0usize;
    let mut col = 0usize;
    let target_line = position.line as usize;
    let target_col = position.character as usize;
    for (idx, ch) in text.char_indices() {
        if line == target_line && col == target_col {
            return idx;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
            if line > target_line {
                return idx;
            }
        } else {
            col += 1;
        }
    }
    text.len()
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0usize;
    let mut col = 0usize;
    for (idx, ch) in text.char_indices() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position {
        line: line as u32,
        character: col as u32,
    }
}

fn offset_to_char_index(text: &str, offset: usize) -> usize {
    let mut count = 0usize;
    for (idx, _) in text.char_indices() {
        if idx >= offset {
            break;
        }
        count += 1;
    }
    count
}

fn range_is_empty(range: &Range) -> bool {
    range.start.line == range.end.line && range.start.character == range.end.character
}

fn form_bounds(text: &str, form: &Form) -> Option<(usize, usize)> {
    let start = span_offset(&form.span, text);
    let end = scan_form_end(text, start)?;
    if end < start {
        return None;
    }
    Some((start, end))
}

fn form_bounds_without_suffixes(text: &str, form: &Form) -> Option<(usize, usize)> {
    let start = span_offset(&form.span, text);
    let end = scan_form_end_base(text, start)?;
    if end < start {
        return None;
    }
    Some((start, end))
}

fn form_range(text: &str, form: &Form) -> Option<Range> {
    let (start, end) = form_bounds(text, form)?;
    Some(Range {
        start: offset_to_position(text, start),
        end: offset_to_position(text, end),
    })
}

fn form_source(text: &str, form: &Form) -> Option<String> {
    let range = form_range(text, form)?;
    slice_range(text, &range)
}

fn form_source_without_suffixes(text: &str, form: &Form) -> Option<String> {
    let (start, end) = form_bounds_without_suffixes(text, form)?;
    text.get(start..end).map(|s| s.to_string())
}

fn scan_form_end(text: &str, start: usize) -> Option<usize> {
    let base_end = scan_form_end_base(text, start)?;
    let mut end = base_end;
    loop {
        if end >= text.len() {
            break;
        }
        let ch = match char_at(text, end) {
            Some(ch) => ch,
            None => break,
        };
        if ch == '[' {
            let close = find_matching_bracket(text, end)?;
            end = close + 1;
            continue;
        }
        if ch == '?' {
            if next_char_at(text, end) == Some('.') {
                end += ch.len_utf8();
                continue;
            }
            break;
        }
        if ch == '.' {
            let after_dot = end + ch.len_utf8();
            end = scan_oop_dot_stage_end(text, after_dot)?;
            continue;
        }
        if ch == ':' {
            end = scan_token_end(text, end)?;
            continue;
        }
        break;
    }
    Some(end)
}

fn scan_form_end_base(text: &str, start: usize) -> Option<usize> {
    let mut idx = start;
    let ch = char_at(text, idx)?;
    if matches!(ch, '\'' | '@' | '$' | '&') {
        idx += ch.len_utf8();
        let next = skip_ws_and_comments(text, idx);
        return scan_form_end_base(text, next);
    }
    match ch {
        '(' => find_matching_paren(text, idx).map(|end| end + 1),
        '[' => find_matching_bracket(text, idx).map(|end| end + 1),
        '{' => find_matching_delim(text, idx, '{', '}').map(|end| end + 1),
        '"' => scan_string_end(text, idx),
        '#' => scan_dispatch_end(text, idx),
        ':' => scan_token_end(text, idx),
        _ => scan_token_end(text, idx),
    }
}

fn scan_dispatch_end(text: &str, start: usize) -> Option<usize> {
    let next = next_char_at(text, start)?;
    let next_offset = start + '#'.len_utf8();
    match next {
        '(' => find_matching_paren(text, next_offset).map(|end| end + 1),
        '{' => find_matching_delim(text, next_offset, '{', '}').map(|end| end + 1),
        '[' => find_matching_bracket(text, next_offset).map(|end| end + 1),
        '"' => scan_string_end(text, next_offset),
        '/' => scan_regex_end(text, next_offset, '/'),
        '_' => {
            let after = next_offset + '_'.len_utf8();
            let after = skip_ws_and_comments(text, after);
            scan_form_end(text, after)
        }
        _ => scan_token_end(text, start),
    }
}

fn scan_string_end(text: &str, start: usize) -> Option<usize> {
    let mut escape = false;
    let mut iter = text.char_indices().skip_while(|(i, _)| *i < start);
    let (_, first) = iter.next()?;
    if first != '"' {
        return None;
    }
    for (idx, ch) in iter {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if ch == '"' {
            return Some(idx + ch.len_utf8());
        }
    }
    None
}

fn scan_regex_end(text: &str, start: usize, delim: char) -> Option<usize> {
    let mut escape = false;
    let mut iter = text.char_indices().skip_while(|(i, _)| *i < start);
    let (_, first) = iter.next()?;
    if first != delim {
        return None;
    }
    for (idx, ch) in iter {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' {
            escape = true;
            continue;
        }
        if ch == delim {
            return Some(idx + ch.len_utf8());
        }
    }
    None
}

fn scan_token_end(text: &str, start: usize) -> Option<usize> {
    let pos = offset_to_position(text, start);
    let line_no = pos.line as usize;
    let line = line_at(text, line_no)?;
    let line_start = position_to_offset(
        text,
        Position {
            line: pos.line,
            character: 0,
        },
    );
    let (_, end_col) = symbol_token_range_from(&line, pos.character as usize);
    Some(line_start + end_col)
}

fn scan_oop_dot_stage_end(text: &str, start: usize) -> Option<usize> {
    let ch = char_at(text, start)?;
    match ch {
        '(' => find_matching_paren(text, start).map(|end| end + 1),
        ':' => scan_token_end(text, start),
        '"' => scan_string_end(text, start),
        '\'' => scan_form_end(text, start),
        '#' => scan_dispatch_end(text, start),
        _ => {
            let name_end = scan_oop_ident_end(text, start)?;
            let mut end = name_end;
            if char_at(text, end) == Some('(') {
                let close = find_matching_paren(text, end)?;
                end = close + 1;
            }
            Some(end)
        }
    }
}

fn scan_oop_ident_end(text: &str, start: usize) -> Option<usize> {
    let mut end = start;
    for (rel, ch) in text[start..].char_indices() {
        if !is_oop_ident_char(ch) {
            break;
        }
        end = start + rel + ch.len_utf8();
    }
    if end == start {
        None
    } else {
        Some(end)
    }
}

fn skip_ws_and_comments(text: &str, mut idx: usize) -> usize {
    while idx < text.len() {
        let ch = match char_at(text, idx) {
            Some(ch) => ch,
            None => break,
        };
        if is_ws_or_comma(ch) {
            idx += ch.len_utf8();
            continue;
        }
        if ch == ';' {
            while idx < text.len() {
                let ch = match char_at(text, idx) {
                    Some(ch) => ch,
                    None => break,
                };
                idx += ch.len_utf8();
                if ch == '\n' {
                    break;
                }
            }
            continue;
        }
        break;
    }
    idx
}

fn char_at(text: &str, offset: usize) -> Option<char> {
    text.get(offset..).and_then(|s| s.chars().next())
}

fn next_char_at(text: &str, offset: usize) -> Option<char> {
    text.get(offset..).and_then(|s| s.chars().nth(1))
}

fn find_matching_delim(text: &str, start: usize, open: char, close: char) -> Option<usize> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escape = false;
    let mut in_comment = false;
    for (idx, ch) in text.char_indices().skip_while(|(i, _)| *i < start) {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '\\' => escape = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }
        match ch {
            ch if ch == open => depth += 1,
            ch if ch == close => {
                if depth == 0 {
                    continue;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            '"' => in_string = true,
            ';' => in_comment = true,
            _ => {}
        }
    }
    None
}

fn find_matching_paren(text: &str, start: usize) -> Option<usize> {
    find_matching_delim(text, start, '(', ')')
}

fn find_matching_bracket(text: &str, start: usize) -> Option<usize> {
    find_matching_delim(text, start, '[', ']')
}

fn line_indentation(text: &str, offset: usize) -> String {
    let mut line_start = 0usize;
    for (idx, ch) in text.char_indices() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line_start = idx + ch.len_utf8();
        }
    }
    let mut indent = String::new();
    for ch in text[line_start..].chars() {
        if ch == ' ' || ch == '\t' {
            indent.push(ch);
        } else {
            break;
        }
    }
    indent
}

fn normalize_prefix(prefix: &str) -> String {
    prefix.trim().to_lowercase()
}

fn entry_matches_prefix(entry: &DocEntry, normalized: &str) -> bool {
    if normalized.is_empty() {
        return true;
    }
    entry_name_variants(entry)
        .iter()
        .any(|variant| variant.starts_with(normalized))
}

fn entry_name_variants(entry: &DocEntry) -> Vec<String> {
    let mut variants = Vec::new();
    variants.push(entry.name.to_lowercase());
    variants.push(entry.canonical.to_lowercase());
    if let Some((_, tail)) = entry.canonical.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    if let Some((_, tail)) = entry.name.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    variants.retain(|v| !v.is_empty());
    variants
}

fn fnmeta_completion_items(
    normalized_prefix: &str,
    replace_range: &Range,
    seen: &HashSet<String>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for meta in fn_meta::all() {
        if !fnmeta_matches_prefix(&meta, normalized_prefix) {
            continue;
        }
        let label = meta.fq_name();
        let key = canonical_symbol_name(&label).to_string();
        if seen.contains(&key) {
            continue;
        }
        let mut item = CompletionItem::default();
        item.label = label.clone();
        item.kind = Some(CompletionItemKind::FUNCTION);
        item.detail = fnmeta_signature_label(&label, &meta);
        item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range.clone(),
            new_text: label,
        }));
        items.push(item);
    }
    items
}

fn fnmeta_matches_prefix(meta: &fn_meta::FnMeta, normalized: &str) -> bool {
    if normalized.is_empty() {
        return true;
    }
    fnmeta_name_variants(meta)
        .iter()
        .any(|variant| variant.starts_with(normalized))
}

fn fnmeta_name_variants(meta: &fn_meta::FnMeta) -> Vec<String> {
    let mut variants = Vec::new();
    let fq = meta.fq_name();
    variants.push(fq.to_lowercase());
    variants.push(meta.name.to_lowercase());
    if let Some((_, tail)) = fq.rsplit_once("::") {
        variants.push(tail.to_lowercase());
    }
    variants.retain(|v| !v.is_empty());
    variants.sort();
    variants.dedup();
    variants
}

fn build_completion_item(entry: &DocEntry, replace_range: &Range) -> CompletionItem {
    let mut item = CompletionItem::default();
    item.label = entry.name.clone();
    item.kind = Some(completion_item_kind(entry));
    item.detail = completion_detail(entry);
    item.documentation = documentation_for_entry(entry);
    item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
        range: replace_range.clone(),
        new_text: entry.name.clone(),
    }));
    item.data = Some(json!({ "symbol": entry.canonical.clone() }));
    item
}

fn build_alias_completion_item(
    entry: &DocEntry,
    alias: &str,
    replace_range: &Range,
) -> CompletionItem {
    let mut item = CompletionItem::default();
    item.label = alias.to_string();
    item.kind = Some(completion_item_kind(entry));
    item.detail = completion_detail(entry);
    item.documentation = documentation_for_entry(entry);
    item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
        range: replace_range.clone(),
        new_text: alias.to_string(),
    }));
    item.data = Some(json!({ "symbol": entry.canonical.clone() }));
    item
}

fn completion_item_kind(entry: &DocEntry) -> CompletionItemKind {
    match entry.origin.as_deref() {
        Some("special form") => CompletionItemKind::KEYWORD,
        _ => CompletionItemKind::FUNCTION,
    }
}

fn completion_detail(entry: &DocEntry) -> Option<String> {
    let signature = entry
        .signature
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty());
    let origin = entry
        .origin
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty());
    let summary = doc_summary(entry);
    match (signature, summary.as_deref(), origin) {
        (Some(sig), Some(sum), Some(origin)) => Some(format!("{} — {} ({})", sig, sum, origin)),
        (Some(sig), Some(sum), None) => Some(format!("{} — {}", sig, sum)),
        (Some(sig), None, Some(origin)) => Some(format!("{} ({})", sig, origin)),
        (Some(sig), None, None) => Some(sig.to_string()),
        (None, Some(sum), Some(origin)) => Some(format!("{} ({})", sum, origin)),
        (None, Some(sum), None) => Some(sum.to_string()),
        (None, None, Some(origin)) => Some(origin.to_string()),
        _ => None,
    }
}

const COMPLETION_SUMMARY_MAX_LEN: usize = 80;

fn doc_summary(entry: &DocEntry) -> Option<String> {
    entry.doc.as_deref().and_then(doc_summary_line)
}

fn doc_summary_line(text: &str) -> Option<String> {
    text.lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .map(|line| ellipsize(line, COMPLETION_SUMMARY_MAX_LEN))
}

fn ellipsize(input: &str, max_chars: usize) -> String {
    let mut out = String::new();
    let mut iter = input.chars();
    for _ in 0..max_chars {
        match iter.next() {
            Some(ch) => out.push(ch),
            None => return out,
        }
    }
    if iter.next().is_some() {
        out.push_str("...");
    }
    out
}

fn documentation_for_entry(entry: &DocEntry) -> Option<Documentation> {
    format_doc_markdown(entry).map(|markdown| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        })
    })
}

fn format_doc_markdown(entry: &DocEntry) -> Option<String> {
    let mut sections = Vec::new();
    if let Some(sig) = entry
        .signature
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        sections.push(format!("```clojure\n{}\n```", sig));
    }
    if let Some(doc) = entry
        .doc
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        sections.push(doc.to_string());
    }
    let aliases = entry_aliases(entry);
    if !aliases.is_empty() {
        sections.push(format!("_Aliases: {}_", aliases.join(", ")));
    }
    if !entry.examples.is_empty() {
        let formatted_examples: Vec<String> = entry
            .examples
            .iter()
            .map(|ex| ex.trim())
            .filter(|ex| !ex.is_empty())
            .map(|ex| format!("```clojure\n{}\n```", ex))
            .collect();
        if !formatted_examples.is_empty() {
            sections.push(format!("**Examples**\n\n{}", formatted_examples.join("\n")));
        }
    }
    if !entry.oop_examples.is_empty() {
        let formatted_examples: Vec<String> = entry
            .oop_examples
            .iter()
            .map(|ex| ex.trim())
            .filter(|ex| !ex.is_empty())
            .map(|ex| format!("```clojure\n{}\n```", ex))
            .collect();
        if !formatted_examples.is_empty() {
            sections.push(format!(
                "**OOP Examples**\n\n{}",
                formatted_examples.join("\n")
            ));
        }
    }
    if let Some(origin) = entry
        .origin
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        sections.push(format!("_Origin: {}_", origin));
    }
    if sections.is_empty() {
        None
    } else {
        Some(sections.join("\n\n"))
    }
}

fn format_fnmeta_markdown(meta: &fn_meta::FnMeta) -> Option<String> {
    let name = meta.fq_name();
    let sig = fnmeta_signature_label(&name, meta)?;
    let mut sections = Vec::new();
    sections.push(format!("```clojure\n{}\n```", sig));
    if let Some(doc) = meta
        .doc
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        sections.push(doc.to_string());
    }
    Some(sections.join("\n\n"))
}

fn entry_aliases(entry: &DocEntry) -> Vec<String> {
    let mut aliases = symbol_aliases(&entry.canonical);
    if entry.name != entry.canonical {
        aliases.push(entry.name.clone());
    }
    aliases.sort();
    aliases.dedup();
    aliases.retain(|alias| alias != &entry.canonical);
    aliases
}

fn type_hover_markdown(name: &str, doc: &DocumentData, store: &DocumentStore) -> Option<String> {
    let lookup = normalize_type_name_for_lookup(name);
    let registry = registry_type_summary(&lookup, store);
    if let Some(info) = lookup_type_symbol_info(&lookup, doc, store) {
        let docstring = info
            .docstring
            .as_ref()
            .map(|d| d.trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .or_else(|| registry.as_ref().and_then(|(_, _, doc, _)| doc.clone()));
        let kind = match info.kind {
            SymbolKind::Defenum => "defenum",
            SymbolKind::Deftype => "deftype",
            _ => "type",
        };
        return Some(format_type_markdown(
            &info.name,
            kind,
            docstring.as_deref(),
            info.fields.as_deref(),
        ));
    }
    if let Some((display, kind, docstring, fields)) = registry {
        return Some(format_type_markdown(
            &display,
            kind,
            docstring.as_deref(),
            fields.as_deref(),
        ));
    }
    None
}

fn enum_variant_hover_markdown(
    word: &str,
    doc: &DocumentData,
    store: &DocumentStore,
) -> Option<String> {
    let resolved = resolve_enum_variant(word, doc, store)?;
    let variant_name = resolved.member.name.clone();
    let enum_display = resolved.enum_raw.clone();
    let display = format!("{enum_display}::{variant_name}");
    let enum_ns = resolved.enum_info.namespace.as_deref();
    let mut infos = store.find_symbol_infos(&variant_name, enum_ns);
    if infos.is_empty() {
        let canon = canonical_symbol_name(&variant_name).into_owned();
        if canon != variant_name {
            infos = store.find_symbol_infos(&canon, enum_ns);
        }
    }
    let info = infos
        .into_iter()
        .find(|info| matches!(info.kind, SymbolKind::Deftype));
    let docstring = info
        .as_ref()
        .and_then(|info| info.docstring.as_ref())
        .map(|doc| doc.trim().to_string())
        .filter(|doc| !doc.is_empty())
        .or_else(|| Some(format!("{} variant {}", enum_display, variant_name)));
    let fields = info.and_then(|info| info.fields);
    Some(format_type_markdown(
        &display,
        "deftype",
        docstring.as_deref(),
        fields.as_deref(),
    ))
}

fn lookup_type_symbol_info(
    name: &str,
    doc: &DocumentData,
    store: &DocumentStore,
) -> Option<SymbolInfo> {
    let candidates = resolve_symbol_candidates(name, doc);
    for (ns, lookup) in candidates {
        let mut infos = store.find_symbol_infos(&lookup, ns.as_deref());
        if infos.is_empty() {
            let canon = canonical_symbol_name(&lookup).into_owned();
            if canon != lookup {
                infos = store.find_symbol_infos(&canon, ns.as_deref());
            }
        }
        if let Some(info) = infos.into_iter().find(|info| info.is_type()) {
            return Some(info);
        }
    }
    let mut infos = store.find_symbol_infos(name, None);
    if infos.is_empty() {
        let canon = canonical_symbol_name(name).into_owned();
        if canon != name {
            infos = store.find_symbol_infos(&canon, None);
        }
    }
    infos.into_iter().find(|info| info.is_type())
}

fn lookup_symbol_info(name: &str, doc: &DocumentData, store: &DocumentStore) -> Option<SymbolInfo> {
    let candidates = resolve_symbol_candidates(name, doc);
    for (ns, lookup) in candidates {
        let mut infos = store.find_symbol_infos(&lookup, ns.as_deref());
        if infos.is_empty() {
            let canon = canonical_symbol_name(&lookup).into_owned();
            if canon != lookup {
                infos = store.find_symbol_infos(&canon, ns.as_deref());
            }
        }
        if let Some(info) = infos.into_iter().next() {
            return Some(info);
        }
    }
    store.lookup_user_symbol_info(name)
}

fn format_type_markdown(
    name: &str,
    kind: &str,
    docstring: Option<&str>,
    fields: Option<&[TypeField]>,
) -> String {
    let mut sections = Vec::new();
    sections.push(format!("```clojure\n({} {})\n```", kind, name));
    if let Some(doc) = docstring {
        if !doc.trim().is_empty() {
            sections.push(doc.to_string());
        }
    }
    if let Some(fields) = fields {
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
    sections.join("\n\n")
}

fn registry_type_summary(
    name: &str,
    store: &DocumentStore,
) -> Option<(String, &'static str, Option<String>, Option<Vec<TypeField>>)> {
    store.with_lsp_runtime(|| {
        for candidate in registry_type_candidates(name) {
            if let Some(entry) = type_registry::get_type_entry(&candidate) {
                let display = display_registry_type_name(&candidate);
                return Some(match entry {
                    TypeEntry::Primitive(meta) => (display, "deftype", meta.doc, None),
                    TypeEntry::Product(meta) => {
                        let doc = meta.doc.clone();
                        let fields = registry_type_fields_from_meta(&meta);
                        (display, "deftype", doc, fields)
                    }
                    TypeEntry::Sum(meta) => (display, "defenum", meta.doc, None),
                    TypeEntry::Alias(meta) => (display, "deftype", meta.doc, None),
                });
            }
        }
        None
    })
}

fn registry_type_fields(name: &str, store: &DocumentStore) -> Option<Vec<TypeField>> {
    store.with_lsp_runtime(|| {
        for candidate in registry_type_candidates(name) {
            if let Some(TypeEntry::Product(meta)) = type_registry::get_type_entry(&candidate) {
                return registry_type_fields_from_meta(&meta);
            }
        }
        None
    })
}

fn registry_type_fields_from_meta(
    meta: &clove_core::type_registry::ProductMeta,
) -> Option<Vec<TypeField>> {
    if meta.fields.is_empty() {
        return None;
    }
    let mut fields = Vec::new();
    for field in &meta.fields {
        fields.push(TypeField {
            name: format!(":{}", field.name),
            schema: field.schema.describe(Some(&meta.namespace)),
        });
    }
    Some(fields)
}

fn canonical_type_alias(name: &str) -> Option<&'static str> {
    match name {
        "String" => Some("Str"),
        "Integer" => Some("Int"),
        "Sym" => Some("Symbol"),
        "Keyword" | "Kw" => Some("Symbol"),
        "Re" => Some("Regex"),
        "Vec" => Some("Vector"),
        _ => None,
    }
}

fn registry_type_candidates(name: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    let (prefix, base) = match name.rsplit_once("::") {
        Some((prefix, base)) => (Some(prefix), base),
        None => (None, name),
    };
    let alias = canonical_type_alias(base);
    let mut push = |value: String| {
        if seen.insert(value.clone()) {
            out.push(value);
        }
    };
    match prefix {
        Some(prefix) => {
            push(name.to_string());
            if let Some(alias) = alias {
                push(format!("{}::{}", prefix, alias));
            }
            if prefix == "core" {
                if let Some(alias) = alias {
                    push(format!("clove::core::{}", alias));
                }
            } else if prefix == "clove::core" {
                if let Some(alias) = alias {
                    push(format!("core::{}", alias));
                }
            }
        }
        None => {
            push(name.to_string());
            push(format!("core::{}", name));
            push(format!("clove::core::{}", name));
            if let Some(alias) = alias {
                push(alias.to_string());
                push(format!("core::{}", alias));
                push(format!("clove::core::{}", alias));
            }
        }
    }
    out
}

fn type_info_at(doc: &DocumentData, line: u32, start: usize) -> Option<String> {
    let offset = position_to_offset(
        &doc.text,
        Position {
            line,
            character: start as u32,
        },
    );
    let char_index = offset_to_char_index(&doc.text, offset);
    let hint = doc.hint_map.get(&char_index);
    let inferred = doc.type_map.get(&char_index);
    format_type_info(hint, inferred)
}

fn format_type_info(hint: Option<&TypeKind>, inferred: Option<&String>) -> Option<String> {
    if hint.is_none() && inferred.is_none() {
        return None;
    }
    let mut lines = Vec::new();
    if let Some(h) = hint {
        lines.push(format!("hint: {}", h.describe()));
    }
    if let Some(t) = inferred {
        lines.push(format!("type: {}", t));
    }
    Some(format!("```clojure\n{}\n```", lines.join("\n")))
}

fn inlay_hints_for_doc(doc: &DocumentData) -> Vec<InlayHint> {
    let mut out = Vec::new();
    let Some(forms) = &doc.ast else {
        return out;
    };
    for form in forms {
        collect_inlay_hints(form, doc, &mut out);
    }
    out
}

fn collect_inlay_hints(form: &Form, doc: &DocumentData, out: &mut Vec<InlayHint>) {
    match &form.kind {
        FormKind::List(items) => {
            if let Some(head) = list_head_symbol(items) {
                match head {
                    "let" | "loop" => collect_let_inlays(items, doc, out),
                    "def" | "def-" => collect_def_inlay(items, doc, out),
                    "defn" | "defn-" => collect_defn_return_inlay(items, doc, out),
                    _ => {}
                }
            }
            for item in items {
                collect_inlay_hints(item, doc, out);
            }
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                collect_inlay_hints(item, doc, out);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_inlay_hints(k, doc, out);
                        collect_inlay_hints(v, doc, out);
                    }
                    clove_core::ast::MapItem::Spread(expr) => {
                        collect_inlay_hints(expr, doc, out);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_inlay_hints(expr, doc, out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_inlay_hints(expr, doc, out);
                }
            }
        }
        _ => {}
    }
}

fn list_head_symbol(items: &[Form]) -> Option<&str> {
    match items.first() {
        Some(Form {
            kind: FormKind::Symbol(sym),
            ..
        }) => Some(sym.as_str()),
        _ => None,
    }
}

fn collect_let_inlays(items: &[Form], doc: &DocumentData, out: &mut Vec<InlayHint>) {
    if items.len() < 2 {
        return;
    }
    let bindings = match &items[1].kind {
        FormKind::Vector(items) => items,
        _ => return,
    };
    let mut idx = 0;
    while idx + 1 < bindings.len() {
        let name_form = &bindings[idx];
        let value_form = &bindings[idx + 1];
        if name_form.type_hint.is_none() {
            if let FormKind::Symbol(_) = name_form.kind {
                if let Some(ty) = doc.type_map.get(&value_form.span.index) {
                    let position = binding_line_end_position(doc, name_form.span);
                    out.push(make_type_inlay_hint(position, ty));
                }
            }
        }
        idx += 2;
    }
}

fn collect_def_inlay(items: &[Form], doc: &DocumentData, out: &mut Vec<InlayHint>) {
    if items.len() < 3 {
        return;
    }
    let name_form = &items[1];
    let value_form = &items[2];
    if name_form.type_hint.is_some() {
        return;
    }
    if let FormKind::Symbol(_) = name_form.kind {
        if let Some(ty) = doc.type_map.get(&value_form.span.index) {
            let position = symbol_end_position(doc, name_form.span);
            out.push(make_type_inlay_hint(position, ty));
        }
    }
}

fn collect_defn_return_inlay(items: &[Form], doc: &DocumentData, out: &mut Vec<InlayHint>) {
    if items.len() < 3 {
        return;
    }
    let name_form = &items[1];
    if name_form.type_hint.is_some() {
        return;
    }
    if let FormKind::Symbol(_) = name_form.kind {
        if let Some(ty) = doc.type_map.get(&name_form.span.index) {
            if let Some(ret_ty) = extract_fn_return_type(ty) {
                let position = defn_return_inlay_position(items, doc)
                    .unwrap_or_else(|| symbol_end_position(doc, name_form.span));
                out.push(make_return_inlay_hint(position, &ret_ty));
            }
        }
    }
}

fn defn_return_inlay_position(items: &[Form], doc: &DocumentData) -> Option<Position> {
    let params_form = defn_params_form(items)?;
    let start = span_offset(&params_form.span, &doc.text);
    let end = find_matching_bracket(&doc.text, start)?;
    let end = end.saturating_add(1).min(doc.text.len());
    let end_pos = offset_to_position(&doc.text, end);
    let line_text = line_at(&doc.text, end_pos.line as usize)?;
    let line_end_pos = Position {
        line: end_pos.line,
        character: line_text.chars().count() as u32,
    };
    if line_suffix_has_return_arrow(&line_text, end_pos.character as usize) {
        return Some(line_end_pos);
    }
    Some(end_pos)
}

fn defn_params_form(items: &[Form]) -> Option<&Form> {
    if items.len() < 3 {
        return None;
    }
    let mut idx = 2;
    let (_, after_doc) = extract_docstring(items, idx, false);
    idx = after_doc;
    if matches!(
        items.get(idx),
        Some(Form {
            kind: FormKind::Vector(_),
            ..
        })
    ) {
        return items.get(idx);
    }
    for form in items.iter().skip(idx) {
        if let FormKind::List(variants) = &form.kind {
            if let Some(Form {
                kind: FormKind::Vector(_),
                ..
            }) = variants.first()
            {
                return Some(&variants[0]);
            }
        }
    }
    None
}

fn line_suffix_has_return_arrow(line: &str, start_col: usize) -> bool {
    let suffix: String = line.chars().skip(start_col).collect();
    let code = suffix.split(';').next().unwrap_or("");
    code.contains("->")
}

fn extract_fn_return_type(ty: &str) -> Option<String> {
    let trimmed = ty.trim();
    if trimmed.is_empty() {
        return None;
    }
    if let Some((_, ret)) = trimmed.rsplit_once("->") {
        return Some(ret.trim().to_string());
    }
    Some(trimmed.to_string())
}

fn symbol_end_position(doc: &DocumentData, span: Span) -> Position {
    let line = span.line.saturating_sub(1);
    let col = span.col.saturating_sub(1);
    if let Some(line_text) = line_at(&doc.text, line) {
        let (_, end) = find_word_range(&line_text, col);
        return Position {
            line: line as u32,
            character: end as u32,
        };
    }
    Position {
        line: line as u32,
        character: col as u32,
    }
}

fn binding_line_end_position(doc: &DocumentData, span: Span) -> Position {
    let line = span.line.saturating_sub(1);
    if let Some(line_text) = line_at(&doc.text, line) {
        let end = line_text.chars().count();
        return Position {
            line: line as u32,
            character: end as u32,
        };
    }
    symbol_end_position(doc, span)
}

fn make_type_inlay_hint(position: Position, ty: &str) -> InlayHint {
    InlayHint {
        position,
        label: format!(": {}", ty).into(),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

fn make_return_inlay_hint(position: Position, ty: &str) -> InlayHint {
    InlayHint {
        position,
        label: format!("-> {}", ty).into(),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

fn position_in_range(pos: Position, range: &Range) -> bool {
    let after_start = pos.line > range.start.line
        || (pos.line == range.start.line && pos.character >= range.start.character);
    let before_end = pos.line < range.end.line
        || (pos.line == range.end.line && pos.character <= range.end.character);
    after_start && before_end
}

fn merge_hover_sections(primary: String, extra: Option<&str>) -> String {
    if let Some(extra) = extra {
        format!("{}\n\n{}", primary, extra)
    } else {
        primary
    }
}

fn signature_help_from_doc(entry: &DocEntry, argument_index: usize) -> SignatureHelp {
    let label = entry
        .signature
        .clone()
        .unwrap_or_else(|| entry.name.clone());
    let params = signature_params_for(entry).unwrap_or_default();
    let (parameter_infos, active_parameter) = build_parameter_infos(&params, argument_index);
    let info = SignatureInformation {
        label,
        documentation: documentation_for_entry(entry),
        parameters: parameter_infos,
        active_parameter,
    };
    SignatureHelp {
        signatures: vec![info],
        active_signature: Some(0),
        active_parameter,
    }
}

fn signature_help_from_symbol(info: &SymbolInfo, argument_index: usize) -> SignatureHelp {
    let params = info.params.clone().unwrap_or_default();
    let (parameter_infos, active_parameter) = build_parameter_infos(&params, argument_index);
    let label = symbol_signature_label(info).unwrap_or_else(|| info.name.clone());
    let info = SignatureInformation {
        label,
        documentation: user_documentation(info),
        parameters: parameter_infos,
        active_parameter,
    };
    SignatureHelp {
        signatures: vec![info],
        active_signature: Some(0),
        active_parameter,
    }
}

fn signature_help_from_fnmeta(meta: &fn_meta::FnMeta, argument_index: usize) -> SignatureHelp {
    let mut signatures = Vec::new();
    let name = meta.fq_name();
    if meta.overloads.is_empty() {
        let label = fnmeta_signature_label(&name, meta).unwrap_or(name.clone());
        signatures.push(SignatureInformation {
            label,
            documentation: meta
                .doc
                .as_ref()
                .map(|doc| MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.clone(),
                })
                .map(Documentation::MarkupContent),
            parameters: None,
            active_parameter: None,
        });
    } else {
        for overload in &meta.overloads {
            let label = format_overload_signature(&name, overload);
            let params = params_from_overload(overload);
            let (parameter_infos, active_parameter) =
                build_parameter_infos(&params, argument_index);
            signatures.push(SignatureInformation {
                label,
                documentation: meta
                    .doc
                    .as_ref()
                    .map(|doc| MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.clone(),
                    })
                    .map(Documentation::MarkupContent),
                parameters: parameter_infos,
                active_parameter,
            });
        }
    }
    SignatureHelp {
        signatures,
        active_signature: Some(0),
        active_parameter: Some(argument_index as u32),
    }
}

fn fnmeta_signature_label(name: &str, meta: &fn_meta::FnMeta) -> Option<String> {
    if !meta.overloads.is_empty() {
        let mut lines = Vec::new();
        for overload in &meta.overloads {
            lines.push(format_overload_signature(name, overload));
        }
        return Some(lines.join("\n"));
    }
    meta.arglist
        .first()
        .map(|args| format!("{} {}", name, args))
}

fn format_overload_signature(name: &str, overload: &fn_meta::FnOverload) -> String {
    let mut parts: Vec<String> = overload.arg_types.iter().map(|ty| ty.describe()).collect();
    if let Some(rest) = &overload.rest {
        parts.push(format!("& {}", rest.describe()));
    }
    format!("{} [{}]", name, parts.join(" "))
}

fn params_from_overload(overload: &fn_meta::FnOverload) -> Vec<String> {
    let mut params: Vec<String> = overload.arg_types.iter().map(|ty| ty.describe()).collect();
    if let Some(rest) = &overload.rest {
        params.push(format!("& {}", rest.describe()));
    }
    params
}

fn build_parameter_infos(
    params: &[String],
    argument_index: usize,
) -> (Option<Vec<ParameterInformation>>, Option<u32>) {
    if params.is_empty() {
        (None, None)
    } else {
        let infos = params
            .iter()
            .map(|param| ParameterInformation {
                label: ParameterLabel::Simple(param.clone()),
                documentation: None,
            })
            .collect();
        let active_parameter = Some(argument_index.min(params.len().saturating_sub(1)) as u32);
        (Some(infos), active_parameter)
    }
}

#[derive(Clone, Copy)]
struct DiagnosticsOptions {
    enable_unbound: bool,
    enable_require: bool,
    enable_arity: bool,
    enable_refer_check: bool,
}

impl Default for DiagnosticsOptions {
    fn default() -> Self {
        Self {
            enable_unbound: true,
            enable_require: true,
            enable_arity: true,
            enable_refer_check: false,
        }
    }
}

fn compute_diagnostics(
    store: &DocumentStore,
    doc: &DocumentData,
    options: DiagnosticsOptions,
) -> Vec<Diagnostic> {
    let mut diagnostics = doc.parse_diagnostics.clone();
    diagnostics.extend(doc.type_diagnostics.clone());
    let forms = match &doc.ast {
        Some(forms) => forms,
        None => return diagnostics,
    };
    if options.enable_require {
        diagnostics.extend(require_diagnostics(store, doc, &options));
    }
    if options.enable_unbound || options.enable_arity {
        let mut analyzer = ReferenceAnalyzer::new(store, doc, options);
        analyzer.walk_forms(forms);
        diagnostics.extend(analyzer.into_diagnostics());
    }
    diagnostics
}

fn require_diagnostics(
    store: &DocumentStore,
    doc: &DocumentData,
    options: &DiagnosticsOptions,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for spec in &doc.requires {
        let target_ns = match require_target_namespace_for(spec, doc) {
            Some(ns) => ns,
            None => continue,
        };
        if matches!(spec.target, RequireTarget::FilePath(_)) {
            continue;
        }
        let mut known = false;
        for key in namespace_lookup_keys(&target_ns) {
            if store.ns_to_uris.contains_key(&key) {
                known = true;
                break;
            }
        }
        if !known {
            diagnostics.push(make_diagnostic(
                span_to_range(spec.span),
                DiagnosticSeverity::ERROR,
                format!("Unknown namespace: {}", target_ns),
            ));
            continue;
        }
        if options.enable_refer_check && !spec.refer_all {
            diagnostics.extend(missing_refer_warnings(store, &target_ns, spec));
        }
    }
    diagnostics
}

fn missing_refer_warnings(
    store: &DocumentStore,
    target_ns: &str,
    spec: &RequireSpec,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let available = namespace_symbol_names(store, target_ns);
    if available.is_empty() {
        return diagnostics;
    }
    for refer in &spec.refers {
        if !symbol_available(&available, refer) {
            diagnostics.push(make_diagnostic(
                span_to_range(spec.span),
                DiagnosticSeverity::WARNING,
                format!("Symbol {} not found in namespace {}", refer, target_ns),
            ));
        }
    }
    for orig in spec.rename.keys() {
        if !symbol_available(&available, orig) {
            diagnostics.push(make_diagnostic(
                span_to_range(spec.span),
                DiagnosticSeverity::WARNING,
                format!("Symbol {} not found in namespace {}", orig, target_ns),
            ));
        }
    }
    diagnostics
}

fn symbol_available(available: &HashSet<String>, name: &str) -> bool {
    if available.contains(name) {
        return true;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    available.contains(&canonical)
}

fn namespace_symbol_names(store: &DocumentStore, namespace: &str) -> HashSet<String> {
    let mut names = HashSet::new();
    if let Some(uris) = store.ns_to_uris.get(namespace) {
        for uri in uris {
            if let Some(doc) = store.docs.get(uri) {
                for info in doc.symbols.entries() {
                    if info.is_private {
                        continue;
                    }
                    let mut ok = info.namespace.as_deref() == Some(namespace);
                    if !ok && info.namespace_aliases.iter().any(|a| a == namespace) {
                        ok = true;
                    }
                    if ok {
                        names.insert(info.name.clone());
                        names.insert(canonical_symbol_name(&info.name).into_owned());
                    }
                }
            }
        }
    }
    names
}

struct ReferenceAnalyzer<'a> {
    store: &'a DocumentStore,
    doc: &'a DocumentData,
    options: DiagnosticsOptions,
    scopes: Vec<HashSet<String>>,
    where_depth: usize,
    diagnostics: Vec<Diagnostic>,
    resolution_cache: HashMap<(Option<String>, String), bool>,
}

impl<'a> ReferenceAnalyzer<'a> {
    fn new(store: &'a DocumentStore, doc: &'a DocumentData, options: DiagnosticsOptions) -> Self {
        Self {
            store,
            doc,
            options,
            scopes: Vec::new(),
            where_depth: 0,
            diagnostics: Vec::new(),
            resolution_cache: HashMap::new(),
        }
    }

    fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    fn walk_forms(&mut self, forms: &[Form]) {
        for form in forms {
            self.visit_form(form);
        }
    }

    fn visit_form(&mut self, form: &Form) {
        match &form.kind {
            FormKind::Symbol(sym) => self.check_symbol(sym, form.span),
            FormKind::List(items) => self.visit_list(items, form),
            FormKind::Vector(items) | FormKind::Set(items) => {
                for item in items {
                    self.visit_form(item);
                }
            }
            FormKind::Map(entries) => {
                for entry in entries {
                    match entry {
                        clove_core::ast::MapItem::KeyValue(k, v) => {
                            self.visit_form(k);
                            self.visit_form(v);
                        }
                        clove_core::ast::MapItem::Spread(f) => self.visit_form(f),
                    }
                }
            }
            FormKind::InterpolatedString(parts) => {
                for part in parts {
                    if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                        self.visit_form(expr);
                    }
                }
            }
            FormKind::InterpolatedRegex { parts, .. } => {
                for part in parts {
                    if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                        self.visit_form(expr);
                    }
                }
            }
            FormKind::ShortFn(_) => {}
            _ => {}
        }
    }

    fn visit_list(&mut self, items: &[Form], form: &Form) {
        if items.is_empty() {
            return;
        }
        if let Some(head) = head_symbol(items) {
            match head.as_str() {
                sym if sym == OOP_BARE_SYM
                    || sym == OOP_INDEX_SYM
                    || sym == OOP_SEG_SYM
                    || sym == MAP_REF_SYM =>
                {
                    return;
                }
                "quote" => return,
                "def" | "def-" => {
                    self.visit_def(head.as_str(), items);
                    return;
                }
                "defn" | "defn-" => {
                    self.visit_defn(head.as_str(), items);
                    return;
                }
                "-def" => {
                    self.visit_local_def(items);
                    return;
                }
                "-defn" => {
                    self.visit_defn(head.as_str(), items);
                    return;
                }
                "deftype" => {
                    self.visit_deftype(items);
                    return;
                }
                "defenum" => {
                    self.visit_defenum(items);
                    return;
                }
                "fn" | "fn*" => {
                    self.visit_fn(items);
                    return;
                }
                "let" | "loop" | "loop*" => {
                    self.visit_let(items);
                    return;
                }
                "when-let" | "if-let" | "if-some" => {
                    self.visit_if_let_like(items);
                    return;
                }
                "dotimes" => {
                    self.visit_dotimes(items);
                    return;
                }
                "for" => {
                    self.visit_for(items);
                    return;
                }
                "doseq" => {
                    self.visit_doseq(items);
                    return;
                }
                "catch" => {
                    self.visit_catch(items);
                    return;
                }
                "try" => {
                    self.visit_try(items);
                    return;
                }
                "if" | "do" | "recur" => {
                    for item in items.iter().skip(1) {
                        self.visit_form(item);
                    }
                    return;
                }
                "where" => {
                    self.where_depth += 1;
                    for item in items.iter().skip(1) {
                        self.visit_form(item);
                    }
                    self.where_depth -= 1;
                    return;
                }
                "match" => {
                    self.visit_match(items);
                    return;
                }
                "ns" | "require" => return,
                _ => {}
            }
        }
        self.visit_call(items, form);
    }

    fn visit_def(&mut self, head: &str, items: &[Form]) {
        if items.len() <= 2 {
            return;
        }
        if !self.scopes.is_empty() {
            let message = if head == "def" {
                "def is top-level only; use -def for local binding or set! to update an existing var"
            } else {
                "def- is top-level only"
            };
            self.diagnostics.push(make_diagnostic(
                span_to_range(items[0].span),
                DiagnosticSeverity::ERROR,
                message.to_string(),
            ));
            return;
        }
        if let Some(Form {
            kind: FormKind::Symbol(name),
            ..
        }) = items.get(1)
        {
            if !self.scopes.is_empty() {
                self.register_binding_in_current_scope(name);
            }
        }
        for item in items.iter().skip(2) {
            self.visit_form(item);
        }
    }

    fn visit_defn(&mut self, head: &str, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if !self.scopes.is_empty() && head != "-defn" {
            let allow_in_where = head == "defn" && self.where_depth > 0;
            if allow_in_where {
                // defn inside where is treated as local definition
            } else {
                let message = if head == "defn" {
                    "defn is top-level only; use -defn for local function"
                } else {
                    "defn- is top-level only"
                };
                self.diagnostics.push(make_diagnostic(
                    span_to_range(items[0].span),
                    DiagnosticSeverity::ERROR,
                    message.to_string(),
                ));
                return;
            }
        }
        if self.scopes.is_empty() && head == "-defn" {
            self.diagnostics.push(make_diagnostic(
                span_to_range(items[0].span),
                DiagnosticSeverity::ERROR,
                "-defn is local-only and must appear in a function body".to_string(),
            ));
            return;
        }
        let mut defn_name = None;
        if let Some(Form {
            kind: FormKind::Symbol(name),
            ..
        }) = items.get(1)
        {
            defn_name = Some(name.clone());
            if !self.scopes.is_empty() {
                self.register_binding_in_current_scope(name);
            }
        }
        let mut idx = 2;
        let (_, after_doc) = extract_docstring(items, idx, false);
        idx = after_doc;
        if matches!(
            items.get(idx),
            Some(Form {
                kind: FormKind::Vector(_),
                ..
            })
        ) {
            let params = &items[idx];
            let body = &items[idx + 1..];
            self.visit_fn_variant(params, body, defn_name.as_deref());
            return;
        }
        for form in items.iter().skip(idx) {
            if let FormKind::List(variants) = &form.kind {
                if let Some(Form {
                    kind: FormKind::Vector(_),
                    ..
                }) = variants.first()
                {
                    self.visit_fn_variant(&variants[0], &variants[1..], defn_name.as_deref());
                    continue;
                }
            }
            self.visit_form(form);
        }
    }

    fn visit_local_def(&mut self, items: &[Form]) {
        if items.len() <= 2 {
            return;
        }
        if self.scopes.is_empty() {
            self.diagnostics.push(make_diagnostic(
                span_to_range(items[0].span),
                DiagnosticSeverity::ERROR,
                "-def is local-only and must appear in a function body".to_string(),
            ));
            return;
        }
        if let Some(Form {
            kind: FormKind::Symbol(name),
            ..
        }) = items.get(1)
        {
            self.register_binding_in_current_scope(name);
        }
        for item in items.iter().skip(2) {
            self.visit_form(item);
        }
    }

    fn visit_deftype(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let Some(Form {
            kind: FormKind::Symbol(name),
            ..
        }) = items.get(1)
        {
            if !self.scopes.is_empty() {
                self.register_binding_in_current_scope(name);
            }
        }
        if self.scopes.is_empty() {
            return;
        }
        let mut idx = 2;
        while idx < items.len() {
            match items.get(idx).map(|form| &form.kind) {
                Some(FormKind::Keyword(name)) if name == "alias" => {
                    idx += 2;
                    continue;
                }
                Some(FormKind::Keyword(name)) if name == "from" => {
                    if let Some(Form {
                        kind: FormKind::Symbol(name),
                        ..
                    }) = items.get(idx + 1)
                    {
                        self.register_binding_in_current_scope(name);
                    }
                    return;
                }
                _ => break,
            }
        }
        if let Some(Form {
            kind: FormKind::List(list_items),
            ..
        }) = items.get(idx)
        {
            if matches!(
                list_items.first().map(|form| &form.kind),
                Some(FormKind::Symbol(sym)) if sym == "def"
            ) {
                if let Some(Form {
                    kind: FormKind::Symbol(name),
                    ..
                }) = list_items.get(1)
                {
                    self.register_binding_in_current_scope(name);
                }
            }
        }
    }

    fn visit_defenum(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let Some(Form {
            kind: FormKind::Symbol(name),
            ..
        }) = items.get(1)
        {
            if !self.scopes.is_empty() {
                self.register_binding_in_current_scope(name);
            }
        }
    }

    fn visit_fn(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        let mut idx = 1;
        if matches!(
            items.get(1),
            Some(Form {
                kind: FormKind::Symbol(_),
                ..
            })
        ) {
            idx = 2;
        }
        if matches!(
            items.get(idx),
            Some(Form {
                kind: FormKind::Vector(_),
                ..
            })
        ) {
            let params = &items[idx];
            let body = &items[idx + 1..];
            self.visit_fn_variant(params, body, None);
            return;
        }
        for form in items.iter().skip(idx) {
            if let FormKind::List(variants) = &form.kind {
                if let Some(Form {
                    kind: FormKind::Vector(_),
                    ..
                }) = variants.first()
                {
                    self.visit_fn_variant(&variants[0], &variants[1..], None);
                    continue;
                }
            }
            self.visit_form(form);
        }
    }

    fn visit_match(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        self.visit_form(&items[1]);
        let mut idx = 2;
        while idx + 1 < items.len() {
            let pattern = &items[idx];
            let value = &items[idx + 1];
            let mut bindings = HashSet::new();
            collect_pattern_bindings(pattern, &mut bindings);
            let has_bindings = !bindings.is_empty();
            if has_bindings {
                self.push_scope(bindings);
            }
            self.visit_form(value);
            if has_bindings {
                self.pop_scope();
            }
            idx += 2;
        }
    }

    fn visit_fn_variant(&mut self, params_form: &Form, body: &[Form], self_name: Option<&str>) {
        let mut bindings = HashSet::new();
        collect_pattern_bindings(params_form, &mut bindings);
        if let Some(name) = self_name {
            bindings.insert(canonical_symbol_name(name).into_owned());
        }
        collect_local_defn_bindings(body, &mut bindings);
        self.push_scope(bindings);
        for form in body {
            self.visit_form(form);
        }
        self.pop_scope();
    }

    fn visit_catch(&mut self, items: &[Form]) {
        if items.len() < 3 {
            return;
        }
        let (binding_idx, body_idx) = if items.len() >= 4 { (2, 3) } else { (1, 2) };
        if let Some(Form {
            kind: FormKind::Symbol(binding),
            ..
        }) = items.get(binding_idx)
        {
            let mut bindings = HashSet::new();
            bindings.insert(canonical_symbol_name(binding).into_owned());
            self.push_scope(bindings);
            for form in items.iter().skip(body_idx) {
                self.visit_form(form);
            }
            self.pop_scope();
        } else {
            for form in items.iter().skip(body_idx) {
                self.visit_form(form);
            }
        }
    }

    fn visit_let(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let FormKind::Vector(bindings) = &items[1].kind {
            let mut accumulated: HashSet<String> = HashSet::new();
            let mut idx = 0;
            while idx < bindings.len() {
                let binding = &bindings[idx];
                let value = bindings.get(idx + 1);
                if !accumulated.is_empty() {
                    self.push_scope(accumulated.clone());
                    if let Some(val) = value {
                        self.visit_form(val);
                    }
                    self.pop_scope();
                } else if let Some(val) = value {
                    self.visit_form(val);
                }
                collect_pattern_bindings(binding, &mut accumulated);
                idx += 2;
            }
            if !accumulated.is_empty() {
                self.push_scope(accumulated);
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
                self.pop_scope();
            } else {
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
            }
        } else {
            for form in items.iter().skip(1) {
                self.visit_form(form);
            }
        }
    }

    fn visit_try(&mut self, items: &[Form]) {
        if is_short_try_binding_form(items) {
            self.visit_let(items);
            return;
        }
        for item in items.iter().skip(1) {
            self.visit_form(item);
        }
    }

    fn visit_if_let_like(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let FormKind::Vector(bindings) = &items[1].kind {
            let mut iter = bindings.iter();
            if let (Some(binding), Some(value)) = (iter.next(), iter.next()) {
                self.visit_form(value);
                let mut new_bindings = HashSet::new();
                collect_pattern_bindings(binding, &mut new_bindings);
                if !new_bindings.is_empty() {
                    self.push_scope(new_bindings);
                    for form in items.iter().skip(2) {
                        self.visit_form(form);
                    }
                    self.pop_scope();
                    return;
                }
            }
        }
        for form in items.iter().skip(1) {
            self.visit_form(form);
        }
    }

    fn visit_dotimes(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let FormKind::Vector(bindings) = &items[1].kind {
            if bindings.len() >= 2 {
                let name_form = &bindings[0];
                let count_form = &bindings[1];
                self.visit_form(count_form);
                let mut new_bindings = HashSet::new();
                collect_pattern_bindings(name_form, &mut new_bindings);
                if !new_bindings.is_empty() {
                    self.push_scope(new_bindings);
                    for form in items.iter().skip(2) {
                        self.visit_form(form);
                    }
                    self.pop_scope();
                    return;
                }
            }
        }
        for form in items.iter().skip(1) {
            self.visit_form(form);
        }
    }

    fn visit_for(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let FormKind::Vector(bindings) = &items[1].kind {
            let mut accumulated: HashSet<String> = HashSet::new();
            let mut idx = 0;
            while idx < bindings.len() {
                let binding = &bindings[idx];
                match &binding.kind {
                    FormKind::Keyword(kw) if kw == "let" => {
                        if let Some(Form {
                            kind: FormKind::Vector(inner),
                            ..
                        }) = bindings.get(idx + 1)
                        {
                            let mut temp = HashSet::new();
                            let mut j = 0;
                            while j + 1 < inner.len() {
                                let pat = &inner[j];
                                let val = &inner[j + 1];
                                if !accumulated.is_empty() {
                                    self.push_scope(accumulated.clone());
                                    self.visit_form(val);
                                    self.pop_scope();
                                } else {
                                    self.visit_form(val);
                                }
                                collect_pattern_bindings(pat, &mut temp);
                                j += 2;
                            }
                            accumulated.extend(temp);
                            idx += 2;
                            continue;
                        }
                    }
                    FormKind::Keyword(kw) if kw == "when" || kw == "while" => {
                        if let Some(test) = bindings.get(idx + 1) {
                            if !accumulated.is_empty() {
                                self.push_scope(accumulated.clone());
                                self.visit_form(test);
                                self.pop_scope();
                            } else {
                                self.visit_form(test);
                            }
                        }
                        idx += 2;
                        continue;
                    }
                    _ => {
                        if let Some(coll) = bindings.get(idx + 1) {
                            if !accumulated.is_empty() {
                                self.push_scope(accumulated.clone());
                                self.visit_form(coll);
                                self.pop_scope();
                            } else {
                                self.visit_form(coll);
                            }
                        }
                        collect_pattern_bindings(binding, &mut accumulated);
                        idx += 2;
                        continue;
                    }
                }
                idx += 1;
            }
            if !accumulated.is_empty() {
                self.push_scope(accumulated);
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
                self.pop_scope();
            } else {
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
            }
        } else {
            for form in items.iter().skip(1) {
                self.visit_form(form);
            }
        }
    }

    fn visit_doseq(&mut self, items: &[Form]) {
        if items.len() < 2 {
            return;
        }
        if let FormKind::Vector(bindings) = &items[1].kind {
            let mut accumulated: HashSet<String> = HashSet::new();
            let mut idx = 0;
            while idx + 1 < bindings.len() {
                let binding = &bindings[idx];
                let value = &bindings[idx + 1];
                // doseq evaluates later collection forms using earlier bindings, so we stack scopes
                if !accumulated.is_empty() {
                    self.push_scope(accumulated.clone());
                    self.visit_form(value);
                    self.pop_scope();
                } else {
                    self.visit_form(value);
                }
                collect_pattern_bindings(binding, &mut accumulated);
                idx += 2;
            }
            if !accumulated.is_empty() {
                self.push_scope(accumulated);
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
                self.pop_scope();
            } else {
                for form in items.iter().skip(2) {
                    self.visit_form(form);
                }
            }
        } else {
            for form in items.iter().skip(1) {
                self.visit_form(form);
            }
        }
    }

    fn visit_call(&mut self, items: &[Form], form: &Form) {
        if form_contains_placeholder(form) || form_contains_spread(form) {
            return;
        }
        if let Some(head) = head_symbol(items) {
            self.check_symbol(&head, items[0].span);
            if !is_special_form(&head) {
                let args = &items[1..];
                self.check_arity(&head, args, form);
            }
        } else {
            self.visit_form(&items[0]);
        }
        for arg in items.iter().skip(1) {
            self.visit_form(arg);
        }
    }

    fn check_symbol(&mut self, sym: &str, span: Span) {
        if !self.options.enable_unbound {
            return;
        }
        let canonical = canonical_symbol_name(sym);
        let sym = normalize_infix_symbol(canonical.as_ref());
        if sym == "_" {
            return;
        }
        if is_label_style_key(sym) {
            return;
        }
        if is_placeholder_symbol(sym) || is_special_form(sym) || sym.starts_with(':') {
            return;
        }
        if is_primitive_type_literal(sym) {
            return;
        }
        if self.is_locally_bound(sym) {
            return;
        }
        if self.is_resolved(sym) {
            return;
        }
        self.diagnostics.push(make_diagnostic(
            span_to_range(span),
            DiagnosticSeverity::ERROR,
            format!("Unbound symbol: {}", sym),
        ));
    }

    fn check_arity(&mut self, head: &str, args: &[Form], call_form: &Form) {
        if !self.options.enable_arity {
            return;
        }
        if args.iter().any(form_contains_placeholder) || args.iter().any(form_contains_spread) {
            return;
        }
        if is_placeholder_symbol(head) {
            return;
        }
        let lookup_head = normalize_infix_symbol(head);
        let arg_count = args.len();
        let candidates = resolve_symbol_candidates(lookup_head, self.doc);
        if candidates.is_empty()
            && self.doc.namespace.is_none()
            && self.doc.namespace_aliases.is_empty()
            && self.doc.requires.is_empty()
        {
            return;
        }
        for (ns, lookup) in candidates {
            let mut infos = self.store.find_symbol_infos(&lookup, ns.as_deref());
            if infos.is_empty() {
                let canon = canonical_symbol_name(&lookup).into_owned();
                if canon != lookup {
                    infos = self.store.find_symbol_infos(&canon, ns.as_deref());
                }
            }
            for info in infos {
                if info.arities.is_empty() {
                    continue;
                }
                if info.arities.iter().any(|a| *a == arg_count) {
                    return;
                }
                let expected = format_expected_arities(&info.arities);
                self.diagnostics.push(make_diagnostic(
                    span_to_range(call_form.span),
                    DiagnosticSeverity::ERROR,
                    format!(
                        "Arity mismatch: {} expects {} but got {}",
                        head, expected, arg_count
                    ),
                ));
                return;
            }
        }
    }

    fn is_locally_bound(&self, sym: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains(sym) {
                return true;
            }
        }
        false
    }

    fn is_resolved(&mut self, sym: &str) -> bool {
        if find_doc(sym).is_some() {
            return true;
        }
        let candidates = resolve_symbol_candidates(sym, self.doc);
        let mut has_unknown_ns_candidate = false;
        if candidates.is_empty()
            && self.doc.namespace.is_none()
            && self.doc.namespace_aliases.is_empty()
            && self.doc.requires.is_empty()
        {
            return true;
        }
        for (ns, lookup) in candidates {
            if let Some(ns_ref) = ns.as_deref() {
                if !namespace_known(self.store, ns_ref) {
                    has_unknown_ns_candidate = true;
                    continue;
                }
            }
            if self.lookup_resolution(ns.clone(), lookup.clone()) {
                return true;
            }
        }
        if has_unknown_ns_candidate {
            return true;
        }
        false
    }

    fn lookup_resolution(&mut self, ns: Option<String>, lookup: String) -> bool {
        let key = (ns.clone(), lookup.clone());
        if let Some(resolved) = self.resolution_cache.get(&key) {
            return *resolved;
        }
        let mut resolved = false;
        let current_ns = self.doc.namespace.as_deref();
        if let Some(ns_ref) = ns.as_deref() {
            let infos = self.store.find_symbol_infos(&lookup, Some(ns_ref));
            if infos
                .iter()
                .any(|info| !info.is_private || current_ns == Some(ns_ref))
            {
                resolved = true;
            } else {
                let canon = canonical_symbol_name(&lookup).into_owned();
                if canon != lookup {
                    let infos = self.store.find_symbol_infos(&canon, Some(ns_ref));
                    if infos
                        .iter()
                        .any(|info| !info.is_private || current_ns == Some(ns_ref))
                    {
                        resolved = true;
                    }
                }
            }
        } else {
            let infos = self.store.find_symbol_infos(&lookup, None);
            if infos
                .iter()
                .any(|info| !info.is_private || info.namespace.as_deref() == current_ns)
            {
                resolved = true;
            } else {
                let canon = canonical_symbol_name(&lookup).into_owned();
                if canon != lookup {
                    let infos = self.store.find_symbol_infos(&canon, None);
                    if infos
                        .iter()
                        .any(|info| !info.is_private || info.namespace.as_deref() == current_ns)
                    {
                        resolved = true;
                    }
                }
            }
        }
        self.resolution_cache.insert(key, resolved);
        resolved
    }

    fn push_scope(&mut self, bindings: HashSet<String>) {
        self.scopes.push(bindings);
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn register_binding_in_current_scope(&mut self, sym: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(canonical_symbol_name(sym).into_owned());
        }
    }
}

fn is_special_form(sym: &str) -> bool {
    matches!(
        sym,
        "if" | "let"
            | "fn"
            | "fn*"
            | "def"
            | "def-"
            | "defn"
            | "defn-"
            | "-def"
            | "-defn"
            | "deftype"
            | "defenum"
            | "quote"
            | "do"
            | "where"
            | "try"
            | "catch"
            | "recur"
            | "loop"
            | "loop*"
            | "when-let"
            | "if-let"
            | "if-some"
            | "for"
            | "dotimes"
            | "doseq"
            | "require"
            | "ns"
            | "finally"
            | "match"
            | "set!"
            | "redef"
            | INDEX_GET_SYM
            | INDEX_GET_IN_SYM
            | OOP_SYNTAX_SYM
            | OOP_DOT_STAGE_SYM
            | OOP_INDEX_SYM
            | OOP_BARE_SYM
            | OOP_SEG_SYM
            | MAP_REF_SYM
            | INFIX_SYNTAX_SYM
    )
}

fn normalize_infix_symbol(sym: &str) -> &str {
    match sym {
        "==" => "=",
        "!=" => "not=",
        "&&" => "and",
        "||" => "or",
        "!" => "not",
        "%" => "mod",
        _ => sym,
    }
}

fn is_label_style_key(sym: &str) -> bool {
    let bytes = sym.as_bytes();
    if bytes.len() < 2 {
        return false;
    }
    if bytes[bytes.len() - 1] != b':' {
        return false;
    }
    let body = &bytes[..bytes.len() - 1];
    if body.is_empty() {
        return false;
    }
    let first = body[0] as char;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    for b in &body[1..] {
        let c = *b as char;
        if !(c.is_ascii_alphanumeric() || c == '_' || c == '-') {
            return false;
        }
    }
    true
}

fn is_numbered_placeholder(sym: &str, prefix: &str) -> bool {
    let rest = match sym.strip_prefix(prefix) {
        Some(rest) if !rest.is_empty() => rest,
        _ => return false,
    };
    if !rest.chars().all(|ch| ch.is_ascii_digit()) {
        return false;
    }
    rest.parse::<usize>().map(|n| n > 0).unwrap_or(false)
}

fn is_placeholder_symbol(sym: &str) -> bool {
    sym == "?"
        || sym == "*?"
        || sym.starts_with('?')
        || sym.starts_with("*?")
        || is_numbered_placeholder(sym, "?")
        || is_numbered_placeholder(sym, "*?")
        || sym.starts_with(DOT_CHAIN_PLACEHOLDER_PREFIX)
}

fn is_primitive_type_literal(sym: &str) -> bool {
    let sym = sym
        .strip_prefix("clove::core::")
        .or_else(|| sym.strip_prefix("core::"))
        .unwrap_or(sym);
    matches!(
        sym,
        "Int"
            | "Integer"
            | "Float"
            | "Bool"
            | "Str"
            | "String"
            | "Nil"
            | "Any"
            | "Keyword"
            | "Kw"
            | "List"
            | "Vector"
            | "Vec"
            | "Seq"
            | "Map"
            | "Set"
            | "Symbol"
            | "Sym"
            | "Regex"
            | "Re"
            | "Duration"
            | "Function"
            | "Atom"
            | "Chan"
            | "Promise"
            | "Task"
            | "Future"
            | "Agent"
            | "Foreign"
    )
}

fn form_contains_placeholder(form: &Form) -> bool {
    match &form.kind {
        FormKind::Symbol(sym) => is_placeholder_symbol(sym),
        FormKind::ShortFn(_) => true,
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            items.iter().any(form_contains_placeholder)
        }
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => {
                form_contains_placeholder(k) || form_contains_placeholder(v)
            }
            clove_core::ast::MapItem::Spread(f) => form_contains_placeholder(f),
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| {
            if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                form_contains_placeholder(expr)
            } else {
                false
            }
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| {
            if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                form_contains_placeholder(expr)
            } else {
                false
            }
        }),
        _ => false,
    }
}

fn form_contains_spread(form: &Form) -> bool {
    match &form.kind {
        FormKind::Symbol(sym) => sym.starts_with('*') && sym.len() > 1,
        FormKind::ShortFn(_) => false,
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            items.iter().any(form_contains_spread)
        }
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => {
                form_contains_spread(k) || form_contains_spread(v)
            }
            clove_core::ast::MapItem::Spread(_) => true,
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| {
            if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                form_contains_spread(expr)
            } else {
                false
            }
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| {
            if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                form_contains_spread(expr)
            } else {
                false
            }
        }),
        _ => false,
    }
}

fn collect_pattern_bindings(form: &Form, out: &mut HashSet<String>) {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if let Some(name) = binding_symbol_name(sym) {
                out.insert(name);
            }
        }
        FormKind::Vector(items) => add_vector_bindings(items, out),
        FormKind::Map(entries) => add_map_bindings(entries, out),
        FormKind::List(items) | FormKind::Set(items) => {
            for item in items {
                collect_pattern_bindings(item, out);
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_pattern_bindings(expr, out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_pattern_bindings(expr, out);
                }
            }
        }
        _ => {}
    }
}

fn collect_local_defn_bindings(body: &[Form], out: &mut HashSet<String>) {
    for form in body {
        collect_local_defn_bindings_from_form(form, false, out);
    }
}

fn collect_local_defn_bindings_from_form(form: &Form, allow_defn: bool, out: &mut HashSet<String>) {
    let FormKind::List(items) = &form.kind else {
        return;
    };
    let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) else {
        return;
    };
    match head.as_str() {
        "defn" if allow_defn => {
            if let Some(Form {
                kind: FormKind::Symbol(name),
                ..
            }) = items.get(1)
            {
                out.insert(canonical_symbol_name(name).into_owned());
            }
        }
        "-defn" => {
            if let Some(Form {
                kind: FormKind::Symbol(name),
                ..
            }) = items.get(1)
            {
                out.insert(canonical_symbol_name(name).into_owned());
            }
        }
        "do" | "where" => {
            let allow_nested_defn = head == "where" || allow_defn;
            for item in items.iter().skip(1) {
                collect_local_defn_bindings_from_form(item, allow_nested_defn, out);
            }
        }
        _ => {}
    }
}

fn binding_symbol_name(sym: &str) -> Option<String> {
    let canonical = canonical_symbol_name(sym);
    let name = canonical.as_ref();
    if is_discard_binding(name) {
        None
    } else {
        Some(name.to_string())
    }
}

fn add_vector_bindings(items: &[Form], out: &mut HashSet<String>) {
    let mut expect_rest = false;
    let mut expect_as = false;
    for item in items {
        match &item.kind {
            FormKind::Symbol(sym) if sym == "&" => {
                expect_rest = true;
                continue;
            }
            FormKind::Symbol(sym) if sym == ":as" => {
                expect_as = true;
                continue;
            }
            _ => {}
        }
        if expect_rest || expect_as {
            if let FormKind::Symbol(sym) = &item.kind {
                if let Some(name) = binding_symbol_name(sym) {
                    out.insert(name);
                }
            }
            expect_rest = false;
            expect_as = false;
            continue;
        }
        collect_pattern_bindings(item, out);
    }
}

fn add_map_bindings(entries: &[clove_core::ast::MapItem], out: &mut HashSet<String>) {
    for entry in entries {
        match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => match &k.kind {
                FormKind::Keyword(kw) if kw == "as" || kw == ":as" => {
                    if let FormKind::Symbol(sym) = &v.kind {
                        if let Some(name) = binding_symbol_name(sym) {
                            out.insert(name);
                        }
                    }
                }
                FormKind::Keyword(kw) if kw == "keys" || kw == ":keys" => {
                    if let FormKind::Vector(items) = &v.kind {
                        for item in items {
                            if let FormKind::Symbol(sym) = &item.kind {
                                if let Some(name) = binding_symbol_name(sym) {
                                    out.insert(name);
                                }
                            }
                        }
                    }
                }
                FormKind::Keyword(kw)
                    if kw == "syms" || kw == ":syms" || kw == "strs" || kw == ":strs" =>
                {
                    if let FormKind::Vector(items) = &v.kind {
                        for item in items {
                            if let FormKind::Symbol(sym) = &item.kind {
                                if let Some(name) = binding_symbol_name(sym) {
                                    out.insert(name);
                                }
                            }
                        }
                    }
                }
                _ => {
                    collect_pattern_bindings(v, out);
                }
            },
            clove_core::ast::MapItem::Spread(inner) => collect_pattern_bindings(inner, out),
        }
    }
}

fn is_discard_binding(sym: &str) -> bool {
    sym == "_" || sym == "&" || sym == ":as"
}

fn head_symbol(items: &[Form]) -> Option<String> {
    match items.first() {
        Some(Form {
            kind: FormKind::Symbol(sym),
            ..
        }) => Some(sym.clone()),
        _ => None,
    }
}

fn is_short_try_binding_form(items: &[Form]) -> bool {
    if items.len() != 5 {
        return false;
    }
    if !matches!(
        items.get(1),
        Some(Form {
            kind: FormKind::Vector(_),
            ..
        })
    ) {
        return false;
    }
    for form in items.iter().skip(2) {
        if let FormKind::List(inner) = &form.kind {
            if let Some(FormKind::Symbol(tag)) = inner.first().map(|f| &f.kind) {
                if tag == "catch" || tag == "finally" {
                    return false;
                }
            }
        }
    }
    true
}

fn format_expected_arities(arities: &[usize]) -> String {
    let mut sorted = arities.to_vec();
    sorted.sort();
    sorted.dedup();
    match sorted.len() {
        0 => "?".to_string(),
        1 => sorted[0].to_string(),
        _ => sorted
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(" or "),
    }
}

fn code_actions_for_unbound(
    params: &CodeActionParams,
    doc: &DocumentData,
    store: &DocumentStore,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();
    let mut seen = HashSet::new();
    for diagnostic in &params.context.diagnostics {
        let Some(sym) = unbound_symbol_from_message(&diagnostic.message) else {
            continue;
        };
        for target_ns in require_candidates_for_symbol(store, doc, sym) {
            let key = format!("{}::{}", target_ns, sym);
            if !seen.insert(key.clone()) {
                continue;
            }
            if let Some(edit) = build_require_edit(doc, &target_ns, sym) {
                let mut changes = HashMap::new();
                changes.insert(params.text_document.uri.clone(), vec![edit]);
                actions.push(CodeAction {
                    title: format!("Add require for {} from {}", sym, target_ns),
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diagnostic.clone()]),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..WorkspaceEdit::default()
                    }),
                    ..CodeAction::default()
                });
            }
        }
    }
    actions
}

fn code_actions_for_rewrite_access(
    params: &CodeActionParams,
    doc: &DocumentData,
    prefs: RefactorPrefs,
) -> Vec<CodeAction> {
    let Some(forms) = doc.ast.as_ref() else {
        return Vec::new();
    };
    let targets = target_forms_for_code_action(forms, &doc.text, &params.range);
    let mut edits = Vec::new();
    for form in targets {
        let Some(outcome) = rewrite_shortest_access(form, &doc.text, prefs) else {
            continue;
        };
        if !outcome.changed {
            continue;
        }
        let Some(range) = form_range(&doc.text, form) else {
            continue;
        };
        edits.push(TextEdit {
            range,
            new_text: outcome.text,
        });
    }
    if edits.is_empty() {
        return Vec::new();
    }
    let mut changes = HashMap::new();
    changes.insert(params.text_document.uri.clone(), edits);
    vec![CodeAction {
        title: "Refactor: Rewrite to shortest access syntax".to_string(),
        kind: Some(CodeActionKind::from(
            "refactor.rewrite.clove.shortestAccess",
        )),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        ..CodeAction::default()
    }]
}

fn code_actions_for_rewrite_canonical_access(
    params: &CodeActionParams,
    doc: &DocumentData,
) -> Vec<CodeAction> {
    let Some(forms) = doc.ast.as_ref() else {
        return Vec::new();
    };
    let targets = target_forms_for_code_action(forms, &doc.text, &params.range);
    let mut edits = Vec::new();
    for form in targets {
        let Some(outcome) = rewrite_canonical_access(form, &doc.text) else {
            continue;
        };
        if !outcome.changed {
            continue;
        }
        let Some(range) = form_range(&doc.text, form) else {
            continue;
        };
        edits.push(TextEdit {
            range,
            new_text: outcome.text,
        });
    }
    if edits.is_empty() {
        return Vec::new();
    }
    let mut changes = HashMap::new();
    changes.insert(params.text_document.uri.clone(), edits);
    vec![CodeAction {
        title: "Refactor: Rewrite to canonical access syntax".to_string(),
        kind: Some(CodeActionKind::from(
            "refactor.rewrite.clove.canonicalAccess",
        )),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        ..CodeAction::default()
    }]
}

fn code_actions_for_toggle_access(
    params: &CodeActionParams,
    doc: &DocumentData,
    prefs: RefactorPrefs,
) -> Vec<CodeAction> {
    let Some(forms) = doc.ast.as_ref() else {
        return Vec::new();
    };
    let targets = target_forms_for_code_action(forms, &doc.text, &params.range);
    let mut edits = Vec::new();
    for form in targets {
        let Some(outcome) = toggle_access_syntax(form, &doc.text, prefs) else {
            continue;
        };
        if !outcome.changed {
            continue;
        }
        let Some(range) = form_range(&doc.text, form) else {
            continue;
        };
        edits.push(TextEdit {
            range,
            new_text: outcome.text,
        });
    }
    if edits.is_empty() {
        return Vec::new();
    }
    let mut changes = HashMap::new();
    changes.insert(params.text_document.uri.clone(), edits);
    vec![CodeAction {
        title: "Refactor: Toggle access syntax".to_string(),
        kind: Some(CodeActionKind::from("refactor.rewrite.clove.toggleAccess")),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        ..CodeAction::default()
    }]
}

fn code_actions_for_toggle_oop(params: &CodeActionParams, doc: &DocumentData) -> Vec<CodeAction> {
    let Some(forms) = doc.ast.as_ref() else {
        return Vec::new();
    };
    let targets = target_forms_for_code_action(forms, &doc.text, &params.range);
    let mut edits = Vec::new();
    for form in targets {
        let Some(new_text) = toggle_oop_form(form, &doc.text, doc) else {
            continue;
        };
        let Some(range) = form_range(&doc.text, form) else {
            continue;
        };
        edits.push(TextEdit { range, new_text });
    }
    if edits.is_empty() {
        return Vec::new();
    }
    let mut changes = HashMap::new();
    changes.insert(params.text_document.uri.clone(), edits);
    vec![CodeAction {
        title: "Refactor: Toggle S-exp / OOP (safe)".to_string(),
        kind: Some(CodeActionKind::from("refactor.rewrite.clove.toggleOop")),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        ..CodeAction::default()
    }]
}

fn target_forms_for_code_action<'a>(forms: &'a [Form], text: &str, range: &Range) -> Vec<&'a Form> {
    if range_is_empty(range) {
        let cursor = position_to_offset(text, range.start);
        let mut best: Option<(usize, &Form)> = None;
        for form in forms {
            find_innermost_compound_form_at_offset(form, text, cursor, &mut best);
        }
        if let Some((_, form)) = best {
            return vec![form];
        }
        let mut fallback: Option<(usize, &Form)> = None;
        for form in forms {
            find_innermost_form_at_offset(form, text, cursor, &mut fallback);
        }
        return fallback.map(|(_, form)| vec![form]).unwrap_or_default();
    }
    let mut sel_start = position_to_offset(text, range.start);
    let mut sel_end = position_to_offset(text, range.end);
    if sel_start > sel_end {
        std::mem::swap(&mut sel_start, &mut sel_end);
    }
    let mut out = Vec::new();
    for form in forms {
        collect_forms_in_selection(form, text, sel_start, sel_end, false, &mut out);
    }
    if out.is_empty() || out.iter().all(|form| !form_has_children(form)) {
        let cursor = position_to_offset(text, range.start);
        let mut best: Option<(usize, &Form)> = None;
        for form in forms {
            find_innermost_compound_form_at_offset(form, text, cursor, &mut best);
        }
        if let Some((_, form)) = best {
            return vec![form];
        }
        let mut fallback: Option<(usize, &Form)> = None;
        for form in forms {
            find_innermost_form_at_offset(form, text, cursor, &mut fallback);
        }
        return fallback.map(|(_, form)| vec![form]).unwrap_or_default();
    }
    out
}

fn collect_forms_in_selection<'a>(
    form: &'a Form,
    text: &str,
    sel_start: usize,
    sel_end: usize,
    parent_selected: bool,
    out: &mut Vec<&'a Form>,
) {
    let Some((start, end)) = form_bounds(text, form) else {
        return;
    };
    if start >= sel_start && end <= sel_end {
        if !parent_selected {
            out.push(form);
        }
        return;
    }
    if end <= sel_start || start >= sel_end {
        return;
    }
    for_each_form_child(form, |child| {
        collect_forms_in_selection(child, text, sel_start, sel_end, false, out);
    });
}

fn find_innermost_form_at_offset<'a>(
    form: &'a Form,
    text: &str,
    offset: usize,
    best: &mut Option<(usize, &'a Form)>,
) {
    let Some((start, end)) = form_bounds(text, form) else {
        return;
    };
    if offset < start || offset >= end {
        return;
    }
    let len = end.saturating_sub(start);
    if best
        .as_ref()
        .map(|(best_len, _)| len < *best_len)
        .unwrap_or(true)
    {
        *best = Some((len, form));
    }
    for_each_form_child(form, |child| {
        find_innermost_form_at_offset(child, text, offset, best);
    });
}

fn find_innermost_compound_form_at_offset<'a>(
    form: &'a Form,
    text: &str,
    offset: usize,
    best: &mut Option<(usize, &'a Form)>,
) {
    let Some((start, end)) = form_bounds(text, form) else {
        return;
    };
    if offset < start || offset >= end {
        return;
    }
    if form_has_children(form) {
        let len = end.saturating_sub(start);
        if best
            .as_ref()
            .map(|(best_len, _)| len < *best_len)
            .unwrap_or(true)
        {
            *best = Some((len, form));
        }
    }
    for_each_form_child(form, |child| {
        find_innermost_compound_form_at_offset(child, text, offset, best);
    });
}

fn for_each_form_child<'a>(form: &'a Form, mut f: impl FnMut(&'a Form)) {
    match &form.kind {
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            for item in items {
                f(item);
            }
        }
        FormKind::ShortFn(items) => {
            for item in items {
                f(item);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        f(k);
                        f(v);
                    }
                    clove_core::ast::MapItem::Spread(expr) => {
                        f(expr);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    f(expr);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    f(expr);
                }
            }
        }
        _ => {}
    }
}

fn form_has_children(form: &Form) -> bool {
    matches!(
        form.kind,
        FormKind::List(_)
            | FormKind::Vector(_)
            | FormKind::Set(_)
            | FormKind::ShortFn(_)
            | FormKind::Map(_)
            | FormKind::InterpolatedString(_)
            | FormKind::InterpolatedRegex { .. }
    )
}

#[derive(Clone, Debug)]
struct RewriteOutcome {
    text: String,
    changed: bool,
}

struct ChildReplacement {
    start: usize,
    end: usize,
    text: String,
}

fn rewrite_shortest_access(
    form: &Form,
    text: &str,
    prefs: RefactorPrefs,
) -> Option<RewriteOutcome> {
    if let Some(outcome) = rewrite_access_match(form, text, prefs) {
        return Some(outcome);
    }
    let mut replacements = Vec::new();
    for_each_form_child(form, |child| {
        if let Some(child_outcome) = rewrite_shortest_access(child, text, prefs) {
            if child_outcome.changed {
                if let Some((start, end)) = form_bounds(text, child) {
                    replacements.push(ChildReplacement {
                        start,
                        end,
                        text: child_outcome.text,
                    });
                }
            }
        }
    });
    if replacements.is_empty() {
        let original = if form_has_children(form) {
            form_source(text, form)?
        } else {
            form_source_without_suffixes(text, form).or_else(|| form_source(text, form))?
        };
        return Some(RewriteOutcome {
            text: original,
            changed: false,
        });
    }
    let (parent_start, parent_end) = form_bounds(text, form)?;
    let mut out = text[parent_start..parent_end].to_string();
    replacements.sort_by(|a, b| b.start.cmp(&a.start));
    for rep in replacements {
        let rel_start = rep.start.saturating_sub(parent_start);
        let rel_end = rep.end.saturating_sub(parent_start);
        if rel_start > rel_end || rel_end > out.len() {
            return None;
        }
        out.replace_range(rel_start..rel_end, &rep.text);
    }
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn rewrite_access_match(form: &Form, text: &str, prefs: RefactorPrefs) -> Option<RewriteOutcome> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return None;
    }
    match &items[0].kind {
        FormKind::Symbol(sym) => match sym.as_str() {
            "get" | INDEX_GET_SYM => rewrite_get_like(items, text, prefs),
            "get-in" | INDEX_GET_IN_SYM => rewrite_get_in(items, text, prefs),
            "nth" => rewrite_nth(items, text, prefs),
            _ => None,
        },
        FormKind::Keyword(_) => rewrite_keyword_call(items, text, prefs),
        _ => None,
    }
}

fn rewrite_get_like(items: &[Form], text: &str, prefs: RefactorPrefs) -> Option<RewriteOutcome> {
    if items.len() != 3 && items.len() != 4 {
        return None;
    }
    let target = rewrite_shortest_access(&items[1], text, prefs)?;
    if items.len() == 3 && prefs.prefer_kw_chain {
        if let Some(seg) = keyword_segment(&items[2]) {
            let out = build_kw_chain(&target.text, &[seg.to_string()]);
            return Some(RewriteOutcome {
                text: out,
                changed: true,
            });
        }
    }
    let key = rewrite_shortest_access(&items[2], text, prefs)?;
    let out = if items.len() == 4 {
        let default = rewrite_shortest_access(&items[3], text, prefs)?;
        format!("{}[{} || {}]", target.text, key.text, default.text)
    } else {
        format!("{}[{}]", target.text, key.text)
    };
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn rewrite_get_in(items: &[Form], text: &str, prefs: RefactorPrefs) -> Option<RewriteOutcome> {
    if items.len() != 3 && items.len() != 4 {
        return None;
    }
    if !matches!(items.get(2).map(|f| &f.kind), Some(FormKind::Vector(_))) {
        return None;
    }
    let target = rewrite_shortest_access(&items[1], text, prefs)?;
    if items.len() == 3 && prefs.prefer_kw_chain {
        if let Some(segments) = keyword_path_segments(&items[2]) {
            let out = build_kw_chain(&target.text, &segments);
            return Some(RewriteOutcome {
                text: out,
                changed: true,
            });
        }
    }
    let path_src = form_source(text, &items[2])?;
    let path_inner = strip_vector_brackets(&path_src)?;
    let out = if items.len() == 4 {
        let default = rewrite_shortest_access(&items[3], text, prefs)?;
        format!("{}[{} || {}]", target.text, path_inner, default.text)
    } else {
        format!("{}[{}]", target.text, path_inner)
    };
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn rewrite_nth(items: &[Form], text: &str, prefs: RefactorPrefs) -> Option<RewriteOutcome> {
    if items.len() != 3 && items.len() != 4 {
        return None;
    }
    let target = rewrite_shortest_access(&items[1], text, prefs)?;
    let index = rewrite_shortest_access(&items[2], text, prefs)?;
    let out = if items.len() == 4 {
        let default = rewrite_shortest_access(&items[3], text, prefs)?;
        format!("{}[{} || {}]", target.text, index.text, default.text)
    } else {
        format!("{}[{}]", target.text, index.text)
    };
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn rewrite_keyword_call(
    items: &[Form],
    text: &str,
    prefs: RefactorPrefs,
) -> Option<RewriteOutcome> {
    if items.len() != 2 {
        return None;
    }
    let target = rewrite_shortest_access(&items[1], text, prefs)?;
    if prefs.prefer_kw_chain {
        if let Some(seg) = keyword_segment(&items[0]) {
            let out = build_kw_chain(&target.text, &[seg.to_string()]);
            return Some(RewriteOutcome {
                text: out,
                changed: true,
            });
        }
    }
    let key = form_source(text, &items[0])?;
    let out = format!("{}[{}]", target.text, key);
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn strip_vector_brackets(src: &str) -> Option<&str> {
    let bytes = src.as_bytes();
    if bytes.first()? != &b'[' || bytes.last()? != &b']' {
        return None;
    }
    Some(&src[1..src.len() - 1])
}

fn keyword_segment(form: &Form) -> Option<&str> {
    match &form.kind {
        FormKind::Keyword(seg) => Some(seg.as_str()),
        _ => None,
    }
}

fn keyword_path_segments(form: &Form) -> Option<Vec<String>> {
    let FormKind::Vector(items) = &form.kind else {
        return None;
    };
    if items.is_empty() {
        return None;
    }
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        let seg = keyword_segment(item)?;
        out.push(seg.to_string());
    }
    Some(out)
}

fn build_kw_chain(target: &str, segments: &[String]) -> String {
    let mut out = String::from(target);
    for seg in segments {
        out.push(':');
        out.push_str(seg);
    }
    out
}

fn rewrite_canonical_access(form: &Form, text: &str) -> Option<RewriteOutcome> {
    if let Some(outcome) = rewrite_canonical_match(form, text) {
        return Some(outcome);
    }
    let mut replacements = Vec::new();
    for_each_form_child(form, |child| {
        if let Some(child_outcome) = rewrite_canonical_access(child, text) {
            if child_outcome.changed {
                if let Some((start, end)) = form_bounds(text, child) {
                    replacements.push(ChildReplacement {
                        start,
                        end,
                        text: child_outcome.text,
                    });
                }
            }
        }
    });
    if replacements.is_empty() {
        let original = if form_has_children(form) {
            form_source(text, form)?
        } else {
            form_source_without_suffixes(text, form).or_else(|| form_source(text, form))?
        };
        return Some(RewriteOutcome {
            text: original,
            changed: false,
        });
    }
    let (parent_start, parent_end) = form_bounds(text, form)?;
    let mut out = text[parent_start..parent_end].to_string();
    replacements.sort_by(|a, b| b.start.cmp(&a.start));
    for rep in replacements {
        let rel_start = rep.start.saturating_sub(parent_start);
        let rel_end = rep.end.saturating_sub(parent_start);
        if rel_start > rel_end || rel_end > out.len() {
            return None;
        }
        out.replace_range(rel_start..rel_end, &rep.text);
    }
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn toggle_access_syntax(form: &Form, text: &str, prefs: RefactorPrefs) -> Option<RewriteOutcome> {
    if let Some(outcome) = rewrite_canonical_access(form, text) {
        if outcome.changed {
            return Some(outcome);
        }
    }
    if let Some(outcome) = rewrite_shortest_access(form, text, prefs) {
        if outcome.changed {
            return Some(outcome);
        }
    }
    None
}

fn rewrite_canonical_match(form: &Form, text: &str) -> Option<RewriteOutcome> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    let head = items.first()?;
    let FormKind::Symbol(sym) = &head.kind else {
        return None;
    };
    if sym == INDEX_GET_SYM {
        return rewrite_index_get_like("get", items, text);
    }
    if sym == INDEX_GET_IN_SYM {
        return rewrite_index_get_like("get-in", items, text);
    }
    if sym == OOP_SYNTAX_SYM {
        return rewrite_oop_index_chain(items, text);
    }
    None
}

fn rewrite_index_get_like(name: &str, items: &[Form], text: &str) -> Option<RewriteOutcome> {
    if items.len() != 3 && items.len() != 4 {
        return None;
    }
    let target = rewrite_canonical_access(&items[1], text)?;
    let key = rewrite_canonical_access(&items[2], text)?;
    let out = if items.len() == 4 {
        let default = rewrite_canonical_access(&items[3], text)?;
        format!("({} {} {} {})", name, target.text, key.text, default.text)
    } else {
        format!("({} {} {})", name, target.text, key.text)
    };
    Some(RewriteOutcome {
        text: out,
        changed: true,
    })
}

fn rewrite_oop_index_chain(items: &[Form], text: &str) -> Option<RewriteOutcome> {
    if items.len() < 3 {
        return None;
    }
    let base = items.get(1)?;
    let base_out = rewrite_canonical_access(base, text)?;
    let mut keys = Vec::new();
    let mut key_forms = Vec::new();
    for stage in &items[2..] {
        if is_oop_nil_safe_marker(stage) {
            return None;
        }
        let FormKind::List(stage_items) = &stage.kind else {
            return None;
        };
        let head = stage_items.first()?;
        let FormKind::Symbol(sym) = &head.kind else {
            return None;
        };
        if sym != OOP_INDEX_SYM {
            return None;
        }
        let key_form = stage_items.get(1)?;
        let key_out = rewrite_canonical_access(key_form, text)?;
        keys.push(key_out.text);
        key_forms.push(key_form);
    }
    if keys.is_empty() {
        return None;
    }
    if keys.len() == 1 {
        let key_form = key_forms[0];
        if matches!(key_form.kind, FormKind::Int(_)) {
            return Some(RewriteOutcome {
                text: format!("(nth {} {})", base_out.text, keys[0]),
                changed: true,
            });
        }
        return Some(RewriteOutcome {
            text: format!("(get {} {})", base_out.text, keys[0]),
            changed: true,
        });
    }
    let path = keys.join(" ");
    Some(RewriteOutcome {
        text: format!("(get-in {} [{}])", base_out.text, path),
        changed: true,
    })
}

fn toggle_oop_form(form: &Form, text: &str, doc: &DocumentData) -> Option<String> {
    if is_oop_syntax_form(form) {
        oop_to_sexp(form, text, doc)
    } else {
        sexp_to_oop(form, text, doc)
    }
}

fn is_oop_syntax_form(form: &Form) -> bool {
    let FormKind::List(items) = &form.kind else {
        return false;
    };
    matches!(
        items.first(),
        Some(Form {
            kind: FormKind::Symbol(sym),
            ..
        }) if sym == OOP_SYNTAX_SYM
    )
}

fn sexp_to_oop(form: &Form, text: &str, doc: &DocumentData) -> Option<String> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    let head = items.first()?;
    let FormKind::Symbol(head_sym) = &head.kind else {
        return None;
    };
    if head_sym == OOP_SYNTAX_SYM {
        return None;
    }
    let subject_pos = resolve_subject_pos_for_symbol(head_sym, doc)?;
    let args = &items[1..];
    if args.is_empty() {
        return None;
    }
    let receiver_idx = match subject_pos {
        fn_meta::SubjectPos::Fixed(pos) => pos.checked_sub(1)?,
        fn_meta::SubjectPos::Last => args.len().saturating_sub(1),
    };
    if receiver_idx >= args.len() {
        return None;
    }
    let receiver = form_source(text, &args[receiver_idx])?;
    let method = form_source(text, head)?;
    let mut arg_texts = Vec::new();
    for (idx, arg) in args.iter().enumerate() {
        if idx == receiver_idx {
            continue;
        }
        let arg_text = form_source(text, arg)?;
        arg_texts.push(arg_text);
    }
    let mut out = String::new();
    out.push_str(&receiver);
    out.push('.');
    out.push_str(&method);
    out.push('(');
    out.push_str(&arg_texts.join(" "));
    out.push(')');
    Some(out)
}

fn oop_to_sexp(form: &Form, text: &str, doc: &DocumentData) -> Option<String> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    if items.len() < 3 {
        return None;
    }
    if !matches!(
        items.first(),
        Some(Form {
            kind: FormKind::Symbol(sym),
            ..
        }) if sym == OOP_SYNTAX_SYM
    ) {
        return None;
    }
    let base = items.get(1)?;
    let mut current = form_source_without_suffixes(text, base)?;
    for stage in &items[2..] {
        if is_oop_nil_safe_marker(stage) {
            return None;
        }
        if let Some(key_form) = parse_oop_index_stage(stage) {
            let key_src = form_source_without_suffixes(text, key_form)
                .or_else(|| form_source(text, key_form))?;
            if matches!(key_form.kind, FormKind::Int(_)) {
                current = format!("(nth {} {})", current, key_src);
            } else {
                current = format!("(get {} {})", current, key_src);
            }
            continue;
        }
        let (method_form, method_name, args) = parse_oop_call_stage(stage)?;
        let subject_pos = resolve_subject_pos_for_symbol(method_name, doc)?;
        let method_src = form_source(text, method_form)?;
        let mut arg_texts = Vec::new();
        for arg in args {
            let arg_text = form_source(text, arg)?;
            arg_texts.push(arg_text);
        }
        let mut call_args = arg_texts;
        match subject_pos {
            fn_meta::SubjectPos::Fixed(pos) => {
                let idx = pos.checked_sub(1)?;
                if idx > call_args.len() {
                    return None;
                }
                call_args.insert(idx, current);
            }
            fn_meta::SubjectPos::Last => {
                call_args.push(current);
            }
        }
        current = format!("({} {})", method_src, call_args.join(" "));
    }
    Some(current)
}

fn parse_oop_index_stage<'a>(stage: &'a Form) -> Option<&'a Form> {
    let FormKind::List(items) = &stage.kind else {
        return None;
    };
    let head = items.first()?;
    let FormKind::Symbol(sym) = &head.kind else {
        return None;
    };
    if sym != OOP_INDEX_SYM {
        return None;
    }
    items.get(1)
}

fn parse_oop_call_stage<'a>(stage: &'a Form) -> Option<(&'a Form, &'a str, Vec<&'a Form>)> {
    let FormKind::List(items) = &stage.kind else {
        return None;
    };
    let head = items.first()?;
    let FormKind::Symbol(sym) = &head.kind else {
        return None;
    };
    if matches!(
        sym.as_str(),
        OOP_NIL_SAFE_SYM
            | OOP_INDEX_SYM
            | OOP_DOT_STAGE_SYM
            | OOP_AS_SYM
            | OOP_LET_SYM
            | OOP_SEG_SYM
    ) {
        return None;
    }
    if sym == OOP_BARE_SYM {
        let method = items.get(1)?;
        let FormKind::Symbol(method_sym) = &method.kind else {
            return None;
        };
        return Some((method, method_sym, Vec::new()));
    }
    Some((head, sym, items.iter().skip(1).collect()))
}

fn is_oop_nil_safe_marker(form: &Form) -> bool {
    matches!(
        &form.kind,
        FormKind::List(items) if items.len() == 1 && matches!(
            &items[0].kind,
            FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM
        )
    )
}

fn resolve_subject_pos_for_symbol(sym: &str, doc: &DocumentData) -> Option<fn_meta::SubjectPos> {
    if let Some(pos) = fn_meta::get(sym).and_then(|meta| meta.subject_pos) {
        return Some(pos);
    }
    if sym.contains("::") {
        return None;
    }
    for (ns, local) in resolve_symbol_candidates(sym, doc) {
        let Some(ns) = ns else {
            continue;
        };
        let full = format!("{}::{}", ns, local);
        if let Some(pos) = fn_meta::get(&full).and_then(|meta| meta.subject_pos) {
            return Some(pos);
        }
    }
    if doc.symbols.lookup(sym).is_none() {
        let full = format!("core::{}", sym);
        if let Some(pos) = fn_meta::get(&full).and_then(|meta| meta.subject_pos) {
            return Some(pos);
        }
    }
    None
}

fn unbound_symbol_from_message(message: &str) -> Option<&str> {
    message
        .strip_prefix("Unbound symbol:")
        .map(str::trim)
        .filter(|s| !s.is_empty())
}

fn require_candidates_for_symbol(
    store: &DocumentStore,
    doc: &DocumentData,
    sym: &str,
) -> Vec<String> {
    let canonical = canonical_symbol_name(sym).into_owned();
    let mut candidates = Vec::new();
    let mut seen = HashSet::new();
    for info in store.iter_user_symbols() {
        let info_canon = canonical_symbol_name(&info.name).into_owned();
        if info_canon != canonical {
            continue;
        }
        let ns = match &info.namespace {
            Some(ns) => ns.clone(),
            None => match info.namespace_aliases.first() {
                Some(alias) => alias.clone(),
                None => continue,
            },
        };
        if doc.namespace.as_deref() == Some(ns.as_str()) {
            continue;
        }
        if !seen.insert(normalize_namespace(&ns)) {
            continue;
        }
        if already_has_symbol_from_ns(doc, &ns, sym) {
            continue;
        }
        candidates.push(ns);
        if candidates.len() >= 8 {
            break;
        }
    }
    candidates
}

fn already_has_symbol_from_ns(doc: &DocumentData, target_ns: &str, sym: &str) -> bool {
    for spec in &doc.requires {
        if let Some(ns) = require_target_namespace_for(spec, doc) {
            if normalize_namespace(&ns) == normalize_namespace(target_ns) {
                if spec.refer_all || spec.refers.iter().any(|r| r == sym) {
                    return true;
                }
                if spec.rename.values().any(|alias| alias == sym) {
                    return true;
                }
            }
        }
    }
    false
}

fn build_require_edit(doc: &DocumentData, target_ns: &str, sym: &str) -> Option<TextEdit> {
    let text = &doc.text;
    if let Some(ns_form) = find_ns_form(doc) {
        if let Some(require_form) = find_require_option_form(ns_form) {
            let start = require_form.span.index;
            let end = find_matching_paren(text, start)?;
            let snippet = {
                let inside = &text[start..end];
                if inside.contains('\n') {
                    format!(
                        "\n{}[{} :refer [{}]]",
                        line_indentation(text, start) + "  ",
                        target_ns,
                        sym
                    )
                } else {
                    format!(" [{} :refer [{}]]", target_ns, sym)
                }
            };
            let position = offset_to_position(text, end);
            return Some(TextEdit {
                range: Range {
                    start: position,
                    end: position,
                },
                new_text: snippet,
            });
        }
        let start = ns_form.span.index;
        let end = find_matching_paren(text, start)?;
        let snippet = {
            let inside = &text[start..end];
            if inside.contains('\n') {
                format!(
                    "\n{}(:require [{} :refer [{}]])",
                    line_indentation(text, start) + "  ",
                    target_ns,
                    sym
                )
            } else {
                format!(" (:require [{} :refer [{}]])", target_ns, sym)
            }
        };
        let position = offset_to_position(text, end);
        return Some(TextEdit {
            range: Range {
                start: position,
                end: position,
            },
            new_text: snippet,
        });
    }
    let insertion = if text.is_empty() {
        format!("(require {} refer [{}])\n", target_ns, sym)
    } else {
        format!("(require {} refer [{}])\n", target_ns, sym)
    };
    let position = Position {
        line: 0,
        character: 0,
    };
    Some(TextEdit {
        range: Range {
            start: position,
            end: position,
        },
        new_text: insertion,
    })
}

fn find_ns_form<'a>(doc: &'a DocumentData) -> Option<&'a Form> {
    let forms = doc.ast.as_ref()?;
    for form in forms {
        if let FormKind::List(items) = &form.kind {
            if let Some(Form {
                kind: FormKind::Symbol(sym),
                ..
            }) = items.first()
            {
                if sym == "ns" {
                    return Some(form);
                }
            }
        }
    }
    None
}

fn find_require_option_form<'a>(ns_form: &'a Form) -> Option<&'a Form> {
    let items = match &ns_form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    for option in items.iter().skip(2) {
        if let FormKind::List(inner) = &option.kind {
            if let Some(Form {
                kind: FormKind::Symbol(sym),
                ..
            }) = inner.first()
            {
                if sym == "require" || sym == ":require" {
                    return Some(option);
                }
            }
            if let Some(Form {
                kind: FormKind::Keyword(kw),
                ..
            }) = inner.first()
            {
                if kw == "require" || kw == ":require" {
                    return Some(option);
                }
            }
        }
    }
    None
}

fn line_at(text: &str, line: usize) -> Option<String> {
    text.lines().nth(line).map(|s| s.to_string())
}

fn char_index_to_byte_index(text: &str, char_idx: usize) -> usize {
    text.char_indices()
        .nth(char_idx)
        .map(|(idx, _)| idx)
        .unwrap_or_else(|| text.len())
}

fn slice_by_char_indices(text: &str, start: usize, end: usize) -> Option<&str> {
    if start > end {
        return None;
    }
    let start_byte = char_index_to_byte_index(text, start);
    let end_byte = char_index_to_byte_index(text, end);
    text.get(start_byte..end_byte)
}

#[allow(dead_code)]
fn find_word_at_position(text: &str, pos: Position) -> Option<String> {
    let line = text.lines().nth(pos.line as usize)?;
    let (s, e) = find_word_range(line, pos.character as usize);
    if s == e {
        None
    } else {
        slice_by_char_indices(line, s, e).map(|slice| slice.to_string())
    }
}

struct TypeToken {
    name: String,
    range: Range,
}

fn type_token_at(text: &str, position: Position) -> Option<TypeToken> {
    let line = line_at(text, position.line as usize)?;
    let col = position.character as usize;
    let (annot_start, annot_end) = find_type_annotation_bounds(&line, col)?;
    let (token_start, token_end) = type_token_bounds(&line, col, annot_start + 1, annot_end);
    if token_start == token_end {
        return None;
    }
    let chars: Vec<char> = line.chars().collect();
    let name: String = chars
        .iter()
        .skip(token_start)
        .take(token_end - token_start)
        .collect();
    Some(TypeToken {
        name,
        range: Range {
            start: Position {
                line: position.line,
                character: token_start as u32,
            },
            end: Position {
                line: position.line,
                character: token_end as u32,
            },
        },
    })
}

fn type_token_in_word(
    line: &str,
    line_no: u32,
    col: usize,
    start: usize,
    end: usize,
) -> Option<TypeToken> {
    let (token_start, token_end) = type_token_bounds(line, col, start, end);
    if token_start == token_end {
        return None;
    }
    let chars: Vec<char> = line.chars().collect();
    let name: String = chars
        .iter()
        .skip(token_start)
        .take(token_end - token_start)
        .collect();
    if name.is_empty() {
        return None;
    }
    Some(TypeToken {
        name,
        range: Range {
            start: Position {
                line: line_no,
                character: token_start as u32,
            },
            end: Position {
                line: line_no,
                character: token_end as u32,
            },
        },
    })
}

fn word_has_type_punctuation(word: &str) -> bool {
    word.chars()
        .any(|ch| matches!(ch, '<' | '>' | '|' | '[' | ']' | '{' | '}' | ',' | '?'))
}

fn find_word_range(line: &str, col: usize) -> (usize, usize) {
    let chars: Vec<char> = line.chars().collect();
    let idx = col.min(chars.len());
    let mut start = idx;
    while start > 0 && is_word_char(chars[start - 1]) {
        start -= 1;
    }
    let mut end = idx;
    while end < chars.len() && is_word_char(chars[end]) {
        end += 1;
    }
    (start, end)
}

fn symbol_token_range_from(line: &str, col: usize) -> (usize, usize) {
    let chars: Vec<char> = line.chars().collect();
    let start = col.min(chars.len());
    let mut idx = start;
    let mut angle_depth = 0usize;
    while idx < chars.len() {
        let ch = chars[idx];
        let next = chars.get(idx + 1).copied();
        let is_dispatch =
            ch == '#' && matches!(next, Some('{') | Some('(') | Some('_') | Some('"'));
        let is_delim = is_ws_or_comma(ch)
            || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';')
            || is_dispatch;
        if is_delim && angle_depth == 0 {
            break;
        }
        if ch == ':' && angle_depth == 0 {
            let prev_is_colon = idx > start && chars[idx - 1] == ':';
            let next_is_colon = next == Some(':');
            if idx > start
                && !prev_is_colon
                && !next_is_colon
                && next.map_or(false, is_kw_seg_start)
            {
                break;
            }
        }
        if ch == '.' && angle_depth == 0 {
            let allow_range_token = idx > start && chars[idx - 1] == '.';
            if !allow_range_token {
                if next == Some('(') {
                    break;
                }
                if next.map_or(false, is_oop_method_start) {
                    break;
                }
                if next == Some('"') {
                    break;
                }
                if next.map_or(false, |c| c.is_ascii_digit()) {
                    let numeric_prefix = chars[start..idx]
                        .iter()
                        .all(|c| c.is_ascii_digit() || *c == '_' || *c == '-');
                    if !numeric_prefix {
                        break;
                    }
                }
            }
        }
        if ch == '<' && idx > start && chars[idx - 1] != ':' {
            angle_depth += 1;
        } else if ch == '>' && angle_depth > 0 {
            angle_depth -= 1;
        }
        idx += 1;
    }
    (start, idx)
}

fn oop_chain_base_symbol(word: &str) -> Option<String> {
    if word.is_empty() || word.starts_with(':') {
        return None;
    }
    if !word.contains(':') && !word.contains('.') {
        return None;
    }
    let (start, end) = symbol_token_range_from(word, 0);
    if start != 0 || end == 0 {
        return None;
    }
    let total_len = word.chars().count();
    if end >= total_len {
        return None;
    }
    let base = slice_by_char_indices(word, start, end)?;
    if base.is_empty() {
        return None;
    }
    Some(base.to_string())
}

fn find_word_occurrences(text: &str, word: &str) -> Vec<Range> {
    if word.is_empty() {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    for (line_idx, line) in text.lines().enumerate() {
        let mut start = 0;
        while let Some(pos) = line[start..].find(word) {
            let abs = start + pos;
            let before = abs.checked_sub(1).and_then(|i| line.chars().nth(i));
            let after = line.chars().nth(abs + word.len());
            if before.map_or(true, |c| !is_word_char(c)) && after.map_or(true, |c| !is_word_char(c))
            {
                ranges.push(Range {
                    start: Position {
                        line: line_idx as u32,
                        character: abs as u32,
                    },
                    end: Position {
                        line: line_idx as u32,
                        character: (abs + word.len()) as u32,
                    },
                });
            }
            start = abs + word.len();
            if start >= line.len() {
                break;
            }
        }
    }
    ranges
}

fn symbol_info_by_start(doc: &DocumentData, start: Position) -> Option<&SymbolInfo> {
    doc.symbols
        .entries()
        .iter()
        .find(|info| info.range.start == start)
}

fn token_at_position(doc: &DocumentData, position: Position) -> Option<TokenAtPosition> {
    if let Some((symbol, range)) = symbol_at_position(doc, position) {
        return Some(TokenAtPosition::Symbol {
            name: symbol,
            range,
        });
    }
    if let Some((keyword, range)) = keyword_at_position(doc, position) {
        return Some(TokenAtPosition::Keyword {
            name: keyword,
            range,
        });
    }
    None
}

fn symbol_at_position(doc: &DocumentData, position: Position) -> Option<(String, Range)> {
    let forms = doc.ast.as_ref()?;
    find_symbol_in_forms_at_position(forms, &doc.text, position)
}

fn keyword_at_position(doc: &DocumentData, position: Position) -> Option<(String, Range)> {
    let forms = doc.ast.as_ref()?;
    find_keyword_in_forms_at_position(forms, &doc.text, position)
}

fn find_symbol_in_forms_at_position(
    forms: &[Form],
    text: &str,
    position: Position,
) -> Option<(String, Range)> {
    for form in forms {
        if let Some(found) = find_symbol_in_form(form, text, position) {
            return Some(found);
        }
    }
    None
}

fn find_keyword_in_forms_at_position(
    forms: &[Form],
    text: &str,
    position: Position,
) -> Option<(String, Range)> {
    for form in forms {
        if let Some(found) = find_keyword_in_form(form, text, position) {
            return Some(found);
        }
    }
    None
}

fn find_symbol_in_form(form: &Form, text: &str, position: Position) -> Option<(String, Range)> {
    match &form.kind {
        FormKind::Symbol(sym) => {
            let range = symbol_range_from_span(text, &form.span)?;
            if position_in_range(position, &range) {
                return Some((sym.clone(), range));
            }
            None
        }
        FormKind::List(items) => {
            if is_quote_form(items) {
                return None;
            }
            for item in items {
                if let Some(found) = find_symbol_in_form(item, text, position) {
                    return Some(found);
                }
            }
            None
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                if let Some(found) = find_symbol_in_form(item, text, position) {
                    return Some(found);
                }
            }
            None
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        if let Some(found) = find_symbol_in_form(k, text, position) {
                            return Some(found);
                        }
                        if let Some(found) = find_symbol_in_form(v, text, position) {
                            return Some(found);
                        }
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        if let Some(found) = find_symbol_in_form(f, text, position) {
                            return Some(found);
                        }
                    }
                }
            }
            None
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    if let Some(found) = find_symbol_in_form(expr, text, position) {
                        return Some(found);
                    }
                }
            }
            None
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    if let Some(found) = find_symbol_in_form(expr, text, position) {
                        return Some(found);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn find_keyword_in_form(form: &Form, text: &str, position: Position) -> Option<(String, Range)> {
    match &form.kind {
        FormKind::Keyword(kw) => {
            let range = symbol_range_from_span(text, &form.span)?;
            if position_in_range(position, &range) {
                return Some((kw.clone(), range));
            }
            None
        }
        FormKind::List(items) => {
            for item in items {
                if let Some(found) = find_keyword_in_form(item, text, position) {
                    return Some(found);
                }
            }
            None
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                if let Some(found) = find_keyword_in_form(item, text, position) {
                    return Some(found);
                }
            }
            None
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        if let Some(found) = find_keyword_in_form(k, text, position) {
                            return Some(found);
                        }
                        if let Some(found) = find_keyword_in_form(v, text, position) {
                            return Some(found);
                        }
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        if let Some(found) = find_keyword_in_form(f, text, position) {
                            return Some(found);
                        }
                    }
                }
            }
            None
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    if let Some(found) = find_keyword_in_form(expr, text, position) {
                        return Some(found);
                    }
                }
            }
            None
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    if let Some(found) = find_keyword_in_form(expr, text, position) {
                        return Some(found);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn symbol_range_from_span(text: &str, span: &Span) -> Option<Range> {
    let line_no = span.line.saturating_sub(1);
    let col = span.col.saturating_sub(1);
    let line = line_at(text, line_no)?;
    let (start, end) = symbol_token_range_from(&line, col);
    if start == end {
        return None;
    }
    Some(Range {
        start: Position {
            line: line_no as u32,
            character: start as u32,
        },
        end: Position {
            line: line_no as u32,
            character: end as u32,
        },
    })
}

fn is_quote_form(items: &[Form]) -> bool {
    matches!(
        items.first(),
        Some(Form {
            kind: FormKind::Symbol(sym),
            ..
        }) if matches!(sym.as_str(), "quote" | "quote*" | "quasiquote" | "syntax-quote")
    )
}

fn collect_symbol_occurrences_in_form(
    form: &Form,
    text: &str,
    target_canon: &str,
    out: &mut Vec<Range>,
) {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if canonical_symbol_name(sym).as_ref() == target_canon {
                if let Some(range) = symbol_range_from_span(text, &form.span) {
                    out.push(range);
                }
            }
        }
        FormKind::List(items) => {
            if is_quote_form(items) {
                return;
            }
            for item in items {
                collect_symbol_occurrences_in_form(item, text, target_canon, out);
            }
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                collect_symbol_occurrences_in_form(item, text, target_canon, out);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_symbol_occurrences_in_form(k, text, target_canon, out);
                        collect_symbol_occurrences_in_form(v, text, target_canon, out);
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        collect_symbol_occurrences_in_form(f, text, target_canon, out);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_symbol_occurrences_in_form(expr, text, target_canon, out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_symbol_occurrences_in_form(expr, text, target_canon, out);
                }
            }
        }
        _ => {}
    }
}

fn collect_keyword_occurrences_in_form(
    form: &Form,
    text: &str,
    target: &str,
    out: &mut Vec<Range>,
) {
    match &form.kind {
        FormKind::Keyword(kw) => {
            if kw == target {
                if let Some(range) = symbol_range_from_span(text, &form.span) {
                    out.push(range);
                }
            }
        }
        FormKind::List(items) => {
            for item in items {
                collect_keyword_occurrences_in_form(item, text, target, out);
            }
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                collect_keyword_occurrences_in_form(item, text, target, out);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_keyword_occurrences_in_form(k, text, target, out);
                        collect_keyword_occurrences_in_form(v, text, target, out);
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        collect_keyword_occurrences_in_form(f, text, target, out);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_keyword_occurrences_in_form(expr, text, target, out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_keyword_occurrences_in_form(expr, text, target, out);
                }
            }
        }
        _ => {}
    }
}

fn collect_symbol_occurrences_in_forms(
    forms: &[Form],
    text: &str,
    target_canon: &str,
) -> Vec<Range> {
    let mut out = Vec::new();
    for form in forms {
        collect_symbol_occurrences_in_form(form, text, target_canon, &mut out);
    }
    out
}

fn collect_keyword_occurrences_in_forms(forms: &[Form], text: &str, target: &str) -> Vec<Range> {
    let mut out = Vec::new();
    for form in forms {
        collect_keyword_occurrences_in_form(form, text, target, &mut out);
    }
    out
}

fn slice_range(text: &str, range: &Range) -> Option<String> {
    let start = position_to_offset(text, range.start);
    let end = position_to_offset(text, range.end);
    text.get(start..end).map(|s| s.to_string())
}

fn apply_rename_to_token(original: &str, new_name: &str) -> String {
    let (base, suffix) = match original.find('<') {
        Some(idx) => (&original[..idx], &original[idx..]),
        None => (original, ""),
    };
    let replaced = if let Some((prefix, _)) = base.rsplit_once("::") {
        if new_name.contains("::") {
            new_name.to_string()
        } else {
            format!("{}::{}", prefix, new_name)
        }
    } else {
        new_name.to_string()
    };
    format!("{}{}", replaced, suffix)
}

fn build_keyword_text_edits(text: &str, ranges: Vec<Range>, new_keyword: &str) -> Vec<TextEdit> {
    let mut edits = Vec::new();
    let mut seen = HashSet::new();
    for range in ranges {
        let key = (
            range.start.line,
            range.start.character,
            range.end.line,
            range.end.character,
        );
        if !seen.insert(key) {
            continue;
        }
        if slice_range(text, &range).is_none() {
            continue;
        }
        edits.push(TextEdit {
            range,
            new_text: new_keyword.to_string(),
        });
    }
    edits.sort_by(|a, b| {
        a.range
            .start
            .line
            .cmp(&b.range.start.line)
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
    edits
}

fn build_text_edits(text: &str, ranges: Vec<Range>, new_name: &str) -> Vec<TextEdit> {
    let mut edits = Vec::new();
    let mut seen = HashSet::new();
    for range in ranges {
        let key = (
            range.start.line,
            range.start.character,
            range.end.line,
            range.end.character,
        );
        if !seen.insert(key) {
            continue;
        }
        let original = match slice_range(text, &range) {
            Some(s) => s,
            None => continue,
        };
        let new_text = apply_rename_to_token(&original, new_name);
        edits.push(TextEdit { range, new_text });
    }
    edits.sort_by(|a, b| {
        a.range
            .start
            .line
            .cmp(&b.range.start.line)
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
    edits
}

fn rename_local_symbol(
    uri: &Url,
    doc: &DocumentData,
    target_canon: &str,
    new_name: &str,
    binding_start: Position,
) -> Option<WorkspaceEdit> {
    let forms = doc.ast.as_ref()?;
    let text = &doc.text;
    let target_offset = position_to_offset(text, binding_start);
    let top_form = enclosing_top_level_form(forms, text, target_offset)?;
    let mut ranges = Vec::new();
    collect_symbol_occurrences_in_form(top_form, text, target_canon, &mut ranges);
    let mut filtered = Vec::new();
    for range in ranges {
        let def_range = find_local_definition_in_forms(text, forms, range.start, target_canon);
        if let Some(def_range) = def_range {
            if def_range.start == binding_start {
                filtered.push(range);
            }
        }
    }
    let edits = build_text_edits(text, filtered, new_name);
    if edits.is_empty() {
        return None;
    }
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

fn normalize_keyword_rename(new_name: &str) -> Option<String> {
    let trimmed = new_name.trim();
    if trimmed.is_empty() {
        return None;
    }
    let raw = trimmed.strip_prefix(':').unwrap_or(trimmed);
    if raw.is_empty() {
        return None;
    }
    Some(format!(":{}", raw))
}

fn rename_keyword_in_document(
    uri: &Url,
    doc: &DocumentData,
    target: &str,
    new_keyword: &str,
) -> Option<WorkspaceEdit> {
    let forms = doc.ast.as_ref()?;
    let text = &doc.text;
    let ranges = collect_keyword_occurrences_in_forms(forms, text, target);
    let edits = build_keyword_text_edits(text, ranges, new_keyword);
    if edits.is_empty() {
        return None;
    }
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

fn rename_global_symbol(
    store: &DocumentStore,
    target_canon: &str,
    new_name: &str,
    anchor: Option<DefinitionAnchor>,
    origin_doc: &DocumentData,
    origin_symbol: &str,
) -> Option<WorkspaceEdit> {
    let anchor = anchor?;
    let is_qualified = target_canon.contains("::");
    let allowed_ns = if let Some(ns) = anchor.namespace.clone() {
        vec![Some(ns)]
    } else {
        let mut allowed: Vec<Option<String>> = Vec::new();
        for (ns, _) in resolve_symbol_candidates(origin_symbol, origin_doc) {
            if !allowed.iter().any(|item| item.as_ref() == ns.as_ref()) {
                allowed.push(ns);
            }
        }
        if allowed.is_empty() {
            allowed.push(None);
        }
        allowed
    };
    let mut changes = HashMap::new();
    for (uri, data) in &store.docs {
        if data.is_virtual {
            continue;
        }
        if !is_qualified {
            if let Some(ns) = anchor.namespace.as_deref() {
                if !doc_supports_namespace(data, ns) {
                    continue;
                }
            } else if !doc_matches_allowed_namespaces(data, &allowed_ns) {
                continue;
            }
        }
        let text = match load_text_for_doc(data, uri) {
            Some(text) => text,
            None => continue,
        };
        let forms = match load_forms_for_doc(data, &text, uri) {
            Some(forms) => forms,
            None => continue,
        };
        let ranges = collect_symbol_occurrences_in_forms(&forms, &text, target_canon);
        if ranges.is_empty() {
            continue;
        }
        let mut filtered = Vec::new();
        for range in ranges {
            if let Some(def_range) =
                find_local_definition_in_forms(&text, &forms, range.start, target_canon)
            {
                let is_anchor = uri == &anchor.uri && def_range.start == anchor.start;
                if !is_anchor {
                    continue;
                }
            }
            filtered.push(range);
        }
        let edits = build_text_edits(&text, filtered, new_name);
        if !edits.is_empty() {
            changes.insert(uri.clone(), edits);
        }
    }
    if changes.is_empty() {
        None
    } else {
        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }
}

fn resolve_definition_location_for_rename(
    store: &DocumentStore,
    doc: &DocumentData,
    name: &str,
) -> Option<Location> {
    let candidates = resolve_symbol_candidates(name, doc);
    for (ns, lookup) in candidates {
        let mut locations = store.find_definitions(&lookup, ns.as_deref());
        if locations.is_empty() {
            let canon_lookup = canonical_symbol_name(&lookup).into_owned();
            if canon_lookup != lookup {
                locations = store.find_definitions(&canon_lookup, ns.as_deref());
            }
        }
        if let Some(loc) = locations.into_iter().next() {
            return Some(loc);
        }
    }
    None
}

fn doc_supports_namespace(doc: &DocumentData, ns: &str) -> bool {
    if doc.namespace.as_deref() == Some(ns) {
        return true;
    }
    if doc.namespace_aliases.iter().any(|alias| alias == ns) {
        return true;
    }
    doc.requires
        .iter()
        .filter_map(|spec| require_target_namespace_for(spec, doc))
        .any(|target| target == ns)
}

fn doc_matches_allowed_namespaces(doc: &DocumentData, allowed: &[Option<String>]) -> bool {
    let mut ns_ok = allowed
        .iter()
        .any(|a| a.is_none() && doc.namespace.is_none());
    if let Some(ns) = &doc.namespace {
        if allowed.iter().any(|a| a.as_deref() == Some(ns)) {
            ns_ok = true;
        }
    }
    if !ns_ok {
        for alias in &doc.namespace_aliases {
            if allowed.iter().any(|a| a.as_deref() == Some(alias)) {
                ns_ok = true;
                break;
            }
        }
    }
    ns_ok
}

fn load_text_for_doc(doc: &DocumentData, uri: &Url) -> Option<String> {
    if !doc.text.is_empty() {
        return Some(doc.text.clone());
    }
    let path = uri.to_file_path().ok()?;
    fs::read_to_string(path).ok()
}

fn load_forms_for_doc(doc: &DocumentData, text: &str, uri: &Url) -> Option<Vec<Form>> {
    let path = uri
        .to_file_path()
        .ok()
        .and_then(|p| p.to_str().map(|s| s.to_string()));
    if let Some(forms) = &doc.ast {
        return Some(forms.clone());
    }
    parse_source_for_lsp_lenient(text, path.as_deref()).ok()
}

fn split_namespace(token: &str) -> Option<(String, String)> {
    if let Some(idx) = token.find("::") {
        let (ns, local) = token.split_at(idx);
        return Some((ns.to_string(), local.trim_start_matches("::").to_string()));
    }
    None
}

fn split_enum_variant(token: &str) -> Option<(String, String)> {
    token
        .rsplit_once("::")
        .map(|(ns, local)| (ns.to_string(), local.to_string()))
        .filter(|(ns, local)| !ns.is_empty() && !local.is_empty())
}

fn normalize_type_name_for_lookup(name: &str) -> String {
    match name {
        "String" => "Str".to_string(),
        "Integer" => "Int".to_string(),
        "Sym" => "Symbol".to_string(),
        "Keyword" | "Kw" => "Symbol".to_string(),
        "Re" => "Regex".to_string(),
        "Vec" => "Vector".to_string(),
        other => other.to_string(),
    }
}

fn resolve_symbol_candidates(word: &str, doc: &DocumentData) -> Vec<(Option<String>, String)> {
    if let Some((ns, local)) = split_namespace(word) {
        if let Some(target_ns) = resolve_alias_namespace(&ns, doc) {
            return vec![(Some(target_ns), local)];
        }
        let normalized = normalize_namespace(&ns);
        return vec![(Some(normalized), local)];
    }
    let mut out = Vec::new();
    if let Some(ns) = &doc.namespace {
        out.push((Some(normalize_namespace(ns)), word.to_string()));
    }
    for alias in &doc.namespace_aliases {
        out.push((Some(normalize_namespace(alias)), word.to_string()));
    }
    for spec in &doc.requires {
        let target_ns = match require_target_namespace_for(spec, doc) {
            Some(ns) => ns,
            None => continue,
        };
        if let Some(orig) = spec.rename.iter().find_map(|(orig, alias)| {
            if alias == word {
                Some(orig.clone())
            } else {
                None
            }
        }) {
            out.push((Some(target_ns.clone()), orig));
            continue;
        }
        if spec.refer_all || spec.refers.iter().any(|r| r == word) {
            out.push((Some(target_ns.clone()), word.to_string()));
        }
    }
    out
}

struct EnumVariantResolution {
    enum_raw: String,
    enum_info: SymbolInfo,
    member: EnumMemberInfo,
}

fn resolve_enum_variant(
    word: &str,
    doc: &DocumentData,
    store: &DocumentStore,
) -> Option<EnumVariantResolution> {
    let canonical = canonical_symbol_name(word);
    let (enum_raw, variant_name) = split_enum_variant(canonical.as_ref())?;
    let candidates = resolve_symbol_candidates(&enum_raw, doc);
    for (ns, lookup) in candidates {
        let mut infos = store.find_symbol_infos(&lookup, ns.as_deref());
        if infos.is_empty() {
            let canon = canonical_symbol_name(&lookup).into_owned();
            if canon != lookup {
                infos = store.find_symbol_infos(&canon, ns.as_deref());
            }
        }
        for info in infos
            .into_iter()
            .filter(|info| matches!(info.kind, SymbolKind::Defenum))
        {
            if let Some(member) = info
                .enum_members
                .iter()
                .find(|m| m.name == variant_name)
                .cloned()
            {
                return Some(EnumVariantResolution {
                    enum_raw: enum_raw.clone(),
                    enum_info: info,
                    member,
                });
            }
        }
    }
    None
}

fn enum_variant_definition_location(
    word: &str,
    doc: &DocumentData,
    store: &DocumentStore,
) -> Option<Location> {
    let canonical = canonical_symbol_name(word);
    let (enum_raw, variant_name) = split_enum_variant(canonical.as_ref())?;
    let candidates = resolve_symbol_candidates(&enum_raw, doc);
    for (ns, lookup) in candidates {
        if let Some(loc) = store.find_enum_variant_location(&lookup, &variant_name, ns.as_deref()) {
            return Some(loc);
        }
        let canon = canonical_symbol_name(&lookup).into_owned();
        if canon != lookup {
            if let Some(loc) =
                store.find_enum_variant_location(&canon, &variant_name, ns.as_deref())
            {
                return Some(loc);
            }
        }
    }
    None
}

fn enum_variant_completion_items(
    prefix: &str,
    replace_range: &Range,
    store: &DocumentStore,
    doc: &DocumentData,
) -> Option<Vec<CompletionItem>> {
    let canonical = canonical_symbol_name(prefix);
    let (enum_raw, variant_prefix) = split_enum_variant(canonical.as_ref())?;
    let candidates = resolve_symbol_candidates(&enum_raw, doc);
    let prefix_lower = variant_prefix.to_lowercase();
    for (ns, lookup) in candidates {
        let mut infos = store.find_symbol_infos(&lookup, ns.as_deref());
        if infos.is_empty() {
            let canon = canonical_symbol_name(&lookup).into_owned();
            if canon != lookup {
                infos = store.find_symbol_infos(&canon, ns.as_deref());
            }
        }
        for info in infos
            .into_iter()
            .filter(|info| matches!(info.kind, SymbolKind::Defenum))
        {
            let mut items = Vec::new();
            for member in &info.enum_members {
                if !prefix_lower.is_empty()
                    && !member.name.to_lowercase().starts_with(&prefix_lower)
                {
                    continue;
                }
                let label = format!("{enum_raw}::{}", member.name);
                let mut item = CompletionItem::default();
                item.label = label.clone();
                item.kind = Some(CompletionItemKind::ENUM_MEMBER);
                item.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
                    range: replace_range.clone(),
                    new_text: label,
                }));
                items.push(item);
            }
            if !items.is_empty() {
                return Some(items);
            }
        }
    }
    None
}

fn resolve_alias_namespace(alias: &str, doc: &DocumentData) -> Option<String> {
    for spec in &doc.requires {
        if let Some(spec_alias) = &spec.alias {
            if spec_alias == alias {
                if let Some(ns) = require_target_namespace_for(spec, doc) {
                    return Some(ns);
                }
            }
        }
    }
    None
}

fn is_word_char(ch: char) -> bool {
    const EXTRA: &[char] = &[
        '_', '-', '?', '!', '*', '+', '<', '>', '=', '%', '&', '\'', ':', '.', '#', '$', '|', '@',
    ];
    ch.is_alphanumeric() || EXTRA.contains(&ch)
}

fn is_ws_or_comma(ch: char) -> bool {
    ch.is_whitespace() || ch == ','
}

fn is_kw_seg_start(ch: char) -> bool {
    if ch.is_whitespace() {
        return false;
    }
    if ch.is_ascii() {
        ch.is_ascii_lowercase() || ch == '_'
    } else {
        true
    }
}

fn is_oop_ident_char(ch: char) -> bool {
    if is_ws_or_comma(ch) {
        return false;
    }
    !matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';' | '.')
}

fn is_oop_method_start(ch: char) -> bool {
    is_oop_ident_char(ch) && !ch.is_ascii_digit()
}

fn find_local_definition(
    uri: &Url,
    doc: &DocumentData,
    position: Position,
    name: &str,
) -> Option<Location> {
    let forms = doc.ast.as_ref()?;
    let range = find_local_definition_in_forms(&doc.text, forms, position, name)?;
    Some(Location {
        uri: uri.clone(),
        range,
    })
}

fn find_local_definition_in_forms(
    text: &str,
    forms: &[Form],
    position: Position,
    name: &str,
) -> Option<Range> {
    let target_offset = position_to_offset(text, position);
    let mut best: Option<(usize, Range)> = None;
    if let Some(form) = enclosing_top_level_form(forms, text, target_offset) {
        collect_bindings(form, text, target_offset, name, &mut best);
    }
    best.map(|(_, range)| range)
}

fn enclosing_top_level_form<'a>(
    forms: &'a [Form],
    text: &str,
    target_offset: usize,
) -> Option<&'a Form> {
    let doc_len = text.len();
    for (idx, form) in forms.iter().enumerate() {
        let start = span_offset(&form.span, text);
        let end = forms
            .get(idx + 1)
            .map(|f| span_offset(&f.span, text))
            .unwrap_or(doc_len);
        if target_offset >= start && target_offset < end {
            return Some(form);
        }
    }
    None
}

fn collect_bindings(
    form: &Form,
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    let form_offset = span_offset(&form.span, text);
    if form_offset > target_offset {
        return;
    }
    match &form.kind {
        FormKind::List(items) => {
            if let Some(Form {
                kind: FormKind::Symbol(head),
                ..
            }) = items.first()
            {
                match head.as_str() {
                    "def" | "def-" | "-def" => {
                        if let Some(Form {
                            kind: FormKind::Symbol(sym),
                            span,
                            ..
                        }) = items.get(1)
                        {
                            consider_binding(sym, span, text, target_offset, name, best);
                        }
                    }
                    "defn" | "defn-" | "-defn" => {
                        if let Some(Form {
                            kind: FormKind::Symbol(sym),
                            span,
                            ..
                        }) = items.get(1)
                        {
                            consider_binding(sym, span, text, target_offset, name, best);
                        }
                        scan_defn_bindings(items, text, target_offset, name, best);
                    }
                    "fn" | "fn*" => scan_fn_bindings(items, text, target_offset, name, best),
                    "let" | "loop" | "loop*" => {
                        scan_let_bindings(items, text, target_offset, name, best)
                    }
                    "try" => {
                        if is_short_try_binding_form(items) {
                            scan_let_bindings(items, text, target_offset, name, best);
                        }
                    }
                    "doseq" => scan_doseq_bindings(items, text, target_offset, name, best),
                    "dotimes" => scan_dotimes_bindings(items, text, target_offset, name, best),
                    "when-let" | "if-let" | "if-some" => {
                        scan_if_let_bindings(items, text, target_offset, name, best)
                    }
                    "match" => scan_match_bindings(items, text, target_offset, name, best),
                    "for" => scan_for_bindings(items, text, target_offset, name, best),
                    _ => {}
                }
            }
            for item in items {
                collect_bindings(item, text, target_offset, name, best);
            }
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            for item in items {
                collect_bindings(item, text, target_offset, name, best);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_bindings(k, text, target_offset, name, best);
                        collect_bindings(v, text, target_offset, name, best);
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        collect_bindings(f, text, target_offset, name, best);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_bindings(expr, text, target_offset, name, best);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_bindings(expr, text, target_offset, name, best);
                }
            }
        }
        _ => {}
    }
}

fn scan_defn_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    let mut idx = 2;
    let (_, after_doc) = extract_docstring(items, idx, false);
    idx = after_doc;
    for form in items.iter().skip(idx) {
        match &form.kind {
            FormKind::Vector(params) => {
                scan_pattern_vector(params, text, target_offset, name, best)
            }
            FormKind::List(variants) => {
                if let Some(Form {
                    kind: FormKind::Vector(params),
                    ..
                }) = variants.first()
                {
                    scan_pattern_vector(params, text, target_offset, name, best);
                }
            }
            _ => {}
        }
    }
}

fn scan_fn_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    let mut idx = 1;
    if matches!(
        items.get(1),
        Some(Form {
            kind: FormKind::Symbol(_),
            ..
        })
    ) {
        idx = 2;
    }
    for form in items.iter().skip(idx) {
        match &form.kind {
            FormKind::Vector(params) => {
                scan_pattern_vector(params, text, target_offset, name, best)
            }
            FormKind::List(variants) => {
                if let Some(Form {
                    kind: FormKind::Vector(params),
                    ..
                }) = variants.first()
                {
                    scan_pattern_vector(params, text, target_offset, name, best);
                }
            }
            _ => {}
        }
    }
}

fn scan_let_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    if let Some(Form {
        kind: FormKind::Vector(bindings),
        ..
    }) = items.get(1)
    {
        let mut iter = bindings.iter();
        let mut idx = 0;
        let body_start = items
            .get(2)
            .map(|f| span_offset(&f.span, text))
            .unwrap_or_else(|| text.len());
        while let Some(binding) = iter.next() {
            let value = iter.next();
            let next_binding_start = bindings.get(idx + 2).map(|f| span_offset(&f.span, text));
            let value_end = next_binding_start.unwrap_or(body_start);
            if !(value
                .map(|v| {
                    let v_start = span_offset(&v.span, text);
                    target_offset >= v_start && target_offset < value_end
                })
                .unwrap_or(false))
            {
                scan_pattern(binding, text, target_offset, name, best);
            }
            idx += 2;
        }
    }
}

fn scan_if_let_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    if let Some(Form {
        kind: FormKind::Vector(bindings),
        ..
    }) = items.get(1)
    {
        let mut iter = bindings.iter();
        if let (Some(binding), Some(value)) = (iter.next(), iter.next()) {
            let value_start = span_offset(&value.span, text);
            let value_end = items
                .get(2)
                .map(|f| span_offset(&f.span, text))
                .unwrap_or_else(|| text.len());
            if !(target_offset >= value_start && target_offset < value_end) {
                scan_pattern(binding, text, target_offset, name, best);
            }
            collect_pattern_value(value, text, target_offset, name, best);
        }
    }
}

fn scan_dotimes_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    if let Some(Form {
        kind: FormKind::Vector(bindings),
        ..
    }) = items.get(1)
    {
        if bindings.len() >= 2 {
            let value = &bindings[1];
            let value_start = span_offset(&value.span, text);
            let value_end = items
                .get(2)
                .map(|f| span_offset(&f.span, text))
                .unwrap_or_else(|| text.len());
            if !(target_offset >= value_start && target_offset < value_end) {
                scan_pattern(&bindings[0], text, target_offset, name, best);
            }
            collect_pattern_value(&bindings[1], text, target_offset, name, best);
        }
    }
}

fn scan_doseq_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    if let Some(Form {
        kind: FormKind::Vector(bindings),
        ..
    }) = items.get(1)
    {
        let mut idx = 0;
        while idx + 1 < bindings.len() {
            let pat = &bindings[idx];
            let val = &bindings[idx + 1];
            let val_start = span_offset(&val.span, text);
            let val_end = bindings
                .get(idx + 2)
                .map(|f| span_offset(&f.span, text))
                .or_else(|| items.get(2).map(|f| span_offset(&f.span, text)))
                .unwrap_or_else(|| text.len());
            if !(target_offset >= val_start && target_offset < val_end) {
                scan_pattern(pat, text, target_offset, name, best);
            }
            collect_pattern_value(val, text, target_offset, name, best);
            idx += 2;
        }
    }
}

fn scan_for_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 2 {
        return;
    }
    if let Some(Form {
        kind: FormKind::Vector(bindings),
        ..
    }) = items.get(1)
    {
        let mut idx = 0;
        while idx < bindings.len() {
            let binding = &bindings[idx];
            match &binding.kind {
                FormKind::Keyword(kw) if kw == "let" => {
                    if let Some(Form {
                        kind: FormKind::Vector(inner),
                        ..
                    }) = bindings.get(idx + 1)
                    {
                        let mut j = 0;
                        while j + 1 < inner.len() {
                            let val = &inner[j + 1];
                            let val_start = span_offset(&val.span, text);
                            let val_end = inner
                                .get(j + 2)
                                .map(|f| span_offset(&f.span, text))
                                .or_else(|| items.get(2).map(|f| span_offset(&f.span, text)))
                                .unwrap_or_else(|| text.len());
                            if !(target_offset >= val_start && target_offset < val_end) {
                                scan_pattern(&inner[j], text, target_offset, name, best);
                            }
                            collect_pattern_value(&inner[j + 1], text, target_offset, name, best);
                            j += 2;
                        }
                    }
                    idx += 2;
                    continue;
                }
                FormKind::Keyword(kw) if kw == "when" || kw == "while" => {
                    if let Some(test) = bindings.get(idx + 1) {
                        collect_pattern_value(test, text, target_offset, name, best);
                    }
                    idx += 2;
                    continue;
                }
                _ => {
                    if let Some(coll) = bindings.get(idx + 1) {
                        collect_pattern_value(coll, text, target_offset, name, best);
                    }
                    scan_pattern(binding, text, target_offset, name, best);
                    idx += 2;
                }
            }
        }
    }
}

fn scan_match_bindings(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    if items.len() < 3 {
        return;
    }
    let mut idx = 2;
    while idx + 1 < items.len() {
        let pattern = &items[idx];
        let value = &items[idx + 1];
        let value_start = span_offset(&value.span, text);
        let value_end = items
            .get(idx + 2)
            .map(|f| span_offset(&f.span, text))
            .unwrap_or_else(|| text.len());
        if target_offset >= value_start && target_offset < value_end {
            scan_pattern(pattern, text, target_offset, name, best);
        }
        collect_bindings(value, text, target_offset, name, best);
        idx += 2;
    }
}

fn collect_pattern_value(
    form: &Form,
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    match &form.kind {
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            for item in items {
                collect_bindings(item, text, target_offset, name, best);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    clove_core::ast::MapItem::KeyValue(k, v) => {
                        collect_bindings(k, text, target_offset, name, best);
                        collect_bindings(v, text, target_offset, name, best);
                    }
                    clove_core::ast::MapItem::Spread(f) => {
                        collect_bindings(f, text, target_offset, name, best);
                    }
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_bindings(expr, text, target_offset, name, best);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let clove_core::ast::InterpolatedPart::Expr(expr) = part {
                    collect_bindings(expr, text, target_offset, name, best);
                }
            }
        }
        _ => {
            collect_bindings(form, text, target_offset, name, best);
        }
    }
}

fn scan_pattern(
    form: &Form,
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    match &form.kind {
        FormKind::Symbol(sym) => consider_binding(sym, &form.span, text, target_offset, name, best),
        FormKind::Vector(items) => scan_pattern_vector(items, text, target_offset, name, best),
        FormKind::Map(entries) => scan_pattern_map(entries, text, target_offset, name, best),
        _ => {}
    }
}

fn scan_pattern_vector(
    items: &[Form],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    let mut expect_rest = false;
    let mut expect_as = false;
    for item in items {
        match &item.kind {
            FormKind::Symbol(sym) if sym == "&" => {
                expect_rest = true;
                continue;
            }
            FormKind::Symbol(sym) if sym == ":as" => {
                expect_as = true;
                continue;
            }
            _ => {}
        }
        if expect_rest || expect_as {
            if let FormKind::Symbol(sym) = &item.kind {
                consider_binding(sym, &item.span, text, target_offset, name, best);
            }
            expect_rest = false;
            expect_as = false;
            continue;
        }
        scan_pattern(item, text, target_offset, name, best);
    }
}

fn scan_pattern_map(
    entries: &[clove_core::ast::MapItem],
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    for entry in entries {
        match entry {
            clove_core::ast::MapItem::KeyValue(k, v) => match &k.kind {
                FormKind::Keyword(kw) if kw == "as" || kw == ":as" => {
                    if let FormKind::Symbol(sym) = &v.kind {
                        consider_binding(sym, &v.span, text, target_offset, name, best);
                    }
                    continue;
                }
                FormKind::Keyword(kw) if kw == "keys" || kw == ":keys" => {
                    if let FormKind::Vector(vec) = &v.kind {
                        for item in vec {
                            if let FormKind::Symbol(sym) = &item.kind {
                                consider_binding(sym, &item.span, text, target_offset, name, best);
                            }
                        }
                    }
                }
                FormKind::Keyword(kw)
                    if kw == "syms" || kw == ":syms" || kw == "strs" || kw == ":strs" =>
                {
                    if let FormKind::Vector(vec) = &v.kind {
                        for item in vec {
                            if let FormKind::Symbol(sym) = &item.kind {
                                consider_binding(sym, &item.span, text, target_offset, name, best);
                            }
                        }
                    }
                }
                FormKind::Keyword(_) => {
                    if let FormKind::Symbol(sym) = &v.kind {
                        consider_binding(sym, &v.span, text, target_offset, name, best);
                    } else {
                        scan_pattern(v, text, target_offset, name, best);
                    }
                }
                _ => {
                    scan_pattern(v, text, target_offset, name, best);
                }
            },
            clove_core::ast::MapItem::Spread(f) => {
                scan_pattern(f, text, target_offset, name, best);
            }
        }
    }
}

fn consider_binding(
    sym: &str,
    span: &Span,
    text: &str,
    target_offset: usize,
    name: &str,
    best: &mut Option<(usize, Range)>,
) {
    let sym_canon = canonical_symbol_name(sym);
    let name_canon = canonical_symbol_name(name);
    if sym_canon.as_ref() != name_canon.as_ref() {
        return;
    }
    let offset = span_offset(span, text);
    if offset > target_offset {
        return;
    }
    let range = span_to_range(*span);
    if best.as_ref().map(|(idx, _)| *idx).unwrap_or(0) <= offset {
        *best = Some((offset, range));
    }
}

fn span_offset(span: &Span, text: &str) -> usize {
    position_to_offset(
        text,
        Position {
            line: span.line.saturating_sub(1) as u32,
            character: span.col.saturating_sub(1) as u32,
        },
    )
}

#[allow(deprecated)]
fn workspace_roots_from_initialize(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    } else if let Some(root_uri) = &params.root_uri {
        if let Ok(path) = root_uri.to_file_path() {
            roots.push(path);
        }
    } else if let Some(root_path) = &params.root_path {
        roots.push(PathBuf::from(root_path));
    }
    roots
}

fn plugin_dirs_from_initialize(params: &InitializeParams) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let mut has_explicit = false;
    if let Some(options) = &params.initialization_options {
        if let Some(items) = options.get("pluginDirs").and_then(|v| v.as_array()) {
            dirs.extend(parse_plugin_dirs(items));
            has_explicit = true;
        }
        if let Some(items) = options.get("plugin_dirs").and_then(|v| v.as_array()) {
            dirs.extend(parse_plugin_dirs(items));
            has_explicit = true;
        }
    }
    if let Some(env_dirs) = env_plugin_dirs() {
        dirs.extend(env_dirs);
        has_explicit = true;
    }
    let roots = workspace_roots_from_initialize(params);
    if !has_explicit {
        let mut auto_dirs = plugin_dirs_from_lock(&roots);
        if auto_dirs.is_empty() {
            auto_dirs.extend(global_plugin_dirs());
        }
        dirs.extend(auto_dirs);
    }
    let base = roots.into_iter().next();
    normalize_plugin_dirs(base.as_ref(), dirs)
}

fn parse_plugin_dirs(items: &[serde_json::Value]) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for item in items {
        if let Some(path) = item.as_str() {
            if !path.is_empty() {
                out.push(PathBuf::from(path));
            }
        }
    }
    out
}

fn env_plugin_dirs() -> Option<Vec<PathBuf>> {
    let raw = std::env::var_os("CLOVE_PLUGIN_DIR")?;
    let mut dirs: Vec<PathBuf> = std::env::split_paths(&raw).collect();
    dirs.retain(|dir| !dir.as_os_str().is_empty());
    if dirs.is_empty() {
        None
    } else {
        Some(dirs)
    }
}

fn normalize_plugin_dirs(base: Option<&PathBuf>, dirs: Vec<PathBuf>) -> Vec<PathBuf> {
    let base_dir = base
        .cloned()
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for dir in dirs {
        let normalized = if dir.is_absolute() {
            dir
        } else {
            base_dir.join(dir)
        };
        if seen.insert(normalized.clone()) {
            out.push(normalized);
        }
    }
    out
}

const LOCK_FILE: &str = "clove.lock.json";

#[derive(Debug, Deserialize)]
struct LockFile {
    #[serde(default)]
    deps: BTreeMap<String, LockDep>,
}

#[derive(Debug, Deserialize)]
struct LockDep {
    origin_url: String,
    commit: String,
}

fn plugin_dirs_from_lock(roots: &[PathBuf]) -> Vec<PathBuf> {
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    let mut project_roots = HashSet::new();
    for root in roots {
        if let Some(project_root) = find_project_root(root) {
            project_roots.insert(project_root);
        }
    }
    if project_roots.is_empty() {
        return out;
    }
    let clove_home = clove_home();
    let registry = match load_registry(&clove_home) {
        Ok(registry) => registry,
        Err(_) => return out,
    };
    let platform = plugin_platform_tag();
    for project_root in project_roots {
        let lock_path = project_root.join(LOCK_FILE);
        let lock = match read_lock_file(&lock_path) {
            Ok(lock) => lock,
            Err(_) => continue,
        };
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
            if platform_dir.is_dir() && seen.insert(platform_dir.clone()) {
                out.push(platform_dir);
            }
            if plugin_root.is_dir() && seen.insert(plugin_root.clone()) {
                out.push(plugin_root);
            }
        }
    }
    out
}

fn find_project_root(start_dir: &PathBuf) -> Option<PathBuf> {
    let mut current: &Path = start_dir.as_path();
    loop {
        let lock_path = current.join(LOCK_FILE);
        if lock_path.is_file() {
            return Some(current.to_path_buf());
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => return None,
        }
    }
}

fn read_lock_file(path: &Path) -> std::result::Result<LockFile, String> {
    let content = fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    let lock: LockFile = serde_json::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", path.display(), err))?;
    Ok(lock)
}

fn plugin_platform_tag() -> String {
    format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

fn global_plugin_dirs() -> Vec<PathBuf> {
    let mut out = Vec::new();
    let root = clove_home().join("plugins");
    if root.is_dir() {
        out.push(root);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn code_action_params_for_range(uri: &Url, range: Range) -> CodeActionParams {
        CodeActionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            range,
            context: CodeActionContext {
                diagnostics: Vec::new(),
                only: None,
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        }
    }

    fn edits_from_action(action: &CodeAction, uri: &Url) -> Vec<TextEdit> {
        let edit = action.edit.as_ref().expect("action edit");
        let changes = edit.changes.as_ref().expect("changes");
        changes.get(uri).cloned().unwrap_or_default()
    }

    fn apply_text_edits(text: &str, edits: &[TextEdit]) -> String {
        let mut out = text.to_string();
        let mut sorted = edits.to_vec();
        sorted.sort_by(|a, b| {
            let a_start = position_to_offset(text, a.range.start);
            let b_start = position_to_offset(text, b.range.start);
            b_start.cmp(&a_start)
        });
        for edit in sorted {
            let start = position_to_offset(text, edit.range.start);
            let end = position_to_offset(text, edit.range.end);
            out.replace_range(start..end, &edit.new_text);
        }
        out
    }

    #[test]
    fn builtin_stubs_available_for_definition_lookup() {
        let store = DocumentStore::default();
        let locations = store.find_definitions("println", None);
        assert!(
            locations
                .iter()
                .any(|loc| loc.uri.path().contains("builtin_stubs")),
            "expected builtin stub location, got {locations:?}"
        );
    }

    #[test]
    fn word_range_includes_namespaces_and_punctuation() {
        let line = "(println (cli::argv) (map? v?) (core::println))";
        let idx = line.find("cli::argv").unwrap() + 4;
        let (s1, e1) = find_word_range(line, idx);
        assert_eq!(slice_by_char_indices(line, s1, e1).unwrap(), "cli::argv");

        let idx2 = line.find("map?").unwrap() + 3;
        let (s2, e2) = find_word_range(line, idx2);
        assert_eq!(slice_by_char_indices(line, s2, e2).unwrap(), "map?");

        let idx3 = line.find("core::println").unwrap() + 8;
        let (s3, e3) = find_word_range(line, idx3);
        assert_eq!(
            slice_by_char_indices(line, s3, e3).unwrap(),
            "core::println"
        );
    }

    #[test]
    fn symbol_range_skips_oop_suffix() {
        let text = "rolled:position.0";
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let range = symbol_range_from_span(text, &span).expect("range");
        let start = range.start.character as usize;
        let end = range.end.character as usize;
        assert_eq!(slice_by_char_indices(text, start, end).unwrap(), "rolled");
    }

    #[test]
    fn oop_chain_base_symbol_is_extracted() {
        let base = oop_chain_base_symbol("rolled:position.0").expect("base");
        assert_eq!(base, "rolled");
        assert!(oop_chain_base_symbol("foo::bar").is_none());
    }

    #[test]
    fn normalize_keyword_rename_adds_colon() {
        assert_eq!(
            normalize_keyword_rename("position").as_deref(),
            Some(":position")
        );
        assert_eq!(
            normalize_keyword_rename(":position").as_deref(),
            Some(":position")
        );
    }

    #[test]
    fn token_at_position_finds_keyword_in_oop_chain() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///oop.clv").unwrap();
        let source = "rolled:position.0";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let position = Position {
            line: 0,
            character: "rolled:pos".len() as u32,
        };
        let target = token_at_position(doc, position).expect("token");
        match target {
            TokenAtPosition::Keyword { name, .. } => {
                assert_eq!(name, "position");
            }
            _ => panic!("expected keyword token"),
        }
    }

    #[test]
    fn indexer_completion_suggests_nested_map_keys() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///assets.clv").unwrap();
        let source = "(def assets {:cfg {:title 1 :w 2} :colors {:bg 0}})\nassets:cfg:title";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let position = Position {
            line: 1,
            character: "assets:cfg:".len() as u32,
        };
        let ctx = indexer_completion_context(&doc.text, position).expect("context");
        let items = indexer_completion_items(&ctx, &store);
        let labels: Vec<String> = items.into_iter().map(|item| item.label).collect();
        assert!(labels.contains(&":title".to_string()));
        assert!(labels.contains(&":w".to_string()));
    }

    #[test]
    fn signature_context_tracks_argument_positions() {
        let text = "(interleave foo bar baz)";
        let head_pos = "(interleave ".len() as u32;
        let ctx = signature_context(
            text,
            Position {
                line: 0,
                character: head_pos,
            },
        )
        .unwrap();
        assert_eq!(ctx.head, "interleave");
        assert_eq!(ctx.argument_index, 0);

        let second_pos = "(interleave foo ".len() as u32;
        let ctx2 = signature_context(
            text,
            Position {
                line: 0,
                character: second_pos,
            },
        )
        .unwrap();
        assert_eq!(ctx2.head, "interleave");
        assert_eq!(ctx2.argument_index, 1);
    }

    #[test]
    fn plugin_dirs_from_lock_uses_registry_install() {
        let temp = tempfile::tempdir().expect("tempdir");
        let project_root = temp.path().join("project");
        std::fs::create_dir_all(&project_root).expect("create project root");
        let lock_path = project_root.join(LOCK_FILE);
        let lock = r#"
{
  "deps": {
    "owner/pkg": {
      "origin_url": "https://example.com/owner/pkg",
      "commit": "abc123"
    }
  }
}
"#;
        std::fs::write(&lock_path, lock).expect("write lock");

        let clove_home_dir = temp.path().join("clove_home");
        let registry_dir = clove_home_dir.join("pkgs");
        std::fs::create_dir_all(&registry_dir).expect("create registry dir");
        let install_dir = registry_dir.join("owner").join("pkg").join("abc123");
        let plugin_dir = install_dir.join("plugins").join(plugin_platform_tag());
        std::fs::create_dir_all(&plugin_dir).expect("create plugin dir");

        let registry_json = format!(
            r#"{{
  "packages": {{
    "owner/pkg": {{
      "origin_url": "https://example.com/owner/pkg",
      "installs": {{
        "abc123": {{
          "commit": "abc123",
          "path": "{path}",
          "src": "{src}",
          "installed_at": 0,
          "rev_spec": null
        }}
      }}
    }}
  }}
}}"#,
            path = install_dir.to_string_lossy().replace('\\', "\\\\"),
            src = install_dir
                .join("src")
                .to_string_lossy()
                .replace('\\', "\\\\"),
        );
        std::fs::create_dir_all(install_dir.join("src")).expect("create src dir");
        std::fs::write(registry_dir.join("registry.json"), registry_json).expect("write registry");

        let original_home = std::env::var_os("CLOVE_HOME");
        std::env::set_var("CLOVE_HOME", &clove_home_dir);
        let dirs = plugin_dirs_from_lock(&[project_root]);
        if let Some(value) = original_home {
            std::env::set_var("CLOVE_HOME", value);
        } else {
            std::env::remove_var("CLOVE_HOME");
        }

        assert!(
            dirs.contains(&plugin_dir),
            "expected plugin dir in {dirs:?}"
        );
    }

    #[test]
    fn multi_arity_defn_provides_params() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///multi.clv").unwrap();
        let source = r#"
        (defn f10
          "doc"
          ([] 0)
          ([x] x))
        "#;
        store.open_or_update(uri, source.to_string());
        let info = store.lookup_user_symbol_info("f10").expect("symbol");
        assert!(info.params.is_some());
    }

    #[test]
    fn format_doc_markdown_lists_aliases() {
        let entry = DocEntry {
            name: "join".to_string(),
            canonical: "core::join".to_string(),
            signature: Some("join [coll sep]".to_string()),
            doc: Some("Join elements.".to_string()),
            origin: Some("core".to_string()),
            examples: vec![],
            oop_examples: vec![],
        };
        let markdown = format_doc_markdown(&entry).expect("markdown");
        assert!(
            markdown.contains("_Aliases: join_"),
            "expected alias list, got {}",
            markdown
        );
    }

    #[test]
    fn diagnostics_reports_unbound_symbol() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///unbound.clv").unwrap();
        let source = "(ns sample::test)\n(defn f [x] (+ x y))";
        store.open_or_update(uri.clone(), source.to_string());
        let requires = {
            let doc = store.get(&uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unbound symbol: y")),
            "expected unbound diagnostic, got {diags:?}"
        );
    }

    #[test]
    fn diagnostics_reports_unknown_namespace_require() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///missing_ns.clv").unwrap();
        let source = "(ns sample::missing)\n(require no::such::ns refer *)";
        store.open_or_update(uri.clone(), source.to_string());
        let requires = {
            let doc = store.get(&uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown namespace: no::such::ns")),
            "expected unknown namespace diagnostic, got {diags:?}"
        );
    }

    #[test]
    fn unknown_namespace_refer_does_not_emit_unbound_for_referred_symbols() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///dag.clv").unwrap();
        let source = r#"
        (ns sample::dag-consumer)
        (require dag refer [graph task run])
        (defn -main []
          (graph)
          (task 1)
          (run))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let requires = {
            let doc = store.get(&uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown namespace: dag")),
            "expected unknown namespace diagnostic, got {diags:?}"
        );
        assert!(
            diags
                .iter()
                .all(|d| !d.message.contains("Unbound symbol")),
            "unexpected unbound diagnostics for referred symbols of unknown namespace, got {diags:?}"
        );
    }

    #[test]
    fn preload_uses_detected_root_even_when_workspace_root_is_slash() {
        use tempfile::tempdir;

        let dir = tempdir().unwrap();
        let proj = dir.path().join("proj");
        std::fs::create_dir_all(proj.join("wk")).unwrap();
        std::fs::write(
            proj.join("Cargo.toml"),
            "[package]\nname=\"demo\"\nversion=\"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            proj.join("wk").join("dag_base.clv"),
            "(ns wk::dag_base)\n(defn graph [] 1)\n",
        )
        .unwrap();
        std::fs::write(
            proj.join("wk").join("dag_all_modes.clv"),
            "(ns wk::dag_all_modes)\n(require wk::dag_base :refer [graph])\n",
        )
        .unwrap();

        let mut store = DocumentStore::default();
        store.workspace_roots = vec![PathBuf::from("/")];
        let uri =
            Url::from_file_path(proj.join("wk").join("dag_all_modes.clv")).expect("uri from path");
        let text = std::fs::read_to_string(proj.join("wk").join("dag_all_modes.clv")).unwrap();
        store.open_or_update(uri.clone(), text);
        let requires = {
            let doc = store.get(&uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        assert!(
            namespace_known(&store, "wk::dag_base"),
            "expected wk::dag_base to be loaded via detected root"
        );
    }

    #[test]
    fn namespace_is_registered_even_if_file_has_parse_error() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///broken.clv").unwrap();
        let source = "(ns broken::file)\n(dag/run (AllMode {:return ReturnValues :...,}) {:a})";
        store.open_or_update(uri.clone(), source.to_string());
        assert!(
            namespace_known(&store, "broken::file"),
            "expected namespace to be registered even on parse error"
        );
        let consumer_uri = Url::parse("file:///consumer.clv").unwrap();
        let consumer_src = "(ns consumer)\n(require broken::file refer [graph])";
        store.open_or_update(consumer_uri.clone(), consumer_src.to_string());
        let doc = store.get(&consumer_uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .all(|d| !d.message.contains("Unknown namespace")),
            "unexpected unknown namespace diagnostics for broken::file, got {diags:?}"
        );
    }

    #[test]
    fn diagnostics_reports_arity_mismatch() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///arity.clv").unwrap();
        let source = "(ns sample::arity)\n(defn g [a b] (+ a b))\n(g 1)";
        store.open_or_update(uri.clone(), source.to_string());
        let requires = {
            let doc = store.get(&uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.iter().any(|d| d.message.contains("Arity mismatch")),
            "expected arity mismatch diagnostic, got {diags:?}"
        );
    }

    #[test]
    fn code_action_suggests_require_for_unbound() {
        let mut store = DocumentStore::default();
        let provider_uri = Url::parse("file:///tools/helpers.clv").unwrap();
        let provider_src = "(ns tools::helpers)\n(defn run-main-loop! [] nil)";
        store.open_or_update(provider_uri, provider_src.to_string());

        let consumer_uri = Url::parse("file:///app/core.clv").unwrap();
        let consumer_src = "(ns app::core)\n(defn main [] (run-main-loop!))";
        store.open_or_update(consumer_uri.clone(), consumer_src.to_string());
        let requires = {
            let doc = store.get(&consumer_uri).expect("doc");
            doc.requires.clone()
        };
        store.preload_requires(&requires);
        let doc = store.get(&consumer_uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        let diag = diags
            .iter()
            .find(|d| d.message.contains("Unbound symbol"))
            .cloned()
            .expect("unbound diagnostic");

        let params = CodeActionParams {
            text_document: TextDocumentIdentifier {
                uri: consumer_uri.clone(),
            },
            range: diag.range,
            context: CodeActionContext {
                diagnostics: vec![diag],
                only: None,
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        let actions = code_actions_for_unbound(&params, doc, &store);
        assert!(
            actions.iter().any(
                |a| a.title.contains("Add require") && a.kind == Some(CodeActionKind::QUICKFIX)
            ),
            "expected require code action, got {actions:?}"
        );
    }

    #[test]
    fn code_action_rewrite_shortest_access_cases() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///rewrite_access.clv").unwrap();
        let cases = vec![
            ("(get x :k \"\")", "x[:k || \"\"]"),
            ("(get-in m [:a :b])", "m:a:b"),
            ("(nth e 2)", "e[2]"),
            ("(:k x)", "x:k"),
            ("m[:a]", "m:a"),
        ];
        for (source, expected) in cases {
            store.open_or_update(uri.clone(), source.to_string());
            let doc = store.get(&uri).expect("doc");
            let range = Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            };
            let params = code_action_params_for_range(&uri, range);
            let actions = code_actions_for_rewrite_access(&params, doc, RefactorPrefs::default());
            assert!(
                actions
                    .iter()
                    .any(|a| a.title.contains("Rewrite to shortest access syntax")),
                "expected refactor action for {source}, got {actions:?}"
            );
            let action = actions
                .iter()
                .find(|a| a.title.contains("Rewrite to shortest access syntax"))
                .expect("action");
            let edits = edits_from_action(action, &uri);
            let updated = apply_text_edits(source, &edits);
            assert_eq!(updated, expected);
        }
    }

    #[test]
    fn code_action_rewrite_shortest_access_selection_multi_form() {
        let source = "(get x :k \"\")\n(nth e 2)";
        let expected = "x[:k || \"\"]\ne[2]";
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///rewrite_access_multi.clv").unwrap();
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: offset_to_position(source, source.len()),
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = code_actions_for_rewrite_access(&params, doc, RefactorPrefs::default());
        assert!(
            actions
                .iter()
                .any(|a| a.title.contains("Rewrite to shortest access syntax")),
            "expected refactor action, got {actions:?}"
        );
        let action = actions
            .iter()
            .find(|a| a.title.contains("Rewrite to shortest access syntax"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(source, &edits);
        assert_eq!(updated, expected);
    }

    #[test]
    fn code_action_rewrite_shortest_access_word_range_uses_enclosing_form() {
        let source = "(get-in m [:a])";
        let expected = "m:a";
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///rewrite_access_word_range.clv").unwrap();
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: offset_to_position(source, 1),
            end: offset_to_position(source, 7),
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = code_actions_for_rewrite_access(&params, doc, RefactorPrefs::default());
        let action = actions
            .iter()
            .find(|a| a.title.contains("Rewrite to shortest access syntax"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(source, &edits);
        assert_eq!(updated, expected);
    }

    #[test]
    fn code_action_rewrite_canonical_access_cases() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///rewrite_canonical.clv").unwrap();
        let cases = vec![
            ("m[:a]", "(get m :a)"),
            ("m:a", "(get m :a)"),
            ("m:a:b", "(get-in m [:a :b])"),
            ("m.0", "(nth m 0)"),
        ];
        for (source, expected) in cases {
            store.open_or_update(uri.clone(), source.to_string());
            let doc = store.get(&uri).expect("doc");
            let range = Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            };
            let params = code_action_params_for_range(&uri, range);
            let actions = code_actions_for_rewrite_canonical_access(&params, doc);
            let action = actions
                .iter()
                .find(|a| a.title.contains("Rewrite to canonical access syntax"))
                .expect("action");
            let edits = edits_from_action(action, &uri);
            let updated = apply_text_edits(source, &edits);
            assert_eq!(updated, expected);
        }
    }

    #[test]
    fn code_action_toggle_access_cycles_between_forms() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///toggle_access.clv").unwrap();
        let source = "(get m :a)";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = code_actions_for_toggle_access(&params, doc, RefactorPrefs::default());
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle access syntax"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(source, &edits);
        assert_eq!(updated, "m:a");

        store.open_or_update(uri.clone(), updated.to_string());
        let doc = store.get(&uri).expect("doc");
        let params = code_action_params_for_range(&uri, range);
        let actions = code_actions_for_toggle_access(&params, doc, RefactorPrefs::default());
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle access syntax"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let toggled_back = apply_text_edits(&updated, &edits);
        assert_eq!(toggled_back, "(get m :a)");
    }

    #[test]
    fn code_action_toggle_oop_converts_sexp_and_chain() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///toggle_oop.clv").unwrap();
        let source = "(map inc xs)";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc));
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle S-exp / OOP"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(source, &edits);
        assert_eq!(updated, "xs.map(inc)");

        let chain_src = "xs.map(inc).filter(pred)";
        store.open_or_update(uri.clone(), chain_src.to_string());
        let doc = store.get(&uri).expect("doc");
        let chain_range = Range {
            start: Position {
                line: 0,
                character: 2,
            },
            end: Position {
                line: 0,
                character: 2,
            },
        };
        let params = code_action_params_for_range(&uri, chain_range);
        let actions = store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc));
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle S-exp / OOP"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(chain_src, &edits);
        assert_eq!(updated, "(filter pred (map inc xs))");

        let access_chain_src = "xs:items.map(inc)";
        store.open_or_update(uri.clone(), access_chain_src.to_string());
        let doc = store.get(&uri).expect("doc");
        let access_chain_range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };
        let params = code_action_params_for_range(&uri, access_chain_range);
        let actions = store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc));
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle S-exp / OOP"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(access_chain_src, &edits);
        assert_eq!(updated, "(map inc (get xs :items))");
    }

    #[test]
    fn code_action_toggle_oop_selection_multi_form() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///toggle_oop_multi.clv").unwrap();
        let source = "(map inc xs)\n(filter pred ys)";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: offset_to_position(source, source.len()),
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc));
        let action = actions
            .iter()
            .find(|a| a.title.contains("Toggle S-exp / OOP"))
            .expect("action");
        let edits = edits_from_action(action, &uri);
        let updated = apply_text_edits(source, &edits);
        assert_eq!(updated, "xs.map(inc)\nys.filter(pred)");
    }

    #[test]
    fn code_action_toggle_oop_skips_unknown_subject_pos() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///toggle_oop_missing.clv").unwrap();
        let source = "(noop x)";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };
        let params = code_action_params_for_range(&uri, range);
        let actions = store.with_lsp_runtime(|| code_actions_for_toggle_oop(&params, doc));
        assert!(
            actions.is_empty(),
            "expected no toggle action, got {actions:?}"
        );
    }

    #[test]
    fn primitive_type_literal_is_not_unbound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///type_literal.clv").unwrap();
        let source = "(ns sample::types)\n(deftype KeyEvent {:key Str})";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.iter().all(|d| !d.message.contains("Str")),
            "expected no diagnostics for Str, got {diags:?}"
        );
    }

    #[test]
    fn typed_param_is_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///typed_param.clv").unwrap();
        let source = "(ns sample::typed)\n(defn f [x<Int>] x)";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for typed params, got {diags:?}"
        );
    }

    #[test]
    fn defenum_members_are_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///enum_members.clv").unwrap();
        let source = "(ns sample::enum)\n(defenum Directions DirUp DirDown)\n(defn f [] (DirUp))";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for defenum members, got {diags:?}"
        );
    }

    #[test]
    fn match_wildcard_is_not_unbound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///match_wildcard.clv").unwrap();
        let source = "(ns sample::match)\n(defn f [x] (match x _ 0))";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for match wildcard, got {diags:?}"
        );
    }

    #[test]
    fn index_get_is_treated_as_special_form() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///index_get.clv").unwrap();
        let source = "(ns sample::idx)\n(defn f [m] (m[:a]))";
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for index-get lowering, got {diags:?}"
        );
    }

    #[test]
    fn destructuring_params_are_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///destructure.clv").unwrap();
        let source = r#"
        (ns sample::destructure)
        (defn build-frame-string [lines]
          (reduce
            (fn [acc [i line]]
              (let [prefix (if (zero? i) "" "\r\n")]
                (str acc prefix line)))
            ""
            (map-indexed vector lines)))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for destructuring params, got {diags:?}"
        );
    }

    #[test]
    fn doseq_bindings_are_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///doseq.clv").unwrap();
        let source = r#"
        (ns sample::doseq)
        (def snake [[0 1] [1 1]])
        (doseq [segment snake]
          (println segment))
        (doseq [[x y] snake]
          (println x y))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for doseq bindings, got {diags:?}"
        );
    }

    #[test]
    fn when_if_let_and_dotimes_bindings_are_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///letlikes.clv").unwrap();
        let source = r#"
        (ns sample::letlikes)
        (when-let [v (some? 1)]
          (println v))
        (if-let [a (some? 2)]
          (println a)
          (println a))
        (if-some [b (some? 3)]
          (println b)
          (println b))
        (dotimes [i 3]
          (println i))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for let-like bindings, got {diags:?}"
        );
    }

    #[test]
    fn for_bindings_and_let_steps_are_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///for.clv").unwrap();
        let source = r#"
        (ns sample::for)
        (for [x [1 2]
              [a b] [[1 2] [3 4]]
              :let [s (+ a b)]
              :when (< x s)]
          (println x a b s))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for for bindings, got {diags:?}"
        );
    }

    #[test]
    fn catch_binding_is_locally_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///catch.clv").unwrap();
        let source = r#"
        (ns sample::catch)
        (defn -main []
          (try
            (println "ok")
            (catch e
              (println e))
            (catch RuntimeError e
              (println e))))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.iter().all(|d| !d.message.contains("Unbound symbol")),
            "expected catch bindings to be treated as local, got {diags:?}"
        );
    }

    #[test]
    fn local_def_inside_fn_is_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///local_def.clv").unwrap();
        let source = r#"
        (ns sample::local-def)
        (defn -main []
          (-def game_over_str "GAME")
          (-defn aaa [] (println 1))
          (println game_over_str)
          (aaa))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for local def/defn, got {diags:?}"
        );
    }

    #[test]
    fn local_def_inside_try_is_bound() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///local_def_try.clv").unwrap();
        let source = r#"
        (ns sample::local-def-try)
        (defn -main []
          (try
            (-def game_over_str "GAME")
            (println game_over_str)
            (finally 0)))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for local -def in try, got {diags:?}"
        );
    }

    #[test]
    fn def_in_fn_body_is_diagnostic() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///def_in_fn.clv").unwrap();
        let source = r#"
        (ns sample::local-def-error)
        (defn -main []
          (def game_over_str "GAME")
          game_over_str)
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("def is top-level only")),
            "expected diagnostics for local def, got {diags:?}"
        );
    }

    #[test]
    fn defn_in_fn_body_is_diagnostic() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///defn_in_fn.clv").unwrap();
        let source = r#"
        (ns sample::local-defn-error)
        (defn -main []
          (defn aaa [] 1)
          (aaa))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("defn is top-level only")),
            "expected diagnostics for local defn, got {diags:?}"
        );
    }

    #[test]
    fn defn_in_where_is_allowed() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///defn_in_where.clv").unwrap();
        let source = r#"
        (ns sample::local-defn-where)
        (defn -main []
          (where
            (defn inner [] 1))
          (inner))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let diags = compute_diagnostics(&store, doc, DiagnosticsOptions::default());
        assert!(
            diags.is_empty(),
            "expected no diagnostics for defn inside where, got {diags:?}"
        );
    }

    #[test]
    fn completion_params_include_rest_destructuring() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///params_rest.clv").unwrap();
        let source = r#"
        (ns sample::params-rest)
        (defn exdic [globpat target & [{exact,}]] nil)
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let info = store.lookup_user_symbol_info("exdic").expect("symbol info");
        assert_eq!(
            info.params,
            Some(vec![
                "globpat".to_string(),
                "target".to_string(),
                "&".to_string(),
                "[{:exact => exact}]".to_string(),
            ])
        );
    }

    #[test]
    fn completion_params_prefer_variadic_clause() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///params_variadic.clv").unwrap();
        let source = r#"
        (ns sample::params-variadic)
        (defn exdic
          ([globpat target] nil)
          ([globpat target & [{exact,}]] nil))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let info = store.lookup_user_symbol_info("exdic").expect("symbol info");
        assert_eq!(
            info.params,
            Some(vec![
                "globpat".to_string(),
                "target".to_string(),
                "&".to_string(),
                "[{:exact => exact}]".to_string(),
            ])
        );
    }

    #[test]
    fn goto_definition_finds_local_def_inside_fn() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///goto_local.clv").unwrap();
        let source = r#"
        (ns sample::goto)
        (defn -main []
          (-def game_over_str "GAME")
          (println game_over_str))
        "#;
        store.open_or_update(uri.clone(), source.to_string());
        let doc = store.get(&uri).expect("doc");
        let position = Position {
            line: 4,
            character: 19,
        }; // inside game_over_str
        let loc =
            find_local_definition(&uri, doc, position, "game_over_str").expect("local definition");
        assert_eq!(loc.range.start.line, 3);
    }
}
