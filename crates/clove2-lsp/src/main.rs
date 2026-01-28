use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use serde_json::Value;
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DidChangeConfigurationParams, DocumentSymbol,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintKind,
    InlayHintParams, Location, MarkupContent, MarkupKind, MessageType, OneOf, Position, Range,
    ServerCapabilities, SymbolKind, TextDocumentContentChangeEvent, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use clove2_core::ast::{Expr, ExprKind};
use clove2_core::builtins::BUILTIN_NAMES;
use clove2_core::docs::{find_doc_entry, format_doc_entry};
use clove2_core::dynamic_detect::contains_dynamic;
use clove2_core::reader::read_all;
use clove2_core::syntax::parse_forms;
use clove2_core::syntax::TopLevel;
use clove2_core::type_infer::{
    builtin_type_for, check_program, infer_program_with_expr_spans, Diagnostic as TypeDiagnostic,
    DiagnosticLevel, ExprTraceKind, ExprTypeEntry, TypeSummary,
};
use clove2_core::types::Type;
use clove2_core::use_directive::{parse_use_directives, Mode, NativeLevel, UseConfig};

#[derive(Clone, Debug)]
struct LspConfig {
    inlay_expr: bool,
}

impl Default for LspConfig {
    fn default() -> Self {
        Self { inlay_expr: true }
    }
}

fn update_config_from_value(config: &mut LspConfig, value: &Value) {
    apply_inlay_config(config, value);
    if let Some(obj) = value.as_object() {
        if let Some(inner) = obj.get("clove2") {
            apply_inlay_config(config, inner);
        }
    }
}

fn apply_inlay_config(config: &mut LspConfig, value: &Value) {
    let Some(obj) = value.as_object() else {
        return;
    };
    let Some(inlay) = obj.get("inlayHints") else {
        return;
    };
    let Some(inlay_obj) = inlay.as_object() else {
        return;
    };
    if let Some(expr) = inlay_obj.get("expressions").and_then(Value::as_bool) {
        config.inlay_expr = expr;
    }
}

struct Backend {
    client: Client,
    docs: Arc<Mutex<HashMap<Url, String>>>,
    config: Arc<Mutex<LspConfig>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            docs: Arc::new(Mutex::new(HashMap::new())),
            config: Arc::new(Mutex::new(LspConfig::default())),
        }
    }

    async fn analyze_and_publish(&self, uri: Url, text: String) {
        self.docs.lock().await.insert(uri.clone(), text.clone());
        let diags = analyze_text(&text);
        self.client.publish_diagnostics(uri, diags, None).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        params: InitializeParams,
    ) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        if let Some(options) = params.initialization_options {
            let mut config = self.config.lock().await;
            update_config_from_value(&mut config, &options);
        }
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions::default()),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "clove2-lsp: initialized")
            .await;
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let mut config = self.config.lock().await;
        update_config_from_value(&mut config, &params.settings);
    }

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.analyze_and_publish(doc.uri, doc.text).await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = extract_full_text(params.content_changes);
        self.analyze_and_publish(uri, text).await;
    }

    async fn did_close(&self, params: tower_lsp::lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.docs.lock().await.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let text = self
            .docs
            .lock()
            .await
            .get(&uri)
            .cloned()
            .unwrap_or_default();
        let items = build_completion_items(&text, position);
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn hover(&self, params: HoverParams) -> tower_lsp::jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let text = self
            .docs
            .lock()
            .await
            .get(&uri)
            .cloned()
            .unwrap_or_default();
        let symbol = symbol_at(&text, position);
        if symbol.is_empty() {
            return Ok(None);
        }
        let signatures = collect_signatures(&text);
        if let Some(signature) = signatures.get(&symbol) {
            return Ok(Some(build_hover_text(signature.clone())));
        }
        if let Some(summary) = infer_summary(&text) {
            if let Some(ty) = summary.values.get(&symbol) {
                let label = signature_from_type(&symbol, ty)
                    .unwrap_or_else(|| format!("{}: {}", symbol, ty));
                return Ok(Some(build_hover_text(label)));
            }
            if let Some(fields) = summary.named_types.get(&symbol) {
                let label = format_named_type_block(&symbol, fields);
                return Ok(Some(build_hover_text(label)));
            }
        }
        if let Some(entry) = find_doc_entry(&symbol) {
            if let Some(text) = format_doc_entry(entry) {
                return Ok(Some(build_hover_text(text)));
            }
        }
        if let Some(ty) = builtin_type_for(&symbol) {
            let label =
                signature_from_type(&symbol, &ty).unwrap_or_else(|| format!("{}: {}", symbol, ty));
            return Ok(Some(build_hover_text(label)));
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let text = self
            .docs
            .lock()
            .await
            .get(&uri)
            .cloned()
            .unwrap_or_default();
        let symbol = symbol_at(&text, position);
        if symbol.is_empty() {
            return Ok(None);
        }
        let defs = collect_definitions(&text);
        for def in defs {
            if def.name == symbol {
                let range = span_to_range(&text, &def.span);
                let location = Location::new(uri, range);
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> tower_lsp::jsonrpc::Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let text = self
            .docs
            .lock()
            .await
            .get(&uri)
            .cloned()
            .unwrap_or_default();
        let symbols = build_document_symbols(&text);
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn inlay_hint(
        &self,
        params: InlayHintParams,
    ) -> tower_lsp::jsonrpc::Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let range = params.range;
        let text = self
            .docs
            .lock()
            .await
            .get(&uri)
            .cloned()
            .unwrap_or_default();
        let config = self.config.lock().await.clone();
        let mut hints = inlay_hints_for_doc(&text, &config);
        hints.retain(|hint| position_in_range(hint.position, &range));
        Ok(Some(hints))
    }
}

fn extract_full_text(changes: Vec<TextDocumentContentChangeEvent>) -> String {
    changes
        .into_iter()
        .last()
        .map(|change| change.text)
        .unwrap_or_default()
}

fn analyze_text(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let forms = match read_all(text) {
        Ok(forms) => forms,
        Err(err) => {
            diags.push(error_diag(err.to_string()));
            return diags;
        }
    };

    let use_cfg = match parse_use_directives(&forms) {
        Ok((cfg, _)) => cfg,
        Err(err) => {
            diags.push(error_diag(err.to_string()));
            UseConfig::default()
        }
    };
    let dynamic = contains_dynamic(&forms);
    if matches!(use_cfg.mode, Some(Mode::Native)) && dynamic {
        diags.push(error_diag(
            "dynamic features detected in native mode".to_string(),
        ));
    }
    if matches!(use_cfg.mode, Some(Mode::Dynamic)) && !dynamic {
        diags.push(warn_diag(
            "dynamic mode is set but no dynamic features detected".to_string(),
        ));
    }

    let parsed = match parse_forms(&forms) {
        Ok(parsed) => parsed,
        Err(err) => {
            diags.push(error_diag(err.to_string()));
            return diags;
        }
    };

    let level = use_cfg.native_level.unwrap_or(NativeLevel::Strict);
    for diag in check_program(&parsed, level) {
        diags.push(type_diag_to_lsp(diag, text));
    }
    diags
}

fn infer_summary(text: &str) -> Option<clove2_core::type_infer::TypeSummary> {
    let forms = read_all(text).ok()?;
    infer_summary_from_forms(&forms)
}

fn infer_summary_from_forms(forms: &[Expr]) -> Option<TypeSummary> {
    let use_cfg = parse_use_directives(forms).ok().map(|(cfg, _)| cfg)?;
    let parsed = parse_forms(forms).ok()?;
    let level = use_cfg.native_level.unwrap_or(NativeLevel::Strict);
    let (summary, _) = infer_program_with_expr_spans(&parsed, forms, level);
    Some(summary)
}

fn build_hover_text(value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::PlainText,
            value,
        }),
        range: None,
    }
}

fn build_completion_items(text: &str, position: Position) -> Vec<CompletionItem> {
    let prefix = symbol_prefix_at(text, position);
    let summary = infer_summary(text);
    let signatures = collect_signatures(text);
    let mut seen = HashSet::new();
    let mut items = Vec::new();
    for name in BUILTIN_NAMES {
        if name.starts_with("__") {
            continue;
        }
        if !prefix.is_empty() && !name.starts_with(&prefix) {
            continue;
        }
        let detail = builtin_type_for(name)
            .map(|ty| signature_from_type(name, &ty).unwrap_or_else(|| format_type_for_lsp(&ty)));
        let documentation = find_doc_entry(name)
            .and_then(format_doc_entry)
            .map(Documentation::String);
        if seen.insert((*name).to_string()) {
            items.push(CompletionItem {
                label: (*name).to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail,
                documentation,
                ..CompletionItem::default()
            });
        }
    }
    for (name, kind) in collect_top_level_symbols(text) {
        if !prefix.is_empty() && !name.starts_with(&prefix) {
            continue;
        }
        if seen.insert(name.clone()) {
            let detail = match kind {
                CompletionItemKind::FUNCTION => signatures.get(&name).cloned().or_else(|| {
                    summary
                        .as_ref()
                        .and_then(|summary| summary.values.get(&name).map(format_type_for_lsp))
                }),
                CompletionItemKind::CLASS => summary.as_ref().and_then(|summary| {
                    summary
                        .named_types
                        .get(&name)
                        .map(|fields| format_named_type_inline(&name, fields))
                }),
                _ => summary
                    .as_ref()
                    .and_then(|summary| summary.values.get(&name).map(format_type_for_lsp)),
            };
            items.push(CompletionItem {
                label: name,
                kind: Some(kind),
                detail,
                ..CompletionItem::default()
            });
        }
    }
    items
}

struct DefinitionEntry {
    name: String,
    kind: SymbolKind,
    span: clove2_core::ast::Span,
}

fn collect_definitions(text: &str) -> Vec<DefinitionEntry> {
    let forms = match read_all(text) {
        Ok(forms) => forms,
        Err(_) => return Vec::new(),
    };
    let parsed = match parse_forms(&forms) {
        Ok(parsed) => parsed,
        Err(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    for item in parsed {
        match item {
            TopLevel::Def { name, span, .. } => out.push(DefinitionEntry {
                name,
                kind: SymbolKind::VARIABLE,
                span,
            }),
            TopLevel::Defn { name, span, .. } => out.push(DefinitionEntry {
                name,
                kind: SymbolKind::FUNCTION,
                span,
            }),
            TopLevel::DefType { name, span, .. } => out.push(DefinitionEntry {
                name,
                kind: SymbolKind::CLASS,
                span,
            }),
            TopLevel::DefForeign { decl, span } => out.push(DefinitionEntry {
                name: decl.name,
                kind: SymbolKind::FUNCTION,
                span,
            }),
            TopLevel::Expr { .. } => {}
        }
    }
    out
}

fn build_document_symbols(text: &str) -> Vec<DocumentSymbol> {
    let defs = collect_definitions(text);
    let summary = infer_summary(text);
    let signatures = collect_signatures(text);
    let mut out = Vec::new();
    for def in defs {
        let range = span_to_range(text, &def.span);
        let detail = match def.kind {
            SymbolKind::FUNCTION => signatures.get(&def.name).cloned().or_else(|| {
                summary
                    .as_ref()
                    .and_then(|summary| summary.values.get(&def.name).map(format_type_for_lsp))
            }),
            SymbolKind::CLASS => summary.as_ref().and_then(|summary| {
                summary
                    .named_types
                    .get(&def.name)
                    .map(|fields| format_named_type_inline(&def.name, fields))
            }),
            _ => summary
                .as_ref()
                .and_then(|summary| summary.values.get(&def.name).map(format_type_for_lsp)),
        };
        #[allow(deprecated)]
        out.push(DocumentSymbol {
            name: def.name,
            detail,
            kind: def.kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        });
    }
    out
}

fn inlay_hints_for_doc(text: &str, config: &LspConfig) -> Vec<InlayHint> {
    let forms = match read_all(text) {
        Ok(forms) => forms,
        Err(_) => return Vec::new(),
    };
    let summary = match infer_summary_from_forms(&forms) {
        Some(summary) => summary,
        None => return Vec::new(),
    };
    let mut out = Vec::new();
    for form in forms {
        collect_inlay_hints(&form, text, &summary, &mut out);
    }
    if config.inlay_expr {
        for entry in &summary.expr_types {
            if !should_render_expr_hint(entry) {
                continue;
            }
            let position = position_at(text, entry.span.end);
            out.push(make_type_inlay_hint(
                position,
                &format_type_for_lsp(&entry.ty),
            ));
        }
    }
    out
}

fn should_render_expr_hint(entry: &ExprTypeEntry) -> bool {
    match entry.kind {
        ExprTraceKind::Literal | ExprTraceKind::Symbol | ExprTraceKind::Keyword => false,
        _ => true,
    }
}

fn collect_inlay_hints(form: &Expr, text: &str, summary: &TypeSummary, out: &mut Vec<InlayHint>) {
    let ExprKind::List(items) = &form.kind else {
        return;
    };
    let Some(head) = list_head_symbol(items) else {
        return;
    };
    match head {
        "def" => collect_def_inlay(items, text, summary, out),
        "defn" => collect_defn_return_inlay(items, text, summary, out),
        _ => {}
    }
}

fn list_head_symbol(items: &[Expr]) -> Option<&str> {
    match items.first() {
        Some(Expr {
            kind: ExprKind::Symbol(sym),
            ..
        }) => Some(sym.as_str()),
        _ => None,
    }
}

fn collect_def_inlay(items: &[Expr], text: &str, summary: &TypeSummary, out: &mut Vec<InlayHint>) {
    if items.len() < 3 {
        return;
    }
    let name_form = &items[1];
    let ExprKind::Symbol(name) = &name_form.kind else {
        return;
    };
    if name.ends_with(':') {
        return;
    }
    let Some(ty) = summary.values.get(name) else {
        return;
    };
    let position = position_at(text, name_form.span.end);
    out.push(make_type_inlay_hint(position, &format_type_for_lsp(ty)));
}

fn collect_defn_return_inlay(
    items: &[Expr],
    text: &str,
    summary: &TypeSummary,
    out: &mut Vec<InlayHint>,
) {
    if items.len() < 4 {
        return;
    }
    let name_form = &items[1];
    let ExprKind::Symbol(name) = &name_form.kind else {
        return;
    };
    if let Some(Expr {
        kind: ExprKind::Symbol(sym),
        ..
    }) = items.get(3)
    {
        if sym == "->" {
            return;
        }
    }
    let Some(ty) = summary.values.get(name) else {
        return;
    };
    let Type::Function { ret, .. } = ty else {
        return;
    };
    let params_form = &items[2];
    let position = position_at(text, params_form.span.end);
    out.push(make_return_inlay_hint(position, &format_type_for_lsp(ret)));
}

fn collect_top_level_symbols(text: &str) -> Vec<(String, CompletionItemKind)> {
    let forms = match read_all(text) {
        Ok(forms) => forms,
        Err(_) => return Vec::new(),
    };
    let parsed = match parse_forms(&forms) {
        Ok(parsed) => parsed,
        Err(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    for item in parsed {
        match item {
            TopLevel::Def { name, .. } => out.push((name, CompletionItemKind::VARIABLE)),
            TopLevel::Defn { name, .. } => out.push((name, CompletionItemKind::FUNCTION)),
            TopLevel::DefType { name, .. } => out.push((name, CompletionItemKind::CLASS)),
            TopLevel::DefForeign { decl, .. } => {
                out.push((decl.name, CompletionItemKind::FUNCTION))
            }
            TopLevel::Expr { .. } => {}
        }
    }
    out
}

fn collect_signatures(text: &str) -> HashMap<String, String> {
    let forms = match read_all(text) {
        Ok(forms) => forms,
        Err(_) => return HashMap::new(),
    };
    let parsed = match parse_forms(&forms) {
        Ok(parsed) => parsed,
        Err(_) => return HashMap::new(),
    };
    let mut out = HashMap::new();
    for item in parsed {
        match item {
            TopLevel::Defn {
                name, params, ret, ..
            } => {
                let sig = signature_from_params(&name, &params, ret.as_ref());
                out.insert(name, sig);
            }
            TopLevel::DefForeign { decl, .. } => {
                let sig = signature_from_params(&decl.name, &decl.params, Some(&decl.ret));
                out.insert(decl.name, sig);
            }
            _ => {}
        }
    }
    out
}

fn signature_from_params(
    name: &str,
    params: &[clove2_core::syntax::Param],
    ret: Option<&Type>,
) -> String {
    let mut parts = Vec::new();
    for param in params {
        let ty = param.ty.clone().unwrap_or(Type::Any);
        if param.rest {
            let rest_ty = ensure_vec_type(ty);
            parts.push(format!(
                "&{}: {}",
                param.name,
                format_type_for_lsp(&rest_ty)
            ));
        } else {
            parts.push(format!("{}: {}", param.name, format_type_for_lsp(&ty)));
        }
    }
    let ret = ret.cloned().unwrap_or(Type::Any);
    format!(
        "{}({}) -> {}",
        name,
        parts.join(", "),
        format_type_for_lsp(&ret)
    )
}

fn signature_from_type(name: &str, ty: &Type) -> Option<String> {
    if let Type::Function { params, rest, ret } = ty {
        let mut parts: Vec<String> = params.iter().map(format_type_for_lsp).collect();
        if let Some(rest_ty) = rest.as_ref() {
            parts.push(format!("&{}", format_type_for_lsp(rest_ty)));
        }
        return Some(format!(
            "{}({}) -> {}",
            name,
            parts.join(", "),
            format_type_for_lsp(ret)
        ));
    }
    None
}

fn ensure_vec_type(ty: Type) -> Type {
    match ty {
        Type::Vec(_) => ty,
        other => Type::Vec(Box::new(other)),
    }
}

fn format_named_type_inline(name: &str, fields: &BTreeMap<String, Type>) -> String {
    if fields.is_empty() {
        return format!("{}{{}}", name);
    }
    let mut parts = Vec::new();
    for (key, ty) in fields {
        parts.push(format!(
            "{} {}",
            format_shape_key(key),
            format_type_for_lsp(ty)
        ));
    }
    format!("{}{{{}}}", name, parts.join(" "))
}

fn format_named_type_block(name: &str, fields: &BTreeMap<String, Type>) -> String {
    if fields.is_empty() {
        return format!("{}{{}}", name);
    }
    let mut lines = Vec::new();
    lines.push(format!("{}{{", name));
    for (key, ty) in fields {
        lines.push(format!(
            "  {} {}",
            format_shape_key(key),
            format_type_for_lsp(ty)
        ));
    }
    lines.push("}".to_string());
    lines.join("\n")
}

fn format_type_for_lsp(ty: &Type) -> String {
    match ty {
        Type::Any => "Any".to_string(),
        Type::Nil => "Nil".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Number => "Number".to_string(),
        Type::Str => "Str".to_string(),
        Type::Keyword => "Keyword".to_string(),
        Type::Symbol => "Symbol".to_string(),
        Type::Vec(inner) => format!("[{}]", format_type_for_lsp(inner)),
        Type::Tuple(items) => {
            let mut parts = Vec::new();
            for item in items {
                parts.push(format_type_for_lsp(item));
            }
            format!("[{}]", parts.join(" "))
        }
        Type::Map(key, value) => format!(
            "{{{} {}}}",
            format_type_for_lsp(key),
            format_type_for_lsp(value)
        ),
        Type::Object(fields) => {
            if fields.is_empty() {
                return "{}".to_string();
            }
            let mut parts = Vec::new();
            for (key, ty) in fields {
                parts.push(format!(
                    "{} {}",
                    format_shape_key(key),
                    format_type_for_lsp(ty)
                ));
            }
            format!("{{{}}}", parts.join(" "))
        }
        Type::Shape(shape) => {
            if shape.fields.is_empty() && !shape.open {
                return "{}".to_string();
            }
            let mut parts = Vec::new();
            for (key, ty) in &shape.fields {
                parts.push(format!(
                    "{} {}",
                    format_shape_key(key),
                    format_type_for_lsp(ty)
                ));
            }
            if shape.open {
                parts.push("..".to_string());
            }
            format!("{{{}}}", parts.join(" "))
        }
        Type::Union(items) => format_union_for_lsp(items),
        Type::Dyn => "Dyn".to_string(),
        Type::DynOf(inner) => format!("Dyn<{}>", format_type_for_lsp(inner)),
        Type::Named(name) => name.clone(),
        Type::Function { params, rest, ret } => {
            let mut parts: Vec<String> = params.iter().map(format_type_for_lsp).collect();
            if let Some(rest_ty) = rest.as_ref() {
                parts.push("&".to_string());
                parts.push(format_type_for_lsp(rest_ty));
            }
            format!("[{}] -> {}", parts.join(" "), format_type_for_lsp(ret))
        }
    }
}

fn format_union_for_lsp(items: &[Type]) -> String {
    let mut non_nil = Vec::new();
    let mut has_nil = false;
    for item in items {
        if matches!(item, Type::Nil) {
            has_nil = true;
        } else {
            non_nil.push(item);
        }
    }
    if has_nil && non_nil.len() == 1 {
        let inner = format_type_for_lsp(non_nil[0]);
        let needs_paren = inner.contains('|') || inner.contains("->");
        if needs_paren {
            return format!("({})?", inner);
        }
        return format!("{}?", inner);
    }
    items
        .iter()
        .map(format_type_for_lsp)
        .collect::<Vec<_>>()
        .join("|")
}

fn format_shape_key(key: &str) -> String {
    format!(":{}", key)
}

fn make_type_inlay_hint(position: Position, ty: &str) -> InlayHint {
    InlayHint {
        position,
        label: ty.to_string().into(),
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

fn symbol_prefix_at(text: &str, position: Position) -> String {
    let idx = index_at(text, position);
    let chars: Vec<char> = text.chars().collect();
    let idx = idx.min(chars.len());
    let mut start = idx;
    while start > 0 {
        let prev = chars[start - 1];
        if !is_symbol_char(prev) {
            break;
        }
        start -= 1;
    }
    chars[start..idx].iter().collect()
}

fn symbol_at(text: &str, position: Position) -> String {
    let idx = index_at(text, position);
    let chars: Vec<char> = text.chars().collect();
    let idx = idx.min(chars.len());
    let mut start = idx;
    let mut end = idx;
    while start > 0 {
        let prev = chars[start - 1];
        if !is_symbol_char(prev) {
            break;
        }
        start -= 1;
    }
    while end < chars.len() {
        let next = chars[end];
        if !is_symbol_char(next) {
            break;
        }
        end += 1;
    }
    chars[start..end].iter().collect()
}

fn is_symbol_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric()
        || matches!(
            ch,
            '-' | '_' | '?' | '!' | '*' | '+' | '<' | '>' | '=' | '/' | ':' | '.'
        )
}

fn index_at(text: &str, position: Position) -> usize {
    let mut line = 0_u32;
    let mut col = 0_u32;
    let mut idx = 0_usize;
    for ch in text.chars() {
        if line == position.line && col == position.character {
            return idx;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        idx += 1;
    }
    idx
}

fn type_diag_to_lsp(diag: TypeDiagnostic, text: &str) -> Diagnostic {
    let range = diag
        .span
        .as_ref()
        .map(|span| span_to_range(text, span))
        .unwrap_or_else(default_range);
    Diagnostic {
        range,
        severity: Some(match diag.level {
            DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
            DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
        }),
        message: diag.message,
        source: Some("clove2-lsp".to_string()),
        ..Diagnostic::default()
    }
}

fn error_diag(message: String) -> Diagnostic {
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        source: Some("clove2-lsp".to_string()),
        ..Diagnostic::default()
    }
}

fn warn_diag(message: String) -> Diagnostic {
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::WARNING),
        message,
        source: Some("clove2-lsp".to_string()),
        ..Diagnostic::default()
    }
}

fn default_range() -> Range {
    Range::new(Position::new(0, 0), Position::new(0, 1))
}

fn position_in_range(pos: Position, range: &Range) -> bool {
    let after_start = pos.line > range.start.line
        || (pos.line == range.start.line && pos.character >= range.start.character);
    let before_end = pos.line < range.end.line
        || (pos.line == range.end.line && pos.character <= range.end.character);
    after_start && before_end
}

fn span_to_range(text: &str, span: &clove2_core::ast::Span) -> Range {
    let start = position_at(text, span.start);
    let end = position_at(text, span.end);
    Range::new(start, end)
}

fn position_at(text: &str, idx: usize) -> Position {
    let mut line = 0;
    let mut col = 0;
    let mut count = 0;
    for ch in text.chars() {
        if count >= idx {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        count += 1;
    }
    Position::new(line, col)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
