use std::cell::{Cell, RefCell};
use std::collections::{HashMap as StdHashMap, HashSet as StdHashSet};
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use crate::ast::{
    FnArity, Form, FormKind, InterpolatedPart, Key, MapItem, RegexValue, Span, Value,
};
use crate::ast::{HashMap, Vector};
use crate::compiler::{Compiler, APPLY_SYM};
use crate::env::{new_ref, Env, EnvRef};
use crate::error::{format_error, CloveError, ERROR_TAG, WARN_TAG};
use crate::eval::{
    call_callable, current_file_name, expand_short_fn_to_list, form_to_value, push_stack_frame,
    set_current_file, to_key_value_checked, Evaluator, CURRENT_NS_KEY,
};
use crate::fn_meta;
use crate::foreign::ForeignEngine;
use crate::form_source;
use crate::namespaces::NamespaceStore;
use crate::native_buf::NativeBufRegistry;
use crate::options::EvalOptions;
use crate::package_registry;
use crate::profiler;
use crate::reader::{Reader, ReaderOptions, INDEX_GET_IN_SYM, INDEX_GET_SYM};
use crate::repl;
use crate::settings::{
    canonical_feature_toggle, FeatureToggle, NamespaceOrigin, RuntimeFeatureId, RuntimeSettings,
    SyntaxFeatureId, MAIN_PACKAGE_ID,
};
use crate::symbol_meta;
use crate::symbols::{canonical_symbol_name, namespace_aliases, symbol_aliases};
use crate::try_form::parse_err_fin_tail;
use crate::type_registry::{self, AliasMeta, TypeEntry};
use crate::type_syntax::normalize_type_syntax_forms;
use crate::types::TypeKind as MetaTypeKind;
use crate::vm;
use crate::vm::profiler as vm_profiler;
use crate::{apply_default_foreign_tags_for_source, detect_default_tag_from_name, interrupt};
use im::HashSet;

#[derive(Clone, Debug)]
pub struct IRProgram {
    forms: Vec<Form>,
}

impl IRProgram {
    pub fn forms(&self) -> &[Form] {
        &self.forms
    }

    pub fn into_forms(self) -> Vec<Form> {
        self.forms
    }
}

#[derive(Hash, PartialEq, Eq)]
enum RequireKey {
    Namespace(String),
    FilePath(String),
}

fn require_key(spec: &RequireSpec) -> RequireKey {
    match &spec.target {
        RequireTarget::Namespace(ns) => RequireKey::Namespace(ns.clone()),
        RequireTarget::FilePath(path) => RequireKey::FilePath(path.clone()),
    }
}

fn dedup_default_requires(specs: &[RequireSpec]) -> Vec<RequireSpec> {
    let mut seen = StdHashSet::new();
    let mut out = Vec::new();
    for spec in specs.iter().rev() {
        let key = require_key(spec);
        if seen.insert(key) {
            out.push(spec.clone());
        }
    }
    out.reverse();
    out
}

#[derive(Clone, Debug, Default)]
pub struct CompileOptions {
    pub engine_tags: Vec<String>,
    pub source_name: Option<String>,
}

/// Lightweight parse helper for LSP.
/// `source_name` is used for spans and default tag resolution.
pub fn parse_source_for_lsp(
    source: &str,
    source_name: Option<&str>,
) -> Result<Vec<Form>, CloveError> {
    parse_source_for_lsp_inner(
        source,
        source_name,
        ReaderOptions::language_defaults(Vec::new()),
    )
}

/// LSP mode: ignore type-annotation parse errors and keep parsing.
pub fn parse_source_for_lsp_lenient(
    source: &str,
    source_name: Option<&str>,
) -> Result<Vec<Form>, CloveError> {
    parse_source_for_lsp_inner(
        source,
        source_name,
        ReaderOptions::language_defaults(Vec::new()).allow_invalid_type_hints(),
    )
}

fn parse_source_for_lsp_inner(
    source: &str,
    source_name: Option<&str>,
    reader_opts: ReaderOptions,
) -> Result<Vec<Form>, CloveError> {
    let (source, _) = split_data_section(source);
    let mut reader_opts = reader_opts;
    reader_opts.source_name = source_name.map(|s| s.to_string());
    let allow_invalid_type_hints = reader_opts.allow_invalid_type_hints;
    let mut reader = Reader::new_with_options(source, reader_opts);
    let forms = reader.read_all()?;
    let expanded = expand_short_fns(forms);
    let normalized = normalize_type_syntax_forms(expanded, allow_invalid_type_hints)?;
    let (tagged, _) = apply_default_foreign_tags_for_source(normalized, source_name, None)?;
    Ok(tagged)
}

fn split_data_section(source: &str) -> (&str, Option<&str>) {
    let bytes = source.as_bytes();
    let mut line_start = 0usize;
    let mut idx = 0usize;
    while idx <= bytes.len() {
        let is_line_end = idx == bytes.len() || bytes[idx] == b'\n';
        if is_line_end {
            let mut line_end = idx;
            if line_end > line_start && bytes[line_end - 1] == b'\r' {
                line_end -= 1;
            }
            let line = &source[line_start..line_end];
            if line == "__DATA__" || line == "__END__" {
                let mut after = idx;
                if idx < bytes.len() && bytes[idx] == b'\n' {
                    after = idx + 1;
                }
                return (&source[..line_start], Some(&source[after..]));
            }
            line_start = idx + 1;
        }
        idx += 1;
    }
    (source, None)
}

const RANGE_KEY: &str = "__range";
const CLOVER_SET_SOURCE: &str = include_str!("../assets/clove_set.clv");
const CLOVER_STD_SOURCE: &str = include_str!("../assets/clove_std.clv");
const CLOVER_DAG_SOURCE: &str = include_str!("../assets/clove_dag.clv");
const USER_TOML_FILE: &str = "user.toml";
const USER_CLV_FILE: &str = "user.clv";
const BUILTIN_PACKAGE_ID: &str = "<builtin>";

thread_local! {
    static NO_USER_CONFIG_LOCAL: Cell<bool> = Cell::new(false);
    static CURRENT_RUNTIME: RefCell<Option<Arc<RuntimeCtx>>> = RefCell::new(None);
    static CURRENT_DEBUG_STASH_ENABLED: Cell<bool> = Cell::new(false);
    static REQUIRE_RUNTIME_CONTEXT: Cell<bool> = Cell::new(false);
}

static NEXT_RUNTIME_ID: AtomicUsize = AtomicUsize::new(1);

fn user_config_disabled() -> bool {
    std::env::var_os("CLOVE_NO_USER_CONFIG").is_some()
        || NO_USER_CONFIG_LOCAL.with(|flag| flag.get())
}

fn is_virtual_source_name(name: &str) -> bool {
    name.starts_with('<')
}

pub(crate) struct RuntimeGuard {
    prev: Option<Arc<RuntimeCtx>>,
    prev_debug_stash_enabled: bool,
}

impl RuntimeGuard {
    pub(crate) fn set(ctx: Arc<RuntimeCtx>) -> Self {
        let prev = CURRENT_RUNTIME.with(|cell| cell.replace(Some(ctx.clone())));
        let prev_debug_stash_enabled = CURRENT_DEBUG_STASH_ENABLED.with(|cell| {
            let prev = cell.get();
            cell.set(ctx.debug_stash_enabled());
            prev
        });
        Self {
            prev,
            prev_debug_stash_enabled,
        }
    }
}

pub(crate) struct RuntimeRequirementGuard {
    prev: bool,
}

impl RuntimeRequirementGuard {
    pub(crate) fn set(required: bool) -> Self {
        let prev = REQUIRE_RUNTIME_CONTEXT.with(|cell| {
            let prev = cell.get();
            cell.set(required);
            prev
        });
        Self { prev }
    }
}

#[derive(Clone, Debug)]
struct DebugStash {
    subject: Value,
    func: Value,
    args: Vec<Value>,
    call_span: Option<Span>,
    call_file: Option<String>,
    call_form: Option<String>,
}

impl Drop for RuntimeRequirementGuard {
    fn drop(&mut self) {
        REQUIRE_RUNTIME_CONTEXT.with(|cell| cell.set(self.prev));
    }
}

pub(crate) fn runtime_context_required() -> bool {
    REQUIRE_RUNTIME_CONTEXT.with(|cell| cell.get())
}

struct RuntimeTaskTracker {
    active: AtomicUsize,
    notify: (std::sync::Mutex<()>, std::sync::Condvar),
}

impl RuntimeTaskTracker {
    fn new() -> Self {
        Self {
            active: AtomicUsize::new(0),
            notify: (std::sync::Mutex::new(()), std::sync::Condvar::new()),
        }
    }

    fn guard(self: &Arc<Self>) -> RuntimeTaskGuard {
        self.active.fetch_add(1, Ordering::SeqCst);
        RuntimeTaskGuard {
            tracker: self.clone(),
        }
    }

    fn wait_for_clear(&self) {
        if self.active.load(Ordering::SeqCst) == 0 {
            return;
        }
        let (lock, cond) = &self.notify;
        let mut guard = lock.lock().unwrap();
        while self.active.load(Ordering::SeqCst) != 0 {
            guard = cond.wait(guard).unwrap();
        }
    }

    fn task_done(&self) {
        if self.active.fetch_sub(1, Ordering::SeqCst) == 1 {
            let (lock, cond) = &self.notify;
            let _guard = lock.lock().unwrap();
            cond.notify_all();
        }
    }
}

pub(crate) struct RuntimeTaskGuard {
    tracker: Arc<RuntimeTaskTracker>,
}

impl Drop for RuntimeTaskGuard {
    fn drop(&mut self) {
        self.tracker.task_done();
    }
}

fn expand_short_fns(forms: Vec<Form>) -> Vec<Form> {
    forms.into_iter().map(expand_short_fn_form).collect()
}

fn expand_short_fn_form(form: Form) -> Form {
    match &form.kind {
        FormKind::ShortFn(body) => expand_short_fn_to_list(body, form.span),
        FormKind::List(items) => {
            let mapped: Vec<Form> = items.iter().cloned().map(expand_short_fn_form).collect();
            form_with_kind(form, FormKind::List(mapped))
        }
        FormKind::Vector(items) => {
            let mapped: Vec<Form> = items.iter().cloned().map(expand_short_fn_form).collect();
            form_with_kind(form, FormKind::Vector(mapped))
        }
        FormKind::Map(entries) => {
            let mapped = entries
                .iter()
                .cloned()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        MapItem::KeyValue(expand_short_fn_form(k), expand_short_fn_form(v))
                    }
                    MapItem::Spread(expr) => MapItem::Spread(expand_short_fn_form(expr)),
                })
                .collect();
            form_with_kind(form, FormKind::Map(mapped))
        }
        FormKind::Set(items) => {
            let mapped: Vec<Form> = items.iter().cloned().map(expand_short_fn_form).collect();
            form_with_kind(form, FormKind::Set(mapped))
        }
        _ => form,
    }
}

fn form_with_kind(form: Form, kind: FormKind) -> Form {
    let mut out = Form::new(kind, form.span);
    out.type_hint = form.type_hint.clone();
    out
}

impl Drop for RuntimeGuard {
    fn drop(&mut self) {
        CURRENT_RUNTIME.with(|cell| cell.replace(self.prev.take()));
        CURRENT_DEBUG_STASH_ENABLED.with(|cell| cell.set(self.prev_debug_stash_enabled));
    }
}

pub fn compile_to_ir(
    source: &str,
    options: Option<CompileOptions>,
) -> Result<IRProgram, CloveError> {
    let opts = options.unwrap_or_default();
    let mut reader_opts = ReaderOptions::language_defaults(opts.engine_tags.clone());
    reader_opts.source_name = opts.source_name.clone();
    let mut reader = Reader::new_with_options(source, reader_opts);
    let forms = expand_short_fns(reader.read_all()?);
    let forms = normalize_type_syntax_forms(forms, false)?;
    let (tagged, _) =
        apply_default_foreign_tags_for_source(forms, opts.source_name.as_deref(), None)?;
    let mut plan = plan_namespace_forms(tagged)?;
    let body = std::mem::take(&mut plan.body_forms);
    let compiler = Compiler::new(opts.engine_tags);
    let lowered = compiler.compile_from_source(source, body)?;
    Ok(IRProgram { forms: lowered })
}

pub fn eval_source_with_engines(
    src: &str,
    opts: EvalOptions,
    engines: &[Arc<dyn ForeignEngine>],
) -> Result<Value, CloveError> {
    let ctx = RuntimeCtx::new(opts, engines);
    ctx.eval_source(src)
}

#[derive(Clone, Debug)]
struct TopLevelErrFin {
    err: Option<Form>,
    fin: Option<Form>,
}

struct RuntimeSourceState {
    default_tag: Option<String>,
    source_name: Option<String>,
    source_line: usize,
    source_col: usize,
    default_namespace: String,
    top_level_err_fin_capture: Option<String>,
    top_level_err_fin: Option<TopLevelErrFin>,
}

struct RuntimeDataState {
    data_sections: StdHashMap<PathBuf, String>,
    repl_data_source: Option<PathBuf>,
}

struct RuntimeLoadState {
    loaded_paths: StdHashSet<PathBuf>,
    loading_paths: StdHashSet<PathBuf>,
}

struct RuntimePackageState {
    package_src_roots: Vec<PathBuf>,
    package_overrides: StdHashMap<String, String>,
    default_requires: Vec<RequireSpec>,
    applied_default_requires: StdHashMap<String, usize>,
    default_requires_version: usize,
}

struct RuntimeEmbeddedState {
    embedded_files: StdHashMap<String, String>,
    embedded_resources: StdHashMap<String, Vec<u8>>,
}

pub struct RuntimeCtx {
    evaluator: Evaluator,
    settings: RuntimeSettings,
    runtime_id: usize,
    task_tracker: Arc<RuntimeTaskTracker>,
    native_bufs: Mutex<NativeBufRegistry>,
    engine_tags: Vec<String>,
    reader_opts: ReaderOptions,
    namespaces: Arc<RwLock<NamespaceStore>>,
    builtin_namespaces: Mutex<StdHashSet<String>>,
    load_state: Mutex<RuntimeLoadState>,
    embedded_state: Mutex<RuntimeEmbeddedState>,
    data_state: Mutex<RuntimeDataState>,
    source_state: Mutex<RuntimeSourceState>,
    project_root: PathBuf,
    working_dir: PathBuf,
    package_state: Mutex<RuntimePackageState>,
    global_versions: Mutex<StdHashMap<String, u64>>,
    lang_extras_installed: std::sync::atomic::AtomicBool,
    use_vm: bool,
    debug_stash_enabled: AtomicBool,
    debug_stash: Mutex<Option<DebugStash>>,
}

impl RuntimeCtx {
    pub fn try_with_current<F, R>(f: F) -> Option<Result<R, CloveError>>
    where
        F: FnOnce(Arc<RuntimeCtx>) -> Result<R, CloveError>,
    {
        CURRENT_RUNTIME.with(|cell| cell.borrow().clone().map(f))
    }

    pub fn with_current<F, R>(f: F) -> Result<R, CloveError>
    where
        F: FnOnce(Arc<RuntimeCtx>) -> Result<R, CloveError>,
    {
        Self::try_with_current(f)
            .unwrap_or_else(|| Err(CloveError::runtime("runtime context is not available")))
    }

    pub(crate) fn current_handle() -> Option<Arc<RuntimeCtx>> {
        CURRENT_RUNTIME.with(|cell| cell.borrow().clone())
    }

    pub fn new(opts: EvalOptions, engines: &[Arc<dyn ForeignEngine>]) -> Arc<Self> {
        profiler::enable_from_env();
        vm_profiler::enable_from_env();
        if opts.vm_profiler {
            vm_profiler::set_enabled(true);
        }
        let runtime_id = NEXT_RUNTIME_ID.fetch_add(1, Ordering::SeqCst);
        let working_dir = opts
            .working_dir
            .clone()
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
        let mut evaluator =
            Evaluator::new_with_options(Env::default(), engines, opts.auto_fallback);
        evaluator.set_call_wrappers_from_iter(vec![APPLY_SYM.to_string()]);
        install_lang_builtins(evaluator.global_env());
        let engine_tags = evaluator.engine_tags();
        let reader_opts = ReaderOptions::language_defaults(engine_tags.clone());
        let default_tag = opts
            .source_name
            .as_deref()
            .and_then(detect_default_tag_from_name);
        let namespaces = Arc::new(RwLock::new(NamespaceStore::new(evaluator.global_env())));
        evaluator.set_namespace_store(namespaces.clone());
        evaluator.set_default_namespace(Some("user".to_string()));
        let loader =
            Arc::new(|path: &Path| RuntimeCtx::with_current(|ctx| ctx.eval_file_inner(path)));
        evaluator.set_file_loader(Some(loader));
        let settings = evaluator.settings();
        let project_root = detect_project_root(&working_dir);
        let ctx = Arc::new(Self {
            evaluator,
            settings,
            runtime_id,
            task_tracker: Arc::new(RuntimeTaskTracker::new()),
            native_bufs: Mutex::new(NativeBufRegistry::new()),
            engine_tags,
            reader_opts,
            namespaces,
            builtin_namespaces: Mutex::new(StdHashSet::new()),
            load_state: Mutex::new(RuntimeLoadState {
                loaded_paths: StdHashSet::new(),
                loading_paths: StdHashSet::new(),
            }),
            embedded_state: Mutex::new(RuntimeEmbeddedState {
                embedded_files: StdHashMap::new(),
                embedded_resources: StdHashMap::new(),
            }),
            data_state: Mutex::new(RuntimeDataState {
                data_sections: StdHashMap::new(),
                repl_data_source: None,
            }),
            source_state: Mutex::new(RuntimeSourceState {
                default_tag,
                source_name: opts.source_name.clone(),
                source_line: 1,
                source_col: 1,
                default_namespace: "user".to_string(),
                top_level_err_fin_capture: None,
                top_level_err_fin: None,
            }),
            project_root,
            working_dir,
            package_state: Mutex::new(RuntimePackageState {
                package_src_roots: Vec::new(),
                package_overrides: opts.package_overrides.clone(),
                default_requires: Vec::new(),
                applied_default_requires: StdHashMap::new(),
                default_requires_version: 0,
            }),
            global_versions: Mutex::new(StdHashMap::new()),
            lang_extras_installed: std::sync::atomic::AtomicBool::new(false),
            use_vm: opts.use_vm,
            debug_stash_enabled: AtomicBool::new(false),
            debug_stash: Mutex::new(None),
        });
        {
            let _guard = RuntimeGuard::set(ctx.clone());
            {
                let mut store = ctx.namespaces.write().unwrap();
                let default_ns = {
                    let state = ctx.source_state.lock().unwrap();
                    state.default_namespace.clone()
                };
                store.ensure(&default_ns);
            }
            {
                let default_ns = {
                    let state = ctx.source_state.lock().unwrap();
                    state.default_namespace.clone()
                };
                ctx.settings
                    .set_namespace_origin(&default_ns, NamespaceOrigin::UserCode);
            }
            if let Err(err) = ctx.ensure_main_package_config_loaded() {
                ctx.report_config_error(err);
            }
            ctx.refresh_debug_stash_enabled();
            ctx.load_package_registry_paths();
            ctx.register_builtin_namespace("core");
            ctx.register_builtin_namespace("string");
            ctx.register_builtin_namespace("async");
            ctx.register_builtin_namespace("walk");
            ctx.register_builtin_namespace("io");
            ctx.register_builtin_namespace("fs");
            ctx.register_builtin_namespace("path");
            ctx.register_builtin_namespace("shell");
            ctx.register_builtin_namespace("process");
            ctx.register_builtin_namespace("json");
            ctx.register_builtin_namespace("ini");
            ctx.register_builtin_namespace("http");
            ctx.register_builtin_namespace("time");
            ctx.register_builtin_namespace("log");
            ctx.register_builtin_namespace("env");
            ctx.register_builtin_namespace("pprint");
            ctx.register_builtin_namespace("dag");
            ctx.load_builtin_dag_library();
            ctx.load_builtin_set_library();
            ctx.register_builtin_namespace("set");
            ctx.register_builtin_namespace("std");
            if !opts.no_std {
                ctx.load_builtin_std_library();
                ctx.install_default_requires();
            }
            let default_ns = {
                let state = ctx.source_state.lock().unwrap();
                state.default_namespace.clone()
            };
            if !opts.no_std {
                if let Err(err) = ctx.apply_default_requires(&default_ns) {
                    ctx.report_config_error(err);
                }
            }
        }
        ctx
    }

    pub fn eval_source(self: &Arc<Self>, src: &str) -> Result<Value, CloveError> {
        interrupt::clear_interrupt();
        let _guard = RuntimeGuard::set(self.clone());
        self.eval_source_inner(src)
    }

    fn eval_source_inner(&self, src: &str) -> Result<Value, CloveError> {
        self.ensure_pkg_config_loaded_for_eval()?;
        let source_name = {
            let state = self.source_state.lock().unwrap();
            state.source_name.clone()
        };
        set_current_file(source_name);
        let prev_err_fin_capture = {
            let mut state = self.source_state.lock().unwrap();
            let prev = state.top_level_err_fin_capture.clone();
            if state.top_level_err_fin_capture.is_some() {
                state.top_level_err_fin = None;
            }
            prev
        };
        let forms = self.read_source_forms(src)?;
        let result = self.eval_forms_with_source(src, forms);
        {
            let mut state = self.source_state.lock().unwrap();
            state.top_level_err_fin_capture = prev_err_fin_capture;
        }
        result
    }

    pub(crate) fn data_section_for_current_file(&self) -> Option<String> {
        let file = current_file_name()?;
        let path = Path::new(&file);
        let state = self.data_state.lock().unwrap();
        if let Some(data) = state.data_sections.get(path) {
            return Some(data.clone());
        }
        if file == "<repl>" {
            return state
                .repl_data_source
                .as_ref()
                .and_then(|source| state.data_sections.get(source))
                .cloned();
        }
        let canonical = canonicalize_path(path);
        state.data_sections.get(&canonical).cloned()
    }

    pub fn set_repl_data_source(&self, path: Option<PathBuf>) {
        match path {
            Some(path) => {
                let abs = canonicalize_path(&path);
                let needs_load = {
                    let state = self.data_state.lock().unwrap();
                    !state.data_sections.contains_key(&abs)
                };
                if needs_load {
                    if let Ok(content) = fs::read_to_string(&abs) {
                        let (_, data) = split_data_section(&content);
                        self.update_data_section(&abs, data);
                    }
                }
                let mut state = self.data_state.lock().unwrap();
                state.repl_data_source = Some(abs);
            }
            None => {
                let mut state = self.data_state.lock().unwrap();
                state.repl_data_source = None;
            }
        }
    }

    pub fn set_top_level_err_fin_capture(&self, source_name: Option<String>) {
        let mut state = self.source_state.lock().unwrap();
        state.top_level_err_fin_capture = source_name;
        state.top_level_err_fin = None;
    }

    pub fn settings(&self) -> RuntimeSettings {
        self.settings.clone()
    }

    pub fn call_callable_with_debug_repl(
        &self,
        callable: Value,
        args: Vec<Value>,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        match call_callable(callable, args) {
            Ok(value) => Ok(value),
            Err(err) => {
                self.maybe_enter_debug_repl(&err, env);
                Err(err)
            }
        }
    }

    pub fn call_main_with_top_level_err_fin(
        &self,
        callable: Value,
        args: &[String],
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let argv = args.iter().cloned().map(Value::String).collect::<Vec<_>>();
        let tail = {
            let mut state = self.source_state.lock().unwrap();
            state.top_level_err_fin.take()
        };
        let Some(tail) = tail else {
            return self.call_callable_with_debug_repl(callable, argv, env);
        };
        let span = Span {
            line: 0,
            col: 0,
            index: 0,
        };
        let mut call_items = Vec::with_capacity(1 + args.len());
        call_items.push(Form::new(FormKind::Symbol("-main".into()), span));
        for arg in args {
            call_items.push(Form::new(FormKind::String(arg.clone()), span));
        }
        let call_form = Form::new(FormKind::List(call_items), span);
        let mut try_items =
            Vec::with_capacity(2 + tail.err.iter().count() + tail.fin.iter().count());
        try_items.push(Form::new(FormKind::Symbol("try".into()), span));
        try_items.push(call_form);
        if let Some(err_form) = tail.err {
            try_items.push(err_form);
        }
        if let Some(fin_form) = tail.fin {
            try_items.push(fin_form);
        }
        let try_form = Form::new(FormKind::List(try_items), span);
        match self.eval_form_with_vm_fallback(&try_form, env.clone()) {
            Ok(value) => Ok(value),
            Err(err) => {
                self.maybe_enter_debug_repl(&err, env);
                Err(err)
            }
        }
    }

    pub fn eval_file(self: &Arc<Self>, path: &Path) -> Result<Value, CloveError> {
        interrupt::clear_interrupt();
        let _guard = RuntimeGuard::set(self.clone());
        self.eval_file_inner(path)
    }

    fn eval_file_inner(&self, path: &Path) -> Result<Value, CloveError> {
        let abs = canonicalize_path(path);
        self.ensure_pkg_config_loaded_for_path(&abs)?;
        let content = fs::read_to_string(&abs).map_err(|err| {
            CloveError::runtime(format!("failed to read {}: {}", abs.display(), err))
        })?;
        let (code_part, data_part) = split_data_section(&content);
        self.update_data_section(&abs, data_part);
        let prev_current_file = current_file_name();
        let (
            prev_source,
            prev_tag,
            prev_default_ns,
            prev_source_line,
            prev_source_col,
            prev_err_fin_capture,
        ) = {
            let mut state = self.source_state.lock().unwrap();
            let prev_source = state.source_name.clone();
            let prev_tag = state.default_tag.clone();
            let prev_default_ns = state.default_namespace.clone();
            let prev_source_line = state.source_line;
            let prev_source_col = state.source_col;
            let prev_err_fin_capture = state.top_level_err_fin_capture.clone();
            state.source_name = Some(abs.to_string_lossy().into_owned());
            state.source_line = 1;
            state.source_col = 1;
            state.default_tag = state
                .source_name
                .as_deref()
                .and_then(detect_default_tag_from_name);
            if state.top_level_err_fin_capture.is_some() {
                state.top_level_err_fin = None;
            }
            (
                prev_source,
                prev_tag,
                prev_default_ns,
                prev_source_line,
                prev_source_col,
                prev_err_fin_capture,
            )
        };
        let source_name = {
            let state = self.source_state.lock().unwrap();
            state.source_name.clone()
        };
        set_current_file(source_name);
        let result = (|| {
            let forms = self.read_source_forms(code_part)?;
            let has_explicit_ns = forms.first().map(is_ns_form).unwrap_or(false);
            if !has_explicit_ns {
                let default_ns = self.resolve_file_namespace(&abs);
                self.set_default_namespace_name(default_ns);
            }
            self.eval_forms_with_source(code_part, forms)
        })();
        {
            let mut state = self.source_state.lock().unwrap();
            state.top_level_err_fin_capture = prev_err_fin_capture;
            state.source_name = prev_source;
            state.source_line = prev_source_line;
            state.source_col = prev_source_col;
            state.default_tag = prev_tag;
        }
        self.set_default_namespace_name(prev_default_ns);
        set_current_file(prev_current_file);
        result
    }

    fn update_data_section(&self, path: &Path, data: Option<&str>) {
        let mut state = self.data_state.lock().unwrap();
        match data {
            Some(value) => {
                state
                    .data_sections
                    .insert(path.to_path_buf(), value.to_string());
            }
            None => {
                state.data_sections.remove(path);
            }
        }
    }

    fn read_source_forms(&self, src: &str) -> Result<Vec<Form>, CloveError> {
        let mut reader_opts = self.reader_opts.clone();
        let (source_name, source_line, source_col) = {
            let state = self.source_state.lock().unwrap();
            (
                state.source_name.clone(),
                state.source_line,
                state.source_col,
            )
        };
        reader_opts.source_name = source_name;
        reader_opts.start_line = source_line;
        reader_opts.start_col = source_col;
        let mut reader = Reader::new_with_options(src, reader_opts);
        let forms = expand_short_fns(reader.read_all()?);
        normalize_type_syntax_forms(forms, false)
    }

    pub fn eval_forms_with_source(&self, src: &str, forms: Vec<Form>) -> Result<Value, CloveError> {
        let compiler = Compiler::new(self.engine_tags.clone());
        let (source_name, default_tag) = {
            let state = self.source_state.lock().unwrap();
            (state.source_name.clone(), state.default_tag.clone())
        };
        let (tagged, _) = apply_default_foreign_tags_for_source(
            forms,
            source_name.as_deref(),
            default_tag.as_deref(),
        )?;
        let mut plan = plan_namespace_forms(tagged)?;
        let body = std::mem::take(&mut plan.body_forms);
        let lowered = compiler.compile_from_source(src, body)?;
        self.eval_namespace_forms(lowered, plan)
    }

    pub fn eval_repl_source(
        self: &Arc<Self>,
        src: &str,
        env: EnvRef,
        source_name: Option<&str>,
        default_tag: Option<&str>,
    ) -> Result<Value, CloveError> {
        let _guard = RuntimeGuard::set(self.clone());
        self.eval_repl_source_inner(src, env, source_name, default_tag)
    }

    fn eval_repl_source_inner(
        &self,
        src: &str,
        env: EnvRef,
        source_name: Option<&str>,
        default_tag: Option<&str>,
    ) -> Result<Value, CloveError> {
        let working_dir = self.working_dir.clone();
        self.ensure_pkg_config_loaded_for_dir(&working_dir)?;
        let file_name = source_name
            .map(|name| name.to_string())
            .unwrap_or_else(|| "<repl>".to_string());
        set_current_file(Some(file_name));
        let mut reader_opts = self.reader_opts.clone();
        reader_opts.source_name = source_name.map(|s| s.to_string());
        let (source_line, source_col) = {
            let state = self.source_state.lock().unwrap();
            (state.source_line, state.source_col)
        };
        reader_opts.start_line = source_line;
        reader_opts.start_col = source_col;
        let mut reader = Reader::new_with_options(src, reader_opts);
        let forms = expand_short_fns(reader.read_all()?);
        let forms = normalize_type_syntax_forms(forms, false)?;
        self.eval_repl_forms(forms, env, default_tag)
    }

    pub fn eval_repl_forms(
        &self,
        forms: Vec<Form>,
        env: EnvRef,
        default_tag: Option<&str>,
    ) -> Result<Value, CloveError> {
        Self::ensure_repl_specials(&env);
        let ns_span = forms.first().filter(|f| is_ns_form(f)).map(|f| f.span);
        let (default_tag_value, default_ns, source_name) = {
            let state = self.source_state.lock().unwrap();
            (
                state.default_tag.clone(),
                state.default_namespace.clone(),
                state.source_name.clone(),
            )
        };
        let tag = default_tag.or(default_tag_value.as_deref());
        let (tagged, _) =
            apply_default_foreign_tags_for_source(forms, source_name.as_deref(), tag)?;
        let plan = plan_namespace_forms(tagged)?;
        if plan.decl.is_some() {
            let span = ns_span.unwrap_or(Span {
                line: 0,
                col: 0,
                index: 0,
            });
            return Err(span_error(
                span,
                "ns may only appear as the first form in a file",
            ));
        }
        let ns_name = env_namespace_name(&env).unwrap_or(default_ns);
        {
            let mut store = self.namespaces.write().unwrap();
            store.ensure(&ns_name);
            if let Some(entry) = store.get_mut(&ns_name) {
                for private in &plan.private_names {
                    entry.mark_private(private.clone());
                }
            }
        }
        for spec in plan.inline_requires {
            self.apply_require_spec(&ns_name, spec)?;
        }
        if let Some(try_form) = self.wrap_err_fin_tail(&plan.body_forms)? {
            match self.eval_form_with_vm_fallback(&try_form, env.clone()) {
                Ok(val) => {
                    Self::update_repl_success(&env, val.clone());
                    self.refresh_namespace_exports(&ns_name)?;
                    return Ok(val);
                }
                Err(err) => {
                    Self::update_repl_error(&env, &err);
                    self.maybe_enter_debug_repl(&err, env.clone());
                    return Err(err);
                }
            }
        }
        let mut last = Value::Nil;
        for form in plan.body_forms {
            if let Some(val) = self.try_handle_namespace_form(&ns_name, &form, env.clone())? {
                Self::update_repl_success(&env, val.clone());
                last = val;
                continue;
            }
            match self.eval_form_with_vm_fallback(&form, env.clone()) {
                Ok(val) => {
                    Self::update_repl_success(&env, val.clone());
                    last = val;
                }
                Err(err) => {
                    Self::update_repl_error(&env, &err);
                    self.maybe_enter_debug_repl(&err, env.clone());
                    return Err(err);
                }
            }
        }
        self.refresh_namespace_exports(&ns_name)?;
        Ok(last)
    }

    fn ensure_repl_specials(env: &EnvRef) {
        let mut writer = env.write().unwrap();
        for name in ["*1", "*2", "*3", "*e"] {
            if writer.get(name).is_none() {
                writer.set(name, Value::Nil);
            }
        }
    }

    fn update_repl_success(env: &EnvRef, value: Value) {
        let (prev_1, prev_2) = {
            let reader = env.read().unwrap();
            (reader.get("*1"), reader.get("*2"))
        };
        let mut writer = env.write().unwrap();
        writer.set("*3", prev_2.unwrap_or(Value::Nil));
        writer.set("*2", prev_1.unwrap_or(Value::Nil));
        writer.set("*1", value);
    }

    fn update_repl_error(env: &EnvRef, err: &CloveError) {
        let mut writer = env.write().unwrap();
        writer.set("*e", Value::String(err.to_string()));
    }

    pub fn with_current_ctx<F, R>(self: &Arc<Self>, f: F) -> R
    where
        F: FnOnce(Arc<RuntimeCtx>) -> R,
    {
        let _guard = RuntimeGuard::set(self.clone());
        f(self.clone())
    }

    fn eval_namespace_forms(
        &self,
        forms: Vec<Form>,
        plan: NamespacePlan,
    ) -> Result<Value, CloveError> {
        let NamespacePlan {
            decl,
            inline_requires,
            private_names,
            ..
        } = plan;
        let default_ns = {
            let state = self.source_state.lock().unwrap();
            state.default_namespace.clone()
        };
        let ns_name = decl.as_ref().map(|d| d.name.clone()).unwrap_or(default_ns);
        let current_origin = self.settings.namespace_origin(Some(&ns_name));
        if matches!(current_origin, NamespaceOrigin::UserCode) {
            self.settings
                .set_namespace_origin(&ns_name, NamespaceOrigin::UserCode);
        }
        {
            let mut store = self.namespaces.write().unwrap();
            store.ensure(&ns_name);
        }
        self.apply_default_requires(&ns_name)?;
        self.register_namespace_source(&ns_name, decl.is_some())?;
        {
            let mut store = self.namespaces.write().unwrap();
            if let Some(entry) = store.get_mut(&ns_name) {
                for name in &private_names {
                    entry.mark_private(name.clone());
                }
            }
        }
        if let Some(declaration) = decl.as_ref() {
            let meta_value_map = if let Some(form) = &declaration.meta_form {
                match form_to_value(form)? {
                    Value::Map(map) => Some(map),
                    _ => return Err(span_error(form.span, "ns attr-map must be a map literal")),
                }
            } else {
                None
            };
            let symbol_info = symbol_meta::SymbolMeta {
                doc: declaration.doc.clone(),
                meta: meta_value_map,
                source: declaration.source.clone(),
            };
            symbol_meta::register(&declaration.name, symbol_info);
        }
        if let Some(declaration) = decl {
            for spec in declaration.require_specs {
                self.apply_require_spec(&ns_name, spec)?;
            }
        }
        for spec in inline_requires {
            self.apply_require_spec(&ns_name, spec)?;
        }
        self.eval_ns_body(&ns_name, forms)
    }

    fn eval_ns_body(&self, ns_name: &str, forms: Vec<Form>) -> Result<Value, CloveError> {
        let env = {
            let store = self.namespaces.read().unwrap();
            store.get(ns_name).expect("namespace must exist").env()
        };
        {
            let mut writer = env.write().unwrap();
            writer.set(CURRENT_NS_KEY, Value::Symbol(ns_name.to_string()));
        }
        if let Some(try_form) = self.wrap_err_fin_tail(&forms)? {
            let label = format!("{ns_name}::<top-level>");
            let _guard = push_stack_frame(label, Some(try_form.span));
            let result = self.eval_form_with_vm_fallback(&try_form, env.clone())?;
            self.refresh_namespace_exports(ns_name)?;
            return Ok(result);
        }
        let mut last = Value::Symbol(ns_name.to_string());
        for form in forms {
            let label = format!("{ns_name}::<top-level>");
            let _guard = push_stack_frame(label, Some(form.span));
            if let Some(val) = self.try_handle_namespace_form(ns_name, &form, env.clone())? {
                last = val;
                continue;
            }
            match self.eval_form_with_vm_fallback(&form, env.clone()) {
                Ok(val) => last = val,
                Err(err) => {
                    self.maybe_enter_debug_repl(&err, env.clone());
                    return Err(err);
                }
            }
        }
        self.refresh_namespace_exports(ns_name)?;
        Ok(last)
    }

    fn wrap_err_fin_tail(&self, forms: &[Form]) -> Result<Option<Form>, CloveError> {
        let tail = parse_err_fin_tail(forms, "a body")
            .map_err(|err| span_error(err.span, &err.message))?;
        let Some(tail) = tail else {
            return Ok(None);
        };
        {
            let mut state = self.source_state.lock().unwrap();
            if let Some(source) = state.top_level_err_fin_capture.clone() {
                if current_file_name().as_deref() == Some(source.as_str()) {
                    state.top_level_err_fin = Some(TopLevelErrFin {
                        err: tail.err.clone(),
                        fin: tail.fin.clone(),
                    });
                }
            }
        }
        if tail.body.is_empty() {
            let span = forms.first().map(|f| f.span).unwrap_or(Span {
                line: 0,
                col: 0,
                index: 0,
            });
            return Err(span_error(span, "top-level expects body before err/fin"));
        }
        let try_span = forms.first().map(|f| f.span).unwrap_or(Span {
            line: 0,
            col: 0,
            index: 0,
        });
        let mut items = Vec::with_capacity(1 + tail.body.len());
        items.push(Form::new(FormKind::Symbol("try".into()), try_span));
        items.extend(tail.body.into_iter());
        if let Some(err_form) = tail.err {
            items.push(err_form);
        }
        if let Some(fin_form) = tail.fin {
            items.push(fin_form);
        }
        Ok(Some(Form::new(FormKind::List(items), try_span)))
    }

    fn eval_form_with_vm_fallback(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        if !self.use_vm {
            return self.evaluator.eval(form, env);
        }
        let expanded = if let Some(span) = find_indexer_span(form) {
            if !self.indexer_enabled_for_env(&env) {
                return Err(span_error(
                    span,
                    "indexer syntax is disabled; enable it via (use indexer true)",
                ));
            }
            Some(expand_indexer_form(form)?)
        } else {
            None
        };
        let target = expanded.as_ref().unwrap_or(form);
        vm_profiler::count_vm_attempt();
        match vm::compile_form(target) {
            Ok(compiled) => match vm::run_chunk(&compiled, &self.evaluator, env.clone()) {
                Ok(val) => Ok(val),
                Err(vm::VmError::Unsupported { span, reason }) => {
                    vm_profiler::count_vm_fallback(
                        vm_profiler::VmFallbackReason::RuntimeUnsupported,
                    );
                    let sample = self.vm_fallback_sample(span);
                    vm_profiler::count_vm_fallback_detail(
                        vm_profiler::VmFallbackReason::RuntimeUnsupported,
                        &reason,
                        sample,
                    );
                    self.evaluator.eval(target, env)
                }
                Err(vm::VmError::Runtime(err)) => Err(err),
            },
            Err(vm::VmError::Unsupported { span, reason }) => {
                vm_profiler::count_vm_fallback(vm_profiler::VmFallbackReason::CompileUnsupported);
                let sample = self.vm_fallback_sample(span);
                vm_profiler::count_vm_fallback_detail(
                    vm_profiler::VmFallbackReason::CompileUnsupported,
                    &reason,
                    sample,
                );
                self.evaluator.eval(target, env)
            }
            Err(vm::VmError::Runtime(err)) => Err(err),
        }
    }

    fn indexer_enabled_for_env(&self, env: &EnvRef) -> bool {
        let ns_name = env_namespace_name(env).unwrap_or_else(|| {
            let state = self.source_state.lock().unwrap();
            state.default_namespace.clone()
        });
        let pkg_id = self.settings.package_for_namespace(Some(&ns_name));
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::Indexer), &pkg_id)
    }

    fn vm_fallback_sample(&self, span: Span) -> Option<String> {
        let file = current_file_name();
        Some(match file {
            Some(name) => format!("{name}:{}:{}", span.line, span.col),
            None => format!("{}:{}", span.line, span.col),
        })
    }

    pub(crate) fn run_vm_prototype(
        &self,
        proto: &vm::bytecode::FunctionPrototype,
        env: EnvRef,
        locals: Vec<Value>,
    ) -> Result<Value, CloveError> {
        match vm::runtime::run_prototype(proto, &self.evaluator, env, locals) {
            Ok(val) => Ok(val),
            Err(vm::VmError::Runtime(err)) => Err(err),
            Err(vm::VmError::Unsupported { .. }) => {
                Err(CloveError::runtime("vm function is unsupported"))
            }
        }
    }

    fn ensure_main_package_config_loaded(&self) -> Result<(), CloveError> {
        let working_dir = self.working_dir.clone();
        let pkg_id = self.ensure_pkg_config_loaded_for_dir(&working_dir)?;
        let default_ns = {
            let state = self.source_state.lock().unwrap();
            state.default_namespace.clone()
        };
        self.settings.set_namespace_package(&default_ns, &pkg_id);
        Ok(())
    }

    fn ensure_pkg_config_loaded_for_path(&self, path: &Path) -> Result<String, CloveError> {
        let (pkg_id, pkg_root) = package_info_for_path(path);
        self.ensure_pkg_config_loaded(&pkg_id, pkg_root.as_deref())?;
        Ok(pkg_id)
    }

    fn ensure_pkg_config_loaded_for_dir(&self, dir: &Path) -> Result<String, CloveError> {
        let (pkg_id, pkg_root) = package_info_for_path(dir);
        self.ensure_pkg_config_loaded(&pkg_id, pkg_root.as_deref())?;
        Ok(pkg_id)
    }

    fn ensure_pkg_config_loaded(
        &self,
        pkg_id: &str,
        pkg_root: Option<&Path>,
    ) -> Result<(), CloveError> {
        if self.settings.pkg_config_loaded(pkg_id) {
            return Ok(());
        }
        if user_config_disabled() {
            self.settings.mark_pkg_config_loaded(pkg_id);
            return Ok(());
        }
        if let Some(root) = pkg_root {
            self.load_user_toml_config(pkg_id, root)?;
            self.load_user_clv_config(pkg_id, root)?;
        }
        self.settings.mark_pkg_config_loaded(pkg_id);
        Ok(())
    }

    fn ensure_pkg_config_loaded_for_eval(&self) -> Result<(), CloveError> {
        let source_name = {
            let state = self.source_state.lock().unwrap();
            state.source_name.clone()
        };
        if let Some(source_name) = source_name.as_deref() {
            if is_virtual_source_name(source_name) {
                return Ok(());
            }
            let path = PathBuf::from(source_name);
            self.ensure_pkg_config_loaded_for_path(&path)?;
            return Ok(());
        }
        let working_dir = self.working_dir.clone();
        self.ensure_pkg_config_loaded_for_dir(&working_dir)?;
        Ok(())
    }

    fn load_builtin_set_library(&self) {
        if CLOVER_SET_SOURCE.trim().is_empty() {
            return;
        }
        let (prev_source, prev_tag) = {
            let mut state = self.source_state.lock().unwrap();
            let prev_source = state.source_name.clone();
            let prev_tag = state.default_tag.clone();
            state.source_name = Some("<set>".to_string());
            state.default_tag = None;
            (prev_source, prev_tag)
        };
        let result = self.eval_source_inner(CLOVER_SET_SOURCE);
        {
            let mut state = self.source_state.lock().unwrap();
            state.source_name = prev_source;
            state.default_tag = prev_tag;
        }
        if let Err(err) = result {
            self.report_config_error(err);
        }
    }

    fn load_builtin_std_library(&self) {
        if CLOVER_STD_SOURCE.trim().is_empty() {
            return;
        }
        let (prev_source, prev_tag) = {
            let mut state = self.source_state.lock().unwrap();
            let prev_source = state.source_name.clone();
            let prev_tag = state.default_tag.clone();
            state.source_name = Some("<std>".to_string());
            state.default_tag = None;
            (prev_source, prev_tag)
        };
        let result = self.eval_source_inner(CLOVER_STD_SOURCE);
        {
            let mut state = self.source_state.lock().unwrap();
            state.source_name = prev_source;
            state.default_tag = prev_tag;
        }
        if let Err(err) = result {
            self.report_config_error(err);
        }
    }

    fn load_builtin_dag_library(&self) {
        if CLOVER_DAG_SOURCE.trim().is_empty() {
            return;
        }
        let (prev_source, prev_tag) = {
            let mut state = self.source_state.lock().unwrap();
            let prev_source = state.source_name.clone();
            let prev_tag = state.default_tag.clone();
            state.source_name = Some("<dag>".to_string());
            state.default_tag = None;
            (prev_source, prev_tag)
        };
        let result = self.eval_source_inner(CLOVER_DAG_SOURCE);
        {
            let mut state = self.source_state.lock().unwrap();
            state.source_name = prev_source;
            state.default_tag = prev_tag;
        }
        if let Err(err) = result {
            self.report_config_error(err);
        }
    }

    fn install_default_requires(&self) {
        let mut state = self.package_state.lock().unwrap();
        if !state.default_requires.is_empty() {
            return;
        }
        state.default_requires.push(RequireSpec {
            target: RequireTarget::Namespace("std".to_string()),
            alias: Some("std".to_string()),
            refers: Vec::new(),
            rename: StdHashMap::new(),
            refer_all: true,
            span: Span {
                line: 0,
                col: 0,
                index: 0,
            },
        });
        state.default_requires_version = state.default_requires_version.saturating_add(1);
    }

    fn apply_default_requires(&self, ns_name: &str) -> Result<(), CloveError> {
        let (current_version, already_applied, specs) = {
            let state = self.package_state.lock().unwrap();
            let current_version = state.default_requires_version;
            let already_applied = state
                .applied_default_requires
                .get(ns_name)
                .copied()
                .unwrap_or_default();
            let specs = dedup_default_requires(&state.default_requires);
            (current_version, already_applied, specs)
        };
        if already_applied >= current_version {
            return Ok(());
        }
        for spec in specs {
            if matches!(
                (&spec.target, ns_name),
                (RequireTarget::Namespace(target), name) if target == name
            ) {
                continue;
            }
            self.apply_require_spec(ns_name, spec)?;
        }
        let mut state = self.package_state.lock().unwrap();
        state
            .applied_default_requires
            .insert(ns_name.to_string(), current_version);
        Ok(())
    }

    fn load_user_toml_config(&self, pkg_id: &str, pkg_root: &Path) -> Result<(), CloveError> {
        let path = pkg_root.join(USER_TOML_FILE);
        if !path.exists() {
            return Ok(());
        }
        let canon = canonicalize_path(&path);
        let content = fs::read_to_string(&canon).map_err(|err| {
            CloveError::runtime(format!("failed to read {}: {}", canon.display(), err))
        })?;
        if content.trim().is_empty() {
            return Ok(());
        }
        let value: toml::Value = toml::from_str(&content).map_err(|err| {
            CloveError::runtime(format!("failed to parse {}: {}", canon.display(), err))
        })?;
        let Some(syntax_table) = value.get("syntax") else {
            return Ok(());
        };
        let table = syntax_table.as_table().ok_or_else(|| {
            CloveError::runtime(format!("{}: [syntax] must be a table", canon.display()))
        })?;
        for (key, val) in table {
            let enabled = val.as_bool().ok_or_else(|| {
                CloveError::runtime(format!(
                    "{}: syntax.{} must be boolean",
                    canon.display(),
                    key
                ))
            })?;
            let feature = canonical_feature_toggle(key).ok_or_else(|| {
                CloveError::runtime(format!(
                    "{}: unknown syntax feature '{}'",
                    canon.display(),
                    key
                ))
            })?;
            self.settings
                .assign_feature_toggle(feature, pkg_id, enabled);
        }
        self.refresh_debug_stash_enabled();
        Ok(())
    }

    fn load_user_clv_config(&self, pkg_id: &str, pkg_root: &Path) -> Result<(), CloveError> {
        let path = pkg_root.join(USER_CLV_FILE);
        if !path.exists() {
            return Ok(());
        }
        let canon = canonicalize_path(&path);
        let content = fs::read_to_string(&canon).map_err(|err| {
            CloveError::runtime(format!("failed to read {}: {}", canon.display(), err))
        })?;
        if content.trim().is_empty() {
            return Ok(());
        }
        let mut reader_opts = ReaderOptions::language_defaults(Vec::new());
        reader_opts.source_name = Some(canon.to_string_lossy().into_owned());
        let mut reader = Reader::new_with_options(&content, reader_opts);
        let forms = reader.read_all()?;
        for form in forms {
            self.apply_user_clv_form(pkg_id, &canon, &form)?;
        }
        Ok(())
    }

    fn apply_user_clv_form(
        &self,
        pkg_id: &str,
        source_path: &Path,
        form: &Form,
    ) -> Result<(), CloveError> {
        let err = |span: Span, msg: &str| {
            span_error(span, msg).with_file(Some(source_path.to_string_lossy().into_owned()))
        };
        let items = match &form.kind {
            FormKind::List(items) => items,
            _ => {
                return Err(err(
                    form.span,
                    "user.clv allows only (use <feature> true|false)",
                ))
            }
        };
        if items.len() != 3 {
            return Err(err(
                form.span,
                "user.clv allows only (use <feature> true|false)",
            ));
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => {
                return Err(err(
                    items[0].span,
                    "user.clv allows only (use <feature> true|false)",
                ))
            }
        };
        if head != "use" {
            return Err(err(
                items[0].span,
                "user.clv allows only (use <feature> true|false)",
            ));
        }
        let feature_sym = match &items[1].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => {
                return Err(err(
                    items[1].span,
                    "user.clv use expects feature symbol (e.g. dot-chain)",
                ))
            }
        };
        let enabled = match &items[2].kind {
            FormKind::Bool(flag) => *flag,
            _ => return Err(err(items[2].span, "user.clv use expects true/false")),
        };
        let feature = canonical_feature_toggle(feature_sym)
            .ok_or_else(|| err(items[1].span, &format!("unknown feature '{}'", feature_sym)))?;
        self.settings
            .assign_feature_toggle(feature, pkg_id, enabled);
        self.refresh_debug_stash_enabled();
        Ok(())
    }

    fn report_config_error(&self, err: CloveError) {
        for line in format_error(&err) {
            eprintln!("{} {}", WARN_TAG, line);
        }
    }

    pub(crate) fn refresh_debug_stash_enabled(&self) {
        let enabled = self.settings.repl_on_error_enabled_any();
        self.debug_stash_enabled.store(enabled, Ordering::Relaxed);
        if let Some(current) = Self::current_handle() {
            if std::ptr::eq(Arc::as_ptr(&current), self) {
                CURRENT_DEBUG_STASH_ENABLED.with(|cell| cell.set(enabled));
            }
        }
    }

    pub(crate) fn debug_stash_enabled(&self) -> bool {
        self.debug_stash_enabled.load(Ordering::Relaxed)
    }

    pub(crate) fn debug_stash_enabled_fast() -> bool {
        CURRENT_DEBUG_STASH_ENABLED.with(|cell| cell.get())
    }

    pub(crate) fn maybe_update_debug_stash(
        &self,
        func: &Value,
        args: &[Value],
        call_span: Option<Span>,
        call_file: Option<String>,
        call_form: Option<String>,
    ) {
        if !self.debug_stash_enabled() {
            return;
        }
        let subject = args.get(0).cloned().unwrap_or(Value::Nil);
        let mut stash = self.debug_stash.lock().unwrap();
        *stash = Some(DebugStash {
            subject,
            func: func.clone(),
            args: args.to_vec(),
            call_span,
            call_file,
            call_form,
        });
    }

    fn debug_repl_env(&self, base_env: EnvRef, err: &CloveError) -> EnvRef {
        let debug_env = new_ref(Env::new_child(base_env));
        let (subject, func, args, call_span, call_file, call_form) = {
            let stash = self.debug_stash.lock().unwrap();
            if let Some(stash) = stash.as_ref() {
                (
                    stash.subject.clone(),
                    stash.func.clone(),
                    stash.args.clone(),
                    stash.call_span,
                    stash.call_file.clone(),
                    stash.call_form.clone(),
                )
            } else {
                (Value::Nil, Value::Nil, Vec::new(), None, None, None)
            }
        };
        {
            let args_vec = Value::Vector(Vector::from(args.clone()));
            let call_info = Self::build_debug_call_map(call_file, call_span, call_form);
            let error_value = Value::String(err.to_string());
            let call0_value = {
                let func = func.clone();
                let args = args.clone();
                Value::native_fn_with_name("call0", FnArity::exact(0), move |_args| {
                    call_callable(func.clone(), args.clone())
                })
            };
            let call_value = {
                let func = func.clone();
                let default_args = args.clone();
                Value::native_fn_with_name("call", FnArity::at_least(0), move |call_args| {
                    let final_args = if call_args.is_empty() {
                        default_args.clone()
                    } else if call_args.len() == 1 {
                        match &call_args[0] {
                            Value::Vector(items) | Value::List(items) => {
                                items.iter().cloned().collect()
                            }
                            other => vec![other.clone()],
                        }
                    } else {
                        call_args.to_vec()
                    };
                    call_callable(func.clone(), final_args)
                })
            };
            let mut writer = debug_env.write().unwrap();
            writer.set("?", subject.clone());
            writer.set("?v", subject.clone());
            writer.set("*?", subject.clone());
            writer.set("*1", subject);
            writer.set("*f", func.clone());
            writer.set("*args", args_vec.clone());
            writer.set("*call", call_info.clone());
            writer.set("*e", error_value.clone());
            writer.set("?f", func);
            writer.set("?args", args_vec);
            writer.set("?call", call_info);
            writer.set("?e", error_value);
            writer.set("call0", call0_value);
            writer.set("call", call_value);
        }
        debug_env
    }

    fn build_debug_call_map(
        call_file: Option<String>,
        call_span: Option<Span>,
        call_form: Option<String>,
    ) -> Value {
        let mut map = HashMap::new();
        map.insert(
            Key::Keyword("file".to_string()),
            call_file.map(Value::String).unwrap_or(Value::Nil),
        );
        map.insert(
            Key::Keyword("span".to_string()),
            call_span.map(Self::span_to_value).unwrap_or(Value::Nil),
        );
        map.insert(
            Key::Keyword("form".to_string()),
            call_form.map(Value::String).unwrap_or(Value::Nil),
        );
        Value::Map(map)
    }

    fn span_to_value(span: Span) -> Value {
        let mut map = HashMap::new();
        map.insert(
            Key::Keyword("line".to_string()),
            Value::Int(span.line as i64),
        );
        map.insert(Key::Keyword("col".to_string()), Value::Int(span.col as i64));
        map.insert(
            Key::Keyword("index".to_string()),
            Value::Int(span.index as i64),
        );
        Value::Map(map)
    }

    fn maybe_enter_debug_repl(&self, err: &CloveError, env: EnvRef) {
        if repl::in_debug_repl() {
            return;
        }
        if !self.should_break_into_debug_repl(err) {
            return;
        }
        repl::set_last_debug_error(err.clone());
        for line in format_error(err) {
            eprintln!("{}", line);
        }
        let base_env = err.env().unwrap_or(env);
        let debug_env = self.debug_repl_env(base_env, err);
        if let Err(repl_err) = repl::run_debug_repl(&self.evaluator, debug_env) {
            eprintln!("{} repl failed: {}", ERROR_TAG, repl_err);
        }
    }

    fn should_break_into_debug_repl(&self, err: &CloveError) -> bool {
        let ns = self.error_namespace(err);
        let default_ns = {
            let state = self.source_state.lock().unwrap();
            state.default_namespace.clone()
        };
        let pkg_id = ns
            .as_deref()
            .and_then(|name| self.settings.namespace_package(name))
            .or_else(|| self.settings.namespace_package(&default_ns))
            .unwrap_or_else(|| MAIN_PACKAGE_ID.to_string());
        self.settings.feature_toggle_enabled(
            FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError),
            &pkg_id,
        )
    }

    fn error_namespace(&self, err: &CloveError) -> Option<String> {
        let stack = err.stack();
        stack.last().and_then(|frame| {
            if let Some(idx) = frame.function.rfind("::") {
                Some(frame.function[..idx].to_string())
            } else {
                None
            }
        })
    }

    fn register_namespace_source(
        &self,
        ns_name: &str,
        warn_on_mismatch: bool,
    ) -> Result<(), CloveError> {
        let Some(path) = self.current_source_path() else {
            return Ok(());
        };
        let abs = canonicalize_path(&path);
        let abs_str = abs.to_string_lossy();
        if !abs.exists() || abs_str.starts_with('<') {
            // Skip non-file sources (REPL/eval/builtin) without warnings.
            return Ok(());
        }
        let (pkg_id, _) = package_info_for_path(&abs);
        self.settings.set_namespace_package(ns_name, &pkg_id);
        {
            let mut store = self.namespaces.write().unwrap();
            store
                .register_file(ns_name, &abs)
                .map_err(|err| CloveError::runtime(err.to_string()))?;
            if let Some(root) = infer_namespace_root(ns_name, &abs) {
                if let Some(entry) = store.get_mut(ns_name) {
                    entry.set_root_if_missing(&root);
                }
            } else if warn_on_mismatch {
                if let Some(entry) = store.get_mut(ns_name) {
                    if entry.warned_path_mismatch {
                        return Ok(());
                    }
                    eprintln!(
                        "{} namespace '{}' defined in '{}' does not match directory layout",
                        WARN_TAG,
                        ns_name,
                        abs.display()
                    );
                    entry.warned_path_mismatch = true;
                }
            }
        }
        Ok(())
    }

    pub fn register_builtin_namespace(&self, name: &str) {
        let exports = builtin_exports_from_env(self.evaluator.global_env(), name);
        if exports.is_empty() {
            return;
        }
        {
            let mut builtin = self.builtin_namespaces.lock().unwrap();
            builtin.insert(name.to_string());
        }
        {
            let mut store = self.namespaces.write().unwrap();
            store.ensure(name);
            if let Some(entry) = store.get_mut(name) {
                {
                    let env = entry.env();
                    let mut writer = env.write().unwrap();
                    for (local, value) in &exports {
                        writer.set(local, value.clone());
                    }
                }
                entry.public_exports = exports;
            }
        }
        self.settings
            .set_namespace_origin(name, NamespaceOrigin::StdLib);
        self.settings
            .set_namespace_package(name, BUILTIN_PACKAGE_ID);
    }

    fn try_handle_namespace_form(
        &self,
        _ns_name: &str,
        form: &Form,
        _env: EnvRef,
    ) -> Result<Option<Value>, CloveError> {
        let items = match &form.kind {
            FormKind::List(items) => items,
            _ => return Ok(None),
        };
        if items.is_empty() {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok(None),
        };
        match head {
            "ns" => Err(span_error(
                form.span,
                "ns may only appear as the first form in a file",
            )),
            _ => Ok(None),
        }
    }

    fn apply_require_spec(&self, current_ns: &str, spec: RequireSpec) -> Result<bool, CloveError> {
        match &spec.target {
            RequireTarget::Namespace(target) => {
                let is_builtin = {
                    let builtin = self.builtin_namespaces.lock().unwrap();
                    builtin.contains(target)
                };
                if is_builtin {
                    self.install_namespace_imports(current_ns, target, &spec)?;
                    Ok(false)
                } else {
                    let loaded = self.load_namespace(current_ns, target, spec.span)?;
                    self.install_namespace_imports(current_ns, target, &spec)?;
                    Ok(loaded)
                }
            }
            RequireTarget::FilePath(raw) => {
                let path = self.resolve_relative_path(raw)?;
                let abs = canonicalize_path(&path);
                let loaded = self.require_file(&abs)?;
                let target_ns = self
                    .namespaces
                    .read()
                    .unwrap()
                    .namespace_for_file(&abs)
                    .ok_or_else(|| {
                        CloveError::runtime(format!(
                            "namespace not found for file {}",
                            abs.display()
                        ))
                    })?;
                self.install_namespace_imports(current_ns, &target_ns, &spec)?;
                Ok(loaded)
            }
        }
    }

    fn load_namespace(
        &self,
        current_ns: &str,
        target_ns: &str,
        span: Span,
    ) -> Result<bool, CloveError> {
        {
            let mut store = self.namespaces.write().unwrap();
            store.ensure(target_ns);
        }
        let existing_path = self
            .namespaces
            .read()
            .unwrap()
            .get(target_ns)
            .and_then(|ns| ns.file_path.clone());
        let resolved = if let Some(path) = existing_path {
            path
        } else {
            let mut bases = Vec::new();
            if let Some(dir) = self
                .namespaces
                .read()
                .unwrap()
                .get(target_ns)
                .and_then(|ns| ns.root_dir.clone())
            {
                bases.push(dir);
            }
            if let Some(dir) = self
                .namespaces
                .read()
                .unwrap()
                .get(current_ns)
                .and_then(|ns| ns.root_dir.clone())
            {
                bases.push(dir);
            }
            if let Some(dir) = self.current_source_dir() {
                bases.push(dir);
            }
            bases.push(self.working_dir.clone());
            if self.project_root != self.working_dir {
                bases.push(self.project_root.clone());
            }
            if let Ok(dir) = std::env::current_dir() {
                bases.push(dir);
            }
            let package_roots = {
                let state = self.package_state.lock().unwrap();
                state.package_src_roots.clone()
            };
            for root in &package_roots {
                bases.push(root.clone());
            }
            let mut rel = namespace_to_path(target_ns);
            rel.set_extension("clv");
            let mut found = None;
            for base in &bases {
                let candidate = base.join(&rel);
                if candidate.exists() {
                    found = Some(candidate);
                    break;
                }
            }
            let candidate = found.unwrap_or_else(|| {
                let base = bases.first().cloned().unwrap_or_else(|| PathBuf::from("."));
                base.join(rel)
            });
            if !candidate.exists() && self.embedded_source(&candidate).is_none() {
                return Err(span_error(
                    span,
                    &format!(
                        "namespace '{}' not found at {}",
                        target_ns,
                        candidate.display()
                    ),
                ));
            }
            candidate
        };
        let resolved = canonicalize_path(&resolved);
        {
            let mut store = self.namespaces.write().unwrap();
            if store
                .get(target_ns)
                .and_then(|ns| ns.file_path.as_ref())
                .is_none()
            {
                store
                    .register_file(target_ns, &resolved)
                    .map_err(|err| CloveError::runtime(err.to_string()))?;
                if let Some(root) = infer_namespace_root(target_ns, &resolved) {
                    if let Some(entry) = store.get_mut(target_ns) {
                        entry.set_root_if_missing(&root);
                    }
                }
            }
        }
        self.require_file(&resolved)
    }

    fn load_package_registry_paths(&self) {
        let clove_home = package_registry::clove_home();
        let overrides = {
            let state = self.package_state.lock().unwrap();
            state.package_overrides.clone()
        };
        match package_registry::load_registry(&clove_home) {
            Ok(registry) => match registry.resolve_src_roots(&overrides) {
                Ok(src_roots) => {
                    let mut roots = Vec::new();
                    for root in src_roots {
                        if root.exists() {
                            roots.push(root);
                        }
                    }
                    let mut state = self.package_state.lock().unwrap();
                    state.package_src_roots = roots;
                }
                Err(err) => {
                    self.report_config_error(CloveError::runtime(format!(
                        "failed to resolve package registry: {}",
                        err
                    )));
                }
            },
            Err(err) => {
                self.report_config_error(CloveError::runtime(format!(
                    "failed to load package registry: {}",
                    err
                )));
            }
        }
    }

    fn install_namespace_imports(
        &self,
        current_ns: &str,
        target_ns: &str,
        spec: &RequireSpec,
    ) -> Result<(), CloveError> {
        let (exports, imported_types) = {
            let store = self.namespaces.read().unwrap();
            if let Some(entry) = store.get(target_ns) {
                (entry.public_exports.clone(), entry.imported_types.clone())
            } else {
                (StdHashMap::new(), StdHashSet::new())
            }
        };
        let type_names = type_registry::list_types_in_namespace(target_ns);
        let type_names: Vec<String> = type_names
            .into_iter()
            .filter(|name| {
                let fqn = format!("{}::{}", target_ns, name);
                !imported_types.contains(&fqn)
            })
            .collect();
        if exports.is_empty()
            && type_names.is_empty()
            && (spec.alias.is_some()
                || spec.refer_all
                || !spec.refers.is_empty()
                || !spec.rename.is_empty())
        {
            return Err(span_error(
                spec.span,
                &format!(
                    "namespace '{}' has no exported symbols to import",
                    target_ns
                ),
            ));
        }
        if let Some(alias) = &spec.alias {
            validate_alias_name(alias, spec.span)?;
            self.register_type_alias_mapping(current_ns, alias, target_ns);
            for (name, value) in &exports {
                let alias_sym = format!("{}::{}", alias, name);
                self.set_imported_symbol(current_ns, &alias_sym, value.clone())?;
            }
        }
        let refer_pairs = build_refer_pairs(&exports, spec)?;
        for (source, local) in refer_pairs {
            let value = exports.get(&source).ok_or_else(|| {
                span_error(
                    spec.span,
                    &format!("namespace '{}' does not export '{}'", target_ns, source),
                )
            })?;
            self.set_imported_symbol(current_ns, &local, value.clone())?;
        }
        self.install_namespace_type_imports(current_ns, target_ns, spec, &type_names)?;
        Ok(())
    }

    fn register_type_alias_mapping(&self, current_ns: &str, alias: &str, target_ns: &str) {
        let mut store = self.namespaces.write().unwrap();
        store.set_type_alias(current_ns, alias, target_ns);
    }

    fn install_namespace_type_imports(
        &self,
        current_ns: &str,
        target_ns: &str,
        spec: &RequireSpec,
        type_names: &[String],
    ) -> Result<(), CloveError> {
        if !spec.refer_all && spec.refers.is_empty() && spec.rename.is_empty() {
            return Ok(());
        }
        if type_names.is_empty() {
            return Ok(());
        }
        let available: StdHashSet<String> = type_names.iter().cloned().collect();
        let mut pairs: Vec<(String, String)> = Vec::new();
        let mut seen = StdHashSet::new();
        if spec.refer_all {
            for name in &available {
                let local = spec
                    .rename
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.clone());
                pairs.push((name.clone(), local));
                seen.insert(name.clone());
            }
        }
        for name in &spec.refers {
            if !available.contains(name) || seen.contains(name) {
                continue;
            }
            let local = spec
                .rename
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone());
            pairs.push((name.clone(), local));
            seen.insert(name.clone());
        }
        for (source, local) in &spec.rename {
            if seen.contains(source) || !available.contains(source) {
                continue;
            }
            pairs.push((source.clone(), local.clone()));
            seen.insert(source.clone());
        }
        let imported_fqns: Vec<String> = pairs
            .iter()
            .map(|(_, local)| format!("{}::{}", current_ns, local))
            .collect();
        for (source, local) in pairs {
            self.register_type_import_alias(current_ns, target_ns, &source, &local, spec.span)?;
        }
        if !imported_fqns.is_empty() {
            let mut store = self.namespaces.write().unwrap();
            if let Some(entry) = store.get_mut(current_ns) {
                entry.record_imported_types(&imported_fqns);
            }
        }
        Ok(())
    }

    fn register_type_import_alias(
        &self,
        current_ns: &str,
        target_ns: &str,
        source: &str,
        local: &str,
        span: Span,
    ) -> Result<(), CloveError> {
        let alias_fqn = format!("{}::{}", current_ns, local);
        let target_fqn = format!("{}::{}", target_ns, source);
        if let Some(existing) = type_registry::get_type_entry(&alias_fqn) {
            if let TypeEntry::Alias(meta) = existing {
                if meta.target == MetaTypeKind::named(target_fqn.clone()) {
                    return Ok(());
                }
            }
            return Err(span_error(
                span,
                &format!("type '{}' is already defined", alias_fqn),
            ));
        }
        let meta = AliasMeta {
            namespace: current_ns.to_string(),
            name: local.to_string(),
            doc: None,
            target: MetaTypeKind::named(target_fqn),
        };
        type_registry::register_alias(meta).map_err(|err| err.with_span(span))?;
        Ok(())
    }

    fn set_imported_symbol(
        &self,
        ns_name: &str,
        symbol: &str,
        value: Value,
    ) -> Result<(), CloveError> {
        let mut store = self.namespaces.write().unwrap();
        let entry = store
            .get_mut(ns_name)
            .ok_or_else(|| CloveError::runtime(format!("namespace '{}' not found", ns_name)))?;
        let aliases = symbol_aliases(symbol);
        let env = crate::builtins::default_env();
        {
            let mut env_guard = env.write().unwrap();
            crate::builtins::memo::install(&mut env_guard);
        }
        let env_ref = entry.env();
        {
            let mut writer = env_ref.write().unwrap();
            for alias in &aliases {
                writer.set(alias, value.clone());
            }
        }
        entry.record_imported_aliases(&aliases);
        Ok(())
    }

    fn resolve_relative_path(&self, raw: &str) -> Result<PathBuf, CloveError> {
        let path = PathBuf::from(raw);
        if path.is_absolute() {
            return Ok(path);
        }
        let mut bases = Vec::new();
        if let Some(dir) = self.current_source_dir() {
            bases.push(dir);
        }
        bases.push(self.working_dir.clone());
        if self.project_root != self.working_dir {
            bases.push(self.project_root.clone());
        }
        if let Ok(dir) = std::env::current_dir() {
            if bases.last().map(|last| last != &dir).unwrap_or(true) {
                bases.push(dir);
            }
        }
        if bases.is_empty() {
            return Err(CloveError::runtime(format!(
                "cannot resolve path '{}'",
                raw
            )));
        }
        for base in &bases {
            let candidate = base.join(&path);
            if candidate.exists() {
                return Ok(candidate);
            }
            if candidate.extension().is_none() {
                let with_ext = candidate.with_extension("clv");
                if with_ext.exists() {
                    return Ok(with_ext);
                }
            }
        }
        Ok(bases[0].join(&path))
    }

    fn implicit_namespace_for_path(&self, abs: &Path) -> String {
        let relative = self.relative_namespace_path(abs);
        let mut segments = Vec::new();
        if let Some(parent) = relative.parent() {
            for component in parent.components() {
                if let Component::Normal(name) = component {
                    segments.push(sanitize_ns_segment(&name.to_string_lossy()));
                }
            }
        }
        let stem = relative
            .file_stem()
            .or_else(|| relative.file_name())
            .and_then(|s| s.to_str())
            .unwrap_or("_");
        segments.push(sanitize_ns_segment(stem));
        let base_ns = if segments.is_empty() {
            "_".to_string()
        } else {
            segments.join("::")
        };
        if self.implicit_namespace_available(&base_ns, abs) {
            return base_ns;
        }
        let hash = implicit_namespace_hash(abs);
        let mut counter = 0;
        loop {
            let suffix = if counter == 0 {
                format!("_{}", hash)
            } else {
                format!("_{}{}", hash, counter)
            };
            let candidate = append_namespace_suffix(&base_ns, &suffix);
            if self.implicit_namespace_available(&candidate, abs) {
                return candidate;
            }
            counter += 1;
        }
    }

    fn implicit_namespace_available(&self, name: &str, path: &Path) -> bool {
        let store = self.namespaces.read().unwrap();
        match store.get(name).and_then(|entry| entry.file_path.as_ref()) {
            Some(existing) => existing == path,
            None => true,
        }
    }

    fn resolve_file_namespace(&self, abs: &Path) -> String {
        self.namespaces
            .read()
            .unwrap()
            .namespace_for_file(abs)
            .unwrap_or_else(|| self.implicit_namespace_for_path(abs))
    }

    fn relative_namespace_path(&self, abs: &Path) -> PathBuf {
        let mut candidates = Vec::new();
        candidates.push(canonicalize_path(&self.working_dir));
        for base in candidates {
            if let Ok(rel) = abs.strip_prefix(&base) {
                return rel.to_path_buf();
            }
        }
        abs.to_path_buf()
    }

    fn embedded_source(&self, path: &Path) -> Option<String> {
        let mut candidates = Vec::new();
        if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            candidates.push(name.to_string());
            if !name.contains('.') {
                candidates.push(format!("{}.clv", name));
            }
        }
        for key in candidates {
            let state = self.embedded_state.lock().unwrap();
            if let Some(src) = state.embedded_files.get(&key) {
                return Some(src.clone());
            }
        }
        None
    }

    fn require_file(&self, path: &Path) -> Result<bool, CloveError> {
        let abs = canonicalize_path(path);
        {
            let mut state = self.load_state.lock().unwrap();
            if state.loaded_paths.contains(&abs) {
                return Ok(false);
            }
            if !state.loading_paths.insert(abs.clone()) {
                return Err(CloveError::runtime(format!(
                    "circular require detected for {}",
                    abs.display()
                )));
            }
        }
        let content = match fs::read_to_string(&abs) {
            Ok(text) => text,
            Err(err) => {
                if let Some(src) = self.embedded_source(&abs) {
                    src
                } else {
                    return Err(CloveError::runtime(format!(
                        "failed to read {}: {}",
                        abs.display(),
                        err
                    )));
                }
            }
        };
        let (code_part, data_part) = split_data_section(&content);
        self.update_data_section(&abs, data_part);
        let prev_current_file = current_file_name();
        let (prev_source, prev_tag, prev_default_ns) = {
            let state = self.source_state.lock().unwrap();
            (
                state.source_name.clone(),
                state.default_tag.clone(),
                state.default_namespace.clone(),
            )
        };
        let default_ns = self.resolve_file_namespace(&abs);
        {
            let mut state = self.source_state.lock().unwrap();
            state.source_name = Some(abs.to_string_lossy().into_owned());
            state.default_tag = state
                .source_name
                .as_deref()
                .and_then(detect_default_tag_from_name);
        }
        self.set_default_namespace_name(default_ns);
        let result = self.eval_source_inner(code_part);
        {
            let mut state = self.source_state.lock().unwrap();
            state.source_name = prev_source;
            state.default_tag = prev_tag;
        }
        self.set_default_namespace_name(prev_default_ns);
        set_current_file(prev_current_file);
        {
            let mut state = self.load_state.lock().unwrap();
            state.loading_paths.remove(&abs);
            if result.is_ok() {
                state.loaded_paths.insert(abs);
            }
        }
        match result {
            Ok(_) => Ok(true),
            Err(err) => Err(err),
        }
    }

    fn current_source_path(&self) -> Option<PathBuf> {
        let state = self.source_state.lock().unwrap();
        state.source_name.as_ref().map(PathBuf::from)
    }

    fn current_source_dir(&self) -> Option<PathBuf> {
        self.current_source_path()
            .and_then(|p| p.parent().map(|dir| dir.to_path_buf()))
    }

    fn refresh_namespace_exports(&self, ns_name: &str) -> Result<(), CloveError> {
        let (env_snapshot, imported, private, previous, shared_env) = {
            let store = self.namespaces.read().unwrap();
            let entry = store
                .get(ns_name)
                .ok_or_else(|| CloveError::runtime(format!("namespace '{}' not found", ns_name)))?;
            (
                entry.env().read().unwrap().clone_data(),
                entry.imported_symbols.clone(),
                entry.private_names.clone(),
                entry.public_exports.clone(),
                store.shared_env(),
            )
        };

        let mut exports = StdHashMap::new();
        for (name, value) in env_snapshot {
            if imported.contains(&name) {
                continue;
            }
            if private.contains(&name) {
                continue;
            }
            if name.contains('/') {
                continue;
            }
            exports.insert(name, value);
        }
        let mut shared = shared_env.write().unwrap();
        let prev_keys: StdHashSet<_> = previous.keys().cloned().collect();
        let new_keys: StdHashSet<_> = exports.keys().cloned().collect();
        for removed in prev_keys.difference(&new_keys) {
            for alias in namespace_aliases(ns_name, removed) {
                shared.remove(&alias);
            }
        }
        for (name, value) in &exports {
            for alias in namespace_aliases(ns_name, name) {
                shared.set(&alias, value.clone());
            }
        }
        drop(shared);
        let mut store = self.namespaces.write().unwrap();
        if let Some(entry) = store.get_mut(ns_name) {
            entry.public_exports = exports;
        }
        Ok(())
    }

    pub fn env(&self) -> EnvRef {
        self.evaluator.global_env()
    }

    pub fn runtime_id(&self) -> usize {
        self.runtime_id
    }

    pub(crate) fn with_native_bufs<R>(&self, f: impl FnOnce(&NativeBufRegistry) -> R) -> R {
        let guard = self.native_bufs.lock().unwrap();
        f(&guard)
    }

    pub(crate) fn with_native_bufs_mut<R>(&self, f: impl FnOnce(&mut NativeBufRegistry) -> R) -> R {
        let mut guard = self.native_bufs.lock().unwrap();
        f(&mut guard)
    }

    pub(crate) fn task_guard(&self) -> RuntimeTaskGuard {
        self.task_tracker.guard()
    }

    pub(crate) fn wait_for_tasks(&self) {
        self.task_tracker.wait_for_clear();
    }

    pub fn mark_lang_extras_installed(&self) -> bool {
        !self.lang_extras_installed.swap(true, Ordering::SeqCst)
    }

    pub fn namespace_names(&self) -> Vec<String> {
        self.namespaces.read().unwrap().names()
    }

    pub fn namespace_store(&self) -> Arc<RwLock<NamespaceStore>> {
        self.namespaces.clone()
    }

    pub fn namespace_for_path(&self, path: &Path) -> Option<String> {
        let abs = canonicalize_path(path);
        self.namespaces.read().unwrap().namespace_for_file(&abs)
    }

    pub fn register_embedded_file(&self, name: impl Into<String>, source: impl Into<String>) {
        let mut state = self.embedded_state.lock().unwrap();
        state.embedded_files.insert(name.into(), source.into());
    }

    pub(crate) fn register_embedded_resource(&self, name: impl Into<String>, bytes: Vec<u8>) {
        let mut state = self.embedded_state.lock().unwrap();
        state.embedded_resources.insert(name.into(), bytes);
    }

    pub(crate) fn embedded_resource(&self, name: &str) -> Option<Vec<u8>> {
        let state = self.embedded_state.lock().unwrap();
        state.embedded_resources.get(name).cloned()
    }

    pub fn set_default_namespace_name(&self, name: String) {
        {
            let mut state = self.source_state.lock().unwrap();
            state.default_namespace = name.clone();
        }
        self.evaluator.set_default_namespace(Some(name));
    }

    pub fn namespace_env(&self, name: &str) -> Option<EnvRef> {
        self.namespaces
            .read()
            .unwrap()
            .get(name)
            .map(|entry| entry.env())
    }

    pub fn default_namespace_name(&self) -> String {
        let state = self.source_state.lock().unwrap();
        state.default_namespace.clone()
    }

    pub fn set_source_name(&self, name: Option<String>) {
        let mut state = self.source_state.lock().unwrap();
        state.source_name = name.clone();
        state.default_tag = name.as_deref().and_then(detect_default_tag_from_name);
        state.source_line = 1;
        state.source_col = 1;
    }

    pub fn set_source_position(&self, line: usize, col: usize) {
        let mut state = self.source_state.lock().unwrap();
        state.source_line = line.max(1);
        state.source_col = col.max(1);
    }

    pub fn source_position(&self) -> (usize, usize) {
        let state = self.source_state.lock().unwrap();
        (state.source_line, state.source_col)
    }

    pub fn set_default_tag(&self, tag: Option<String>) {
        let mut state = self.source_state.lock().unwrap();
        state.default_tag = tag;
    }

    pub fn source_name(&self) -> Option<String> {
        let state = self.source_state.lock().unwrap();
        state.source_name.clone()
    }

    pub fn default_tag(&self) -> Option<String> {
        let state = self.source_state.lock().unwrap();
        state.default_tag.clone()
    }

    pub fn working_dir(&self) -> &Path {
        &self.working_dir
    }

    pub(crate) fn global_version(&self, name: &str, env: &EnvRef) -> u64 {
        let key = self.global_version_key(name, env);
        let state = self.global_versions.lock().unwrap();
        state.get(&key).copied().unwrap_or(0)
    }

    pub(crate) fn bump_global_version(&self, name: &str, env: &EnvRef) {
        let key = self.global_version_key(name, env);
        let mut state = self.global_versions.lock().unwrap();
        let entry = state.entry(key).or_insert(0);
        *entry = entry.saturating_add(1);
    }

    pub(crate) fn clear_imported_flag(&self, env: &EnvRef, name: &str) {
        let Some(ns) = env_namespace_name(env) else {
            return;
        };
        let mut guard = match self.namespaces.write() {
            Ok(guard) => guard,
            Err(_) => return,
        };
        if let Some(entry) = guard.get_mut(&ns) {
            entry.imported_symbols.remove(name);
        }
    }

    pub(crate) fn mark_private_name(&self, env: &EnvRef, name: &str) {
        let Some(ns) = env_namespace_name(env) else {
            return;
        };
        let mut guard = match self.namespaces.write() {
            Ok(guard) => guard,
            Err(_) => return,
        };
        if let Some(entry) = guard.get_mut(&ns) {
            entry.mark_private(name.to_string());
        }
    }

    fn global_version_key(&self, name: &str, env: &EnvRef) -> String {
        let canonical = canonical_symbol_name(name);
        let canonical_name = canonical.as_ref();
        if canonical_name.contains("::") {
            return canonical.into_owned();
        }
        if let Some(ns) = env_namespace_name(env) {
            return format!("{ns}::{canonical_name}");
        }
        canonical.into_owned()
    }
}

impl Drop for RuntimeCtx {
    fn drop(&mut self) {
        self.wait_for_tasks();
        if let Ok(mut bufs) = self.native_bufs.lock() {
            bufs.clear();
        }
        type_registry::clear_runtime(self.runtime_id);
        symbol_meta::clear_runtime(self.runtime_id);
        fn_meta::clear_runtime(self.runtime_id);
    }
}

fn env_namespace_name(env: &EnvRef) -> Option<String> {
    env.read()
        .unwrap()
        .get(CURRENT_NS_KEY)
        .and_then(|val| match val {
            Value::Symbol(ns) => Some(ns),
            Value::String(ns) => Some(ns),
            _ => None,
        })
}

fn install_lang_builtins(env: EnvRef) {
    let mut env = env.write().unwrap();
    env.set(
        "__apply",
        Value::native_fn(FnArity::at_least(0), |args| apply_builtin(args)),
    );
    env.set(
        "__range_literal",
        Value::native_fn(FnArity::at_least(0), |args| range_literal_builtin(args)),
    );
    mirror_core_namespace(&mut env);
}

fn mirror_core_namespace(env: &mut Env) {
    let entries = env.clone_data();
    for (name, value) in entries {
        if name.contains('/') || name.contains("::") {
            continue;
        }
        for alias in namespace_aliases("core", &name) {
            env.set(&alias, value.clone());
        }
    }
}

fn builtin_exports_from_env(env: EnvRef, namespace: &str) -> StdHashMap<String, Value> {
    let data = env.read().unwrap().clone_data();
    let colon_prefix = format!("{}::", namespace);
    let mut exports = StdHashMap::new();
    for (name, value) in data {
        if let Some(rest) = name.strip_prefix(&colon_prefix) {
            if rest.contains("::") {
                continue;
            }
            exports.insert(rest.to_string(), value);
            continue;
        }
    }
    exports
}

// helper to wrap error
fn err<T>(msg: impl Into<String>) -> Result<T, CloveError> {
    Err(CloveError::runtime(msg))
}

fn range_literal_builtin(args: &[Value]) -> Result<Value, CloveError> {
    if args.len() != 3 {
        return err("range literal expects start, end, exclusive");
    }
    let start = value_to_optional_int(&args[0])?;
    let end = value_to_optional_int(&args[1])?;
    let exclusive = matches!(args[2], Value::Bool(true));
    Ok(Value::Map(build_range_map(start, end, exclusive)))
}

fn value_to_optional_int(value: &Value) -> Result<Option<i64>, CloveError> {
    match value {
        Value::Nil => Ok(None),
        Value::Int(n) => Ok(Some(*n)),
        Value::Float(f) if f.fract() == 0.0 => Ok(Some(*f as i64)),
        _ => err("range bounds must be integers or nil"),
    }
}

fn build_range_map(start: Option<i64>, end: Option<i64>, exclusive: bool) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    map.insert(Key::Keyword(RANGE_KEY.to_string()), Value::Bool(true));
    map.insert(
        Key::Keyword("start".to_string()),
        match start {
            Some(n) => Value::Int(n),
            None => Value::Nil,
        },
    );
    map.insert(
        Key::Keyword("end".to_string()),
        match end {
            Some(n) => Value::Int(n),
            None => Value::Nil,
        },
    );
    map.insert(
        Key::Keyword("exclusive".to_string()),
        Value::Bool(exclusive),
    );
    map
}

pub(crate) fn apply_builtin(args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("__apply expects callable and arguments");
    }
    let head = args[0].clone();
    let tail = args[1..].to_vec();
    if parse_range_spec(&head).is_some() {
        return apply_indexer(&head, &tail);
    }
    match head {
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => call_callable(head, tail),
        Value::Vector(ref v) => apply_sequence(v, &tail, SequenceKind::Vector),
        Value::List(ref v) => apply_sequence(v, &tail, SequenceKind::List),
        Value::MutVector(ref v) => {
            let items = v.lock().unwrap_or_else(|e| e.into_inner());
            apply_sequence(&items, &tail, SequenceKind::Vector)
        }
        Value::String(ref s) => {
            if !tail.is_empty() {
                if matches!(tail[0], Value::Map(_)) && parse_range_spec(&tail[0]).is_some() {
                    apply_string(s, &tail)
                } else if matches!(tail[0], Value::Map(_) | Value::Set(_)) {
                    apply_indexer(&head, &tail)
                } else {
                    apply_string(s, &tail)
                }
            } else {
                apply_string(s, &tail)
            }
        }
        Value::Map(ref m) => apply_map_head(m, &tail),
        Value::MutMap(ref m) => {
            let map = m.lock().unwrap_or_else(|e| e.into_inner());
            apply_map_head(&map, &tail)
        }
        Value::Set(ref s) => apply_set_head(s, &tail),
        Value::MutSet(ref s) => {
            let set = s.lock().unwrap_or_else(|e| e.into_inner());
            apply_set_head(&set, &tail)
        }
        Value::Regex(ref regex) => apply_regex(regex, &tail),
        Value::Symbol(_) | Value::Int(_) | Value::Float(_) => apply_indexer(&head, &tail),
        other => err(format!(
            "cannot call value (type: {}, value: {})",
            other.type_name(),
            other
        )),
    }
}

fn apply_sequence(
    seq: &Vector<Value>,
    args: &[Value],
    kind: SequenceKind,
) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("collection call expects arguments");
    }
    let default = args.get(1).cloned();
    let result = match parse_lookup_request(&args[0])? {
        LookupRequest::Index(idx) => {
            let len = seq.len();
            match normalize_index(idx, len, false) {
                Ok(resolved) => Ok(seq[resolved].clone()),
                Err(e) => Err(e),
            }
        }
        LookupRequest::Range(spec) => slice_sequence(seq, spec, kind),
        LookupRequest::Indexes(indexes) => gather_sequence(seq, indexes, kind),
    };
    match result {
        Ok(val) => Ok(val),
        Err(e) => match default {
            Some(def) => Ok(def),
            None => Err(e),
        },
    }
}

fn apply_string(text: &str, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("string call expects arguments");
    }
    if let Value::Regex(re) = &args[0] {
        if args.len() > 2 {
            return err("string regex call expects optional default");
        }
        return Ok(regex_lookup(re, text, args.get(1).cloned()));
    }
    let chars: Vec<char> = text.chars().collect();
    let default = args.get(1).cloned();
    let result = match parse_lookup_request(&args[0])? {
        LookupRequest::Index(idx) => match normalize_index(idx, chars.len(), false) {
            Ok(resolved) => Ok(Value::String(chars[resolved].to_string())),
            Err(e) => Err(e),
        },
        LookupRequest::Range(spec) => slice_string(&chars, spec),
        LookupRequest::Indexes(indexes) => gather_string(&chars, indexes),
    };
    match result {
        Ok(val) => Ok(val),
        Err(e) => match default {
            Some(def) => Ok(def),
            None => Err(e),
        },
    }
}

fn apply_map_head(map: &HashMap<Key, Value>, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("map call expects key");
    }
    let key = to_key_value_checked(&args[0])?;
    match map.get(&key) {
        Some(val) => Ok(val.clone()),
        None => Ok(args.get(1).cloned().unwrap_or(Value::Nil)),
    }
}

fn apply_set_head(set: &HashSet<Value>, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("set call expects element");
    }
    if set.contains(&args[0]) {
        Ok(args[0].clone())
    } else {
        Ok(Value::Nil)
    }
}

pub(crate) fn apply_regex(regex: &RegexValue, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("regex call expects string");
    }
    if args.len() > 2 {
        return err("regex call expects string and optional default");
    }
    let text = match &args[0] {
        Value::String(s) => s.as_str(),
        other => {
            return err(format!(
                "regex call expects string, got {}",
                other.type_name()
            ))
        }
    };
    Ok(regex_lookup(regex, text, args.get(1).cloned()))
}

fn regex_lookup(regex: &RegexValue, text: &str, default: Option<Value>) -> Value {
    match regex_match_value(regex, text) {
        Some(val) => val,
        None => default.unwrap_or(Value::Nil),
    }
}

fn regex_match_value(regex: &RegexValue, text: &str) -> Option<Value> {
    let captures = regex.regex.captures(text)?;
    let full = captures.get(0)?;
    if full.start() != 0 || full.end() != text.len() {
        return None;
    }
    if captures.len() == 1 {
        return Some(Value::String(full.as_str().to_string()));
    }
    let mut items = Vector::new();
    for idx in 0..captures.len() {
        match captures.get(idx) {
            Some(m) => items.push_back(Value::String(m.as_str().to_string())),
            None => items.push_back(Value::Nil),
        }
    }
    Some(Value::Vector(items))
}

fn apply_indexer(head: &Value, args: &[Value]) -> Result<Value, CloveError> {
    if args.is_empty() {
        return err("indexer call expects target collection");
    }
    match &args[0] {
        Value::Vector(v) => apply_sequence_with_key(head, v, &args[1..], SequenceKind::Vector),
        Value::List(v) => apply_sequence_with_key(head, v, &args[1..], SequenceKind::List),
        Value::MutVector(v) => {
            let items = v.lock().unwrap_or_else(|e| e.into_inner());
            apply_sequence_with_key(head, &items, &args[1..], SequenceKind::Vector)
        }
        Value::String(s) => apply_string_with_key(head, s, &args[1..]),
        Value::Map(m) => apply_map_index(head, m, &args[1..]),
        Value::MutMap(m) => {
            let map = m.lock().unwrap_or_else(|e| e.into_inner());
            apply_map_index(head, &map, &args[1..])
        }
        Value::Set(s) => apply_set_index(head, s),
        Value::MutSet(s) => {
            let set = s.lock().unwrap_or_else(|e| e.into_inner());
            apply_set_index(head, &set)
        }
        other => err(format!("cannot apply index to {}", other.type_name())),
    }
}

fn apply_sequence_with_key(
    key: &Value,
    seq: &Vector<Value>,
    rest: &[Value],
    kind: SequenceKind,
) -> Result<Value, CloveError> {
    let mut args = Vec::with_capacity(rest.len() + 1);
    args.push(key.clone());
    args.extend_from_slice(rest);
    apply_sequence(seq, &args, kind)
}

fn apply_string_with_key(key: &Value, text: &str, rest: &[Value]) -> Result<Value, CloveError> {
    let mut args = Vec::with_capacity(rest.len() + 1);
    args.push(key.clone());
    args.extend_from_slice(rest);
    apply_string(text, &args)
}

fn apply_map_index(
    key: &Value,
    map: &HashMap<Key, Value>,
    rest: &[Value],
) -> Result<Value, CloveError> {
    let key_val = to_key_value_checked(key)?;
    match map.get(&key_val) {
        Some(val) => Ok(val.clone()),
        None => Ok(rest.get(0).cloned().unwrap_or(Value::Nil)),
    }
}

fn apply_set_index(key: &Value, set: &HashSet<Value>) -> Result<Value, CloveError> {
    if set.contains(key) {
        Ok(key.clone())
    } else {
        Ok(Value::Nil)
    }
}

pub(crate) enum SequenceKind {
    Vector,
    List,
}

pub(crate) enum LookupRequest {
    Index(i64),
    Range(RangeSpec),
    Indexes(Vec<i64>),
}

#[derive(Clone, Copy)]
pub(crate) struct RangeSpec {
    start: Option<i64>,
    end: Option<i64>,
    exclusive: bool,
}

pub(crate) fn parse_lookup_request(value: &Value) -> Result<LookupRequest, CloveError> {
    if let Some(spec) = parse_range_spec(value) {
        return Ok(LookupRequest::Range(spec));
    }
    if let Some(indexes) = parse_index_list(value)? {
        return Ok(LookupRequest::Indexes(indexes));
    }
    Ok(LookupRequest::Index(value_to_int(value)?))
}

fn parse_range_spec(value: &Value) -> Option<RangeSpec> {
    if let Value::Map(map) = value {
        match map.get(&Key::Keyword(RANGE_KEY.to_string())) {
            Some(Value::Bool(true)) => {
                let start = map.get(&Key::Keyword("start".to_string()));
                let end = map.get(&Key::Keyword("end".to_string()));
                let exclusive = map
                    .get(&Key::Keyword("exclusive".to_string()))
                    .and_then(|v| match v {
                        Value::Bool(b) => Some(*b),
                        _ => None,
                    })
                    .unwrap_or(false);
                let start_val = start.and_then(|v| match v {
                    Value::Nil => None,
                    Value::Int(n) => Some(*n),
                    Value::Float(f) if f.fract() == 0.0 => Some(*f as i64),
                    _ => None,
                });
                let end_val = end.and_then(|v| match v {
                    Value::Nil => None,
                    Value::Int(n) => Some(*n),
                    Value::Float(f) if f.fract() == 0.0 => Some(*f as i64),
                    _ => None,
                });
                return Some(RangeSpec {
                    start: start_val,
                    end: end_val,
                    exclusive,
                });
            }
            _ => {}
        }
    }
    None
}

fn parse_index_list(value: &Value) -> Result<Option<Vec<i64>>, CloveError> {
    match value {
        Value::Vector(items) | Value::List(items) => {
            let mut indexes = Vec::with_capacity(items.len());
            for item in items {
                indexes.push(value_to_int(item)?);
            }
            Ok(Some(indexes))
        }
        _ => Ok(None),
    }
}

fn value_to_int(value: &Value) -> Result<i64, CloveError> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Float(f) if f.fract() == 0.0 => Ok(*f as i64),
        _ => err("index must be integer"),
    }
}

pub(crate) fn normalize_index(
    idx: i64,
    len: usize,
    allow_equal: bool,
) -> Result<usize, CloveError> {
    let len_i64 = len as i64;
    let resolved = if idx < 0 { len_i64 + idx } else { idx };
    if allow_equal {
        if resolved < 0 || resolved > len_i64 {
            return err("index out of bounds");
        }
    } else if resolved < 0 || resolved >= len_i64 {
        return err("index out of bounds");
    }
    Ok(resolved as usize)
}

fn normalize_slice_start(idx: i64, len: usize) -> Result<usize, CloveError> {
    let len_i64 = len as i64;
    let resolved = if idx < 0 { len_i64 + idx } else { idx };
    if resolved < 0 || resolved > len_i64 {
        return err("index out of bounds");
    }
    Ok(resolved as usize)
}

fn normalize_slice_end(idx: i64, len: usize, exclusive: bool) -> usize {
    let len_i64 = len as i64;
    let resolved = if idx < 0 { len_i64 + idx } else { idx };
    let mut end = if exclusive { resolved } else { resolved + 1 };
    if end < 0 {
        end = 0;
    }
    if end > len_i64 {
        end = len_i64;
    }
    end as usize
}

pub(crate) fn slice_sequence(
    seq: &Vector<Value>,
    spec: RangeSpec,
    kind: SequenceKind,
) -> Result<Value, CloveError> {
    let len = seq.len();
    let start = match spec.start {
        Some(idx) => normalize_slice_start(idx, len)?,
        None => 0,
    };
    let mut end = match spec.end {
        Some(idx) => normalize_slice_end(idx, len, spec.exclusive),
        None => len,
    };
    if end < start {
        end = start;
    }
    let slice: Vector<Value> = seq.iter().skip(start).take(end - start).cloned().collect();
    Ok(match kind {
        SequenceKind::Vector => Value::Vector(slice),
        SequenceKind::List => Value::List(slice),
    })
}

pub(crate) fn gather_sequence(
    seq: &Vector<Value>,
    indexes: Vec<i64>,
    kind: SequenceKind,
) -> Result<Value, CloveError> {
    let len = seq.len();
    let mut collected = Vector::new();
    for idx in indexes {
        let resolved = normalize_index(idx, len, false)?;
        collected.push_back(seq[resolved].clone());
    }
    Ok(match kind {
        SequenceKind::Vector => Value::Vector(collected),
        SequenceKind::List => Value::List(collected),
    })
}

pub(crate) fn slice_string(chars: &[char], spec: RangeSpec) -> Result<Value, CloveError> {
    let len = chars.len();
    let start = match spec.start {
        Some(idx) => normalize_slice_start(idx, len)?,
        None => 0,
    };
    let mut end = match spec.end {
        Some(idx) => normalize_slice_end(idx, len, spec.exclusive),
        None => len,
    };
    if end < start {
        end = start;
    }
    let slice: String = chars[start..end].iter().collect();
    Ok(Value::String(slice))
}

pub(crate) fn gather_string(chars: &[char], indexes: Vec<i64>) -> Result<Value, CloveError> {
    let len = chars.len();
    let mut result = String::new();
    for idx in indexes {
        let resolved = normalize_index(idx, len, false)?;
        result.push(chars[resolved]);
    }
    Ok(Value::String(result))
}

fn plan_namespace_forms(forms: Vec<Form>) -> Result<NamespacePlan, CloveError> {
    let mut iter = forms.into_iter();
    let mut decl = None;
    let mut body = Vec::new();
    if let Some(first) = iter.next() {
        if is_ns_form(&first) {
            decl = Some(parse_ns_form(&first)?);
        } else {
            body.push(first);
        }
    }
    body.extend(iter);
    let mut inline_requires = Vec::new();
    let mut private_names = StdHashSet::new();
    let mut sanitized = Vec::new();
    for form in body {
        if is_require_form(&form) {
            if let FormKind::List(items) = &form.kind {
                let specs = parse_require_invocation(&items[1..], form.span)?;
                inline_requires.extend(specs);
                continue;
            }
        }
        if let Some(rewritten) = rewrite_private_def(&form, &mut private_names)? {
            sanitized.push(rewritten);
            continue;
        }
        sanitized.push(form);
    }
    Ok(NamespacePlan {
        decl,
        body_forms: sanitized,
        inline_requires,
        private_names,
    })
}

fn is_ns_form(form: &Form) -> bool {
    matches!(
        &form.kind,
        FormKind::List(items)
            if matches!(
                items.first().map(|f| &f.kind),
                Some(FormKind::Symbol(sym)) if sym == "ns"
            )
    )
}

fn is_require_form(form: &Form) -> bool {
    matches!(
        &form.kind,
        FormKind::List(items)
            if matches!(
                items.first().map(|f| &f.kind),
                Some(FormKind::Symbol(sym)) if sym == "require"
            )
    )
}

fn parse_ns_form(form: &Form) -> Result<NsDeclaration, CloveError> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return Err(span_error(form.span, "ns declaration must be a list")),
    };
    if items.len() < 2 {
        return Err(span_error(form.span, "ns requires a name"));
    }
    let name = match &items[1].kind {
        FormKind::Symbol(sym) => sym.clone(),
        _ => return Err(span_error(items[1].span, "namespace name must be a symbol")),
    };
    validate_ns_name(&name, items[1].span)?;
    let (doc, meta_form, idx) = parse_ns_doc_meta(items, 2)?;
    let mut decl = NsDeclaration {
        name,
        require_specs: Vec::new(),
        span: form.span,
        doc,
        meta_form,
        source: Some(form_source::form_to_source(form)),
    };
    for option in &items[idx..] {
        match &option.kind {
            FormKind::List(inner) if !inner.is_empty() => {
                let tag = symbol_dsl_name(&inner[0]).ok_or_else(|| {
                    span_error(option.span, "ns options must start with a keyword")
                })?;
                match tag.as_str() {
                    "require" => {
                        let specs = parse_require_invocation(&inner[1..], option.span)?;
                        decl.require_specs.extend(specs);
                    }
                    other => {
                        return Err(span_error(
                            option.span,
                            &format!("unknown ns option '{}'", other),
                        ))
                    }
                }
            }
            _ => return Err(span_error(option.span, "invalid ns option form")),
        }
    }
    Ok(decl)
}

fn parse_ns_doc_meta(
    items: &[Form],
    start_idx: usize,
) -> Result<(Option<String>, Option<Form>, usize), CloveError> {
    let mut idx = start_idx;
    let mut doc = None;
    let mut meta_form = None;
    while idx < items.len() {
        match &items[idx].kind {
            FormKind::String(text) => {
                if doc.is_some() {
                    return Err(span_error(items[idx].span, "ns allows only one docstring"));
                }
                doc = Some(text.clone());
                idx += 1;
            }
            FormKind::Map(_) => {
                if meta_form.is_some() {
                    return Err(span_error(items[idx].span, "ns allows only one attr-map"));
                }
                meta_form = Some(items[idx].clone());
                idx += 1;
            }
            _ => break,
        }
    }
    Ok((doc, meta_form, idx))
}

pub fn parse_require_invocation(args: &[Form], span: Span) -> Result<Vec<RequireSpec>, CloveError> {
    if args.is_empty() {
        return Err(span_error(span, "require expects at least one target"));
    }
    let mut specs = Vec::new();
    let mut idx = 0;
    while idx < args.len() {
        let form = &args[idx];
        match &form.kind {
            FormKind::Vector(items) => {
                specs.push(parse_rich_require_spec(items, form.span)?);
                idx += 1;
            }
            FormKind::List(items) => {
                if is_quote_form(form) {
                    let (spec, consumed) = parse_flat_require_spec(args, idx)?;
                    specs.push(spec);
                    idx += consumed;
                } else {
                    specs.push(parse_rich_require_spec(items, form.span)?);
                    idx += 1;
                }
            }
            FormKind::String(_) => {
                let (spec, consumed) = parse_flat_require_spec(args, idx)?;
                specs.push(spec);
                idx += consumed;
            }
            FormKind::Keyword(_) => {
                return Err(span_error(
                    form.span,
                    "require target cannot start with keyword",
                ));
            }
            _ => {
                let (spec, consumed) = parse_flat_require_spec(args, idx)?;
                specs.push(spec);
                idx += consumed;
            }
        }
    }
    Ok(specs)
}

pub fn parse_require_spec(form: &Form) -> Result<RequireSpec, CloveError> {
    let target_form = unwrap_quote(form);
    match &target_form.kind {
        FormKind::String(path) => Ok(RequireSpec {
            target: RequireTarget::FilePath(path.clone()),
            alias: None,
            refers: Vec::new(),
            rename: StdHashMap::new(),
            refer_all: false,
            span: form.span,
        }),
        FormKind::Symbol(sym) => {
            validate_ns_name(sym, form.span)?;
            Ok(RequireSpec {
                target: RequireTarget::Namespace(sym.clone()),
                alias: None,
                refers: Vec::new(),
                rename: StdHashMap::new(),
                refer_all: false,
                span: form.span,
            })
        }
        FormKind::Vector(items) => parse_rich_require_spec(items, form.span),
        FormKind::List(items) => parse_rich_require_spec(items, form.span),
        _ => Err(span_error(form.span, "unsupported require target")),
    }
}

fn parse_rich_require_spec(items: &[Form], span: Span) -> Result<RequireSpec, CloveError> {
    if items.is_empty() {
        return Err(span_error(span, "require spec cannot be empty"));
    }
    let target_form = unwrap_quote(&items[0]);
    let target = match &target_form.kind {
        FormKind::Symbol(sym) => {
            validate_ns_name(sym, items[0].span)?;
            RequireTarget::Namespace(sym.clone())
        }
        FormKind::String(path) => RequireTarget::FilePath(path.clone()),
        _ => {
            return Err(span_error(
                items[0].span,
                "require target must be a symbol or string",
            ))
        }
    };
    let mut spec = RequireSpec {
        target,
        alias: None,
        refers: Vec::new(),
        rename: StdHashMap::new(),
        refer_all: false,
        span,
    };
    let mut idx = 1;
    while idx < items.len() {
        let keyword = parse_symbol_dsl_atom(&items[idx], "expected keyword in require spec")?;
        idx += 1;
        let value = items
            .get(idx)
            .ok_or_else(|| span_error(span, "require option missing value"))?;
        idx += 1;
        match keyword.as_str() {
            "as" => {
                let alias = match &value.kind {
                    FormKind::Symbol(sym) => sym.clone(),
                    _ => return Err(span_error(value.span, ":as expects a symbol")),
                };
                spec.alias = Some(alias);
            }
            "refer" => match &value.kind {
                FormKind::Keyword(kw) if is_refer_all_literal(kw) => spec.refer_all = true,
                FormKind::Symbol(sym) if is_refer_all_literal(sym) => spec.refer_all = true,
                FormKind::Keyword(kw) => spec.refers.push(kw.clone()),
                FormKind::Symbol(sym) => spec.refers.push(sym.clone()),
                FormKind::Vector(list) | FormKind::List(list) => {
                    for item in list {
                        let name = symbol_literal_from_form(item, ":refer expects symbols")?;
                        spec.refers.push(name);
                    }
                }
                _ => {
                    return Err(span_error(
                        value.span,
                        ":refer expects :all or a list of symbols",
                    ))
                }
            },
            "rename" => {
                let map = parse_rename_map(value)?;
                spec.rename.extend(map);
            }
            "scope" => return Err(span_error(value.span, "require :scope has been removed")),
            other => {
                return Err(span_error(
                    value.span,
                    &format!("unknown require option '{}'", other),
                ))
            }
        }
    }
    Ok(spec)
}

fn parse_flat_require_spec(
    args: &[Form],
    start: usize,
) -> Result<(RequireSpec, usize), CloveError> {
    let mut end = start + 1;
    while end < args.len() {
        if !is_require_option_form(&args[end]) {
            break;
        }
        let keyword_form = &args[end];
        end += 1;
        if end >= args.len() {
            return Err(span_error(
                keyword_form.span,
                "require option missing value",
            ));
        }
        end += 1;
    }
    let slice = args[start..end].to_vec();
    let synthetic = Form::new(FormKind::Vector(slice), args[start].span);
    let spec = parse_require_spec(&synthetic)?;
    Ok((spec, end - start))
}

fn is_quote_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) if items.len() == 2 => {
            matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "quote")
        }
        _ => false,
    }
}

fn parse_rename_map(form: &Form) -> Result<StdHashMap<String, String>, CloveError> {
    match &form.kind {
        FormKind::Map(entries) => {
            let mut map = StdHashMap::new();
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let key = match &k.kind {
                            FormKind::Symbol(sym) => sym.clone(),
                            _ => return Err(span_error(k.span, ":rename keys must be symbols")),
                        };
                        let value = symbol_from_form(v, ":rename values must be symbols")?;
                        map.insert(key, value);
                    }
                    MapItem::Spread(expr) => {
                        return Err(span_error(
                            expr.span,
                            ":rename map does not support spread entries",
                        ))
                    }
                }
            }
            Ok(map)
        }
        _ => Err(span_error(form.span, ":rename expects a map")),
    }
}

fn symbol_dsl_name(form: &Form) -> Option<String> {
    match &form.kind {
        FormKind::Symbol(sym) => Some(sym.clone()),
        FormKind::Keyword(kw) => Some(kw.clone()),
        _ => None,
    }
}

fn parse_symbol_dsl_atom(form: &Form, msg: &str) -> Result<String, CloveError> {
    symbol_dsl_name(form).ok_or_else(|| span_error(form.span, msg))
}

fn symbol_literal_from_form(form: &Form, msg: &str) -> Result<String, CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => Ok(sym.clone()),
        FormKind::Keyword(kw) => Ok(kw.clone()),
        _ => Err(span_error(form.span, msg)),
    }
}

#[derive(Clone, Debug)]
pub struct LspNamespaceInfo {
    pub namespace: Option<String>,
    pub requires: Vec<RequireSpec>,
    pub namespace_span: Option<Span>,
}

/// LSP helper to extract ns/require info.
/// `forms` is expected to be the result of `parse_source_for_lsp`.
pub fn lsp_namespace_info(forms: &[Form]) -> Result<LspNamespaceInfo, CloveError> {
    let plan = plan_namespace_forms(forms.to_vec())?;
    let ns = plan.decl.as_ref().map(|d| d.name.clone());
    let ns_span = plan.decl.as_ref().map(|d| d.span);
    let mut requires = Vec::new();
    if let Some(decl) = &plan.decl {
        requires.extend(decl.require_specs.clone());
    }
    requires.extend(plan.inline_requires.clone());
    Ok(LspNamespaceInfo {
        namespace: ns,
        namespace_span: ns_span,
        requires,
    })
}

fn is_require_option_name(name: &str) -> bool {
    matches!(name, "as" | "refer" | "rename" | "scope")
}

fn is_require_option_form(form: &Form) -> bool {
    symbol_dsl_name(form)
        .map(|name| is_require_option_name(&name))
        .unwrap_or(false)
}

fn is_refer_all_literal(name: &str) -> bool {
    matches!(name, "all" | ":all" | "*" | ":*")
}

fn unwrap_quote<'a>(form: &'a Form) -> &'a Form {
    if let FormKind::List(items) = &form.kind {
        if items.len() == 2 {
            if let FormKind::Symbol(sym) = &items[0].kind {
                if sym == "quote" {
                    return &items[1];
                }
            }
        }
    }
    form
}

fn symbol_from_form(form: &Form, msg: &str) -> Result<String, CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => Ok(sym.clone()),
        _ => Err(span_error(form.span, msg)),
    }
}

fn extract_def_name(form: &Form) -> Result<String, CloveError> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return Err(span_error(form.span, "def form must be a list")),
    };
    if items.len() < 2 {
        return Err(span_error(form.span, "def requires a name"));
    }
    symbol_from_form(&items[1], "def name must be a symbol")
}

fn rewrite_list_head(form: &Form, head: &str) -> Form {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return form.clone(),
    };
    let mut rewritten = items.clone();
    if let Some(first) = rewritten.first_mut() {
        *first = Form::new(FormKind::Symbol(head.to_string()), first.span);
    }
    Form::new(FormKind::List(rewritten), form.span)
}

fn rewrite_private_def(
    form: &Form,
    private_names: &mut StdHashSet<String>,
) -> Result<Option<Form>, CloveError> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return Ok(None),
    };
    if items.is_empty() {
        return Ok(None);
    }
    let head_symbol = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return Ok(None),
    };
    match head_symbol {
        "def-" => {
            let name = extract_def_name(form)?;
            private_names.insert(name.clone());
            Ok(Some(rewrite_list_head(form, "def")))
        }
        "defn-" => {
            let name = extract_def_name(form)?;
            private_names.insert(name.clone());
            Ok(Some(rewrite_list_head(form, "defn")))
        }
        _ => Ok(None),
    }
}

fn build_refer_pairs(
    exports: &StdHashMap<String, Value>,
    spec: &RequireSpec,
) -> Result<Vec<(String, String)>, CloveError> {
    let mut pairs = Vec::new();
    let mut seen = StdHashSet::new();
    if spec.refer_all {
        for name in exports.keys() {
            let local = spec
                .rename
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone());
            pairs.push((name.clone(), local));
            seen.insert(name.clone());
        }
    }
    for name in &spec.refers {
        validate_symbol_name(name, spec.span)?;
        if seen.contains(name) {
            continue;
        }
        let local = spec
            .rename
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.clone());
        pairs.push((name.clone(), local));
        seen.insert(name.clone());
    }
    for (source, local) in &spec.rename {
        validate_symbol_name(source, spec.span)?;
        validate_symbol_name(local, spec.span)?;
        if seen.contains(source) {
            continue;
        }
        pairs.push((source.clone(), local.clone()));
        seen.insert(source.clone());
    }
    Ok(pairs)
}

fn namespace_segments<'a>(name: &'a str) -> impl Iterator<Item = &'a str> {
    name.split("::")
        .flat_map(|part| part.split('.'))
        .filter(|s| !s.is_empty())
}

fn sanitize_ns_segment(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-') {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        out.push('_');
    }
    let needs_prefix = out
        .chars()
        .next()
        .map(|ch| !(ch.is_ascii_alphabetic() || ch == '_'))
        .unwrap_or(true);
    if needs_prefix {
        out.insert(0, '_');
    }
    out
}

fn implicit_namespace_hash(path: &Path) -> String {
    let text = path.to_string_lossy();
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in text.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{:08x}", (hash & 0xffff_ffff) as u32)
}

fn append_namespace_suffix(ns: &str, suffix: &str) -> String {
    if let Some(pos) = ns.rfind("::") {
        let (head, tail) = ns.split_at(pos);
        let tail = &tail[2..];
        format!("{head}::{tail}{suffix}")
    } else {
        format!("{ns}{suffix}")
    }
}

fn namespace_to_path(name: &str) -> PathBuf {
    let mut buf = PathBuf::new();
    for segment in namespace_segments(name) {
        buf.push(segment);
    }
    buf
}

fn infer_namespace_root(ns: &str, path: &Path) -> Option<PathBuf> {
    let segments: Vec<&str> = namespace_segments(ns).collect();
    if segments.is_empty() {
        return None;
    }
    let stem = path.file_stem()?.to_str()?;
    if stem != *segments.last()? {
        return None;
    }
    let mut current = path.parent()?.to_path_buf();
    for segment in segments.iter().rev().skip(1) {
        let name = current.file_name()?.to_str()?;
        if name != *segment {
            return None;
        }
        current = current.parent()?.to_path_buf();
    }
    Some(current)
}

fn canonicalize_path(path: &Path) -> PathBuf {
    fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

fn package_info_for_path(path: &Path) -> (String, Option<PathBuf>) {
    let root = find_package_root(path);
    let pkg_id = root
        .as_ref()
        .map(|dir| canonicalize_path(dir).to_string_lossy().into_owned())
        .unwrap_or_else(|| MAIN_PACKAGE_ID.to_string());
    (pkg_id, root)
}

fn find_package_root(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()?.to_path_buf()
    };
    loop {
        if has_package_config(&current) {
            return Some(current);
        }
        if !current.pop() {
            break;
        }
    }
    None
}

fn has_package_config(dir: &Path) -> bool {
    dir.join(USER_TOML_FILE).exists() || dir.join(USER_CLV_FILE).exists()
}

fn detect_project_root(start: &Path) -> PathBuf {
    let mut current = start.to_path_buf();
    let mut found: Option<PathBuf> = None;
    loop {
        if has_package_config(&current) {
            found = Some(current.clone());
        }
        if !current.pop() {
            break;
        }
    }
    found.unwrap_or_else(|| start.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Form, Span, Value, Vector};
    use crate::env::EnvRef;
    use crate::foreign::ForeignEngine;
    use crate::options::EvalOptions;
    use crate::type_registry::{self, TypeEntry};
    use crate::types::TypeKind as MetaTypeKind;
    use std::ffi::OsString;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::Arc;
    use std::thread;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn runtime_ctx() -> Arc<RuntimeCtx> {
        let _guard = LocalUserConfigGuard::disable();
        RuntimeCtx::new(EvalOptions::default(), &[])
    }

    #[derive(Clone)]
    struct TagEchoEngine {
        tag: String,
    }

    impl TagEchoEngine {
        fn new(tag: &str) -> Self {
            Self {
                tag: tag.to_string(),
            }
        }
    }

    impl ForeignEngine for TagEchoEngine {
        fn tag(&self) -> &str {
            &self.tag
        }

        fn eval_block(
            &self,
            code: &str,
            _env: EnvRef,
            _span: Option<Span>,
        ) -> Result<Value, CloveError> {
            Ok(Value::String(format!("{}:{}", self.tag, code.trim())))
        }

        fn eval_fallback(&self, _form: &Form, _env: EnvRef) -> Result<Value, CloveError> {
            Err(CloveError::runtime("fallback not supported"))
        }

        fn call_symbol(
            &self,
            path: &str,
            _args: &[Value],
            _span: Option<Span>,
        ) -> Result<Value, CloveError> {
            Ok(Value::String(format!("{}::{}", self.tag, path)))
        }
    }

    fn runtime_ctx_with_foreign_engines(opts: EvalOptions) -> Arc<RuntimeCtx> {
        let _guard = LocalUserConfigGuard::disable();
        let engines: Vec<Arc<dyn ForeignEngine>> = vec![
            Arc::new(TagEchoEngine::new("rb")),
            Arc::new(TagEchoEngine::new("py")),
        ];
        RuntimeCtx::new(opts, &engines)
    }

    fn run_with_large_stack<F>(f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let handle = thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(f)
            .expect("failed to spawn test thread with larger stack");
        handle.join().expect("test thread panicked");
    }

    fn unique_ns_suffix() -> String {
        format!(
            "{}_{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos()
        )
    }

    struct LocalUserConfigGuard {
        prev: bool,
    }

    impl LocalUserConfigGuard {
        fn disable() -> Self {
            let prev = NO_USER_CONFIG_LOCAL.with(|flag| {
                let prev = flag.get();
                flag.set(true);
                prev
            });
            Self { prev }
        }
    }

    impl Drop for LocalUserConfigGuard {
        fn drop(&mut self) {
            NO_USER_CONFIG_LOCAL.with(|flag| flag.set(self.prev));
        }
    }

    struct EnvVarGuard {
        name: String,
        prev: Option<OsString>,
    }

    impl EnvVarGuard {
        fn remove(name: &str) -> Self {
            let prev = std::env::var_os(name);
            std::env::remove_var(name);
            Self {
                name: name.to_string(),
                prev,
            }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            if let Some(prev) = self.prev.take() {
                std::env::set_var(&self.name, prev);
            } else {
                std::env::remove_var(&self.name);
            }
        }
    }

    struct TempDir {
        path: PathBuf,
    }

    impl TempDir {
        fn new(prefix: &str) -> Self {
            let mut path = std::env::temp_dir();
            let unique = format!(
                "{}-{}-{}",
                prefix,
                std::process::id(),
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos()
            );
            path.push(unique);
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn escape_string_literal(value: &str) -> String {
        value.replace('\\', "\\\\").replace('"', "\\\"")
    }

    #[test]
    fn repl_path_has_current_runtime_and_file() {
        let mut ctx = runtime_ctx();
        let env = ctx.env();
        let file_name = format!("clove_repl_test_{}.clv", unique_ns_suffix());
        let path = std::env::temp_dir().join(file_name);
        fs::write(&path, "(def aaa 1)\n").expect("write repl temp file");
        let path_text = escape_string_literal(&path.to_string_lossy());
        let load_source = format!("(load-file \"{}\")", path_text);
        ctx.eval_repl_source(&load_source, env.clone(), None, None)
            .expect("load-file should succeed in repl path");
        let err = ctx
            .eval_repl_source("(unknown-symbol)", env.clone(), None, None)
            .expect_err("should fail with unknown symbol");
        assert_eq!(err.file(), Some("<repl>"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn dot_chain_enabled_by_default() {
        let mut ctx = runtime_ctx();
        let value = ctx
            .eval_source("(inc 10).(+ 1 ?)")
            .expect("dot-chain should be enabled");
        assert_eq!(value, Value::Int(12));
    }

    #[test]
    fn dot_chain_toggle_controls_availability() {
        let mut ctx = runtime_ctx();
        ctx.eval_source("(use dotchain-syntax false)")
            .expect("disable dot-chain");
        let err = ctx
            .eval_source("(inc 10).(+ 1 ?)")
            .expect_err("dot-chain should be disabled");
        assert!(
            err.to_string().contains("dot-chain syntax is disabled"),
            "unexpected error message: {}",
            err
        );
        ctx.eval_source("(use dot-chain true)")
            .expect("enable dot-chain for this ns");
        let value = ctx
            .eval_source("(inc 10).(+ 1 ?)")
            .expect("dot-chain should be enabled for this ns");
        assert_eq!(value, Value::Int(12));
    }

    #[test]
    fn top_level_err_fin_runs_in_eval_source() {
        let mut ctx = runtime_ctx();
        let value = ctx
            .eval_source(
                "(do (def x (atom 0))\n     (throw 1)\n     (err (do (atom-set! x 9) ?))\n     (fin (atom-set! x 10)))",
            )
            .expect("top-level err/fin should run");
        assert_eq!(value, Value::Int(1));
        let x_val = ctx.eval_source("@x").expect("read x");
        assert_eq!(x_val, Value::Int(10));
    }

    #[test]
    fn tail_recur_is_preserved_through_compiler_lowering() {
        let mut ctx = runtime_ctx();
        let value = ctx
            .eval_source(
                "(do (defn bump-to-three [x]\n                       (if (< x 3)\n                         (recur (inc x))\n                         x))\n                     (bump-to-three 0))",
            )
            .expect("tail recur should work after lowering");
        assert_eq!(value, Value::Int(3));
    }

    #[test]
    fn package_config_isolated_per_package() {
        let _guard = EnvVarGuard::remove("CLOVE_NO_USER_CONFIG");
        let temp = TempDir::new("clove-pkg-config");
        let pkg_a = temp.path().join("pkgA");
        let pkg_b = temp.path().join("pkgB");
        fs::create_dir_all(&pkg_a).expect("create pkgA dir");
        fs::create_dir_all(&pkg_b).expect("create pkgB dir");
        fs::write(pkg_a.join("user.toml"), "[syntax]\ndot-chain = false\n")
            .expect("write pkgA user.toml");
        fs::write(pkg_a.join("a.clv"), "(ns pkgA::a)\n(inc 10).(+ 1 ?)\n")
            .expect("write pkgA a.clv");
        fs::write(pkg_b.join("b.clv"), "(ns pkgB::b)\n(inc 10).(+ 1 ?)\n")
            .expect("write pkgB b.clv");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let err = ctx
            .eval_file(&pkg_a.join("a.clv"))
            .expect_err("pkgA should disable dot-chain");
        assert!(err.to_string().contains("dot-chain syntax is disabled"));
        let value = ctx
            .eval_file(&pkg_b.join("b.clv"))
            .expect("pkgB should keep dot-chain enabled");
        assert_eq!(value, Value::Int(12));
    }

    #[test]
    fn eval_file_uses_implicit_namespace_when_no_ns() {
        let temp = TempDir::new("clove-implicit-ns");
        let base = temp.path().join("examples").join("clodius");
        fs::create_dir_all(&base).expect("create example dir");
        let file = base.join("step02_wireframe.clv");
        fs::write(&file, "(def answer 42)\nanswer\n").expect("write script");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx.eval_file(&file).expect("eval file");
        assert_eq!(value, Value::Int(42));

        let ns = ctx
            .namespace_for_path(&file)
            .expect("namespace should be registered");
        assert_eq!(ns, "examples::clodius::step02_wireframe");
        let env = ctx.namespace_env(&ns).expect("namespace env");
        let got = env.read().unwrap().get("answer").unwrap_or(Value::Nil);
        assert_eq!(got, Value::Int(42));
    }

    #[test]
    fn load_file_splits_data_section() {
        let mut ctx = runtime_ctx();
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..");
        let file = repo_root.join("examples").join("__data01.clv");
        let path_text = escape_string_literal(&file.to_string_lossy());
        let expr = format!("(load-file \"{}\")", path_text);
        let value = ctx.eval_source(&expr).expect("load-file should succeed");
        let expected = "{\"a\":1,\"b\":2}\n(unknown-symbol)\n";
        assert_eq!(value, Value::String(expected.to_string()));
    }

    #[test]
    fn eval_file_supports_end_marker_data_section() {
        let mut ctx = runtime_ctx();
        let temp = TempDir::new("clove-data-end");
        let file = temp.path().join("end.clv");
        fs::write(&file, "(def data __DATA__)\ndata\n__END__\nhello\n")
            .expect("write end marker file");
        let value = ctx.eval_file(&file).expect("eval file");
        assert_eq!(value, Value::String("hello\n".to_string()));
    }

    #[test]
    fn data_marker_with_whitespace_is_not_split() {
        let mut ctx = runtime_ctx();
        let temp = TempDir::new("clove-data-space");
        let file = temp.path().join("space.clv");
        fs::write(&file, "(def x 1)\n  __DATA__  \n(+ x 2)\n").expect("write space file");
        let value = ctx.eval_file(&file).expect("eval file");
        assert_eq!(value, Value::Int(3));
    }

    #[test]
    fn data_symbol_is_nil_without_marker() {
        let mut ctx = runtime_ctx();
        let temp = TempDir::new("clove-data-none");
        let file = temp.path().join("none.clv");
        fs::write(&file, "(def data __DATA__)\ndata\n").expect("write none file");
        let value = ctx.eval_file(&file).expect("eval file");
        assert_eq!(value, Value::Nil);
    }

    #[test]
    fn repl_uses_data_section_from_repl_source() {
        let mut ctx = runtime_ctx();
        let temp = TempDir::new("clove-repl-data");
        let file = temp.path().join("repl_data.clv");
        fs::write(&file, "(def data __DATA__)\n__DATA__\nhello\n").expect("write repl file");
        let _ = ctx.eval_file(&file).expect("eval file");
        ctx.set_repl_data_source(Some(file));
        let env = ctx.env();
        let value = ctx
            .eval_repl_source("__DATA__", env, Some("<repl>"), None)
            .expect("repl __DATA__");
        assert_eq!(value, Value::String("hello\n".to_string()));
    }

    #[test]
    fn user_clv_overrides_user_toml() {
        let _guard = EnvVarGuard::remove("CLOVE_NO_USER_CONFIG");
        let temp = TempDir::new("clove-user-clv-override");
        let pkg = temp.path().join("pkg");
        fs::create_dir_all(&pkg).expect("create pkg dir");
        fs::write(pkg.join("user.toml"), "[syntax]\ndot-chain = true\n").expect("write user.toml");
        fs::write(pkg.join("user.clv"), "(use dot-chain false)\n").expect("write user.clv");
        fs::write(pkg.join("main.clv"), "(ns pkg::main)\n(inc 10).(+ 1 ?)\n")
            .expect("write main.clv");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let err = ctx
            .eval_file(&pkg.join("main.clv"))
            .expect_err("user.clv should override user.toml");
        assert!(err.to_string().contains("dot-chain syntax is disabled"));
    }

    #[test]
    fn user_clv_rejects_non_use_forms() {
        let _guard = EnvVarGuard::remove("CLOVE_NO_USER_CONFIG");
        let temp = TempDir::new("clove-user-clv-guard");
        let pkg = temp.path().join("pkg");
        fs::create_dir_all(&pkg).expect("create pkg dir");
        fs::write(pkg.join("user.clv"), "(println 1)\n").expect("write user.clv");
        fs::write(pkg.join("main.clv"), "(ns pkg::main)\n1\n").expect("write main.clv");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let err = ctx
            .eval_file(&pkg.join("main.clv"))
            .expect_err("user.clv should reject non-use form");
        assert!(err
            .to_string()
            .contains("user.clv allows only (use <feature> true|false)"));
    }

    #[test]
    fn default_require_applied_to_namespaces() {
        run_with_large_stack(|| {
            let temp = TempDir::new("clove-default-require");
            let opts = EvalOptions {
                working_dir: Some(temp.path().to_path_buf()),
                apply_working_dir: false,
                ..EvalOptions::default()
            };
            let ctx = RuntimeCtx::new(opts, &[]);
            let value = ctx
                .eval_source("(ns demo)\n(interleave [1 2] [10 20])")
                .expect("default require applied to namespace");
            assert_eq!(
                value,
                Value::Vector(Vector::from(vec![
                    Value::Int(1),
                    Value::Int(10),
                    Value::Int(2),
                    Value::Int(20),
                ]))
            );
        });
    }

    #[test]
    fn require_imports_types_and_aliases() {
        let temp = TempDir::new("clove-type-import");
        let _guard = LocalUserConfigGuard::disable();
        let suffix = unique_ns_suffix();
        let ns1 = format!("tmp::ns1_{}", suffix);
        let ns2 = format!("tmp::ns2_{}", suffix);

        let tmp_dir = temp.path().join("tmp");
        fs::create_dir_all(&tmp_dir).expect("create tmp dir");
        let ns1_file = tmp_dir.join(format!("ns1_{}.clv", suffix));
        let ns2_file = tmp_dir.join(format!("ns2_{}.clv", suffix));

        fs::write(
            &ns1_file,
            format!("(ns {ns1})\n(defenum Action\n  Noop {{:x Int}}\n  Quit)\n"),
        )
        .expect("write ns1");

        let action_noop = format!("{ns1}::Action::Noop");
        fs::write(
            &ns2_file,
            format!(
                "(ns {ns2})\n(require {ns1} :as ns1 :refer :*)\n(deftype T1 {{:a Noop}})\n(deftype T2 {{:a ns1::Noop}})\n(deftype T3 {{:a {action_noop}}})\n",
            ),
        )
        .expect("write ns2");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        ctx.eval_file(&ns2_file).expect("require type import");

        ctx.with_current_ctx(|_ctx| {
            let imported_alias = format!("{}::Noop", ns2);
            let target_alias = format!("{}::Noop", ns1);
            let imported_entry =
                type_registry::get_type_entry(&imported_alias).expect("imported alias");
            match imported_entry {
                TypeEntry::Alias(meta) => {
                    assert_eq!(meta.target, MetaTypeKind::named(target_alias));
                }
                other => panic!("expected alias, got {:?}", other),
            }

            let quit_alias = format!("{}::Quit", ns1);
            let quit_target = format!("{}::Action::Quit", ns1);
            let quit_entry = type_registry::get_type_entry(&quit_alias).expect("quit alias");
            match quit_entry {
                TypeEntry::Alias(meta) => {
                    assert_eq!(meta.target, MetaTypeKind::named(quit_target));
                }
                other => panic!("expected alias, got {:?}", other),
            }
        });
    }

    #[test]
    fn type_ctor_shorthand_accepts_label_keys_for_imported_alias() {
        let temp = TempDir::new("clove-type-ctor-label");
        let _guard = LocalUserConfigGuard::disable();
        let suffix = unique_ns_suffix();
        let ns1 = format!("tmp::ctor1_{}", suffix);
        let ns2 = format!("tmp::ctor2_{}", suffix);

        let tmp_dir = temp.path().join("tmp");
        fs::create_dir_all(&tmp_dir).expect("create tmp dir");
        let ns1_file = tmp_dir.join(format!("ctor1_{}.clv", suffix));
        let ns2_file = tmp_dir.join(format!("ctor2_{}.clv", suffix));

        fs::write(
            &ns1_file,
            format!("(ns {ns1})\n(deftype Bird {{:y Int :vy Int}})\n"),
        )
        .expect("write ns1");
        fs::write(
            &ns2_file,
            format!("(ns {ns2})\n(require {ns1} :refer :*)\n(def b (Bird y: 1, vy: 2))\nb\n"),
        )
        .expect("write ns2");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx.eval_file(&ns2_file).expect("label style constructor");

        match value {
            Value::Map(map) => {
                assert_eq!(
                    map.get(&Key::Keyword("y".to_string())),
                    Some(&Value::Int(1))
                );
                assert_eq!(
                    map.get(&Key::Keyword("vy".to_string())),
                    Some(&Value::Int(2))
                );
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn match_resolves_enum_variant_through_type_alias() {
        let temp = TempDir::new("clove-match-alias");
        let _guard = LocalUserConfigGuard::disable();
        let suffix = unique_ns_suffix();
        let ns1 = format!("tmp::match_alias_a_{}", suffix);
        let ns2 = format!("tmp::match_alias_b_{}", suffix);

        let tmp_dir = temp.path().join("tmp");
        fs::create_dir_all(&tmp_dir).expect("create tmp dir");
        let ns1_file = tmp_dir.join(format!("match_alias_a_{}.clv", suffix));
        let ns2_file = tmp_dir.join(format!("match_alias_b_{}.clv", suffix));

        fs::write(
            &ns1_file,
            format!("(ns {ns1})\n(defenum Mode Running GameOver)\n"),
        )
        .expect("write ns1");
        fs::write(
            &ns2_file,
            format!(
                "(ns {ns2})\n(require {ns1} :refer :*)\n(let [mode {{:type '{ns1}::Mode::GameOver}}]\n  (match mode\n    Mode::Running 1\n    Mode::GameOver 2\n    _ 3))\n",
            ),
        )
        .expect("write ns2");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx.eval_file(&ns2_file).expect("match enum alias");
        assert_eq!(value, Value::Int(2));
    }

    #[test]
    fn type_pattern_keyword_shorthand_in_bind_and_match() {
        let temp = TempDir::new("clove-type-pattern-kw");
        let _guard = LocalUserConfigGuard::disable();
        let suffix = unique_ns_suffix();
        let ns = format!("tmp::type_pattern_kw_{}", suffix);

        let tmp_dir = temp.path().join("tmp");
        fs::create_dir_all(&tmp_dir).expect("create tmp dir");
        let file = tmp_dir.join(format!("type_pattern_kw_{}.clv", suffix));

        fs::write(
            &file,
            format!(
                "(ns {ns})\n(deftype Pair {{:a Int :b Int}})\n(let [v (Pair :a 1 :b 2)\n      res1 (let [(Pair :a :b) v] (+ a b))\n      res2 (match v\n             (Pair :a :b) (+ a b)\n             _ 0)]\n  (+ res1 res2))\n"
            ),
        )
        .expect("write file");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx.eval_file(&file).expect("keyword shorthand pattern");
        assert_eq!(value, Value::Int(6));
    }

    #[test]
    fn flat_file_require_accepts_refer_options() {
        let temp = TempDir::new("clove-file-require-flat");
        let _guard = LocalUserConfigGuard::disable();
        fs::write(
            temp.path().join("match_type.clv"),
            "(def a 1)\n(defn get-a [] a)\n",
        )
        .expect("write match_type");
        fs::write(
            temp.path().join("match_type1.clv"),
            "(ns tmp::match_type1)\n(require \"./match_type\" :refer :*)\n(def y (get-a))\ny\n",
        )
        .expect("write match_type1");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx
            .eval_file(&temp.path().join("match_type1.clv"))
            .expect("flat file require with refer");
        assert_eq!(value, Value::Int(1));
    }

    #[test]
    fn rich_file_require_supports_alias_and_refer() {
        let temp = TempDir::new("clove-file-require-rich");
        let _guard = LocalUserConfigGuard::disable();
        fs::write(
            temp.path().join("match_type.clv"),
            "(def a 1)\n(defn get-a [] a)\n",
        )
        .expect("write match_type");
        fs::write(
            temp.path().join("match_type2.clv"),
            "(ns tmp::match_type2\n  (:require [\"./match_type\" :as mt :refer :*]))\n(def y (get-a))\n(def z (mt::get-a))\n(+ y z)\n",
        )
        .expect("write match_type2");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value = ctx
            .eval_file(&temp.path().join("match_type2.clv"))
            .expect("rich file require with alias");
        assert_eq!(value, Value::Int(2));
    }

    #[test]
    fn file_require_applies_imports_per_namespace() {
        let temp = TempDir::new("clove-file-require-imports");
        let _guard = LocalUserConfigGuard::disable();
        fs::write(temp.path().join("match_type.clv"), "(defn get-a [] 1)\n")
            .expect("write match_type");
        fs::write(
            temp.path().join("caller_a.clv"),
            "(ns tmp::caller_a)\n(require \"./match_type\" :refer [get-a])\n(get-a)\n",
        )
        .expect("write caller_a");
        fs::write(
            temp.path().join("caller_b.clv"),
            "(ns tmp::caller_b)\n(require \"./match_type\" :refer [get-a])\n(get-a)\n",
        )
        .expect("write caller_b");

        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = RuntimeCtx::new(opts, &[]);
        let value_a = ctx
            .eval_file(&temp.path().join("caller_a.clv"))
            .expect("require in caller_a");
        assert_eq!(value_a, Value::Int(1));
        let value_b = ctx
            .eval_file(&temp.path().join("caller_b.clv"))
            .expect("require in caller_b");
        assert_eq!(value_b, Value::Int(1));
    }

    #[test]
    fn foreign_blocks_can_be_disabled() {
        let mut ctx = runtime_ctx();
        let err = ctx
            .eval_source("(do (use foreign false) $py{println \"hi\"})")
            .expect_err("foreign blocks disabled");
        assert!(
            err.to_string().contains("foreign blocks are disabled"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn file_default_tag_uses_default_interop() {
        let temp = TempDir::new("clove-use-interop");
        let file = temp.path().join("use_interop.clv");
        fs::write(&file, "(use default-interop :py)\n${child}\n").expect("write use_interop");
        let ctx = runtime_ctx_with_foreign_engines(EvalOptions::default());
        let value = ctx.eval_file(&file).expect("eval use_interop");
        assert_eq!(value, Value::String("py:child".to_string()));
    }

    #[test]
    fn file_default_tag_uses_extension() {
        let temp = TempDir::new("clove-use-ext");
        let file = temp.path().join("use_ext.py.clv");
        fs::write(&file, "${ext}\n").expect("write use_ext");
        let ctx = runtime_ctx_with_foreign_engines(EvalOptions::default());
        let value = ctx.eval_file(&file).expect("eval use_ext");
        assert_eq!(value, Value::String("py:ext".to_string()));
    }

    #[test]
    fn file_default_tag_extension_overrides_use() {
        let temp = TempDir::new("clove-use-ext-override");
        let file = temp.path().join("use_ext_override.py.clv");
        fs::write(&file, "(use default-foreign :rb)\n${conflict}\n").expect("write use_ext");
        let ctx = runtime_ctx_with_foreign_engines(EvalOptions::default());
        let value = ctx.eval_file(&file).expect("eval use_ext");
        assert_eq!(value, Value::String("py:conflict".to_string()));
    }

    #[test]
    fn file_default_tag_falls_back_to_ruby() {
        let temp = TempDir::new("clove-use-default");
        let file = temp.path().join("use_default.clv");
        fs::write(&file, "${fallback}\n").expect("write use_default");
        let ctx = runtime_ctx_with_foreign_engines(EvalOptions::default());
        let value = ctx.eval_file(&file).expect("eval use_default");
        assert_eq!(value, Value::String("rb:fallback".to_string()));
    }

    #[test]
    fn require_uses_file_default_without_leak() {
        let temp = TempDir::new("clove-use-require");
        let child = temp.path().join("child.rb.clv");
        fs::write(&child, "(def child-tag ${child})\n").expect("write child");
        let parent = temp.path().join("parent.clv");
        fs::write(
            &parent,
            "(ns tmp::parent)\n(use default-interop :py)\n(require \"./child.rb.clv\" :refer :*)\n(def parent-tag ${parent})\n[child-tag parent-tag]\n",
        )
        .expect("write parent");
        let opts = EvalOptions {
            working_dir: Some(temp.path().to_path_buf()),
            apply_working_dir: false,
            ..EvalOptions::default()
        };
        let ctx = runtime_ctx_with_foreign_engines(opts);
        let value = ctx.eval_file(&parent).expect("eval parent");
        assert_eq!(
            value,
            Value::Vector(Vector::from(vec![
                Value::String("rb:child".to_string()),
                Value::String("py:parent".to_string())
            ]))
        );
    }

    #[test]
    fn private_defs_are_not_accessible_from_other_namespaces() {
        let mut ctx = runtime_ctx();
        ctx.eval_source(
            "(ns a)
             (def- x 1)
             (defn t [] (set! x 2) x)",
        )
        .expect("define private var");
        let value = ctx
            .eval_source("(ns a)\n(t)")
            .expect("set! within same ns should work");
        assert_eq!(value, Value::Int(2));

        let err = ctx
            .eval_source("(ns b)\n(set! a::x 3)")
            .expect_err("private set! should be rejected");
        assert!(
            err.to_string()
                .contains("set! cannot access private var from another namespace"),
            "unexpected error: {}",
            err
        );
        let err = ctx
            .eval_source("(ns b)\na::x")
            .expect_err("private access should be rejected");
        assert!(
            err.to_string().contains("Unbound symbol"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn std_registers_metadata() {
        let _guard = EnvVarGuard::remove("CLOVE_NO_USER_CONFIG");
        crate::fn_meta::clear_for_tests();
        let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
        let meta = ctx.with_current_ctx(|ctx| {
            let _ = ctx
                .eval_source("(std::map inc [1 2])")
                .expect("std map should work");
            crate::fn_meta::get("std::map").expect("metadata for std::map")
        });
        let arg_sig = meta.arglist.get(0).cloned().unwrap_or_default();
        assert!(
            arg_sig.contains("f") && arg_sig.contains("coll"),
            "unexpected arglist: {}",
            arg_sig
        );
        assert!(meta.doc.unwrap_or_default().to_lowercase().contains("map"));
    }

    #[test]
    fn clove_std_collection_functions_cover_common_paths() {
        run_with_large_stack(|| {
            let _guard = EnvVarGuard::remove("CLOVE_NO_USER_CONFIG");
            let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);

            let concat_val = ctx
                .eval_source("(std::concat [1 2] '(3 4) [5])")
                .expect("std concat works");
            let expected_concat = ctx
                .eval_source("[1 2 3 4 5]")
                .expect("literal concat vector");
            assert_eq!(concat_val, expected_concat);

            let mapcat_val = ctx
                .eval_source("(std::mapcat (fn [x] [x (* 10 x)]) [1 2 3])")
                .expect("std mapcat works");
            let expected_mapcat = ctx
                .eval_source("[1 10 2 20 3 30]")
                .expect("literal mapcat vector");
            assert_eq!(mapcat_val, expected_mapcat);

            let zipmap_val = ctx
                .eval_source("(std::zipmap [:a :b :c] [1 2])")
                .expect("std zipmap works");
            let expected_zipmap = ctx.eval_source("{:a 1 :b 2}").expect("literal zipmap");
            assert_eq!(zipmap_val, expected_zipmap);

            let interleave_val = ctx
                .eval_source("(std::interleave [1 2] [:a :b] (list \"x\" \"y\"))")
                .expect("std interleave works");
            let expected_interleave = ctx
                .eval_source("[1 :a \"x\" 2 :b \"y\"]")
                .expect("literal interleave");
            assert_eq!(interleave_val, expected_interleave);

            let interpose = ctx
                .eval_source("(std::interpose \":\" [\"a\" \"b\" \"c\"])")
                .expect("std interpose works");
            let expected_interpose = ctx
                .eval_source("[\"a\" \":\" \"b\" \":\" \"c\"]")
                .expect("literal interpose");
            assert_eq!(interpose, expected_interpose);

            let partition_val = ctx
                .eval_source("(std::partition 3 2 [0 0] [1 2 3 4 5])")
                .expect("std partition works");
            let expected_partition = ctx
                .eval_source("[[1 2 3] [3 4 5] [5 0 0]]")
                .expect("literal partition");
            assert_eq!(partition_val, expected_partition);

            let partition_all_val = ctx
                .eval_source("(std::partition-all 3 2 [1 2 3 4 5])")
                .expect("std partition-all works");
            let expected_partition_all = ctx
                .eval_source("[[1 2 3] [3 4 5] [5]]")
                .expect("literal partition-all");
            assert_eq!(partition_all_val, expected_partition_all);

            let partition_by_val = ctx
                .eval_source("(std::partition-by (fn [x] (= 0 (mod x 2))) [1 3 2 4 5])")
                .expect("std partition-by works");
            let expected_partition_by = ctx
                .eval_source("[[1 3] [2 4] [5]]")
                .expect("literal partition-by");
            assert_eq!(partition_by_val, expected_partition_by);

            let flatten_val = ctx
                .eval_source("(std::flatten [1 [2 nil [3 []]] 4])")
                .expect("std flatten works");
            let expected_flatten = ctx.eval_source("[1 2 3 4]").expect("literal flatten");
            assert_eq!(flatten_val, expected_flatten);

            let freq_val = ctx
                .eval_source("(std::frequencies [:a :b :a :c :a :b])")
                .expect("std frequencies works");
            let expected_freq = ctx
                .eval_source("{:a 3 :b 2 :c 1}")
                .expect("literal frequencies");
            assert_eq!(freq_val, expected_freq);

            let distinct_val = ctx
                .eval_source("(std::distinct [1 2 1 3 2 4 3])")
                .expect("std distinct works");
            let expected_distinct = ctx.eval_source("[1 2 3 4]").expect("literal distinct");
            assert_eq!(distinct_val, expected_distinct);

            let dedupe_val = ctx
                .eval_source("(std::dedupe [1 1 2 2 2 3 1 1])")
                .expect("std dedupe works");
            let expected_dedupe = ctx.eval_source("[1 2 3 1]").expect("literal dedupe");
            assert_eq!(dedupe_val, expected_dedupe);

            let group_by_val = ctx
                .eval_source("(std::group-by (fn [x] (mod x 2)) [1 2 3 4])")
                .expect("std group-by works");
            let expected_group_by = ctx
                .eval_source("{1 [1 3] 0 [2 4]}")
                .expect("literal group-by");
            assert_eq!(group_by_val, expected_group_by);

            let every_true = ctx
                .eval_source("(std::every? (fn [x] (< x 10)) [1 3 5])")
                .expect("std every? true");
            assert_eq!(every_true, Value::Bool(true));
            let every_false = ctx
                .eval_source("(std::every? (fn [x] (< x 3)) [1 2 3])")
                .expect("std every? false");
            assert_eq!(every_false, Value::Bool(false));

            let not_any = ctx
                .eval_source("(std::not-any? (fn [x] (> x 5)) [1 3 5])")
                .expect("std not-any?");
            assert_eq!(not_any, Value::Bool(true));

            let juxt_val = ctx
                .eval_source("((std::juxt inc dec str) 10)")
                .expect("std juxt works");
            let expected_juxt = ctx.eval_source("[11 9 \"10\"]").expect("literal juxt");
            assert_eq!(juxt_val, expected_juxt);

            let get_in_val = ctx
                .eval_source("(std::get-in {:a {:b {:c 10}}} [:a :b :c])")
                .expect("std get-in works");
            assert_eq!(get_in_val, Value::Int(10));
            let get_in_default = ctx
                .eval_source("(std::get-in {:a 1} [:b :c] 42)")
                .expect("std get-in default");
            assert_eq!(get_in_default, Value::Int(42));

            let assoc_in_val = ctx
                .eval_source("(std::assoc-in {} [:a :b :c] 99)")
                .expect("std assoc-in works");
            let expected_assoc_in = ctx
                .eval_source("{:a {:b {:c 99}}}")
                .expect("literal assoc-in");
            assert_eq!(assoc_in_val, expected_assoc_in);

            let update_in_val = ctx
                .eval_source("(std::update-in {:a {:b 1}} [:a :b] inc)")
                .expect("std update-in works");
            let expected_update_in = ctx.eval_source("{:a {:b 2}}").expect("literal update-in");
            assert_eq!(update_in_val, expected_update_in);

            let select_keys_val = ctx
                .eval_source("(std::select-keys {:a 1 :b 2 :c 3} [:a :c])")
                .expect("std select-keys works");
            let expected_select_keys = ctx.eval_source("{:a 1 :c 3}").expect("literal select-keys");
            assert_eq!(select_keys_val, expected_select_keys);

            let merge_with_sum = ctx
                .eval_source("(std::merge-with + {:a 1 :b 2} {:a 3 :c 4})")
                .expect("std merge-with sum");
            let expected_merge_sum = ctx
                .eval_source("{:a 4 :b 2 :c 4}")
                .expect("literal merge-with sum");
            assert_eq!(merge_with_sum, expected_merge_sum);

            let merge_with_vec = ctx
                .eval_source("(std::merge-with vector {:a 1} {:a 2} {:a 3 :b 4})")
                .expect("std merge-with vector");
            let expected_merge_vec = ctx
                .eval_source("{:a [1 2 3] :b 4}")
                .expect("literal merge-with vector");
            assert_eq!(merge_with_vec, expected_merge_vec);

            let sort_default = ctx
                .eval_source("(std::sort [3 1 2 2])")
                .expect("std sort default");
            let expected_sort_default = ctx.eval_source("[1 2 2 3]").expect("literal sort");
            assert_eq!(sort_default, expected_sort_default);

            let sort_custom = ctx
                .eval_source("(std::sort (fn [a b] (> a b)) [1 2 3 0])")
                .expect("std sort custom");
            let expected_sort_custom = ctx.eval_source("[3 2 1 0]").expect("literal desc sort");
            assert_eq!(sort_custom, expected_sort_custom);

            let sort_by_count = ctx
                .eval_source("(std::sort-by count [\"aaa\" \"b\" \"cc\"])")
                .expect("std sort-by count");
            let expected_sort_by_count = ctx
                .eval_source("[\"b\" \"cc\" \"aaa\"]")
                .expect("literal sort-by");
            assert_eq!(sort_by_count, expected_sort_by_count);

            let sort_by_custom = ctx
                .eval_source(
                    "(std::sort-by first (fn [a b] (core::compare b a)) [[1 2] [2 1] [0 5]])",
                )
                .expect("std sort-by custom comparator");
            let expected_sort_by_custom = ctx
                .eval_source("[[2 1] [1 2] [0 5]]")
                .expect("literal sort-by custom");
            assert_eq!(sort_by_custom, expected_sort_by_custom);

            let re_find_simple = ctx
                .eval_source("(re-find #/\\d+/ \"foo123bar\")")
                .expect("re-find simple");
            assert_eq!(re_find_simple, Value::String("123".into()));

            let re_find_groups = ctx
                .eval_source("(re-find #/(\\d+)-(\\w+)/ \"123-abc-zzz\")")
                .expect("re-find groups");
            let expected_re_find_groups = ctx
                .eval_source("[\"123-abc\" \"123\" \"abc\"]")
                .expect("literal capture vector");
            assert_eq!(re_find_groups, expected_re_find_groups);

            let re_matches_hit = ctx
                .eval_source("(re-matches #/\\d+/ \"12345\")")
                .expect("re-matches hit");
            assert_eq!(re_matches_hit, Value::String("12345".into()));

            let re_matches_miss = ctx
                .eval_source("(re-matches #/\\d+/ \"abc123\")")
                .expect("re-matches miss");
            assert_eq!(re_matches_miss, Value::Nil);
        });
    }

    #[test]
    fn indexer_toggle_controls_availability() {
        let mut ctx = runtime_ctx();
        let value = ctx
            .eval_source("{:a 1}[:a]")
            .expect("indexer should be enabled");
        assert_eq!(value, Value::Int(1));
        ctx.eval_source("(use indexer false)")
            .expect("disable indexer");
        let err = ctx
            .eval_source("{:a 1}[:a]")
            .expect_err("indexer should be disabled");
        assert!(
            err.to_string().contains("indexer syntax is disabled"),
            "unexpected error message: {}",
            err
        );
        ctx.eval_source("(use indexer true)")
            .expect("enable indexer for this ns");
        let enabled = ctx.eval_source("{:a 1}[:a]").expect("indexer re-enabled");
        assert_eq!(enabled, Value::Int(1));
    }

    #[test]
    fn reduce_follows_clojuredocs_behaviors() {
        let mut ctx = runtime_ctx();
        let zero_default = ctx
            .eval_source("(reduce + [])")
            .expect("reduce should call zero-arity f on empty coll");
        assert_eq!(zero_default, Value::Int(0));

        let init_only = ctx
            .eval_source("(reduce + 10 [])")
            .expect("reduce should return init on empty coll");
        assert_eq!(init_only, Value::Int(10));

        let summed = ctx
            .eval_source("(reduce + 1 [2 3 4])")
            .expect("reduce with init should accumulate");
        assert_eq!(summed, Value::Int(10));

        let as_set = ctx
            .eval_source("(reduce conj #{} [:x :y :x])")
            .expect("reduce conj into set");
        let expected_set = ctx.eval_source("#{ :x :y }").expect("literal set");
        assert_eq!(as_set, expected_set);
    }

    #[test]
    fn map_handles_lazy_and_parallel_inputs() {
        let mut ctx = runtime_ctx();
        let parallel = ctx
            .eval_source("(map + [1 2 3] [10 20 30])")
            .expect("map over two vectors");
        let expected_parallel = ctx
            .eval_source("[11 22 33]")
            .expect("expected mapped vector");
        assert_eq!(parallel, expected_parallel);

        let lazy_take = ctx
            .eval_source("(vec (take 4 (map inc (range))))")
            .expect("map should stay lazy on seq inputs and realize correctly");
        let expected_lazy = ctx.eval_source("[1 2 3 4]").expect("expected lazy take");
        assert_eq!(lazy_take, expected_lazy);
    }

    #[test]
    fn range_and_repeat_cover_common_paths() {
        let mut ctx = runtime_ctx();
        let forward = ctx
            .eval_source("(range 2 8 3)")
            .expect("bounded forward range");
        let expected_forward = ctx.eval_source("[2 5]").expect("expected forward");
        assert_eq!(forward, expected_forward);

        let backward = ctx
            .eval_source("(range 5 0 -2)")
            .expect("bounded backward range");
        let expected_backward = ctx.eval_source("[5 3 1]").expect("expected backward");
        assert_eq!(backward, expected_backward);

        let infinite_prefix = ctx
            .eval_source("(vec (take 5 (range)))")
            .expect("prefix of infinite range");
        let expected_prefix = ctx.eval_source("[0 1 2 3 4]").expect("expected prefix");
        assert_eq!(infinite_prefix, expected_prefix);

        let err = ctx
            .eval_source("(range 0 5 0)")
            .expect_err("step 0 should error");
        assert!(
            err.to_string().contains("step cannot be zero"),
            "unexpected error: {}",
            err
        );

        let repeating_lazy = ctx
            .eval_source("(vec (take 3 (repeat :z)))")
            .expect("repeat one-arg lazy");
        let expected_repeating_lazy = ctx.eval_source("[:z :z :z]").expect("expected lazy");
        assert_eq!(repeating_lazy, expected_repeating_lazy);

        let repeating_eager = ctx
            .eval_source("(repeat 4 {:a 1})")
            .expect("repeat two-arg eager");
        let expected_repeating_eager = ctx
            .eval_source("[{:a 1} {:a 1} {:a 1} {:a 1}]")
            .expect("expected eager");
        assert_eq!(repeating_eager, expected_repeating_eager);
    }

    #[test]
    fn division_matches_single_and_multi_arity() {
        let mut ctx = runtime_ctx();
        let reciprocal = ctx.eval_source("(/ 10)").expect("single arg division");
        assert_eq!(reciprocal, Value::Float(0.1));

        let multi = ctx.eval_source("(/ 40 5 2)").expect("multi arg division");
        assert_eq!(multi, Value::Int(4));

        let err = ctx.eval_source("(/ 1 0)").expect_err("division by zero");
        assert!(
            err.to_string().contains("division by zero"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn count_covers_nil_strings_and_maps() {
        let mut ctx = runtime_ctx();
        let nil_count = ctx.eval_source("(count nil)").expect("count nil");
        assert_eq!(nil_count, Value::Int(0));

        let string_count = ctx.eval_source("(count \"hello!\")").expect("count string");
        assert_eq!(string_count, Value::Int(6));

        let map_count = ctx
            .eval_source("(count {:a 1 :b 2 :c 3})")
            .expect("count map");
        assert_eq!(map_count, Value::Int(3));
    }

    #[test]
    fn take_drop_assoc_and_get_cover_common_examples() {
        let mut ctx = runtime_ctx();

        let take_list = ctx
            .eval_source("(vec (take 2 '(9 8 7 6)))")
            .expect("take from list");
        let expected_take_list = ctx.eval_source("[9 8]").expect("expected take list");
        assert_eq!(take_list, expected_take_list);

        let take_shorter = ctx
            .eval_source("(vec (take 5 [1 2 3]))")
            .expect("take beyond length");
        let expected_take_shorter = ctx.eval_source("[1 2 3]").expect("expected short");
        assert_eq!(take_shorter, expected_take_shorter);

        let drop_negative = ctx
            .eval_source("(vec (drop -2 [4 5 6]))")
            .expect("drop negative treated as zero");
        let expected_drop_negative = ctx.eval_source("[4 5 6]").expect("expected drop neg");
        assert_eq!(drop_negative, expected_drop_negative);

        let drop_lazy = ctx
            .eval_source("(vec (take 3 (drop 4 (range))))")
            .expect("drop should work on seq");
        let expected_drop_lazy = ctx.eval_source("[4 5 6]").expect("expected drop lazy");
        assert_eq!(drop_lazy, expected_drop_lazy);

        let assoc_update = ctx
            .eval_source("(assoc {:k 1} :k 10 :z 0)")
            .expect("assoc should update and add keys");
        let expected_assoc_update = ctx.eval_source("{:k 10 :z 0}").expect("expected assoc map");
        assert_eq!(assoc_update, expected_assoc_update);

        let assoc_nil = ctx
            .eval_source("(assoc nil :flag true)")
            .expect("assoc nil should create map");
        let expected_assoc_nil = ctx
            .eval_source("{:flag true}")
            .expect("expected assoc nil map");
        assert_eq!(assoc_nil, expected_assoc_nil);

        let dissoc_multi = ctx
            .eval_source("(dissoc {:a 1 :b 2 :c 3} :b :x)")
            .expect("dissoc should remove matching keys only");
        let expected_dissoc_multi = ctx.eval_source("{:a 1 :c 3}").expect("expected dissoc map");
        assert_eq!(dissoc_multi, expected_dissoc_multi);

        let get_default = ctx
            .eval_source("(get #{:a :b} :c :fallback)")
            .expect("get with default from set");
        let expected_default = ctx
            .eval_source(":fallback")
            .expect("expected default keyword");
        assert_eq!(get_default, expected_default);

        let get_vector_index = ctx
            .eval_source("(get [\"aa\" \"bb\"] 1 \"none\")")
            .expect("get vector by index");
        let expected_vector_index = ctx.eval_source("\"bb\"").expect("expected vector item");
        assert_eq!(get_vector_index, expected_vector_index);

        let empty_nil = ctx.eval_source("(empty? nil)").expect("empty? nil");
        assert_eq!(empty_nil, Value::Bool(true));

        let empty_vector = ctx.eval_source("(empty? [42])").expect("empty? vector");
        assert_eq!(empty_vector, Value::Bool(false));
    }

    #[test]
    fn arithmetic_and_comparisons_follow_examples() {
        let mut ctx = runtime_ctx();

        let zero_sum = ctx.eval_source("(+)").expect("zero-arity +");
        assert_eq!(zero_sum, Value::Int(0));

        let mixed_sum = ctx
            .eval_source("(+ 5 -2 3.5)")
            .expect("mixed addition should produce float");
        assert_eq!(mixed_sum, Value::Float(6.5));

        let negation = ctx.eval_source("(- 7)").expect("unary minus");
        assert_eq!(negation, Value::Int(-7));

        let chained_sub = ctx.eval_source("(- 20 5 3)").expect("multi subtraction");
        assert_eq!(chained_sub, Value::Int(12));

        let product = ctx.eval_source("(* 2 3 0)").expect("product with zero");
        assert_eq!(product, Value::Int(0));

        let modulo = ctx
            .eval_source("(mod -9 5)")
            .expect("mod handles negative dividend");
        assert_eq!(modulo, Value::Int(1));

        let mod_err = ctx
            .eval_source("(mod 1 0)")
            .expect_err("mod by zero should fail");
        assert!(
            mod_err.to_string().contains("mod by zero"),
            "unexpected mod error: {}",
            mod_err
        );

        let greater_chain = ctx
            .eval_source("(> 9 5 5)")
            .expect("greater? should stop on equal");
        assert_eq!(greater_chain, Value::Bool(false));

        let gte_chain = ctx.eval_source("(>= 7 7 3)").expect(">= allows equals");
        assert_eq!(gte_chain, Value::Bool(true));

        let equality = ctx
            .eval_source("(= {:a 1 :b 2} {:b 2 :a 1})")
            .expect("map equality should ignore order");
        assert_eq!(equality, Value::Bool(true));

        let inequality = ctx
            .eval_source("(not= 1 1 2)")
            .expect("not= detects difference");
        assert_eq!(inequality, Value::Bool(true));

        let zero_pred = ctx.eval_source("(zero? 0.0)").expect("zero? float");
        assert_eq!(zero_pred, Value::Bool(true));

        let dec_float = ctx.eval_source("(dec 1.5)").expect("dec float");
        assert_eq!(dec_float, Value::Float(0.5));
    }

    #[test]
    fn seq_basics_follow_examples() {
        let mut ctx = runtime_ctx();

        let first_val = ctx.eval_source("(first [10 20 30])").expect("first vector");
        assert_eq!(first_val, Value::Int(10));

        let rest_list = ctx
            .eval_source("(rest '(9 8 7))")
            .expect("rest list should drop first");
        let expected_rest = ctx.eval_source("(list 8 7)").expect("expected rest");
        assert_eq!(rest_list, expected_rest);

        let next_single = ctx.eval_source("(next [5])").expect("next single element");
        assert_eq!(next_single, Value::Nil);

        let last_vec = ctx.eval_source("(last [1 2 3 4])").expect("last vector");
        assert_eq!(last_vec, Value::Int(4));

        let cons_list = ctx.eval_source("(cons :head '(1 2))").expect("cons list");
        let expected_cons = ctx.eval_source("(list :head 1 2)").expect("expected cons");
        assert_eq!(cons_list, expected_cons);

        let conj_vector = ctx
            .eval_source("(conj [1 2] 3 4)")
            .expect("conj vector appends");
        let expected_conj_vector = ctx.eval_source("[1 2 3 4]").expect("expected vector");
        assert_eq!(conj_vector, expected_conj_vector);

        let conj_list = ctx
            .eval_source("(conj '(1 2) 3)")
            .expect("conj list prepends");
        let expected_conj_list = ctx.eval_source("(list 3 1 2)").expect("expected list");
        assert_eq!(conj_list, expected_conj_list);

        let conj_map_pair = ctx
            .eval_source("(conj {:a 1} [:b 2])")
            .expect("conj map with kv vector");
        let expected_conj_map = ctx.eval_source("{:a 1 :b 2}").expect("expected map");
        assert_eq!(conj_map_pair, expected_conj_map);

        let seq_nil = ctx.eval_source("(seq [])").expect("seq empty vector");
        assert_eq!(seq_nil, Value::Nil);

        let seq_vector = ctx
            .eval_source("(vec (seq [4 5]))")
            .expect("seq vector to seq then vec");
        let expected_seq_vector = ctx.eval_source("[4 5]").expect("expected seq");
        assert_eq!(seq_vector, expected_seq_vector);

        let vec_string = ctx
            .eval_source("(vec \"ab\")")
            .expect("vec on string splits chars");
        let expected_vec_string = ctx.eval_source("[\"a\" \"b\"]").expect("expected chars");
        assert_eq!(vec_string, expected_vec_string);

        let nth_default = ctx.eval_source("(nth [9 8] 5 :none)").expect("nth default");
        let expected_nth_default = ctx.eval_source(":none").expect("expected default");
        assert_eq!(nth_default, expected_nth_default);

        let get_string = ctx
            .eval_source("(get \"xyz\" 1 \"-\")")
            .expect("get string by index");
        let expected_get_string = ctx.eval_source("\"y\"").expect("expected char");
        assert_eq!(get_string, expected_get_string);

        let keys_all = ctx
            .eval_source("(let [ks (reduce conj #{} (keys {:p 10 :q 20}))] (and (= 2 (count ks)) (contains? ks :p) (contains? ks :q)))")
            .expect("keys membership check");
        assert_eq!(keys_all, Value::Bool(true));

        let vals_set = ctx
            .eval_source("(reduce conj #{} (vals {:p 10 :q 20}))")
            .expect("vals as set");
        let expected_vals_set = ctx.eval_source("#{10 20}").expect("expected vals set");
        assert_eq!(vals_set, expected_vals_set);

        let vector_literal = ctx.eval_source("(vector 7 nil)").expect("vector literal");
        let expected_vector_literal = ctx.eval_source("[7 nil]").expect("expected vector");
        assert_eq!(vector_literal, expected_vector_literal);

        let list_literal = ctx.eval_source("(list :a :b)").expect("list literal");
        let expected_list_literal = ctx.eval_source("(list :a :b)").expect("expected list");
        assert_eq!(list_literal, expected_list_literal);

        let hash_map_build = ctx
            .eval_source("(hash-map :x 1 :x 2 :y 3)")
            .expect("hash-map overwrites duplicate");
        let expected_hash_map = ctx.eval_source("{:x 2 :y 3}").expect("expected map");
        assert_eq!(hash_map_build, expected_hash_map);

        let hash_set_build = ctx
            .eval_source("(hash-set 1 2 1)")
            .expect("hash-set unique");
        let expected_hash_set = ctx.eval_source("#{1 2}").expect("expected set");
        assert_eq!(hash_set_build, expected_hash_set);

        let take_lazy = ctx
            .eval_source("(vec (take 3 (range 5 10)))")
            .expect("take from finite");
        let expected_take_lazy = ctx.eval_source("[5 6 7]").expect("expected take");
        assert_eq!(take_lazy, expected_take_lazy);

        let drop_all = ctx
            .eval_source("(vec (drop 5 [1 2 3]))")
            .expect("drop beyond length");
        let expected_drop_all = ctx.eval_source("[]").expect("expected empty");
        assert_eq!(drop_all, expected_drop_all);
    }

    #[test]
    fn transient_api_basics_follow_spec() {
        let mut ctx = runtime_ctx();

        let vec_roundtrip = ctx
            .eval_source(
                "(let [t (transient [1 2]) t2 (conj! t 3 4) t3 (assoc! t2 0 9) p (persistent! t3)] p)",
            )
            .expect("transient vector roundtrip");
        let expected_vec = ctx.eval_source("[9 2 3 4]").expect("expected vector");
        assert_eq!(vec_roundtrip, expected_vec);

        let map_roundtrip = ctx
            .eval_source(
                "(let [t (transient {:a 1}) t2 (assoc! t :b 2 :a 9) t3 (dissoc! t2 :b) p (persistent! t3)] p)",
            )
            .expect("transient map roundtrip");
        let expected_map = ctx.eval_source("{:a 9}").expect("expected map");
        assert_eq!(map_roundtrip, expected_map);

        let set_roundtrip = ctx
            .eval_source(
                "(let [t (transient #{1 2}) t2 (conj! t 3) t3 (disj! t2 1) p (persistent! t3)] p)",
            )
            .expect("transient set roundtrip");
        let expected_set = ctx.eval_source("#{2 3}").expect("expected set");
        assert_eq!(set_roundtrip, expected_set);

        let read_ops = ctx
            .eval_source("(let [t (transient [4 5 6])] [(nth t 1) (count t) (get t 2)])")
            .expect("read ops on transient");
        let expected_read = ctx.eval_source("[5 3 6]").expect("expected read result");
        assert_eq!(read_ops, expected_read);

        let pop_res = ctx
            .eval_source("(let [t (transient [1 2]) t2 (pop! t) p (persistent! t2)] p)")
            .expect("pop! removes last element");
        let expected_pop = ctx.eval_source("[1]").expect("expected pop result");
        assert_eq!(pop_res, expected_pop);

        let err = ctx
            .eval_source("(conj (transient [1]) 2)")
            .expect_err("conj on transient should fail");
        assert!(
            err.to_string().contains("conj does not accept transient"),
            "unexpected error: {}",
            err
        );

        let err = ctx
            .eval_source("(let [t (transient [1]) t2 (conj! t 2)] (count t))")
            .expect_err("stale transient should error");
        assert!(
            err.to_string().contains("stale transient"),
            "unexpected error: {}",
            err
        );

        let err = ctx
            .eval_source("(let [t (transient [1]) p (persistent! t)] (count t))")
            .expect_err("transient after persistent should error");
        assert!(
            err.to_string().contains("after persistent"),
            "unexpected error: {}",
            err
        );

        let err = ctx
            .eval_source("(seq (transient [1]))")
            .expect_err("seq on transient should error");
        assert!(
            err.to_string()
                .contains("seq does not accept transient collections"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn map_updates_preserve_immutability() {
        run_with_large_stack(|| {
            let mut ctx = runtime_ctx();
            let _ = ctx
                .eval_source("(require std :refer :all)")
                .expect("require std");

            let assoc_ok = ctx
                .eval_source("(let [m1 {:a 1} m2 (assoc m1 :a 9)] (= m1 {:a 1}))")
                .expect("assoc immutability");
            assert_eq!(assoc_ok, Value::Bool(true));

            let dissoc_ok = ctx
                .eval_source("(let [m1 {:a 1 :b 2} m2 (dissoc m1 :a)] (= m1 {:a 1 :b 2}))")
                .expect("dissoc immutability");
            assert_eq!(dissoc_ok, Value::Bool(true));

            let merge_ok = ctx
                .eval_source(
                    "(let [m1 {:a 1} m2 {:a 2} m3 (merge m1 m2)] (and (= m1 {:a 1}) (= m2 {:a 2})))",
                )
                .expect("merge immutability");
            assert_eq!(merge_ok, Value::Bool(true));

            let update_in_ok = ctx
                .eval_source(
                    "(let [m1 {:a {:b 1}} m2 (update-in m1 [:a :b] inc)] (= m1 {:a {:b 1}}))",
                )
                .expect("update-in immutability");
            assert_eq!(update_in_ok, Value::Bool(true));
        });
    }

    #[test]
    fn transient_map_does_not_mutate_persistent() {
        run_with_large_stack(|| {
            let mut ctx = runtime_ctx();

            let ok = ctx
                .eval_source(
                    "(let [m1 {:a 1} t (transient m1) t2 (assoc! t :b 2) p (persistent! t2)] (and (= m1 {:a 1}) (= p {:a 1 :b 2})))",
                )
                .expect("transient map immutability");
            assert_eq!(ok, Value::Bool(true));
        });
    }

    #[test]
    fn map_and_set_operations_follow_examples() {
        run_with_large_stack(|| {
            let mut ctx = runtime_ctx();
            let _ = ctx
                .eval_source("(require std :refer :all)")
                .expect("require std");

            let merged = ctx
                .eval_source("(merge {:a 1 :b 2} {:b 9 :c 3})")
                .expect("merge maps overrides later");
            let expected_merged = ctx
                .eval_source("{:a 1 :b 9 :c 3}")
                .expect("expected merged map");
            assert_eq!(merged, expected_merged);

            let merge_with_nil = ctx
                .eval_source("(merge nil {:x 1})")
                .expect("merge nil and map");
            let expected_merge_nil = ctx.eval_source("{:x 1}").expect("expected map");
            assert_eq!(merge_with_nil, expected_merge_nil);

            let get_in_hit = ctx
                .eval_source("(get-in {:profile {:name \"S\" :city \"A\"}} [:profile :city])")
                .expect("get-in nested");
            assert_eq!(get_in_hit, Value::String("A".into()));

            let get_in_default = ctx
                .eval_source("(get-in {:profile {}} [:profile :zip] \"missing\")")
                .expect("get-in default");
            assert_eq!(get_in_default, Value::String("missing".into()));

            let get_in_vector = ctx
                .eval_source("(get-in [[0 1] [2 3]] [1 0])")
                .expect("get-in vector index");
            assert_eq!(get_in_vector, Value::Int(2));

            let assoc_in_extend = ctx
                .eval_source("(assoc-in {:a {:b 1}} [:a :c] 9)")
                .expect("assoc-in adds nested key");
            let expected_assoc_in = ctx
                .eval_source("{:a {:b 1 :c 9}}")
                .expect("expected assoc-in");
            assert_eq!(assoc_in_extend, expected_assoc_in);

            let assoc_in_new_path = ctx
                .eval_source("(assoc-in {} [:x :y] 5)")
                .expect("assoc-in creates maps");
            let expected_assoc_in_new =
                ctx.eval_source("{:x {:y 5}}").expect("expected new nested");
            assert_eq!(assoc_in_new_path, expected_assoc_in_new);

            let update_in_inc = ctx
                .eval_source("(update-in {:age 40} [:age] inc)")
                .expect("update-in inc");
            assert_eq!(update_in_inc, ctx.eval_source("{:age 41}").expect("age 41"));

            let update_in_add = ctx
                .eval_source("(update-in {:age 40} [:age] + 5)")
                .expect("update-in add");
            assert_eq!(update_in_add, ctx.eval_source("{:age 45}").expect("age 45"));

            let update_nil = ctx
                .eval_source("(update {:count 1} :count (fnil + 0) 4)")
                .expect("update with fnil");
            assert_eq!(update_nil, ctx.eval_source("{:count 5}").expect("count 5"));

            let dissoc_multi = ctx
                .eval_source("(dissoc {:a 1 :b 2 :c 3} :b :d)")
                .expect("dissoc missing key ignored");
            let expected_dissoc_multi = ctx
                .eval_source("{:a 1 :c 3}")
                .expect("expected dissoc result");
            assert_eq!(dissoc_multi, expected_dissoc_multi);
        });
    }

    #[test]
    fn string_utilities_follow_examples() {
        let mut ctx = runtime_ctx();

        let str_concat = ctx
            .eval_source("(str \"hi\" 123)")
            .expect("str concatenates");
        assert_eq!(str_concat, Value::String("hi123".into()));

        let pr_str_roundtrip = ctx
            .eval_source("(let [x {:a 1 :b [2 3]}] (= x (read-string (pr-str x))))")
            .expect("pr-str should be readable");
        assert_eq!(pr_str_roundtrip, Value::Bool(true));

        let substr = ctx
            .eval_source("(subs \"abcdef\" 2 5)")
            .expect("subs slice");
        assert_eq!(substr, Value::String("cde".into()));

        let replace_simple = ctx
            .eval_source("(string::replace \"a-b-c\" \"-\" \":\")")
            .expect("string replace");
        assert_eq!(replace_simple, Value::String("a:b:c".into()));

        let replace_first = ctx
            .eval_source("(string::replace-first \"one two three\" \" \" \";\")")
            .expect("replace-first string");
        assert_eq!(replace_first, Value::String("one;two three".into()));

        let split_regex = ctx
            .eval_source("(string::split \"a1b22c\" #/\\d+/)")
            .expect("split by regex digits");
        assert_eq!(
            split_regex,
            ctx.eval_source("[\"a\" \"b\" \"c\"]")
                .expect("expected split")
        );

        let join_with_sep = ctx
            .eval_source("(string::join [\"x\" \"y\" \"z\"] \",\")")
            .expect("join with separator");
        assert_eq!(join_with_sep, Value::String("x,y,z".into()));

        let trim_both = ctx
            .eval_source("(string::trim \"  hello \\n\")")
            .expect("trim whitespace");
        assert_eq!(trim_both, Value::String("hello".into()));

        let upper = ctx
            .eval_source("(string::upper-case \"Abc\")")
            .expect("upper-case");
        assert_eq!(upper, Value::String("ABC".into()));

        let lower = ctx
            .eval_source("(string::lower-case \"AbC\")")
            .expect("lower-case");
        assert_eq!(lower, Value::String("abc".into()));

        let includes_pred = ctx
            .eval_source("(string::includes? \"clove\" \"lov\")")
            .expect("includes?");
        assert_eq!(includes_pred, Value::Bool(true));

        let starts_pred = ctx
            .eval_source("(string::starts-with? \"clove\" \"cl\")")
            .expect("starts-with?");
        assert_eq!(starts_pred, Value::Bool(true));

        let ends_pred = ctx
            .eval_source("(string::ends-with? \"clove\" \"ve\")")
            .expect("ends-with?");
        assert_eq!(ends_pred, Value::Bool(true));
    }

    #[test]
    fn predicate_utilities_follow_examples() {
        let mut ctx = runtime_ctx();
        let _ = ctx
            .eval_source("(require std :refer :all)")
            .expect("require std");

        let nil_pred = ctx.eval_source("(nil? nil)").expect("nil?");
        assert_eq!(nil_pred, Value::Bool(true));
        let some_pred = ctx.eval_source("(some? 0)").expect("some?");
        assert_eq!(some_pred, Value::Bool(true));

        let true_pred = ctx.eval_source("(true? true)").expect("true?");
        assert_eq!(true_pred, Value::Bool(true));
        let false_pred = ctx.eval_source("(false? false)").expect("false?");
        assert_eq!(false_pred, Value::Bool(true));

        let num_pred = ctx.eval_source("(number? 3.5)").expect("number?");
        assert_eq!(num_pred, Value::Bool(true));
        let int_pred = ctx.eval_source("(integer? 4.0)").expect("integer?");
        assert_eq!(int_pred, Value::Bool(true));

        let string_pred = ctx.eval_source("(string? \"s\")").expect("string?");
        assert_eq!(string_pred, Value::Bool(true));

        let map_pred = ctx.eval_source("(map? {:a 1})").expect("map?");
        assert_eq!(map_pred, Value::Bool(true));
        let vector_pred = ctx.eval_source("(vector? [1 2])").expect("vector?");
        assert_eq!(vector_pred, Value::Bool(true));
        let list_pred = ctx.eval_source("(list? '(1 2))").expect("list?");
        assert_eq!(list_pred, Value::Bool(true));
        let set_pred = ctx.eval_source("(set? #{1 2})").expect("set?");
        assert_eq!(set_pred, Value::Bool(true));

        let sequential_pred = ctx
            .eval_source("(sequential? (rest [1 2 3]))")
            .expect("sequential?");
        assert_eq!(sequential_pred, Value::Bool(true));

        let fn_pred = ctx.eval_source("(fn? (fn [x] (+ x 1)))").expect("fn?");
        assert_eq!(fn_pred, Value::Bool(true));

        let keyword_pred = ctx.eval_source("(keyword? :k)").expect("keyword?");
        assert_eq!(keyword_pred, Value::Bool(true));

        let symbol_pred = ctx.eval_source("(symbol? 'foo)").expect("symbol?");
        assert_eq!(symbol_pred, Value::Bool(true));

        let some_fn = ctx
            .eval_source("(some odd? [2 4 5 6])")
            .expect("some returns first truthy result");
        assert_eq!(some_fn, Value::Bool(true));

        let every_pred = ctx
            .eval_source("(every? number? [1 2.0 3])")
            .expect("every?");
        assert_eq!(every_pred, Value::Bool(true));

        let not_any_pred = ctx
            .eval_source("(not-any? zero? [1 2 3])")
            .expect("not-any?");
        assert_eq!(not_any_pred, Value::Bool(true));
    }
}

fn find_indexer_span(form: &Form) -> Option<Span> {
    match &form.kind {
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(sym)) = items.first().map(|item| &item.kind) {
                if sym == INDEX_GET_SYM || sym == INDEX_GET_IN_SYM {
                    return Some(form.span);
                }
            }
            items.iter().find_map(find_indexer_span)
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            items.iter().find_map(find_indexer_span)
        }
        FormKind::InterpolatedString(parts) => parts.iter().find_map(|part| match part {
            InterpolatedPart::Text(_) => None,
            InterpolatedPart::Expr(expr) => find_indexer_span(expr),
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().find_map(|part| match part {
            InterpolatedPart::Text(_) => None,
            InterpolatedPart::Expr(expr) => find_indexer_span(expr),
        }),
        FormKind::Map(entries) => entries.iter().find_map(|entry| match entry {
            MapItem::KeyValue(k, v) => find_indexer_span(k).or_else(|| find_indexer_span(v)),
            MapItem::Spread(expr) => find_indexer_span(expr),
        }),
        _ => None,
    }
}

fn expand_indexer_form(form: &Form) -> Result<Form, CloveError> {
    match &form.kind {
        FormKind::List(items) => {
            if items.is_empty() {
                return Ok(form.clone());
            }
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(expand_indexer_form(item)?);
            }
            let head_sym = match &items[0].kind {
                FormKind::Symbol(sym) => sym.as_str(),
                _ => "",
            };
            let convert = |target: &str, lowered: Vec<Form>| -> Result<Form, CloveError> {
                if lowered.len() < 3 || lowered.len() > 4 {
                    return Err(span_error(
                        form.span,
                        "malformed indexer form generated by reader",
                    ));
                }
                let mut new_items = Vec::with_capacity(lowered.len());
                new_items.push(Form::new(FormKind::Symbol(target.into()), lowered[0].span));
                new_items.extend(lowered.into_iter().skip(1));
                Ok(Form::new(FormKind::List(new_items), form.span))
            };
            match head_sym {
                INDEX_GET_SYM => convert("get", lowered),
                INDEX_GET_IN_SYM => convert("get-in", lowered),
                _ => Ok(Form::new(FormKind::List(lowered), form.span)),
            }
        }
        FormKind::Vector(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(expand_indexer_form(item)?);
            }
            Ok(Form::new(FormKind::Vector(lowered), form.span))
        }
        FormKind::Set(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(expand_indexer_form(item)?);
            }
            Ok(Form::new(FormKind::Set(lowered), form.span))
        }
        FormKind::ShortFn(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(expand_indexer_form(item)?);
            }
            Ok(Form::new(FormKind::ShortFn(lowered), form.span))
        }
        FormKind::InterpolatedString(parts) => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                let next = match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => {
                        InterpolatedPart::Expr(expand_indexer_form(expr)?)
                    }
                };
                lowered.push(next);
            }
            Ok(Form::new(FormKind::InterpolatedString(lowered), form.span))
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                let next = match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => {
                        InterpolatedPart::Expr(expand_indexer_form(expr)?)
                    }
                };
                lowered.push(next);
            }
            Ok(Form::new(
                FormKind::InterpolatedRegex {
                    parts: lowered,
                    delim: *delim,
                },
                form.span,
            ))
        }
        FormKind::Map(entries) => {
            let mut lowered_entries = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        lowered_entries.push(MapItem::KeyValue(
                            expand_indexer_form(k)?,
                            expand_indexer_form(v)?,
                        ));
                    }
                    MapItem::Spread(expr) => {
                        lowered_entries.push(MapItem::Spread(expand_indexer_form(expr)?));
                    }
                }
            }
            Ok(Form::new(FormKind::Map(lowered_entries), form.span))
        }
        _ => Ok(form.clone()),
    }
}

fn span_error(span: Span, msg: &str) -> CloveError {
    CloveError::runtime(msg.to_string()).with_span(span)
}

fn validate_ns_name(name: &str, span: Span) -> Result<(), CloveError> {
    if name.is_empty() {
        return Err(span_error(span, "namespace name cannot be empty"));
    }
    for segment in namespace_segments(name) {
        validate_ns_segment(segment, span)?;
    }
    Ok(())
}

fn validate_alias_name(name: &str, span: Span) -> Result<(), CloveError> {
    validate_ns_segment(name, span)
}

fn validate_symbol_name(name: &str, span: Span) -> Result<(), CloveError> {
    if name == "/" {
        return Ok(());
    }
    if name.is_empty() {
        return Err(span_error(span, "names cannot be empty"));
    }
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if is_valid_symbol_start_char(c) => {}
        _ => {
            return Err(span_error(
                span,
                "names must start with an ASCII letter or one of '_', '*', '+', '-', '!', '?', '<', '>', '='",
            ))
        }
    }
    for ch in chars {
        if !is_valid_symbol_char(ch) {
            return Err(span_error(
                span,
                "names may only contain ASCII letters, digits, or '_', '-', '!', '?', '*', '+', '<', '>', '='",
            ));
        }
    }
    Ok(())
}

fn validate_ns_segment(segment: &str, span: Span) -> Result<(), CloveError> {
    if segment.is_empty() {
        return Err(span_error(span, "names cannot be empty"));
    }
    let mut chars = segment.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => {
            return Err(span_error(
                span,
                "names must start with an ASCII letter or '_'",
            ))
        }
    }
    for ch in chars {
        if !(ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-')) {
            return Err(span_error(
                span,
                "names may only contain ASCII letters, digits, '_', or '-'",
            ));
        }
    }
    Ok(())
}

fn is_valid_symbol_start_char(ch: char) -> bool {
    ch.is_ascii_alphabetic() || matches!(ch, '_' | '*' | '+' | '-' | '!' | '?' | '<' | '>' | '=')
}

fn is_valid_symbol_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | '!' | '?' | '*' | '+' | '<' | '>' | '=')
}

pub struct NamespacePlan {
    decl: Option<NsDeclaration>,
    body_forms: Vec<Form>,
    inline_requires: Vec<RequireSpec>,
    private_names: StdHashSet<String>,
}

pub struct NsDeclaration {
    pub name: String,
    pub require_specs: Vec<RequireSpec>,
    pub span: Span,
    pub doc: Option<String>,
    pub meta_form: Option<Form>,
    pub source: Option<String>,
}

#[derive(Clone, Debug)]
pub struct RequireSpec {
    pub target: RequireTarget,
    pub alias: Option<String>,
    pub refers: Vec<String>,
    pub rename: StdHashMap<String, String>,
    pub refer_all: bool,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum RequireTarget {
    Namespace(String),
    FilePath(String),
}
