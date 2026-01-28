use std::cell::RefCell;
use std::collections::HashMap as StdHashMap;
use std::collections::HashSet as StdHashSet;
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, RwLock, Weak};

use crate::apply_default_foreign_tags_for_source;
use crate::ast::{
    contains_mut_collection, desugar_interpolated_regex, desugar_interpolated_string, ComposeKind,
    FnArity, Form, FormKind, InterpolatedPart, Key, LambdaClause, LocalDefn, LocalDefnClause,
    MapItem, NativeFn, RegexValue, Span, TransientKind, Value,
};
use crate::ast::{HashMap, Vector};
use crate::builtins::core::{meta_lookup, meta_set};
use crate::builtins::{self, seq_items};
use crate::compiler::APPLY_SYM;
use crate::concurrency::{
    current_cancel_ctx, promise_like_from_value, spawn_task, with_cancel_ctx, CancelContext,
    DelayHandle,
};
use crate::dynamic_vars;
use crate::env::{new_ref, Env, EnvRef};
use crate::error::{CloveError, ErrorContext, StackFrame, WARN_TAG};
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::foreign::{ForeignEngine, ForeignValue};
use crate::form_source;
use crate::form_to_string::form_to_string;
use crate::guard;
use crate::ir::{self, ParamType};
use crate::namespaces::NamespaceStore;
use crate::profiler;
use crate::reader::{
    Reader, ReaderOptions, DOT_CHAIN_PLACEHOLDER_PREFIX, INDEX_GET_IN_SYM, INDEX_GET_MANY_SYM,
    INDEX_GET_SYM, INFIX_SYNTAX_SYM, MAP_REF_SYM, MATCH_OR_SYM, OOP_AS_SYM, OOP_BARE_SYM,
    OOP_DOT_STAGE_SYM, OOP_INDEX_SYM, OOP_LET_SYM, OOP_METHOD_SYM, OOP_NIL_SAFE_SYM, OOP_SEG_SYM,
    OOP_SYNTAX_SYM,
};
use crate::repl;
use crate::runtime::{apply_regex, RuntimeCtx};
use crate::settings::{
    canonical_feature_toggle, FeatureToggle, RuntimeSettings, SyntaxFeatureId, MAIN_PACKAGE_ID,
    REPL_ON_ERROR_VAR,
};
use crate::short_fn::{placeholder_info_for_form, replace_placeholders, PlaceholderNames};
use crate::spread::strip_spread_symbol;
use crate::symbol_meta;
use crate::symbols::{builtin_alias_target, builtin_aliases, canonical_symbol_name};
use crate::try_form::{
    build_err_clause_handler, build_fin_clause_handler, format_try_error_message,
    parse_err_fin_tail, parse_try_short, validate_try_bindings, TryShortPlan,
};
use crate::type_registry::{
    self, AliasMeta, FieldMeta, FieldSchema, FnSigSpec, PrimitiveType, ProductMeta, RequiredMethod,
    SumMeta, TypeEntry, TypeKind,
};
use crate::type_syntax::{
    normalize_type_syntax_forms, parse_type_expr_from_forms, parse_type_from_form,
};
use crate::types::TypeKind as MetaTypeKind;
use crate::types::{TypeHint, TypeHintStyle};
use im::HashMap as ImHashMap;
use im::HashSet;
use once_cell::sync::Lazy;

pub const CURRENT_NS_KEY: &str = "__clove_current_ns";

thread_local! {
    static CURRENT_FILE: RefCell<Option<String>> = RefCell::new(None);
    static STACK: RefCell<Vec<StackFrame>> = RefCell::new(Vec::new());
    static PENDING_FRAME: RefCell<Option<(String, Option<Span>)>> = RefCell::new(None);
    static PENDING_CALL_FORM: RefCell<Option<String>> = RefCell::new(None);
    static RECUR_STACK: RefCell<Vec<RecurContext>> = RefCell::new(Vec::new());
    static MAP_REF_STACK: RefCell<Vec<MapRefFrame>> = RefCell::new(Vec::new());
}

static LOOP_COUNTER: AtomicUsize = AtomicUsize::new(0);
static PLACEHOLDER_ALLOC_COUNTER: AtomicUsize = AtomicUsize::new(0);
static VM_ARITY_STORE: Lazy<Mutex<StdHashMap<usize, VmArityEntry>>> =
    Lazy::new(|| Mutex::new(StdHashMap::new()));

#[derive(Clone, Copy)]
struct VmArityClause {
    param_count: usize,
    has_rest: bool,
}

struct VmArityEntry {
    func: Weak<NativeFn>,
    clauses: Vec<VmArityClause>,
}

#[derive(Clone)]
struct FnClauseSpec {
    params_form: Form,
    body: Vec<Form>,
}

impl FnClauseSpec {
    fn single(params_form: Form, body: Vec<Form>) -> Result<Self, CloveError> {
        if body.is_empty() {
            return Err(span_runtime_error(
                params_form.span,
                "fn arity expects at least one form in the body",
            ));
        }
        Ok(Self { params_form, body })
    }

    fn from_clause_form(form: &Form) -> Result<Self, CloveError> {
        match &form.kind {
            FormKind::List(items) if !items.is_empty() => {
                if !matches!(items[0].kind, FormKind::Vector(_)) {
                    return Err(span_runtime_error(
                        items[0].span,
                        "fn arity params must be a vector",
                    ));
                }
                let body = if items.len() > 1 {
                    items[1..].to_vec()
                } else {
                    return Err(span_runtime_error(
                        form.span,
                        "fn arity expects at least one form in the body",
                    ));
                };
                Ok(Self {
                    params_form: items[0].clone(),
                    body,
                })
            }
            _ => Err(span_runtime_error(
                form.span,
                "fn arity must be a list of [params] and body",
            )),
        }
    }
}

struct DefnSignature {
    doc: Option<String>,
    clauses: Vec<FnClauseSpec>,
    fn_args: Vec<Form>,
    is_multi: bool,
    meta_map: Option<Form>,
    subject_pos_meta: Option<SubjectPos>,
}

#[derive(Clone)]
struct ParsedParams {
    params: Vec<String>,
    rest: Option<String>,
    destructure_bindings: Vec<(Form, Form)>,
    subject_pos: Option<usize>,
}

pub(crate) struct StackGuard {
    active: bool,
}

impl Drop for StackGuard {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        STACK.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

pub(crate) fn push_stack_frame(name: impl Into<String>, span: Option<Span>) -> StackGuard {
    let file = current_file_name();
    let frame = StackFrame {
        function: name.into(),
        span,
        file,
    };
    STACK.with(|stack| {
        stack.borrow_mut().push(frame);
    });
    StackGuard { active: true }
}

pub(crate) fn push_stack_frame_with_file(
    name: impl Into<String>,
    span: Option<Span>,
    file: Option<String>,
) -> StackGuard {
    let frame = StackFrame {
        function: name.into(),
        span,
        file,
    };
    STACK.with(|stack| {
        stack.borrow_mut().push(frame);
    });
    StackGuard { active: true }
}

#[derive(Clone)]
struct RecurContext {
    id: usize,
    positional_count: usize,
    has_rest: bool,
    kind: RecurKind,
    name: Option<String>,
}

fn describe_recur_target(ctx: &RecurContext) -> String {
    match ctx.kind {
        RecurKind::Loop => ctx.name.clone().unwrap_or_else(|| "loop".into()),
        RecurKind::Function => ctx
            .name
            .clone()
            .map(|n| format!("function {}", n))
            .unwrap_or_else(|| "anonymous function".into()),
    }
}

#[derive(Clone, Copy)]
enum RecurKind {
    Loop,
    Function,
}

struct RecurGuard {
    active: bool,
}

impl Drop for RecurGuard {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        RECUR_STACK.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

fn push_recur_context(ctx: RecurContext) -> RecurGuard {
    RECUR_STACK.with(|stack| {
        stack.borrow_mut().push(ctx);
    });
    RecurGuard { active: true }
}

fn current_recur_context() -> Option<RecurContext> {
    RECUR_STACK.with(|stack| stack.borrow().last().cloned())
}

fn capture_stack() -> Vec<StackFrame> {
    STACK.with(|stack| stack.borrow().clone())
}

fn set_pending_frame(name: String, span: Option<Span>) {
    PENDING_FRAME.with(|pending| {
        *pending.borrow_mut() = Some((name, span));
    });
}

fn take_pending_frame() -> Option<(String, Option<Span>)> {
    PENDING_FRAME.with(|pending| pending.borrow_mut().take())
}

fn set_pending_call_form(form: String) {
    PENDING_CALL_FORM.with(|pending| {
        *pending.borrow_mut() = Some(form);
    });
}

fn take_pending_call_form() -> Option<String> {
    PENDING_CALL_FORM.with(|pending| pending.borrow_mut().take())
}

fn args_span(args: &[Form], form_span: Span) -> Span {
    args.first().map(|f| f.span).unwrap_or(form_span)
}

pub fn set_current_file(name: Option<String>) {
    CURRENT_FILE.with(|cell| {
        *cell.borrow_mut() = name;
    });
}

pub fn current_file_name() -> Option<String> {
    CURRENT_FILE.with(|cell| cell.borrow().clone())
}

pub struct Evaluator {
    global: EnvRef,
    auto_fallback: bool,
    engines: Vec<Arc<dyn ForeignEngine>>,
    call_wrappers: Arc<StdHashSet<String>>,
    settings: RuntimeSettings,
    namespace_store: Option<Arc<RwLock<NamespaceStore>>>,
    file_loader: Option<Arc<dyn Fn(&Path) -> Result<Value, CloveError> + Send + Sync>>,
    default_namespace: RwLock<Option<String>>,
}

impl Evaluator {
    pub fn new(base: Env) -> Self {
        Self::new_with_engines(base, &[])
    }

    pub fn new_with_engines(base: Env, engines: &[Arc<dyn ForeignEngine>]) -> Self {
        Self::new_with_options(base, engines, false)
    }

    pub fn new_with_options(
        base: Env,
        engines: &[Arc<dyn ForeignEngine>],
        auto_fallback: bool,
    ) -> Self {
        let mut env = base;
        let builtins_env = builtins::default_env();
        for (k, v) in builtins_env.read().unwrap().clone_data() {
            env.set(&k, v);
        }
        Self {
            global: new_ref(env),
            auto_fallback,
            engines: engines.to_vec(),
            call_wrappers: Arc::new(StdHashSet::new()),
            settings: RuntimeSettings::new(),
            namespace_store: None,
            file_loader: None,
            default_namespace: RwLock::new(None),
        }
    }

    pub fn new_with_env_ref(
        env: EnvRef,
        engines: &[Arc<dyn ForeignEngine>],
        auto_fallback: bool,
    ) -> Self {
        Self {
            global: env,
            auto_fallback,
            engines: engines.to_vec(),
            call_wrappers: Arc::new(StdHashSet::new()),
            settings: RuntimeSettings::new(),
            namespace_store: None,
            file_loader: None,
            default_namespace: RwLock::new(None),
        }
    }

    pub fn set_call_wrappers_from_iter(&mut self, wrappers: impl IntoIterator<Item = String>) {
        self.call_wrappers = Arc::new(wrappers.into_iter().collect());
    }

    pub fn set_call_wrappers_arc(&mut self, wrappers: Arc<StdHashSet<String>>) {
        self.call_wrappers = wrappers;
    }

    pub fn call_wrappers(&self) -> Arc<StdHashSet<String>> {
        self.call_wrappers.clone()
    }

    pub fn settings(&self) -> RuntimeSettings {
        self.settings.clone()
    }

    pub fn set_namespace_store(&mut self, store: Arc<RwLock<NamespaceStore>>) {
        self.namespace_store = Some(store);
    }

    pub fn set_file_loader(
        &mut self,
        loader: Option<Arc<dyn Fn(&Path) -> Result<Value, CloveError> + Send + Sync>>,
    ) {
        self.file_loader = loader;
    }

    pub fn set_default_namespace(&self, name: Option<String>) {
        let mut guard = self.default_namespace.write().unwrap();
        *guard = name;
    }

    pub fn set_settings(&mut self, settings: RuntimeSettings) {
        self.settings = settings;
    }

    pub fn global_env(&self) -> EnvRef {
        self.global.clone()
    }

    pub fn engines(&self) -> &[Arc<dyn ForeignEngine>] {
        &self.engines
    }

    pub fn engine_tags(&self) -> Vec<String> {
        self.engines.iter().map(|e| e.tag().to_string()).collect()
    }

    pub fn eval_forms(&mut self, forms: &[Form]) -> Result<Value, CloveError> {
        let mut last = Value::Nil;
        for form in forms {
            last = self.eval(form, self.global.clone())?;
        }
        Ok(last)
    }

    pub fn eval(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        guard::tick(Some(form.span))?;
        if let FormKind::ForeignRaw { tag, code } = &form.kind {
            return self.eval_foreign_raw(tag.as_deref(), code, env, Some(form.span));
        }
        let result = match self.eval_inner(form, env.clone()) {
            Ok(v) => Ok(v),
            Err(e) if self.auto_fallback && !matches!(e, CloveError::Thrown(_, _)) => {
                self.eval_with_fallback(form, env.clone(), e)
            }
            Err(e) => Err(e),
        };
        result.map_err(|err| self.decorate_error(err, form.span, env))
    }

    fn eval_inner(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        if let Some(lowered) = desugar_interpolated_string(form) {
            return self.eval(&lowered, env);
        }
        if let Some(lowered) = desugar_interpolated_regex(form) {
            return self.eval(&lowered, env);
        }
        if let Some(map_ref_value) = self.try_eval_map_ref(form, env.clone())? {
            return Ok(map_ref_value);
        }
        match &form.kind {
            FormKind::Int(n) => Ok(Value::Int(*n)),
            FormKind::Float(n) => Ok(Value::Float(*n)),
            FormKind::String(s) => Ok(Value::String(s.clone())),
            FormKind::InterpolatedString(_) => unreachable!("handled before match"),
            FormKind::InterpolatedRegex { .. } => unreachable!("handled before match"),
            FormKind::Bool(b) => Ok(Value::Bool(*b)),
            FormKind::Nil => Ok(Value::Nil),
            FormKind::Duration(d) => Ok(Value::Duration(*d)),
            FormKind::Keyword(k) => Ok(Value::Symbol(format!(":{}", k))),
            FormKind::Symbol(name) => self.resolve_symbol_value(name, form.span, &env),
            FormKind::Vector(items) => {
                let mut out = Vector::new();
                let mut idx = 0;
                while idx < items.len() {
                    if let Some((spread_value, span)) =
                        self.try_eval_spread_value(items, &mut idx, env.clone())?
                    {
                        spread_into_vector(spread_value, &mut out, span)?;
                    } else {
                        out.push_back(self.eval(&items[idx], env.clone())?);
                        idx += 1;
                    }
                }
                Ok(Value::Vector(out))
            }
            FormKind::Map(entries) => {
                if form_contains_map_ref(form) {
                    if !self.map_refs_enabled(&env) {
                        return Err(span_runtime_error(
                            form.span,
                            "map-refs syntax is disabled; enable it via (use map-refs true)",
                        ));
                    }
                    return self.eval_map_with_refs(form, env);
                }
                let mut map = HashMap::new();
                for entry in entries {
                    match entry {
                        MapItem::KeyValue(k, v) => {
                            let key = to_key(k)?;
                            let val = self.eval(v, env.clone())?;
                            map.insert(key, val);
                        }
                        MapItem::Spread(expr) => {
                            let value = self.eval(expr, env.clone())?;
                            spread_into_map(value, &mut map, expr.span)?;
                        }
                    }
                }
                Ok(Value::Map(map))
            }
            FormKind::Set(items) => {
                let mut set = HashSet::new();
                let mut idx = 0;
                while idx < items.len() {
                    if let Some((spread_value, span)) =
                        self.try_eval_spread_value(items, &mut idx, env.clone())?
                    {
                        spread_into_set(spread_value, &mut set, span)?;
                    } else {
                        set.insert(self.eval(&items[idx], env.clone())?);
                        idx += 1;
                    }
                }
                Ok(Value::Set(set))
            }
            FormKind::Regex { pattern, .. } => self.eval_regex_literal(pattern),
            FormKind::ForeignBlock { tag, code } => {
                self.eval_foreign(tag, code, env, Some(form.span))
            }
            FormKind::ForeignRaw { tag, code } => {
                self.eval_foreign_raw(tag.as_deref(), code, env, Some(form.span))
            }
            FormKind::ForeignSymbol { tag, path } => {
                self.eval_foreign_symbol(tag.as_deref(), path, env.clone(), form.span)
            }
            FormKind::ShortFn(body) => self.eval_short_fn(body, form.span, env),
            FormKind::List(_) => self.eval_list(form, env),
        }
    }

    pub(crate) fn resolve_symbol_value(
        &self,
        name: &str,
        span: Span,
        env: &EnvRef,
    ) -> Result<Value, CloveError> {
        let canonical = canonical_symbol_name(name);
        let canonical_name = canonical.as_ref();
        if canonical_name == "__DATA__" {
            if let Some(result) =
                RuntimeCtx::try_with_current(|ctx| Ok(ctx.data_section_for_current_file()))
            {
                let data = result?;
                return Ok(data.map(Value::String).unwrap_or(Value::Nil));
            }
            return Ok(Value::Nil);
        }
        if let Some(v) = dynamic_vars::current_value(canonical_name) {
            return Ok(v);
        }
        let enum_variant = self.enum_variant_lookup(canonical_name, env);
        if let Some(value) = env.read().unwrap().get(canonical_name) {
            if enum_variant.is_some() {
                return Err(span_runtime_error(
                    span,
                    &format!(
                        "ambiguous qualified symbol: {} (namespace and enum both match)",
                        canonical_name
                    ),
                ));
            }
            return Ok(value);
        }
        if let Some(variant) = enum_variant {
            if let Some(value) = self.lookup_symbol_value(&variant.variant_fqn, env) {
                return Ok(value);
            }
            if let Some(value) = env.read().unwrap().get(&variant.variant_name) {
                return Ok(value);
            }
            return Ok(make_type_constructor(
                &variant.variant_fqn,
                variant.is_unit,
                HashMap::new(),
            ));
        }
        if canonical_name.ends_with('?') && canonical_name.contains("::") {
            let base = canonical_name.trim_end_matches('?');
            if !base.is_empty() {
                if let Some(variant) = self.enum_variant_lookup(base, env) {
                    let predicate_name = format!("{}?", variant.variant_fqn);
                    if let Some(value) = self.lookup_symbol_value(&predicate_name, env) {
                        return Ok(value);
                    }
                    return Ok(make_type_predicate(&variant.variant_fqn));
                }
            }
        }
        if parse_question_placeholder(canonical_name).is_some() {
            return Err(span_runtime_error(
                span,
                "? is a placeholder; use #(...) or (... ? ...)",
            ));
        }
        Err(CloveError::unbound_symbol(format_span(
            span,
            &format!("Unbound symbol: '{}'", canonical_name),
        )))
    }

    fn try_eval_map_ref(&self, form: &Form, env: EnvRef) -> Result<Option<Value>, CloveError> {
        let map_ref = match parse_map_ref_form(form)? {
            Some(map_ref) => map_ref,
            None => return Ok(None),
        };
        if !self.map_refs_enabled(&env) {
            return Err(span_runtime_error(
                form.span,
                "map-refs syntax is disabled; enable it via (use map-refs true)",
            ));
        }
        let frame = current_map_ref_frame().ok_or_else(|| {
            span_runtime_error(form.span, "map ref is only available inside map literals")
        })?;
        let value = MapRefRuntime::resolve_map_ref_value(
            frame.runtime,
            &map_ref,
            frame.this_id,
            self,
            env,
        )?;
        Ok(Some(value))
    }

    fn eval_map_with_refs(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        let root_runtime =
            current_map_ref_frame().map(|frame| MapRefRuntime::root_runtime(frame.runtime));
        let runtime = MapRefRuntime::from_form(form, root_runtime)?;
        let runtime = Rc::new(RefCell::new(runtime));
        MapRefRuntime::eval_root(runtime, self, env)
    }

    fn eval_with_fallback(
        &self,
        form: &Form,
        env: EnvRef,
        s_expr_err: CloveError,
    ) -> Result<Value, CloveError> {
        if self.engines.is_empty() {
            return Err(s_expr_err);
        }
        let mut errs = Vec::new();
        for eng in &self.engines {
            match eng.eval_fallback(form, env.clone()) {
                Ok(v) => return Ok(v),
                Err(err) => errs.push((eng.tag().to_string(), err)),
            }
        }
        if errs.is_empty() {
            return Err(s_expr_err);
        }
        let joined = errs
            .into_iter()
            .map(|(t, err)| format!("  {} error: {}", t, err))
            .collect::<Vec<_>>()
            .join("\n");
        Err(span_runtime_error(
            form.span,
            format!(
                "auto-fallback failed\n  s-expr error: {}\n{}",
                s_expr_err, joined
            ),
        ))
    }

    fn eval_list(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        let items = match &form.kind {
            FormKind::List(items) => items,
            _ => return Err(span_runtime_error(form.span, "expected list form")),
        };
        if items.is_empty() {
            return Ok(Value::List(Vector::new()));
        }
        if !self.dot_chain_enabled(&env) && form_contains_dot_chain(form) {
            return Err(span_runtime_error(
                form.span,
                "dot-chain syntax is disabled; enable it via (use dot-chain true)",
            ));
        }
        let frame_label = stack_label_for_list(items);
        let head = &items[0];

        if let Some(expanded) = self.try_expand_oop_apply_form(form, env.clone())? {
            return self.eval(&expanded, env);
        }

        if let Some(expanded) = self.try_expand_oop_head_form(form, env.clone())? {
            return self.eval(&expanded, env);
        }

        if let Some(expanded) = self.try_expand_oop_form(form, env.clone())? {
            return self.eval(&expanded, env);
        }

        if let Some(expanded) = self.try_expand_infix_form(form)? {
            return self.eval(&expanded, env);
        }

        if let Some(expanded) = try_expand_index_form(form)? {
            if !self.indexer_enabled(&env) {
                return Err(span_runtime_error(
                    form.span,
                    "indexer syntax is disabled; enable it via (use indexer true)",
                ));
            }
            return self.eval(&expanded, env);
        }

        let mut args = Vec::with_capacity(items.len().saturating_sub(1));
        let mut idx = 1;
        let mut call_wrapper_target = None;
        if let FormKind::Symbol(s) = &head.kind {
            let sym_ref = s.as_str();
            if sym_ref == OOP_INDEX_SYM {
                return self.eval_oop_index(&items[1..], env, form.span);
            }
            if sym_ref == OOP_SEG_SYM {
                return self.eval_oop_seg(&items[1..], env, form.span);
            }
            if sym_ref == OOP_METHOD_SYM {
                return self.eval_oop_method(&items[1..], env, form.span);
            }
            if canonical_symbol_name(sym_ref).as_ref() == "method" {
                return self.eval_method(&items[1..], env, form.span);
            }
            if self.call_wrappers.contains(sym_ref) {
                if items.len() >= 2 {
                    idx = 2;
                    call_wrapper_target = Some(&items[1]);
                }
            } else {
                if canonical_symbol_name(sym_ref).as_ref() == "go-loop" {
                    let _guard = push_stack_frame(frame_label.clone(), Some(form.span));
                    return self.eval_go_loop(items, env);
                }
                let _guard = push_stack_frame(frame_label.clone(), Some(form.span));
                match self.try_eval_special_form(sym_ref, items, env.clone(), form.span) {
                    Ok(Some(res)) => return Ok(res),
                    Ok(None) => {}
                    Err(err) => return Err(self.decorate_error(err, form.span, env.clone())),
                }
                match self.try_eval_foreign(sym_ref, items, env.clone()) {
                    Ok(Some(res)) => return Ok(res),
                    Ok(None) => {}
                    Err(err) => return Err(self.decorate_error(err, form.span, env.clone())),
                }
            }
        }

        let (placeholder_head, placeholder_args) = if let Some(target) = call_wrapper_target {
            (target, &items[idx..])
        } else {
            (head, &items[1..])
        };

        if let FormKind::Symbol(sym) = &placeholder_head.kind {
            if parse_question_placeholder(sym).is_some() {
                return Err(span_runtime_error(
                    placeholder_head.span,
                    "placeholder cannot be used as callee; use #(...) or (fn ...)",
                ));
            }
        }

        let args_have_placeholder = placeholder_args
            .iter()
            .any(|arg| form_contains_question_placeholder_scoped(arg, &StdHashSet::new()));

        if args_have_placeholder {
            if !matches!(placeholder_head.kind, FormKind::Symbol(_)) {
                if let Some((expanded, _)) = wrap_question_lambda(form) {
                    return self.eval(&expanded, env);
                }
            }
            let (fn_arg_positions, meta_known) =
                self.fn_arg_positions_for_head(placeholder_head, placeholder_args.len(), &env);
            if !meta_known {
                let needs_full = placeholder_args
                    .iter()
                    .any(|arg| self.form_contains_unresolved_placeholder(arg, &env));
                if needs_full {
                    if let Some((expanded, _)) = wrap_question_lambda(form) {
                        return self.eval(&expanded, env);
                    }
                }
            } else {
                let mut replaced_items = Vec::with_capacity(items.len());
                replaced_items.push(head.clone());
                if call_wrapper_target.is_some() {
                    replaced_items.push(placeholder_head.clone());
                }
                let mut any_wrapped = false;
                for (arg_idx, arg) in placeholder_args.iter().enumerate() {
                    let pos = arg_idx + 1;
                    if fn_arg_positions.contains(&pos) {
                        if let Some((wrapped, _)) = wrap_question_lambda(arg) {
                            replaced_items.push(wrapped);
                            any_wrapped = true;
                        } else {
                            replaced_items.push(arg.clone());
                        }
                    } else {
                        replaced_items.push(arg.clone());
                    }
                }

                let mut needs_full = false;
                for (arg_idx, arg) in placeholder_args.iter().enumerate() {
                    let pos = arg_idx + 1;
                    if !fn_arg_positions.contains(&pos)
                        && self.form_contains_unresolved_placeholder(arg, &env)
                    {
                        needs_full = true;
                        break;
                    }
                }

                if needs_full {
                    let call_form = Form::new(FormKind::List(replaced_items), form.span);
                    if let Some((expanded, _)) = wrap_question_lambda(&call_form) {
                        return self.eval(&expanded, env);
                    }
                } else if any_wrapped {
                    let call_form = Form::new(FormKind::List(replaced_items), form.span);
                    return self.eval(&call_form, env);
                }
            }
        }

        // Normal function application
        let head_val = self.eval(head, env.clone())?;
        let expanded_args;
        let arg_forms = if idx == 1 {
            if let Some(expanded) = self.expand_type_ctor_shorthand_args(head, &items[1..], &env) {
                expanded_args = expanded;
                expanded_args.as_slice()
            } else {
                &items[1..]
            }
        } else if let Some(target) = call_wrapper_target {
            if let Some(expanded) =
                self.expand_type_ctor_shorthand_args(target, &items[idx..], &env)
            {
                expanded_args = expanded;
                expanded_args.as_slice()
            } else {
                &items[idx..]
            }
        } else {
            &items[idx..]
        };
        if let Some(target) = call_wrapper_target {
            args.push(self.eval(target, env.clone())?);
        }
        let mut arg_idx = 0;
        while arg_idx < arg_forms.len() {
            if let Some((spread_value, span)) =
                self.try_eval_spread_value(arg_forms, &mut arg_idx, env.clone())?
            {
                spread_into_args(spread_value, &mut args, span)?;
            } else {
                args.push(self.eval(&arg_forms[arg_idx], env.clone())?);
                arg_idx += 1;
            }
        }
        if let Some(Ok(true)) = RuntimeCtx::try_with_current(|ctx| Ok(ctx.debug_stash_enabled())) {
            set_pending_call_form(form_to_string(form, ""));
        }
        set_pending_frame(frame_label, Some(form.span));
        call_callable(head_val, args).map_err(|err| match err {
            CloveError::TypeMismatch {
                expected, actual, ..
            } if expected == "function" => span_runtime_error(
                head.span,
                &format!(
                    "cannot call {} (type: {})",
                    form_to_string(head, ""),
                    actual
                ),
            ),
            CloveError::Other(data) if data.message == "cannot call foreign value" => {
                span_runtime_error(
                    head.span,
                    &format!("cannot call foreign value {}", form_to_string(head, "")),
                )
            }
            other => other,
        })
    }

    fn try_eval_spread_value(
        &self,
        forms: &[Form],
        idx: &mut usize,
        env: EnvRef,
    ) -> Result<Option<(Value, Span)>, CloveError> {
        if *idx >= forms.len() {
            return Ok(None);
        }
        if let FormKind::Symbol(sym) = &forms[*idx].kind {
            if sym.starts_with('*') && sym.len() > 1 && env.read().unwrap().get(sym).is_some() {
                return Ok(None);
            }
        }
        if let Some(symbol_form) = spread_symbol_form(&forms[*idx]) {
            let span = forms[*idx].span;
            let value = self.eval(&symbol_form, env)?;
            *idx += 1;
            return Ok(Some((value, span)));
        }
        if matches!(&forms[*idx].kind, FormKind::Symbol(sym) if sym == "*") {
            if *idx + 1 >= forms.len() {
                return Err(span_runtime_error(
                    forms[*idx].span,
                    "spread '*' requires an expression",
                ));
            }
            let span = forms[*idx].span;
            let value = self.eval(&forms[*idx + 1], env)?;
            *idx += 2;
            return Ok(Some((value, span)));
        }
        Ok(None)
    }

    fn expand_type_ctor_shorthand_args(
        &self,
        head: &Form,
        args: &[Form],
        env: &EnvRef,
    ) -> Option<Vec<Form>> {
        if args.is_empty() {
            return None;
        }
        if key_form_from_shorthand(&args[0]).is_none() {
            return None;
        }
        if !self.is_type_ctor_head(head, env) {
            return None;
        }
        let mut expanded = Vec::with_capacity(args.len());
        let mut idx = 0;
        let mut changed = false;
        while idx < args.len() {
            if let Some((name, key_form)) = key_form_from_shorthand(&args[idx]) {
                if !matches!(args[idx].kind, FormKind::Keyword(_)) {
                    changed = true;
                }
                let next = args.get(idx + 1);
                let has_explicit_value = next
                    .map(|form| key_form_from_shorthand(form).is_none())
                    .unwrap_or(false);
                expanded.push(key_form);
                if has_explicit_value {
                    expanded.push(next.unwrap().clone());
                    idx += 2;
                } else {
                    expanded.push(Form::new(FormKind::Symbol(name), args[idx].span));
                    changed = true;
                    idx += 1;
                }
                continue;
            }
            expanded.push(args[idx].clone());
            idx += 1;
        }
        if changed {
            Some(expanded)
        } else {
            None
        }
    }

    fn is_type_ctor_head(&self, head: &Form, env: &EnvRef) -> bool {
        let FormKind::Symbol(sym) = &head.kind else {
            return false;
        };
        let canonical = canonical_symbol_name(sym);
        let sym = canonical.as_ref();
        let current_ns =
            current_namespace_name(env).or_else(|| self.default_namespace.read().unwrap().clone());
        if enum_variant_lookup_with_ns(sym, current_ns.as_deref()).is_some() {
            return true;
        }
        let mut candidates = Vec::new();
        if sym.contains("::") {
            candidates.push(sym.to_string());
            if let Some(ns) = current_ns.as_deref() {
                candidates.push(format!("{ns}::{sym}"));
            }
        } else if let Some(ns) = current_ns.as_deref() {
            candidates.push(format!("{ns}::{sym}"));
        } else {
            candidates.push(sym.to_string());
        }
        if candidates
            .iter()
            .any(|name| type_name_is_product_like(name))
        {
            return true;
        }
        if sym.contains("::") {
            return false;
        }
        let suffix = format!("::{}", sym);
        let matches: Vec<_> = type_registry::list_all_types()
            .into_iter()
            .filter(|name| name.ends_with(&suffix))
            .filter(|name| type_name_is_product_like(name))
            .collect();
        if matches.len() != 1 {
            return false;
        }
        true
    }

    fn try_eval_special_form(
        &self,
        sym: &str,
        items: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Option<Value>, CloveError> {
        if sym == "go-loop" {
            return Ok(Some(self.eval_go_loop(items, env)?));
        }
        let head_name = canonical_symbol_name(sym);
        let rest = &items[1..];
        match head_name.as_ref() {
            "def" => Ok(Some(self.eval_def(rest, env, form_span)?)),
            "def-" => Ok(Some(self.eval_def_private(rest, env, form_span)?)),
            "defn" => Ok(Some(self.eval_defn(rest, env, form_span)?)),
            "defn-" => Ok(Some(self.eval_defn_private(rest, env, form_span)?)),
            "deftype" => Ok(Some(self.eval_deftype(rest, env, form_span)?)),
            "defenum" => Ok(Some(self.eval_defenum(rest, env, form_span)?)),
            "doc" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_doc(rest, env, form_span)?))
            }
            "meta" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_meta_form(rest, env, form_span)?))
            }
            "source" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_source_form(rest, env, form_span)?))
            }
            "describe" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_describe(rest, env, form_span)?))
            }
            "describe-type" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_describe(rest, env, form_span)?))
            }
            "infer-type" => Ok(Some(self.eval_infer_type(rest, env, form_span)?)),
            "enum-members" => {
                if !self.symbol_is_builtin(&head_name, &env) {
                    return Ok(None);
                }
                Ok(Some(self.eval_enum_members(rest, env, form_span)?))
            }
            OOP_INDEX_SYM => Ok(Some(self.eval_oop_index(rest, env, form_span)?)),
            OOP_SEG_SYM => Ok(Some(self.eval_oop_seg(rest, env, form_span)?)),
            OOP_METHOD_SYM => Ok(Some(self.eval_oop_method(rest, env, form_span)?)),
            "use-syntax" => Ok(Some(self.eval_use_syntax(rest, env, form_span)?)),
            "use" => Ok(Some(self.eval_use_syntax(rest, env, form_span)?)),
            "current-ns" => Ok(Some(self.eval_current_ns(env)?)),
            "let" => Ok(Some(self.eval_let(rest, env, form_span)?)),
            "if" => Ok(Some(self.eval_if(rest, env, form_span)?)),
            "do" => Ok(Some(self.eval_do(rest, env)?)),
            "where" => Ok(Some(self.eval_where(rest, env)?)),
            "fn" => Ok(Some(self.eval_fn(rest, env, form_span)?)),
            "method" => Ok(Some(self.eval_method(rest, env, form_span)?)),
            "quote" => Ok(Some(self.eval_quote(rest, form_span)?)),
            "set!" => Ok(Some(self.eval_set(rest, env, form_span)?)),
            "redef" => Ok(Some(self.eval_set(rest, env, form_span)?)),
            "__set-in-chain" => Ok(Some(self.eval_set_in_chain(rest, env, form_span)?)),
            "-def" => Ok(Some(self.eval_local_def(rest, env, form_span)?)),
            "-defn" => Ok(Some(self.eval_local_defn(rest, form_span)?)),
            "and" => Ok(Some(self.eval_and(rest, env)?)),
            "or" => Ok(Some(self.eval_or(rest, env)?)),
            "when" => Ok(Some(self.eval_when(rest, env, form_span)?)),
            "when-not" => Ok(Some(self.eval_when_not(rest, env, form_span)?)),
            "when-let" => Ok(Some(self.eval_when_let(rest, env, form_span)?)),
            "if-not" => Ok(Some(self.eval_if_not(rest, env, form_span)?)),
            "if-let" => Ok(Some(self.eval_if_let(rest, env, form_span)?)),
            "if-some" => Ok(Some(self.eval_if_some(rest, env, form_span)?)),
            "cond" => Ok(Some(self.eval_cond(rest, env, form_span)?)),
            "condp" => Ok(Some(self.eval_condp(rest, env, form_span)?)),
            "map" => {
                if let Some(value) = self.eval_map_sugar(rest, env.clone(), form_span)? {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "pmap" => {
                if let Some(value) =
                    self.eval_pmap_sugar(rest, env.clone(), form_span, "core::pmap")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "dag::pmap" => {
                if let Some(value) =
                    self.eval_pmap_sugar(rest, env.clone(), form_span, "dag::pmap")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "std::pmap" => {
                if let Some(value) =
                    self.eval_pmap_sugar(rest, env.clone(), form_span, "std::pmap")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "pfilter" => {
                if let Some(value) =
                    self.eval_pfilter_sugar(rest, env.clone(), form_span, "core::pfilter")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "dag::pfilter" => {
                if let Some(value) =
                    self.eval_pfilter_sugar(rest, env.clone(), form_span, "dag::pfilter")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "std::pfilter" => {
                if let Some(value) =
                    self.eval_pfilter_sugar(rest, env.clone(), form_span, "std::pfilter")?
                {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "filter" => {
                if let Some(value) = self.eval_filter_sugar(rest, env.clone(), form_span)? {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            "->" => Ok(Some(self.eval_thread_first(rest, env, form_span)?)),
            "->>" => Ok(Some(self.eval_thread_last(rest, env, form_span)?)),
            "as->" => Ok(Some(self.eval_as_thread(rest, env, form_span)?)),
            "cond->" => Ok(Some(self.eval_cond_thread(
                rest,
                env,
                ThreadStyle::First,
                form_span,
            )?)),
            "cond->>" => Ok(Some(self.eval_cond_thread(
                rest,
                env,
                ThreadStyle::Last,
                form_span,
            )?)),
            "some->" => Ok(Some(self.eval_some_thread(
                rest,
                env,
                ThreadStyle::First,
                form_span,
            )?)),
            "some->>" => Ok(Some(self.eval_some_thread(
                rest,
                env,
                ThreadStyle::Last,
                form_span,
            )?)),
            "loop" => Ok(Some(self.eval_loop(rest, env, form_span)?)),
            "for" => Ok(Some(self.eval_for(rest, env, form_span)?)),
            "while" => Ok(Some(self.eval_while(rest, env, form_span)?)),
            "doseq" => Ok(Some(self.eval_doseq(rest, env, form_span, "doseq")?)),
            "each" => Ok(Some(self.eval_each(rest, env, form_span)?)),
            "dotimes" => Ok(Some(self.eval_dotimes(rest, env, form_span)?)),
            "try" => Ok(Some(self.eval_try(rest, env, form_span)?)),
            "err" => Err(span_runtime_error(
                form_span,
                "err must appear at end of try or a body",
            )),
            "fin" => Err(span_runtime_error(
                form_span,
                "fin must appear at end of try or a body",
            )),
            "throw" => Ok(Some(self.eval_throw(rest, env, form_span)?)),
            "p" => Ok(Some(self.eval_p(rest, env, form_span)?)),
            "pvalues" => Ok(Some(self.eval_pvalues(rest, env)?)),
            "comment" => Ok(Some(Value::Nil)),
            "match" => Ok(Some(self.eval_match(rest, env, form_span)?)),
            "break" => Ok(Some(self.eval_break(env, form_span)?)),
            "repl" => Ok(Some(self.eval_repl(env, form_span, rest)?)),
            "debug" => Ok(Some(self.eval_repl(env, form_span, rest)?)),
            "recur" => Ok(Some(self.eval_recur(rest, env, form_span)?)),
            "with-redefs" => Ok(Some(self.eval_with_redefs(rest, env, form_span)?)),
            "with-dyn" => Ok(Some(self.eval_with_dyn(rest, env, form_span)?)),
            "go-loop" => Ok(Some(self.eval_go_loop(items, env)?)),
            "scope-loop" | "async::scope-loop" => Ok(Some(self.eval_scope_loop(items, env)?)),
            "async-scope" => Ok(Some(self.eval_async_scope(items, env)?)),
            "async::scope" => Ok(Some(self.eval_async_scope(items, env)?)),
            "ns-map" => Ok(Some(self.eval_ns_map(rest, env, form_span)?)),
            "nav" | "lookup" => Ok(Some(self.eval_nav(rest, env, form_span)?)),
            "create-ns" => Ok(Some(self.eval_create_ns(rest, env, form_span)?)),
            "refer" => Ok(Some(self.eval_refer(rest, env, form_span)?)),
            "resolve" => Ok(Some(self.eval_resolve(rest, env, form_span)?)),
            "load-file" => Ok(Some(self.eval_load_file(rest, env, form_span)?)),
            "load-string" => Ok(Some(self.eval_load_string(rest, env, form_span)?)),
            "delay" => Ok(Some(self.eval_delay(rest, env, form_span)?)),
            "doto" => Ok(Some(self.eval_doto(rest, env, form_span)?)),
            "with-open" => Ok(Some(self.eval_with_open(rest, env, form_span)?)),
            "__range_literal" => Ok(Some(self.eval_range_literal(rest, env, form_span)?)),
            "eval" => {
                if rest.len() != 1 {
                    return Err(span_runtime_error(form_span, "eval expects one argument"));
                }
                let data = self.eval(&rest[0], env.clone())?;
                let target_form = value_to_form(&data)?;
                Ok(Some(self.eval(&target_form, env)?))
            }
            _ => Ok(None),
        }
    }

    fn eval_range_literal(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 3 {
            return Err(span_runtime_error(
                form_span,
                "range literal expects start, end, exclusive",
            ));
        }
        let start_val = self.eval_range_bound(&args[0], env.clone())?;
        let end_val = self.eval_range_bound(&args[1], env.clone())?;
        let exclusive_val = self.eval(&args[2], env)?;
        let exclusive = matches!(exclusive_val, Value::Bool(true));
        let start = range_value_to_optional_int(start_val, form_span)?;
        let end = range_value_to_optional_int(end_val, form_span)?;
        Ok(Value::Map(build_range_map(start, end, exclusive)))
    }

    fn eval_range_bound(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        if let FormKind::Symbol(sym) = &form.kind {
            if let Some(v) = dynamic_vars::current_value(sym) {
                return Ok(v);
            }
            if let Some(v) = env.read().unwrap().get(sym) {
                return Ok(v);
            }
            if let Some(stripped) = sym.strip_prefix('-') {
                if !stripped.is_empty() {
                    let inner_form = Form::new(FormKind::Symbol(stripped.to_string()), form.span);
                    let inner_val = self.eval_range_bound(&inner_form, env)?;
                    return negate_range_value(inner_val, form.span);
                }
            }
        }
        self.eval(form, env)
    }

    fn eval_oop_index(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 2 {
            return Err(span_runtime_error(
                form_span,
                "oop-index expects target and key",
            ));
        }
        let target = self.eval(&args[0], env.clone())?;
        let key = self.eval(&args[1], env.clone())?;
        self.oop_index_value(target, key, env, form_span)
    }

    fn eval_oop_seg(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 2 {
            return Err(span_runtime_error(
                form_span,
                "oop-seg expects target and name",
            ));
        }
        let target = self.eval(&args[0], env.clone())?;
        let name_val = self.eval(&args[1], env.clone())?;
        let name = match name_val {
            Value::String(s) => s,
            Value::Symbol(s) => s.trim_start_matches(':').to_string(),
            other => {
                return Err(span_runtime_error(
                    form_span,
                    format!(
                        "oop-seg expects name as string or symbol, got {}",
                        other.type_name()
                    ),
                ))
            }
        };

        let mut found_count = 0usize;
        let mut found_value: Option<Value> = None;
        let mut track_found = |value: Value| {
            found_count += 1;
            if found_count == 1 {
                found_value = Some(value);
            }
        };

        match &target {
            Value::Map(map) => {
                let key_kw = Key::Keyword(name.clone());
                let key_str = Key::String(name.clone());
                let key_sym = Key::Symbol(name.clone());
                if let Some(val) = map.get(&key_kw) {
                    track_found(val.clone());
                }
                if let Some(val) = map.get(&key_str) {
                    track_found(val.clone());
                }
                if let Some(val) = map.get(&key_sym) {
                    track_found(val.clone());
                }
                if found_count > 1 {
                    return Err(span_runtime_error(
                        form_span,
                        format!(
                            "oop-seg segment '{}' is ambiguous; use explicit key segment",
                            name
                        ),
                    ));
                }
                if let Some(val) = found_value {
                    return Ok(val);
                }
            }
            Value::SortedMap(map) => {
                let key_kw = Key::Keyword(name.clone());
                let key_str = Key::String(name.clone());
                let key_sym = Key::Symbol(name.clone());
                if let Some(val) = builtins::sorted_map_get(map, &key_kw)? {
                    track_found(val);
                }
                if let Some(val) = builtins::sorted_map_get(map, &key_str)? {
                    track_found(val);
                }
                if let Some(val) = builtins::sorted_map_get(map, &key_sym)? {
                    track_found(val);
                }
                if found_count > 1 {
                    return Err(span_runtime_error(
                        form_span,
                        format!(
                            "oop-seg segment '{}' is ambiguous; use explicit key segment",
                            name
                        ),
                    ));
                }
                if let Some(val) = found_value {
                    return Ok(val);
                }
            }
            _ => {}
        }

        let subject_policy = self.resolve_subject_pos(&name, 0, form_span, &env)?;
        let method_value = self.lookup_symbol_value(&name, &env);
        let callable = match method_value {
            Some(value) if is_callable_value(&value) => value,
            _ => {
                return Err(span_runtime_error(
                    form_span,
                    format!("oop-seg could not resolve segment '{}'", name),
                ))
            }
        };
        let _subject_pos = self.materialize_subject_pos(subject_policy, 0, form_span)?;
        call_callable(callable, vec![target])
    }

    fn eval_oop_method(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "oop-method expects target and name",
            ));
        }
        let args_have_placeholder = args
            .iter()
            .any(|arg| form_contains_question_placeholder_scoped(arg, &StdHashSet::new()));
        if args_have_placeholder {
            let (fn_arg_positions, meta_known) =
                self.fn_arg_positions_for_oop_method_call(args, &env, form_span)?;
            if meta_known {
                let mut replaced_args = Vec::with_capacity(args.len());
                let mut any_wrapped = false;
                for (arg_idx, arg) in args.iter().enumerate() {
                    let pos = arg_idx + 1;
                    if fn_arg_positions.contains(&pos) {
                        if let Some((wrapped, _)) = wrap_question_lambda(arg) {
                            replaced_args.push(wrapped);
                            any_wrapped = true;
                        } else {
                            replaced_args.push(arg.clone());
                        }
                    } else {
                        replaced_args.push(arg.clone());
                    }
                }
                let mut needs_full = false;
                for (arg_idx, arg) in args.iter().enumerate() {
                    let pos = arg_idx + 1;
                    if !fn_arg_positions.contains(&pos)
                        && self.form_contains_unresolved_placeholder(arg, &env)
                    {
                        needs_full = true;
                        break;
                    }
                }
                if needs_full {
                    let mut call_items = Vec::with_capacity(args.len() + 1);
                    call_items.push(Form::new(
                        FormKind::Symbol(OOP_METHOD_SYM.to_string()),
                        form_span,
                    ));
                    call_items.extend(args.iter().cloned());
                    let call_form = Form::new(FormKind::List(call_items), form_span);
                    if let Some((expanded, _)) = wrap_question_lambda(&call_form) {
                        return self.eval(&expanded, env);
                    }
                } else if any_wrapped {
                    let target = self.eval(&replaced_args[0], env.clone())?;
                    let name_val = self.eval(&replaced_args[1], env.clone())?;
                    let mut call_args = Vec::with_capacity(replaced_args.len().saturating_sub(2));
                    for arg in &replaced_args[2..] {
                        call_args.push(self.eval(arg, env.clone())?);
                    }
                    return self.oop_method_value(target, name_val, call_args, env, form_span);
                }
            } else {
                let needs_full = args
                    .iter()
                    .any(|arg| self.form_contains_unresolved_placeholder(arg, &env));
                if needs_full {
                    let mut call_items = Vec::with_capacity(args.len() + 1);
                    call_items.push(Form::new(
                        FormKind::Symbol(OOP_METHOD_SYM.to_string()),
                        form_span,
                    ));
                    call_items.extend(args.iter().cloned());
                    let call_form = Form::new(FormKind::List(call_items), form_span);
                    if let Some((expanded, _)) = wrap_question_lambda(&call_form) {
                        return self.eval(&expanded, env);
                    }
                }
            }
        }
        let target = self.eval(&args[0], env.clone())?;
        let name_val = self.eval(&args[1], env.clone())?;
        let mut call_args = Vec::with_capacity(args.len().saturating_sub(2));
        for arg in &args[2..] {
            call_args.push(self.eval(arg, env.clone())?);
        }
        self.oop_method_value(target, name_val, call_args, env, form_span)
    }

    pub(crate) fn oop_index_value(
        &self,
        target: Value,
        key: Value,
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if matches!(target, Value::Set(_) | Value::SortedSet(_)) {
            return Err(span_runtime_error(
                form_span,
                "dot-indexer does not support set targets; use contains? or get",
            ));
        }
        let getter = self
            .lookup_symbol_value("get", &env)
            .ok_or_else(|| span_runtime_error(form_span, "get is not available for dot-indexer"))?;
        call_callable(getter, vec![target, key])
    }

    pub(crate) fn oop_method_value(
        &self,
        target: Value,
        name_val: Value,
        call_args: Vec<Value>,
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let name = match name_val {
            Value::String(s) => s,
            Value::Symbol(s) => s.trim_start_matches(':').to_string(),
            other => {
                return Err(span_runtime_error(
                    form_span,
                    format!(
                        "oop-method expects name as string or symbol, got {}",
                        other.type_name()
                    ),
                ))
            }
        };

        let method_key = Key::Keyword(name.clone());
        let method_value = match &target {
            Value::Map(map) => map.get(&method_key).cloned(),
            Value::SortedMap(map) => builtins::sorted_map_get(map, &method_key)?,
            _ => None,
        };
        if let Some(value) = method_value {
            if is_callable_value(&value) {
                if is_method_value(&value) {
                    let mut method_args = Vec::with_capacity(call_args.len() + 1);
                    method_args.push(target);
                    method_args.extend(call_args);
                    return call_callable(value, method_args);
                }
                return call_callable(value, call_args);
            }
            return Err(span_runtime_error(
                form_span,
                format!(
                    "recv has key :{}, but it is not callable. Access it via recv.:{} or recv[:{}].",
                    name, name, name
                ),
            ));
        }

        let subject_policy = self.resolve_subject_pos(&name, call_args.len(), form_span, &env)?;
        let method_value = self.lookup_symbol_value(&name, &env);
        let callable = match method_value {
            Some(value) if is_callable_value(&value) => value,
            _ => {
                return Err(span_runtime_error(
                    form_span,
                    format!("oop-seg could not resolve segment '{}'", name),
                ))
            }
        };
        let subject_pos =
            self.materialize_subject_pos(subject_policy, call_args.len(), form_span)?;
        let insert_idx = subject_pos.saturating_sub(1);
        let mut inserted = false;
        let mut final_args = Vec::with_capacity(call_args.len() + 1);
        for (idx, arg) in call_args.into_iter().enumerate() {
            if idx == insert_idx {
                final_args.push(target.clone());
                inserted = true;
            }
            final_args.push(arg);
        }
        if !inserted {
            final_args.push(target);
        }
        call_callable(callable, final_args)
    }

    fn eval_pvalues(&self, args: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        let mut out = Vector::new();
        for form in args {
            out.push_back(self.eval(form, env.clone())?);
        }
        Ok(Value::Vector(out))
    }

    fn eval_p(&self, rest: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if rest.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "p expects at least one argument",
            ));
        }
        if rest.len() >= 2 {
            let call_form = Form::new(FormKind::List(rest.to_vec()), form_span);
            let value = self.eval(&call_form, env)?;
            println!("{}", format_span(form_span, &value.to_string()));
            return Ok(value);
        }
        let arg = &rest[0];
        if let FormKind::Symbol(name) = &arg.kind {
            let is_top_level = self.is_top_level_env(&env);
            if !is_top_level && env.read().unwrap().contains_local(name) {
                let value = self.eval(arg, env)?;
                println!("{}", format_span(form_span, &value.to_string()));
                return Ok(value);
            }
            let value = self.eval(arg, env.clone())?;
            if value_accepts_n(&value, 0) {
                let called = call_callable(value.clone(), vec![])?;
                println!("{}", format_span(form_span, &called.to_string()));
                return Ok(called);
            }
            println!("{}", format_span(form_span, &value.to_string()));
            return Ok(value);
        }
        let value = self.eval(arg, env)?;
        println!("{}", format_span(form_span, &value.to_string()));
        Ok(value)
    }

    fn try_eval_foreign(
        &self,
        tag: &str,
        items: &[Form],
        env: EnvRef,
    ) -> Result<Option<Value>, CloveError> {
        if let Some(eng) = self.engines.iter().find(|e| e.tag() == tag) {
            let mut code = if items.len() == 2 {
                match &items[1].kind {
                    FormKind::ForeignBlock { code, .. } => code.clone(),
                    FormKind::String(s) => s.clone(),
                    _ => form_to_string(&items[1], tag),
                }
            } else {
                if tag == "rb" {
                    // Ruby Chain Logic
                    let head = match &items[1].kind {
                        FormKind::ForeignBlock { code, .. } => code.clone(),
                        FormKind::String(s) => s.clone(),
                        _ => form_to_string(&items[1], tag),
                    };
                    let tail: String = items[2..]
                        .iter()
                        .map(|f| match &f.kind {
                            FormKind::ForeignBlock { code, .. } => code.clone(),
                            FormKind::String(s) => s.clone(),
                            _ => form_to_string(f, tag),
                        })
                        .collect();
                    format!("({}){}", head, tail)
                } else {
                    // Flatten logic
                    let parts: Vec<String> = items[1..]
                        .iter()
                        .map(|f| match &f.kind {
                            FormKind::ForeignBlock { code, .. } => code.clone(),
                            FormKind::String(s) => s.clone(),
                            _ => form_to_string(f, tag),
                        })
                        .collect();
                    parts.join(" ")
                }
            };
            if tag == "rb"
                && !matches!(
                    items[1].kind,
                    FormKind::ForeignBlock { .. } | FormKind::String(_)
                )
            {
                code = format!("({})", code);
            }
            return eng.eval_block(&code, env, Some(items[1].span)).map(Some);
        }
        Ok(None)
    }

    fn eval_def(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if !self.is_top_level_env(&env) {
            return Err(span_runtime_error(
                form_span,
                "def is top-level only; use -def for local binding or set! to update an existing var",
            ));
        }
        if args.len() < 2 {
            return Err(span_runtime_error(form_span, "def expects name and value"));
        }
        let name_form = &args[0];
        let name = if let FormKind::Symbol(s) = &name_form.kind {
            s.clone()
        } else {
            return Err(span_runtime_error(args[0].span, "def name must be symbol"));
        };
        let (doc, meta_form, idx) = self.parse_doc_and_meta(args, 1, "def", true)?;
        if idx + 1 != args.len() {
            return Err(span_runtime_error(form_span, "def expects name and value"));
        }
        let meta_value_map = if let Some(form) = &meta_form {
            match form_to_value(form)? {
                Value::Map(map) => Some(map),
                _ => {
                    return Err(span_runtime_error(
                        form.span,
                        "def attr-map must be a map literal",
                    ))
                }
            }
        } else {
            None
        };
        let qualified_name = qualified_function_name(&env, &name);
        let mut val =
            attach_lambda_name(self.eval(&args[idx], env.clone())?, qualified_name.clone());
        val = attach_lambda_doc(val, doc.clone());
        val = attach_lambda_meta(val, meta_value_map.clone());
        if let Some(meta_map) = meta_value_map.clone() {
            if !matches!(val, Value::Lambda { .. } | Value::MultiLambda { .. }) {
                meta_set(val.clone(), Value::Map(meta_map));
            }
        }
        let source = build_list_source("def", args, form_span);
        let meta_name = qualified_name.unwrap_or_else(|| name.clone());
        let symbol_info = symbol_meta::SymbolMeta {
            doc,
            meta: meta_value_map,
            source: Some(source),
        };
        symbol_meta::register(&meta_name, symbol_info);
        {
            env.write().unwrap().set(&name, val.clone());
        }
        bump_global_version(&name, &env);
        self.clear_imported_flag(&env, &name);
        Ok(val)
    }

    fn eval_def_private(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let val = self.eval_def(args, env.clone(), form_span)?;
        if let Some(FormKind::Symbol(name)) = args.first().map(|form| &form.kind) {
            self.mark_private_name(&env, name);
        }
        Ok(val)
    }

    fn eval_defn(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if !self.is_top_level_env(&env) {
            return Err(span_runtime_error(
                form_span,
                "defn is top-level only; use -defn for local function",
            ));
        }
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "defn expects name, params vector, and body",
            ));
        }
        let name_form = &args[0];
        let name = if let FormKind::Symbol(s) = &name_form.kind {
            s.clone()
        } else {
            return Err(span_runtime_error(
                name_form.span,
                "defn name must be symbol",
            ));
        };
        let mut signature = self.parse_defn_signature(&args[1..], form_span)?;
        let (method_clauses, is_method) = methodify_clauses(&signature.clauses, false);
        if is_method {
            signature = with_method_clauses(signature, method_clauses);
        }
        let qualified_name = qualified_function_name(&env, &name);
        let doc_for_meta = signature.doc.clone();
        let doc_for_symbol = signature.doc.clone();
        let meta_value_map = if let Some(meta_form) = &signature.meta_map {
            match form_to_value(meta_form)? {
                Value::Map(m) => Some(m),
                _ => {
                    return Err(span_runtime_error(
                        meta_form.span,
                        "defn attr-map must be a map literal",
                    ))
                }
            }
        } else {
            None
        };
        let source_meta = build_lambda_source_meta();
        let meta_value_map = merge_meta_maps(meta_value_map, source_meta);
        let meta_span = signature
            .meta_map
            .as_ref()
            .map(|f| f.span)
            .unwrap_or(form_span);
        let source = build_list_source("defn", args, form_span);
        let mut clause_param_info: Vec<(Span, usize, Option<usize>)> = Vec::new();
        let mut parsed_clauses = Vec::with_capacity(signature.clauses.len());
        for clause in &signature.clauses {
            let parsed = self.parse_params(&clause.params_form)?;
            clause_param_info.push((
                clause.params_form.span,
                parsed.params.len(),
                parsed.subject_pos,
            ));
            parsed_clauses.push(parsed);
        }
        let mut subject_pos_hint = self.infer_subject_pos_from_parsed_params(&parsed_clauses);
        if let Some(meta_pos) = signature.subject_pos_meta.clone() {
            match &meta_pos {
                SubjectPos::Last => {
                    for (_span, param_count, marker_pos) in &clause_param_info {
                        if let Some(pos) = marker_pos {
                            if *pos != *param_count {
                                return Err(span_runtime_error(
                                    meta_span,
                                    "subject-pos metadata conflicts with '$' parameter marker",
                                ));
                            }
                        }
                    }
                }
                SubjectPos::Fixed(n) => {
                    for (_span, _param_count, marker_pos) in &clause_param_info {
                        if let Some(pos) = marker_pos {
                            if *pos != *n {
                                return Err(span_runtime_error(
                                    meta_span,
                                    "subject-pos metadata conflicts with '$' parameter marker",
                                ));
                            }
                        }
                    }
                }
            }
            subject_pos_hint = Some(meta_pos);
        }
        let mut inferred_type = None;
        let mut optimized_body = None;
        let mut meta_opt = None;
        let mut cleaned_clauses = Vec::with_capacity(signature.clauses.len());
        for clause in &signature.clauses {
            let (_, body_forms) = self.collect_local_defns(&clause.body)?;
            cleaned_clauses.push(FnClauseSpec {
                params_form: clause.params_form.clone(),
                body: body_forms,
            });
        }
        let meta_value_map_for_func = meta_value_map.clone();
        if !signature.is_multi {
            let clause = &cleaned_clauses[0];
            if let Some((meta, forms, lambda_ty)) = self.build_fn_meta_for_defn(
                &env,
                name_form,
                &clause.params_form,
                &clause.body,
                doc_for_meta.clone(),
                qualified_name.as_deref(),
                subject_pos_hint,
            ) {
                meta_opt = Some(meta);
                optimized_body = forms;
                inferred_type = Some(lambda_ty);
            }
        } else if let Some(meta) = self.build_multi_fn_meta(
            &env,
            name_form,
            &cleaned_clauses,
            doc_for_meta.clone(),
            qualified_name.as_deref(),
            subject_pos_hint,
        ) {
            meta_opt = Some(meta);
        }
        let mut func = attach_lambda_doc(
            self.eval_fn(&signature.fn_args, env.clone(), form_span)?,
            signature.doc.clone(),
        );
        if !signature.is_multi {
            if let Some(body) = optimized_body {
                func = attach_lambda_body(func, body);
            }
        }
        func = attach_lambda_inferred_type(func, inferred_type);
        func = attach_lambda_name(func, qualified_name.clone());
        func = attach_lambda_meta(func, meta_value_map_for_func);
        if is_method {
            func = mark_method_value(func);
        }
        if let Some(meta) = meta_opt {
            fn_meta::register(meta);
        }
        let meta_name = qualified_name.clone().unwrap_or_else(|| name.clone());
        let symbol_info = symbol_meta::SymbolMeta {
            doc: doc_for_symbol,
            meta: meta_value_map,
            source: Some(source),
        };
        symbol_meta::register(&meta_name, symbol_info);
        {
            env.write().unwrap().set(&name, func.clone());
        }
        bump_global_version(&name, &env);
        self.clear_imported_flag(&env, &name);
        Ok(func)
    }

    fn eval_defn_private(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let val = self.eval_defn(args, env.clone(), form_span)?;
        if let Some(FormKind::Symbol(name)) = args.first().map(|form| &form.kind) {
            self.mark_private_name(&env, name);
        }
        Ok(val)
    }

    fn clear_imported_flag(&self, env: &EnvRef, name: &str) {
        if let Some(store) = &self.namespace_store {
            if let Some(ns) = current_namespace_name(env) {
                if let Ok(mut guard) = store.write() {
                    if let Some(entry) = guard.get_mut(&ns) {
                        entry.imported_symbols.remove(name);
                    }
                }
            }
        }
    }

    fn namespace_store_opt(&self) -> Option<Arc<RwLock<NamespaceStore>>> {
        if let Some(store) = self.namespace_store.as_ref() {
            return Some(store.clone());
        }
        RuntimeCtx::try_with_current(|ctx| Ok(ctx.namespace_store())).and_then(|res| res.ok())
    }

    fn namespace_env_for(&self, ns: &str) -> Option<EnvRef> {
        let store = self.namespace_store_opt()?;
        let guard = store.read().ok()?;
        guard.get(ns).map(|entry| entry.env())
    }

    fn bind_enum_variant_qualified(
        &self,
        enum_fqn: &str,
        variant_local: &str,
        constructor: Value,
        predicate: Value,
    ) {
        let Some(store) = self.namespace_store_opt() else {
            return;
        };
        let mut guard = match store.write() {
            Ok(guard) => guard,
            Err(_) => return,
        };
        let entry = guard.ensure(enum_fqn);
        let ns_env = entry.env();
        let mut writer = ns_env.write().unwrap();
        writer.set(variant_local, constructor);
        writer.set(&format!("{}?", variant_local), predicate);
    }

    fn current_namespace_env(&self, env: &EnvRef) -> Option<(String, EnvRef)> {
        let ns = current_namespace_name(env)?;
        let ns_env = self.namespace_env_for(&ns)?;
        Some((ns, ns_env))
    }

    fn is_top_level_env(&self, env: &EnvRef) -> bool {
        if let Some((_, ns_env)) = self.current_namespace_env(env) {
            return Arc::ptr_eq(&ns_env, env);
        }
        env.read().unwrap().outer_ref().is_none()
    }

    fn mark_private_name(&self, env: &EnvRef, name: &str) {
        let Some(ns) = current_namespace_name(env) else {
            return;
        };
        let Some(store) = self.namespace_store_opt() else {
            return;
        };
        let mut guard = match store.write() {
            Ok(guard) => guard,
            Err(_) => return,
        };
        if let Some(entry) = guard.get_mut(&ns) {
            entry.mark_private(name.to_string());
        }
    }

    fn is_private_name(&self, ns: &str, name: &str) -> bool {
        let Some(store) = self.namespace_store_opt() else {
            return false;
        };
        let Ok(guard) = store.read() else {
            return false;
        };
        guard
            .get(ns)
            .map(|entry| entry.private_names.contains(name))
            .unwrap_or(false)
    }

    fn local_binding_exists_until(
        &self,
        env: &EnvRef,
        stop_at: Option<&EnvRef>,
        name: &str,
    ) -> bool {
        let mut current = Some(env.clone());
        while let Some(cursor) = current {
            if let Some(stop_env) = stop_at {
                if Arc::ptr_eq(&cursor, stop_env) {
                    break;
                }
            }
            let reader = cursor.read().unwrap();
            if reader.contains_local(name) {
                return true;
            }
            current = reader.outer_ref();
        }
        false
    }

    fn root_env(&self, env: EnvRef) -> EnvRef {
        let mut current = env;
        loop {
            let next = current.read().unwrap().outer_ref();
            match next {
                Some(next_env) => current = next_env,
                None => return current,
            }
        }
    }

    fn eval_deftype(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "deftype expects name and schema map",
            ));
        }
        let name_form = &args[0];
        let type_name_raw = match &name_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                return Err(span_runtime_error(
                    name_form.span,
                    "deftype name must be symbol",
                ));
            }
        };
        let (doc, mut meta_form, idx) = self.parse_doc_and_meta(args, 1, "deftype", true)?;
        let mut meta_value_map = if let Some(form) = &meta_form {
            match form_to_value(form)? {
                Value::Map(map) => Some(map),
                _ => {
                    return Err(span_runtime_error(
                        form.span,
                        "deftype attr-map must be a map literal",
                    ))
                }
            }
        } else {
            None
        };
        let current_ns = current_namespace_name(&env).ok_or_else(|| {
            span_runtime_error(name_form.span, "deftype requires namespace context")
        })?;
        let (alias_target, mut from_binding, schema_start) =
            parse_deftype_options(args, idx, &current_ns)?;
        let (mut field_forms, where_forms, method_forms) =
            split_deftype_forms(&args[schema_start..]);
        if field_forms.is_empty() && (!where_forms.is_empty() || !method_forms.is_empty()) {
            if let Some(form) = meta_form.take() {
                field_forms.push(form);
                meta_value_map = None;
            }
        }
        if from_binding.is_none() {
            if let Some(binding) = self.parse_deftype_from_def(&field_forms)? {
                from_binding = Some(binding);
                field_forms.clear();
            }
        }
        if from_binding.is_some() && !field_forms.is_empty() {
            return Err(span_runtime_error(
                field_forms[0].span,
                "deftype :from does not accept fields",
            ));
        }
        let (methods, required_methods) =
            self.collect_deftype_methods(&where_forms, &method_forms, env.clone())?;
        if alias_target.is_some() && !methods.is_empty() {
            return Err(span_runtime_error(
                where_forms[0].span,
                "deftype :alias does not accept methods",
            ));
        }
        let (type_ns, type_local) = resolve_type_name(&current_ns, &type_name_raw, name_form.span)?;
        if type_ns != current_ns {
            return Err(span_runtime_error(
                name_form.span,
                "deftype name must be in the current namespace",
            ));
        }
        let fqn = format!("{}::{}", type_ns, type_local);
        let mut from_value = None;
        let mut from_fields = None;
        if alias_target.is_none() {
            if let Some(binding) = &from_binding {
                let value = self.eval(&binding.value_form, env.clone())?;
                if !matches!(value, Value::Map(_) | Value::SortedMap(_)) {
                    let msg = if binding.is_def {
                        "deftype (def ...) expects map value"
                    } else {
                        "deftype :from expects map value"
                    };
                    return Err(span_runtime_error(binding.value_form.span, msg));
                }
                let fields = if let Some(kind) = &binding.type_expr {
                    let span = binding.type_span.unwrap_or(binding.value_form.span);
                    fields_from_type_expr(kind, span, &current_ns)?
                } else {
                    fields_from_value(&value, binding.value_form.span, &current_ns)?
                };
                from_fields = Some(fields);
                from_value = Some(value);
            }
        }
        let constructor = if let Some(alias_target) = alias_target {
            if !field_forms.is_empty() {
                return Err(span_runtime_error(
                    field_forms[0].span,
                    "deftype :alias does not accept fields",
                ));
            }
            let meta = AliasMeta {
                namespace: type_ns.clone(),
                name: type_local.clone(),
                doc: doc.clone(),
                target: alias_target.clone(),
            };
            type_registry::register_alias(meta)?;
            let alias_constructor = make_alias_constructor(&fqn, alias_target.clone());
            if let Some(meta_map) = meta_value_map.clone() {
                meta_set(alias_constructor.clone(), Value::Map(meta_map));
            }
            let predicate = make_alias_predicate(alias_target);
            env.write()
                .unwrap()
                .set(&type_local, alias_constructor.clone());
            env.write()
                .unwrap()
                .set(&format!("{}?", type_local), predicate);
            alias_constructor
        } else {
            let fields = if let Some(fields) = from_fields {
                fields
            } else {
                parse_type_fields(&field_forms, &type_ns)?
            };
            let field_names: StdHashSet<String> =
                fields.iter().map(|field| field.name.clone()).collect();
            for key in methods.keys() {
                if let Key::Keyword(name) = key {
                    if field_names.contains(name) {
                        return Err(span_runtime_error(
                            name_form.span,
                            "method name conflicts with field name in deftype",
                        ));
                    }
                }
            }
            let fields_empty = fields.is_empty();
            let meta = ProductMeta {
                namespace: type_ns.clone(),
                name: type_local.clone(),
                doc: doc.clone(),
                fields,
                belongs_to: StdHashSet::new(),
                required_methods,
            };
            type_registry::register_product(meta)?;
            let type_constructor = make_type_constructor(&fqn, fields_empty, methods.clone());
            if let Some(meta_map) = meta_value_map.clone() {
                meta_set(type_constructor.clone(), Value::Map(meta_map));
            }
            let predicate = make_type_predicate(&fqn);
            env.write()
                .unwrap()
                .set(&type_local, type_constructor.clone());
            env.write()
                .unwrap()
                .set(&format!("{}?", type_local), predicate);
            type_constructor
        };
        if let Some(binding) = from_binding {
            let value = match from_value {
                Some(value) => value,
                None => self.eval(&binding.value_form, env.clone())?,
            };
            let constructed = call_callable(constructor.clone(), vec![value])?;
            let mut bound =
                attach_lambda_name(constructed, qualified_function_name(&env, &binding.name));
            bound = attach_lambda_doc(bound, binding.doc.clone());
            let meta_value_map = if let Some(form) = &binding.meta_form {
                match form_to_value(form)? {
                    Value::Map(map) => Some(map),
                    _ => {
                        return Err(span_runtime_error(
                            form.span,
                            "def attr-map must be a map literal",
                        ))
                    }
                }
            } else {
                None
            };
            bound = attach_lambda_meta(bound, meta_value_map.clone());
            if let Some(meta_map) = meta_value_map.clone() {
                if !matches!(bound, Value::Lambda { .. } | Value::MultiLambda { .. }) {
                    meta_set(bound.clone(), Value::Map(meta_map));
                }
            }
            env.write().unwrap().set(&binding.name, bound.clone());
            self.clear_imported_flag(&env, &binding.name);
        }
        let source = build_list_source("deftype", args, form_span);
        let symbol_info = symbol_meta::SymbolMeta {
            doc,
            meta: meta_value_map,
            source: Some(source),
        };
        symbol_meta::register(&fqn, symbol_info);
        self.clear_imported_flag(&env, &type_local);
        self.clear_imported_flag(&env, &format!("{}?", type_local));
        Ok(Value::Symbol(fqn))
    }

    fn collect_deftype_methods(
        &self,
        where_forms: &[Form],
        method_forms: &[Form],
        env: EnvRef,
    ) -> Result<(HashMap<Key, Value>, Vec<RequiredMethod>), CloveError> {
        if where_forms.is_empty() && method_forms.is_empty() {
            return Ok((HashMap::new(), Vec::new()));
        }
        let mut methods = HashMap::new();
        let mut required_methods = Vec::new();
        for where_form in where_forms {
            let FormKind::List(items) = &where_form.kind else {
                continue;
            };
            if items.is_empty() {
                continue;
            }
            if !matches!(
                items.first().map(|item| &item.kind),
                Some(FormKind::Symbol(sym)) if sym == "where"
            ) {
                continue;
            }
            for item in items.iter().skip(1) {
                let FormKind::List(def_items) = &item.kind else {
                    return Err(span_runtime_error(
                        item.span,
                        "where in deftype expects defn or method forms",
                    ));
                };
                let head = match def_items.first().map(|f| &f.kind) {
                    Some(FormKind::Symbol(sym)) => sym.as_str(),
                    _ => {
                        return Err(span_runtime_error(
                            item.span,
                            "where in deftype expects defn or method forms",
                        ))
                    }
                };
                if head != "defn" && head != "method" {
                    return Err(span_runtime_error(
                        item.span,
                        "where in deftype expects defn or method forms",
                    ));
                }
                let method_def =
                    self.build_deftype_method_from_items(def_items, head, env.clone(), item.span)?;
                let key = Key::Keyword(method_def.name.clone());
                if methods.contains_key(&key) {
                    return Err(span_runtime_error(
                        item.span,
                        "duplicate method name in deftype",
                    ));
                }
                methods.insert(key, method_def.value);
                required_methods.push(method_def.required);
            }
        }
        for method_form in method_forms {
            let FormKind::List(def_items) = &method_form.kind else {
                return Err(span_runtime_error(
                    method_form.span,
                    "deftype expects defn or method forms",
                ));
            };
            let head = match def_items.first().map(|f| &f.kind) {
                Some(FormKind::Symbol(sym)) => sym.as_str(),
                _ => {
                    return Err(span_runtime_error(
                        method_form.span,
                        "deftype expects defn or method forms",
                    ))
                }
            };
            if head != "defn" && head != "method" {
                return Err(span_runtime_error(
                    method_form.span,
                    "deftype expects defn or method forms",
                ));
            }
            let method_def = self.build_deftype_method_from_items(
                def_items,
                head,
                env.clone(),
                method_form.span,
            )?;
            let key = Key::Keyword(method_def.name.clone());
            if methods.contains_key(&key) {
                return Err(span_runtime_error(
                    method_form.span,
                    "duplicate method name in deftype",
                ));
            }
            methods.insert(key, method_def.value);
            required_methods.push(method_def.required);
        }
        Ok((methods, required_methods))
    }

    fn build_deftype_method_from_items(
        &self,
        items: &[Form],
        head: &str,
        env: EnvRef,
        form_span: Span,
    ) -> Result<DeftypeMethodDef, CloveError> {
        if items.len() < 3 {
            return Err(span_runtime_error(
                form_span,
                format!("{head} expects name, params vector, and body"),
            ));
        }
        let name_form = &items[1];
        let name = if let FormKind::Symbol(sym) = &name_form.kind {
            sym.clone()
        } else {
            return Err(span_runtime_error(
                name_form.span,
                format!("{head} name must be symbol"),
            ));
        };
        let mut signature = self.parse_defn_signature_with_context(&items[2..], form_span, head)?;
        ensure_method_params_without_self(&signature.clauses)?;
        let required_sig = self.method_signature_from_defn(&signature)?;
        let (method_clauses, _) = methodify_clauses(&signature.clauses, true);
        signature = with_method_clauses(signature, method_clauses);
        let mut func = if signature.clauses.len() == 1 {
            self.build_single_lambda(Some(name.clone()), &signature.clauses[0], env.clone())?
        } else {
            self.build_multi_lambda(Some(name.clone()), &signature.clauses, env.clone())?
        };
        func = attach_lambda_doc(func, signature.doc.clone());
        if let Some(meta_form) = &signature.meta_map {
            let meta_value_map = match form_to_value(meta_form)? {
                Value::Map(map) => Some(map),
                _ => {
                    return Err(span_runtime_error(
                        meta_form.span,
                        "defn attr-map must be a map literal",
                    ))
                }
            };
            func = attach_lambda_meta(func, meta_value_map);
        }
        func = mark_method_value(func);
        Ok(DeftypeMethodDef {
            name: name.clone(),
            value: func,
            required: RequiredMethod {
                name,
                sig: required_sig,
            },
        })
    }

    fn method_signature_from_defn(
        &self,
        signature: &DefnSignature,
    ) -> Result<Option<FnSigSpec>, CloveError> {
        if signature.clauses.len() != 1 {
            return Ok(None);
        }
        let clause = &signature.clauses[0];
        let params_form = &clause.params_form;
        let (param_hints, rest_hint) = collect_param_type_hints(params_form);
        let ret_hint = return_hint_from_form(params_form);
        let has_hint = param_hints.iter().any(|hint| hint.is_some())
            || rest_hint.is_some()
            || ret_hint.is_some();
        if !has_hint {
            return Ok(None);
        }
        let parsed = self.parse_params(params_form)?;
        let params = param_hints
            .into_iter()
            .map(|hint| hint.unwrap_or_else(MetaTypeKind::any))
            .collect();
        let rest = parsed.rest.as_ref().map(|_| rest_type_from_hint(rest_hint));
        let ret = ret_hint.unwrap_or_else(MetaTypeKind::any);
        Ok(Some(FnSigSpec { params, rest, ret }))
    }

    fn parse_deftype_from_def(&self, forms: &[Form]) -> Result<Option<DeftypeFrom>, CloveError> {
        if forms.len() != 1 {
            return Ok(None);
        }
        let form = &forms[0];
        let FormKind::List(items) = &form.kind else {
            return Ok(None);
        };
        let head = match items.first().map(|f| &f.kind) {
            Some(FormKind::Symbol(sym)) => sym.as_str(),
            _ => return Ok(None),
        };
        if head != "def" {
            return Ok(None);
        }
        if items.len() < 3 {
            return Err(span_runtime_error(
                form.span,
                "deftype (def ...) expects name and value",
            ));
        }
        let name_form = &items[1];
        let name = match &name_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                return Err(span_runtime_error(
                    name_form.span,
                    "deftype (def ...) name must be symbol",
                ))
            }
        };
        let (doc, meta_form, idx) = self.parse_doc_and_meta(items, 2, "def", true)?;
        if idx >= items.len() {
            return Err(span_runtime_error(
                form.span,
                "deftype (def ...) expects name and value",
            ));
        }
        let value_form = items[idx].clone();
        let mut type_expr = None;
        let mut type_span = None;
        if idx + 1 < items.len() {
            let mut type_start = idx + 1;
            let marker = &items[type_start];
            let is_type_marker = matches!(
                &marker.kind,
                FormKind::Symbol(sym) if sym == ":"
            ) || matches!(
                &marker.kind,
                FormKind::Keyword(name) if name.is_empty()
            );
            if is_type_marker {
                type_start += 1;
            }
            if type_start >= items.len() {
                return Err(span_runtime_error(
                    marker.span,
                    "deftype (def ...) expects type expression",
                ));
            }
            let type_forms = &items[type_start..];
            let (kind, consumed) = parse_type_expr_from_forms(type_forms)?;
            if consumed != type_forms.len() {
                return Err(span_runtime_error(
                    type_forms[consumed].span,
                    "deftype (def ...) expects a single type expression",
                ));
            }
            type_span = Some(type_forms[0].span);
            type_expr = Some(kind);
        }
        Ok(Some(DeftypeFrom {
            name,
            value_form,
            doc,
            meta_form,
            type_expr,
            type_span,
            is_def: true,
        }))
    }

    fn eval_defenum(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "defenum expects name and at least one member",
            ));
        }
        let name_form = &args[0];
        let enum_name_raw = match &name_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                return Err(span_runtime_error(
                    name_form.span,
                    "defenum name must be symbol",
                ));
            }
        };
        let (doc, meta_form, idx) = self.parse_doc_and_meta(args, 1, "defenum", false)?;
        let meta_value_map = if let Some(form) = &meta_form {
            match form_to_value(form)? {
                Value::Map(map) => Some(map),
                _ => {
                    return Err(span_runtime_error(
                        form.span,
                        "defenum attr-map must be a map literal",
                    ))
                }
            }
        } else {
            None
        };
        let (qualified_only, member_start) = parse_defenum_options(args, idx)?;
        let member_forms = &args[member_start..];
        if member_forms.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "defenum requires at least one member",
            ));
        }
        let current_ns = current_namespace_name(&env).ok_or_else(|| {
            span_runtime_error(name_form.span, "defenum requires namespace context")
        })?;
        let (enum_ns, enum_local) = resolve_type_name(&current_ns, &enum_name_raw, name_form.span)?;
        if enum_ns != current_ns {
            return Err(span_runtime_error(
                name_form.span,
                "defenum name must be in the current namespace",
            ));
        }
        let enum_fqn = format!("{}::{}", enum_ns, enum_local);
        let variant_ns = format!("{}::{}", enum_ns, enum_local);
        let mut member_idx = 0;
        while member_idx < member_forms.len() {
            let member_form = &member_forms[member_idx];
            let sym = match &member_form.kind {
                FormKind::Symbol(sym) => sym.clone(),
                _ => {
                    return Err(span_runtime_error(
                        member_form.span,
                        "defenum members must be symbols",
                    ));
                }
            };
            if sym.starts_with('*') {
                member_idx += 1;
                continue;
            }
            let payload_form = member_forms
                .get(member_idx + 1)
                .filter(|form| matches!(form.kind, FormKind::Map(_)));
            let (member_ns, member_local) = resolve_type_name(&current_ns, &sym, member_form.span)?;
            if member_ns != current_ns {
                return Err(span_runtime_error(
                    member_form.span,
                    "defenum member types must be in the current namespace",
                ));
            }
            if let Some(payload_form) = payload_form {
                let variant_fqn = format!("{}::{}", variant_ns, member_local);
                let fields = parse_type_fields(std::slice::from_ref(payload_form), &enum_ns)?;
                let fields_empty = fields.is_empty();
                let meta = ProductMeta {
                    namespace: variant_ns.clone(),
                    name: member_local.clone(),
                    doc: None,
                    fields,
                    belongs_to: StdHashSet::new(),
                    required_methods: Vec::new(),
                };
                type_registry::register_product(meta)?;
                let constructor = make_type_constructor(&variant_fqn, fields_empty, HashMap::new());
                let predicate = make_type_predicate(&variant_fqn);
                self.bind_enum_variant_qualified(
                    &variant_ns,
                    &member_local,
                    constructor.clone(),
                    predicate.clone(),
                );
                if !qualified_only {
                    env.write().unwrap().set(&member_local, constructor);
                    env.write()
                        .unwrap()
                        .set(&format!("{}?", member_local), predicate);
                    register_variant_type_alias(
                        &current_ns,
                        &member_local,
                        &variant_fqn,
                        member_form.span,
                    )?;
                }
                member_idx += 2;
            } else {
                let source_fqn = format!("{}::{}", current_ns, member_local);
                if let Some(TypeEntry::Product(meta)) = type_registry::get_type_entry(&source_fqn) {
                    let variant_fqn = format!("{}::{}", variant_ns, member_local);
                    if type_registry::get_type_entry(&variant_fqn).is_none() {
                        let mut variant_meta = meta.clone();
                        variant_meta.namespace = variant_ns.clone();
                        variant_meta.name = member_local.clone();
                        variant_meta.belongs_to = StdHashSet::new();
                        type_registry::register_product(variant_meta)?;
                    }
                }
                ensure_product_type_exists(
                    &variant_ns,
                    &member_local,
                    env.clone(),
                    !qualified_only,
                )?;
                let variant_fqn = format!("{}::{}", variant_ns, member_local);
                let fields_empty = match type_registry::get_type_entry(&variant_fqn) {
                    Some(TypeEntry::Product(meta)) => meta.fields.is_empty(),
                    _ => true,
                };
                let constructor = make_type_constructor(&variant_fqn, fields_empty, HashMap::new());
                let predicate = make_type_predicate(&variant_fqn);
                self.bind_enum_variant_qualified(
                    &variant_ns,
                    &member_local,
                    constructor,
                    predicate,
                );
                if !qualified_only {
                    register_variant_type_alias(
                        &current_ns,
                        &member_local,
                        &variant_fqn,
                        member_form.span,
                    )?;
                }
                member_idx += 1;
            }
        }
        let members = collect_enum_members(member_forms, &enum_ns, &enum_fqn)?;
        if members.is_empty() {
            return Err(span_runtime_error(
                name_form.span,
                "defenum resolved to empty member set",
            ));
        }
        let meta = SumMeta {
            namespace: enum_ns.clone(),
            name: enum_local.clone(),
            doc: doc.clone(),
            members: members.clone(),
            qualified_only,
        };
        let enum_fqn = format!("{}::{}", enum_ns, enum_local);
        type_registry::register_sum(meta)?;
        for member in &members {
            type_registry::add_product_membership(member, &enum_fqn);
        }
        let source = build_list_source("defenum", args, form_span);
        let symbol_info = symbol_meta::SymbolMeta {
            doc,
            meta: meta_value_map,
            source: Some(source),
        };
        symbol_meta::register(&enum_fqn, symbol_info);
        self.clear_imported_flag(&env, &enum_local);
        self.clear_imported_flag(&env, &format!("{}?", enum_local));
        Ok(Value::Symbol(enum_fqn))
    }

    fn eval_match(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "match expects target expression and clauses",
            ));
        }
        let target_val = self.eval(&args[0], env.clone())?;
        let mut clauses = Vec::new();
        let mut idx = 1;
        let current_ns = current_namespace_name(&env);
        while idx < args.len() {
            let pattern_form = args[idx].clone();
            idx += 1;
            if idx >= args.len() {
                return Err(span_runtime_error(
                    pattern_form.span,
                    "match clause missing result expression",
                ));
            }
            let mut guard = None;
            let mut as_binding = None;
            while idx + 1 < args.len() {
                match &args[idx].kind {
                    FormKind::Keyword(kw) if kw == "when" || kw == "if" => {
                        guard = Some(args[idx + 1].clone());
                        idx += 2;
                        continue;
                    }
                    FormKind::Keyword(kw) if kw == "as" => {
                        let sym = match &args[idx + 1].kind {
                            FormKind::Symbol(s) => s.clone(),
                            _ => {
                                return Err(span_runtime_error(
                                    args[idx + 1].span,
                                    ":as expects symbol binding",
                                ));
                            }
                        };
                        as_binding = Some(sym);
                        idx += 2;
                        continue;
                    }
                    _ => {}
                }
                break;
            }
            if idx >= args.len() {
                return Err(span_runtime_error(
                    pattern_form.span,
                    "match clause missing result expression",
                ));
            }
            let expr = args[idx].clone();
            idx += 1;
            let pattern = build_pattern(&pattern_form, current_ns.as_deref(), pattern_form.span)?;
            clauses.push(MatchClause {
                pattern,
                guard,
                as_binding,
                expr,
            });
        }
        if clauses.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "match requires at least one clause",
            ));
        }
        for clause in clauses {
            let mut bindings = Vec::new();
            if clause.pattern.matches(&target_val, &mut bindings) {
                let child = new_ref(Env::new_child(env.clone()));
                {
                    let mut writer = child.write().unwrap();
                    for (name, value) in bindings {
                        writer.set(&name, value);
                    }
                    if let Some(as_name) = &clause.as_binding {
                        writer.set(as_name, target_val.clone());
                    }
                }
                if let Some(guard_form) = clause.guard {
                    let guard_val = self.eval(&guard_form, child.clone())?;
                    if !truthy(&guard_val) {
                        continue;
                    }
                }
                return self.eval(&clause.expr, child);
            }
        }
        Err(span_runtime_error(
            form_span,
            "no matching clause in match expression",
        ))
    }

    fn eval_describe(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(
                form_span,
                "describe expects exactly one argument",
            ));
        }
        let value = match &args[0].kind {
            FormKind::Symbol(sym) => {
                let current_ns = current_namespace_name(&env).unwrap_or_else(|| "user".into());
                let (ns, local) = resolve_type_name(&current_ns, sym, args[0].span)?;
                Value::Symbol(format!("{}::{}", ns, local))
            }
            _ => self.eval(&args[0], env.clone())?,
        };
        let current_ns = current_namespace_name(&env);
        type_registry::describe_value_arg(&value, current_ns.as_deref())
    }

    fn eval_infer_type(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(
                form_span,
                "infer-type expects exactly one argument",
            ));
        }
        let forms = vec![args[0].clone()];
        let result = crate::typing::infer::infer_forms_with_diags(&forms);
        let ty = result
            .forms
            .first()
            .map(|(_, ty)| ty.describe_pretty())
            .unwrap_or_else(|| "Any".to_string());
        if let Some(alias) = self.infer_type_alias_hint(&args[0], &env) {
            return Ok(Value::String(alias));
        }
        Ok(Value::String(ty))
    }

    fn infer_type_alias_hint(&self, form: &Form, env: &EnvRef) -> Option<String> {
        let current_ns = current_namespace_name(env);
        if let FormKind::List(items) = &form.kind {
            if let Some(FormKind::Symbol(sym)) = items.first().map(|form| &form.kind) {
                if let Some(fqn) = type_registry::resolve_alias_name(sym, current_ns.as_deref()) {
                    return Some(format_type_name_for_ns(&fqn, current_ns.as_deref()));
                }
            }
        }
        if let FormKind::Symbol(sym) = &form.kind {
            if let Some(value) = self.lookup_symbol_value(sym, env) {
                if let Some(alias) = alias_name_for_value(&value, current_ns.as_deref()) {
                    return Some(alias);
                }
                if let Some(tagged) = tagged_type_name_for_value(&value, current_ns.as_deref()) {
                    return Some(tagged);
                }
            }
        }
        None
    }

    fn eval_enum_members(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let described = self.eval_describe(args, env, form_span)?;
        match described {
            Value::Map(map) => Ok(map
                .get(&Key::Keyword("members".into()))
                .cloned()
                .unwrap_or(Value::Nil)),
            other => Ok(other),
        }
    }

    fn eval_doc(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(form_span, "doc expects one argument"));
        }
        let doc = match &args[0].kind {
            FormKind::Symbol(sym) => self.doc_for_name(sym, &env).or_else(|| {
                self.lookup_symbol_value(sym, &env)
                    .and_then(|value| doc_from_value(&value))
            }),
            FormKind::String(name) => self.doc_for_name(name, &env),
            _ => {
                let value = self.eval(&args[0], env.clone())?;
                match &value {
                    Value::Symbol(sym) | Value::String(sym) => self.doc_for_name(sym, &env),
                    _ => doc_from_value(&value),
                }
            }
        };
        Ok(doc.map(Value::String).unwrap_or(Value::Nil))
    }

    fn eval_nav(&self, args: &[Form], env: EnvRef, _form_span: Span) -> Result<Value, CloveError> {
        #[derive(Clone)]
        enum NavQuery {
            Text(String),
            Regex(RegexValue),
            Other(Value),
        }

        #[derive(Default)]
        struct NavOptions {
            topics: StdHashSet<String>,
            include_internal: bool,
        }

        #[derive(Clone, Copy)]
        enum ContextKind {
            Set,
            Map,
            Vec,
            String,
            NumberInt,
            NumberFloat,
            Fn,
            Chan,
            Time,
        }

        struct ContextRule {
            exact: &'static [&'static str],
            prefixes: &'static [&'static str],
            suffixes: &'static [&'static str],
            namespaces: &'static [&'static str],
        }

        #[derive(Clone)]
        struct VarCandidate {
            ns: String,
            name: String,
            sym: String,
            value: Value,
        }

        struct VarEntry {
            sym: String,
            ns: String,
            name: String,
            aliases: Vec<String>,
            sig: Option<String>,
            doc: Option<String>,
            match_name: bool,
            match_doc: bool,
            topics: StdHashSet<String>,
            score: i64,
        }

        struct DocEntryOut {
            name: String,
            sig: Option<String>,
            doc: Option<String>,
            score: i64,
            match_name: bool,
            match_doc: bool,
        }

        struct NsEntry {
            name: String,
            score: i64,
        }

        let query_value = |query: &NavQuery| match query {
            NavQuery::Text(text) => Value::String(text.clone()),
            NavQuery::Regex(regex) => Value::Regex(regex.clone()),
            NavQuery::Other(value) => value.clone(),
        };
        let match_points = |query: &NavQuery, target: &str| -> Option<i64> {
            match query {
                NavQuery::Text(text) => {
                    if target == text {
                        Some(100)
                    } else if target.starts_with(text) {
                        Some(60)
                    } else if target.contains(text) {
                        Some(30)
                    } else {
                        None
                    }
                }
                NavQuery::Regex(regex) => regex.regex.is_match(target).then_some(20),
                NavQuery::Other(_) => None,
            }
        };
        let doc_has_token = |doc: &str, token: &str| -> bool {
            doc.split(|c: char| !c.is_alphanumeric())
                .any(|part| part == token)
        };
        let context_kinds_from_value = |value: &Value| -> Vec<ContextKind> {
            match value {
                Value::Set(_) | Value::SortedSet(_) => vec![ContextKind::Set],
                Value::Map(_) | Value::SortedMap(_) => vec![ContextKind::Map],
                Value::Vector(_) => vec![ContextKind::Vec],
                Value::String(_) => vec![ContextKind::String],
                Value::Int(_) => vec![ContextKind::NumberInt],
                Value::Float(_) => vec![ContextKind::NumberFloat],
                Value::Func(_)
                | Value::Partial { .. }
                | Value::Compose { .. }
                | Value::Lambda { .. }
                | Value::MultiLambda { .. } => vec![ContextKind::Fn],
                Value::Chan(_) => vec![ContextKind::Chan],
                Value::Duration(_) => vec![ContextKind::Time],
                _ => Vec::new(),
            }
        };
        let context_rule_for = |kind: ContextKind| -> ContextRule {
            const EMPTY: &[&str] = &[];
            match kind {
                ContextKind::Set => ContextRule {
                    exact: &[
                        "set?",
                        "set",
                        "hash-set",
                        "sorted-set",
                        "sorted-set-by",
                        "sorted-set-desc",
                        "conj",
                        "disj",
                        "includes?",
                        "contains?",
                        "union",
                        "intersection",
                        "difference",
                        "subset?",
                        "superset?",
                    ],
                    prefixes: &["sorted-set"],
                    suffixes: EMPTY,
                    namespaces: &["set"],
                },
                ContextKind::Map => ContextRule {
                    exact: &[
                        "map?",
                        "get",
                        "find",
                        "assoc",
                        "dissoc",
                        "keys",
                        "vals",
                        "merge",
                        "contains?",
                        "includes?",
                        "update",
                        "update-in",
                        "get-in",
                        "assoc-in",
                    ],
                    prefixes: EMPTY,
                    suffixes: &["-in"],
                    namespaces: EMPTY,
                },
                ContextKind::Vec => ContextRule {
                    exact: &[
                        "vector", "vec", "vector?", "nth", "get", "subvec", "conj", "pop", "peek",
                        "count",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: EMPTY,
                },
                ContextKind::String => ContextRule {
                    exact: &[
                        "str",
                        "pr-str",
                        "split",
                        "join",
                        "trim",
                        "replace",
                        "starts-with?",
                        "ends-with?",
                        "lower-case",
                        "upper-case",
                        "re-find",
                        "re-matches",
                        "re-seq",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: &["string"],
                },
                ContextKind::NumberInt => ContextRule {
                    exact: &[
                        "+",
                        "-",
                        "*",
                        "/",
                        "inc",
                        "dec",
                        "mod",
                        "compare",
                        "=",
                        "<",
                        ">",
                        "<=",
                        ">=",
                        "int",
                        "float",
                        "bit-and",
                        "bit-or",
                        "bit-xor",
                        "bit-set",
                        "bit-clear",
                        "bit-shift-left",
                        "bit-shift-right",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: EMPTY,
                },
                ContextKind::NumberFloat => ContextRule {
                    exact: &[
                        "+", "-", "*", "/", "inc", "dec", "mod", "compare", "=", "<", ">", "<=",
                        ">=", "int", "float", "rand",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: EMPTY,
                },
                ContextKind::Fn => ContextRule {
                    exact: &[
                        "apply", "comp", "partial", "map", "filter", "reduce", "keep", "some",
                        "every?",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: EMPTY,
                },
                ContextKind::Chan => ContextRule {
                    exact: &[
                        "chan",
                        "chan-put!",
                        "put!",
                        "chan-take!",
                        "take!",
                        "chan-close!",
                        "select",
                    ],
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: &["async"],
                },
                ContextKind::Time => ContextRule {
                    exact: EMPTY,
                    prefixes: EMPTY,
                    suffixes: EMPTY,
                    namespaces: &["time"],
                },
            }
        };
        let infer_topics_from_namespace = |ns: &str| -> StdHashSet<String> {
            let mut topics = StdHashSet::new();
            let topic = match ns {
                "set" => Some("set"),
                "string" => Some("string"),
                "path" => Some("path"),
                "io" => Some("io"),
                "fs" => Some("fs"),
                "json" => Some("json"),
                "ini" => Some("ini"),
                "http" => Some("http"),
                "walk" => Some("walk"),
                "async" => Some("async"),
                "time" => Some("time"),
                "process" => Some("process"),
                "log" => Some("log"),
                "env" => Some("env"),
                "pprint" => Some("pprint"),
                "core" => Some("core"),
                _ => None,
            };
            if let Some(topic) = topic {
                topics.insert(topic.to_string());
            }
            topics
        };
        let infer_topics_from_core_name = |name: &str| -> StdHashSet<String> {
            let mut topics = StdHashSet::new();
            if matches!(
                name,
                "disj"
                    | "hash-set"
                    | "set"
                    | "set?"
                    | "sorted-set"
                    | "sorted-set-by"
                    | "sorted-set-desc"
            ) {
                topics.insert("set".to_string());
            }
            topics
        };
        let infer_topics_from_doc = |doc: &str| -> StdHashSet<String> {
            let mut topics = StdHashSet::new();
            let doc_lower = doc.to_ascii_lowercase();
            if doc_lower.contains("relation")
                || doc_lower.contains("xrel")
                || doc_lower.contains("yrel")
                || doc_lower.contains("set of maps")
                || doc_lower.contains("keys ks")
            {
                topics.insert("rel".to_string());
            }
            if doc_lower.contains("filesystem") || doc_lower.contains("file") {
                topics.insert("fs".to_string());
            }
            if doc_lower.contains("path") {
                topics.insert("path".to_string());
            }
            if doc_has_token(&doc_lower, "map") || doc_has_token(&doc_lower, "maps") {
                topics.insert("map".to_string());
            }
            if doc_has_token(&doc_lower, "vector")
                || doc_has_token(&doc_lower, "vectors")
                || doc_has_token(&doc_lower, "vec")
            {
                topics.insert("vec".to_string());
            }
            topics
        };
        let infer_topics_from_value = |value: &Value| -> StdHashSet<String> {
            let mut topics = StdHashSet::new();
            match value {
                Value::Set(_) | Value::SortedSet(_) => {
                    topics.insert("set".to_string());
                }
                Value::Map(_) | Value::SortedMap(_) => {
                    topics.insert("map".to_string());
                }
                Value::Vector(_) => {
                    topics.insert("vec".to_string());
                }
                Value::String(_) => {
                    topics.insert("string".to_string());
                }
                Value::Duration(_) => {
                    topics.insert("time".to_string());
                }
                Value::Chan(_)
                | Value::Promise(_)
                | Value::Task(_)
                | Value::Future(_)
                | Value::Agent(_)
                | Value::Delay(_) => {
                    topics.insert("async".to_string());
                }
                _ => {}
            }
            topics
        };
        let topic_hint_from_text = |text: &str| -> StdHashSet<String> {
            let mut topics = StdHashSet::new();
            let lower = text.to_ascii_lowercase();
            if lower == "set"
                || lower == "sets"
                || lower.starts_with("set-")
                || lower.contains("set-")
            {
                topics.insert("set".to_string());
            }
            for (topic, needle) in [
                ("string", "string"),
                ("path", "path"),
                ("io", "io"),
                ("fs", "fs"),
                ("json", "json"),
                ("ini", "ini"),
                ("http", "http"),
                ("walk", "walk"),
                ("async", "async"),
                ("time", "time"),
                ("process", "process"),
                ("log", "log"),
                ("env", "env"),
                ("pprint", "pprint"),
                ("core", "core"),
                ("map", "map"),
                ("vec", "vec"),
            ] {
                if lower == needle || lower.starts_with(&format!("{needle}-")) {
                    topics.insert(topic.to_string());
                }
            }
            topics
        };
        let topics_to_value = |topics: &StdHashSet<String>| -> Value {
            let mut set = HashSet::new();
            let mut sorted = topics.iter().cloned().collect::<Vec<_>>();
            sorted.sort();
            for topic in sorted {
                set.insert(Value::Symbol(format!(":{}", topic)));
            }
            Value::Set(set)
        };
        let doc_summary = |doc: &str| -> String {
            let line = doc
                .lines()
                .find(|line| !line.trim().is_empty())
                .unwrap_or("");
            line.split_whitespace().collect::<Vec<_>>().join(" ")
        };
        let alias_list_for = |canonical_sym: &str, group: &[VarCandidate]| -> Vec<String> {
            let mut aliases = builtin_aliases(canonical_sym);
            aliases.extend(group.iter().map(|cand| cand.sym.clone()));
            aliases.sort();
            aliases.dedup();
            aliases.retain(|alias| alias != canonical_sym);
            aliases
        };
        fn map_lookup<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
            match value {
                Value::Map(map) => map.iter().find_map(|(k, v)| match k {
                    Key::Keyword(name) if name == key => Some(v),
                    _ => None,
                }),
                Value::SortedMap(map) => map.entries.iter().find_map(|(k, v)| match k {
                    Key::Keyword(name) if name == key => Some(v),
                    _ => None,
                }),
                _ => None,
            }
        }
        let parse_topics_value =
            |value: &Value, span: Span| -> Result<StdHashSet<String>, CloveError> {
                let mut topics = StdHashSet::new();
                let mut push_topic = |value: &Value| -> Result<(), CloveError> {
                    match value {
                        Value::Symbol(sym) | Value::String(sym) => {
                            let trimmed = sym.trim_start_matches(':');
                            if trimmed.is_empty() {
                                return Err(span_runtime_error(
                                    span,
                                    "nav :topics expects non-empty keywords",
                                ));
                            }
                            topics.insert(trimmed.to_string());
                            Ok(())
                        }
                        other => Err(span_runtime_error(
                            span,
                            &format!(
                                "nav :topics expects keyword values, got {}",
                                other.type_name()
                            ),
                        )),
                    }
                };
                match value {
                    Value::Nil => Ok(topics),
                    Value::Symbol(_) | Value::String(_) => {
                        push_topic(value)?;
                        Ok(topics)
                    }
                    Value::Set(items) => {
                        for item in items {
                            push_topic(item)?;
                        }
                        Ok(topics)
                    }
                    Value::SortedSet(items) => {
                        for item in &items.entries {
                            push_topic(item)?;
                        }
                        Ok(topics)
                    }
                    Value::Vector(items) => {
                        for item in items {
                            push_topic(item)?;
                        }
                        Ok(topics)
                    }
                    other => Err(span_runtime_error(
                        span,
                        &format!(
                            "nav :topics expects set of keywords, got {}",
                            other.type_name()
                        ),
                    )),
                }
            };
        let parse_nav_options =
            |value: &Value, span: Span| -> Result<Option<NavOptions>, CloveError> {
                match value {
                    Value::Map(_) | Value::SortedMap(_) => {
                        let mut opts = NavOptions::default();
                        if let Some(value) = map_lookup(value, "topics") {
                            opts.topics = parse_topics_value(value, span)?;
                        }
                        if let Some(value) = map_lookup(value, "include-internal?") {
                            match value {
                                Value::Bool(flag) => opts.include_internal = *flag,
                                other => {
                                    return Err(span_runtime_error(
                                        span,
                                        &format!(
                                            "nav :include-internal? expects bool, got {}",
                                            other.type_name()
                                        ),
                                    ))
                                }
                            }
                        }
                        Ok(Some(opts))
                    }
                    _ => Ok(None),
                }
            };
        let is_internal_var = |name: &str, sym: &str| -> bool {
            name.starts_with("__clove_") || name == CURRENT_NS_KEY || sym.contains("::__clove_")
        };

        let mut namespace_names = Vec::new();
        let mut var_candidates = Vec::new();
        if let Some(store) = self.namespace_store_opt() {
            if let Ok(guard) = store.read() {
                namespace_names = guard.names();
                for ns in namespace_names.iter() {
                    if let Some(entry) = guard.get(ns) {
                        for (name, value) in entry.public_exports.iter() {
                            var_candidates.push(VarCandidate {
                                ns: ns.clone(),
                                name: name.clone(),
                                sym: format!("{ns}::{name}"),
                                value: value.clone(),
                            });
                        }
                    }
                }
            }
        }

        if args.is_empty() {
            namespace_names.sort();
            let ns_vec = namespace_names
                .into_iter()
                .map(|ns| {
                    let mut map = HashMap::new();
                    map.insert(Key::Keyword("ns".into()), Value::Symbol(ns));
                    map.insert(
                        Key::Keyword("match".into()),
                        Value::Vector(Vector::from(vec![Value::Symbol(":name".into())])),
                    );
                    Value::Map(map)
                })
                .collect::<Vec<_>>();
            let mut map = HashMap::new();
            map.insert(Key::Keyword("kind".into()), Value::Symbol(":nav".into()));
            map.insert(Key::Keyword("query".into()), Value::String(String::new()));
            map.insert(
                Key::Keyword("ns".into()),
                Value::Vector(Vector::from(ns_vec)),
            );
            map.insert(Key::Keyword("var".into()), Value::Vector(Vector::new()));
            map.insert(Key::Keyword("doc".into()), Value::Vector(Vector::new()));
            return Ok(Value::Map(map));
        }

        let query_value_raw = self.eval(&args[0], env.clone())?;
        let query = match &query_value_raw {
            Value::Symbol(s) | Value::String(s) => NavQuery::Text(s.clone()),
            Value::Regex(r) => NavQuery::Regex(r.clone()),
            other => NavQuery::Other(other.clone()),
        };
        let is_empty_text_query = matches!(query, NavQuery::Text(ref text) if text.is_empty());
        let is_text_query =
            matches!(query, NavQuery::Text(_) | NavQuery::Regex(_)) && !is_empty_text_query;

        let mut opts = NavOptions::default();
        let mut selector_values = Vec::new();
        let mut selector_spans = Vec::new();
        if args.len() > 1 {
            for selector in &args[1..] {
                selector_values.push(self.eval(selector, env.clone())?);
                selector_spans.push(selector.span);
            }
            if let Some(last_value) = selector_values.last() {
                let last_span = *selector_spans.last().unwrap();
                if let Some(parsed) = parse_nav_options(last_value, last_span)? {
                    opts = parsed;
                    selector_values.pop();
                    selector_spans.pop();
                }
            }
        }

        let mut select_ns = false;
        let mut select_var = false;
        let mut select_doc = false;
        if args.len() == 1 || selector_values.is_empty() {
            select_ns = true;
            select_var = true;
            select_doc = true;
        } else {
            for (value, span) in selector_values.iter().zip(selector_spans.iter()) {
                let raw = match value {
                    Value::Symbol(sym) => sym.clone(),
                    other => {
                        return Err(span_runtime_error(
                            *span,
                            &format!(
                                "nav selector expects keyword :ns/:var/:doc, got {}",
                                other.type_name()
                            ),
                        ))
                    }
                };
                let name = raw.strip_prefix(':').ok_or_else(|| {
                    span_runtime_error(*span, "nav selector expects keyword :ns/:var/:doc")
                })?;
                match name {
                    "ns" => select_ns = true,
                    "var" => select_var = true,
                    "doc" => select_doc = true,
                    _ => {
                        return Err(span_runtime_error(
                            *span,
                            "nav selector expects :ns, :var, or :doc",
                        ))
                    }
                }
            }
        }

        if !opts.include_internal {
            var_candidates.retain(|cand| !is_internal_var(&cand.name, &cand.sym));
        }

        let mut preferred_topics = StdHashSet::new();
        if let NavQuery::Text(text) = &query {
            preferred_topics.extend(topic_hint_from_text(text));
        }
        if !is_text_query {
            preferred_topics.extend(infer_topics_from_value(&query_value_raw));
        }
        preferred_topics.extend(opts.topics.iter().cloned());

        let context_kinds = if is_text_query {
            Vec::new()
        } else {
            context_kinds_from_value(&query_value_raw)
        };

        let mut var_groups: StdHashMap<String, Vec<VarCandidate>> = StdHashMap::new();
        let mut var_by_name: StdHashMap<String, Vec<String>> = StdHashMap::new();
        for cand in &var_candidates {
            let canonical = builtin_alias_target(&cand.sym)
                .map(|name| name.to_string())
                .unwrap_or_else(|| cand.sym.clone());
            var_groups
                .entry(canonical.clone())
                .or_default()
                .push(cand.clone());
            var_by_name
                .entry(cand.name.clone())
                .or_default()
                .push(canonical);
        }
        for names in var_by_name.values_mut() {
            names.sort();
            names.dedup();
        }

        let apply_text_boosts =
            |points: i64, topics: &StdHashSet<String>, ns: &str, name: &str| -> i64 {
                let mut score = points;
                if !preferred_topics.is_empty() && !topics.is_empty() {
                    if !topics.is_disjoint(&preferred_topics) {
                        score += 50;
                    }
                    if preferred_topics.contains(ns) {
                        score += 30;
                    }
                }
                if preferred_topics.contains("set") && !topics.contains("set") {
                    if name == "set!" || name.ends_with("-set!") {
                        score -= 30;
                    }
                }
                score
            };
        let context_boost_for = |name: &str, ns: &str| -> i64 {
            let mut best = 0;
            for kind in &context_kinds {
                let rule = context_rule_for(*kind);
                let mut score = 0;
                if rule.exact.iter().any(|target| *target == name) {
                    score = score.max(80);
                }
                if rule.prefixes.iter().any(|prefix| name.starts_with(prefix)) {
                    score = score.max(40);
                }
                if rule.suffixes.iter().any(|suffix| name.ends_with(suffix)) {
                    score = score.max(40);
                }
                if rule.namespaces.iter().any(|target| *target == ns) {
                    score += 20;
                }
                best = best.max(score);
            }
            best
        };
        let score_context = |topics: &StdHashSet<String>, ns: &str, name: &str| -> i64 {
            let mut score = context_boost_for(name, ns);
            if !preferred_topics.is_empty() && !topics.is_empty() {
                if !topics.is_disjoint(&preferred_topics) {
                    score += 100;
                }
                if preferred_topics.contains(ns) {
                    score += 20;
                }
            }
            score
        };

        let mut ns_entries = Vec::new();
        if select_ns {
            for ns in namespace_names {
                if is_text_query {
                    if let Some(score) = match_points(&query, &ns) {
                        ns_entries.push(NsEntry { name: ns, score });
                    }
                } else if preferred_topics.contains(ns.as_str()) {
                    ns_entries.push(NsEntry {
                        name: ns,
                        score: 20,
                    });
                }
            }
        }

        let mut var_entries: StdHashMap<String, VarEntry> = StdHashMap::new();
        if select_var {
            for (canonical_sym, group) in &var_groups {
                let canonical_cand = group
                    .iter()
                    .find(|cand| cand.sym == *canonical_sym)
                    .unwrap_or(&group[0]);
                let mut sig = None;
                let mut doc_full = None;
                if let Some(entry) = crate::docs::find_doc_entry(canonical_sym)
                    .or_else(|| crate::docs::find_doc_entry(&canonical_cand.name))
                {
                    sig = entry.signature.clone();
                    doc_full = normalize_doc(entry.doc.clone());
                }
                if doc_full.is_none() {
                    doc_full = doc_from_value(&canonical_cand.value);
                }
                let doc = doc_full
                    .as_deref()
                    .map(doc_summary)
                    .filter(|text| !text.is_empty());
                let mut topics = infer_topics_from_namespace(&canonical_cand.ns);
                if canonical_cand.ns == "core" {
                    topics.extend(infer_topics_from_core_name(&canonical_cand.name));
                }
                if let Some(doc_text) = doc_full.as_deref() {
                    topics.extend(infer_topics_from_doc(doc_text));
                }
                let aliases = alias_list_for(canonical_sym, group);

                if is_text_query {
                    let mut canonical_points = None;
                    let mut alias_match = false;
                    let mut match_name = false;
                    for cand in group {
                        let mut cand_points = None;
                        if let Some(points) = match_points(&query, &cand.name) {
                            cand_points =
                                Some(cand_points.map_or(points, |cur: i64| cur.max(points)));
                        }
                        if let Some(points) = match_points(&query, &cand.sym) {
                            cand_points =
                                Some(cand_points.map_or(points, |cur: i64| cur.max(points)));
                        }
                        if let Some(points) = cand_points {
                            match_name = true;
                            if cand.sym == *canonical_sym {
                                canonical_points = Some(
                                    canonical_points.map_or(points, |cur: i64| cur.max(points)),
                                );
                            } else {
                                alias_match = true;
                            }
                        }
                    }
                    let base_points = if let Some(points) = canonical_points {
                        points
                    } else if alias_match {
                        20
                    } else {
                        continue;
                    };
                    let score = apply_text_boosts(
                        base_points,
                        &topics,
                        &canonical_cand.ns,
                        &canonical_cand.name,
                    );
                    var_entries.insert(
                        canonical_sym.clone(),
                        VarEntry {
                            sym: canonical_sym.clone(),
                            ns: canonical_cand.ns.clone(),
                            name: canonical_cand.name.clone(),
                            aliases,
                            sig,
                            doc,
                            match_name,
                            match_doc: false,
                            topics,
                            score,
                        },
                    );
                } else {
                    let score = score_context(&topics, &canonical_cand.ns, &canonical_cand.name);
                    if score == 0 {
                        continue;
                    }
                    var_entries.insert(
                        canonical_sym.clone(),
                        VarEntry {
                            sym: canonical_sym.clone(),
                            ns: canonical_cand.ns.clone(),
                            name: canonical_cand.name.clone(),
                            aliases,
                            sig,
                            doc,
                            match_name: false,
                            match_doc: false,
                            topics,
                            score,
                        },
                    );
                }
            }
        }

        let mut doc_entries = Vec::new();
        if select_doc {
            for doc_entry in crate::docs::doc_entries() {
                if !opts.include_internal
                    && (doc_entry.name.starts_with("__clove_")
                        || doc_entry.canonical.contains("::__clove_"))
                {
                    continue;
                }
                let doc_text = normalize_doc(doc_entry.doc.clone());
                let summary = doc_text
                    .as_deref()
                    .map(doc_summary)
                    .filter(|text| !text.is_empty());
                let mut match_name = false;
                let mut match_doc = false;
                let mut score = 0;
                if is_text_query {
                    let name_points = match_points(&query, &doc_entry.name).or_else(|| {
                        if doc_entry.canonical != doc_entry.name {
                            match_points(&query, &doc_entry.canonical)
                        } else {
                            None
                        }
                    });
                    let doc_points = doc_text
                        .as_deref()
                        .and_then(|text| match_points(&query, text))
                        .map(|_| 10);
                    if let Some(points) = name_points {
                        match_name = true;
                        score = score.max(points);
                    }
                    if let Some(points) = doc_points {
                        match_doc = true;
                        score = score.max(points);
                    }
                    if score == 0 {
                        continue;
                    }
                } else {
                    let (ns_name, local_name) = doc_entry
                        .canonical
                        .rsplit_once("::")
                        .map(|(ns, local)| (ns, local))
                        .unwrap_or(("", doc_entry.canonical.as_str()));
                    let mut topics = infer_topics_from_namespace(ns_name);
                    if let Some(doc_text) = doc_text.as_deref() {
                        topics.extend(infer_topics_from_doc(doc_text));
                    }
                    score = score_context(&topics, ns_name, local_name);
                    if score == 0 {
                        continue;
                    }
                }

                let mut merge_sym = None;
                if select_var {
                    if doc_entry.canonical.contains("::") {
                        if var_groups.contains_key(&doc_entry.canonical) {
                            merge_sym = Some(doc_entry.canonical.clone());
                        }
                    } else if let Some(cands) = var_by_name.get(&doc_entry.canonical) {
                        if cands.len() == 1 {
                            merge_sym = Some(cands[0].clone());
                        }
                    }
                }
                if let Some(sym) = merge_sym {
                    if let Some(group) = var_groups.get(&sym) {
                        let canonical_cand = group
                            .iter()
                            .find(|cand| cand.sym == sym)
                            .unwrap_or(&group[0]);
                        let mut topics = infer_topics_from_namespace(&canonical_cand.ns);
                        if canonical_cand.ns == "core" {
                            topics.extend(infer_topics_from_core_name(&canonical_cand.name));
                        }
                        if let Some(doc_text) = doc_text.as_deref() {
                            topics.extend(infer_topics_from_doc(doc_text));
                        }
                        let adjusted_score = if is_text_query {
                            apply_text_boosts(
                                score,
                                &topics,
                                &canonical_cand.ns,
                                &canonical_cand.name,
                            )
                        } else {
                            score
                        };
                        let var_entry =
                            var_entries.entry(sym.clone()).or_insert_with(|| VarEntry {
                                sym: sym.clone(),
                                ns: canonical_cand.ns.clone(),
                                name: canonical_cand.name.clone(),
                                aliases: alias_list_for(&sym, group),
                                sig: doc_entry.signature.clone(),
                                doc: summary.clone(),
                                match_name,
                                match_doc,
                                topics: topics.clone(),
                                score: adjusted_score,
                            });
                        var_entry.match_name |= match_name;
                        var_entry.match_doc |= match_doc;
                        var_entry.score = var_entry.score.max(adjusted_score);
                        if let Some(summary) = summary.clone() {
                            var_entry.doc = Some(summary);
                        }
                        if var_entry.sig.is_none() {
                            var_entry.sig = doc_entry.signature.clone();
                        }
                        continue;
                    }
                }
                doc_entries.push(DocEntryOut {
                    name: doc_entry.name.clone(),
                    sig: doc_entry.signature.clone(),
                    doc: summary,
                    score,
                    match_name,
                    match_doc,
                });
            }
        }

        ns_entries.sort_by(|a, b| b.score.cmp(&a.score).then_with(|| a.name.cmp(&b.name)));
        let mut var_entries: Vec<VarEntry> = var_entries.into_values().collect();
        var_entries.sort_by(|a, b| {
            let score_cmp = b.score.cmp(&a.score);
            if score_cmp != std::cmp::Ordering::Equal {
                return score_cmp;
            }
            let a_intersects = !preferred_topics.is_disjoint(&a.topics);
            let b_intersects = !preferred_topics.is_disjoint(&b.topics);
            if a_intersects != b_intersects {
                return b_intersects.cmp(&a_intersects);
            }
            let a_ns_match = preferred_topics.contains(a.ns.as_str());
            let b_ns_match = preferred_topics.contains(b.ns.as_str());
            if a_ns_match != b_ns_match {
                return b_ns_match.cmp(&a_ns_match);
            }
            a.sym.cmp(&b.sym)
        });
        doc_entries.sort_by(|a, b| b.score.cmp(&a.score).then_with(|| a.name.cmp(&b.name)));

        let ns_vec = ns_entries
            .into_iter()
            .map(|entry| {
                let mut map = HashMap::new();
                map.insert(Key::Keyword("ns".into()), Value::Symbol(entry.name));
                map.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(vec![Value::Symbol(":name".into())])),
                );
                Value::Map(map)
            })
            .collect::<Vec<_>>();

        let var_vec = var_entries
            .into_iter()
            .map(|entry| {
                let mut map = HashMap::new();
                map.insert(Key::Keyword("sym".into()), Value::Symbol(entry.sym));
                map.insert(Key::Keyword("ns".into()), Value::Symbol(entry.ns));
                map.insert(Key::Keyword("name".into()), Value::Symbol(entry.name));
                let aliases = entry
                    .aliases
                    .into_iter()
                    .map(Value::Symbol)
                    .collect::<Vec<_>>();
                map.insert(
                    Key::Keyword("aliases".into()),
                    Value::Vector(Vector::from(aliases)),
                );
                map.insert(
                    Key::Keyword("sig".into()),
                    entry.sig.map(Value::String).unwrap_or(Value::Nil),
                );
                map.insert(
                    Key::Keyword("doc".into()),
                    entry.doc.map(Value::String).unwrap_or(Value::Nil),
                );
                map.insert(
                    Key::Keyword("topics".into()),
                    topics_to_value(&entry.topics),
                );
                let mut matches = Vec::new();
                if entry.match_name {
                    matches.push(Value::Symbol(":name".into()));
                }
                if entry.match_doc {
                    matches.push(Value::Symbol(":doc".into()));
                }
                map.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(matches)),
                );
                Value::Map(map)
            })
            .collect::<Vec<_>>();

        let doc_vec = doc_entries
            .into_iter()
            .map(|entry| {
                let mut map = HashMap::new();
                map.insert(Key::Keyword("name".into()), Value::Symbol(entry.name));
                map.insert(
                    Key::Keyword("sig".into()),
                    entry.sig.map(Value::String).unwrap_or(Value::Nil),
                );
                map.insert(
                    Key::Keyword("doc".into()),
                    entry.doc.map(Value::String).unwrap_or(Value::Nil),
                );
                let mut matches = Vec::new();
                if entry.match_name {
                    matches.push(Value::Symbol(":name".into()));
                }
                if entry.match_doc {
                    matches.push(Value::Symbol(":doc".into()));
                }
                map.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(matches)),
                );
                Value::Map(map)
            })
            .collect::<Vec<_>>();

        let mut map = HashMap::new();
        map.insert(Key::Keyword("kind".into()), Value::Symbol(":nav".into()));
        map.insert(Key::Keyword("query".into()), query_value(&query));
        map.insert(
            Key::Keyword("ns".into()),
            Value::Vector(Vector::from(ns_vec)),
        );
        map.insert(
            Key::Keyword("var".into()),
            Value::Vector(Vector::from(var_vec)),
        );
        map.insert(
            Key::Keyword("doc".into()),
            Value::Vector(Vector::from(doc_vec)),
        );
        Ok(Value::Map(map))
    }

    fn eval_meta_form(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(form_span, "meta expects one argument"));
        }
        let meta = match &args[0].kind {
            FormKind::Symbol(sym) => self
                .meta_for_name(sym, &env)
                .map(Value::Map)
                .or_else(|| self.lookup_symbol_value(sym, &env).map(|v| meta_lookup(&v))),
            FormKind::String(name) => self.meta_for_name(name, &env).map(Value::Map),
            _ => {
                let value = self.eval(&args[0], env.clone())?;
                Some(meta_lookup(&value))
            }
        };
        Ok(meta.unwrap_or(Value::Nil))
    }

    fn eval_source_form(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(form_span, "source expects one argument"));
        }
        let source = match &args[0].kind {
            FormKind::Symbol(sym) => self.source_for_name(sym, &env).or_else(|| {
                self.lookup_symbol_value(sym, &env)
                    .and_then(|value| source_from_value(&value))
            }),
            FormKind::String(name) => self.source_for_name(name, &env),
            _ => {
                let value = self.eval(&args[0], env.clone())?;
                match &value {
                    Value::Symbol(sym) | Value::String(sym) => self.source_for_name(sym, &env),
                    _ => source_from_value(&value),
                }
            }
        };
        Ok(source.map(Value::String).unwrap_or(Value::Nil))
    }

    fn eval_current_ns(&self, env: EnvRef) -> Result<Value, CloveError> {
        let ns = current_namespace_name(&env).unwrap_or_else(|| "user".into());
        Ok(Value::Symbol(ns))
    }

    fn enum_variant_lookup(&self, name: &str, env: &EnvRef) -> Option<EnumVariantLookup> {
        let current_ns =
            current_namespace_name(env).or_else(|| self.default_namespace.read().unwrap().clone());
        enum_variant_lookup_with_ns(name, current_ns.as_deref())
    }

    fn symbol_lookup_keys(&self, name: &str, env: &EnvRef) -> Vec<String> {
        let canonical = canonical_symbol_name(name).into_owned();
        let mut keys = Vec::new();
        if name.contains("::") {
            keys.push(canonical.clone());
        } else if let Some(ns) =
            current_namespace_name(env).or_else(|| self.default_namespace.read().unwrap().clone())
        {
            keys.push(format!("{ns}::{canonical}"));
        }
        if !keys.iter().any(|k| k == &canonical) {
            keys.push(canonical);
        }
        keys
    }

    fn lookup_symbol_meta(&self, name: &str, env: &EnvRef) -> Option<symbol_meta::SymbolMeta> {
        for key in self.symbol_lookup_keys(name, env) {
            if let Some(meta) = symbol_meta::get(&key) {
                return Some(meta);
            }
        }
        None
    }

    fn doc_for_name(&self, name: &str, env: &EnvRef) -> Option<String> {
        let mut doc_override = None;
        if let Some(meta) = self.lookup_symbol_meta(name, env) {
            if let Some(doc) = normalize_doc(meta.doc) {
                doc_override = Some(doc);
            }
        }
        if doc_override.is_none() {
            for key in self.symbol_lookup_keys(name, env) {
                if let Some(meta) = fn_meta::get(&key) {
                    if let Some(doc) = normalize_doc(meta.doc) {
                        doc_override = Some(doc);
                        break;
                    }
                }
            }
        }
        if doc_override.is_none() {
            for key in self.symbol_lookup_keys(name, env) {
                if let Some(entry) = type_registry::get_type_entry(&key) {
                    let doc = match entry {
                        TypeEntry::Primitive(meta) => meta.doc,
                        TypeEntry::Product(meta) => meta.doc,
                        TypeEntry::Sum(meta) => meta.doc,
                        TypeEntry::Alias(meta) => meta.doc,
                    };
                    if let Some(doc) = normalize_doc(doc) {
                        doc_override = Some(doc);
                        break;
                    }
                }
            }
        }
        if let Some(entry) = crate::docs::find_doc_entry(name) {
            let mut combined = entry.clone();
            let needs_doc = combined
                .doc
                .as_ref()
                .map(|text| text.trim().is_empty())
                .unwrap_or(true);
            if needs_doc {
                combined.doc = doc_override.clone();
            }
            if let Some(formatted) = crate::docs::format_doc_entry(&combined) {
                return Some(formatted);
            }
        }
        if let Some(doc) = doc_override {
            return Some(doc);
        }
        self.enum_variant_doc_for_name(name, env)
    }

    fn enum_variant_doc_for_name(&self, name: &str, env: &EnvRef) -> Option<String> {
        let variant = self.enum_variant_lookup(name, env)?;
        if let Some(meta) = symbol_meta::get(&variant.variant_fqn) {
            if let Some(doc) = normalize_doc(meta.doc) {
                return Some(doc);
            }
        }
        if let Some(entry) = type_registry::get_type_entry(&variant.variant_fqn) {
            let doc = match entry {
                TypeEntry::Primitive(meta) => meta.doc,
                TypeEntry::Product(meta) => meta.doc,
                TypeEntry::Sum(meta) => meta.doc,
                TypeEntry::Alias(meta) => meta.doc,
            };
            if let Some(doc) = normalize_doc(doc) {
                return Some(doc);
            }
        }
        Some(format!(
            "{} variant {}",
            variant.enum_raw, variant.variant_name
        ))
    }

    fn meta_for_name(&self, name: &str, env: &EnvRef) -> Option<HashMap<Key, Value>> {
        self.lookup_symbol_meta(name, env)
            .and_then(|meta| meta.meta)
    }

    fn source_for_name(&self, name: &str, env: &EnvRef) -> Option<String> {
        self.lookup_symbol_meta(name, env)
            .and_then(|meta| meta.source)
    }

    fn lookup_symbol_value(&self, name: &str, env: &EnvRef) -> Option<Value> {
        let lookup = canonical_symbol_name(name);
        if let Some((ns, local)) = split_namespace_parts(lookup.as_ref()) {
            if let Some(store) = self.namespace_store.as_ref() {
                if let Some(entry) = store.read().unwrap().get(&ns) {
                    if let Some(value) = entry.env().read().unwrap().get(&local) {
                        return Some(value);
                    }
                }
            }
            return env.read().unwrap().get(lookup.as_ref());
        }
        env.read().unwrap().get(lookup.as_ref())
    }

    fn fn_arg_positions_for_head(
        &self,
        head: &Form,
        arg_len: usize,
        env: &EnvRef,
    ) -> (StdHashSet<usize>, bool) {
        let sym = match &head.kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return (StdHashSet::new(), false),
        };

        if let Some(meta) = fn_meta::get(sym) {
            return (
                fn_arg_positions_from_meta_with_core_fallback(&meta, arg_len),
                true,
            );
        }

        if let Some(value) = self.lookup_symbol_value(sym, env) {
            match &value {
                Value::Func(func) => {
                    if let Some(name) = func.debug_name() {
                        if let Some(meta) = fn_meta::get(name) {
                            return (
                                fn_arg_positions_from_meta_with_core_fallback(&meta, arg_len),
                                true,
                            );
                        }
                    }
                }
                Value::Lambda {
                    name,
                    inferred_type,
                    ..
                }
                | Value::MultiLambda {
                    name,
                    inferred_type,
                    ..
                } => {
                    if let Some(name) = name {
                        if let Some(meta) = fn_meta::get(name) {
                            return (
                                fn_arg_positions_from_meta_with_core_fallback(&meta, arg_len),
                                true,
                            );
                        }
                    }
                    if let Some(kind) = inferred_type.as_ref() {
                        return (fn_arg_positions_from_type(kind, arg_len), true);
                    }
                }
                _ => {}
            }
        }

        (StdHashSet::new(), false)
    }

    fn fn_arg_positions_for_oop_method_call(
        &self,
        args: &[Form],
        env: &EnvRef,
        span: Span,
    ) -> Result<(StdHashSet<usize>, bool), CloveError> {
        if args.len() < 2 {
            return Ok((StdHashSet::new(), false));
        }
        let method_name = match &args[1].kind {
            FormKind::String(name) => name.as_str(),
            FormKind::Symbol(name) => name.trim_start_matches(':'),
            FormKind::Keyword(name) => name.as_str(),
            _ => return Ok((StdHashSet::new(), false)),
        };
        let call_args_len = args.len().saturating_sub(2);
        let method_head = Form::new(FormKind::Symbol(method_name.to_string()), args[1].span);
        let (fn_arg_positions, meta_known) =
            self.fn_arg_positions_for_head(&method_head, call_args_len + 1, env);
        if !meta_known {
            return Ok((StdHashSet::new(), false));
        }
        let subject_policy = self.resolve_subject_pos(method_name, call_args_len, span, env)?;
        let subject_pos = self.materialize_subject_pos(subject_policy, call_args_len, span)?;
        let mut mapped = StdHashSet::new();
        for pos in fn_arg_positions {
            let mapped_pos = if pos == subject_pos {
                1
            } else if pos < subject_pos {
                pos + 2
            } else {
                pos + 1
            };
            mapped.insert(mapped_pos);
        }
        Ok((mapped, true))
    }

    fn symbol_is_builtin(&self, sym: &str, env: &EnvRef) -> bool {
        let resolved = env.read().unwrap().get(sym);
        let builtin = self.global_env().read().unwrap().get(sym);
        match (resolved, builtin) {
            (Some(Value::Func(a)), Some(Value::Func(b))) => Arc::ptr_eq(&a, &b),
            _ => false,
        }
    }

    fn is_special_form_head(&self, sym: &str, env: &EnvRef) -> bool {
        let head_name = canonical_symbol_name(sym);
        match head_name.as_ref() {
            "doc" | "meta" | "source" | "describe" | "describe-type" | "enum-members" => {
                self.symbol_is_builtin(head_name.as_ref(), env)
            }
            "def" | "def-" | "defn" | "defn-" | "method" | "deftype" | "defenum" | "infer-type"
            | OOP_INDEX_SYM | OOP_SEG_SYM | "use-syntax" | "use" | "current-ns" | "let" | "if"
            | "do" | "where" | "fn" | "quote" | "set!" | "redef" | "-def" | "-defn" | "and"
            | "or" | "when" | "when-not" | "when-let" | "if-not" | "if-let" | "if-some"
            | "cond" | "condp" | "->" | "->>" | "as->" | "cond->" | "cond->>" | "some->"
            | "some->>" | "loop" | "for" | "while" | "doseq" | "each" | "dotimes" | "try"
            | "err" | "fin" | "throw" | "p" | "pvalues" | "comment" | "match" | "break"
            | "repl" | "debug" | "recur" | "with-redefs" | "with-dyn" | "go-loop"
            | "scope-loop" | "async::scope-loop" | "async-scope" | "async::scope" | "ns-map"
            | "nav" | "lookup" | "create-ns" | "refer" | "resolve" | "load-file"
            | "load-string" | "delay" | "doto" | "with-open" | "__range_literal" | "eval" => true,
            _ => false,
        }
    }

    fn list_is_call_like(&self, items: &[Form], env: &EnvRef) -> bool {
        let head = match items.first() {
            Some(head) => head,
            None => return false,
        };
        match &head.kind {
            FormKind::Symbol(sym) => {
                if self.call_wrappers.contains(sym) {
                    return true;
                }
                if self.engines.iter().any(|eng| eng.tag() == sym) {
                    return false;
                }
                !self.is_special_form_head(sym, env)
            }
            _ => true,
        }
    }

    fn form_contains_unresolved_placeholder(&self, form: &Form, env: &EnvRef) -> bool {
        self.form_contains_unresolved_placeholder_scoped(form, env, &StdHashSet::new())
    }

    fn form_contains_unresolved_placeholder_scoped(
        &self,
        form: &Form,
        env: &EnvRef,
        shadowed: &StdHashSet<String>,
    ) -> bool {
        match &form.kind {
            FormKind::Symbol(sym) => {
                !shadowed.contains(sym) && parse_question_placeholder(sym).is_some()
            }
            FormKind::List(items) => {
                if items.is_empty() {
                    return false;
                }
                if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                    if head == "let" {
                        let mut shadowed_body = shadowed.clone();
                        if let Some(bind_form) = items.get(1) {
                            if let FormKind::Vector(binds) = &bind_form.kind {
                                let mut idx = 0;
                                while idx < binds.len() {
                                    let name_form = &binds[idx];
                                    if let FormKind::Symbol(name) = &name_form.kind {
                                        shadowed_body.insert(name.clone());
                                    }
                                    if let Some(val_form) = binds.get(idx + 1) {
                                        if self.form_contains_unresolved_placeholder_scoped(
                                            val_form, env, shadowed,
                                        ) {
                                            return true;
                                        }
                                    }
                                    idx += 2;
                                }
                            } else if self.form_contains_unresolved_placeholder_scoped(
                                bind_form, env, shadowed,
                            ) {
                                return true;
                            }
                        }
                        return items.iter().skip(2).any(|body| {
                            self.form_contains_unresolved_placeholder_scoped(
                                body,
                                env,
                                &shadowed_body,
                            )
                        });
                    }
                    if head == "fn" || head == "method" {
                        let mut shadowed_body = shadowed.clone();
                        if let Some(params_form) = items.get(1) {
                            if let FormKind::Vector(params) = &params_form.kind {
                                for param in params {
                                    if let FormKind::Symbol(name) = &param.kind {
                                        shadowed_body.insert(name.clone());
                                    }
                                }
                            }
                        }
                        return items.iter().skip(2).any(|body| {
                            self.form_contains_unresolved_placeholder_scoped(
                                body,
                                env,
                                &shadowed_body,
                            )
                        });
                    }
                }

                let head_form = &items[0];
                if let FormKind::Symbol(sym) = &head_form.kind {
                    if parse_question_placeholder(sym).is_some() {
                        return true;
                    }
                    if self.call_wrappers.contains(sym) && items.len() >= 2 {
                        return self
                            .form_contains_unresolved_placeholder_scoped(&items[1], env, shadowed);
                    }
                }

                if self.list_is_call_like(items, env) {
                    if self.form_contains_unresolved_placeholder_scoped(head_form, env, shadowed) {
                        return true;
                    }
                    if let FormKind::Symbol(_) = &head_form.kind {
                        let (fn_arg_positions, meta_known) = self.fn_arg_positions_for_head(
                            head_form,
                            items.len().saturating_sub(1),
                            env,
                        );
                        if meta_known {
                            for (arg_idx, arg) in items.iter().enumerate().skip(1) {
                                let pos = arg_idx;
                                if fn_arg_positions.contains(&pos) {
                                    continue;
                                }
                                if self
                                    .form_contains_unresolved_placeholder_scoped(arg, env, shadowed)
                                {
                                    return true;
                                }
                            }
                            return false;
                        }
                    }
                    return items.iter().skip(1).any(|item| {
                        self.form_contains_unresolved_placeholder_scoped(item, env, shadowed)
                    });
                }

                items.iter().any(|item| {
                    self.form_contains_unresolved_placeholder_scoped(item, env, shadowed)
                })
            }
            FormKind::Vector(items) | FormKind::Set(items) => items
                .iter()
                .any(|item| self.form_contains_unresolved_placeholder_scoped(item, env, shadowed)),
            FormKind::Map(entries) => entries.iter().any(|entry| match entry {
                MapItem::KeyValue(k, v) => {
                    self.form_contains_unresolved_placeholder_scoped(k, env, shadowed)
                        || self.form_contains_unresolved_placeholder_scoped(v, env, shadowed)
                }
                MapItem::Spread(expr) => {
                    self.form_contains_unresolved_placeholder_scoped(expr, env, shadowed)
                }
            }),
            FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
                InterpolatedPart::Text(_) => false,
                InterpolatedPart::Expr(expr) => {
                    self.form_contains_unresolved_placeholder_scoped(expr, env, shadowed)
                }
            }),
            FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
                InterpolatedPart::Text(_) => false,
                InterpolatedPart::Expr(expr) => {
                    self.form_contains_unresolved_placeholder_scoped(expr, env, shadowed)
                }
            }),
            FormKind::ShortFn(_) => false,
            _ => false,
        }
    }

    fn eval_let(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "let expects binding vector and body",
            ));
        }
        let bindings_form = &args[0];
        let body = &args[1..];
        let bindings = self.parse_binding_pairs(bindings_form)?;
        let child = new_ref(Env::new_child(env));
        for (pat, value_form) in bindings {
            let val = {
                let _guard = push_stack_frame("let", Some(value_form.span));
                self.eval(&value_form, child.clone())?
            };
            let _guard = push_stack_frame("let", Some(pat.span));
            if let Err(err) = bind_pattern(child.clone(), &pat, val) {
                return Err(self.decorate_error(err, pat.span, child.clone()));
            }
        }
        self.eval_do(body, child)
    }

    fn eval_doto(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "doto expects target and forms",
            ));
        }
        let target = self.eval(&args[0], env.clone())?;
        for form in &args[1..] {
            match &form.kind {
                FormKind::List(items) => {
                    if items.is_empty() {
                        return Err(span_runtime_error(form.span, "doto form must not be empty"));
                    }
                    let head = self.eval(&items[0], env.clone())?;
                    let mut call_args = Vec::with_capacity(items.len());
                    call_args.push(target.clone());
                    let mut idx = 1;
                    while idx < items.len() {
                        if let Some((spread_value, span)) =
                            self.try_eval_spread_value(items, &mut idx, env.clone())?
                        {
                            spread_into_args(spread_value, &mut call_args, span)?;
                        } else {
                            call_args.push(self.eval(&items[idx], env.clone())?);
                            idx += 1;
                        }
                    }
                    call_callable(head, call_args)?;
                }
                FormKind::Symbol(_) => {
                    let head = self.eval(form, env.clone())?;
                    call_callable(head, vec![target.clone()])?;
                }
                _ => {
                    return Err(span_runtime_error(
                        form.span,
                        "doto expects symbol or list forms",
                    ))
                }
            }
        }
        Ok(target)
    }

    fn eval_loop(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "loop expects binding vector and body",
            ));
        }
        let bindings_form = &args[0];
        let body_forms = &args[1..];
        if body_forms.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "loop expects body expressions",
            ));
        }
        let bindings = self.parse_binding_pairs(bindings_form)?;
        let temp_env = new_ref(Env::new_child(env.clone()));
        let mut initial_vals = Vec::new();
        for (pat, value_form) in &bindings {
            let val = self.eval(value_form, temp_env.clone())?;
            bind_pattern(temp_env.clone(), pat, val.clone())?;
            initial_vals.push(val);
        }
        let loop_id = LOOP_COUNTER.fetch_add(1, Ordering::SeqCst);
        let mut current_vals = initial_vals;
        loop {
            let child = new_ref(Env::new_child(env.clone()));
            {
                for (pair, val) in bindings.iter().zip(current_vals.iter()) {
                    let pat = &pair.0;
                    bind_pattern(child.clone(), pat, val.clone())?;
                }
            }
            let guard = push_recur_context(RecurContext {
                id: loop_id,
                positional_count: bindings.len(),
                has_rest: false,
                kind: RecurKind::Loop,
                name: Some("loop".into()),
            });
            let result = self.eval_do(body_forms, child.clone());
            drop(guard);
            match result {
                Ok(val) => return Ok(val),
                Err(CloveError::RecurSignal { target, values }) if target == loop_id => {
                    if values.len() != bindings.len() {
                        return Err(span_runtime_error(
                            form_span,
                            format!(
                                "recur expected {} argument(s), got {}",
                                bindings.len(),
                                values.len()
                            ),
                        ));
                    }
                    current_vals = values;
                    continue;
                }
                Err(err) => return Err(err),
            }
        }
    }

    fn eval_recur(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        let ctx = current_recur_context().ok_or_else(|| {
            span_runtime_error(
                form_span,
                "recur is only allowed within loop or function bodies",
            )
        })?;
        let expected = ctx.positional_count;
        let target_label = describe_recur_target(&ctx);
        if !ctx.has_rest && args.len() != expected {
            return Err(span_runtime_error(
                form_span,
                format!(
                    "recur expects {} argument(s) for {}, got {}",
                    expected,
                    target_label,
                    args.len()
                ),
            ));
        }
        if ctx.has_rest && args.len() < expected {
            return Err(span_runtime_error(
                form_span,
                format!(
                    "recur expects at least {} argument(s) before rest for {}, got {}",
                    expected,
                    target_label,
                    args.len()
                ),
            ));
        }
        let mut values = Vec::with_capacity(if ctx.has_rest {
            expected + 1
        } else {
            args.len()
        });
        if ctx.has_rest {
            let (positional_args, rest_args) = args.split_at(expected);
            for arg in positional_args {
                values.push(self.eval(arg, env.clone())?);
            }
            let rest_list = if rest_args.is_empty() {
                Vector::new()
            } else if rest_args.len() == 1 {
                match self.eval(&rest_args[0], env.clone())? {
                    Value::List(items) => items,
                    Value::Vector(items) => items.into(),
                    other => vec![other].into(),
                }
            } else {
                let mut evaluated = Vec::with_capacity(rest_args.len());
                for arg in rest_args {
                    evaluated.push(self.eval(arg, env.clone())?);
                }
                evaluated.into()
            };
            values.push(Value::List(rest_list));
        } else {
            for arg in args {
                values.push(self.eval(arg, env.clone())?);
            }
        }
        Err(CloveError::RecurSignal {
            target: ctx.id,
            values,
        })
    }

    fn eval_if(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(span_runtime_error(form_span, "if expects 2 or 3 arguments"));
        }
        let test = self.eval(&args[0], env.clone())?;
        if truthy(&test) {
            self.eval(&args[1], env)
        } else if args.len() == 3 {
            self.eval(&args[2], env)
        } else {
            Ok(Value::Nil)
        }
    }

    pub(crate) fn eval_do(&self, forms: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        let err_fin = parse_err_fin_tail(forms, "a body")
            .map_err(|err| span_runtime_error(err.span, err.message))?;
        if let Some(tail) = err_fin {
            if tail.body.is_empty() {
                let span = tail
                    .err
                    .as_ref()
                    .map(|f| f.span)
                    .or_else(|| tail.fin.as_ref().map(|f| f.span))
                    .unwrap_or_else(|| forms.first().map(|f| f.span).unwrap());
                return Err(span_runtime_error(span, "do expects body before err/fin"));
            }
            let span = forms.first().map(|f| f.span).unwrap();
            return self.eval_try_err_fin(tail, None, env, span);
        }
        let mut last = Value::Nil;
        for form in forms {
            if is_where_form(form) {
                self.eval(form, env.clone())?;
                continue;
            }
            last = self.eval(form, env.clone())?;
        }
        Ok(last)
    }

    fn eval_where(&self, forms: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        let _ = self.eval_do(forms, env)?;
        Ok(Value::Nil)
    }

    fn eval_and(&self, args: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Ok(Value::Bool(true));
        }
        let mut last = Value::Bool(true);
        for form in args {
            last = self.eval(form, env.clone())?;
            if !truthy(&last) {
                return Ok(last);
            }
        }
        Ok(last)
    }

    fn eval_or(&self, args: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Ok(Value::Nil);
        }
        for form in args {
            let val = self.eval(form, env.clone())?;
            if truthy(&val) {
                return Ok(val);
            }
        }
        Ok(Value::Bool(false))
    }

    fn eval_when(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(form_span, "when expects test and body"));
        }
        let test = self.eval(&args[0], env.clone())?;
        if truthy(&test) {
            self.eval_do(&args[1..], env)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_when_not(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "when-not expects test and body",
            ));
        }
        let test = self.eval(&args[0], env.clone())?;
        if !truthy(&test) {
            self.eval_do(&args[1..], env)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_if_not(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(span_runtime_error(
                form_span,
                "if-not expects 2 or 3 arguments",
            ));
        }
        let test = self.eval(&args[0], env.clone())?;
        if !truthy(&test) {
            self.eval(&args[1], env)
        } else if args.len() == 3 {
            self.eval(&args[2], env)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_when_let(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "when-let expects binding vector and body",
            ));
        }
        let (pat, value_form) = self.parse_binding_pair(&args[0])?;
        let value = self.eval(&value_form, env.clone())?;
        if truthy(&value) {
            let child = bind_pattern_child(env.clone(), &pat, value)?;
            self.eval_do(&args[1..], child)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_if_let(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(span_runtime_error(
                form_span,
                "if-let expects binding vector, then, and optional else",
            ));
        }
        let (pat, value_form) = self.parse_binding_pair(&args[0])?;
        let value = self.eval(&value_form, env.clone())?;
        if truthy(&value) {
            let child = bind_pattern_child(env.clone(), &pat, value)?;
            self.eval(&args[1], child)
        } else if args.len() == 3 {
            self.eval(&args[2], env)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_if_some(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(span_runtime_error(
                form_span,
                "if-some expects binding vector, then, and optional else",
            ));
        }
        let (pat, value_form) = self.parse_binding_pair(&args[0])?;
        let value = self.eval(&value_form, env.clone())?;
        if is_some_value(&value) {
            let child = bind_pattern_child(env.clone(), &pat, value)?;
            self.eval(&args[1], child)
        } else if args.len() == 3 {
            self.eval(&args[2], env)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_cond(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() % 2 != 0 {
            return Err(span_runtime_error(
                form_span,
                "cond expects even number of test/expr pairs",
            ));
        }
        let mut idx = 0;
        while idx < args.len() {
            let test_form = &args[idx];
            let expr_form = &args[idx + 1];
            let is_else = matches!(test_form.kind, FormKind::Keyword(ref k) if k == "else")
                || matches!(test_form.kind, FormKind::Symbol(ref s) if s == ":else" || s == "_");
            if is_else {
                return self.eval(expr_form, env.clone());
            }
            let test_val = self.eval(test_form, env.clone())?;
            if truthy(&test_val) {
                return self.eval(expr_form, env.clone());
            }
            idx += 2;
        }
        Ok(Value::Nil)
    }

    fn eval_thread_first(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        self.eval_thread(args, env, ThreadStyle::First, false, form_span)
    }

    fn eval_thread_last(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        self.eval_thread(args, env, ThreadStyle::Last, false, form_span)
    }

    fn eval_some_thread(
        &self,
        args: &[Form],
        env: EnvRef,
        style: ThreadStyle,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        self.eval_thread(args, env, style, true, form_span)
    }

    fn eval_thread(
        &self,
        args: &[Form],
        env: EnvRef,
        style: ThreadStyle,
        stop_on_nil: bool,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "threading form expects initial value",
            ));
        }
        let mut acc = self.eval(&args[0], env.clone())?;
        if stop_on_nil && matches!(acc, Value::Nil) {
            return Ok(Value::Nil);
        }
        for step in &args[1..] {
            if stop_on_nil && matches!(acc, Value::Nil) {
                return Ok(Value::Nil);
            }
            acc = self.apply_thread_step(step, acc, env.clone(), style)?;
        }
        Ok(acc)
    }

    fn eval_use_syntax(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "use-syntax expects feature name and true/false",
            ));
        }
        if let Some(scope_form) = args
            .iter()
            .find(|arg| matches!(&arg.kind, FormKind::Keyword(sym) if sym == "scope"))
        {
            return Err(span_runtime_error(
                scope_form.span,
                "use-syntax only accepts (use <feature> true|false); :scope is removed",
            ));
        }
        if args.len() != 2 {
            return Err(span_runtime_error(
                form_span,
                "use-syntax expects feature name and true/false",
            ));
        }
        let feature_form = &args[0];
        let feature_sym = match &feature_form.kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => {
                return Err(span_runtime_error(
                    feature_form.span,
                    "use-syntax feature name must be symbol (e.g. dot-chain)",
                ))
            }
        };
        let feature = canonical_feature_toggle(feature_sym).ok_or_else(|| {
            span_runtime_error(
                feature_form.span,
                &format!("unknown feature '{}'", feature_sym),
            )
        })?;
        let enabled_value = self.eval(&args[1], env.clone())?;
        let enabled = match enabled_value {
            Value::Bool(flag) => flag,
            other => {
                return Err(span_runtime_error(
                    args[1].span,
                    format!("use-syntax expects true/false, got {}", other.type_name()),
                ))
            }
        };
        let pkg_id = self.package_id_for_env(&env);
        self.settings
            .assign_feature_toggle(feature, &pkg_id, enabled);
        let _ = RuntimeCtx::try_with_current(|ctx| {
            ctx.refresh_debug_stash_enabled();
            Ok(())
        });
        Ok(Value::Bool(enabled))
    }

    fn eval_as_thread(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "as-> expects value, name, and optional body",
            ));
        }
        let value = self.eval(&args[0], env.clone())?;
        let name = match &args[1].kind {
            FormKind::Symbol(s) => s.clone(),
            _ => return Err(span_runtime_error(args[1].span, "as-> name must be symbol")),
        };
        let body = &args[2..];
        if body.is_empty() {
            return Ok(value);
        }
        let env_with_binding = bind_single(env, &name, value);
        let mut current = Value::Nil;
        for form in body {
            if let FormKind::List(items) = &form.kind {
                let (head_form, arg_forms) = self.unpack_call_form(items);
                if let FormKind::Symbol(head) = &head_form.kind {
                    match head.as_str() {
                        "as" => {
                            if arg_forms.len() != 1 {
                                return Err(span_runtime_error(
                                    form.span,
                                    "dot as stage expects single symbol",
                                ));
                            }
                            let sym = match &arg_forms[0].kind {
                                FormKind::Symbol(sym) => sym,
                                _ => {
                                    return Err(span_runtime_error(
                                        form.span,
                                        "dot as stage expects single symbol",
                                    ));
                                }
                            };
                            let value = env_with_binding
                                .read()
                                .unwrap()
                                .get(&name)
                                .unwrap_or(Value::Nil);
                            current = value.clone();
                            env_with_binding.write().unwrap().set(sym, value);
                            continue;
                        }
                        "let" => {
                            if arg_forms.len() != 1 {
                                return Err(span_runtime_error(
                                    form.span,
                                    "dot let stage expects single symbol",
                                ));
                            }
                            let sym = match &arg_forms[0].kind {
                                FormKind::Symbol(sym) => sym,
                                _ => {
                                    return Err(span_runtime_error(
                                        form.span,
                                        "dot let stage expects single symbol",
                                    ));
                                }
                            };
                            if sym.starts_with('*') {
                                return Err(span_runtime_error(
                                    form.span,
                                    "dot let stage expects non-star symbol",
                                ));
                            }
                            let value = env_with_binding
                                .read()
                                .unwrap()
                                .get(&name)
                                .unwrap_or(Value::Nil);
                            current = value.clone();
                            let stash = format!("*{}", sym);
                            env_with_binding
                                .write()
                                .unwrap()
                                .set_in_chain(&stash, value);
                            continue;
                        }
                        _ => {}
                    }
                }
            }
            current = self.eval(form, env_with_binding.clone())?;
            env_with_binding
                .write()
                .unwrap()
                .set(&name, current.clone());
        }
        Ok(current)
    }

    fn eval_cond_thread(
        &self,
        args: &[Form],
        env: EnvRef,
        style: ThreadStyle,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "cond-> expects initial value and clauses",
            ));
        }
        if (args.len() - 1) % 2 != 0 {
            return Err(span_runtime_error(
                form_span,
                "cond-> expects pairs of test and form",
            ));
        }
        let mut acc = self.eval(&args[0], env.clone())?;
        let mut idx = 1;
        while idx < args.len() {
            let test_val = self.eval(&args[idx], env.clone())?;
            if truthy(&test_val) {
                acc = self.apply_thread_step(&args[idx + 1], acc, env.clone(), style)?;
            }
            idx += 2;
        }
        Ok(acc)
    }

    fn eval_while(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(form_span, "while expects test and body"));
        }
        loop {
            let cond = self.eval(&args[0], env.clone())?;
            if !truthy(&cond) {
                break;
            }
            self.eval_do(&args[1..], env.clone())?;
        }
        Ok(Value::Nil)
    }

    fn eval_doseq(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
        label: &str,
    ) -> Result<Value, CloveError> {
        let _guard = profiler::enter(label);
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "doseq expects bindings vector and body",
            ));
        }
        let bindings = self.parse_binding_pairs(&args[0])?;
        if bindings.is_empty() {
            return Err(span_runtime_error(
                args[0].span,
                "doseq requires at least one binding",
            ));
        }
        self.run_doseq(&bindings, 0, env, &args[1..])?;
        Ok(Value::Nil)
    }

    fn eval_each(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        let _guard = profiler::enter("each");
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "each expects [bindings] body... or (each f coll)",
            ));
        }
        match &args[0].kind {
            FormKind::Vector(_) => {
                let bindings = self.parse_binding_pairs(&args[0])?;
                if bindings.is_empty() {
                    return Err(span_runtime_error(
                        args[0].span,
                        "each requires at least one binding",
                    ));
                }
                let receiver = {
                    let (_, coll_form) = &bindings[0];
                    self.eval(coll_form, env.clone())?
                };
                let items = seq_items(&receiver)?;
                let (pattern, _) = &bindings[0];
                for item in items {
                    let child = bind_pattern_child(env.clone(), pattern, item)?;
                    self.run_doseq(&bindings, 1, child, &args[1..])?;
                }
                Ok(receiver)
            }
            _ => {
                if args.len() != 2 {
                    return Err(span_runtime_error(
                        form_span,
                        "each expects [bindings] body... or (each f coll)",
                    ));
                }
                let func = self.eval(&args[0], env.clone())?;
                let receiver = self.eval(&args[1], env.clone())?;
                let items = seq_items(&receiver)?;
                for item in items {
                    call_callable(func.clone(), vec![item])?;
                }
                Ok(receiver)
            }
        }
    }

    fn run_doseq(
        &self,
        bindings: &[(Form, Form)],
        idx: usize,
        env: EnvRef,
        body: &[Form],
    ) -> Result<(), CloveError> {
        if idx == bindings.len() {
            self.eval_do(body, env)?;
            return Ok(());
        }
        let (pattern, coll_form) = &bindings[idx];
        let coll_val = self.eval(coll_form, env.clone())?;
        let items = builtins::seq_items(&coll_val)?;
        for item in items {
            let child = bind_pattern_child(env.clone(), pattern, item)?;
            self.run_doseq(bindings, idx + 1, child, body)?;
        }
        Ok(())
    }

    fn eval_map_sugar(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Option<Value>, CloveError> {
        if args.is_empty() {
            return Ok(None);
        }
        let binding_form = &args[0];
        if !matches!(binding_form.kind, FormKind::Vector(_)) {
            return Ok(None);
        }
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "map expects binding and body",
            ));
        }
        let (pattern, coll_form) = self.parse_binding_pair(binding_form)?;
        let fn_body_span = binding_form.span;
        let it_name = format!(
            "__it{}",
            PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst)
        );
        let it_symbol = Form::new(FormKind::Symbol(it_name), fn_body_span);
        let bindings_vec = Form::new(
            FormKind::Vector(vec![pattern, it_symbol.clone()]),
            fn_body_span,
        );
        let mut let_items = Vec::with_capacity(args.len() + 2);
        let_items.push(Form::new(FormKind::Symbol("let".into()), fn_body_span));
        let_items.push(bindings_vec);
        for body in &args[1..] {
            let_items.push(body.clone());
        }
        let let_form = Form::new(FormKind::List(let_items), fn_body_span);
        let fn_params = Form::new(FormKind::Vector(vec![it_symbol]), fn_body_span);
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), fn_body_span),
                fn_params,
                let_form,
            ]),
            fn_body_span,
        );
        let call_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("core::map".into()), form_span),
                fn_form,
                coll_form,
            ]),
            form_span,
        );
        Ok(Some(self.eval(&call_form, env)?))
    }

    fn eval_pmap_sugar(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
        target_sym: &str,
    ) -> Result<Option<Value>, CloveError> {
        if args.is_empty() {
            return Ok(None);
        }
        let mut binding_idx = 0;
        let mut opts_form: Option<&Form> = None;
        if args.len() >= 2
            && matches!(args[0].kind, FormKind::Map(_))
            && matches!(args[1].kind, FormKind::Vector(_))
        {
            opts_form = Some(&args[0]);
            binding_idx = 1;
        }
        let binding_form = &args[binding_idx];
        if !matches!(binding_form.kind, FormKind::Vector(_)) {
            return Ok(None);
        }
        if args.len() <= binding_idx + 1 {
            return Err(span_runtime_error(
                form_span,
                "pmap expects binding and body",
            ));
        }
        let mut body_idx = binding_idx + 1;
        if args.len() > body_idx && matches!(args[body_idx].kind, FormKind::Map(_)) {
            if opts_form.is_some() {
                return Err(span_runtime_error(
                    form_span,
                    "pmap accepts a single opts map",
                ));
            }
            opts_form = Some(&args[body_idx]);
            body_idx += 1;
        }
        if args.len() <= body_idx {
            return Err(span_runtime_error(
                form_span,
                "pmap expects binding and body",
            ));
        }
        let (pattern, coll_form) = self.parse_binding_pair(binding_form)?;
        let fn_body_span = binding_form.span;
        let it_name = format!(
            "__it{}",
            PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst)
        );
        let it_symbol = Form::new(FormKind::Symbol(it_name), fn_body_span);
        let bindings_vec = Form::new(
            FormKind::Vector(vec![pattern, it_symbol.clone()]),
            fn_body_span,
        );
        let mut let_items = Vec::with_capacity(args.len() + 1);
        let_items.push(Form::new(FormKind::Symbol("let".into()), fn_body_span));
        let_items.push(bindings_vec);
        for body in &args[body_idx..] {
            let_items.push(body.clone());
        }
        let let_form = Form::new(FormKind::List(let_items), fn_body_span);
        let fn_params = Form::new(FormKind::Vector(vec![it_symbol]), fn_body_span);
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), fn_body_span),
                fn_params,
                let_form,
            ]),
            fn_body_span,
        );
        let mut call_items = vec![
            Form::new(FormKind::Symbol(target_sym.into()), form_span),
            fn_form,
            coll_form,
        ];
        if let Some(opts_form) = opts_form {
            call_items.push(opts_form.clone());
        }
        let call_form = Form::new(FormKind::List(call_items), form_span);
        Ok(Some(self.eval(&call_form, env)?))
    }

    fn eval_filter_sugar(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Option<Value>, CloveError> {
        let Some(binding_form) = args.first() else {
            return Ok(None);
        };
        if !matches!(binding_form.kind, FormKind::Vector(_)) {
            return Ok(None);
        }
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "filter expects binding and predicate body",
            ));
        }
        let (pattern, coll_form) = self.parse_binding_pair(binding_form)?;
        let fn_body_span = binding_form.span;
        let it_name = format!(
            "__it{}",
            PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst)
        );
        let it_symbol = Form::new(FormKind::Symbol(it_name), fn_body_span);
        let bindings_vec = Form::new(
            FormKind::Vector(vec![pattern, it_symbol.clone()]),
            fn_body_span,
        );
        let mut let_items = Vec::with_capacity(args.len() + 2);
        let_items.push(Form::new(FormKind::Symbol("let".into()), fn_body_span));
        let_items.push(bindings_vec);
        for body in &args[1..] {
            let_items.push(body.clone());
        }
        let let_form = Form::new(FormKind::List(let_items), fn_body_span);
        let fn_params = Form::new(FormKind::Vector(vec![it_symbol]), fn_body_span);
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), fn_body_span),
                fn_params,
                let_form,
            ]),
            fn_body_span,
        );
        let call_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("core::filter".into()), form_span),
                fn_form,
                coll_form,
            ]),
            form_span,
        );
        Ok(Some(self.eval(&call_form, env)?))
    }

    fn eval_pfilter_sugar(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
        target_sym: &str,
    ) -> Result<Option<Value>, CloveError> {
        if args.is_empty() {
            return Ok(None);
        }
        let mut binding_idx = 0;
        let mut opts_form: Option<&Form> = None;
        if args.len() >= 2
            && matches!(args[0].kind, FormKind::Map(_))
            && matches!(args[1].kind, FormKind::Vector(_))
        {
            opts_form = Some(&args[0]);
            binding_idx = 1;
        }
        let binding_form = &args[binding_idx];
        if !matches!(binding_form.kind, FormKind::Vector(_)) {
            return Ok(None);
        }
        if args.len() <= binding_idx + 1 {
            return Err(span_runtime_error(
                form_span,
                "pfilter expects binding and predicate body",
            ));
        }
        let mut body_idx = binding_idx + 1;
        if args.len() > body_idx && matches!(args[body_idx].kind, FormKind::Map(_)) {
            if opts_form.is_some() {
                return Err(span_runtime_error(
                    form_span,
                    "pfilter accepts a single opts map",
                ));
            }
            opts_form = Some(&args[body_idx]);
            body_idx += 1;
        }
        if args.len() <= body_idx {
            return Err(span_runtime_error(
                form_span,
                "pfilter expects binding and predicate body",
            ));
        }
        let (pattern, coll_form) = self.parse_binding_pair(binding_form)?;
        let fn_body_span = binding_form.span;
        let it_name = format!(
            "__it{}",
            PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst)
        );
        let it_symbol = Form::new(FormKind::Symbol(it_name), fn_body_span);
        let bindings_vec = Form::new(
            FormKind::Vector(vec![pattern, it_symbol.clone()]),
            fn_body_span,
        );
        let mut let_items = Vec::with_capacity(args.len() + 2);
        let_items.push(Form::new(FormKind::Symbol("let".into()), fn_body_span));
        let_items.push(bindings_vec);
        for body in &args[body_idx..] {
            let_items.push(body.clone());
        }
        let let_form = Form::new(FormKind::List(let_items), fn_body_span);
        let fn_params = Form::new(FormKind::Vector(vec![it_symbol]), fn_body_span);
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), fn_body_span),
                fn_params,
                let_form,
            ]),
            fn_body_span,
        );
        let mut call_items = vec![
            Form::new(FormKind::Symbol(target_sym.into()), form_span),
            fn_form,
            coll_form,
        ];
        if let Some(opts_form) = opts_form {
            call_items.push(opts_form.clone());
        }
        let call_form = Form::new(FormKind::List(call_items), form_span);
        Ok(Some(self.eval(&call_form, env)?))
    }

    fn eval_dotimes(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "dotimes expects binding vector and body",
            ));
        }
        let (name, count_form) = self.parse_symbol_binding_pair(&args[0])?;
        let count_val = self.eval(&count_form, env.clone())?;
        let (count_num, _) = builtins::as_number(&count_val)?;
        let limit = builtins::num_to_isize(count_num)?;
        if limit < 0 {
            return Err(span_runtime_error(
                count_form.span,
                "dotimes count must be non-negative",
            ));
        }
        for idx in 0..(limit as usize) {
            let child = bind_single(env.clone(), &name, Value::Int(idx as i64));
            self.eval_do(&args[1..], child)?;
        }
        Ok(Value::Nil)
    }

    fn eval_for(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "for expects bindings vector and body",
            ));
        }
        let steps = parse_for_steps(&args[0])?;
        if steps.is_empty() {
            return Err(span_runtime_error(
                args[0].span,
                "for requires at least one binding",
            ));
        }
        let mut out = Vec::new();
        run_for_steps(self, &steps, 0, env, &args[1..], &mut out)?;
        Ok(Value::Vector(out.into()))
    }

    fn eval_condp(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() < 2 {
            return Err(span_runtime_error(
                form_span,
                "condp expects predicate, expr, and clauses",
            ));
        }
        let pred_fn = self.eval(&args[0], env.clone())?;
        let expr_val = self.eval(&args[1], env.clone())?;
        let mut idx = 2;
        while idx < args.len() {
            let test_form = &args[idx];
            let is_default = matches!(test_form.kind, FormKind::Keyword(ref kw) if kw == "else")
                || matches!(test_form.kind, FormKind::Symbol(ref s) if s == ":else" || s == "_");
            if is_default {
                if idx + 1 >= args.len() {
                    return Err(span_runtime_error(
                        test_form.span,
                        "condp :else expects result expression",
                    ));
                }
                return self.eval(&args[idx + 1], env);
            }
            if idx + 1 >= args.len() {
                return Err(span_runtime_error(
                    test_form.span,
                    "condp clause expects test and result",
                ));
            }
            let next_form = &args[idx + 1];
            let pred_result = call_callable(
                pred_fn.clone(),
                vec![self.eval(test_form, env.clone())?, expr_val.clone()],
            )?;
            let is_truthy = truthy(&pred_result);
            if let FormKind::Keyword(kw) = &next_form.kind {
                if kw == ">>" {
                    if idx + 2 >= args.len() {
                        return Err(span_runtime_error(
                            next_form.span,
                            "condp :>> expects transformer",
                        ));
                    }
                    if is_truthy {
                        let transformer = self.eval(&args[idx + 2], env.clone())?;
                        return call_callable(transformer, vec![pred_result]);
                    }
                    idx += 3;
                    continue;
                }
            }
            if is_truthy {
                return self.eval(next_form, env);
            }
            idx += 2;
        }
        Ok(Value::Nil)
    }

    fn eval_with_redefs(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "with-redefs expects bindings vector and body",
            ));
        }
        let mut evaluated: Vec<(String, Value)> = Vec::new();
        if matches!(args[0].kind, FormKind::Vector(_)) {
            let bindings = self.parse_symbol_binding_pairs(&args[0])?;
            if bindings.is_empty() {
                return Err(span_runtime_error(
                    args[0].span,
                    "with-redefs requires at least one binding",
                ));
            }
            for (name, form) in &bindings {
                evaluated.push((name.clone(), self.eval(form, env.clone())?));
            }
        } else {
            let bindings_val = self.eval(&args[0], env.clone())?;
            let vector = match bindings_val {
                Value::Vector(v) => v,
                _ => {
                    return Err(span_runtime_error(
                        form_span,
                        "with-redefs expects bindings vector and body",
                    ))
                }
            };
            if vector.is_empty() {
                return Err(span_runtime_error(
                    form_span,
                    "with-redefs requires at least one binding",
                ));
            }
            if vector.len() % 2 != 0 {
                return Err(span_runtime_error(
                    form_span,
                    "binding vector must have even number of forms",
                ));
            }
            let mut idx = 0;
            while idx < vector.len() {
                let name_val = &vector[idx];
                let new_val = vector[idx + 1].clone();
                let name = match name_val {
                    Value::Symbol(s) => s.clone(),
                    _ => return Err(span_runtime_error(form_span, "binding name must be symbol")),
                };
                evaluated.push((name, new_val));
                idx += 2;
            }
        }
        let _dyn_guard = dynamic_vars::push_bindings(&evaluated);
        let mut backups: Vec<(String, Option<Value>)> = Vec::new();
        let mut missing: Vec<(String, Value)> = Vec::new();
        let mut bump_targets: Vec<(String, EnvRef)> = Vec::new();
        {
            let mut writer = env.write().unwrap();
            for (name, new_val) in &evaluated {
                let prev = writer.get(name);
                if prev.is_some() {
                    backups.push((name.clone(), prev));
                    writer.set_in_chain(name, new_val.clone());
                    bump_targets.push((name.clone(), env.clone()));
                } else {
                    missing.push((name.clone(), new_val.clone()));
                }
            }
        }

        let mut extra_backups: Vec<(EnvRef, Vec<(String, Option<Value>)>)> = Vec::new();
        if !missing.is_empty() {
            let mut ns_envs: Vec<(String, EnvRef)> = Vec::new();
            let mut seen_envs: StdHashSet<usize> = StdHashSet::new();
            let mut push_env = |name: String, candidate: EnvRef| {
                let key = Arc::as_ptr(&candidate) as usize;
                if Arc::ptr_eq(&candidate, &env) || !seen_envs.insert(key) {
                    return;
                }
                ns_envs.push((name, candidate));
            };
            if let Ok(store) = self.namespace_store() {
                if let Ok(guard) = store.read() {
                    for ns in guard.names() {
                        if let Some(data) = guard.get(&ns) {
                            push_env(ns.clone(), data.env());
                        }
                    }
                }
            }
            if let Some(Ok(envs)) = RuntimeCtx::try_with_current(|ctx| {
                let mut collected = Vec::new();
                for ns in ctx.namespace_names() {
                    if let Some(ns_env) = ctx.namespace_env(&ns) {
                        collected.push((ns, ns_env));
                    }
                }
                Ok(collected)
            }) {
                for (ns_name, ns_env) in envs {
                    push_env(ns_name, ns_env);
                }
            }
            let mut missing_locals: Vec<(String, Value)> = Vec::new();
            // Update any namespace env that already has the binding; fall back to a local shadow if unresolved.
            for (name, new_val) in missing {
                let mut applied = false;
                for (_, target_env) in &ns_envs {
                    let has_binding = target_env.read().unwrap().get(&name).is_some();
                    if !has_binding {
                        continue;
                    }
                    applied = true;
                    let backups_vec = if let Some((_, backups_vec)) = extra_backups
                        .iter_mut()
                        .find(|(env_ref, _)| Arc::ptr_eq(env_ref, target_env))
                    {
                        backups_vec
                    } else {
                        extra_backups.push((target_env.clone(), Vec::new()));
                        &mut extra_backups.last_mut().unwrap().1
                    };
                    let prev = {
                        let mut writer = target_env.write().unwrap();
                        let prev = writer.get(&name);
                        writer.set_in_chain(&name, new_val.clone());
                        prev
                    };
                    backups_vec.push((name.clone(), prev));
                    bump_targets.push((name.clone(), target_env.clone()));
                }
                if !applied {
                    missing_locals.push((name.clone(), new_val));
                }
            }
            if !missing_locals.is_empty() {
                let mut writer = env.write().unwrap();
                for (name, new_val) in missing_locals {
                    let prev = writer.get(&name);
                    backups.push((name.clone(), prev));
                    writer.set(&name, new_val);
                    bump_targets.push((name.clone(), env.clone()));
                }
            }
        }
        for (name, target_env) in bump_targets {
            bump_global_version(&name, &target_env);
        }

        let result = self.eval_do(&args[1..], env.clone());
        let mut restore_targets: Vec<(String, EnvRef)> = Vec::new();
        {
            let mut writer = env.write().unwrap();
            for (name, prev) in backups {
                if let Some(val) = prev {
                    writer.set_in_chain(&name, val);
                } else {
                    writer.remove(&name);
                }
                restore_targets.push((name.clone(), env.clone()));
            }
        }
        for (env_ref, env_backups) in extra_backups {
            let mut writer = env_ref.write().unwrap();
            for (name, prev) in env_backups {
                if let Some(prev_val) = prev {
                    writer.set_in_chain(&name, prev_val);
                } else {
                    writer.remove(&name);
                }
                restore_targets.push((name.clone(), env_ref.clone()));
            }
        }
        for (name, target_env) in restore_targets {
            bump_global_version(&name, &target_env);
        }
        result
    }

    fn eval_with_open(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "with-open expects bindings vector and body",
            ));
        }
        let bindings = self.parse_symbol_binding_pairs(&args[0])?;
        if bindings.is_empty() {
            return Err(span_runtime_error(
                args[0].span,
                "with-open requires at least one binding",
            ));
        }
        let child = new_ref(Env::new_child(env));
        let mut resources = Vec::with_capacity(bindings.len());
        for (name, value_form) in bindings {
            let value = match self.eval(&value_form, child.clone()) {
                Ok(val) => val,
                Err(err) => {
                    if let Err(close_err) =
                        self.close_with_open_resources(&resources, child.clone())
                    {
                        return Err(close_err);
                    }
                    return Err(err);
                }
            };
            child.write().unwrap().set(&name, value.clone());
            resources.push(value);
        }
        let result = self.eval_do(&args[1..], child.clone());
        if let Err(err) = self.close_with_open_resources(&resources, child.clone()) {
            return Err(err);
        }
        result
    }

    fn close_with_open_resources(
        &self,
        resources: &[Value],
        env: EnvRef,
    ) -> Result<(), CloveError> {
        for value in resources.iter().rev() {
            self.close_with_open_resource(value, env.clone())?;
        }
        Ok(())
    }

    fn close_with_open_resource(&self, value: &Value, env: EnvRef) -> Result<(), CloveError> {
        if let Some(close_fn) = self.meta_close_callable(value)? {
            call_callable(close_fn, vec![value.clone()])?;
            return Ok(());
        }
        if let Value::Chan(handle) = value {
            handle.close();
            return Ok(());
        }
        if let Some(close_fn) = env.read().unwrap().get("close!") {
            call_callable(close_fn, vec![value.clone()])?;
            return Ok(());
        }
        if let Some(close_fn) = env.read().unwrap().get("close") {
            call_callable(close_fn, vec![value.clone()])?;
            return Ok(());
        }
        Err(CloveError::runtime(format!(
            "with-open cannot close {}",
            value.type_name()
        )))
    }

    fn meta_close_callable(&self, value: &Value) -> Result<Option<Value>, CloveError> {
        let close_bang = Key::Keyword("close!".into());
        let close = Key::Keyword("close".into());
        match meta_lookup(value) {
            Value::Map(meta) => Ok(meta
                .get(&close_bang)
                .cloned()
                .or_else(|| meta.get(&close).cloned())),
            Value::SortedMap(meta) => {
                if let Some(found) = builtins::sorted_map_get(&meta, &close_bang)? {
                    return Ok(Some(found));
                }
                if let Some(found) = builtins::sorted_map_get(&meta, &close)? {
                    return Ok(Some(found));
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn eval_with_dyn(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() < 1 {
            return Err(span_runtime_error(
                form_span,
                "with-dyn expects bindings vector and body",
            ));
        }
        let bindings = self.parse_symbol_binding_pairs(&args[0])?;
        if bindings.is_empty() {
            return Err(span_runtime_error(
                args[0].span,
                "with-dyn requires at least one binding",
            ));
        }
        let mut evaluated = Vec::with_capacity(bindings.len());
        for (name, form) in bindings {
            if !dynamic_vars::is_dynamic_var(&name) {
                return Err(span_runtime_error(
                    args[0].span,
                    format!(
                        "with-dyn expects dynamic var, but '{}' is not dynamic",
                        name
                    ),
                ));
            }
            let val = self.eval(&form, env.clone())?;
            let normalized = self.normalize_dynamic_value(&name, val)?;
            evaluated.push((name, normalized));
        }
        let _guard = dynamic_vars::push_bindings(&evaluated);
        self.eval_do(&args[1..], env)
    }

    fn eval_go_loop(&self, items: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        if items.len() < 3 {
            return Err(span_runtime_error(
                items[0].span,
                "go-loop expects bindings vector and body",
            ));
        }
        let bindings_form = items[1].clone();
        let body_forms = &items[2..];
        let mut loop_items = Vec::with_capacity(body_forms.len() + 2);
        loop_items.push(Form::new(
            FormKind::Symbol("loop".into()),
            bindings_form.span,
        ));
        loop_items.push(bindings_form);
        loop_items.extend_from_slice(body_forms);
        let loop_form = Form::new(FormKind::List(loop_items), items[0].span);
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), items[0].span),
                Form::new(FormKind::Vector(Vec::new()), items[0].span),
                loop_form,
            ]),
            items[0].span,
        );
        let callable = self.eval(&fn_form, env.clone())?;
        let task = spawn_task(callable);
        Ok(Value::Task(task))
    }

    fn eval_scope_loop(&self, items: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        if items.len() < 3 {
            return Err(span_runtime_error(
                items[0].span,
                "scope-loop expects bindings vector and body",
            ));
        }
        let head_span = items[0].span;
        let bindings_form = items[1].clone();
        let body_forms = &items[2..];

        let mut do_items = Vec::with_capacity(body_forms.len() + 1);
        do_items.push(Form::new(FormKind::Symbol("do".into()), head_span));
        do_items.extend(body_forms.iter().cloned());
        let do_form = Form::new(FormKind::List(do_items), head_span);

        let mut if_items = Vec::with_capacity(4);
        if_items.push(Form::new(FormKind::Symbol("if".into()), head_span));
        if_items.push(Form::new(
            FormKind::List(vec![Form::new(
                FormKind::Symbol("async::cancelled?".into()),
                head_span,
            )]),
            head_span,
        ));
        if_items.push(Form::new(FormKind::Nil, head_span));
        if_items.push(do_form);
        let if_form = Form::new(FormKind::List(if_items), head_span);

        let mut loop_items = Vec::with_capacity(3);
        loop_items.push(Form::new(FormKind::Symbol("loop".into()), head_span));
        loop_items.push(bindings_form);
        loop_items.push(if_form);
        let loop_form = Form::new(FormKind::List(loop_items), head_span);
        self.eval(&loop_form, env)
    }

    fn eval_async_scope(&self, items: &[Form], env: EnvRef) -> Result<Value, CloveError> {
        const MAIN_BODY_PROMISE_LIKE_MSG: &str =
            "main-body must not return promise-like; put it in children or deref it";
        let head_span = items[0].span;
        if items.len() < 3 {
            return Err(span_runtime_error(
                head_span,
                "async-scope expects children vector and body",
            ));
        }
        let children_form = &items[1];
        let body_forms = &items[2..];
        let child_forms = match &children_form.kind {
            FormKind::Vector(forms) => forms.clone(),
            _ => {
                return Err(span_runtime_error(
                    children_form.span,
                    "async-scope expects children vector",
                ))
            }
        };
        if body_forms.is_empty() {
            return Err(span_runtime_error(
                head_span,
                "async-scope expects at least one form in the body",
            ));
        }
        let ctx = CancelContext::new(current_cancel_ctx());
        let (children_values, children_promises, body_task) = {
            let env_for_scope = env.clone();
            let ctx_clone = ctx.clone();
            with_cancel_ctx(Some(ctx_clone), || -> Result<_, CloveError> {
                let mut child_values = Vec::with_capacity(child_forms.len());
                let mut child_promises = Vec::with_capacity(child_forms.len());
                for (idx, child_form) in child_forms.iter().enumerate() {
                    let value = self.eval(child_form, env_for_scope.clone())?;
                    if let Some(kind) = promise_like_from_value(&value) {
                        child_values.push(value);
                        child_promises.push(kind);
                    } else {
                        let msg = format!(
                            "async-scope children must be promise-like (bad child at index {}: {})",
                            idx,
                            value.type_name()
                        );
                        return Err(span_runtime_error(child_form.span, msg));
                    }
                }
                let mut fn_items = Vec::with_capacity(body_forms.len() + 2);
                fn_items.push(Form::new(FormKind::Symbol("fn".into()), head_span));
                fn_items.push(Form::new(FormKind::Vector(Vec::new()), head_span));
                fn_items.extend(body_forms.iter().cloned());
                let fn_form = Form::new(FormKind::List(fn_items), head_span);
                let body_callable = self.eval(&fn_form, env_for_scope)?;
                let runner_ctx = ctx.clone();
                let runner_callable = Value::native_fn(FnArity::exact(0), move |_| {
                    let result = call_callable(body_callable.clone(), Vec::new());
                    runner_ctx.cancel();
                    result
                });
                let task = spawn_task(runner_callable);
                Ok((child_values, child_promises, task))
            })?
        };
        let children_values_vec = Value::Vector(Vector::from(children_values));
        let children_promises = Arc::new(children_promises);
        let body_task_for_await = body_task.clone();
        let child_promises_for_await = Arc::clone(&children_promises);
        let await_span = head_span;
        let cancel_ctx_for_await = ctx.clone();
        let await_fn = Value::native_fn(FnArity::exact(0), move |_| {
            let mut body_val: Option<Value> = None;
            let mut body_err: Option<CloveError> = None;
            match body_task_for_await.wait() {
                Ok(val) => body_val = Some(val),
                Err(err) => body_err = Some(err),
            }
            let mut promise_like_err = None;
            if let Some(value) = &body_val {
                if promise_like_from_value(value).is_some() {
                    if async_scope_strict_enabled() {
                        promise_like_err =
                            Some(span_runtime_error(await_span, MAIN_BODY_PROMISE_LIKE_MSG));
                    } else {
                        eprintln!("{} {}", WARN_TAG, MAIN_BODY_PROMISE_LIKE_MSG);
                    }
                }
            }
            let mut child_err = None;
            for child in child_promises_for_await.iter() {
                if let Err(err) = child.wait() {
                    cancel_ctx_for_await.cancel();
                    if child_err.is_none() {
                        child_err = Some(err);
                    }
                }
            }
            if let Some(err) = body_err {
                return Err(err);
            }
            if let Some(err) = promise_like_err {
                return Err(err);
            }
            if let Some(err) = child_err {
                return Err(err);
            }
            Ok(body_val.unwrap_or(Value::Nil))
        });
        let ctx_for_cancel = ctx.clone();
        let cancel_fn = Value::native_fn(FnArity::exact(0), move |_| {
            let already = ctx_for_cancel.is_cancelled();
            ctx_for_cancel.cancel();
            Ok(Value::Bool(!already))
        });
        let ctx_for_cancelled = ctx.clone();
        let cancelled_fn = Value::native_fn(FnArity::exact(0), move |_| {
            Ok(Value::Bool(ctx_for_cancelled.is_cancelled()))
        });
        let cancel_chan = Value::Chan(ctx.cancel_chan());
        let mut handle = HashMap::new();
        handle.insert(Key::Keyword("cancel!".into()), cancel_fn);
        handle.insert(Key::Keyword("await".into()), await_fn);
        handle.insert(Key::Keyword("cancelled?".into()), cancelled_fn);
        handle.insert(Key::Keyword("cancel-chan".into()), cancel_chan);
        handle.insert(Key::Keyword("task".into()), Value::Task(body_task));
        handle.insert(Key::Keyword("children".into()), children_values_vec);
        Ok(Value::Map(handle))
    }

    fn namespace_store(&self) -> Result<Arc<RwLock<NamespaceStore>>, CloveError> {
        self.namespace_store
            .clone()
            .ok_or_else(|| CloveError::runtime("namespace store is unavailable"))
    }

    fn eval_ns_map(
        &self,
        args: &[Form],
        env: EnvRef,
        _form_span: Span,
    ) -> Result<Value, CloveError> {
        let target_ns = if args.is_empty() {
            current_namespace_name(&env).or_else(|| self.default_namespace.read().unwrap().clone())
        } else {
            let raw = self.eval(&args[0], env.clone())?;
            match raw {
                Value::Symbol(s) | Value::String(s) => Some(canonical_symbol_name(&s).into_owned()),
                other => {
                    return Err(span_runtime_error(
                        args[0].span,
                        &format!(
                            "ns-map expects namespace symbol or string, got {}",
                            other.type_name()
                        ),
                    ))
                }
            }
        }
        .unwrap_or_else(|| "user".to_string());
        let store = self.namespace_store()?;
        let store = store.read().unwrap();
        let Some(ns) = store.get(&target_ns) else {
            return Ok(Value::Nil);
        };
        let entries = ns.env().read().unwrap().flatten();
        let mut map = HashMap::new();
        for (name, value) in entries {
            map.insert(Key::Keyword(name), value);
        }
        Ok(Value::Map(map))
    }

    fn eval_create_ns(
        &self,
        args: &[Form],
        _env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let raw = args
            .get(0)
            .ok_or_else(|| span_runtime_error(form_span, "create-ns expects namespace symbol"))?;
        let name_val = self.eval(raw, self.global_env())?;
        let name = match name_val {
            Value::Symbol(s) | Value::String(s) => canonical_symbol_name(&s).into_owned(),
            other => {
                return Err(span_runtime_error(
                    raw.span,
                    &format!(
                        "create-ns expects namespace symbol, got {}",
                        other.type_name()
                    ),
                ))
            }
        };
        let store = self.namespace_store()?;
        let mut store = store.write().unwrap();
        store.ensure(&name);
        Ok(Value::Symbol(name))
    }

    fn eval_refer(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "refer expects target namespace",
            ));
        }
        let ns_raw = self.eval(&args[0], env.clone())?;
        let target_ns = match ns_raw {
            Value::Symbol(s) | Value::String(s) => canonical_symbol_name(&s).into_owned(),
            other => {
                return Err(span_runtime_error(
                    args[0].span,
                    &format!("refer expects namespace symbol, got {}", other.type_name()),
                ))
            }
        };
        let mut only: Option<StdHashSet<String>> = None;
        let mut exclude: StdHashSet<String> = StdHashSet::new();
        let mut idx = 1;
        while idx + 1 < args.len() {
            let key_form = &args[idx];
            let val_form = &args[idx + 1];
            match &key_form.kind {
                FormKind::Keyword(name) if name == "only" => {
                    let list = self.eval(val_form, env.clone())?;
                    let mut set = StdHashSet::new();
                    let seq = seq_items(&list)?;
                    for item in seq {
                        if let Value::Symbol(s) | Value::String(s) = item {
                            set.insert(canonical_symbol_name(&s).into_owned());
                        }
                    }
                    only = Some(set);
                }
                FormKind::Keyword(name) if name == "exclude" => {
                    let list = self.eval(val_form, env.clone())?;
                    let seq = seq_items(&list)?;
                    for item in seq {
                        if let Value::Symbol(s) | Value::String(s) = item {
                            exclude.insert(canonical_symbol_name(&s).into_owned());
                        }
                    }
                }
                _ => break,
            }
            idx += 2;
        }
        let store = self.namespace_store()?;
        let store = store.read().unwrap();
        let Some(ns) = store.get(&target_ns) else {
            return Ok(Value::Nil);
        };
        let exports = &ns.public_exports;
        let mut writer = env.write().unwrap();
        for (name, value) in exports {
            if exclude.contains(name) {
                continue;
            }
            if let Some(allow) = &only {
                if !allow.contains(name) {
                    continue;
                }
            }
            writer.set(name, value.clone());
        }
        Ok(Value::Symbol(target_ns))
    }

    fn eval_resolve(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(form_span, "resolve expects symbol"));
        }
        let sym_val = self.eval(&args[0], env.clone())?;
        let name = match sym_val {
            Value::Symbol(s) | Value::String(s) => canonical_symbol_name(&s).into_owned(),
            other => {
                return Err(span_runtime_error(
                    args[0].span,
                    &format!(
                        "resolve expects symbol or string, got {}",
                        other.type_name()
                    ),
                ))
            }
        };
        if let Some((ns, local)) = split_namespace_parts(&name) {
            if let Some(store) = self.namespace_store.as_ref() {
                if let Some(entry) = store.read().unwrap().get(&ns) {
                    if let Some(val) = entry.env().read().unwrap().get(&local) {
                        return Ok(val);
                    }
                }
            }
            return Ok(Value::Nil);
        }
        let current_ns =
            current_namespace_name(&env).or_else(|| self.default_namespace.read().unwrap().clone());
        if let Some(ns_name) = current_ns {
            if let Some(store) = self.namespace_store.as_ref() {
                if let Some(entry) = store.read().unwrap().get(&ns_name) {
                    if let Some(val) = entry.env().read().unwrap().get(&name) {
                        return Ok(val);
                    }
                }
            }
        }
        Ok(env.read().unwrap().get(&name).unwrap_or(Value::Nil))
    }

    fn eval_load_file(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let path_form = args
            .get(0)
            .ok_or_else(|| span_runtime_error(form_span, "load-file expects path"))?;
        let path_val = self.eval(path_form, env)?;
        let path = match path_val {
            Value::String(s) | Value::Symbol(s) => Path::new(&s).to_path_buf(),
            other => {
                return Err(span_runtime_error(
                    path_form.span,
                    &format!("load-file expects path string, got {}", other.type_name()),
                ))
            }
        };
        if let Some(loader) = &self.file_loader {
            return loader(&path);
        }
        let content = std::fs::read_to_string(&path)
            .map_err(|e| span_runtime_error(path_form.span, format!("load-file failed: {}", e)))?;
        let mut reader = Reader::new_with_options(&content, ReaderOptions::default());
        let forms = reader.read_all()?;
        let forms = normalize_type_syntax_forms(forms, false)?;
        let source_name = path.to_string_lossy();
        let (forms, _) =
            apply_default_foreign_tags_for_source(forms, Some(source_name.as_ref()), None)?;
        let mut last = Value::Nil;
        for form in &forms {
            last = self.eval(form, self.global_env())?;
        }
        Ok(last)
    }

    fn eval_load_string(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        let src_form = args
            .get(0)
            .ok_or_else(|| span_runtime_error(form_span, "load-string expects source"))?;
        let src_val = self.eval(src_form, env)?;
        let source = match src_val {
            Value::String(s) => s,
            other => {
                return Err(span_runtime_error(
                    src_form.span,
                    &format!("load-string expects string, got {}", other.type_name()),
                ))
            }
        };
        let mut reader = Reader::new_with_options(&source, ReaderOptions::default());
        let forms = reader.read_all()?;
        let forms = normalize_type_syntax_forms(forms, false)?;
        let (forms, _) = apply_default_foreign_tags_for_source(forms, None, None)?;
        let mut last = Value::Nil;
        for form in &forms {
            last = self.eval(form, self.global_env())?;
        }
        Ok(last)
    }

    fn eval_delay(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "delay expects at least one form",
            ));
        }
        let body_form = if args.len() == 1 {
            args[0].clone()
        } else {
            let mut forms = Vec::with_capacity(args.len() + 1);
            forms.push(Form::new(FormKind::Symbol("do".into()), form_span));
            forms.extend(args.iter().cloned());
            Form::new(FormKind::List(forms), form_span)
        };
        let fn_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("fn".into()), form_span),
                Form::new(FormKind::Vector(Vec::new()), form_span),
                body_form,
            ]),
            form_span,
        );
        let thunk = self.eval(&fn_form, env)?;
        Ok(Value::Delay(DelayHandle::new(thunk)))
    }

    fn apply_thread_step(
        &self,
        step: &Form,
        acc: Value,
        env: EnvRef,
        style: ThreadStyle,
    ) -> Result<Value, CloveError> {
        if let Some((expanded, holes)) = expand_placeholder_partial(step) {
            let func = self.eval(&expanded, env)?;
            let count = holes.max(1);
            let mut args = Vec::with_capacity(count);
            for _ in 0..count {
                args.push(acc.clone());
            }
            return call_callable(func, args);
        }
        match &step.kind {
            FormKind::List(items) => {
                if items.is_empty() {
                    return Ok(acc);
                }
                let (head_form, arg_forms) = self.unpack_call_form(items);
                let func = self.eval(head_form, env.clone())?;
                let mut args = Vec::new();
                let mut acc_slot = Some(acc);
                if matches!(style, ThreadStyle::First) {
                    args.push(acc_slot.take().unwrap());
                }
                let mut idx = 0;
                while idx < arg_forms.len() {
                    if let Some((spread_value, span)) =
                        self.try_eval_spread_value(arg_forms, &mut idx, env.clone())?
                    {
                        spread_into_args(spread_value, &mut args, span)?;
                    } else {
                        args.push(self.eval(&arg_forms[idx], env.clone())?);
                        idx += 1;
                    }
                }
                if matches!(style, ThreadStyle::Last) {
                    args.push(acc_slot.take().unwrap());
                }
                call_callable(func, args)
            }
            _ => {
                let callable = self.eval(step, env.clone())?;
                call_callable(callable, vec![acc])
            }
        }
    }

    fn unpack_call_form<'a>(&self, items: &'a [Form]) -> (&'a Form, &'a [Form]) {
        if items.len() >= 2 {
            if let FormKind::Symbol(sym) = &items[0].kind {
                if self.call_wrappers.contains(sym) {
                    return (&items[1], &items[2..]);
                }
            }
        }
        (&items[0], &items[1..])
    }

    fn eval_quote(&self, args: &[Form], form_span: Span) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(form_span, "quote expects one argument"));
        }
        form_to_value(&args[0])
    }

    fn eval_set(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() != 2 {
            return Err(span_runtime_error(form_span, "set! expects name and value"));
        }
        let name = match &args[0].kind {
            FormKind::Symbol(s) => s.clone(),
            _ => return Err(span_runtime_error(args[0].span, "set! name must be symbol")),
        };
        let canonical = canonical_symbol_name(&name).into_owned();
        let val = self.eval(&args[1], env.clone())?;
        let normalized = self.normalize_special_assignment(&canonical, val, env.clone())?;

        let target_env: Option<EnvRef>;
        let mut target_name = canonical.clone();
        let mut target_ns: Option<String> = None;
        let mut target_private = false;

        if let Some((ns, local)) = split_namespace_parts(&canonical) {
            if let Some(ns_env) = self.namespace_env_for(&ns) {
                target_private = self.is_private_name(&ns, &local);
                if target_private {
                    let current_ns = current_namespace_name(&env);
                    if current_ns.as_deref() != Some(ns.as_str()) {
                        return Err(span_runtime_error(
                            args[0].span,
                            "set! cannot access private var from another namespace",
                        ));
                    }
                }
                target_env = Some(ns_env);
                target_name = local;
                target_ns = Some(ns);
            } else {
                target_env = Some(self.root_env(env.clone()));
                target_name = canonical.clone();
            }
        } else if let Some((ns, ns_env)) = self.current_namespace_env(&env) {
            if self.local_binding_exists_until(&env, Some(&ns_env), &canonical) {
                return Err(span_runtime_error(
                    args[0].span,
                    "set! expects an existing namespace var (not a local binding)",
                ));
            }
            target_private = self.is_private_name(&ns, &canonical);
            target_env = Some(ns_env);
            target_ns = Some(ns);
        } else {
            let root = self.root_env(env.clone());
            if self.local_binding_exists_until(&env, Some(&root), &canonical) {
                return Err(span_runtime_error(
                    args[0].span,
                    "set! expects an existing namespace var (not a local binding)",
                ));
            }
            target_env = Some(root);
        }

        let Some(target_env) = target_env else {
            return Err(span_runtime_error(args[0].span, "set! target is undefined"));
        };
        let is_dynamic = dynamic_vars::is_dynamic_var(&canonical);
        let has_local = target_env.read().unwrap().contains_local(&target_name);
        if !has_local && !is_dynamic {
            return Err(span_runtime_error(args[0].span, "set! target is undefined"));
        }
        if has_local {
            target_env
                .write()
                .unwrap()
                .set(&target_name, normalized.clone());
            if let Some(ns) = target_ns.as_deref() {
                if !target_private {
                    if let Some(store) = self.namespace_store_opt() {
                        if let Ok(guard) = store.read() {
                            let shared = guard.shared_env();
                            shared
                                .write()
                                .unwrap()
                                .set(&format!("{ns}::{target_name}"), normalized.clone());
                        }
                    }
                }
            }
        }
        if is_dynamic {
            dynamic_vars::set_root_value(&canonical, normalized.clone());
        }
        let version_name = if let Some(ns) = target_ns {
            format!("{ns}::{target_name}")
        } else {
            target_name.clone()
        };
        bump_global_version(&version_name, &target_env);
        Ok(normalized)
    }

    fn eval_set_in_chain(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.len() != 2 {
            return Err(span_runtime_error(
                form_span,
                "__set-in-chain expects name and value",
            ));
        }
        let name = match &args[0].kind {
            FormKind::Symbol(s) => s.clone(),
            _ => {
                return Err(span_runtime_error(
                    args[0].span,
                    "__set-in-chain name must be symbol",
                ))
            }
        };
        let val = self.eval(&args[1], env.clone())?;
        env.write().unwrap().set_in_chain(&name, val.clone());
        Ok(val)
    }

    fn eval_local_def(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if self.is_top_level_env(&env) {
            return Err(span_runtime_error(
                form_span,
                "-def is local-only; use def at top level",
            ));
        }
        if args.len() != 2 {
            return Err(span_runtime_error(form_span, "-def expects name and value"));
        }
        let name_form = &args[0];
        let name = match &name_form.kind {
            FormKind::Symbol(s) => s.clone(),
            _ => {
                return Err(span_runtime_error(
                    name_form.span,
                    "-def name must be symbol",
                ))
            }
        };
        let val = self.eval(&args[1], env.clone())?;
        env.write().unwrap().set(&name, val.clone());
        Ok(val)
    }

    fn eval_local_defn(&self, _args: &[Form], form_span: Span) -> Result<Value, CloveError> {
        Err(span_runtime_error(
            form_span,
            "-defn is local-only and must appear in a function body",
        ))
    }

    pub(crate) fn eval_local_defn_value(
        &self,
        form: &Form,
        env: EnvRef,
    ) -> Result<(String, Value), CloveError> {
        let items = match &form.kind {
            FormKind::List(items) => items,
            _ => return Err(span_runtime_error(form.span, "-defn expects list form")),
        };
        if items.len() < 2 {
            return Err(span_runtime_error(
                form.span,
                "-defn expects name, params vector, and body",
            ));
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => {
                return Err(span_runtime_error(
                    items[0].span,
                    "-defn expects name, params vector, and body",
                ))
            }
        };
        let mut rewritten_form = None;
        let defn_form = if head == "defn" {
            let mut rewritten = Vec::with_capacity(items.len());
            let mut head_form = items[0].clone();
            head_form.kind = FormKind::Symbol("-defn".into());
            rewritten.push(head_form);
            rewritten.extend_from_slice(&items[1..]);
            rewritten_form = Some(Form::new(FormKind::List(rewritten), form.span));
            rewritten_form.as_ref().unwrap_or(form)
        } else {
            form
        };
        let FormKind::List(defn_items) = &defn_form.kind else {
            return Err(span_runtime_error(
                defn_form.span,
                "-defn expects list form",
            ));
        };
        if head != "-defn" && head != "defn" {
            return Err(span_runtime_error(
                defn_form.span,
                "-defn expects name, params vector, and body",
            ));
        }
        let local_defn = self.build_local_defn(defn_form, defn_items)?;
        let value = build_local_defn_value(
            &local_defn,
            env,
            &self.engines,
            self.auto_fallback,
            self.call_wrappers(),
            self.settings.clone(),
        );
        Ok((local_defn.name.clone(), value))
    }

    fn normalize_special_assignment(
        &self,
        name: &str,
        value: Value,
        _env: EnvRef,
    ) -> Result<Value, CloveError> {
        if name == REPL_ON_ERROR_VAR {
            return Err(CloveError::runtime(
                "use (use repl-on-error true|false); set! *repl-on-error* is removed".to_string(),
            ));
        }
        Ok(value)
    }

    fn normalize_dynamic_value(&self, name: &str, value: Value) -> Result<Value, CloveError> {
        if name == REPL_ON_ERROR_VAR {
            return Err(CloveError::runtime(
                "use (use repl-on-error true|false); with-dyn *repl-on-error* is removed"
                    .to_string(),
            ));
        }
        Ok(value)
    }

    fn package_id_for_env(&self, env: &EnvRef) -> String {
        let ns =
            current_namespace_name(env).or_else(|| self.default_namespace.read().unwrap().clone());
        if let Some(name) = ns.as_deref() {
            if let Some(pkg_id) = self.settings.namespace_package(name) {
                return pkg_id;
            }
        }
        if let Some(default_ns) = self.default_namespace.read().unwrap().as_deref() {
            if let Some(pkg_id) = self.settings.namespace_package(default_ns) {
                return pkg_id;
            }
        }
        MAIN_PACKAGE_ID.to_string()
    }

    fn dot_chain_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::DotChain), &pkg_id)
    }

    fn oop_syntax_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::OopSyntax), &pkg_id)
    }

    fn dot_indexer_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::DotIndexer), &pkg_id)
    }

    fn indexer_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::Indexer), &pkg_id)
    }

    fn map_refs_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings
            .feature_toggle_enabled(FeatureToggle::Syntax(SyntaxFeatureId::MapRefs), &pkg_id)
    }

    fn foreign_blocks_enabled(&self, env: &EnvRef) -> bool {
        let pkg_id = self.package_id_for_env(env);
        self.settings.feature_toggle_enabled(
            FeatureToggle::Syntax(SyntaxFeatureId::ForeignBlocks),
            &pkg_id,
        )
    }

    fn ensure_foreign_allowed(&self, env: &EnvRef, span: Option<Span>) -> Result<(), CloveError> {
        if self.foreign_blocks_enabled(env) {
            Ok(())
        } else {
            let span = span.unwrap_or(Span {
                line: 0,
                col: 0,
                index: 0,
            });
            Err(span_runtime_error(
                span,
                "foreign blocks are disabled; enable via (use foreign true)",
            ))
        }
    }

    fn eval_fn(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "fn expects params vector or arities",
            ));
        }
        let mut idx = 0;
        let mut fn_name = None;
        if let FormKind::Symbol(sym) = &args[0].kind {
            if args.len() >= 2 {
                if matches!(args[1].kind, FormKind::Vector(_))
                    || matches!(args[1].kind, FormKind::List(_))
                {
                    fn_name = Some(sym.clone());
                    idx = 1;
                }
            }
        }
        let clauses = self.parse_fn_clauses(&args[idx..], form_span)?;
        if clauses.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "fn expects at least one arity",
            ));
        }
        let self_bound = env.read().unwrap().get("self").is_some();
        let (method_clauses, is_method) = if self_bound {
            let (rewritten, _) = rewrite_method_clauses(&clauses);
            (rewritten, false)
        } else {
            methodify_clauses(&clauses, false)
        };
        let mut value = if method_clauses.len() == 1 {
            self.build_single_lambda(fn_name, &method_clauses[0], env)?
        } else {
            self.build_multi_lambda(fn_name, &method_clauses, env)?
        };
        if is_method {
            value = mark_method_value(value);
        }
        Ok(value)
    }

    fn parse_fn_clauses(
        &self,
        forms: &[Form],
        form_span: Span,
    ) -> Result<Vec<FnClauseSpec>, CloveError> {
        if forms.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "fn expects params vector or arities",
            ));
        }
        match &forms[0].kind {
            FormKind::Vector(_) => {
                if forms.len() < 2 {
                    return Err(span_runtime_error(
                        forms[0].span,
                        "fn expects params vector and body",
                    ));
                }
                Ok(vec![FnClauseSpec::single(
                    forms[0].clone(),
                    forms[1..].to_vec(),
                )?])
            }
            FormKind::List(_) => {
                let mut clauses = Vec::with_capacity(forms.len());
                for form in forms {
                    clauses.push(FnClauseSpec::from_clause_form(form)?);
                }
                Ok(clauses)
            }
            _ => Err(span_runtime_error(
                args_span(forms, form_span),
                "fn expects params vector or arities",
            )),
        }
    }

    fn eval_method(
        &self,
        args: &[Form],
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "method expects params vector or arities",
            ));
        }
        let mut idx = 0;
        let mut fn_name = None;
        if let FormKind::Symbol(sym) = &args[0].kind {
            if args.len() >= 2 {
                if matches!(args[1].kind, FormKind::Vector(_))
                    || matches!(args[1].kind, FormKind::List(_))
                {
                    fn_name = Some(sym.clone());
                    idx = 1;
                }
            }
        }
        let clauses = self.parse_fn_clauses(&args[idx..], form_span)?;
        if clauses.is_empty() {
            return Err(span_runtime_error(
                form_span,
                "method expects at least one arity",
            ));
        }
        ensure_method_params_without_self(&clauses)?;
        let (method_clauses, _) = methodify_clauses(&clauses, true);
        let mut value = if method_clauses.len() == 1 {
            self.build_single_lambda(fn_name, &method_clauses[0], env)?
        } else {
            self.build_multi_lambda(fn_name, &method_clauses, env)?
        };
        value = mark_method_value(value);
        Ok(value)
    }

    fn collect_local_defns(
        &self,
        body: &[Form],
    ) -> Result<(Vec<LocalDefn>, Vec<Form>), CloveError> {
        self.collect_local_defns_with(body, false)
    }

    fn collect_local_defns_with(
        &self,
        body: &[Form],
        allow_defn: bool,
    ) -> Result<(Vec<LocalDefn>, Vec<Form>), CloveError> {
        let mut local_defns = Vec::new();
        let mut forms = Vec::new();
        for form in body {
            let (mut nested, kept) = self.extract_local_defns_from_form(form, allow_defn)?;
            local_defns.append(&mut nested);
            if let Some(kept_form) = kept {
                forms.push(kept_form);
            }
        }
        Ok((local_defns, forms))
    }

    fn extract_local_defns_from_form(
        &self,
        form: &Form,
        allow_defn: bool,
    ) -> Result<(Vec<LocalDefn>, Option<Form>), CloveError> {
        let mut local_defns = Vec::new();
        let items = match &form.kind {
            FormKind::List(items) if !items.is_empty() => items,
            _ => return Ok((local_defns, Some(form.clone()))),
        };
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok((local_defns, Some(form.clone()))),
        };
        match head {
            "-defn" => {
                local_defns.push(self.build_local_defn(form, items)?);
                Ok((local_defns, None))
            }
            "defn" if allow_defn => {
                let mut rewritten = Vec::with_capacity(items.len());
                let mut head_form = items[0].clone();
                head_form.kind = FormKind::Symbol("-defn".into());
                rewritten.push(head_form);
                rewritten.extend_from_slice(&items[1..]);
                let rewritten_form = Form::new(FormKind::List(rewritten), form.span);
                let FormKind::List(rewritten_items) = &rewritten_form.kind else {
                    unreachable!();
                };
                local_defns.push(self.build_local_defn(&rewritten_form, rewritten_items)?);
                Ok((local_defns, None))
            }
            "do" | "where" => {
                let allow_nested_defn = head == "where" || allow_defn;
                let (mut nested, forms) =
                    self.collect_local_defns_with(&items[1..], allow_nested_defn)?;
                local_defns.append(&mut nested);
                let mut rewritten = Vec::with_capacity(1 + forms.len());
                rewritten.push(items[0].clone());
                rewritten.extend(forms);
                Ok((
                    local_defns,
                    Some(Form::new(FormKind::List(rewritten), form.span)),
                ))
            }
            _ => Ok((local_defns, Some(form.clone()))),
        }
    }

    fn build_local_defn(&self, form: &Form, items: &[Form]) -> Result<LocalDefn, CloveError> {
        if items.len() < 3 {
            return Err(span_runtime_error(
                form.span,
                "-defn expects name, params vector, and body",
            ));
        }
        let name_form = &items[1];
        let name = if let FormKind::Symbol(s) = &name_form.kind {
            s.clone()
        } else {
            return Err(span_runtime_error(
                name_form.span,
                "-defn name must be symbol",
            ));
        };
        let signature = self.parse_defn_signature(&items[2..], form.span)?;
        let meta_value_map = if let Some(meta_form) = &signature.meta_map {
            match form_to_value(meta_form)? {
                Value::Map(m) => Some(m),
                _ => {
                    return Err(span_runtime_error(
                        meta_form.span,
                        "-defn attr-map must be a map literal",
                    ))
                }
            }
        } else {
            None
        };
        let mut clauses = Vec::with_capacity(signature.clauses.len());
        for clause in &signature.clauses {
            let parsed = self.parse_params(&clause.params_form)?;
            let (local_defns, body_forms) = self.collect_local_defns(&clause.body)?;
            let body = self.wrap_param_destructuring(&clause.params_form, &parsed, &body_forms);
            ensure_recur_tail_positions(&body)?;
            let inferred_type = self.infer_lambda_type_kind(&clause.params_form, &parsed, &body);
            let recur_id = LOOP_COUNTER.fetch_add(1, Ordering::SeqCst);
            clauses.push(LocalDefnClause {
                params: parsed.params,
                rest: parsed.rest,
                body,
                local_defns,
                inferred_type: Some(inferred_type),
                recur_id,
            });
        }
        Ok(LocalDefn {
            name,
            doc: signature.doc.clone(),
            meta: meta_value_map,
            clauses,
        })
    }

    fn build_single_lambda(
        &self,
        fn_name: Option<String>,
        clause: &FnClauseSpec,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let parsed = self.parse_params(&clause.params_form)?;
        let recur_id = LOOP_COUNTER.fetch_add(1, Ordering::SeqCst);
        let (local_defns, body_forms) = self.collect_local_defns(&clause.body)?;
        let body = self.wrap_param_destructuring(&clause.params_form, &parsed, &body_forms);
        ensure_recur_tail_positions(&body)?;
        let inferred_type = self.infer_lambda_type_kind(&clause.params_form, &parsed, &body);
        let source_meta = build_lambda_source_meta();
        Ok(Value::Lambda {
            params: parsed.params,
            rest: parsed.rest,
            body,
            local_defns,
            env: env.clone(),
            engines: self.engines.clone(),
            auto_fallback: self.auto_fallback,
            call_wrappers: self.call_wrappers(),
            settings: self.settings.clone(),
            meta: source_meta,
            doc: None,
            name: fn_name,
            inferred_type: Some(inferred_type),
            recur_id,
        })
    }

    fn build_multi_lambda(
        &self,
        fn_name: Option<String>,
        clauses: &[FnClauseSpec],
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let mut parsed_clauses = Vec::with_capacity(clauses.len());
        for clause in clauses {
            let parsed = self.parse_params(&clause.params_form)?;
            let recur_id = LOOP_COUNTER.fetch_add(1, Ordering::SeqCst);
            let (local_defns, body_forms) = self.collect_local_defns(&clause.body)?;
            let body = self.wrap_param_destructuring(&clause.params_form, &parsed, &body_forms);
            ensure_recur_tail_positions(&body)?;
            parsed_clauses.push(LambdaClause {
                params: parsed.params,
                rest: parsed.rest,
                body,
                local_defns,
                recur_id,
            });
        }
        let source_meta = build_lambda_source_meta();
        Ok(Value::MultiLambda {
            clauses: parsed_clauses,
            env: env.clone(),
            engines: self.engines.clone(),
            auto_fallback: self.auto_fallback,
            call_wrappers: self.call_wrappers(),
            settings: self.settings.clone(),
            meta: source_meta,
            doc: None,
            name: fn_name,
            inferred_type: None,
        })
    }

    fn infer_lambda_type_kind(
        &self,
        params_form: &Form,
        parsed: &ParsedParams,
        body: &[Form],
    ) -> MetaTypeKind {
        let (param_hints, rest_hint) = collect_param_type_hints(params_form);
        let rest_hint_for_param = rest_hint.clone();
        let mut param_infos: Vec<ParamType> = parsed
            .params
            .iter()
            .enumerate()
            .map(|(idx, name)| ParamType {
                name: name.clone(),
                hint: param_hints.get(idx).cloned().flatten(),
            })
            .collect();
        if let Some(rest_name) = &parsed.rest {
            param_infos.push(ParamType {
                name: rest_name.clone(),
                hint: Some(rest_type_from_hint(rest_hint_for_param)),
            });
        }
        let analysis = ir::analyze_fn_body(&param_infos, body);
        let ret_type = return_hint_from_form(params_form).unwrap_or(analysis.ret_type);
        let param_types: Vec<MetaTypeKind> = parsed
            .params
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                param_hints
                    .get(idx)
                    .cloned()
                    .flatten()
                    .unwrap_or_else(MetaTypeKind::any)
            })
            .collect();
        let rest_type = parsed.rest.as_ref().map(|_| rest_type_from_hint(rest_hint));
        MetaTypeKind::function(param_types, rest_type, ret_type)
    }

    fn subject_pos_from_meta_map(&self, map_form: &Form) -> Result<Option<SubjectPos>, CloveError> {
        let entries = match &map_form.kind {
            FormKind::Map(entries) => entries,
            _ => return Ok(None),
        };
        for entry in entries {
            if let MapItem::KeyValue(key, value) = entry {
                let is_subject_key = matches!(&key.kind, FormKind::Keyword(kw) if kw == "subject-pos")
                    || matches!(&key.kind, FormKind::Symbol(sym) if sym == "subject-pos");
                if !is_subject_key {
                    continue;
                }
                let pos = match &value.kind {
                    FormKind::Int(n) if *n > 0 => SubjectPos::Fixed(*n as usize),
                    FormKind::Int(n) if *n == -1 => SubjectPos::Last,
                    FormKind::Keyword(kw) if kw == "last" => SubjectPos::Last,
                    FormKind::Int(_) => {
                        return Err(span_runtime_error(
                            value.span,
                            "subject-pos must be positive integer or :last",
                        ))
                    }
                    _ => {
                        return Err(span_runtime_error(
                            value.span,
                            "subject-pos in defn metadata must be integer or :last",
                        ))
                    }
                };
                return Ok(Some(pos));
            }
        }
        Ok(None)
    }

    fn parse_doc_and_meta(
        &self,
        items: &[Form],
        start_idx: usize,
        context: &str,
        require_tail: bool,
    ) -> Result<(Option<String>, Option<Form>, usize), CloveError> {
        let mut idx = start_idx;
        let mut doc = None;
        let mut meta_map = None;
        while idx < items.len() {
            if require_tail && idx + 1 >= items.len() {
                break;
            }
            match &items[idx].kind {
                FormKind::String(text) => {
                    if doc.is_some() {
                        return Err(span_runtime_error(
                            items[idx].span,
                            &format!("{context} allows only one docstring"),
                        ));
                    }
                    doc = Some(text.clone());
                    idx += 1;
                }
                FormKind::Map(_) => {
                    if meta_map.is_some() {
                        return Err(span_runtime_error(
                            items[idx].span,
                            &format!("{context} allows only one attr-map"),
                        ));
                    }
                    meta_map = Some(items[idx].clone());
                    idx += 1;
                }
                _ => break,
            }
        }
        Ok((doc, meta_map, idx))
    }

    fn rewrite_defn_body_err_fin(
        &self,
        body: &[Form],
        context: &str,
        form_span: Span,
    ) -> Result<Vec<Form>, CloveError> {
        let err_context = format!("{context} body");
        let tail = parse_err_fin_tail(body, &err_context)
            .map_err(|err| span_runtime_error(err.span, err.message))?;
        if let Some(tail) = tail {
            if tail.body.is_empty() {
                return Err(span_runtime_error(
                    form_span,
                    format!("{context} expects body"),
                ));
            }
        }
        Ok(body.to_vec())
    }

    fn parse_defn_signature(
        &self,
        items: &[Form],
        form_span: Span,
    ) -> Result<DefnSignature, CloveError> {
        self.parse_defn_signature_with_context(items, form_span, "defn")
    }

    fn parse_defn_signature_with_context(
        &self,
        items: &[Form],
        form_span: Span,
        context: &str,
    ) -> Result<DefnSignature, CloveError> {
        if items.is_empty() {
            return Err(span_runtime_error(
                form_span,
                format!("{context} expects params vector or arities"),
            ));
        }
        let (doc, meta_map, mut idx) = self.parse_doc_and_meta(items, 0, context, false)?;
        let mut subject_pos_meta: Option<SubjectPos> = None;
        if let Some(meta_form) = &meta_map {
            subject_pos_meta = self.subject_pos_from_meta_map(meta_form)?;
        }
        if let Some(kind) = items.get(idx).map(|f| &f.kind) {
            let has_return_hint = match kind {
                FormKind::Keyword(_) => true,
                FormKind::Symbol(s) if s.starts_with(':') => true,
                _ => false,
            };
            if has_return_hint {
                idx += 1;
            }
        }
        if idx >= items.len() {
            return Err(span_runtime_error(
                form_span,
                format!("{context} expects params vector or arities"),
            ));
        }
        match &items[idx].kind {
            FormKind::Vector(_) => {
                if idx + 1 >= items.len() {
                    return Err(span_runtime_error(
                        items[idx].span,
                        format!("{context} expects body"),
                    ));
                }
                let params_form = items[idx].clone();
                let body = self.rewrite_defn_body_err_fin(&items[idx + 1..], context, form_span)?;
                let clause = FnClauseSpec::single(params_form.clone(), body.clone())?;
                let mut fn_args = Vec::with_capacity(1 + body.len());
                fn_args.push(params_form);
                fn_args.extend(body);
                Ok(DefnSignature {
                    doc,
                    clauses: vec![clause],
                    fn_args,
                    is_multi: false,
                    meta_map,
                    subject_pos_meta,
                })
            }
            FormKind::List(_) => {
                let clause_forms: Vec<Form> = items[idx..].to_vec();
                if clause_forms.is_empty() {
                    return Err(span_runtime_error(
                        form_span,
                        format!("{context} expects at least one arity"),
                    ));
                }
                let mut clauses = Vec::with_capacity(clause_forms.len());
                for form in &clause_forms {
                    let mut clause = FnClauseSpec::from_clause_form(form)?;
                    clause.body =
                        self.rewrite_defn_body_err_fin(&clause.body, context, form_span)?;
                    clauses.push(clause);
                }
                Ok(DefnSignature {
                    doc,
                    clauses,
                    fn_args: clause_forms,
                    is_multi: true,
                    meta_map,
                    subject_pos_meta,
                })
            }
            _ => Err(span_runtime_error(
                args_span(items, form_span),
                format!("{context} expects params vector or arities"),
            )),
        }
    }

    fn build_fn_meta_for_defn(
        &self,
        env: &EnvRef,
        name_form: &Form,
        params_form: &Form,
        body: &[Form],
        doc: Option<String>,
        qualified_name: Option<&str>,
        subject_pos: Option<SubjectPos>,
    ) -> Option<(FnMeta, Option<Vec<Form>>, MetaTypeKind)> {
        let (ns, local) = resolve_fn_namespace(env, name_form, qualified_name)?;
        let parsed = self.parse_params(params_form).ok()?;
        let wrapped_body = self.wrap_param_destructuring(params_form, &parsed, body);
        let param_names = parsed.params.clone();
        let rest_param = parsed.rest.clone();
        let (param_hints, rest_hint) = collect_param_type_hints(params_form);
        let rest_hint_for_param = rest_hint.clone();
        let mut param_infos: Vec<ParamType> = param_names
            .iter()
            .enumerate()
            .map(|(idx, name)| ParamType {
                name: name.clone(),
                hint: param_hints.get(idx).cloned().flatten(),
            })
            .collect();
        if let Some(rest_name) = rest_param {
            let hint = Some(rest_type_from_hint(rest_hint_for_param));
            param_infos.push(ParamType {
                name: rest_name,
                hint,
            });
        }
        let analysis = ir::analyze_fn_body(&param_infos, &wrapped_body);
        let inferred_ret = analysis.ret_type;
        let ret_type = return_hint_from_form(params_form)
            .or_else(|| type_hint_from_form(name_form))
            .unwrap_or(inferred_ret.clone());
        let rest_type = parsed
            .rest
            .as_ref()
            .map(|_| rest_type_from_hint(rest_hint.clone()));
        let lambda_fn_type = MetaTypeKind::function(
            param_names
                .iter()
                .enumerate()
                .map(|(idx, _)| {
                    param_hints
                        .get(idx)
                        .cloned()
                        .flatten()
                        .unwrap_or_else(MetaTypeKind::any)
                })
                .collect(),
            rest_type.clone(),
            ret_type.clone(),
        );
        let optimized_body = ir::ir_exprs_to_forms(&analysis.body);
        let arg_types = param_names
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                param_hints
                    .get(idx)
                    .cloned()
                    .flatten()
                    .unwrap_or_else(MetaTypeKind::any)
            })
            .collect();
        let overload = FnOverload {
            arg_types,
            rest: rest_type,
            ret_type,
            special_op: None,
        };
        let mut meta = FnMeta::new(ns, local);
        meta.doc = doc;
        meta.arglist.push(form_to_string(params_form, ""));
        meta.overloads.push(overload);
        meta.source_file = current_file_name();
        meta.source_span = Some(name_form.span);
        meta.subject_pos = subject_pos;
        Some((meta, optimized_body, lambda_fn_type))
    }

    fn build_multi_fn_meta(
        &self,
        env: &EnvRef,
        name_form: &Form,
        clauses: &[FnClauseSpec],
        doc: Option<String>,
        qualified_name: Option<&str>,
        subject_pos: Option<SubjectPos>,
    ) -> Option<FnMeta> {
        let (ns, local) = resolve_fn_namespace(env, name_form, qualified_name)?;
        let mut meta = FnMeta::new(ns, local);
        meta.doc = doc;
        meta.source_file = current_file_name();
        meta.source_span = Some(name_form.span);
        meta.subject_pos = subject_pos;
        for clause in clauses {
            meta.arglist.push(form_to_string(&clause.params_form, ""));
            let (param_hints, rest_hint) = collect_param_type_hints(&clause.params_form);
            let arg_types: Vec<MetaTypeKind> = param_hints
                .into_iter()
                .map(|hint| hint.unwrap_or_else(MetaTypeKind::any))
                .collect();
            let rest = self
                .parse_params(&clause.params_form)
                .ok()
                .and_then(|parsed| parsed.rest)
                .map(|_| rest_type_from_hint(rest_hint));
            let ret_type =
                return_hint_from_form(&clause.params_form).unwrap_or_else(MetaTypeKind::any);
            meta.overloads.push(FnOverload {
                arg_types,
                rest,
                ret_type,
                special_op: None,
            });
        }
        Some(meta)
    }

    fn try_expand_oop_form(&self, form: &Form, env: EnvRef) -> Result<Option<Form>, CloveError> {
        let items = match &form.kind {
            FormKind::List(items) => items,
            _ => return Ok(None),
        };
        if items.is_empty() {
            return Ok(None);
        }
        let (base_start, stage_start, had_wrapper) = match &items[0].kind {
            FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM => (1, 2, false),
            FormKind::Symbol(sym)
                if self.call_wrappers.contains(sym.as_str())
                    && matches!(
                        items.get(1).map(|f| &f.kind),
                        Some(FormKind::Symbol(f)) if f == OOP_SYNTAX_SYM
                    ) =>
            {
                (2, 3, true)
            }
            _ => return Ok(None),
        };
        if !self.oop_syntax_enabled(&env) {
            return Err(span_runtime_error(
                form.span,
                "oop-syntax is disabled; enable it via (use oop-syntax true)",
            ));
        }
        if items.len() < stage_start {
            return Err(span_runtime_error(
                form.span,
                "clove.syntax::oop expects base expression and at least one method stage",
            ));
        }
        let mut current = items
            .get(base_start)
            .cloned()
            .ok_or_else(|| span_runtime_error(form.span, "oop form missing base expression"))?;
        let mut nil_safe_active = false;
        let mut nil_safe_stages: Vec<Form> = Vec::new();
        let mut lexical_binds: Vec<(Form, Form, Span)> = Vec::new();
        let mut oop_let_counter = 0usize;
        let mut processed_stages = 0usize;
        for stage in &items[stage_start..] {
            if self.is_oop_nil_safe_marker(stage) {
                if !nil_safe_active {
                    if processed_stages == 0 {
                        let (next_base, start_nil_safe) =
                            self.resolve_oop_nil_safe_start(current, &env, stage.span)?;
                        current = next_base;
                        nil_safe_active = start_nil_safe;
                    } else {
                        nil_safe_active = true;
                    }
                }
                continue;
            }
            if nil_safe_active {
                nil_safe_stages.push(stage.clone());
                continue;
            }
            if let Some((repl_name, sym_name)) = self.parse_oop_repl_stash_stage(stage)? {
                let tmp_name = format!("__oop_let{}_{}", form.span.index, oop_let_counter);
                oop_let_counter += 1;
                let stashed =
                    self.build_oop_let_stash_form(stage.span, &sym_name, current, tmp_name);
                current = self.build_oop_repl_call(stage.span, &repl_name, Vec::new(), stashed)?;
                processed_stages += 1;
                continue;
            }
            if let Some(sym) = self.parse_oop_as_stage(stage)? {
                let previous = current;
                lexical_binds.push((sym.clone(), previous, stage.span));
                current = sym;
                processed_stages += 1;
                continue;
            }
            if let Some(sym_name) = self.parse_oop_let_stage(stage)? {
                let tmp_name = format!("__oop_let{}_{}", form.span.index, oop_let_counter);
                oop_let_counter += 1;
                current = self.build_oop_let_stash_form(stage.span, &sym_name, current, tmp_name);
                processed_stages += 1;
                continue;
            }
            current = self.build_oop_stage_call(stage, current, &env, form.span)?;
            processed_stages += 1;
        }
        if nil_safe_active {
            if nil_safe_stages.is_empty() {
                return Err(span_runtime_error(
                    form.span,
                    "nil-safe chain requires at least one stage",
                ));
            }
            current = self.build_oop_nil_safe_chain(current, &nil_safe_stages, &env, form.span)?;
        }
        for (sym, value, span) in lexical_binds.into_iter().rev() {
            let binding_vec = Form::new(FormKind::Vector(vec![sym, value]), span);
            current = Form::new(
                FormKind::List(vec![
                    Form::new(FormKind::Symbol("let".into()), span),
                    binding_vec,
                    current,
                ]),
                span,
            );
        }
        let result = if had_wrapper {
            match &current.kind {
                FormKind::List(parts) => {
                    let is_let = matches!(
                        parts.first().map(|f| &f.kind),
                        Some(FormKind::Symbol(sym)) if canonical_symbol_name(sym).as_ref() == "let"
                    );
                    let is_internal = matches!(
                        parts.first().map(|f| &f.kind),
                        Some(FormKind::Symbol(sym))
                            if sym == OOP_INDEX_SYM
                                || sym == OOP_SEG_SYM
                                || sym == OOP_METHOD_SYM
                    );
                    if is_let || is_internal {
                        current
                    } else {
                        let mut wrapped = Vec::with_capacity(parts.len() + 1);
                        wrapped.push(items[0].clone());
                        wrapped.extend(parts.iter().cloned());
                        Form::new(FormKind::List(wrapped), form.span)
                    }
                }
                _ => current,
            }
        } else {
            current
        };
        Ok(Some(result))
    }

    fn try_expand_infix_form(&self, form: &Form) -> Result<Option<Form>, CloveError> {
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
        if head != INFIX_SYNTAX_SYM {
            return Ok(None);
        }
        if items.len() != 2 {
            return Err(span_runtime_error(
                form.span,
                "clove.syntax::infix expects exactly one expression",
            ));
        }
        let lowered = self.lower_infix_expr(&items[1])?;
        Ok(Some(lowered))
    }

    fn lower_infix_expr(&self, form: &Form) -> Result<Form, CloveError> {
        let with_kind = |kind| Form {
            kind,
            span: form.span,
            type_hint: form.type_hint.clone(),
        };
        match &form.kind {
            FormKind::List(items) => {
                if items.is_empty() {
                    return Ok(form.clone());
                }
                let head = match &items[0].kind {
                    FormKind::Symbol(sym) => sym.as_str(),
                    _ => "",
                };
                if matches!(head, "==" | "=") && items.len() == 3 {
                    let lhs = self.lower_infix_expr(&items[1])?;
                    let rhs = self.lower_infix_expr(&items[2])?;
                    let mut new_items = Vec::with_capacity(3);
                    new_items.push(Form::new(FormKind::Symbol("=".into()), items[0].span));
                    new_items.push(lhs);
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                if matches!(head, "!=" | "not=") && items.len() == 3 {
                    let lhs = self.lower_infix_expr(&items[1])?;
                    let rhs = self.lower_infix_expr(&items[2])?;
                    let mut new_items = Vec::with_capacity(3);
                    new_items.push(Form::new(FormKind::Symbol("not=".into()), items[0].span));
                    new_items.push(lhs);
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                if head == "%" && items.len() == 3 {
                    let lhs = self.lower_infix_expr(&items[1])?;
                    let rhs = self.lower_infix_expr(&items[2])?;
                    let mut new_items = Vec::with_capacity(3);
                    new_items.push(Form::new(FormKind::Symbol("mod".into()), items[0].span));
                    new_items.push(lhs);
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                if head == "&&" && items.len() == 3 {
                    let lhs = self.lower_infix_expr(&items[1])?;
                    let rhs = self.lower_infix_expr(&items[2])?;
                    let mut new_items = Vec::with_capacity(3);
                    new_items.push(Form::new(FormKind::Symbol("and".into()), items[0].span));
                    new_items.push(lhs);
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                if head == "||" && items.len() == 3 {
                    let lhs = self.lower_infix_expr(&items[1])?;
                    let rhs = self.lower_infix_expr(&items[2])?;
                    let mut new_items = Vec::with_capacity(3);
                    new_items.push(Form::new(FormKind::Symbol("or".into()), items[0].span));
                    new_items.push(lhs);
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                if head == "!" && items.len() == 2 {
                    let rhs = self.lower_infix_expr(&items[1])?;
                    let mut new_items = Vec::with_capacity(2);
                    new_items.push(Form::new(FormKind::Symbol("not".into()), items[0].span));
                    new_items.push(rhs);
                    return Ok(with_kind(FormKind::List(new_items)));
                }
                let mut lowered_items = Vec::with_capacity(items.len());
                for item in items {
                    lowered_items.push(self.lower_infix_expr(item)?);
                }
                Ok(with_kind(FormKind::List(lowered_items)))
            }
            FormKind::Vector(items) => {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_infix_expr(item)?);
                }
                Ok(with_kind(FormKind::Vector(lowered)))
            }
            FormKind::Set(items) => {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_infix_expr(item)?);
                }
                Ok(with_kind(FormKind::Set(lowered)))
            }
            FormKind::Map(entries) => {
                let mut lowered = Vec::with_capacity(entries.len());
                for entry in entries {
                    match entry {
                        MapItem::KeyValue(k, v) => {
                            let key = self.lower_infix_expr(k)?;
                            let value = self.lower_infix_expr(v)?;
                            lowered.push(MapItem::KeyValue(key, value));
                        }
                        MapItem::Spread(expr) => {
                            let value = self.lower_infix_expr(expr)?;
                            lowered.push(MapItem::Spread(value));
                        }
                    }
                }
                Ok(with_kind(FormKind::Map(lowered)))
            }
            FormKind::InterpolatedString(parts) => {
                let mut lowered = Vec::with_capacity(parts.len());
                for part in parts {
                    match part {
                        InterpolatedPart::Text(text) => {
                            lowered.push(InterpolatedPart::Text(text.clone()));
                        }
                        InterpolatedPart::Expr(expr) => {
                            lowered.push(InterpolatedPart::Expr(self.lower_infix_expr(expr)?));
                        }
                    }
                }
                Ok(with_kind(FormKind::InterpolatedString(lowered)))
            }
            FormKind::InterpolatedRegex { parts, delim } => {
                let mut lowered = Vec::with_capacity(parts.len());
                for part in parts {
                    match part {
                        InterpolatedPart::Text(text) => {
                            lowered.push(InterpolatedPart::Text(text.clone()));
                        }
                        InterpolatedPart::Expr(expr) => {
                            lowered.push(InterpolatedPart::Expr(self.lower_infix_expr(expr)?));
                        }
                    }
                }
                Ok(with_kind(FormKind::InterpolatedRegex {
                    parts: lowered,
                    delim: *delim,
                }))
            }
            FormKind::ShortFn(items) => {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_infix_expr(item)?);
                }
                Ok(with_kind(FormKind::ShortFn(lowered)))
            }
            _ => Ok(form.clone()),
        }
    }

    fn try_expand_oop_apply_form(
        &self,
        form: &Form,
        env: EnvRef,
    ) -> Result<Option<Form>, CloveError> {
        let FormKind::List(items) = &form.kind else {
            return Ok(None);
        };
        if items.len() < 2 {
            return Ok(None);
        }
        if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == APPLY_SYM) {
            return Ok(None);
        }
        let target = &items[1];
        let extra_args = &items[2..];
        let Some(expanded) = self.try_expand_oop_form(target, env)? else {
            return Ok(None);
        };
        if extra_args.is_empty() {
            return Ok(Some(expanded));
        }
        let FormKind::List(expanded_items) = &expanded.kind else {
            return Ok(None);
        };
        if expanded_items.len() != 3 {
            return Ok(None);
        }
        if !matches!(
            expanded_items.first().map(|item| &item.kind),
            Some(FormKind::Symbol(sym)) if sym == OOP_METHOD_SYM
        ) {
            return Ok(None);
        }
        let mut items_out = Vec::with_capacity(expanded_items.len() + extra_args.len());
        items_out.extend(expanded_items.iter().cloned());
        items_out.extend(extra_args.iter().cloned());
        Ok(Some(Form::new(FormKind::List(items_out), form.span)))
    }

    fn try_expand_oop_head_form(
        &self,
        form: &Form,
        env: EnvRef,
    ) -> Result<Option<Form>, CloveError> {
        let FormKind::List(items) = &form.kind else {
            return Ok(None);
        };
        if items.is_empty() {
            return Ok(None);
        }
        let head = &items[0];
        let args = &items[1..];
        let Some(expanded) = self.try_expand_oop_form(head, env)? else {
            return Ok(None);
        };
        let FormKind::List(expanded_items) = &expanded.kind else {
            return Ok(None);
        };
        if expanded_items.len() != 3 {
            return Ok(None);
        }
        if !matches!(
            expanded_items.first().map(|item| &item.kind),
            Some(FormKind::Symbol(sym)) if sym == OOP_METHOD_SYM
        ) {
            return Ok(None);
        }
        let mut items_out = Vec::with_capacity(expanded_items.len() + args.len());
        items_out.extend(expanded_items.iter().cloned());
        items_out.extend(args.iter().cloned());
        Ok(Some(Form::new(FormKind::List(items_out), form.span)))
    }

    fn parse_oop_as_stage(&self, stage: &Form) -> Result<Option<Form>, CloveError> {
        let FormKind::List(items) = &stage.kind else {
            return Ok(None);
        };
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.len() != 2 {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok(None),
        };
        if head != OOP_AS_SYM {
            return Ok(None);
        }
        let sym = match &items[1].kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                return Err(span_runtime_error(
                    stage.span,
                    "oop as stage expects symbol",
                ));
            }
        };
        Ok(Some(Form::new(FormKind::Symbol(sym), items[1].span)))
    }

    fn parse_oop_let_stage(&self, stage: &Form) -> Result<Option<String>, CloveError> {
        let FormKind::List(items) = &stage.kind else {
            return Ok(None);
        };
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.len() != 2 {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok(None),
        };
        if head != OOP_LET_SYM {
            return Ok(None);
        }
        let sym = match &items[1].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => {
                return Err(span_runtime_error(
                    stage.span,
                    "oop let stage expects symbol",
                ));
            }
        };
        if sym.starts_with('*') {
            return Err(span_runtime_error(
                stage.span,
                "oop let stage expects non-star symbol",
            ));
        }
        Ok(Some(sym.to_string()))
    }

    fn parse_oop_repl_stash_stage(
        &self,
        stage: &Form,
    ) -> Result<Option<(String, String)>, CloveError> {
        let FormKind::List(items) = &stage.kind else {
            return Ok(None);
        };
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.len() != 2 {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok(None),
        };
        if head != "repl" && head != "debug" {
            return Ok(None);
        }
        let sym = match &items[1].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Ok(None),
        };
        if sym == "?" || sym == "*?" || sym.starts_with('*') {
            return Ok(None);
        }
        Ok(Some((head.to_string(), sym.to_string())))
    }

    fn build_oop_let_stash_form(
        &self,
        span: Span,
        name: &str,
        base: Form,
        tmp_name: String,
    ) -> Form {
        let stash = format!("*{}", canonical_symbol_name(name));
        let tmp_sym = Form::new(FormKind::Symbol(tmp_name.clone()), span);
        let binding_vec = Form::new(
            FormKind::Vector(vec![Form::new(FormKind::Symbol(tmp_name), span), base]),
            span,
        );
        let def_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("__set-in-chain".into()), span),
                Form::new(FormKind::Symbol(stash), span),
                tmp_sym.clone(),
            ]),
            span,
        );
        Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("let".into()), span),
                binding_vec,
                def_form,
                tmp_sym,
            ]),
            span,
        )
    }

    fn parse_oop_dot_stage(&self, stage: &Form) -> Result<Option<(Form, Form)>, CloveError> {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => return Ok(None),
        };
        if items.is_empty() {
            return Ok(None);
        }
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.is_empty() {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) if sym == OOP_DOT_STAGE_SYM => sym,
            _ => return Ok(None),
        };
        if items.len() != 3 {
            let msg = format!("{} stage must have placeholder symbol and body form", head);
            return Err(span_runtime_error(stage.span, msg));
        }
        let placeholder = items[1].clone();
        if !matches!(placeholder.kind, FormKind::Symbol(_)) {
            let msg = format!("{} stage placeholder must be symbol", head);
            return Err(span_runtime_error(stage.span, msg));
        }
        Ok(Some((placeholder, items[2].clone())))
    }

    fn parse_oop_index_stage(&self, stage: &Form) -> Result<Option<Form>, CloveError> {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => return Ok(None),
        };
        if items.is_empty() {
            return Ok(None);
        }
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.is_empty() {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) if sym == OOP_INDEX_SYM => sym,
            _ => return Ok(None),
        };
        if items.len() != 2 {
            let msg = format!("{} stage must have key form", head);
            return Err(span_runtime_error(stage.span, msg));
        }
        Ok(Some(items[1].clone()))
    }

    fn parse_oop_bare_stage(&self, stage: &Form) -> Result<Option<Form>, CloveError> {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => return Ok(None),
        };
        if items.is_empty() {
            return Ok(None);
        }
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        if items.is_empty() {
            return Ok(None);
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) if sym == OOP_BARE_SYM => sym,
            _ => return Ok(None),
        };
        if items.len() != 2 {
            let msg = format!("{} stage must have method symbol", head);
            return Err(span_runtime_error(stage.span, msg));
        }
        if !matches!(items[1].kind, FormKind::Symbol(_)) {
            return Err(span_runtime_error(stage.span, "oop method must be symbol"));
        }
        Ok(Some(items[1].clone()))
    }

    fn unwrap_oop_stage_items<'a>(&self, items: &'a [Form]) -> (&'a [Form], bool) {
        if items.len() >= 2 {
            if let FormKind::Symbol(sym) = &items[0].kind {
                if self.call_wrappers.contains(sym.as_str()) {
                    return (&items[1..], true);
                }
            }
        }
        (items, false)
    }

    fn build_oop_dot_call(
        &self,
        span: Span,
        placeholder: Form,
        body: Form,
        base: Form,
    ) -> Result<Form, CloveError> {
        if !matches!(placeholder.kind, FormKind::Symbol(_)) {
            return Err(span_runtime_error(
                span,
                "oop dot stage placeholder must be symbol",
            ));
        }
        let mut bind_items = Vec::with_capacity(2);
        bind_items.push(placeholder);
        bind_items.push(base);
        let let_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("let".into()), span),
                Form::new(FormKind::Vector(bind_items), span),
                body,
            ]),
            span,
        );
        Ok(let_form)
    }

    fn build_oop_index_call(&self, span: Span, key: Form, base: Form) -> Result<Form, CloveError> {
        let items = vec![
            Form::new(FormKind::Symbol(OOP_INDEX_SYM.to_string()), span),
            base,
            key,
        ];
        Ok(Form::new(FormKind::List(items), span))
    }

    fn parse_oop_stage(&self, stage: &Form) -> Result<(Form, Vec<Form>), CloveError> {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => {
                return Err(span_runtime_error(
                    stage.span,
                    "oop method stage must be a list",
                ))
            }
        };
        if items.is_empty() {
            return Err(span_runtime_error(
                stage.span,
                "oop method stage requires method name",
            ));
        }
        let (method, args_start) = if matches!(&items[0].kind, FormKind::Symbol(sym) if self
            .call_wrappers
            .contains(sym.as_str()))
        {
            if items.len() < 2 {
                return Err(span_runtime_error(
                    stage.span,
                    "oop method stage requires method name",
                ));
            }
            (items[1].clone(), 2)
        } else {
            (items[0].clone(), 1)
        };
        if !matches!(method.kind, FormKind::Symbol(_)) {
            return Err(span_runtime_error(method.span, "oop method must be symbol"));
        }
        Ok((method, items[args_start..].to_vec()))
    }

    fn build_oop_call(
        &self,
        span: Span,
        method: Form,
        args: Vec<Form>,
        base: Form,
    ) -> Result<Form, CloveError> {
        let method_name = match &method.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => return Err(span_runtime_error(method.span, "oop method must be symbol")),
        };
        let mut items = Vec::with_capacity(args.len() + 3);
        items.push(Form::new(
            FormKind::Symbol(OOP_METHOD_SYM.to_string()),
            span,
        ));
        items.push(base);
        items.push(Form::new(FormKind::String(method_name), span));
        items.extend(args);
        Ok(Form::new(FormKind::List(items), span))
    }

    fn is_oop_nil_safe_marker(&self, stage: &Form) -> bool {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => return false,
        };
        let (items, _wrapped) = self.unwrap_oop_stage_items(items);
        items.len() == 1
            && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM)
    }

    fn resolve_oop_nil_safe_start(
        &self,
        base: Form,
        env: &EnvRef,
        span: Span,
    ) -> Result<(Form, bool), CloveError> {
        let FormKind::Symbol(sym) = &base.kind else {
            return Ok((base, true));
        };
        if sym.ends_with('?') {
            let fallback = format!("{}?", sym);
            let base_defined = self.lookup_symbol_value(sym, env).is_some();
            let fallback_defined = self.lookup_symbol_value(&fallback, env).is_some();
            if base_defined && fallback_defined {
                let msg = format!(
                    "nil-safe receiver '{}' is ambiguous; '{}' and '{}' are both defined",
                    sym, sym, fallback
                );
                return Err(span_runtime_error(span, msg));
            }
            if base_defined {
                return Ok((base, true));
            }
            let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
            return Ok((fallback_form, false));
        }
        let fallback = format!("{}?", sym);
        if self.lookup_symbol_value(&fallback, env).is_some() {
            let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
            return Ok((fallback_form, false));
        }
        Ok((base, true))
    }

    fn build_oop_repl_call(
        &self,
        span: Span,
        name: &str,
        mut args: Vec<Form>,
        base: Form,
    ) -> Result<Form, CloveError> {
        let mut items = Vec::with_capacity(args.len() + 2);
        items.push(Form::new(FormKind::Symbol(name.to_string()), span));
        if args.is_empty() {
            items.push(base);
        } else {
            items.append(&mut args);
        }
        Ok(Form::new(FormKind::List(items), span))
    }

    fn build_oop_stage_call(
        &self,
        stage: &Form,
        base: Form,
        env: &EnvRef,
        form_span: Span,
    ) -> Result<Form, CloveError> {
        if let Some((placeholder, body)) = self.parse_oop_dot_stage(stage)? {
            return self.build_oop_dot_call(stage.span, placeholder, body, base);
        }
        if let Some(key_form) = self.parse_oop_index_stage(stage)? {
            if !self.dot_indexer_enabled(env) {
                return Err(span_runtime_error(
                    form_span,
                    "dot-indexer syntax is disabled; enable it via (use dot-indexer true)",
                ));
            }
            return self.build_oop_index_call(stage.span, key_form, base);
        }
        if let Some(method_form) = self.parse_oop_bare_stage(stage)? {
            if let FormKind::Symbol(sym) = &method_form.kind {
                if sym == "repl" || sym == "debug" {
                    return self.build_oop_repl_call(stage.span, sym, Vec::new(), base);
                }
            }
            return self.build_oop_call(stage.span, method_form, Vec::new(), base);
        }
        let (method_form, args) = self.parse_oop_stage(stage)?;
        let method_name = match &method_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => unreachable!(),
        };
        if method_name == "repl" || method_name == "debug" {
            return self.build_oop_repl_call(stage.span, &method_name, args, base);
        }
        if method_name == OOP_DOT_STAGE_SYM {
            if args.len() != 2 {
                let msg = format!(
                    "{} stage must have placeholder symbol and body form",
                    OOP_DOT_STAGE_SYM
                );
                return Err(span_runtime_error(stage.span, msg));
            }
            return self.build_oop_dot_call(stage.span, args[0].clone(), args[1].clone(), base);
        }
        if method_name == OOP_INDEX_SYM {
            if args.len() != 1 {
                let msg = format!("{} stage must have key form", OOP_INDEX_SYM);
                return Err(span_runtime_error(stage.span, msg));
            }
            if !self.dot_indexer_enabled(env) {
                return Err(span_runtime_error(
                    form_span,
                    "dot-indexer syntax is disabled; enable it via (use dot-indexer true)",
                ));
            }
            return self.build_oop_index_call(stage.span, args[0].clone(), base);
        }
        self.build_oop_call(stage.span, method_form, args, base)
    }

    fn build_oop_nil_safe_chain(
        &self,
        base: Form,
        stages: &[Form],
        env: &EnvRef,
        form_span: Span,
    ) -> Result<Form, CloveError> {
        self.build_oop_nil_safe_chain_step(base, stages, env, form_span, 0)
    }

    fn build_oop_nil_safe_chain_step(
        &self,
        base: Form,
        stages: &[Form],
        env: &EnvRef,
        form_span: Span,
        idx: usize,
    ) -> Result<Form, CloveError> {
        let Some((stage, rest)) = stages.split_first() else {
            return Ok(base);
        };
        let tmp_name = format!("__oop_nil_safe{}_{}", form_span.index, idx);
        let tmp_sym = Form::new(FormKind::Symbol(tmp_name.clone()), stage.span);
        let next_form = if let Some(sym) = self.parse_oop_as_stage(stage)? {
            let rest_form = if rest.is_empty() {
                sym.clone()
            } else {
                self.build_oop_nil_safe_chain_step(sym.clone(), rest, env, form_span, idx + 1)?
            };
            let binding_vec = Form::new(
                FormKind::Vector(vec![sym.clone(), tmp_sym.clone()]),
                stage.span,
            );
            Form::new(
                FormKind::List(vec![
                    Form::new(FormKind::Symbol("let".into()), stage.span),
                    binding_vec,
                    rest_form,
                ]),
                stage.span,
            )
        } else {
            let stage_form = if let Some((repl_name, sym_name)) =
                self.parse_oop_repl_stash_stage(stage)?
            {
                let tmp_let = format!("__oop_let{}_{}", form_span.index, idx);
                let stashed =
                    self.build_oop_let_stash_form(stage.span, &sym_name, tmp_sym.clone(), tmp_let);
                self.build_oop_repl_call(stage.span, &repl_name, Vec::new(), stashed)?
            } else if let Some(sym_name) = self.parse_oop_let_stage(stage)? {
                let tmp_let = format!("__oop_let{}_{}", form_span.index, idx);
                self.build_oop_let_stash_form(stage.span, &sym_name, tmp_sym.clone(), tmp_let)
            } else {
                self.build_oop_stage_call(stage, tmp_sym.clone(), env, form_span)?
            };
            if rest.is_empty() {
                stage_form
            } else {
                self.build_oop_nil_safe_chain_step(stage_form, rest, env, form_span, idx + 1)?
            }
        };
        let nil_check = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("nil?".into()), stage.span),
                tmp_sym,
            ]),
            stage.span,
        );
        let if_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("if".into()), stage.span),
                nil_check,
                Form::new(FormKind::Nil, stage.span),
                next_form,
            ]),
            stage.span,
        );
        let binding_vec = Form::new(
            FormKind::Vector(vec![
                Form::new(FormKind::Symbol(tmp_name), stage.span),
                base,
            ]),
            stage.span,
        );
        Ok(Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("let".into()), stage.span),
                binding_vec,
                if_form,
            ]),
            stage.span,
        ))
    }

    fn materialize_subject_pos(
        &self,
        policy: SubjectPos,
        args_len: usize,
        span: Span,
    ) -> Result<usize, CloveError> {
        let pos = match policy {
            SubjectPos::Fixed(n) => n,
            SubjectPos::Last => args_len + 1,
        };
        if pos == 0 {
            return Err(span_runtime_error(span, "subject-pos must be at least 1"));
        }
        Ok(pos)
    }

    fn resolve_subject_pos(
        &self,
        method_name: &str,
        args_len: usize,
        span: Span,
        env: &EnvRef,
    ) -> Result<SubjectPos, CloveError> {
        if let Some(meta) = fn_meta::get(method_name) {
            return self.subject_pos_from_meta(method_name, args_len, span, &meta);
        }
        if let Some(value) = self.lookup_symbol_value(method_name, env) {
            if let Some(meta) = self.fn_meta_from_value(&value) {
                return self.subject_pos_from_meta(method_name, args_len, span, &meta);
            }
        }
        Ok(SubjectPos::Fixed(1))
    }

    fn subject_pos_from_meta(
        &self,
        method_name: &str,
        args_len: usize,
        span: Span,
        meta: &FnMeta,
    ) -> Result<SubjectPos, CloveError> {
        if meta.ns == "std" {
            let core_name = format!("core::{}", meta.name);
            if let Some(core_meta) = fn_meta::get(&core_name) {
                if let Some(pos) = core_meta
                    .subject_pos
                    .clone()
                    .or_else(|| self.infer_subject_pos_from_arglists(&core_meta.arglist))
                {
                    return Ok(pos);
                }
            }
        }
        if let Some(pos) = meta.subject_pos.clone() {
            return Ok(pos);
        }
        if let Some(pos) = self.infer_subject_pos_from_arglists(&meta.arglist) {
            return Ok(pos);
        }
        if args_len == 0 && self.fn_meta_supports_arity(meta, 1) {
            return Ok(SubjectPos::Fixed(1));
        }
        let msg = format!(
            "cannot determine subject position for method '{}'; add {{:subject-pos N}} or use $ marker",
            method_name
        );
        Err(span_runtime_error(span, msg))
    }

    fn fn_meta_from_value(&self, value: &Value) -> Option<FnMeta> {
        match value {
            Value::Func(func) => func
                .debug_name()
                .and_then(|name| self.fn_meta_from_name(&name)),
            Value::Lambda {
                name: Some(name), ..
            }
            | Value::MultiLambda {
                name: Some(name), ..
            } => self.fn_meta_from_name(name),
            _ => None,
        }
    }

    fn fn_meta_from_name(&self, name: &str) -> Option<FnMeta> {
        if let Some(meta) = fn_meta::get(name) {
            return Some(meta);
        }
        if !name.contains("::") {
            for ns in ["core", "std"] {
                let qualified = format!("{ns}::{name}");
                if let Some(meta) = fn_meta::get(&qualified) {
                    return Some(meta);
                }
            }
        }
        None
    }

    fn infer_subject_pos_from_arglists(&self, arglists: &[String]) -> Option<SubjectPos> {
        let mut parsed = Vec::with_capacity(arglists.len());
        for arglist in arglists {
            let Some(params) = self.parse_arglist_string(arglist) else {
                return None;
            };
            parsed.push(params);
        }
        self.infer_subject_pos_from_parsed_params(&parsed)
    }

    fn infer_subject_pos_from_parsed_params(
        &self,
        parsed_list: &[ParsedParams],
    ) -> Option<SubjectPos> {
        if parsed_list.is_empty() {
            return None;
        }
        let mut positions = Vec::with_capacity(parsed_list.len());
        let mut all_last = true;
        for parsed in parsed_list {
            let pos = parsed
                .subject_pos
                .or_else(|| infer_subject_pos_from_names(&parsed.params))?;
            positions.push(pos);
            if parsed.rest.is_some() || pos != parsed.params.len() {
                all_last = false;
            }
        }
        let first = positions[0];
        if positions.iter().all(|p| *p == first) {
            return Some(SubjectPos::Fixed(first));
        }
        if all_last {
            return Some(SubjectPos::Last);
        }
        None
    }

    fn fn_meta_supports_arity(&self, meta: &FnMeta, target: usize) -> bool {
        if meta.overloads.iter().any(|o| {
            if target < o.arg_types.len() {
                false
            } else if target == o.arg_types.len() {
                true
            } else {
                o.rest.is_some()
            }
        }) {
            return true;
        }
        meta.arglist
            .iter()
            .filter_map(|a| self.parse_arglist_string(a))
            .any(|parsed| {
                let base = parsed.params.len();
                let total = if parsed.rest.is_some() {
                    base + 1
                } else {
                    base
                };
                total == target
            })
    }

    fn parse_arglist_string(&self, arglist: &str) -> Option<ParsedParams> {
        let mut reader = Reader::new(arglist);
        let forms = reader.read_all().ok()?;
        let forms = normalize_type_syntax_forms(forms, false).ok()?;
        let params_form = forms
            .iter()
            .find(|form| matches!(form.kind, FormKind::Vector(_)))?;
        self.parse_params(params_form).ok()
    }

    fn eval_short_fn(&self, body: &[Form], span: Span, env: EnvRef) -> Result<Value, CloveError> {
        let (mapping, body_form) = build_short_fn_body(body, span);
        let mut param_forms: Vec<Form> = mapping
            .args
            .iter()
            .map(|arg| Form::new(FormKind::Symbol(arg.clone()), span))
            .collect();
        if let Some(rest) = &mapping.rest {
            param_forms.push(Form::new(FormKind::Symbol("&".into()), span));
            param_forms.push(Form::new(FormKind::Symbol(rest.clone()), span));
        }
        let params_form = Form::new(FormKind::Vector(param_forms), span);
        let parsed = ParsedParams {
            params: mapping.args.clone(),
            rest: mapping.rest.clone(),
            destructure_bindings: Vec::new(),
            subject_pos: None,
        };
        let body = vec![body_form];
        let (local_defns, body_forms) = self.collect_local_defns(&body)?;
        ensure_recur_tail_positions(&body_forms)?;
        let inferred_type = self.infer_lambda_type_kind(&params_form, &parsed, &body_forms);
        let recur_id = LOOP_COUNTER.fetch_add(1, Ordering::SeqCst);
        let source_meta = build_lambda_source_meta();
        Ok(Value::Lambda {
            params: mapping.args,
            rest: mapping.rest,
            body: body_forms,
            local_defns,
            env: env.clone(),
            engines: self.engines.clone(),
            auto_fallback: self.auto_fallback,
            call_wrappers: self.call_wrappers(),
            settings: self.settings.clone(),
            meta: source_meta,
            doc: None,
            name: None,
            inferred_type: Some(inferred_type),
            recur_id,
        })
    }

    fn parse_params(&self, params_form: &Form) -> Result<ParsedParams, CloveError> {
        let mut params = Vec::new();
        let mut rest = None;
        let mut destructure_bindings = Vec::new();
        let mut subject_pos = None;
        match &params_form.kind {
            FormKind::Vector(vs) => {
                let mut idx = 0;
                while idx < vs.len() {
                    let current = &vs[idx];
                    if matches!(&current.kind, FormKind::Symbol(sym) if sym == "&") {
                        let rest_form = vs.get(idx + 1).ok_or_else(|| {
                            span_runtime_error(current.span, "& must be followed by a binding form")
                        })?;
                        if matches!(&rest_form.kind, FormKind::Symbol(sym) if sym.starts_with('$'))
                        {
                            return Err(span_runtime_error(
                                rest_form.span,
                                "rest parameter does not support '$' subject marker",
                            ));
                        }
                        let (name, binding) = parse_param_binding(rest_form, params.len(), true)?;
                        if idx + 2 < vs.len() {
                            return Err(span_runtime_error(
                                current.span,
                                "only one rest param allowed",
                            ));
                        }
                        rest = Some(name);
                        if let Some(pair) = binding {
                            destructure_bindings.push(pair);
                        }
                        break;
                    }
                    let (binding_form, is_subject) = match &current.kind {
                        FormKind::Symbol(sym) if sym.starts_with('$') => {
                            let trimmed = sym.trim_start_matches('$');
                            if trimmed.is_empty() {
                                return Err(span_runtime_error(
                                    current.span,
                                    "parameter name after '$' cannot be empty",
                                ));
                            }
                            let mut param_form = current.clone();
                            param_form.kind = FormKind::Symbol(trimmed.to_string());
                            (param_form, true)
                        }
                        _ => (current.clone(), false),
                    };
                    let (name, binding) = parse_param_binding(&binding_form, params.len(), false)?;
                    if is_subject {
                        if subject_pos.is_some() {
                            return Err(span_runtime_error(
                                current.span,
                                "multiple '$' parameters are not allowed",
                            ));
                        }
                        subject_pos = Some(params.len() + 1);
                    }
                    params.push(name);
                    if let Some(pair) = binding {
                        destructure_bindings.push(pair);
                    }
                    idx += 1;
                }
            }
            _ => {
                return Err(span_runtime_error(
                    params_form.span,
                    "fn params must be vector",
                ))
            }
        }
        Ok(ParsedParams {
            params,
            rest,
            destructure_bindings,
            subject_pos,
        })
    }

    fn wrap_param_destructuring(
        &self,
        params_form: &Form,
        parsed: &ParsedParams,
        body: &[Form],
    ) -> Vec<Form> {
        if parsed.destructure_bindings.is_empty() {
            return body.to_vec();
        }
        let mut binding_items = Vec::with_capacity(parsed.destructure_bindings.len() * 2);
        for (pattern, value_form) in &parsed.destructure_bindings {
            binding_items.push(pattern.clone());
            binding_items.push(value_form.clone());
        }
        let bindings_vec = Form::new(FormKind::Vector(binding_items), params_form.span);
        let mut let_items = Vec::with_capacity(body.len() + 2);
        let_items.push(Form::new(FormKind::Symbol("let".into()), params_form.span));
        let_items.push(bindings_vec);
        let_items.extend(body.iter().cloned());
        vec![Form::new(FormKind::List(let_items), params_form.span)]
    }

    fn parse_binding_pair(&self, form: &Form) -> Result<(Form, Form), CloveError> {
        parse_pattern_binding_pair_form(form)
    }

    fn parse_binding_pairs(&self, form: &Form) -> Result<Vec<(Form, Form)>, CloveError> {
        parse_pattern_binding_pairs_form(form)
    }

    fn parse_symbol_binding_pair(&self, form: &Form) -> Result<(String, Form), CloveError> {
        parse_symbol_binding_pair_form(form)
    }

    fn parse_symbol_binding_pairs(&self, form: &Form) -> Result<Vec<(String, Form)>, CloveError> {
        parse_symbol_binding_pairs_form(form)
    }

    fn eval_throw(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.len() != 1 {
            return Err(span_runtime_error(form_span, "throw expects one argument"));
        }
        let val = self.eval(&args[0], env)?;
        Err(CloveError::Thrown(val, ErrorContext::default()))
    }

    fn eval_short_try_handler(
        &self,
        body_forms: &[Form],
        handler_form: Option<&Form>,
        finally_form: Option<&Form>,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let mut caught_err: Option<CloveError> = None;
        let mut result: Option<Value> = None;

        match self.eval_do(body_forms, env.clone()) {
            Ok(v) => result = Some(v),
            Err(err) => {
                if matches!(err, CloveError::RecurSignal { .. }) {
                    return Err(err);
                }
                if let Some(handler_form) = handler_form {
                    match self.eval(handler_form, env.clone()) {
                        Ok(handler) => match call_callable(handler, vec![error_to_value(&err)]) {
                            Ok(v) => result = Some(v),
                            Err(handler_err) => {
                                if matches!(handler_err, CloveError::RecurSignal { .. }) {
                                    return Err(handler_err);
                                }
                                caught_err = Some(handler_err);
                            }
                        },
                        Err(handler_err) => {
                            if matches!(handler_err, CloveError::RecurSignal { .. }) {
                                return Err(handler_err);
                            }
                            caught_err = Some(handler_err);
                        }
                    }
                } else {
                    caught_err = Some(err);
                }
            }
        }

        if let Some(finally_form) = finally_form {
            let finally_callable = self.eval(finally_form, env.clone())?;
            if let Err(err) = call_callable(finally_callable, Vec::new()) {
                return Err(err);
            }
        }

        if let Some(v) = result {
            Ok(v)
        } else if let Some(err) = caught_err {
            Err(err)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_short_try_with_bindings(
        &self,
        bindings_form: &Form,
        plan: TryShortPlan,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let bindings = self.parse_binding_pairs(bindings_form)?;
        let child = new_ref(Env::new_child(env));
        for (pat, value_form) in bindings {
            let val = self.eval(&value_form, child.clone())?;
            bind_pattern(child.clone(), &pat, val)?;
        }
        match plan {
            TryShortPlan::Handler { body, on_error } => {
                self.eval_short_try_handler(&body, Some(&on_error), None, child)
            }
            TryShortPlan::Finally { body, on_finally } => {
                self.eval_short_try_handler(&body, None, Some(&on_finally), child)
            }
            TryShortPlan::HandlerFinally {
                body,
                on_error,
                on_finally,
            } => self.eval_short_try_handler(&body, Some(&on_error), Some(&on_finally), child),
        }
    }

    fn eval_try_short(&self, plan: TryShortPlan, env: EnvRef) -> Result<Value, CloveError> {
        match plan {
            TryShortPlan::Handler { body, on_error } => {
                self.eval_short_try_handler(&body, Some(&on_error), None, env)
            }
            TryShortPlan::Finally { body, on_finally } => {
                self.eval_short_try_handler(&body, None, Some(&on_finally), env)
            }
            TryShortPlan::HandlerFinally {
                body,
                on_error,
                on_finally,
            } => self.eval_short_try_handler(&body, Some(&on_error), Some(&on_finally), env),
        }
    }

    fn eval_try_err_fin(
        &self,
        tail: crate::try_form::ErrFinTail,
        bindings_form: Option<&Form>,
        env: EnvRef,
        form_span: Span,
    ) -> Result<Value, CloveError> {
        if tail.body.is_empty() {
            return Err(span_runtime_error(form_span, "try expects body"));
        }
        let err_handler = if let Some(err_form) = tail.err.as_ref() {
            Some(
                build_err_clause_handler(err_form)
                    .map_err(|err| span_runtime_error(err.span, err.message))?,
            )
        } else {
            None
        };
        let fin_handler = if let Some(fin_form) = tail.fin.as_ref() {
            Some(
                build_fin_clause_handler(fin_form)
                    .map_err(|err| span_runtime_error(err.span, err.message))?,
            )
        } else {
            None
        };
        if let Some(bindings_form) = bindings_form {
            let bindings = self.parse_binding_pairs(bindings_form)?;
            let child = new_ref(Env::new_child(env));
            for (pat, value_form) in bindings {
                let val = self.eval(&value_form, child.clone())?;
                bind_pattern(child.clone(), &pat, val)?;
            }
            return self.eval_short_try_handler(
                &tail.body,
                err_handler.as_ref(),
                fin_handler.as_ref(),
                child,
            );
        }
        self.eval_short_try_handler(&tail.body, err_handler.as_ref(), fin_handler.as_ref(), env)
    }

    fn eval_try(&self, args: &[Form], env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        if args.is_empty() {
            return Err(span_runtime_error(form_span, "try expects body"));
        }
        let has_catch_finally = args.iter().any(|form| match &form.kind {
            FormKind::List(items) => match items.first().map(|f| &f.kind) {
                Some(FormKind::Symbol(tag)) => tag == "catch" || tag == "finally",
                _ => false,
            },
            _ => false,
        });
        if !has_catch_finally {
            if matches!(args[0].kind, FormKind::Vector(_)) {
                let err_fin = parse_err_fin_tail(&args[1..], "try body")
                    .map_err(|err| span_runtime_error(err.span, err.message))?;
                if let Some(tail) = err_fin {
                    if let Err(err) = validate_try_bindings(&args[0]) {
                        return Err(span_runtime_error(
                            form_span,
                            format_try_error_message(&err),
                        ));
                    }
                    return self.eval_try_err_fin(tail, Some(&args[0]), env, form_span);
                }
                if let Err(err) = validate_try_bindings(&args[0]) {
                    return Err(span_runtime_error(
                        form_span,
                        format_try_error_message(&err),
                    ));
                }
                let plan = parse_try_short(&args[1..])
                    .map_err(|err| span_runtime_error(form_span, format_try_error_message(&err)))?;
                return self.eval_short_try_with_bindings(&args[0], plan, env);
            }
            let err_fin = parse_err_fin_tail(args, "try body")
                .map_err(|err| span_runtime_error(err.span, err.message))?;
            if let Some(tail) = err_fin {
                return self.eval_try_err_fin(tail, None, env, form_span);
            }
            let plan = parse_try_short(args)
                .map_err(|err| span_runtime_error(form_span, format_try_error_message(&err)))?;
            return self.eval_try_short(plan, env);
        }
        if let Some(_tail) = parse_err_fin_tail(args, "try body")
            .map_err(|err| span_runtime_error(err.span, err.message))?
        {
            return Err(span_runtime_error(
                form_span,
                "err/fin cannot be combined with catch/finally",
            ));
        }
        let mut body_forms = Vec::new();
        let mut catch_clause: Option<(Option<String>, String, Vec<Form>)> = None;
        let mut finally_clause: Option<Vec<Form>> = None;

        for form in args {
            if let FormKind::List(items) = &form.kind {
                if let Some(FormKind::Symbol(tag)) = items.first().map(|f| &f.kind) {
                    match tag.as_str() {
                        "catch" => {
                            if catch_clause.is_some() {
                                return Err(span_runtime_error(
                                    items[0].span,
                                    "multiple catch clauses are not supported",
                                ));
                            }
                            if finally_clause.is_some() {
                                return Err(span_runtime_error(
                                    items[0].span,
                                    "catch must appear before finally",
                                ));
                            }
                            if items.len() < 3 {
                                return Err(span_runtime_error(
                                    items[0].span,
                                    "catch expects binding and body",
                                ));
                            }
                            let mut idx = 1;
                            let first = match &items[idx].kind {
                                FormKind::Symbol(s) => s.clone(),
                                _ => {
                                    return Err(span_runtime_error(
                                        items[idx].span,
                                        "catch binding must be symbol",
                                    ))
                                }
                            };
                            idx += 1;
                            let (type_name, binding) = if items.len() >= 4 {
                                let bind = match &items[idx].kind {
                                    FormKind::Symbol(s) => s.clone(),
                                    _ => {
                                        return Err(span_runtime_error(
                                            items[idx].span,
                                            "catch binding must be symbol",
                                        ))
                                    }
                                };
                                idx += 1;
                                (Some(first), bind)
                            } else {
                                (None, first)
                            };
                            if idx >= items.len() {
                                return Err(span_runtime_error(
                                    items[0].span,
                                    "catch expects body",
                                ));
                            }
                            catch_clause = Some((type_name, binding, items[idx..].to_vec()));
                            continue;
                        }
                        "finally" => {
                            if finally_clause.is_some() {
                                return Err(span_runtime_error(
                                    items[0].span,
                                    "multiple finally clauses are not supported",
                                ));
                            }
                            finally_clause = Some(items[1..].to_vec());
                            continue;
                        }
                        _ => {}
                    }
                }
            }
            if catch_clause.is_some() || finally_clause.is_some() {
                return Err(span_runtime_error(
                    form.span,
                    "no body forms allowed after catch/finally",
                ));
            }
            body_forms.push(form.clone());
        }

        let mut caught_err: Option<CloveError> = None;
        let mut result: Option<Value> = None;

        match self.eval_do(&body_forms, env.clone()) {
            Ok(v) => result = Some(v),
            Err(err) => {
                if matches!(err, CloveError::RecurSignal { .. }) {
                    return Err(err);
                }
                if let Some((type_name, binding, catch_body)) = catch_clause {
                    let is_match = match type_name.as_deref() {
                        Some("RuntimeError") => is_runtime_error(&err),
                        Some(_) => false,
                        None => true,
                    };
                    if is_match {
                        let child_env = new_ref(Env::new_child(env.clone()));
                        if binding != "_" {
                            child_env
                                .write()
                                .unwrap()
                                .set(&binding, error_to_value(&err));
                        }
                        let catch_result = self.eval_do(&catch_body, child_env)?;
                        result = Some(catch_result);
                    } else {
                        caught_err = Some(err);
                    }
                } else {
                    caught_err = Some(err);
                }
            }
        }

        if let Some(finally_body) = finally_clause {
            if let Err(e) = self.eval_do(&finally_body, env.clone()) {
                return Err(e);
            }
        }

        if let Some(v) = result {
            Ok(v)
        } else if let Some(err) = caught_err {
            Err(err)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_break(&self, env: EnvRef, form_span: Span) -> Result<Value, CloveError> {
        // Open a minimal REPL in place. Exit with :q / :quit.
        let debug_err = CloveError::runtime("debug repl")
            .with_span(form_span)
            .with_file(current_file_name());
        repl::set_last_debug_error(debug_err);
        repl::run_debug_repl(self, env)?;
        Ok(Value::Nil)
    }

    fn eval_repl(&self, env: EnvRef, form_span: Span, args: &[Form]) -> Result<Value, CloveError> {
        if args.len() > 1 {
            return Err(span_runtime_error(
                form_span,
                "repl expects zero or one argument",
            ));
        }
        let mut repl_value: Option<Value> = None;
        if let Some(arg) = args.first() {
            let value = self.eval(arg, env.clone())?;
            {
                let mut writer = env.write().unwrap();
                writer.set_in_chain("?", value.clone());
                writer.set_in_chain("?v", value.clone());
                writer.set_in_chain("*?", value.clone());
                writer.set_in_chain("*1", value.clone());
            }
            repl_value = Some(value);
        }
        let debug_err = CloveError::runtime("debug repl")
            .with_span(form_span)
            .with_file(current_file_name());
        repl::set_last_debug_error(debug_err);
        repl::run_debug_repl(self, env)?;
        Ok(repl_value.unwrap_or(Value::Nil))
    }

    fn eval_foreign(
        &self,
        tag: &str,
        code: &str,
        env: EnvRef,
        span: Option<Span>,
    ) -> Result<Value, CloveError> {
        self.ensure_foreign_allowed(&env, span)?;
        if let Some(eng) = self.engines.iter().find(|e| e.tag() == tag) {
            eng.eval_block(code, env, span)
        } else {
            let err_span = span.unwrap_or(Span {
                line: 0,
                col: 0,
                index: 0,
            });
            Err(span_runtime_error(
                err_span,
                format!("unknown foreign tag: {}", tag),
            ))
        }
    }

    fn eval_regex_literal(&self, pattern: &str) -> Result<Value, CloveError> {
        RegexValue::new(pattern.to_string()).map(Value::Regex)
    }

    fn eval_foreign_raw(
        &self,
        tag: Option<&str>,
        code: &str,
        env: EnvRef,
        span: Option<Span>,
    ) -> Result<Value, CloveError> {
        self.ensure_foreign_allowed(&env, span)?;
        match tag {
            Some(t) => self.eval_foreign(t, code, env, span),
            None => {
                let mut errs = Vec::new();
                for eng in &self.engines {
                    match eng.eval_block(code, env.clone(), span) {
                        Ok(v) => return Ok(v),
                        Err(e) => errs.push((eng.tag().to_string(), e)),
                    }
                }
                let joined = errs
                    .into_iter()
                    .map(|(t, err)| format!("  {} error: {}", t, err))
                    .collect::<Vec<_>>()
                    .join("\n");
                let err_span = span.unwrap_or(Span {
                    line: 0,
                    col: 0,
                    index: 0,
                });
                Err(span_runtime_error(
                    err_span,
                    format!("auto adapter failed\n{}", joined),
                ))
            }
        }
    }

    fn eval_foreign_symbol(
        &self,
        tag: Option<&str>,
        path: &str,
        env: EnvRef,
        span: Span,
    ) -> Result<Value, CloveError> {
        self.ensure_foreign_allowed(&env, Some(span))?;
        let mut resolved_tag = tag.map(|t| t.to_string());
        if resolved_tag.is_none() && path.contains("::") {
            if let Some(eng) = self.engines.iter().find(|e| e.tag() == "rb") {
                resolved_tag = Some(eng.tag().to_string());
            }
        }
        let tag = resolved_tag.ok_or_else(|| {
            span_runtime_error(
                span,
                "foreign symbol requires explicit tag (e.g. $rb/ $py) or default language (e.g. :lang rb)",
            )
        })?;
        let engine = self
            .engines
            .iter()
            .find(|e| e.tag() == tag)
            .cloned()
            .ok_or_else(|| {
                let msg = format!("unknown foreign tag: {}", tag);
                span_runtime_error(span, &msg)
            })?;
        Ok(Value::ForeignCallable {
            tag: tag.to_string(),
            path: path.to_string(),
            engine,
            span: Some(span),
        })
    }

    fn decorate_error(&self, err: CloveError, span: Span, env: EnvRef) -> CloveError {
        let with_span = err.with_span(span);
        let with_file = with_span.with_file(current_file_name());
        let with_env = with_file.with_env(env);
        let stack = capture_stack();
        with_env.with_stack(stack)
    }
}

#[derive(Clone, Copy)]
enum ThreadStyle {
    First,
    Last,
}

#[derive(Clone, Debug)]
struct EnumVariantLookup {
    enum_raw: String,
    variant_name: String,
    variant_fqn: String,
    is_unit: bool,
}

fn truthy(v: &Value) -> bool {
    !matches!(v, Value::Nil | Value::Bool(false))
}

fn build_lambda_source_meta() -> Option<HashMap<Key, Value>> {
    let file = current_file_name()?;
    let mut map = HashMap::new();
    map.insert(Key::Keyword("source-file".to_string()), Value::String(file));
    Some(map)
}

fn merge_meta_maps(
    existing: Option<HashMap<Key, Value>>,
    incoming: Option<HashMap<Key, Value>>,
) -> Option<HashMap<Key, Value>> {
    match (existing, incoming) {
        (None, None) => None,
        (Some(map), None) | (None, Some(map)) => Some(map),
        (Some(mut base), Some(extra)) => {
            for (key, value) in extra {
                base.insert(key, value);
            }
            Some(base)
        }
    }
}

fn lambda_source_file(value: &Value) -> Option<String> {
    match value {
        Value::Lambda {
            meta: Some(meta), ..
        }
        | Value::MultiLambda {
            meta: Some(meta), ..
        } => meta
            .get(&Key::Keyword("source-file".to_string()))
            .and_then(|val| match val {
                Value::String(s) => Some(s.clone()),
                _ => None,
            }),
        _ => None,
    }
}

fn attach_lambda_meta(value: Value, meta: Option<HashMap<Key, Value>>) -> Value {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta: existing,
            doc,
            name,
            inferred_type,
            recur_id,
        } => Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta: merge_meta_maps(existing, meta),
            doc,
            name,
            inferred_type,
            recur_id,
        },
        Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta: existing,
            doc,
            name,
            inferred_type,
        } => Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta: merge_meta_maps(existing, meta),
            doc,
            name,
            inferred_type,
        },
        other => other,
    }
}

fn attach_lambda_doc(value: Value, doc: Option<String>) -> Value {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc: existing,
            name,
            inferred_type,
            recur_id,
        } => Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc: doc.or(existing),
            name,
            inferred_type,
            recur_id,
        },
        Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc: existing,
            name,
            inferred_type,
        } => Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc: doc.or(existing),
            name,
            inferred_type,
        },
        other => other,
    }
}

fn attach_lambda_body(value: Value, new_body: Vec<Form>) -> Value {
    match value {
        Value::Lambda {
            params,
            rest,
            body: _,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            inferred_type,
            recur_id,
        } => Value::Lambda {
            params,
            rest,
            body: new_body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            inferred_type,
            recur_id,
        },
        other => other,
    }
}

fn attach_lambda_name(value: Value, name: Option<String>) -> Value {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name: existing,
            inferred_type,
            recur_id,
        } => Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name: name.or(existing),
            inferred_type,
            recur_id,
        },
        Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name: existing,
            inferred_type,
        } => Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name: name.or(existing),
            inferred_type,
        },
        other => other,
    }
}

fn attach_lambda_inferred_type(value: Value, ty: Option<MetaTypeKind>) -> Value {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            recur_id,
            ..
        } => Value::Lambda {
            params,
            rest,
            body,
            local_defns,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            inferred_type: ty,
            recur_id,
        },
        Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            ..
        } => Value::MultiLambda {
            clauses,
            env,
            engines,
            auto_fallback,
            call_wrappers,
            settings,
            meta,
            doc,
            name,
            inferred_type: ty,
        },
        other => other,
    }
}

fn is_some_value(v: &Value) -> bool {
    !matches!(v, Value::Nil)
}

fn bind_single(env: EnvRef, name: &str, value: Value) -> EnvRef {
    let child = new_ref(Env::new_child(env));
    if name != "_" {
        child.write().unwrap().set(name, value);
    }
    child
}

fn bind_pattern(env: EnvRef, pattern: &Form, value: Value) -> Result<(), CloveError> {
    match &pattern.kind {
        FormKind::Symbol(name) => {
            if name != "_" {
                env.write().unwrap().set(name, value);
            }
            Ok(())
        }
        FormKind::Vector(items) => bind_vector_pattern(env, items, value),
        FormKind::Map(entries) => bind_map_pattern(env, entries, value),
        FormKind::List(items) => bind_type_pattern(env, pattern, items, value),
        _ => Err(span_runtime_error(
            pattern.span,
            "binding name must be symbol, vector destructuring, or map destructuring",
        )),
    }
}

fn bind_type_pattern(
    env: EnvRef,
    pattern: &Form,
    items: &[Form],
    value: Value,
) -> Result<(), CloveError> {
    if items.is_empty() {
        return Err(span_runtime_error(
            pattern.span,
            "type pattern expects a type name",
        ));
    }
    let keyword_fields = if items.len() > 1 {
        keyword_shorthand_fields(&items[1..])
    } else {
        None
    };
    if keyword_fields.is_none() && items.len() > 2 {
        return Err(span_runtime_error(
            pattern.span,
            "type pattern accepts at most one inner pattern",
        ));
    }
    let head = &items[0];
    let sym = match &head.kind {
        FormKind::Symbol(sym) => sym,
        _ => {
            return Err(span_runtime_error(
                head.span,
                "type pattern expects symbol type name",
            ))
        }
    };
    let current_ns = current_namespace_name(&env);
    let fqn = if let Some(variant) = enum_variant_lookup_with_ns(sym, current_ns.as_deref()) {
        variant.variant_fqn
    } else if let Some(variant) =
        enum_variant_lookup_unqualified(sym, current_ns.as_deref(), head.span)?
    {
        variant.variant_fqn
    } else {
        let default_ns = current_ns.as_deref().unwrap_or("user");
        let (ns, local) = resolve_type_name(default_ns, sym, head.span)?;
        let fqn = format!("{}::{}", ns, local);
        match type_registry::get_type_entry(&fqn) {
            Some(TypeEntry::Product(_)) | Some(TypeEntry::Alias(_)) => fqn,
            Some(TypeEntry::Sum(_)) => {
                return Err(span_runtime_error(
                    head.span,
                    "binding pattern cannot use enum directly",
                ))
            }
            Some(TypeEntry::Primitive(_)) => {
                return Err(span_runtime_error(
                    head.span,
                    "binding pattern cannot use primitive type",
                ))
            }
            None => {
                return Err(span_runtime_error(
                    head.span,
                    "unknown type in binding pattern",
                ))
            }
        }
    };
    if !type_registry::conforms_named(&fqn, &value) {
        return Err(span_runtime_error(
            pattern.span,
            format!(
                "expected {} but got {}",
                fqn,
                type_registry::type_of_value(&value)
            ),
        ));
    }
    if let Some(fields) = keyword_fields {
        let entries = keyword_shorthand_map_items(&fields);
        bind_map_pattern(env, &entries, value)
    } else if items.len() == 2 {
        bind_pattern(env, &items[1], value)
    } else {
        Ok(())
    }
}

fn bind_pattern_child(env: EnvRef, pattern: &Form, value: Value) -> Result<EnvRef, CloveError> {
    let child = new_ref(Env::new_child(env));
    bind_pattern(child.clone(), pattern, value)?;
    Ok(child)
}

fn bind_vector_pattern(env: EnvRef, items: &[Form], value: Value) -> Result<(), CloveError> {
    let seq = builtins::seq_items(&value)?;
    let mut patterns = Vec::new();
    let mut rest: Option<Form> = None;
    let mut as_name: Option<String> = None;
    let mut idx = 0;
    while idx < items.len() {
        match &items[idx].kind {
            FormKind::Symbol(sym) if sym == "&" => {
                let next = items.get(idx + 1).ok_or_else(|| {
                    span_runtime_error(items[idx].span, "& must be followed by binding form")
                })?;
                rest = Some(next.clone());
                break;
            }
            FormKind::Keyword(kw) if kw == "as" => {
                let next = items.get(idx + 1).ok_or_else(|| {
                    span_runtime_error(items[idx].span, ":as expects following symbol")
                })?;
                let as_sym = match &next.kind {
                    FormKind::Symbol(sym) => sym.clone(),
                    _ => {
                        return Err(span_runtime_error(
                            next.span,
                            ":as expects symbol binding name",
                        ))
                    }
                };
                as_name = Some(as_sym);
                idx += 2;
            }
            _ => {
                patterns.push(items[idx].clone());
                idx += 1;
            }
        }
    }

    for (i, pat) in patterns.iter().enumerate() {
        let v = seq.get(i).cloned().unwrap_or(Value::Nil);
        bind_pattern(env.clone(), pat, v)?;
    }

    if let Some(rest_pat) = rest {
        let rest_items: Vector<Value> = if seq.len() > patterns.len() {
            seq.iter().skip(patterns.len()).cloned().collect()
        } else {
            Vector::new()
        };
        bind_pattern(env.clone(), &rest_pat, Value::Vector(rest_items))?;
    }

    if let Some(name) = as_name {
        if name != "_" {
            env.write().unwrap().set(&name, value);
        }
    }
    Ok(())
}

fn bind_map_pattern(env: EnvRef, entries: &[MapItem], value: Value) -> Result<(), CloveError> {
    let mut as_name: Option<String> = None;
    let map_opt = match &value {
        Value::Map(m) => Some(m.clone()),
        _ => None,
    };
    let mut idx = 0;
    while idx < entries.len() {
        // Special case: shorthand like {:a :as whole} without a comma before :as.
        if let MapItem::KeyValue(k_form, pat_form) = &entries[idx] {
            if matches!(&pat_form.kind, FormKind::Keyword(kw) if kw == "as") {
                if let Some(MapItem::KeyValue(next_k_form, next_pat_form)) = entries.get(idx + 1) {
                    let alias_opt = match &next_k_form.kind {
                        FormKind::Symbol(s) | FormKind::Keyword(s) => Some(s.clone()),
                        _ => None,
                    };
                    let alias_pat_opt = match &next_pat_form.kind {
                        FormKind::Symbol(s) | FormKind::Keyword(s) => Some(s.clone()),
                        _ => None,
                    };
                    if let (Some(alias), Some(alias_pat)) = (alias_opt, alias_pat_opt) {
                        if alias == alias_pat {
                            let bind_name = match &k_form.kind {
                                FormKind::Keyword(name) | FormKind::Symbol(name) => name.clone(),
                                _ => String::new(),
                            };
                            if !bind_name.is_empty() {
                                let key = to_key(k_form)?;
                                let val = map_opt
                                    .as_ref()
                                    .and_then(|m| m.get(&key).cloned())
                                    .unwrap_or(Value::Nil);
                                let bind_pat =
                                    Form::new(FormKind::Symbol(bind_name), pat_form.span);
                                bind_pattern(env.clone(), &bind_pat, val)?;
                            }
                            if as_name.is_none() {
                                as_name = Some(alias.clone());
                            }
                            idx += 2;
                            continue;
                        }
                    }
                }
            }
        }

        match &entries[idx] {
            MapItem::KeyValue(k_form, pat_form) => {
                if let FormKind::Keyword(kw) = &k_form.kind {
                    if kw == "as" {
                        let sym = match &pat_form.kind {
                            FormKind::Symbol(s) => s.clone(),
                            _ => {
                                return Err(span_runtime_error(
                                    pat_form.span,
                                    ":as expects symbol binding name",
                                ))
                            }
                        };
                        as_name = Some(sym);
                        idx += 1;
                        continue;
                    }
                    if kw == "keys" {
                        let items = match &pat_form.kind {
                            FormKind::Vector(v) => v,
                            _ => {
                                return Err(span_runtime_error(
                                    pat_form.span,
                                    ":keys expects vector of symbols",
                                ))
                            }
                        };
                        for item in items {
                            let (bind_name, key) = match &item.kind {
                                FormKind::Symbol(s) => (s.clone(), Key::Keyword(s.clone())),
                                FormKind::Keyword(s) => (s.clone(), Key::Keyword(s.clone())),
                                _ => {
                                    return Err(span_runtime_error(
                                        item.span,
                                        ":keys accepts symbols or keywords",
                                    ))
                                }
                            };
                            let bind_pat =
                                Form::new(FormKind::Symbol(bind_name.clone()), item.span);
                            let val = map_opt
                                .as_ref()
                                .and_then(|m| m.get(&key).cloned())
                                .unwrap_or(Value::Nil);
                            bind_pattern(env.clone(), &bind_pat, val)?;
                        }
                        idx += 1;
                        continue;
                    }
                }
                let key = to_key(k_form)?;
                let val = map_opt
                    .as_ref()
                    .and_then(|m| m.get(&key).cloned())
                    .unwrap_or(Value::Nil);
                bind_pattern(env.clone(), pat_form, val)?;
            }
            MapItem::Spread(expr) => {
                return Err(span_runtime_error(
                    expr.span,
                    "map destructuring does not support spread entries",
                ))
            }
        }
        idx += 1;
    }
    if let Some(name) = as_name {
        if name != "_" {
            env.write().unwrap().set(&name, value);
        }
    }
    Ok(())
}

fn parse_param_binding(
    form: &Form,
    positional_idx: usize,
    is_rest: bool,
) -> Result<(String, Option<(Form, Form)>), CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if sym == "&" {
                return Err(span_runtime_error(
                    form.span,
                    "fn params must be symbols, vector destructuring, or map destructuring",
                ));
            }
            if sym != "_" {
                validate_binding_symbol(sym, form.span)?;
            }
            Ok((sym.clone(), None))
        }
        FormKind::Vector(_) | FormKind::Map(_) => {
            validate_binding_pattern(form)?;
            let name = generated_param_name(positional_idx, is_rest);
            let value_form = Form::new(FormKind::Symbol(name.clone()), form.span);
            Ok((name, Some((form.clone(), value_form))))
        }
        _ => Err(span_runtime_error(
            form.span,
            "fn params must be symbols, vector destructuring, or map destructuring",
        )),
    }
}

fn generated_param_name(idx: usize, is_rest: bool) -> String {
    if is_rest {
        format!("__destr_rest_arg{}", idx)
    } else {
        format!("__destr_arg{}", idx)
    }
}

fn validate_binding_symbol(name: &str, span: Span) -> Result<(), CloveError> {
    if parse_question_placeholder(name).is_some() {
        return Err(span_runtime_error(
            span,
            "binding name cannot be placeholder",
        ));
    }
    Ok(())
}

fn validate_binding_pattern(form: &Form) -> Result<(), CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if sym != "_" {
                validate_binding_symbol(sym, form.span)?;
            }
            Ok(())
        }
        FormKind::Vector(items) => validate_vector_binding_pattern(items),
        FormKind::Map(entries) => validate_map_binding_pattern(entries),
        FormKind::List(items) => validate_type_binding_pattern(items),
        _ => Ok(()),
    }
}

fn validate_vector_binding_pattern(items: &[Form]) -> Result<(), CloveError> {
    let mut idx = 0;
    while idx < items.len() {
        match &items[idx].kind {
            FormKind::Symbol(sym) if sym == "&" => {
                let next = items.get(idx + 1).ok_or_else(|| {
                    span_runtime_error(items[idx].span, "& must be followed by binding form")
                })?;
                validate_binding_pattern(next)?;
                break;
            }
            FormKind::Keyword(kw) if kw == "as" => {
                let next = items.get(idx + 1).ok_or_else(|| {
                    span_runtime_error(items[idx].span, ":as expects following symbol")
                })?;
                let sym = match &next.kind {
                    FormKind::Symbol(sym) => sym,
                    _ => {
                        return Err(span_runtime_error(
                            next.span,
                            ":as expects symbol binding name",
                        ))
                    }
                };
                validate_binding_symbol(sym, next.span)?;
                idx += 2;
            }
            _ => {
                validate_binding_pattern(&items[idx])?;
                idx += 1;
            }
        }
    }
    Ok(())
}

fn binding_name_from_key_form(form: &Form) -> Option<String> {
    match &form.kind {
        FormKind::Symbol(name) | FormKind::Keyword(name) => Some(name.clone()),
        _ => None,
    }
}

fn validate_map_binding_pattern(entries: &[MapItem]) -> Result<(), CloveError> {
    let mut idx = 0;
    while idx < entries.len() {
        match &entries[idx] {
            MapItem::KeyValue(k_form, pat_form) => {
                if let FormKind::Keyword(kw) = &k_form.kind {
                    if kw == "as" {
                        let sym = match &pat_form.kind {
                            FormKind::Symbol(sym) => sym,
                            _ => {
                                return Err(span_runtime_error(
                                    pat_form.span,
                                    ":as expects symbol binding name",
                                ))
                            }
                        };
                        validate_binding_symbol(sym, pat_form.span)?;
                        idx += 1;
                        continue;
                    }
                    if kw == "keys" {
                        let items = match &pat_form.kind {
                            FormKind::Vector(v) => v,
                            _ => {
                                return Err(span_runtime_error(
                                    pat_form.span,
                                    ":keys expects vector of symbols",
                                ))
                            }
                        };
                        for item in items {
                            let bind_name = match &item.kind {
                                FormKind::Symbol(s) | FormKind::Keyword(s) => s,
                                _ => {
                                    return Err(span_runtime_error(
                                        item.span,
                                        ":keys accepts symbols or keywords",
                                    ))
                                }
                            };
                            validate_binding_symbol(bind_name, item.span)?;
                        }
                        idx += 1;
                        continue;
                    }
                }
                if matches!(&pat_form.kind, FormKind::Keyword(kw) if kw == "as") {
                    if let Some(name) = binding_name_from_key_form(k_form) {
                        validate_binding_symbol(&name, k_form.span)?;
                    }
                    if let Some(MapItem::KeyValue(next_k, next_pat)) = entries.get(idx + 1) {
                        if let (Some(alias), Some(alias_pat)) = (
                            binding_name_from_key_form(next_k),
                            binding_name_from_key_form(next_pat),
                        ) {
                            if alias == alias_pat {
                                validate_binding_symbol(&alias, next_k.span)?;
                            }
                        }
                    }
                    idx += 1;
                    continue;
                }
                validate_binding_pattern(pat_form)?;
            }
            MapItem::Spread(_) => {}
        }
        idx += 1;
    }
    Ok(())
}

fn validate_type_binding_pattern(items: &[Form]) -> Result<(), CloveError> {
    if items.len() > 1 {
        if let Some(fields) = keyword_shorthand_fields(&items[1..]) {
            for (name, span) in fields {
                if name != "_" {
                    validate_binding_symbol(&name, span)?;
                }
            }
        } else if items.len() == 2 {
            validate_binding_pattern(&items[1])?;
        }
    }
    Ok(())
}

#[derive(Clone)]
enum ForStep {
    Bind(Form, Form),
    Let(Vec<(Form, Form)>),
    When(Form),
    While(Form),
}

fn parse_pattern_binding_pairs_form(form: &Form) -> Result<Vec<(Form, Form)>, CloveError> {
    match &form.kind {
        FormKind::Vector(items) if items.len() % 2 == 0 => {
            let mut pairs = Vec::new();
            let mut idx = 0;
            while idx < items.len() {
                let binding = items[idx].clone();
                validate_binding_pattern(&binding)?;
                pairs.push((binding, items[idx + 1].clone()));
                idx += 2;
            }
            Ok(pairs)
        }
        FormKind::Vector(_) => Err(span_runtime_error(
            form.span,
            "binding vector must have even number of forms",
        )),
        _ => Err(span_runtime_error(form.span, "bindings must be vector")),
    }
}

fn parse_symbol_binding_pairs_form(form: &Form) -> Result<Vec<(String, Form)>, CloveError> {
    let pairs = parse_pattern_binding_pairs_form(form)?;
    let mut out = Vec::with_capacity(pairs.len());
    for (pat, val) in pairs {
        match pat.kind {
            FormKind::Symbol(s) => {
                if s != "_" {
                    validate_binding_symbol(&s, pat.span)?;
                }
                out.push((s, val))
            }
            _ => return Err(span_runtime_error(pat.span, "binding name must be symbol")),
        }
    }
    Ok(out)
}

fn parse_pattern_binding_pair_form(form: &Form) -> Result<(Form, Form), CloveError> {
    let mut pairs = parse_pattern_binding_pairs_form(form)?;
    if pairs.len() != 1 {
        return Err(span_runtime_error(
            form.span,
            "binding vector must be [name value]",
        ));
    }
    Ok(pairs.pop().unwrap())
}

fn parse_symbol_binding_pair_form(form: &Form) -> Result<(String, Form), CloveError> {
    let (pat, val) = parse_pattern_binding_pair_form(form)?;
    match pat.kind {
        FormKind::Symbol(s) => {
            if s != "_" {
                validate_binding_symbol(&s, pat.span)?;
            }
            Ok((s, val))
        }
        _ => Err(span_runtime_error(pat.span, "binding name must be symbol")),
    }
}

fn parse_for_steps(form: &Form) -> Result<Vec<ForStep>, CloveError> {
    let mut steps = Vec::new();
    match &form.kind {
        FormKind::Vector(items) => {
            let mut idx = 0;
            while idx < items.len() {
                let binding = &items[idx];
                match &binding.kind {
                    FormKind::Keyword(kw) if kw == "let" => {
                        let binds = items.get(idx + 1).ok_or_else(|| {
                            span_runtime_error(binding.span, "for :let expects binding vector")
                        })?;
                        let pairs = parse_pattern_binding_pairs_form(binds)?;
                        steps.push(ForStep::Let(pairs));
                        idx += 2;
                    }
                    FormKind::Keyword(kw) if kw == "when" => {
                        let test = items.get(idx + 1).ok_or_else(|| {
                            span_runtime_error(binding.span, "for :when expects test expression")
                        })?;
                        steps.push(ForStep::When(test.clone()));
                        idx += 2;
                    }
                    FormKind::Keyword(kw) if kw == "while" => {
                        let test = items.get(idx + 1).ok_or_else(|| {
                            span_runtime_error(binding.span, "for :while expects test expression")
                        })?;
                        steps.push(ForStep::While(test.clone()));
                        idx += 2;
                    }
                    _ => {
                        let binding = items[idx].clone();
                        let coll_form = items.get(idx + 1).ok_or_else(|| {
                            span_runtime_error(
                                binding.span,
                                "for binding expects collection expression",
                            )
                        })?;
                        steps.push(ForStep::Bind(binding, coll_form.clone()));
                        idx += 2;
                    }
                }
            }
        }
        _ => return Err(span_runtime_error(form.span, "for bindings must be vector")),
    }
    Ok(steps)
}

enum ForControl {
    Continue,
    Stop,
}

fn run_for_steps(
    eval: &Evaluator,
    steps: &[ForStep],
    idx: usize,
    env: EnvRef,
    body: &[Form],
    out: &mut Vec<Value>,
) -> Result<ForControl, CloveError> {
    if idx == steps.len() {
        out.push(eval.eval_do(body, env)?);
        return Ok(ForControl::Continue);
    }
    match &steps[idx] {
        ForStep::Bind(binding, coll_form) => {
            let coll_val = eval.eval(coll_form, env.clone())?;
            let items = builtins::seq_items(&coll_val)?;
            for item in items {
                let child = bind_pattern_child(env.clone(), binding, item)?;
                if let ForControl::Stop = run_for_steps(eval, steps, idx + 1, child, body, out)? {
                    return Ok(ForControl::Stop);
                }
            }
            Ok(ForControl::Continue)
        }
        ForStep::Let(pairs) => {
            let child = new_ref(Env::new_child(env.clone()));
            for (pat, form) in pairs {
                let val = eval.eval(form, child.clone())?;
                bind_pattern(child.clone(), pat, val)?;
            }
            run_for_steps(eval, steps, idx + 1, child, body, out)
        }
        ForStep::When(test_form) => {
            let cond = eval.eval(test_form, env.clone())?;
            if truthy(&cond) {
                run_for_steps(eval, steps, idx + 1, env, body, out)
            } else {
                Ok(ForControl::Continue)
            }
        }
        ForStep::While(test_form) => {
            let cond = eval.eval(test_form, env.clone())?;
            if truthy(&cond) {
                run_for_steps(eval, steps, idx + 1, env, body, out)
            } else {
                Ok(ForControl::Stop)
            }
        }
    }
}

struct MatchClause {
    pattern: Pattern,
    guard: Option<Form>,
    as_binding: Option<String>,
    expr: Form,
}

#[derive(Clone)]
enum Pattern {
    Wildcard,
    Literal(Value),
    Bind(String),
    Or(Vec<Pattern>),
    Vector(Vec<Pattern>, Option<Box<Pattern>>),
    Map(Vec<(String, Pattern)>),
    Type {
        fqn: String,
        inner: Option<Box<Pattern>>,
    },
}

impl Pattern {
    fn matches(&self, value: &Value, bindings: &mut Vec<(String, Value)>) -> bool {
        match self {
            Pattern::Wildcard => true,
            Pattern::Literal(lit) => value == lit,
            Pattern::Bind(name) => {
                bindings.push((name.clone(), value.clone()));
                true
            }
            Pattern::Or(items) => {
                for pat in items {
                    let before = bindings.len();
                    if pat.matches(value, bindings) {
                        return true;
                    }
                    bindings.truncate(before);
                }
                false
            }
            Pattern::Vector(items, rest) => {
                let seq: Vec<Value> = match value {
                    Value::Vector(v) => v.iter().cloned().collect(),
                    Value::List(v) => v.iter().cloned().collect(),
                    _ => return false,
                };
                if seq.len() < items.len() {
                    return false;
                }
                if rest.is_none() && seq.len() != items.len() {
                    return false;
                }
                for (idx, pat) in items.iter().enumerate() {
                    if !pat.matches(&seq[idx], bindings) {
                        return false;
                    }
                }
                if let Some(rest_pat) = rest {
                    let remaining = seq[items.len()..].to_vec();
                    let rest_value = Value::Vector(Vector::from(remaining));
                    rest_pat.matches(&rest_value, bindings)
                } else {
                    seq.len() == items.len()
                }
            }
            Pattern::Map(entries) => {
                let map = match value {
                    Value::Map(m) => m,
                    _ => return false,
                };
                for (field, pat) in entries {
                    let key = Key::Keyword(field.clone());
                    let mapped = match map.get(&key) {
                        Some(v) => v.clone(),
                        None => return false,
                    };
                    if !pat.matches(&mapped, bindings) {
                        return false;
                    }
                }
                true
            }
            Pattern::Type { fqn, inner } => {
                if !type_registry::conforms_named(fqn, value) {
                    return false;
                }
                if let Some(inner_pat) = inner {
                    inner_pat.matches(value, bindings)
                } else {
                    true
                }
            }
        }
    }
}

fn qualified_function_name(env: &EnvRef, raw: &str) -> Option<String> {
    if raw == "/" {
        return Some(raw.to_string());
    }
    if raw.contains("::") {
        return Some(raw.to_string());
    }
    current_namespace_name(env).map(|ns| format!("{ns}::{raw}"))
}

fn bump_global_version(name: &str, env: &EnvRef) {
    let _ = RuntimeCtx::try_with_current(|ctx| {
        ctx.bump_global_version(name, env);
        Ok(())
    });
}

fn split_namespace_parts(symbol: &str) -> Option<(String, String)> {
    if let Some(idx) = symbol.rfind("::") {
        let ns = &symbol[..idx];
        let local = &symbol[idx + 2..];
        if !ns.is_empty() && !local.is_empty() {
            return Some((ns.to_string(), local.to_string()));
        }
    }
    None
}

fn format_type_name_for_ns(name: &str, current_ns: Option<&str>) -> String {
    if let Some(stripped) = name.strip_prefix("clove::core::") {
        return stripped.to_string();
    }
    if let Some(ns) = current_ns {
        let prefix = format!("{}::", ns);
        if let Some(stripped) = name.strip_prefix(&prefix) {
            return stripped.to_string();
        }
    }
    name.to_string()
}

fn tagged_type_fqn(value: &Value) -> Option<String> {
    match value {
        Value::Map(map) => map
            .get(&Key::Keyword("type".into()))
            .and_then(|value| match value {
                Value::Symbol(sym) => Some(sym.clone()),
                _ => None,
            }),
        Value::SortedMap(map) => map.entries.iter().find_map(|(k, v)| match k {
            Key::Keyword(name) if name == "type" => match v {
                Value::Symbol(sym) => Some(sym.clone()),
                _ => None,
            },
            _ => None,
        }),
        _ => None,
    }
}

fn tagged_type_name_for_value(value: &Value, current_ns: Option<&str>) -> Option<String> {
    let tag = tagged_type_fqn(value)?;
    Some(format_type_name_for_ns(&tag, current_ns))
}

fn alias_fqn_for_value(value: &Value, current_ns: Option<&str>) -> Option<String> {
    let mut fallback = None;
    let mut fallback_ambiguous = false;
    let mut current_match: Option<String> = None;
    for (name, meta) in type_registry::list_aliases() {
        if !type_registry::matches_type_expr(&meta.target, value) {
            continue;
        }
        if let Some(ns) = current_ns {
            if meta.namespace == ns {
                if let Some(existing) = &current_match {
                    if existing != &name {
                        return None;
                    }
                } else {
                    current_match = Some(name);
                }
                continue;
            }
        }
        if fallback.is_some() {
            fallback_ambiguous = true;
        } else {
            fallback = Some(name);
        }
    }
    if let Some(current_match) = current_match {
        return Some(current_match);
    }
    if fallback_ambiguous {
        None
    } else {
        fallback
    }
}

fn alias_name_for_value(value: &Value, current_ns: Option<&str>) -> Option<String> {
    let mut fallback = None;
    let mut fallback_ambiguous = false;
    let mut current_match: Option<String> = None;
    for (name, meta) in type_registry::list_aliases() {
        if !type_registry::matches_type_expr(&meta.target, value) {
            continue;
        }
        let display = format_type_name_for_ns(&name, current_ns);
        if let Some(ns) = current_ns {
            if meta.namespace == ns {
                if let Some(existing) = &current_match {
                    if existing != &display {
                        return None;
                    }
                } else {
                    current_match = Some(display);
                }
                continue;
            }
        }
        if fallback.is_some() {
            fallback_ambiguous = true;
        } else {
            fallback = Some(display);
        }
    }
    if let Some(current_match) = current_match {
        return Some(current_match);
    }
    if fallback_ambiguous {
        None
    } else {
        fallback
    }
}

fn enum_member_local_name(member: &str) -> &str {
    member
        .rsplit_once("::")
        .map(|(_, local)| local)
        .unwrap_or(member)
}

fn type_name_is_product_like(name: &str) -> bool {
    type_registry::get_type_entry(name).is_some_and(|entry| type_entry_is_product_like(&entry))
}

fn type_entry_is_product_like(entry: &TypeEntry) -> bool {
    match entry {
        TypeEntry::Product(_) => true,
        TypeEntry::Alias(meta) => alias_target_is_product(&meta.target, &mut StdHashSet::new()),
        _ => false,
    }
}

fn alias_target_is_product(target: &MetaTypeKind, seen: &mut StdHashSet<String>) -> bool {
    let MetaTypeKind::Named(name) = target else {
        return false;
    };
    let lookup = name.split('<').next().unwrap_or(name);
    if !seen.insert(lookup.to_string()) {
        return false;
    }
    match type_registry::get_type_entry(lookup) {
        Some(TypeEntry::Product(_)) => true,
        Some(TypeEntry::Alias(meta)) => alias_target_is_product(&meta.target, seen),
        _ => false,
    }
}

fn key_form_from_shorthand(form: &Form) -> Option<(String, Form)> {
    match &form.kind {
        FormKind::Keyword(name) => Some((name.clone(), form.clone())),
        FormKind::Symbol(sym) if is_label_style_key(sym) => {
            let name = sym.trim_end_matches(':').to_string();
            Some((name.clone(), Form::new(FormKind::Keyword(name), form.span)))
        }
        _ => None,
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

fn enum_variant_lookup_unqualified(
    sym: &str,
    current_ns: Option<&str>,
    span: Span,
) -> Result<Option<EnumVariantLookup>, CloveError> {
    if sym.contains("::") {
        return Ok(None);
    }
    let current_ns = match current_ns {
        Some(ns) => ns,
        None => return Ok(None),
    };
    let mut found: Option<EnumVariantLookup> = None;
    for name in type_registry::list_all_types() {
        let Some(TypeEntry::Sum(meta)) = type_registry::get_type_entry(&name) else {
            continue;
        };
        if meta.qualified_only {
            continue;
        }
        if meta.namespace != current_ns {
            continue;
        }
        let variant_fqn = meta
            .members
            .iter()
            .find(|member| enum_member_local_name(member.as_str()) == sym)
            .cloned();
        let Some(variant_fqn) = variant_fqn else {
            continue;
        };
        let product = match type_registry::get_type_entry(&variant_fqn) {
            Some(TypeEntry::Product(product)) => product,
            _ => continue,
        };
        let entry = EnumVariantLookup {
            enum_raw: meta.name.clone(),
            variant_name: sym.to_string(),
            variant_fqn,
            is_unit: product.fields.is_empty(),
        };
        if let Some(existing) = &found {
            if existing.variant_fqn != entry.variant_fqn {
                return Err(span_runtime_error(
                    span,
                    "ambiguous enum variant in match pattern",
                ));
            }
        } else {
            found = Some(entry);
        }
    }
    Ok(found)
}

fn enum_variant_lookup_with_ns(sym: &str, current_ns: Option<&str>) -> Option<EnumVariantLookup> {
    let canonical = canonical_symbol_name(sym);
    let sym = canonical.as_ref();
    let (enum_raw, variant_name) = split_namespace_parts(sym)?;
    if enum_raw.contains('/') {
        return None;
    }
    let enum_fqn = if enum_raw.contains("::") {
        enum_raw.clone()
    } else {
        let ns = current_ns.unwrap_or("user");
        format!("{ns}::{enum_raw}")
    };
    let (_enum_fqn, meta) = match type_registry::get_type_entry(&enum_fqn)? {
        TypeEntry::Sum(meta) => (enum_fqn, meta),
        TypeEntry::Alias(meta) => match meta.target {
            MetaTypeKind::Named(target) => match type_registry::get_type_entry(&target) {
                Some(TypeEntry::Sum(sum_meta)) => (target, sum_meta),
                _ => return None,
            },
            _ => return None,
        },
        _ => return None,
    };
    let variant_fqn = meta
        .members
        .iter()
        .find(|member| enum_member_local_name(member.as_str()) == variant_name)
        .cloned()?;
    let TypeEntry::Product(product) = type_registry::get_type_entry(&variant_fqn)? else {
        return None;
    };
    let is_unit = product.fields.is_empty();
    Some(EnumVariantLookup {
        enum_raw,
        variant_name,
        variant_fqn,
        is_unit,
    })
}

fn resolve_fn_namespace(
    env: &EnvRef,
    name_form: &Form,
    qualified_name: Option<&str>,
) -> Option<(String, String)> {
    if let Some(qname) = qualified_name {
        let canonical = canonical_symbol_name(qname);
        split_namespace_parts(canonical.as_ref())
    } else {
        let symbol = match &name_form.kind {
            FormKind::Symbol(sym) => sym,
            _ => return None,
        };
        let canonical = canonical_symbol_name(symbol);
        let ns = current_namespace_name(env).unwrap_or_else(|| "user".into());
        let local = canonical
            .as_ref()
            .rsplit("::")
            .next()
            .unwrap_or(canonical.as_ref())
            .to_string();
        Some((ns, local))
    }
}

fn collect_param_type_hints(
    params_form: &Form,
) -> (Vec<Option<MetaTypeKind>>, Option<MetaTypeKind>) {
    match &params_form.kind {
        FormKind::Vector(items) => {
            let mut hints = Vec::new();
            let mut idx = 0;
            while idx < items.len() {
                match &items[idx].kind {
                    FormKind::Symbol(sym) if sym == "&" => {
                        if idx + 1 < items.len() {
                            return (hints, type_hint_from_form(&items[idx + 1]));
                        }
                        break;
                    }
                    FormKind::Symbol(_) => hints.push(type_hint_from_form(&items[idx])),
                    _ => {}
                }
                idx += 1;
            }
            (hints, None)
        }
        _ => (Vec::new(), None),
    }
}

fn type_hint_from_form(form: &Form) -> Option<MetaTypeKind> {
    form.type_hint.as_ref().map(|h| h.kind.clone())
}

fn return_hint_from_form(form: &Form) -> Option<MetaTypeKind> {
    form.type_hint
        .as_ref()
        .filter(|h| matches!(h.style, TypeHintStyle::Return))
        .map(|h| h.kind.clone())
}

fn rest_type_from_hint(hint: Option<MetaTypeKind>) -> MetaTypeKind {
    match hint {
        Some(MetaTypeKind::Vector(inner)) => MetaTypeKind::Vector(inner),
        Some(other) => MetaTypeKind::vector(other),
        None => MetaTypeKind::vector(MetaTypeKind::any()),
    }
}

fn current_namespace_name(env: &EnvRef) -> Option<String> {
    env.read()
        .unwrap()
        .get(CURRENT_NS_KEY)
        .and_then(|val| match val {
            Value::Symbol(ns) => Some(ns),
            Value::String(ns) => Some(ns),
            _ => None,
        })
}

fn resolve_type_name(
    default_ns: &str,
    raw: &str,
    span: Span,
) -> Result<(String, String), CloveError> {
    let expanded = expand_type_alias_prefix(raw, default_ns);
    let raw = expanded.as_str();
    if raw.contains('/') {
        return Err(span_runtime_error(
            span,
            "type names must use '::' separators",
        ));
    }
    if let Some(idx) = raw.rfind("::") {
        let namespace = &raw[..idx];
        let name = &raw[idx + 2..];
        if namespace.is_empty() || name.is_empty() {
            return Err(span_runtime_error(span, "invalid qualified type name"));
        }
        Ok((namespace.to_string(), name.to_string()))
    } else {
        Ok((default_ns.to_string(), raw.to_string()))
    }
}

fn expand_type_alias_prefix(raw: &str, current_ns: &str) -> String {
    let (head, tail) = match raw.split_once("::") {
        Some(parts) => parts,
        None => return raw.to_string(),
    };
    let Some(target_ns) = type_alias_target_for_ns(current_ns, head) else {
        return raw.to_string();
    };
    if tail.is_empty() {
        target_ns
    } else {
        format!("{}::{}", target_ns, tail)
    }
}

fn type_alias_target_for_ns(current_ns: &str, alias: &str) -> Option<String> {
    let store = RuntimeCtx::try_with_current(|ctx| Ok(ctx.namespace_store()))?.ok()?;
    let guard = store.read().ok()?;
    guard.type_alias_target(current_ns, alias)
}

fn parse_type_fields(forms: &[Form], current_ns: &str) -> Result<Vec<FieldMeta>, CloveError> {
    if forms.is_empty() {
        return Ok(Vec::new());
    }
    if let Some(fields) = parse_typed_field_list(forms, current_ns)? {
        return Ok(fields);
    }
    let pairs: Vec<(Form, Form)> = if forms.len() == 1 {
        match &forms[0].kind {
            FormKind::Map(entries) => {
                let mut pairs = Vec::new();
                for entry in entries {
                    match entry {
                        MapItem::KeyValue(k, v) => {
                            pairs.push((k.clone(), v.clone()));
                        }
                        MapItem::Spread(expr) => {
                            return Err(span_runtime_error(
                                expr.span,
                                "type field map does not support spread",
                            ));
                        }
                    }
                }
                pairs
            }
            FormKind::Vector(items) | FormKind::List(items) => collect_field_pairs(items)?,
            _ => collect_field_pairs(forms)?,
        }
    } else {
        collect_field_pairs(forms)?
    };
    let mut seen = StdHashSet::new();
    let mut fields = Vec::new();
    for (key_form, value_form) in pairs {
        let field_name = match &key_form.kind {
            FormKind::Keyword(name) | FormKind::Symbol(name) => name.clone(),
            _ => {
                return Err(span_runtime_error(
                    key_form.span,
                    "field name must be keyword or symbol",
                ));
            }
        };
        if !seen.insert(field_name.clone()) {
            return Err(span_runtime_error(
                key_form.span,
                "duplicate field name in schema",
            ));
        }
        let schema = field_schema_from_form(&value_form, current_ns)?;
        fields.push(FieldMeta {
            name: field_name,
            schema,
        });
    }
    Ok(fields)
}

fn fields_from_value(
    value: &Value,
    span: Span,
    current_ns: &str,
) -> Result<Vec<FieldMeta>, CloveError> {
    let mut fields = Vec::new();
    let mut seen = StdHashSet::new();
    let iter: Vec<(&Key, &Value)> = match value {
        Value::Map(map) => map.iter().collect(),
        Value::SortedMap(map) => map.entries.iter().map(|(k, v)| (k, v)).collect(),
        _ => return Err(span_runtime_error(span, "deftype :from expects map value")),
    };
    for (key, value) in iter {
        let name = match key {
            Key::Keyword(name) | Key::Symbol(name) => name.clone(),
            _ => {
                return Err(span_runtime_error(
                    span,
                    "deftype :from expects keyword or symbol keys",
                ))
            }
        };
        if !seen.insert(name.clone()) {
            return Err(span_runtime_error(span, "duplicate field name in schema"));
        }
        fields.push(FieldMeta {
            name,
            schema: FieldSchema::TypeExpr(infer_type_expr_from_value(value, current_ns)),
        });
    }
    fields.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(fields)
}

fn fields_from_type_expr(
    kind: &MetaTypeKind,
    span: Span,
    current_ns: &str,
) -> Result<Vec<FieldMeta>, CloveError> {
    let resolved = resolve_type_expr(kind, span, current_ns)?;
    let MetaTypeKind::Record(fields) = resolved else {
        return Err(span_runtime_error(
            span,
            "deftype (def ...) expects record type expression",
        ));
    };
    let mut out = Vec::with_capacity(fields.len());
    for (name, field_kind) in fields {
        out.push(FieldMeta {
            name,
            schema: FieldSchema::TypeExpr(field_kind),
        });
    }
    out.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(out)
}

fn infer_type_expr_from_value(value: &Value, current_ns: &str) -> MetaTypeKind {
    if let Some(tag) = tagged_type_fqn(value) {
        return MetaTypeKind::named(tag);
    }
    if let Some(alias) = alias_fqn_for_value(value, Some(current_ns)) {
        return MetaTypeKind::named(alias);
    }
    match value {
        Value::Int(_) => MetaTypeKind::Int,
        Value::Float(_) => MetaTypeKind::Float,
        Value::Bool(_) => MetaTypeKind::Bool,
        Value::String(_) => MetaTypeKind::Str,
        Value::Nil => MetaTypeKind::Nil,
        Value::Vector(items) => {
            if items.is_empty() {
                return MetaTypeKind::vector(MetaTypeKind::any());
            }
            let inferred: Vec<_> = items
                .iter()
                .map(|item| infer_type_expr_from_value(item, current_ns))
                .collect();
            if inferred.windows(2).all(|pair| pair[0] == pair[1]) {
                MetaTypeKind::vector(inferred[0].clone())
            } else {
                MetaTypeKind::Tuple(inferred)
            }
        }
        Value::MutVector(_) => MetaTypeKind::vector(MetaTypeKind::any()),
        Value::TransientVector(handle) => handle
            .read("type", |kind| match kind {
                TransientKind::Vector(items) => {
                    if items.is_empty() {
                        return Ok(MetaTypeKind::vector(MetaTypeKind::any()));
                    }
                    let inferred: Vec<_> = items
                        .iter()
                        .map(|item| infer_type_expr_from_value(item, current_ns))
                        .collect();
                    if inferred.windows(2).all(|pair| pair[0] == pair[1]) {
                        Ok(MetaTypeKind::vector(inferred[0].clone()))
                    } else {
                        Ok(MetaTypeKind::Tuple(inferred))
                    }
                }
                _ => Ok(MetaTypeKind::any()),
            })
            .unwrap_or_else(|_| MetaTypeKind::any()),
        Value::List(_) => MetaTypeKind::named("clove::core::List"),
        Value::Map(map) => infer_type_expr_from_map(map.iter(), current_ns),
        Value::MutMap(_) => MetaTypeKind::map(MetaTypeKind::any(), MetaTypeKind::any()),
        Value::SortedMap(map) => {
            infer_type_expr_from_map(map.entries.iter().map(|(k, v)| (k, v)), current_ns)
        }
        Value::TransientMap(handle) => handle
            .read("type", |kind| match kind {
                TransientKind::Map(map) => Ok(infer_type_expr_from_map(map.iter(), current_ns)),
                _ => Ok(MetaTypeKind::any()),
            })
            .unwrap_or_else(|_| MetaTypeKind::any()),
        Value::Set(_) | Value::SortedSet(_) | Value::MutSet(_) => {
            MetaTypeKind::named("clove::core::Set")
        }
        Value::TransientSet(handle) => handle
            .read("type", |kind| match kind {
                TransientKind::Set(_) => Ok(MetaTypeKind::named("clove::core::Set")),
                _ => Ok(MetaTypeKind::any()),
            })
            .unwrap_or_else(|_| MetaTypeKind::any()),
        Value::Regex(_) => MetaTypeKind::named("clove::core::Regex"),
        Value::Duration(_) => MetaTypeKind::named("clove::core::Duration"),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => MetaTypeKind::named("clove::core::Function"),
        Value::Atom(_) => MetaTypeKind::named("clove::core::Atom"),
        Value::Chan(_) => MetaTypeKind::named("clove::core::Chan"),
        Value::Promise(_) => MetaTypeKind::named("clove::core::Promise"),
        Value::Task(_) => MetaTypeKind::named("clove::core::Task"),
        Value::Future(_) => MetaTypeKind::named("clove::core::Future"),
        Value::Agent(_) => MetaTypeKind::named("clove::core::Agent"),
        Value::Delay(_) => MetaTypeKind::named("clove::core::Delay"),
        Value::NativeBuf { ty, .. } => MetaTypeKind::named(format!(
            "clove::native::{}",
            crate::native_buf::native_buf_type_name(*ty)
        )),
        Value::Seq(_) => MetaTypeKind::named("clove::core::Seq"),
        Value::Symbol(_) => MetaTypeKind::named("clove::core::Symbol"),
        Value::Foreign(_) => MetaTypeKind::named("clove::core::Foreign"),
    }
}

fn infer_type_expr_from_map<'a, I>(entries: I, current_ns: &str) -> MetaTypeKind
where
    I: IntoIterator<Item = (&'a Key, &'a Value)>,
{
    let mut fields = ImHashMap::new();
    for (key, value) in entries {
        let name = match key {
            Key::Keyword(name) | Key::Symbol(name) => name.clone(),
            _ => {
                return MetaTypeKind::map(MetaTypeKind::any(), MetaTypeKind::any());
            }
        };
        fields.insert(name, infer_type_expr_from_value(value, current_ns));
    }
    MetaTypeKind::Record(fields)
}

fn parse_typed_field_list(
    forms: &[Form],
    current_ns: &str,
) -> Result<Option<Vec<FieldMeta>>, CloveError> {
    let items: &[Form] = if forms.len() == 1 {
        match &forms[0].kind {
            FormKind::Vector(items) | FormKind::List(items) => items,
            _ => return Ok(None),
        }
    } else {
        forms
    };
    if items.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut fields = Vec::new();
    let mut seen = StdHashSet::new();
    for item in items {
        let hint = match item.type_hint.as_ref() {
            Some(hint) => hint,
            None => return Ok(None),
        };
        let field_name = match &item.kind {
            FormKind::Keyword(name) | FormKind::Symbol(name) => name.clone(),
            _ => return Ok(None),
        };
        if !seen.insert(field_name.clone()) {
            return Err(span_runtime_error(
                item.span,
                "duplicate field name in schema",
            ));
        }
        let schema = field_schema_from_hint(hint, item.span, current_ns)?;
        fields.push(FieldMeta {
            name: field_name,
            schema,
        });
    }
    Ok(Some(fields))
}

fn field_schema_from_hint(
    hint: &TypeHint,
    span: Span,
    current_ns: &str,
) -> Result<FieldSchema, CloveError> {
    let resolved = resolve_type_expr(&hint.kind, span, current_ns)?;
    Ok(FieldSchema::TypeExpr(resolved))
}

fn field_schema_from_form(form: &Form, current_ns: &str) -> Result<FieldSchema, CloveError> {
    let kind = parse_type_from_form(form)?;
    let resolved = resolve_type_expr(&kind, form.span, current_ns)?;
    Ok(FieldSchema::TypeExpr(resolved))
}

fn resolve_type_expr(
    kind: &MetaTypeKind,
    span: Span,
    current_ns: &str,
) -> Result<MetaTypeKind, CloveError> {
    match kind {
        MetaTypeKind::Vector(inner) => Ok(MetaTypeKind::vector(resolve_type_expr(
            inner, span, current_ns,
        )?)),
        MetaTypeKind::Tuple(items) => {
            let mut resolved = Vec::new();
            for item in items {
                resolved.push(resolve_type_expr(item, span, current_ns)?);
            }
            Ok(MetaTypeKind::Tuple(resolved))
        }
        MetaTypeKind::Map(key, value) => Ok(MetaTypeKind::map(
            resolve_type_expr(key, span, current_ns)?,
            resolve_type_expr(value, span, current_ns)?,
        )),
        MetaTypeKind::Option(inner) => Ok(MetaTypeKind::option(resolve_type_expr(
            inner, span, current_ns,
        )?)),
        MetaTypeKind::Record(fields) => {
            let mut resolved = ImHashMap::new();
            for (name, value) in fields.iter() {
                resolved.insert(name.clone(), resolve_type_expr(value, span, current_ns)?);
            }
            Ok(MetaTypeKind::Record(resolved))
        }
        MetaTypeKind::Union(types) => {
            let mut resolved = Vec::new();
            for ty in types {
                resolved.push(resolve_type_expr(ty, span, current_ns)?);
            }
            Ok(MetaTypeKind::union(resolved))
        }
        MetaTypeKind::Function { params, rest, ret } => {
            let mut resolved_params = Vec::new();
            for param in params {
                resolved_params.push(resolve_type_expr(param, span, current_ns)?);
            }
            let resolved_rest = match rest.as_ref() {
                Some(rest_ty) => Some(resolve_type_expr(rest_ty, span, current_ns)?),
                None => None,
            };
            let resolved_ret = resolve_type_expr(ret, span, current_ns)?;
            Ok(MetaTypeKind::function(
                resolved_params,
                resolved_rest,
                resolved_ret,
            ))
        }
        MetaTypeKind::Named(name) => resolve_named_type_expr(name, span, current_ns),
        _ => Ok(kind.clone()),
    }
}

fn resolve_named_type_expr(
    name: &str,
    span: Span,
    current_ns: &str,
) -> Result<MetaTypeKind, CloveError> {
    if let Some(primitive) = PrimitiveType::from_symbol(name) {
        return Ok(MetaTypeKind::from_primitive_type(primitive));
    }
    let (base, suffix) = match name.find('<') {
        Some(idx) => (&name[..idx], Some(&name[idx..])),
        None => (name, None),
    };
    if let Some(variant) = enum_variant_lookup_with_ns(base, Some(current_ns)) {
        let resolved = match suffix {
            Some(suffix) => format!("{}{}", variant.variant_fqn, suffix),
            None => variant.variant_fqn,
        };
        return Ok(MetaTypeKind::named(resolved));
    }
    let (ns, local) = resolve_type_name(current_ns, base, span)?;
    let fqn = format!("{}::{}", ns, local);
    if let Some(entry) = type_registry::get_type_entry(&fqn) {
        return match entry.kind() {
            TypeKind::Primitive => {
                let primitive = PrimitiveType::from_symbol(entry.name()).ok_or_else(|| {
                    span_runtime_error(span, &format!("unknown primitive type '{}'", entry.name()))
                })?;
                Ok(MetaTypeKind::from_primitive_type(primitive))
            }
            TypeKind::Product | TypeKind::Sum | TypeKind::Alias => {
                let resolved = match suffix {
                    Some(suffix) => format!("{}{}", fqn, suffix),
                    None => fqn,
                };
                Ok(MetaTypeKind::named(resolved))
            }
        };
    }
    if !base.contains("::") {
        let fallback = format!("clove::core::{}", base);
        if let Some(entry) = type_registry::get_type_entry(&fallback) {
            return match entry.kind() {
                TypeKind::Primitive => {
                    let primitive = PrimitiveType::from_symbol(entry.name()).ok_or_else(|| {
                        span_runtime_error(
                            span,
                            &format!("unknown primitive type '{}'", entry.name()),
                        )
                    })?;
                    Ok(MetaTypeKind::from_primitive_type(primitive))
                }
                TypeKind::Product | TypeKind::Sum | TypeKind::Alias => {
                    let resolved = match suffix {
                        Some(suffix) => format!("{}{}", fallback, suffix),
                        None => fallback,
                    };
                    Ok(MetaTypeKind::named(resolved))
                }
            };
        }
    }
    Err(span_runtime_error(
        span,
        &format!("unknown type '{}'", name),
    ))
}

fn collect_field_pairs(items: &[Form]) -> Result<Vec<(Form, Form)>, CloveError> {
    if items.is_empty() {
        return Ok(Vec::new());
    }
    if items.len() % 2 != 0 {
        let span = items.last().unwrap().span;
        return Err(span_runtime_error(
            span,
            "field specification expects even number of forms",
        ));
    }
    let mut pairs = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        pairs.push((items[idx].clone(), items[idx + 1].clone()));
        idx += 2;
    }
    Ok(pairs)
}

fn ensure_known_field_keys(
    type_name: &str,
    field_names: &StdHashSet<String>,
    data: &HashMap<Key, Value>,
) -> Result<(), CloveError> {
    for key in data.keys() {
        match key {
            Key::Keyword(name) => {
                if !field_names.contains(name) {
                    return Err(CloveError::runtime(format!(
                        "unknown field :{} for type {}",
                        name, type_name
                    )));
                }
            }
            _ => return Err(CloveError::runtime("type constructor expects keyword keys")),
        }
    }
    Ok(())
}

fn make_type_constructor(
    type_name: &str,
    allow_empty: bool,
    methods: HashMap<Key, Value>,
) -> Value {
    let type_name = type_name.to_string();
    let methods = methods;
    let min_args = if allow_empty { 0 } else { 1 };
    Value::native_fn(FnArity::at_least(min_args), move |args| {
        let field_names = match type_registry::get_type_entry(&type_name) {
            Some(TypeEntry::Product(meta)) => meta
                .fields
                .iter()
                .map(|field| field.name.clone())
                .collect::<StdHashSet<_>>(),
            _ => {
                return Err(CloveError::runtime(format!(
                    "unknown product type '{}'",
                    type_name
                )))
            }
        };
        match args {
            [] if allow_empty => {
                let data = HashMap::new();
                type_registry::validate_product_fields(&type_name, &data)?;
                let mut tagged = type_registry::add_type_tag(data, &type_name);
                if !methods.is_empty() {
                    for (key, value) in &methods {
                        tagged.insert(key.clone(), value.clone());
                    }
                }
                Ok(Value::Map(tagged))
            }
            [Value::Map(data)] => {
                ensure_known_field_keys(&type_name, &field_names, data)?;
                type_registry::validate_product_fields(&type_name, data)?;
                let mut tagged = type_registry::add_type_tag(data.clone(), &type_name);
                if !methods.is_empty() {
                    for (key, value) in &methods {
                        tagged.insert(key.clone(), value.clone());
                    }
                }
                Ok(Value::Map(tagged))
            }
            _ => {
                if let Some(Value::Symbol(sym)) = args.first() {
                    if sym.starts_with(':') {
                        if args.len() % 2 != 0 {
                            return Err(CloveError::runtime(
                                "type constructor expects even number of keyword arguments",
                            ));
                        }
                        let mut data = HashMap::new();
                        let mut seen = StdHashSet::new();
                        let mut idx = 0;
                        while idx < args.len() {
                            let key = match &args[idx] {
                                Value::Symbol(sym) if sym.starts_with(':') => {
                                    sym.trim_start_matches(':').to_string()
                                }
                                _ => {
                                    return Err(CloveError::runtime(
                                        "type constructor expects keyword keys",
                                    ))
                                }
                            };
                            if !seen.insert(key.clone()) {
                                return Err(CloveError::runtime(format!(
                                    "duplicate field :{} for type {}",
                                    key, type_name
                                )));
                            }
                            if !field_names.contains(&key) {
                                return Err(CloveError::runtime(format!(
                                    "unknown field :{} for type {}",
                                    key, type_name
                                )));
                            }
                            data.insert(Key::Keyword(key), args[idx + 1].clone());
                            idx += 2;
                        }
                        type_registry::validate_product_fields(&type_name, &data)?;
                        let mut tagged = type_registry::add_type_tag(data, &type_name);
                        if !methods.is_empty() {
                            for (key, value) in &methods {
                                tagged.insert(key.clone(), value.clone());
                            }
                        }
                        return Ok(Value::Map(tagged));
                    }
                }
                Err(CloveError::runtime("type constructor expects map argument"))
            }
        }
    })
}

fn make_alias_constructor(type_name: &str, target: MetaTypeKind) -> Value {
    let type_name = type_name.to_string();
    let display = type_name
        .rsplit_once("::")
        .map(|(_, local)| local.to_string())
        .unwrap_or_else(|| type_name.clone());
    Value::native_fn(FnArity::exact(1), move |args| {
        let value = match args {
            [value] => value,
            _ => unreachable!(),
        };
        if type_registry::matches_type_expr(&target, value) {
            Ok(value.clone())
        } else {
            Err(CloveError::runtime(format!(
                "alias {} expects value of type {}",
                display,
                target.describe()
            )))
        }
    })
}

fn make_alias_predicate(target: MetaTypeKind) -> Value {
    Value::native_fn(FnArity::exact(1), move |args| {
        let result = match args {
            [value] => type_registry::matches_type_expr(&target, value),
            _ => unreachable!(),
        };
        Ok(Value::Bool(result))
    })
}

fn make_type_predicate(type_name: &str) -> Value {
    let type_name = type_name.to_string();
    Value::native_fn(FnArity::exact(1), move |args| {
        let result = match args {
            [value] => type_registry::conforms_named(&type_name, value),
            _ => unreachable!(),
        };
        Ok(Value::Bool(result))
    })
}

fn ensure_product_type_exists(
    namespace: &str,
    local: &str,
    env: EnvRef,
    bind_unqualified: bool,
) -> Result<(), CloveError> {
    let fqn = format!("{}::{}", namespace, local);
    let mut fields_empty = true;
    match type_registry::get_type_entry(&fqn) {
        Some(TypeEntry::Product(meta)) => {
            fields_empty = meta.fields.is_empty();
        }
        Some(_) => {
            return Ok(());
        }
        None => {
            let meta = ProductMeta {
                namespace: namespace.to_string(),
                name: local.to_string(),
                doc: None,
                fields: Vec::new(),
                belongs_to: StdHashSet::new(),
                required_methods: Vec::new(),
            };
            type_registry::register_product(meta)?;
        }
    }
    if bind_unqualified {
        let constructor = make_type_constructor(&fqn, fields_empty, HashMap::new());
        let predicate = make_type_predicate(&fqn);
        env.write().unwrap().set(local, constructor);
        env.write().unwrap().set(&format!("{}?", local), predicate);
    }
    Ok(())
}

fn register_variant_type_alias(
    current_ns: &str,
    local: &str,
    target_fqn: &str,
    span: Span,
) -> Result<(), CloveError> {
    let alias_fqn = format!("{}::{}", current_ns, local);
    if let Some(entry) = type_registry::get_type_entry(&alias_fqn) {
        if let TypeEntry::Alias(meta) = entry {
            if meta.target == MetaTypeKind::named(target_fqn.to_string()) {
                return Ok(());
            }
        }
        return Ok(());
    }
    let meta = AliasMeta {
        namespace: current_ns.to_string(),
        name: local.to_string(),
        doc: None,
        target: MetaTypeKind::named(target_fqn.to_string()),
    };
    type_registry::register_alias(meta).map_err(|err| err.with_span(span))?;
    Ok(())
}

#[derive(Clone, Debug)]
struct DeftypeFrom {
    name: String,
    value_form: Form,
    doc: Option<String>,
    meta_form: Option<Form>,
    type_expr: Option<MetaTypeKind>,
    type_span: Option<Span>,
    is_def: bool,
}

struct DeftypeMethodDef {
    name: String,
    value: Value,
    required: RequiredMethod,
}

fn parse_deftype_options(
    args: &[Form],
    start: usize,
    current_ns: &str,
) -> Result<(Option<MetaTypeKind>, Option<DeftypeFrom>, usize), CloveError> {
    let mut alias_target = None;
    let mut from_binding = None;
    let mut idx = start;
    while let Some(form) = args.get(idx) {
        let option_name = match &form.kind {
            FormKind::Keyword(name) => name.as_str(),
            _ => break,
        };
        match option_name {
            "alias" => {
                if alias_target.is_some() {
                    return Err(span_runtime_error(
                        form.span,
                        "deftype :alias specified more than once",
                    ));
                }
                let value_forms = args.get(idx + 1..).ok_or_else(|| {
                    span_runtime_error(form.span, "deftype :alias expects type expression")
                })?;
                if value_forms.is_empty() {
                    return Err(span_runtime_error(
                        form.span,
                        "deftype :alias expects type expression",
                    ));
                }
                let (kind, consumed) = parse_type_expr_from_forms(value_forms)?;
                let value_span = value_forms[0].span;
                let resolved = resolve_type_expr(&kind, value_span, current_ns)?;
                alias_target = Some(resolved);
                idx += 1 + consumed;
            }
            "from" => {
                if from_binding.is_some() {
                    return Err(span_runtime_error(
                        form.span,
                        "deftype :from specified more than once",
                    ));
                }
                let name_form = args.get(idx + 1).ok_or_else(|| {
                    span_runtime_error(form.span, "deftype :from expects symbol and value")
                })?;
                let value_form = args.get(idx + 2).ok_or_else(|| {
                    span_runtime_error(form.span, "deftype :from expects symbol and value")
                })?;
                let name = match &name_form.kind {
                    FormKind::Symbol(sym) => sym.clone(),
                    _ => {
                        return Err(span_runtime_error(
                            name_form.span,
                            "deftype :from expects symbol",
                        ))
                    }
                };
                from_binding = Some(DeftypeFrom {
                    name,
                    value_form: value_form.clone(),
                    doc: None,
                    meta_form: None,
                    type_expr: None,
                    type_span: None,
                    is_def: false,
                });
                idx += 3;
            }
            _ => break,
        }
    }
    Ok((alias_target, from_binding, idx))
}

fn parse_defenum_options(args: &[Form], start: usize) -> Result<(bool, usize), CloveError> {
    let mut qualified_only = false;
    let mut idx = start;
    while let Some(form) = args.get(idx) {
        let option_name = match &form.kind {
            FormKind::Keyword(name) => name.as_str(),
            _ => break,
        };
        let value_form = args
            .get(idx + 1)
            .ok_or_else(|| span_runtime_error(form.span, "defenum option expects a value"))?;
        match option_name {
            "qualified-only" => match &value_form.kind {
                FormKind::Bool(value) => qualified_only = *value,
                _ => {
                    return Err(span_runtime_error(
                        value_form.span,
                        "defenum :qualified-only expects boolean",
                    ))
                }
            },
            _ => {
                return Err(span_runtime_error(
                    form.span,
                    &format!("unknown defenum option :{}", option_name),
                ))
            }
        }
        idx += 2;
    }
    Ok((qualified_only, idx))
}

fn collect_enum_members(
    forms: &[Form],
    current_ns: &str,
    enum_fqn: &str,
) -> Result<Vec<String>, CloveError> {
    let mut ordered = Vec::new();
    let mut seen = StdHashSet::new();
    let mut idx = 0;
    while idx < forms.len() {
        let form = &forms[idx];
        let sym = match &form.kind {
            FormKind::Symbol(s) => s.clone(),
            _ => {
                return Err(span_runtime_error(
                    form.span,
                    "defenum members must be symbols",
                ));
            }
        };
        if sym.starts_with('*') {
            let target_raw = &sym[1..];
            if target_raw.is_empty() {
                return Err(span_runtime_error(
                    form.span,
                    "spread target cannot be empty",
                ));
            }
            let (ns, local) = resolve_type_name(current_ns, target_raw, form.span)?;
            let spread_fqn = format!("{}::{}", ns, local);
            if spread_fqn == enum_fqn {
                return Err(span_runtime_error(
                    form.span,
                    "defenum cannot spread itself",
                ));
            }
            match type_registry::get_type_entry(&spread_fqn) {
                Some(TypeEntry::Sum(meta)) => {
                    for member in &meta.members {
                        if seen.insert(member.clone()) {
                            ordered.push(member.clone());
                        }
                    }
                }
                Some(_) => {
                    return Err(span_runtime_error(form.span, "spread target must be enum"));
                }
                None => {
                    return Err(span_runtime_error(
                        form.span,
                        &format!("unknown enum '{}'", target_raw),
                    ));
                }
            }
            idx += 1;
            continue;
        }
        let payload_form = forms
            .get(idx + 1)
            .filter(|next| matches!(next.kind, FormKind::Map(_)));
        let (ns, local) = resolve_type_name(current_ns, &sym, form.span)?;
        if ns != current_ns {
            return Err(span_runtime_error(
                form.span,
                "defenum member types must be in the current namespace",
            ));
        }
        if payload_form.is_some() {
            let fqn = format!("{}::{}", enum_fqn, local);
            match type_registry::get_type_entry(&fqn) {
                Some(TypeEntry::Product(_)) => {
                    if seen.insert(fqn.clone()) {
                        ordered.push(fqn);
                    }
                }
                Some(_) => {
                    return Err(span_runtime_error(
                        form.span,
                        "defenum members must refer to product types",
                    ));
                }
                None => {
                    return Err(span_runtime_error(
                        form.span,
                        &format!("unknown type '{}'", sym),
                    ));
                }
            }
            idx += 2;
            continue;
        }
        let fqn = format!("{}::{}", enum_fqn, local);
        match type_registry::get_type_entry(&fqn) {
            Some(TypeEntry::Product(_)) => {
                if seen.insert(fqn.clone()) {
                    ordered.push(fqn);
                }
            }
            Some(_) => {
                return Err(span_runtime_error(
                    form.span,
                    "defenum members must refer to product types",
                ));
            }
            None => {
                return Err(span_runtime_error(
                    form.span,
                    &format!("unknown type '{}'", sym),
                ));
            }
        }
        idx += 1;
    }
    Ok(ordered)
}

fn build_pattern(form: &Form, current_ns: Option<&str>, span: Span) -> Result<Pattern, CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => build_symbol_pattern(sym, form.span, current_ns),
        FormKind::Keyword(kw) => Ok(Pattern::Literal(Value::Symbol(kw.clone()))),
        FormKind::Bool(b) => Ok(Pattern::Literal(Value::Bool(*b))),
        FormKind::Int(n) => Ok(Pattern::Literal(Value::Int(*n))),
        FormKind::Float(n) => Ok(Pattern::Literal(Value::Float(*n))),
        FormKind::String(s) => Ok(Pattern::Literal(Value::String(s.clone()))),
        FormKind::Nil => Ok(Pattern::Literal(Value::Nil)),
        FormKind::Vector(items) => build_vector_pattern(items, current_ns),
        FormKind::Map(entries) => build_map_pattern(entries, current_ns),
        FormKind::List(items) => {
            if is_match_or_form(items) {
                build_or_pattern(items, current_ns, form.span)
            } else {
                build_list_pattern(items, current_ns, form.span)
            }
        }
        _ => Err(span_runtime_error(span, "unsupported pattern form")),
    }
}

fn build_symbol_pattern(
    sym: &str,
    span: Span,
    current_ns: Option<&str>,
) -> Result<Pattern, CloveError> {
    if sym == "_" {
        return Ok(Pattern::Wildcard);
    }
    if let Some(variant) = enum_variant_lookup_with_ns(sym, current_ns) {
        return Ok(Pattern::Type {
            fqn: variant.variant_fqn,
            inner: None,
        });
    }
    if let Some(variant) = enum_variant_lookup_unqualified(sym, current_ns, span)? {
        return Ok(Pattern::Type {
            fqn: variant.variant_fqn,
            inner: None,
        });
    }
    let default_ns = current_ns.unwrap_or("user");
    if let Ok((ns, local)) = resolve_type_name(default_ns, sym, span) {
        let fqn = format!("{}::{}", ns, local);
        if let Some(entry) = type_registry::get_type_entry(&fqn) {
            return match entry.kind() {
                TypeKind::Product | TypeKind::Alias => Ok(Pattern::Type { fqn, inner: None }),
                TypeKind::Sum => Err(span_runtime_error(
                    span,
                    "match pattern cannot use enum directly",
                )),
                TypeKind::Primitive => Err(span_runtime_error(
                    span,
                    "use literals for primitive patterns",
                )),
            };
        }
    }
    Ok(Pattern::Bind(sym.to_string()))
}

fn build_vector_pattern(items: &[Form], current_ns: Option<&str>) -> Result<Pattern, CloveError> {
    let mut patterns = Vec::new();
    let mut rest = None;
    let mut idx = 0;
    while idx < items.len() {
        if let FormKind::Symbol(sym) = &items[idx].kind {
            if sym == "&" {
                if rest.is_some() || idx + 1 >= items.len() {
                    return Err(span_runtime_error(
                        items[idx].span,
                        "invalid vector rest pattern",
                    ));
                }
                rest = Some(Box::new(build_pattern(
                    &items[idx + 1],
                    current_ns,
                    items[idx + 1].span,
                )?));
                idx += 2;
                if idx < items.len() {
                    return Err(span_runtime_error(
                        items[idx].span,
                        "rest pattern must appear at end",
                    ));
                }
                break;
            }
        }
        patterns.push(build_pattern(&items[idx], current_ns, items[idx].span)?);
        idx += 1;
    }
    Ok(Pattern::Vector(patterns, rest))
}

fn build_map_pattern(entries: &[MapItem], current_ns: Option<&str>) -> Result<Pattern, CloveError> {
    let mut patterns = Vec::new();
    for entry in entries {
        match entry {
            MapItem::KeyValue(key_form, value_form) => {
                let field = match &key_form.kind {
                    FormKind::Keyword(name) => name.clone(),
                    _ => {
                        return Err(span_runtime_error(
                            key_form.span,
                            "map pattern keys must be keywords",
                        ))
                    }
                };
                let pat = build_pattern(value_form, current_ns, value_form.span)?;
                patterns.push((field, pat));
            }
            MapItem::Spread(expr) => {
                return Err(span_runtime_error(
                    expr.span,
                    "map pattern does not support spread",
                ))
            }
        }
    }
    Ok(Pattern::Map(patterns))
}

fn is_match_or_form(items: &[Form]) -> bool {
    matches!(
        items.first().map(|form| &form.kind),
        Some(FormKind::Symbol(sym)) if sym == MATCH_OR_SYM
    )
}

fn build_or_pattern(
    items: &[Form],
    current_ns: Option<&str>,
    span: Span,
) -> Result<Pattern, CloveError> {
    if items.len() < 2 {
        return Err(span_runtime_error(
            span,
            "match-or expects at least one pattern",
        ));
    }
    let mut patterns = Vec::with_capacity(items.len() - 1);
    for item in &items[1..] {
        patterns.push(build_pattern(item, current_ns, item.span)?);
    }
    Ok(Pattern::Or(patterns))
}

fn build_list_pattern(
    items: &[Form],
    current_ns: Option<&str>,
    span: Span,
) -> Result<Pattern, CloveError> {
    if items.is_empty() {
        return Err(span_runtime_error(
            span,
            "empty list pattern is not allowed",
        ));
    }
    if let FormKind::Symbol(sym) = &items[0].kind {
        if let Some(variant) = enum_variant_lookup_with_ns(sym, current_ns) {
            return build_type_pattern_with_items(variant.variant_fqn, items, current_ns, span);
        }
        if let Some(variant) = enum_variant_lookup_unqualified(sym, current_ns, items[0].span)? {
            return build_type_pattern_with_items(variant.variant_fqn, items, current_ns, span);
        }
        let default_ns = current_ns.unwrap_or("user");
        if let Ok((ns, local)) = resolve_type_name(default_ns, sym, items[0].span) {
            let fqn = format!("{}::{}", ns, local);
            match type_registry::get_type_entry(&fqn) {
                Some(TypeEntry::Product(_)) | Some(TypeEntry::Alias(_)) => {
                    return build_type_pattern_with_items(fqn, items, current_ns, span);
                }
                Some(TypeEntry::Sum(_)) => {
                    return Err(span_runtime_error(
                        span,
                        "match pattern cannot use enum directly",
                    ))
                }
                Some(TypeEntry::Primitive(_)) => {}
                None => {}
            }
        }
    }
    Err(span_runtime_error(span, "unsupported list pattern"))
}

fn keyword_shorthand_fields(items: &[Form]) -> Option<Vec<(String, Span)>> {
    if items.is_empty() {
        return None;
    }
    let mut fields = Vec::with_capacity(items.len());
    for item in items {
        match &item.kind {
            FormKind::Keyword(name) => fields.push((name.clone(), item.span)),
            _ => return None,
        }
    }
    Some(fields)
}

fn keyword_shorthand_map_items(fields: &[(String, Span)]) -> Vec<MapItem> {
    fields
        .iter()
        .map(|(name, span)| {
            let key = Form::new(FormKind::Keyword(name.clone()), *span);
            let value = Form::new(FormKind::Symbol(name.clone()), *span);
            MapItem::KeyValue(key, value)
        })
        .collect()
}

fn build_type_pattern_with_items(
    fqn: String,
    items: &[Form],
    current_ns: Option<&str>,
    span: Span,
) -> Result<Pattern, CloveError> {
    if items.len() == 1 {
        return Ok(Pattern::Type { fqn, inner: None });
    }
    if let Some(fields) = keyword_shorthand_fields(&items[1..]) {
        let entries = fields
            .into_iter()
            .map(|(name, _)| (name.clone(), Pattern::Bind(name)))
            .collect();
        return Ok(Pattern::Type {
            fqn,
            inner: Some(Box::new(Pattern::Map(entries))),
        });
    }
    if items.len() == 2 {
        let inner = build_pattern(&items[1], current_ns, items[1].span)?;
        return Ok(Pattern::Type {
            fqn,
            inner: Some(Box::new(inner)),
        });
    }
    Err(span_runtime_error(
        span,
        "type pattern accepts at most one inner pattern",
    ))
}

fn error_to_value(err: &CloveError) -> Value {
    match err {
        CloveError::Thrown(v, _) => v.clone(),
        other => Value::String(error_message(other)),
    }
}

fn is_runtime_error(err: &CloveError) -> bool {
    matches!(
        err,
        CloveError::UnboundSymbol(_)
            | CloveError::Message(_)
            | CloveError::Arity(_)
            | CloveError::TypeMismatch { .. }
            | CloveError::Foreign { .. }
            | CloveError::Parse(_)
            | CloveError::Other(_)
    )
}

fn error_message(err: &CloveError) -> String {
    let text = err.to_string();
    if let Some(stripped) = text.strip_prefix("Runtime error: ") {
        stripped.to_string()
    } else {
        text
    }
}

pub(crate) fn form_to_value(form: &Form) -> Result<Value, CloveError> {
    match &form.kind {
        FormKind::Int(n) => Ok(Value::Int(*n)),
        FormKind::Float(n) => Ok(Value::Float(*n)),
        FormKind::String(s) => Ok(Value::String(s.clone())),
        FormKind::InterpolatedString(_) => {
            if let Some(lowered) = desugar_interpolated_string(form) {
                form_to_value(&lowered)
            } else {
                Err(span_runtime_error(
                    form.span,
                    "invalid interpolated string expansion",
                ))
            }
        }
        FormKind::InterpolatedRegex { .. } => {
            if let Some(lowered) = desugar_interpolated_regex(form) {
                form_to_value(&lowered)
            } else {
                Err(span_runtime_error(
                    form.span,
                    "invalid interpolated regex expansion",
                ))
            }
        }
        FormKind::Bool(b) => Ok(Value::Bool(*b)),
        FormKind::Nil => Ok(Value::Nil),
        FormKind::Duration(d) => Ok(Value::Duration(*d)),
        FormKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        FormKind::Keyword(k) => Ok(Value::Symbol(format!(":{}", k))),
        FormKind::Regex { pattern, .. } => RegexValue::new(pattern.clone()).map(Value::Regex),
        FormKind::ShortFn(body) => {
            let expanded = expand_short_fn_to_list(body, form.span);
            form_to_value(&expanded)
        }
        FormKind::List(items) => {
            let mut vals = Vector::new();
            for it in items {
                vals.push_back(form_to_value(it)?);
            }
            Ok(Value::List(vals))
        }
        FormKind::Vector(items) => {
            let mut vals = Vector::new();
            for it in items {
                vals.push_back(form_to_value(it)?);
            }
            Ok(Value::Vector(vals))
        }
        FormKind::Map(entries) => {
            let mut map = HashMap::new();
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        if let Ok(key) = to_key(k) {
                            map.insert(key, form_to_value(v)?);
                        }
                    }
                    MapItem::Spread(expr) => {
                        return Err(span_runtime_error(
                            expr.span,
                            "quote does not support map spread",
                        ));
                    }
                }
            }
            Ok(Value::Map(map))
        }
        FormKind::Set(items) => Ok(Value::Set(
            items
                .iter()
                .map(|it| form_to_value(it))
                .collect::<Result<HashSet<_>, _>>()?,
        )),
        FormKind::ForeignBlock { tag, code } => Ok(Value::Foreign(ForeignValue {
            tag: tag.clone(),
            data: Arc::new(code.clone()),
        })),
        FormKind::ForeignRaw { tag, code } => Ok(Value::Foreign(ForeignValue {
            tag: tag.clone().unwrap_or_default(),
            data: Arc::new(code.clone()),
        })),
        FormKind::ForeignSymbol { tag, path } => Ok(Value::Foreign(ForeignValue {
            tag: tag.clone().unwrap_or_default(),
            data: Arc::new(path.clone()),
        })),
    }
}

fn build_list_source(head: &str, args: &[Form], span: Span) -> String {
    let mut items = Vec::with_capacity(args.len() + 1);
    items.push(Form::new(FormKind::Symbol(head.to_string()), span));
    items.extend_from_slice(args);
    let form = Form::new(FormKind::List(items), span);
    form_source::form_to_source(&form)
}

fn doc_from_value(value: &Value) -> Option<String> {
    match value {
        Value::Lambda { doc, .. } | Value::MultiLambda { doc, .. } => normalize_doc(doc.clone()),
        _ => None,
    }
}

fn source_from_value(value: &Value) -> Option<String> {
    match value {
        Value::Lambda {
            params, rest, body, ..
        } => Some(format!(
            "(fn {} {})",
            format_arglist(params, rest.as_ref()),
            format_body_source(body)
        )),
        Value::MultiLambda { clauses, .. } => {
            let parts: Vec<String> = clauses
                .iter()
                .map(|clause| {
                    format!(
                        "({} {})",
                        format_arglist(&clause.params, clause.rest.as_ref()),
                        format_body_source(&clause.body)
                    )
                })
                .collect();
            Some(format!("(fn {})", parts.join(" ")))
        }
        _ => None,
    }
}

fn format_arglist(params: &[String], rest: Option<&String>) -> String {
    let mut parts = Vec::new();
    parts.extend(params.iter().cloned());
    if let Some(rest_name) = rest {
        parts.push("&".into());
        parts.push(rest_name.clone());
    }
    format!("[{}]", parts.join(" "))
}

fn format_body_source(body: &[Form]) -> String {
    if body.is_empty() {
        "nil".into()
    } else {
        body.iter()
            .map(form_source::form_to_source)
            .collect::<Vec<_>>()
            .join(" ")
    }
}

fn normalize_doc(doc: Option<String>) -> Option<String> {
    doc.and_then(|text| {
        if text.trim().is_empty() {
            None
        } else {
            Some(text)
        }
    })
}

fn to_key(form: &Form) -> Result<Key, CloveError> {
    match &form.kind {
        FormKind::Keyword(k) => Ok(Key::Keyword(k.clone())),
        FormKind::Symbol(s) => Ok(Key::Symbol(s.clone())),
        FormKind::String(s) => Ok(Key::String(s.clone())),
        FormKind::Int(n) => Ok(Key::Number(*n)),
        FormKind::Float(n) => Ok(Key::Number(*n as i64)),
        FormKind::List(items)
            if items.len() == 2
                && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "quote") =>
        {
            match &items[1].kind {
                FormKind::Symbol(sym) => Ok(Key::Symbol(sym.clone())),
                _ => Err(span_runtime_error(form.span, "invalid map key")),
            }
        }
        _ => Err(span_runtime_error(form.span, "invalid map key")),
    }
}

pub fn to_key_value(val: Value) -> Key {
    match val {
        Value::Symbol(s) => {
            if let Some(name) = s.strip_prefix(':') {
                Key::Keyword(name.to_string())
            } else {
                Key::Symbol(s)
            }
        }
        Value::String(s) => Key::String(s),
        Value::Int(n) => Key::Number(n),
        Value::Float(n) => Key::Number(n as i64),
        Value::Bool(b) => Key::Bool(b),
        _ => Key::String(format!("{:?}", val)),
    }
}

pub fn to_key_value_checked(val: &Value) -> Result<Key, CloveError> {
    match val {
        Value::Symbol(s) => {
            if let Some(name) = s.strip_prefix(':') {
                Ok(Key::Keyword(name.to_string()))
            } else {
                Ok(Key::Symbol(s.clone()))
            }
        }
        Value::String(s) => Ok(Key::String(s.clone())),
        Value::Int(n) => Ok(Key::Number(*n)),
        Value::Float(n) => Ok(Key::Number(*n as i64)),
        Value::Bool(b) => Ok(Key::Bool(*b)),
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => Err(CloveError::runtime(
            "cannot use mutable collection as map key; use (imut x)",
        )),
        other => Ok(Key::String(format!("{:?}", other))),
    }
}

fn spread_symbol_form(form: &Form) -> Option<Form> {
    if let FormKind::Symbol(sym) = &form.kind {
        strip_spread_symbol(sym).map(|inner| Form::new(FormKind::Symbol(inner), form.span))
    } else {
        None
    }
}

fn spread_items(value: &Value, span: Span) -> Result<Vector<Value>, CloveError> {
    match value {
        Value::Nil
        | Value::List(_)
        | Value::Vector(_)
        | Value::Set(_)
        | Value::SortedSet(_)
        | Value::Seq(_)
        | Value::Map(_)
        | Value::SortedMap(_)
        | Value::MutVector(_)
        | Value::MutMap(_)
        | Value::MutSet(_) => seq_items(value),
        _ => Err(span_runtime_error(
            span,
            &format!("spread expects a collection, got {}", value.type_name()),
        )),
    }
}

fn spread_into_args(value: Value, args: &mut Vec<Value>, span: Span) -> Result<(), CloveError> {
    for item in spread_items(&value, span)? {
        args.push(item);
    }
    Ok(())
}

fn spread_into_vector(
    value: Value,
    target: &mut Vector<Value>,
    span: Span,
) -> Result<(), CloveError> {
    for item in spread_items(&value, span)? {
        target.push_back(item);
    }
    Ok(())
}

fn spread_into_set(
    value: Value,
    target: &mut HashSet<Value>,
    span: Span,
) -> Result<(), CloveError> {
    for item in spread_items(&value, span)? {
        target.insert(item);
    }
    Ok(())
}

fn spread_into_map(
    value: Value,
    target: &mut HashMap<Key, Value>,
    span: Span,
) -> Result<(), CloveError> {
    match value {
        Value::Nil => Ok(()),
        Value::Map(entries) => {
            for (k, v) in entries {
                target.insert(k, v);
            }
            Ok(())
        }
        Value::MutMap(handle) => {
            let map = handle.lock().unwrap_or_else(|e| e.into_inner());
            for (k, v) in map.iter() {
                target.insert(k.clone(), v.clone());
            }
            Ok(())
        }
        other => {
            let seq = match &other {
                Value::List(_)
                | Value::Vector(_)
                | Value::Set(_)
                | Value::Seq(_)
                | Value::MutVector(_)
                | Value::MutSet(_)
                | Value::MutMap(_) => spread_items(&other, span)?,
                _ => {
                    return Err(span_runtime_error(
                        span,
                        "map spread expects map or [k v] seq",
                    ));
                }
            };
            for item in seq {
                match item {
                    Value::Vector(v) | Value::List(v) if v.len() == 2 => {
                        let key = to_key_value_checked(&v[0])?;
                        target.insert(key, v[1].clone());
                    }
                    _ => {
                        return Err(span_runtime_error(
                            span,
                            "map spread expects map or [k v] seq",
                        ));
                    }
                }
            }
            Ok(())
        }
    }
}

pub fn key_to_value(k: &Key) -> Value {
    match k {
        Key::Keyword(s) => Value::Symbol(format!(":{}", s)),
        Key::Symbol(s) => Value::Symbol(s.clone()),
        Key::String(s) => Value::String(s.clone()),
        Key::Number(n) => Value::Int(*n),
        Key::Bool(b) => Value::Bool(*b),
    }
}

fn stack_label_for_list(items: &[Form]) -> String {
    if items.is_empty() {
        return "<list>".into();
    }
    match &items[0].kind {
        FormKind::Symbol(sym) => sym.clone(),
        FormKind::Keyword(kw) => kw.clone(),
        _ => form_to_string(&items[0], ""),
    }
}

fn callable_stack_label(callable: &Value) -> Option<String> {
    crate::ast::callable_label(callable)
}

fn callable_frame_meta(callable: &Value) -> Option<(Option<String>, Option<Span>)> {
    match callable {
        Value::Lambda { name, .. } | Value::MultiLambda { name, .. } => {
            let name = name.as_ref()?;
            fn_meta::get(name).map(|meta| (meta.source_file, meta.source_span))
        }
        Value::Partial { callable, .. } => callable_frame_meta(callable),
        _ => None,
    }
}

fn is_callable_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. }
    ) || matches!(value, Value::Symbol(sym) if sym.starts_with(':'))
}

fn is_method_value(value: &Value) -> bool {
    match meta_lookup(value) {
        Value::Map(map) => matches!(
            map.get(&Key::Keyword("is-method".into())),
            Some(Value::Bool(true))
        ),
        _ => false,
    }
}

fn mark_method_value(value: Value) -> Value {
    let mut meta = HashMap::new();
    meta.insert(Key::Keyword("is-method".into()), Value::Bool(true));
    attach_lambda_meta(value, Some(meta))
}

pub(crate) fn register_vm_multi_arity(func: &Arc<NativeFn>, clauses: Vec<(usize, bool)>) {
    let key = Arc::as_ptr(func) as usize;
    let entries = clauses
        .into_iter()
        .map(|(param_count, has_rest)| VmArityClause {
            param_count,
            has_rest,
        })
        .collect();
    let entry = VmArityEntry {
        func: Arc::downgrade(func),
        clauses: entries,
    };
    VM_ARITY_STORE.lock().unwrap().insert(key, entry);
}

fn vm_multi_arity_for(func: &Arc<NativeFn>) -> Option<Vec<VmArityClause>> {
    let key = Arc::as_ptr(func) as usize;
    let mut store = VM_ARITY_STORE.lock().unwrap();
    let entry = match store.get(&key) {
        Some(entry) => entry,
        None => return None,
    };
    if let Some(strong) = entry.func.upgrade() {
        if Arc::as_ptr(&strong) as usize == key {
            return Some(entry.clauses.clone());
        }
    }
    store.remove(&key);
    None
}

fn vm_multi_accepts(clauses: &[VmArityClause], provided: usize) -> bool {
    for clause in clauses {
        if clause.has_rest && provided >= clause.param_count {
            return true;
        }
        if !clause.has_rest && provided == clause.param_count {
            return true;
        }
    }
    false
}

fn value_accepts_n(value: &Value, n: usize) -> bool {
    match value {
        Value::Func(func) => match vm_multi_arity_for(func) {
            Some(clauses) => vm_multi_accepts(&clauses, n),
            None => matches!(check_arity(func.arity(), n), ArityCheck::Call),
        },
        Value::Lambda { params, rest, .. } => {
            let arity = lambda_arity(params.len(), rest.is_some());
            matches!(check_arity(arity, n), ArityCheck::Call)
        }
        Value::MultiLambda { clauses, .. } => {
            matches!(multi_arity_check(clauses, n), MultiArityCheck::Call(_))
        }
        Value::Partial { remaining, .. } => matches!(check_arity(*remaining, n), ArityCheck::Call),
        Value::Compose { funcs, kind } => {
            if funcs.is_empty() {
                return false;
            }
            match kind {
                ComposeKind::Comp => {
                    let (tail, last) = funcs.split_at(funcs.len() - 1);
                    if !value_accepts_n(&last[0], n) {
                        return false;
                    }
                    tail.iter().all(|func| value_accepts_n(func, 1))
                }
                ComposeKind::Pipe => {
                    let (first, tail) = funcs.split_at(1);
                    if !value_accepts_n(&first[0], n) {
                        return false;
                    }
                    tail.iter().all(|func| value_accepts_n(func, 1))
                }
            }
        }
        _ => false,
    }
}

fn profile_label_for_callable(callable: &Value) -> Option<String> {
    match callable {
        Value::Func(func) => func.debug_name().map(|name| name.to_string()),
        Value::Lambda {
            name: Some(name), ..
        } => Some(name.clone()),
        Value::MultiLambda {
            name: Some(name), ..
        } => Some(name.clone()),
        _ => None,
    }
}

pub(crate) fn call_native_func(func: &Arc<NativeFn>, args: &[Value]) -> Result<Value, CloveError> {
    if vm_multi_arity_for(func).is_some() {
        return func.call(args);
    }
    let arity = func.arity();
    match check_arity(arity, args.len()) {
        ArityCheck::Call => func.call(args),
        ArityCheck::NeedMore(_) => Err(arity_error(arity, args.len())),
        ArityCheck::TooMany => Err(arity_error(arity, args.len())),
    }
}

pub fn call_callable(callable: Value, args: Vec<Value>) -> Result<Value, CloveError> {
    let pending_frame = take_pending_frame();
    let call_form = take_pending_call_form();
    let fallback_label = callable_stack_label(&callable);
    let (fallback_file, fallback_span) = fallback_label
        .as_ref()
        .and_then(|_| callable_frame_meta(&callable))
        .unwrap_or((None, None));
    let call_span = pending_frame
        .as_ref()
        .and_then(|(_, span)| *span)
        .or(fallback_span);
    let call_file = if pending_frame.is_some() {
        current_file_name().or(fallback_file.clone())
    } else {
        fallback_file.clone()
    };
    if RuntimeCtx::debug_stash_enabled_fast() {
        let _ = RuntimeCtx::try_with_current(|ctx| {
            ctx.maybe_update_debug_stash(&callable, &args, call_span, call_file, call_form);
            Ok(())
        });
    }
    let _frame_guard = pending_frame
        .map(|(name, span)| push_stack_frame(name, span))
        .or_else(|| {
            fallback_label
                .map(|name| push_stack_frame_with_file(name, fallback_span, fallback_file.clone()))
        });
    let profile_label = profile_label_for_callable(&callable);
    let _profile_guard = profile_label.as_deref().and_then(profiler::enter);
    match callable {
        Value::Partial {
            ref callable,
            ref captured,
            remaining,
        } => match check_arity(remaining, args.len()) {
            ArityCheck::Call => {
                let mut all_args = captured.clone();
                all_args.extend_from_slice(&args);
                call_callable((**callable).clone(), all_args)
            }
            ArityCheck::NeedMore(rem) => {
                let mut next_captured = captured.clone();
                next_captured.extend_from_slice(&args);
                Ok(make_partial((**callable).clone(), next_captured, rem))
            }
            ArityCheck::TooMany => Err(arity_error(remaining, args.len())),
        },
        Value::Compose { ref funcs, kind } => match kind {
            ComposeKind::Comp => {
                let mut iter = funcs.iter().rev();
                let last = iter
                    .next()
                    .cloned()
                    .ok_or_else(|| CloveError::runtime("compose expects a function"))?;
                let mut acc = call_callable(last, args)?;
                for func in iter {
                    acc = call_callable(func.clone(), vec![acc])?;
                }
                Ok(acc)
            }
            ComposeKind::Pipe => {
                let mut iter = funcs.iter();
                let first = iter
                    .next()
                    .cloned()
                    .ok_or_else(|| CloveError::runtime("pipe expects a function"))?;
                let mut acc = call_callable(first, args)?;
                for func in iter {
                    acc = call_callable(func.clone(), vec![acc])?;
                }
                Ok(acc)
            }
        },
        Value::Func(func) => {
            if vm_multi_arity_for(&func).is_some() {
                return func.call(&args);
            }
            let arity = func.arity();
            match check_arity(arity, args.len()) {
                ArityCheck::Call => func.call(&args),
                ArityCheck::NeedMore(_remaining) => Err(arity_error(arity, args.len())),
                ArityCheck::TooMany => Err(arity_error(arity, args.len())),
            }
        }
        Value::Lambda {
            ref params,
            ref rest,
            ref body,
            ref local_defns,
            ref env,
            ref engines,
            auto_fallback,
            ref call_wrappers,
            ref doc,
            ref name,
            ref inferred_type,
            ref settings,
            ref meta,
            recur_id,
        } => {
            let callable_value = Value::Lambda {
                params: params.clone(),
                rest: rest.clone(),
                body: body.clone(),
                local_defns: local_defns.clone(),
                env: env.clone(),
                engines: engines.clone(),
                auto_fallback,
                call_wrappers: call_wrappers.clone(),
                doc: doc.clone(),
                name: name.clone(),
                inferred_type: inferred_type.clone(),
                settings: settings.clone(),
                meta: meta.clone(),
                recur_id,
            };
            let arity = lambda_arity(params.len(), rest.is_some());
            match check_arity(arity, args.len()) {
                ArityCheck::Call => invoke_lambda(
                    callable_value.clone(),
                    params.clone(),
                    rest.clone(),
                    body.clone(),
                    local_defns.clone(),
                    env.clone(),
                    engines.clone(),
                    auto_fallback,
                    call_wrappers.clone(),
                    name.clone(),
                    settings.clone(),
                    recur_id,
                    args,
                ),
                ArityCheck::NeedMore(_remaining) => Err(arity_error(arity, args.len())),
                ArityCheck::TooMany => Err(arity_error(arity, args.len())),
            }
        }
        Value::MultiLambda {
            ref clauses,
            ref env,
            ref engines,
            auto_fallback,
            ref call_wrappers,
            ref doc,
            ref name,
            ref inferred_type,
            ref settings,
            ref meta,
        } => {
            let callable_value = Value::MultiLambda {
                clauses: clauses.clone(),
                env: env.clone(),
                engines: engines.clone(),
                auto_fallback,
                call_wrappers: call_wrappers.clone(),
                doc: doc.clone(),
                name: name.clone(),
                inferred_type: inferred_type.clone(),
                settings: settings.clone(),
                meta: meta.clone(),
            };
            match multi_arity_check(clauses, args.len()) {
                MultiArityCheck::Call(idx) => {
                    let clause = clauses
                        .get(idx)
                        .ok_or_else(|| CloveError::runtime("invalid arity index"))?;
                    invoke_lambda(
                        callable_value.clone(),
                        clause.params.clone(),
                        clause.rest.clone(),
                        clause.body.clone(),
                        clause.local_defns.clone(),
                        env.clone(),
                        engines.clone(),
                        auto_fallback,
                        call_wrappers.clone(),
                        name.clone(),
                        settings.clone(),
                        clause.recur_id,
                        args,
                    )
                }
                MultiArityCheck::NeedMore(_remaining) => {
                    Err(multi_arity_error(clauses, args.len()))
                }
                MultiArityCheck::TooMany => Err(multi_arity_error(clauses, args.len())),
            }
        }
        Value::ForeignCallable {
            engine,
            path,
            span,
            tag,
        } => {
            if args.iter().any(contains_mut_collection) {
                return Err(CloveError::runtime(
                    "foreign call cannot accept mutable collection; use (imut x)",
                ));
            }
            engine
                .call_symbol(&path, &args, span)
                .map_err(|err| match err {
                    CloveError::Other(data) => CloveError::Foreign {
                        tag,
                        message: data.message,
                        context: data.context,
                    },
                    other => other,
                })
        }
        Value::Symbol(sym) => {
            if sym.starts_with(':') {
                let key = to_key_value(Value::Symbol(sym.clone()));
                let (target, default) = match args.as_slice() {
                    [map] => (map, None),
                    [map, fallback] => (map, Some(fallback)),
                    _ => {
                        return Err(CloveError::arity(format!(
                            "expected 1 or 2 args, got {}",
                            args.len()
                        )))
                    }
                };
                match target {
                    Value::Map(map) => Ok(map
                        .get(&key)
                        .cloned()
                        .unwrap_or_else(|| default.cloned().unwrap_or(Value::Nil))),
                    Value::Nil => Ok(default.cloned().unwrap_or(Value::Nil)),
                    other => Err(CloveError::type_mismatch("map", other.type_name())),
                }
            } else {
                Err(CloveError::type_mismatch("function", "symbol"))
            }
        }
        Value::Set(set) => {
            if args.len() != 1 {
                return Err(CloveError::arity(format!(
                    "expected 1 args, got {}",
                    args.len()
                )));
            }
            let probe = &args[0];
            if set.contains(probe) {
                Ok(probe.clone())
            } else {
                Ok(Value::Nil)
            }
        }
        Value::Regex(regex) => apply_regex(&regex, &args),
        Value::Foreign(_) => Err(CloveError::runtime("cannot call foreign value")),
        other => Err(CloveError::type_mismatch("function", other.type_name())),
    }
}

enum ArityCheck {
    Call,
    NeedMore(FnArity),
    TooMany,
}

fn check_arity(arity: FnArity, provided: usize) -> ArityCheck {
    if provided < arity.min() {
        return ArityCheck::NeedMore(arity.remaining_after(provided));
    }
    if let Some(max) = arity.max() {
        if provided > max {
            return ArityCheck::TooMany;
        }
    }
    ArityCheck::Call
}

fn lambda_arity(param_count: usize, has_rest: bool) -> FnArity {
    if has_rest {
        FnArity::at_least(param_count)
    } else {
        FnArity::exact(param_count)
    }
}

fn arity_error(arity: FnArity, provided: usize) -> CloveError {
    match arity.max() {
        Some(max) if arity.min() == max => {
            CloveError::arity(format!("expected {} args, got {}", max, provided))
        }
        Some(max) => CloveError::arity(format!(
            "expected between {} and {} args, got {}",
            arity.min(),
            max,
            provided
        )),
        None => CloveError::arity(format!(
            "expected at least {} args, got {}",
            arity.min(),
            provided
        )),
    }
}

enum MultiArityCheck {
    Call(usize),
    NeedMore(FnArity),
    TooMany,
}

fn multi_arity_check(clauses: &[LambdaClause], provided: usize) -> MultiArityCheck {
    let mut min_needed: Option<usize> = None;
    let mut max_needed: Option<usize> = None;
    let mut unbounded = false;
    for (idx, clause) in clauses.iter().enumerate() {
        let positional = clause.params.len();
        if clause.rest.is_none() && provided == positional {
            return MultiArityCheck::Call(idx);
        }
        if clause.rest.is_some() && provided >= positional {
            return MultiArityCheck::Call(idx);
        }
        if provided < positional {
            let needed = positional - provided;
            min_needed = Some(min_needed.map_or(needed, |current| current.min(needed)));
            if clause.rest.is_some() {
                unbounded = true;
            } else {
                max_needed = Some(max_needed.map_or(needed, |current| current.max(needed)));
            }
        }
    }
    if let Some(min_missing) = min_needed {
        let arity = if unbounded {
            FnArity::new(min_missing, None)
        } else {
            let max_missing = max_needed.unwrap_or(min_missing);
            FnArity::new(min_missing, Some(max_missing))
        };
        return MultiArityCheck::NeedMore(arity);
    }
    MultiArityCheck::TooMany
}

fn multi_arity_bounds(clauses: &[LambdaClause]) -> (usize, Option<usize>) {
    let mut min_args = usize::MAX;
    let mut max_args = None;
    for clause in clauses {
        let positional = clause.params.len();
        if positional < min_args {
            min_args = positional;
        }
        if clause.rest.is_none() {
            max_args = Some(max_args.map_or(positional, |current: usize| current.max(positional)));
        } else {
            max_args = None;
            break;
        }
    }
    if min_args == usize::MAX {
        min_args = 0;
    }
    (min_args, max_args)
}

fn multi_arity_error(clauses: &[LambdaClause], provided: usize) -> CloveError {
    let (min_args, max_args) = multi_arity_bounds(clauses);
    match max_args {
        Some(max) => CloveError::arity(format!(
            "expected between {} and {} args, got {}",
            min_args, max, provided
        )),
        None => CloveError::arity(format!(
            "expected at least {} args, got {}",
            min_args, provided
        )),
    }
}

fn make_partial(callable: Value, captured: Vec<Value>, remaining: FnArity) -> Value {
    Value::Partial {
        callable: Box::new(callable),
        captured,
        remaining,
    }
}

fn pack_lambda_args(mut args: Vec<Value>, positional_len: usize, has_rest: bool) -> Vec<Value> {
    if has_rest {
        let rest_values = if args.len() > positional_len {
            args.split_off(positional_len)
        } else {
            Vec::new()
        };
        let rest_list: Vector<_> = rest_values.into_iter().collect();
        args.push(Value::List(rest_list));
    }
    args
}

#[allow(clippy::too_many_arguments)]
fn invoke_lambda(
    lambda_value: Value,
    params: Vec<String>,
    rest: Option<String>,
    body: Vec<Form>,
    local_defns: Vec<LocalDefn>,
    env: EnvRef,
    engines: Vec<Arc<dyn ForeignEngine>>,
    auto_fallback: bool,
    call_wrappers: Arc<StdHashSet<String>>,
    name: Option<String>,
    settings: RuntimeSettings,
    recur_id: usize,
    args: Vec<Value>,
) -> Result<Value, CloveError> {
    struct FileGuard {
        prev: Option<String>,
    }
    impl Drop for FileGuard {
        fn drop(&mut self) {
            set_current_file(self.prev.clone());
        }
    }

    let positional_len = params.len();
    let has_rest = rest.is_some();
    let total_params = positional_len + if has_rest { 1 } else { 0 };
    let mut current_args = pack_lambda_args(args, positional_len, has_rest);
    if current_args.len() != total_params {
        return Err(CloveError::runtime(format!(
            "expected {} argument(s), got {}",
            total_params,
            current_args.len()
        )));
    }
    let _file_guard = if let Some(source_file) = lambda_source_file(&lambda_value) {
        let prev = current_file_name();
        set_current_file(Some(source_file));
        Some(FileGuard { prev })
    } else if let Some(fn_name) = &name {
        if let Some(meta) = fn_meta::get(fn_name) {
            if meta.source_file.is_some() {
                let prev = current_file_name();
                set_current_file(meta.source_file.clone());
                Some(FileGuard { prev })
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };
    loop {
        let child = new_ref(Env::new_child(env.clone()));
        {
            let mut writer = child.write().unwrap();
            if let Some(fn_name) = &name {
                writer.set(fn_name, lambda_value.clone());
            }
            for (idx, param) in params.iter().enumerate() {
                if let Some(value) = current_args.get(idx) {
                    writer.set(param, value.clone());
                }
            }
            if let Some(rest_name) = &rest {
                let rest_index = positional_len;
                let rest_value = current_args
                    .get(rest_index)
                    .cloned()
                    .unwrap_or_else(|| Value::List(Vector::new()));
                writer.set(rest_name, rest_value);
            }
        }
        install_local_defns(
            child.clone(),
            &local_defns,
            &engines,
            auto_fallback,
            &call_wrappers,
            &settings,
        );
        let guard = push_recur_context(RecurContext {
            id: recur_id,
            positional_count: positional_len,
            has_rest,
            kind: RecurKind::Function,
            name: name.clone(),
        });
        let mut evaluator = Evaluator::new_with_env_ref(env.clone(), &engines, auto_fallback);
        evaluator.set_settings(settings.clone());
        evaluator.set_call_wrappers_arc(call_wrappers.clone());
        if let Some(Ok(store)) = RuntimeCtx::try_with_current(|ctx| Ok(ctx.namespace_store())) {
            evaluator.set_namespace_store(store);
        }
        let result = evaluator.eval_do(&body, child.clone());
        drop(guard);
        match result {
            Ok(value) => return Ok(value),
            Err(CloveError::RecurSignal { target, values }) if target == recur_id => {
                if values.len() != total_params {
                    return Err(CloveError::runtime(format!(
                        "recur expected {} argument(s), got {}",
                        total_params,
                        values.len()
                    )));
                }
                current_args = values;
            }
            Err(err) => return Err(err),
        }
    }
}

fn install_local_defns(
    env: EnvRef,
    local_defns: &[LocalDefn],
    engines: &[Arc<dyn ForeignEngine>],
    auto_fallback: bool,
    call_wrappers: &Arc<StdHashSet<String>>,
    settings: &RuntimeSettings,
) {
    if local_defns.is_empty() {
        return;
    }
    let mut writer = env.write().unwrap();
    for defn in local_defns {
        if defn.name == "_" {
            continue;
        }
        let value = build_local_defn_value(
            defn,
            env.clone(),
            engines,
            auto_fallback,
            call_wrappers.clone(),
            settings.clone(),
        );
        writer.set(&defn.name, value);
    }
}

fn build_local_defn_value(
    defn: &LocalDefn,
    env: EnvRef,
    engines: &[Arc<dyn ForeignEngine>],
    auto_fallback: bool,
    call_wrappers: Arc<StdHashSet<String>>,
    settings: RuntimeSettings,
) -> Value {
    if defn.clauses.len() == 1 {
        let clause = &defn.clauses[0];
        Value::Lambda {
            params: clause.params.clone(),
            rest: clause.rest.clone(),
            body: clause.body.clone(),
            local_defns: clause.local_defns.clone(),
            env,
            engines: engines.to_vec(),
            auto_fallback,
            call_wrappers,
            settings,
            meta: defn.meta.clone(),
            doc: defn.doc.clone(),
            name: Some(defn.name.clone()),
            inferred_type: clause.inferred_type.clone(),
            recur_id: clause.recur_id,
        }
    } else {
        let mut clauses = Vec::with_capacity(defn.clauses.len());
        for clause in &defn.clauses {
            clauses.push(LambdaClause {
                params: clause.params.clone(),
                rest: clause.rest.clone(),
                body: clause.body.clone(),
                local_defns: clause.local_defns.clone(),
                recur_id: clause.recur_id,
            });
        }
        Value::MultiLambda {
            clauses,
            env,
            engines: engines.to_vec(),
            auto_fallback,
            call_wrappers,
            settings,
            meta: defn.meta.clone(),
            doc: defn.doc.clone(),
            name: Some(defn.name.clone()),
            inferred_type: None,
        }
    }
}

#[derive(Clone, Copy)]
enum QuestionPlaceholderKind {
    Value,
    Spread,
}

#[derive(Default)]
struct PlaceholderAlloc {
    id: usize,
    used_indices: StdHashSet<usize>,
    next_auto: usize,
    max_index: usize,
    found: bool,
}

impl PlaceholderAlloc {
    fn new() -> Self {
        Self {
            id: PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst),
            used_indices: StdHashSet::new(),
            next_auto: 1,
            max_index: 0,
            found: false,
        }
    }

    fn name_for_index(&self, index: usize) -> String {
        format!("__hole{}__auto{}", index, self.id)
    }

    fn alloc_auto(&mut self) -> usize {
        let mut idx = self.next_auto.max(1);
        loop {
            if !self.used_indices.contains(&idx) {
                self.used_indices.insert(idx);
                self.max_index = self.max_index.max(idx);
                self.found = true;
                self.next_auto = idx + 1;
                return idx;
            }
            idx += 1;
        }
    }

    fn use_index(&mut self, index: usize) {
        self.used_indices.insert(index);
        self.max_index = self.max_index.max(index);
        self.found = true;
    }
}

fn parse_positive_placeholder_index(text: &str) -> Option<usize> {
    if text.is_empty() || !text.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    let index = text.parse::<usize>().ok()?;
    if index == 0 {
        None
    } else {
        Some(index)
    }
}

fn parse_question_placeholder(sym: &str) -> Option<(QuestionPlaceholderKind, Option<usize>)> {
    if sym == "?" {
        return Some((QuestionPlaceholderKind::Value, Some(1)));
    }
    if sym == "*?" {
        return Some((QuestionPlaceholderKind::Spread, Some(1)));
    }
    if let Some(rest) = sym.strip_prefix('?') {
        return parse_positive_placeholder_index(rest)
            .map(|index| (QuestionPlaceholderKind::Value, Some(index)));
    }
    if let Some(rest) = sym.strip_prefix("*?") {
        return parse_positive_placeholder_index(rest)
            .map(|index| (QuestionPlaceholderKind::Spread, Some(index)));
    }
    None
}

fn form_contains_question_placeholder_scoped(form: &Form, shadowed: &StdHashSet<String>) -> bool {
    match &form.kind {
        FormKind::Symbol(sym) => {
            !shadowed.contains(sym) && parse_question_placeholder(sym).is_some()
        }
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "let" {
                    let mut shadowed_body = shadowed.clone();
                    if let Some(bind_form) = items.get(1) {
                        if let FormKind::Vector(binds) = &bind_form.kind {
                            let mut idx = 0;
                            while idx < binds.len() {
                                let name_form = &binds[idx];
                                if let FormKind::Symbol(name) = &name_form.kind {
                                    shadowed_body.insert(name.clone());
                                }
                                if let Some(val_form) = binds.get(idx + 1) {
                                    if form_contains_question_placeholder_scoped(val_form, shadowed)
                                    {
                                        return true;
                                    }
                                }
                                idx += 2;
                            }
                        } else if form_contains_question_placeholder_scoped(bind_form, shadowed) {
                            return true;
                        }
                    }
                    return items.iter().skip(2).any(|body| {
                        form_contains_question_placeholder_scoped(body, &shadowed_body)
                    });
                }
                if head == "fn" || head == "method" {
                    let mut shadowed_body = shadowed.clone();
                    if let Some(params_form) = items.get(1) {
                        if let FormKind::Vector(params) = &params_form.kind {
                            for param in params {
                                if let FormKind::Symbol(name) = &param.kind {
                                    shadowed_body.insert(name.clone());
                                }
                            }
                        }
                    }
                    return items.iter().skip(2).any(|body| {
                        form_contains_question_placeholder_scoped(body, &shadowed_body)
                    });
                }
            }
            items
                .iter()
                .any(|item| form_contains_question_placeholder_scoped(item, shadowed))
        }
        FormKind::Vector(items) | FormKind::Set(items) => items
            .iter()
            .any(|item| form_contains_question_placeholder_scoped(item, shadowed)),
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => {
                form_contains_question_placeholder_scoped(k, shadowed)
                    || form_contains_question_placeholder_scoped(v, shadowed)
            }
            MapItem::Spread(expr) => form_contains_question_placeholder_scoped(expr, shadowed),
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => {
                form_contains_question_placeholder_scoped(expr, shadowed)
            }
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => {
                form_contains_question_placeholder_scoped(expr, shadowed)
            }
        }),
        FormKind::ShortFn(_) => false,
        _ => false,
    }
}

fn replace_question_placeholders_scoped(
    form: &Form,
    alloc: &mut PlaceholderAlloc,
    shadowed: &StdHashSet<String>,
) -> Form {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if shadowed.contains(sym) {
                form.clone()
            } else if let Some((kind, index)) = parse_question_placeholder(sym) {
                let idx = match index {
                    Some(index) => {
                        alloc.use_index(index);
                        index
                    }
                    None => alloc.alloc_auto(),
                };
                let name = alloc.name_for_index(idx);
                let symbol = match kind {
                    QuestionPlaceholderKind::Value => name,
                    QuestionPlaceholderKind::Spread => format!("*{}", name),
                };
                Form::new(FormKind::Symbol(symbol), form.span)
            } else {
                form.clone()
            }
        }
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "let" {
                    let mut out = Vec::with_capacity(items.len());
                    out.push(items[0].clone());
                    let mut shadowed_body = shadowed.clone();
                    if let Some(bind_form) = items.get(1) {
                        if let FormKind::Vector(binds) = &bind_form.kind {
                            let mut replaced_binds = Vec::with_capacity(binds.len());
                            let mut idx = 0;
                            while idx < binds.len() {
                                let name_form = &binds[idx];
                                replaced_binds.push(name_form.clone());
                                if let FormKind::Symbol(name) = &name_form.kind {
                                    shadowed_body.insert(name.clone());
                                }
                                if let Some(val_form) = binds.get(idx + 1) {
                                    replaced_binds.push(replace_question_placeholders_scoped(
                                        val_form, alloc, shadowed,
                                    ));
                                }
                                idx += 2;
                            }
                            out.push(Form::new(FormKind::Vector(replaced_binds), bind_form.span));
                        } else {
                            out.push(replace_question_placeholders_scoped(
                                bind_form, alloc, shadowed,
                            ));
                        }
                    }
                    for body in items.iter().skip(2) {
                        out.push(replace_question_placeholders_scoped(
                            body,
                            alloc,
                            &shadowed_body,
                        ));
                    }
                    return Form::new(FormKind::List(out), form.span);
                }
                if head == "fn" || head == "method" {
                    let mut out = Vec::with_capacity(items.len());
                    out.push(items[0].clone());
                    let mut shadowed_body = shadowed.clone();
                    if let Some(params_form) = items.get(1) {
                        if let FormKind::Vector(params) = &params_form.kind {
                            for param in params {
                                if let FormKind::Symbol(name) = &param.kind {
                                    shadowed_body.insert(name.clone());
                                }
                            }
                        }
                        out.push(params_form.clone());
                    }
                    for body in items.iter().skip(2) {
                        out.push(replace_question_placeholders_scoped(
                            body,
                            alloc,
                            &shadowed_body,
                        ));
                    }
                    return Form::new(FormKind::List(out), form.span);
                }
            }
            let mapped = items
                .iter()
                .map(|item| replace_question_placeholders_scoped(item, alloc, shadowed))
                .collect();
            Form::new(FormKind::List(mapped), form.span)
        }
        FormKind::Vector(items) => {
            let mapped = items
                .iter()
                .map(|item| replace_question_placeholders_scoped(item, alloc, shadowed))
                .collect();
            Form::new(FormKind::Vector(mapped), form.span)
        }
        FormKind::Map(entries) => {
            let mut out = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => out.push(MapItem::KeyValue(
                        replace_question_placeholders_scoped(k, alloc, shadowed),
                        replace_question_placeholders_scoped(v, alloc, shadowed),
                    )),
                    MapItem::Spread(expr) => out.push(MapItem::Spread(
                        replace_question_placeholders_scoped(expr, alloc, shadowed),
                    )),
                }
            }
            Form::new(FormKind::Map(out), form.span)
        }
        FormKind::Set(items) => {
            let mapped = items
                .iter()
                .map(|item| replace_question_placeholders_scoped(item, alloc, shadowed))
                .collect();
            Form::new(FormKind::Set(mapped), form.span)
        }
        FormKind::InterpolatedString(parts) => {
            let mapped = parts
                .iter()
                .map(|part| match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => InterpolatedPart::Expr(
                        replace_question_placeholders_scoped(expr, alloc, shadowed),
                    ),
                })
                .collect();
            Form::new(FormKind::InterpolatedString(mapped), form.span)
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mapped = parts
                .iter()
                .map(|part| match part {
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                    InterpolatedPart::Expr(expr) => InterpolatedPart::Expr(
                        replace_question_placeholders_scoped(expr, alloc, shadowed),
                    ),
                })
                .collect();
            Form::new(
                FormKind::InterpolatedRegex {
                    parts: mapped,
                    delim: *delim,
                },
                form.span,
            )
        }
        FormKind::ShortFn(_) => form.clone(),
        _ => form.clone(),
    }
}

fn type_includes_function(kind: &MetaTypeKind) -> bool {
    match kind {
        MetaTypeKind::Function { .. } => true,
        MetaTypeKind::Named(name) => name == "Function" || name.ends_with("::Function"),
        MetaTypeKind::Union(types) => types.iter().any(type_includes_function),
        MetaTypeKind::Option(inner) => type_includes_function(inner),
        _ => false,
    }
}

fn fn_arg_positions_from_meta(meta: &FnMeta, arg_len: usize) -> StdHashSet<usize> {
    let mut positions = StdHashSet::new();
    for overload in &meta.overloads {
        for (idx, ty) in overload.arg_types.iter().enumerate() {
            if type_includes_function(ty) {
                positions.insert(idx + 1);
            }
        }
        if let Some(rest) = &overload.rest {
            if type_includes_function(rest) {
                let start = overload.arg_types.len() + 1;
                for idx in start..=arg_len {
                    positions.insert(idx);
                }
            }
        }
    }
    positions
}

fn fn_arg_positions_from_meta_with_core_fallback(
    meta: &FnMeta,
    arg_len: usize,
) -> StdHashSet<usize> {
    let mut positions = fn_arg_positions_from_meta(meta, arg_len);
    if positions.is_empty() && meta.ns == "std" {
        let core_name = format!("core::{}", meta.name);
        if let Some(core_meta) = fn_meta::get(&core_name) {
            positions = fn_arg_positions_from_meta(&core_meta, arg_len);
        }
    }
    positions
}

fn fn_arg_positions_from_type(kind: &MetaTypeKind, arg_len: usize) -> StdHashSet<usize> {
    let mut positions = StdHashSet::new();
    match kind {
        MetaTypeKind::Function { params, rest, .. } => {
            for (idx, ty) in params.iter().enumerate() {
                if type_includes_function(ty) {
                    positions.insert(idx + 1);
                }
            }
            if let Some(rest) = rest {
                if type_includes_function(rest) {
                    let start = params.len() + 1;
                    for idx in start..=arg_len {
                        positions.insert(idx);
                    }
                }
            }
        }
        MetaTypeKind::Union(types) => {
            for inner in types {
                positions.extend(fn_arg_positions_from_type(inner, arg_len));
            }
        }
        MetaTypeKind::Option(inner) => {
            positions.extend(fn_arg_positions_from_type(inner, arg_len));
        }
        _ => {}
    }
    positions
}

fn wrap_question_lambda(form: &Form) -> Option<(Form, usize)> {
    let mut alloc = PlaceholderAlloc::new();
    let replaced = replace_question_placeholders_scoped(form, &mut alloc, &StdHashSet::new());
    if !alloc.found {
        return None;
    }
    debug_assert!(!form_contains_question_placeholder_scoped(
        &replaced,
        &StdHashSet::new()
    ));

    let params = (1..=alloc.max_index)
        .map(|index| Form::new(FormKind::Symbol(alloc.name_for_index(index)), form.span))
        .collect();
    let fn_head = Form::new(FormKind::Symbol("fn".into()), form.span);
    let params_form = Form::new(FormKind::Vector(params), form.span);

    let placeholder_uses = alloc.max_index;
    let lambda_items = vec![fn_head, params_form, replaced];
    Some((
        Form::new(FormKind::List(lambda_items), form.span),
        placeholder_uses,
    ))
}

fn expand_placeholder_partial(form: &Form) -> Option<(Form, usize)> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 2 {
        return None;
    }
    if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
        if parse_question_placeholder(head).is_some() {
            return None;
        }
    }
    if !form_contains_question_placeholder_scoped(form, &StdHashSet::new()) {
        return None;
    }
    wrap_question_lambda(form)
}

#[derive(Clone, Copy)]
struct MethodRewriteCtx {
    in_quote: bool,
}

fn methodify_clauses(clauses: &[FnClauseSpec], force: bool) -> (Vec<FnClauseSpec>, bool) {
    let (rewritten, used) = rewrite_method_clauses(clauses);
    let used = used || force;
    if !used {
        return (clauses.to_vec(), false);
    }
    let mut out = Vec::with_capacity(clauses.len());
    for clause in rewritten {
        let params_form = ensure_self_param(&clause.params_form);
        out.push(FnClauseSpec {
            params_form,
            body: clause.body,
        });
    }
    (out, true)
}

fn rewrite_method_clauses(clauses: &[FnClauseSpec]) -> (Vec<FnClauseSpec>, bool) {
    let mut rewritten = Vec::with_capacity(clauses.len());
    let mut used = false;
    for clause in clauses {
        let (body, body_used) = rewrite_method_body(&clause.body);
        if body_used {
            used = true;
        }
        rewritten.push(FnClauseSpec {
            params_form: clause.params_form.clone(),
            body,
        });
    }
    if used {
        (rewritten, true)
    } else {
        (clauses.to_vec(), false)
    }
}

fn with_method_clauses(signature: DefnSignature, clauses: Vec<FnClauseSpec>) -> DefnSignature {
    let fn_args = build_fn_args_from_clauses(&clauses, signature.is_multi);
    DefnSignature {
        clauses,
        fn_args,
        ..signature
    }
}

fn build_fn_args_from_clauses(clauses: &[FnClauseSpec], is_multi: bool) -> Vec<Form> {
    if !is_multi {
        let clause = clauses.first().expect("defn has at least one clause");
        let mut fn_args = Vec::with_capacity(1 + clause.body.len());
        fn_args.push(clause.params_form.clone());
        fn_args.extend(clause.body.iter().cloned());
        return fn_args;
    }
    clauses
        .iter()
        .map(|clause| {
            let mut items = Vec::with_capacity(1 + clause.body.len());
            items.push(clause.params_form.clone());
            items.extend(clause.body.iter().cloned());
            Form::new(FormKind::List(items), clause.params_form.span)
        })
        .collect()
}

fn ensure_self_param(params_form: &Form) -> Form {
    let FormKind::Vector(items) = &params_form.kind else {
        return params_form.clone();
    };
    if matches!(
        items.first().map(|item| &item.kind),
        Some(FormKind::Symbol(sym)) if sym == "self"
    ) {
        return params_form.clone();
    }
    let mut new_items = Vec::with_capacity(items.len() + 1);
    new_items.push(Form::new(FormKind::Symbol("self".into()), params_form.span));
    new_items.extend(items.iter().cloned());
    Form::new(FormKind::Vector(new_items), params_form.span)
}

fn form_contains_symbol(form: &Form, target: &str) -> bool {
    match &form.kind {
        FormKind::Symbol(sym) => sym == target,
        FormKind::Vector(items) | FormKind::List(items) => {
            items.iter().any(|item| form_contains_symbol(item, target))
        }
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => {
                form_contains_symbol(k, target) || form_contains_symbol(v, target)
            }
            MapItem::Spread(expr) => form_contains_symbol(expr, target),
        }),
        _ => false,
    }
}

fn ensure_method_params_without_self(clauses: &[FnClauseSpec]) -> Result<(), CloveError> {
    for clause in clauses {
        if form_contains_symbol(&clause.params_form, "self") {
            return Err(span_runtime_error(
                clause.params_form.span,
                "self is reserved; do not declare it as a parameter",
            ));
        }
    }
    Ok(())
}

fn rewrite_method_body(body: &[Form]) -> (Vec<Form>, bool) {
    let mut used = false;
    let mut out = Vec::with_capacity(body.len());
    for form in body {
        let (rewritten, form_used) =
            rewrite_method_form(form, MethodRewriteCtx { in_quote: false });
        if form_used {
            used = true;
        }
        out.push(rewritten);
    }
    (out, used)
}

fn rewrite_method_list_items(form: &Form, items: &[Form], ctx: MethodRewriteCtx) -> (Form, bool) {
    let mut used = false;
    let mut new_items = Vec::with_capacity(items.len());
    for item in items {
        let (rewritten, item_used) = rewrite_method_form(item, ctx);
        if item_used {
            used = true;
        }
        new_items.push(rewritten);
    }
    if !used {
        return (form.clone(), false);
    }
    (
        Form {
            kind: FormKind::List(new_items),
            span: form.span,
            type_hint: form.type_hint.clone(),
        },
        true,
    )
}

fn rewrite_fn_clause_form(form: &Form, ctx: MethodRewriteCtx) -> (Form, bool) {
    let FormKind::List(items) = &form.kind else {
        return rewrite_method_form(form, ctx);
    };
    if items.is_empty() {
        return (form.clone(), false);
    }
    if !matches!(items[0].kind, FormKind::Vector(_)) {
        return rewrite_method_list_items(form, items, ctx);
    }
    let mut used = false;
    let mut new_items = Vec::with_capacity(items.len());
    new_items.push(items[0].clone());
    for item in items.iter().skip(1) {
        let (rewritten, item_used) = rewrite_method_form(item, ctx);
        if item_used {
            used = true;
        }
        new_items.push(rewritten);
    }
    if !used {
        return (form.clone(), false);
    }
    (
        Form {
            kind: FormKind::List(new_items),
            span: form.span,
            type_hint: form.type_hint.clone(),
        },
        true,
    )
}

fn rewrite_fn_like_list(form: &Form, items: &[Form], ctx: MethodRewriteCtx) -> (Form, bool) {
    let mut new_items = Vec::with_capacity(items.len());
    new_items.push(items[0].clone());
    let mut idx = 1;
    if items.len() >= 3 {
        if matches!(items[1].kind, FormKind::Symbol(_))
            && matches!(items[2].kind, FormKind::Vector(_) | FormKind::List(_))
        {
            new_items.push(items[1].clone());
            idx = 2;
        }
    }
    if idx >= items.len() {
        return (form.clone(), false);
    }
    match &items[idx].kind {
        FormKind::Vector(_) => {
            new_items.push(items[idx].clone());
            let mut used = false;
            for item in items.iter().skip(idx + 1) {
                let (rewritten, item_used) = rewrite_method_form(item, ctx);
                if item_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::List(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::List(_) => {
            let mut used = false;
            for clause in items.iter().skip(idx) {
                let (rewritten, clause_used) = rewrite_fn_clause_form(clause, ctx);
                if clause_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::List(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        _ => rewrite_method_list_items(form, items, ctx),
    }
}

fn rewrite_defn_like_list(form: &Form, items: &[Form], ctx: MethodRewriteCtx) -> (Form, bool) {
    if items.len() < 2 {
        return (form.clone(), false);
    }
    let mut new_items = Vec::with_capacity(items.len());
    new_items.push(items[0].clone());
    new_items.push(items[1].clone());
    let mut idx = 2;
    let mut doc_seen = false;
    let mut meta_seen = false;
    while let Some(item) = items.get(idx) {
        match &item.kind {
            FormKind::String(_) if !doc_seen => {
                doc_seen = true;
                new_items.push(item.clone());
                idx += 1;
            }
            FormKind::Map(_) if !meta_seen => {
                meta_seen = true;
                new_items.push(item.clone());
                idx += 1;
            }
            _ => break,
        }
    }
    if let Some(item) = items.get(idx) {
        let has_return_hint = match &item.kind {
            FormKind::Keyword(_) => true,
            FormKind::Symbol(sym) => sym.starts_with(':'),
            _ => false,
        };
        if has_return_hint {
            new_items.push(item.clone());
            idx += 1;
        }
    }
    if idx >= items.len() {
        return (form.clone(), false);
    }
    match &items[idx].kind {
        FormKind::Vector(_) => {
            new_items.push(items[idx].clone());
            let mut used = false;
            for item in items.iter().skip(idx + 1) {
                let (rewritten, item_used) = rewrite_method_form(item, ctx);
                if item_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::List(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::List(_) => {
            let mut used = false;
            for clause in items.iter().skip(idx) {
                let (rewritten, clause_used) = rewrite_fn_clause_form(clause, ctx);
                if clause_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::List(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        _ => rewrite_method_list_items(form, items, ctx),
    }
}

fn rewrite_method_form(form: &Form, ctx: MethodRewriteCtx) -> (Form, bool) {
    if ctx.in_quote {
        return (form.clone(), false);
    }
    match &form.kind {
        FormKind::Symbol(sym) if sym == "&" => {
            return (
                Form {
                    kind: FormKind::Symbol("self".into()),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            );
        }
        FormKind::Symbol(sym) if sym == "self" => return (form.clone(), true),
        FormKind::List(items) => {
            if is_quote_form(items) {
                return (form.clone(), false);
            }
            if is_map_ref_form(form) {
                let items = unwrap_apply_map_ref_items(items).unwrap_or(items);
                if let Some(rewritten) = map_ref_to_self_access(form, items) {
                    return (rewritten, true);
                }
                return (form.clone(), true);
            }
            if let Some(FormKind::Symbol(head)) = items.first().map(|item| &item.kind) {
                if head == "fn" || head == "method" {
                    return rewrite_fn_like_list(form, items, ctx);
                }
                if head == "defn" || head == "defn-" || head == "-defn" {
                    return rewrite_defn_like_list(form, items, ctx);
                }
            }
            rewrite_method_list_items(form, items, ctx)
        }
        FormKind::Vector(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let (rewritten, item_used) = rewrite_method_form(item, ctx);
                if item_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::Vector(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::Set(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let (rewritten, item_used) = rewrite_method_form(item, ctx);
                if item_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::Set(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::ShortFn(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let (rewritten, item_used) = rewrite_method_form(item, ctx);
                if item_used {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::ShortFn(new_items),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::InterpolatedString(parts) => {
            let mut used = false;
            let mut new_parts = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        new_parts.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        let (rewritten, expr_used) = rewrite_method_form(expr, ctx);
                        if expr_used {
                            used = true;
                        }
                        new_parts.push(InterpolatedPart::Expr(rewritten));
                    }
                }
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::InterpolatedString(new_parts),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut used = false;
            let mut new_parts = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        new_parts.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        let (rewritten, expr_used) = rewrite_method_form(expr, ctx);
                        if expr_used {
                            used = true;
                        }
                        new_parts.push(InterpolatedPart::Expr(rewritten));
                    }
                }
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::InterpolatedRegex {
                        parts: new_parts,
                        delim: *delim,
                    },
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        FormKind::Map(entries) => {
            let mut used = false;
            let mut new_entries = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let (new_k, key_used) = rewrite_method_form(k, ctx);
                        let (new_v, val_used) = rewrite_method_form(v, ctx);
                        if key_used || val_used {
                            used = true;
                        }
                        new_entries.push(MapItem::KeyValue(new_k, new_v));
                    }
                    MapItem::Spread(expr) => {
                        let (new_expr, expr_used) = rewrite_method_form(expr, ctx);
                        if expr_used {
                            used = true;
                        }
                        new_entries.push(MapItem::Spread(new_expr));
                    }
                }
            }
            if !used {
                return (form.clone(), false);
            }
            (
                Form {
                    kind: FormKind::Map(new_entries),
                    span: form.span,
                    type_hint: form.type_hint.clone(),
                },
                true,
            )
        }
        _ => (form.clone(), false),
    }
}

fn map_ref_to_self_access(form: &Form, items: &[Form]) -> Option<Form> {
    if items.len() < 3 {
        return None;
    }
    match items.get(1).map(|item| &item.kind) {
        Some(FormKind::Keyword(scope)) if scope == "this" || scope == "root" => {}
        _ => return None,
    }
    let mut current = Form::new(FormKind::Symbol("self".into()), form.span);
    for segment in &items[2..] {
        let key_form = map_ref_segment_to_key_form(segment)?;
        let mut parts = Vec::with_capacity(3);
        parts.push(Form::new(
            FormKind::Symbol(OOP_INDEX_SYM.to_string()),
            form.span,
        ));
        parts.push(current);
        parts.push(key_form);
        current = Form::new(FormKind::List(parts), form.span);
    }
    Some(current)
}

fn map_ref_segment_to_key_form(segment: &Form) -> Option<Form> {
    match &segment.kind {
        FormKind::Keyword(_) | FormKind::String(_) | FormKind::Int(_) => Some(segment.clone()),
        FormKind::Symbol(sym) if sym == ".." => None,
        FormKind::Symbol(sym) => Some(Form::new(FormKind::Keyword(sym.clone()), segment.span)),
        FormKind::List(items)
            if items.len() == 2
                && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "quote") =>
        {
            match &items[1].kind {
                FormKind::Symbol(sym) => {
                    Some(Form::new(FormKind::Symbol(sym.clone()), segment.span))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn is_quote_form(items: &[Form]) -> bool {
    matches!(
        items.first().map(|item| &item.kind),
        Some(FormKind::Symbol(sym)) if sym == "quote"
    )
}

fn form_contains_dot_chain(form: &Form) -> bool {
    match &form.kind {
        FormKind::Symbol(sym) => sym.starts_with(DOT_CHAIN_PLACEHOLDER_PREFIX),
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            items.iter().any(form_contains_dot_chain)
        }
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => form_contains_dot_chain(k) || form_contains_dot_chain(v),
            MapItem::Spread(expr) => form_contains_dot_chain(expr),
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => form_contains_dot_chain(expr),
        }),
        FormKind::ShortFn(items) => items.iter().any(form_contains_dot_chain),
        _ => false,
    }
}

fn form_contains_map_ref(form: &Form) -> bool {
    if is_map_ref_form(form) {
        return true;
    }
    match &form.kind {
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            items.iter().any(form_contains_map_ref)
        }
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => form_contains_map_ref(expr),
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => form_contains_map_ref(expr),
        }),
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => form_contains_map_ref(k) || form_contains_map_ref(v),
            MapItem::Spread(expr) => form_contains_map_ref(expr),
        }),
        FormKind::ShortFn(items) => items.iter().any(form_contains_map_ref),
        _ => false,
    }
}

fn try_expand_index_form(form: &Form) -> Result<Option<Form>, CloveError> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return Ok(None),
    };
    if items.is_empty() {
        return Ok(None);
    }
    let head_sym = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return Ok(None),
    };
    let convert = |target: &str| -> Result<Form, CloveError> {
        if items.len() < 3 || items.len() > 4 {
            return Err(span_runtime_error(
                form.span,
                "malformed indexer form generated by reader",
            ));
        }
        let mut new_items = Vec::with_capacity(items.len());
        new_items.push(Form::new(FormKind::Symbol(target.into()), items[0].span));
        for arg in &items[1..] {
            new_items.push(arg.clone());
        }
        Ok(Form::new(FormKind::List(new_items), form.span))
    };
    match head_sym {
        INDEX_GET_SYM => Ok(Some(convert("get")?)),
        INDEX_GET_IN_SYM => Ok(Some(convert("get-in")?)),
        INDEX_GET_MANY_SYM => Ok(Some(convert("get-in-many")?)),
        _ => Ok(None),
    }
}

fn infer_subject_pos_from_names(params: &[String]) -> Option<usize> {
    for (idx, name) in params.iter().enumerate() {
        if matches!(name.as_str(), "coll" | "xs" | "seq") {
            return Some(idx + 1);
        }
    }
    None
}

fn async_scope_strict_enabled() -> bool {
    match std::env::var("ASYNC_SCOPE_STRICT") {
        Ok(val) => {
            let trimmed = val.trim();
            if trimmed.is_empty()
                || trimmed == "0"
                || trimmed.eq_ignore_ascii_case("false")
                || trimmed.eq_ignore_ascii_case("no")
            {
                false
            } else {
                true
            }
        }
        Err(_) => false,
    }
}

fn span_runtime_error(span: Span, msg: impl Into<String>) -> CloveError {
    CloveError::runtime(msg)
        .with_span(span)
        .with_file(current_file_name())
}

fn is_where_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => matches!(
            items.first().map(|f| &f.kind),
            Some(FormKind::Symbol(sym)) if sym == "where"
        ),
        _ => false,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RecurTailContext {
    Tail,
    Value,
}

fn ensure_recur_tail_positions(body: &[Form]) -> Result<(), CloveError> {
    check_recur_body(body, RecurTailContext::Tail)
}

fn check_recur_body(body: &[Form], tail: RecurTailContext) -> Result<(), CloveError> {
    let err_fin = parse_err_fin_tail(body, "a body")
        .map_err(|err| span_runtime_error(err.span, err.message))?;
    if let Some(tail_forms) = err_fin {
        if tail_forms.body.is_empty() {
            let span = tail_forms
                .err
                .as_ref()
                .map(|f| f.span)
                .or_else(|| tail_forms.fin.as_ref().map(|f| f.span))
                .unwrap_or_else(|| body.first().map(|f| f.span).unwrap());
            return Err(span_runtime_error(span, "do expects body before err/fin"));
        }
        check_recur_body(&tail_forms.body, tail)?;
        if let Some(err_form) = tail_forms.err.as_ref() {
            if let Some(err_body) = err_fin_clause_body(err_form) {
                check_recur_body(err_body, RecurTailContext::Value)?;
            }
        }
        if let Some(fin_form) = tail_forms.fin.as_ref() {
            if let Some(fin_body) = err_fin_clause_body(fin_form) {
                check_recur_body(fin_body, RecurTailContext::Value)?;
            }
        }
        return Ok(());
    }
    let mut last_non_where = None;
    for (idx, form) in body.iter().enumerate() {
        if !is_where_form(form) {
            last_non_where = Some(idx);
        }
    }
    let Some(last_idx) = last_non_where else {
        return Ok(());
    };
    for (idx, form) in body.iter().enumerate() {
        let ctx = if idx == last_idx && tail == RecurTailContext::Tail {
            RecurTailContext::Tail
        } else {
            RecurTailContext::Value
        };
        check_recur_form(form, ctx)?;
    }
    Ok(())
}

fn check_recur_form(form: &Form, tail: RecurTailContext) -> Result<(), CloveError> {
    match &form.kind {
        FormKind::List(items) => check_recur_list(form, items, tail),
        FormKind::Vector(items) | FormKind::Set(items) => {
            for item in items {
                check_recur_form(item, RecurTailContext::Value)?;
            }
            Ok(())
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        check_recur_form(k, RecurTailContext::Value)?;
                        check_recur_form(v, RecurTailContext::Value)?;
                    }
                    MapItem::Spread(expr) => {
                        check_recur_form(expr, RecurTailContext::Value)?;
                    }
                }
            }
            Ok(())
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    check_recur_form(expr, RecurTailContext::Value)?;
                }
            }
            Ok(())
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    check_recur_form(expr, RecurTailContext::Value)?;
                }
            }
            Ok(())
        }
        FormKind::ShortFn(_) => Ok(()),
        _ => Ok(()),
    }
}

fn check_recur_list(form: &Form, items: &[Form], tail: RecurTailContext) -> Result<(), CloveError> {
    if items.is_empty() {
        return Ok(());
    }
    if let Some(FormKind::Symbol(sym)) = items.first().map(|f| &f.kind) {
        let head = canonical_symbol_name(sym);
        match head.as_ref() {
            "recur" => {
                if tail == RecurTailContext::Tail {
                    Ok(())
                } else {
                    Err(span_runtime_error(
                        form.span,
                        "recur must appear in tail position",
                    ))
                }
            }
            "do" => check_recur_body(&items[1..], tail),
            "where" => check_recur_body(&items[1..], RecurTailContext::Value),
            "let" => {
                if let Some(bindings_form) = items.get(1) {
                    check_recur_form(bindings_form, RecurTailContext::Value)?;
                }
                check_recur_body(&items[2..], tail)
            }
            "if" | "if-not" => {
                if let Some(test_form) = items.get(1) {
                    check_recur_form(test_form, RecurTailContext::Value)?;
                }
                if let Some(then_form) = items.get(2) {
                    check_recur_form(then_form, tail)?;
                }
                if let Some(else_form) = items.get(3) {
                    check_recur_form(else_form, tail)?;
                }
                Ok(())
            }
            "when" | "when-not" => {
                if let Some(test_form) = items.get(1) {
                    check_recur_form(test_form, RecurTailContext::Value)?;
                }
                check_recur_body(&items[2..], tail)
            }
            "if-let" | "if-some" => {
                if let Some(bind_form) = items.get(1) {
                    check_recur_form(bind_form, RecurTailContext::Value)?;
                }
                if let Some(then_form) = items.get(2) {
                    check_recur_form(then_form, tail)?;
                }
                if let Some(else_form) = items.get(3) {
                    check_recur_form(else_form, tail)?;
                }
                Ok(())
            }
            "when-let" => {
                if let Some(bind_form) = items.get(1) {
                    check_recur_form(bind_form, RecurTailContext::Value)?;
                }
                check_recur_body(&items[2..], tail)
            }
            "cond" => {
                let mut idx = 1;
                while idx + 1 < items.len() {
                    check_recur_form(&items[idx], RecurTailContext::Value)?;
                    check_recur_form(&items[idx + 1], tail)?;
                    idx += 2;
                }
                Ok(())
            }
            "condp" => {
                if let Some(pred_form) = items.get(1) {
                    check_recur_form(pred_form, RecurTailContext::Value)?;
                }
                if let Some(expr_form) = items.get(2) {
                    check_recur_form(expr_form, RecurTailContext::Value)?;
                }
                let mut idx = 3;
                while idx < items.len() {
                    check_recur_form(&items[idx], RecurTailContext::Value)?;
                    if idx + 1 >= items.len() {
                        break;
                    }
                    let next_form = &items[idx + 1];
                    if matches!(next_form.kind, FormKind::Keyword(ref kw) if kw == ">>") {
                        if let Some(transform_form) = items.get(idx + 2) {
                            check_recur_form(transform_form, tail)?;
                        }
                        idx += 3;
                    } else {
                        check_recur_form(next_form, tail)?;
                        idx += 2;
                    }
                }
                Ok(())
            }
            "match" => {
                if let Some(target_form) = items.get(1) {
                    check_recur_form(target_form, RecurTailContext::Value)?;
                }
                let mut idx = 2;
                while idx < items.len() {
                    let pattern_form = &items[idx];
                    check_recur_form(pattern_form, RecurTailContext::Value)?;
                    idx += 1;
                    if idx >= items.len() {
                        break;
                    }
                    while idx + 1 < items.len() {
                        match &items[idx].kind {
                            FormKind::Keyword(kw) if kw == "when" || kw == "if" => {
                                check_recur_form(&items[idx + 1], RecurTailContext::Value)?;
                                idx += 2;
                                continue;
                            }
                            FormKind::Keyword(kw) if kw == "as" => {
                                idx += 2;
                                continue;
                            }
                            _ => {}
                        }
                        break;
                    }
                    if idx >= items.len() {
                        break;
                    }
                    check_recur_form(&items[idx], tail)?;
                    idx += 1;
                }
                Ok(())
            }
            "and" | "or" => {
                if items.len() <= 1 {
                    return Ok(());
                }
                let last_idx = items.len() - 1;
                for (idx, item) in items.iter().enumerate().skip(1) {
                    let ctx = if idx == last_idx && tail == RecurTailContext::Tail {
                        RecurTailContext::Tail
                    } else {
                        RecurTailContext::Value
                    };
                    check_recur_form(item, ctx)?;
                }
                Ok(())
            }
            "try" => check_recur_try(&items[1..], tail),
            "loop" | "go-loop" | "scope-loop" | "async::scope-loop" => {
                if let Some(bindings_form) = items.get(1) {
                    check_recur_form(bindings_form, RecurTailContext::Value)?;
                }
                check_recur_body(&items[2..], RecurTailContext::Tail)
            }
            "fn" | "defn" | "defn-" | "-defn" | "method" | "quote" | "comment" => Ok(()),
            _ => {
                for item in items.iter().skip(1) {
                    check_recur_form(item, RecurTailContext::Value)?;
                }
                Ok(())
            }
        }
    } else {
        for item in items {
            check_recur_form(item, RecurTailContext::Value)?;
        }
        Ok(())
    }
}

fn err_fin_clause_body(form: &Form) -> Option<&[Form]> {
    match &form.kind {
        FormKind::List(items) if items.len() >= 1 => Some(&items[1..]),
        _ => None,
    }
}

fn check_recur_try(args: &[Form], tail: RecurTailContext) -> Result<(), CloveError> {
    if args.is_empty() {
        return Ok(());
    }
    let has_catch_finally = args.iter().any(|form| match &form.kind {
        FormKind::List(items) => match items.first().map(|f| &f.kind) {
            Some(FormKind::Symbol(tag)) => tag == "catch" || tag == "finally",
            _ => false,
        },
        _ => false,
    });
    let mut rest_args = args;
    if !has_catch_finally && matches!(args[0].kind, FormKind::Vector(_)) {
        check_recur_form(&args[0], RecurTailContext::Value)?;
        rest_args = &args[1..];
    }
    if let Ok(Some(tail_forms)) = parse_err_fin_tail(rest_args, "try body") {
        check_recur_body(&tail_forms.body, tail)?;
        if let Some(err_form) = tail_forms.err.as_ref() {
            if let Some(err_body) = err_fin_clause_body(err_form) {
                check_recur_body(err_body, RecurTailContext::Value)?;
            }
        }
        if let Some(fin_form) = tail_forms.fin.as_ref() {
            if let Some(fin_body) = err_fin_clause_body(fin_form) {
                check_recur_body(fin_body, RecurTailContext::Value)?;
            }
        }
        return Ok(());
    }
    if has_catch_finally {
        let mut body_forms = Vec::new();
        let mut catch_forms: Vec<Vec<Form>> = Vec::new();
        let mut finally_clause: Option<Vec<Form>> = None;
        let mut saw_handler = false;
        for form in args {
            if let FormKind::List(items) = &form.kind {
                if let Some(FormKind::Symbol(tag)) = items.first().map(|f| &f.kind) {
                    match tag.as_str() {
                        "catch" => {
                            if items.len() >= 3 {
                                let body_idx = if items.len() >= 4 { 3 } else { 2 };
                                catch_forms.push(items[body_idx..].to_vec());
                                saw_handler = true;
                                continue;
                            }
                        }
                        "finally" => {
                            if items.len() >= 2 {
                                finally_clause = Some(items[1..].to_vec());
                                saw_handler = true;
                                continue;
                            }
                        }
                        _ => {}
                    }
                }
            }
            if saw_handler {
                continue;
            }
            body_forms.push(form.clone());
        }
        check_recur_body(&body_forms, tail)?;
        for catch_body in catch_forms {
            check_recur_body(&catch_body, RecurTailContext::Value)?;
        }
        if let Some(finally_body) = finally_clause {
            check_recur_body(&finally_body, RecurTailContext::Value)?;
        }
        return Ok(());
    }
    if let Ok(plan) = parse_try_short(rest_args) {
        match plan {
            TryShortPlan::Handler { body, on_error } => {
                check_recur_body(&body, tail)?;
                check_recur_form(&on_error, RecurTailContext::Value)?;
            }
            TryShortPlan::Finally { body, on_finally } => {
                check_recur_body(&body, tail)?;
                check_recur_form(&on_finally, RecurTailContext::Value)?;
            }
            TryShortPlan::HandlerFinally {
                body,
                on_error,
                on_finally,
            } => {
                check_recur_body(&body, tail)?;
                check_recur_form(&on_error, RecurTailContext::Value)?;
                check_recur_form(&on_finally, RecurTailContext::Value)?;
            }
        }
        return Ok(());
    }
    check_recur_body(rest_args, tail)
}

fn is_deftype_method_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => matches!(
            items.first().map(|f| &f.kind),
            Some(FormKind::Symbol(sym)) if sym == "defn" || sym == "method"
        ),
        _ => false,
    }
}

fn split_deftype_forms(forms: &[Form]) -> (Vec<Form>, Vec<Form>, Vec<Form>) {
    let mut fields = Vec::new();
    let mut wheres = Vec::new();
    let mut methods = Vec::new();
    for form in forms {
        if is_where_form(form) {
            wheres.push(form.clone());
        } else if is_deftype_method_form(form) {
            methods.push(form.clone());
        } else {
            fields.push(form.clone());
        }
    }
    (fields, wheres, methods)
}

fn range_value_to_optional_int(value: Value, span: Span) -> Result<Option<i64>, CloveError> {
    match value {
        Value::Nil => Ok(None),
        Value::Int(n) => Ok(Some(n)),
        Value::Float(f) if f.fract() == 0.0 => Ok(Some(f as i64)),
        _ => Err(span_runtime_error(
            span,
            "range bounds must be integers or nil",
        )),
    }
}

fn negate_range_value(value: Value, span: Span) -> Result<Value, CloveError> {
    match value {
        Value::Int(n) => Ok(Value::Int(-n)),
        Value::Float(f) => Ok(Value::Float(-f)),
        _ => Err(span_runtime_error(
            span,
            "range bounds must be integers or nil",
        )),
    }
}

fn build_range_map(start: Option<i64>, end: Option<i64>, exclusive: bool) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    map.insert(Key::Keyword("__range".into()), Value::Bool(true));
    map.insert(
        Key::Keyword("start".into()),
        match start {
            Some(n) => Value::Int(n),
            None => Value::Nil,
        },
    );
    map.insert(
        Key::Keyword("end".into()),
        match end {
            Some(n) => Value::Int(n),
            None => Value::Nil,
        },
    );
    map.insert(Key::Keyword("exclusive".into()), Value::Bool(exclusive));
    map
}

fn format_span(span: Span, msg: &str) -> String {
    let file = current_file_name().unwrap_or_else(|| "unknown".into());
    format!("{}:{}:{}: {}", file, span.line, span.col, msg)
}

fn value_to_form(v: &Value) -> Result<Form, CloveError> {
    let span = Span {
        line: 0,
        col: 0,
        index: 0,
    };
    let kind = match v {
        Value::Int(n) => FormKind::Int(*n),
        Value::Float(n) => FormKind::Float(*n),
        Value::String(s) => FormKind::String(s.clone()),
        Value::Bool(b) => FormKind::Bool(*b),
        Value::Nil => FormKind::Nil,
        Value::Duration(d) => FormKind::Duration(*d),
        Value::Symbol(s) => FormKind::Symbol(s.clone()),
        Value::List(items) => FormKind::List(
            items
                .iter()
                .map(|i| value_to_form(i))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Value::Vector(items) => FormKind::Vector(
            items
                .iter()
                .map(|i| value_to_form(i))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Value::Set(items) => FormKind::Set(
            items
                .iter()
                .map(|i| value_to_form(i))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Value::Map(map) => {
            let mut entries = Vec::new();
            for (k, v) in map {
                let key_form = match k {
                    Key::Keyword(s) => Form::new(FormKind::Keyword(s.clone()), span),
                    Key::Symbol(s) => Form::new(FormKind::Symbol(s.clone()), span),
                    Key::String(s) => Form::new(FormKind::String(s.clone()), span),
                    Key::Number(n) => Form::new(FormKind::Int(*n), span),
                    Key::Bool(b) => Form::new(FormKind::Bool(*b), span),
                };
                entries.push(MapItem::KeyValue(key_form, value_to_form(v)?));
            }
            FormKind::Map(entries)
        }
        Value::Seq(_) => {
            return Err(CloveError::runtime(
                "cannot eval sequence pipeline directly; provide quoted form",
            ))
        }
        _ => {
            return Err(CloveError::runtime(
                "eval expects code data (list/vector/map/set/symbol)",
            ))
        }
    };
    Ok(Form::new(kind, span))
}

fn build_short_fn_parts(body: &[Form]) -> (PlaceholderNames, Vec<Form>) {
    let span = body.first().map(|f| f.span).unwrap_or(Span {
        line: 0,
        col: 0,
        index: 0,
    });
    let body_form = Form::new(FormKind::List(body.to_vec()), span);
    let info = placeholder_info_for_form(&body_form);
    let mapping = PlaceholderNames::new(info.max_index, info.has_rest);
    let replaced = replace_placeholders(&body_form, &mapping);
    match replaced.kind {
        FormKind::List(items) => (mapping, items),
        _ => (mapping, vec![replaced]),
    }
}

fn build_short_fn_body(body: &[Form], span: Span) -> (PlaceholderNames, Form) {
    let (mapping, replaced_body) = build_short_fn_parts(body);
    let body_form = Form::new(FormKind::List(replaced_body), span);
    (mapping, body_form)
}

pub fn expand_short_fn_to_list(body: &[Form], span: Span) -> Form {
    let (mapping, body_form) = build_short_fn_body(body, span);
    let mut params = Vec::new();
    for arg in &mapping.args {
        params.push(Form::new(FormKind::Symbol(arg.clone()), span));
    }
    if let Some(rest) = &mapping.rest {
        params.push(Form::new(FormKind::Symbol("&".into()), span));
        params.push(Form::new(FormKind::Symbol(rest.clone()), span));
    }
    let fn_head = Form::new(FormKind::Symbol("fn".into()), span);
    let fn_params = Form::new(FormKind::Vector(params), span);
    let mut items = Vec::with_capacity(3);
    items.push(fn_head);
    items.push(fn_params);
    items.push(body_form);
    Form::new(FormKind::List(items), span)
}

#[derive(Clone)]
struct MapRefFrame {
    runtime: Rc<RefCell<MapRefRuntime>>,
    this_id: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MapRefScope {
    This,
    Root,
}

#[derive(Clone)]
struct MapRef {
    scope: MapRefScope,
    segments: Vec<MapRefSegment>,
    span: Span,
}

#[derive(Clone)]
enum MapRefSegment {
    Parent,
    Keyword(String),
    String(String),
    Number(i64),
    Ident(String),
    Symbol(String),
}

#[derive(Clone)]
enum CalcEntry {
    KeyValue {
        key: Key,
        value: CalcValue,
        span: Span,
    },
    Spread {
        form: Form,
        span: Span,
    },
}

#[derive(Clone)]
enum CalcValue {
    Expr(Form),
    Map(usize),
}

#[derive(Clone)]
struct CalcMapNode {
    path: Vec<Key>,
    entries: Vec<CalcEntry>,
    entries_by_key: StdHashMap<Key, usize>,
}

struct MapRefRuntime {
    nodes: Vec<CalcMapNode>,
    nodes_by_path: StdHashMap<Vec<Key>, usize>,
    root_id: usize,
    root_runtime: Option<Rc<RefCell<MapRefRuntime>>>,
    cache: StdHashMap<Vec<Key>, Value>,
    in_progress: Vec<Vec<Key>>,
}

struct MapRefBuilder {
    nodes: Vec<CalcMapNode>,
}

impl MapRefBuilder {
    fn build_node(&mut self, form: &Form, path: Vec<Key>) -> Result<usize, CloveError> {
        let entries = match &form.kind {
            FormKind::Map(entries) => entries,
            _ => {
                return Err(span_runtime_error(
                    form.span,
                    "map ref runtime expects a map literal",
                ))
            }
        };
        let id = self.nodes.len();
        self.nodes.push(CalcMapNode {
            path: path.clone(),
            entries: Vec::new(),
            entries_by_key: StdHashMap::new(),
        });
        for entry in entries {
            match entry {
                MapItem::KeyValue(k_form, v_form) => {
                    let key = to_key(k_form)?;
                    let value = match &v_form.kind {
                        FormKind::Map(_) => {
                            let mut child_path = path.clone();
                            child_path.push(key.clone());
                            let child_id = self.build_node(v_form, child_path)?;
                            CalcValue::Map(child_id)
                        }
                        _ => CalcValue::Expr(v_form.clone()),
                    };
                    let entry_idx = self.nodes[id].entries.len();
                    self.nodes[id].entries.push(CalcEntry::KeyValue {
                        key: key.clone(),
                        value,
                        span: v_form.span,
                    });
                    self.nodes[id].entries_by_key.insert(key, entry_idx);
                }
                MapItem::Spread(expr) => {
                    self.nodes[id].entries.push(CalcEntry::Spread {
                        form: expr.clone(),
                        span: expr.span,
                    });
                }
            }
        }
        Ok(id)
    }
}

impl MapRefRuntime {
    fn root_runtime(runtime: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        let mut current = runtime;
        loop {
            let next = current.borrow().root_runtime.clone();
            match next {
                Some(next) => current = next,
                None => return current,
            }
        }
    }

    fn from_form(
        form: &Form,
        root_runtime: Option<Rc<RefCell<MapRefRuntime>>>,
    ) -> Result<Self, CloveError> {
        let mut builder = MapRefBuilder { nodes: Vec::new() };
        let root_id = builder.build_node(form, Vec::new())?;
        let mut nodes_by_path = StdHashMap::new();
        for (idx, node) in builder.nodes.iter().enumerate() {
            nodes_by_path.insert(node.path.clone(), idx);
        }
        Ok(Self {
            nodes: builder.nodes,
            nodes_by_path,
            root_id,
            root_runtime,
            cache: StdHashMap::new(),
            in_progress: Vec::new(),
        })
    }

    fn eval_root(
        runtime: Rc<RefCell<Self>>,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let root_id = runtime.borrow().root_id;
        Self::eval_node(runtime, root_id, evaluator, env)
    }

    fn eval_node(
        runtime: Rc<RefCell<Self>>,
        node_id: usize,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let (entries, entries_by_key, base_path) = {
            let runtime_ref = runtime.borrow();
            let node = runtime_ref.nodes.get(node_id).ok_or_else(|| {
                span_runtime_error(
                    Span {
                        line: 0,
                        col: 0,
                        index: 0,
                    },
                    "invalid map ref node",
                )
            })?;
            (
                node.entries.clone(),
                node.entries_by_key.clone(),
                node.path.clone(),
            )
        };
        let mut map = HashMap::new();
        for (idx, entry) in entries.iter().enumerate() {
            match entry {
                CalcEntry::KeyValue { key, value, span } => {
                    let is_last = entries_by_key.get(key).copied() == Some(idx);
                    let value = if is_last {
                        let mut path = base_path.clone();
                        path.push(key.clone());
                        let cached = runtime.borrow().cache.get(&path).cloned();
                        if let Some(cached) = cached {
                            cached
                        } else {
                            Self::eval_path(runtime.clone(), path, *span, evaluator, env.clone())?
                        }
                    } else {
                        Self::eval_entry_value(
                            runtime.clone(),
                            node_id,
                            value.clone(),
                            evaluator,
                            env.clone(),
                        )?
                    };
                    map.insert(key.clone(), value);
                }
                CalcEntry::Spread { form, span } => {
                    let value = with_map_ref_frame(runtime.clone(), node_id, || {
                        evaluator.eval(form, env.clone())
                    })?;
                    spread_into_map(value, &mut map, *span)?;
                }
            }
        }
        Ok(Value::Map(map))
    }

    fn eval_node_snapshot(
        runtime: Rc<RefCell<Self>>,
        node_id: usize,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        let (entries, entries_by_key, base_path, in_progress) = {
            let runtime_ref = runtime.borrow();
            let node = runtime_ref.nodes.get(node_id).ok_or_else(|| {
                span_runtime_error(
                    Span {
                        line: 0,
                        col: 0,
                        index: 0,
                    },
                    "invalid map ref node",
                )
            })?;
            (
                node.entries.clone(),
                node.entries_by_key.clone(),
                node.path.clone(),
                runtime_ref.in_progress.clone(),
            )
        };
        let mut map = HashMap::new();
        for (idx, entry) in entries.iter().enumerate() {
            match entry {
                CalcEntry::KeyValue { key, value, span } => {
                    let mut path = base_path.clone();
                    path.push(key.clone());
                    let blocked = in_progress.iter().any(|p| p.starts_with(&path));
                    if blocked {
                        let cached = runtime.borrow().cache.get(&path).cloned();
                        if let Some(cached) = cached {
                            map.insert(key.clone(), cached);
                        }
                        continue;
                    }
                    let is_last = entries_by_key.get(key).copied() == Some(idx);
                    let value = if is_last {
                        let cached = runtime.borrow().cache.get(&path).cloned();
                        if let Some(cached) = cached {
                            cached
                        } else {
                            Self::eval_path(runtime.clone(), path, *span, evaluator, env.clone())?
                        }
                    } else {
                        Self::eval_entry_value(
                            runtime.clone(),
                            node_id,
                            value.clone(),
                            evaluator,
                            env.clone(),
                        )?
                    };
                    map.insert(key.clone(), value);
                }
                CalcEntry::Spread { form, span } => {
                    let value = with_map_ref_frame(runtime.clone(), node_id, || {
                        evaluator.eval(form, env.clone())
                    })?;
                    spread_into_map(value, &mut map, *span)?;
                }
            }
        }
        Ok(Value::Map(map))
    }

    fn eval_entry_value(
        runtime: Rc<RefCell<Self>>,
        this_id: usize,
        value: CalcValue,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        match value {
            CalcValue::Expr(form) => {
                with_map_ref_frame(runtime, this_id, || evaluator.eval(&form, env))
            }
            CalcValue::Map(child_id) => Self::eval_node(runtime, child_id, evaluator, env),
        }
    }

    fn eval_path(
        runtime: Rc<RefCell<Self>>,
        path: Vec<Key>,
        span: Span,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        if let Some(cached) = runtime.borrow().cache.get(&path).cloned() {
            return Ok(cached);
        }
        if let Some(cycle) = runtime.borrow().cycle_for_path(&path) {
            let formatted = cycle
                .iter()
                .map(|p| format_map_ref_path(p))
                .collect::<Vec<_>>()
                .join(" -> ");
            return Err(span_runtime_error(
                span,
                &format!("map ref cycle detected: {}", formatted),
            ));
        }
        runtime.borrow_mut().in_progress.push(path.clone());
        let _guard = MapRefInProgressGuard {
            runtime: runtime.clone(),
            path: path.clone(),
        };
        let (this_id, value, _entry_span) = runtime.borrow().find_entry(&path, span)?;
        let value =
            Self::eval_entry_value(runtime.clone(), this_id, value, evaluator, env.clone())?;
        runtime
            .borrow_mut()
            .cache
            .insert(path.clone(), value.clone());
        Ok(value)
    }

    fn resolve_map_ref_value(
        runtime: Rc<RefCell<Self>>,
        map_ref: &MapRef,
        this_id: usize,
        evaluator: &Evaluator,
        env: EnvRef,
    ) -> Result<Value, CloveError> {
        if map_ref.scope == MapRefScope::Root {
            if let Some(root_runtime) = runtime.borrow().root_runtime.clone() {
                let root_id = root_runtime.borrow().root_id;
                return Self::resolve_map_ref_value(root_runtime, map_ref, root_id, evaluator, env);
            }
        }
        let target_id = match map_ref.scope {
            MapRefScope::Root => runtime.borrow().root_id,
            MapRefScope::This => this_id,
        };
        if map_ref.segments.is_empty() {
            return Self::eval_node_snapshot(runtime, target_id, evaluator, env);
        }
        let path = runtime.borrow().resolve_map_ref_path(map_ref, target_id)?;
        Self::eval_path(runtime, path, map_ref.span, evaluator, env)
    }

    fn resolve_map_ref_path(
        &self,
        map_ref: &MapRef,
        start_id: usize,
    ) -> Result<Vec<Key>, CloveError> {
        let mut current_id = start_id;
        let mut path = self
            .nodes
            .get(current_id)
            .map(|node| node.path.clone())
            .ok_or_else(|| span_runtime_error(map_ref.span, "invalid map ref scope"))?;
        let mut iter = map_ref.segments.iter().peekable();
        while let Some(seg) = iter.next() {
            match seg {
                MapRefSegment::Parent => {
                    if path.is_empty() {
                        return Err(span_runtime_error(
                            map_ref.span,
                            "map ref parent goes above root",
                        ));
                    }
                    path.pop();
                    current_id = *self.nodes_by_path.get(&path).ok_or_else(|| {
                        span_runtime_error(map_ref.span, "map ref parent is not a map")
                    })?;
                }
                _ => {
                    let key = self.resolve_segment_key(current_id, seg, map_ref.span, &path)?;
                    path.push(key.clone());
                    if let Some(next) = iter.peek() {
                        if !matches!(next, MapRefSegment::Parent) {
                            current_id =
                                self.child_node_id(current_id, &key, map_ref.span, &path)?;
                        }
                    }
                }
            }
        }
        Ok(path)
    }

    fn resolve_segment_key(
        &self,
        node_id: usize,
        seg: &MapRefSegment,
        span: Span,
        path: &[Key],
    ) -> Result<Key, CloveError> {
        let node = self
            .nodes
            .get(node_id)
            .ok_or_else(|| span_runtime_error(span, "invalid map ref scope"))?;
        match seg {
            MapRefSegment::Parent => Err(span_runtime_error(
                span,
                "map ref parent must precede keyword segments",
            )),
            MapRefSegment::Keyword(name) => {
                let key = Key::Keyword(name.clone());
                if node.entries_by_key.contains_key(&key) {
                    Ok(key)
                } else {
                    Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment :{} not found in {}",
                            name,
                            format_map_ref_path(path)
                        ),
                    ))
                }
            }
            MapRefSegment::String(name) => {
                let key = Key::String(name.clone());
                if node.entries_by_key.contains_key(&key) {
                    Ok(key)
                } else {
                    Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment \"{}\" not found in {}",
                            name,
                            format_map_ref_path(path)
                        ),
                    ))
                }
            }
            MapRefSegment::Number(num) => {
                let key = Key::Number(*num);
                if node.entries_by_key.contains_key(&key) {
                    Ok(key)
                } else {
                    Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment {} not found in {}",
                            num,
                            format_map_ref_path(path)
                        ),
                    ))
                }
            }
            MapRefSegment::Symbol(name) => {
                let key = Key::Symbol(name.clone());
                if node.entries_by_key.contains_key(&key) {
                    Ok(key)
                } else {
                    Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment '{}' not found in {}",
                            name,
                            format_map_ref_path(path)
                        ),
                    ))
                }
            }
            MapRefSegment::Ident(name) => {
                let mut matches = Vec::new();
                let kw = Key::Keyword(name.clone());
                if node.entries_by_key.contains_key(&kw) {
                    matches.push(kw);
                }
                let s = Key::String(name.clone());
                if node.entries_by_key.contains_key(&s) {
                    matches.push(s);
                }
                let sym = Key::Symbol(name.clone());
                if node.entries_by_key.contains_key(&sym) {
                    matches.push(sym);
                }
                match matches.len() {
                    0 => Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment '{}' not found in {}",
                            name,
                            format_map_ref_path(path)
                        ),
                    )),
                    1 => Ok(matches.remove(0)),
                    _ => Err(span_runtime_error(
                        span,
                        &format!(
                            "map ref segment '{}' is ambiguous in {} (candidates: {})",
                            name,
                            format_map_ref_path(path),
                            matches
                                .iter()
                                .map(format_map_ref_key)
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    )),
                }
            }
        }
    }

    fn child_node_id(
        &self,
        node_id: usize,
        key: &Key,
        span: Span,
        path: &[Key],
    ) -> Result<usize, CloveError> {
        let node = self
            .nodes
            .get(node_id)
            .ok_or_else(|| span_runtime_error(span, "invalid map ref scope"))?;
        let idx = node.entries_by_key.get(key).copied().ok_or_else(|| {
            span_runtime_error(
                span,
                &format!("map ref path not found: {}", format_map_ref_path(path)),
            )
        })?;
        match &node.entries[idx] {
            CalcEntry::KeyValue { value, .. } => match value {
                CalcValue::Map(child_id) => Ok(*child_id),
                _ => Err(span_runtime_error(
                    span,
                    &format!("map ref path expects map at {}", format_map_ref_path(path)),
                )),
            },
            CalcEntry::Spread { .. } => Err(span_runtime_error(
                span,
                &format!("map ref path expects map at {}", format_map_ref_path(path)),
            )),
        }
    }

    fn find_entry(&self, path: &[Key], span: Span) -> Result<(usize, CalcValue, Span), CloveError> {
        if path.is_empty() {
            return Err(span_runtime_error(span, "map ref path cannot be empty"));
        }
        let mut current_id = self.root_id;
        for (idx, key) in path.iter().enumerate() {
            let node = self
                .nodes
                .get(current_id)
                .ok_or_else(|| span_runtime_error(span, "invalid map ref scope"))?;
            let entry_idx = node.entries_by_key.get(key).copied().ok_or_else(|| {
                span_runtime_error(
                    span,
                    &format!("map ref path not found: {}", format_map_ref_path(path)),
                )
            })?;
            match &node.entries[entry_idx] {
                CalcEntry::KeyValue {
                    value,
                    span: entry_span,
                    ..
                } => {
                    if idx + 1 == path.len() {
                        return Ok((current_id, value.clone(), *entry_span));
                    }
                    match value {
                        CalcValue::Map(child_id) => {
                            current_id = *child_id;
                        }
                        _ => {
                            return Err(span_runtime_error(
                                span,
                                &format!(
                                    "map ref path expects map at {}",
                                    format_map_ref_path(&path[..=idx])
                                ),
                            ))
                        }
                    }
                }
                CalcEntry::Spread { .. } => {
                    return Err(span_runtime_error(
                        span,
                        &format!("map ref path expects map at {}", format_map_ref_path(path)),
                    ))
                }
            }
        }
        Err(span_runtime_error(
            span,
            &format!("map ref path not found: {}", format_map_ref_path(path)),
        ))
    }

    fn cycle_for_path(&self, path: &[Key]) -> Option<Vec<Vec<Key>>> {
        let pos = self.in_progress.iter().position(|p| p == path)?;
        let mut cycle = self.in_progress[pos..].to_vec();
        cycle.push(path.to_vec());
        Some(cycle)
    }
}

struct MapRefInProgressGuard {
    runtime: Rc<RefCell<MapRefRuntime>>,
    path: Vec<Key>,
}

impl Drop for MapRefInProgressGuard {
    fn drop(&mut self) {
        let mut runtime = self.runtime.borrow_mut();
        if let Some(last) = runtime.in_progress.last() {
            if last == &self.path {
                runtime.in_progress.pop();
                return;
            }
        }
        if let Some(pos) = runtime.in_progress.iter().position(|p| p == &self.path) {
            runtime.in_progress.remove(pos);
        }
    }
}

struct MapRefGuard;

impl Drop for MapRefGuard {
    fn drop(&mut self) {
        MAP_REF_STACK.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

fn with_map_ref_frame<T>(
    runtime: Rc<RefCell<MapRefRuntime>>,
    this_id: usize,
    f: impl FnOnce() -> Result<T, CloveError>,
) -> Result<T, CloveError> {
    MAP_REF_STACK.with(|stack| {
        stack.borrow_mut().push(MapRefFrame { runtime, this_id });
    });
    let _guard = MapRefGuard;
    f()
}

fn current_map_ref_frame() -> Option<MapRefFrame> {
    MAP_REF_STACK.with(|stack| stack.borrow().last().cloned())
}

fn parse_map_ref_form(form: &Form) -> Result<Option<MapRef>, CloveError> {
    let FormKind::List(items) = &form.kind else {
        return Ok(None);
    };
    let items = unwrap_apply_map_ref_items(items).unwrap_or(items);
    if !is_map_ref_items(items) {
        return Ok(None);
    }
    if items.len() < 2 {
        return Err(span_runtime_error(form.span, "map ref expects scope"));
    }
    let scope = match &items[1].kind {
        FormKind::Keyword(sym) if sym == "this" => MapRefScope::This,
        FormKind::Keyword(sym) if sym == "root" => MapRefScope::Root,
        _ => {
            return Err(span_runtime_error(
                items[1].span,
                "map ref scope must be :this or :root",
            ))
        }
    };
    let mut segments = Vec::new();
    for seg_form in items.iter().skip(2) {
        segments.push(parse_map_ref_segment(seg_form)?);
    }
    Ok(Some(MapRef {
        scope,
        segments,
        span: form.span,
    }))
}

fn parse_map_ref_segment(form: &Form) -> Result<MapRefSegment, CloveError> {
    match &form.kind {
        FormKind::Keyword(sym) => Ok(MapRefSegment::Keyword(sym.clone())),
        FormKind::Symbol(sym) if sym == ".." => Ok(MapRefSegment::Parent),
        FormKind::List(items)
            if items.len() == 4
                && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "__range_literal")
                && matches!(&items[1].kind, FormKind::Nil)
                && matches!(&items[2].kind, FormKind::Nil)
                && matches!(&items[3].kind, FormKind::Bool(false)) =>
        {
            Ok(MapRefSegment::Parent)
        }
        _ => Err(span_runtime_error(
            form.span,
            "map ref segment must be a keyword (use &^:k or &:k, parent: &^../:k)",
        )),
    }
}

fn is_map_ref_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => {
            is_map_ref_items(items)
                || unwrap_apply_map_ref_items(items)
                    .map(is_map_ref_items)
                    .unwrap_or(false)
        }
        _ => false,
    }
}

fn is_map_ref_items(items: &[Form]) -> bool {
    matches!(
        items.first().map(|item| &item.kind),
        Some(FormKind::Symbol(sym)) if sym == MAP_REF_SYM
    )
}

fn unwrap_apply_map_ref_items(items: &[Form]) -> Option<&[Form]> {
    if items.len() < 2 {
        return None;
    }
    if matches!(&items[0].kind, FormKind::Symbol(sym) if sym == APPLY_SYM)
        && matches!(&items[1].kind, FormKind::Symbol(sym) if sym == MAP_REF_SYM)
    {
        return Some(&items[1..]);
    }
    None
}

fn format_map_ref_path(path: &[Key]) -> String {
    let inner = path
        .iter()
        .map(format_map_ref_key)
        .collect::<Vec<_>>()
        .join(" ");
    format!("[{}]", inner)
}

fn format_map_ref_key(key: &Key) -> String {
    match key {
        Key::Keyword(name) => format!(":{}", name),
        Key::String(name) => format!("\"{}\"", name),
        Key::Symbol(name) => name.clone(),
        Key::Number(num) => num.to_string(),
        Key::Bool(val) => val.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;
    use crate::fn_meta;
    use crate::fn_meta::SubjectPos;
    use crate::namespaces::NamespaceStore;
    use crate::options::EvalOptions;
    use crate::reader::ReaderOptions;
    use crate::runtime::eval_source_with_engines;
    use crate::types::TypeKind as MetaTypeKind;
    use std::sync::{Arc, RwLock};
    use std::thread;

    use crate::reader::Reader;

    fn eval_src(src: &str) -> Value {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        let forms = normalize_type_syntax_forms(forms, false).unwrap();
        let mut eval = Evaluator::new(Env::default());
        eval.eval_forms(&forms).unwrap()
    }

    fn eval_err(src: &str) -> CloveError {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        let forms = normalize_type_syntax_forms(forms, false).unwrap();
        let mut eval = Evaluator::new(Env::default());
        eval.eval_forms(&forms)
            .expect_err("expected evaluation error")
    }

    fn eval_nav_src(src: &str) -> Value {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        let forms = normalize_type_syntax_forms(forms, false).unwrap();
        let mut eval = Evaluator::new(Env::default());
        let store = Arc::new(RwLock::new(NamespaceStore::new(eval.global_env())));
        {
            let mut guard = store.write().unwrap();
            let ns = guard.ensure("user");
            ns.public_exports.insert("disj".to_string(), Value::Nil);
        }
        eval.set_namespace_store(store);
        eval.eval_forms(&forms).unwrap()
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

    #[test]
    fn defn_defines_function() {
        let v = eval_src("(do (defn add1 [x] (+ x 1)) (add1 2))");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn nav_returns_var_hits() {
        let v = eval_nav_src("(nav 'disj :var)");
        let map = match v {
            Value::Map(map) => map,
            other => panic!("expected nav map, got {:?}", other),
        };
        assert_eq!(
            map.get(&Key::Keyword("kind".into())),
            Some(&Value::Symbol(":nav".into()))
        );
        let vars = match map.get(&Key::Keyword("var".into())) {
            Some(Value::Vector(items)) => items,
            other => panic!("expected :var vector, got {:?}", other),
        };
        let Some(Value::Map(entry)) = vars.get(0) else {
            panic!("expected var entry");
        };
        assert!(entry.contains_key(&Key::Keyword("sym".into())));
        assert!(entry.contains_key(&Key::Keyword("match".into())));
    }

    #[test]
    fn nav_accepts_regex_queries() {
        let v = eval_nav_src("(nav /disj/ :var)");
        let map = match v {
            Value::Map(map) => map,
            other => panic!("expected nav map, got {:?}", other),
        };
        assert_eq!(
            map.get(&Key::Keyword("kind".into())),
            Some(&Value::Symbol(":nav".into()))
        );
    }

    #[test]
    fn nav_matches_doc_name() {
        let v = eval_nav_src("(nav \"nav\" :doc)");
        let map = match v {
            Value::Map(map) => map,
            other => panic!("expected nav map, got {:?}", other),
        };
        let docs = match map.get(&Key::Keyword("doc".into())) {
            Some(Value::Vector(items)) => items,
            other => panic!("expected :doc vector, got {:?}", other),
        };
        let mut found = false;
        for entry in docs {
            if let Value::Map(entry) = entry {
                if entry.get(&Key::Keyword("name".into())) == Some(&Value::Symbol("nav".into())) {
                    found = true;
                    break;
                }
            }
        }
        assert!(found, "expected doc entry for nav");
    }

    #[test]
    fn defn_registers_fn_meta() {
        fn_meta::clear_for_tests();
        eval_src("(do (defn sum [x: Int y: Int] -> Int (+ x y)) sum)");
        let meta = fn_meta::get("user::sum").expect("meta");
        assert_eq!(meta.ns, "user");
        assert_eq!(meta.arglist, vec!["[x: Int, y: Int] -> Int"]);
        let overload = meta.overloads.first().expect("overload");
        assert_eq!(
            overload.arg_types,
            vec![MetaTypeKind::Int, MetaTypeKind::Int]
        );
        assert_eq!(overload.ret_type, MetaTypeKind::Int);
    }

    #[test]
    fn defn_accepts_last_subject_pos_metadata() {
        eval_src("(defn fn-last-meta {:subject-pos :last} \"doc\" [x] x)");
        let meta = fn_meta::get("user::fn-last-meta").expect("fn-last-meta metadata");
        assert_eq!(meta.subject_pos, Some(SubjectPos::Last));
    }

    #[test]
    fn defn_attaches_meta_map_to_function_value() {
        let meta_value =
            eval_src("(do (defn fn-meta-doc \"doc\" {:meta 123} [x] x) (meta fn-meta-doc))");
        let map = match meta_value {
            Value::Map(m) => m,
            other => panic!("expected meta map, got {:?}", other),
        };
        assert_eq!(
            map.get(&Key::Keyword("meta".into())),
            Some(&Value::Int(123))
        );
    }

    #[test]
    fn local_defn_ignores_tail_defn_return() {
        let v = eval_src("(do (defn t1 [] 1 (-defn g [] 2)) (t1))");
        assert_eq!(v, Value::Int(1));
    }

    #[test]
    fn local_defn_can_appear_mid_body() {
        let v = eval_src("(do (defn t2 [] (-defn g [] 3) 10 (g)) (t2))");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn local_defn_allows_mutual_recursion() {
        run_with_large_stack(|| {
            let v = eval_src(
                "(do (defn t3 [n]
                       (-defn even? [x]
                         (if (zero? x) true (odd? (dec x))))
                       (-defn odd? [x]
                         (if (zero? x) false (even? (dec x))))
                       (even? n))
                     [(t3 10) (t3 11)])",
            );
            assert_eq!(
                v,
                Value::Vector(Vector::from(vec![Value::Bool(true), Value::Bool(false)]))
            );
        });
    }

    #[test]
    fn local_defn_is_scoped_to_outer_function() {
        let v = eval_src("(do (defn outer [] (-defn inner [] 1) (inner)) (outer))");
        assert_eq!(v, Value::Int(1));
        let err = eval_err("(do (defn outer [] (-defn inner [] 1) (inner)) (inner))");
        assert!(err.to_string().contains("Unbound symbol"));
    }

    #[test]
    fn local_defn_nested_ok() {
        let v = eval_src("(do (defn t5 [] (-defn a [] (-defn b [] 7) (b)) (a)) (t5))");
        assert_eq!(v, Value::Int(7));
    }

    #[test]
    fn local_defn_returns_nil_without_body() {
        let v = eval_src("(do (defn t6 [] (-defn a [] 1)) (t6))");
        assert_eq!(v, Value::Nil);
    }

    #[test]
    fn def_is_top_level_only() {
        let err = eval_err("(do (defn t1 [] (def x 1) x) (t1))");
        assert!(err.to_string().contains("def is top-level only"));
    }

    #[test]
    fn defn_is_top_level_only() {
        let err = eval_err("(do (defn t2 [] (defn g [] 1) (g)) (t2))");
        assert!(err.to_string().contains("defn is top-level only"));
    }

    #[test]
    fn local_def_binds_sequentially() {
        let v = eval_src(
            "(do (defn t4 [x]
                   (-def a (+ x 1))
                   (-def b (+ a 1))
                   (+ a b))
                 (t4 1))",
        );
        assert_eq!(v, Value::Int(5));
    }

    #[test]
    fn set_updates_existing_var() {
        let v = eval_src("(do (def x 1) (set! x 2) x)");
        assert_eq!(v, Value::Int(2));
    }

    #[test]
    fn redef_aliases_set() {
        let v = eval_src("(do (def x 1) (redef x 3) x)");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn where_does_not_override_return_value() {
        let v = eval_src("(do (defn f [] 1 (where (-defn i [] 123))) (f))");
        assert_eq!(v, Value::Int(1));
        let v = eval_src("(do (defn f [] (i) (where (-defn i [] 123))) (f))");
        assert_eq!(v, Value::Int(123));
        let v = eval_src("(do (defn f [] (i) (where (-defn i [] 123)) 9) (f))");
        assert_eq!(v, Value::Int(9));
    }

    #[test]
    fn where_accepts_defn_as_local_defn() {
        let v = eval_src("(do (defn f [] (where (defn inner [] 2)) (inner)) (f))");
        assert_eq!(v, Value::Int(2));
        let err = eval_err("(do (defn f [] (where (defn inner [] 2)) (inner)) (inner))");
        assert!(err.to_string().contains("Unbound symbol"));
    }

    #[test]
    fn set_errors_on_undefined_var() {
        let err = eval_err("(do (defn t5 [] (set! y 1)) (t5))");
        assert!(err.to_string().contains("set! target is undefined"));
    }

    #[test]
    fn multi_arity_defn_dispatches_by_argument_count() {
        let v = eval_src(
            "(do (defn describe
                   ([x] (str \"solo:\" x))
                   ([x y] (str x \"+\" y)))
                 [(describe \"a\") (describe \"a\" \"b\")])",
        );
        assert_eq!(v, eval_src("[\"solo:a\" \"a+b\"]"));
    }

    #[test]
    fn multi_arity_defn_prefers_exact_match() {
        let v = eval_src(
            "(do (defn multi
                   ([x] [:one x])
                   ([x y z] [:three x y z]))
                 (multi 42))",
        );
        assert_eq!(v, eval_src("[:one 42]"));
    }

    #[test]
    fn multi_arity_defn_errors_on_missing_arity() {
        eval_err(
            "(do (defn route
                   ([x] [:one x])
                   ([x y z] [:three x y z]))
                 ((route 1 2) 3))",
        );
    }

    #[test]
    fn when_and_cond_work() {
        let v1 = eval_src("(when false 1 2)");
        assert_eq!(v1, Value::Nil);
        let v2 = eval_src("(when true 1 2)");
        assert_eq!(v2, Value::Int(2));
        let v3 = eval_src("(cond false 1 :else 5)");
        assert_eq!(v3, Value::Int(5));
        let v4 = eval_src("(cond false 1 _ 9)");
        assert_eq!(v4, Value::Int(9));
    }

    #[test]
    fn repl_on_error_is_use_only() {
        let err = eval_err("(set! *repl-on-error* true)");
        assert!(err
            .to_string()
            .contains("use (use repl-on-error true|false)"));
    }

    #[test]
    fn extended_when_forms_work() {
        let v1 = eval_src("(when-not false 1 2)");
        assert_eq!(v1, Value::Int(2));
        let v2 = eval_src("(when-not true 1 2)");
        assert_eq!(v2, Value::Nil);
        let v3 = eval_src("(when-let [x 1] (+ x 2))");
        assert_eq!(v3, Value::Int(3));
        let v4 = eval_src("(if-let [x false] 1 2)");
        assert_eq!(v4, Value::Int(2));
        let v5 = eval_src("(if-some [x false] x 2)");
        assert_eq!(v5, Value::Bool(false));
        let v6 = eval_src("(if-some [x nil] x 9)");
        assert_eq!(v6, Value::Int(9));
    }

    #[test]
    fn threading_forms_work() {
        let t1 = eval_src("(-> 1 inc (+ 2))");
        assert_eq!(t1, Value::Int(4));
        let t2 = eval_src("(->> [1 2 3] (map inc) (reduce + 0))");
        assert_eq!(t2, Value::Int(9));
        let t3 = eval_src("(as-> 1 x (+ x 2) (* x 3))");
        assert_eq!(t3, Value::Int(9));
        let c1 = eval_src("(cond-> 1 true inc false inc)");
        assert_eq!(c1, Value::Int(2));
        let c2 = eval_src("(cond->> [1 2] true (map inc) true (reduce + 0))");
        assert_eq!(c2, Value::Int(5));
        let s1 = eval_src("(some-> {:a 1} (get :a) inc)");
        assert_eq!(s1, Value::Int(2));
        let s2 = eval_src("(some-> {:a nil} (get :a) inc)");
        assert_eq!(s2, Value::Nil);
        let s3 = eval_src("(some->> [1 2 3] (map inc) (filter #(> % 3)) first)");
        assert_eq!(s3, Value::Int(4));
        let s4 = eval_src("(some->> [1 2 3] (map inc) (filter #(> % 10)) first inc)");
        assert_eq!(s4, Value::Nil);
    }

    #[test]
    fn as_thread_supports_as_directive() {
        let v = eval_src("(as-> 10 v (as orig) (+ v 2) (+ v orig))");
        assert_eq!(v, Value::Int(22));
    }

    #[test]
    fn as_thread_supports_let_directive() {
        let v = eval_src("(do (as-> 10 v (let orig) (+ v 2)) *orig)");
        assert_eq!(v, Value::Int(10));
    }

    #[test]
    fn oop_chain_as_directive_spans_dot_and_oop_stages() {
        let v = eval_src("10.(as x).inc.inc.(+ ? x)");
        assert_eq!(v, Value::Int(22));
    }

    #[test]
    fn oop_chain_let_directive_spans_dot_and_oop_stages() {
        let v = eval_src("10.(let x).inc.inc.(+ ? *x)");
        assert_eq!(v, Value::Int(22));
    }

    #[test]
    fn threading_placeholder_partials_work() {
        let r1 = eval_src("(-> 0 inc (vector :head ?))");
        assert_eq!(r1, eval_src("[:head 1]"));
        let r2 = eval_src("(-> 0 inc (vector ? :tail))");
        assert_eq!(r2, eval_src("[1 :tail]"));
        let r3 = eval_src("(->> 0 inc (vector :tail ?))");
        assert_eq!(r3, eval_src("[:tail 1]"));
    }

    #[test]
    fn spread_expands_function_args() {
        let direct = eval_src("(let [xs [2 3]] (vec ((fn [& args] args) 1 *xs 4)))");
        match direct {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
        let expr = eval_src("(vec ((fn [& args] args) 0 * (range 2)))");
        match expr {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::Int(0), Value::Int(0), Value::Int(1)].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
        let adjacent = eval_src("(vec ((fn [& args] args) 7 *(range 2)))");
        match adjacent {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::Int(7), Value::Int(0), Value::Int(1)].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
        let nested = eval_src("(do (def *xs [9]) (vec ((fn [& args] args) **xs 10)))");
        match nested {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(9), Value::Int(10)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn spread_in_literals() {
        let vec_val = eval_src("(let [xs [2 3]] [1 *xs 4 * (list 5 6)])");
        match vec_val {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![
                        Value::Int(1),
                        Value::Int(2),
                        Value::Int(3),
                        Value::Int(4),
                        Value::Int(5),
                        Value::Int(6)
                    ]
                    .into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
        let set_val = eval_src("#{1 * [2 3] 4}");
        match set_val {
            Value::Set(items) => {
                assert_eq!(items.len(), 4);
                assert!(items.contains(&Value::Int(1)));
                assert!(items.contains(&Value::Int(2)));
                assert!(items.contains(&Value::Int(3)));
                assert!(items.contains(&Value::Int(4)));
            }
            other => panic!("expected set, got {:?}", other),
        }
        let set_spread_set = eval_src("#{1 *#{2 3} 4}");
        match set_spread_set {
            Value::Set(items) => {
                assert!(items.contains(&Value::Int(2)));
                assert!(items.contains(&Value::Int(3)));
            }
            other => panic!("expected set, got {:?}", other),
        }
        let map_val = eval_src("(let [m {:x 1} more [[:z 3]]] {:a 0 *m :b 9 * more})");
        match map_val {
            Value::Map(map) => {
                assert_eq!(map.get(&Key::Keyword("a".into())), Some(&Value::Int(0)));
                assert_eq!(map.get(&Key::Keyword("x".into())), Some(&Value::Int(1)));
                assert_eq!(map.get(&Key::Keyword("b".into())), Some(&Value::Int(9)));
                assert_eq!(map.get(&Key::Keyword("z".into())), Some(&Value::Int(3)));
            }
            other => panic!("expected map, got {:?}", other),
        }
    }

    #[test]
    fn spread_errors_on_invalid_inputs() {
        let err = eval_err("(let [n 1] ((fn [& args] args) *n))");
        assert!(err.to_string().contains("spread expects a collection"));
        let map_err = eval_err("(let [n 1] {:a 1 *n})");
        assert!(map_err
            .to_string()
            .contains("map spread expects map or [k v] seq"));
    }

    #[test]
    fn spread_compact_forms_work() {
        let compact_args = eval_src("(vec ((fn [& args] args) *[1 2] 3))");
        assert_eq!(compact_args, eval_src("[1 2 3]"));

        let compact_vec = eval_src("[1 2 *[3 4] 5]");
        assert_eq!(compact_vec, eval_src("[1 2 3 4 5]"));

        let mapped = eval_src("(vec (map *[inc [0 1 2]]))");
        assert_eq!(mapped, eval_src("[1 2 3]"));
    }

    #[test]
    fn placeholder_partial_transforms_call_to_lambda() {
        let v = eval_src("((+ ? 5) 7)");
        assert_eq!(v, Value::Int(12));
    }

    #[test]
    fn question_placeholder_reuses_first_arg() {
        let v = eval_src("((+ ? ?) 3)");
        assert_eq!(v, Value::Int(6));

        let err = eval_err("((+ ? ?) 3 4)");
        assert!(err.to_string().contains("Arity mismatch"));
    }

    #[test]
    fn nested_placeholder_partials_work() {
        let v = eval_src("((str (first ?)) [10 20])");
        assert_eq!(v, Value::String("10".into()));
    }

    #[test]
    fn placeholder_partials_use_function_arg_positions() {
        let v = eval_src("(filter (< (first ?) 2) [[1 0] [3 0]])");
        assert_eq!(v, eval_src("[[1 0]]"));
    }

    #[test]
    fn placeholder_partials_do_not_lambdaize_outer_call_with_nested_call() {
        let v = eval_src("(first (filter (even? ?) [1 2 3 4]))");
        assert_eq!(v, Value::Int(2));
    }

    #[test]
    fn placeholder_partials_work_with_identity() {
        let v = eval_src("((identity (inc ?)) 1)");
        assert_eq!(v, Value::Int(2));
    }

    #[test]
    fn placeholder_partials_work_with_oop() {
        let v = eval_src("(use oop-syntax true) ((identity ?.inc) 1)");
        assert_eq!(v, Value::Int(2));
    }

    #[test]
    fn placeholder_partials_work_in_oop_method_args() {
        let v = eval_src(
            "(use oop-syntax true)
             (let [xs [[1] [2] [3]]]
               (vec xs.map((do ?.0))))",
        );
        assert_eq!(v, eval_src("[1 2 3]"));
    }

    #[test]
    fn nested_fn_in_method_captures_self() {
        let v = eval_src(
            "(use oop-syntax true)
             (let [self {:line (method line [a b] (+ a b))
                         :pairs [[1 2] [3 4]]}]
               (reduce (fn [_ p] (self.line (get p 0) (get p 1))) nil self:pairs))",
        );
        assert_eq!(v, Value::Int(7));
    }

    #[test]
    fn nested_fn_rest_params_do_not_trigger_method_rewrite() {
        let v = eval_src("(do (defn t [] ((fn [& args] (count args)) 1)) (t))");
        assert_eq!(v, Value::Int(1));
    }

    #[test]
    fn nested_fn_body_can_use_self_placeholder() {
        let v = eval_src(
            "(use oop-syntax true)
             (let [obj {:x 10 :make (fn [] (fn [] &:x))}]
               ((obj.make)))",
        );
        assert_eq!(v, Value::Int(10));
    }

    #[test]
    fn placeholder_partials_work_with_regex() {
        let matched = eval_source_with_engines(
            "((#/.*map.*/ (str (first ?))) [\"amapaa\"])",
            EvalOptions::default(),
            &[],
        )
        .unwrap();
        assert_eq!(matched, Value::String("amapaa".into()));
    }

    #[test]
    fn placeholder_partials_lambdaize_non_function_args() {
        let v = eval_src("((+ 1 (inc ?)) 10)");
        assert_eq!(v, Value::Int(12));
    }

    #[test]
    fn numbered_placeholders_work_in_partials() {
        let v = eval_src("((list ?3 *?4 (+ ?1 ?2)) 10 20 30 [1 2 3])");
        let expected = eval_src("(list 30 1 2 3 30)");
        assert_eq!(v, expected);
    }

    #[test]
    fn rest_placeholder_expands_arguments() {
        let joined = eval_src("((str *?) [\"a\" \"b\" \"c\"])");
        assert_eq!(joined, Value::String("abc".into()));

        let with_positional = eval_src("((list ?1 *?2 9) 1 [2 3 4])");
        let expected = eval_src("(list 1 2 3 4 9)");
        assert_eq!(with_positional, expected);
    }

    #[test]
    fn short_fn_accepts_question_placeholders() {
        let v = eval_src("(#(+ 1 ?) 2)");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn short_fn_spread_placeholder_expands_arguments() {
        let v = eval_src("(#(list *?) [1 2 3])");
        let expected = eval_src("(list 1 2 3)");
        assert_eq!(v, expected);

        let with_positional = eval_src("(#(list %1 *?2 9) 1 [2 3 4])");
        let expected = eval_src("(list 1 2 3 4 9)");
        assert_eq!(with_positional, expected);
    }

    #[test]
    fn short_fn_interpolated_string_replaces_placeholder() {
        let v = eval_src("(#(do \"[#{?}]\") 9)");
        assert_eq!(v, Value::String("[9]".into()));
    }

    #[test]
    fn unbound_question_placeholder_errors() {
        let err = eval_err("?");
        assert!(err
            .to_string()
            .contains("? is a placeholder; use #(...) or (... ? ...)"));
        let callee_err = eval_err("(? 1)");
        assert!(callee_err
            .to_string()
            .contains("placeholder cannot be used as callee"));
    }

    #[test]
    fn fn_type_reports_lambda_signature() {
        let v = eval_src("(fn-type (fn [x] x))");
        match v {
            Value::String(s) => assert!(s.contains("->")),
            other => panic!("expected string, got {:?}", other),
        }
    }

    #[test]
    fn fn_type_reports_short_fn_return() {
        let v = eval_src("(fn-type #(str %))");
        match v {
            Value::String(s) => assert!(s.contains("-> Str")),
            other => panic!("expected string, got {:?}", other),
        }
    }

    #[test]
    fn fn_type_reports_placeholder_partial_return() {
        let v = eval_src("(fn-type (str ?3 *?4 (int (+ ?1 ?2))))");
        match v {
            Value::String(s) => assert!(s.contains("-> Str")),
            other => panic!("expected string, got {:?}", other),
        }
    }

    #[test]
    fn describe_includes_fn_type_for_functions() {
        let v = eval_src("(describe (fn [x] x))");
        match v {
            Value::Map(map) => assert!(map.contains_key(&Key::Keyword("fn-type".into()))),
            other => panic!("expected map, got {:?}", other),
        }
    }

    #[test]
    fn looping_forms_work() {
        let l = eval_src(
            "(loop [cnt 3 acc 0]
               (if (= cnt 0)
                 acc
                 (recur (dec cnt) (+ acc cnt))))",
        );
        assert_eq!(l, Value::Int(6));
        let w = eval_src("(do (def x 0) (while (< x 3) (set! x (+ x 1))) x)");
        assert_eq!(w, Value::Int(3));
        let d = eval_src(
            "(do (def pairs [])
                  (doseq [x [1 2] y [3 4]]
                    (set! pairs (conj pairs [x y])))
                  pairs)",
        );
        match d {
            Value::Vector(items) => {
                let expected = vec![
                    Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(3)])),
                    Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(4)])),
                    Value::Vector(Vector::from(vec![Value::Int(2), Value::Int(3)])),
                    Value::Vector(Vector::from(vec![Value::Int(2), Value::Int(4)])),
                ]
                .into();
                assert_eq!(items, expected);
            }
            other => panic!("expected vector, got {:?}", other),
        }
        let dt = eval_src(
            "(do (def xs [])
                  (dotimes [i 3]
                    (set! xs (conj xs i)))
                  xs)",
        );
        match dt {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::Int(0), Value::Int(1), Value::Int(2)].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn zero_predicate_handles_numbers() {
        let zero_int = eval_src("(zero? 0)");
        assert_eq!(zero_int, Value::Bool(true));
        let zero_float = eval_src("(zero? 0.0)");
        assert_eq!(zero_float, Value::Bool(true));
        let non_zero = eval_src("(zero? 3)");
        assert_eq!(non_zero, Value::Bool(false));
    }

    #[test]
    fn for_comprehension_builds_cross_product() {
        let v = eval_src("(for [x [1 2] y [10 20]] (+ x y))");
        match v {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![
                        Value::Int(11),
                        Value::Int(21),
                        Value::Int(12),
                        Value::Int(22)
                    ]
                    .into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn for_supports_when_and_let() {
        let v = eval_src("(for [x [1 2 3 4] :when (even? x) :let [z (+ x 1)]] z)");
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(3), Value::Int(5)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn condp_supports_default_and_result() {
        let v = eval_src("(condp = 3 1 :one 2 :two 3 :three :else :none)");
        assert_eq!(v, eval_src(":three"));
        let v2 = eval_src("(condp = 3 1 :one 2 :two _ :none)");
        assert_eq!(v2, eval_src(":none"));
    }

    #[test]
    fn condp_supports_threading_result() {
        let v = eval_src("(condp re-find \"abc123\" #/\\d+/ :>> count :else 0)");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn with_redefs_temporarily_overrides() {
        let v = eval_src(
            "(do (defn greet [] \"hi\")\n                  (let [a (with-redefs [greet (fn [] \"yo\")] (greet))\n                        b (greet)]\n                    [a b]))",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::String("yo".into()), Value::String("hi".into())].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn read_string_parses_forms_without_eval() {
        let vec_value = eval_src("(read-string \"[1 2 3]\")");
        match vec_value {
            Value::Vector(items) => assert_eq!(
                items,
                vec![Value::Int(1), Value::Int(2), Value::Int(3)].into()
            ),
            other => panic!("expected vector, got {:?}", other),
        }
        let list_value = eval_src("(read-string \"(+ 1 2)\")");
        match list_value {
            Value::List(items) => {
                assert_eq!(
                    items,
                    vec![Value::Symbol("+".into()), Value::Int(1), Value::Int(2)].into()
                );
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn tree_seq_traverses_preorder() {
        let v = eval_src(
            "(let [tree {:v 1 :children [{:v 2} {:v 3}]}\n                   branch? (fn [n] (contains? n :children))\n                   kids (fn [n] (get n :children []))]\n               (vec (tree-seq branch? kids tree)))",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items.len(), 3);
                assert!(matches!(&items[0], Value::Map(_)));
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn re_seq_returns_seq_value() {
        let v = eval_src("(vec (re-seq #/\\d+/ \"a1b22\"))");
        match v {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::String("1".into()), Value::String("22".into())].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn recur_inside_function_body() {
        let via_defn = eval_src(
            "(do (defn sum-down [n acc]
                    (if (= n 0)
                      acc
                      (recur (dec n) (+ acc n))))
                 (sum-down 5 0))",
        );
        assert_eq!(via_defn, Value::Int(15));
        let via_fn = eval_src(
            "((fn [n acc]
                (if (= n 0)
                  acc
                  (recur (dec n) (+ acc n))))
              5 0)",
        );
        assert_eq!(via_fn, Value::Int(15));
    }

    #[test]
    fn recur_in_err_fin_is_error() {
        let err = eval_err(
            "(defn f [n]
               (if (zero? n)
                 n
                 (recur (dec n)))
               (err (recur n)))",
        );
        assert!(error_message(&err).contains("recur must appear in tail position"));
    }

    #[test]
    fn recur_with_rest_parameters() {
        let v = eval_src(
            "((fn [n & xs]
                (if (zero? n)
                  xs
                  (recur (dec n) (rest xs))))
              2 1 2 3 4)",
        );
        match v {
            Value::List(items) => {
                assert_eq!(items, vec![Value::Int(3), Value::Int(4)].into());
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn recur_in_defn_tail_calls_large_depth() {
        let v = eval_src(
            "(do (defn countdown [n]
                    (if (zero? n)
                      0
                      (recur (dec n))))
                 (countdown 50000))",
        );
        assert_eq!(v, Value::Int(0));
    }

    #[test]
    fn fin_runs_once_after_recur() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (defn countdown [n]
                   (if (zero? n)
                     0
                     (recur (dec n)))
                   (fin (atom-set! x (+ @x 1))))
                 (countdown 3)
                 @x)",
        );
        assert_eq!(v, Value::Int(1));
    }

    #[test]
    fn recur_packs_rest_arguments_for_variadic_functions() {
        let v = eval_src(
            "((fn [n acc & xs]
                (if (zero? n)
                  acc
                  (recur (dec n) (+ acc (count xs)) 1 2 3)))
              2 0)",
        );
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn loop_handles_large_iteration_counts() {
        let v = eval_src(
            "(loop [cnt 100000 acc 0]
               (if (zero? cnt)
                 acc
                 (recur (dec cnt) (+ acc 1))))",
        );
        assert_eq!(v, Value::Int(100000));
    }

    #[test]
    fn short_fn_preserves_call_structure() {
        let v = eval_src("(do (def f #(+ %1 %2)) (f 1 2))");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn short_fn_respects_let_shadowing() {
        let v = eval_src("(do (def f #(let [it %] (+ it 1))) (f 2))");
        assert_eq!(v, Value::Int(3));
    }

    #[test]
    fn short_fn_keeps_let_binding_names() {
        let mut reader =
            Reader::new_with_options("#(let [it %] $rb{it})", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let expanded = match &forms[0].kind {
            FormKind::ShortFn(body) => expand_short_fn_to_list(body, forms[0].span),
            other => panic!("expected short fn, got {:?}", other),
        };
        match &expanded.kind {
            FormKind::List(items) => {
                assert_eq!(items.len(), 3);
                match &items[2].kind {
                    FormKind::List(body_items) => match &body_items[1].kind {
                        FormKind::Vector(bindings) => {
                            let name = match bindings.get(0).map(|f| &f.kind) {
                                Some(FormKind::Symbol(s)) => s,
                                other => panic!("expected binding symbol, got {:?}", other),
                            };
                            assert_eq!(name, "it");
                        }
                        other => panic!("expected bindings vector, got {:?}", other),
                    },
                    other => panic!("expected let body list, got {:?}", other),
                }
            }
            other => panic!("expected fn list, got {:?}", other),
        }
    }

    #[test]
    fn short_fn_allows_special_forms() {
        let v = eval_src("(map #(let [it %] (+ it 1)) [1 2])");
        match v {
            Value::Vector(items) => assert_eq!(items, vec![Value::Int(2), Value::Int(3)].into()),
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn try_catches_throw_and_runs_finally() {
        let v = eval_src(
            "(do (def x 0)
                 (def r (try (throw 1)
                             (catch e (do (set! x e) e))
                             (finally (set! x 99))))
                 [r x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(1), Value::Int(99)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn try_err_fin_calls_handler_and_finally() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (def r (try (throw 7)
                             (err (do (atom-set! x 1) ?))
                             (fin (atom-set! x 2))))
                 [r @x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(7), Value::Int(2)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn try_err_fin_order_errors() {
        let err = eval_err("(try 1 (fin 2) (err 3))");
        assert!(error_message(&err).contains("fin must appear at end of try body"));
        let err = eval_err("(try 1 (err 2) (err 3))");
        assert!(error_message(&err).contains("multiple err clauses"));
    }

    #[test]
    fn defn_tail_err_fin_desugars() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (defn f []
                   (throw 1)
                   (err (do (atom-set! x 1) ?))
                   (fin (atom-set! x 2)))
                 [(f) @x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(1), Value::Int(2)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn do_tail_err_fin_desugars() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (def r (do (throw 7)
                            (err (do (atom-set! x 1) ?))
                            (fin (atom-set! x 2))))
                 [r @x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(7), Value::Int(2)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn fn_tail_err_fin_desugars() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (def f (fn []
                          (throw 3)
                          (err (do (atom-set! x 1) ?))
                          (fin (atom-set! x 2))))
                 [(f) @x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(3), Value::Int(2)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn short_fn_do_allows_err_fin() {
        let v = eval_src("(#(do (throw 4) (err 9)))");
        assert_eq!(v, Value::Int(9));
    }

    #[test]
    fn err_fin_tail_must_be_last_in_body() {
        let err = eval_err("(do 1 (err 2) 3)");
        assert!(error_message(&err).contains("err must appear at end of a body"));
    }

    #[test]
    fn err_fin_cannot_combine_with_catch_finally() {
        let err = eval_err("(try 1 (catch RuntimeError e :ok) (err 2))");
        assert!(error_message(&err).contains("err/fin cannot be combined with catch/finally"));
    }

    #[test]
    fn short_try_calls_handler_on_error() {
        let v = eval_src("(try (throw 7) (fn [e] e))");
        assert_eq!(v, Value::Int(7));
    }

    #[test]
    fn short_try_two_args_non_callable_is_error() {
        let err = eval_err("(try (throw 1) 42)");
        assert!(error_message(&err).contains("Invalid try form"));
    }

    #[test]
    fn short_try_with_bindings_calls_handler_and_finally() {
        let v = eval_src(
            "(do (def x 9)
                 (def r (try [i 0]
                             (throw 7)
                             (fn [e] (+ i e))
                             (fn [] (set! x i))))
                 [r x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(7), Value::Int(0)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn short_try_with_bindings_calls_handler_only() {
        let v = eval_src("(do (def r (try [i 1] (throw 7) (fn [e] (+ i e)))) r)");
        assert_eq!(v, Value::Int(8));
    }

    #[test]
    fn short_try_with_bindings_finally_only_runs() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (def r (try [i 2] 10 (fn [] (atom-set! x i))))
                 [r @x])",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(items, vec![Value::Int(10), Value::Int(2)].into());
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn invalid_short_try_is_syntax_error_before_eval() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (try
                   (try 1 (atom-set! x 1) (atom-set! x 2))
                   (catch RuntimeError e :ok))
                 @x)",
        );
        assert_eq!(v, Value::Int(0));
    }

    #[test]
    fn short_try_finally_only_runs() {
        let v = eval_src(
            "(do (def x (atom 0))
                 (try 1 2 3 #(atom-set! x 9))
                 @x)",
        );
        assert_eq!(v, Value::Int(9));
    }

    #[test]
    fn short_try_handler_placeholder_runs() {
        let v = eval_src(
            "(do (def y (atom 0))
                 (try (/ 1 0) (list (atom-set! y 7) ?))
                 @y)",
        );
        assert_eq!(v, Value::Int(7));
    }

    #[test]
    fn short_try_handler_finally_ordered() {
        let v = eval_src(
            "(do (def a (atom []))
                 (try (/ 1 0)
                      (list (swap! a conj :err) ?)
                      #(swap! a conj :fin))
                 @a)",
        );
        match v {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::Symbol(":err".into()), Value::Symbol(":fin".into())].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn short_try_reverse_order_is_syntax_error_before_eval() {
        let v = eval_src(
            "(do (def z (atom 0))
                 (try
                   (try 1 #(atom-set! z 1) (println \"err:\" ?))
                   (catch RuntimeError e :ok))
                 @z)",
        );
        assert_eq!(v, Value::Int(0));
    }

    #[test]
    fn regex_literal_is_evaluated() {
        let v = eval_src("(split \"foo123bar\" /[0-9]+/)");
        match v {
            Value::Vector(items) => {
                assert_eq!(
                    items,
                    vec![Value::String("foo".into()), Value::String("bar".into())].into()
                );
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn infix_expr_evaluates_math() {
        let v = eval_src("(clove.syntax::infix (+ 1 (* 2 3)))");
        assert_eq!(v, Value::Int(7));
    }

    #[test]
    fn infix_expr_supports_mod() {
        let v = eval_src("(clove.syntax::infix (% 5 2))");
        assert_eq!(v, Value::Int(1));
    }

    #[test]
    fn infix_expr_grouping_changes_precedence() {
        let v = eval_src("(clove.syntax::infix (* (+ 1 2) 3))");
        assert_eq!(v, Value::Int(9));
    }

    #[test]
    fn infix_expr_supports_string_ops() {
        let v = eval_src("(clove.syntax::infix (+ \"s\" \"t\"))");
        assert_eq!(v, Value::String("st".into()));
        let v = eval_src("(clove.syntax::infix (* \"s\" 3))");
        assert_eq!(v, Value::String("sss".into()));
    }

    #[test]
    fn infix_expr_short_circuits() {
        let v = eval_src("(clove.syntax::infix (&& false (/ 1 0)))");
        assert_eq!(v, Value::Bool(false));
        let v = eval_src("(clove.syntax::infix (|| true (/ 1 0)))");
        assert_eq!(v, Value::Bool(true));
    }

    #[test]
    fn infix_expr_supports_eq_ops() {
        let v = eval_src("(clove.syntax::infix (== 1 1))");
        assert_eq!(v, Value::Bool(true));
        let v = eval_src("(clove.syntax::infix (!= 1 2))");
        assert_eq!(v, Value::Bool(true));
    }
}
