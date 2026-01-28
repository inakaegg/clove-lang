use crate::ast::{Form, FormKind, InterpolatedPart, Key, MapItem, Span, Value};
use crate::compiler::APPLY_SYM;
use crate::cow::{HashMap as CowHashMap, Vector};
use crate::eval::form_to_value;
use crate::reader::{
    MAP_REF_SYM, OOP_BARE_SYM, OOP_DOT_STAGE_SYM, OOP_INDEX_SYM, OOP_METHOD_SYM, OOP_NIL_SAFE_SYM,
    OOP_SEG_SYM, OOP_SYNTAX_SYM,
};
use crate::spread::strip_spread_symbol;
use crate::symbols::canonical_symbol_name;
use crate::try_form::parse_err_fin_tail;
use crate::types::TypeKind;
use crate::vm::bytecode::{BuiltinId, Capture, Chunk, FunctionPrototype, Instruction};
use crate::vm::error::VmError;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

const UNSUPPORTED_SPECIAL_FORM_HEADS: &[&str] = &[
    "deftype",
    "defenum",
    "doc",
    "meta",
    "source",
    "describe",
    "describe-type",
    "infer-type",
    "enum-members",
    "clove.syntax::oop",
    "clove.syntax::oop-bare",
    "clove.syntax::oop-dot",
    "clove.syntax::oop-index",
    "clove.syntax::oop-seg",
    "clove.syntax::oop-method",
    "use-syntax",
    "use",
    "current-ns",
    "quote",
    "set!",
    "redef",
    "__set-in-chain",
    "-def",
    "-defn",
    "when-not",
    "when-let",
    "if-not",
    "if-let",
    "if-some",
    "condp",
    "as->",
    "cond->",
    "cond->>",
    "some->",
    "some->>",
    "for",
    "while",
    "doseq",
    "each",
    "dotimes",
    "try",
    "err",
    "fin",
    "throw",
    "p",
    "pvalues",
    "comment",
    "match",
    "break",
    "repl",
    "debug",
    "with-redefs",
    "with-dyn",
    "go-loop",
    "scope-loop",
    "async::scope-loop",
    "async-scope",
    "async::scope",
    "ns-map",
    "nav",
    "lookup",
    "create-ns",
    "refer",
    "resolve",
    "load-file",
    "load-string",
    "delay",
    "doto",
    "with-open",
    "__range_literal",
    "eval",
];

static VM_PLACEHOLDER_ALLOC_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TailContext {
    Value,
    Tail,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ThreadStyle {
    First,
    Last,
}

#[derive(Clone, Debug)]
enum RecurTarget {
    Loop {
        start_ip: usize,
        locals: Vec<usize>,
        base_count: usize,
    },
    Function {
        start_ip: usize,
        param_count: usize,
        has_rest: bool,
        base_count: usize,
    },
}

#[derive(Clone, Debug)]
pub struct CompiledChunk {
    pub chunk: Chunk,
    pub local_count: usize,
}

#[derive(Clone, Debug)]
struct Local {
    name: String,
    depth: usize,
    index: usize,
    ty: LocalType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LocalType {
    Int,
    Float,
    Bool,
    Unknown,
}

#[derive(Clone, Debug)]
struct ParamSpec {
    name: String,
    ty: LocalType,
}

#[derive(Clone, Debug)]
struct ParsedParams {
    params: Vec<ParamSpec>,
    rest: Option<String>,
}

#[derive(Clone, Debug)]
struct FnClauseSpec {
    params_form: Form,
    body: Vec<Form>,
}

#[derive(Clone, Debug)]
struct DefnSignature {
    doc: Option<String>,
    clauses: Vec<FnClauseSpec>,
    meta_map: Option<Form>,
}

#[derive(Clone)]
struct LocalDefnSpec {
    name: String,
    form: Form,
}

#[derive(Default)]
struct TypeScope {
    stack: Vec<HashMap<String, LocalType>>,
    fn_stack: Vec<HashMap<String, LocalType>>,
}

impl TypeScope {
    fn push(&mut self) {
        self.stack.push(HashMap::new());
        self.fn_stack.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.stack.pop();
        self.fn_stack.pop();
    }

    fn insert(&mut self, name: &str, ty: LocalType) {
        let Some(scope) = self.stack.last_mut() else {
            return;
        };
        scope.insert(canonical_symbol_name(name).into_owned(), ty);
    }

    fn insert_fn_return(&mut self, name: &str, ty: LocalType) {
        let Some(scope) = self.fn_stack.last_mut() else {
            return;
        };
        scope.insert(canonical_symbol_name(name).into_owned(), ty);
    }

    fn lookup(&self, name: &str) -> Option<LocalType> {
        let canonical = canonical_symbol_name(name);
        for scope in self.stack.iter().rev() {
            if let Some(ty) = scope.get(canonical.as_ref()) {
                return Some(*ty);
            }
        }
        None
    }

    fn lookup_fn_return(&self, name: &str) -> Option<LocalType> {
        let canonical = canonical_symbol_name(name);
        for scope in self.fn_stack.iter().rev() {
            if let Some(ty) = scope.get(canonical.as_ref()) {
                return Some(*ty);
            }
        }
        None
    }
}

#[derive(Clone, Copy)]
struct MethodRewriteCtx {
    in_quote: bool,
}

impl FnClauseSpec {
    fn single(params_form: Form, body: Vec<Form>) -> Result<Self, VmError> {
        if body.is_empty() {
            return Err(VmError::unsupported(
                params_form.span,
                "fn arity expects body",
            ));
        }
        Ok(Self { params_form, body })
    }

    fn from_clause_form(form: &Form) -> Result<Self, VmError> {
        match &form.kind {
            FormKind::List(items) if !items.is_empty() => {
                if !matches!(items[0].kind, FormKind::Vector(_)) {
                    return Err(VmError::unsupported(
                        items[0].span,
                        "fn arity params must be vector",
                    ));
                }
                let body = if items.len() > 1 {
                    items[1..].to_vec()
                } else {
                    return Err(VmError::unsupported(form.span, "fn arity expects body"));
                };
                Ok(Self {
                    params_form: items[0].clone(),
                    body,
                })
            }
            _ => Err(VmError::unsupported(form.span, "fn arity must be list")),
        }
    }
}

#[derive(Debug)]
struct Compiler {
    chunk: Chunk,
    locals: Vec<Local>,
    fn_return_scope: Vec<HashMap<String, LocalType>>,
    scope_depth: usize,
    max_locals: usize,
    recur_stack: Vec<RecurTarget>,
    globals: HashMap<String, usize>,
    enclosing: Option<HashMap<String, usize>>,
    captures: Vec<Capture>,
    capture_map: HashMap<String, usize>,
    allow_def: bool,
}

impl Compiler {
    fn new(allow_def: bool) -> Self {
        Self {
            chunk: Chunk::new(),
            locals: Vec::new(),
            fn_return_scope: vec![HashMap::new()],
            scope_depth: 0,
            max_locals: 0,
            recur_stack: Vec::new(),
            globals: HashMap::new(),
            enclosing: None,
            captures: Vec::new(),
            capture_map: HashMap::new(),
            allow_def,
        }
    }

    fn new_with_enclosing(
        allow_def: bool,
        enclosing: HashMap<String, usize>,
        fn_return_scope: Vec<HashMap<String, LocalType>>,
    ) -> Self {
        let fn_return_scope = if fn_return_scope.is_empty() {
            vec![HashMap::new()]
        } else {
            fn_return_scope
        };
        Self {
            chunk: Chunk::new(),
            locals: Vec::new(),
            fn_return_scope,
            scope_depth: 0,
            max_locals: 0,
            recur_stack: Vec::new(),
            globals: HashMap::new(),
            enclosing: Some(enclosing),
            captures: Vec::new(),
            capture_map: HashMap::new(),
            allow_def,
        }
    }

    fn finish(self) -> CompiledChunk {
        CompiledChunk {
            chunk: self.chunk,
            local_count: self.max_locals,
        }
    }

    fn emit(&mut self, instr: Instruction) -> usize {
        self.chunk.push(instr)
    }

    fn emit_const(&mut self, value: Value, span: Span) {
        match value {
            Value::Int(n) => {
                self.emit(Instruction::ConstI64(n, span));
            }
            Value::Float(n) => {
                self.emit(Instruction::ConstF64(n, span));
            }
            Value::Bool(b) => {
                self.emit(Instruction::ConstBool(b, span));
            }
            Value::Nil => {
                self.emit(Instruction::ConstNil(span));
            }
            other => {
                let idx = self.chunk.add_const(other);
                self.emit(Instruction::Const(idx, span));
            }
        }
    }

    fn emit_return(&mut self, span: Span) {
        self.emit(Instruction::Return(span));
    }

    fn emit_jump_if_false_for(&mut self, cond: &Form, span: Span) -> usize {
        if self.infer_expr_type(cond) == LocalType::Bool {
            self.emit(Instruction::JumpIfFalseBool(0, span))
        } else {
            self.emit(Instruction::JumpIfFalse(0, span))
        }
    }

    fn emit_method_meta(&mut self, span: Span) {
        let mut meta = HashMap::new();
        meta.insert(Key::Keyword("is-method".into()), Value::Bool(true));
        let idx = self.chunk.add_const(Value::Map(meta.into()));
        self.emit(Instruction::AttachMeta(idx, span));
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.fn_return_scope.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        let depth = self.scope_depth;
        self.locals.retain(|local| local.depth < depth);
        self.scope_depth -= 1;
        self.fn_return_scope.pop();
    }

    fn add_local(&mut self, name: &str) -> usize {
        self.add_local_with_type(name, LocalType::Unknown)
    }

    fn add_local_with_type(&mut self, name: &str, ty: LocalType) -> usize {
        let canonical = canonical_symbol_name(name).into_owned();
        let index = self.locals.len();
        self.locals.push(Local {
            name: canonical,
            depth: self.scope_depth,
            index,
            ty,
        });
        if self.locals.len() > self.max_locals {
            self.max_locals = self.locals.len();
        }
        index
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        let canonical = canonical_symbol_name(name);
        self.locals
            .iter()
            .rev()
            .find(|local| local.name == canonical.as_ref())
            .map(|local| local.index)
    }

    fn local_type_for_symbol(&self, name: &str) -> LocalType {
        let canonical = canonical_symbol_name(name);
        self.locals
            .iter()
            .rev()
            .find(|local| local.name == canonical.as_ref())
            .map(|local| local.ty)
            .unwrap_or(LocalType::Unknown)
    }

    fn fn_return_type_for_symbol(&self, name: &str) -> Option<LocalType> {
        let canonical = canonical_symbol_name(name);
        for scope in self.fn_return_scope.iter().rev() {
            if let Some(ty) = scope.get(canonical.as_ref()) {
                return Some(*ty);
            }
        }
        None
    }

    fn insert_fn_return_type(&mut self, name: &str, ty: LocalType) {
        if ty == LocalType::Unknown {
            return;
        }
        let Some(scope) = self.fn_return_scope.last_mut() else {
            return;
        };
        scope.insert(canonical_symbol_name(name).into_owned(), ty);
    }

    fn update_local_type(&mut self, index: usize, incoming: LocalType) {
        let Some(local) = self.locals.get_mut(index) else {
            return;
        };
        local.ty = merge_local_type(local.ty, incoming);
    }

    fn type_hint_to_local_type(&self, form: &Form) -> Option<LocalType> {
        local_type_from_hint(form)
    }

    fn infer_expr_type(&self, form: &Form) -> LocalType {
        let mut scope = TypeScope::default();
        self.infer_expr_type_with_scope(form, &mut scope)
    }

    fn infer_expr_type_with_scope(&self, form: &Form, scope: &mut TypeScope) -> LocalType {
        if let Some(ty) = self.type_hint_to_local_type(form) {
            return ty;
        }
        match &form.kind {
            FormKind::Int(_) => LocalType::Int,
            FormKind::Float(_) => LocalType::Float,
            FormKind::Bool(_) => LocalType::Bool,
            FormKind::Symbol(sym) => scope
                .lookup(sym)
                .unwrap_or_else(|| self.local_type_for_symbol(sym)),
            FormKind::List(items) if !items.is_empty() => {
                let head = match &items[0].kind {
                    FormKind::Symbol(sym) => sym,
                    _ => return LocalType::Unknown,
                };
                let canonical = canonical_symbol_name(head);
                match canonical.as_ref() {
                    "if" if items.len() >= 3 => {
                        if items.len() < 4 {
                            return LocalType::Unknown;
                        }
                        let then_ty = self.infer_expr_type_with_scope(&items[2], scope);
                        let else_ty = self.infer_expr_type_with_scope(&items[3], scope);
                        if then_ty == else_ty {
                            return then_ty;
                        }
                        return LocalType::Unknown;
                    }
                    "do" => {
                        return self.infer_do_type(&items[1..], scope);
                    }
                    "let" | "loop" => {
                        return self.infer_let_loop_type(&items[1..], scope);
                    }
                    "and" | "or" => {
                        return self.infer_and_or_type(&items[1..], scope);
                    }
                    "cond" => {
                        return self.infer_cond_type(&items[1..], scope);
                    }
                    _ => {}
                }
                let Some(builtin_id) = builtin_id_for(canonical.as_ref()) else {
                    if let Some(ty) = scope
                        .lookup_fn_return(head)
                        .or_else(|| self.fn_return_type_for_symbol(head))
                    {
                        return ty;
                    }
                    return LocalType::Unknown;
                };
                let mut arg_types = Vec::with_capacity(items.len().saturating_sub(1));
                for arg in &items[1..] {
                    arg_types.push(self.infer_expr_type_with_scope(arg, scope));
                }
                let all_int =
                    !arg_types.is_empty() && arg_types.iter().all(|ty| *ty == LocalType::Int);
                let any_float = arg_types.iter().any(|ty| *ty == LocalType::Float);
                match builtin_id {
                    BuiltinId::Add
                    | BuiltinId::Sub
                    | BuiltinId::Mul
                    | BuiltinId::Inc
                    | BuiltinId::Dec => {
                        if all_int {
                            LocalType::Int
                        } else if any_float {
                            LocalType::Float
                        } else {
                            LocalType::Unknown
                        }
                    }
                    BuiltinId::Eq
                    | BuiltinId::Lt
                    | BuiltinId::Le
                    | BuiltinId::Gt
                    | BuiltinId::Ge => {
                        if all_int || any_float {
                            LocalType::Bool
                        } else {
                            LocalType::Unknown
                        }
                    }
                    BuiltinId::Div => {
                        if any_float || all_int {
                            LocalType::Float
                        } else {
                            LocalType::Unknown
                        }
                    }
                }
            }
            _ => LocalType::Unknown,
        }
    }

    fn infer_do_type(&self, forms: &[Form], scope: &mut TypeScope) -> LocalType {
        let mut last = None;
        for form in forms {
            if is_where_form(form) {
                continue;
            }
            last = Some(form);
        }
        let Some(form) = last else {
            return LocalType::Unknown;
        };
        self.infer_expr_type_with_scope(form, scope)
    }

    fn infer_let_loop_type(&self, forms: &[Form], scope: &mut TypeScope) -> LocalType {
        if forms.len() < 2 {
            return LocalType::Unknown;
        }
        let bindings_form = &forms[0];
        let body = &forms[1..];
        let bindings = match &bindings_form.kind {
            FormKind::Vector(items) => items.as_slice(),
            _ => return LocalType::Unknown,
        };
        if bindings.len() % 2 != 0 {
            return LocalType::Unknown;
        }
        scope.push();
        for pair in bindings.chunks(2) {
            let name_form = &pair[0];
            let value_form = &pair[1];
            let name = match &name_form.kind {
                FormKind::Symbol(name) => name.as_str(),
                _ => {
                    scope.pop();
                    return LocalType::Unknown;
                }
            };
            let local_type = self
                .type_hint_to_local_type(name_form)
                .unwrap_or_else(|| self.infer_expr_type_with_scope(value_form, scope));
            scope.insert(name, local_type);
            if let Some(fn_ty) = fn_return_type_from_fn_form(value_form) {
                scope.insert_fn_return(name, fn_ty);
            }
        }
        let ty = self.infer_do_type(body, scope);
        scope.pop();
        ty
    }

    fn infer_and_or_type(&self, forms: &[Form], scope: &mut TypeScope) -> LocalType {
        let mut out = None;
        for form in forms {
            let ty = self.infer_expr_type_with_scope(form, scope);
            if ty == LocalType::Unknown {
                return LocalType::Unknown;
            }
            match out {
                Some(prev) if prev != ty => return LocalType::Unknown,
                Some(_) => {}
                None => out = Some(ty),
            }
        }
        out.unwrap_or(LocalType::Unknown)
    }

    fn infer_cond_type(&self, forms: &[Form], scope: &mut TypeScope) -> LocalType {
        if forms.len() % 2 != 0 {
            return LocalType::Unknown;
        }
        let mut types = Vec::new();
        let mut idx = 0;
        let mut has_else = false;
        while idx + 1 < forms.len() {
            let test_form = &forms[idx];
            let expr_form = &forms[idx + 1];
            let expr_ty = self.infer_expr_type_with_scope(expr_form, scope);
            types.push(expr_ty);
            if is_cond_else(test_form) {
                has_else = true;
                break;
            }
            idx += 2;
        }
        if !has_else {
            return LocalType::Unknown;
        }
        let mut out = None;
        for ty in types {
            if ty == LocalType::Unknown {
                return LocalType::Unknown;
            }
            match out {
                Some(prev) if prev != ty => return LocalType::Unknown,
                Some(_) => {}
                None => out = Some(ty),
            }
        }
        out.unwrap_or(LocalType::Unknown)
    }

    fn binding_type(&self, name_form: &Form, value_form: &Form) -> LocalType {
        self.type_hint_to_local_type(name_form)
            .unwrap_or_else(|| self.infer_expr_type(value_form))
    }

    fn enclosing_locals_map(&self) -> HashMap<String, usize> {
        let mut map = HashMap::new();
        for local in &self.locals {
            map.insert(local.name.clone(), local.index);
        }
        map
    }

    fn resolve_enclosing(&self, name: &str) -> Option<usize> {
        let canonical = canonical_symbol_name(name);
        self.enclosing
            .as_ref()
            .and_then(|map| map.get(canonical.as_ref()).copied())
    }

    fn record_capture(&mut self, name: &str, index: usize) {
        let canonical = canonical_symbol_name(name).into_owned();
        if self.capture_map.contains_key(&canonical) {
            return;
        }
        let capture_index = self.captures.len();
        self.captures.push(Capture {
            index,
            name: canonical.clone(),
        });
        self.capture_map.insert(canonical, capture_index);
    }

    fn compile_form(&mut self, form: &Form) -> Result<(), VmError> {
        self.compile_form_with_context(form, TailContext::Tail)
    }

    fn compile_form_with_context(&mut self, form: &Form, tail: TailContext) -> Result<(), VmError> {
        if let Some(expanded) = self.expand_oop_form(form)? {
            return self.compile_form_with_context(&expanded, tail);
        }
        match &form.kind {
            FormKind::Int(n) => {
                self.emit_const(Value::Int(*n), form.span);
                Ok(())
            }
            FormKind::Float(n) => {
                self.emit_const(Value::Float(*n), form.span);
                Ok(())
            }
            FormKind::String(s) => {
                self.emit_const(Value::String(s.clone()), form.span);
                Ok(())
            }
            FormKind::Bool(b) => {
                self.emit_const(Value::Bool(*b), form.span);
                Ok(())
            }
            FormKind::Nil => {
                self.emit_const(Value::Nil, form.span);
                Ok(())
            }
            FormKind::Keyword(k) => {
                self.emit_const(Value::Symbol(format!(":{}", k)), form.span);
                Ok(())
            }
            FormKind::Symbol(name) => self.compile_symbol(name, form.span),
            FormKind::List(items) => self.compile_list(items, form.span, tail),
            FormKind::Vector(items) => self.compile_vector(items, form.span),
            FormKind::Map(entries) => self.compile_map(entries, form.span),
            FormKind::Set(items) => self.compile_set(items, form.span),
            _ => Err(VmError::unsupported(form.span, "unsupported form")),
        }
    }

    fn compile_symbol(&mut self, name: &str, span: Span) -> Result<(), VmError> {
        if is_question_placeholder(name) {
            return Err(VmError::unsupported(span, "placeholder symbol"));
        }
        if strip_spread_symbol(name).is_some() {
            return Err(VmError::unsupported(span, "spread symbol"));
        }
        if matches!(
            name,
            OOP_SYNTAX_SYM
                | OOP_BARE_SYM
                | OOP_DOT_STAGE_SYM
                | OOP_INDEX_SYM
                | OOP_NIL_SAFE_SYM
                | OOP_SEG_SYM
                | OOP_METHOD_SYM
        ) {
            return Err(VmError::unsupported(
                span,
                format!("oop syntax symbol: {}", name),
            ));
        }
        if let Some(idx) = self.resolve_local(name) {
            self.emit(Instruction::LoadLocal(idx, span));
            Ok(())
        } else {
            if let Some(enc_idx) = self.resolve_enclosing(name) {
                self.record_capture(name, enc_idx);
            }
            let global_id = self.global_id_for(name);
            self.emit(Instruction::LoadGlobalId(global_id, span));
            Ok(())
        }
    }

    fn compile_list(
        &mut self,
        items: &[Form],
        span: Span,
        tail: TailContext,
    ) -> Result<(), VmError> {
        if items.is_empty() {
            self.emit_const(Value::List(Vector::new()), span);
            return Ok(());
        }
        let head = &items[0];
        if let FormKind::Symbol(sym) = &head.kind {
            let canonical = canonical_symbol_name(sym);
            if canonical.as_ref() == APPLY_SYM {
                return self.compile_apply(&items[1..], span);
            }
            if canonical.as_ref() == "recur" {
                return self.compile_recur(&items[1..], head.span, tail);
            }
            match canonical.as_ref() {
                "do" => return self.compile_do(&items[1..], span, tail),
                "if" => return self.compile_if(&items[1..], span, tail),
                "let" => return self.compile_let(&items[1..], span, tail),
                "loop" => return self.compile_loop(&items[1..], span),
                "fn" => return self.compile_fn(&items[1..], span),
                "and" => return self.compile_and(&items[1..], span, tail),
                "or" => return self.compile_or(&items[1..], span, tail),
                "when" => return self.compile_when(&items[1..], span, tail),
                "cond" => return self.compile_cond(&items[1..], span, tail),
                "->" => return self.compile_thread(&items[1..], span, tail, ThreadStyle::First),
                "->>" => return self.compile_thread(&items[1..], span, tail, ThreadStyle::Last),
                "def" => return self.compile_def(&items[1..], span, false),
                "def-" => return self.compile_def(&items[1..], span, true),
                "defn" => return self.compile_defn(items, span, false),
                "defn-" => return self.compile_defn(items, span, true),
                "where" => return self.compile_where(&items[1..], span),
                "method" => return self.compile_method(&items[1..], span),
                OOP_INDEX_SYM => return self.compile_oop_index(&items[1..], span),
                OOP_METHOD_SYM => return self.compile_oop_method(&items[1..], span),
                "map" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::map",
                        "map expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "filter" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::filter",
                        "filter expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "remove" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::remove",
                        "remove expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "keep" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::keep",
                        "keep expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "some" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::some",
                        "some expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "every?" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::every?",
                        "every? expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "not-any?" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::not-any?",
                        "not-any? expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "not-every?" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::not-every?",
                        "not-every? expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "take-while" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::take-while",
                        "take-while expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "drop-while" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::drop-while",
                        "drop-while expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "split-with" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::split-with",
                        "split-with expects binding and predicate body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "partition-by" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::partition-by",
                        "partition-by expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "group-by" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::group-by",
                        "group-by expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "run!" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::run!",
                        "run! expects binding and body",
                        false,
                    )? {
                        return Ok(());
                    }
                }
                "sort-by" => {
                    if items.len() == 3
                        && self.compile_binding_sugar(
                            &items[1..],
                            span,
                            tail,
                            "core::sort-by",
                            "sort-by expects binding and body",
                            false,
                        )?
                    {
                        return Ok(());
                    }
                }
                "pmap" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::pmap",
                        "pmap expects binding and body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                "pfilter" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "core::pfilter",
                        "pfilter expects binding and predicate body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                "dag::pmap" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "dag::pmap",
                        "pmap expects binding and body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                "dag::pfilter" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "dag::pfilter",
                        "pfilter expects binding and predicate body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                "std::pmap" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "std::pmap",
                        "pmap expects binding and body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                "std::pfilter" => {
                    if self.compile_binding_sugar(
                        &items[1..],
                        span,
                        tail,
                        "std::pfilter",
                        "pfilter expects binding and predicate body",
                        true,
                    )? {
                        return Ok(());
                    }
                }
                _ => {}
            }
            if is_unsupported_special_form_head(canonical.as_ref()) {
                return Err(VmError::unsupported(
                    head.span,
                    format!("special form: {}", canonical.as_ref()),
                ));
            }
        }
        if let Some(span) = call_label_key_span(items) {
            return Err(VmError::unsupported(span, "label-style key"));
        }
        self.compile_call(items, span)
    }

    fn compile_do(&mut self, body: &[Form], span: Span, tail: TailContext) -> Result<(), VmError> {
        let err_fin = match parse_err_fin_tail(body, "a body") {
            Ok(tail) => tail,
            Err(err) => return Err(VmError::unsupported(err.span, err.message)),
        };
        if let Some(tail_forms) = err_fin {
            if tail_forms.body.is_empty() {
                return Err(VmError::unsupported(span, "do expects body before err/fin"));
            }
            let try_span = body.first().map(|f| f.span).unwrap_or(span);
            let mut items = Vec::with_capacity(1 + tail_forms.body.len());
            items.push(Form::new(FormKind::Symbol("try".into()), try_span));
            items.extend(tail_forms.body.into_iter());
            if let Some(err_form) = tail_forms.err {
                items.push(err_form);
            }
            if let Some(fin_form) = tail_forms.fin {
                items.push(fin_form);
            }
            let try_form = Form::new(FormKind::List(items), try_span);
            return self.compile_form_with_context(&try_form, tail);
        }
        if body.is_empty() {
            self.emit_const(Value::Nil, span);
            return Ok(());
        }
        let mut last_non_where = None;
        for (idx, form) in body.iter().enumerate() {
            if !is_where_form(form) {
                last_non_where = Some(idx);
            }
        }
        let Some(last_idx) = last_non_where else {
            self.emit_const(Value::Nil, span);
            return Ok(());
        };
        for (idx, form) in body.iter().enumerate() {
            if is_where_form(form) {
                self.compile_form_with_context(form, TailContext::Value)?;
                self.emit(Instruction::Pop(form.span));
                continue;
            }
            let is_last = idx == last_idx;
            let context = if is_last { tail } else { TailContext::Value };
            self.compile_form_with_context(form, context)?;
            if !is_last {
                self.emit(Instruction::Pop(form.span));
            }
        }
        Ok(())
    }

    fn compile_if(&mut self, args: &[Form], span: Span, tail: TailContext) -> Result<(), VmError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(VmError::unsupported(span, "if arity"));
        }
        self.compile_form_with_context(&args[0], TailContext::Value)?;
        let jump_if_false = self.emit_jump_if_false_for(&args[0], args[0].span);
        self.compile_form_with_context(&args[1], tail)?;
        let jump_to_end = self.emit(Instruction::Jump(0, span));
        let else_start = self.chunk.code().len();
        let _ = self.chunk.patch_jump(jump_if_false, else_start);
        if args.len() == 3 {
            self.compile_form_with_context(&args[2], tail)?;
        } else {
            self.emit_const(Value::Nil, span);
        }
        let end = self.chunk.code().len();
        let _ = self.chunk.patch_jump(jump_to_end, end);
        Ok(())
    }

    fn compile_and(&mut self, args: &[Form], span: Span, tail: TailContext) -> Result<(), VmError> {
        if args.is_empty() {
            self.emit_const(Value::Bool(true), span);
            return Ok(());
        }
        let arg_tail = if tail == TailContext::Tail {
            TailContext::Tail
        } else {
            TailContext::Value
        };
        let mut false_jumps = Vec::new();
        for (idx, arg) in args.iter().enumerate() {
            let is_last = idx + 1 == args.len();
            if is_last {
                self.compile_form_with_context(arg, arg_tail)?;
                break;
            }
            self.compile_form_with_context(arg, arg_tail)?;
            self.emit(Instruction::Dup(arg.span));
            let jump_if_false = self.emit(Instruction::JumpIfFalse(0, arg.span));
            false_jumps.push(jump_if_false);
            self.emit(Instruction::Pop(arg.span));
        }
        let end = self.chunk.code().len();
        for jump in false_jumps {
            let _ = self.chunk.patch_jump(jump, end);
        }
        Ok(())
    }

    fn compile_or(&mut self, args: &[Form], span: Span, tail: TailContext) -> Result<(), VmError> {
        if args.is_empty() {
            self.emit_const(Value::Nil, span);
            return Ok(());
        }
        let arg_tail = if tail == TailContext::Tail {
            TailContext::Tail
        } else {
            TailContext::Value
        };
        let mut end_jumps = Vec::new();
        for (idx, arg) in args.iter().enumerate() {
            let is_last = idx + 1 == args.len();
            if is_last {
                self.compile_form_with_context(arg, arg_tail)?;
                break;
            }
            self.compile_form_with_context(arg, arg_tail)?;
            self.emit(Instruction::Dup(arg.span));
            let jump_if_false = self.emit(Instruction::JumpIfFalse(0, arg.span));
            let jump_to_end = self.emit(Instruction::Jump(0, span));
            end_jumps.push(jump_to_end);
            let next = self.chunk.code().len();
            let _ = self.chunk.patch_jump(jump_if_false, next);
            self.emit(Instruction::Pop(arg.span));
        }
        let end = self.chunk.code().len();
        for jump in end_jumps {
            let _ = self.chunk.patch_jump(jump, end);
        }
        Ok(())
    }

    fn compile_when(
        &mut self,
        args: &[Form],
        span: Span,
        tail: TailContext,
    ) -> Result<(), VmError> {
        if args.is_empty() {
            return Err(VmError::unsupported(span, "when expects test and body"));
        }
        self.compile_form_with_context(&args[0], TailContext::Value)?;
        let jump_if_false = self.emit_jump_if_false_for(&args[0], args[0].span);
        self.compile_do(&args[1..], span, tail)?;
        let jump_to_end = self.emit(Instruction::Jump(0, span));
        let else_start = self.chunk.code().len();
        let _ = self.chunk.patch_jump(jump_if_false, else_start);
        self.emit_const(Value::Nil, span);
        let end = self.chunk.code().len();
        let _ = self.chunk.patch_jump(jump_to_end, end);
        Ok(())
    }

    fn compile_cond(
        &mut self,
        args: &[Form],
        span: Span,
        tail: TailContext,
    ) -> Result<(), VmError> {
        if args.len() % 2 != 0 {
            return Err(VmError::unsupported(
                span,
                "cond expects even number of test/expr pairs",
            ));
        }
        let mut end_jumps = Vec::new();
        let mut idx = 0;
        while idx < args.len() {
            let test_form = &args[idx];
            let expr_form = &args[idx + 1];
            if is_cond_else(test_form) {
                self.compile_form_with_context(expr_form, tail)?;
                let end = self.chunk.code().len();
                for jump in end_jumps {
                    let _ = self.chunk.patch_jump(jump, end);
                }
                return Ok(());
            }
            self.compile_form_with_context(test_form, TailContext::Value)?;
            let jump_if_false = self.emit_jump_if_false_for(test_form, test_form.span);
            self.compile_form_with_context(expr_form, tail)?;
            let jump_to_end = self.emit(Instruction::Jump(0, span));
            end_jumps.push(jump_to_end);
            let next = self.chunk.code().len();
            let _ = self.chunk.patch_jump(jump_if_false, next);
            idx += 2;
        }
        self.emit_const(Value::Nil, span);
        let end = self.chunk.code().len();
        for jump in end_jumps {
            let _ = self.chunk.patch_jump(jump, end);
        }
        Ok(())
    }

    fn compile_thread(
        &mut self,
        args: &[Form],
        span: Span,
        tail: TailContext,
        style: ThreadStyle,
    ) -> Result<(), VmError> {
        if args.is_empty() {
            return Err(VmError::unsupported(
                span,
                "threading form expects initial value",
            ));
        }
        let mut acc = args[0].clone();
        for step in &args[1..] {
            let new_form = match &step.kind {
                FormKind::List(items) => {
                    if items.is_empty() {
                        acc.clone()
                    } else {
                        let mut rewritten = Vec::with_capacity(items.len().saturating_add(1));
                        let mut arg_start = 1;
                        if matches!(&items[0].kind, FormKind::Symbol(sym) if sym == APPLY_SYM)
                            && items.len() >= 2
                        {
                            rewritten.push(items[0].clone());
                            rewritten.push(items[1].clone());
                            arg_start = 2;
                        } else {
                            rewritten.push(items[0].clone());
                        }
                        match style {
                            ThreadStyle::First => {
                                rewritten.push(acc.clone());
                                rewritten.extend_from_slice(&items[arg_start..]);
                            }
                            ThreadStyle::Last => {
                                rewritten.extend_from_slice(&items[arg_start..]);
                                rewritten.push(acc.clone());
                            }
                        }
                        Form {
                            kind: FormKind::List(rewritten),
                            span: step.span,
                            type_hint: step.type_hint.clone(),
                        }
                    }
                }
                _ => Form {
                    kind: FormKind::List(vec![step.clone(), acc.clone()]),
                    span: step.span,
                    type_hint: step.type_hint.clone(),
                },
            };
            acc = new_form;
        }
        self.compile_form_with_context(&acc, tail)
    }

    fn compile_oop_index(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.len() != 2 {
            return Err(VmError::unsupported(
                span,
                "oop-index expects target and key",
            ));
        }
        self.compile_form_with_context(&args[0], TailContext::Value)?;
        self.compile_form_with_context(&args[1], TailContext::Value)?;
        self.emit(Instruction::OopIndex(span));
        Ok(())
    }

    fn compile_oop_method(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.len() < 2 {
            return Err(VmError::unsupported(
                span,
                "oop-method expects target and name",
            ));
        }
        self.compile_form_with_context(&args[0], TailContext::Value)?;
        self.compile_form_with_context(&args[1], TailContext::Value)?;
        for arg in &args[2..] {
            self.compile_form_with_context(arg, TailContext::Value)?;
        }
        self.emit(Instruction::OopMethod(args.len().saturating_sub(2), span));
        Ok(())
    }

    fn compile_binding_sugar(
        &mut self,
        args: &[Form],
        span: Span,
        tail: TailContext,
        core_name: &str,
        error_label: &str,
        allow_opts: bool,
    ) -> Result<bool, VmError> {
        if args.is_empty() {
            return Ok(false);
        }
        let mut binding_idx = 0;
        let mut opts_form: Option<&Form> = None;
        if allow_opts
            && args.len() >= 2
            && matches!(args[0].kind, FormKind::Map(_))
            && matches!(args[1].kind, FormKind::Vector(_))
        {
            opts_form = Some(&args[0]);
            binding_idx = 1;
        }
        let binding_form = &args[binding_idx];
        if !matches!(binding_form.kind, FormKind::Vector(_)) {
            return Ok(false);
        }
        if args.len() <= binding_idx + 1 {
            return Err(VmError::unsupported(span, error_label));
        }
        let mut body_idx = binding_idx + 1;
        if allow_opts && args.len() > body_idx && matches!(args[body_idx].kind, FormKind::Map(_)) {
            if opts_form.is_some() {
                return Err(VmError::unsupported(
                    span,
                    "pmap/pfilter accepts a single opts map",
                ));
            }
            opts_form = Some(&args[body_idx]);
            body_idx += 1;
        }
        if args.len() <= body_idx {
            return Err(VmError::unsupported(span, error_label));
        }
        let (pattern, coll_form) = parse_binding_pair_form(binding_form)?;
        let fn_body_span = binding_form.span;
        let it_name = format!(
            "__it{}",
            VM_PLACEHOLDER_ALLOC_COUNTER.fetch_add(1, Ordering::SeqCst)
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
        let mut call_items = vec![
            Form::new(FormKind::Symbol(core_name.to_string()), span),
            fn_form,
            coll_form,
        ];
        if let Some(opts_form) = opts_form {
            call_items.push(opts_form.clone());
        }
        let call_form = Form::new(FormKind::List(call_items), span);
        self.compile_form_with_context(&call_form, tail)?;
        Ok(true)
    }

    fn compile_let(&mut self, args: &[Form], span: Span, tail: TailContext) -> Result<(), VmError> {
        if args.len() < 2 {
            return Err(VmError::unsupported(span, "let body"));
        }
        let bindings_form = &args[0];
        let body = &args[1..];
        if body.is_empty() {
            return Err(VmError::unsupported(span, "let body"));
        }
        let bindings = match &bindings_form.kind {
            FormKind::Vector(items) => items.as_slice(),
            _ => return Err(VmError::unsupported(bindings_form.span, "let bindings")),
        };
        if bindings.len() % 2 != 0 {
            return Err(VmError::unsupported(bindings_form.span, "let bindings"));
        }
        self.begin_scope();
        for pair in bindings.chunks(2) {
            let name_form = &pair[0];
            let value_form = &pair[1];
            let name = match &name_form.kind {
                FormKind::Symbol(name) => name,
                _ => {
                    self.end_scope();
                    return Err(VmError::unsupported(name_form.span, "let binding"));
                }
            };
            let local_type = self.binding_type(name_form, value_form);
            let fn_return_type = fn_return_type_from_fn_form(value_form);
            self.compile_form_with_context(value_form, TailContext::Value)?;
            let local = self.add_local_with_type(name, local_type);
            if let Some(fn_ty) = fn_return_type {
                self.insert_fn_return_type(name, fn_ty);
            }
            self.emit(Instruction::StoreLocal(local, name_form.span));
        }
        let result = self.compile_do(body, span, tail);
        self.end_scope();
        result
    }

    fn compile_loop(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.len() < 2 {
            return Err(VmError::unsupported(span, "loop body"));
        }
        let bindings_form = &args[0];
        let body = &args[1..];
        if body.is_empty() {
            return Err(VmError::unsupported(span, "loop body"));
        }
        let bindings = match &bindings_form.kind {
            FormKind::Vector(items) => items.as_slice(),
            _ => return Err(VmError::unsupported(bindings_form.span, "loop bindings")),
        };
        if bindings.len() % 2 != 0 {
            return Err(VmError::unsupported(bindings_form.span, "loop bindings"));
        }
        self.begin_scope();
        let mut locals = Vec::new();
        for pair in bindings.chunks(2) {
            let name_form = &pair[0];
            let value_form = &pair[1];
            let name = match &name_form.kind {
                FormKind::Symbol(name) => name,
                _ => {
                    self.end_scope();
                    return Err(VmError::unsupported(name_form.span, "loop binding"));
                }
            };
            let local_type = self.binding_type(name_form, value_form);
            let fn_return_type = fn_return_type_from_fn_form(value_form);
            self.compile_form_with_context(value_form, TailContext::Value)?;
            let local = self.add_local_with_type(name, local_type);
            locals.push(local);
            if let Some(fn_ty) = fn_return_type {
                self.insert_fn_return_type(name, fn_ty);
            }
            self.emit(Instruction::StoreLocal(local, name_form.span));
        }
        let loop_start = self.chunk.code().len();
        let base_count = self.locals.len();
        self.recur_stack.push(RecurTarget::Loop {
            start_ip: loop_start,
            locals,
            base_count,
        });
        let result = self.compile_do(body, span, TailContext::Tail);
        self.recur_stack.pop();
        self.end_scope();
        result
    }

    fn compile_fn(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.is_empty() {
            return Err(VmError::unsupported(span, "fn args"));
        }
        let mut idx = 0;
        let mut fn_name = None;
        if let FormKind::Symbol(sym) = &args[0].kind {
            if args.len() >= 2
                && (matches!(args[1].kind, FormKind::Vector(_))
                    || matches!(args[1].kind, FormKind::List(_)))
            {
                fn_name = Some(canonical_symbol_name(sym).into_owned());
                idx = 1;
            }
        }
        let clauses = parse_fn_clauses(&args[idx..], span)?;
        if clauses.is_empty() {
            return Err(VmError::unsupported(span, "fn arity"));
        }
        let self_bound = self.resolve_local("self").is_some();
        let (method_clauses, is_method) = if self_bound {
            let (rewritten, _) = rewrite_method_clauses(&clauses);
            (rewritten, false)
        } else {
            methodify_clauses(&clauses, false)
        };
        self.compile_fn_clauses(fn_name.clone(), &method_clauses, span)?;
        if is_method {
            self.emit_method_meta(span);
        }
        Ok(())
    }

    fn compile_fn_clauses(
        &mut self,
        fn_name: Option<String>,
        clauses: &[FnClauseSpec],
        span: Span,
    ) -> Result<(), VmError> {
        let mut func_indices = Vec::with_capacity(clauses.len());
        let mut captures = Vec::new();
        let mut capture_map = HashMap::new();
        for clause in clauses {
            let (func_idx, clause_captures) = self.compile_fn_clause(fn_name.clone(), clause)?;
            func_indices.push(func_idx);
            for capture in clause_captures {
                if capture_map.contains_key(&capture.name) {
                    continue;
                }
                capture_map.insert(capture.name.clone(), captures.len());
                captures.push(capture);
            }
        }
        if func_indices.len() == 1 {
            let func_idx = func_indices[0];
            self.emit(Instruction::MakeClosure(func_idx, captures, span));
        } else {
            self.emit(Instruction::MakeMultiClosure(
                func_indices,
                captures,
                fn_name,
                span,
            ));
        }
        Ok(())
    }

    fn compile_fn_clause(
        &mut self,
        fn_name: Option<String>,
        clause: &FnClauseSpec,
    ) -> Result<(usize, Vec<Capture>), VmError> {
        let parsed = parse_params(&clause.params_form)?;
        let mut compiler = Compiler::new_with_enclosing(
            false,
            self.enclosing_locals_map(),
            self.fn_return_scope.clone(),
        );
        for param in &parsed.params {
            compiler.add_local_with_type(&param.name, param.ty);
        }
        if let Some(rest) = &parsed.rest {
            compiler.add_local(rest);
        }
        let (local_defns, body_forms) = collect_local_defns_with(&clause.body, false)?;
        for defn in &local_defns {
            compiler.compile_local_defn(defn)?;
        }
        let start_ip = compiler.chunk.code().len();
        let base_count = compiler.locals.len();
        compiler.recur_stack.push(RecurTarget::Function {
            start_ip,
            param_count: parsed.params.len(),
            has_rest: parsed.rest.is_some(),
            base_count,
        });
        compiler.compile_do(&body_forms, clause.params_form.span, TailContext::Tail)?;
        compiler.recur_stack.pop();
        compiler.emit_return(clause.params_form.span);
        let proto = FunctionPrototype {
            chunk: compiler.chunk,
            param_count: parsed.params.len(),
            has_rest: parsed.rest.is_some(),
            name: fn_name,
            needs_env: !local_defns.is_empty(),
        };
        let func_idx = self.chunk.add_function(proto);
        Ok((func_idx, compiler.captures))
    }

    fn compile_recur(
        &mut self,
        args: &[Form],
        span: Span,
        tail: TailContext,
    ) -> Result<(), VmError> {
        if tail != TailContext::Tail {
            return Err(VmError::unsupported(span, "recur in non-tail position"));
        }
        let target = match self.recur_stack.last() {
            Some(target) => target.clone(),
            None => return Err(VmError::unsupported(span, "recur outside")),
        };
        match target {
            RecurTarget::Loop {
                start_ip,
                locals,
                base_count,
            } => {
                if args.len() != locals.len() {
                    return Err(VmError::unsupported(span, "recur arity"));
                }
                let arg_types: Vec<LocalType> =
                    args.iter().map(|arg| self.infer_expr_type(arg)).collect();
                for arg in args {
                    self.compile_form_with_context(arg, TailContext::Value)?;
                }
                for &local in locals.iter().rev() {
                    self.emit(Instruction::StoreLocal(local, span));
                }
                for (local, ty) in locals.iter().zip(arg_types.iter()) {
                    self.update_local_type(*local, *ty);
                }
                self.emit(Instruction::TruncateLocals(base_count, span));
                self.emit(Instruction::Jump(start_ip, span));
                Ok(())
            }
            RecurTarget::Function {
                start_ip,
                param_count,
                has_rest,
                base_count,
            } => {
                if has_rest {
                    return Err(VmError::unsupported(span, "recur with rest"));
                }
                if args.len() != param_count {
                    return Err(VmError::unsupported(span, "recur arity"));
                }
                let arg_types: Vec<LocalType> =
                    args.iter().map(|arg| self.infer_expr_type(arg)).collect();
                for arg in args {
                    self.compile_form_with_context(arg, TailContext::Value)?;
                }
                for local in (0..param_count).rev() {
                    self.emit(Instruction::StoreLocal(local, span));
                }
                for (idx, ty) in arg_types.iter().enumerate() {
                    self.update_local_type(idx, *ty);
                }
                self.emit(Instruction::TruncateLocals(base_count, span));
                self.emit(Instruction::Jump(start_ip, span));
                Ok(())
            }
        }
    }

    fn compile_call(&mut self, items: &[Form], span: Span) -> Result<(), VmError> {
        for arg in &items[1..] {
            if let FormKind::Symbol(sym) = &arg.kind {
                if sym == "*" || strip_spread_symbol(sym).is_some() {
                    return Err(VmError::unsupported(arg.span, "spread arg"));
                }
            }
        }
        if let FormKind::Symbol(sym) = &items[0].kind {
            if self.resolve_local(sym).is_none() {
                let canonical = canonical_symbol_name(sym);
                if let Some(builtin_id) = builtin_id_for(canonical.as_ref()) {
                    if self.try_emit_typed_builtin(
                        builtin_id,
                        canonical.as_ref(),
                        &items[1..],
                        span,
                    )? {
                        return Ok(());
                    }
                    for arg in &items[1..] {
                        self.compile_form_with_context(arg, TailContext::Value)?;
                    }
                    let global_id = self.global_id_for(canonical.as_ref());
                    self.emit(Instruction::CallBuiltin(
                        builtin_id,
                        items.len().saturating_sub(1),
                        global_id,
                        span,
                    ));
                    return Ok(());
                }
            }
        }
        self.compile_form_with_context(&items[0], TailContext::Value)?;
        for arg in &items[1..] {
            self.compile_form_with_context(arg, TailContext::Value)?;
        }
        let argc = items.len().saturating_sub(1);
        let instr = match argc {
            0 => Instruction::Call0(span),
            1 => Instruction::Call1(span),
            2 => Instruction::Call2(span),
            3 => Instruction::Call3(span),
            4 => Instruction::Call4(span),
            _ => Instruction::Call(argc, span),
        };
        self.emit(instr);
        Ok(())
    }

    fn try_emit_typed_builtin(
        &mut self,
        builtin_id: BuiltinId,
        name: &str,
        args: &[Form],
        span: Span,
    ) -> Result<bool, VmError> {
        let expected = match builtin_id {
            BuiltinId::Add
            | BuiltinId::Sub
            | BuiltinId::Mul
            | BuiltinId::Eq
            | BuiltinId::Lt
            | BuiltinId::Le
            | BuiltinId::Gt
            | BuiltinId::Ge
            | BuiltinId::Div => 2,
            BuiltinId::Inc | BuiltinId::Dec => 1,
        };
        if args.len() != expected {
            return Ok(false);
        }
        let arg_types: Vec<LocalType> = args.iter().map(|arg| self.infer_expr_type(arg)).collect();
        if arg_types.iter().any(|ty| *ty == LocalType::Unknown) {
            return Ok(false);
        }
        let all_int = arg_types.iter().all(|ty| *ty == LocalType::Int);
        let all_float = arg_types.iter().all(|ty| *ty == LocalType::Float);
        let all_numeric = arg_types
            .iter()
            .all(|ty| matches!(ty, LocalType::Int | LocalType::Float));
        let any_float = arg_types.iter().any(|ty| *ty == LocalType::Float);
        for arg in args {
            self.compile_form_with_context(arg, TailContext::Value)?;
        }
        let global_id = self.global_id_for(name);
        let instr = match builtin_id {
            BuiltinId::Add => {
                if all_int {
                    Instruction::AddI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::AddF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Sub => {
                if all_int {
                    Instruction::SubI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::SubF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Mul => {
                if all_int {
                    Instruction::MulI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::MulF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Div => {
                if all_float {
                    Instruction::DivF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Eq => {
                if all_int {
                    Instruction::EqI64(global_id, span)
                } else if all_float {
                    Instruction::EqF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Lt => {
                if all_int {
                    Instruction::LtI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::LtF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Le => {
                if all_int {
                    Instruction::LeI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::LeF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Gt => {
                if all_int {
                    Instruction::GtI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::GtF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Ge => {
                if all_int {
                    Instruction::GeI64(global_id, span)
                } else if all_numeric && any_float {
                    Instruction::GeF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Inc => {
                if all_int {
                    Instruction::IncI64(global_id, span)
                } else if all_float {
                    Instruction::IncF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
            BuiltinId::Dec => {
                if all_int {
                    Instruction::DecI64(global_id, span)
                } else if all_float {
                    Instruction::DecF64(global_id, span)
                } else {
                    return Ok(false);
                }
            }
        };
        self.emit(instr);
        Ok(true)
    }

    fn compile_def(&mut self, args: &[Form], span: Span, is_private: bool) -> Result<(), VmError> {
        if !self.allow_def {
            return Err(VmError::unsupported(span, "def in non-top-level"));
        }
        if args.len() != 2 {
            return Err(VmError::unsupported(span, "def arity"));
        }
        let name_form = &args[0];
        let name = match &name_form.kind {
            FormKind::Symbol(name) => name,
            _ => return Err(VmError::unsupported(name_form.span, "def name")),
        };
        if let Some(fn_ty) = fn_return_type_from_fn_form(&args[1]) {
            self.insert_fn_return_type(name, fn_ty);
        }
        self.compile_form_with_context(&args[1], TailContext::Value)?;
        let global_id = self.global_id_for(name);
        self.emit(Instruction::DefGlobalId(global_id, is_private, span));
        Ok(())
    }

    fn compile_defn(&mut self, args: &[Form], span: Span, is_private: bool) -> Result<(), VmError> {
        if !self.allow_def {
            return Err(VmError::unsupported(span, "defn in non-top-level"));
        }
        if args.len() < 3 {
            return Err(VmError::unsupported(span, "defn arity"));
        }
        let name_form = &args[1];
        let name = match &name_form.kind {
            FormKind::Symbol(name) => name,
            _ => return Err(VmError::unsupported(name_form.span, "defn name")),
        };
        let signature = parse_defn_signature(&args[2..], span)?;
        let (method_clauses, is_method) = methodify_clauses(&signature.clauses, false);
        let clauses = if is_method {
            method_clauses
        } else {
            signature.clauses
        };
        if let Some(ret_ty) = fn_return_type_from_clauses(&clauses) {
            self.insert_fn_return_type(name, ret_ty);
        }
        let fn_name = Some(canonical_symbol_name(name).into_owned());
        self.compile_fn_clauses(fn_name, &clauses, span)?;
        let defn_form = Form::new(FormKind::List(args.to_vec()), span);
        let global_id = self.global_id_for(name);
        self.emit(Instruction::DefnGlobalId(
            global_id, is_private, defn_form, span,
        ));
        Ok(())
    }

    fn compile_where(&mut self, forms: &[Form], span: Span) -> Result<(), VmError> {
        if forms.is_empty() {
            self.emit_const(Value::Nil, span);
            return Ok(());
        }
        self.compile_do(forms, span, TailContext::Value)?;
        self.emit(Instruction::Pop(span));
        self.emit_const(Value::Nil, span);
        Ok(())
    }

    fn compile_method(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.is_empty() {
            return Err(VmError::unsupported(span, "method args"));
        }
        let mut idx = 0;
        let mut fn_name = None;
        if let FormKind::Symbol(sym) = &args[0].kind {
            if args.len() >= 2
                && (matches!(args[1].kind, FormKind::Vector(_))
                    || matches!(args[1].kind, FormKind::List(_)))
            {
                fn_name = Some(canonical_symbol_name(sym).into_owned());
                idx = 1;
            }
        }
        let clauses = parse_fn_clauses(&args[idx..], span)?;
        if clauses.is_empty() {
            return Err(VmError::unsupported(span, "method arity"));
        }
        ensure_method_params_without_self(&clauses)?;
        let (method_clauses, _) = methodify_clauses(&clauses, true);
        if let Some(name) = fn_name.as_deref() {
            if let Some(ret_ty) = fn_return_type_from_clauses(&method_clauses) {
                self.insert_fn_return_type(name, ret_ty);
            }
        }
        self.compile_fn_clauses(fn_name, &method_clauses, span)?;
        self.emit_method_meta(span);
        Ok(())
    }

    fn compile_local_defn(&mut self, defn: &LocalDefnSpec) -> Result<(), VmError> {
        let items = match &defn.form.kind {
            FormKind::List(items) => items,
            _ => return Err(VmError::unsupported(defn.form.span, "local defn")),
        };
        if items.len() < 3 {
            return Err(VmError::unsupported(defn.form.span, "local defn"));
        }
        let head = match &items[0].kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return Err(VmError::unsupported(items[0].span, "local defn")),
        };
        if head != "-defn" && head != "defn" {
            return Err(VmError::unsupported(defn.form.span, "local defn"));
        }
        let name = defn.name.clone();
        let signature = parse_defn_signature(&items[2..], defn.form.span)?;
        let (method_clauses, is_method) = methodify_clauses(&signature.clauses, false);
        let clauses = if is_method {
            method_clauses
        } else {
            signature.clauses
        };
        if let Some(ret_ty) = fn_return_type_from_clauses(&clauses) {
            self.insert_fn_return_type(&name, ret_ty);
        }
        self.compile_fn_clauses(Some(name.clone()), &clauses, defn.form.span)?;

        let mut meta_value: Option<CowHashMap<Key, Value>> = None;
        if let Some(meta_form) = &signature.meta_map {
            let value = form_to_value(meta_form)
                .map_err(|_| VmError::unsupported(meta_form.span, "defn attr-map"))?;
            let Value::Map(map) = value else {
                return Err(VmError::unsupported(meta_form.span, "defn attr-map"));
            };
            meta_value = Some(map);
        }
        if is_method {
            let mut map = meta_value.unwrap_or_else(CowHashMap::new);
            map.insert(Key::Keyword("is-method".into()), Value::Bool(true));
            meta_value = Some(map);
        }
        if let Some(meta_map) = meta_value {
            let idx = self.chunk.add_const(Value::Map(meta_map));
            self.emit(Instruction::AttachMeta(idx, defn.form.span));
        }
        if let Some(doc) = signature.doc {
            let idx = self.chunk.add_const(Value::String(doc));
            self.emit(Instruction::AttachDoc(idx, defn.form.span));
        }
        let global_id = self.global_id_for(&name);
        self.emit(Instruction::DefLocalId(global_id, defn.form.span));
        Ok(())
    }

    fn compile_vector(&mut self, items: &[Form], span: Span) -> Result<(), VmError> {
        if let Some(spread_span) = spread_span(items) {
            return Err(VmError::unsupported(spread_span, "vector spread"));
        }
        for item in items {
            self.compile_form_with_context(item, TailContext::Value)?;
        }
        self.emit(Instruction::MakeVector(items.len(), span));
        Ok(())
    }

    fn compile_set(&mut self, items: &[Form], span: Span) -> Result<(), VmError> {
        if let Some(spread_span) = spread_span(items) {
            return Err(VmError::unsupported(spread_span, "set spread"));
        }
        for item in items {
            self.compile_form_with_context(item, TailContext::Value)?;
        }
        self.emit(Instruction::MakeSet(items.len(), span));
        Ok(())
    }

    fn compile_map(&mut self, entries: &[MapItem], span: Span) -> Result<(), VmError> {
        if form_contains_map_ref_entries(entries) {
            return Err(VmError::unsupported(span, "map refs"));
        }
        let mut keys = Vec::with_capacity(entries.len());
        for entry in entries {
            match entry {
                MapItem::KeyValue(key_form, value_form) => {
                    let key = map_key_from_form(key_form)?;
                    keys.push(key);
                    self.compile_form_with_context(value_form, TailContext::Value)?;
                }
                MapItem::Spread(form) => {
                    return Err(VmError::unsupported(form.span, "map spread"));
                }
            }
        }
        self.emit(Instruction::MakeMap(keys, span));
        Ok(())
    }

    fn compile_apply(&mut self, args: &[Form], span: Span) -> Result<(), VmError> {
        if args.is_empty() {
            return Err(VmError::unsupported(span, "apply arity"));
        }
        for (idx, arg) in args.iter().enumerate() {
            if idx == 0 {
                continue;
            }
            if let FormKind::Symbol(sym) = &arg.kind {
                if sym == "*" || strip_spread_symbol(sym).is_some() {
                    return Err(VmError::unsupported(arg.span, "spread arg"));
                }
            }
        }
        for arg in &args[1..] {
            if let FormKind::Symbol(sym) = &arg.kind {
                if is_label_style_key(sym) {
                    return Err(VmError::unsupported(arg.span, "label-style key"));
                }
            }
        }
        self.compile_form_with_context(&args[0], TailContext::Value)?;
        for arg in &args[1..] {
            self.compile_form_with_context(arg, TailContext::Value)?;
        }
        self.emit(Instruction::Apply(args.len().saturating_sub(1), span));
        Ok(())
    }

    fn global_id_for(&mut self, name: &str) -> usize {
        let canonical = canonical_symbol_name(name).into_owned();
        if let Some(&id) = self.globals.get(&canonical) {
            return id;
        }
        let id = self.chunk.add_global(canonical.clone());
        self.globals.insert(canonical, id);
        id
    }

    fn expand_oop_form(&self, form: &Form) -> Result<Option<Form>, VmError> {
        if let Some(expanded) = self.try_expand_oop_apply_form(form)? {
            return Ok(Some(expanded));
        }
        if let Some(expanded) = self.try_expand_oop_head_form(form)? {
            return Ok(Some(expanded));
        }
        if let Some(expanded) = self.try_expand_oop_form(form)? {
            return Ok(Some(expanded));
        }
        Ok(None)
    }

    fn try_expand_oop_apply_form(&self, form: &Form) -> Result<Option<Form>, VmError> {
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
        let Some(expanded) = self.try_expand_oop_form(target)? else {
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

    fn try_expand_oop_head_form(&self, form: &Form) -> Result<Option<Form>, VmError> {
        let FormKind::List(items) = &form.kind else {
            return Ok(None);
        };
        if items.is_empty() {
            return Ok(None);
        }
        let head = &items[0];
        let args = &items[1..];
        let Some(expanded) = self.try_expand_oop_form(head)? else {
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

    fn try_expand_oop_form(&self, form: &Form) -> Result<Option<Form>, VmError> {
        let FormKind::List(items) = &form.kind else {
            return Ok(None);
        };
        if items.is_empty() {
            return Ok(None);
        }
        let (base_start, stage_start, had_wrapper) = match &items[0].kind {
            FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM => (1, 2, false),
            FormKind::Symbol(sym)
                if sym == APPLY_SYM
                    && matches!(
                        items.get(1).map(|f| &f.kind),
                        Some(FormKind::Symbol(f)) if f == OOP_SYNTAX_SYM
                    ) =>
            {
                (2, 3, true)
            }
            _ => return Ok(None),
        };
        if items.len() < stage_start {
            return Err(VmError::unsupported(
                form.span,
                "clove.syntax::oop expects base expression and at least one method stage",
            ));
        }
        let mut current = items
            .get(base_start)
            .cloned()
            .ok_or_else(|| VmError::unsupported(form.span, "oop form missing base expression"))?;
        let mut nil_safe_active = false;
        let mut nil_safe_stages: Vec<Form> = Vec::new();
        let mut processed_stages = 0usize;
        for stage in &items[stage_start..] {
            if self.is_oop_nil_safe_marker(stage) {
                if !nil_safe_active {
                    if processed_stages == 0 {
                        let (next_base, start_nil_safe) =
                            self.resolve_oop_nil_safe_start(current, stage.span)?;
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
            current = self.build_oop_stage_call(stage, current, form.span)?;
            processed_stages += 1;
        }
        if nil_safe_active {
            if nil_safe_stages.is_empty() {
                return Err(VmError::unsupported(
                    form.span,
                    "nil-safe chain requires at least one stage",
                ));
            }
            current = self.build_oop_nil_safe_chain(current, &nil_safe_stages, form.span)?;
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

    fn parse_oop_dot_stage(&self, stage: &Form) -> Result<Option<(Form, Form)>, VmError> {
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
            return Err(VmError::unsupported(
                stage.span,
                format!("{} stage expects placeholder and body", head),
            ));
        }
        let placeholder = items[1].clone();
        if !matches!(placeholder.kind, FormKind::Symbol(_)) {
            return Err(VmError::unsupported(
                stage.span,
                format!("{} stage placeholder must be symbol", head),
            ));
        }
        Ok(Some((placeholder, items[2].clone())))
    }

    fn parse_oop_index_stage(&self, stage: &Form) -> Result<Option<Form>, VmError> {
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
            return Err(VmError::unsupported(
                stage.span,
                format!("{} stage expects key", head),
            ));
        }
        Ok(Some(items[1].clone()))
    }

    fn parse_oop_bare_stage(&self, stage: &Form) -> Result<Option<Form>, VmError> {
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
            return Err(VmError::unsupported(
                stage.span,
                format!("{} stage expects method symbol", head),
            ));
        }
        if !matches!(items[1].kind, FormKind::Symbol(_)) {
            return Err(VmError::unsupported(
                stage.span,
                "oop method must be symbol",
            ));
        }
        Ok(Some(items[1].clone()))
    }

    fn unwrap_oop_stage_items<'a>(&self, items: &'a [Form]) -> (&'a [Form], bool) {
        if items.len() >= 2 {
            if let FormKind::Symbol(sym) = &items[0].kind {
                if sym == APPLY_SYM {
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
    ) -> Result<Form, VmError> {
        if !matches!(placeholder.kind, FormKind::Symbol(_)) {
            return Err(VmError::unsupported(
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

    fn build_oop_index_call(&self, span: Span, key: Form, base: Form) -> Form {
        let items = vec![
            Form::new(FormKind::Symbol(OOP_INDEX_SYM.to_string()), span),
            base,
            key,
        ];
        Form::new(FormKind::List(items), span)
    }

    fn parse_oop_stage(&self, stage: &Form) -> Result<(Form, Vec<Form>), VmError> {
        let items = match &stage.kind {
            FormKind::List(items) => items,
            _ => {
                return Err(VmError::unsupported(
                    stage.span,
                    "oop method stage must be list",
                ))
            }
        };
        if items.is_empty() {
            return Err(VmError::unsupported(
                stage.span,
                "oop method stage requires name",
            ));
        }
        let (method, args_start) = if matches!(&items[0].kind, FormKind::Symbol(sym) if sym == APPLY_SYM)
        {
            if items.len() < 2 {
                return Err(VmError::unsupported(
                    stage.span,
                    "oop method stage requires name",
                ));
            }
            (items[1].clone(), 2)
        } else {
            (items[0].clone(), 1)
        };
        if !matches!(method.kind, FormKind::Symbol(_)) {
            return Err(VmError::unsupported(
                stage.span,
                "oop method must be symbol",
            ));
        }
        Ok((method, items[args_start..].to_vec()))
    }

    fn build_oop_call(
        &self,
        span: Span,
        method: Form,
        args: Vec<Form>,
        base: Form,
    ) -> Result<Form, VmError> {
        let method_name = match &method.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                return Err(VmError::unsupported(
                    method.span,
                    "oop method must be symbol",
                ))
            }
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

    fn symbol_is_defined(&self, name: &str) -> bool {
        if self.resolve_local(name).is_some() || self.resolve_enclosing(name).is_some() {
            return true;
        }
        let canonical = canonical_symbol_name(name);
        self.globals.contains_key(canonical.as_ref())
    }

    fn resolve_oop_nil_safe_start(&self, base: Form, span: Span) -> Result<(Form, bool), VmError> {
        let FormKind::Symbol(sym) = &base.kind else {
            return Ok((base, true));
        };
        if sym.ends_with('?') {
            let fallback = format!("{}?", sym);
            let base_defined = self.symbol_is_defined(sym);
            let fallback_defined = self.symbol_is_defined(&fallback);
            if base_defined && fallback_defined {
                let msg = format!(
                    "nil-safe receiver '{}' is ambiguous; '{}' and '{}' are both defined",
                    sym, sym, fallback
                );
                return Err(VmError::unsupported(span, msg));
            }
            if base_defined {
                return Ok((base, true));
            }
            let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
            return Ok((fallback_form, false));
        }
        let fallback = format!("{}?", sym);
        if self.symbol_is_defined(&fallback) {
            let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
            return Ok((fallback_form, false));
        }
        Ok((base, true))
    }

    fn build_oop_stage_call(
        &self,
        stage: &Form,
        base: Form,
        form_span: Span,
    ) -> Result<Form, VmError> {
        if let Some((placeholder, body)) = self.parse_oop_dot_stage(stage)? {
            return self.build_oop_dot_call(stage.span, placeholder, body, base);
        }
        if let Some(key_form) = self.parse_oop_index_stage(stage)? {
            return Ok(self.build_oop_index_call(stage.span, key_form, base));
        }
        if let Some(method_form) = self.parse_oop_bare_stage(stage)? {
            return self.build_oop_call(stage.span, method_form, Vec::new(), base);
        }
        let (method_form, args) = self.parse_oop_stage(stage)?;
        let method_name = match &method_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => unreachable!(),
        };
        if method_name == OOP_DOT_STAGE_SYM {
            if args.len() != 2 {
                return Err(VmError::unsupported(
                    stage.span,
                    "clove.syntax::oop-dot expects placeholder and body",
                ));
            }
            return self.build_oop_dot_call(stage.span, args[0].clone(), args[1].clone(), base);
        }
        if method_name == OOP_INDEX_SYM {
            if args.len() != 1 {
                return Err(VmError::unsupported(
                    stage.span,
                    "clove.syntax::oop-index expects key",
                ));
            }
            return Ok(self.build_oop_index_call(stage.span, args[0].clone(), base));
        }
        self.build_oop_call(stage.span, method_form, args, base)
    }

    fn build_oop_nil_safe_chain(
        &self,
        base: Form,
        stages: &[Form],
        form_span: Span,
    ) -> Result<Form, VmError> {
        self.build_oop_nil_safe_chain_step(base, stages, form_span, 0)
    }

    fn build_oop_nil_safe_chain_step(
        &self,
        base: Form,
        stages: &[Form],
        form_span: Span,
        idx: usize,
    ) -> Result<Form, VmError> {
        let Some((stage, rest)) = stages.split_first() else {
            return Ok(base);
        };
        let tmp_name = format!("__oop_nil_safe{}_{}", form_span.index, idx);
        let tmp_sym = Form::new(FormKind::Symbol(tmp_name.clone()), stage.span);
        let stage_form = self.build_oop_stage_call(stage, tmp_sym.clone(), form_span)?;
        let next_form = if rest.is_empty() {
            stage_form
        } else {
            self.build_oop_nil_safe_chain_step(stage_form, rest, form_span, idx + 1)?
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
}

fn parse_params(params_form: &Form) -> Result<ParsedParams, VmError> {
    let mut params = Vec::new();
    let mut rest = None;
    let mut subject_seen = false;
    let items = match &params_form.kind {
        FormKind::Vector(items) => items.as_slice(),
        _ => return Err(VmError::unsupported(params_form.span, "fn params")),
    };
    let mut idx = 0;
    while idx < items.len() {
        let form = &items[idx];
        match &form.kind {
            FormKind::Symbol(sym) if sym == "&" => {
                let next = items
                    .get(idx + 1)
                    .ok_or_else(|| VmError::unsupported(form.span, "fn rest parameter"))?;
                let rest_name = match &next.kind {
                    FormKind::Symbol(name) => name,
                    _ => return Err(VmError::unsupported(next.span, "fn rest parameter")),
                };
                if rest_name.starts_with('$') {
                    return Err(VmError::unsupported(next.span, "fn rest parameter"));
                }
                if rest.is_some() || idx + 2 != items.len() {
                    return Err(VmError::unsupported(form.span, "fn rest parameter"));
                }
                rest = Some(canonical_symbol_name(rest_name).into_owned());
                break;
            }
            FormKind::Symbol(sym) => {
                let (param_name, is_subject) = if sym.starts_with('$') {
                    let trimmed = sym.trim_start_matches('$');
                    if trimmed.is_empty() {
                        return Err(VmError::unsupported(form.span, "fn param"));
                    }
                    (trimmed, true)
                } else {
                    (sym.as_str(), false)
                };
                if is_question_placeholder(sym) {
                    return Err(VmError::unsupported(form.span, "fn param"));
                }
                if is_subject {
                    if subject_seen {
                        return Err(VmError::unsupported(form.span, "fn param"));
                    }
                    subject_seen = true;
                }
                let ty = local_type_from_hint(form).unwrap_or(LocalType::Unknown);
                params.push(ParamSpec {
                    name: canonical_symbol_name(param_name).into_owned(),
                    ty,
                });
            }
            _ => return Err(VmError::unsupported(form.span, "fn param")),
        }
        idx += 1;
    }
    Ok(ParsedParams { params, rest })
}

fn local_type_from_hint(form: &Form) -> Option<LocalType> {
    let hint = form.type_hint.as_ref()?;
    match hint.kind {
        TypeKind::Int => Some(LocalType::Int),
        TypeKind::Float => Some(LocalType::Float),
        TypeKind::Bool => Some(LocalType::Bool),
        _ => None,
    }
}

fn merge_local_type(current: LocalType, incoming: LocalType) -> LocalType {
    if incoming == LocalType::Unknown {
        return current;
    }
    if current == LocalType::Unknown {
        return incoming;
    }
    if current == incoming {
        return current;
    }
    LocalType::Unknown
}

fn fn_return_type_from_clauses(clauses: &[FnClauseSpec]) -> Option<LocalType> {
    let mut out = None;
    for clause in clauses {
        let ty = local_type_from_hint(&clause.params_form).unwrap_or(LocalType::Unknown);
        if ty == LocalType::Unknown {
            return None;
        }
        match out {
            Some(prev) if prev != ty => return None,
            Some(_) => {}
            None => out = Some(ty),
        }
    }
    out
}

fn parse_fn_clauses(forms: &[Form], span: Span) -> Result<Vec<FnClauseSpec>, VmError> {
    if forms.is_empty() {
        return Err(VmError::unsupported(span, "fn expects params"));
    }
    match &forms[0].kind {
        FormKind::Vector(_) => {
            if forms.len() < 2 {
                return Err(VmError::unsupported(forms[0].span, "fn expects body"));
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
        _ => Err(VmError::unsupported(span, "fn expects params")),
    }
}

fn fn_return_type_from_fn_form(form: &Form) -> Option<LocalType> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    if items.is_empty() {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "fn" && head != "fn*" {
        return None;
    }
    let mut idx = 1;
    if items.len() >= 3 {
        if matches!(items[1].kind, FormKind::Symbol(_))
            && matches!(items[2].kind, FormKind::Vector(_) | FormKind::List(_))
        {
            idx = 2;
        }
    }
    let clauses = parse_fn_clauses(&items[idx..], form.span).ok()?;
    fn_return_type_from_clauses(&clauses)
}

fn parse_doc_and_meta(
    items: &[Form],
    start_idx: usize,
    context: &str,
    require_tail: bool,
) -> Result<(Option<String>, Option<Form>, usize), VmError> {
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
                    return Err(VmError::unsupported(
                        items[idx].span,
                        format!("{context} allows only one docstring"),
                    ));
                }
                doc = Some(text.clone());
                idx += 1;
            }
            FormKind::Map(_) => {
                if meta_map.is_some() {
                    return Err(VmError::unsupported(
                        items[idx].span,
                        format!("{context} allows only one attr-map"),
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

fn parse_defn_signature(items: &[Form], form_span: Span) -> Result<DefnSignature, VmError> {
    if items.is_empty() {
        return Err(VmError::unsupported(form_span, "defn expects params"));
    }
    let (doc, meta_map, mut idx) = parse_doc_and_meta(items, 0, "defn", false)?;
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
        return Err(VmError::unsupported(form_span, "defn expects params"));
    }
    match &items[idx].kind {
        FormKind::Vector(_) => {
            if idx + 1 >= items.len() {
                return Err(VmError::unsupported(items[idx].span, "defn expects body"));
            }
            let params_form = items[idx].clone();
            let body = items[idx + 1..].to_vec();
            let clause = FnClauseSpec::single(params_form, body)?;
            Ok(DefnSignature {
                doc,
                clauses: vec![clause],
                meta_map,
            })
        }
        FormKind::List(_) => {
            let clause_forms: Vec<Form> = items[idx..].to_vec();
            if clause_forms.is_empty() {
                return Err(VmError::unsupported(form_span, "defn expects arity"));
            }
            let mut clauses = Vec::with_capacity(clause_forms.len());
            for form in &clause_forms {
                clauses.push(FnClauseSpec::from_clause_form(form)?);
            }
            Ok(DefnSignature {
                doc,
                clauses,
                meta_map,
            })
        }
        _ => Err(VmError::unsupported(form_span, "defn expects params")),
    }
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
    Form {
        kind: FormKind::Vector(new_items),
        span: params_form.span,
        type_hint: params_form.type_hint.clone(),
    }
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

fn ensure_method_params_without_self(clauses: &[FnClauseSpec]) -> Result<(), VmError> {
    for clause in clauses {
        if form_contains_symbol(&clause.params_form, "self") {
            return Err(VmError::unsupported(
                clause.params_form.span,
                "self is reserved",
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

fn spread_span(items: &[Form]) -> Option<Span> {
    for form in items {
        if let FormKind::Symbol(sym) = &form.kind {
            if sym == "*" || strip_spread_symbol(sym).is_some() {
                return Some(form.span);
            }
        }
    }
    None
}

fn map_key_from_form(form: &Form) -> Result<Key, VmError> {
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
                _ => Err(VmError::unsupported(form.span, "invalid map key")),
            }
        }
        _ => Err(VmError::unsupported(form.span, "invalid map key")),
    }
}

fn form_contains_map_ref_entries(entries: &[MapItem]) -> bool {
    entries.iter().any(|entry| match entry {
        MapItem::KeyValue(k, v) => form_contains_map_ref_any(k) || form_contains_map_ref_any(v),
        MapItem::Spread(form) => form_contains_map_ref_any(form),
    })
}

fn form_contains_map_ref_any(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => {
            if is_map_ref_form(form) {
                return true;
            }
            items.iter().any(form_contains_map_ref_any)
        }
        FormKind::Vector(items) | FormKind::Set(items) | FormKind::ShortFn(items) => {
            items.iter().any(form_contains_map_ref_any)
        }
        FormKind::Map(entries) => form_contains_map_ref_entries(entries),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_contains_map_ref_any(expr),
            InterpolatedPart::Text(_) => false,
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_contains_map_ref_any(expr),
            InterpolatedPart::Text(_) => false,
        }),
        _ => false,
    }
}

fn call_label_key_span(items: &[Form]) -> Option<Span> {
    for form in items.iter().skip(1) {
        if let FormKind::Symbol(sym) = &form.kind {
            if is_label_style_key(sym) {
                return Some(form.span);
            }
        }
    }
    None
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

fn collect_local_defns_with(
    body: &[Form],
    allow_defn: bool,
) -> Result<(Vec<LocalDefnSpec>, Vec<Form>), VmError> {
    let mut local_defns = Vec::new();
    let mut forms = Vec::new();
    for form in body {
        let (mut nested, kept) = extract_local_defns_from_form(form, allow_defn)?;
        local_defns.append(&mut nested);
        if let Some(kept_form) = kept {
            forms.push(kept_form);
        }
    }
    Ok((local_defns, forms))
}

fn extract_local_defns_from_form(
    form: &Form,
    allow_defn: bool,
) -> Result<(Vec<LocalDefnSpec>, Option<Form>), VmError> {
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
            let name = local_defn_name(form, items)?;
            local_defns.push(LocalDefnSpec {
                name,
                form: form.clone(),
            });
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
            let name = local_defn_name(&rewritten_form, rewritten_items)?;
            local_defns.push(LocalDefnSpec {
                name,
                form: rewritten_form,
            });
            Ok((local_defns, None))
        }
        "do" | "where" => {
            let allow_nested_defn = head == "where" || allow_defn;
            let (mut nested, forms) = collect_local_defns_with(&items[1..], allow_nested_defn)?;
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

fn local_defn_name(form: &Form, items: &[Form]) -> Result<String, VmError> {
    if items.len() < 3 {
        return Err(VmError::unsupported(form.span, "local defn expects name"));
    }
    let name_form = &items[1];
    let name = if let FormKind::Symbol(s) = &name_form.kind {
        s.clone()
    } else {
        return Err(VmError::unsupported(name_form.span, "local defn name"));
    };
    Ok(canonical_symbol_name(&name).into_owned())
}

fn is_where_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) if !items.is_empty() => {
            matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "where")
        }
        _ => false,
    }
}

fn is_question_placeholder(name: &str) -> bool {
    if name == "?" {
        return true;
    }
    if let Some(rest) = name.strip_prefix('?') {
        return !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit());
    }
    false
}

fn is_cond_else(form: &Form) -> bool {
    matches!(&form.kind, FormKind::Keyword(k) if k == "else")
        || matches!(&form.kind, FormKind::Symbol(s) if s == ":else" || s == "_")
}

fn parse_binding_pair_form(form: &Form) -> Result<(Form, Form), VmError> {
    match &form.kind {
        FormKind::Vector(items) if items.len() == 2 => Ok((items[0].clone(), items[1].clone())),
        FormKind::Vector(_) => Err(VmError::unsupported(form.span, "binding pair")),
        _ => Err(VmError::unsupported(form.span, "binding pair")),
    }
}

fn builtin_id_for(name: &str) -> Option<BuiltinId> {
    match name {
        "+" => Some(BuiltinId::Add),
        "-" => Some(BuiltinId::Sub),
        "*" => Some(BuiltinId::Mul),
        "/" => Some(BuiltinId::Div),
        "=" => Some(BuiltinId::Eq),
        "<" => Some(BuiltinId::Lt),
        "<=" => Some(BuiltinId::Le),
        ">" => Some(BuiltinId::Gt),
        ">=" => Some(BuiltinId::Ge),
        "inc" => Some(BuiltinId::Inc),
        "dec" => Some(BuiltinId::Dec),
        _ => None,
    }
}

fn is_unsupported_special_form_head(name: &str) -> bool {
    UNSUPPORTED_SPECIAL_FORM_HEADS
        .iter()
        .any(|head| head == &name)
}

pub fn compile_form(form: &Form) -> Result<CompiledChunk, VmError> {
    let mut compiler = Compiler::new(true);
    compiler.compile_form(form)?;
    compiler.emit_return(form.span);
    Ok(compiler.finish())
}
