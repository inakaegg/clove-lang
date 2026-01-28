//! Minimal HM type inference constraint generation/solving.
//! Not yet covers the whole language, but used for basic type checks with `--opt hm`.

use std::collections::{HashMap, HashSet};
use std::sync::Once;

use crate::ast::{Form, FormKind, InterpolatedPart, MapItem, Span};
use crate::builtins;
use crate::compiler::APPLY_SYM;
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::expand_short_fn_to_list;
use crate::fn_meta::{self, FnOverload, SubjectPos};
use crate::plugin_meta;
use crate::reader::{
    INFIX_SYNTAX_SYM, MAP_REF_SYM, MATCH_OR_SYM, OOP_AS_SYM, OOP_BARE_SYM, OOP_DOT_STAGE_SYM,
    OOP_INDEX_SYM, OOP_LET_SYM, OOP_NIL_SAFE_SYM, OOP_SYNTAX_SYM,
};
use crate::symbols::{canonical_symbol_name, doc_lookup_keys};
use crate::try_form::{
    build_err_clause_handler, build_fin_clause_handler, format_try_error_message,
    parse_err_fin_tail, parse_try_short, validate_try_bindings, TryShortPlan,
};
use crate::type_registry;
use crate::type_syntax::{normalize_type_syntax_forms, parse_type_expr_from_forms};
use crate::types::TypeKind;

use super::hm::{instantiate, unify, PrimType, Scheme, Subst, Type, TypeEnv, TypeError, TypeVar};

pub type TypedForm = (Form, Type);

/// Inference result (top-level forms + map from span-index to types).
pub struct InferResult {
    pub forms: Vec<TypedForm>,
    pub type_map: HashMap<usize, Type>,
}

pub struct TypeDiag {
    pub span: Span,
    pub message: String,
}

pub struct InferResultWithDiags {
    pub forms: Vec<TypedForm>,
    pub type_map: HashMap<usize, Type>,
    pub diags: Vec<TypeDiag>,
}

pub fn infer_forms(forms: &[Form]) -> Result<InferResult, CloveError> {
    let forms = normalize_type_syntax_forms(forms.to_vec(), false)?;
    let mut state = InferState::new();
    let mut out = Vec::new();
    for form in &forms {
        let ty = infer_form(form, &mut state).map_err(to_ce)?;
        out.push((form.clone(), ty));
    }
    let type_map = finalize_type_map(&state);
    let forms = out
        .into_iter()
        .map(|(form, ty)| (form, state.subst.apply(&ty)))
        .collect();
    Ok(InferResult { forms, type_map })
}

pub fn infer_forms_with_diags(forms: &[Form]) -> InferResultWithDiags {
    let forms =
        normalize_type_syntax_forms(forms.to_vec(), true).unwrap_or_else(|_| forms.to_vec());
    let mut state = InferState::new_best_effort();
    let mut out = Vec::new();
    for form in &forms {
        match infer_form(form, &mut state) {
            Ok(ty) => out.push((form.clone(), ty)),
            Err(err) => {
                let span = err.span.unwrap_or(form.span);
                state.record_diag(span, err.message.clone());
                let fallback = Type::Any;
                state.record_type(form.span, &fallback);
                out.push((form.clone(), fallback));
            }
        }
    }
    let type_map = finalize_type_map(&state);
    let forms = out
        .into_iter()
        .map(|(form, ty)| (form, state.subst.apply(&ty)))
        .collect();
    InferResultWithDiags {
        forms,
        type_map,
        diags: state.diags,
    }
}

struct InferState {
    env: TypeEnv,
    subst: Subst,
    type_map: HashMap<usize, Type>,
    type_defs: HashMap<String, Type>,
    diags: Vec<TypeDiag>,
    best_effort: bool,
}

impl InferState {
    fn new() -> Self {
        Self {
            env: prelude_env(),
            subst: Subst::new(),
            type_map: HashMap::new(),
            type_defs: HashMap::new(),
            diags: Vec::new(),
            best_effort: false,
        }
    }

    fn new_best_effort() -> Self {
        Self {
            best_effort: true,
            ..Self::new()
        }
    }

    fn fresh_var(&mut self) -> Type {
        Type::Var(self.env.fresh_var())
    }

    fn generalize(&self, ty: &Type) -> Scheme {
        self.env.generalize_with_subst(ty, &self.subst)
    }

    fn instantiate_scheme(&mut self, scheme: &Scheme) -> Type {
        let mut subst = self.subst.clone();
        for var in &scheme.vars {
            subst.map.remove(var);
        }
        let applied = subst.apply(&scheme.ty);
        let applied_scheme = Scheme {
            vars: scheme.vars.clone(),
            ty: applied,
        };
        instantiate(&applied_scheme, &mut self.env)
    }

    fn record_type(&mut self, span: Span, ty: &Type) {
        self.type_map.insert(span.index, ty.clone());
    }

    fn record_diag(&mut self, span: Span, message: String) {
        self.diags.push(TypeDiag { span, message });
    }

    fn register_named_type(&mut self, name: impl Into<String>, ty: Type) {
        self.type_defs.insert(name.into(), ty);
    }

    fn lookup_named_type(&self, name: &str) -> Option<Type> {
        self.type_defs
            .get(name)
            .cloned()
            .or_else(|| lookup_builtin_named_type(name))
    }

    fn try_unify_soft(&mut self, lhs: &Type, rhs: &Type) -> bool {
        match unify(lhs, rhs) {
            Ok(s) => {
                self.subst = s.compose(&self.subst);
                true
            }
            Err(_) => false,
        }
    }

    fn try_unify(
        &mut self,
        lhs: &Type,
        rhs: &Type,
        span: Span,
        emit_diag: bool,
    ) -> Result<bool, TypeError> {
        match unify(lhs, rhs) {
            Ok(s) => {
                self.subst = s.compose(&self.subst);
                Ok(true)
            }
            Err(err) => {
                if self.best_effort {
                    if emit_diag {
                        self.record_diag(span, err.message);
                    }
                    Ok(false)
                } else {
                    Err(err.with_span(Some(span)))
                }
            }
        }
    }
}

fn finalize_type_map(state: &InferState) -> HashMap<usize, Type> {
    state
        .type_map
        .iter()
        .map(|(k, ty)| (*k, state.subst.apply(ty)))
        .collect()
}

fn infer_form(form: &Form, st: &mut InferState) -> Result<Type, TypeError> {
    let mut skip_hint = false;
    let mut ty = match &form.kind {
        FormKind::Int(_) => Ok(Type::Prim(PrimType::Int)),
        FormKind::Float(_) => Ok(Type::Prim(PrimType::Float)),
        FormKind::Bool(_) => Ok(Type::Prim(PrimType::Bool)),
        FormKind::String(_) => Ok(Type::Prim(PrimType::Str)),
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    infer_form(expr, st)?;
                }
            }
            Ok(Type::Prim(PrimType::Str))
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    infer_form(expr, st)?;
                }
            }
            Ok(Type::Opaque("core::Regex".to_string()))
        }
        FormKind::Nil => Ok(Type::Prim(PrimType::Nil)),
        FormKind::Regex { .. } => Ok(Type::Opaque("core::Regex".to_string())),
        FormKind::Symbol(_) => infer_symbol(form, st),
        FormKind::List(items) => infer_list(items, form.span, st),
        FormKind::Vector(items) => {
            skip_hint = true;
            infer_vector_form(form, items, st)
        }
        FormKind::Map(entries) => {
            let mut fields = HashMap::new();
            let mut has_spread = false;
            let mut all_keyword_keys = true;
            let mut key_entries: Vec<(Type, Span)> = Vec::new();
            let mut val_entries: Vec<(Type, Span)> = Vec::new();
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let kt = infer_form(k, st)?;
                        let vt = infer_form(v, st)?;
                        if let FormKind::Keyword(name) = &k.kind {
                            fields.insert(name.clone(), vt.clone());
                        } else {
                            all_keyword_keys = false;
                        }
                        key_entries.push((kt, k.span));
                        val_entries.push((vt, v.span));
                    }
                    MapItem::Spread(expr) => {
                        has_spread = true;
                        infer_form(expr, st)?;
                    }
                }
            }
            if all_keyword_keys && !has_spread && !fields.is_empty() {
                Ok(Type::Record(fields))
            } else {
                let mut key_ty: Option<Type> = None;
                let mut val_ty: Option<Type> = None;
                for (kt, span) in key_entries {
                    key_ty = match key_ty {
                        Some(current) => {
                            if st.try_unify(&current, &kt, span, true)? {
                                Some(st.subst.apply(&current))
                            } else {
                                Some(Type::Any)
                            }
                        }
                        None => Some(kt),
                    };
                }
                for (vt, span) in val_entries {
                    val_ty = match val_ty {
                        Some(current) => {
                            if st.try_unify(&current, &vt, span, true)? {
                                Some(st.subst.apply(&current))
                            } else {
                                Some(Type::Any)
                            }
                        }
                        None => Some(vt),
                    };
                }
                Ok(Type::Map(
                    Box::new(key_ty.unwrap_or_else(|| st.fresh_var())),
                    Box::new(val_ty.unwrap_or_else(|| st.fresh_var())),
                ))
            }
        }
        FormKind::Set(items) => infer_set_form(form, items, st),
        FormKind::ShortFn(body) => {
            let expanded = expand_short_fn_to_list(body, form.span);
            infer_form(&expanded, st)
        }
        _ => Ok(Type::Any),
    }?;
    if !skip_hint {
        if let Some(type_hint) = &form.type_hint {
            if let Ok(hint) = convert_annotation(type_hint.kind.clone()) {
                ty = hint;
            }
        }
    }
    let record_span = match &form.kind {
        FormKind::List(items)
            if matches!(
                items.first().map(|item| &item.kind),
                Some(FormKind::Symbol(sym)) if sym == OOP_SYNTAX_SYM
            ) =>
        {
            synthetic_span(form.span, 2)
        }
        _ => form.span,
    };
    st.record_type(record_span, &ty);
    Ok(ty)
}

fn infer_vector_form(form: &Form, items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if let Some(type_hint) = &form.type_hint {
        if let Ok(hint) = convert_annotation(type_hint.kind.clone()) {
            match hint {
                Type::Tuple(expected) => {
                    return infer_vector_with_tuple_hint(items, &expected, st, form.span)
                }
                Type::Vector(inner) => {
                    return infer_vector_with_elem_hint(items, &inner, st);
                }
                other => {
                    for item in items {
                        infer_form(item, st)?;
                    }
                    return Ok(other);
                }
            }
        }
    }
    infer_vector_default(items, st)
}

fn infer_set_form(form: &Form, items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if let Some(type_hint) = &form.type_hint {
        if let Ok(hint) = convert_annotation(type_hint.kind.clone()) {
            match hint {
                Type::Set(inner) => {
                    return infer_set_with_elem_hint(items, &inner, st);
                }
                other => {
                    for item in items {
                        infer_form(item, st)?;
                    }
                    return Ok(other);
                }
            }
        }
    }
    infer_set_default(items, st)
}

fn infer_vector_with_elem_hint(
    items: &[Form],
    expected: &Type,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let mut elem_ty = expected.clone();
    for item in items {
        let t = infer_form(item, st)?;
        let expected_applied = st.subst.apply(expected);
        if st.try_unify(&t, &expected_applied, item.span, true)? {
            elem_ty = st.subst.apply(&elem_ty);
        }
    }
    Ok(Type::Vector(Box::new(elem_ty)))
}

fn infer_set_with_elem_hint(
    items: &[Form],
    expected: &Type,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let mut elem_ty = expected.clone();
    for item in items {
        let t = infer_form(item, st)?;
        let expected_applied = st.subst.apply(expected);
        if st.try_unify(&t, &expected_applied, item.span, true)? {
            elem_ty = st.subst.apply(&elem_ty);
        }
    }
    Ok(Type::Set(Box::new(elem_ty)))
}

fn infer_vector_with_tuple_hint(
    items: &[Form],
    expected: &[Type],
    st: &mut InferState,
    span: Span,
) -> Result<Type, TypeError> {
    let mut inferred = Vec::new();
    for item in items {
        inferred.push(infer_form(item, st)?);
    }
    if expected.len() < 2 || expected.len() != items.len() {
        let message = format!(
            "tuple arity mismatch: expected {}, got {}",
            expected.len(),
            items.len()
        );
        if st.best_effort {
            st.record_diag(span, message);
            return Ok(Type::Any);
        }
        return Err(TypeError::new(message).with_span(Some(span)));
    }
    for (item, (actual, expected)) in items.iter().zip(inferred.iter().zip(expected.iter())) {
        let expected_applied = st.subst.apply(expected);
        st.try_unify(&actual, &expected_applied, item.span, true)?;
    }
    let applied = expected.iter().map(|t| st.subst.apply(t)).collect();
    Ok(Type::Tuple(applied))
}

fn infer_vector_default(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let mut elem_types = Vec::new();
    let mut unified: Option<Type> = None;
    let mut unify_failed = false;
    for item in items {
        let t = infer_form(item, st)?;
        elem_types.push(t.clone());
        if unify_failed {
            continue;
        }
        unified = match unified {
            Some(current) => {
                if st.try_unify_soft(&current, &t) {
                    Some(st.subst.apply(&current))
                } else {
                    unify_failed = true;
                    None
                }
            }
            None => Some(t),
        };
    }
    if unify_failed && elem_types.len() >= 2 {
        let applied = elem_types.iter().map(|t| st.subst.apply(t)).collect();
        return Ok(Type::Tuple(applied));
    }
    let resolved = unified.map(|t| st.subst.apply(&t)).unwrap_or(Type::Any);
    Ok(Type::Vector(Box::new(resolved)))
}

fn infer_set_default(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let mut elem_ty: Option<Type> = None;
    let mut unify_failed = false;
    for item in items {
        let t = infer_form(item, st)?;
        if unify_failed {
            continue;
        }
        elem_ty = match elem_ty {
            Some(current) => {
                if st.try_unify(&current, &t, item.span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    unify_failed = true;
                    Some(Type::Any)
                }
            }
            None => Some(t),
        };
    }
    let resolved = if unify_failed {
        Type::Any
    } else {
        elem_ty.unwrap_or_else(|| st.fresh_var())
    };
    Ok(Type::Set(Box::new(resolved)))
}

fn infer_symbol(form: &Form, st: &mut InferState) -> Result<Type, TypeError> {
    let (base, annot_ty) = symbol_base_and_hint(form, st);
    if let Some(scheme) = st.env.lookup(&base) {
        Ok(st.instantiate_scheme(&scheme))
    } else {
        // For unknown symbols, use annotation if present; otherwise Any.
        Ok(annot_ty.unwrap_or(Type::Any))
    }
}

fn infer_list(items: &[Form], span: Span, st: &mut InferState) -> Result<Type, TypeError> {
    if items.is_empty() {
        return Ok(Type::Any);
    }
    if matches!(&items[0].kind, FormKind::Keyword(_)) {
        return infer_keyword_call(items, st);
    }
    let head_sym = match &items[0].kind {
        FormKind::Symbol(s) => s.as_str(),
        _ => "",
    };
    match head_sym {
        "def" => infer_def(items, st),
        "defn" => infer_defn(items, st),
        "let" => infer_let(items, st),
        "fn" => infer_fn(items, st),
        "if" => infer_if(items, st),
        "if-not" => infer_if(items, st),
        "when" => infer_when(items, st),
        "when-not" => infer_when(items, st),
        "when-let" => infer_when_let(items, st),
        "if-let" => infer_if_let(items, st),
        "if-some" => infer_if_some(items, st),
        "cond" => infer_cond(items, st),
        "cond->" => infer_cond_thread(items, st, false),
        "cond->>" => infer_cond_thread(items, st, true),
        "condp" => infer_condp(items, st),
        "and" => infer_and_or(items, st),
        "or" => infer_and_or(items, st),
        "do" => infer_do(items, st),
        "where" => infer_where(items, st),
        "p" => infer_p(items, span, st),
        "comment" => infer_comment(items, st),
        "__set-in-chain" => {
            if items.len() >= 3 {
                infer_form(&items[2], st)
            } else {
                Ok(Type::Any)
            }
        }
        "->" => infer_thread(items, st, false),
        "->>" => infer_thread(items, st, true),
        "as->" => infer_as_thread(items, st),
        OOP_SYNTAX_SYM => infer_oop_chain(items, st),
        INFIX_SYNTAX_SYM => infer_infix(items, st),
        "some->" => infer_some_thread(items, st, false),
        "some->>" => infer_some_thread(items, st, true),
        "doto" => infer_doto(items, st),
        "vector" | "list" => infer_vector_literal(items, st),
        "hash-set" => infer_set_literal(items, st),
        "hash-map" => infer_hash_map_literal(items, st),
        "get" => infer_get(items, st),
        "assoc" => infer_assoc(items, st),
        "dissoc" => infer_dissoc(items, st),
        "merge" => infer_merge(items, st),
        "reduce" => infer_reduce(items, st),
        "filter" => infer_filter(items, st),
        "pfilter" => infer_filter(items, st),
        "map" => infer_map(items, st),
        "contains?" => infer_contains(items, st),
        "subs" => infer_subs(items, st),
        "glob" | "fs::glob" | "io::glob" | "std::glob" | "glob*" | "fs::glob*" | "io::glob*"
        | "std::glob*" => infer_glob(items, st),
        "json::read-file" => infer_json_read_file(items, st),
        "json::read-file-seq" => infer_json_read_file(items, st),
        "json::read-seq" => infer_json_read_seq(items, st),
        "union" | "intersection" | "difference" | "std::union" | "std::intersection"
        | "std::difference" | "set::union" | "set::intersection" | "set::difference" => {
            infer_set_op(items, st)
        }
        "match" => infer_match(items, st),
        "for" => infer_for(items, st),
        "doseq" => infer_doseq(items, st),
        "each" => infer_each(items, st),
        "dotimes" => infer_dotimes(items, st),
        "while" => infer_while(items, st),
        "loop" => infer_loop(items, st),
        "go-loop" => infer_go_loop(items, st),
        "scope-loop" => infer_scope_loop(items, st),
        "async::scope-loop" => infer_scope_loop(items, st),
        "async-scope" => infer_async_scope(items, st),
        "async::scope" => infer_async_scope(items, st),
        "with-open" => infer_with_open(items, st),
        "with-redefs" => infer_with_redefs(items, st),
        "with-dyn" => infer_with_dyn(items, st),
        "try" => infer_try(items, st),
        "catch" => infer_catch_clause(items, st),
        "finally" => infer_finally_clause(items, st),
        "err" => Err(TypeError::new("err must appear at end of try or a body")
            .with_span(Some(items[0].span))),
        "fin" => Err(TypeError::new("fin must appear at end of try or a body")
            .with_span(Some(items[0].span))),
        "throw" => infer_throw(items, st),
        "recur" => infer_recur(items, st),
        "set!" => infer_set(items, st),
        "delay" => infer_delay(items, st),
        "eval" => infer_eval(items, st),
        "load-file" => infer_simple_any(items, st),
        "load-string" => infer_simple_any(items, st),
        "use" => infer_simple_nil_no_eval(items, st),
        "use-syntax" => infer_simple_nil_no_eval(items, st),
        "require" => infer_simple_nil_no_eval(items, st),
        "require-native" => infer_simple_nil_no_eval(items, st),
        "ns-map" => infer_simple_any(items, st),
        "nav" => infer_simple_any(items, st),
        "lookup" => infer_simple_any(items, st),
        "create-ns" => infer_simple_any(items, st),
        "refer" => infer_simple_any(items, st),
        "resolve" => infer_simple_any(items, st),
        "current-ns" => infer_simple_any(items, st),
        "describe" => infer_simple_any(items, st),
        "describe-type" => infer_simple_any(items, st),
        "infer-type" => infer_simple_any(items, st),
        "enum-members" => infer_simple_any(items, st),
        "defenum" => infer_defenum(items, st),
        "deftype" => infer_deftype(items, st),
        "method" => infer_simple_any(items, st),
        "break" => infer_simple_nil(items, st),
        "repl" => infer_simple_nil(items, st),
        "debug" => infer_simple_nil(items, st),
        "str" => infer_vararg_str(items, st),
        "pp" | "pprint" => infer_pp(items, st),
        "pp-str" | "pprint-str" => infer_pp_str(items, st),
        "println" | "print" | "prn" => infer_vararg_nil(items, st),
        "+" | "-" | "*" | "/" => infer_vararg_numeric(items, st, head_sym),
        ">" | "<" | ">=" | "<=" => infer_vararg_compare(items, st),
        "=" | "not=" => infer_vararg_eq(items, st, head_sym),
        "max" | "min" => infer_vararg_extremum(items, st),
        _ => infer_call(items, span, st),
    }
}

fn infer_infix(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() != 2 {
        return Err(
            TypeError::new("clove.syntax::infix expects exactly one expression")
                .with_span(Some(items[0].span)),
        );
    }
    let lowered = lower_infix_expr_for_infer(&items[1]);
    infer_form(&lowered, st)
}

fn lower_infix_expr_for_infer(form: &Form) -> Form {
    let with_kind = |kind| Form {
        kind,
        span: form.span,
        type_hint: form.type_hint.clone(),
    };
    match &form.kind {
        FormKind::List(items) => {
            if items.is_empty() {
                return form.clone();
            }
            let head = match &items[0].kind {
                FormKind::Symbol(sym) => sym.as_str(),
                _ => "",
            };
            if matches!(head, "==" | "=") && items.len() == 3 {
                let lhs = lower_infix_expr_for_infer(&items[1]);
                let rhs = lower_infix_expr_for_infer(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("=".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if matches!(head, "!=" | "not=") && items.len() == 3 {
                let lhs = lower_infix_expr_for_infer(&items[1]);
                let rhs = lower_infix_expr_for_infer(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("not=".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "%" && items.len() == 3 {
                let lhs = lower_infix_expr_for_infer(&items[1]);
                let rhs = lower_infix_expr_for_infer(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("mod".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "&&" && items.len() == 3 {
                let lhs = lower_infix_expr_for_infer(&items[1]);
                let rhs = lower_infix_expr_for_infer(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("and".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "||" && items.len() == 3 {
                let lhs = lower_infix_expr_for_infer(&items[1]);
                let rhs = lower_infix_expr_for_infer(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("or".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "!" && items.len() == 2 {
                let rhs = lower_infix_expr_for_infer(&items[1]);
                let mut new_items = Vec::with_capacity(2);
                new_items.push(Form::new(FormKind::Symbol("not".into()), items[0].span));
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            let mut lowered_items = Vec::with_capacity(items.len());
            for item in items {
                lowered_items.push(lower_infix_expr_for_infer(item));
            }
            with_kind(FormKind::List(lowered_items))
        }
        FormKind::Vector(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr_for_infer(item));
            }
            with_kind(FormKind::Vector(lowered))
        }
        FormKind::Set(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr_for_infer(item));
            }
            with_kind(FormKind::Set(lowered))
        }
        FormKind::Map(entries) => {
            let mut lowered = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let key = lower_infix_expr_for_infer(k);
                        let value = lower_infix_expr_for_infer(v);
                        lowered.push(MapItem::KeyValue(key, value));
                    }
                    MapItem::Spread(expr) => {
                        let value = lower_infix_expr_for_infer(expr);
                        lowered.push(MapItem::Spread(value));
                    }
                }
            }
            with_kind(FormKind::Map(lowered))
        }
        FormKind::InterpolatedString(parts) => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        lowered.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        lowered.push(InterpolatedPart::Expr(lower_infix_expr_for_infer(expr)));
                    }
                }
            }
            with_kind(FormKind::InterpolatedString(lowered))
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        lowered.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        lowered.push(InterpolatedPart::Expr(lower_infix_expr_for_infer(expr)));
                    }
                }
            }
            with_kind(FormKind::InterpolatedRegex {
                parts: lowered,
                delim: *delim,
            })
        }
        FormKind::ShortFn(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr_for_infer(item));
            }
            with_kind(FormKind::ShortFn(lowered))
        }
        _ => form.clone(),
    }
}

#[derive(Clone, Debug)]
struct ParamEntry {
    name: String,
    ty: Type,
    span: Span,
    is_rest: bool,
}

fn collect_param_types(params: &[Form], st: &mut InferState) -> (Vec<ParamEntry>, Option<Type>) {
    let mut out = Vec::new();
    let mut rest_elem = None;
    let mut idx = 0;
    while idx < params.len() {
        match &params[idx].kind {
            FormKind::Symbol(sym) if sym == "&" => {
                if let Some(rest_form) = params.get(idx + 1) {
                    if let FormKind::Symbol(_) = rest_form.kind {
                        let (name, hint) = symbol_base_and_hint(rest_form, st);
                        let (param_ty, elem_ty) = rest_types_from_hint(hint);
                        out.push(ParamEntry {
                            name,
                            ty: param_ty,
                            span: rest_form.span,
                            is_rest: true,
                        });
                        rest_elem = Some(elem_ty);
                    }
                }
                break;
            }
            FormKind::Symbol(_) => {
                let (name, hint) = symbol_base_and_hint(&params[idx], st);
                let ty = hint.unwrap_or_else(|| st.fresh_var());
                out.push(ParamEntry {
                    name,
                    ty,
                    span: params[idx].span,
                    is_rest: false,
                });
            }
            _ => {}
        }
        idx += 1;
    }
    (out, rest_elem)
}

fn rest_types_from_hint(hint: Option<Type>) -> (Type, Type) {
    match hint {
        Some(Type::Vector(inner)) => {
            let elem = (*inner).clone();
            (Type::Vector(inner), elem)
        }
        Some(other) => {
            let elem = other.clone();
            (Type::Vector(Box::new(other)), elem)
        }
        None => (Type::Vector(Box::new(Type::Any)), Type::Any),
    }
}

fn split_local_defns(body_forms: &[Form]) -> (Vec<Form>, Vec<Form>) {
    collect_local_defns_with(body_forms, false)
}

fn collect_local_defns_with(body: &[Form], allow_defn: bool) -> (Vec<Form>, Vec<Form>) {
    let mut local_defns = Vec::new();
    let mut forms = Vec::new();
    for form in body {
        let (mut nested, kept) = extract_local_defns_from_form(form, allow_defn);
        local_defns.append(&mut nested);
        if let Some(kept_form) = kept {
            forms.push(kept_form);
        }
    }
    (local_defns, forms)
}

fn extract_local_defns_from_form(form: &Form, allow_defn: bool) -> (Vec<Form>, Option<Form>) {
    let mut local_defns = Vec::new();
    let items = match &form.kind {
        FormKind::List(items) if !items.is_empty() => items,
        _ => return (local_defns, Some(form.clone())),
    };
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return (local_defns, Some(form.clone())),
    };
    match head {
        "-defn" => {
            local_defns.push(form.clone());
            (local_defns, None)
        }
        "defn" if allow_defn => {
            local_defns.push(form.clone());
            (local_defns, None)
        }
        "do" | "where" => {
            let allow_nested_defn = head == "where" || allow_defn;
            let (mut nested, forms) = collect_local_defns_with(&items[1..], allow_nested_defn);
            local_defns.append(&mut nested);
            let mut rewritten = Vec::with_capacity(1 + forms.len());
            rewritten.push(items[0].clone());
            rewritten.extend(forms);
            let rebuilt = Form {
                kind: FormKind::List(rewritten),
                span: form.span,
                type_hint: form.type_hint.clone(),
            };
            (local_defns, Some(rebuilt))
        }
        _ => (local_defns, Some(form.clone())),
    }
}

fn predeclare_local_defns(local_defns: &[Form], st: &mut InferState) {
    for form in local_defns {
        let FormKind::List(items) = &form.kind else {
            continue;
        };
        if items.len() < 2 {
            continue;
        }
        let name = match &items[1].kind {
            FormKind::Symbol(_) => symbol_base_and_hint(&items[1], st).0,
            _ => continue,
        };
        if name == "_" {
            continue;
        }
        let mut idx = 2;
        if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::String(_))) {
            idx += 1;
        }
        if idx + 1 < items.len()
            && matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::Map(_)))
        {
            idx += 1;
        }
        if idx >= items.len() {
            continue;
        }
        match &items[idx].kind {
            FormKind::Vector(params) => {
                let (entries, rest_elem) = collect_param_types(params, st);
                let param_tys = entries
                    .iter()
                    .filter(|entry| !entry.is_rest)
                    .map(|entry| entry.ty.clone())
                    .collect();
                let ret_ty = st.fresh_var();
                let fn_ty = Type::Func(param_tys, rest_elem.map(Box::new), Box::new(ret_ty));
                st.env.insert(
                    name,
                    Scheme {
                        vars: vec![],
                        ty: fn_ty,
                    },
                );
            }
            FormKind::List(_) => {
                st.env.insert(
                    name,
                    Scheme {
                        vars: vec![],
                        ty: Type::Any,
                    },
                );
            }
            _ => {}
        }
    }
}

fn infer_def(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let name_form = &items[1];
    let name = match &name_form.kind {
        FormKind::Symbol(sym) => sym.clone(),
        _ => return Ok(Type::Any),
    };
    let mut idx = 2;
    if idx + 1 < items.len() && matches!(&items[idx].kind, FormKind::String(_)) {
        idx += 1;
    }
    if idx + 1 < items.len() && matches!(&items[idx].kind, FormKind::Map(_)) {
        idx += 1;
    }
    if idx >= items.len() {
        return Ok(Type::Any);
    }
    let val_ty = infer_form(&items[idx], st)?;
    let applied = st.subst.apply(&val_ty);
    let scheme = st.generalize(&applied);
    st.env.insert(name, scheme);
    Ok(applied)
}

fn infer_set(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let value_ty = infer_form(&items[2], st)?;
    if let FormKind::Symbol(sym) = &items[1].kind {
        let applied = st.subst.apply(&value_ty);
        st.env.insert(
            sym.clone(),
            Scheme {
                vars: vec![],
                ty: applied.clone(),
            },
        );
    }
    Ok(st.subst.apply(&value_ty))
}

fn infer_defn(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let (name, ret_hint) = match &items[1].kind {
        FormKind::Symbol(_) => symbol_base_and_hint(&items[1], st),
        _ => return Ok(Type::Any),
    };
    let mut idx = 2;
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::String(_))) {
        idx += 1;
    }
    if idx >= items.len() {
        return Ok(Type::Any);
    }
    let params_form = match &items[idx].kind {
        FormKind::Vector(_) => &items[idx],
        FormKind::List(_) => return Ok(Type::Any),
        _ => return Ok(Type::Any),
    };
    let body_forms = &items[idx + 1..];
    let body_items = rewrite_defn_body_err_fin(body_forms, items[0].span)?;

    let mut param_entries = Vec::new();
    let mut rest_elem = None;
    if let FormKind::Vector(params) = &params_form.kind {
        let (entries, rest) = collect_param_types(params, st);
        param_entries = entries;
        rest_elem = rest;
    }
    let param_tys: Vec<Type> = param_entries
        .iter()
        .filter(|entry| !entry.is_rest)
        .map(|entry| entry.ty.clone())
        .collect();
    let ret_ty = st.fresh_var();
    let fn_ty = Type::Func(
        param_tys.clone(),
        rest_elem.clone().map(Box::new),
        Box::new(ret_ty.clone()),
    );
    // Provisional entry to handle recursion.
    st.env.insert(
        name.clone(),
        Scheme {
            vars: vec![],
            ty: fn_ty.clone(),
        },
    );

    let saved = st.env.snapshot_bindings();
    let result = (|| {
        for entry in &param_entries {
            st.env.insert(
                entry.name.clone(),
                Scheme {
                    vars: vec![],
                    ty: entry.ty.clone(),
                },
            );
        }
        let (local_defns, body_items) = split_local_defns(&body_items);
        predeclare_local_defns(&local_defns, st);
        for local in local_defns {
            if let FormKind::List(items) = &local.kind {
                let _ = infer_defn(items, st)?;
            }
        }
        let body_ty = infer_body(&body_items, st)?;
        // If a return annotation exists, unify with it.
        let ret_target = if let Some(hint) = ret_hint.clone() {
            if st.try_unify(&ret_ty, &hint, items[0].span, true)? {
                hint
            } else {
                Type::Any
            }
        } else {
            ret_ty.clone()
        };
        let mut ret_final = ret_target.clone();
        if !st.try_unify(&ret_target, &body_ty, items[0].span, true)? {
            ret_final = Type::Any;
        }
        ret_final = st.subst.apply(&ret_final);
        let fn_ty_final = st.subst.apply(&Type::Func(
            param_tys.clone(),
            rest_elem.clone().map(Box::new),
            Box::new(ret_final.clone()),
        ));
        if let Type::Func(args, _, _) = &fn_ty_final {
            let mut arg_iter = args.iter();
            for entry in &param_entries {
                if entry.is_rest {
                    let ty = st.subst.apply(&entry.ty);
                    st.record_type(entry.span, &ty);
                } else if let Some(arg) = arg_iter.next() {
                    st.record_type(entry.span, arg);
                }
            }
        }
        let scheme = st.generalize(&fn_ty_final);
        Ok((fn_ty_final, scheme))
    })();
    st.env.restore_bindings(saved);
    let (fn_ty_final, scheme) = result?;
    st.env.insert(name, scheme);
    st.record_type(items[1].span, &fn_ty_final);
    Ok(fn_ty_final)
}

fn infer_deftype(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let name_form = &items[1];
    let name = match &name_form.kind {
        FormKind::Symbol(_) => symbol_base_and_hint(name_form, st).0,
        _ => return Ok(Type::Any),
    };
    let mut idx = 2;
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::String(_))) {
        idx += 1;
    }
    let mut meta_form = None;
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::Map(_))) && idx + 1 < items.len() {
        meta_form = Some(items[idx].clone());
        idx += 1;
    }
    let mut alias_ty = None;
    let mut field_forms = Vec::new();
    let mut where_forms = Vec::new();
    let mut method_forms = Vec::new();
    while idx < items.len() {
        let form = &items[idx];
        match &form.kind {
            FormKind::Keyword(kw) if kw == "alias" => {
                if let Ok((kind, consumed)) = parse_type_expr_from_forms(&items[idx + 1..]) {
                    if let Ok(ty) = convert_annotation_with_lookup(kind, Some(st)) {
                        alias_ty = Some(ty);
                        idx += 1 + consumed;
                        continue;
                    }
                }
            }
            FormKind::Keyword(kw) if kw == "from" => {
                if let Some(value_form) = items.get(idx + 2) {
                    let _ = infer_form(value_form, st)?;
                }
                idx += 3;
                continue;
            }
            FormKind::List(list_items) if is_where_form_items(list_items) => {
                where_forms.push(form.clone());
                idx += 1;
                continue;
            }
            FormKind::List(list_items) if is_deftype_method_items(list_items) => {
                method_forms.push(form.clone());
                idx += 1;
                continue;
            }
            FormKind::List(list_items)
                if matches!(
                    list_items.first().map(|f| &f.kind),
                    Some(FormKind::Symbol(sym)) if sym == "def"
                ) =>
            {
                let _ = infer_form(form, st)?;
                idx += 1;
                continue;
            }
            _ => {
                field_forms.push(form.clone());
                idx += 1;
                continue;
            }
        }
        idx += 1;
    }
    if field_forms.is_empty() && (!where_forms.is_empty() || !method_forms.is_empty()) {
        if let Some(form) = meta_form.take() {
            field_forms.push(form);
        }
    }
    let ty = alias_ty.unwrap_or_else(|| Type::Record(parse_deftype_fields(&field_forms, st)));
    st.register_named_type(name.clone(), ty.clone());
    register_type_constructor(&name, &ty, st);
    infer_deftype_methods(&method_forms, &where_forms, &ty, st)?;
    Ok(Type::Any)
}

fn infer_defenum(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let name_form = &items[1];
    let enum_name = match &name_form.kind {
        FormKind::Symbol(_) => symbol_base_and_hint(name_form, st).0,
        _ => return Ok(Type::Any),
    };
    let mut idx = 2;
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::String(_))) {
        idx += 1;
    }
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::Map(_))) {
        idx += 1;
    }
    let mut qualified_only = false;
    while let Some(FormKind::Keyword(kw)) = items.get(idx).map(|f| &f.kind) {
        if kw != "qualified-only" {
            break;
        }
        if let Some(FormKind::Bool(value)) = items.get(idx + 1).map(|f| &f.kind) {
            qualified_only = *value;
            idx += 2;
            continue;
        }
        break;
    }
    let member_forms = &items[idx..];
    let mut member_idx = 0;
    while member_idx < member_forms.len() {
        let member_form = &member_forms[member_idx];
        let member_name = match &member_form.kind {
            FormKind::Symbol(sym) => sym.clone(),
            _ => {
                member_idx += 1;
                continue;
            }
        };
        if member_name.starts_with('*') {
            member_idx += 1;
            continue;
        }
        let mut fields = HashMap::new();
        let mut consumed = 1;
        if let Some(payload_form) = member_forms
            .get(member_idx + 1)
            .filter(|form| matches!(form.kind, FormKind::Map(_)))
        {
            fields = parse_deftype_fields(std::slice::from_ref(payload_form), st);
            consumed = 2;
        }
        let variant_ty = if !fields.is_empty() {
            Type::Record(fields)
        } else {
            st.lookup_named_type(&member_name)
                .unwrap_or_else(|| Type::Record(HashMap::new()))
        };
        let qualified = format!("{}::{}", enum_name, member_name);
        if !qualified_only {
            st.register_named_type(member_name.clone(), variant_ty.clone());
            register_type_constructor(&member_name, &variant_ty, st);
        }
        if qualified_only || qualified != member_name {
            st.register_named_type(qualified.clone(), variant_ty.clone());
            register_type_constructor(&qualified, &variant_ty, st);
        }
        member_idx += consumed;
    }
    Ok(Type::Any)
}

fn infer_deftype_methods(
    method_forms: &[Form],
    where_forms: &[Form],
    self_ty: &Type,
    st: &mut InferState,
) -> Result<(), TypeError> {
    for where_form in where_forms {
        if let FormKind::List(items) = &where_form.kind {
            for form in items.iter().skip(1) {
                if let FormKind::List(method_items) = &form.kind {
                    if is_deftype_method_items(method_items) {
                        let _ = infer_method_defn(method_items, self_ty, st)?;
                    }
                }
            }
        }
    }
    for method_form in method_forms {
        if let FormKind::List(method_items) = &method_form.kind {
            let _ = infer_method_defn(method_items, self_ty, st)?;
        }
    }
    Ok(())
}

fn infer_method_defn(
    items: &[Form],
    self_ty: &Type,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let (name, ret_hint) = match &items[1].kind {
        FormKind::Symbol(_) => symbol_base_and_hint(&items[1], st),
        _ => return Ok(Type::Any),
    };
    let mut idx = 2;
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::String(_))) {
        idx += 1;
    }
    if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::Map(_))) {
        idx += 1;
    }
    if idx >= items.len() {
        return Ok(Type::Any);
    }
    let params_form = match &items[idx].kind {
        FormKind::Vector(_) => &items[idx],
        FormKind::List(_) => return Ok(Type::Any),
        _ => return Ok(Type::Any),
    };
    let body_forms = &items[idx + 1..];
    let mut param_entries = Vec::new();
    let mut rest_elem = None;
    if let FormKind::Vector(params) = &params_form.kind {
        let (entries, rest) = collect_param_types(params, st);
        param_entries = entries;
        rest_elem = rest;
    }
    let has_self_param = param_entries
        .first()
        .is_some_and(|entry| entry.name == "self");
    let inserted_self = !has_self_param;
    if has_self_param {
        if let Some(entry) = param_entries.first_mut() {
            entry.ty = self_ty.clone();
        }
    } else {
        param_entries.insert(
            0,
            ParamEntry {
                name: "self".into(),
                ty: self_ty.clone(),
                span: params_form.span,
                is_rest: false,
            },
        );
    }
    let param_tys: Vec<Type> = param_entries
        .iter()
        .filter(|entry| !entry.is_rest)
        .map(|entry| entry.ty.clone())
        .collect();
    let ret_ty = st.fresh_var();
    let fn_ty = Type::Func(
        param_tys.clone(),
        rest_elem.clone().map(Box::new),
        Box::new(ret_ty.clone()),
    );
    st.env.insert(
        name.clone(),
        Scheme {
            vars: vec![],
            ty: fn_ty.clone(),
        },
    );
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        for entry in &param_entries {
            st.env.insert(
                entry.name.clone(),
                Scheme {
                    vars: vec![],
                    ty: entry.ty.clone(),
                },
            );
        }
        let (local_defns, body_items) = split_local_defns(body_forms);
        predeclare_local_defns(&local_defns, st);
        for local in local_defns {
            if let FormKind::List(items) = &local.kind {
                let _ = infer_defn(items, st)?;
            }
        }
        let mut body_ty = Type::Prim(PrimType::Nil);
        for form in body_items {
            let rewritten = rewrite_method_form(&form);
            body_ty = infer_form(&rewritten, st)?;
        }
        let ret_target = if let Some(hint) = ret_hint.clone() {
            if st.try_unify(&ret_ty, &hint, items[0].span, true)? {
                hint
            } else {
                Type::Any
            }
        } else {
            ret_ty.clone()
        };
        let mut ret_final = ret_target.clone();
        if !st.try_unify(&ret_target, &body_ty, items[0].span, true)? {
            ret_final = Type::Any;
        }
        ret_final = st.subst.apply(&ret_final);
        let fn_ty_final = st.subst.apply(&Type::Func(
            param_tys.clone(),
            rest_elem.clone().map(Box::new),
            Box::new(ret_final.clone()),
        ));
        if let Type::Func(args, _, _) = &fn_ty_final {
            let mut arg_iter = args.iter();
            for (idx, entry) in param_entries.iter().enumerate() {
                if inserted_self && idx == 0 {
                    if !entry.is_rest {
                        let _ = arg_iter.next();
                    }
                    continue;
                }
                if entry.is_rest {
                    let ty = st.subst.apply(&entry.ty);
                    st.record_type(entry.span, &ty);
                } else if let Some(arg) = arg_iter.next() {
                    st.record_type(entry.span, arg);
                }
            }
        }
        let scheme = st.generalize(&fn_ty_final);
        Ok((fn_ty_final, scheme))
    })();
    st.env.restore_bindings(saved);
    let (fn_ty_final, scheme) = result?;
    st.env.insert(name, scheme);
    Ok(fn_ty_final)
}

fn rewrite_defn_body_err_fin(body: &[Form], span: Span) -> Result<Vec<Form>, TypeError> {
    let tail = parse_err_fin_tail(body, "defn body")
        .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?;
    if let Some(tail) = tail {
        if tail.body.is_empty() {
            return Err(TypeError::new("defn expects body").with_span(Some(span)));
        }
    }
    Ok(body.to_vec())
}

fn parse_deftype_fields(forms: &[Form], st: &InferState) -> HashMap<String, Type> {
    if forms.is_empty() {
        return HashMap::new();
    }
    if forms.len() == 1 {
        match &forms[0].kind {
            FormKind::Map(entries) => return parse_deftype_field_map(entries, st),
            FormKind::Vector(items) | FormKind::List(items) => {
                return parse_deftype_field_pairs(items, st);
            }
            _ => {}
        }
    }
    parse_deftype_field_pairs(forms, st)
}

fn parse_deftype_field_map(entries: &[MapItem], st: &InferState) -> HashMap<String, Type> {
    let mut fields = HashMap::new();
    for entry in entries {
        if let MapItem::KeyValue(k, v) = entry {
            if let Some((name, hint)) = field_name_and_hint(k, st) {
                let ty = hint.or_else(|| type_from_form(v, st)).unwrap_or(Type::Any);
                fields.insert(name, ty);
            }
        }
    }
    fields
}

fn parse_deftype_field_pairs(items: &[Form], st: &InferState) -> HashMap<String, Type> {
    let mut fields = HashMap::new();
    let mut idx = 0;
    while idx < items.len() {
        if let Some((name, hint)) = field_name_and_hint(&items[idx], st) {
            if let Some(ty) = hint {
                fields.insert(name, ty);
                idx += 1;
                continue;
            }
            if let Some(next) = items.get(idx + 1) {
                let ty = type_from_form(next, st).unwrap_or(Type::Any);
                fields.insert(name, ty);
                idx += 2;
                continue;
            }
            fields.insert(name, Type::Any);
        }
        idx += 1;
    }
    fields
}

fn field_name_and_hint(form: &Form, st: &InferState) -> Option<(String, Option<Type>)> {
    match &form.kind {
        FormKind::Keyword(name) => Some((name.clone(), None)),
        FormKind::Symbol(_) => {
            let (base, hint) = symbol_base_and_hint(form, st);
            Some((normalize_field_name(&base), hint))
        }
        _ => None,
    }
}

fn normalize_field_name(name: &str) -> String {
    name.trim_end_matches(':').to_string()
}

fn type_from_form(form: &Form, st: &InferState) -> Option<Type> {
    let (kind, _) = parse_type_expr_from_forms(std::slice::from_ref(form)).ok()?;
    convert_annotation_with_lookup(kind, Some(st)).ok()
}

fn register_type_constructor(name: &str, ret_ty: &Type, st: &mut InferState) {
    let ctor_ty = Type::Func(
        Vec::new(),
        Some(Box::new(Type::Any)),
        Box::new(ret_ty.clone()),
    );
    st.env.insert(
        name.to_string(),
        Scheme {
            vars: vec![],
            ty: ctor_ty,
        },
    );
}

fn is_where_form_items(items: &[Form]) -> bool {
    matches!(
        items.first().map(|f| &f.kind),
        Some(FormKind::Symbol(sym)) if sym == "where"
    )
}

fn is_deftype_method_items(items: &[Form]) -> bool {
    matches!(
        items.first().map(|f| &f.kind),
        Some(FormKind::Symbol(sym)) if sym == "defn" || sym == "method"
    )
}

fn rewrite_method_form(form: &Form) -> Form {
    match &form.kind {
        FormKind::List(items) => {
            if is_quote_form(items) {
                return form.clone();
            }
            if let Some(rewritten) = map_ref_to_self_get(form, items) {
                return rewritten;
            }
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let rewritten = rewrite_method_form(item);
                if rewritten != *item {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::List(new_items),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::Vector(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let rewritten = rewrite_method_form(item);
                if rewritten != *item {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::Vector(new_items),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::Set(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let rewritten = rewrite_method_form(item);
                if rewritten != *item {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::Set(new_items),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::Map(entries) => {
            let mut used = false;
            let mut new_entries = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let new_k = rewrite_method_form(k);
                        let new_v = rewrite_method_form(v);
                        used |= new_k != *k || new_v != *v;
                        new_entries.push(MapItem::KeyValue(new_k, new_v));
                    }
                    MapItem::Spread(expr) => {
                        let new_expr = rewrite_method_form(expr);
                        used |= new_expr != *expr;
                        new_entries.push(MapItem::Spread(new_expr));
                    }
                }
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::Map(new_entries),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::InterpolatedString(parts) => {
            let mut used = false;
            let mut new_parts = Vec::with_capacity(parts.len());
            for part in parts {
                let new_part = match part {
                    InterpolatedPart::Expr(expr) => {
                        let new_expr = rewrite_method_form(expr);
                        used |= new_expr != *expr;
                        InterpolatedPart::Expr(new_expr)
                    }
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                };
                new_parts.push(new_part);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::InterpolatedString(new_parts),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut used = false;
            let mut new_parts = Vec::with_capacity(parts.len());
            for part in parts {
                let new_part = match part {
                    InterpolatedPart::Expr(expr) => {
                        let new_expr = rewrite_method_form(expr);
                        used |= new_expr != *expr;
                        InterpolatedPart::Expr(new_expr)
                    }
                    InterpolatedPart::Text(text) => InterpolatedPart::Text(text.clone()),
                };
                new_parts.push(new_part);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::InterpolatedRegex {
                    parts: new_parts,
                    delim: *delim,
                },
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        FormKind::ShortFn(items) => {
            let mut used = false;
            let mut new_items = Vec::with_capacity(items.len());
            for item in items {
                let rewritten = rewrite_method_form(item);
                if rewritten != *item {
                    used = true;
                }
                new_items.push(rewritten);
            }
            if !used {
                return form.clone();
            }
            Form {
                kind: FormKind::ShortFn(new_items),
                span: form.span,
                type_hint: form.type_hint.clone(),
            }
        }
        _ => form.clone(),
    }
}

fn map_ref_to_self_get(form: &Form, items: &[Form]) -> Option<Form> {
    if items.len() < 3 {
        return None;
    }
    match items.first().map(|item| &item.kind) {
        Some(FormKind::Symbol(sym)) if sym == MAP_REF_SYM => {}
        _ => return None,
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
            FormKind::Symbol("get".into()),
            synthetic_span(form.span, 1),
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

fn infer_let(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let bindings = match &items[1].kind {
        FormKind::Vector(vec) => vec,
        _ => return Ok(Type::Any),
    };
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let mut idx = 0;
        while idx + 1 < bindings.len() {
            let (name, hint) = match &bindings[idx].kind {
                FormKind::Symbol(_) => symbol_base_and_hint(&bindings[idx], st),
                _ => {
                    idx += 2;
                    continue;
                }
            };
            let val_ty = infer_form(&bindings[idx + 1], st)?;
            let target_ty = if let Some(h) = hint {
                if st.try_unify(&val_ty, &h, bindings[idx].span, true)? {
                    h
                } else {
                    Type::Any
                }
            } else {
                val_ty
            };
            let applied = st.subst.apply(&target_ty);
            let gen = if is_non_expansive(&bindings[idx + 1]) {
                st.generalize(&applied)
            } else {
                Scheme {
                    vars: vec![],
                    ty: applied,
                }
            };
            st.env.insert(name, gen);
            idx += 2;
        }
        let mut last = Type::Prim(PrimType::Nil);
        for body in items.iter().skip(2) {
            last = infer_form(body, st)?;
        }
        Ok(last)
    })();
    st.env.restore_bindings(saved);
    result
}

fn is_non_expansive(form: &Form) -> bool {
    match &form.kind {
        FormKind::Int(_)
        | FormKind::Float(_)
        | FormKind::Bool(_)
        | FormKind::String(_)
        | FormKind::Keyword(_)
        | FormKind::Nil
        | FormKind::Regex { .. }
        | FormKind::Symbol(_)
        | FormKind::ShortFn(_) => true,
        FormKind::Vector(items) => items.iter().all(is_non_expansive),
        FormKind::Set(items) => items.iter().all(is_non_expansive),
        FormKind::Map(entries) => entries.iter().all(|entry| match entry {
            MapItem::KeyValue(k, v) => is_non_expansive(k) && is_non_expansive(v),
            MapItem::Spread(expr) => is_non_expansive(expr),
        }),
        FormKind::List(items) => match items.first().map(|f| &f.kind) {
            Some(FormKind::Symbol(sym)) if sym == "fn" => true,
            _ => false,
        },
        _ => false,
    }
}

fn infer_fn(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let mut idx = 1;
    if matches!(items.get(1).map(|f| &f.kind), Some(FormKind::Symbol(_))) && items.len() >= 3 {
        if matches!(items[2].kind, FormKind::Vector(_))
            || matches!(items[2].kind, FormKind::List(_))
        {
            idx = 2;
        }
    }
    if idx >= items.len() {
        return Ok(Type::Any);
    }
    let params_form = match &items[idx].kind {
        FormKind::Vector(_) => &items[idx],
        FormKind::List(_) => return Ok(Type::Any),
        _ => return Ok(Type::Any),
    };
    let body_forms = &items[idx + 1..];
    let mut param_entries = Vec::new();
    let mut rest_elem = None;
    if let FormKind::Vector(params) = &params_form.kind {
        let (entries, rest) = collect_param_types(params, st);
        param_entries = entries;
        rest_elem = rest;
    }
    let mut param_tys = Vec::new();
    let mut shadow_env: HashMap<String, Scheme> = HashMap::new();
    for entry in &param_entries {
        st.record_type(entry.span, &entry.ty);
        shadow_env.insert(
            entry.name.clone(),
            Scheme {
                vars: vec![],
                ty: entry.ty.clone(),
            },
        );
        if !entry.is_rest {
            param_tys.push(entry.ty.clone());
        }
    }
    // save outer env and shadow during fn inference
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        for (k, v) in shadow_env {
            st.env.insert(k, v);
        }
        let (local_defns, body_items) = split_local_defns(body_forms);
        predeclare_local_defns(&local_defns, st);
        for local in local_defns {
            if let FormKind::List(items) = &local.kind {
                let _ = infer_defn(items, st)?;
            }
        }
        let body_ty = infer_body(&body_items, st)?;
        let fn_ty = st.subst.apply(&Type::Func(
            param_tys.clone(),
            rest_elem.map(Box::new),
            Box::new(body_ty.clone()),
        ));
        if let Type::Func(args, _, _) = &fn_ty {
            let mut arg_iter = args.iter();
            for entry in &param_entries {
                if entry.is_rest {
                    let ty = st.subst.apply(&entry.ty);
                    st.record_type(entry.span, &ty);
                } else if let Some(arg) = arg_iter.next() {
                    st.record_type(entry.span, arg);
                }
            }
        }
        Ok(fn_ty)
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_if(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let cond_ty = infer_form(&items[1], st)?;
    st.try_unify(&cond_ty, &Type::Prim(PrimType::Bool), items[0].span, true)?;
    let then_ty = infer_form(&items[2], st)?;
    let else_ty = if items.len() > 3 {
        infer_form(&items[3], st)?
    } else {
        Type::Prim(PrimType::Nil)
    };
    let then_applied = st.subst.apply(&then_ty);
    let else_applied = st.subst.apply(&else_ty);
    if let Some(joined) = join_if_branches(&then_applied, &else_applied, st, items[0].span)? {
        return Ok(st.subst.apply(&joined));
    }
    if st.try_unify(&then_applied, &else_applied, items[0].span, true)? {
        Ok(st.subst.apply(&then_applied))
    } else {
        Ok(Type::Any)
    }
}

fn infer_body(forms: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let err_fin = parse_err_fin_tail(forms, "a body")
        .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?;
    if let Some(tail) = err_fin {
        if tail.body.is_empty() {
            let span = tail
                .err
                .as_ref()
                .map(|f| f.span)
                .or_else(|| tail.fin.as_ref().map(|f| f.span))
                .unwrap_or_else(|| forms.first().map(|f| f.span).unwrap());
            return Err(TypeError::new("do expects body before err/fin").with_span(Some(span)));
        }
        return infer_try_err_fin(tail, st);
    }
    let mut last = Type::Prim(PrimType::Nil);
    let mut has_non_where = false;
    for form in forms {
        let ty = infer_form(form, st)?;
        if is_where_form(form) {
            continue;
        }
        last = ty;
        has_non_where = true;
    }
    if has_non_where {
        Ok(last)
    } else {
        Ok(Type::Prim(PrimType::Nil))
    }
}

fn join_with_nil(ty: &Type, st: &mut InferState, span: Span) -> Result<Type, TypeError> {
    let nil_ty = Type::Prim(PrimType::Nil);
    let applied = st.subst.apply(ty);
    if let Some(joined) = join_if_branches(&applied, &nil_ty, st, span)? {
        Ok(st.subst.apply(&joined))
    } else if st.try_unify(&applied, &nil_ty, span, true)? {
        Ok(st.subst.apply(&applied))
    } else {
        Ok(Type::Any)
    }
}

fn parse_single_binding(
    form: &Form,
    st: &mut InferState,
    context: &str,
) -> Result<Option<(Form, Form)>, TypeError> {
    match &form.kind {
        FormKind::Vector(items) => {
            if items.len() != 2 {
                let message = format!("{} expects binding vector [name value]", context);
                if st.best_effort {
                    st.record_diag(form.span, message);
                    return Ok(None);
                }
                return Err(TypeError::new(message).with_span(Some(form.span)));
            }
            Ok(Some((items[0].clone(), items[1].clone())))
        }
        _ => {
            let message = format!("{} expects binding vector [name value]", context);
            if st.best_effort {
                st.record_diag(form.span, message);
                return Ok(None);
            }
            Err(TypeError::new(message).with_span(Some(form.span)))
        }
    }
}

fn infer_when(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let cond_ty = infer_form(&items[1], st)?;
    st.try_unify(&cond_ty, &Type::Prim(PrimType::Bool), items[0].span, true)?;
    let body_ty = infer_body(&items[2..], st)?;
    join_with_nil(&body_ty, st, items[0].span)
}

fn infer_when_let(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let binding_opt = parse_single_binding(&items[1], st, "when-let")?;
    let Some((binding, value_form)) = binding_opt else {
        infer_body(&items[2..], st)?;
        return Ok(Type::Any);
    };
    let val_ty = infer_form(&value_form, st)?;
    let saved = st.env.snapshot_bindings();
    let body_result = (|| {
        bind_pattern_with_type(&binding, &val_ty, st, true)?;
        infer_body(&items[2..], st)
    })();
    st.env.restore_bindings(saved);
    let body_ty = body_result?;
    join_with_nil(&body_ty, st, items[0].span)
}

fn infer_if_let(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let binding_opt = parse_single_binding(&items[1], st, "if-let")?;
    let Some((binding, value_form)) = binding_opt else {
        infer_form(&items[2], st)?;
        if items.len() > 3 {
            infer_form(&items[3], st)?;
        }
        return Ok(Type::Any);
    };
    let val_ty = infer_form(&value_form, st)?;
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        bind_pattern_with_type(&binding, &val_ty, st, true)?;
        let then_ty = infer_form(&items[2], st)?;
        st.env.restore_bindings(saved.clone());
        let else_ty = if items.len() > 3 {
            infer_form(&items[3], st)?
        } else {
            Type::Prim(PrimType::Nil)
        };
        let then_applied = st.subst.apply(&then_ty);
        let else_applied = st.subst.apply(&else_ty);
        if let Some(joined) = join_if_branches(&then_applied, &else_applied, st, items[0].span)? {
            Ok(st.subst.apply(&joined))
        } else if st.try_unify(&then_applied, &else_applied, items[0].span, true)? {
            Ok(st.subst.apply(&then_applied))
        } else {
            Ok(Type::Any)
        }
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_if_some(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    infer_if_let(items, st)
}

fn infer_cond(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    if (items.len() - 1) % 2 != 0 {
        let message = "cond expects even number of test/expr pairs".to_string();
        if st.best_effort {
            st.record_diag(items[0].span, message);
            for form in items.iter().skip(1) {
                infer_form(form, st)?;
            }
            return Ok(Type::Any);
        }
        return Err(TypeError::new(message).with_span(Some(items[0].span)));
    }
    let mut idx = 1;
    let mut saw_else = false;
    let mut result: Option<Type> = None;
    while idx + 1 < items.len() {
        let test_form = &items[idx];
        let expr_form = &items[idx + 1];
        if is_else_form(test_form) {
            saw_else = true;
        } else {
            infer_form(test_form, st)?;
        }
        let expr_ty = infer_form(expr_form, st)?;
        result = Some(join_branch_types(result, expr_ty, st, expr_form.span)?);
        idx += 2;
        if saw_else {
            break;
        }
    }
    let mut out = result.unwrap_or(Type::Prim(PrimType::Nil));
    if !saw_else {
        let nil_ty = Type::Prim(PrimType::Nil);
        if let Some(joined) = join_if_branches(&out, &nil_ty, st, items[0].span)? {
            out = st.subst.apply(&joined);
        } else if st.try_unify(&out, &nil_ty, items[0].span, true)? {
            out = st.subst.apply(&out);
        } else {
            out = Type::Any;
        }
    }
    Ok(out)
}

fn infer_cond_thread(
    items: &[Form],
    st: &mut InferState,
    thread_last: bool,
) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    if items.len() == 2 {
        return infer_form(&items[1], st);
    }
    if (items.len() - 2) % 2 != 0 {
        let message = "cond-> expects pairs of test and form".to_string();
        if st.best_effort {
            st.record_diag(items[0].span, message);
            for form in items.iter().skip(1) {
                infer_form(form, st)?;
            }
            return Ok(Type::Any);
        }
        return Err(TypeError::new(message).with_span(Some(items[0].span)));
    }
    let mut acc_ty = infer_form(&items[1], st)?;
    let mut idx = 2;
    while idx + 1 < items.len() {
        infer_form(&items[idx], st)?;
        let step_ty = infer_thread_step(&items[idx + 1], &acc_ty, st, thread_last)?;
        acc_ty = join_branch_types(Some(acc_ty), step_ty, st, items[idx + 1].span)?;
        idx += 2;
    }
    Ok(st.subst.apply(&acc_ty))
}

fn infer_thread(items: &[Form], st: &mut InferState, thread_last: bool) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let mut acc_ty = infer_form(&items[1], st)?;
    for step in items.iter().skip(2) {
        acc_ty = infer_thread_step(step, &acc_ty, st, thread_last)?;
    }
    Ok(acc_ty)
}

fn infer_some_thread(
    items: &[Form],
    st: &mut InferState,
    thread_last: bool,
) -> Result<Type, TypeError> {
    let acc_ty = infer_thread(items, st, thread_last)?;
    join_with_nil(&acc_ty, st, items[0].span)
}

fn infer_as_thread(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let value_ty = infer_form(&items[1], st)?;
    let name = match &items[2].kind {
        FormKind::Symbol(sym) => sym.clone(),
        _ => return Ok(Type::Any),
    };
    if items.len() == 3 {
        return Ok(st.subst.apply(&value_ty));
    }
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        st.env.insert(
            name.clone(),
            Scheme {
                vars: vec![],
                ty: st.subst.apply(&value_ty),
            },
        );
        let mut current = st.subst.apply(&value_ty);
        for form in items.iter().skip(3) {
            if let Some(sym) = parse_dot_chain_as_directive(form) {
                st.env.insert(
                    sym,
                    Scheme {
                        vars: vec![],
                        ty: current.clone(),
                    },
                );
                continue;
            }
            if let Some(sym) = parse_dot_chain_let_directive(form) {
                let stash = format!("*{}", sym);
                st.env.insert(
                    stash,
                    Scheme {
                        vars: vec![],
                        ty: current.clone(),
                    },
                );
                continue;
            }
            let ty = infer_form(form, st)?;
            current = st.subst.apply(&ty);
            st.env.insert(
                name.clone(),
                Scheme {
                    vars: vec![],
                    ty: current.clone(),
                },
            );
        }
        Ok(current)
    })();
    st.env.restore_bindings(saved);
    result
}

fn parse_dot_chain_as_directive(form: &Form) -> Option<String> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    if items.len() != 2 {
        return None;
    }
    if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "as") {
        return None;
    }
    match &items[1].kind {
        FormKind::Symbol(sym) => Some(sym.clone()),
        _ => None,
    }
}

fn parse_dot_chain_let_directive(form: &Form) -> Option<String> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    if items.len() != 2 {
        return None;
    }
    if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "let") {
        return None;
    }
    match &items[1].kind {
        FormKind::Symbol(sym) if !sym.starts_with('*') => Some(sym.clone()),
        _ => None,
    }
}

fn infer_oop_chain(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let mut current = items[1].clone();
    let mut nil_safe_active = false;
    let mut nil_safe_stages: Vec<Form> = Vec::new();
    let mut lexical_binds: Vec<(Form, Form, Span)> = Vec::new();
    let mut oop_let_counter = 0usize;
    let mut processed_stages = 0usize;
    for stage in items.iter().skip(2) {
        if is_oop_nil_safe_marker(stage) {
            if !nil_safe_active {
                if processed_stages == 0 {
                    let (next_base, start_nil_safe, ambiguous) =
                        resolve_oop_nil_safe_start(current, st);
                    if ambiguous {
                        return Ok(Type::Any);
                    }
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
        if let Some((repl_name, sym_name)) = parse_oop_repl_stash_stage(stage) {
            let tmp_name = format!("__oop_let{}_{}", items[0].span.index, oop_let_counter);
            oop_let_counter += 1;
            let stashed = build_oop_let_stash_form(stage.span, &sym_name, current, tmp_name);
            current = build_oop_repl_stage_form(stage.span, &repl_name, Vec::new(), stashed);
            processed_stages += 1;
            continue;
        }
        if let Some(sym) = parse_oop_as_stage(stage) {
            let previous = current;
            lexical_binds.push((sym.clone(), previous, stage.span));
            current = sym;
            processed_stages += 1;
            continue;
        }
        if let Some(sym_name) = parse_oop_let_stage(stage) {
            let tmp_name = format!("__oop_let{}_{}", items[0].span.index, oop_let_counter);
            oop_let_counter += 1;
            current = build_oop_let_stash_form(stage.span, &sym_name, current, tmp_name);
            processed_stages += 1;
            continue;
        }
        let Some(next) = build_oop_stage_form(stage, current) else {
            return Ok(Type::Any);
        };
        current = next;
        processed_stages += 1;
    }
    if nil_safe_active {
        if nil_safe_stages.is_empty() {
            return Ok(Type::Any);
        }
        let Some(next) = build_oop_nil_safe_chain(current, &nil_safe_stages) else {
            return Ok(Type::Any);
        };
        current = next;
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
    infer_form(&current, st)
}

fn synthetic_span(base: Span, tag: usize) -> Span {
    let high_bit = 1usize << (usize::BITS - 1);
    let index = high_bit | base.index.saturating_mul(4) | (tag & 0b11);
    Span {
        index,
        line: base.line,
        col: base.col,
    }
}

fn unwrap_oop_stage_items<'a>(items: &'a [Form]) -> &'a [Form] {
    if items.len() >= 2 {
        if let FormKind::Symbol(sym) = &items[0].kind {
            if sym == APPLY_SYM {
                return &items[1..];
            }
        }
    }
    items
}

fn is_oop_nil_safe_marker(stage: &Form) -> bool {
    let FormKind::List(items) = &stage.kind else {
        return false;
    };
    let items = unwrap_oop_stage_items(items);
    items.len() == 1 && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM)
}

fn resolve_oop_nil_safe_start(base: Form, st: &InferState) -> (Form, bool, bool) {
    let FormKind::Symbol(sym) = &base.kind else {
        return (base, true, false);
    };
    if sym.ends_with('?') {
        let fallback = format!("{}?", sym);
        let base_defined = st.env.lookup(sym).is_some();
        let fallback_defined = st.env.lookup(&fallback).is_some();
        if base_defined && fallback_defined {
            return (base, false, true);
        }
        if base_defined {
            return (base, true, false);
        }
        let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
        return (fallback_form, false, false);
    }
    let fallback = format!("{}?", sym);
    if st.env.lookup(&fallback).is_some() {
        let fallback_form = Form::new(FormKind::Symbol(fallback), base.span);
        return (fallback_form, false, false);
    }
    (base, true, false)
}

fn parse_oop_as_stage(stage: &Form) -> Option<Form> {
    let FormKind::List(items) = &stage.kind else {
        return None;
    };
    let items = unwrap_oop_stage_items(items);
    if items.len() != 2 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != OOP_AS_SYM {
        return None;
    }
    let sym = match &items[1].kind {
        FormKind::Symbol(sym) => sym.clone(),
        _ => return None,
    };
    Some(Form::new(FormKind::Symbol(sym), items[1].span))
}

fn parse_oop_let_stage(stage: &Form) -> Option<String> {
    let FormKind::List(items) = &stage.kind else {
        return None;
    };
    let items = unwrap_oop_stage_items(items);
    if items.len() != 2 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != OOP_LET_SYM {
        return None;
    }
    let sym = match &items[1].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if sym.starts_with('*') {
        return None;
    }
    Some(sym.to_string())
}

fn parse_oop_repl_stash_stage(stage: &Form) -> Option<(String, String)> {
    let FormKind::List(items) = &stage.kind else {
        return None;
    };
    let items = unwrap_oop_stage_items(items);
    if items.len() != 2 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "repl" && head != "debug" {
        return None;
    }
    let sym = match &items[1].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if sym == "?" || sym == "*?" || sym.starts_with('*') {
        return None;
    }
    Some((head.to_string(), sym.to_string()))
}

fn build_oop_let_stash_form(span: Span, name: &str, base: Form, tmp_name: String) -> Form {
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

fn build_oop_repl_stage_form(span: Span, name: &str, mut args: Vec<Form>, base: Form) -> Form {
    let mut items = Vec::with_capacity(args.len() + 2);
    items.push(Form::new(FormKind::Symbol(name.to_string()), span));
    if args.is_empty() {
        items.push(base);
    } else {
        items.append(&mut args);
    }
    Form::new(FormKind::List(items), span)
}

fn build_oop_stage_form(stage: &Form, base: Form) -> Option<Form> {
    let items = match &stage.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head == OOP_DOT_STAGE_SYM {
        if items.len() != 3 {
            return None;
        }
        let placeholder = items[1].clone();
        let body = items[2].clone();
        let as_sym = Form::new(FormKind::Symbol("as->".into()), stage.span);
        return Some(Form::new(
            FormKind::List(vec![as_sym, base, placeholder, body]),
            stage.span,
        ));
    }
    if head == OOP_INDEX_SYM {
        if items.len() != 2 {
            return None;
        }
        let get_sym = Form::new(
            FormKind::Symbol("get".into()),
            synthetic_span(stage.span, 1),
        );
        return Some(Form::new(
            FormKind::List(vec![get_sym, base, items[1].clone()]),
            stage.span,
        ));
    }
    if head == OOP_BARE_SYM {
        if items.len() != 2 {
            return None;
        }
        let method = items[1].clone();
        let method_name = match &method.kind {
            FormKind::Symbol(sym) => sym.as_str(),
            _ => return None,
        };
        let args = Vec::new();
        if matches!(method_name, "repl" | "debug") {
            return Some(build_oop_repl_stage_form(
                stage.span,
                method_name,
                args,
                base,
            ));
        }
        let subject_pos = oop_subject_pos(method_name, args.len());
        return Some(build_oop_call_form(
            stage.span,
            method,
            args,
            base,
            subject_pos,
        ));
    }
    let method = items[0].clone();
    let method_name = match &method.kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    let args = items[1..].to_vec();
    if matches!(method_name, "repl" | "debug") {
        return Some(build_oop_repl_stage_form(
            stage.span,
            method_name,
            args,
            base,
        ));
    }
    let subject_pos = oop_subject_pos(method_name, args.len());
    Some(build_oop_call_form(
        stage.span,
        method,
        args,
        base,
        subject_pos,
    ))
}

fn build_oop_nil_safe_chain(base: Form, stages: &[Form]) -> Option<Form> {
    build_oop_nil_safe_chain_step(base, stages, 0)
}

fn build_oop_nil_safe_chain_step(base: Form, stages: &[Form], idx: usize) -> Option<Form> {
    let (stage, rest) = stages.split_first()?;
    let tmp_name = format!("__oop_nil_safe{}_{}", base.span.index, idx);
    let tmp_sym = Form::new(FormKind::Symbol(tmp_name.clone()), stage.span);
    let next_form = if let Some(sym) = parse_oop_as_stage(stage) {
        let rest_form = if rest.is_empty() {
            sym.clone()
        } else {
            build_oop_nil_safe_chain_step(sym.clone(), rest, idx + 1)?
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
        let stage_form = if let Some((repl_name, sym_name)) = parse_oop_repl_stash_stage(stage) {
            let tmp_let = format!("__oop_let{}_{}", base.span.index, idx);
            let stashed = build_oop_let_stash_form(stage.span, &sym_name, tmp_sym.clone(), tmp_let);
            build_oop_repl_stage_form(stage.span, &repl_name, Vec::new(), stashed)
        } else if let Some(sym_name) = parse_oop_let_stage(stage) {
            let tmp_let = format!("__oop_let{}_{}", base.span.index, idx);
            build_oop_let_stash_form(stage.span, &sym_name, tmp_sym.clone(), tmp_let)
        } else {
            build_oop_stage_form(stage, tmp_sym.clone())?
        };
        if rest.is_empty() {
            stage_form
        } else {
            build_oop_nil_safe_chain_step(stage_form, rest, idx + 1)?
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
    Some(Form::new(
        FormKind::List(vec![
            Form::new(FormKind::Symbol("let".into()), stage.span),
            binding_vec,
            if_form,
        ]),
        stage.span,
    ))
}

fn oop_subject_pos(method_name: &str, args_len: usize) -> usize {
    let policy = fn_meta::get(method_name)
        .and_then(|meta| meta.subject_pos.clone())
        .unwrap_or(SubjectPos::Fixed(1));
    match policy {
        SubjectPos::Fixed(n) => n.max(1),
        SubjectPos::Last => args_len + 1,
    }
}

fn build_oop_call_form(
    span: Span,
    method: Form,
    args: Vec<Form>,
    base: Form,
    subject_pos: usize,
) -> Form {
    let mut items = Vec::with_capacity(args.len() + 2);
    items.push(method);
    let insert_idx = subject_pos.saturating_sub(1);
    let mut inserted = false;
    for (idx, arg) in args.into_iter().enumerate() {
        if idx == insert_idx {
            items.push(base.clone());
            inserted = true;
        }
        items.push(arg);
    }
    if !inserted {
        items.push(base);
    }
    Form::new(FormKind::List(items), span)
}

fn infer_doto(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let target_ty = infer_form(&items[1], st)?;
    for step in items.iter().skip(2) {
        match &step.kind {
            FormKind::List(list) if !list.is_empty() => {
                let head = &list[0];
                let mut arg_tys = Vec::with_capacity(list.len());
                arg_tys.push(st.subst.apply(&target_ty));
                for arg in list.iter().skip(1) {
                    arg_tys.push(infer_form(arg, st)?);
                }
                infer_call_with_args(head, arg_tys, st)?;
            }
            _ => {
                infer_call_with_args(step, vec![st.subst.apply(&target_ty)], st)?;
            }
        }
    }
    Ok(st.subst.apply(&target_ty))
}

fn infer_condp(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let pred_form = &items[1];
    let expr_form = &items[2];
    infer_form(pred_form, st)?;
    let expr_ty = infer_form(expr_form, st)?;
    let mut idx = 3;
    let mut result: Option<Type> = None;
    let mut saw_else = false;
    while idx < items.len() {
        let test_form = &items[idx];
        if is_else_form(test_form) {
            saw_else = true;
            if let Some(result_form) = items.get(idx + 1) {
                let ty = infer_form(result_form, st)?;
                result = Some(join_branch_types(result, ty, st, result_form.span)?);
            }
            break;
        }
        if idx + 1 >= items.len() {
            break;
        }
        let test_ty = infer_form(test_form, st)?;
        let pred_result_ty = infer_call_with_args(
            pred_form,
            vec![st.subst.apply(&test_ty), st.subst.apply(&expr_ty)],
            st,
        )?;
        let next_form = &items[idx + 1];
        let branch_ty = if matches!(&next_form.kind, FormKind::Keyword(kw) if kw == ">>") {
            if let Some(transform_form) = items.get(idx + 2) {
                infer_call_with_args(transform_form, vec![pred_result_ty], st)?
            } else {
                Type::Any
            }
        } else {
            infer_form(next_form, st)?
        };
        result = Some(join_branch_types(result, branch_ty, st, next_form.span)?);
        if matches!(&next_form.kind, FormKind::Keyword(kw) if kw == ">>") {
            idx += 3;
        } else {
            idx += 2;
        }
    }
    let mut out = result.unwrap_or(Type::Prim(PrimType::Nil));
    if !saw_else {
        let nil_ty = Type::Prim(PrimType::Nil);
        if let Some(joined) = join_if_branches(&out, &nil_ty, st, items[0].span)? {
            out = st.subst.apply(&joined);
        } else if st.try_unify(&out, &nil_ty, items[0].span, true)? {
            out = st.subst.apply(&out);
        } else {
            out = Type::Any;
        }
    }
    Ok(out)
}

fn infer_try(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let args = &items[1..];
    let has_catch_finally = args.iter().any(|form| match &form.kind {
        FormKind::List(list) => match list.first().map(|f| &f.kind) {
            Some(FormKind::Symbol(sym)) => sym == "catch" || sym == "finally",
            _ => false,
        },
        _ => false,
    });
    if !has_catch_finally {
        if matches!(args[0].kind, FormKind::Vector(_)) {
            let err_fin = parse_err_fin_tail(&args[1..], "try body")
                .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?;
            if let Some(tail) = err_fin {
                let saved = st.env.snapshot_bindings();
                let result = (|| {
                    let pairs = parse_binding_pairs(&args[0]);
                    for (binding, value_form) in pairs {
                        let val_ty = infer_form(&value_form, st)?;
                        bind_pattern_with_type(&binding, &val_ty, st, true)?;
                    }
                    infer_try_err_fin(tail, st)
                })();
                st.env.restore_bindings(saved);
                return result;
            }
            if let Err(err) = validate_try_bindings(&args[0]) {
                return Err(
                    TypeError::new(format_try_error_message(&err)).with_span(Some(items[0].span))
                );
            }
            if args.len() == 4 {
                return infer_short_try_with_bindings(&args[0], &args[1], &args[2], &args[3], st);
            }
        }
        let err_fin = parse_err_fin_tail(args, "try body")
            .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?;
        if let Some(tail) = err_fin {
            return infer_try_err_fin(tail, st);
        }
        let plan = parse_try_short(args).map_err(|err| {
            TypeError::new(format_try_error_message(&err)).with_span(Some(items[0].span))
        })?;
        return infer_try_short_plan(plan, st);
    }
    if let Some(_tail) = parse_err_fin_tail(args, "try body")
        .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?
    {
        return Err(
            TypeError::new("err/fin cannot be combined with catch/finally")
                .with_span(Some(items[0].span)),
        );
    }
    let mut body_forms = Vec::new();
    let mut catch_clauses: Vec<(Form, Vec<Form>)> = Vec::new();
    let mut finally_clause: Option<Vec<Form>> = None;
    let mut saw_handler = false;
    for form in args {
        if let FormKind::List(list) = &form.kind {
            if let Some(FormKind::Symbol(tag)) = list.first().map(|f| &f.kind) {
                match tag.as_str() {
                    "catch" => {
                        if list.len() >= 3 {
                            let (binding_idx, body_idx) =
                                if list.len() >= 4 { (2, 3) } else { (1, 2) };
                            let binding = list[binding_idx].clone();
                            let body = list[body_idx..].to_vec();
                            catch_clauses.push((binding, body));
                            saw_handler = true;
                            continue;
                        }
                    }
                    "finally" => {
                        if list.len() >= 2 {
                            finally_clause = Some(list[1..].to_vec());
                            saw_handler = true;
                            continue;
                        }
                    }
                    _ => {}
                }
            }
        }
        if saw_handler && st.best_effort {
            st.record_diag(
                form.span,
                "no body forms allowed after catch/finally".to_string(),
            );
        }
        body_forms.push(form.clone());
    }
    let mut result_ty = infer_body(&body_forms, st)?;
    for (binding, catch_body) in catch_clauses {
        let saved = st.env.snapshot_bindings();
        let catch_result = (|| {
            bind_pattern_with_type(&binding, &Type::Any, st, true)?;
            infer_body(&catch_body, st)
        })();
        st.env.restore_bindings(saved);
        let catch_ty = catch_result?;
        result_ty = join_branch_types(Some(result_ty), catch_ty, st, items[0].span)?;
    }
    if let Some(finally_body) = finally_clause {
        infer_body(&finally_body, st)?;
    }
    Ok(result_ty)
}

fn infer_try_err_fin(
    tail: crate::try_form::ErrFinTail,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    if tail.body.is_empty() {
        return Err(TypeError::new("try expects body"));
    }
    let err_handler = if let Some(err_form) = tail.err.as_ref() {
        Some(
            build_err_clause_handler(err_form)
                .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?,
        )
    } else {
        None
    };
    let fin_handler = if let Some(fin_form) = tail.fin.as_ref() {
        Some(
            build_fin_clause_handler(fin_form)
                .map_err(|err| TypeError::new(err.message).with_span(Some(err.span)))?,
        )
    } else {
        None
    };
    match (err_handler, fin_handler) {
        (Some(err_form), fin_form) => {
            infer_short_try_handler(&tail.body, &err_form, fin_form.as_ref(), st)
        }
        (None, Some(fin_form)) => {
            let body_ty = infer_body(&tail.body, st)?;
            infer_call_with_args(&fin_form, Vec::new(), st)?;
            Ok(body_ty)
        }
        (None, None) => infer_body(&tail.body, st),
    }
}

fn infer_short_try_handler(
    body_forms: &[Form],
    handler_form: &Form,
    finally_form: Option<&Form>,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let expr_ty = infer_body(body_forms, st)?;
    let handler_ty = infer_call_with_args(handler_form, vec![Type::Any], st)?;
    if let Some(finally_form) = finally_form {
        infer_call_with_args(finally_form, Vec::new(), st)?;
    }
    let span = body_forms
        .first()
        .map(|f| f.span)
        .unwrap_or(handler_form.span);
    join_branch_types(Some(expr_ty), handler_ty, st, span)
}

fn infer_short_try_with_bindings(
    bindings_form: &Form,
    expr: &Form,
    handler_form: &Form,
    finally_form: &Form,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let pairs = parse_binding_pairs(bindings_form);
        for (binding, value_form) in pairs {
            let val_ty = infer_form(&value_form, st)?;
            bind_pattern_with_type(&binding, &val_ty, st, true)?;
        }
        infer_short_try_handler(&[expr.clone()], handler_form, Some(finally_form), st)
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_try_short_plan(plan: TryShortPlan, st: &mut InferState) -> Result<Type, TypeError> {
    match plan {
        TryShortPlan::Handler { body, on_error } => {
            infer_short_try_handler(&body, &on_error, None, st)
        }
        TryShortPlan::Finally { body, on_finally } => {
            let body_ty = infer_body(&body, st)?;
            infer_call_with_args(&on_finally, Vec::new(), st)?;
            Ok(body_ty)
        }
        TryShortPlan::HandlerFinally {
            body,
            on_error,
            on_finally,
        } => {
            let ty = infer_short_try_handler(&body, &on_error, Some(&on_finally), st)?;
            Ok(ty)
        }
    }
}

fn infer_catch_clause(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let (binding_idx, body_idx) = if items.len() >= 4 { (2, 3) } else { (1, 2) };
    let binding = &items[binding_idx];
    let saved = st.env.snapshot_bindings();
    let body_result = (|| {
        bind_pattern_with_type(binding, &Type::Any, st, true)?;
        infer_body(&items[body_idx..], st)
    })();
    st.env.restore_bindings(saved);
    body_result
}

fn infer_finally_clause(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    infer_body(&items[1..], st)?;
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_throw(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for form in items.iter().skip(1) {
        infer_form(form, st)?;
    }
    Ok(Type::Any)
}

fn infer_recur(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for form in items.iter().skip(1) {
        infer_form(form, st)?;
    }
    Ok(Type::Any)
}

fn infer_delay(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    infer_body(&items[1..], st)?;
    Ok(Type::Any)
}

fn infer_eval(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for form in items.iter().skip(1) {
        infer_form(form, st)?;
    }
    Ok(Type::Any)
}

fn infer_simple_any(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for form in items.iter().skip(1) {
        infer_form(form, st)?;
    }
    Ok(Type::Any)
}

fn infer_simple_nil(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for form in items.iter().skip(1) {
        infer_form(form, st)?;
    }
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_simple_nil_no_eval(_items: &[Form], _st: &mut InferState) -> Result<Type, TypeError> {
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_comment(_items: &[Form], _st: &mut InferState) -> Result<Type, TypeError> {
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_match(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let target_ty = infer_form(&items[1], st)?;
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let mut idx = 2;
        let mut result: Option<Type> = None;
        while idx < items.len() {
            let pattern_form = &items[idx];
            idx += 1;
            if idx >= items.len() {
                break;
            }
            let mut guard_form: Option<Form> = None;
            let mut as_binding: Option<String> = None;
            while idx + 1 < items.len() {
                match &items[idx].kind {
                    FormKind::Keyword(kw) if kw == "when" || kw == "if" => {
                        guard_form = Some(items[idx + 1].clone());
                        idx += 2;
                        continue;
                    }
                    FormKind::Keyword(kw) if kw == "as" => {
                        if let FormKind::Symbol(sym) = &items[idx + 1].kind {
                            as_binding = Some(sym.clone());
                        }
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
            let expr_form = &items[idx];
            idx += 1;
            let target_applied = st.subst.apply(&target_ty);
            if let Some(lit_ty) = pattern_literal_type(pattern_form) {
                st.try_unify(&target_applied, &lit_ty, pattern_form.span, true)?;
            }
            st.env.restore_bindings(saved.clone());
            bind_pattern_with_type(pattern_form, &target_applied, st, true)?;
            if let Some(name) = as_binding {
                st.env.insert(
                    name,
                    Scheme {
                        vars: vec![],
                        ty: target_applied.clone(),
                    },
                );
            }
            if let Some(guard) = guard_form {
                infer_form(&guard, st)?;
            }
            let expr_ty = infer_form(expr_form, st)?;
            result = Some(join_branch_types(result, expr_ty, st, expr_form.span)?);
        }
        Ok(result.unwrap_or(Type::Any))
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_loop(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let bindings_form = &items[1];
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let pairs = parse_binding_pairs(bindings_form);
        for (binding, value_form) in pairs {
            let val_ty = infer_form(&value_form, st)?;
            if let Some((name, hint)) = binding_symbol(&binding, st) {
                let target = if let Some(h) = hint {
                    if st.try_unify(&val_ty, &h, binding.span, true)? {
                        h
                    } else {
                        Type::Any
                    }
                } else {
                    val_ty
                };
                let gen = st.generalize(&st.subst.apply(&target));
                st.env.insert(name, gen);
            }
        }
        let mut last = Type::Prim(PrimType::Nil);
        for body in items.iter().skip(2) {
            last = infer_form(body, st)?;
        }
        Ok(last)
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_go_loop(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    infer_loop(items, st)?;
    Ok(Type::Any)
}

fn infer_scope_loop(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    infer_loop(items, st)
}

fn infer_async_scope(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    if let FormKind::Vector(children) = &items[1].kind {
        for child in children {
            infer_form(child, st)?;
        }
    } else {
        infer_form(&items[1], st)?;
    }
    infer_body(&items[2..], st)?;
    Ok(Type::Any)
}

fn infer_dotimes(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let bindings_form = &items[1];
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let pairs = parse_binding_pairs(bindings_form);
        if let Some((binding, count_form)) = pairs.get(0) {
            let count_ty = infer_form(count_form, st)?;
            st.try_unify(&count_ty, &Type::Prim(PrimType::Int), count_form.span, true)?;
            if let Some((name, hint)) = binding_symbol(binding, st) {
                let mut target = Type::Prim(PrimType::Int);
                if let Some(h) = hint {
                    if st.try_unify(&target, &h, binding.span, true)? {
                        target = st.subst.apply(&target);
                    } else {
                        target = Type::Any;
                    }
                }
                st.env.insert(
                    name,
                    Scheme {
                        vars: vec![],
                        ty: target,
                    },
                );
            }
        }
        for body in items.iter().skip(2) {
            infer_form(body, st)?;
        }
        Ok(Type::Prim(PrimType::Nil))
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_while(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    infer_form(&items[1], st)?;
    for body in items.iter().skip(2) {
        infer_form(body, st)?;
    }
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_doseq(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let bindings_form = &items[1];
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let pairs = parse_binding_pairs(bindings_form);
        for (binding, coll_form) in pairs {
            let coll_ty = infer_form(&coll_form, st)?;
            let elem_ty = seq_elem_type(&coll_ty);
            bind_pattern_with_type(&binding, &elem_ty, st, true)?;
        }
        for body in items.iter().skip(2) {
            infer_form(body, st)?;
        }
        Ok(Type::Prim(PrimType::Nil))
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_each(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Prim(PrimType::Nil));
    }
    let first = &items[1];
    if matches!(first.kind, FormKind::Vector(_)) {
        if items.len() < 3 {
            return Ok(Type::Prim(PrimType::Nil));
        }
        let bindings_form = first;
        let saved = st.env.snapshot_bindings();
        let result = (|| {
            let pairs = parse_binding_pairs(bindings_form);
            if pairs.is_empty() {
                return Ok(Type::Prim(PrimType::Nil));
            }
            let receiver_ty = infer_form(&pairs[0].1, st)?;
            for (binding, coll_form) in pairs {
                let coll_ty = infer_form(&coll_form, st)?;
                let elem_ty = seq_elem_type(&coll_ty);
                bind_pattern_with_type(&binding, &elem_ty, st, true)?;
            }
            for body in items.iter().skip(2) {
                infer_form(body, st)?;
            }
            Ok(st.subst.apply(&receiver_ty))
        })();
        st.env.restore_bindings(saved);
        result
    } else if items.len() == 3 {
        infer_form(first, st)?;
        infer_form(&items[2], st)
    } else {
        Ok(Type::Prim(PrimType::Nil))
    }
}

fn infer_for(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Vector(Box::new(Type::Any)));
    }
    let bindings_form = &items[1];
    let steps = parse_for_steps(bindings_form);
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        for step in steps {
            match step {
                ForStep::Bind(binding, coll_form) => {
                    let coll_ty = infer_form(&coll_form, st)?;
                    let elem_ty = seq_elem_type(&coll_ty);
                    bind_pattern_with_type(&binding, &elem_ty, st, true)?;
                }
                ForStep::Let(pairs) => {
                    for (binding, value_form) in pairs {
                        let val_ty = infer_form(&value_form, st)?;
                        if let Some((name, hint)) = binding_symbol(&binding, st) {
                            let target = if let Some(h) = hint {
                                if st.try_unify(&val_ty, &h, binding.span, true)? {
                                    h
                                } else {
                                    Type::Any
                                }
                            } else {
                                val_ty
                            };
                            let gen = st.generalize(&st.subst.apply(&target));
                            st.env.insert(name, gen);
                        }
                    }
                }
                ForStep::When(form) | ForStep::While(form) => {
                    infer_form(&form, st)?;
                }
            }
        }
        let mut body_ty = Type::Prim(PrimType::Nil);
        for body in items.iter().skip(2) {
            body_ty = infer_form(body, st)?;
        }
        Ok(Type::Vector(Box::new(st.subst.apply(&body_ty))))
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_thread_step(
    step: &Form,
    acc_ty: &Type,
    st: &mut InferState,
    thread_last: bool,
) -> Result<Type, TypeError> {
    match &step.kind {
        FormKind::List(items) => {
            if items.is_empty() {
                return Ok(acc_ty.clone());
            }
            let head = &items[0];
            let mut arg_tys = Vec::new();
            if !thread_last {
                arg_tys.push(st.subst.apply(acc_ty));
            }
            for arg in items.iter().skip(1) {
                arg_tys.push(infer_form(arg, st)?);
            }
            if thread_last {
                arg_tys.push(st.subst.apply(acc_ty));
            }
            infer_call_with_args(head, arg_tys, st)
        }
        _ => infer_call_with_args(step, vec![st.subst.apply(acc_ty)], st),
    }
}

fn infer_call_with_args(
    callee: &Form,
    arg_tys: Vec<Type>,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let callee_ty = infer_form(callee, st)?;
    let ret_ty = st.fresh_var();
    let expected = Type::Func(arg_tys, None, Box::new(ret_ty.clone()));
    let expected_applied = st.subst.apply(&expected);
    let callee_applied = st.subst.apply(&callee_ty);
    if is_regex_type(&callee_applied) {
        return Ok(Type::Any);
    }
    if let Type::Overloaded(candidates) = callee_applied {
        for cand in candidates {
            if let Ok(s) = unify(&cand, &expected_applied) {
                st.subst = s.compose(&st.subst);
                return Ok(st.subst.apply(&ret_ty));
            }
        }
        return Ok(Type::Any);
    }
    if st.try_unify(&callee_applied, &expected_applied, callee.span, true)? {
        Ok(st.subst.apply(&ret_ty))
    } else {
        Ok(Type::Any)
    }
}

#[derive(Clone, Debug)]
enum ForStep {
    Bind(Form, Form),
    Let(Vec<(Form, Form)>),
    When(Form),
    While(Form),
}

fn parse_for_steps(form: &Form) -> Vec<ForStep> {
    let mut steps = Vec::new();
    let FormKind::Vector(items) = &form.kind else {
        return steps;
    };
    let mut idx = 0;
    while idx < items.len() {
        let binding = &items[idx];
        match &binding.kind {
            FormKind::Keyword(kw) if kw == "let" => {
                if let Some(binds) = items.get(idx + 1) {
                    steps.push(ForStep::Let(parse_binding_pairs(binds)));
                }
                idx += 2;
            }
            FormKind::Keyword(kw) if kw == "when" => {
                if let Some(test) = items.get(idx + 1) {
                    steps.push(ForStep::When(test.clone()));
                }
                idx += 2;
            }
            FormKind::Keyword(kw) if kw == "while" => {
                if let Some(test) = items.get(idx + 1) {
                    steps.push(ForStep::While(test.clone()));
                }
                idx += 2;
            }
            _ => {
                if let Some(coll_form) = items.get(idx + 1) {
                    steps.push(ForStep::Bind(binding.clone(), coll_form.clone()));
                }
                idx += 2;
            }
        }
    }
    steps
}

fn infer_with_bindings(
    bindings_form: &Form,
    body: &[Form],
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let saved = st.env.snapshot_bindings();
    let result = (|| {
        let pairs = parse_binding_pairs(bindings_form);
        for (binding, value_form) in pairs {
            let val_ty = infer_form(&value_form, st)?;
            bind_pattern_with_type(&binding, &val_ty, st, true)?;
        }
        infer_body(body, st)
    })();
    st.env.restore_bindings(saved);
    result
}

fn infer_with_open(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    infer_with_bindings(&items[1], &items[2..], st)
}

fn infer_with_dyn(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    infer_with_bindings(&items[1], &items[2..], st)
}

fn infer_with_redefs(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    if matches!(items[1].kind, FormKind::Vector(_)) {
        infer_with_bindings(&items[1], &items[2..], st)
    } else {
        infer_form(&items[1], st)?;
        infer_body(&items[2..], st)
    }
}

fn parse_binding_pairs(form: &Form) -> Vec<(Form, Form)> {
    let mut out = Vec::new();
    let FormKind::Vector(items) = &form.kind else {
        return out;
    };
    let mut idx = 0;
    while idx + 1 < items.len() {
        out.push((items[idx].clone(), items[idx + 1].clone()));
        idx += 2;
    }
    out
}

fn binding_symbol(form: &Form, st: &InferState) -> Option<(String, Option<Type>)> {
    if matches!(form.kind, FormKind::Symbol(_)) {
        Some(symbol_base_and_hint(form, st))
    } else {
        None
    }
}

fn seq_elem_type(ty: &Type) -> Type {
    match ty {
        Type::Vector(inner) => *inner.clone(),
        Type::Option(inner) => seq_elem_type(inner),
        _ => Type::Any,
    }
}

fn is_else_form(form: &Form) -> bool {
    matches!(&form.kind, FormKind::Keyword(kw) if kw == "else")
        || matches!(&form.kind, FormKind::Symbol(sym) if sym == ":else")
}

fn join_branch_types(
    current: Option<Type>,
    next: Type,
    st: &mut InferState,
    span: Span,
) -> Result<Type, TypeError> {
    match current {
        None => Ok(next),
        Some(current) => {
            let current_applied = st.subst.apply(&current);
            let next_applied = st.subst.apply(&next);
            if let Some(joined) = join_if_branches(&current_applied, &next_applied, st, span)? {
                Ok(st.subst.apply(&joined))
            } else if st.try_unify(&current_applied, &next_applied, span, true)? {
                Ok(st.subst.apply(&current_applied))
            } else {
                Ok(Type::Any)
            }
        }
    }
}

fn pattern_literal_type(form: &Form) -> Option<Type> {
    match &form.kind {
        FormKind::Int(_) => Some(Type::Prim(PrimType::Int)),
        FormKind::Float(_) => Some(Type::Prim(PrimType::Float)),
        FormKind::Bool(_) => Some(Type::Prim(PrimType::Bool)),
        FormKind::String(_) => Some(Type::Prim(PrimType::Str)),
        FormKind::Nil => Some(Type::Prim(PrimType::Nil)),
        FormKind::List(items) if is_match_or_form(items) => {
            let mut types = Vec::new();
            for item in items.iter().skip(1) {
                let ty = pattern_literal_type(item)?;
                types.push(ty);
            }
            let first = types.first()?.clone();
            if types.iter().all(|ty| *ty == first) {
                Some(first)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_match_or_form(items: &[Form]) -> bool {
    matches!(
        items.first().map(|form| &form.kind),
        Some(FormKind::Symbol(sym)) if sym == MATCH_OR_SYM
    )
}

fn type_pattern_type(items: &[Form], st: &InferState) -> Option<Type> {
    let head = items.first()?;
    let name = match &head.kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    st.lookup_named_type(name)
}

fn bind_type_pattern(
    items: &[Form],
    pattern_ty: &Type,
    st: &mut InferState,
) -> Result<(), TypeError> {
    if items.len() == 1 {
        return Ok(());
    }
    let applied = st.subst.apply(pattern_ty);
    if let Some(fields) = keyword_shorthand_fields(&items[1..]) {
        for (name, span) in fields {
            let field_ty = match &applied {
                Type::Record(record_fields) => {
                    record_fields.get(&name).cloned().unwrap_or(Type::Any)
                }
                _ => Type::Any,
            };
            let bind_form = Form::new(FormKind::Symbol(name), span);
            bind_pattern_with_type(&bind_form, &field_ty, st, false)?;
        }
        return Ok(());
    }
    if let Some(pairs) = keyword_pair_fields(&items[1..]) {
        for (name, pat_form) in pairs {
            let field_ty = match &applied {
                Type::Record(record_fields) => {
                    record_fields.get(&name).cloned().unwrap_or(Type::Any)
                }
                _ => Type::Any,
            };
            bind_pattern_with_type(&pat_form, &field_ty, st, false)?;
        }
        return Ok(());
    }
    if items.len() == 2 {
        bind_pattern_with_type(&items[1], &applied, st, false)?;
        return Ok(());
    }
    if st.best_effort {
        st.record_diag(
            items[0].span,
            "type pattern accepts at most one inner pattern".into(),
        );
    }
    Ok(())
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

fn keyword_pair_fields(items: &[Form]) -> Option<Vec<(String, Form)>> {
    if items.is_empty() || items.len() % 2 != 0 {
        return None;
    }
    let mut out = Vec::with_capacity(items.len() / 2);
    let mut idx = 0;
    while idx + 1 < items.len() {
        let key = match &items[idx].kind {
            FormKind::Keyword(name) => name.clone(),
            _ => return None,
        };
        let value = items[idx + 1].clone();
        out.push((key, value));
        idx += 2;
    }
    Some(out)
}

fn strip_option_type(ty: &Type) -> Type {
    match ty {
        Type::Option(inner) => strip_option_type(inner),
        _ => ty.clone(),
    }
}

fn record_field_type(
    fields: &HashMap<String, Type>,
    key: &Form,
    allow_symbol: bool,
) -> Option<Type> {
    match &key.kind {
        FormKind::Keyword(name) => fields.get(name).cloned(),
        FormKind::Symbol(name) if allow_symbol => fields.get(name).cloned(),
        _ => None,
    }
}

fn bind_pattern_with_type(
    pattern: &Form,
    ty: &Type,
    st: &mut InferState,
    _top_level: bool,
) -> Result<(), TypeError> {
    let applied = st.subst.apply(ty);
    match &pattern.kind {
        FormKind::Symbol(sym) => {
            if sym == "_" || sym == "&" {
                return Ok(());
            }
            let (name, hint) = symbol_base_and_hint(pattern, st);
            let mut target = applied.clone();
            if let Some(h) = hint {
                if st.try_unify(&target, &h, pattern.span, true)? {
                    target = st.subst.apply(&target);
                } else {
                    target = Type::Any;
                }
            }
            st.env.insert(
                name,
                Scheme {
                    vars: vec![],
                    ty: target,
                },
            );
        }
        FormKind::Vector(items) => {
            let seq_ty = strip_option_type(&applied);
            let (elem_ty, tuple_items) = match &seq_ty {
                Type::Vector(inner) => (inner.as_ref().clone(), None),
                Type::Tuple(items) => (Type::Any, Some(items.clone())),
                _ => (Type::Any, None),
            };
            let mut idx = 0;
            let mut pos = 0;
            while idx < items.len() {
                if matches!(&items[idx].kind, FormKind::Symbol(sym) if sym == "&") {
                    if let Some(rest) = items.get(idx + 1) {
                        let rest_ty = Type::Vector(Box::new(elem_ty.clone()));
                        bind_pattern_with_type(rest, &rest_ty, st, false)?;
                    }
                    idx += 2;
                    continue;
                }
                if matches!(&items[idx].kind, FormKind::Keyword(kw) if kw == "as") {
                    if let Some(alias) = items.get(idx + 1) {
                        bind_pattern_with_type(alias, &applied, st, false)?;
                    }
                    idx += 2;
                    continue;
                }
                let item_ty = if let Some(tuple_items) = &tuple_items {
                    tuple_items.get(pos).cloned().unwrap_or(Type::Any)
                } else {
                    elem_ty.clone()
                };
                bind_pattern_with_type(&items[idx], &item_ty, st, false)?;
                idx += 1;
                pos += 1;
            }
        }
        FormKind::List(items) => {
            if is_match_or_form(items) {
                for item in &items[1..] {
                    bind_pattern_with_type(item, &applied, st, false)?;
                }
                return Ok(());
            }
            if let Some(type_ty) = type_pattern_type(items, st) {
                let _ = st.try_unify(&applied, &type_ty, pattern.span, true)?;
                let unified = st.subst.apply(&type_ty);
                bind_type_pattern(items, &unified, st)?;
                return Ok(());
            }
            if matches!(
                items.first().map(|form| &form.kind),
                Some(FormKind::Symbol(_))
            ) && items.len() <= 2
            {
                if let Some(inner) = items.get(1) {
                    bind_pattern_with_type(inner, &applied, st, false)?;
                }
                return Ok(());
            }
            let seq_ty = strip_option_type(&applied);
            let (elem_ty, tuple_items) = match &seq_ty {
                Type::Vector(inner) => (inner.as_ref().clone(), None),
                Type::Tuple(items) => (Type::Any, Some(items.clone())),
                _ => (Type::Any, None),
            };
            let mut pos = 0;
            for item in items {
                let item_ty = if let Some(tuple_items) = &tuple_items {
                    tuple_items.get(pos).cloned().unwrap_or(Type::Any)
                } else {
                    elem_ty.clone()
                };
                bind_pattern_with_type(item, &item_ty, st, false)?;
                pos += 1;
            }
        }
        FormKind::Map(entries) => {
            let map_ty = strip_option_type(&applied);
            let mut idx = 0;
            while idx < entries.len() {
                if let MapItem::KeyValue(k_form, pat_form) = &entries[idx] {
                    if matches!(&pat_form.kind, FormKind::Keyword(kw) if kw == "as") {
                        if let Some(MapItem::KeyValue(next_k_form, next_pat_form)) =
                            entries.get(idx + 1)
                        {
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
                                        FormKind::Keyword(name) | FormKind::Symbol(name) => {
                                            Some(name.clone())
                                        }
                                        _ => None,
                                    };
                                    if let Some(name) = bind_name {
                                        let val_ty = match &map_ty {
                                            Type::Record(fields) => {
                                                record_field_type(fields, k_form, false)
                                                    .unwrap_or(Type::Any)
                                            }
                                            Type::Map(_, v) => (**v).clone(),
                                            _ => Type::Any,
                                        };
                                        let bind_form =
                                            Form::new(FormKind::Symbol(name), k_form.span);
                                        bind_pattern_with_type(&bind_form, &val_ty, st, false)?;
                                    }
                                    let alias_form =
                                        Form::new(FormKind::Symbol(alias), next_k_form.span);
                                    bind_pattern_with_type(&alias_form, &applied, st, false)?;
                                    idx += 2;
                                    continue;
                                }
                            }
                        }
                    }
                }
                match &entries[idx] {
                    MapItem::KeyValue(k, v) => {
                        if let FormKind::Keyword(kw) = &k.kind {
                            if kw == "as" {
                                bind_pattern_with_type(v, &applied, st, false)?;
                                idx += 1;
                                continue;
                            }
                            if kw == "keys" {
                                if let FormKind::Vector(items) = &v.kind {
                                    for item in items {
                                        let key_ty = match &map_ty {
                                            Type::Record(fields) => {
                                                record_field_type(fields, item, true)
                                                    .unwrap_or(Type::Any)
                                            }
                                            Type::Map(_, v) => (**v).clone(),
                                            _ => Type::Any,
                                        };
                                        match &item.kind {
                                            FormKind::Symbol(_) => {
                                                bind_pattern_with_type(item, &key_ty, st, false)?;
                                            }
                                            FormKind::Keyword(name) => {
                                                let bind_form = Form::new(
                                                    FormKind::Symbol(name.clone()),
                                                    item.span,
                                                );
                                                bind_pattern_with_type(
                                                    &bind_form, &key_ty, st, false,
                                                )?;
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                idx += 1;
                                continue;
                            }
                        }
                        let val_ty = match &map_ty {
                            Type::Record(fields) => {
                                record_field_type(fields, k, false).unwrap_or(Type::Any)
                            }
                            Type::Map(_, v) => (**v).clone(),
                            _ => Type::Any,
                        };
                        bind_pattern_with_type(v, &val_ty, st, false)?;
                    }
                    MapItem::Spread(form) => {
                        bind_pattern_with_type(form, &Type::Any, st, false)?;
                    }
                }
                idx += 1;
            }
        }
        FormKind::Set(items) => {
            for item in items {
                bind_pattern_with_type(item, &Type::Any, st, false)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn join_if_branches(
    then_ty: &Type,
    else_ty: &Type,
    st: &mut InferState,
    span: Span,
) -> Result<Option<Type>, TypeError> {
    if then_ty == else_ty {
        return Ok(Some(then_ty.clone()));
    }
    if is_nil_type(then_ty) {
        return Ok(Some(Type::Option(Box::new(else_ty.clone()))));
    }
    if is_nil_type(else_ty) {
        return Ok(Some(Type::Option(Box::new(then_ty.clone()))));
    }
    if let (Type::Option(inner), other) = (then_ty, else_ty) {
        if !matches!(other, Type::Option(_)) {
            if st.try_unify(inner, other, span, true)? {
                let joined_inner = st.subst.apply(inner);
                return Ok(Some(Type::Option(Box::new(joined_inner))));
            }
            return Ok(None);
        }
    }
    if let (other, Type::Option(inner)) = (then_ty, else_ty) {
        if st.try_unify(inner, other, span, true)? {
            let joined_inner = st.subst.apply(inner);
            return Ok(Some(Type::Option(Box::new(joined_inner))));
        }
        return Ok(None);
    }
    Ok(None)
}

fn is_nil_type(ty: &Type) -> bool {
    matches!(ty, Type::Prim(PrimType::Nil))
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

fn infer_do(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    infer_body(&items[1..], st)
}

fn infer_where(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for it in items.iter().skip(1) {
        let _ = infer_form(it, st)?;
    }
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_p(items: &[Form], span: Span, st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    if items.len() >= 3 {
        return infer_call(&items[1..], span, st);
    }
    infer_form(&items[1], st)
}

fn infer_and_or(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() <= 1 {
        return Ok(Type::Prim(PrimType::Bool));
    }
    for expr in items.iter().skip(1) {
        let t = infer_form(expr, st)?;
        st.try_unify(&t, &Type::Prim(PrimType::Bool), expr.span, true)?;
    }
    Ok(Type::Prim(PrimType::Bool))
}

fn infer_numeric_target(
    items: &[Form],
    st: &mut InferState,
    force_float: bool,
) -> Result<Type, TypeError> {
    let mut arg_tys: Vec<(Type, Span)> = Vec::new();
    let mut saw_float = force_float;
    for expr in items.iter().skip(1) {
        let t = infer_form(expr, st)?;
        let applied = st.subst.apply(&t);
        if matches!(applied, Type::Prim(PrimType::Float)) {
            saw_float = true;
        }
        arg_tys.push((t, expr.span));
    }
    let target = if saw_float {
        Type::Prim(PrimType::Float)
    } else {
        Type::Prim(PrimType::Int)
    };
    let allow_float_target = matches!(target, Type::Prim(PrimType::Float));
    for (t, span) in arg_tys {
        let applied = st.subst.apply(&t);
        match applied {
            Type::Prim(PrimType::Int) | Type::Var(_) | Type::Any if allow_float_target => {}
            Type::Prim(PrimType::Int) | Type::Prim(PrimType::Float) | Type::Var(_) | Type::Any => {
                st.try_unify(&t, &target, span, true)?;
            }
            _ => {
                st.try_unify(&t, &target, span, true)?;
            }
        }
    }
    Ok(target)
}

fn infer_vararg_numeric(items: &[Form], st: &mut InferState, op: &str) -> Result<Type, TypeError> {
    infer_numeric_target(items, st, op == "/")
}

fn infer_vararg_compare(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() > 1 {
        infer_numeric_target(items, st, false)?;
    }
    Ok(Type::Prim(PrimType::Bool))
}

fn infer_vararg_eq(items: &[Form], st: &mut InferState, _op: &str) -> Result<Type, TypeError> {
    if items.len() <= 1 {
        return Ok(Type::Prim(PrimType::Bool));
    }
    // unify all args to a common type, but return Bool
    let mut last_ty = infer_form(&items[1], st)?;
    for expr in items.iter().skip(2) {
        let t = infer_form(expr, st)?;
        st.try_unify(&last_ty, &t, expr.span, true)?;
        last_ty = st.subst.apply(&last_ty);
    }
    Ok(Type::Prim(PrimType::Bool))
}

fn infer_vararg_str(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for expr in items.iter().skip(1) {
        infer_form(expr, st)?;
    }
    Ok(Type::Prim(PrimType::Str))
}

fn infer_pp(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let target_ty = infer_form(&items[1], st)?;
    if items.len() > 2 {
        infer_form(&items[2], st)?;
    }
    Ok(st.subst.apply(&target_ty))
}

fn infer_pp_str(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() > 1 {
        infer_form(&items[1], st)?;
    }
    if items.len() > 2 {
        infer_form(&items[2], st)?;
    }
    Ok(Type::Prim(PrimType::Str))
}

fn infer_vararg_nil(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for expr in items.iter().skip(1) {
        infer_form(expr, st)?;
    }
    Ok(Type::Prim(PrimType::Nil))
}

fn infer_vararg_extremum(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() <= 1 {
        return Ok(Type::Any);
    }
    infer_numeric_target(items, st, false)
}

fn infer_vector_literal(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let mut elem_ty: Option<Type> = None;
    let mut unify_failed = false;
    for expr in items.iter().skip(1) {
        let t = infer_form(expr, st)?;
        elem_ty = match elem_ty {
            Some(current) => {
                if st.try_unify(&current, &t, expr.span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    unify_failed = true;
                    Some(Type::Any)
                }
            }
            None => Some(t),
        };
    }
    let resolved = if unify_failed {
        Type::Any
    } else {
        elem_ty.unwrap_or(Type::Any)
    };
    Ok(Type::Vector(Box::new(resolved)))
}

fn infer_set_literal(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let mut elem_ty: Option<Type> = None;
    let mut unify_failed = false;
    for expr in items.iter().skip(1) {
        let t = infer_form(expr, st)?;
        elem_ty = match elem_ty {
            Some(current) => {
                if st.try_unify(&current, &t, expr.span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    unify_failed = true;
                    Some(Type::Any)
                }
            }
            None => Some(t),
        };
    }
    let resolved = if unify_failed {
        Type::Any
    } else {
        elem_ty.unwrap_or(Type::Any)
    };
    Ok(Type::Set(Box::new(resolved)))
}

fn infer_hash_map_literal(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    let mut key_ty: Option<Type> = None;
    let mut val_ty: Option<Type> = None;
    let mut iter = items.iter().skip(1);
    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
        let kt = infer_form(k, st)?;
        let vt = infer_form(v, st)?;
        key_ty = match key_ty {
            Some(current) => {
                if st.try_unify(&current, &kt, k.span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    Some(Type::Any)
                }
            }
            None => Some(kt),
        };
        val_ty = match val_ty {
            Some(current) => {
                if st.try_unify(&current, &vt, v.span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    Some(Type::Any)
                }
            }
            None => Some(vt),
        };
    }
    Ok(Type::Map(
        Box::new(key_ty.unwrap_or_else(|| st.fresh_var())),
        Box::new(val_ty.unwrap_or_else(|| st.fresh_var())),
    ))
}

fn infer_get(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let map_ty = infer_form(&items[1], st)?;
    infer_lookup(map_ty, &items[2], items.get(3), st)
}

fn infer_keyword_call(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let map_ty = infer_form(&items[1], st)?;
    infer_lookup(map_ty, &items[0], items.get(2), st)
}

fn infer_lookup(
    map_ty: Type,
    key_form: &Form,
    default_form: Option<&Form>,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let key_ty = infer_form(key_form, st)?;
    match map_ty {
        Type::Option(inner) => {
            let inner_ty = *inner;
            infer_lookup_inner(inner_ty, key_form, &key_ty, default_form, st, true)
        }
        other => infer_lookup_inner(other, key_form, &key_ty, default_form, st, false),
    }
}

fn infer_lookup_inner(
    map_ty: Type,
    key_form: &Form,
    key_ty: &Type,
    default_form: Option<&Form>,
    st: &mut InferState,
    outer_optional: bool,
) -> Result<Type, TypeError> {
    match map_ty {
        Type::Record(fields) => {
            let result = infer_record_lookup(&fields, key_form, default_form, st)?;
            if outer_optional && default_form.is_none() {
                Ok(Type::Option(Box::new(result)))
            } else {
                Ok(result)
            }
        }
        Type::Map(k, v) => {
            st.try_unify(key_ty, &k, key_form.span, true)?;
            let mut val_ty = *v;
            if let Some(default) = default_form {
                let def_ty = infer_form(default, st)?;
                st.try_unify(&val_ty, &def_ty, default.span, true)?;
                val_ty = st.subst.apply(&val_ty);
                return Ok(val_ty);
            }
            Ok(Type::Option(Box::new(st.subst.apply(&val_ty))))
        }
        _ => {
            if let Some(default) = default_form {
                infer_form(default, st)?;
            }
            Ok(Type::Any)
        }
    }
}

fn infer_record_lookup(
    fields: &HashMap<String, Type>,
    key_form: &Form,
    default_form: Option<&Form>,
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let name = match &key_form.kind {
        FormKind::Keyword(name) => name,
        _ => {
            if let Some(default) = default_form {
                infer_form(default, st)?;
            }
            return Ok(Type::Any);
        }
    };
    let Some(field_ty) = fields.get(name) else {
        let message = format!("record missing field :{}", name);
        if st.best_effort {
            st.record_diag(key_form.span, message);
            return Ok(Type::Any);
        }
        return Err(TypeError::new(message).with_span(Some(key_form.span)));
    };
    let mut result = st.subst.apply(field_ty);
    if let Some(default) = default_form {
        let def_ty = infer_form(default, st)?;
        st.try_unify(&result, &def_ty, default.span, true)?;
        result = st.subst.apply(&result);
    }
    Ok(result)
}

fn infer_assoc(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    let target_ty = infer_form(&items[1], st)?;
    if let Type::Record(fields) = target_ty.clone() {
        return infer_assoc_record(fields, false, items, st);
    }
    if let Type::Option(inner) = target_ty.clone() {
        if let Type::Record(fields) = *inner {
            return infer_assoc_record(fields, true, items, st);
        }
    }
    if matches!(target_ty, Type::Any | Type::Var(_)) {
        let mut idx = 2;
        while idx + 1 < items.len() {
            infer_form(&items[idx], st)?;
            infer_form(&items[idx + 1], st)?;
            idx += 2;
        }
        if idx < items.len() {
            infer_form(&items[idx], st)?;
        }
        return Ok(target_ty);
    }
    let mut key_ty: Option<Type> = None;
    let mut val_ty: Option<Type> = None;
    let mut idx = 2;
    while idx + 1 < items.len() {
        let kt = infer_form(&items[idx], st)?;
        let vt = infer_form(&items[idx + 1], st)?;
        key_ty = match key_ty {
            Some(current) => {
                if st.try_unify(&current, &kt, items[idx].span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    Some(Type::Any)
                }
            }
            None => Some(kt),
        };
        val_ty = match val_ty {
            Some(current) => {
                if st.try_unify(&current, &vt, items[idx + 1].span, true)? {
                    Some(st.subst.apply(&current))
                } else {
                    Some(Type::Any)
                }
            }
            None => Some(vt),
        };
        idx += 2;
    }
    if idx < items.len() {
        infer_form(&items[idx], st)?;
    }
    let mut out_key = key_ty.unwrap_or(Type::Any);
    let mut out_val = val_ty.unwrap_or(Type::Any);
    match target_ty {
        Type::Record(_) => {}
        Type::Option(inner) => match *inner {
            Type::Record(_) => {}
            Type::Map(k, v) => {
                st.try_unify(&out_key, &k, items[1].span, true)?;
                st.try_unify(&out_val, &v, items[1].span, true)?;
                out_key = st.subst.apply(&out_key);
                out_val = st.subst.apply(&out_val);
            }
            _ => {}
        },
        Type::Map(k, v) => {
            st.try_unify(&out_key, &k, items[1].span, true)?;
            st.try_unify(&out_val, &v, items[1].span, true)?;
            out_key = st.subst.apply(&out_key);
            out_val = st.subst.apply(&out_val);
        }
        _ => {}
    }
    Ok(Type::Map(Box::new(out_key), Box::new(out_val)))
}

fn infer_assoc_record(
    fields: HashMap<String, Type>,
    outer_optional: bool,
    items: &[Form],
    st: &mut InferState,
) -> Result<Type, TypeError> {
    let mut updated = fields;
    let mut idx = 2;
    while idx + 1 < items.len() {
        let key_form = &items[idx];
        let val_form = &items[idx + 1];
        let val_ty = infer_form(val_form, st)?;
        let key_name = match &key_form.kind {
            FormKind::Keyword(name) => Some(name.clone()),
            FormKind::Symbol(sym) if sym.starts_with(':') => {
                Some(sym.trim_start_matches(':').to_string())
            }
            _ => None,
        };
        if let Some(name) = key_name {
            let Some(current) = updated.get(&name).cloned() else {
                let map_ty = Type::Map(Box::new(Type::Any), Box::new(Type::Any));
                return if outer_optional {
                    Ok(Type::Option(Box::new(map_ty)))
                } else {
                    Ok(map_ty)
                };
            };
            let next_ty = match st.try_unify(&current, &val_ty, val_form.span, true) {
                Ok(true) => st.subst.apply(&current),
                Ok(false) | Err(_) => Type::Any,
            };
            updated.insert(name, next_ty);
        } else {
            infer_form(key_form, st)?;
            if idx + 2 < items.len() {
                infer_form(&items[idx + 2], st)?;
            }
            let map_ty = Type::Map(Box::new(Type::Any), Box::new(Type::Any));
            return if outer_optional {
                Ok(Type::Option(Box::new(map_ty)))
            } else {
                Ok(map_ty)
            };
        }
        idx += 2;
    }
    if idx < items.len() {
        infer_form(&items[idx], st)?;
    }
    let record_ty = Type::Record(updated);
    if outer_optional {
        Ok(Type::Option(Box::new(record_ty)))
    } else {
        Ok(record_ty)
    }
}

fn infer_dissoc(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let target_ty = infer_form(&items[1], st)?;
    for key_form in items.iter().skip(2) {
        infer_form(key_form, st)?;
    }
    match target_ty {
        Type::Record(_) => Ok(Type::Map(Box::new(Type::Any), Box::new(Type::Any))),
        Type::Option(inner) => match *inner {
            Type::Record(_) => Ok(Type::Map(Box::new(Type::Any), Box::new(Type::Any))),
            Type::Map(k, v) => Ok(Type::Option(Box::new(Type::Map(k, v)))),
            _ => Ok(Type::Any),
        },
        Type::Map(k, v) => Ok(Type::Map(k, v)),
        _ => Ok(Type::Any),
    }
}

fn infer_merge(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 2 {
        return Ok(Type::Any);
    }
    let mut key_ty: Option<Type> = None;
    let mut val_ty: Option<Type> = None;
    let mut saw_record = false;
    for form in items.iter().skip(1) {
        let ty = infer_form(form, st)?;
        match ty {
            Type::Record(_) => saw_record = true,
            Type::Option(inner) => match *inner {
                Type::Record(_) => saw_record = true,
                Type::Map(k, v) => {
                    key_ty = match key_ty {
                        Some(current) => {
                            if st.try_unify(&current, &k, form.span, true)? {
                                Some(st.subst.apply(&current))
                            } else {
                                Some(Type::Any)
                            }
                        }
                        None => Some(*k),
                    };
                    val_ty = match val_ty {
                        Some(current) => {
                            if st.try_unify(&current, &v, form.span, true)? {
                                Some(st.subst.apply(&current))
                            } else {
                                Some(Type::Any)
                            }
                        }
                        None => Some(*v),
                    };
                }
                _ => {}
            },
            Type::Map(k, v) => {
                key_ty = match key_ty {
                    Some(current) => {
                        if st.try_unify(&current, &k, form.span, true)? {
                            Some(st.subst.apply(&current))
                        } else {
                            Some(Type::Any)
                        }
                    }
                    None => Some(*k),
                };
                val_ty = match val_ty {
                    Some(current) => {
                        if st.try_unify(&current, &v, form.span, true)? {
                            Some(st.subst.apply(&current))
                        } else {
                            Some(Type::Any)
                        }
                    }
                    None => Some(*v),
                };
            }
            _ => {}
        }
    }
    if saw_record {
        return Ok(Type::Map(Box::new(Type::Any), Box::new(Type::Any)));
    }
    Ok(Type::Map(
        Box::new(key_ty.unwrap_or(Type::Any)),
        Box::new(val_ty.unwrap_or(Type::Any)),
    ))
}

fn infer_reduce(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Any);
    }
    infer_form(&items[1], st)?;
    if items.len() >= 4 {
        let init_ty = infer_form(&items[2], st)?;
        infer_form(&items[3], st)?;
        return Ok(init_ty);
    }
    infer_form(&items[2], st)?;
    Ok(Type::Any)
}

fn infer_subs(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Str));
    }
    let s_ty = infer_form(&items[1], st)?;
    let start_ty = infer_form(&items[2], st)?;
    st.try_unify(&s_ty, &Type::Prim(PrimType::Str), items[1].span, true)?;
    st.try_unify(&start_ty, &Type::Prim(PrimType::Int), items[2].span, true)?;
    if items.len() >= 4 {
        let end_ty = infer_form(&items[3], st)?;
        st.try_unify(&end_ty, &Type::Prim(PrimType::Int), items[3].span, true)?;
    }
    Ok(Type::Prim(PrimType::Str))
}

fn infer_glob(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    for item in items.iter().skip(1) {
        infer_form(item, st)?;
    }
    Ok(Type::Vector(Box::new(Type::Prim(PrimType::Str))))
}

fn infer_filter(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        for item in items.iter().skip(1) {
            infer_form(item, st)?;
        }
        return Ok(Type::Vector(Box::new(Type::Any)));
    }
    let pred_form = &items[1];
    let coll_form = &items[2];
    let coll_ty = infer_form(coll_form, st)?;
    let coll_applied = st.subst.apply(&coll_ty);
    let (elem_ty, ret_ty) = match coll_applied {
        Type::Vector(inner) => {
            let elem = (*inner).clone();
            (elem, Type::Vector(inner))
        }
        Type::Set(inner) => {
            let elem = (*inner).clone();
            (elem, Type::Vector(inner))
        }
        Type::Any | Type::Var(_) => {
            let inner = st.fresh_var();
            let vec_ty = Type::Vector(Box::new(inner.clone()));
            st.try_unify(&coll_ty, &vec_ty, coll_form.span, true)?;
            (inner, vec_ty)
        }
        _ => (Type::Any, Type::Vector(Box::new(Type::Any))),
    };
    infer_call_with_args(pred_form, vec![st.subst.apply(&elem_ty)], st)?;
    Ok(st.subst.apply(&ret_ty))
}

fn infer_map(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        for item in items.iter().skip(1) {
            infer_form(item, st)?;
        }
        return Ok(Type::Vector(Box::new(Type::Any)));
    }
    let transform_form = &items[1];
    let coll_form = &items[2];
    let coll_ty = infer_form(coll_form, st)?;
    let coll_applied = st.subst.apply(&coll_ty);
    let elem_ty = match coll_applied {
        Type::Vector(inner) => (*inner).clone(),
        Type::Set(inner) => (*inner).clone(),
        Type::Any | Type::Var(_) => {
            let inner = st.fresh_var();
            let vec_ty = Type::Vector(Box::new(inner.clone()));
            st.try_unify(&coll_ty, &vec_ty, coll_form.span, true)?;
            inner
        }
        _ => Type::Any,
    };
    let out_ty = infer_call_with_args(transform_form, vec![st.subst.apply(&elem_ty)], st)?;
    Ok(Type::Vector(Box::new(st.subst.apply(&out_ty))))
}

fn infer_json_read_file(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if let Some(path) = items.get(1) {
        let path_ty = infer_form(path, st)?;
        let _ = st.try_unify(&path_ty, &Type::Prim(PrimType::Str), path.span, true)?;
    }
    Ok(Type::Vector(Box::new(Type::Any)))
}

fn infer_json_read_seq(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if let Some(arg) = items.get(1) {
        let _ = infer_form(arg, st)?;
    }
    Ok(Type::Vector(Box::new(Type::Any)))
}

fn infer_contains(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() < 3 {
        return Ok(Type::Prim(PrimType::Bool));
    }
    let coll_ty = infer_form(&items[1], st)?;
    let key_ty = infer_form(&items[2], st)?;
    match coll_ty.clone() {
        Type::Map(k, _) => {
            st.try_unify(&*k, &key_ty, items[2].span, true)?;
        }
        Type::Vector(inner) => {
            st.try_unify(&*inner, &key_ty, items[2].span, true)?;
        }
        Type::Set(inner) => {
            st.try_unify(&*inner, &key_ty, items[2].span, true)?;
        }
        _ => {}
    }
    Ok(Type::Prim(PrimType::Bool))
}

fn infer_set_op(items: &[Form], st: &mut InferState) -> Result<Type, TypeError> {
    if items.len() <= 1 {
        return Ok(Type::Set(Box::new(Type::Any)));
    }
    let mut elem_ty: Option<Type> = None;
    for arg in items.iter().skip(1) {
        let t = infer_form(arg, st)?;
        let inner = match &t {
            Type::Set(inner) => *inner.clone(),
            Type::Vector(inner) => *inner.clone(),
            _ => Type::Any,
        };
        let is_empty_literal =
            matches!(&arg.kind, FormKind::Set(items) | FormKind::Vector(items) if items.is_empty());
        elem_ty = match elem_ty {
            Some(current) => {
                if matches!(inner, Type::Any) && is_empty_literal {
                    Some(current)
                } else {
                    if st.try_unify(&current, &inner, arg.span, true)? {
                        Some(st.subst.apply(&current))
                    } else {
                        Some(Type::Any)
                    }
                }
            }
            None => {
                if matches!(inner, Type::Any) && is_empty_literal {
                    None
                } else {
                    Some(inner)
                }
            }
        };
    }
    Ok(Type::Set(Box::new(elem_ty.unwrap_or(Type::Any))))
}

fn infer_call(items: &[Form], _span: Span, st: &mut InferState) -> Result<Type, TypeError> {
    let head = &items[0];
    let callee_ty = infer_form(head, st)?;
    let mut arg_tys = Vec::new();
    for arg in items.iter().skip(1) {
        arg_tys.push(infer_form(arg, st)?);
    }
    let ret_ty = st.fresh_var();
    let expected = Type::Func(arg_tys, None, Box::new(ret_ty.clone()));
    let expected_applied = st.subst.apply(&expected);
    let callee_applied = st.subst.apply(&callee_ty);
    if is_regex_type(&callee_applied) {
        return Ok(Type::Any);
    }
    if let Type::Overloaded(candidates) = callee_applied {
        let mut last_err = None;
        for cand in candidates {
            let result = unify(&cand, &expected_applied);
            match result {
                Ok(s) => {
                    st.subst = s.compose(&st.subst);
                    return Ok(st.subst.apply(&ret_ty));
                }
                Err(err) => last_err = Some(err),
            }
        }
        let message = last_err
            .map(|err| err.message)
            .unwrap_or_else(|| "no matching overload".to_string());
        if st.best_effort {
            st.record_diag(head.span, message);
            Ok(Type::Any)
        } else {
            Err(TypeError::new(message).with_span(Some(head.span)))
        }
    } else if st.try_unify(&callee_applied, &expected_applied, head.span, true)? {
        Ok(st.subst.apply(&ret_ty))
    } else {
        Ok(Type::Any)
    }
}

fn to_ce(err: TypeError) -> CloveError {
    let msg = err.message;
    let mut out = CloveError::runtime(msg);
    if let Some(span) = err.span {
        out = out.with_span(span);
    }
    out
}

fn symbol_base_and_hint(form: &Form, st: &InferState) -> (String, Option<Type>) {
    let mut base = String::new();
    let mut hint = None;
    if let FormKind::Symbol(raw) = &form.kind {
        if let Some((b, annot)) = split_symbol_type_suffix(raw) {
            base = b.to_string();
            hint = convert_annotation_str_with_lookup(annot, Some(st)).ok();
        } else {
            base = raw.clone();
        }
    }
    if hint.is_none() {
        if let Some(type_hint) = &form.type_hint {
            hint = convert_annotation_with_lookup(type_hint.kind.clone(), Some(st)).ok();
        }
    }
    (base, hint)
}

fn split_symbol_type_suffix(name: &str) -> Option<(&str, &str)> {
    let open = name.find('<')?;
    let close = name.rfind('>')?;
    if close > open {
        let base = &name[..open];
        let annot = &name[open + 1..close];
        Some((base, annot))
    } else {
        None
    }
}

fn is_regex_type(ty: &Type) -> bool {
    matches!(ty, Type::Opaque(name) if name.ends_with("Regex"))
}

fn convert_annotation(kind: TypeKind) -> Result<Type, ()> {
    convert_annotation_with_lookup(kind, None)
}

fn convert_annotation_with_lookup(kind: TypeKind, st: Option<&InferState>) -> Result<Type, ()> {
    let mut seen = HashSet::new();
    convert_annotation_inner(kind, st, &mut seen)
}

fn convert_annotation_inner(
    kind: TypeKind,
    st: Option<&InferState>,
    seen_aliases: &mut HashSet<String>,
) -> Result<Type, ()> {
    Ok(match kind {
        TypeKind::Int => Type::Prim(PrimType::Int),
        TypeKind::Float => Type::Prim(PrimType::Float),
        TypeKind::Bool => Type::Prim(PrimType::Bool),
        TypeKind::Str => Type::Prim(PrimType::Str),
        TypeKind::Nil => Type::Prim(PrimType::Nil),
        TypeKind::Any => Type::Any,
        TypeKind::Vector(inner) => Type::Vector(Box::new(convert_annotation_inner(
            *inner,
            st,
            seen_aliases,
        )?)),
        TypeKind::Tuple(items) => {
            let mut out = Vec::new();
            for item in items {
                out.push(convert_annotation_inner(item, st, seen_aliases)?);
            }
            Type::Tuple(out)
        }
        TypeKind::Map(k, v) => Type::Map(
            Box::new(convert_annotation_inner(*k, st, seen_aliases)?),
            Box::new(convert_annotation_inner(*v, st, seen_aliases)?),
        ),
        TypeKind::Set(inner) => Type::Set(Box::new(convert_annotation_inner(
            *inner,
            st,
            seen_aliases,
        )?)),
        TypeKind::Option(inner) => Type::Option(Box::new(convert_annotation_inner(
            *inner,
            st,
            seen_aliases,
        )?)),
        TypeKind::Mut(inner) => Type::Mut(Box::new(convert_annotation_inner(
            *inner,
            st,
            seen_aliases,
        )?)),
        TypeKind::Record(fields) => {
            let mut m = HashMap::new();
            for (k, v) in fields {
                m.insert(k, convert_annotation_inner(v, st, seen_aliases)?);
            }
            Type::Record(m)
        }
        TypeKind::Union(_) => Type::Any,
        TypeKind::Function { params, rest, ret } => {
            let params = params
                .into_iter()
                .map(|param| convert_annotation_inner(param, st, seen_aliases))
                .collect::<Result<Vec<_>, _>>()?;
            let rest = match rest {
                Some(rest_ty) => {
                    let rest_ty = convert_annotation_inner(*rest_ty, st, seen_aliases)?;
                    let elem_ty = match rest_ty {
                        Type::Vector(inner) => *inner,
                        other => other,
                    };
                    Some(Box::new(elem_ty))
                }
                None => None,
            };
            let ret = convert_annotation_inner(*ret, st, seen_aliases)?;
            Type::Func(params, rest, Box::new(ret))
        }
        TypeKind::Named(name) => {
            if name == "Set" || name == "clove::core::Set" {
                return Ok(Type::Set(Box::new(Type::Any)));
            }
            if let Some(ty) = lookup_named_type_from_state(&name, st) {
                return Ok(ty);
            }
            if let Some(fqn) = type_registry::resolve_alias_name(&name, None) {
                if seen_aliases.insert(fqn.clone()) {
                    if let Some(type_registry::TypeEntry::Alias(meta)) =
                        type_registry::get_type_entry(&fqn)
                    {
                        return convert_annotation_inner(meta.target.clone(), st, seen_aliases);
                    }
                }
            }
            Type::Any
        }
    })
}

fn convert_annotation_str(s: &str) -> Result<Type, ()> {
    convert_annotation_str_with_lookup(s, None)
}

fn convert_annotation_str_with_lookup(s: &str, st: Option<&InferState>) -> Result<Type, ()> {
    let kind = TypeKind::parse(s).map_err(|_| ())?;
    convert_annotation_with_lookup(kind, st)
}

fn lookup_named_type_from_state(name: &str, st: Option<&InferState>) -> Option<Type> {
    match st {
        Some(st) => st.lookup_named_type(name),
        None => lookup_builtin_named_type(name),
    }
}

fn lookup_builtin_named_type(name: &str) -> Option<Type> {
    if plugin_meta::is_opaque_type(name) {
        return Some(Type::Opaque(name.to_string()));
    }
    let canonical = canonicalize_type_name_for_infer(name);
    if canonical == "Json" || canonical == "core::Json" {
        return Some(Type::Opaque("core::Json".to_string()));
    }
    let prim = type_registry::PrimitiveType::from_symbol(&canonical)?;
    Some(match prim {
        type_registry::PrimitiveType::Int => Type::Prim(PrimType::Int),
        type_registry::PrimitiveType::Float => Type::Prim(PrimType::Float),
        type_registry::PrimitiveType::Str => Type::Prim(PrimType::Str),
        type_registry::PrimitiveType::Bool => Type::Prim(PrimType::Bool),
        type_registry::PrimitiveType::Nil => Type::Prim(PrimType::Nil),
        type_registry::PrimitiveType::Vector => Type::Vector(Box::new(Type::Any)),
        type_registry::PrimitiveType::Map => Type::Map(Box::new(Type::Any), Box::new(Type::Any)),
        type_registry::PrimitiveType::Set => Type::Set(Box::new(Type::Any)),
        type_registry::PrimitiveType::List
        | type_registry::PrimitiveType::Seq
        | type_registry::PrimitiveType::Symbol
        | type_registry::PrimitiveType::Regex
        | type_registry::PrimitiveType::Duration
        | type_registry::PrimitiveType::Atom
        | type_registry::PrimitiveType::Chan
        | type_registry::PrimitiveType::Promise
        | type_registry::PrimitiveType::Task
        | type_registry::PrimitiveType::Future
        | type_registry::PrimitiveType::Agent
        | type_registry::PrimitiveType::Foreign => Type::Opaque(opaque_core_name(&canonical)),
        type_registry::PrimitiveType::Function => {
            Type::Func(Vec::new(), Some(Box::new(Type::Any)), Box::new(Type::Any))
        }
    })
}

fn canonicalize_type_name_for_infer(name: &str) -> String {
    let name = name.trim_start_matches(':');
    let name = name.strip_prefix("clove::").unwrap_or(name);
    if name == "String" {
        return "Str".to_string();
    }
    if let Some(prefix) = name.strip_suffix("::String") {
        return format!("{}::Str", prefix);
    }
    name.to_string()
}

fn opaque_core_name(name: &str) -> String {
    let base = name.rsplit("::").next().unwrap_or(name);
    format!("core::{}", base)
}

fn prelude_env() -> TypeEnv {
    ensure_fn_meta_registry();
    let mut env = TypeEnv::new();
    for (name, scheme) in fnmeta_prelude_entries() {
        env.insert(name, scheme);
    }
    apply_manual_prelude_entries(&mut env);
    env
}

fn ensure_fn_meta_registry() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        let mut env = Env::default();
        builtins::install_core(&mut env);
    });
}

fn fnmeta_prelude_entries() -> Vec<(String, Scheme)> {
    let mut entries = HashMap::new();
    for meta in fn_meta::all() {
        if let Some(scheme) = scheme_from_overloads(&meta.overloads) {
            let keys = if meta.ns == "core" || meta.ns == "string" {
                doc_lookup_keys(&meta.fq_name())
            } else {
                vec![canonical_symbol_name(&meta.fq_name()).into_owned()]
            };
            for key in keys {
                entries.entry(key).or_insert_with(|| scheme.clone());
            }
        }
    }
    entries.into_iter().collect()
}

fn scheme_from_overloads(overloads: &[FnOverload]) -> Option<Scheme> {
    let mut types = Vec::new();
    for overload in overloads {
        if let Some(ty) = type_from_overload(overload) {
            types.push(ty);
        }
    }
    if types.is_empty() {
        return None;
    }
    let ty = if types.len() == 1 {
        types.remove(0)
    } else {
        Type::Overloaded(types)
    };
    Some(Scheme { vars: vec![], ty })
}

fn type_from_overload(overload: &FnOverload) -> Option<Type> {
    let args = overload
        .arg_types
        .iter()
        .cloned()
        .map(convert_annotation)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    let rest = overload
        .rest
        .clone()
        .map(convert_annotation)
        .transpose()
        .ok()?
        .map(|ty| match ty {
            Type::Vector(inner) => *inner,
            other => other,
        });
    let ret = convert_annotation(overload.ret_type.clone()).ok()?;
    Some(Type::Func(args, rest.map(Box::new), Box::new(ret)))
}

fn apply_manual_prelude_entries(env: &mut TypeEnv) {
    use PrimType::*;
    fn scheme(args: Vec<Type>, ret: Type) -> Scheme {
        Scheme {
            vars: vec![],
            ty: Type::Func(args, None, Box::new(ret)),
        }
    }
    fn scheme_rest(args: Vec<Type>, rest: Option<Type>, ret: Type) -> Scheme {
        Scheme {
            vars: vec![],
            ty: Type::Func(args, rest.map(Box::new), Box::new(ret)),
        }
    }
    fn scheme_poly(vars: Vec<TypeVar>, args: Vec<Type>, ret: Type) -> Scheme {
        Scheme {
            vars,
            ty: Type::Func(args, None, Box::new(ret)),
        }
    }
    fn tv(id: u32) -> Type {
        Type::Var(TypeVar(id))
    }
    let entries: Vec<(&str, Scheme)> = vec![
        // Numbers / comparisons
        ("inc", scheme(vec![Type::Prim(Int)], Type::Prim(Int))),
        ("dec", scheme(vec![Type::Prim(Int)], Type::Prim(Int))),
        ("abs", scheme(vec![Type::Prim(Int)], Type::Prim(Int))),
        (
            "+",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Int)),
        ),
        (
            "-",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Int)),
        ),
        (
            "*",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Int)),
        ),
        (
            "/",
            scheme(
                vec![Type::Prim(Float), Type::Prim(Float)],
                Type::Prim(Float),
            ),
        ),
        (
            ">",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Bool)),
        ),
        (
            "<",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Bool)),
        ),
        (
            ">=",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Bool)),
        ),
        (
            "<=",
            scheme(vec![Type::Prim(Int), Type::Prim(Int)], Type::Prim(Bool)),
        ),
        ("=", scheme(vec![Type::Any, Type::Any], Type::Prim(Bool))),
        ("not=", scheme(vec![Type::Any, Type::Any], Type::Prim(Bool))),
        // Logic
        (
            "and",
            scheme(vec![Type::Prim(Bool), Type::Prim(Bool)], Type::Prim(Bool)),
        ),
        (
            "or",
            scheme(vec![Type::Prim(Bool), Type::Prim(Bool)], Type::Prim(Bool)),
        ),
        ("not", scheme(vec![Type::Prim(Bool)], Type::Prim(Bool))),
        // Concurrency / state
        (
            "atom-set!",
            scheme(vec![Type::Any, Type::Any], Type::Prim(Nil)),
        ),
        (
            "reset!",
            scheme(vec![Type::Any, Type::Any], Type::Prim(Nil)),
        ),
        (
            "sdl2::destroy-texture",
            scheme(vec![Type::Any], Type::Prim(Nil)),
        ),
        // Strings
        ("str", scheme_rest(vec![], Some(Type::Any), Type::Prim(Str))),
        (
            "println",
            scheme_rest(vec![], Some(Type::Any), Type::Prim(Nil)),
        ),
        ("prn", scheme_rest(vec![], Some(Type::Any), Type::Prim(Nil))),
        // Collection construction (fill with Any at first)
        (
            "vector",
            scheme(vec![Type::Any], Type::Vector(Box::new(Type::Any))),
        ),
        (
            "list",
            scheme(vec![Type::Any], Type::Vector(Box::new(Type::Any))),
        ),
        (
            "hash-map",
            scheme(
                vec![Type::Any, Type::Any],
                Type::Map(Box::new(Type::Any), Box::new(Type::Any)),
            ),
        ),
        (
            "hash-set",
            scheme(vec![Type::Any], Type::Set(Box::new(Type::Any))),
        ),
    ];
    for (name, scheme) in entries {
        insert_scheme_if_absent(env, name, scheme);
    }
    let a = tv(0);
    let mut_a = Type::Mut(Box::new(a.clone()));
    env.insert(
        "mut".to_string(),
        Scheme {
            vars: vec![TypeVar(0)],
            ty: Type::Func(vec![a.clone()], None, Box::new(mut_a.clone())),
        },
    );
    env.insert(
        "imut".to_string(),
        Scheme {
            vars: vec![TypeVar(0)],
            ty: Type::Func(vec![mut_a], None, Box::new(a.clone())),
        },
    );
    // Polymorphic collection operations
    insert_scheme_if_absent(
        env,
        "first",
        scheme_poly(
            vec![TypeVar(0)],
            vec![Type::Vector(Box::new(a.clone()))],
            Type::Option(Box::new(a.clone())),
        ),
    );
    insert_scheme_if_absent(
        env,
        "rest",
        scheme_poly(
            vec![TypeVar(0)],
            vec![Type::Vector(Box::new(a.clone()))],
            Type::Vector(Box::new(a.clone())),
        ),
    );
    insert_scheme_if_absent(
        env,
        "conj",
        Scheme {
            vars: vec![TypeVar(0)],
            ty: Type::Overloaded(vec![
                Type::Func(
                    vec![Type::Vector(Box::new(a.clone())), a.clone()],
                    None,
                    Box::new(Type::Vector(Box::new(a.clone()))),
                ),
                Type::Func(
                    vec![Type::Set(Box::new(a.clone())), a.clone()],
                    None,
                    Box::new(Type::Set(Box::new(a.clone()))),
                ),
            ]),
        },
    );
    insert_scheme_if_absent(
        env,
        "disj",
        Scheme {
            vars: vec![TypeVar(0)],
            ty: Type::Func(
                vec![Type::Set(Box::new(a.clone()))],
                Some(Box::new(a.clone())),
                Box::new(Type::Set(Box::new(a.clone()))),
            ),
        },
    );
    insert_scheme_if_absent(
        env,
        "count",
        scheme_poly(
            vec![TypeVar(0)],
            vec![Type::Vector(Box::new(a.clone()))],
            Type::Prim(Int),
        ),
    );
    insert_scheme_if_absent(
        env,
        "empty?",
        scheme_poly(
            vec![TypeVar(0)],
            vec![Type::Vector(Box::new(a.clone()))],
            Type::Prim(Bool),
        ),
    );
    insert_scheme_if_absent(
        env,
        "nth",
        scheme_poly(
            vec![TypeVar(0)],
            vec![Type::Vector(Box::new(a.clone())), Type::Prim(Int)],
            Type::Option(Box::new(a.clone())),
        ),
    );
    let k = tv(1);
    let v = tv(2);
    insert_scheme_if_absent(
        env,
        "into",
        Scheme {
            vars: vec![TypeVar(0), TypeVar(1), TypeVar(2)],
            ty: Type::Overloaded(vec![
                Type::Func(
                    vec![
                        Type::Vector(Box::new(a.clone())),
                        Type::Vector(Box::new(a.clone())),
                    ],
                    None,
                    Box::new(Type::Vector(Box::new(a.clone()))),
                ),
                Type::Func(
                    vec![Type::Set(Box::new(a.clone())), Type::Any],
                    None,
                    Box::new(Type::Set(Box::new(a.clone()))),
                ),
                Type::Func(
                    vec![
                        Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                        Type::Any,
                    ],
                    None,
                    Box::new(Type::Map(Box::new(k.clone()), Box::new(v.clone()))),
                ),
                Type::Func(vec![a.clone(), Type::Any], None, Box::new(a.clone())),
            ]),
        },
    );
    // Polymorphic map operations
    insert_scheme_if_absent(
        env,
        "assoc",
        scheme_poly(
            vec![TypeVar(1), TypeVar(2)],
            vec![
                Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                k.clone(),
                v.clone(),
            ],
            Type::Map(Box::new(k.clone()), Box::new(v.clone())),
        ),
    );
    insert_scheme_if_absent(
        env,
        "dissoc",
        scheme_poly(
            vec![TypeVar(1), TypeVar(2)],
            vec![
                Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                k.clone(),
            ],
            Type::Map(Box::new(k.clone()), Box::new(v.clone())),
        ),
    );
    insert_scheme_if_absent(
        env,
        "get",
        scheme_poly(
            vec![TypeVar(1), TypeVar(2)],
            vec![
                Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                k.clone(),
            ],
            Type::Option(Box::new(v.clone())),
        ),
    );
    // contains? type for partial application / higher-order use. infer_contains allows Vec/Set at call sites.
    insert_scheme_if_absent(
        env,
        "contains?",
        scheme_poly(
            vec![TypeVar(1), TypeVar(2)],
            vec![
                Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                k.clone(),
            ],
            Type::Prim(Bool),
        ),
    );
    // Polymorphic max/min (assume comparable types)
    let b = tv(3);
    insert_scheme_if_absent(
        env,
        "max",
        scheme_poly(vec![TypeVar(3)], vec![b.clone(), b.clone()], b.clone()),
    );
    insert_scheme_if_absent(
        env,
        "min",
        scheme_poly(vec![TypeVar(3)], vec![b.clone(), b.clone()], b),
    );
}

fn insert_scheme_if_absent(env: &mut TypeEnv, name: &str, scheme: Scheme) {
    if env.lookup(name).is_none() {
        env.insert(name.to_string(), scheme);
    }
}

#[cfg(test)]
mod tests {
    use super::super::hm::{PrimType, Scheme, Type};
    use super::{apply_manual_prelude_entries, fnmeta_prelude_entries, TypeEnv};
    use crate::plugin_meta;
    use crate::reader::{Reader, ReaderOptions};
    use std::collections::HashMap;
    use std::fs;
    use std::sync::Once;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn ensure_fnmeta_initialized() {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            let _ = crate::builtins::default_env();
        });
    }

    #[test]
    fn fnmeta_prelude_includes_aliases_for_basic_funcs() {
        ensure_fnmeta_initialized();
        let entries: HashMap<_, _> = fnmeta_prelude_entries().into_iter().collect();
        assert!(
            entries.contains_key("mod"),
            "expected 'mod' entry derived from FnMeta"
        );
        assert!(
            entries.contains_key("core::mod"),
            "expected 'core::mod' alias entry derived from FnMeta"
        );
    }

    #[test]
    fn manual_entries_do_not_override_existing_schemes() {
        ensure_fnmeta_initialized();
        let mut env = TypeEnv::new();
        env.insert(
            "vector",
            Scheme {
                vars: vec![],
                ty: Type::Prim(PrimType::Int),
            },
        );
        apply_manual_prelude_entries(&mut env);
        let scheme = env.lookup("vector").expect("vector entry should exist");
        assert!(
            matches!(scheme.ty, Type::Prim(PrimType::Int)),
            "manual entries must not overwrite existing FnMeta-derived bindings"
        );
    }

    #[test]
    fn manual_nilable_entries_return_option() {
        ensure_fnmeta_initialized();
        let env = super::prelude_env();

        let first = env.lookup("first").expect("first exists");
        assert!(matches!(
            first.ty,
            Type::Func(_, _, ret) if matches!(*ret, Type::Option(_))
        ));

        let nth = env.lookup("nth").expect("nth exists");
        assert!(matches!(
            nth.ty,
            Type::Func(_, _, ret) if matches!(*ret, Type::Option(_))
        ));

        let get = env.lookup("get").expect("get exists");
        assert!(matches!(
            get.ty,
            Type::Func(_, _, ret) if matches!(*ret, Type::Option(_))
        ));
    }

    #[test]
    fn infer_mut_and_imut_round_trip() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(imut (mut [1 2]))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Vector(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_assoc_bang_rejects_immutable() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(assoc! {:a 1} :a 2)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        assert!(super::infer_forms(&forms).is_err());
    }

    #[test]
    fn infer_if_with_nil_produces_option() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(if true 1 nil)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Option(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_get_distinguishes_default_presence() {
        ensure_fnmeta_initialized();
        let src = "(fn [m<Map<Int, Str>> k<Int>] (get m k)) (fn [m<Map<Int, Str>> k<Int> d<Str>] (get m k d))";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read forms");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        assert_eq!(result.forms.len(), 2);

        let without_default = &result.forms[0].1;
        assert!(matches!(
            without_default,
            Type::Func(_, _, ret) if matches!(**ret, Type::Option(_))
        ));

        let with_default = &result.forms[1].1;
        assert!(matches!(
            with_default,
            Type::Func(_, _, ret) if matches!(**ret, Type::Prim(PrimType::Str))
        ));
    }

    #[test]
    fn infer_vector_annotation_as_tuple() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("[1 2]: [Int Int]", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Tuple(items)
                if matches!(items.as_slice(), [Type::Prim(PrimType::Int), Type::Prim(PrimType::Int)])
        ));
    }

    #[test]
    fn infer_hetero_vector_as_tuple() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("[\"a\" 1]", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Tuple(items)
                if matches!(items.as_slice(), [Type::Prim(PrimType::Str), Type::Prim(PrimType::Int)])
        ));
    }

    #[test]
    fn infer_tuple_arity_mismatch_emits_diag() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("[1 2]: [Int Int Int]", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms_with_diags(&forms);
        assert!(
            !result.diags.is_empty(),
            "expected tuple arity mismatch to produce diagnostics"
        );
    }

    #[test]
    fn infer_record_literal_fields() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("{:a 1 :b \"x\"}", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        match ty {
            Type::Record(fields) => {
                assert!(matches!(fields.get("a"), Some(Type::Prim(PrimType::Int))));
                assert!(matches!(fields.get("b"), Some(Type::Prim(PrimType::Str))));
            }
            other => panic!("expected record type, got {:?}", other),
        }
    }

    #[test]
    fn infer_get_on_record_returns_field_type() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(get {:a 1 :b \"x\"} :b)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Str)));
    }

    #[test]
    fn infer_keyword_call_on_record_returns_field_type() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(:a {:a 1})", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_missing_record_field_emits_diag() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(get {:a 1} :b)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms_with_diags(&forms);
        assert!(
            !result.diags.is_empty(),
            "expected missing record field to produce diagnostics"
        );
    }

    #[test]
    fn infer_assoc_on_record_degrades_to_map() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(assoc {:a 1} :b 2)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Map(_, _)));
    }

    #[test]
    fn infer_cond_with_else_unifies_branches() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(cond true 1 :else 2)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_cond_without_else_is_option() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(cond false 1)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Option(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_match_binds_symbol() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(match 1 x x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_match_binds_vector_pattern_elem_type() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(match [1 2] [x y] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_match_binds_record_field_type() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(match {:a 1} {:a x} x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_defenum_match_shorthand_unifies() {
        ensure_fnmeta_initialized();
        let src = "(defenum Pets Dog {:name Str} Cat {:name Str}) (defn describe [pet] (match pet (Dog :name) name (Cat :name) name))";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read forms");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[1].1;
        assert!(matches!(
            ty,
            Type::Func(_, _, ret) if matches!(**ret, Type::Prim(PrimType::Str))
        ));
    }

    #[test]
    fn infer_deftype_method_registers_function() {
        ensure_fnmeta_initialized();
        let src =
            "(deftype Point {:x Int} (method inc-x [] (+ &:x 1))) (defn use [p<Point>] (inc-x p))";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read forms");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[1].1;
        assert!(matches!(
            ty,
            Type::Func(_, _, ret) if matches!(**ret, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_for_returns_vector() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(for [x [1 2]] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Vector(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_for_destructuring_uses_elem_type() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(for [[x y] [[1 2] [3 4]]] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Vector(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_doseq_returns_nil() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(doseq [x [1 2]] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Nil)));
    }

    #[test]
    fn infer_where_returns_nil() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(where 1 2)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Nil)));
    }

    #[test]
    fn infer_do_ignores_where_for_return() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(do 1 (where 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_do_allows_err_fin_tail() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(do 1 (err 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_fn_allows_err_fin_tail() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(fn [] 1 (err 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Func(_, _, ret) if matches!(**ret, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_do_rejects_err_fin_mid_body() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(do (err 1) 2)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        assert!(super::infer_forms(&forms).is_err());
    }

    #[test]
    fn infer_loop_returns_body_type() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(loop [x 1] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_cond_thread_keeps_int() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options(
            "(cond-> 1 true (+ 1) false (+ 2))",
            ReaderOptions::default(),
        );
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_cond_thread_last_keeps_int() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options(
            "(cond->> 1 true (+ 1) false (+ 2))",
            ReaderOptions::default(),
        );
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_condp_with_else() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(condp = 1 1 10 2 20 :else 0)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_condp_threading_uses_pred_result_type() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options(
            "(condp (fn [a b] [1 2]) 0 1 :>> first)",
            ReaderOptions::default(),
        );
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        match ty {
            Type::Option(inner) => match inner.as_ref() {
                Type::Option(inner2) => {
                    assert!(matches!(inner2.as_ref(), Type::Prim(PrimType::Int)));
                }
                other => panic!("expected nested option, got {:?}", other),
            },
            other => panic!("expected option, got {:?}", other),
        }
    }

    #[test]
    fn infer_when_returns_option() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(when true 1)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Option(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_when_let_binds_value() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(when-let [x 1] x)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Option(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_if_let_binds_value() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(if-let [x 1] x 0)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_thread_first_keeps_int() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(-> 1 (+ 1) (+ 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_as_thread_updates_binding() {
        ensure_fnmeta_initialized();
        let mut reader =
            Reader::new_with_options("(as-> 1 x (+ x 2) (* x 3))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_oop_chain_returns_str() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(range 3).str", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Str)));
    }

    #[test]
    fn infer_oop_chain_dot_stage_returns_str() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(range 3).(str ?)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Str)));
    }

    #[test]
    fn infer_meta_opaque_function_returns_opaque() {
        ensure_fnmeta_initialized();
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("clove-meta-test-{}", stamp));
        fs::create_dir_all(&temp_dir).expect("create temp dir");
        let meta_path = temp_dir.join("dummy.meta.json");
        let meta = r#"
{
  "schema": 1,
  "types": [
    {"name": "dummy::Canvas", "kind": "opaque"}
  ],
  "fns": [
    {"name": "dummy::make-canvas", "type": "[] -> dummy::Canvas"}
  ]
}
"#;
        fs::write(&meta_path, meta).expect("write meta");
        plugin_meta::clear_meta_for_tests();
        plugin_meta::load_meta_from_dirs(&[temp_dir]).expect("load meta");

        let mut reader = Reader::new_with_options("(dummy::make-canvas)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Opaque(name) if name == "dummy::Canvas"));
    }

    #[test]
    fn infer_some_thread_wraps_option() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(some-> 1 (+ 1))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(
            ty,
            Type::Option(inner) if matches!(**inner, Type::Prim(PrimType::Int))
        ));
    }

    #[test]
    fn infer_try_joins_catch() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(try 1 (catch e 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_short_try_handler_joins() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(try 1 (fn [e] 2))", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_short_try_with_bindings_joins() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options(
            "(try [x 1] x (fn [e] 2) (fn [] 3))",
            ReaderOptions::default(),
        );
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Int)));
    }

    #[test]
    fn infer_dotimes_returns_nil() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(dotimes [i 3] i)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Nil)));
    }

    #[test]
    fn infer_while_returns_nil() {
        ensure_fnmeta_initialized();
        let mut reader = Reader::new_with_options("(while true 1)", ReaderOptions::default());
        let forms = reader.read_all().expect("failed to read form");
        let result = super::infer_forms(&forms).expect("infer should succeed");
        let ty = &result.forms[0].1;
        assert!(matches!(ty, Type::Prim(PrimType::Nil)));
    }
}
