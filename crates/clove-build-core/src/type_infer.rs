use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use crate::aliases;
use crate::ast::{Expr, ExprKind, Span};
use crate::error::Clove2Error;
use crate::json_infer::{
    content_hash, infer_json_schema, read_snapshot, snapshot_path, validate_json_value,
    write_snapshot,
};
use crate::syntax::{AstExpr, Binding, Param, TopLevel};
use crate::types::Type;
use crate::use_directive::{MutMode, NativeLevel};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub span: Option<Span>,
}

pub struct TypeSummary {
    pub values: HashMap<String, Type>,
    pub named_types: HashMap<String, BTreeMap<String, Type>>,
    pub expr_types: Vec<ExprTypeEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprTraceKind {
    Expr,
    Literal,
    Symbol,
    Keyword,
    Vector,
    Set,
    Map,
    Quote,
    ForeignBlock,
    Fn,
    If,
    Let,
    SetVar,
    Call,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExprTypeEntry {
    pub span: Span,
    pub kind: ExprTraceKind,
    pub ty: Type,
}

pub fn check_program(items: &[TopLevel], level: NativeLevel) -> Vec<Diagnostic> {
    let (_, diags) = check_program_internal(items, level);
    diags
}

pub fn infer_program(items: &[TopLevel], level: NativeLevel) -> (TypeSummary, Vec<Diagnostic>) {
    let (env, diags) = check_program_internal(items, level);
    (env.snapshot(), diags)
}

pub fn infer_program_with_expr_spans(
    items: &[TopLevel],
    forms: &[Expr],
    level: NativeLevel,
) -> (TypeSummary, Vec<Diagnostic>) {
    let mut env = TypeEnv::new();
    env.enable_expr_trace();
    let (env, diags) = check_program_internal_with_env(items, level, env);
    let mut summary = env.snapshot();
    if let Some(trace) = env.expr_trace_snapshot() {
        let spans = collect_expr_spans_for_forms(forms);
        if spans.len() == trace.len() {
            summary.expr_types = spans
                .into_iter()
                .zip(trace.into_iter())
                .map(|(span, entry)| ExprTypeEntry {
                    span,
                    kind: entry.kind,
                    ty: entry.ty,
                })
                .collect();
        }
    }
    (summary, diags)
}

fn check_program_internal(items: &[TopLevel], level: NativeLevel) -> (TypeEnv, Vec<Diagnostic>) {
    check_program_internal_with_env(items, level, TypeEnv::new())
}

fn check_program_internal_with_env(
    items: &[TopLevel],
    level: NativeLevel,
    mut env: TypeEnv,
) -> (TypeEnv, Vec<Diagnostic>) {
    let mut diags = Vec::new();
    let mut def_use_counts: HashMap<String, usize> = HashMap::new();
    let def_names: Vec<String> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Def { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect();
    for name in &def_names {
        let mut count = 0;
        for item in items {
            count += count_symbol_uses_in_toplevel(item, name);
        }
        def_use_counts.insert(name.clone(), count);
    }

    for item in items {
        if let TopLevel::DefType { name, fields, .. } = item {
            if env.get_type(name).is_some() {
                diags.push(error_diag(format!("duplicate type definition: {}", name)));
            } else {
                env.set_type(name, fields.clone());
            }
        }
        if let TopLevel::Defn {
            name, params, ret, ..
        } = item
        {
            let mut fixed = Vec::new();
            let mut rest = None;
            for param in params {
                if param.rest {
                    let ty = param
                        .ty
                        .clone()
                        .unwrap_or_else(|| Type::Vec(Box::new(Type::Any)));
                    rest = Some(Box::new(ensure_vec_type(ty)));
                } else {
                    fixed.push(param.ty.clone().unwrap_or(Type::Any));
                }
            }
            let ret = ret.clone().unwrap_or(Type::Any);
            env.set(
                name,
                Type::Function {
                    params: fixed,
                    rest,
                    ret: Box::new(ret),
                },
            );
        }
        if let TopLevel::DefForeign { decl, .. } = item {
            let mut fixed = Vec::new();
            let mut rest = None;
            for param in &decl.params {
                if param.rest {
                    let ty = param
                        .ty
                        .clone()
                        .unwrap_or_else(|| Type::Vec(Box::new(Type::Any)));
                    rest = Some(Box::new(ensure_vec_type(ty)));
                } else {
                    fixed.push(param.ty.clone().unwrap_or(Type::Any));
                }
            }
            env.set(
                &decl.name,
                Type::Function {
                    params: fixed,
                    rest,
                    ret: Box::new(decl.ret.clone()),
                },
            );
        }
    }

    for item in items {
        let before = diags.len();
        match item {
            TopLevel::Def {
                name, ty, value, ..
            } => {
                let value_ty = infer_expr(value, &mut env, &mut diags, level);
                let final_ty = if let Some(expected) = ty {
                    if !is_assignable_with_env(&value_ty, expected, &env) {
                        if is_optional_assignable(&value_ty, expected, &env) {
                            report_optional_usage(
                                &value_ty,
                                level,
                                &mut diags,
                                &format!("def {}", name),
                            );
                        } else {
                            diags.push(error_diag(format!(
                                "def {} expects {}, got {}",
                                name, expected, value_ty
                            )));
                        }
                    }
                    expected.clone()
                } else {
                    value_ty
                };
                if !should_skip_unresolved_any(value, &env, &final_ty) {
                    report_unresolved(&final_ty, level, &mut diags, format!("def {}", name));
                }
                env.set(name, final_ty);
                if let Some(kind) = fresh_kind_for_expr(value) {
                    if def_use_counts.get(name).copied().unwrap_or(0) == 1 {
                        env.set_unique(name, kind);
                    }
                }
            }
            TopLevel::Defn {
                name,
                params,
                ret,
                body,
                ..
            } => {
                let mut fn_env = env.child();
                let mut param_types = Vec::new();
                let mut rest_ty = None;
                for Param {
                    name: param,
                    ty,
                    rest,
                } in params
                {
                    if *rest {
                        let rest_label = format!("rest param {} of {}", param, name);
                        let ty = normalize_rest_type(ty.clone(), level, &mut diags, &rest_label);
                        report_unresolved(&ty, level, &mut diags, rest_label);
                        fn_env.set(param, ty.clone());
                        rest_ty = Some(Box::new(ty));
                    } else {
                        let ty = ty.clone().unwrap_or(Type::Any);
                        report_unresolved(
                            &ty,
                            level,
                            &mut diags,
                            format!("param {} of {}", param, name),
                        );
                        fn_env.set(param, ty.clone());
                        param_types.push(ty);
                    }
                }
                let body_ty = infer_body(body, &mut fn_env, &mut diags, level);
                let ret_ty = if let Some(ret_ty) = ret {
                    if !is_assignable_with_env(&body_ty, ret_ty, &env) {
                        if is_optional_assignable(&body_ty, ret_ty, &env) {
                            report_optional_usage(
                                &body_ty,
                                level,
                                &mut diags,
                                &format!("defn {}", name),
                            );
                        } else {
                            diags.push(error_diag(format!(
                                "defn {} expects {}, got {}",
                                name, ret_ty, body_ty
                            )));
                        }
                    }
                    report_unresolved(ret_ty, level, &mut diags, format!("defn {}", name));
                    ret_ty.clone()
                } else {
                    report_unresolved(&body_ty, level, &mut diags, format!("defn {}", name));
                    body_ty
                };
                env.set(
                    name,
                    Type::Function {
                        params: param_types,
                        rest: rest_ty,
                        ret: Box::new(ret_ty),
                    },
                );
            }
            TopLevel::DefForeign { decl, .. } => {
                let mut params = Vec::new();
                let mut rest_ty = None;
                for param in &decl.params {
                    if param.rest {
                        let rest_label =
                            format!("def-foreign {} rest param {}", decl.name, param.name);
                        let ty =
                            normalize_rest_type(param.ty.clone(), level, &mut diags, &rest_label);
                        report_unresolved(&ty, level, &mut diags, rest_label);
                        rest_ty = Some(Box::new(ty));
                    } else {
                        let ty = param.ty.clone().unwrap_or(Type::Any);
                        report_unresolved(
                            &ty,
                            level,
                            &mut diags,
                            format!("def-foreign {} param {}", decl.name, param.name),
                        );
                        params.push(ty);
                    }
                }
                report_unresolved(
                    &decl.ret,
                    level,
                    &mut diags,
                    format!("def-foreign {}", decl.name),
                );
                env.set(
                    &decl.name,
                    Type::Function {
                        params,
                        rest: rest_ty,
                        ret: Box::new(decl.ret.clone()),
                    },
                );
            }
            TopLevel::DefType { name, fields, .. } => {
                for (field, ty) in fields {
                    report_unresolved(ty, level, &mut diags, format!("deftype {}.{}", name, field));
                }
            }
            TopLevel::Expr { expr, .. } => {
                let ty = infer_expr(expr, &mut env, &mut diags, level);
                if !should_skip_unresolved_any(expr, &env, &ty) {
                    report_unresolved(&ty, level, &mut diags, "top-level expr".to_string());
                }
                if let TopLevel::Expr { span, .. } = item {
                    env.record_expr_type(span.clone(), ty);
                }
            }
        }
        attach_span(&mut diags[before..], item);
    }

    (env, diags)
}

fn infer_body(
    body: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let mut out = Type::Nil;
    for expr in body {
        out = infer_expr(expr, env, diags, level);
    }
    out
}

fn infer_expr(
    expr: &AstExpr,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    infer_expr_inner(expr, env, diags, level, TraceMode::Normal)
}

fn infer_expr_skip_trace(
    expr: &AstExpr,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    infer_expr_inner(expr, env, diags, level, TraceMode::Skip)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TraceMode {
    Normal,
    Skip,
}

fn infer_expr_inner(
    expr: &AstExpr,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
    trace_mode: TraceMode,
) -> Type {
    let (kind, ty) = match expr {
        AstExpr::Literal(lit) => (
            ExprTraceKind::Literal,
            match lit {
                crate::ast::Literal::Nil => Type::Nil,
                crate::ast::Literal::Bool(_) => Type::Bool,
                crate::ast::Literal::Int(_) => Type::Int,
                crate::ast::Literal::Float(_) => Type::Float,
                crate::ast::Literal::Str(_) | crate::ast::Literal::Regex(_) => Type::Str,
            },
        ),
        AstExpr::Symbol(sym) => (
            ExprTraceKind::Symbol,
            env.get(sym).unwrap_or_else(|| {
                let canonical = aliases::resolve_alias(sym);
                if let Some(ty) = builtin_type(canonical) {
                    return ty;
                }
                diags.push(error_diag(format!("unknown symbol: {}", sym)));
                Type::Any
            }),
        ),
        AstExpr::Keyword(_) => (ExprTraceKind::Keyword, Type::Keyword),
        AstExpr::Vector(items) => (
            ExprTraceKind::Vector,
            infer_vector(items, env, diags, level),
        ),
        AstExpr::Set(items) => (ExprTraceKind::Set, infer_vector(items, env, diags, level)),
        AstExpr::Map(entries) => (ExprTraceKind::Map, infer_map(entries, env, diags, level)),
        AstExpr::Quote(expr) => (ExprTraceKind::Quote, infer_quote_expr(expr)),
        AstExpr::ForeignBlock { .. } => {
            let ty = Type::Dyn;
            report_unresolved(&ty, level, diags, "foreign block".to_string());
            (ExprTraceKind::ForeignBlock, ty)
        }
        AstExpr::Fn { params, ret, body } => {
            let mut fn_env = env.child();
            let mut param_types = Vec::new();
            let mut rest_ty = None;
            for param in params {
                if param.rest {
                    let rest_label = format!("fn rest param {}", param.name);
                    let ty = normalize_rest_type(param.ty.clone(), level, diags, &rest_label);
                    report_unresolved(&ty, level, diags, rest_label);
                    fn_env.set(&param.name, ty.clone());
                    rest_ty = Some(Box::new(ty));
                } else {
                    let ty = param.ty.clone().unwrap_or(Type::Any);
                    report_unresolved(&ty, level, diags, format!("fn param {}", param.name));
                    fn_env.set(&param.name, ty.clone());
                    param_types.push(ty);
                }
            }
            let body_ty = infer_body(body, &mut fn_env, diags, level);
            let ret_ty = if let Some(ret_ty) = ret {
                if !is_assignable_with_env(&body_ty, ret_ty, env) {
                    if is_optional_assignable(&body_ty, ret_ty, env) {
                        report_optional_usage(&body_ty, level, diags, "fn");
                    } else {
                        diags.push(error_diag(format!(
                            "fn expects {}, got {}",
                            ret_ty, body_ty
                        )));
                    }
                }
                report_unresolved(ret_ty, level, diags, "fn".to_string());
                ret_ty.clone()
            } else {
                report_unresolved(&body_ty, level, diags, "fn".to_string());
                body_ty
            };
            (
                ExprTraceKind::Fn,
                Type::Function {
                    params: param_types,
                    rest: rest_ty,
                    ret: Box::new(ret_ty),
                },
            )
        }
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => {
            let _cond_ty = infer_expr(cond, env, diags, level);
            let (mut then_env, mut else_env) =
                if let Some((then_env, else_env)) = narrow_env_for_condition(cond, env) {
                    (then_env, else_env)
                } else {
                    (env.fork(), env.fork())
                };
            let then_ty = infer_expr(then_expr, &mut then_env, diags, level);
            let else_ty = if let Some(else_expr) = else_expr {
                infer_expr(else_expr, &mut else_env, diags, level)
            } else {
                Type::Nil
            };
            (ExprTraceKind::If, merge_types(then_ty, else_ty))
        }
        AstExpr::Let { bindings, body } => {
            env.push();
            let mut binding_use_counts: HashMap<String, usize> = HashMap::new();
            for (idx, binding) in bindings.iter().enumerate() {
                let mut rest_exprs: Vec<&AstExpr> = Vec::new();
                for next in bindings.iter().skip(idx + 1) {
                    rest_exprs.push(&next.value);
                }
                for expr in body {
                    rest_exprs.push(expr);
                }
                let count = count_symbol_uses_in_exprs(&rest_exprs, &binding.name);
                binding_use_counts.insert(binding.name.clone(), count);
            }
            for Binding { name, ty, value } in bindings {
                if let AstExpr::Fn { params, ret, .. } = value {
                    let mut param_types = Vec::new();
                    let mut rest_ty = None;
                    for param in params {
                        if param.rest {
                            let inner = param.ty.clone().unwrap_or(Type::Any);
                            rest_ty = Some(Box::new(inner));
                        } else {
                            param_types.push(param.ty.clone().unwrap_or(Type::Any));
                        }
                    }
                    let ret_ty = ret.clone().unwrap_or(Type::Nil);
                    env.set(
                        name,
                        Type::Function {
                            params: param_types,
                            rest: rest_ty,
                            ret: Box::new(ret_ty),
                        },
                    );
                }
                let value_ty = infer_expr(value, env, diags, level);
                let final_ty = if let Some(expected) = ty {
                    if !is_assignable_with_env(&value_ty, expected, env) {
                        if is_optional_assignable(&value_ty, expected, env) {
                            report_optional_usage(
                                &value_ty,
                                level,
                                diags,
                                &format!("let {}", name),
                            );
                        } else {
                            diags.push(error_diag(format!(
                                "let {} expects {}, got {}",
                                name, expected, value_ty
                            )));
                        }
                    }
                    expected.clone()
                } else {
                    value_ty
                };
                if !should_skip_unresolved_any(value, env, &final_ty) {
                    report_unresolved(&final_ty, level, diags, format!("let {}", name));
                }
                env.set(name, final_ty);
                if let Some(kind) = fresh_kind_for_expr(value) {
                    if binding_use_counts.get(name).copied().unwrap_or(0) == 1 {
                        env.set_unique(name, kind);
                    }
                }
            }
            let result = infer_body(body, env, diags, level);
            env.pop();
            (ExprTraceKind::Let, result)
        }
        AstExpr::SetVar { name, value } => {
            let value_ty = infer_expr(value, env, diags, level);
            if let Some(target_ty) = env.get_global(name) {
                if !is_assignable_with_env(&value_ty, &target_ty, env) {
                    if is_optional_assignable(&value_ty, &target_ty, env) {
                        report_optional_usage(&value_ty, level, diags, "set!");
                    } else {
                        diags.push(error_diag(format!(
                            "set! expects {}, got {}",
                            target_ty, value_ty
                        )));
                    }
                }
            } else {
                diags.push(error_diag(format!("set! expects global var: {}", name)));
            }
            (ExprTraceKind::SetVar, value_ty)
        }
        AstExpr::Call { callee, args } => {
            let ty = if let AstExpr::Keyword(name) = callee.as_ref() {
                infer_keyword_call(name, args, env, diags, level)
            } else if let AstExpr::Symbol(sym) = callee.as_ref() {
                let canonical = aliases::resolve_alias(sym);
                if canonical == "use" || canonical == "comment" {
                    Type::Nil
                } else if canonical == "try" {
                    infer_try_call(args, env, diags, level)
                } else if canonical == "throw" {
                    infer_throw_call(args, env, diags, level)
                } else if matches!(canonical, "mut" | "imut" | "do") {
                    if canonical == "mut" || canonical == "imut" {
                        let mode = if canonical == "imut" {
                            MutMode::Imut
                        } else {
                            MutMode::Mut
                        };
                        env.push_mut_mode(mode);
                    }
                    let mut last = Type::Nil;
                    for arg in args {
                        last = infer_expr(arg, env, diags, level);
                    }
                    if canonical == "mut" || canonical == "imut" {
                        env.pop_mut_mode();
                    }
                    last
                } else if canonical == "and" {
                    if args.is_empty() {
                        Type::Bool
                    } else {
                        let mut out = infer_expr(&args[0], env, diags, level);
                        for arg in args.iter().skip(1) {
                            out = merge_types(out, infer_expr(arg, env, diags, level));
                        }
                        out
                    }
                } else if canonical == "or" {
                    if args.is_empty() {
                        Type::Nil
                    } else {
                        let mut out = infer_expr(&args[0], env, diags, level);
                        for arg in args.iter().skip(1) {
                            out = merge_types(out, infer_expr(arg, env, diags, level));
                        }
                        out
                    }
                } else if canonical == "when" {
                    if args.is_empty() {
                        Type::Nil
                    } else {
                        let _ = infer_expr(&args[0], env, diags, level);
                        let mut last = Type::Nil;
                        for arg in args.iter().skip(1) {
                            last = infer_expr(arg, env, diags, level);
                        }
                        merge_types(last, Type::Nil)
                    }
                } else if matches!(canonical, "repl" | "debug" | "break") {
                    Type::Nil
                } else if canonical == "expect" {
                    infer_expect_call(args, env, diags, level)
                } else if canonical == "as" {
                    infer_as_call(args, env, diags, level)
                } else if let Some(fn_ty) = env.get(sym) {
                    if let Type::Function { params, rest, ret } = fn_ty {
                        infer_call_with_signature(
                            sym,
                            args,
                            &params,
                            rest.as_deref(),
                            &ret,
                            env,
                            diags,
                            level,
                        )
                    } else if let Some(result) =
                        infer_known_call(canonical, args, env, diags, level)
                    {
                        result
                    } else {
                        for arg in args {
                            let _ = infer_expr(arg, env, diags, level);
                        }
                        Type::Any
                    }
                } else if let Some(result) = infer_known_call(canonical, args, env, diags, level) {
                    result
                } else {
                    let mut infer_non_symbol_call = || {
                        let callee_ty = infer_expr_skip_trace(callee, env, diags, level);
                        if let Type::Function { params, rest, ret } = callee_ty {
                            infer_call_with_signature(
                                "call",
                                args,
                                &params,
                                rest.as_deref(),
                                &ret,
                                env,
                                diags,
                                level,
                            )
                        } else {
                            for arg in args {
                                let _ = infer_expr(arg, env, diags, level);
                            }
                            Type::Any
                        }
                    };
                    if let AstExpr::Call {
                        callee,
                        args: juxt_args,
                    } = callee.as_ref()
                    {
                        if let AstExpr::Symbol(sym) = callee.as_ref() {
                            if sym == "juxt" {
                                infer_juxt_apply(juxt_args, args, env, diags, level)
                            } else {
                                infer_non_symbol_call()
                            }
                        } else {
                            infer_non_symbol_call()
                        }
                    } else {
                        infer_non_symbol_call()
                    }
                }
            } else {
                let mut infer_non_symbol_call = || {
                    let callee_ty = infer_expr_skip_trace(callee, env, diags, level);
                    if let Type::Function { params, rest, ret } = callee_ty {
                        infer_call_with_signature(
                            "call",
                            args,
                            &params,
                            rest.as_deref(),
                            &ret,
                            env,
                            diags,
                            level,
                        )
                    } else {
                        for arg in args {
                            let _ = infer_expr(arg, env, diags, level);
                        }
                        Type::Any
                    }
                };
                if let AstExpr::Call {
                    callee,
                    args: juxt_args,
                } = callee.as_ref()
                {
                    if let AstExpr::Symbol(sym) = callee.as_ref() {
                        if sym == "juxt" {
                            infer_juxt_apply(juxt_args, args, env, diags, level)
                        } else {
                            infer_non_symbol_call()
                        }
                    } else {
                        infer_non_symbol_call()
                    }
                } else {
                    infer_non_symbol_call()
                }
            };
            (ExprTraceKind::Call, ty)
        }
    };
    if trace_mode == TraceMode::Normal {
        env.record_expr_trace(kind, ty.clone());
    }
    ty
}

#[derive(Clone, Debug)]
enum NarrowPredicate {
    Nil,
    Some,
    Type(Type),
}

fn narrow_env_for_condition(cond: &AstExpr, env: &TypeEnv) -> Option<(TypeEnv, TypeEnv)> {
    let (name, pred, negated) = extract_narrow_predicate(cond)?;
    let current = env.get(&name).unwrap_or(Type::Any);
    let (then_ty, else_ty) = match pred {
        NarrowPredicate::Nil => {
            let non_nil = strip_nil(&current).unwrap_or_else(|| current.clone());
            (Type::Nil, non_nil)
        }
        NarrowPredicate::Some => {
            let non_nil = strip_nil(&current).unwrap_or_else(|| current.clone());
            (non_nil, Type::Nil)
        }
        NarrowPredicate::Type(target) => {
            let then_ty = narrow_type(&current, &target, env);
            let else_ty = exclude_type(&current, &target, env);
            (then_ty, else_ty)
        }
    };
    let (then_ty, else_ty) = if negated {
        (else_ty, then_ty)
    } else {
        (then_ty, else_ty)
    };
    let mut then_env = env.fork();
    let mut else_env = env.fork();
    then_env.set(&name, then_ty);
    else_env.set(&name, else_ty);
    Some((then_env, else_env))
}

fn extract_narrow_predicate(cond: &AstExpr) -> Option<(String, NarrowPredicate, bool)> {
    if let AstExpr::Symbol(name) = cond {
        return Some((name.clone(), NarrowPredicate::Some, false));
    }
    let AstExpr::Call { callee, args } = cond else {
        return None;
    };
    if let AstExpr::Symbol(sym) = callee.as_ref() {
        if sym == "not" && args.len() == 1 {
            let (name, pred, negated) = extract_narrow_predicate(&args[0])?;
            return Some((name, pred, !negated));
        }
        if args.len() != 1 {
            return None;
        }
        let AstExpr::Symbol(name) = &args[0] else {
            return None;
        };
        let pred = match sym.as_str() {
            "nil?" => NarrowPredicate::Nil,
            "some?" => NarrowPredicate::Some,
            "bool?" | "boolean?" => NarrowPredicate::Type(Type::Bool),
            "int?" => NarrowPredicate::Type(Type::Int),
            "float?" => NarrowPredicate::Type(Type::Float),
            "number?" => NarrowPredicate::Type(Type::Number),
            "str?" => NarrowPredicate::Type(Type::Str),
            "keyword?" => NarrowPredicate::Type(Type::Keyword),
            "symbol?" => NarrowPredicate::Type(Type::Symbol),
            _ => return None,
        };
        return Some((name.clone(), pred, false));
    }
    None
}

fn narrow_type(value: &Type, target: &Type, env: &TypeEnv) -> Type {
    match value {
        Type::Union(items) => {
            let mut keep = Vec::new();
            for item in items {
                if is_assignable_with_env(item, target, env) {
                    keep.push(item.clone());
                }
            }
            if keep.is_empty() {
                value.clone()
            } else {
                Type::union(keep)
            }
        }
        _ => {
            if is_assignable_with_env(value, target, env) {
                value.clone()
            } else {
                value.clone()
            }
        }
    }
}

fn exclude_type(value: &Type, target: &Type, env: &TypeEnv) -> Type {
    match value {
        Type::Union(items) => {
            let mut keep = Vec::new();
            for item in items {
                if !is_assignable_with_env(item, target, env) {
                    keep.push(item.clone());
                }
            }
            if keep.is_empty() {
                value.clone()
            } else {
                Type::union(keep)
            }
        }
        _ => {
            if is_assignable_with_env(value, target, env) {
                value.clone()
            } else {
                value.clone()
            }
        }
    }
}

fn collect_expr_spans_for_forms(forms: &[Expr]) -> Vec<Span> {
    let mut out = Vec::new();
    for form in forms {
        collect_top_level_expr_spans(form, &mut out);
    }
    out
}

fn collect_top_level_expr_spans(form: &Expr, out: &mut Vec<Span>) {
    if let ExprKind::List(items) = &form.kind {
        if let Some(head) = list_head_symbol_expr(items) {
            match head {
                "def" => {
                    collect_def_value_spans(items, out);
                    return;
                }
                "defn" => {
                    collect_defn_body_spans(items, out);
                    return;
                }
                "deftype" | "def-foreign" | "def-interop" | "interop" => {
                    return;
                }
                _ => {}
            }
        }
    }
    collect_expr_spans(form, out);
}

fn collect_def_value_spans(items: &[Expr], out: &mut Vec<Span>) {
    if items.len() < 3 {
        return;
    }
    let consumed = typed_name_consumed(items, 1);
    if consumed == 0 {
        return;
    }
    let value_index = 1 + consumed;
    if let Some(value) = items.get(value_index) {
        collect_expr_spans(value, out);
    }
}

fn collect_defn_body_spans(items: &[Expr], out: &mut Vec<Span>) {
    if items.len() < 4 {
        return;
    }
    let mut idx = 3;
    if let Some(Expr {
        kind: ExprKind::Symbol(sym),
        ..
    }) = items.get(idx)
    {
        if sym == "->" {
            idx += 2;
        }
    }
    for expr in items.iter().skip(idx) {
        collect_expr_spans(expr, out);
    }
}

fn typed_name_consumed(items: &[Expr], start: usize) -> usize {
    let Some(name_expr) = items.get(start) else {
        return 0;
    };
    let ExprKind::Symbol(name) = &name_expr.kind else {
        return 0;
    };
    if name.ends_with(':') {
        if items.get(start + 1).is_some() {
            return 2;
        }
        return 0;
    }
    1
}

fn collect_expr_spans(expr: &Expr, out: &mut Vec<Span>) {
    match &expr.kind {
        ExprKind::Literal(_)
        | ExprKind::Symbol(_)
        | ExprKind::Keyword(_)
        | ExprKind::ForeignBlock { .. } => {
            out.push(expr.span.clone());
        }
        ExprKind::Vector(items) | ExprKind::Set(items) => {
            for item in items {
                collect_expr_spans(item, out);
            }
            out.push(expr.span.clone());
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                collect_expr_spans(k, out);
                collect_expr_spans(v, out);
            }
            out.push(expr.span.clone());
        }
        ExprKind::List(items) => {
            collect_list_expr_spans(items, expr.span.clone(), out);
        }
    }
}

fn collect_list_expr_spans(items: &[Expr], span: Span, out: &mut Vec<Span>) {
    let Some(head) = list_head_symbol_expr(items) else {
        for arg in items.iter().skip(1) {
            collect_expr_spans(arg, out);
        }
        out.push(span);
        return;
    };
    match head {
        "if" => {
            if let Some(cond) = items.get(1) {
                collect_expr_spans(cond, out);
            }
            if let Some(then_expr) = items.get(2) {
                collect_expr_spans(then_expr, out);
            }
            if let Some(else_expr) = items.get(3) {
                collect_expr_spans(else_expr, out);
            }
            out.push(span);
        }
        "let" => {
            if let Some(bindings) = items.get(1) {
                collect_let_binding_spans(bindings, out);
            }
            for expr in items.iter().skip(2) {
                collect_expr_spans(expr, out);
            }
            out.push(span);
        }
        "fn" => {
            let mut idx = 2;
            if let Some(Expr {
                kind: ExprKind::Symbol(sym),
                ..
            }) = items.get(idx)
            {
                if sym == "->" {
                    idx += 2;
                }
            }
            for expr in items.iter().skip(idx) {
                collect_expr_spans(expr, out);
            }
            out.push(span);
        }
        "quote" => {
            out.push(span);
        }
        _ => {
            for arg in items.iter().skip(1) {
                collect_expr_spans(arg, out);
            }
            out.push(span);
        }
    }
}

fn collect_let_binding_spans(expr: &Expr, out: &mut Vec<Span>) {
    let ExprKind::Vector(items) = &expr.kind else {
        return;
    };
    let mut idx = 0;
    while idx < items.len() {
        let consumed = typed_name_consumed(items, idx);
        if consumed == 0 {
            break;
        }
        idx += consumed;
        if let Some(value) = items.get(idx) {
            collect_expr_spans(value, out);
        } else {
            break;
        }
        idx += 1;
    }
}

fn list_head_symbol_expr(items: &[Expr]) -> Option<&str> {
    match items.first() {
        Some(Expr {
            kind: ExprKind::Symbol(sym),
            ..
        }) => Some(sym.as_str()),
        _ => None,
    }
}

fn infer_known_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Option<Type> {
    if name == "json::read-file" {
        return Some(infer_json_read_file(args, env, diags));
    }
    if name == "json::read-string" {
        return Some(infer_json_read_string(args, env, diags));
    }
    if name == "json::write-string" {
        return Some(infer_json_write_string(args, env, diags, level));
    }
    if name == "json::write-file" {
        return Some(infer_json_write_file(args, env, diags, level));
    }
    if name == "identity" {
        return Some(infer_identity_call(args, env, diags, level));
    }
    if name == "constantly" {
        return Some(infer_constantly_call(args, env, diags, level));
    }
    if name == "partial" {
        return Some(infer_partial_call(args, env, diags, level));
    }
    if name == "complement" {
        return Some(infer_complement_call(args, env, diags, level));
    }
    if name == "pipe" {
        return Some(infer_pipe_call(args, env, diags, level));
    }
    if name == "comp" {
        return Some(infer_comp_call(args, env, diags, level));
    }
    if name == "juxt" {
        return Some(infer_juxt_call(args, env, diags, level));
    }
    if name == "get" {
        return Some(infer_get_call(args, env, diags, level));
    }
    if name == "map" {
        return Some(infer_map_call(args, env, diags, level));
    }
    if name == "map-indexed" {
        return Some(infer_map_indexed_call(args, env, diags, level));
    }
    if name == "mapcat" {
        return Some(infer_mapcat_call(args, env, diags, level));
    }
    if name == "filter" {
        return Some(infer_filter_call(args, env, diags, level));
    }
    if name == "remove" {
        return Some(infer_remove_call(args, env, diags, level));
    }
    if name == "keep" {
        return Some(infer_keep_call(args, env, diags, level));
    }
    if name == "keep-indexed" {
        return Some(infer_keep_indexed_call(args, env, diags, level));
    }
    if name == "every?" {
        return Some(infer_every_call(args, env, diags, level));
    }
    if name == "not-every?" {
        return Some(infer_not_every_call(args, env, diags, level));
    }
    if name == "not-any?" {
        return Some(infer_not_any_call(args, env, diags, level));
    }
    if name == "some" {
        return Some(infer_some_call(args, env, diags, level));
    }
    if matches!(name, "take-while" | "drop-while") {
        return Some(infer_take_drop_while_call(name, args, env, diags, level));
    }
    if name == "partition" {
        return Some(infer_partition_call(args, env, diags, level, false));
    }
    if name == "partition-all" {
        return Some(infer_partition_call(args, env, diags, level, true));
    }
    if name == "partition-by" {
        return Some(infer_partition_by_call(args, env, diags, level));
    }
    if name == "sort" {
        return Some(infer_sort_call(args, env, diags, level));
    }
    if name == "sort-by" {
        return Some(infer_sort_by_call(args, env, diags, level));
    }
    if name == "distinct" {
        return Some(infer_distinct_call(args, env, diags, level));
    }
    if name == "dedupe" {
        return Some(infer_dedupe_call(args, env, diags, level));
    }
    if name == "group-by" {
        return Some(infer_group_by_call(args, env, diags, level));
    }
    if name == "zip" {
        return Some(infer_zip_call(args, env, diags, level));
    }
    if name == "zip-with" {
        return Some(infer_zip_with_call(args, env, diags, level));
    }
    if name == "zipmap" {
        return Some(infer_zipmap_call(args, env, diags, level));
    }
    if name == "interpose" {
        return Some(infer_interpose_call(args, env, diags, level));
    }
    if name == "interleave" {
        return Some(infer_interleave_call(args, env, diags, level));
    }
    if name == "flatten" {
        return Some(infer_flatten_call(args, env, diags, level));
    }
    if matches!(name, "println" | "print" | "prn") {
        return Some(infer_print_call(name, args, env, diags, level));
    }
    if name == "pr-str" {
        return Some(infer_pr_str_call(args, env, diags, level));
    }
    if name == "str" {
        return Some(infer_str_call(args, env, diags, level));
    }
    if name == "not" {
        return Some(infer_not_call(args, env, diags, level));
    }
    if matches!(
        name,
        "=" | "!=" | "not=" | "<" | ">" | "<=" | ">=" | "compare"
    ) {
        return Some(infer_compare_call(name, args, env, diags, level));
    }
    if name == "count" {
        return Some(infer_count_call(args, env, diags, level));
    }
    if name == "empty?" {
        return Some(infer_empty_call(args, env, diags, level));
    }
    if name == "odd?" || name == "even?" {
        return Some(infer_parity_call(name, args, env, diags, level));
    }
    if matches!(name, "zero?" | "pos?" | "neg?") {
        return Some(infer_number_pred_call(name, args, env, diags, level));
    }
    if name == "nil?" {
        return Some(infer_nil_call(args, env, diags, level));
    }
    if matches!(
        name,
        "bool?"
            | "int?"
            | "float?"
            | "number?"
            | "str?"
            | "keyword?"
            | "symbol?"
            | "vec?"
            | "map?"
            | "fn?"
            | "boolean?"
            | "integer?"
            | "string?"
            | "vector?"
            | "coll?"
            | "sequential?"
            | "some?"
            | "true?"
            | "false?"
    ) {
        return Some(infer_predicate_call(name, args, env, diags, level));
    }
    if name == "nth" {
        return Some(infer_nth_call(args, env, diags, level));
    }
    if name == "seq" {
        return Some(infer_seq_call(args, env, diags, level));
    }
    if name == "reduce" {
        return Some(infer_reduce_call(args, env, diags, level));
    }
    if name == "reduce-kv" {
        return Some(infer_reduce_kv_call(args, env, diags, level));
    }
    if name == "apply" {
        return Some(infer_apply_call(args, env, diags, level));
    }
    if name == "merge" {
        return Some(infer_merge_call(args, env, diags, level));
    }
    if name == "merge-with" {
        return Some(infer_merge_with_call(args, env, diags, level));
    }
    if name == "contains?" {
        return Some(infer_contains_call(args, env, diags, level));
    }
    if name == "includes?" {
        return Some(infer_includes_call(args, env, diags, level));
    }
    if name == "get-in" {
        return Some(infer_get_in_call(args, env, diags, level));
    }
    if name == "dissoc" {
        return Some(infer_dissoc_call(args, env, diags, level));
    }
    if name == "assoc-in" {
        return Some(infer_assoc_in_call(args, env, diags, level));
    }
    if name == "update-in" {
        return Some(infer_update_in_call(args, env, diags, level));
    }
    if name == "take" {
        return Some(infer_take_call(args, env, diags, level));
    }
    if name == "drop" {
        return Some(infer_drop_call(args, env, diags, level));
    }
    if name == "take-last" {
        return Some(infer_take_last_call(args, env, diags, level));
    }
    if name == "drop-last" {
        return Some(infer_drop_last_call(args, env, diags, level));
    }
    if name == "reverse" {
        return Some(infer_reverse_call(args, env, diags, level));
    }
    if name == "shuffle" {
        return Some(infer_shuffle_call(args, env, diags, level));
    }
    if name == "concat" {
        return Some(infer_concat_call(args, env, diags, level));
    }
    if matches!(
        name,
        "starts-with?"
            | "ends-with?"
            | "trim"
            | "triml"
            | "trimr"
            | "upper-case"
            | "lower-case"
            | "reverse-str"
    ) {
        return Some(infer_string_call(name, args, env, diags, level));
    }
    if name == "split" {
        return Some(infer_split_call(args, env, diags, level));
    }
    if name == "join" {
        return Some(infer_join_call(args, env, diags, level));
    }
    if name == "format" {
        return Some(infer_format_call(args, env, diags, level));
    }
    if name == "subs" {
        return Some(infer_subs_call(args, env, diags, level));
    }
    if name == "slurp" {
        return Some(infer_slurp_call(args, env, diags, level));
    }
    if name == "spit" {
        return Some(infer_spit_call(args, env, diags, level));
    }
    if matches!(name, "re-find" | "re-matches" | "re-seq") {
        return Some(infer_regex_call(name, args, env, diags, level));
    }
    if name == "dorun" {
        return Some(infer_dorun_call(args, env, diags, level));
    }
    if name == "gensym" {
        return Some(infer_gensym_call(args, env, diags, level));
    }
    if name == "instance?" {
        return Some(infer_instance_call(args, env, diags, level));
    }
    if name == "time" {
        return Some(infer_time_call(args, env, diags, level));
    }
    if name == "bench" {
        return Some(infer_bench_call(args, env, diags, level));
    }
    if matches!(
        name,
        "blank?"
            | "replace-first"
            | "split-lines"
            | "lines"
            | "index-of"
            | "last-index-of"
            | "capitalize"
            | "trim-newline"
    ) {
        return Some(infer_string_extra_call(name, args, env, diags, level));
    }
    if name == "replace" {
        return Some(infer_replace_call(args, env, diags, level));
    }
    if name == "conj" {
        return Some(infer_conj_call(args, env, diags, level));
    }
    if name == "cons" {
        return Some(infer_cons_call(args, env, diags, level));
    }
    if name == "list" {
        return Some(infer_list_call(args, env, diags, level));
    }
    if name == "vector" {
        return Some(infer_vector_call(args, env, diags, level));
    }
    if name == "hash-map" {
        return Some(infer_hash_map_call(args, env, diags, level));
    }
    if name == "vec" {
        return Some(infer_vec_call(args, env, diags, level));
    }
    if name == "into" {
        return Some(infer_into_call(args, env, diags, level));
    }
    if name == "assoc" {
        return Some(infer_assoc_call(args, env, diags, level));
    }
    if name == "update" {
        return Some(infer_update_call(args, env, diags, level));
    }
    if name == "keys" {
        return Some(infer_keys_call(args, env, diags, level));
    }
    if name == "vals" {
        return Some(infer_vals_call(args, env, diags, level));
    }
    if name == "select-keys" {
        return Some(infer_select_keys_call(args, env, diags, level));
    }
    if name == "frequencies" {
        return Some(infer_frequencies_call(args, env, diags, level));
    }
    if name == "first" {
        return Some(infer_first_call(args, env, diags, level));
    }
    if name == "second" {
        return Some(infer_seq_head("second", args, env, diags, level));
    }
    if name == "last" {
        return Some(infer_last_call(args, env, diags, level));
    }
    if name == "peek" {
        return Some(infer_seq_head("peek", args, env, diags, level));
    }
    if name == "rest" {
        return Some(infer_rest_call(args, env, diags, level));
    }
    if name == "butlast" {
        return Some(infer_butlast_call(args, env, diags, level));
    }
    if name == "next" {
        return Some(infer_next_call(args, env, diags, level));
    }
    if name == "pop" {
        return Some(infer_pop_call(args, env, diags, level));
    }
    if name == "empty" {
        return Some(infer_empty_value_call(args, env, diags, level));
    }
    if name == "repeat" {
        return Some(infer_repeat_call(args, env, diags, level));
    }
    if name == "repeatedly" {
        return Some(infer_repeatedly_call(args, env, diags, level));
    }
    if name == "iterate" {
        return Some(infer_iterate_call(args, env, diags, level));
    }
    if name == "subvec" {
        return Some(infer_subvec_call(args, env, diags, level));
    }
    if name == "not-empty" {
        return Some(infer_not_empty_call(args, env, diags, level));
    }
    if matches!(
        name,
        "+" | "-" | "*" | "/" | "inc" | "dec" | "abs" | "min" | "max"
    ) {
        return Some(infer_numeric_op(name, args, env, diags, level));
    }
    if matches!(name, "mod" | "quot" | "rem") {
        return Some(infer_int_binary_call(name, args, env, diags, level));
    }
    if matches!(name, "bit-and" | "bit-or" | "bit-xor" | "bit-not") {
        return Some(infer_bit_op_call(name, args, env, diags, level));
    }
    if matches!(name, "bit-shift-left" | "bit-shift-right") {
        return Some(infer_bit_shift_call(name, args, env, diags, level));
    }
    if name == "rand" {
        return Some(infer_rand_call(args, env, diags, level));
    }
    if name == "rand-int" {
        return Some(infer_rand_int_call(args, env, diags, level));
    }
    if name == "rand-nth" {
        return Some(infer_rand_nth_call(args, env, diags, level));
    }
    if name == "range" {
        return Some(infer_range_call(args, env, diags, level));
    }
    if name == "name" {
        return Some(infer_name_call(args, env, diags, level));
    }
    if name == "namespace" {
        return Some(infer_namespace_call(args, env, diags, level));
    }
    if name == "keyword" {
        if args.len() != 1 && args.len() != 2 {
            diags.push(error_diag("keyword expects 1 or 2 arguments".to_string()));
            return Some(Type::Keyword);
        }
        for arg in args {
            let _ = infer_expr(arg, env, diags, level);
        }
        return Some(Type::Keyword);
    }
    let expected = match name {
        "int" => Type::Int,
        "float" => Type::Float,
        "bool" => Type::Bool,
        "symbol" => Type::Symbol,
        _ => return None,
    };
    if args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
        return Some(expected);
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    report_optional_usage(&arg_ty, level, diags, name);
    Some(expected)
}

fn infer_range_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() > 3 {
        diags.push(error_diag("range expects 0 to 3 arguments".to_string()));
        return Type::Vec(Box::new(Type::Int));
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Int, env) {
            if is_optional_assignable(&arg_ty, &Type::Int, env) {
                report_optional_usage(&arg_ty, level, diags, "range");
            } else {
                diags.push(error_diag(format!(
                    "range expects Int arguments, got {}",
                    arg_ty
                )));
            }
        }
    }
    Type::Vec(Box::new(Type::Int))
}

fn infer_rand_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() > 1 {
        diags.push(error_diag(
            "rand expects zero or one numeric argument".to_string(),
        ));
        return Type::Float;
    }
    if args.len() == 1 {
        let arg_ty = infer_expr(&args[0], env, diags, level);
        if contains_any(&arg_ty) || contains_dyn(&arg_ty) {
            report_unresolved(&arg_ty, level, diags, "rand".to_string());
        } else if !is_numeric_type(&arg_ty) {
            if let Some(stripped) = strip_nil(&arg_ty) {
                if is_numeric_type(&stripped) {
                    report_optional_usage(&arg_ty, level, diags, "rand");
                } else {
                    diags.push(error_diag(format!("rand expects number, got {}", arg_ty)));
                }
            } else {
                diags.push(error_diag(format!("rand expects number, got {}", arg_ty)));
            }
        }
    }
    Type::Float
}

fn infer_rand_int_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("rand-int expects 1 argument".to_string()));
        return Type::Int;
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&arg_ty, &Type::Int, env) {
        if is_optional_assignable(&arg_ty, &Type::Int, env) {
            report_optional_usage(&arg_ty, level, diags, "rand-int");
        } else {
            diags.push(error_diag(format!("rand-int expects Int, got {}", arg_ty)));
        }
    }
    Type::Int
}

fn infer_rand_nth_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("rand-nth expects 1 argument".to_string()));
        return Type::Any;
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => *inner,
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "rand-nth");
                        *inner
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, "rand-nth");
                        Type::Str
                    }
                    _ => {
                        diags.push(error_diag(format!(
                            "rand-nth expects vector or string, got {}",
                            other
                        )));
                        Type::Any
                    }
                }
            } else {
                diags.push(error_diag(format!(
                    "rand-nth expects vector or string, got {}",
                    other
                )));
                Type::Any
            }
        }
    }
}

fn infer_numeric_op(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return if name == "/" {
            Type::Float
        } else {
            Type::Number
        };
    }
    if matches!(name, "inc" | "dec" | "abs") && args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
    }
    let mut has_float = false;
    let mut has_unknown = false;
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if contains_any(&arg_ty) || contains_dyn(&arg_ty) {
            report_unresolved(&arg_ty, level, diags, name.to_string());
            has_unknown = true;
            continue;
        }
        if !is_numeric_type(&arg_ty) {
            if let Some(stripped) = strip_nil(&arg_ty) {
                if is_numeric_type(&stripped) {
                    report_optional_usage(&arg_ty, level, diags, name);
                    continue;
                }
            }
            diags.push(error_diag(format!(
                "{} expects numeric arguments, got {}",
                name, arg_ty
            )));
        }
        if matches!(arg_ty, Type::Float) {
            has_float = true;
        }
    }
    if name == "/" || has_float {
        Type::Float
    } else if has_unknown {
        Type::Number
    } else {
        Type::Int
    }
}

fn infer_numeric_return_from_types(
    name: &str,
    arg_tys: &[Type],
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
    ctx: &str,
) -> Type {
    if arg_tys.is_empty() {
        return if name == "/" {
            Type::Float
        } else {
            Type::Number
        };
    }
    let mut has_float = false;
    let mut has_unknown = false;
    for arg_ty in arg_tys {
        if contains_any(arg_ty) || contains_dyn(arg_ty) {
            report_unresolved(arg_ty, level, diags, ctx.to_string());
            has_unknown = true;
            continue;
        }
        if !is_numeric_like(arg_ty) {
            if let Some(stripped) = strip_nil(arg_ty) {
                if is_numeric_like(&stripped) {
                    report_optional_usage(arg_ty, level, diags, ctx);
                    if matches!(stripped, Type::Float) {
                        has_float = true;
                    } else if !matches!(stripped, Type::Int) {
                        has_unknown = true;
                    }
                    continue;
                }
            }
            diags.push(error_diag(format!(
                "{} expects numeric arguments, got {}",
                ctx, arg_ty
            )));
            continue;
        }
        match arg_ty {
            Type::Float => has_float = true,
            Type::Int => {}
            _ => has_unknown = true,
        }
    }
    if name == "/" || has_float {
        Type::Float
    } else if has_unknown {
        Type::Number
    } else {
        Type::Int
    }
}

fn infer_int_binary_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag(format!("{} expects 2 arguments", name)));
        return Type::Int;
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Int, env) {
            if is_optional_assignable(&arg_ty, &Type::Int, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!(
                    "{} expects Int arguments, got {}",
                    name, arg_ty
                )));
            }
        }
    }
    Type::Int
}

fn infer_bit_op_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let (min_arity, max_arity) = if name == "bit-not" {
        (1, 1)
    } else {
        (0, usize::MAX)
    };
    if args.len() < min_arity || args.len() > max_arity {
        if name == "bit-not" {
            diags.push(error_diag("bit-not expects 1 argument".to_string()));
        } else {
            diags.push(error_diag(format!("{} expects at least 1 argument", name)));
        }
        return Type::Int;
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Int, env) {
            if is_optional_assignable(&arg_ty, &Type::Int, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!(
                    "{} expects Int arguments, got {}",
                    name, arg_ty
                )));
            }
        }
    }
    Type::Int
}

fn infer_bit_shift_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag(format!("{} expects 2 arguments", name)));
        return Type::Int;
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Int, env) {
            if is_optional_assignable(&arg_ty, &Type::Int, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!(
                    "{} expects Int arguments, got {}",
                    name, arg_ty
                )));
            }
        }
    }
    Type::Int
}

fn extract_unary_fn_types(
    fn_ty: Type,
    err_msg: &str,
    diags: &mut Vec<Diagnostic>,
) -> Option<(Type, Type)> {
    match fn_ty {
        Type::Function { params, rest, ret } => {
            if params.len() == 1 {
                Some((params[0].clone(), *ret))
            } else if params.is_empty() {
                if let Some(rest_ty) = rest {
                    if let Type::Vec(inner) = *rest_ty {
                        Some((*inner, *ret))
                    } else {
                        diags.push(error_diag(err_msg.to_string()));
                        None
                    }
                } else {
                    diags.push(error_diag(err_msg.to_string()));
                    None
                }
            } else {
                diags.push(error_diag(err_msg.to_string()));
                None
            }
        }
        _ => {
            diags.push(error_diag(err_msg.to_string()));
            None
        }
    }
}

fn map_like_call_signature(fn_ty: &Type, env: &TypeEnv) -> Option<(Type, Type)> {
    match fn_ty {
        Type::Map(key, val) => Some(((**key).clone(), merge_types((**val).clone(), Type::Nil))),
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            Some((Type::Keyword, merge_types(value_union, Type::Nil)))
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let key_ty = Type::union_two(Type::Str, Type::Keyword);
            Some((key_ty, merge_types(value_union, Type::Nil)))
        }
        Type::Named(name) => env.get_type(name).map(|fields| {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let key_ty = Type::union_two(Type::Str, Type::Keyword);
            (key_ty, merge_types(value_union, Type::Nil))
        }),
        Type::Any | Type::Dyn | Type::DynOf(_) => Some((Type::Any, Type::Any)),
        _ => None,
    }
}

fn extract_unary_callable_types(
    fn_expr: &AstExpr,
    fn_ty: Type,
    env: &TypeEnv,
    diags: &mut Vec<Diagnostic>,
    err_msg: &str,
) -> Option<(Type, Type)> {
    if matches!(fn_expr, AstExpr::Set(_)) {
        let elem_ty = match fn_ty {
            Type::Vec(inner) => *inner,
            Type::Tuple(items) => Type::union(items),
            other => other,
        };
        return Some((elem_ty.clone(), merge_types(elem_ty, Type::Nil)));
    }
    if let Some(sig) = map_like_call_signature(&fn_ty, env) {
        return Some(sig);
    }
    extract_unary_fn_types(fn_ty, err_msg, diags)
}

fn extract_fixed_fn_types(
    fn_ty: Type,
    arity: usize,
    err_msg: &str,
    diags: &mut Vec<Diagnostic>,
) -> Option<(Vec<Type>, Type)> {
    match fn_ty {
        Type::Function { params, ret, .. } if params.len() == arity => Some((params, *ret)),
        _ => {
            diags.push(error_diag(err_msg.to_string()));
            None
        }
    }
}

fn extract_zero_arg_fn_ret(
    fn_ty: Type,
    err_msg: &str,
    diags: &mut Vec<Diagnostic>,
) -> Option<Type> {
    match fn_ty {
        Type::Function { params, ret, .. } => {
            if params.is_empty() {
                Some(*ret)
            } else {
                diags.push(error_diag(err_msg.to_string()));
                None
            }
        }
        _ => {
            diags.push(error_diag(err_msg.to_string()));
            None
        }
    }
}

struct BindingSugar<'a> {
    name: String,
    coll: &'a AstExpr,
    body: &'a AstExpr,
}

fn extract_binding_sugar<'a>(
    args: &'a [AstExpr],
    name: &str,
    diags: &mut Vec<Diagnostic>,
) -> Result<Option<BindingSugar<'a>>, ()> {
    if args.len() != 2 {
        return Ok(None);
    }
    let AstExpr::Vector(bindings) = &args[0] else {
        return Ok(None);
    };
    if bindings.len() != 2 {
        diags.push(error_diag(format!(
            "{} binding form expects [name coll]",
            name
        )));
        return Err(());
    }
    let AstExpr::Symbol(sym) = &bindings[0] else {
        diags.push(error_diag(format!(
            "{} binding form expects symbol name",
            name
        )));
        return Err(());
    };
    Ok(Some(BindingSugar {
        name: sym.clone(),
        coll: &bindings[1],
        body: &args[1],
    }))
}

fn expect_vec_like_elem(
    raw_ty: Type,
    level: NativeLevel,
    diags: &mut Vec<Diagnostic>,
    ctx: &str,
) -> Option<Type> {
    match raw_ty {
        Type::Vec(inner) => Some(*inner),
        Type::Tuple(items) => Some(Type::union(items)),
        Type::Str => Some(Type::Str),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, ctx);
                        Some(*inner)
                    }
                    Type::Tuple(items) => {
                        report_optional_usage(&other, level, diags, ctx);
                        Some(Type::union(items))
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, ctx);
                        Some(Type::Str)
                    }
                    _ => {
                        diags.push(error_diag(format!(
                            "{} expects vector or string, got {}",
                            ctx, other
                        )));
                        None
                    }
                }
            } else {
                diags.push(error_diag(format!(
                    "{} expects vector or string, got {}",
                    ctx, other
                )));
                None
            }
        }
    }
}

fn function_param_type_at(params: &[Type], rest: Option<&Type>, index: usize) -> Option<Type> {
    if let Some(param) = params.get(index) {
        return Some(param.clone());
    }
    rest.and_then(rest_elem_type)
}

fn function_param_types_for_arity(
    params: &[Type],
    rest: Option<&Type>,
    arity: usize,
) -> Option<Vec<Type>> {
    let mut out = Vec::with_capacity(arity);
    for idx in 0..arity {
        let Some(param) = function_param_type_at(params, rest, idx) else {
            return None;
        };
        out.push(param);
    }
    Some(out)
}

fn infer_inline_fn_with_expected_params(
    fn_expr: &AstExpr,
    expected_params: &[Type],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
    ctx: &str,
) -> Option<Type> {
    let AstExpr::Fn { params, ret, body } = fn_expr else {
        return None;
    };
    if params.len() != expected_params.len() || params.iter().any(|param| param.rest) {
        diags.push(error_diag(format!(
            "{} expects a function of arity {}",
            ctx,
            expected_params.len()
        )));
        return None;
    }
    let mut fn_env = env.child();
    let mut param_types = Vec::with_capacity(params.len());
    for (param, expected) in params.iter().zip(expected_params.iter()) {
        let ty = if let Some(ann) = &param.ty {
            if !is_assignable_with_env(expected, ann, env) {
                if is_optional_assignable(expected, ann, env) {
                    report_optional_usage(expected, level, diags, ctx);
                } else {
                    diags.push(error_diag(format!(
                        "{} expects {}, got {}",
                        ctx, ann, expected
                    )));
                }
            }
            ann.clone()
        } else {
            expected.clone()
        };
        fn_env.set(&param.name, ty.clone());
        param_types.push(ty);
    }
    let body_ty = infer_body(body, &mut fn_env, diags, level);
    let ret_ty = if let Some(ret_ty) = ret {
        if !is_assignable_with_env(&body_ty, ret_ty, env) {
            if is_optional_assignable(&body_ty, ret_ty, env) {
                report_optional_usage(&body_ty, level, diags, ctx);
            } else {
                diags.push(error_diag(format!(
                    "{} expects {}, got {}",
                    ctx, ret_ty, body_ty
                )));
            }
        }
        ret_ty.clone()
    } else {
        body_ty
    };
    Some(Type::Function {
        params: param_types,
        rest: None,
        ret: Box::new(ret_ty),
    })
}

fn infer_map_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "map", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "map") else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let body_ty = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(body_ty));
    }
    if args.len() < 2 {
        diags.push(error_diag("map expects at least 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Symbol(sym) = &args[0] {
        if sym == "identity" && args.len() == 2 {
            let coll_ty = infer_expr(&args[1], env, diags, level);
            let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "map") {
                Some(elem) => elem,
                None => Type::Any,
            };
            return Type::Vec(Box::new(elem_ty));
        }
        if matches!(
            sym.as_str(),
            "+" | "-" | "*" | "/" | "inc" | "dec" | "abs" | "min" | "max"
        ) {
            let coll_count = args.len() - 1;
            if matches!(sym.as_str(), "inc" | "dec" | "abs") && coll_count != 1 {
                // Delegate to existing function type checks
            } else {
                let mut elem_tys = Vec::with_capacity(coll_count);
                for arg in args.iter().skip(1) {
                    let coll_ty = infer_expr(arg, env, diags, level);
                    let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "map") {
                        Some(elem) => elem,
                        None => Type::Any,
                    };
                    elem_tys.push(elem_ty);
                }
                let ret = infer_numeric_return_from_types(sym, &elem_tys, diags, level, "map");
                return Type::Vec(Box::new(ret));
            }
        }
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let mut expected_params = Vec::with_capacity(args.len() - 1);
        for arg in args.iter().skip(1) {
            let coll_ty = infer_expr(arg, env, diags, level);
            let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "map") {
                Some(elem) => elem,
                None => Type::Any,
            };
            expected_params.push(elem_ty);
        }
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &expected_params,
            env,
            diags,
            level,
            "map",
        ) {
            return Type::Vec(ret);
        }
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = if matches!(&args[0], AstExpr::Literal(crate::ast::Literal::Regex(_))) {
        Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(Type::union(vec![Type::Str, Type::Nil])),
        }
    } else {
        let fn_ty = match &args[0] {
            AstExpr::Symbol(sym) => env.get(sym),
            _ => None,
        };
        let arg_fn_ty = infer_expr(&args[0], env, diags, level);
        match fn_ty {
            Some(ty) => ty,
            None => arg_fn_ty,
        }
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag("map expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    let coll_count = args.len() - 1;
    let Some(param_tys) = function_param_types_for_arity(&params, rest.as_deref(), coll_count)
    else {
        diags.push(error_diag("map expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    for (arg, param_ty) in args.iter().skip(1).zip(param_tys.iter()) {
        let coll_ty = infer_expr(arg, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "map") else {
            continue;
        };
        if !is_assignable_with_env(&elem_ty, param_ty, env) {
            if is_optional_assignable(&elem_ty, param_ty, env) {
                report_optional_usage(&elem_ty, level, diags, "map");
            } else {
                diags.push(error_diag(format!(
                    "map expects {}, got {}",
                    param_ty, elem_ty
                )));
            }
        }
    }
    Type::Vec(ret)
}

fn infer_map_indexed_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("map-indexed expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "map-indexed") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if let AstExpr::Fn { .. } = &args[0] {
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[Type::Int, elem_ty.clone()],
            env,
            diags,
            level,
            "map-indexed",
        ) {
            return Type::Vec(ret);
        }
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag("map-indexed expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    let Some(param_tys) = function_param_types_for_arity(&params, rest.as_deref(), 2) else {
        diags.push(error_diag("map-indexed expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    let param_ty = (param_tys[0].clone(), param_tys[1].clone());
    if !is_assignable_with_env(&param_ty.0, &Type::Int, env) {
        if is_optional_assignable(&param_ty.0, &Type::Int, env) {
            report_optional_usage(&param_ty.0, level, diags, "map-indexed");
        } else {
            diags.push(error_diag(format!(
                "map-indexed expects Int index, got {}",
                param_ty.0
            )));
        }
    }
    if !is_assignable_with_env(&elem_ty, &param_ty.1, env) {
        if is_optional_assignable(&elem_ty, &param_ty.1, env) {
            report_optional_usage(&elem_ty, level, diags, "map-indexed");
        } else {
            diags.push(error_diag(format!(
                "map-indexed expects {}, got {}",
                param_ty.1, elem_ty
            )));
        }
    }
    Type::Vec(ret)
}

fn infer_mapcat_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    fn mapcat_return_from_type(
        ty: &Type,
        level: NativeLevel,
        diags: &mut Vec<Diagnostic>,
    ) -> Option<Type> {
        let Some(stripped) = strip_nil(ty) else {
            return None;
        };
        if &stripped != ty {
            report_optional_usage(ty, level, diags, "mapcat");
        }
        match stripped {
            Type::Vec(inner) => Some(Type::Vec(inner)),
            Type::Tuple(items) => Some(Type::Vec(Box::new(Type::union(items)))),
            Type::Str => Some(Type::Vec(Box::new(Type::Str))),
            Type::Union(items) => {
                let mut inner_types = Vec::new();
                for item in items {
                    match item {
                        Type::Vec(inner) => inner_types.push(*inner),
                        Type::Tuple(items) => inner_types.push(Type::union(items)),
                        Type::Str => inner_types.push(Type::Str),
                        _ => return None,
                    }
                }
                if inner_types.len() > 1 {
                    inner_types.retain(|item| *item != Type::Any);
                    if inner_types.is_empty() {
                        inner_types.push(Type::Any);
                    }
                }
                Some(Type::Vec(Box::new(Type::union(inner_types))))
            }
            _ => None,
        }
    }

    if let Ok(Some(binding)) = extract_binding_sugar(args, "mapcat", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "mapcat") else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let body_ty = infer_expr(binding.body, &mut fn_env, diags, level);
        if let Some(mapped) = mapcat_return_from_type(&body_ty, level, diags) {
            return mapped;
        }
        diags.push(error_diag(format!(
            "mapcat expects vector return, got {}",
            body_ty
        )));
        return Type::Vec(Box::new(Type::Any));
    }
    if args.len() < 2 {
        diags.push(error_diag(
            "mapcat expects at least 2 arguments".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let mut expected_params = Vec::with_capacity(args.len() - 1);
        for arg in args.iter().skip(1) {
            let coll_ty = infer_expr(arg, env, diags, level);
            let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "mapcat") {
                Some(elem) => elem,
                None => Type::Any,
            };
            expected_params.push(elem_ty);
        }
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &expected_params,
            env,
            diags,
            level,
            "mapcat",
        ) {
            if let Some(mapped) = mapcat_return_from_type(ret.as_ref(), level, diags) {
                return mapped;
            }
            diags.push(error_diag(format!(
                "mapcat expects vector return, got {}",
                ret
            )));
            return Type::Vec(Box::new(Type::Any));
        }
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag("mapcat expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    let coll_count = args.len() - 1;
    let Some(param_tys) = function_param_types_for_arity(&params, rest.as_deref(), coll_count)
    else {
        diags.push(error_diag("mapcat expects a function".to_string()));
        return Type::Vec(Box::new(Type::Any));
    };
    for (arg, param_ty) in args.iter().skip(1).zip(param_tys.iter()) {
        let coll_ty = infer_expr(arg, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "mapcat") else {
            continue;
        };
        if !is_assignable_with_env(&elem_ty, param_ty, env) {
            if is_optional_assignable(&elem_ty, param_ty, env) {
                report_optional_usage(&elem_ty, level, diags, "mapcat");
            } else {
                diags.push(error_diag(format!(
                    "mapcat expects {}, got {}",
                    param_ty, elem_ty
                )));
            }
        }
    }
    let ret_ty = *ret;
    if let Some(mapped) = mapcat_return_from_type(&ret_ty, level, diags) {
        return mapped;
    }
    diags.push(error_diag(format!(
        "mapcat expects vector return, got {}",
        ret_ty
    )));
    Type::Vec(Box::new(Type::Any))
}

fn infer_filter_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "filter", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "filter") else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(elem_ty));
    }
    if args.len() != 2 {
        diags.push(error_diag("filter expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "filter") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            "filter",
        ) {
            if !is_assignable_with_env(ret.as_ref(), &Type::Bool, env) {
                if is_optional_assignable(ret.as_ref(), &Type::Bool, env) {
                    report_optional_usage(ret.as_ref(), level, diags, "filter");
                } else {
                    diags.push(error_diag(format!(
                        "filter expects Bool, got {}",
                        ret.as_ref()
                    )));
                }
            }
        }
        return Type::Vec(Box::new(elem_ty));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "filter expects a predicate",
    ) {
        Some(types) => types,
        None => return Type::Vec(Box::new(Type::Any)),
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "filter") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "filter");
        } else {
            diags.push(error_diag(format!(
                "filter expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Vec(Box::new(elem_ty))
}

fn infer_remove_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "remove", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "remove") else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(elem_ty));
    }
    if args.len() != 2 {
        diags.push(error_diag("remove expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "remove") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            "remove",
        ) {
            if !is_assignable_with_env(ret.as_ref(), &Type::Bool, env) {
                if is_optional_assignable(ret.as_ref(), &Type::Bool, env) {
                    report_optional_usage(ret.as_ref(), level, diags, "remove");
                } else {
                    diags.push(error_diag(format!(
                        "remove expects Bool, got {}",
                        ret.as_ref()
                    )));
                }
            }
        }
        return Type::Vec(Box::new(elem_ty));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "remove expects a predicate",
    ) {
        Some(types) => types,
        None => return Type::Vec(Box::new(Type::Any)),
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "remove") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "remove");
        } else {
            diags.push(error_diag(format!(
                "remove expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Vec(Box::new(elem_ty))
}

fn infer_keep_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "keep", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "keep") else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let body_ty = infer_expr(binding.body, &mut fn_env, diags, level);
        let out_ty = strip_nil(&body_ty).unwrap_or(Type::Any);
        return Type::Vec(Box::new(out_ty));
    }
    if args.len() != 2 {
        diags.push(error_diag("keep expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "keep") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            "keep",
        ) {
            let out_ty = strip_nil(ret.as_ref()).unwrap_or(Type::Any);
            return Type::Vec(Box::new(out_ty));
        }
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "keep expects a function",
    ) {
        Some(types) => types,
        None => return Type::Vec(Box::new(Type::Any)),
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "keep") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "keep");
        } else {
            diags.push(error_diag(format!(
                "keep expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    let refined_ret = if let AstExpr::Symbol(sym) = &args[0] {
        if let Some(refined) = seq_head_type_for_elem(sym, &elem_ty) {
            refined
        } else {
            ret_ty
        }
    } else {
        ret_ty
    };
    let out_ty = strip_nil(&refined_ret).unwrap_or(Type::Any);
    Type::Vec(Box::new(out_ty))
}

fn infer_keep_indexed_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("keep-indexed expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "keep-indexed") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[Type::Int, elem_ty.clone()],
            env,
            diags,
            level,
            "keep-indexed",
        ) {
            let out_ty = strip_nil(ret.as_ref()).unwrap_or(Type::Any);
            return Type::Vec(Box::new(out_ty));
        }
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag(
            "keep-indexed expects a function of arity 2".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    };
    let Some(param_tys) = function_param_types_for_arity(&params, rest.as_deref(), 2) else {
        diags.push(error_diag(
            "keep-indexed expects a function of arity 2".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    };
    if !is_assignable_with_env(&param_tys[0], &Type::Int, env) {
        if is_optional_assignable(&param_tys[0], &Type::Int, env) {
            report_optional_usage(&param_tys[0], level, diags, "keep-indexed");
        } else {
            diags.push(error_diag(format!(
                "keep-indexed expects Int index, got {}",
                param_tys[0]
            )));
        }
    }
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "keep-indexed") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_tys[1], env) {
        if is_optional_assignable(&elem_ty, &param_tys[1], env) {
            report_optional_usage(&elem_ty, level, diags, "keep-indexed");
        } else {
            diags.push(error_diag(format!(
                "keep-indexed expects {}, got {}",
                param_tys[1], elem_ty
            )));
        }
    }
    let out_ty = strip_nil(ret.as_ref()).unwrap_or(Type::Any);
    Type::Vec(Box::new(out_ty))
}

fn infer_every_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "every?", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "every?") else {
            return Type::Bool;
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Bool;
    }
    if args.len() != 2 {
        diags.push(error_diag("every? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "every? expects a predicate",
    ) {
        Some(types) => types,
        None => return Type::Bool,
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "every?") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "every?");
        } else {
            diags.push(error_diag(format!(
                "every? expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Bool
}

fn infer_not_every_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "not-every?", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "not-every?") else {
            return Type::Bool;
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Bool;
    }
    if args.len() != 2 {
        diags.push(error_diag("not-every? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "not-every? expects a predicate",
    ) {
        Some(types) => types,
        None => return Type::Bool,
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "not-every?") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "not-every?");
        } else {
            diags.push(error_diag(format!(
                "not-every? expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Bool
}

fn infer_not_any_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "not-any?", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "not-any?") else {
            return Type::Bool;
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Bool;
    }
    if args.len() != 2 {
        diags.push(error_diag("not-any? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "not-any? expects a predicate",
    ) {
        Some(types) => types,
        None => return Type::Bool,
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "not-any?") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "not-any?");
        } else {
            diags.push(error_diag(format!(
                "not-any? expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Bool
}

fn infer_some_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "some", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "some") else {
            return Type::union(vec![Type::Any, Type::Nil]);
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty);
        let body_ty = infer_expr(binding.body, &mut fn_env, diags, level);
        return merge_types(body_ty, Type::Nil);
    }
    if args.len() != 2 {
        diags.push(error_diag("some expects 2 arguments".to_string()));
        return Type::union(vec![Type::Any, Type::Nil]);
    }
    if let AstExpr::Fn { .. } = &args[0] {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "some") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            "some",
        ) {
            return merge_types((*ret).clone(), Type::Nil);
        }
        return Type::union(vec![Type::Any, Type::Nil]);
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "some expects a function",
    ) {
        Some(types) => types,
        None => return Type::union(vec![Type::Any, Type::Nil]),
    };
    let collection_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(collection_ty, level, diags, "some") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "some");
        } else {
            diags.push(error_diag(format!(
                "some expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    merge_types(ret_ty, Type::Nil)
}

fn infer_take_drop_while_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, name, diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, name) else {
            return Type::Vec(Box::new(Type::Any));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(elem_ty));
    }
    if args.len() != 2 {
        diags.push(error_diag(format!("{} expects 2 arguments", name)));
        return Type::Vec(Box::new(Type::Any));
    }
    let elem_ty = if let AstExpr::Call { callee, args } = &args[1] {
        if let AstExpr::Symbol(sym) = callee.as_ref() {
            if sym == "iterate" && args.len() == 2 {
                let iter_fn_ty = infer_expr(&args[0], env, diags, level);
                let init_ty = infer_expr(&args[1], env, diags, level);
                let iter_ret = match iter_fn_ty {
                    Type::Function { ret, .. } => (*ret).clone(),
                    other => other,
                };
                merge_types(init_ty, iter_ret)
            } else {
                let collection_ty = infer_expr(&args[1], env, diags, level);
                match expect_vec_like_elem(collection_ty, level, diags, name) {
                    Some(elem) => elem,
                    None => Type::Any,
                }
            }
        } else {
            let collection_ty = infer_expr(&args[1], env, diags, level);
            match expect_vec_like_elem(collection_ty, level, diags, name) {
                Some(elem) => elem,
                None => Type::Any,
            }
        }
    } else {
        let collection_ty = infer_expr(&args[1], env, diags, level);
        match expect_vec_like_elem(collection_ty, level, diags, name) {
            Some(elem) => elem,
            None => Type::Any,
        }
    };
    if let AstExpr::Fn { .. } = &args[0] {
        let _ = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            name,
        );
        return Type::Vec(Box::new(elem_ty));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        &format!("{} expects a predicate", name),
    ) {
        Some(types) => types,
        None => return Type::Vec(Box::new(Type::Any)),
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, name);
        } else {
            diags.push(error_diag(format!(
                "{} expects {}, got {}",
                name, param_ty, elem_ty
            )));
        }
    }
    Type::Vec(Box::new(elem_ty))
}

fn infer_partition_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
    allow_incomplete: bool,
) -> Type {
    let _ = allow_incomplete;
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("partition expects 2 or 3 arguments".to_string()));
        return Type::Vec(Box::new(Type::Vec(Box::new(Type::Any))));
    }
    let size_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&size_ty, &Type::Int, env) {
        if is_optional_assignable(&size_ty, &Type::Int, env) {
            report_optional_usage(&size_ty, level, diags, "partition");
        } else {
            diags.push(error_diag(format!(
                "partition expects Int size, got {}",
                size_ty
            )));
        }
    }
    let coll_arg = if args.len() == 3 { &args[2] } else { &args[1] };
    if args.len() == 3 {
        let step_arg = &args[1];
        let step_ty = infer_expr(step_arg, env, diags, level);
        if !is_assignable_with_env(&step_ty, &Type::Int, env) {
            if is_optional_assignable(&step_ty, &Type::Int, env) {
                report_optional_usage(&step_ty, level, diags, "partition");
            } else {
                diags.push(error_diag(format!(
                    "partition expects Int step, got {}",
                    step_ty
                )));
            }
        }
    }
    let coll_ty = infer_expr(coll_arg, env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(Box::new(Type::Vec(inner))),
        Type::Str => Type::Vec(Box::new(Type::Vec(Box::new(Type::Str)))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "partition");
                    return Type::Vec(Box::new(Type::Vec(inner)));
                }
                if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "partition");
                    return Type::Vec(Box::new(Type::Vec(Box::new(Type::Str))));
                }
            }
            diags.push(error_diag(format!(
                "partition expects vector or string, got {}",
                other
            )));
            Type::Vec(Box::new(Type::Vec(Box::new(Type::Any))))
        }
    }
}

fn infer_partition_by_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "partition-by", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "partition-by") else {
            return Type::Vec(Box::new(Type::Vec(Box::new(Type::Any))));
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(Type::Vec(Box::new(elem_ty))));
    }
    if args.len() != 2 {
        diags.push(error_diag("partition-by expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Vec(Box::new(Type::Any))));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, _ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "partition-by expects a function",
    ) {
        Some(types) => types,
        None => return Type::Vec(Box::new(Type::Vec(Box::new(Type::Any)))),
    };
    let coll_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "partition-by") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "partition-by");
        } else {
            diags.push(error_diag(format!(
                "partition-by expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    Type::Vec(Box::new(Type::Vec(Box::new(elem_ty))))
}

fn infer_sort_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag("sort expects 1 or 2 arguments".to_string()));
        return Type::Any;
    }
    if args.len() == 2 {
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "sort") {
            Some(elem) => elem,
            None => Type::Any,
        };
        if let AstExpr::Fn { .. } = &args[0] {
            let _ = infer_inline_fn_with_expected_params(
                &args[0],
                &[elem_ty.clone(), elem_ty.clone()],
                env,
                diags,
                level,
                "sort",
            );
        } else {
            let _ = infer_expr(&args[0], env, diags, level);
        }
        return Type::Vec(Box::new(elem_ty));
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "sort");
                        return Type::Vec(inner);
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, "sort");
                        return Type::Str;
                    }
                    Type::Tuple(items) => {
                        report_optional_usage(&other, level, diags, "sort");
                        return Type::Vec(Box::new(Type::union(items)));
                    }
                    _ => {}
                }
            }
            diags.push(error_diag(format!(
                "sort expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn seq_head_type_for_elem(name: &str, elem_ty: &Type) -> Option<Type> {
    match elem_ty {
        Type::Vec(inner) => Some((**inner).clone()),
        Type::Tuple(items) => match name {
            "first" | "peek" => items.get(0).cloned(),
            "second" => items.get(1).cloned(),
            "last" => items.last().cloned(),
            _ => None,
        },
        Type::Str => Some(Type::Str),
        Type::Union(items) => {
            let mut out = Vec::new();
            for item in items {
                if let Some(next) = seq_head_type_for_elem(name, item) {
                    out.push(next);
                }
            }
            if out.is_empty() {
                None
            } else {
                Some(Type::union(out))
            }
        }
        _ => None,
    }
}

fn infer_sort_by_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "sort-by", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "sort-by") else {
            return Type::Any;
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let _ = infer_expr(binding.body, &mut fn_env, diags, level);
        return Type::Vec(Box::new(elem_ty));
    }
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("sort-by expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let coll_arg = if args.len() == 3 { &args[2] } else { &args[1] };
    let coll_ty = infer_expr(coll_arg, env, diags, level);
    let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "sort-by") {
        Some(elem) => elem,
        None => Type::Any,
    };
    let key_ret_ty = if let AstExpr::Fn { .. } = &args[0] {
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[elem_ty.clone()],
            env,
            diags,
            level,
            "sort-by",
        ) {
            (*ret).clone()
        } else {
            Type::Any
        }
    } else {
        let fn_ty = match &args[0] {
            AstExpr::Symbol(sym) => env.get(sym),
            _ => None,
        };
        let arg_fn_ty = infer_expr(&args[0], env, diags, level);
        let fn_ty = match fn_ty {
            Some(ty) => ty,
            None => arg_fn_ty,
        };
        let (param_ty, ret_ty) = match extract_unary_callable_types(
            &args[0],
            fn_ty,
            env,
            diags,
            "sort-by expects a function",
        ) {
            Some(types) => types,
            None => return Type::Any,
        };
        if !is_assignable_with_env(&elem_ty, &param_ty, env) {
            if is_optional_assignable(&elem_ty, &param_ty, env) {
                report_optional_usage(&elem_ty, level, diags, "sort-by");
            } else {
                diags.push(error_diag(format!(
                    "sort-by expects {}, got {}",
                    param_ty, elem_ty
                )));
            }
        }
        if let AstExpr::Symbol(sym) = &args[0] {
            if let Some(refined) = seq_head_type_for_elem(sym, &elem_ty) {
                refined
            } else {
                ret_ty
            }
        } else {
            ret_ty
        }
    };
    if args.len() == 3 {
        if let AstExpr::Fn { .. } = &args[1] {
            let _ = infer_inline_fn_with_expected_params(
                &args[1],
                &[key_ret_ty.clone(), key_ret_ty.clone()],
                env,
                diags,
                level,
                "sort-by",
            );
        } else {
            let _ = infer_expr(&args[1], env, diags, level);
        }
    }
    Type::Vec(Box::new(elem_ty))
}

fn infer_distinct_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("distinct expects 1 argument".to_string()));
        return Type::Any;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "distinct");
                    return Type::Vec(inner);
                }
                if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "distinct");
                    return Type::Str;
                }
            }
            diags.push(error_diag(format!(
                "distinct expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_dedupe_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("dedupe expects 1 argument".to_string()));
        return Type::Any;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "dedupe");
                    return Type::Vec(inner);
                }
                if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "dedupe");
                    return Type::Str;
                }
            }
            diags.push(error_diag(format!(
                "dedupe expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_group_by_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Ok(Some(binding)) = extract_binding_sugar(args, "group-by", diags) {
        let coll_ty = infer_expr(binding.coll, env, diags, level);
        let Some(elem_ty) = expect_vec_like_elem(coll_ty, level, diags, "group-by") else {
            return Type::Map(
                Box::new(Type::Any),
                Box::new(Type::Vec(Box::new(Type::Any))),
            );
        };
        let mut fn_env = env.child();
        fn_env.set(&binding.name, elem_ty.clone());
        let ret_ty = infer_expr(binding.body, &mut fn_env, diags, level);
        let key_ty = if let Some(stripped) = strip_nil(&ret_ty) {
            report_optional_usage(&ret_ty, level, diags, "group-by");
            stripped
        } else {
            ret_ty.clone()
        };
        return Type::Map(Box::new(key_ty), Box::new(Type::Vec(Box::new(elem_ty))));
    }
    if args.len() != 2 {
        diags.push(error_diag("group-by expects 2 arguments".to_string()));
        return Type::Map(
            Box::new(Type::Any),
            Box::new(Type::Vec(Box::new(Type::Any))),
        );
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty, ret_ty) = match extract_unary_callable_types(
        &args[0],
        fn_ty,
        env,
        diags,
        "group-by expects a function",
    ) {
        Some(types) => types,
        None => {
            return Type::Map(
                Box::new(Type::Any),
                Box::new(Type::Vec(Box::new(Type::Any))),
            )
        }
    };
    let coll_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match expect_vec_like_elem(coll_ty, level, diags, "group-by") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&elem_ty, &param_ty, env) {
        if is_optional_assignable(&elem_ty, &param_ty, env) {
            report_optional_usage(&elem_ty, level, diags, "group-by");
        } else {
            diags.push(error_diag(format!(
                "group-by expects {}, got {}",
                param_ty, elem_ty
            )));
        }
    }
    let key_ty = if let Some(stripped) = strip_nil(&ret_ty) {
        report_optional_usage(&ret_ty, level, diags, "group-by");
        stripped
    } else {
        ret_ty.clone()
    };
    Type::Map(Box::new(key_ty), Box::new(Type::Vec(Box::new(elem_ty))))
}

fn infer_zip_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("zip expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Vec(Box::new(Type::Any))));
    }
    let left_ty = infer_expr(&args[0], env, diags, level);
    let right_ty = infer_expr(&args[1], env, diags, level);
    let left_elem = match left_ty {
        Type::Vec(inner) => *inner,
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "zip");
                    *inner
                } else if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "zip");
                    Type::Str
                } else {
                    diags.push(error_diag(format!(
                        "zip expects vector or string, got {}",
                        other
                    )));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!(
                    "zip expects vector or string, got {}",
                    other
                )));
                Type::Any
            }
        }
    };
    let right_elem = match right_ty {
        Type::Vec(inner) => *inner,
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "zip");
                    *inner
                } else if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "zip");
                    Type::Str
                } else {
                    diags.push(error_diag(format!(
                        "zip expects vector or string, got {}",
                        other
                    )));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!(
                    "zip expects vector or string, got {}",
                    other
                )));
                Type::Any
            }
        }
    };
    let pair_elem = merge_types(left_elem, right_elem);
    Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))))
}

fn infer_zip_with_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 3 {
        diags.push(error_diag("zip-with expects 3 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag(
            "zip-with expects a function of arity 2".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    };
    let Some(param_ty) = function_param_types_for_arity(&params, rest.as_deref(), 2) else {
        diags.push(error_diag(
            "zip-with expects a function of arity 2".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    };
    let left_ty = infer_expr(&args[1], env, diags, level);
    let right_ty = infer_expr(&args[2], env, diags, level);
    let left_elem = match expect_vec_like_elem(left_ty, level, diags, "zip-with") {
        Some(elem) => elem,
        None => Type::Any,
    };
    let right_elem = match expect_vec_like_elem(right_ty, level, diags, "zip-with") {
        Some(elem) => elem,
        None => Type::Any,
    };
    if !is_assignable_with_env(&left_elem, &param_ty[0], env) {
        if is_optional_assignable(&left_elem, &param_ty[0], env) {
            report_optional_usage(&left_elem, level, diags, "zip-with");
        } else {
            diags.push(error_diag(format!(
                "zip-with expects {}, got {}",
                param_ty[0], left_elem
            )));
        }
    }
    if !is_assignable_with_env(&right_elem, &param_ty[1], env) {
        if is_optional_assignable(&right_elem, &param_ty[1], env) {
            report_optional_usage(&right_elem, level, diags, "zip-with");
        } else {
            diags.push(error_diag(format!(
                "zip-with expects {}, got {}",
                param_ty[1], right_elem
            )));
        }
    }
    Type::Vec(ret)
}

fn infer_zipmap_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("zipmap expects 2 arguments".to_string()));
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    let keys_ty = infer_expr(&args[0], env, diags, level);
    let vals_ty = infer_expr(&args[1], env, diags, level);
    let key_elem = match keys_ty {
        Type::Vec(inner) => *inner,
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "zipmap");
                    *inner
                } else if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "zipmap");
                    Type::Str
                } else {
                    diags.push(error_diag(format!(
                        "zipmap expects vector or string, got {}",
                        other
                    )));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!(
                    "zipmap expects vector or string, got {}",
                    other
                )));
                Type::Any
            }
        }
    };
    let val_elem = match vals_ty {
        Type::Vec(inner) => *inner,
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "zipmap");
                    *inner
                } else if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "zipmap");
                    Type::Str
                } else {
                    diags.push(error_diag(format!(
                        "zipmap expects vector or string, got {}",
                        other
                    )));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!(
                    "zipmap expects vector or string, got {}",
                    other
                )));
                Type::Any
            }
        }
    };
    let key_union = Type::union(vec![Type::Keyword, Type::Str, Type::Symbol]);
    let key_ty = if is_assignable_with_env(&key_elem, &key_union, env) {
        key_elem
    } else if let Some(stripped) = strip_nil(&key_elem) {
        if is_assignable_with_env(&stripped, &key_union, env) {
            report_optional_usage(&key_elem, level, diags, "zipmap");
            stripped
        } else {
            diags.push(error_diag(format!(
                "zipmap expects key-like elements, got {}",
                key_elem
            )));
            Type::Any
        }
    } else {
        diags.push(error_diag(format!(
            "zipmap expects key-like elements, got {}",
            key_elem
        )));
        Type::Any
    };
    Type::Map(Box::new(key_ty), Box::new(val_elem))
}

fn infer_interpose_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("interpose expects 2 arguments".to_string()));
        return Type::Any;
    }
    let sep_ty = infer_expr(&args[0], env, diags, level);
    let coll_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => {
            let out_ty = merge_types(*inner, sep_ty);
            Type::Vec(Box::new(out_ty))
        }
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "interpose");
                    let out_ty = merge_types(*inner, sep_ty);
                    return Type::Vec(Box::new(out_ty));
                }
                if let Type::Str = stripped {
                    report_optional_usage(&other, level, diags, "interpose");
                    return Type::Str;
                }
            }
            diags.push(error_diag(format!(
                "interpose expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_interleave_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("interleave expects 2 arguments".to_string()));
        return Type::Any;
    }
    let left_ty = infer_expr(&args[0], env, diags, level);
    let right_ty = infer_expr(&args[1], env, diags, level);
    match (left_ty, right_ty) {
        (Type::Vec(left), Type::Vec(right)) => {
            let elem = merge_types(*left, *right);
            Type::Vec(Box::new(elem))
        }
        (Type::Str, Type::Str) => Type::Str,
        (left, right) => {
            diags.push(error_diag(format!(
                "interleave expects both vector or both string, got {} and {}",
                left, right
            )));
            Type::Any
        }
    }
}

fn infer_flatten_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    fn flatten_elem_type(ty: Type) -> Type {
        match ty {
            Type::Vec(inner) => flatten_elem_type(*inner),
            Type::Tuple(items) => {
                let items = items.into_iter().map(flatten_elem_type).collect();
                Type::union(items)
            }
            Type::Str => Type::Str,
            other => other,
        }
    }
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag("flatten expects 1 or 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    if args.len() == 2 {
        let depth_ty = infer_expr(&args[1], env, diags, level);
        if !is_assignable_with_env(&depth_ty, &Type::Int, env) {
            if is_optional_assignable(&depth_ty, &Type::Int, env) {
                report_optional_usage(&depth_ty, level, diags, "flatten");
            } else {
                diags.push(error_diag(format!(
                    "flatten expects Int depth, got {}",
                    depth_ty
                )));
            }
        }
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(Box::new(flatten_elem_type(*inner))),
        Type::Tuple(items) => Type::Vec(Box::new(flatten_elem_type(Type::Tuple(items)))),
        Type::Str => Type::Vec(Box::new(Type::Str)),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Tuple(_) | Type::Str) {
                    report_optional_usage(&other, level, diags, "flatten");
                    return match stripped {
                        Type::Vec(inner) => Type::Vec(Box::new(flatten_elem_type(*inner))),
                        Type::Tuple(items) => {
                            Type::Vec(Box::new(flatten_elem_type(Type::Tuple(items))))
                        }
                        Type::Str => Type::Vec(Box::new(Type::Str)),
                        _ => Type::Vec(Box::new(Type::Any)),
                    };
                }
            }
            diags.push(error_diag(format!("flatten expects vector, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_print_call(
    _name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    for arg in args {
        let _ = infer_expr(arg, env, diags, level);
    }
    Type::Nil
}

fn infer_pr_str_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    for arg in args {
        let _ = infer_expr(arg, env, diags, level);
    }
    Type::Str
}

fn infer_str_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    for arg in args {
        let _ = infer_expr(arg, env, diags, level);
    }
    Type::Str
}

fn infer_not_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("not expects 1 argument".to_string()));
    }
    let _arg_ty = infer_expr(&args[0], env, diags, level);
    Type::Bool
}

fn infer_compare_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let returns_int = name == "compare";
    if args.len() < 2 {
        diags.push(error_diag(format!("{} expects at least 2 arguments", name)));
        return if returns_int { Type::Int } else { Type::Bool };
    }
    let numeric_only = matches!(name, "<" | ">" | "<=" | ">=");
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if numeric_only && (contains_any(&arg_ty) || contains_dyn(&arg_ty)) {
            report_unresolved(&arg_ty, level, diags, name.to_string());
            continue;
        }
        if numeric_only && !is_numeric_type(&arg_ty) {
            if let Some(stripped) = strip_nil(&arg_ty) {
                if is_numeric_type(&stripped) {
                    report_optional_usage(&arg_ty, level, diags, name);
                    continue;
                }
            }
            diags.push(error_diag(format!(
                "{} expects numeric arguments, got {}",
                name, arg_ty
            )));
        }
    }
    if returns_int {
        Type::Int
    } else {
        Type::Bool
    }
}

fn infer_count_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("count expects 1 argument".to_string()));
        return Type::Int;
    }
    let ty = infer_expr(&args[0], env, diags, level);
    match ty {
        Type::Vec(_)
        | Type::Map(_, _)
        | Type::Shape(_)
        | Type::Object(_)
        | Type::Named(_)
        | Type::Str => Type::Int,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(
                    stripped,
                    Type::Vec(_)
                        | Type::Map(_, _)
                        | Type::Shape(_)
                        | Type::Object(_)
                        | Type::Named(_)
                        | Type::Str
                ) {
                    report_optional_usage(&other, level, diags, "count");
                    return Type::Int;
                }
            }
            diags.push(error_diag(format!(
                "count expects collection, got {}",
                other
            )));
            Type::Int
        }
    }
}

fn infer_empty_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("empty? expects 1 argument".to_string()));
        return Type::Bool;
    }
    let ty = infer_expr(&args[0], env, diags, level);
    match ty {
        Type::Vec(_)
        | Type::Map(_, _)
        | Type::Shape(_)
        | Type::Object(_)
        | Type::Named(_)
        | Type::Str => Type::Bool,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(
                    stripped,
                    Type::Vec(_)
                        | Type::Map(_, _)
                        | Type::Shape(_)
                        | Type::Object(_)
                        | Type::Named(_)
                        | Type::Str
                ) {
                    report_optional_usage(&other, level, diags, "empty?");
                    return Type::Bool;
                }
            }
            diags.push(error_diag(format!(
                "empty? expects collection, got {}",
                other
            )));
            Type::Bool
        }
    }
}

fn infer_not_empty_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("not-empty expects 1 argument".to_string()));
        return Type::Any;
    }
    if is_empty_collection_literal(&args[0]) {
        return Type::Nil;
    }
    let ty = infer_expr(&args[0], env, diags, level);
    let (inner_ty, optional) = if let Some(stripped) = strip_nil(&ty) {
        (stripped, true)
    } else {
        (ty.clone(), false)
    };
    match inner_ty {
        Type::Vec(_)
        | Type::Map(_, _)
        | Type::Shape(_)
        | Type::Object(_)
        | Type::Named(_)
        | Type::Str => {
            if optional {
                ty
            } else {
                Type::union(vec![inner_ty, Type::Nil])
            }
        }
        other => {
            diags.push(error_diag(format!(
                "not-empty expects collection, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_parity_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
        return Type::Bool;
    }
    let ty = infer_expr(&args[0], env, diags, level);
    if contains_any(&ty) || contains_dyn(&ty) {
        report_unresolved(&ty, level, diags, name.to_string());
        return Type::Bool;
    }
    match ty {
        Type::Int => Type::Bool,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Int) {
                    report_optional_usage(&other, level, diags, name);
                    return Type::Bool;
                }
            }
            diags.push(error_diag(format!("{} expects Int, got {}", name, other)));
            Type::Bool
        }
    }
}

fn infer_nil_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("nil? expects 1 argument".to_string()));
        return Type::Bool;
    }
    let _ = infer_expr(&args[0], env, diags, level);
    Type::Bool
}

fn infer_predicate_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
        return Type::Bool;
    }
    let _ = infer_expr(&args[0], env, diags, level);
    Type::Bool
}

fn infer_nth_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 || args.len() > 3 {
        diags.push(error_diag("nth expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let vec_ty = infer_expr(&args[0], env, diags, level);
    let idx_ty = infer_expr(&args[1], env, diags, level);
    let idx_vec_elem = match idx_ty.clone() {
        Type::Vec(inner) => Some(*inner),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "nth index");
                    Some(*inner)
                } else {
                    None
                }
            } else {
                None
            }
        }
    };
    let is_multi_index = idx_vec_elem.is_some();
    if !is_multi_index && !is_assignable_with_env(&idx_ty, &Type::Int, env) {
        if is_optional_assignable(&idx_ty, &Type::Int, env) {
            report_optional_usage(&idx_ty, level, diags, "nth index");
        } else {
            diags.push(error_diag(format!("nth expects Int index, got {}", idx_ty)));
        }
    }
    if let Some(elem_ty) = idx_vec_elem {
        if !is_assignable_with_env(&elem_ty, &Type::Int, env) {
            if is_optional_assignable(&elem_ty, &Type::Int, env) {
                report_optional_usage(&elem_ty, level, diags, "nth index");
            } else {
                diags.push(error_diag(format!(
                    "nth expects Int index, got {}",
                    elem_ty
                )));
            }
        }
    }
    let tuple_elem = |items: &[Type], idx_expr: &AstExpr, diags: &mut Vec<Diagnostic>| -> Type {
        match idx_expr {
            AstExpr::Literal(crate::ast::Literal::Int(value)) => {
                if *value < 0 {
                    diags.push(error_diag("nth index out of range".to_string()));
                    return Type::Any;
                }
                let index = *value as usize;
                if let Some(item) = items.get(index) {
                    item.clone()
                } else {
                    diags.push(error_diag("nth index out of range".to_string()));
                    Type::Any
                }
            }
            _ => {
                if items.is_empty() {
                    Type::Any
                } else {
                    Type::union(items.to_vec())
                }
            }
        }
    };
    let mut elem_ty = match vec_ty {
        Type::Vec(inner) => *inner,
        Type::Tuple(items) => tuple_elem(&items, &args[1], diags),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "nth");
                        *inner
                    }
                    Type::Tuple(items) => {
                        report_optional_usage(&other, level, diags, "nth");
                        tuple_elem(&items, &args[1], diags)
                    }
                    _ => {
                        diags.push(error_diag(format!(
                            "nth expects vector or tuple, got {}",
                            other
                        )));
                        Type::Any
                    }
                }
            } else {
                diags.push(error_diag(format!(
                    "nth expects vector or tuple, got {}",
                    other
                )));
                Type::Any
            }
        }
    };
    if args.len() == 3 {
        let default_ty = infer_expr(&args[2], env, diags, level);
        elem_ty = merge_types(elem_ty, default_ty);
    }
    if is_multi_index {
        Type::Vec(Box::new(elem_ty))
    } else {
        elem_ty
    }
}

fn infer_seq_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("seq expects 1 argument".to_string()));
        return Type::union(vec![Type::Vec(Box::new(Type::Any)), Type::Nil]);
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    let out_ty = match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Vec(Box::new(Type::Str)),
        Type::Map(key, val) => {
            let entry_elem = merge_types(*key, *val);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let entry_elem = merge_types(Type::Keyword, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let entry_elem = merge_types(Type::Str, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name).cloned() {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                let entry_elem = merge_types(Type::Str, value_union);
                Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                Type::Vec(Box::new(Type::Any))
            }
        }
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(
                    stripped,
                    Type::Vec(_)
                        | Type::Str
                        | Type::Map(_, _)
                        | Type::Shape(_)
                        | Type::Object(_)
                        | Type::Named(_)
                ) {
                    report_optional_usage(&other, level, diags, "seq");
                    return infer_seq_call_value(stripped, env, diags);
                }
            }
            diags.push(error_diag(format!("seq expects collection, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    };
    Type::union(vec![out_ty, Type::Nil])
}

fn infer_seq_call_value(ty: Type, env: &mut TypeEnv, diags: &mut Vec<Diagnostic>) -> Type {
    match ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Vec(Box::new(Type::Str)),
        Type::Map(key, val) => {
            let entry_elem = merge_types(*key, *val);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let entry_elem = merge_types(Type::Keyword, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let entry_elem = merge_types(Type::Str, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name).cloned() {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                let entry_elem = merge_types(Type::Str, value_union);
                Type::Vec(Box::new(Type::Vec(Box::new(entry_elem))))
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                Type::Vec(Box::new(Type::Any))
            }
        }
        _ => Type::Vec(Box::new(Type::Any)),
    }
}

fn infer_reduce_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("reduce expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_ty_a, param_ty_b, ret_ty) = match fn_ty {
        Type::Function { params, rest, ret } => {
            let ret_ty = *ret;
            match (params.len(), rest) {
                (2, None) => (params[0].clone(), params[1].clone(), ret_ty),
                (1, Some(rest)) => {
                    let rest_elem = match *rest {
                        Type::Vec(inner) => *inner,
                        other => other,
                    };
                    (params[0].clone(), rest_elem, ret_ty)
                }
                (2, Some(rest)) => {
                    let _rest_elem = match *rest {
                        Type::Vec(inner) => *inner,
                        other => other,
                    };
                    (params[0].clone(), params[1].clone(), ret_ty)
                }
                _ => {
                    diags.push(error_diag("reduce expects a function".to_string()));
                    return Type::Any;
                }
            }
        }
        _ => {
            diags.push(error_diag("reduce expects a function".to_string()));
            return Type::Any;
        }
    };
    let mut ret_ty = ret_ty;
    let (init_ty, coll_ty) = if args.len() == 3 {
        (
            Some(infer_expr(&args[1], env, diags, level)),
            infer_expr(&args[2], env, diags, level),
        )
    } else {
        (None, infer_expr(&args[1], env, diags, level))
    };
    let elem_ty = match coll_ty {
        Type::Vec(inner) => *inner,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "reduce");
                    *inner
                } else {
                    diags.push(error_diag(format!("reduce expects vector, got {}", other)));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!("reduce expects vector, got {}", other)));
                Type::Any
            }
        }
    };
    if let Some(init_ty) = &init_ty {
        if !is_assignable_with_env(init_ty, &param_ty_a, env) {
            if is_optional_assignable(init_ty, &param_ty_a, env) {
                report_optional_usage(init_ty, level, diags, "reduce");
            } else {
                diags.push(error_diag(format!(
                    "reduce expects {}, got {}",
                    param_ty_a, init_ty
                )));
            }
        }
    } else if !is_assignable_with_env(&elem_ty, &param_ty_a, env) {
        if is_optional_assignable(&elem_ty, &param_ty_a, env) {
            report_optional_usage(&elem_ty, level, diags, "reduce");
        } else {
            diags.push(error_diag(format!(
                "reduce expects {}, got {}",
                param_ty_a, elem_ty
            )));
        }
    }
    if !is_assignable_with_env(&elem_ty, &param_ty_b, env) {
        if is_optional_assignable(&elem_ty, &param_ty_b, env) {
            report_optional_usage(&elem_ty, level, diags, "reduce");
        } else {
            diags.push(error_diag(format!(
                "reduce expects {}, got {}",
                param_ty_b, elem_ty
            )));
        }
    }
    if let AstExpr::Symbol(sym) = &args[0] {
        match sym.as_str() {
            "conj" => {
                let mut elem_opt = None;
                if let Some(init_ty) = &init_ty {
                    if let Type::Vec(inner) = init_ty {
                        let init_is_empty = args.get(1).map_or(false, is_empty_collection_literal);
                        if **inner != Type::Any || !init_is_empty {
                            elem_opt = Some((**inner).clone());
                        }
                    }
                }
                elem_opt = Some(match elem_opt.take() {
                    Some(prev) => merge_types(prev, elem_ty.clone()),
                    None => elem_ty.clone(),
                });
                if let Some(elem) = elem_opt {
                    ret_ty = Type::Vec(Box::new(elem));
                }
            }
            "/" => ret_ty = Type::Float,
            "+" | "-" | "*" => {
                let mut has_float = false;
                let mut has_number = false;
                let mut push_ty = |ty: &Type| {
                    let stripped = strip_nil(ty).unwrap_or_else(|| ty.clone());
                    if matches!(stripped, Type::Float) {
                        has_float = true;
                    } else if matches!(stripped, Type::Number) {
                        has_number = true;
                    }
                };
                if let Some(init_ty) = &init_ty {
                    push_ty(init_ty);
                }
                push_ty(&elem_ty);
                ret_ty = if has_float {
                    Type::Float
                } else if has_number {
                    Type::Number
                } else {
                    Type::Int
                };
            }
            _ => {}
        }
    }
    ret_ty
}

fn infer_reduce_kv_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 3 {
        diags.push(error_diag("reduce-kv expects 3 arguments".to_string()));
        return Type::Any;
    }
    let init_ty = infer_expr(&args[1], env, diags, level);
    let raw_coll_ty = infer_expr(&args[2], env, diags, level);
    let coll_ty = if let Some(stripped) = strip_nil(&raw_coll_ty) {
        if matches!(
            stripped,
            Type::Map(_, _) | Type::Shape(_) | Type::Object(_) | Type::Named(_)
        ) {
            report_optional_usage(&raw_coll_ty, level, diags, "reduce-kv");
            stripped
        } else {
            raw_coll_ty.clone()
        }
    } else {
        raw_coll_ty.clone()
    };
    let (key_ty, val_ty) = match coll_ty {
        Type::Map(key, val) => (*key, *val),
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            (Type::Keyword, value_union)
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            (Type::Str, value_union)
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name).cloned() {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                (Type::Str, value_union)
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                (Type::Any, Type::Any)
            }
        }
        Type::Any | Type::Dyn | Type::DynOf(_) => (Type::Any, Type::Any),
        other => {
            diags.push(error_diag(format!("reduce-kv expects map, got {}", other)));
            (Type::Any, Type::Any)
        }
    };
    if let AstExpr::Fn { .. } = &args[0] {
        if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
            &args[0],
            &[init_ty.clone(), key_ty.clone(), val_ty.clone()],
            env,
            diags,
            level,
            "reduce-kv",
        ) {
            return (*ret).clone();
        }
        return Type::Any;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (params, ret_ty) =
        match extract_fixed_fn_types(fn_ty, 3, "reduce-kv expects a function of arity 3", diags) {
            Some(types) => types,
            None => return Type::Any,
        };
    let param_ty_a = params[0].clone();
    let param_ty_b = params[1].clone();
    let param_ty_c = params[2].clone();
    if !is_assignable_with_env(&init_ty, &param_ty_a, env) {
        if is_optional_assignable(&init_ty, &param_ty_a, env) {
            report_optional_usage(&init_ty, level, diags, "reduce-kv");
        } else {
            diags.push(error_diag(format!(
                "reduce-kv expects {}, got {}",
                param_ty_a, init_ty
            )));
        }
    }
    if !is_assignable_with_env(&key_ty, &param_ty_b, env) {
        if is_optional_assignable(&key_ty, &param_ty_b, env) {
            report_optional_usage(&key_ty, level, diags, "reduce-kv");
        } else {
            diags.push(error_diag(format!(
                "reduce-kv expects {}, got {}",
                param_ty_b, key_ty
            )));
        }
    }
    if !is_assignable_with_env(&val_ty, &param_ty_c, env) {
        if is_optional_assignable(&val_ty, &param_ty_c, env) {
            report_optional_usage(&val_ty, level, diags, "reduce-kv");
        } else {
            diags.push(error_diag(format!(
                "reduce-kv expects {}, got {}",
                param_ty_c, val_ty
            )));
        }
    }
    ret_ty
}

fn infer_apply_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 {
        diags.push(error_diag("apply expects at least 2 arguments".to_string()));
        return Type::Any;
    }
    let fn_sym = match &args[0] {
        AstExpr::Symbol(sym) => Some(sym.as_str()),
        _ => None,
    };
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag("apply expects a function".to_string()));
        return Type::Any;
    };
    let ret = (*ret).clone();
    let fixed_count = args.len().saturating_sub(2);
    if fixed_count > params.len() && rest.is_none() {
        diags.push(error_diag(format!(
            "apply expects at most {} fixed arguments",
            params.len()
        )));
        return ret;
    }
    let mut fixed_arg_tys = Vec::with_capacity(fixed_count);
    for (idx, arg) in args.iter().skip(1).take(fixed_count).enumerate() {
        let param_ty = match function_param_type_at(&params, rest.as_deref(), idx) {
            Some(param) => param,
            None => Type::Any,
        };
        let arg_ty = infer_expr(arg, env, diags, level);
        fixed_arg_tys.push(arg_ty.clone());
        if !is_assignable_with_env(&arg_ty, &param_ty, env) {
            if is_optional_assignable(&arg_ty, &param_ty, env) {
                report_optional_usage(&arg_ty, level, diags, "apply");
            } else {
                diags.push(error_diag(format!(
                    "apply expects {}, got {}",
                    param_ty, arg_ty
                )));
            }
        }
    }

    let tail_ty = infer_expr(&args[args.len() - 1], env, diags, level);
    let tail_elem_ty = match tail_ty {
        Type::Vec(inner) => *inner,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "apply");
                    *inner
                } else {
                    diags.push(error_diag(format!("apply expects vector, got {}", other)));
                    return ret.clone();
                }
            } else {
                diags.push(error_diag(format!("apply expects vector, got {}", other)));
                return ret.clone();
            }
        }
    };

    let Some(rest_ty) = rest.as_deref() else {
        match level {
            NativeLevel::Strict => diags.push(error_diag(
                "apply expects function with rest parameter".to_string(),
            )),
            NativeLevel::Warn => diags.push(warn_diag(
                "apply expects function with rest parameter".to_string(),
            )),
            NativeLevel::Allow => {}
        }
        return ret;
    };
    if fixed_count < params.len() {
        let missing = params.len().saturating_sub(fixed_count);
        let tail_len = literal_collection_len(&args[args.len() - 1]);
        let should_report = match tail_len {
            Some(len) => len < missing,
            None => false,
        };
        if should_report {
            match level {
                NativeLevel::Strict => diags.push(error_diag(
                    "apply may be missing required arguments".to_string(),
                )),
                NativeLevel::Warn => diags.push(warn_diag(
                    "apply may be missing required arguments".to_string(),
                )),
                NativeLevel::Allow => {}
            }
        }
    }
    let rest_elem_ty = match rest_ty {
        Type::Vec(inner) => inner.as_ref(),
        other => {
            diags.push(error_diag(format!(
                "apply rest expects Vec<...>, got {}",
                other
            )));
            return ret;
        }
    };
    if !is_assignable_with_env(&tail_elem_ty, rest_elem_ty, env) {
        if is_optional_assignable(&tail_elem_ty, rest_elem_ty, env) {
            report_optional_usage(&tail_elem_ty, level, diags, "apply");
        } else {
            diags.push(error_diag(format!(
                "apply expects {}, got {}",
                rest_elem_ty, tail_elem_ty
            )));
        }
    }
    if matches!(fn_sym, Some("vector") | Some("list")) {
        let mut elem_ty = None;
        for fixed in fixed_arg_tys {
            elem_ty = Some(match elem_ty.take() {
                Some(prev) => merge_types(prev, fixed),
                None => fixed,
            });
        }
        elem_ty = Some(match elem_ty.take() {
            Some(prev) => merge_types(prev, tail_elem_ty),
            None => tail_elem_ty,
        });
        return Type::Vec(Box::new(elem_ty.unwrap_or(Type::Any)));
    }
    ret
}

fn infer_identity_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("identity expects 1 argument".to_string()));
        return Type::Any;
    }
    infer_expr(&args[0], env, diags, level)
}

fn infer_constantly_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("constantly expects 1 argument".to_string()));
        return Type::Any;
    }
    let ret_ty = infer_expr(&args[0], env, diags, level);
    Type::Function {
        params: Vec::new(),
        rest: Some(Box::new(Type::Vec(Box::new(Type::Any)))),
        ret: Box::new(ret_ty),
    }
}

fn infer_partial_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        diags.push(error_diag(
            "partial expects at least 1 argument".to_string(),
        ));
        return Type::Any;
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let Type::Function { params, rest, ret } = fn_ty else {
        diags.push(error_diag("partial expects a function".to_string()));
        return Type::Any;
    };
    let pre_args = &args[1..];
    let mut remaining_params = params.clone();
    let rest_elem = rest.as_deref().and_then(|ty| match ty {
        Type::Vec(inner) => Some(inner.as_ref()),
        _ => None,
    });
    for (idx, arg) in pre_args.iter().enumerate() {
        let arg_ty = infer_expr(arg, env, diags, level);
        if idx < params.len() {
            let expected = &params[idx];
            if !is_assignable_with_env(&arg_ty, expected, env) {
                if is_optional_assignable(&arg_ty, expected, env) {
                    report_optional_usage(&arg_ty, level, diags, "partial");
                } else {
                    diags.push(error_diag(format!(
                        "partial expects {}, got {}",
                        expected, arg_ty
                    )));
                }
            }
        } else if let Some(elem_ty) = rest_elem {
            if !is_assignable_with_env(&arg_ty, elem_ty, env) {
                if is_optional_assignable(&arg_ty, elem_ty, env) {
                    report_optional_usage(&arg_ty, level, diags, "partial");
                } else {
                    diags.push(error_diag(format!(
                        "partial expects {}, got {}",
                        elem_ty, arg_ty
                    )));
                }
            }
        } else {
            diags.push(error_diag(
                "partial expects fewer arguments for function".to_string(),
            ));
        }
    }
    if pre_args.len() <= params.len() {
        remaining_params = params[pre_args.len()..].to_vec();
    } else {
        remaining_params.clear();
    }
    Type::Function {
        params: remaining_params,
        rest,
        ret,
    }
}

fn infer_complement_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("complement expects 1 argument".to_string()));
        return Type::Function {
            params: vec![Type::Any],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Any)))),
            ret: Box::new(Type::Bool),
        };
    }
    let fn_ty = infer_expr(&args[0], env, diags, level);
    match fn_ty {
        Type::Function { params, rest, .. } => Type::Function {
            params,
            rest,
            ret: Box::new(Type::Bool),
        },
        _ => {
            diags.push(error_diag("complement expects a function".to_string()));
            Type::Function {
                params: vec![Type::Any],
                rest: Some(Box::new(Type::Vec(Box::new(Type::Any)))),
                ret: Box::new(Type::Bool),
            }
        }
    }
}

fn infer_comp_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return Type::Function {
            params: vec![Type::Any],
            rest: None,
            ret: Box::new(Type::Any),
        };
    }
    let mut types = Vec::with_capacity(args.len());
    for arg in args {
        types.push(infer_expr(arg, env, diags, level));
    }
    if types.len() == 1 {
        return types[0].clone();
    }
    let last_ty = types.last().cloned().unwrap_or(Type::Any);
    let Type::Function {
        params: last_params,
        rest: last_rest,
        ret: last_ret,
    } = last_ty
    else {
        diags.push(error_diag("comp expects functions".to_string()));
        return Type::Any;
    };
    let mut next_ret = *last_ret;
    for ty in types.iter().rev().skip(1) {
        let Type::Function { params, rest, ret } = ty else {
            diags.push(error_diag(
                "comp expects unary functions except the last".to_string(),
            ));
            return Type::Any;
        };
        let Some(param_ty) = unary_param_type(params, rest.as_deref()) else {
            diags.push(error_diag(
                "comp expects unary functions except the last".to_string(),
            ));
            return Type::Any;
        };
        if !is_assignable_with_env(&next_ret, &param_ty, env) {
            if is_optional_assignable(&next_ret, &param_ty, env) {
                report_optional_usage(&next_ret, level, diags, "comp");
            } else if is_numeric_compatible(&next_ret, &param_ty) {
                // Allow numeric type merging (precision for Int/Float/Number is handled later)
            } else {
                diags.push(error_diag(format!(
                    "comp expects {}, got {}",
                    param_ty, next_ret
                )));
            }
        }
        next_ret = (**ret).clone();
    }
    Type::Function {
        params: last_params,
        rest: last_rest,
        ret: Box::new(next_ret),
    }
}

fn infer_pipe_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return Type::Function {
            params: vec![Type::Any],
            rest: None,
            ret: Box::new(Type::Any),
        };
    }
    let first_ty = infer_expr(&args[0], env, diags, level);
    if args.len() == 1 {
        return first_ty.clone();
    }
    let Type::Function {
        params: first_params,
        rest: first_rest,
        ret: first_ret,
    } = first_ty
    else {
        diags.push(error_diag("pipe expects functions".to_string()));
        return Type::Any;
    };
    let mut next_ret = *first_ret;
    for arg in args.iter().skip(1) {
        let ty = if let AstExpr::Fn { .. } = arg {
            infer_inline_fn_with_expected_params(
                arg,
                &[next_ret.clone()],
                env,
                diags,
                level,
                "pipe",
            )
            .unwrap_or(Type::Any)
        } else {
            infer_expr(arg, env, diags, level)
        };
        let Type::Function { params, rest, ret } = ty else {
            diags.push(error_diag(
                "pipe expects unary functions except the first".to_string(),
            ));
            return Type::Any;
        };
        let Some(param_ty) = unary_param_type(&params, rest.as_deref()) else {
            diags.push(error_diag(
                "pipe expects unary functions except the first".to_string(),
            ));
            return Type::Any;
        };
        if !is_assignable_with_env(&next_ret, &param_ty, env) {
            if is_optional_assignable(&next_ret, &param_ty, env) {
                report_optional_usage(&next_ret, level, diags, "pipe");
            } else if is_numeric_compatible(&next_ret, &param_ty) {
                // Allow numeric type flow (precision for Int/Float/Number is handled later)
            } else {
                diags.push(error_diag(format!(
                    "pipe expects {}, got {}",
                    param_ty, next_ret
                )));
            }
        }
        next_ret = (*ret).clone();
    }
    Type::Function {
        params: first_params,
        rest: first_rest,
        ret: Box::new(next_ret),
    }
}

fn unary_param_type(params: &[Type], rest: Option<&Type>) -> Option<Type> {
    match params.len() {
        0 => match rest {
            Some(Type::Vec(inner)) => Some((**inner).clone()),
            Some(other) => Some(other.clone()),
            None => None,
        },
        1 => Some(params[0].clone()),
        _ => None,
    }
}

fn rest_elem_type(rest: &Type) -> Option<Type> {
    match rest {
        Type::Vec(inner) => Some((**inner).clone()),
        other => Some(other.clone()),
    }
}

fn infer_juxt_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return Type::Function {
            params: vec![Type::Any],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Any)))),
            ret: Box::new(Type::Vec(Box::new(Type::Any))),
        };
    }
    let mut types = Vec::with_capacity(args.len());
    for arg in args {
        types.push(infer_expr(arg, env, diags, level));
    }
    let first_ty = types[0].clone();
    let Type::Function { params, rest, ret } = first_ty else {
        diags.push(error_diag("juxt expects functions".to_string()));
        return Type::Any;
    };
    let mut ret_union = (*ret).clone();
    for ty in types.iter().skip(1) {
        let Type::Function {
            params: other_params,
            rest: other_rest,
            ret: other_ret,
        } = ty
        else {
            diags.push(error_diag("juxt expects functions".to_string()));
            return Type::Any;
        };

        if rest.is_some() && other_rest.is_none() {
            diags.push(error_diag(
                "juxt expects functions with compatible arity".to_string(),
            ));
            return Type::Any;
        }
        if other_params.len() > params.len() && rest.is_none() {
            diags.push(error_diag(
                "juxt expects functions with compatible arity".to_string(),
            ));
            return Type::Any;
        }

        let other_rest_elem = other_rest.as_deref().and_then(|ty| rest_elem_type(ty));
        for (idx, base) in params.iter().enumerate() {
            let expected = if idx < other_params.len() {
                other_params[idx].clone()
            } else if let Some(rest_elem) = &other_rest_elem {
                rest_elem.clone()
            } else {
                diags.push(error_diag(
                    "juxt expects functions with compatible arity".to_string(),
                ));
                return Type::Any;
            };
            if !is_assignable_with_env(base, &expected, env) {
                if is_optional_assignable(base, &expected, env) {
                    report_optional_usage(base, level, diags, "juxt");
                } else {
                    diags.push(error_diag(format!(
                        "juxt expects {}, got {}",
                        expected, base
                    )));
                }
            }
        }
        if other_params.len() > params.len() {
            let rest_elem = match rest.as_deref().and_then(|ty| rest_elem_type(ty)) {
                Some(rest_elem) => rest_elem,
                None => {
                    diags.push(error_diag(
                        "juxt expects functions with compatible arity".to_string(),
                    ));
                    return Type::Any;
                }
            };
            for expected in other_params.iter().skip(params.len()) {
                if !is_assignable_with_env(&rest_elem, expected, env) {
                    if is_optional_assignable(&rest_elem, expected, env) {
                        report_optional_usage(&rest_elem, level, diags, "juxt");
                    } else {
                        diags.push(error_diag(format!(
                            "juxt expects {}, got {}",
                            expected, rest_elem
                        )));
                    }
                }
            }
        }
        ret_union = merge_types(ret_union, (**other_ret).clone());
    }
    Type::Function {
        params,
        rest,
        ret: Box::new(Type::Vec(Box::new(ret_union))),
    }
}

fn infer_juxt_apply(
    funcs: &[AstExpr],
    call_args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if funcs.is_empty() {
        return Type::Vec(Box::new(Type::Any));
    }
    let mut arg_tys = Vec::with_capacity(call_args.len());
    for arg in call_args {
        arg_tys.push(infer_expr(arg, env, diags, level));
    }
    let mut ret_union = Type::Any;
    let mut first_ret = true;
    for func in funcs {
        let fn_ty = if let AstExpr::Fn { .. } = func {
            infer_inline_fn_with_expected_params(func, &arg_tys, env, diags, level, "juxt")
                .unwrap_or(Type::Any)
        } else {
            infer_expr(func, env, diags, level)
        };
        let Type::Function { params, rest, ret } = fn_ty else {
            diags.push(error_diag("juxt expects functions".to_string()));
            continue;
        };
        if rest.is_some() {
            if call_args.len() < params.len() {
                diags.push(error_diag(format!(
                    "juxt expects at least {} arguments",
                    params.len()
                )));
            }
        } else if call_args.len() != params.len() {
            diags.push(error_diag(format!(
                "juxt expects {} arguments",
                params.len()
            )));
        }
        for (idx, param_ty) in params.iter().enumerate() {
            if let Some(arg_ty) = arg_tys.get(idx) {
                if !is_assignable_with_env(arg_ty, param_ty, env) {
                    if is_optional_assignable(arg_ty, param_ty, env) {
                        report_optional_usage(arg_ty, level, diags, "juxt");
                    } else {
                        diags.push(error_diag(format!(
                            "juxt expects {}, got {}",
                            param_ty, arg_ty
                        )));
                    }
                }
            }
        }
        let mut next_ret = (*ret).clone();
        if let AstExpr::Symbol(sym) = func {
            if let Some(arg_ty) = arg_tys.first() {
                if let Some(refined) = seq_head_type_for_elem(sym, arg_ty) {
                    next_ret = refined;
                }
            }
        }
        if first_ret {
            ret_union = next_ret;
            first_ret = false;
        } else {
            ret_union = merge_types(ret_union, next_ret);
        }
    }
    Type::Vec(Box::new(ret_union))
}

fn infer_merge_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        diags.push(error_diag("merge expects at least 1 argument".to_string()));
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    let mut object_fields: Option<BTreeMap<String, Type>> = Some(BTreeMap::new());
    let mut shape_only = true;
    let mut shape_open = false;
    let mut map_key: Option<Type> = None;
    let mut map_val: Option<Type> = None;

    for arg in args {
        let raw_ty = infer_expr(arg, env, diags, level);
        let Some(ty) = unwrap_optional_map_like(&raw_ty, level, diags, "merge") else {
            diags.push(error_diag(format!("merge expects map, got {}", raw_ty)));
            object_fields = None;
            map_key = Some(Type::Any);
            map_val = Some(Type::Any);
            continue;
        };
        match ty {
            Type::Map(key, val) => {
                object_fields = None;
                shape_only = false;
                map_key = Some(match map_key.take() {
                    Some(prev) => merge_types(prev, *key),
                    None => *key,
                });
                map_val = Some(match map_val.take() {
                    Some(prev) => merge_types(prev, *val),
                    None => *val,
                });
            }
            Type::Shape(shape) => {
                shape_open |= shape.open;
                if let Some(obj_fields) = object_fields.as_mut() {
                    for (name, ty) in shape.fields {
                        let entry = obj_fields.entry(name).or_insert(ty.clone());
                        if *entry != ty {
                            *entry = merge_types(entry.clone(), ty);
                        }
                    }
                } else {
                    let value_union = shape
                        .fields
                        .values()
                        .cloned()
                        .reduce(merge_types)
                        .unwrap_or(Type::Any);
                    map_key = Some(match map_key.take() {
                        Some(prev) => merge_types(prev, Type::Keyword),
                        None => Type::Keyword,
                    });
                    map_val = Some(match map_val.take() {
                        Some(prev) => merge_types(prev, value_union),
                        None => value_union,
                    });
                }
            }
            Type::Object(fields) => {
                shape_only = false;
                if let Some(obj_fields) = object_fields.as_mut() {
                    for (name, ty) in fields {
                        let entry = obj_fields.entry(name).or_insert(ty.clone());
                        if *entry != ty {
                            *entry = merge_types(entry.clone(), ty);
                        }
                    }
                } else {
                    let value_union = fields
                        .values()
                        .cloned()
                        .reduce(merge_types)
                        .unwrap_or(Type::Any);
                    map_key = Some(match map_key.take() {
                        Some(prev) => merge_types(prev, Type::Str),
                        None => Type::Str,
                    });
                    map_val = Some(match map_val.take() {
                        Some(prev) => merge_types(prev, value_union),
                        None => value_union,
                    });
                }
            }
            Type::Named(name) => {
                shape_only = false;
                if let Some(fields) = env.get_type(&name).cloned() {
                    if let Some(obj_fields) = object_fields.as_mut() {
                        for (field, ty) in fields {
                            let entry = obj_fields.entry(field).or_insert(ty.clone());
                            if *entry != ty {
                                *entry = merge_types(entry.clone(), ty);
                            }
                        }
                    } else {
                        let value_union = fields
                            .values()
                            .cloned()
                            .reduce(merge_types)
                            .unwrap_or(Type::Any);
                        map_key = Some(match map_key.take() {
                            Some(prev) => merge_types(prev, Type::Str),
                            None => Type::Str,
                        });
                        map_val = Some(match map_val.take() {
                            Some(prev) => merge_types(prev, value_union),
                            None => value_union,
                        });
                    }
                } else {
                    diags.push(error_diag(format!("unknown type: {}", name)));
                    object_fields = None;
                    map_key = Some(Type::Any);
                    map_val = Some(Type::Any);
                }
            }
            Type::Any | Type::Dyn | Type::DynOf(_) => {
                object_fields = None;
                shape_only = false;
                map_key = Some(match map_key.take() {
                    Some(prev) => merge_types(prev, Type::Any),
                    None => Type::Any,
                });
                map_val = Some(match map_val.take() {
                    Some(prev) => merge_types(prev, Type::Any),
                    None => Type::Any,
                });
            }
            other => {
                diags.push(error_diag(format!("merge expects map, got {}", other)));
                object_fields = None;
                shape_only = false;
                map_key = Some(Type::Any);
                map_val = Some(Type::Any);
            }
        }
    }

    if let Some(fields) = object_fields {
        if map_key.is_none() && map_val.is_none() {
            if shape_only {
                return if shape_open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                };
            }
            return Type::Object(fields);
        }
        let value_union = fields
            .values()
            .cloned()
            .reduce(merge_types)
            .unwrap_or(Type::Any);
        let default_key = if shape_only { Type::Keyword } else { Type::Str };
        map_key = Some(match map_key.take() {
            Some(prev) => merge_types(prev, default_key),
            None => default_key,
        });
        map_val = Some(match map_val.take() {
            Some(prev) => merge_types(prev, value_union),
            None => value_union,
        });
    }
    let key = map_key.unwrap_or(Type::Any);
    let val = map_val.unwrap_or(Type::Any);
    Type::Map(Box::new(key), Box::new(val))
}

fn infer_merge_with_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 {
        diags.push(error_diag(
            "merge-with expects at least 2 arguments".to_string(),
        ));
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    let fn_sym = match &args[0] {
        AstExpr::Symbol(sym) => Some(sym.as_str()),
        _ => None,
    };
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let (param_left, param_right, mut ret_ty) = match fn_ty {
        Type::Function { params, rest, ret } => {
            let Some(param_tys) = function_param_types_for_arity(&params, rest.as_deref(), 2)
            else {
                diags.push(error_diag(
                    "merge-with expects a function of arity 2".to_string(),
                ));
                return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
            };
            (param_tys[0].clone(), param_tys[1].clone(), *ret)
        }
        _ => {
            diags.push(error_diag(
                "merge-with expects a function of arity 2".to_string(),
            ));
            return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
        }
    };

    let mut object_fields: Option<BTreeMap<String, Type>> = Some(BTreeMap::new());
    let mut shape_only = true;
    let mut shape_open = false;
    let mut map_key: Option<Type> = None;
    let mut map_val: Option<Type> = None;
    let mut input_val: Option<Type> = None;
    let mut map_count = 0usize;

    let merge_opt = |slot: &mut Option<Type>, next: Type| {
        *slot = Some(match slot.take() {
            Some(prev) => merge_types(prev, next),
            None => next,
        });
    };

    for arg in &args[1..] {
        let raw_ty = infer_expr(arg, env, diags, level);
        let Some(ty) = unwrap_optional_map_like(&raw_ty, level, diags, "merge-with") else {
            diags.push(error_diag(format!(
                "merge-with expects map, got {}",
                raw_ty
            )));
            object_fields = None;
            map_count += 1;
            merge_opt(&mut map_key, Type::Any);
            merge_opt(&mut map_val, Type::Any);
            merge_opt(&mut input_val, Type::Any);
            continue;
        };
        match ty {
            Type::Map(key, val) => {
                object_fields = None;
                shape_only = false;
                map_count += 1;
                let val_ty = *val;
                merge_opt(&mut map_key, *key);
                merge_opt(&mut map_val, val_ty.clone());
                merge_opt(&mut input_val, val_ty);
            }
            Type::Shape(shape) => {
                shape_open |= shape.open;
                map_count += 1;
                let value_union = shape
                    .fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                merge_opt(&mut input_val, value_union.clone());
                if let Some(obj_fields) = object_fields.as_mut() {
                    for (name, ty) in shape.fields {
                        if let Some(entry) = obj_fields.get_mut(&name) {
                            let merged = merge_types(entry.clone(), ty.clone());
                            *entry = if matches!(fn_sym, Some("into")) {
                                merged
                            } else {
                                merge_types(merged, ret_ty.clone())
                            };
                        } else {
                            obj_fields.insert(name, ty);
                        }
                    }
                } else {
                    merge_opt(&mut map_key, Type::Keyword);
                    merge_opt(&mut map_val, value_union);
                }
            }
            Type::Object(fields) => {
                shape_only = false;
                map_count += 1;
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                merge_opt(&mut input_val, value_union.clone());
                if let Some(obj_fields) = object_fields.as_mut() {
                    for (name, ty) in fields {
                        if let Some(entry) = obj_fields.get_mut(&name) {
                            let merged = merge_types(entry.clone(), ty.clone());
                            *entry = if matches!(fn_sym, Some("into")) {
                                merged
                            } else {
                                merge_types(merged, ret_ty.clone())
                            };
                        } else {
                            obj_fields.insert(name, ty);
                        }
                    }
                } else {
                    merge_opt(&mut map_key, Type::Str);
                    merge_opt(&mut map_val, value_union);
                }
            }
            Type::Named(name) => {
                shape_only = false;
                if let Some(fields) = env.get_type(&name).cloned() {
                    map_count += 1;
                    let value_union = fields
                        .values()
                        .cloned()
                        .reduce(merge_types)
                        .unwrap_or(Type::Any);
                    merge_opt(&mut input_val, value_union.clone());
                    if let Some(obj_fields) = object_fields.as_mut() {
                        for (field, ty) in fields {
                            if let Some(entry) = obj_fields.get_mut(&field) {
                                let merged = merge_types(entry.clone(), ty.clone());
                                *entry = if matches!(fn_sym, Some("into")) {
                                    merged
                                } else {
                                    merge_types(merged, ret_ty.clone())
                                };
                            } else {
                                obj_fields.insert(field, ty);
                            }
                        }
                    } else {
                        merge_opt(&mut map_key, Type::Str);
                        merge_opt(&mut map_val, value_union);
                    }
                } else {
                    diags.push(error_diag(format!("unknown type: {}", name)));
                    object_fields = None;
                    map_count += 1;
                    merge_opt(&mut map_key, Type::Any);
                    merge_opt(&mut map_val, Type::Any);
                    merge_opt(&mut input_val, Type::Any);
                }
            }
            Type::Any | Type::Dyn | Type::DynOf(_) => {
                object_fields = None;
                shape_only = false;
                map_count += 1;
                merge_opt(&mut map_key, Type::Any);
                merge_opt(&mut map_val, Type::Any);
                merge_opt(&mut input_val, Type::Any);
            }
            other => {
                diags.push(error_diag(format!("merge-with expects map, got {}", other)));
                object_fields = None;
                shape_only = false;
                map_count += 1;
                merge_opt(&mut map_key, Type::Any);
                merge_opt(&mut map_val, Type::Any);
                merge_opt(&mut input_val, Type::Any);
            }
        }
    }

    let input_val = input_val.unwrap_or(Type::Any);
    if matches!(fn_sym, Some("into")) {
        ret_ty = input_val.clone();
    }
    if !is_assignable_with_env(&input_val, &param_left, env) {
        if is_optional_assignable(&input_val, &param_left, env) {
            report_optional_usage(&input_val, level, diags, "merge-with");
        } else {
            diags.push(error_diag(format!(
                "merge-with expects {}, got {}",
                param_left, input_val
            )));
        }
    }
    if !is_assignable_with_env(&input_val, &param_right, env) {
        if is_optional_assignable(&input_val, &param_right, env) {
            report_optional_usage(&input_val, level, diags, "merge-with");
        } else {
            diags.push(error_diag(format!(
                "merge-with expects {}, got {}",
                param_right, input_val
            )));
        }
    }

    if let Some(fields) = object_fields {
        if map_key.is_none() && map_val.is_none() {
            if shape_only {
                return if shape_open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                };
            }
            return Type::Object(fields);
        }
        let value_union = fields
            .values()
            .cloned()
            .reduce(merge_types)
            .unwrap_or(Type::Any);
        let default_key = if shape_only { Type::Keyword } else { Type::Str };
        merge_opt(&mut map_key, default_key);
        merge_opt(&mut map_val, value_union);
    }

    let key = map_key.unwrap_or(Type::Any);
    let val = map_val.unwrap_or(Type::Any);
    let result_val = if map_count > 1 {
        merge_types(val, ret_ty)
    } else {
        val
    };
    Type::Map(Box::new(key), Box::new(result_val))
}

fn infer_contains_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("contains? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    let key_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Map(key, _) => {
            if !is_assignable_with_env(&key_ty, &key, env) {
                if is_optional_assignable(&key_ty, &key, env) {
                    report_optional_usage(&key_ty, level, diags, "contains?");
                } else {
                    diags.push(error_diag(format!(
                        "contains? expects {}, got {}",
                        key, key_ty
                    )));
                }
            }
        }
        Type::Shape(_) => {
            if !is_assignable_with_env(&key_ty, &Type::Keyword, env) {
                if is_optional_assignable(&key_ty, &Type::Keyword, env) {
                    report_optional_usage(&key_ty, level, diags, "contains?");
                } else {
                    diags.push(error_diag(format!(
                        "contains? expects Keyword key, got {}",
                        key_ty
                    )));
                }
            }
        }
        Type::Object(_) => {
            let key_expect = Type::union_two(Type::Str, Type::Keyword);
            if !is_assignable_with_env(&key_ty, &key_expect, env) {
                if is_optional_assignable(&key_ty, &key_expect, env) {
                    report_optional_usage(&key_ty, level, diags, "contains?");
                } else {
                    diags.push(error_diag(format!(
                        "contains? expects Str or Keyword key, got {}",
                        key_ty
                    )));
                }
            }
        }
        Type::Named(name) => {
            if env.get_type(&name).is_none() {
                diags.push(error_diag(format!("unknown type: {}", name)));
            } else {
                let key_expect = Type::union_two(Type::Str, Type::Keyword);
                if !is_assignable_with_env(&key_ty, &key_expect, env) {
                    if is_optional_assignable(&key_ty, &key_expect, env) {
                        report_optional_usage(&key_ty, level, diags, "contains?");
                    } else {
                        diags.push(error_diag(format!(
                            "contains? expects Str or Keyword key, got {}",
                            key_ty
                        )));
                    }
                }
            }
        }
        Type::Vec(_) => {
            if !is_assignable_with_env(&key_ty, &Type::Int, env) {
                if is_optional_assignable(&key_ty, &Type::Int, env) {
                    report_optional_usage(&key_ty, level, diags, "contains?");
                } else {
                    diags.push(error_diag(format!(
                        "contains? expects Int index, got {}",
                        key_ty
                    )));
                }
            }
        }
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(_)
                    | Type::Map(_, _)
                    | Type::Shape(_)
                    | Type::Object(_)
                    | Type::Named(_) => {
                        report_optional_usage(&other, level, diags, "contains?");
                    }
                    _ => diags.push(error_diag(format!(
                        "contains? expects map or vector, got {}",
                        other
                    ))),
                }
            } else {
                diags.push(error_diag(format!(
                    "contains? expects map or vector, got {}",
                    other
                )));
            }
        }
    }
    Type::Bool
}

fn infer_number_pred_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
        return Type::Bool;
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    if contains_any(&arg_ty) || contains_dyn(&arg_ty) {
        report_unresolved(&arg_ty, level, diags, name.to_string());
    } else if !is_numeric_type(&arg_ty) {
        if let Some(stripped) = strip_nil(&arg_ty) {
            if is_numeric_type(&stripped) {
                report_optional_usage(&arg_ty, level, diags, name);
                return Type::Bool;
            }
        }
        diags.push(error_diag(format!(
            "{} expects number, got {}",
            name, arg_ty
        )));
    }
    Type::Bool
}

fn infer_includes_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("includes? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    let item_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => {
            if !is_assignable_with_env(&item_ty, &inner, env) {
                if is_optional_assignable(&item_ty, &inner, env) {
                    report_optional_usage(&item_ty, level, diags, "includes?");
                } else {
                    diags.push(error_diag(format!(
                        "includes? expects {}, got {}",
                        inner, item_ty
                    )));
                }
            }
        }
        Type::Map(key, _) => {
            if !is_assignable_with_env(&item_ty, &key, env) {
                if is_optional_assignable(&item_ty, &key, env) {
                    report_optional_usage(&item_ty, level, diags, "includes?");
                } else {
                    diags.push(error_diag(format!(
                        "includes? expects {}, got {}",
                        key, item_ty
                    )));
                }
            }
        }
        Type::Shape(_) => {
            if !is_assignable_with_env(&item_ty, &Type::Keyword, env) {
                if is_optional_assignable(&item_ty, &Type::Keyword, env) {
                    report_optional_usage(&item_ty, level, diags, "includes?");
                } else {
                    diags.push(error_diag(format!(
                        "includes? expects Keyword key, got {}",
                        item_ty
                    )));
                }
            }
        }
        Type::Object(_) => {
            let key_expect = Type::union_two(Type::Str, Type::Keyword);
            if !is_assignable_with_env(&item_ty, &key_expect, env) {
                if is_optional_assignable(&item_ty, &key_expect, env) {
                    report_optional_usage(&item_ty, level, diags, "includes?");
                } else {
                    diags.push(error_diag(format!(
                        "includes? expects Str or Keyword key, got {}",
                        item_ty
                    )));
                }
            }
        }
        Type::Named(name) => {
            if env.get_type(&name).is_none() {
                diags.push(error_diag(format!("unknown type: {}", name)));
            } else {
                let key_expect = Type::union_two(Type::Str, Type::Keyword);
                if !is_assignable_with_env(&item_ty, &key_expect, env) {
                    if is_optional_assignable(&item_ty, &key_expect, env) {
                        report_optional_usage(&item_ty, level, diags, "includes?");
                    } else {
                        diags.push(error_diag(format!(
                            "includes? expects Str or Keyword key, got {}",
                            item_ty
                        )));
                    }
                }
            }
        }
        Type::Str => {
            if !is_assignable_with_env(&item_ty, &Type::Str, env) {
                if is_optional_assignable(&item_ty, &Type::Str, env) {
                    report_optional_usage(&item_ty, level, diags, "includes?");
                } else {
                    diags.push(error_diag(format!(
                        "includes? expects Str, got {}",
                        item_ty
                    )));
                }
            }
        }
        Type::Nil => {}
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(_)
                    | Type::Map(_, _)
                    | Type::Shape(_)
                    | Type::Object(_)
                    | Type::Named(_)
                    | Type::Str => {
                        report_optional_usage(&other, level, diags, "includes?");
                    }
                    _ => diags.push(error_diag(format!(
                        "includes? expects collection, got {}",
                        other
                    ))),
                }
            } else {
                diags.push(error_diag(format!(
                    "includes? expects collection, got {}",
                    other
                )));
            }
        }
    }
    Type::Bool
}

fn infer_get_in_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("get-in expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let path_expr = &args[1];
    let path_ty = infer_expr(path_expr, env, diags, level);
    let mut missing_possible = false;
    let path_vec = match path_ty {
        Type::Vec(_) => true,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_)) {
                    report_optional_usage(&other, level, diags, "get-in");
                    missing_possible = true;
                    true
                } else {
                    diags.push(error_diag(format!(
                        "get-in expects vector path, got {}",
                        other
                    )));
                    false
                }
            } else {
                diags.push(error_diag(format!(
                    "get-in expects vector path, got {}",
                    other
                )));
                false
            }
        }
    };
    let path_items = match path_expr {
        AstExpr::Vector(items) => Some(items.as_slice()),
        _ => None,
    };
    let mut current_ty = raw_coll_ty;
    if path_vec {
        if let Some(items) = path_items {
            if items.is_empty() {
                return current_ty;
            }
            for key_expr in items {
                if let Some(stripped) = strip_nil(&current_ty) {
                    report_optional_usage(&current_ty, level, diags, "get-in");
                    missing_possible = true;
                    current_ty = stripped;
                }
                match current_ty {
                    Type::Map(map_key, map_val) => {
                        let key_ty = infer_expr(key_expr, env, diags, level);
                        if !is_assignable_with_env(&key_ty, &map_key, env) {
                            if is_optional_assignable(&key_ty, &map_key, env) {
                                report_optional_usage(&key_ty, level, diags, "get-in key");
                            } else {
                                diags.push(error_diag(format!(
                                    "get-in expects key {}, got {}",
                                    map_key, key_ty
                                )));
                            }
                        }
                        current_ty = *map_val;
                        missing_possible = true;
                    }
                    Type::Shape(shape) => {
                        let field = match key_expr {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            if let Some(value) = shape.fields.get(&field) {
                                current_ty = value.clone();
                            } else if shape.open {
                                current_ty = Type::Any;
                                missing_possible = true;
                            } else {
                                current_ty = Type::Nil;
                                missing_possible = true;
                            }
                        } else {
                            current_ty = Type::Any;
                            missing_possible = true;
                        }
                    }
                    Type::Object(fields) => {
                        let field = match key_expr {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            current_ty = fields.get(&field).cloned().unwrap_or(Type::Nil);
                        } else {
                            current_ty = Type::Any;
                            missing_possible = true;
                        }
                    }
                    Type::Named(name) => {
                        if let Some(fields) = env.get_type(&name) {
                            let field = match key_expr {
                                AstExpr::Keyword(name) => Some(name.clone()),
                                AstExpr::Literal(crate::ast::Literal::Str(name)) => {
                                    Some(name.clone())
                                }
                                _ => None,
                            };
                            if let Some(field) = field {
                                current_ty = fields.get(&field).cloned().unwrap_or(Type::Nil);
                            } else {
                                current_ty = Type::Any;
                                missing_possible = true;
                            }
                        } else {
                            diags.push(error_diag(format!("unknown type: {}", name)));
                            current_ty = Type::Any;
                        }
                    }
                    Type::Vec(inner) => {
                        let key_ty = infer_expr(key_expr, env, diags, level);
                        if !is_assignable_with_env(&key_ty, &Type::Int, env) {
                            if is_optional_assignable(&key_ty, &Type::Int, env) {
                                report_optional_usage(&key_ty, level, diags, "get-in index");
                            } else {
                                diags.push(error_diag(format!(
                                    "get-in expects Int index, got {}",
                                    key_ty
                                )));
                            }
                        }
                        current_ty = *inner;
                        missing_possible = true;
                    }
                    Type::Any | Type::Dyn | Type::DynOf(_) => {
                        current_ty = Type::Any;
                        missing_possible = true;
                    }
                    other => {
                        diags.push(error_diag(format!(
                            "get-in expects map or vector, got {}",
                            other
                        )));
                        current_ty = Type::Any;
                    }
                }
            }
        } else {
            missing_possible = true;
            current_ty = Type::Any;
        }
    } else {
        current_ty = Type::Any;
    }
    if args.len() == 3 {
        let default_ty = infer_expr(&args[2], env, diags, level);
        current_ty = merge_types(current_ty, default_ty);
    } else if missing_possible {
        current_ty = merge_types(current_ty, Type::Nil);
    }
    current_ty
}

fn resolve_literal_path_type(raw_ty: &Type, path_expr: &AstExpr, env: &TypeEnv) -> Option<Type> {
    let AstExpr::Vector(items) = path_expr else {
        return None;
    };
    if items.is_empty() {
        return Some(raw_ty.clone());
    }
    let mut current_ty = raw_ty.clone();
    for key_expr in items {
        if let Some(stripped) = strip_nil(&current_ty) {
            current_ty = stripped;
        }
        current_ty = match current_ty {
            Type::Map(_, val) => *val,
            Type::Shape(shape) => {
                let field = match key_expr {
                    AstExpr::Keyword(name) => Some(name.clone()),
                    _ => None,
                };
                if let Some(field) = field {
                    shape.fields.get(&field).cloned().unwrap_or_else(|| {
                        if shape.open {
                            Type::Any
                        } else {
                            Type::Nil
                        }
                    })
                } else if shape.open {
                    Type::Any
                } else {
                    Type::Nil
                }
            }
            Type::Object(fields) => {
                if let Some(field) = literal_field_name(key_expr) {
                    fields.get(&field).cloned().unwrap_or(Type::Any)
                } else {
                    Type::Any
                }
            }
            Type::Named(name) => {
                if let Some(fields) = env.get_type(&name) {
                    if let Some(field) = literal_field_name(key_expr) {
                        fields.get(&field).cloned().unwrap_or(Type::Any)
                    } else {
                        Type::Any
                    }
                } else {
                    Type::Any
                }
            }
            Type::Any | Type::Dyn | Type::DynOf(_) => Type::Any,
            other => other,
        };
    }
    Some(current_ty)
}

fn update_type_path(current: Type, path: &[AstExpr], new_ty: Type, env: &TypeEnv) -> Option<Type> {
    if path.is_empty() {
        return Some(new_ty);
    }
    let key_expr = &path[0];
    match current {
        Type::Shape(mut shape) => {
            let field = match key_expr {
                AstExpr::Keyword(name) => Some(name.clone()),
                _ => None,
            }?;
            let next = shape.fields.get(&field).cloned().unwrap_or(Type::Any);
            let updated = update_type_path(next, &path[1..], new_ty, env)?;
            shape.fields.insert(field, updated);
            Some(Type::Shape(shape))
        }
        Type::Object(mut fields) => {
            let field = literal_field_name(key_expr)?;
            let next = fields.get(&field).cloned().unwrap_or(Type::Any);
            let updated = update_type_path(next, &path[1..], new_ty, env)?;
            fields.insert(field, updated);
            Some(Type::Object(fields))
        }
        Type::Named(name) => {
            let mut fields = env.get_type(&name)?.clone();
            let field = literal_field_name(key_expr)?;
            let next = fields.get(&field).cloned().unwrap_or(Type::Any);
            let updated = update_type_path(next, &path[1..], new_ty, env)?;
            fields.insert(field, updated);
            Some(Type::Object(fields))
        }
        Type::Map(key, val) => {
            let updated = update_type_path(*val, &path[1..], new_ty, env)?;
            Some(Type::Map(key, Box::new(updated)))
        }
        _ => None,
    }
}

fn infer_dissoc_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 {
        diags.push(error_diag("dissoc expects map and keys".to_string()));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "dissoc");
    if coll_ty.is_some() {
        require_unique_in_mut("dissoc", UniqueKind::Map, &args[0], env, diags);
    }
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            for key_expr in &args[1..] {
                let key_ty = infer_expr(key_expr, env, diags, level);
                if !is_assignable_with_env(&key_ty, &map_key, env) {
                    if is_optional_assignable(&key_ty, &map_key, env) {
                        report_optional_usage(&key_ty, level, diags, "dissoc key");
                    } else {
                        diags.push(error_diag(format!(
                            "dissoc expects key {}, got {}",
                            map_key, key_ty
                        )));
                    }
                }
            }
            Type::Map(map_key, map_val)
        }
        Some(Type::Shape(shape)) => {
            let mut fields = shape.fields;
            let open = shape.open;
            let mut fallback_to_map = false;
            for key_expr in &args[1..] {
                if let Some(field) = keyword_field_name(key_expr) {
                    fields.remove(&field);
                } else {
                    fallback_to_map = true;
                }
            }
            if fallback_to_map {
                let mut merged = Type::Nil;
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(Box::new(Type::Keyword), Box::new(merged))
            } else if open {
                Type::open_shape(fields)
            } else {
                Type::shape(fields)
            }
        }
        Some(Type::Object(mut fields)) => {
            let mut fallback_to_map = false;
            for key_expr in &args[1..] {
                let field = match key_expr {
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                    _ => None,
                };
                if let Some(field) = field {
                    fields.remove(&field);
                } else {
                    fallback_to_map = true;
                }
            }
            if fallback_to_map {
                let mut merged = Type::Nil;
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(
                    Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                    Box::new(merged),
                )
            } else {
                Type::Object(fields)
            }
        }
        Some(Type::Named(name)) => {
            if let Some(base_fields) = env.get_type(&name).cloned() {
                let mut fields = base_fields;
                let mut fallback_to_map = false;
                for key_expr in &args[1..] {
                    let field = match key_expr {
                        AstExpr::Keyword(name) => Some(name.clone()),
                        AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                        _ => None,
                    };
                    if let Some(field) = field {
                        fields.remove(&field);
                    } else {
                        fallback_to_map = true;
                    }
                }
                if fallback_to_map {
                    let mut merged = Type::Nil;
                    for ty in fields.values() {
                        merged = merge_types(merged, ty.clone());
                    }
                    Type::Map(
                        Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                        Box::new(merged),
                    )
                } else {
                    Type::Object(fields)
                }
            } else {
                diags.push(error_diag(format!("dissoc unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("dissoc expects map, got {}", other)));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "dissoc expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn infer_assoc_in_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 3 {
        diags.push(error_diag(
            "assoc-in expects map, path, and value".to_string(),
        ));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let path_expr = &args[1];
    let path_ty = infer_expr(path_expr, env, diags, level);
    if !matches!(path_ty, Type::Vec(_)) {
        if let Some(stripped) = strip_nil(&path_ty) {
            if matches!(stripped, Type::Vec(_)) {
                report_optional_usage(&path_ty, level, diags, "assoc-in");
            } else {
                diags.push(error_diag(format!(
                    "assoc-in expects vector path, got {}",
                    path_ty
                )));
            }
        } else {
            diags.push(error_diag(format!(
                "assoc-in expects vector path, got {}",
                path_ty
            )));
        }
    }
    let value_ty = infer_expr(&args[2], env, diags, level);
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "assoc-in");
    if coll_ty.is_some() {
        require_unique_in_mut("assoc-in", UniqueKind::Map, &args[0], env, diags);
    }
    let path_items = match path_expr {
        AstExpr::Vector(items) => Some(items.as_slice()),
        _ => None,
    };
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            if let Some(items) = path_items {
                if items.len() == 1 {
                    let key_ty = infer_expr(&items[0], env, diags, level);
                    if !is_assignable_with_env(&key_ty, &map_key, env) {
                        if is_optional_assignable(&key_ty, &map_key, env) {
                            report_optional_usage(&key_ty, level, diags, "assoc-in key");
                        } else {
                            diags.push(error_diag(format!(
                                "assoc-in expects key {}, got {}",
                                map_key, key_ty
                            )));
                        }
                    }
                    let merged_val = merge_types(*map_val, value_ty);
                    return Type::Map(map_key, Box::new(merged_val));
                }
            }
            Type::Map(map_key, map_val)
        }
        Some(Type::Shape(shape)) => {
            let mut fields = shape.fields;
            let open = shape.open;
            if let Some(items) = path_items {
                if items.len() == 1 {
                    if let Some(field) = keyword_field_name(&items[0]) {
                        let existing = fields.get(&field).cloned().unwrap_or(Type::Nil);
                        fields.insert(field, merge_types(existing, value_ty));
                        return if open {
                            Type::open_shape(fields)
                        } else {
                            Type::shape(fields)
                        };
                    }
                }
            }
            let mut merged = Type::Nil;
            for ty in fields.values() {
                merged = merge_types(merged, ty.clone());
            }
            Type::Map(
                Box::new(Type::Keyword),
                Box::new(merge_types(merged, value_ty)),
            )
        }
        Some(Type::Object(mut fields)) => {
            if let Some(items) = path_items {
                if items.len() == 1 {
                    let field = match &items[0] {
                        AstExpr::Keyword(name) => Some(name.clone()),
                        AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                        _ => None,
                    };
                    if let Some(field) = field {
                        if let Some(existing) = fields.get(&field).cloned() {
                            fields.insert(field, merge_types(existing, value_ty));
                            return Type::Object(fields);
                        }
                    }
                }
            }
            let mut merged = Type::Nil;
            for ty in fields.values() {
                merged = merge_types(merged, ty.clone());
            }
            Type::Map(
                Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                Box::new(merge_types(merged, value_ty)),
            )
        }
        Some(Type::Named(name)) => {
            if let Some(base_fields) = env.get_type(&name).cloned() {
                let mut fields = base_fields;
                if let Some(items) = path_items {
                    if items.len() == 1 {
                        let field = match &items[0] {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            if let Some(existing) = fields.get(&field).cloned() {
                                fields.insert(field, merge_types(existing, value_ty));
                                return Type::Object(fields);
                            }
                        }
                    }
                }
                let mut merged = Type::Nil;
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(
                    Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                    Box::new(merge_types(merged, value_ty)),
                )
            } else {
                diags.push(error_diag(format!("assoc-in unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("assoc-in expects map, got {}", other)));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "assoc-in expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn infer_update_in_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 3 {
        diags.push(error_diag(
            "update-in expects map, path, and function".to_string(),
        ));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let path_expr = &args[1];
    let path_ty = infer_expr(path_expr, env, diags, level);
    let path_vec = match path_ty {
        Type::Vec(_) => true,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_)) {
                    report_optional_usage(&other, level, diags, "update-in");
                    true
                } else {
                    diags.push(error_diag(format!(
                        "update-in expects vector path, got {}",
                        other
                    )));
                    false
                }
            } else {
                diags.push(error_diag(format!(
                    "update-in expects vector path, got {}",
                    other
                )));
                false
            }
        }
    };
    let path_items = match path_expr {
        AstExpr::Vector(items) => Some(items.as_slice()),
        _ => None,
    };
    let fn_expr = &args[2];
    let fn_sym = match fn_expr {
        AstExpr::Symbol(sym) => Some(sym.as_str()),
        _ => None,
    };
    let fn_ty = match fn_expr {
        AstExpr::Symbol(sym) => env
            .get(sym)
            .unwrap_or_else(|| infer_expr(fn_expr, env, diags, level)),
        _ => infer_expr(fn_expr, env, diags, level),
    };
    let mut current_ty = raw_coll_ty.clone();
    let mut missing_possible = false;
    if path_vec {
        if let Some(items) = path_items {
            for key_expr in items {
                if let Some(stripped) = strip_nil(&current_ty) {
                    report_optional_usage(&current_ty, level, diags, "update-in");
                    missing_possible = true;
                    current_ty = stripped;
                }
                match current_ty {
                    Type::Map(map_key, map_val) => {
                        let key_ty = infer_expr(key_expr, env, diags, level);
                        if !is_assignable_with_env(&key_ty, &map_key, env) {
                            if is_optional_assignable(&key_ty, &map_key, env) {
                                report_optional_usage(&key_ty, level, diags, "update-in key");
                            } else {
                                diags.push(error_diag(format!(
                                    "update-in expects key {}, got {}",
                                    map_key, key_ty
                                )));
                            }
                        }
                        current_ty = *map_val;
                        missing_possible = true;
                    }
                    Type::Shape(shape) => {
                        let field = match key_expr {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            if let Some(value) = shape.fields.get(&field) {
                                current_ty = value.clone();
                            } else if shape.open {
                                current_ty = Type::Any;
                                missing_possible = true;
                            } else {
                                current_ty = Type::Nil;
                                missing_possible = true;
                            }
                        } else {
                            current_ty = Type::Any;
                            missing_possible = true;
                        }
                    }
                    Type::Object(fields) => {
                        let field = match key_expr {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            if let Some(value) = fields.get(&field) {
                                current_ty = value.clone();
                            } else {
                                current_ty = Type::Nil;
                                missing_possible = true;
                            }
                        } else {
                            current_ty = Type::Any;
                            missing_possible = true;
                        }
                    }
                    Type::Named(name) => {
                        if let Some(fields) = env.get_type(&name) {
                            let field = match key_expr {
                                AstExpr::Keyword(name) => Some(name.clone()),
                                AstExpr::Literal(crate::ast::Literal::Str(name)) => {
                                    Some(name.clone())
                                }
                                _ => None,
                            };
                            if let Some(field) = field {
                                if let Some(value) = fields.get(&field) {
                                    current_ty = value.clone();
                                } else {
                                    current_ty = Type::Nil;
                                    missing_possible = true;
                                }
                            } else {
                                current_ty = Type::Any;
                                missing_possible = true;
                            }
                        } else {
                            diags.push(error_diag(format!("update-in unknown type: {}", name)));
                            current_ty = Type::Any;
                            missing_possible = true;
                        }
                    }
                    Type::Any | Type::Dyn | Type::DynOf(_) => {
                        current_ty = Type::Any;
                        missing_possible = true;
                    }
                    other => {
                        diags.push(error_diag(format!("update-in expects map, got {}", other)));
                        current_ty = Type::Any;
                    }
                }
            }
        } else {
            missing_possible = true;
            current_ty = Type::Any;
        }
    } else {
        current_ty = Type::Any;
    }
    if matches!(
        raw_coll_ty,
        Type::Shape(_) | Type::Object(_) | Type::Named(_)
    ) {
        if let Some(resolved) = resolve_literal_path_type(&raw_coll_ty, path_expr, env) {
            if !contains_nil(&resolved)
                && !matches!(resolved, Type::Any | Type::Dyn | Type::DynOf(_))
            {
                current_ty = resolved;
                missing_possible = false;
            }
        }
    }
    if missing_possible {
        current_ty = merge_types(current_ty, Type::Nil);
    }
    let mut extra_arg_types = Vec::new();
    for arg in args.iter().skip(3) {
        extra_arg_types.push(infer_expr(arg, env, diags, level));
    }
    let fn_label = match fn_expr {
        AstExpr::Symbol(sym) => aliases::resolve_alias(sym),
        _ => "update-in",
    };
    let ret_ty = if matches!(fn_sym, Some("conj")) {
        if contains_nil(&current_ty) {
            report_optional_usage(&current_ty, level, diags, "update-in");
        }
        let mut base = resolve_literal_path_type(&raw_coll_ty, path_expr, env)
            .unwrap_or_else(|| current_ty.clone());
        if missing_possible {
            base = merge_types(base, Type::Nil);
        }
        if let Some(stripped) = strip_nil(&base) {
            base = stripped;
        }
        let mut elem_ty: Option<Type> = match base {
            Type::Vec(inner) => {
                if *inner == Type::Any {
                    None
                } else {
                    Some(*inner)
                }
            }
            other if matches!(other, Type::Any | Type::Dyn | Type::DynOf(_)) => None,
            other => {
                diags.push(error_diag(format!(
                    "update-in expects vector, got {}",
                    other
                )));
                None
            }
        };
        for arg in &extra_arg_types {
            elem_ty = Some(match elem_ty.take() {
                Some(prev) => merge_types(prev, arg.clone()),
                None => arg.clone(),
            });
        }
        match elem_ty {
            Some(elem_ty) => Type::Vec(Box::new(elem_ty)),
            None => Type::Any,
        }
    } else {
        apply_update_fn(
            &current_ty,
            &fn_ty,
            &extra_arg_types,
            env,
            diags,
            level,
            fn_label,
        )
    };
    let updated_ty = match path_expr {
        AstExpr::Vector(items) => update_type_path(raw_coll_ty.clone(), items, ret_ty.clone(), env),
        _ => None,
    };
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "update-in");
    if coll_ty.is_some() {
        require_unique_in_mut("update-in", UniqueKind::Map, &args[0], env, diags);
    }
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            Type::Map(map_key, Box::new(merge_types(*map_val, ret_ty)))
        }
        Some(Type::Shape(shape)) => {
            let shape = match updated_ty {
                Some(Type::Shape(updated)) => updated,
                _ => shape,
            };
            let mut merged = ret_ty;
            for ty in shape.fields.values() {
                merged = merge_types(merged, ty.clone());
            }
            Type::Map(Box::new(Type::Keyword), Box::new(merged))
        }
        Some(Type::Object(fields)) => {
            let fields = match updated_ty {
                Some(Type::Object(updated)) => updated,
                _ => fields,
            };
            let mut merged = ret_ty;
            for ty in fields.values() {
                merged = merge_types(merged, ty.clone());
            }
            Type::Map(
                Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                Box::new(merged),
            )
        }
        Some(Type::Named(name)) => {
            let fields = match updated_ty {
                Some(Type::Object(updated)) => Some(updated),
                _ => env.get_type(&name).cloned(),
            };
            if let Some(fields) = fields {
                let mut merged = ret_ty;
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(
                    Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                    Box::new(merged),
                )
            } else {
                diags.push(error_diag(format!("update-in unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("update-in expects map, got {}", other)));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "update-in expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn infer_take_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("take expects 2 arguments".to_string()));
        return Type::Any;
    }
    let count_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&count_ty, &Type::Int, env) {
        if is_optional_assignable(&count_ty, &Type::Int, env) {
            report_optional_usage(&count_ty, level, diags, "take");
        } else {
            diags.push(error_diag(format!(
                "take expects Int count, got {}",
                count_ty
            )));
        }
    }
    if let AstExpr::Call { callee, args } = &args[1] {
        if let AstExpr::Symbol(sym) = callee.as_ref() {
            match sym.as_str() {
                "range" if args.is_empty() => {
                    return Type::Vec(Box::new(Type::Int));
                }
                "repeat" if args.len() == 1 => {
                    let val_ty = infer_expr(&args[0], env, diags, level);
                    return Type::Vec(Box::new(val_ty));
                }
                "repeatedly" if args.len() == 1 => {
                    let fn_ty = infer_expr(&args[0], env, diags, level);
                    let ret_ty = match fn_ty {
                        Type::Function { ret, .. } => (*ret).clone(),
                        other => other,
                    };
                    return Type::Vec(Box::new(ret_ty));
                }
                "iterate" if args.len() == 2 => {
                    let init_ty = infer_expr(&args[1], env, diags, level);
                    if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
                        &args[0],
                        &[init_ty.clone()],
                        env,
                        diags,
                        level,
                        "take",
                    ) {
                        return Type::Vec(Box::new(merge_types(init_ty, *ret)));
                    }
                    let fn_ty = infer_expr(&args[0], env, diags, level);
                    let ret_ty = match fn_ty {
                        Type::Function { ret, .. } => (*ret).clone(),
                        other => other,
                    };
                    return Type::Vec(Box::new(merge_types(init_ty, ret_ty)));
                }
                _ => {}
            }
        }
    }
    let coll_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str | Type::Tuple(_)) {
                    report_optional_usage(&other, level, diags, "take");
                    return match stripped {
                        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
                        _ => stripped,
                    };
                }
            }
            diags.push(error_diag(format!(
                "take expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_drop_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("drop expects 2 arguments".to_string()));
        return Type::Any;
    }
    let count_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&count_ty, &Type::Int, env) {
        if is_optional_assignable(&count_ty, &Type::Int, env) {
            report_optional_usage(&count_ty, level, diags, "drop");
        } else {
            diags.push(error_diag(format!(
                "drop expects Int count, got {}",
                count_ty
            )));
        }
    }
    let coll_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str | Type::Tuple(_)) {
                    report_optional_usage(&other, level, diags, "drop");
                    return match stripped {
                        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
                        _ => stripped,
                    };
                }
            }
            diags.push(error_diag(format!(
                "drop expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_take_last_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("take-last expects 2 arguments".to_string()));
        return Type::Any;
    }
    let count_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&count_ty, &Type::Int, env) {
        if is_optional_assignable(&count_ty, &Type::Int, env) {
            report_optional_usage(&count_ty, level, diags, "take-last");
        } else {
            diags.push(error_diag(format!(
                "take-last expects Int count, got {}",
                count_ty
            )));
        }
    }
    let coll_ty = infer_expr(&args[1], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str | Type::Tuple(_)) {
                    report_optional_usage(&other, level, diags, "take-last");
                    return match stripped {
                        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
                        _ => stripped,
                    };
                }
            }
            diags.push(error_diag(format!(
                "take-last expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_drop_last_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag("drop-last expects 1 or 2 arguments".to_string()));
        return Type::Any;
    }
    let coll_arg = if args.len() == 1 {
        &args[0]
    } else {
        let count_ty = infer_expr(&args[0], env, diags, level);
        if !is_assignable_with_env(&count_ty, &Type::Int, env) {
            if is_optional_assignable(&count_ty, &Type::Int, env) {
                report_optional_usage(&count_ty, level, diags, "drop-last");
            } else {
                diags.push(error_diag(format!(
                    "drop-last expects Int count, got {}",
                    count_ty
                )));
            }
        }
        &args[1]
    };
    let coll_ty = infer_expr(coll_arg, env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str | Type::Tuple(_)) {
                    report_optional_usage(&other, level, diags, "drop-last");
                    return match stripped {
                        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
                        _ => stripped,
                    };
                }
            }
            diags.push(error_diag(format!(
                "drop-last expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_reverse_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("reverse expects 1 argument".to_string()));
        return Type::Any;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => {
            require_unique_in_mut("reverse", UniqueKind::Vec, &args[0], env, diags);
            Type::Vec(inner)
        }
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str) {
                    report_optional_usage(&other, level, diags, "reverse");
                    return stripped;
                }
            }
            diags.push(error_diag(format!(
                "reverse expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_shuffle_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("shuffle expects 1 argument".to_string()));
        return Type::Any;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Str => Type::Str,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if matches!(stripped, Type::Vec(_) | Type::Str) {
                    report_optional_usage(&other, level, diags, "shuffle");
                    return stripped;
                }
            }
            diags.push(error_diag(format!(
                "shuffle expects vector or string, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_subvec_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("subvec expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    for arg in &args[1..] {
        let idx_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&idx_ty, &Type::Int, env) {
            if is_optional_assignable(&idx_ty, &Type::Int, env) {
                report_optional_usage(&idx_ty, level, diags, "subvec");
            } else {
                diags.push(error_diag(format!(
                    "subvec expects Int index, got {}",
                    idx_ty
                )));
            }
        }
    }
    match coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "subvec");
                    return Type::Vec(inner);
                }
            }
            diags.push(error_diag(format!("subvec expects vector, got {}", other)));
            Type::Any
        }
    }
}

fn infer_concat_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        diags.push(error_diag("concat expects at least 1 argument".to_string()));
        return Type::Any;
    }
    let mut vec_elem: Option<Type> = None;
    let mut is_vec = false;
    let mut is_str = false;
    for arg in args {
        let ty = infer_expr(arg, env, diags, level);
        match ty {
            Type::Vec(inner) => {
                is_vec = true;
                let elem = *inner;
                vec_elem = Some(match vec_elem.take() {
                    Some(prev) => merge_types(prev, elem),
                    None => elem,
                });
            }
            Type::Str => {
                is_str = true;
            }
            other => {
                if let Some(stripped) = strip_nil(&other) {
                    match stripped {
                        Type::Vec(inner) => {
                            report_optional_usage(&other, level, diags, "concat");
                            is_vec = true;
                            let elem = *inner;
                            vec_elem = Some(match vec_elem.take() {
                                Some(prev) => merge_types(prev, elem),
                                None => elem,
                            });
                            continue;
                        }
                        Type::Str => {
                            report_optional_usage(&other, level, diags, "concat");
                            is_str = true;
                            continue;
                        }
                        _ => {}
                    }
                }
                diags.push(error_diag(format!(
                    "concat expects vector or string, got {}",
                    other
                )));
                return Type::Any;
            }
        }
    }
    if is_vec && is_str {
        diags.push(error_diag("concat expects same type".to_string()));
        return Type::Any;
    }
    if is_vec {
        let elem = vec_elem.unwrap_or(Type::Any);
        return Type::Vec(Box::new(elem));
    }
    if is_str {
        return Type::Str;
    }
    Type::Any
}

fn infer_string_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let arity = match name {
        "starts-with?" | "ends-with?" => 2,
        _ => 1,
    };
    if args.len() != arity {
        diags.push(error_diag(format!("{} expects {} arguments", name, arity)));
        return match name {
            "starts-with?" | "ends-with?" => Type::Bool,
            _ => Type::Str,
        };
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Str, env) {
            if is_optional_assignable(&arg_ty, &Type::Str, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!("{} expects Str, got {}", name, arg_ty)));
            }
        }
    }
    match name {
        "starts-with?" | "ends-with?" => Type::Bool,
        _ => Type::Str,
    }
}

fn infer_split_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("split expects 2 or 3 arguments".to_string()));
        return Type::Vec(Box::new(Type::Str));
    }
    for arg in &args[0..2] {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Str, env) {
            if is_optional_assignable(&arg_ty, &Type::Str, env) {
                report_optional_usage(&arg_ty, level, diags, "split");
            } else {
                diags.push(error_diag(format!("split expects Str, got {}", arg_ty)));
            }
        }
    }
    if args.len() == 3 {
        let limit_ty = infer_expr(&args[2], env, diags, level);
        if !is_assignable_with_env(&limit_ty, &Type::Int, env) {
            if is_optional_assignable(&limit_ty, &Type::Int, env) {
                report_optional_usage(&limit_ty, level, diags, "split");
            } else {
                diags.push(error_diag(format!(
                    "split expects Int limit, got {}",
                    limit_ty
                )));
            }
        }
    }
    Type::Vec(Box::new(Type::Str))
}

fn infer_join_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag("join expects 1 or 2 arguments".to_string()));
        return Type::Str;
    }
    fn check_sep(sep_ty: Type, env: &TypeEnv, diags: &mut Vec<Diagnostic>, level: NativeLevel) {
        if !is_assignable_with_env(&sep_ty, &Type::Str, env) {
            if is_optional_assignable(&sep_ty, &Type::Str, env) {
                report_optional_usage(&sep_ty, level, diags, "join");
            } else {
                diags.push(error_diag(format!(
                    "join expects Str separator, got {}",
                    sep_ty
                )));
            }
        }
    }

    fn check_coll(coll_ty: Type, env: &TypeEnv, diags: &mut Vec<Diagnostic>, level: NativeLevel) {
        match coll_ty {
            Type::Vec(inner) => {
                if !is_assignable_with_env(&inner, &Type::Str, env) {
                    if is_optional_assignable(&inner, &Type::Str, env) {
                        report_optional_usage(&inner, level, diags, "join");
                    } else {
                        diags.push(error_diag(format!(
                            "join expects Vec<Str>, got Vec<{}>",
                            inner
                        )));
                    }
                }
            }
            Type::Tuple(items) => {
                let elem = Type::union(items);
                if !is_assignable_with_env(&elem, &Type::Str, env) {
                    diags.push(error_diag(format!(
                        "join expects Vec<Str>, got Vec<{}>",
                        elem
                    )));
                }
            }
            other => {
                if let Some(stripped) = strip_nil(&other) {
                    match stripped {
                        Type::Vec(inner) => {
                            report_optional_usage(&other, level, diags, "join");
                            if !is_assignable_with_env(&inner, &Type::Str, env) {
                                diags.push(error_diag(format!(
                                    "join expects Vec<Str>, got Vec<{}>",
                                    inner
                                )));
                            }
                            return;
                        }
                        Type::Tuple(items) => {
                            report_optional_usage(&other, level, diags, "join");
                            let elem = Type::union(items);
                            if !is_assignable_with_env(&elem, &Type::Str, env) {
                                diags.push(error_diag(format!(
                                    "join expects Vec<Str>, got Vec<{}>",
                                    elem
                                )));
                            }
                            return;
                        }
                        _ => {}
                    }
                }
                diags.push(error_diag(format!("join expects Vec<Str>, got {}", other)));
            }
        }
    }
    if args.len() == 2 {
        let first_ty = infer_expr(&args[0], env, diags, level);
        let second_ty = infer_expr(&args[1], env, diags, level);
        if is_assignable_with_env(&first_ty, &Type::Str, env) {
            check_sep(first_ty, env, diags, level);
            check_coll(second_ty, env, diags, level);
        } else if is_assignable_with_env(&second_ty, &Type::Str, env) {
            check_sep(second_ty, env, diags, level);
            check_coll(first_ty, env, diags, level);
        } else {
            check_coll(first_ty, env, diags, level);
            check_coll(second_ty, env, diags, level);
        }
        return Type::Str;
    }
    let coll_ty = infer_expr(&args[0], env, diags, level);
    check_coll(coll_ty, env, diags, level);
    Type::Str
}

fn infer_format_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        diags.push(error_diag("format expects at least 1 argument".to_string()));
        return Type::Str;
    }
    let fmt_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&fmt_ty, &Type::Str, env) {
        if is_optional_assignable(&fmt_ty, &Type::Str, env) {
            report_optional_usage(&fmt_ty, level, diags, "format");
        } else {
            diags.push(error_diag(format!(
                "format expects Str template, got {}",
                fmt_ty
            )));
        }
    }
    for arg in &args[1..] {
        let _ = infer_expr(arg, env, diags, level);
    }
    Type::Str
}

fn infer_string_extra_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if name == "replace" && args.len() == 2 {
        let map_ty = infer_expr(&args[0], env, diags, level);
        if unwrap_optional_map_like(&map_ty, level, diags, "replace").is_none() {
            diags.push(error_diag(format!("replace expects map, got {}", map_ty)));
        }
        let coll_ty = infer_expr(&args[1], env, diags, level);
        return match coll_ty {
            Type::Vec(inner) => Type::Vec(inner),
            Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
            Type::Map(key, val) => Type::Map(key, val),
            Type::Shape(shape) => {
                let value_union = shape
                    .fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                Type::Map(Box::new(Type::Keyword), Box::new(value_union))
            }
            Type::Object(fields) => {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                Type::Map(Box::new(Type::Str), Box::new(value_union))
            }
            Type::Named(name) => {
                if let Some(fields) = env.get_type(&name) {
                    let value_union = fields
                        .values()
                        .cloned()
                        .reduce(merge_types)
                        .unwrap_or(Type::Any);
                    Type::Map(Box::new(Type::Str), Box::new(value_union))
                } else {
                    diags.push(error_diag(format!("unknown type: {}", name)));
                    Type::Any
                }
            }
            other => {
                if let Some(stripped) = strip_nil(&other) {
                    match stripped {
                        Type::Vec(inner) => {
                            report_optional_usage(&other, level, diags, "replace");
                            Type::Vec(inner)
                        }
                        Type::Tuple(items) => {
                            report_optional_usage(&other, level, diags, "replace");
                            Type::Vec(Box::new(Type::union(items)))
                        }
                        Type::Map(key, val) => {
                            report_optional_usage(&other, level, diags, "replace");
                            Type::Map(key, val)
                        }
                        _ => {
                            diags.push(error_diag(format!(
                                "replace expects collection, got {}",
                                other
                            )));
                            Type::Any
                        }
                    }
                } else {
                    diags.push(error_diag(format!(
                        "replace expects collection, got {}",
                        other
                    )));
                    Type::Any
                }
            }
        };
    }
    if matches!(name, "index-of" | "last-index-of") {
        let ret = Type::union(vec![Type::Int, Type::Nil]);
        if args.len() != 2 && args.len() != 3 {
            diags.push(error_diag(format!("{} expects 2 or 3 arguments", name)));
            return ret;
        }
        for arg in &args[0..2] {
            let arg_ty = infer_expr(arg, env, diags, level);
            if !is_assignable_with_env(&arg_ty, &Type::Str, env) {
                if is_optional_assignable(&arg_ty, &Type::Str, env) {
                    report_optional_usage(&arg_ty, level, diags, name);
                } else {
                    diags.push(error_diag(format!("{} expects Str, got {}", name, arg_ty)));
                }
            }
        }
        if args.len() == 3 {
            let idx_ty = infer_expr(&args[2], env, diags, level);
            if !is_assignable_with_env(&idx_ty, &Type::Int, env) {
                if is_optional_assignable(&idx_ty, &Type::Int, env) {
                    report_optional_usage(&idx_ty, level, diags, name);
                } else {
                    diags.push(error_diag(format!(
                        "{} expects Int start, got {}",
                        name, idx_ty
                    )));
                }
            }
        }
        return ret;
    }
    let (arity, ret) = match name {
        "blank?" => (1, Type::Bool),
        "replace" | "replace-first" => (3, Type::Str),
        "split-lines" | "lines" => (1, Type::Vec(Box::new(Type::Str))),
        "capitalize" | "trim-newline" => (1, Type::Str),
        _ => (1, Type::Any),
    };
    if args.len() != arity {
        diags.push(error_diag(format!("{} expects {} arguments", name, arity)));
        return ret;
    }
    for arg in args {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Str, env) {
            if is_optional_assignable(&arg_ty, &Type::Str, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!("{} expects Str, got {}", name, arg_ty)));
            }
        }
    }
    ret
}

fn infer_replace_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() == 2 {
        let smap_ty = infer_expr(&args[0], env, diags, level);
        let coll_ty = infer_expr(&args[1], env, diags, level);
        let (_map_key, map_val) = match smap_ty {
            Type::Map(key, val) => (*key, *val),
            other => {
                diags.push(error_diag(format!("replace expects map, got {}", other)));
                return Type::Any;
            }
        };
        match coll_ty {
            Type::Vec(elem) => {
                let merged = merge_types(*elem, map_val);
                Type::Vec(Box::new(merged))
            }
            Type::Map(key, val) => {
                let merged_key = merge_types(*key, map_val);
                Type::Map(Box::new(merged_key), val)
            }
            other => {
                diags.push(error_diag(format!(
                    "replace expects collection, got {}",
                    other
                )));
                Type::Any
            }
        }
    } else if args.len() == 3 {
        let text_ty = infer_expr(&args[0], env, diags, level);
        let from_ty = infer_expr(&args[1], env, diags, level);
        let to_ty = infer_expr(&args[2], env, diags, level);
        for (name, ty) in [
            ("replace", text_ty),
            ("replace", from_ty),
            ("replace", to_ty),
        ] {
            if !is_assignable_with_env(&ty, &Type::Str, env) {
                if is_optional_assignable(&ty, &Type::Str, env) {
                    report_optional_usage(&ty, level, diags, name);
                } else {
                    diags.push(error_diag(format!("replace expects Str, got {}", ty)));
                }
            }
        }
        Type::Str
    } else {
        diags.push(error_diag("replace expects 2 or 3 arguments".to_string()));
        Type::Any
    }
}

fn infer_regex_call(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let cap_item = Type::union(vec![Type::Str, Type::Nil]);
    let cap_vec = Type::Vec(Box::new(cap_item));
    let match_type = Type::union(vec![Type::Str, cap_vec]);
    let ret = if name == "re-seq" {
        Type::Vec(Box::new(match_type))
    } else {
        Type::union(vec![Type::Nil, match_type])
    };
    if args.len() != 2 {
        diags.push(error_diag(format!("{} expects 2 arguments", name)));
        return ret;
    }
    for arg in &args[0..2] {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, &Type::Str, env) {
            if is_optional_assignable(&arg_ty, &Type::Str, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!("{} expects Str, got {}", name, arg_ty)));
            }
        }
    }
    ret
}

fn infer_dorun_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    match args.len() {
        1 => {
            let _ = infer_expr(&args[0], env, diags, level);
        }
        2 => {
            let count_ty = infer_expr(&args[0], env, diags, level);
            if !is_assignable_with_env(&count_ty, &Type::Int, env) {
                if is_optional_assignable(&count_ty, &Type::Int, env) {
                    report_optional_usage(&count_ty, level, diags, "dorun");
                } else {
                    diags.push(error_diag(format!(
                        "dorun expects Int count, got {}",
                        count_ty
                    )));
                }
            }
            let _ = infer_expr(&args[1], env, diags, level);
        }
        _ => {
            diags.push(error_diag("dorun expects 1 or 2 arguments".to_string()));
        }
    }
    Type::Nil
}

fn infer_gensym_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let prefix_ty = Type::union(vec![Type::Str, Type::Symbol, Type::Keyword]);
    if args.len() > 1 {
        diags.push(error_diag("gensym expects 0 or 1 argument".to_string()));
        return Type::Symbol;
    }
    if args.len() == 1 {
        let arg_ty = infer_expr(&args[0], env, diags, level);
        if !is_assignable_with_env(&arg_ty, &prefix_ty, env) {
            if is_optional_assignable(&arg_ty, &prefix_ty, env) {
                report_optional_usage(&arg_ty, level, diags, "gensym");
            } else {
                diags.push(error_diag(format!(
                    "gensym expects Str/Symbol/Keyword, got {}",
                    arg_ty
                )));
            }
        }
    }
    Type::Symbol
}

fn infer_instance_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let name_ty = Type::union(vec![Type::Str, Type::Symbol, Type::Keyword]);
    if args.len() != 2 {
        diags.push(error_diag("instance? expects 2 arguments".to_string()));
        return Type::Bool;
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&arg_ty, &name_ty, env) {
        if is_optional_assignable(&arg_ty, &name_ty, env) {
            report_optional_usage(&arg_ty, level, diags, "instance?");
        } else {
            diags.push(error_diag(format!(
                "instance? expects type name, got {}",
                arg_ty
            )));
        }
    }
    let _ = infer_expr(&args[1], env, diags, level);
    Type::Bool
}

fn infer_subs_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 && args.len() != 3 {
        diags.push(error_diag("subs expects 2 or 3 arguments".to_string()));
        return Type::Str;
    }
    let text_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&text_ty, &Type::Str, env) {
        if is_optional_assignable(&text_ty, &Type::Str, env) {
            report_optional_usage(&text_ty, level, diags, "subs");
        } else {
            diags.push(error_diag(format!("subs expects Str, got {}", text_ty)));
        }
    }
    for arg in &args[1..] {
        let idx_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&idx_ty, &Type::Int, env) {
            if is_optional_assignable(&idx_ty, &Type::Int, env) {
                report_optional_usage(&idx_ty, level, diags, "subs");
            } else {
                diags.push(error_diag(format!(
                    "subs expects Int index, got {}",
                    idx_ty
                )));
            }
        }
    }
    Type::Str
}

fn infer_slurp_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("slurp expects 1 argument".to_string()));
        return Type::Str;
    }
    let path_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&path_ty, &Type::Str, env) {
        if is_optional_assignable(&path_ty, &Type::Str, env) {
            report_optional_usage(&path_ty, level, diags, "slurp");
        } else {
            diags.push(error_diag(format!("slurp expects Str, got {}", path_ty)));
        }
    }
    Type::Str
}

fn infer_spit_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("spit expects 2 arguments".to_string()));
        return Type::Str;
    }
    let path_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&path_ty, &Type::Str, env) {
        if is_optional_assignable(&path_ty, &Type::Str, env) {
            report_optional_usage(&path_ty, level, diags, "spit");
        } else {
            diags.push(error_diag(format!(
                "spit expects Str path, got {}",
                path_ty
            )));
        }
    }
    let content_ty = infer_expr(&args[1], env, diags, level);
    if !is_assignable_with_env(&content_ty, &Type::Str, env) {
        if is_optional_assignable(&content_ty, &Type::Str, env) {
            report_optional_usage(&content_ty, level, diags, "spit");
        } else {
            diags.push(error_diag(format!(
                "spit expects Str content, got {}",
                content_ty
            )));
        }
    }
    Type::Str
}

fn infer_time_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("time expects 1 argument".to_string()));
        return measurement_type();
    }
    let fn_ty = match &args[0] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[0], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let ret_ty =
        extract_zero_arg_fn_ret(fn_ty, "time expects a function", diags).unwrap_or(Type::Any);
    measurement_type_with(ret_ty)
}

fn infer_bench_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("bench expects 2 arguments".to_string()));
        return measurement_type();
    }
    let count_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&count_ty, &Type::Int, env) {
        if is_optional_assignable(&count_ty, &Type::Int, env) {
            report_optional_usage(&count_ty, level, diags, "bench");
        } else {
            diags.push(error_diag(format!(
                "bench expects Int iterations, got {}",
                count_ty
            )));
        }
    }
    let fn_ty = match &args[1] {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(&args[1], env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let ret_ty =
        extract_zero_arg_fn_ret(fn_ty, "bench expects a function", diags).unwrap_or(Type::Any);
    measurement_type_with(ret_ty)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum UniqueKind {
    Map,
    Vec,
}

fn require_unique_in_mut(
    name: &str,
    kind: UniqueKind,
    expr: &AstExpr,
    env: &TypeEnv,
    diags: &mut Vec<Diagnostic>,
) {
    if env.current_mut_mode() != MutMode::Mut {
        return;
    }
    if let AstExpr::Symbol(sym) = expr {
        if let Some(unique_kind) = env.get_unique(sym) {
            if unique_kind == kind {
                return;
            }
        }
    }
    if is_fresh_collection_expr(expr, kind) {
        return;
    }
    let kind_label = match kind {
        UniqueKind::Map => "map",
        UniqueKind::Vec => "vector",
    };
    diags.push(error_diag(format!(
        "{} requires unique {} in mut mode",
        name, kind_label
    )));
}

fn is_fresh_collection_expr(expr: &AstExpr, kind: UniqueKind) -> bool {
    match (kind, expr) {
        (UniqueKind::Map, AstExpr::Map(_)) => true,
        (UniqueKind::Vec, AstExpr::Vector(_)) => true,
        (UniqueKind::Vec, AstExpr::Set(_)) => true,
        (UniqueKind::Map, AstExpr::Call { callee, .. }) => match callee.as_ref() {
            AstExpr::Symbol(sym) => {
                let canonical = aliases::resolve_alias(sym);
                matches!(
                    canonical,
                    "hash-map"
                        | "assoc"
                        | "assoc-in"
                        | "update"
                        | "update-in"
                        | "dissoc"
                        | "merge"
                        | "merge-with"
                        | "select-keys"
                        | "into"
                )
            }
            _ => false,
        },
        (UniqueKind::Vec, AstExpr::Call { callee, .. }) => match callee.as_ref() {
            AstExpr::Symbol(sym) => {
                let canonical = aliases::resolve_alias(sym);
                matches!(
                    canonical,
                    "vec"
                        | "vector"
                        | "list"
                        | "range"
                        | "map"
                        | "filter"
                        | "rest"
                        | "conj"
                        | "cons"
                        | "into"
                        | "interleave"
                        | "take"
                        | "drop"
                        | "take-while"
                        | "drop-while"
                        | "partition"
                        | "partition-all"
                        | "partition-by"
                )
            }
            _ => false,
        },
        (UniqueKind::Vec, AstExpr::Quote(expr)) => matches!(
            expr.kind,
            ExprKind::Vector(_) | ExprKind::List(_) | ExprKind::Set(_)
        ),
        (UniqueKind::Map, AstExpr::Quote(expr)) => matches!(expr.kind, ExprKind::Map(_)),
        _ => false,
    }
}

fn fresh_kind_for_expr(expr: &AstExpr) -> Option<UniqueKind> {
    if is_fresh_collection_expr(expr, UniqueKind::Map) {
        return Some(UniqueKind::Map);
    }
    if is_fresh_collection_expr(expr, UniqueKind::Vec) {
        return Some(UniqueKind::Vec);
    }
    None
}

fn count_symbol_uses_in_exprs(exprs: &[&AstExpr], target: &str) -> usize {
    let mut total = 0;
    for expr in exprs {
        total += count_symbol_uses_in_expr(expr, target, false);
    }
    total
}

fn count_symbol_uses_in_expr(expr: &AstExpr, target: &str, shadowed: bool) -> usize {
    match expr {
        AstExpr::Symbol(name) => {
            if !shadowed && name == target {
                1
            } else {
                0
            }
        }
        AstExpr::Literal(_) | AstExpr::Keyword(_) | AstExpr::ForeignBlock { .. } => 0,
        AstExpr::Quote(_) => 0,
        AstExpr::Vector(items) | AstExpr::Set(items) => items
            .iter()
            .map(|item| count_symbol_uses_in_expr(item, target, shadowed))
            .sum(),
        AstExpr::Map(entries) => entries
            .iter()
            .map(|(k, v)| {
                count_symbol_uses_in_expr(k, target, shadowed)
                    + count_symbol_uses_in_expr(v, target, shadowed)
            })
            .sum(),
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => {
            let mut total = count_symbol_uses_in_expr(cond, target, shadowed);
            total += count_symbol_uses_in_expr(then_expr, target, shadowed);
            if let Some(else_expr) = else_expr {
                total += count_symbol_uses_in_expr(else_expr, target, shadowed);
            }
            total
        }
        AstExpr::Let { bindings, body } => {
            let mut total = 0;
            let mut shadows = false;
            for Binding { name, value, .. } in bindings {
                total += count_symbol_uses_in_expr(value, target, shadowed);
                if name == target {
                    shadows = true;
                }
            }
            let body_shadowed = shadowed || shadows;
            for expr in body {
                total += count_symbol_uses_in_expr(expr, target, body_shadowed);
            }
            total
        }
        AstExpr::Call { callee, args } => {
            let mut total = count_symbol_uses_in_expr(callee, target, shadowed);
            for arg in args {
                total += count_symbol_uses_in_expr(arg, target, shadowed);
            }
            total
        }
        AstExpr::Fn { params, body, .. } => {
            let mut shadows = shadowed;
            if params.iter().any(|param| param.name == target) {
                shadows = true;
            }
            body.iter()
                .map(|expr| count_symbol_uses_in_expr(expr, target, shadows))
                .sum()
        }
        AstExpr::SetVar { value, .. } => count_symbol_uses_in_expr(value, target, shadowed),
    }
}

fn count_symbol_uses_in_toplevel(item: &TopLevel, target: &str) -> usize {
    match item {
        TopLevel::Def { value, .. } => count_symbol_uses_in_expr(value, target, false),
        TopLevel::Defn { params, body, .. } => {
            let shadowed = params.iter().any(|param| param.name == target);
            body.iter()
                .map(|expr| count_symbol_uses_in_expr(expr, target, shadowed))
                .sum()
        }
        TopLevel::Expr { expr, .. } => count_symbol_uses_in_expr(expr, target, false),
        _ => 0,
    }
}

fn is_empty_collection_literal(expr: &AstExpr) -> bool {
    match expr {
        AstExpr::Vector(items) | AstExpr::Set(items) => items.is_empty(),
        AstExpr::Map(entries) => entries.is_empty(),
        AstExpr::Quote(expr) => match &expr.kind {
            ExprKind::Vector(items) | ExprKind::List(items) | ExprKind::Set(items) => {
                items.is_empty()
            }
            ExprKind::Map(entries) => entries.is_empty(),
            _ => false,
        },
        _ => false,
    }
}

fn literal_collection_len(expr: &AstExpr) -> Option<usize> {
    match expr {
        AstExpr::Vector(items) | AstExpr::Set(items) => Some(items.len()),
        AstExpr::Map(entries) => Some(entries.len()),
        AstExpr::Call { callee, args } => match callee.as_ref() {
            AstExpr::Symbol(sym) if matches!(sym.as_str(), "vector" | "list") => Some(args.len()),
            AstExpr::Symbol(sym) if sym == "hash-map" => Some(args.len() / 2),
            _ => None,
        },
        AstExpr::Quote(expr) => match &expr.kind {
            ExprKind::Vector(items) | ExprKind::List(items) | ExprKind::Set(items) => {
                Some(items.len())
            }
            ExprKind::Map(entries) => Some(entries.len()),
            _ => None,
        },
        _ => None,
    }
}

fn infer_name_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("name expects 1 argument".to_string()));
        return Type::Str;
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    let expected = Type::union(vec![Type::Keyword, Type::Symbol, Type::Str]);
    if !is_assignable_with_env(&arg_ty, &expected, env) {
        if is_optional_assignable(&arg_ty, &expected, env) {
            report_optional_usage(&arg_ty, level, diags, "name");
        } else {
            diags.push(error_diag(format!(
                "name expects keyword, symbol, or string, got {}",
                arg_ty
            )));
        }
    }
    Type::Str
}

fn infer_namespace_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("namespace expects 1 argument".to_string()));
        return Type::union(vec![Type::Str, Type::Nil]);
    }
    let arg_ty = infer_expr(&args[0], env, diags, level);
    let expected = Type::union(vec![Type::Keyword, Type::Symbol]);
    if !is_assignable_with_env(&arg_ty, &expected, env) {
        if is_optional_assignable(&arg_ty, &expected, env) {
            report_optional_usage(&arg_ty, level, diags, "namespace");
        } else {
            diags.push(error_diag(format!(
                "namespace expects keyword or symbol, got {}",
                arg_ty
            )));
        }
    }
    Type::union(vec![Type::Str, Type::Nil])
}

fn infer_conj_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 {
        diags.push(error_diag("conj expects at least 2 arguments".to_string()));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let mut elem_ty = match raw_coll_ty {
        Type::Vec(inner) => {
            require_unique_in_mut("conj", UniqueKind::Vec, &args[0], env, diags);
            *inner
        }
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "conj");
                    require_unique_in_mut("conj", UniqueKind::Vec, &args[0], env, diags);
                    *inner
                } else {
                    diags.push(error_diag(format!("conj expects vector, got {}", other)));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!("conj expects vector, got {}", other)));
                Type::Any
            }
        }
    };
    for arg in &args[1..] {
        let arg_ty = infer_expr(arg, env, diags, level);
        elem_ty = merge_types(elem_ty, arg_ty);
    }
    Type::Vec(Box::new(elem_ty))
}

fn infer_cons_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("cons expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let item_ty = infer_expr(&args[0], env, diags, level);
    let raw_coll_ty = infer_expr(&args[1], env, diags, level);
    let elem_ty = match raw_coll_ty {
        Type::Vec(inner) => {
            require_unique_in_mut("cons", UniqueKind::Vec, &args[1], env, diags);
            *inner
        }
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "cons");
                    require_unique_in_mut("cons", UniqueKind::Vec, &args[1], env, diags);
                    *inner
                } else {
                    diags.push(error_diag(format!("cons expects vector, got {}", other)));
                    Type::Any
                }
            } else {
                diags.push(error_diag(format!("cons expects vector, got {}", other)));
                Type::Any
            }
        }
    };
    let merged = merge_types(elem_ty, item_ty);
    Type::Vec(Box::new(merged))
}

fn infer_list_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return Type::Vec(Box::new(Type::Any));
    }
    let mut acc = infer_expr(&args[0], env, diags, level);
    for arg in &args[1..] {
        let arg_ty = infer_expr(arg, env, diags, level);
        acc = merge_types(acc, arg_ty);
    }
    Type::Vec(Box::new(acc))
}

fn infer_vector_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    infer_list_call(args, env, diags, level)
}

fn infer_hash_map_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    if args.len() % 2 != 0 {
        diags.push(error_diag("hash-map expects even arguments".to_string()));
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    let mut key_ty = infer_expr(&args[0], env, diags, level);
    let mut val_ty = infer_expr(&args[1], env, diags, level);
    let mut fields: BTreeMap<String, Type> = BTreeMap::new();
    let mut literal_keys = true;

    for pair in args.chunks(2) {
        let k = &pair[0];
        let v = &pair[1];
        let next_key = infer_expr(k, env, diags, level);
        let next_val = infer_expr(v, env, diags, level);
        key_ty = merge_types(key_ty, next_key);
        val_ty = merge_types(val_ty, next_val.clone());

        let field_name = match k {
            AstExpr::Keyword(name) => Some(name.clone()),
            AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
            _ => None,
        };
        if let Some(name) = field_name {
            let entry = fields.entry(name).or_insert_with(|| next_val.clone());
            if *entry != next_val {
                *entry = merge_types(entry.clone(), next_val);
            }
        } else {
            literal_keys = false;
        }
    }

    if literal_keys {
        Type::Object(fields)
    } else {
        Type::Map(Box::new(key_ty), Box::new(val_ty))
    }
}

fn infer_vec_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("vec expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    match raw_coll_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Tuple(items) => Type::Vec(Box::new(Type::union(items))),
        Type::Str => Type::Vec(Box::new(Type::Str)),
        Type::Map(key, val) => {
            let pair_elem = merge_types(*key, *val);
            Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))))
        }
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let pair_elem = merge_types(Type::Keyword, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))))
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            let pair_elem = merge_types(Type::Str, value_union);
            Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))))
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name) {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                let pair_elem = merge_types(Type::Str, value_union);
                Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))))
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                Type::Vec(Box::new(Type::Any))
            }
        }
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "vec");
                        return Type::Vec(inner);
                    }
                    Type::Tuple(items) => {
                        report_optional_usage(&other, level, diags, "vec");
                        return Type::Vec(Box::new(Type::union(items)));
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, "vec");
                        return Type::Vec(Box::new(Type::Str));
                    }
                    Type::Map(key, val) => {
                        report_optional_usage(&other, level, diags, "vec");
                        let pair_elem = merge_types(*key, *val);
                        return Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))));
                    }
                    Type::Shape(shape) => {
                        report_optional_usage(&other, level, diags, "vec");
                        let value_union = shape
                            .fields
                            .values()
                            .cloned()
                            .reduce(merge_types)
                            .unwrap_or(Type::Any);
                        let pair_elem = merge_types(Type::Keyword, value_union);
                        return Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))));
                    }
                    Type::Object(fields) => {
                        report_optional_usage(&other, level, diags, "vec");
                        let value_union = fields
                            .values()
                            .cloned()
                            .reduce(merge_types)
                            .unwrap_or(Type::Any);
                        let pair_elem = merge_types(Type::Str, value_union);
                        return Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))));
                    }
                    Type::Named(name) => {
                        report_optional_usage(&other, level, diags, "vec");
                        if let Some(fields) = env.get_type(&name) {
                            let value_union = fields
                                .values()
                                .cloned()
                                .reduce(merge_types)
                                .unwrap_or(Type::Any);
                            let pair_elem = merge_types(Type::Str, value_union);
                            return Type::Vec(Box::new(Type::Vec(Box::new(pair_elem))));
                        }
                    }
                    _ => {}
                }
            }
            diags.push(error_diag(format!("vec expects vector, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_into_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("into expects 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let target_ty = infer_expr(&args[0], env, diags, level);
    let source_ty = infer_expr(&args[1], env, diags, level);
    match target_ty {
        Type::Vec(target_elem) => {
            require_unique_in_mut("into", UniqueKind::Vec, &args[0], env, diags);
            let target_elem = *target_elem;
            let source_elem = match source_ty {
                Type::Vec(inner) => *inner,
                Type::Tuple(items) => Type::union(items),
                other => {
                    if let Some(stripped) = strip_nil(&other) {
                        match stripped {
                            Type::Vec(inner) => {
                                report_optional_usage(&other, level, diags, "into");
                                *inner
                            }
                            Type::Tuple(items) => {
                                report_optional_usage(&other, level, diags, "into");
                                Type::union(items)
                            }
                            _ => {
                                diags.push(error_diag(format!(
                                    "into expects vector source, got {}",
                                    other
                                )));
                                Type::Any
                            }
                        }
                    } else {
                        diags.push(error_diag(format!(
                            "into expects vector source, got {}",
                            other
                        )));
                        Type::Any
                    }
                }
            };
            Type::Vec(Box::new(merge_types(target_elem, source_elem)))
        }
        Type::Map(key, val) => {
            require_unique_in_mut("into", UniqueKind::Map, &args[0], env, diags);
            let empty_target = matches!(&args[0], AstExpr::Map(entries) if entries.is_empty());
            let mut out_key = *key;
            let mut out_val = *val;
            let source_elem = match source_ty {
                Type::Vec(inner) => *inner,
                Type::Tuple(items) => Type::union(items),
                other => {
                    if let Some(stripped) = strip_nil(&other) {
                        match stripped {
                            Type::Vec(inner) => {
                                report_optional_usage(&other, level, diags, "into");
                                *inner
                            }
                            Type::Tuple(items) => {
                                report_optional_usage(&other, level, diags, "into");
                                Type::union(items)
                            }
                            _ => Type::Any,
                        }
                    } else {
                        Type::Any
                    }
                }
            };
            if let Type::Tuple(items) = &source_elem {
                if items.len() == 2 {
                    if empty_target {
                        out_key = items[0].clone();
                        out_val = items[1].clone();
                    } else {
                        out_key = merge_types(out_key, items[0].clone());
                        out_val = merge_types(out_val, items[1].clone());
                    }
                }
            }
            Type::Map(Box::new(out_key), Box::new(out_val))
        }
        Type::Shape(shape) => {
            require_unique_in_mut("into", UniqueKind::Map, &args[0], env, diags);
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            Type::Map(Box::new(Type::Keyword), Box::new(value_union))
        }
        Type::Object(fields) => {
            require_unique_in_mut("into", UniqueKind::Map, &args[0], env, diags);
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            Type::Map(Box::new(Type::Str), Box::new(value_union))
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name) {
                require_unique_in_mut("into", UniqueKind::Map, &args[0], env, diags);
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                Type::Map(Box::new(Type::Str), Box::new(value_union))
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                Type::Any
            }
        }
        other => {
            if let Some(_stripped) = strip_nil(&other) {
                report_optional_usage(&other, level, diags, "into");
            } else {
                diags.push(error_diag(format!(
                    "into expects collection target, got {}",
                    other
                )));
            }
            Type::Any
        }
    }
}

fn infer_assoc_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 3 || (args.len() - 1) % 2 != 0 {
        diags.push(error_diag(
            "assoc expects map and key/value pairs".to_string(),
        ));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let vec_like = match raw_coll_ty.clone() {
        Type::Vec(inner) => Some(Type::Vec(inner)),
        Type::Tuple(items) => Some(Type::Vec(Box::new(Type::union(items)))),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "assoc");
                        Some(Type::Vec(inner))
                    }
                    Type::Tuple(items) => {
                        report_optional_usage(&other, level, diags, "assoc");
                        Some(Type::Vec(Box::new(Type::union(items))))
                    }
                    _ => None,
                }
            } else {
                None
            }
        }
    };
    if let Some(Type::Vec(mut elem_ty)) = vec_like {
        require_unique_in_mut("assoc", UniqueKind::Vec, &args[0], env, diags);
        for pair in args[1..].chunks(2) {
            let key = infer_expr(&pair[0], env, diags, level);
            let value = infer_expr(&pair[1], env, diags, level);
            if !is_assignable_with_env(&key, &Type::Int, env) {
                if is_optional_assignable(&key, &Type::Int, env) {
                    report_optional_usage(&key, level, diags, "assoc");
                } else {
                    diags.push(error_diag(format!(
                        "assoc expects Int index for vector, got {}",
                        key
                    )));
                }
            }
            elem_ty = Box::new(merge_types(*elem_ty, value));
        }
        return Type::Vec(elem_ty);
    }
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "assoc");
    if coll_ty.is_some() {
        require_unique_in_mut("assoc", UniqueKind::Map, &args[0], env, diags);
    }
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            let mut key_ty = *map_key;
            let mut val_ty = *map_val;
            for pair in args[1..].chunks(2) {
                let key = infer_expr(&pair[0], env, diags, level);
                let value = infer_expr(&pair[1], env, diags, level);
                if !is_assignable_with_env(&key, &key_ty, env) {
                    if is_optional_assignable(&key, &key_ty, env) {
                        report_optional_usage(&key, level, diags, "assoc key");
                    } else {
                        diags.push(error_diag(format!(
                            "assoc expects key {}, got {}",
                            key_ty, key
                        )));
                    }
                }
                key_ty = merge_types(key_ty, key);
                val_ty = merge_types(val_ty, value);
            }
            Type::Map(Box::new(key_ty), Box::new(val_ty))
        }
        Some(Type::Shape(shape)) => {
            let mut fields = shape.fields;
            let open = shape.open;
            let mut fallback_to_map = false;
            let mut map_key_ty: Option<Type> = None;
            let mut map_val_ty: Option<Type> = None;
            for pair in args[1..].chunks(2) {
                let key_expr = &pair[0];
                let value_ty = infer_expr(&pair[1], env, diags, level);
                let key_ty = infer_expr(key_expr, env, diags, level);
                if let Some(field) = keyword_field_name(key_expr) {
                    let entry = fields.get(&field).cloned();
                    match entry {
                        Some(existing) => {
                            fields.insert(field, merge_types(existing, value_ty.clone()));
                        }
                        None => {
                            fields.insert(field, value_ty.clone());
                        }
                    }
                } else {
                    fallback_to_map = true;
                }
                if fallback_to_map {
                    map_key_ty = Some(match map_key_ty {
                        Some(prev) => merge_types(prev, key_ty),
                        None => key_ty,
                    });
                    map_val_ty = Some(match map_val_ty {
                        Some(prev) => merge_types(prev, value_ty),
                        None => value_ty,
                    });
                }
            }
            if fallback_to_map {
                let mut merged = map_val_ty.unwrap_or(Type::Nil);
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                let key_union = map_key_ty.unwrap_or(Type::Keyword);
                Type::Map(Box::new(key_union), Box::new(merged))
            } else if open {
                Type::open_shape(fields)
            } else {
                Type::shape(fields)
            }
        }
        Some(Type::Object(mut fields)) => {
            let mut fallback_to_map = false;
            let mut map_val_ty = None;
            for pair in args[1..].chunks(2) {
                let key_expr = &pair[0];
                let value_ty = infer_expr(&pair[1], env, diags, level);
                let field_name = match key_expr {
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                    _ => None,
                };
                let Some(field) = field_name else {
                    fallback_to_map = true;
                    map_val_ty = Some(match map_val_ty {
                        Some(prev) => merge_types(prev, value_ty),
                        None => value_ty,
                    });
                    continue;
                };
                let entry = fields.get(&field);
                match entry {
                    Some(existing) => {
                        fields.insert(field, merge_types(existing.clone(), value_ty));
                    }
                    None => {
                        fallback_to_map = true;
                        map_val_ty = Some(match map_val_ty {
                            Some(prev) => merge_types(prev, value_ty),
                            None => value_ty,
                        });
                    }
                }
            }
            if fallback_to_map {
                let mut merged = map_val_ty.unwrap_or(Type::Nil);
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(
                    Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                    Box::new(merged),
                )
            } else {
                Type::Object(fields)
            }
        }
        Some(Type::Named(name)) => {
            if let Some(base_fields) = env.get_type(&name).cloned() {
                let mut fields = base_fields;
                let mut fallback_to_map = false;
                let mut map_val_ty = None;
                for pair in args[1..].chunks(2) {
                    let key_expr = &pair[0];
                    let value_ty = infer_expr(&pair[1], env, diags, level);
                    let field_name = match key_expr {
                        AstExpr::Keyword(name) => Some(name.clone()),
                        AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                        _ => None,
                    };
                    let Some(field) = field_name else {
                        fallback_to_map = true;
                        map_val_ty = Some(match map_val_ty {
                            Some(prev) => merge_types(prev, value_ty),
                            None => value_ty,
                        });
                        continue;
                    };
                    let entry = fields.get(&field);
                    match entry {
                        Some(existing) => {
                            fields.insert(field, merge_types(existing.clone(), value_ty));
                        }
                        None => {
                            fallback_to_map = true;
                            map_val_ty = Some(match map_val_ty {
                                Some(prev) => merge_types(prev, value_ty),
                                None => value_ty,
                            });
                        }
                    }
                }
                if fallback_to_map {
                    let mut merged = map_val_ty.unwrap_or(Type::Nil);
                    for ty in fields.values() {
                        merged = merge_types(merged, ty.clone());
                    }
                    Type::Map(
                        Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                        Box::new(merged),
                    )
                } else {
                    Type::Object(fields)
                }
            } else {
                diags.push(error_diag(format!("assoc unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("assoc expects map, got {}", other)));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "assoc expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn infer_update_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 3 {
        diags.push(error_diag(
            "update expects map, key, and function".to_string(),
        ));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let key_expr = &args[1];
    let key_ty = infer_expr(key_expr, env, diags, level);
    let fn_expr = &args[2];
    let fn_ty = match fn_expr {
        AstExpr::Symbol(sym) => env
            .get(sym)
            .unwrap_or_else(|| infer_expr(fn_expr, env, diags, level)),
        _ => infer_expr(fn_expr, env, diags, level),
    };
    let mut extra_arg_types = Vec::new();
    for arg in &args[3..] {
        extra_arg_types.push(infer_expr(arg, env, diags, level));
    }
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "update");
    if coll_ty.is_some() {
        require_unique_in_mut("update", UniqueKind::Map, &args[0], env, diags);
    }
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            if !is_assignable_with_env(&key_ty, &map_key, env) {
                if is_optional_assignable(&key_ty, &map_key, env) {
                    report_optional_usage(&key_ty, level, diags, "update key");
                } else {
                    diags.push(error_diag(format!(
                        "update expects key {}, got {}",
                        map_key, key_ty
                    )));
                }
            }
            let current_ty = *map_val;
            let next_ty = apply_update_fn(
                &current_ty,
                &fn_ty,
                &extra_arg_types,
                env,
                diags,
                level,
                "update",
            );
            Type::Map(map_key, Box::new(next_ty))
        }
        Some(Type::Shape(shape)) => {
            let fields = shape.fields;
            let open = shape.open;
            let field = match key_expr {
                AstExpr::Keyword(name) => Some(name.clone()),
                _ => None,
            };
            let Some(field) = field else {
                diags.push(error_diag(
                    "update expects keyword field for shape".to_string(),
                ));
                return if open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                };
            };
            let current_ty = fields.get(&field).cloned().unwrap_or(Type::Nil);
            let next_ty = apply_update_fn(
                &current_ty,
                &fn_ty,
                &extra_arg_types,
                env,
                diags,
                level,
                "update",
            );
            let mut next_fields = fields;
            next_fields.insert(field, merge_types(current_ty, next_ty));
            if open {
                Type::open_shape(next_fields)
            } else {
                Type::shape(next_fields)
            }
        }
        Some(Type::Object(fields)) => {
            let field = match key_expr {
                AstExpr::Keyword(name) => Some(name.clone()),
                AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                _ => None,
            };
            let Some(field) = field else {
                diags.push(error_diag(
                    "update expects literal field for object".to_string(),
                ));
                return Type::Object(fields);
            };
            let current_ty = fields.get(&field).cloned().unwrap_or(Type::Nil);
            let next_ty = apply_update_fn(
                &current_ty,
                &fn_ty,
                &extra_arg_types,
                env,
                diags,
                level,
                "update",
            );
            let mut next_fields = fields;
            if next_fields.contains_key(&field) {
                next_fields.insert(field, merge_types(current_ty, next_ty));
            }
            Type::Object(next_fields)
        }
        Some(Type::Named(name)) => {
            if let Some(fields) = env.get_type(&name).cloned() {
                let field = match key_expr {
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                    _ => None,
                };
                let Some(field) = field else {
                    diags.push(error_diag(
                        "update expects literal field for named type".to_string(),
                    ));
                    return Type::Object(fields);
                };
                let current_ty = fields.get(&field).cloned().unwrap_or(Type::Nil);
                let next_ty = apply_update_fn(
                    &current_ty,
                    &fn_ty,
                    &extra_arg_types,
                    env,
                    diags,
                    level,
                    "update",
                );
                let mut next_fields = fields;
                if next_fields.contains_key(&field) {
                    next_fields.insert(field, merge_types(current_ty, next_ty));
                }
                Type::Object(next_fields)
            } else {
                diags.push(error_diag(format!("update unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("update expects map, got {}", other)));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "update expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn apply_update_fn(
    current_ty: &Type,
    fn_ty: &Type,
    extra_args: &[Type],
    env: &TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
    label: &str,
) -> Type {
    let (params, rest, ret) = match fn_ty {
        Type::Function { params, rest, ret } => (params, rest.as_deref(), ret.as_ref()),
        _ => {
            diags.push(error_diag(format!("{} expects a function", label)));
            return current_ty.clone();
        }
    };
    let arity = 1 + extra_args.len();
    let Some(param_tys) = function_param_types_for_arity(params, rest, arity) else {
        diags.push(error_diag(format!(
            "{} expects function of arity {}",
            label, arity
        )));
        return ret.clone();
    };
    if let Some(first) = param_tys.first() {
        if !is_assignable_with_env(current_ty, first, env) {
            if is_optional_assignable(current_ty, first, env) {
                let msg = format!("{} expects {}, got Nil", label, first);
                match level {
                    NativeLevel::Strict => diags.push(error_diag(msg)),
                    NativeLevel::Warn => diags.push(warn_diag(msg)),
                    NativeLevel::Allow => {}
                }
            } else {
                diags.push(error_diag(format!(
                    "{} expects {}, got {}",
                    label, first, current_ty
                )));
            }
        }
    }
    for (arg, param) in extra_args.iter().zip(param_tys.iter().skip(1)) {
        if !is_assignable_with_env(arg, param, env) {
            if is_optional_assignable(arg, param, env) {
                let msg = format!("{} expects {}, got Nil", label, param);
                match level {
                    NativeLevel::Strict => diags.push(error_diag(msg)),
                    NativeLevel::Warn => diags.push(warn_diag(msg)),
                    NativeLevel::Allow => {}
                }
            } else {
                diags.push(error_diag(format!(
                    "{} expects {}, got {}",
                    label, param, arg
                )));
            }
        }
    }
    ret.clone()
}

fn infer_keys_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("keys expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    let ty = unwrap_optional_map_like(&raw_ty, level, diags, "keys");
    match ty {
        Some(Type::Map(key, _)) => Type::Vec(key),
        Some(Type::Shape(shape)) => {
            let mut key_ty = Type::Nil;
            for _ in shape.fields.keys() {
                key_ty = merge_types(key_ty, Type::Keyword);
            }
            Type::Vec(Box::new(key_ty))
        }
        Some(Type::Object(fields)) => {
            let mut key_ty = Type::Nil;
            for _ in fields.keys() {
                key_ty = merge_types(key_ty, Type::Str);
            }
            Type::Vec(Box::new(key_ty))
        }
        Some(Type::Named(name)) => {
            if let Some(fields) = env.get_type(&name) {
                let mut key_ty = Type::Nil;
                for _ in fields.keys() {
                    key_ty = merge_types(key_ty, Type::Str);
                }
                Type::Vec(Box::new(key_ty))
            } else {
                diags.push(error_diag(format!("keys unknown type: {}", name)));
                Type::Vec(Box::new(Type::Any))
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("keys expects map, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
        None => {
            diags.push(error_diag(format!("keys expects map, got {}", raw_ty)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_vals_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("vals expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    let ty = unwrap_optional_map_like(&raw_ty, level, diags, "vals");
    match ty {
        Some(Type::Map(_, val)) => Type::Vec(val),
        Some(Type::Shape(shape)) => {
            let mut val_ty = Type::Nil;
            for ty in shape.fields.values() {
                val_ty = merge_types(val_ty, ty.clone());
            }
            Type::Vec(Box::new(val_ty))
        }
        Some(Type::Object(fields)) => {
            let mut val_ty = Type::Nil;
            for ty in fields.values() {
                val_ty = merge_types(val_ty, ty.clone());
            }
            Type::Vec(Box::new(val_ty))
        }
        Some(Type::Named(name)) => {
            if let Some(fields) = env.get_type(&name) {
                let mut val_ty = Type::Nil;
                for ty in fields.values() {
                    val_ty = merge_types(val_ty, ty.clone());
                }
                Type::Vec(Box::new(val_ty))
            } else {
                diags.push(error_diag(format!("vals unknown type: {}", name)));
                Type::Vec(Box::new(Type::Any))
            }
        }
        Some(other) => {
            diags.push(error_diag(format!("vals expects map, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
        None => {
            diags.push(error_diag(format!("vals expects map, got {}", raw_ty)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_select_keys_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("select-keys expects map and keys".to_string()));
        return Type::Any;
    }
    let raw_coll_ty = infer_expr(&args[0], env, diags, level);
    let raw_keys_ty = infer_expr(&args[1], env, diags, level);
    let keys_elem_ty = match raw_keys_ty {
        Type::Vec(inner) => *inner,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "select-keys");
                    *inner
                } else {
                    diags.push(error_diag(format!(
                        "select-keys expects vector keys, got {}",
                        other
                    )));
                    return Type::Any;
                }
            } else {
                diags.push(error_diag(format!(
                    "select-keys expects vector keys, got {}",
                    other
                )));
                return Type::Any;
            }
        }
    };
    let coll_ty = unwrap_optional_map_like(&raw_coll_ty, level, diags, "select-keys");
    match coll_ty {
        Some(Type::Map(map_key, map_val)) => {
            if !is_assignable_with_env(&keys_elem_ty, &map_key, env) {
                if is_optional_assignable(&keys_elem_ty, &map_key, env) {
                    report_optional_usage(&keys_elem_ty, level, diags, "select-keys");
                } else {
                    diags.push(error_diag(format!(
                        "select-keys expects key {}, got {}",
                        map_key, keys_elem_ty
                    )));
                }
            }
            Type::Map(map_key, map_val)
        }
        Some(Type::Shape(shape)) => {
            let mut fallback_to_map = false;
            let mut selected = BTreeMap::new();
            if let AstExpr::Vector(keys) = &args[1] {
                for key_expr in keys {
                    if let Some(field) = keyword_field_name(key_expr) {
                        if let Some(value) = shape.fields.get(&field) {
                            selected.insert(field, value.clone());
                        }
                    } else {
                        fallback_to_map = true;
                    }
                }
            } else {
                fallback_to_map = true;
            }
            if fallback_to_map {
                let mut merged = Type::Nil;
                for ty in shape.fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(Box::new(Type::Keyword), Box::new(merged))
            } else {
                Type::shape(selected)
            }
        }
        Some(Type::Object(fields)) => {
            let mut fallback_to_map = false;
            let mut selected = BTreeMap::new();
            if let AstExpr::Vector(keys) = &args[1] {
                for key_expr in keys {
                    let field = match key_expr {
                        AstExpr::Keyword(name) => Some(name.clone()),
                        AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                        _ => None,
                    };
                    if let Some(field) = field {
                        if let Some(value) = fields.get(&field) {
                            selected.insert(field, value.clone());
                        }
                    } else {
                        fallback_to_map = true;
                    }
                }
            } else {
                fallback_to_map = true;
            }
            if fallback_to_map {
                let mut merged = Type::Nil;
                for ty in fields.values() {
                    merged = merge_types(merged, ty.clone());
                }
                Type::Map(
                    Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                    Box::new(merged),
                )
            } else {
                Type::Object(selected)
            }
        }
        Some(Type::Named(name)) => {
            if let Some(fields) = env.get_type(&name).cloned() {
                let mut fallback_to_map = false;
                let mut selected = BTreeMap::new();
                if let AstExpr::Vector(keys) = &args[1] {
                    for key_expr in keys {
                        let field = match key_expr {
                            AstExpr::Keyword(name) => Some(name.clone()),
                            AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
                            _ => None,
                        };
                        if let Some(field) = field {
                            if let Some(value) = fields.get(&field) {
                                selected.insert(field, value.clone());
                            }
                        } else {
                            fallback_to_map = true;
                        }
                    }
                } else {
                    fallback_to_map = true;
                }
                if fallback_to_map {
                    let mut merged = Type::Nil;
                    for ty in fields.values() {
                        merged = merge_types(merged, ty.clone());
                    }
                    Type::Map(
                        Box::new(Type::union(vec![Type::Str, Type::Keyword])),
                        Box::new(merged),
                    )
                } else {
                    Type::Object(selected)
                }
            } else {
                diags.push(error_diag(format!("select-keys unknown type: {}", name)));
                Type::Any
            }
        }
        Some(other) => {
            diags.push(error_diag(format!(
                "select-keys expects map, got {}",
                other
            )));
            Type::Any
        }
        None => {
            diags.push(error_diag(format!(
                "select-keys expects map, got {}",
                raw_coll_ty
            )));
            Type::Any
        }
    }
}

fn infer_frequencies_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("frequencies expects 1 argument".to_string()));
        return Type::Map(Box::new(Type::Any), Box::new(Type::Int));
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => {
            let key_ty = if let Some(stripped) = strip_nil(&inner) {
                report_optional_usage(&inner, level, diags, "frequencies");
                stripped
            } else {
                *inner
            };
            Type::Map(Box::new(key_ty), Box::new(Type::Int))
        }
        Type::Str => Type::Map(Box::new(Type::Str), Box::new(Type::Int)),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "frequencies");
                        return Type::Map(Box::new(*inner), Box::new(Type::Int));
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, "frequencies");
                        return Type::Map(Box::new(Type::Str), Box::new(Type::Int));
                    }
                    _ => {}
                }
            }
            diags.push(error_diag(format!(
                "frequencies expects vector or string, got {}",
                other
            )));
            Type::Map(Box::new(Type::Any), Box::new(Type::Int))
        }
    }
}

fn infer_first_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    infer_seq_head("first", args, env, diags, level)
}

fn infer_last_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    infer_seq_head("last", args, env, diags, level)
}

fn infer_rest_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("rest expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let ty = infer_expr(&args[0], env, diags, level);
    match ty {
        Type::Vec(inner) => Type::Vec(inner),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "rest");
                    return Type::Vec(inner);
                }
            }
            diags.push(error_diag(format!("rest expects vector, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_butlast_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("butlast expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => Type::Vec(inner),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "butlast");
                    return Type::Vec(inner);
                }
            }
            diags.push(error_diag(format!("butlast expects vector, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_next_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("next expects 1 argument".to_string()));
        return Type::Any;
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => Type::union(vec![Type::Vec(inner), Type::Nil]),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "next");
                    return Type::union(vec![Type::Vec(inner), Type::Nil]);
                }
            }
            diags.push(error_diag(format!("next expects vector, got {}", other)));
            Type::Any
        }
    }
}

fn infer_pop_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("pop expects 1 argument".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => Type::Vec(inner),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, "pop");
                    return Type::Vec(inner);
                }
            }
            diags.push(error_diag(format!("pop expects vector, got {}", other)));
            Type::Vec(Box::new(Type::Any))
        }
    }
}

fn infer_empty_value_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("empty expects 1 argument".to_string()));
        return Type::Any;
    }
    let raw_ty = infer_expr(&args[0], env, diags, level);
    match raw_ty {
        Type::Vec(inner) => Type::Vec(inner),
        Type::Map(key, val) => Type::Map(key, val),
        Type::Shape(shape) => {
            let value_union = shape
                .fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            Type::Map(Box::new(Type::Keyword), Box::new(value_union))
        }
        Type::Object(fields) => {
            let value_union = fields
                .values()
                .cloned()
                .reduce(merge_types)
                .unwrap_or(Type::Any);
            Type::Map(Box::new(Type::Str), Box::new(value_union))
        }
        Type::Named(name) => {
            if let Some(fields) = env.get_type(&name) {
                let value_union = fields
                    .values()
                    .cloned()
                    .reduce(merge_types)
                    .unwrap_or(Type::Any);
                Type::Map(Box::new(Type::Str), Box::new(value_union))
            } else {
                diags.push(error_diag(format!("unknown type: {}", name)));
                Type::Any
            }
        }
        Type::Str => Type::Str,
        Type::Nil => Type::Nil,
        other => {
            if let Some(stripped) = strip_nil(&other) {
                match stripped {
                    Type::Vec(inner) => {
                        report_optional_usage(&other, level, diags, "empty");
                        return Type::union(vec![Type::Vec(inner), Type::Nil]);
                    }
                    Type::Map(key, val) => {
                        report_optional_usage(&other, level, diags, "empty");
                        return Type::union(vec![Type::Map(key, val), Type::Nil]);
                    }
                    Type::Shape(shape) => {
                        report_optional_usage(&other, level, diags, "empty");
                        let value_union = shape
                            .fields
                            .values()
                            .cloned()
                            .reduce(merge_types)
                            .unwrap_or(Type::Any);
                        return Type::union(vec![
                            Type::Map(Box::new(Type::Keyword), Box::new(value_union)),
                            Type::Nil,
                        ]);
                    }
                    Type::Object(fields) => {
                        report_optional_usage(&other, level, diags, "empty");
                        let value_union = fields
                            .values()
                            .cloned()
                            .reduce(merge_types)
                            .unwrap_or(Type::Any);
                        return Type::union(vec![
                            Type::Map(Box::new(Type::Str), Box::new(value_union)),
                            Type::Nil,
                        ]);
                    }
                    Type::Named(name) => {
                        report_optional_usage(&other, level, diags, "empty");
                        if let Some(fields) = env.get_type(&name) {
                            let value_union = fields
                                .values()
                                .cloned()
                                .reduce(merge_types)
                                .unwrap_or(Type::Any);
                            return Type::union(vec![
                                Type::Map(Box::new(Type::Str), Box::new(value_union)),
                                Type::Nil,
                            ]);
                        }
                        diags.push(error_diag(format!("unknown type: {}", name)));
                        return Type::Any;
                    }
                    Type::Str => {
                        report_optional_usage(&other, level, diags, "empty");
                        return Type::union(vec![Type::Str, Type::Nil]);
                    }
                    _ => {}
                }
            }
            diags.push(error_diag(format!(
                "empty expects collection, got {}",
                other
            )));
            Type::Any
        }
    }
}

fn infer_repeat_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag("repeat expects 1 or 2 arguments".to_string()));
        return Type::Vec(Box::new(Type::Any));
    }
    let value_arg = if args.len() == 2 {
        let count_ty = infer_expr(&args[0], env, diags, level);
        if !is_assignable_with_env(&count_ty, &Type::Int, env) {
            if is_optional_assignable(&count_ty, &Type::Int, env) {
                report_optional_usage(&count_ty, level, diags, "repeat");
            } else {
                diags.push(error_diag(format!(
                    "repeat expects Int count, got {}",
                    count_ty
                )));
            }
        }
        &args[1]
    } else {
        &args[0]
    };
    let value_ty = infer_expr(value_arg, env, diags, level);
    Type::Vec(Box::new(value_ty))
}

fn infer_repeatedly_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 && args.len() != 2 {
        diags.push(error_diag(
            "repeatedly expects 1 or 2 arguments".to_string(),
        ));
        return Type::Vec(Box::new(Type::Any));
    }
    let fn_arg = if args.len() == 2 {
        let count_ty = infer_expr(&args[0], env, diags, level);
        if !is_assignable_with_env(&count_ty, &Type::Int, env) {
            if is_optional_assignable(&count_ty, &Type::Int, env) {
                report_optional_usage(&count_ty, level, diags, "repeatedly");
            } else {
                diags.push(error_diag(format!(
                    "repeatedly expects Int count, got {}",
                    count_ty
                )));
            }
        }
        &args[1]
    } else {
        &args[0]
    };
    let fn_ty = match fn_arg {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(fn_arg, env, diags, level);
    let fn_ty = match fn_ty {
        Some(ty) => ty,
        None => arg_fn_ty,
    };
    let ret_ty = match fn_ty {
        Type::Function {
            params,
            rest: None,
            ret,
        } if params.is_empty() => *ret,
        _ => {
            diags.push(error_diag("repeatedly expects a function".to_string()));
            return Type::Vec(Box::new(Type::Any));
        }
    };
    Type::Vec(Box::new(ret_ty))
}

fn infer_iterate_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let (fn_arg, init_arg) = match args.len() {
        2 => (&args[0], &args[1]),
        3 => {
            let count_ty = infer_expr(&args[0], env, diags, level);
            if !is_assignable_with_env(&count_ty, &Type::Int, env) {
                if is_optional_assignable(&count_ty, &Type::Int, env) {
                    report_optional_usage(&count_ty, level, diags, "iterate");
                } else {
                    diags.push(error_diag(format!(
                        "iterate expects Int count, got {}",
                        count_ty
                    )));
                }
            }
            (&args[1], &args[2])
        }
        _ => {
            diags.push(error_diag("iterate expects 2 or 3 arguments".to_string()));
            return Type::Vec(Box::new(Type::Any));
        }
    };
    let init_ty = infer_expr(init_arg, env, diags, level);
    if let Some(Type::Function { ret, .. }) = infer_inline_fn_with_expected_params(
        fn_arg,
        &[init_ty.clone()],
        env,
        diags,
        level,
        "iterate",
    ) {
        return Type::Vec(Box::new(merge_types(init_ty, *ret)));
    }
    let fn_ty = match fn_arg {
        AstExpr::Symbol(sym) => env.get(sym),
        _ => None,
    };
    let arg_fn_ty = infer_expr(fn_arg, env, diags, level);
    let fn_ty = fn_ty.unwrap_or(arg_fn_ty);
    let ret_ty = match fn_ty {
        Type::Function { params, rest, ret } if params.len() == 1 && rest.is_none() => {
            if !is_assignable_with_env(&init_ty, &params[0], env) {
                if is_optional_assignable(&init_ty, &params[0], env) {
                    report_optional_usage(&init_ty, level, diags, "iterate");
                } else {
                    diags.push(error_diag(format!(
                        "iterate expects {} for function argument, got {}",
                        params[0], init_ty
                    )));
                }
            }
            *ret
        }
        _ => {
            diags.push(error_diag("iterate expects unary function".to_string()));
            return Type::Vec(Box::new(Type::Any));
        }
    };
    Type::Vec(Box::new(merge_types(init_ty, ret_ty)))
}

fn infer_seq_head(
    name: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag(format!("{} expects 1 argument", name)));
        return Type::Any;
    }
    if is_empty_collection_literal(&args[0]) {
        return Type::Nil;
    }
    let ty = infer_expr(&args[0], env, diags, level);
    match ty {
        Type::Vec(inner) => merge_types(*inner, Type::Nil),
        other => {
            if let Some(stripped) = strip_nil(&other) {
                if let Type::Vec(inner) = stripped {
                    report_optional_usage(&other, level, diags, name);
                    return merge_types(*inner, Type::Nil);
                }
            }
            diags.push(error_diag(format!(
                "{} expects vector, got {}",
                name, other
            )));
            Type::Any
        }
    }
}

fn infer_call_with_signature(
    name: &str,
    args: &[AstExpr],
    params: &[Type],
    rest: Option<&Type>,
    ret: &Type,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if let Some(_) = rest {
        if args.len() < params.len() {
            diags.push(error_diag(format!(
                "{} expects at least {} arguments",
                name,
                params.len()
            )));
            return ret.clone();
        }
    } else if args.len() != params.len() {
        diags.push(error_diag(format!(
            "{} expects {} arguments",
            name,
            params.len()
        )));
        return ret.clone();
    }
    for (arg, param_ty) in args.iter().take(params.len()).zip(params.iter()) {
        let arg_ty = infer_expr(arg, env, diags, level);
        if !is_assignable_with_env(&arg_ty, param_ty, env) {
            if is_optional_assignable(&arg_ty, param_ty, env) {
                report_optional_usage(&arg_ty, level, diags, name);
            } else {
                diags.push(error_diag(format!(
                    "{} expects {}, got {}",
                    name, param_ty, arg_ty
                )));
            }
        }
    }
    if let Some(rest_ty) = rest {
        let rest_elem = match rest_ty {
            Type::Vec(inner) => Some(inner.as_ref()),
            _ => {
                diags.push(error_diag(format!(
                    "{} rest parameter expects Vec<...>, got {}",
                    name, rest_ty
                )));
                None
            }
        };
        if let Some(elem_ty) = rest_elem {
            for arg in args.iter().skip(params.len()) {
                let arg_ty = infer_expr(arg, env, diags, level);
                if !is_assignable_with_env(&arg_ty, elem_ty, env) {
                    if is_optional_assignable(&arg_ty, elem_ty, env) {
                        report_optional_usage(&arg_ty, level, diags, name);
                    } else {
                        diags.push(error_diag(format!(
                            "{} expects {}, got {}",
                            name, elem_ty, arg_ty
                        )));
                    }
                }
            }
        }
    }
    ret.clone()
}

fn infer_json_read_file(args: &[AstExpr], env: &mut TypeEnv, diags: &mut Vec<Diagnostic>) -> Type {
    let Some(path_expr) = args.first() else {
        diags.push(error_diag("json::read-file expects a path".to_string()));
        return Type::Dyn;
    };
    let path = match path_expr {
        AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
        _ => None,
    };
    let mut infer = false;
    let mut schema: Option<String> = None;
    let mut type_name: Option<String> = None;
    let mut idx = 1;
    while idx + 1 < args.len() {
        let key = match &args[idx] {
            AstExpr::Keyword(name) => name.as_str(),
            AstExpr::Symbol(name) => name.as_str(),
            _ => {
                idx += 1;
                continue;
            }
        };
        match key {
            "infer" => {
                if let AstExpr::Literal(crate::ast::Literal::Bool(value)) = &args[idx + 1] {
                    infer = *value;
                }
            }
            "schema" => {
                schema = match &args[idx + 1] {
                    AstExpr::Symbol(name) => Some(name.clone()),
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
                    _ => schema,
                };
            }
            "type" => {
                type_name = match &args[idx + 1] {
                    AstExpr::Symbol(name) => Some(name.clone()),
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
                    _ => type_name,
                };
            }
            _ => {}
        }
        idx += 2;
    }
    if let Some(name) = schema {
        if let Some(fields) = env.get_type(&name) {
            if let Some(path) = &path {
                let content = match std::fs::read_to_string(path) {
                    Ok(content) => content,
                    Err(err) => {
                        diags.push(error_diag(format!("json::read-file failed: {}", err)));
                        return Type::Named(name);
                    }
                };
                let value: serde_json::Value = match serde_json::from_str(&content) {
                    Ok(value) => value,
                    Err(err) => {
                        diags.push(error_diag(format!("json::read-file failed: {}", err)));
                        return Type::Named(name);
                    }
                };
                let schema_ty = Type::Object(fields.clone());
                if !validate_json_value(&value, &schema_ty) {
                    diags.push(error_diag(format!(
                        "json::read-file schema mismatch: {}",
                        name
                    )));
                }
            }
        } else {
            diags.push(error_diag(format!("unknown schema type: {}", name)));
        }
        return Type::Named(name);
    }
    if type_name.is_some() && !infer {
        diags.push(error_diag(
            "json::read-file :type requires :infer true".to_string(),
        ));
    }
    if !infer {
        return Type::Dyn;
    }
    let Some(path) = path else {
        diags.push(error_diag(
            "json::read-file expects a string path for :infer".to_string(),
        ));
        return Type::Dyn;
    };
    let content = match std::fs::read_to_string(&path) {
        Ok(content) => content,
        Err(err) => {
            diags.push(error_diag(format!("json::read-file failed: {}", err)));
            return Type::Dyn;
        }
    };
    let snapshot = snapshot_path(Path::new(&path));
    if let Ok(snapshot) = read_snapshot(&snapshot) {
        let hash = content_hash(&content);
        if snapshot.content_hash.as_deref() == Some(hash.as_str()) {
            return apply_json_type_name(snapshot.ty, type_name, env, diags);
        }
    }
    let value: serde_json::Value = match serde_json::from_str(&content) {
        Ok(value) => value,
        Err(err) => {
            diags.push(error_diag(format!("json::read-file failed: {}", err)));
            return Type::Dyn;
        }
    };
    let inferred = infer_json_schema(&value);
    if let Err(err) = write_snapshot(Path::new(&path), &content, &inferred) {
        diags.push(error_diag(err.to_string()));
    }
    apply_json_type_name(inferred.ty, type_name, env, diags)
}

fn infer_json_read_string(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
) -> Type {
    let Some(input_expr) = args.first() else {
        diags.push(error_diag(
            "json::read-string expects json string".to_string(),
        ));
        return Type::Dyn;
    };
    let input = match input_expr {
        AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
        _ => None,
    };
    let mut infer = false;
    let mut schema: Option<String> = None;
    let mut type_name: Option<String> = None;
    let mut idx = 1;
    while idx + 1 < args.len() {
        let key = match &args[idx] {
            AstExpr::Keyword(name) => name.as_str(),
            AstExpr::Symbol(name) => name.as_str(),
            _ => {
                idx += 1;
                continue;
            }
        };
        match key {
            "infer" => {
                if let AstExpr::Literal(crate::ast::Literal::Bool(value)) = &args[idx + 1] {
                    infer = *value;
                }
            }
            "schema" => {
                schema = match &args[idx + 1] {
                    AstExpr::Symbol(name) => Some(name.clone()),
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
                    _ => schema,
                };
            }
            "type" => {
                type_name = match &args[idx + 1] {
                    AstExpr::Symbol(name) => Some(name.clone()),
                    AstExpr::Keyword(name) => Some(name.clone()),
                    AstExpr::Literal(crate::ast::Literal::Str(value)) => Some(value.clone()),
                    _ => type_name,
                };
            }
            _ => {}
        }
        idx += 2;
    }
    if let Some(name) = schema {
        if let Some(fields) = env.get_type(&name) {
            if let Some(input) = &input {
                let value: serde_json::Value = match serde_json::from_str(input) {
                    Ok(value) => value,
                    Err(err) => {
                        diags.push(error_diag(format!("json::read-string failed: {}", err)));
                        return Type::Named(name);
                    }
                };
                let schema_ty = Type::Object(fields.clone());
                if !validate_json_value(&value, &schema_ty) {
                    diags.push(error_diag(format!(
                        "json::read-string schema mismatch: {}",
                        name
                    )));
                }
            }
        } else {
            diags.push(error_diag(format!("unknown schema type: {}", name)));
        }
        return Type::Named(name);
    }
    if type_name.is_some() && !infer {
        diags.push(error_diag(
            "json::read-string :type requires :infer true".to_string(),
        ));
    }
    if !infer {
        return Type::Dyn;
    }
    let Some(input) = input else {
        diags.push(error_diag(
            "json::read-string expects string literal for :infer".to_string(),
        ));
        return Type::Dyn;
    };
    let value: serde_json::Value = match serde_json::from_str(&input) {
        Ok(value) => value,
        Err(err) => {
            diags.push(error_diag(format!("json::read-string failed: {}", err)));
            return Type::Dyn;
        }
    };
    let inferred = infer_json_schema(&value);
    apply_json_type_name_with("json::read-string", inferred.ty, type_name, env, diags)
}

fn infer_json_write_string(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag(
            "json::write-string expects 1 argument".to_string(),
        ));
        return Type::Str;
    }
    let _ = infer_expr(&args[0], env, diags, level);
    Type::Str
}

fn infer_json_write_file(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag(
            "json::write-file expects 2 arguments".to_string(),
        ));
        return Type::Nil;
    }
    let path_ty = infer_expr(&args[0], env, diags, level);
    if !is_assignable_with_env(&path_ty, &Type::Str, env) {
        if is_optional_assignable(&path_ty, &Type::Str, env) {
            report_optional_usage(&path_ty, level, diags, "json::write-file");
        } else {
            diags.push(error_diag(format!(
                "json::write-file expects Str path, got {}",
                path_ty
            )));
        }
    }
    let _ = infer_expr(&args[1], env, diags, level);
    Type::Nil
}

fn apply_json_type_name(
    inferred_ty: Type,
    type_name: Option<String>,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
) -> Type {
    apply_json_type_name_with("json::read-file", inferred_ty, type_name, env, diags)
}

fn apply_json_type_name_with(
    ctx: &str,
    inferred_ty: Type,
    type_name: Option<String>,
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
) -> Type {
    if let Some(name) = type_name {
        match &inferred_ty {
            Type::Object(fields) => {
                if let Some(existing) = env.get_type(&name) {
                    if existing != fields {
                        diags.push(error_diag(format!(
                            "{} :type {} conflicts with existing type",
                            ctx, name
                        )));
                    }
                } else {
                    env.set_type(&name, fields.clone());
                }
                return Type::Named(name);
            }
            other => {
                diags.push(error_diag(format!(
                    "{} :type {} expects object, got {}",
                    ctx, name, other
                )));
            }
        }
    }
    inferred_ty
}

fn is_numeric_type(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Float | Type::Number)
}

fn is_numeric_like(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Number => true,
        Type::Union(items) => items.iter().all(is_numeric_like),
        _ => false,
    }
}

fn is_numeric_compatible(value: &Type, expected: &Type) -> bool {
    is_numeric_like(value) && is_numeric_like(expected)
}

fn infer_get_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() < 2 || args.len() > 3 {
        diags.push(error_diag("get expects 2 or 3 arguments".to_string()));
        return Type::Any;
    }
    let raw_map_ty = infer_expr(&args[0], env, diags, level);
    let key_ty = infer_expr(&args[1], env, diags, level);
    if let Type::Vec(inner) = raw_map_ty.clone() {
        if !is_assignable_with_env(&key_ty, &Type::Int, env) {
            if is_optional_assignable(&key_ty, &Type::Int, env) {
                report_optional_usage(&key_ty, level, diags, "get index");
            } else {
                diags.push(error_diag(format!("get expects Int index, got {}", key_ty)));
            }
        }
        let value_ty = *inner;
        if args.len() == 3 {
            let default_ty = infer_expr(&args[2], env, diags, level);
            return merge_types(value_ty, default_ty);
        }
        return merge_types(value_ty, Type::Nil);
    }
    if let Some(stripped) = strip_nil(&raw_map_ty) {
        if let Type::Vec(inner) = stripped {
            report_optional_usage(&raw_map_ty, level, diags, "get");
            if !is_assignable_with_env(&key_ty, &Type::Int, env) {
                if is_optional_assignable(&key_ty, &Type::Int, env) {
                    report_optional_usage(&key_ty, level, diags, "get index");
                } else {
                    diags.push(error_diag(format!("get expects Int index, got {}", key_ty)));
                }
            }
            let value_ty = *inner;
            if args.len() == 3 {
                let default_ty = infer_expr(&args[2], env, diags, level);
                return merge_types(value_ty, default_ty);
            }
            return merge_types(value_ty, Type::Nil);
        }
    }
    let map_ty = unwrap_optional_map_like(&raw_map_ty, level, diags, "get");
    let (value_ty, missing_possible) = match map_ty {
        Some(Type::Map(map_key, map_val)) => {
            if !is_assignable_with_env(&key_ty, &map_key, env) {
                if is_optional_assignable(&key_ty, &map_key, env) {
                    report_optional_usage(&key_ty, level, diags, "get key");
                } else {
                    diags.push(error_diag(format!(
                        "get expects key {}, got {}",
                        map_key, key_ty
                    )));
                }
            }
            ((*map_val).clone(), true)
        }
        Some(Type::Shape(shape)) => match &args[1] {
            AstExpr::Keyword(name) => match shape.fields.get(name) {
                Some(value) => (value.clone(), false),
                None => {
                    if shape.open {
                        (Type::Any, true)
                    } else {
                        diags.push(error_diag(format!(
                            "get expects key :{}, got missing",
                            name
                        )));
                        (Type::Any, false)
                    }
                }
            },
            _ => (Type::Any, true),
        },
        Some(Type::Object(fields)) => match &args[1] {
            AstExpr::Keyword(name) => (fields.get(name).cloned().unwrap_or(Type::Nil), false),
            AstExpr::Literal(crate::ast::Literal::Str(name)) => {
                (fields.get(name).cloned().unwrap_or(Type::Nil), false)
            }
            _ => (Type::Any, true),
        },
        Some(Type::Named(name)) => match env.get_type(&name) {
            Some(fields) => match &args[1] {
                AstExpr::Keyword(key) => (fields.get(key).cloned().unwrap_or(Type::Nil), false),
                AstExpr::Literal(crate::ast::Literal::Str(key)) => {
                    (fields.get(key).cloned().unwrap_or(Type::Nil), false)
                }
                _ => (Type::Any, true),
            },
            None => (Type::Any, true),
        },
        Some(Type::Any) | Some(Type::Dyn) | Some(Type::DynOf(_)) => (Type::Any, true),
        Some(other) => {
            diags.push(error_diag(format!("get expects map, got {}", other)));
            (Type::Any, true)
        }
        None => {
            diags.push(error_diag(format!("get expects map, got {}", raw_map_ty)));
            (Type::Any, true)
        }
    };
    if args.len() == 3 {
        let default_ty = infer_expr(&args[2], env, diags, level);
        merge_types(value_ty, default_ty)
    } else {
        if missing_possible {
            merge_types(value_ty, Type::Nil)
        } else {
            value_ty
        }
    }
}

fn infer_keyword_call(
    key: &str,
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() || args.len() > 2 {
        diags.push(error_diag(
            "keyword lookup expects 1 or 2 arguments".to_string(),
        ));
        return Type::Any;
    }
    let mut get_args = Vec::with_capacity(args.len() + 1);
    get_args.push(args[0].clone());
    get_args.push(AstExpr::Keyword(key.to_string()));
    if args.len() == 2 {
        get_args.push(args[1].clone());
    }
    infer_get_call(&get_args, env, diags, level)
}

fn infer_expect_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("expect expects 2 arguments".to_string()));
        return Type::Any;
    }
    let target = match parse_type_expr(&args[0]) {
        Ok(ty) => ty,
        Err(err) => {
            diags.push(error_diag(err.to_string()));
            return Type::Any;
        }
    };
    let value_ty = infer_expr(&args[1], env, diags, level);
    if !is_expectable(&value_ty, &target, env) {
        diags.push(error_diag(format!(
            "{} expects {}, got {}",
            "expect", target, value_ty
        )));
    }
    report_unresolved(&target, level, diags, "expect target".to_string());
    target
}

fn infer_as_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 2 {
        diags.push(error_diag("as expects 2 arguments".to_string()));
        return Type::Any;
    }
    let target = match parse_type_expr(&args[0]) {
        Ok(ty) => ty,
        Err(err) => {
            diags.push(error_diag(err.to_string()));
            return Type::Any;
        }
    };
    let _ = infer_expr(&args[1], env, diags, level);
    report_unresolved(&target, level, diags, "as target".to_string());
    merge_types(target, Type::Nil)
}

fn infer_throw_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.len() != 1 {
        diags.push(error_diag("throw expects 1 argument".to_string()));
        return Type::Any;
    }
    let _ = infer_expr(&args[0], env, diags, level);
    Type::Nil
}

fn infer_try_call(
    args: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if args.is_empty() {
        diags.push(error_diag("try expects body".to_string()));
        return Type::Any;
    }
    env.push();
    let mut idx = 0;
    if let Some(AstExpr::Vector(items)) = args.get(0) {
        let mut i = 0;
        while i < items.len() {
            let (name, consumed) = parse_try_binding_name(items, i, diags);
            i += consumed;
            if i >= items.len() {
                diags.push(error_diag("try binding missing value".to_string()));
                break;
            }
            let value_ty = infer_expr(&items[i], env, diags, level);
            env.set(&name, value_ty);
            i += 1;
        }
        idx = 1;
    }

    let mut body: Vec<&AstExpr> = Vec::new();
    let mut handler_types: Vec<Type> = Vec::new();
    let mut has_explicit_handlers = false;
    for expr in args.iter().skip(idx) {
        if let Some((name, body_exprs)) = parse_try_catch_like(expr) {
            has_explicit_handlers = true;
            let mut handler_env = env.child();
            handler_env.set(&name, Type::Any);
            let handler_refs: Vec<&AstExpr> = body_exprs.iter().collect();
            let handler_ty = infer_body_refs(&handler_refs, &mut handler_env, diags, level);
            handler_types.push(handler_ty);
            continue;
        }
        if let Some(fin_exprs) = parse_try_finally_like(expr) {
            has_explicit_handlers = true;
            let fin_refs: Vec<&AstExpr> = fin_exprs.iter().collect();
            let _ = infer_body_refs(&fin_refs, env, diags, level);
            continue;
        }
        body.push(expr);
    }

    let mut on_error: Option<&AstExpr> = None;
    let mut on_finally: Option<&AstExpr> = None;
    let mut body_refs = body;
    if !has_explicit_handlers
        && body_refs.len() >= 2
        && is_try_handler_expr(body_refs.last().unwrap())
    {
        if is_try_handler_expr(&body_refs[body_refs.len() - 2]) {
            on_finally = body_refs.pop();
            on_error = body_refs.pop();
        } else {
            on_error = body_refs.pop();
        }
    }
    if body_refs.is_empty() {
        diags.push(error_diag("try expects body".to_string()));
        env.pop();
        return Type::Any;
    }
    let body_ty = infer_body_refs(&body_refs, env, diags, level);
    let mut out = body_ty;
    for ty in handler_types {
        out = merge_types(out, ty);
    }
    if let Some(handler) = on_error {
        let handler_ty = infer_expr(handler, env, diags, level);
        if let Type::Function { ret, .. } = handler_ty {
            out = merge_types(out, *ret);
        } else {
            out = merge_types(out, Type::Any);
        }
    }
    if let Some(handler) = on_finally {
        let _ = infer_expr(handler, env, diags, level);
    }
    env.pop();
    out
}

fn parse_try_binding_name(
    items: &[AstExpr],
    idx: usize,
    diags: &mut Vec<Diagnostic>,
) -> (String, usize) {
    let AstExpr::Symbol(name_sym) = &items[idx] else {
        diags.push(error_diag("try binding name must be symbol".to_string()));
        return ("_".to_string(), 1);
    };
    if name_sym.ends_with(':') {
        let trimmed = name_sym.trim_end_matches(':').to_string();
        (trimmed, 2)
    } else {
        (name_sym.clone(), 1)
    }
}

fn parse_try_catch_like(expr: &AstExpr) -> Option<(String, Vec<AstExpr>)> {
    let AstExpr::Call { callee, args } = expr else {
        return None;
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return None;
    };
    match sym.as_str() {
        "catch" => {
            if args.len() < 3 {
                return None;
            }
            let AstExpr::Symbol(name_sym) = &args[1] else {
                return None;
            };
            Some((name_sym.clone(), args.iter().skip(2).cloned().collect()))
        }
        "err" => {
            if args.is_empty() {
                return None;
            }
            let (name, start) = if args.len() == 1 {
                ("?".to_string(), 0)
            } else if let AstExpr::Symbol(name_sym) = &args[0] {
                (name_sym.clone(), 1)
            } else {
                ("?".to_string(), 0)
            };
            Some((name, args.iter().skip(start).cloned().collect()))
        }
        _ => None,
    }
}

fn parse_try_finally_like(expr: &AstExpr) -> Option<Vec<AstExpr>> {
    let AstExpr::Call { callee, args } = expr else {
        return None;
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return None;
    };
    if sym == "finally" || sym == "fin" {
        return Some(args.clone());
    }
    None
}

fn is_try_handler_expr(expr: &AstExpr) -> bool {
    matches!(expr, AstExpr::Fn { .. } | AstExpr::Symbol(_))
}

fn infer_body_refs(
    body: &[&AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    let mut last = Type::Nil;
    for expr in body {
        last = infer_expr(expr, env, diags, level);
    }
    last
}

fn keyword_field_name(expr: &AstExpr) -> Option<String> {
    match expr {
        AstExpr::Keyword(name) => Some(name.clone()),
        _ => None,
    }
}

fn literal_field_name(expr: &AstExpr) -> Option<String> {
    match expr {
        AstExpr::Keyword(name) => Some(name.clone()),
        AstExpr::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
        _ => None,
    }
}

fn keyword_field_name_expr(expr: &Expr) -> Option<String> {
    match &expr.kind {
        ExprKind::Keyword(name) => Some(name.clone()),
        _ => None,
    }
}

fn literal_field_name_expr(expr: &Expr) -> Option<String> {
    match &expr.kind {
        ExprKind::Keyword(name) => Some(name.clone()),
        ExprKind::Literal(crate::ast::Literal::Str(name)) => Some(name.clone()),
        _ => None,
    }
}

fn merge_vector_element(base: &Type, next: &Type) -> Option<Type> {
    if base == next {
        return Some(base.clone());
    }
    if matches!(base, Type::Any) || matches!(next, Type::Any) {
        return Some(Type::Any);
    }
    if matches!(base, Type::Dyn | Type::DynOf(_)) || matches!(next, Type::Dyn | Type::DynOf(_)) {
        return Some(Type::Dyn);
    }
    if is_numeric_type(base) && is_numeric_type(next) {
        return Some(Type::Number);
    }
    if matches!(base, Type::Union(_)) && is_assignable(next, base) {
        return Some(base.clone());
    }
    if matches!(next, Type::Union(_)) && is_assignable(base, next) {
        return Some(next.clone());
    }
    if matches!(base, Type::Nil) || matches!(next, Type::Nil) {
        return Some(Type::union_two(base.clone(), next.clone()));
    }
    None
}

fn infer_vector(
    items: &[AstExpr],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if items.is_empty() {
        return Type::Vec(Box::new(Type::Any));
    }
    let mut item_types = Vec::with_capacity(items.len());
    for item in items {
        item_types.push(infer_expr(item, env, diags, level));
    }
    if item_types.len() == 1 {
        return Type::Vec(Box::new(item_types[0].clone()));
    }
    let mut acc = item_types[0].clone();
    let mut unified = true;
    for item_ty in item_types.iter().skip(1) {
        let next = merge_vector_element(&acc, item_ty);
        match next {
            Some(next_ty) => acc = next_ty,
            None => {
                unified = false;
                break;
            }
        }
    }
    if unified {
        Type::Vec(Box::new(acc))
    } else {
        Type::Tuple(item_types)
    }
}

fn infer_map(
    entries: &[(AstExpr, AstExpr)],
    env: &mut TypeEnv,
    diags: &mut Vec<Diagnostic>,
    level: NativeLevel,
) -> Type {
    if entries.is_empty() {
        return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
    }
    let mut key_ty = infer_expr(&entries[0].0, env, diags, level);
    let mut val_ty = infer_expr(&entries[0].1, env, diags, level);
    let mut fields: BTreeMap<String, Type> = BTreeMap::new();
    let mut all_literal = true;
    let mut all_keyword = true;

    for (k, v) in entries {
        let next_key = infer_expr(k, env, diags, level);
        let next_val = infer_expr(v, env, diags, level);
        key_ty = merge_types(key_ty, next_key);
        val_ty = merge_types(val_ty, next_val.clone());

        if let Some(name) = literal_field_name(k) {
            let entry = fields.entry(name).or_insert_with(|| next_val.clone());
            if *entry != next_val {
                *entry = merge_types(entry.clone(), next_val);
            }
            if keyword_field_name(k).is_none() {
                all_keyword = false;
            }
        } else {
            all_literal = false;
            all_keyword = false;
        }
    }

    if all_keyword {
        Type::shape(fields)
    } else if all_literal {
        Type::Object(fields)
    } else {
        Type::Map(Box::new(key_ty), Box::new(val_ty))
    }
}

fn infer_quote_expr(expr: &Expr) -> Type {
    match &expr.kind {
        ExprKind::Literal(lit) => match lit {
            crate::ast::Literal::Nil => Type::Nil,
            crate::ast::Literal::Bool(_) => Type::Bool,
            crate::ast::Literal::Int(_) => Type::Int,
            crate::ast::Literal::Float(_) => Type::Float,
            crate::ast::Literal::Str(_) | crate::ast::Literal::Regex(_) => Type::Str,
        },
        ExprKind::Symbol(_) => Type::Symbol,
        ExprKind::Keyword(_) => Type::Keyword,
        ExprKind::Vector(items) | ExprKind::List(items) | ExprKind::Set(items) => {
            if items.is_empty() {
                return Type::Vec(Box::new(Type::Any));
            }
            let mut item_types = Vec::with_capacity(items.len());
            for item in items {
                item_types.push(infer_quote_expr(item));
            }
            if item_types.len() == 1 {
                return Type::Vec(Box::new(item_types[0].clone()));
            }
            let mut acc = item_types[0].clone();
            let mut unified = true;
            for item in item_types.iter().skip(1) {
                let next = merge_vector_element(&acc, item);
                match next {
                    Some(next_ty) => acc = next_ty,
                    None => {
                        unified = false;
                        break;
                    }
                }
            }
            if unified {
                Type::Vec(Box::new(acc))
            } else {
                Type::Tuple(item_types)
            }
        }
        ExprKind::Map(entries) => {
            if entries.is_empty() {
                return Type::Map(Box::new(Type::Any), Box::new(Type::Any));
            }
            let mut key_ty = infer_quote_expr(&entries[0].0);
            let mut val_ty = infer_quote_expr(&entries[0].1);
            let mut fields: BTreeMap<String, Type> = BTreeMap::new();
            let mut all_literal = true;
            let mut all_keyword = true;
            for (k, v) in entries {
                key_ty = merge_types(key_ty, infer_quote_expr(k));
                let next_val = infer_quote_expr(v);
                val_ty = merge_types(val_ty, next_val.clone());
                if let Some(name) = literal_field_name_expr(k) {
                    let entry = fields.entry(name).or_insert_with(|| next_val.clone());
                    if *entry != next_val {
                        *entry = merge_types(entry.clone(), next_val);
                    }
                    if keyword_field_name_expr(k).is_none() {
                        all_keyword = false;
                    }
                } else {
                    all_literal = false;
                    all_keyword = false;
                }
            }
            if all_keyword {
                Type::shape(fields)
            } else if all_literal {
                Type::Object(fields)
            } else {
                Type::Map(Box::new(key_ty), Box::new(val_ty))
            }
        }
        ExprKind::ForeignBlock { .. } => Type::Dyn,
    }
}
fn merge_types(a: Type, b: Type) -> Type {
    if a == b {
        return a;
    }
    if matches!(a, Type::Any) || matches!(b, Type::Any) {
        return Type::Any;
    }
    if matches!(a, Type::Dyn | Type::DynOf(_)) || matches!(b, Type::Dyn | Type::DynOf(_)) {
        return Type::Dyn;
    }
    if matches!(a, Type::Int | Type::Float | Type::Number)
        && matches!(b, Type::Int | Type::Float | Type::Number)
    {
        return Type::Number;
    }
    Type::union_two(a, b)
}

fn is_expectable(value: &Type, expected: &Type, env: &TypeEnv) -> bool {
    if contains_any(value) || contains_dyn(value) {
        return true;
    }
    if is_assignable_with_env(value, expected, env) {
        return true;
    }
    match value {
        Type::Union(items) => items
            .iter()
            .any(|item| is_assignable_with_env(item, expected, env)),
        _ => false,
    }
}

fn is_assignable_with_env(value: &Type, expected: &Type, env: &TypeEnv) -> bool {
    if let Type::Map(exp_key, exp_val) = expected {
        match value {
            Type::Object(fields) => {
                if !is_assignable_with_env(&Type::Str, exp_key, env) {
                    return false;
                }
                return fields
                    .values()
                    .all(|val| is_assignable_with_env(val, exp_val, env));
            }
            Type::Shape(shape) => {
                if !is_assignable_with_env(&Type::Keyword, exp_key, env) {
                    return false;
                }
                return shape
                    .fields
                    .values()
                    .all(|val| is_assignable_with_env(val, exp_val, env));
            }
            Type::Named(name) => {
                if let Some(fields) = env.get_type(name) {
                    if !is_assignable_with_env(&Type::Str, exp_key, env) {
                        return false;
                    }
                    return fields
                        .values()
                        .all(|val| is_assignable_with_env(val, exp_val, env));
                }
            }
            _ => {}
        }
    }
    if let Type::Named(name) = expected {
        if let Some(fields) = env.get_type(name) {
            if let Type::Object(obj_fields) = value {
                return object_matches_named(obj_fields, fields, env);
            }
            if let Type::Shape(shape) = value {
                if shape.open {
                    return false;
                }
                return object_matches_named(&shape.fields, fields, env);
            }
        }
    }
    is_assignable(value, expected)
}

fn object_matches_named(
    value_fields: &BTreeMap<String, Type>,
    expected_fields: &BTreeMap<String, Type>,
    env: &TypeEnv,
) -> bool {
    if value_fields.len() != expected_fields.len() {
        return false;
    }
    for (name, expected_ty) in expected_fields {
        let Some(value_ty) = value_fields.get(name) else {
            return false;
        };
        if !is_assignable_with_env(value_ty, expected_ty, env) {
            return false;
        }
    }
    true
}

fn is_assignable(value: &Type, expected: &Type) -> bool {
    if matches!(value, Type::Any | Type::Dyn | Type::DynOf(_)) {
        return true;
    }
    if let Type::Union(items) = value {
        return items.iter().all(|item| is_assignable(item, expected));
    }
    match expected {
        Type::Any | Type::Dyn | Type::DynOf(_) => true,
        Type::Union(items) => items.iter().any(|item| is_assignable(value, item)),
        Type::Number => match value {
            Type::Int | Type::Float | Type::Number => true,
            Type::Union(items) => items
                .iter()
                .all(|item| matches!(item, Type::Int | Type::Float | Type::Number)),
            _ => false,
        },
        Type::Vec(inner) => match value {
            Type::Vec(val_inner) => is_assignable(val_inner, inner),
            Type::Tuple(items) => items.iter().all(|item| is_assignable(item, inner)),
            _ => false,
        },
        Type::Tuple(items) => match value {
            Type::Tuple(val_items) => {
                if items.len() != val_items.len() {
                    return false;
                }
                items
                    .iter()
                    .zip(val_items.iter())
                    .all(|(exp, got)| is_assignable(got, exp))
            }
            _ => false,
        },
        Type::Map(exp_key, exp_val) => match value {
            Type::Map(val_key, val_val) => {
                is_assignable(val_key, exp_key) && is_assignable(val_val, exp_val)
            }
            Type::Object(fields) => {
                if !is_assignable(&Type::Str, exp_key) {
                    return false;
                }
                fields
                    .values()
                    .all(|value_ty| is_assignable(value_ty, exp_val))
            }
            Type::Shape(shape) => {
                if !is_assignable(&Type::Keyword, exp_key) {
                    return false;
                }
                shape
                    .fields
                    .values()
                    .all(|value_ty| is_assignable(value_ty, exp_val))
            }
            _ => false,
        },
        Type::Shape(expected_shape) => match value {
            Type::Shape(value_shape) => {
                if !expected_shape.open {
                    if value_shape.open {
                        return false;
                    }
                    return value_shape.fields == expected_shape.fields;
                }
                if value_shape.open {
                    if value_shape.fields.len() < expected_shape.fields.len() {
                        return false;
                    }
                }
                expected_shape.fields.iter().all(|(key, exp_ty)| {
                    value_shape
                        .fields
                        .get(key)
                        .is_some_and(|val_ty| is_assignable(val_ty, exp_ty))
                })
            }
            _ => false,
        },
        Type::Object(_) => value == expected,
        Type::Function { params, rest, ret } => match value {
            Type::Function {
                params: value_params,
                rest: value_rest,
                ret: value_ret,
            } => {
                if params.len() != value_params.len() {
                    return false;
                }
                if rest.is_some() != value_rest.is_some() {
                    return false;
                }
                for (exp, got) in params.iter().zip(value_params.iter()) {
                    if !is_assignable(got, exp) {
                        return false;
                    }
                }
                if let (Some(exp_rest), Some(val_rest)) = (rest.as_deref(), value_rest.as_deref()) {
                    if !is_assignable(val_rest, exp_rest) {
                        return false;
                    }
                }
                is_assignable(value_ret, ret)
            }
            _ => false,
        },
        Type::Named(name) => matches!(value, Type::Named(value_name) if value_name == name),
        _ => value == expected,
    }
}

fn report_unresolved(ty: &Type, level: NativeLevel, diags: &mut Vec<Diagnostic>, ctx: String) {
    let has_any = contains_any_for_diag(ty);
    let has_dyn = contains_dyn(ty);
    match level {
        NativeLevel::Strict => {
            if has_any {
                diags.push(error_diag(format!("{} contains Any", ctx)));
            }
            if has_dyn {
                diags.push(error_diag(format!("{} contains Dyn", ctx)));
            }
        }
        NativeLevel::Warn => {
            if has_any {
                diags.push(warn_diag(format!("{} contains Any", ctx)));
            }
            if has_dyn {
                diags.push(warn_diag(format!("{} contains Dyn", ctx)));
            }
        }
        NativeLevel::Allow => {}
    }
}

fn contains_any_for_diag(ty: &Type) -> bool {
    match ty {
        Type::Function { ret, .. } => contains_any(ret),
        _ => contains_any(ty),
    }
}

fn should_skip_unresolved_any(expr: &AstExpr, env: &TypeEnv, ty: &Type) -> bool {
    if is_json_read_call(expr) {
        return true;
    }
    if *ty != Type::Any {
        return false;
    }
    is_open_shape_get(expr, env)
}

fn is_json_read_call(expr: &AstExpr) -> bool {
    let AstExpr::Call { callee, .. } = expr else {
        return false;
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return false;
    };
    matches!(sym.as_str(), "json::read-file" | "json::read-string")
}

fn is_open_shape_get(expr: &AstExpr, env: &TypeEnv) -> bool {
    let AstExpr::Call { callee, args } = expr else {
        return false;
    };
    let name = match callee.as_ref() {
        AstExpr::Symbol(name) => name.as_str(),
        AstExpr::Keyword(name) => name.as_str(),
        _ => return false,
    };
    if name != "get" && name != "get-in" {
        return false;
    }
    let Some(target) = args.first() else {
        return false;
    };
    let target_ty = match target {
        AstExpr::Symbol(name) => env.get(name),
        _ => None,
    };
    matches!(target_ty, Some(Type::Shape(shape)) if shape.open)
}

fn ensure_vec_type(ty: Type) -> Type {
    if matches!(ty, Type::Vec(_)) {
        ty
    } else {
        Type::Vec(Box::new(Type::Any))
    }
}

fn normalize_rest_type(
    ty: Option<Type>,
    level: NativeLevel,
    diags: &mut Vec<Diagnostic>,
    ctx: &str,
) -> Type {
    let ty = match ty {
        Some(ty) => ty,
        None => {
            match level {
                NativeLevel::Strict => {
                    diags.push(error_diag(format!("{} requires Vec<...> type", ctx)))
                }
                NativeLevel::Warn => {
                    diags.push(warn_diag(format!("{} requires Vec<...> type", ctx)))
                }
                NativeLevel::Allow => {}
            }
            return Type::Vec(Box::new(Type::Any));
        }
    };
    if matches!(ty, Type::Vec(_)) {
        ty
    } else {
        match level {
            NativeLevel::Strict => {
                diags.push(error_diag(format!("{} expects Vec<...>, got {}", ctx, ty)))
            }
            NativeLevel::Warn => {
                diags.push(warn_diag(format!("{} expects Vec<...>, got {}", ctx, ty)))
            }
            NativeLevel::Allow => {}
        }
        Type::Vec(Box::new(Type::Any))
    }
}

fn report_optional_usage(ty: &Type, level: NativeLevel, diags: &mut Vec<Diagnostic>, ctx: &str) {
    if !contains_nil(ty) {
        return;
    }
    match level {
        NativeLevel::Strict => {
            diags.push(error_diag(format!("{} expects non-optional value", ctx)));
        }
        NativeLevel::Warn => {
            diags.push(warn_diag(format!("{} expects non-optional value", ctx)));
        }
        NativeLevel::Allow => {}
    }
}

fn measurement_type() -> Type {
    measurement_type_with(Type::Any)
}

fn measurement_type_with(result_ty: Type) -> Type {
    let mut fields = BTreeMap::new();
    fields.insert("elapsed-ms".to_string(), Type::Float);
    fields.insert("result".to_string(), result_ty);
    fields.insert("runs".to_string(), Type::Int);
    fields.insert("avg-ms".to_string(), Type::Float);
    Type::Object(fields)
}

pub fn builtin_type_for(name: &str) -> Option<Type> {
    builtin_type(name)
}

fn builtin_type(name: &str) -> Option<Type> {
    let number = Type::Number;
    let bool_ty = Type::Bool;
    let any = Type::Any;
    let vec_any = Type::Vec(Box::new(any.clone()));
    let map_any = Type::Map(Box::new(any.clone()), Box::new(any.clone()));
    let type_name_arg = Type::union(vec![Type::Str, Type::Symbol, Type::Keyword]);
    let regex_cap_item = Type::union(vec![Type::Str, Type::Nil]);
    let regex_cap_vec = Type::Vec(Box::new(regex_cap_item));
    let regex_match = Type::union(vec![Type::Str, regex_cap_vec]);
    let rest_number = Some(Box::new(Type::Vec(Box::new(number.clone()))));
    let rest_any = Some(Box::new(Type::Vec(Box::new(any.clone()))));
    let rest_int = Some(Box::new(Type::Vec(Box::new(Type::Int))));
    let rest_vec_any = Some(Box::new(Type::Vec(Box::new(vec_any.clone()))));
    let rest_map_any = Some(Box::new(Type::Vec(Box::new(map_any.clone()))));
    match name {
        "+" | "-" | "*" | "/" => Some(Type::Function {
            params: vec![number.clone()],
            rest: rest_number,
            ret: Box::new(number),
        }),
        "inc" | "dec" => Some(Type::Function {
            params: vec![number.clone()],
            rest: None,
            ret: Box::new(number),
        }),
        "mod" | "quot" | "rem" => Some(Type::Function {
            params: vec![Type::Int, Type::Int],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "bit-and" | "bit-or" | "bit-xor" => Some(Type::Function {
            params: vec![Type::Int, Type::Int],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Int)))),
            ret: Box::new(Type::Int),
        }),
        "bit-not" => Some(Type::Function {
            params: vec![Type::Int],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "bit-shift-left" | "bit-shift-right" => Some(Type::Function {
            params: vec![Type::Int, Type::Int],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "identity" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "constantly" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Function {
                params: Vec::new(),
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }),
        }),
        "partial" => Some(Type::Function {
            params: vec![Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }),
        }),
        "complement" => Some(Type::Function {
            params: vec![Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: None,
            ret: Box::new(Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(Type::Bool),
            }),
        }),
        "pipe" => Some(Type::Function {
            params: vec![Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }),
        }),
        "runtime-error" => Some(Type::Function {
            params: vec![any.clone()],
            rest: rest_any,
            ret: Box::new(Type::Nil),
        }),
        "comp" => Some(Type::Function {
            params: vec![Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }),
        }),
        "juxt" => Some(Type::Function {
            params: vec![Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Function {
                params: vec![any.clone()],
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(Type::Vec(Box::new(any.clone()))),
            }),
        }),
        "=" | "!=" | "not=" => Some(Type::Function {
            params: vec![any.clone()],
            rest: rest_any,
            ret: Box::new(bool_ty),
        }),
        "<" | ">" | "<=" | ">=" => Some(Type::Function {
            params: vec![number.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(number)))),
            ret: Box::new(bool_ty),
        }),
        "compare" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "not" | "empty?" | "nil?" | "bool?" | "boolean?" | "int?" | "integer?" | "float?"
        | "number?" | "str?" | "string?" | "keyword?" | "symbol?" | "vec?" | "vector?" | "map?"
        | "fn?" | "coll?" | "sequential?" | "some?" | "true?" | "false?" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "instance?" => Some(Type::Function {
            params: vec![type_name_arg.clone(), any.clone()],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "get-in" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone()))],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Any),
        }),
        "dissoc" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(any.clone()),
        }),
        "assoc-in" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone())), any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "update-in" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone())), any.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(any.clone()),
        }),
        "take" | "drop" | "take-last" | "drop-last" => Some(Type::Function {
            params: vec![Type::Int, any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "dorun" => Some(Type::Function {
            params: vec![any.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Nil),
        }),
        "reverse" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "concat" => Some(Type::Function {
            params: vec![any.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(any.clone()),
        }),
        "starts-with?" | "ends-with?" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "trim" | "triml" | "trimr" | "upper-case" | "lower-case" | "reverse-str" => {
            Some(Type::Function {
                params: vec![Type::Str],
                rest: None,
                ret: Box::new(Type::Str),
            })
        }
        "split" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Int)))),
            ret: Box::new(Type::Vec(Box::new(Type::Str))),
        }),
        "join" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(Type::Str))],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "blank?" => Some(Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "replace" | "replace-first" => Some(Type::Function {
            params: vec![Type::Str, Type::Str, Type::Str],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "re-find" | "re-matches" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: None,
            ret: Box::new(Type::union(vec![Type::Nil, regex_match.clone()])),
        }),
        "re-seq" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(regex_match.clone()))),
        }),
        "split-lines" | "lines" => Some(Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(Type::Str))),
        }),
        "index-of" | "last-index-of" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: None,
            ret: Box::new(Type::union(vec![Type::Int, Type::Nil])),
        }),
        "capitalize" | "trim-newline" => Some(Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "subs" => Some(Type::Function {
            params: vec![Type::Str, Type::Int],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Int)))),
            ret: Box::new(Type::Str),
        }),
        "format" => Some(Type::Function {
            params: vec![Type::Str],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Str),
        }),
        "println" | "print" | "prn" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Nil),
        }),
        "pr-str" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Str),
        }),
        "slurp" => Some(Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "spit" => Some(Type::Function {
            params: vec![Type::Str, Type::Str],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "fs::delete" => Some(Type::Function {
            params: vec![Type::Str],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "time" => Some(Type::Function {
            params: vec![Type::Function {
                params: Vec::new(),
                rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                ret: Box::new(any.clone()),
            }],
            rest: None,
            ret: Box::new(measurement_type()),
        }),
        "bench" => Some(Type::Function {
            params: vec![
                Type::Int,
                Type::Function {
                    params: Vec::new(),
                    rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
                    ret: Box::new(any.clone()),
                },
            ],
            rest: None,
            ret: Box::new(measurement_type()),
        }),
        "includes?" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "odd?" | "even?" => Some(Type::Function {
            params: vec![Type::Int],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "zero?" | "pos?" | "neg?" => Some(Type::Function {
            params: vec![number.clone()],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "abs" => Some(Type::Function {
            params: vec![number.clone()],
            rest: None,
            ret: Box::new(number),
        }),
        "min" | "max" => Some(Type::Function {
            params: vec![number.clone()],
            rest: rest_number,
            ret: Box::new(number),
        }),
        "str" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Str),
        }),
        "int" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "float" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Float),
        }),
        "bool" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Bool),
        }),
        "keyword" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Keyword),
        }),
        "symbol" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Symbol),
        }),
        "name" => Some(Type::Function {
            params: vec![Type::union(vec![Type::Keyword, Type::Symbol, Type::Str])],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "namespace" => Some(Type::Function {
            params: vec![Type::union(vec![Type::Keyword, Type::Symbol])],
            rest: None,
            ret: Box::new(Type::union(vec![Type::Str, Type::Nil])),
        }),
        "gensym" => Some(Type::Function {
            params: Vec::new(),
            rest: Some(Box::new(Type::Vec(Box::new(type_name_arg.clone())))),
            ret: Box::new(Type::Symbol),
        }),
        "count" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "rand" => Some(Type::Function {
            params: Vec::new(),
            rest: Some(Box::new(Type::Vec(Box::new(number.clone())))),
            ret: Box::new(Type::Float),
        }),
        "rand-int" => Some(Type::Function {
            params: vec![Type::Int],
            rest: None,
            ret: Box::new(Type::Int),
        }),
        "rand-nth" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "first" | "second" | "last" | "peek" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(any),
        }),
        "rest" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any))),
        }),
        "butlast" | "pop" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "next" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::union(vec![
                Type::Vec(Box::new(any.clone())),
                Type::Nil,
            ])),
        }),
        "empty" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "cons" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "list" | "vector" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "hash-map" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Map(Box::new(any.clone()), Box::new(any.clone()))),
        }),
        "vec" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "into" => Some(Type::Function {
            params: vec![
                Type::Vec(Box::new(any.clone())),
                Type::Vec(Box::new(any.clone())),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "take-while" | "drop-while" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any))),
        }),
        "subvec" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone())), Type::Int],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Int)))),
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "not-empty" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Any),
        }),
        "iterate" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "repeat" => Some(Type::Function {
            params: vec![Type::Int, any.clone()],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "repeatedly" => Some(Type::Function {
            params: vec![
                Type::Int,
                Type::Function {
                    params: Vec::new(),
                    rest: None,
                    ret: Box::new(any.clone()),
                },
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any))),
        }),
        "partition" | "partition-all" => Some(Type::Function {
            params: vec![Type::Int, any.clone()],
            rest: Some(Box::new(Type::Vec(Box::new(any.clone())))),
            ret: Box::new(Type::Vec(Box::new(Type::Vec(Box::new(any))))),
        }),
        "partition-by" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(Type::Vec(Box::new(any.clone()))))),
        }),
        "keep" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                Type::Vec(Box::new(any.clone())),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "keep-indexed" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![Type::Int, any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                Type::Vec(Box::new(any.clone())),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "sort" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "sort-by" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
            ],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "distinct" | "dedupe" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "group-by" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
            ],
            rest: None,
            ret: Box::new(Type::Map(
                Box::new(any.clone()),
                Box::new(Type::Vec(Box::new(any.clone()))),
            )),
        }),
        "zip" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(Type::Vec(Box::new(any.clone()))))),
        }),
        "zip-with" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone(), any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
                any.clone(),
            ],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "zipmap" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(Type::Map(Box::new(any.clone()), Box::new(any.clone()))),
        }),
        "interpose" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "interleave" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "flatten" => Some(Type::Function {
            params: vec![Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(any.clone()))),
        }),
        "every?" | "not-any?" | "not-every?" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(bool_ty.clone()),
                },
                Type::Vec(Box::new(any.clone())),
            ],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "some" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                Type::Vec(Box::new(any.clone())),
            ],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "shuffle" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(any),
        }),
        "frequencies" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Map(Box::new(any.clone()), Box::new(Type::Int))),
        }),
        "select-keys" => Some(Type::Function {
            params: vec![any.clone(), Type::Vec(Box::new(any.clone()))],
            rest: None,
            ret: Box::new(any),
        }),
        "get" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: rest_any,
            ret: Box::new(any.clone()),
        }),
        "nth" => Some(Type::Function {
            params: vec![vec_any.clone(), Type::Int],
            rest: rest_any,
            ret: Box::new(any.clone()),
        }),
        "seq" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::union(vec![vec_any.clone(), Type::Nil])),
        }),
        "map" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                vec_any.clone(),
            ],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "map-indexed" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![Type::Int, any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                vec_any.clone(),
            ],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "mapcat" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(vec_any.clone()),
                },
                vec_any.clone(),
            ],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "filter" | "remove" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: None,
                    ret: Box::new(Type::Bool),
                },
                vec_any.clone(),
            ],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "reduce" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone(), any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                vec_any.clone(),
            ],
            rest: rest_vec_any,
            ret: Box::new(any.clone()),
        }),
        "reduce-kv" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone(), any.clone(), any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                any.clone(),
                map_any.clone(),
            ],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "apply" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone()],
                    rest: rest_any.clone(),
                    ret: Box::new(any.clone()),
                },
                any.clone(),
            ],
            rest: rest_any,
            ret: Box::new(any.clone()),
        }),
        "merge" => Some(Type::Function {
            params: vec![map_any.clone()],
            rest: rest_map_any,
            ret: Box::new(map_any.clone()),
        }),
        "merge-with" => Some(Type::Function {
            params: vec![
                Type::Function {
                    params: vec![any.clone(), any.clone()],
                    rest: None,
                    ret: Box::new(any.clone()),
                },
                map_any.clone(),
            ],
            rest: rest_map_any,
            ret: Box::new(map_any.clone()),
        }),
        "contains?" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(bool_ty),
        }),
        "conj" => Some(Type::Function {
            params: vec![vec_any.clone()],
            rest: rest_any,
            ret: Box::new(vec_any.clone()),
        }),
        "assoc" => Some(Type::Function {
            params: vec![map_any.clone(), any.clone(), any.clone()],
            rest: rest_any,
            ret: Box::new(map_any.clone()),
        }),
        "update" => Some(Type::Function {
            params: vec![
                map_any.clone(),
                any.clone(),
                Type::Function {
                    params: vec![any.clone()],
                    rest: rest_any.clone(),
                    ret: Box::new(any.clone()),
                },
            ],
            rest: rest_any,
            ret: Box::new(map_any.clone()),
        }),
        "keys" => Some(Type::Function {
            params: vec![map_any.clone()],
            rest: None,
            ret: Box::new(Type::Vec(Box::new(Type::union(vec![
                Type::Str,
                Type::Keyword,
                Type::Symbol,
            ])))),
        }),
        "vals" => Some(Type::Function {
            params: vec![map_any.clone()],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "range" => Some(Type::Function {
            params: vec![Type::Int],
            rest: rest_int,
            ret: Box::new(Type::Vec(Box::new(Type::Int))),
        }),
        "__comp-call" => Some(Type::Function {
            params: vec![vec_any.clone(), vec_any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "__juxt-call" => Some(Type::Function {
            params: vec![vec_any.clone(), vec_any.clone()],
            rest: None,
            ret: Box::new(vec_any.clone()),
        }),
        "expect" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(any.clone()),
        }),
        "as" => Some(Type::Function {
            params: vec![any.clone(), any.clone()],
            rest: None,
            ret: Box::new(Type::union(vec![any.clone(), Type::Nil])),
        }),
        "json::read-file" | "json::read-string" => Some(Type::Function {
            params: vec![Type::Str],
            rest: rest_any,
            ret: Box::new(any.clone()),
        }),
        "json::write-string" => Some(Type::Function {
            params: vec![any.clone()],
            rest: None,
            ret: Box::new(Type::Str),
        }),
        "json::write-file" => Some(Type::Function {
            params: vec![Type::Str, any.clone()],
            rest: None,
            ret: Box::new(Type::Nil),
        }),
        "repl" | "debug" | "break" => Some(Type::Function {
            params: Vec::new(),
            rest: rest_any,
            ret: Box::new(Type::Nil),
        }),
        _ => None,
    }
}

fn contains_any(ty: &Type) -> bool {
    match ty {
        Type::Any => true,
        Type::Vec(inner) | Type::DynOf(inner) => contains_any(inner),
        Type::Map(key, val) => contains_any(key) || contains_any(val),
        Type::Object(fields) => fields.values().any(contains_any),
        Type::Shape(shape) => shape.fields.values().any(contains_any),
        Type::Tuple(items) => items.iter().any(contains_any),
        Type::Union(items) => items.iter().any(contains_any),
        Type::Function { params, rest, ret } => {
            params.iter().any(contains_any)
                || rest.as_deref().map_or(false, contains_any)
                || contains_any(ret)
        }
        _ => false,
    }
}

fn contains_dyn(ty: &Type) -> bool {
    match ty {
        Type::Dyn | Type::DynOf(_) => true,
        Type::Vec(inner) => contains_dyn(inner),
        Type::Map(key, val) => contains_dyn(key) || contains_dyn(val),
        Type::Object(fields) => fields.values().any(contains_dyn),
        Type::Shape(shape) => shape.fields.values().any(contains_dyn),
        Type::Tuple(items) => items.iter().any(contains_dyn),
        Type::Union(items) => items.iter().any(contains_dyn),
        Type::Function { params, rest, ret } => {
            params.iter().any(contains_dyn)
                || rest.as_deref().map_or(false, contains_dyn)
                || contains_dyn(ret)
        }
        _ => false,
    }
}

fn contains_nil(ty: &Type) -> bool {
    match ty {
        Type::Nil => true,
        Type::Union(items) => items.iter().any(contains_nil),
        Type::Vec(inner) | Type::DynOf(inner) => contains_nil(inner),
        Type::Map(key, val) => contains_nil(key) || contains_nil(val),
        Type::Object(fields) => fields.values().any(contains_nil),
        Type::Shape(shape) => shape.fields.values().any(contains_nil),
        Type::Tuple(items) => items.iter().any(contains_nil),
        Type::Function { params, rest, ret } => {
            params.iter().any(contains_nil)
                || rest.as_deref().map_or(false, contains_nil)
                || contains_nil(ret)
        }
        _ => false,
    }
}

fn strip_nil(ty: &Type) -> Option<Type> {
    match ty {
        Type::Nil => None,
        Type::Union(items) => {
            let mut out = Vec::new();
            for item in items {
                if !matches!(item, Type::Nil) {
                    out.push(item.clone());
                }
            }
            if out.is_empty() {
                None
            } else {
                Some(Type::union(out))
            }
        }
        other => Some(other.clone()),
    }
}

fn unwrap_optional_map_like(
    ty: &Type,
    level: NativeLevel,
    diags: &mut Vec<Diagnostic>,
    ctx: &str,
) -> Option<Type> {
    if matches!(
        ty,
        Type::Map(_, _)
            | Type::Shape(_)
            | Type::Object(_)
            | Type::Named(_)
            | Type::Any
            | Type::Dyn
            | Type::DynOf(_)
    ) {
        return Some(ty.clone());
    }
    if let Some(stripped) = strip_nil(ty) {
        if matches!(
            stripped,
            Type::Map(_, _) | Type::Shape(_) | Type::Object(_) | Type::Named(_)
        ) {
            report_optional_usage(ty, level, diags, ctx);
            return Some(stripped);
        }
    }
    None
}

fn is_optional_assignable(value: &Type, expected: &Type, env: &TypeEnv) -> bool {
    if !contains_nil(value) || contains_nil(expected) {
        return false;
    }
    let Some(stripped) = strip_nil(value) else {
        return false;
    };
    is_assignable_with_env(&stripped, expected, env)
}

fn parse_type_expr(expr: &AstExpr) -> Result<Type, Clove2Error> {
    match expr {
        AstExpr::Symbol(sym) => Type::parse(sym),
        AstExpr::Keyword(sym) => Type::parse(sym),
        AstExpr::Vector(items) => {
            if items.is_empty() {
                return Err(Clove2Error::new(
                    "vector type expression expects at least 1 element",
                ));
            }
            if items.len() == 1 {
                let inner = parse_type_expr(&items[0])?;
                return Ok(Type::Vec(Box::new(inner)));
            }
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(parse_type_expr(item)?);
            }
            Ok(Type::Tuple(out))
        }
        AstExpr::Map(entries) => {
            if entries.is_empty() {
                return Err(Clove2Error::new(
                    "map type expression expects at least 1 entry",
                ));
            }
            let mut fields: BTreeMap<String, Type> = BTreeMap::new();
            let mut all_keyword = true;
            let mut open = false;
            for (key, value) in entries {
                match key {
                    AstExpr::Keyword(name) => {
                        fields.insert(name.clone(), parse_type_expr(value)?);
                    }
                    AstExpr::Symbol(sym) if sym == ".." || sym == "..." => {
                        if open {
                            return Err(Clove2Error::new("shape open marker is duplicated"));
                        }
                        open = true;
                    }
                    _ => {
                        all_keyword = false;
                        break;
                    }
                }
            }
            if all_keyword {
                return Ok(if open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                });
            }
            if entries.len() != 1 {
                return Err(Clove2Error::new(
                    "map type expression expects 1 entry or keyword shape",
                ));
            }
            let (key, value) = &entries[0];
            let key_ty = parse_type_expr(key)?;
            let value_ty = parse_type_expr(value)?;
            Ok(Type::Map(Box::new(key_ty), Box::new(value_ty)))
        }
        _ => Err(Clove2Error::new("type expression must be symbol")),
    }
}

fn error_diag(message: String) -> Diagnostic {
    Diagnostic {
        level: DiagnosticLevel::Error,
        message,
        span: None,
    }
}

fn warn_diag(message: String) -> Diagnostic {
    Diagnostic {
        level: DiagnosticLevel::Warning,
        message,
        span: None,
    }
}

fn attach_span(diags: &mut [Diagnostic], item: &TopLevel) {
    let span = match item {
        TopLevel::Def { span, .. } => span,
        TopLevel::Defn { span, .. } => span,
        TopLevel::DefType { span, .. } => span,
        TopLevel::DefForeign { span, .. } => span,
        TopLevel::Expr { span, .. } => span,
    };
    for diag in diags {
        if diag.span.is_none() {
            diag.span = Some(span.clone());
        }
    }
}

struct TypeEnv {
    scopes: Vec<HashMap<String, Type>>,
    unique_scopes: Vec<HashMap<String, UniqueKind>>,
    named_types: HashMap<String, BTreeMap<String, Type>>,
    expr_types: Vec<ExprTypeEntry>,
    expr_trace: Option<std::rc::Rc<std::cell::RefCell<Vec<ExprTraceEntry>>>>,
    mut_mode_stack: Vec<MutMode>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            unique_scopes: vec![HashMap::new()],
            named_types: HashMap::new(),
            expr_types: Vec::new(),
            expr_trace: None,
            mut_mode_stack: vec![MutMode::Mut],
        }
    }

    fn child(&self) -> Self {
        let mut env = Self {
            scopes: self.scopes.clone(),
            unique_scopes: self.unique_scopes.clone(),
            named_types: self.named_types.clone(),
            expr_types: self.expr_types.clone(),
            expr_trace: self.expr_trace.clone(),
            mut_mode_stack: self.mut_mode_stack.clone(),
        };
        env.push();
        env
    }

    fn fork(&self) -> Self {
        Self {
            scopes: self.scopes.clone(),
            unique_scopes: self.unique_scopes.clone(),
            named_types: self.named_types.clone(),
            expr_types: self.expr_types.clone(),
            expr_trace: self.expr_trace.clone(),
            mut_mode_stack: self.mut_mode_stack.clone(),
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
        self.unique_scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
        self.unique_scopes.pop();
    }

    fn record_expr_type(&mut self, span: Span, ty: Type) {
        self.expr_types.push(ExprTypeEntry {
            span,
            kind: ExprTraceKind::Expr,
            ty,
        });
    }

    fn enable_expr_trace(&mut self) {
        self.expr_trace = Some(std::rc::Rc::new(std::cell::RefCell::new(Vec::new())));
    }

    fn record_expr_trace(&mut self, kind: ExprTraceKind, ty: Type) {
        let Some(trace) = &self.expr_trace else {
            return;
        };
        trace.borrow_mut().push(ExprTraceEntry { kind, ty });
    }

    fn current_mut_mode(&self) -> MutMode {
        self.mut_mode_stack.last().cloned().unwrap_or(MutMode::Mut)
    }

    fn push_mut_mode(&mut self, mode: MutMode) {
        self.mut_mode_stack.push(mode);
    }

    fn pop_mut_mode(&mut self) {
        if self.mut_mode_stack.len() > 1 {
            self.mut_mode_stack.pop();
        }
    }

    fn expr_trace_snapshot(&self) -> Option<Vec<ExprTraceEntry>> {
        self.expr_trace.as_ref().map(|trace| trace.borrow().clone())
    }

    fn set(&mut self, name: &str, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    fn set_unique(&mut self, name: &str, kind: UniqueKind) {
        if let Some(scope) = self.unique_scopes.last_mut() {
            scope.insert(name.to_string(), kind);
        }
    }

    fn set_type(&mut self, name: &str, fields: BTreeMap<String, Type>) {
        self.named_types.insert(name.to_string(), fields);
    }

    fn get(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn get_unique(&self, name: &str) -> Option<UniqueKind> {
        for scope in self.unique_scopes.iter().rev() {
            if let Some(kind) = scope.get(name) {
                return Some(*kind);
            }
        }
        None
    }

    fn get_global(&self, name: &str) -> Option<Type> {
        self.scopes
            .first()
            .and_then(|scope| scope.get(name))
            .cloned()
    }

    fn get_type(&self, name: &str) -> Option<&BTreeMap<String, Type>> {
        self.named_types.get(name)
    }

    fn snapshot(&self) -> TypeSummary {
        let values = self.scopes.first().cloned().unwrap_or_else(HashMap::new);
        let named_types = self.named_types.clone();
        TypeSummary {
            values,
            named_types,
            expr_types: self.expr_types.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprTraceEntry {
    kind: ExprTraceKind,
    ty: Type,
}
