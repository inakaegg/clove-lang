use std::collections::HashMap as StdHashMap;

use crate::ast::{
    desugar_interpolated_regex, desugar_interpolated_string, Form, FormKind, MapItem, Span,
};
use crate::fn_meta;
use crate::symbols::canonical_symbol_name;
use crate::types::TypeKind;
use im::HashMap;

pub const FAST_INT_ADD_SYM: &str = "__clove.core::fast-add-int";
pub const FAST_FLOAT_ADD_SYM: &str = "__clove.core::fast-add-float";
pub const FAST_INT_INC_SYM: &str = "__clove.core::fast-inc-int";
pub const FAST_FLOAT_INC_SYM: &str = "__clove.core::fast-inc-float";
pub const FAST_INT_DEC_SYM: &str = "__clove.core::fast-dec-int";
pub const FAST_FLOAT_DEC_SYM: &str = "__clove.core::fast-dec-float";

#[derive(Clone, Debug)]
pub struct IRExpr {
    pub kind: IRExprKind,
    pub span: Span,
    pub inferred_type: Option<TypeKind>,
    pub source_form: Option<Form>,
}

#[derive(Clone, Debug)]
pub enum IRExprKind {
    Literal(IRLiteral),
    Symbol(String),
    Call {
        callee: Option<String>,
        args: Vec<IRExpr>,
    },
    Vector(Vec<IRExpr>),
    Set(Vec<IRExpr>),
    Map(Vec<(IRExpr, IRExpr)>),
    AddInt(Box<IRExpr>, Box<IRExpr>),
    AddFloat(Box<IRExpr>, Box<IRExpr>),
    IncInt(Box<IRExpr>),
    IncFloat(Box<IRExpr>),
    DecInt(Box<IRExpr>),
    DecFloat(Box<IRExpr>),
    Let {
        bindings: Vec<IRLetBinding>,
        body: Vec<IRExpr>,
    },
    If {
        cond: Box<IRExpr>,
        then_branch: Vec<IRExpr>,
        else_branch: Vec<IRExpr>,
    },
    Do(Vec<IRExpr>),
    Unknown,
}

#[derive(Clone, Debug)]
pub struct IRLetBinding {
    pub name: Option<String>,
    pub value: IRExpr,
    pub type_hint: Option<TypeKind>,
}

#[derive(Clone, Debug)]
pub enum IRLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Keyword(String),
    Nil,
}

#[derive(Clone, Debug)]
pub struct ParamType {
    pub name: String,
    pub hint: Option<TypeKind>,
}

#[derive(Clone, Debug)]
pub struct FnAnalysis {
    pub ret_type: TypeKind,
    pub body: Vec<IRExpr>,
}

pub fn infer_types_for_forms(forms: &[Form]) -> StdHashMap<Span, TypeKind> {
    let mut env = TypeEnv::new();
    let mut lowered: Vec<IRExpr> = forms.iter().map(lower_form).collect();
    infer_block(&mut lowered, &mut env);
    let mut map = StdHashMap::new();
    for expr in &lowered {
        collect_types(expr, &mut map);
    }
    map
}

pub fn analyze_fn_body(params: &[ParamType], body: &[Form]) -> FnAnalysis {
    let mut env = TypeEnv::new();
    env.push_scope();
    for param in params {
        env.set(
            &param.name,
            param.hint.clone().unwrap_or_else(TypeKind::any),
        );
    }
    let mut lowered: Vec<IRExpr> = body.iter().map(lower_form).collect();
    let ret_type = infer_block(&mut lowered, &mut env);
    FnAnalysis {
        ret_type,
        body: lowered,
    }
}

fn collect_types(expr: &IRExpr, out: &mut StdHashMap<Span, TypeKind>) {
    if let Some(ty) = &expr.inferred_type {
        out.insert(expr.span, ty.clone());
    }
    match &expr.kind {
        IRExprKind::Vector(items) | IRExprKind::Set(items) | IRExprKind::Do(items) => {
            for item in items {
                collect_types(item, out);
            }
        }
        IRExprKind::Map(entries) => {
            for (k, v) in entries {
                collect_types(k, out);
                collect_types(v, out);
            }
        }
        IRExprKind::AddInt(left, right) | IRExprKind::AddFloat(left, right) => {
            collect_types(left, out);
            collect_types(right, out);
        }
        IRExprKind::IncInt(inner)
        | IRExprKind::IncFloat(inner)
        | IRExprKind::DecInt(inner)
        | IRExprKind::DecFloat(inner) => {
            collect_types(inner, out);
        }
        IRExprKind::Call { args, .. } => {
            for arg in args {
                collect_types(arg, out);
            }
        }
        IRExprKind::Let { bindings, body } => {
            for binding in bindings {
                collect_types(&binding.value, out);
            }
            for item in body {
                collect_types(item, out);
            }
        }
        IRExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_types(cond, out);
            for item in then_branch {
                collect_types(item, out);
            }
            for item in else_branch {
                collect_types(item, out);
            }
        }
        IRExprKind::Literal(_) | IRExprKind::Symbol(_) | IRExprKind::Unknown => {}
    }
}

fn lower_form(form: &Form) -> IRExpr {
    if let Some(lowered) = desugar_interpolated_string(form) {
        return lower_form(&lowered);
    }
    if let Some(lowered) = desugar_interpolated_regex(form) {
        return lower_form(&lowered);
    }
    let kind = match &form.kind {
        FormKind::Int(n) => IRExprKind::Literal(IRLiteral::Int(*n)),
        FormKind::Float(n) => IRExprKind::Literal(IRLiteral::Float(*n)),
        FormKind::String(s) => IRExprKind::Literal(IRLiteral::String(s.clone())),
        FormKind::Bool(b) => IRExprKind::Literal(IRLiteral::Bool(*b)),
        FormKind::Keyword(name) => IRExprKind::Literal(IRLiteral::Keyword(name.clone())),
        FormKind::Nil => IRExprKind::Literal(IRLiteral::Nil),
        FormKind::Vector(items) => IRExprKind::Vector(items.iter().map(lower_form).collect()),
        FormKind::Map(entries) => {
            let mut lowered = Vec::new();
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        lowered.push((lower_form(k), lower_form(v)));
                    }
                    MapItem::Spread(_) => {
                        return IRExpr {
                            kind: IRExprKind::Unknown,
                            span: form.span,
                            inferred_type: None,
                            source_form: Some(form.clone()),
                        };
                    }
                }
            }
            IRExprKind::Map(lowered)
        }
        FormKind::List(items) => lower_list(items, form.span),
        FormKind::Symbol(name) => IRExprKind::Symbol(name.clone()),
        FormKind::ShortFn(items) => IRExprKind::Do(items.iter().map(lower_form).collect()),
        FormKind::Set(items) => IRExprKind::Set(items.iter().map(lower_form).collect()),
        _ => IRExprKind::Unknown,
    };
    IRExpr {
        kind,
        span: form.span,
        inferred_type: None,
        source_form: Some(form.clone()),
    }
}

fn lower_list(items: &[Form], span: Span) -> IRExprKind {
    if items.is_empty() {
        return IRExprKind::Unknown;
    }
    let head_symbol = match &items[0].kind {
        FormKind::Symbol(s) => s.as_str(),
        _ => "",
    };
    match head_symbol {
        "do" => IRExprKind::Do(items.iter().skip(1).map(lower_form).collect()),
        "let" => {
            if items.len() < 3 {
                return IRExprKind::Unknown;
            }
            let bindings = match &items[1].kind {
                FormKind::Vector(vec) => lower_let_bindings(vec),
                _ => Vec::new(),
            };
            let body = items.iter().skip(2).map(lower_form).collect();
            IRExprKind::Let { bindings, body }
        }
        "if" => {
            if items.len() < 3 {
                return IRExprKind::Unknown;
            }
            let cond = Box::new(lower_form(&items[1]));
            let then_branch = vec![lower_form(&items[2])];
            let else_branch = if items.len() > 3 {
                vec![lower_form(&items[3])]
            } else {
                vec![IRExpr {
                    kind: IRExprKind::Literal(IRLiteral::Nil),
                    span,
                    inferred_type: None,
                    source_form: None,
                }]
            };
            IRExprKind::If {
                cond,
                then_branch,
                else_branch,
            }
        }
        _ => {
            let callee = match &items[0].kind {
                FormKind::Symbol(s) => Some(s.clone()),
                _ => None,
            };
            let args = items.iter().skip(1).map(lower_form).collect();
            IRExprKind::Call { callee, args }
        }
    }
}

fn lower_let_bindings(items: &[Form]) -> Vec<IRLetBinding> {
    let mut bindings = Vec::new();
    let mut idx = 0;
    while idx + 1 < items.len() {
        let name = match &items[idx].kind {
            FormKind::Symbol(s) => Some(s.clone()),
            _ => None,
        };
        let type_hint = items[idx].type_hint.as_ref().map(|h| h.kind.clone());
        let value = lower_form(&items[idx + 1]);
        bindings.push(IRLetBinding {
            name,
            value,
            type_hint,
        });
        idx += 2;
    }
    bindings
}

fn infer_block(exprs: &mut [IRExpr], env: &mut TypeEnv) -> TypeKind {
    let mut last = TypeKind::Nil;
    for expr in exprs {
        last = infer_expr(expr, env);
    }
    last
}

fn infer_expr(expr: &mut IRExpr, env: &mut TypeEnv) -> TypeKind {
    let result = match &mut expr.kind {
        IRExprKind::Literal(lit) => match lit {
            IRLiteral::Int(_) => TypeKind::Int,
            IRLiteral::Float(_) => TypeKind::Float,
            IRLiteral::Bool(_) => TypeKind::Bool,
            IRLiteral::String(_) => TypeKind::Str,
            IRLiteral::Keyword(_) => TypeKind::named("clove::core::Symbol"),
            IRLiteral::Nil => TypeKind::Nil,
        },
        IRExprKind::Symbol(name) => env.get(name).cloned().unwrap_or_else(TypeKind::any),
        IRExprKind::Vector(items) => {
            let mut element_type: Option<TypeKind> = None;
            let mut element_types = Vec::new();
            for item in items {
                let ty = infer_expr(item, env);
                if let Some(current) = &element_type {
                    if current != &ty {
                        element_type = None;
                    }
                } else {
                    element_type = Some(ty.clone());
                }
                element_types.push(ty);
            }
            match element_types.len() {
                0 => TypeKind::vector(TypeKind::any()),
                1 => TypeKind::vector(element_types.remove(0)),
                _ => match element_type {
                    Some(ty) => TypeKind::vector(ty),
                    None => TypeKind::Tuple(element_types),
                },
            }
        }
        IRExprKind::Set(items) => {
            for item in items {
                infer_expr(item, env);
            }
            TypeKind::named("clove::core::Set")
        }
        IRExprKind::Map(entries) => {
            let mut record = HashMap::new();
            let mut all_keywords = true;
            for (k, v) in entries.iter_mut() {
                let key_name = match &k.kind {
                    IRExprKind::Symbol(sym) => Some(sym.trim_start_matches(':').to_string()),
                    IRExprKind::Literal(IRLiteral::String(sym)) => Some(sym.clone()),
                    IRExprKind::Literal(IRLiteral::Keyword(sym)) => Some(sym.clone()),
                    _ => None,
                };
                if let Some(name) = key_name {
                    let value_ty = infer_expr(v, env);
                    record.insert(name, value_ty);
                } else {
                    all_keywords = false;
                }
            }
            if all_keywords && !record.is_empty() {
                TypeKind::Record(record)
            } else {
                TypeKind::map(TypeKind::Any, TypeKind::Any)
            }
        }
        IRExprKind::Call { .. } => {
            let call = std::mem::replace(&mut expr.kind, IRExprKind::Unknown);
            if let IRExprKind::Call { callee, mut args } = call {
                if let Some(match_type) = infer_match_call(callee.as_deref(), &mut args, env) {
                    expr.kind = IRExprKind::Call {
                        callee: callee.clone(),
                        args,
                    };
                    match_type
                } else {
                    let arg_types: Vec<TypeKind> =
                        args.iter_mut().map(|a| infer_expr(a, env)).collect();
                    let specialized = match (callee.as_deref(), arg_types.as_slice()) {
                        (Some("+"), [TypeKind::Int, TypeKind::Int]) if args.len() == 2 => {
                            let mut taken = args;
                            let right = taken.pop().unwrap();
                            let left = taken.pop().unwrap();
                            expr.kind = IRExprKind::AddInt(Box::new(left), Box::new(right));
                            Some(TypeKind::Int)
                        }
                        (Some("+"), [TypeKind::Float, TypeKind::Float]) if args.len() == 2 => {
                            let mut taken = args;
                            let right = taken.pop().unwrap();
                            let left = taken.pop().unwrap();
                            expr.kind = IRExprKind::AddFloat(Box::new(left), Box::new(right));
                            Some(TypeKind::Float)
                        }
                        (Some("inc"), [TypeKind::Int]) if args.len() == 1 => {
                            let inner = args.pop().unwrap();
                            expr.kind = IRExprKind::IncInt(Box::new(inner));
                            Some(TypeKind::Int)
                        }
                        (Some("inc"), [TypeKind::Float]) if args.len() == 1 => {
                            let inner = args.pop().unwrap();
                            expr.kind = IRExprKind::IncFloat(Box::new(inner));
                            Some(TypeKind::Float)
                        }
                        (Some("dec"), [TypeKind::Int]) if args.len() == 1 => {
                            let inner = args.pop().unwrap();
                            expr.kind = IRExprKind::DecInt(Box::new(inner));
                            Some(TypeKind::Int)
                        }
                        (Some("dec"), [TypeKind::Float]) if args.len() == 1 => {
                            let inner = args.pop().unwrap();
                            expr.kind = IRExprKind::DecFloat(Box::new(inner));
                            Some(TypeKind::Float)
                        }
                        _ => {
                            expr.kind = IRExprKind::Call {
                                callee: callee.clone(),
                                args,
                            };
                            None
                        }
                    };
                    specialized.unwrap_or_else(|| {
                        callee
                            .as_deref()
                            .and_then(|name| resolve_call_type(name, &arg_types))
                            .unwrap_or_else(TypeKind::any)
                    })
                }
            } else {
                TypeKind::any()
            }
        }
        IRExprKind::AddInt(left, right) => {
            infer_expr(left, env);
            infer_expr(right, env);
            TypeKind::Int
        }
        IRExprKind::AddFloat(left, right) => {
            infer_expr(left, env);
            infer_expr(right, env);
            TypeKind::Float
        }
        IRExprKind::IncInt(inner) => {
            infer_expr(inner, env);
            TypeKind::Int
        }
        IRExprKind::IncFloat(inner) => {
            infer_expr(inner, env);
            TypeKind::Float
        }
        IRExprKind::DecInt(inner) => {
            infer_expr(inner, env);
            TypeKind::Int
        }
        IRExprKind::DecFloat(inner) => {
            infer_expr(inner, env);
            TypeKind::Float
        }
        IRExprKind::Let { bindings, body } => {
            env.push_scope();
            for binding in bindings {
                let value_ty = infer_expr(&mut binding.value, env);
                let bind_type = binding
                    .type_hint
                    .clone()
                    .unwrap_or_else(|| value_ty.clone());
                if let Some(name) = &binding.name {
                    env.set(name, bind_type);
                }
            }
            let body_ty = infer_block(body, env);
            env.pop_scope();
            body_ty
        }
        IRExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            infer_expr(cond, env);
            let (mut then_env, mut else_env) = narrow_env_for_if(cond, env);
            let then_ty = infer_block(then_branch, &mut then_env);
            let else_ty = infer_block(else_branch, &mut else_env);
            if then_ty == else_ty {
                then_ty
            } else {
                TypeKind::union(vec![then_ty, else_ty])
            }
        }
        IRExprKind::Do(items) => infer_block(items, env),
        IRExprKind::Unknown => match expr.source_form.as_ref().map(|f| &f.kind) {
            Some(
                FormKind::ForeignRaw { .. }
                | FormKind::ForeignBlock { .. }
                | FormKind::ForeignSymbol { .. },
            ) => TypeKind::named("clove::core::Foreign"),
            _ => TypeKind::any(),
        },
    };
    let mut result = result;
    if let Some(hint) = expr
        .source_form
        .as_ref()
        .and_then(|form| form.type_hint.as_ref())
    {
        result = hint.kind.clone();
    }
    expr.inferred_type = Some(result.clone());
    result
}

fn narrow_env_for_if(cond: &IRExpr, env: &TypeEnv) -> (TypeEnv, TypeEnv) {
    let mut then_env = env.clone();
    let mut else_env = env.clone();
    if let Some((name, then_ty, else_ty)) = infer_cond_narrowing(cond, env) {
        then_env.set(&name, then_ty);
        else_env.set(&name, else_ty);
    }
    (then_env, else_env)
}

fn infer_cond_narrowing(cond: &IRExpr, env: &TypeEnv) -> Option<(String, TypeKind, TypeKind)> {
    let IRExprKind::Call { callee, args } = &cond.kind else {
        return None;
    };
    let callee = callee.as_deref()?;
    if let Some(pred_ty) = predicate_type_for(callee) {
        if args.len() == 1 {
            if let IRExprKind::Symbol(name) = &args[0].kind {
                let current = env.get(name).cloned().unwrap_or_else(TypeKind::any);
                let else_ty = remove_type_from(&current, &pred_ty);
                return Some((name.clone(), pred_ty, else_ty));
            }
        }
    }
    infer_literal_equality_narrowing(callee, args, env)
}

fn predicate_type_for(name: &str) -> Option<TypeKind> {
    match name {
        "keyword?" | "symbol?" => Some(TypeKind::named("clove::core::Symbol")),
        "nil?" => Some(TypeKind::Nil),
        "int?" | "integer?" => Some(TypeKind::Int),
        "float?" => Some(TypeKind::Float),
        "bool?" | "boolean?" => Some(TypeKind::Bool),
        "string?" | "str?" => Some(TypeKind::Str),
        "vec?" | "vector?" => Some(TypeKind::vector(TypeKind::Any)),
        "map?" => Some(TypeKind::map(TypeKind::Any, TypeKind::Any)),
        "list?" => Some(TypeKind::named("clove::core::List")),
        "set?" => Some(TypeKind::named("clove::core::Set")),
        _ => None,
    }
}

fn infer_literal_equality_narrowing(
    callee: &str,
    args: &[IRExpr],
    env: &TypeEnv,
) -> Option<(String, TypeKind, TypeKind)> {
    let is_eq = matches!(callee, "=" | "==" | "===");
    let is_neq = matches!(callee, "!=" | "not=" | "!==");
    if !is_eq && !is_neq {
        return None;
    }
    if args.len() != 2 {
        return None;
    }
    let (name, lit_ty) = match (&args[0].kind, &args[1].kind) {
        (IRExprKind::Symbol(name), IRExprKind::Literal(lit)) => (name, literal_type(lit)?),
        (IRExprKind::Literal(lit), IRExprKind::Symbol(name)) => (name, literal_type(lit)?),
        _ => return None,
    };
    let current = env.get(name).cloned().unwrap_or_else(TypeKind::any);
    if is_eq {
        let else_ty = remove_type_from(&current, &lit_ty);
        Some((name.clone(), lit_ty, else_ty))
    } else {
        let then_ty = remove_type_from(&current, &lit_ty);
        Some((name.clone(), then_ty, lit_ty))
    }
}

fn literal_type(lit: &IRLiteral) -> Option<TypeKind> {
    match lit {
        IRLiteral::Nil => Some(TypeKind::Nil),
        IRLiteral::Keyword(_) => Some(TypeKind::named("clove::core::Symbol")),
        _ => None,
    }
}

fn remove_type_from(current: &TypeKind, target: &TypeKind) -> TypeKind {
    let remove_set = match target {
        TypeKind::Union(types) => types.clone(),
        other => vec![other.clone()],
    };
    match current {
        TypeKind::Union(types) => {
            let remaining: Vec<_> = types
                .iter()
                .filter(|ty| !remove_set.contains(ty))
                .cloned()
                .collect();
            if remaining.is_empty() {
                TypeKind::any()
            } else {
                TypeKind::union(remaining)
            }
        }
        _ if remove_set.contains(current) => TypeKind::any(),
        _ => current.clone(),
    }
}

fn infer_match_call(
    callee: Option<&str>,
    args: &mut [IRExpr],
    env: &mut TypeEnv,
) -> Option<TypeKind> {
    if callee != Some("match") {
        return None;
    }
    if args.len() < 3 || (args.len() - 1) % 2 != 0 {
        return None;
    }
    let target_sym = match &args[0].kind {
        IRExprKind::Call {
            callee,
            args: inner,
        } if callee.as_deref() == Some("type") => {
            if inner.len() != 1 {
                return None;
            }
            if let IRExprKind::Symbol(name) = &inner[0].kind {
                name.clone()
            } else {
                return None;
            }
        }
        _ => return None,
    };
    infer_expr(&mut args[0], env);
    let mut out_ty: Option<TypeKind> = None;
    let mut idx = 1;
    while idx + 1 < args.len() {
        let (left, right) = args.split_at_mut(idx + 1);
        let pattern = &left[idx];
        let value = &mut right[0];
        let mut branch_env = env.clone();
        if let Some(pat_ty) = pattern_type_hint(pattern) {
            branch_env.set(&target_sym, pat_ty);
        }
        let branch_ty = infer_expr(value, &mut branch_env);
        out_ty = Some(match out_ty {
            Some(current) => TypeKind::union(vec![current, branch_ty]),
            None => branch_ty,
        });
        idx += 2;
    }
    let result = out_ty.unwrap_or_else(TypeKind::any);
    Some(result)
}

fn pattern_type_hint(pattern: &IRExpr) -> Option<TypeKind> {
    match &pattern.kind {
        IRExprKind::Symbol(sym) => {
            if sym == "_" {
                return None;
            }
            TypeKind::parse(sym).ok()
        }
        _ => None,
    }
}

fn resolve_call_type(name: &str, arg_types: &[TypeKind]) -> Option<TypeKind> {
    match name {
        FAST_INT_ADD_SYM => return Some(TypeKind::Int),
        FAST_FLOAT_ADD_SYM => return Some(TypeKind::Float),
        FAST_INT_INC_SYM => return Some(TypeKind::Int),
        FAST_FLOAT_INC_SYM => return Some(TypeKind::Float),
        FAST_INT_DEC_SYM => return Some(TypeKind::Int),
        FAST_FLOAT_DEC_SYM => return Some(TypeKind::Float),
        "range" => return Some(infer_range_return(arg_types)),
        "map" => return Some(infer_map_return(arg_types)),
        "reduce" => return Some(infer_reduce_return(arg_types)),
        "count" => return Some(TypeKind::Int),
        _ => {}
    }
    let meta = fn_meta::get(name)?;
    for overload in &meta.overloads {
        if arg_types.len() < overload.arg_types.len() {
            continue;
        }
        let mut matches = true;
        for (expected, actual) in overload.arg_types.iter().zip(arg_types) {
            if expected != actual && !matches_any(expected, actual) {
                matches = false;
                break;
            }
        }
        if matches && arg_types.len() > overload.arg_types.len() {
            let Some(rest) = overload.rest.as_ref() else {
                continue;
            };
            let rest_elem = match rest {
                TypeKind::Vector(inner) => inner.as_ref(),
                other => other,
            };
            for actual in arg_types.iter().skip(overload.arg_types.len()) {
                if rest_elem != actual && !matches_any(rest_elem, actual) {
                    matches = false;
                    break;
                }
            }
        }
        if matches {
            return Some(overload.ret_type.clone());
        }
    }
    None
}

fn matches_any(expected: &TypeKind, actual: &TypeKind) -> bool {
    if matches!(expected, TypeKind::Any) {
        return true;
    }
    if expected == actual {
        return true;
    }
    match (expected, actual) {
        (TypeKind::Union(expected_types), TypeKind::Union(actual_types)) => {
            actual_types.iter().all(|ty| expected_types.contains(ty))
        }
        (TypeKind::Union(types), other) => types.iter().any(|ty| ty == other),
        (expected, TypeKind::Union(types)) => types.iter().all(|ty| ty == expected),
        _ => false,
    }
}

fn infer_range_return(arg_types: &[TypeKind]) -> TypeKind {
    let ints = arg_types.iter().all(|ty| matches!(ty, TypeKind::Int));
    if ints {
        TypeKind::vector(TypeKind::Int)
    } else {
        TypeKind::vector(TypeKind::any())
    }
}

fn infer_map_return(arg_types: &[TypeKind]) -> TypeKind {
    if arg_types.len() >= 2 {
        if let TypeKind::Vector(inner) = &arg_types[1] {
            return TypeKind::vector((**inner).clone());
        }
    }
    TypeKind::vector(TypeKind::any())
}

fn infer_reduce_return(arg_types: &[TypeKind]) -> TypeKind {
    if arg_types.len() >= 3 {
        return arg_types[1].clone();
    }
    if let Some(TypeKind::Vector(inner)) = arg_types.get(1) {
        return (**inner).clone();
    }
    TypeKind::any()
}

pub fn ir_exprs_to_forms(exprs: &[IRExpr]) -> Option<Vec<Form>> {
    exprs
        .iter()
        .map(ir_expr_to_form)
        .collect::<Option<Vec<_>>>()
}

fn ir_expr_to_form(expr: &IRExpr) -> Option<Form> {
    let span = expr.span;
    let kind = match &expr.kind {
        IRExprKind::Literal(IRLiteral::Int(n)) => FormKind::Int(*n),
        IRExprKind::Literal(IRLiteral::Float(n)) => FormKind::Float(*n),
        IRExprKind::Literal(IRLiteral::Bool(b)) => FormKind::Bool(*b),
        IRExprKind::Literal(IRLiteral::String(s)) => FormKind::String(s.clone()),
        IRExprKind::Literal(IRLiteral::Keyword(s)) => FormKind::Keyword(s.clone()),
        IRExprKind::Literal(IRLiteral::Nil) => FormKind::Nil,
        IRExprKind::Symbol(name) => FormKind::Symbol(name.clone()),
        IRExprKind::Vector(items) => FormKind::Vector(ir_exprs_to_forms(items)?),
        IRExprKind::Set(items) => FormKind::Set(ir_exprs_to_forms(items)?),
        IRExprKind::Map(entries) => {
            let mut converted = Vec::new();
            for (k, v) in entries {
                converted.push(MapItem::KeyValue(ir_expr_to_form(k)?, ir_expr_to_form(v)?));
            }
            FormKind::Map(converted)
        }
        IRExprKind::AddInt(left, right) => {
            let head = Form::new(FormKind::Symbol(FAST_INT_ADD_SYM.into()), span);
            let l = ir_expr_to_form(left)?;
            let r = ir_expr_to_form(right)?;
            FormKind::List(vec![head, l, r])
        }
        IRExprKind::AddFloat(left, right) => {
            let head = Form::new(FormKind::Symbol(FAST_FLOAT_ADD_SYM.into()), span);
            let l = ir_expr_to_form(left)?;
            let r = ir_expr_to_form(right)?;
            FormKind::List(vec![head, l, r])
        }
        IRExprKind::IncInt(inner) => {
            let head = Form::new(FormKind::Symbol(FAST_INT_INC_SYM.into()), span);
            let val = ir_expr_to_form(inner)?;
            FormKind::List(vec![head, val])
        }
        IRExprKind::IncFloat(inner) => {
            let head = Form::new(FormKind::Symbol(FAST_FLOAT_INC_SYM.into()), span);
            let val = ir_expr_to_form(inner)?;
            FormKind::List(vec![head, val])
        }
        IRExprKind::DecInt(inner) => {
            let head = Form::new(FormKind::Symbol(FAST_INT_DEC_SYM.into()), span);
            let val = ir_expr_to_form(inner)?;
            FormKind::List(vec![head, val])
        }
        IRExprKind::DecFloat(inner) => {
            let head = Form::new(FormKind::Symbol(FAST_FLOAT_DEC_SYM.into()), span);
            let val = ir_expr_to_form(inner)?;
            FormKind::List(vec![head, val])
        }
        IRExprKind::Let { bindings, body } => {
            let mut parts = Vec::new();
            parts.push(Form::new(FormKind::Symbol("let".into()), span));
            let mut bind_forms = Vec::new();
            for binding in bindings {
                let name_form = match &binding.name {
                    Some(name) => Form::new(FormKind::Symbol(name.clone()), binding.value.span),
                    None => return None,
                };
                bind_forms.push(name_form);
                bind_forms.push(ir_expr_to_form(&binding.value)?);
            }
            parts.push(Form::new(FormKind::Vector(bind_forms), span));
            for expr in body {
                parts.push(ir_expr_to_form(expr)?);
            }
            FormKind::List(parts)
        }
        IRExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let mut parts = Vec::new();
            parts.push(Form::new(FormKind::Symbol("if".into()), span));
            parts.push(ir_expr_to_form(cond)?);
            if let Some(first) = then_branch.first() {
                parts.push(ir_expr_to_form(first)?);
            } else {
                parts.push(Form::new(FormKind::Nil, span));
            }
            if let Some(first) = else_branch.first() {
                parts.push(ir_expr_to_form(first)?);
            }
            FormKind::List(parts)
        }
        IRExprKind::Do(exprs) => {
            let mut parts = Vec::new();
            parts.push(Form::new(FormKind::Symbol("do".into()), span));
            for expr in exprs {
                parts.push(ir_expr_to_form(expr)?);
            }
            FormKind::List(parts)
        }
        IRExprKind::Call { callee, args } => {
            let name = callee.clone()?;
            let mut parts = Vec::new();
            parts.push(Form::new(FormKind::Symbol(name), span));
            for arg in args {
                parts.push(ir_expr_to_form(arg)?);
            }
            FormKind::List(parts)
        }
        IRExprKind::Unknown => return None,
    };
    Some(Form::new(kind, span))
}

#[derive(Clone)]
struct TypeEnv {
    stack: Vec<StdHashMap<String, TypeKind>>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            stack: vec![StdHashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.stack.push(StdHashMap::new());
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn set(&mut self, name: &str, ty: TypeKind) {
        if let Some(frame) = self.stack.last_mut() {
            frame.insert(canonical_symbol_name(name).into_owned(), ty);
        }
    }

    fn get(&self, name: &str) -> Option<&TypeKind> {
        let canonical = canonical_symbol_name(name);
        for frame in self.stack.iter().rev() {
            if let Some(ty) = frame.get(canonical.as_ref()) {
                return Some(ty);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fn_meta::{self, FnMeta, FnOverload};
    use crate::reader::{Reader, ReaderOptions};

    fn read_forms(src: &str) -> Vec<Form> {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        reader.read_all().unwrap()
    }

    #[test]
    fn analyze_simple_add() {
        fn_meta::clear_for_tests();
        let mut meta = FnMeta::new("core", "+");
        meta.overloads.push(FnOverload {
            arg_types: vec![TypeKind::Int, TypeKind::Int],
            rest: None,
            ret_type: TypeKind::Int,
            special_op: None,
        });
        fn_meta::register(meta);
        let forms = read_forms("(+ x y)");
        let params = vec![
            ParamType {
                name: "x".into(),
                hint: Some(TypeKind::Int),
            },
            ParamType {
                name: "y".into(),
                hint: Some(TypeKind::Int),
            },
        ];
        let analysis = analyze_fn_body(&params, &forms);
        assert_eq!(analysis.ret_type, TypeKind::Int);
    }

    #[test]
    fn analyze_vector_literal() {
        fn_meta::clear_for_tests();
        let forms = read_forms("[1 2 3]");
        let params = Vec::new();
        let analysis = analyze_fn_body(&params, &forms);
        assert_eq!(analysis.ret_type, TypeKind::vector(TypeKind::Int));
    }

    #[test]
    fn range_returns_int_vector() {
        fn_meta::clear_for_tests();
        let forms = read_forms("(range 0 10)");
        let params = Vec::new();
        let analysis = analyze_fn_body(&params, &forms);
        assert_eq!(analysis.ret_type, TypeKind::vector(TypeKind::Int));
    }

    #[test]
    fn map_preserves_vector_inner_type() {
        fn_meta::clear_for_tests();
        let forms = read_forms("(map inc xs)");
        let params = vec![ParamType {
            name: "xs".into(),
            hint: Some(TypeKind::vector(TypeKind::Int)),
        }];
        let analysis = analyze_fn_body(&params, &forms);
        assert_eq!(analysis.ret_type, TypeKind::vector(TypeKind::Int));
    }

    #[test]
    fn reduce_returns_init_type() {
        fn_meta::clear_for_tests();
        register_plus_meta();
        let forms = read_forms("(reduce + 0 xs)");
        let params = vec![ParamType {
            name: "xs".into(),
            hint: Some(TypeKind::vector(TypeKind::Int)),
        }];
        let analysis = analyze_fn_body(&params, &forms);
        assert_eq!(analysis.ret_type, TypeKind::Int);
    }

    fn register_plus_meta() {
        let mut meta = FnMeta::new("core", "+");
        meta.overloads.push(FnOverload {
            arg_types: vec![TypeKind::Int, TypeKind::Int],
            rest: None,
            ret_type: TypeKind::Int,
            special_op: None,
        });
        fn_meta::register(meta);
    }
}
