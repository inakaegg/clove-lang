use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use clove_core::typed_ir::{Literal, TypedExpr, TypedExprKind};
use clove_core::typing::hm::{PrimType, Type};

pub struct GeneratedProject {
    pub build_dir: PathBuf,
    pub crate_name: String,
}

#[derive(Clone, Debug)]
struct LocalBinding {
    name: String,
    ident: String,
    ty: Type,
}

#[derive(Clone, Debug, Default)]
struct RenderCtx {
    locals: Vec<LocalBinding>,
    global_fns: HashSet<String>,
    global_vars: HashMap<String, String>,
}

impl RenderCtx {
    fn with_globals(global_fns: &HashSet<String>, global_vars: &HashMap<String, String>) -> Self {
        Self {
            locals: Vec::new(),
            global_fns: global_fns.clone(),
            global_vars: global_vars.clone(),
        }
    }

    fn with_binding(&self, name: &str, ident: &str, ty: &Type) -> Self {
        if name == "_" || name == "&" {
            return Self {
                locals: self.locals.clone(),
                global_fns: self.global_fns.clone(),
                global_vars: self.global_vars.clone(),
            };
        }
        let mut locals = self.locals.clone();
        locals.push(LocalBinding {
            name: name.to_string(),
            ident: ident.to_string(),
            ty: ty.clone(),
        });
        Self {
            locals,
            global_fns: self.global_fns.clone(),
            global_vars: self.global_vars.clone(),
        }
    }

    fn is_local(&self, name: &str) -> bool {
        self.locals.iter().any(|local| local.name == name)
    }

    fn is_global_fn(&self, name: &str) -> bool {
        self.global_fns.contains(name)
    }

    fn is_global_symbol(&self, name: &str) -> bool {
        self.is_global_fn(name) || self.global_vars.contains_key(name)
    }

    fn global_var_ident(&self, name: &str) -> Option<&str> {
        self.global_vars.get(name).map(String::as_str)
    }

    fn local_ty(&self, name: &str) -> Option<&Type> {
        self.locals
            .iter()
            .find(|local| local.name == name)
            .map(|local| &local.ty)
    }
}

pub fn generate_rust_project(
    build_dir: &Path,
    crate_name: &str,
    _output: &Path,
    source_path: &Path,
    exprs: &[TypedExpr],
) -> Result<GeneratedProject, String> {
    fs::create_dir_all(build_dir.join("src"))
        .map_err(|e| format!("failed to create build dir: {}", e))?;
    let (funcs, defs, main_exprs) = split_defs(exprs);
    let global_fns = collect_global_fns(exprs);
    let global_vars = collect_global_vars(exprs);
    let source_path = fs::canonicalize(source_path).unwrap_or_else(|_| source_path.to_path_buf());
    let clove_core_path = find_clove_core_path(&source_path);
    let cargo = render_cargo_toml(crate_name, &clove_core_path);
    fs::write(build_dir.join("Cargo.toml"), cargo)
        .map_err(|e| format!("failed to write Cargo.toml: {}", e))?;
    let main_rs = render_main_rs(
        &funcs,
        &defs,
        &main_exprs,
        Some(&source_path),
        None,
        &global_fns,
        &global_vars,
    );
    fs::write(build_dir.join("src").join("main.rs"), main_rs)
        .map_err(|e| format!("failed to write main.rs: {}", e))?;
    Ok(GeneratedProject {
        build_dir: build_dir.to_path_buf(),
        crate_name: crate_name.to_string(),
    })
}

fn find_clove_core_path(source_path: &Path) -> PathBuf {
    let start = if source_path.is_dir() {
        Some(source_path)
    } else {
        source_path.parent()
    };
    let Some(start) = start else {
        return Path::new("crates").join("clove-core");
    };
    for ancestor in start.ancestors() {
        let candidate = ancestor
            .join("crates")
            .join("clove-core")
            .join("Cargo.toml");
        if candidate.is_file() {
            return fs::canonicalize(candidate.parent().unwrap())
                .unwrap_or_else(|_| ancestor.join("crates").join("clove-core"));
        }
    }
    Path::new("crates").join("clove-core")
}

pub fn requires_dynamic_fallback(exprs: &[TypedExpr]) -> bool {
    let global_fns = collect_global_fns(exprs);
    let global_vars = collect_global_vars(exprs);
    let ctx = RenderCtx::with_globals(&global_fns, &global_vars);
    exprs
        .iter()
        .any(|expr| expr_requires_global_fallback(expr, &ctx, false))
}

fn collect_global_fns(exprs: &[TypedExpr]) -> HashSet<String> {
    let mut names = HashSet::new();
    for expr in exprs {
        if let Some(name) = defn_symbol_name(expr) {
            names.insert(name);
        }
    }
    names
}

fn collect_global_vars(exprs: &[TypedExpr]) -> HashMap<String, String> {
    let mut vars = HashMap::new();
    for expr in exprs {
        if let Some(def) = extract_def(expr) {
            vars.insert(def.name.clone(), def.ident.clone());
        }
    }
    vars
}

fn defn_symbol_name(expr: &TypedExpr) -> Option<String> {
    let TypedExprKind::Call { callee, args } = &expr.kind else {
        return None;
    };
    let TypedExprKind::Symbol(sym) = &callee.kind else {
        return None;
    };
    if sym != "defn" && sym != "defn-" {
        return None;
    }
    let Some(first) = args.first() else {
        return None;
    };
    match &first.kind {
        TypedExprKind::Symbol(name) => Some(name.clone()),
        _ => None,
    }
}

fn expr_requires_global_fallback(expr: &TypedExpr, ctx: &RenderCtx, allow_recur: bool) -> bool {
    if allow_recur && is_recur_call(expr) {
        return false;
    }
    if expr_needs_runtime(expr, ctx) {
        let supported = runtime_fallback_supported(expr, ctx);
        if !supported && std::env::var("CLOVE_HM_FALLBACK_DEBUG").is_ok() {
            eprintln!(
                "[hm][fallback] expr requires global fallback: {:?} (type={:?})",
                expr.source, expr.ty
            );
        }
        return !supported;
    }
    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Symbol(sym) = &callee.kind {
                if sym == "loop" {
                    let mut next_ctx = ctx.clone();
                    if let Some(first) = args.first() {
                        if let TypedExprKind::Vector(vec) = &first.kind {
                            let mut iter = vec.iter();
                            while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                                if expr_requires_global_fallback(v, &next_ctx, allow_recur) {
                                    return true;
                                }
                                if let TypedExprKind::Symbol(name) = &k.kind {
                                    let ident = sanitize_ident(name);
                                    next_ctx = next_ctx.with_binding(name, &ident, &v.ty);
                                }
                            }
                        } else {
                            return true;
                        }
                    }
                    for expr in args.iter().skip(1) {
                        if expr_requires_global_fallback(expr, &next_ctx, true) {
                            return true;
                        }
                    }
                    return false;
                }
                if sym == "defn" || sym == "defn-" {
                    let Some(defn) = extract_defn(expr) else {
                        return true;
                    };
                    let mut next_ctx = ctx.clone();
                    for param in &defn.params {
                        next_ctx = next_ctx.with_binding(&param.name, &param.ident, &param.ty);
                    }
                    return defn
                        .body
                        .iter()
                        .any(|expr| expr_requires_global_fallback(expr, &next_ctx, allow_recur));
                }
                if sym == "try" {
                    if let Some((body, finally_body)) = parse_try_finally(args) {
                        for expr in body {
                            if expr_requires_global_fallback(expr, ctx, allow_recur) {
                                return true;
                            }
                        }
                        for expr in finally_body {
                            if expr_requires_global_fallback(expr, ctx, allow_recur) {
                                return true;
                            }
                        }
                        return false;
                    }
                }
            }
            let callee_needs = match &callee.kind {
                TypedExprKind::Symbol(_) => false,
                _ => expr_requires_global_fallback(callee, ctx, allow_recur),
            };
            callee_needs
                || args
                    .iter()
                    .any(|arg| expr_requires_global_fallback(arg, ctx, allow_recur))
        }
        TypedExprKind::Vector(items) => items
            .iter()
            .any(|item| expr_requires_global_fallback(item, ctx, allow_recur)),
        TypedExprKind::Map(map) => map
            .values()
            .any(|v| expr_requires_global_fallback(v, ctx, allow_recur)),
        TypedExprKind::Fn { params, body } => {
            let mut next_ctx = ctx.clone();
            let mut param_tys = Vec::new();
            if let Type::Func(args, _rest, _ret) = &expr.ty {
                param_tys = args.clone();
            }
            for (idx, name) in params.iter().enumerate() {
                let ident = sanitize_ident(name);
                let ty = param_tys.get(idx).cloned().unwrap_or(Type::Any);
                next_ctx = next_ctx.with_binding(name, &ident, &ty);
            }
            body.iter()
                .any(|expr| expr_requires_global_fallback(expr, &next_ctx, allow_recur))
        }
        TypedExprKind::Let { bindings, body } => {
            let mut next_ctx = ctx.clone();
            for (name, expr) in bindings {
                if expr_requires_global_fallback(expr, &next_ctx, allow_recur) {
                    return true;
                }
                let ident = sanitize_ident(name);
                next_ctx = next_ctx.with_binding(name, &ident, &expr.ty);
            }
            body.iter()
                .any(|expr| expr_requires_global_fallback(expr, &next_ctx, allow_recur))
        }
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            expr_requires_global_fallback(cond, ctx, allow_recur)
                || then_branch
                    .iter()
                    .any(|expr| expr_requires_global_fallback(expr, ctx, allow_recur))
                || else_branch
                    .iter()
                    .any(|expr| expr_requires_global_fallback(expr, ctx, allow_recur))
        }
        TypedExprKind::Do(items) => items
            .iter()
            .any(|expr| expr_requires_global_fallback(expr, ctx, allow_recur)),
        TypedExprKind::Unknown | TypedExprKind::Literal(_) | TypedExprKind::Symbol(_) => false,
    }
}

fn expr_needs_runtime(expr: &TypedExpr, ctx: &RenderCtx) -> bool {
    match &expr.kind {
        TypedExprKind::Unknown => true,
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Symbol(name) = &callee.kind {
                match name.as_str() {
                    "cond" => {
                        return !cond_supported(args);
                    }
                    "when" | "when-not" => {
                        return args.len() < 2;
                    }
                    "match" => return !match_supported(args),
                    "println" | "print" | "prn" | "str" => {
                        return !args.iter().all(|arg| format_supported(&arg.ty));
                    }
                    "defn" => {
                        if args.len() < 3 {
                            return true;
                        }
                        if !matches!(args[1].kind, TypedExprKind::Vector(_)) {
                            return true;
                        }
                        return false;
                    }
                    "defn-" => {
                        if args.len() < 3 {
                            return true;
                        }
                        if !matches!(args[1].kind, TypedExprKind::Vector(_)) {
                            return true;
                        }
                        return false;
                    }
                    "ns" | "deftype" | "defenum" => {
                        return false;
                    }
                    _ => {}
                }
                if !call_supported_by_codegen(name, args, ctx) {
                    return true;
                }
            }
            false
        }
        TypedExprKind::Map(map) => map.values().any(|v| !value_supported(&v.ty)),
        TypedExprKind::Symbol(sym) => {
            !(ctx.is_local(sym) || ctx.is_global_symbol(sym) || is_builtin_symbol(sym))
        }
        TypedExprKind::Literal(_)
        | TypedExprKind::Vector(_)
        | TypedExprKind::Fn { .. }
        | TypedExprKind::Let { .. }
        | TypedExprKind::If { .. }
        | TypedExprKind::Do(_) => false,
    }
}

fn format_supported(ty: &Type) -> bool {
    match ty {
        Type::Prim(_) => true,
        Type::Any | Type::Var(_) => true,
        Type::Option(inner) => format_supported(inner),
        Type::Vector(inner) => format_supported(inner),
        Type::Tuple(items) => items.iter().all(format_supported),
        Type::Map(key, val) => format_supported(key) && format_supported(val),
        Type::Set(inner) => format_supported(inner),
        Type::Any
        | Type::Var(_)
        | Type::Func(_, _, _)
        | Type::Record(_)
        | Type::Opaque(_)
        | Type::Overloaded(_)
        | Type::Mut(_) => false,
    }
}

fn value_supported(ty: &Type) -> bool {
    match ty {
        Type::Prim(_) => true,
        Type::Option(inner) => value_supported(inner),
        Type::Vector(inner) => value_supported(inner),
        Type::Set(inner) => value_supported(inner),
        Type::Map(key, val) => value_supported(key) && value_supported(val),
        Type::Record(_) => true,
        Type::Any | Type::Var(_) => true,
        Type::Opaque(name) if is_symbol_type_name(name) => true,
        _ => false,
    }
}

fn runtime_fallback_supported(expr: &TypedExpr, ctx: &RenderCtx) -> bool {
    if !value_supported(&expr.ty) {
        return false;
    }
    ctx.locals.iter().all(|local| value_supported(&local.ty))
}

fn call_supported_by_codegen(name: &str, args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if ctx.is_local(name) || ctx.is_global_fn(name) {
        return true;
    }
    let argc = args.len();
    match name {
        "+" | "-" | "*" | "/" | "=" | "not=" | ">" | "<" | ">=" | "<=" => argc == 2,
        "not" => argc == 1,
        "and" | "or" | "str" => true,
        "cond" => cond_supported(args),
        "when" | "when-not" => argc >= 2,
        "println" | "print" | "prn" | "puts" => true,
        "match" => match_supported(args),
        "map" => map_supported(args, ctx),
        "map-indexed" => map_indexed_supported(args, ctx),
        "filter" => filter_supported(args, ctx),
        "pmap" => pmap_supported(args, ctx),
        "doseq" | "each" => doseq_supported(args),
        "reduce" => reduce_supported(args, ctx),
        "update" => update_supported(args, ctx),
        "try" => try_supported(args),
        "throw" => throw_supported(args),
        "as->" => as_thread_supported(args),
        "glob" | "fs::glob" | "io::glob" | "std::glob" | "glob*" | "fs::glob*" | "io::glob*"
        | "std::glob*" => args.len() == 1,
        "json::read-file" => args.len() == 1,
        "json::read-seq" => args.len() == 1,
        "json::read-file-seq" => args.len() == 1,
        "includes?" => args.len() == 2,
        "string::includes?" | "str::includes?" => args.len() == 2,
        "pr-str" => true,
        "vector" | "list" | "hash-map" | "hash-set" => true,
        "loop" => matches!(
            args.get(0).map(|arg| &arg.kind),
            Some(TypedExprKind::Vector(_))
        ),
        "inc" | "abs" | "count" | "empty?" | "first" | "rest" => argc == 1,
        "nth" | "conj" => argc == 2,
        "into" => into_supported(args),
        "dissoc" => argc == 2 && map_like_type(&args[0].ty),
        "assoc" => argc >= 3 && argc % 2 == 1 && map_like_type(&args[0].ty),
        "get" => argc == 2 || argc == 3,
        "contains?" => argc == 2 && contains_like_type(&args[0].ty),
        "subs" => argc == 2 || argc == 3,
        "max" | "min" => true,
        "union" | "std::union" | "set::union" => true,
        "intersection" | "std::intersection" | "set::intersection" => true,
        "difference" | "std::difference" | "set::difference" => true,
        "disj" => argc >= 1,
        "ns" | "deftype" | "defenum" | "defn" | "defn-" => true,
        _ => false,
    }
}

fn as_thread_supported(args: &[TypedExpr]) -> bool {
    if args.len() < 3 {
        return false;
    }
    matches!(args[1].kind, TypedExprKind::Symbol(_))
}

fn map_like_type(ty: &Type) -> bool {
    match ty {
        Type::Map(_, _) | Type::Record(_) => true,
        Type::Option(inner) => map_like_type(inner),
        _ => false,
    }
}

fn contains_like_type(ty: &Type) -> bool {
    match ty {
        Type::Map(_, _) | Type::Record(_) | Type::Set(_) | Type::Vector(_) => true,
        Type::Option(inner) => contains_like_type(inner),
        _ => false,
    }
}

fn into_supported(args: &[TypedExpr]) -> bool {
    if args.len() != 2 {
        return false;
    }
    matches!(
        (&args[0].ty, &args[1].ty),
        (Type::Vector(_), Type::Vector(_))
    ) || matches!((&args[0].ty, &args[1].ty), (Type::Set(_), Type::Set(_)))
}

fn try_supported(args: &[TypedExpr]) -> bool {
    parse_try_finally(args).is_some()
}

fn throw_supported(args: &[TypedExpr]) -> bool {
    args.len() == 1
}

fn map_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() < 2 {
        return false;
    }
    let mut elem_tys = Vec::new();
    for coll in args.iter().skip(1) {
        let Type::Vector(inner) = &coll.ty else {
            return false;
        };
        elem_tys.push((**inner).clone());
    }
    callable_supported(&args[0], &elem_tys, ctx)
}

fn parse_doseq_bindings<'a>(bindings: &'a TypedExpr) -> Option<Vec<(String, &'a TypedExpr)>> {
    let TypedExprKind::Vector(items) = &bindings.kind else {
        return None;
    };
    if items.is_empty() || items.len() % 2 != 0 {
        return None;
    }
    let mut out = Vec::with_capacity(items.len() / 2);
    let mut idx = 0;
    while idx + 1 < items.len() {
        let TypedExprKind::Symbol(name) = &items[idx].kind else {
            return None;
        };
        out.push((name.clone(), &items[idx + 1]));
        idx += 2;
    }
    Some(out)
}

fn doseq_supported(args: &[TypedExpr]) -> bool {
    let Some(bindings_form) = args.first() else {
        return false;
    };
    let Some(bindings) = parse_doseq_bindings(bindings_form) else {
        return false;
    };
    bindings.iter().all(|(_, coll)| {
        matches!(coll.ty, Type::Vector(_))
            || is_doseq_coll_call(coll, "json::read-file")
            || is_doseq_coll_call(coll, "json::read-seq")
            || is_doseq_coll_call(coll, "json::read-file-seq")
    })
}

fn is_doseq_coll_call(expr: &TypedExpr, name: &str) -> bool {
    match &expr.kind {
        TypedExprKind::Call { callee, .. } => match &callee.kind {
            TypedExprKind::Symbol(sym) => sym == name,
            _ => false,
        },
        _ => false,
    }
}

fn map_indexed_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() != 2 {
        return false;
    }
    let Type::Vector(inner) = &args[1].ty else {
        return false;
    };
    let arg_tys = vec![Type::Prim(PrimType::Int), (**inner).clone()];
    callable_supported(&args[0], &arg_tys, ctx)
}

fn filter_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() != 2 {
        return false;
    }
    let Type::Vector(inner) = &args[1].ty else {
        return false;
    };
    let arg_tys = vec![(**inner).clone()];
    if !callable_supported(&args[0], &arg_tys, ctx) {
        return false;
    }
    let Some(ret_ty) = callable_return_type(&args[0]) else {
        return false;
    };
    truthy_expr_for_type("__keep", &ret_ty).is_some()
}

fn pfilter_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() != 2 && args.len() != 3 {
        return false;
    }
    let func = &args[0];
    let coll = &args[1];
    let Type::Vector(inner) = &coll.ty else {
        return false;
    };
    let arg_tys = vec![(**inner).clone()];
    match &func.kind {
        TypedExprKind::Symbol(name) => {
            if ctx.is_global_fn(name) {
                if !matches!(func.ty, Type::Any | Type::Var(_))
                    && !callable_supported(func, &arg_tys, ctx)
                {
                    return false;
                }
            } else if !builtin_callable_supported(name, &arg_tys) {
                return false;
            }
        }
        _ => return false,
    }
    let Some(ret_ty) = callable_return_type(func) else {
        return false;
    };
    if truthy_expr_for_type("__keep", &ret_ty).is_none() {
        return false;
    }
    if args.len() == 3 {
        return pmap_opts_value(&args[2]).is_some();
    }
    true
}

fn pmap_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() != 2 && args.len() != 3 {
        return false;
    }
    let func = &args[0];
    let coll = &args[1];
    let Type::Vector(inner) = &coll.ty else {
        return false;
    };
    let arg_tys = vec![(**inner).clone()];
    match &func.kind {
        TypedExprKind::Symbol(name) => {
            if ctx.is_global_fn(name) {
                if !matches!(func.ty, Type::Any | Type::Var(_))
                    && !callable_supported(func, &arg_tys, ctx)
                {
                    return false;
                }
            } else if !builtin_callable_supported(name, &arg_tys) {
                return false;
            }
        }
        _ => return false,
    }
    if args.len() == 3 {
        return pmap_opts_value(&args[2]).is_some();
    }
    true
}

fn pmap_max_parallel(opts: &TypedExpr) -> Option<usize> {
    let TypedExprKind::Map(map) = &opts.kind else {
        return None;
    };
    let value = map.get("max-parallel")?;
    match &value.kind {
        TypedExprKind::Literal(Literal::Int(n)) if *n > 0 => Some(*n as usize),
        _ => None,
    }
}

fn pmap_opts_value(opts: &TypedExpr) -> Option<&TypedExpr> {
    let TypedExprKind::Map(map) = &opts.kind else {
        return None;
    };
    map.get("max-parallel")
}

fn reduce_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() != 2 && args.len() != 3 {
        return false;
    }
    let coll = if args.len() == 2 { &args[1] } else { &args[2] };
    let Type::Vector(inner) = &coll.ty else {
        return false;
    };
    let elem_ty = (**inner).clone();
    let acc_ty = if args.len() == 2 {
        elem_ty.clone()
    } else {
        args[1].ty.clone()
    };
    let arg_tys = vec![acc_ty, elem_ty];
    callable_supported(&args[0], &arg_tys, ctx)
}

fn update_supported(args: &[TypedExpr], ctx: &RenderCtx) -> bool {
    if args.len() < 3 {
        return false;
    }
    let value_ty = match &args[0].ty {
        Type::Map(_, val) => Some((**val).clone()),
        Type::Option(inner) => match inner.as_ref() {
            Type::Map(_, val) => Some((**val).clone()),
            _ => None,
        },
        _ => None,
    };
    let Some(value_ty) = value_ty else {
        return false;
    };
    let mut arg_tys = Vec::with_capacity(args.len() - 1);
    arg_tys.push(value_ty);
    for extra in args.iter().skip(3) {
        arg_tys.push(extra.ty.clone());
    }
    callable_supported(&args[2], &arg_tys, ctx)
}

fn parse_try_finally<'a>(
    args: &'a [TypedExpr],
) -> Option<(Vec<&'a TypedExpr>, Vec<&'a TypedExpr>)> {
    let mut body = Vec::new();
    let mut finally_body: Option<Vec<&TypedExpr>> = None;
    for (idx, arg) in args.iter().enumerate() {
        if symbol_call_args(arg, "catch").is_some() {
            return None;
        }
        if let Some(fin_args) = symbol_call_args(arg, "finally") {
            if finally_body.is_some() || idx + 1 != args.len() || fin_args.is_empty() {
                return None;
            }
            finally_body = Some(fin_args.iter().collect());
            continue;
        }
        if finally_body.is_some() {
            return None;
        }
        body.push(arg);
    }
    let finally_body = finally_body?;
    if body.is_empty() {
        return None;
    }
    Some((body, finally_body))
}

fn symbol_call_args<'a>(expr: &'a TypedExpr, name: &str) -> Option<&'a [TypedExpr]> {
    match &expr.kind {
        TypedExprKind::Call { callee, args } => match &callee.kind {
            TypedExprKind::Symbol(sym) if sym == name => Some(args.as_slice()),
            _ => None,
        },
        _ => None,
    }
}

fn callable_supported(func: &TypedExpr, arg_tys: &[Type], ctx: &RenderCtx) -> bool {
    if let TypedExprKind::Symbol(name) = &func.kind {
        if ctx.is_local(name) || ctx.is_global_fn(name) {
            return callable_arity_matches(&func.ty, arg_tys.len());
        }
        return builtin_callable_supported(name, arg_tys);
    }
    callable_arity_matches(&func.ty, arg_tys.len())
}

fn callable_arity_matches(ty: &Type, argc: usize) -> bool {
    match ty {
        Type::Func(args, rest, _ret) => {
            if rest.is_some() {
                argc >= args.len()
            } else {
                argc == args.len()
            }
        }
        Type::Overloaded(cands) => cands.iter().any(|cand| callable_arity_matches(cand, argc)),
        _ => false,
    }
}

fn callable_return_type(func: &TypedExpr) -> Option<Type> {
    match &func.ty {
        Type::Func(_, _, ret) => Some((**ret).clone()),
        Type::Overloaded(cands) => cands.iter().find_map(|cand| match cand {
            Type::Func(_, _, ret) => Some((**ret).clone()),
            _ => None,
        }),
        _ => None,
    }
}

fn builtin_callable_supported(name: &str, arg_tys: &[Type]) -> bool {
    let argc = arg_tys.len();
    match name {
        "+" | "-" | "*" | "/" => argc >= 1 && arg_tys.iter().all(is_numeric_type),
        "=" | "not=" | ">" | "<" | ">=" | "<=" => argc == 2 && arg_tys.iter().all(is_numeric_type),
        "inc" | "dec" => argc == 1 && is_numeric_type(&arg_tys[0]),
        "max" | "min" => argc == 2 && arg_tys.iter().all(is_numeric_type),
        "vector" | "list" => true,
        _ => false,
    }
}

fn is_numeric_type(ty: &Type) -> bool {
    matches!(ty, Type::Prim(PrimType::Int) | Type::Prim(PrimType::Float))
}

fn is_int_type(ty: &Type) -> bool {
    matches!(ty, Type::Prim(PrimType::Int))
}

fn render_number_as_f64(expr: &str, ty: &Type) -> String {
    if is_int_type(ty) {
        format!("({} as f64)", expr)
    } else {
        expr.to_string()
    }
}

fn resolved_numeric_type(expr: &TypedExpr, ctx: &RenderCtx) -> Type {
    if matches!(expr.ty, Type::Any | Type::Var(_)) {
        if let TypedExprKind::Symbol(name) = &expr.kind {
            if let Some(local_ty) = ctx.local_ty(name) {
                return local_ty.clone();
            }
        }
    }
    expr.ty.clone()
}

fn render_expr_as_f64(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let rendered = render_expr(expr, ctx);
    let ty = resolved_numeric_type(expr, ctx);
    if is_int_type(&ty) {
        format!("({} as f64)", rendered)
    } else {
        rendered
    }
}

fn callable_first_param_type(func: &TypedExpr) -> Option<Type> {
    match &func.ty {
        Type::Func(args, _rest, _ret) => args.first().cloned(),
        Type::Overloaded(cands) => cands.iter().find_map(|cand| match cand {
            Type::Func(args, _rest, _ret) => args.first().cloned(),
            _ => None,
        }),
        _ => None,
    }
}

fn match_supported(args: &[TypedExpr]) -> bool {
    if args.len() < 3 || (args.len() - 1) % 2 != 0 {
        return false;
    }
    let mut idx = 1;
    while idx + 1 < args.len() {
        let pat = &args[idx];
        if is_default_match_pattern(pat) {
            return idx + 2 == args.len();
        }
        if !is_literal_match_pattern(pat) {
            return false;
        }
        idx += 2;
    }
    true
}

fn cond_supported(args: &[TypedExpr]) -> bool {
    if args.len() < 2 || args.len() % 2 != 0 {
        return false;
    }
    let mut idx = 0;
    while idx + 1 < args.len() {
        let test = &args[idx];
        if is_cond_default_test(test) {
            return idx + 2 == args.len();
        }
        idx += 2;
    }
    true
}

fn is_literal_match_pattern(expr: &TypedExpr) -> bool {
    matches!(expr.kind, TypedExprKind::Literal(_))
}

fn is_default_match_pattern(expr: &TypedExpr) -> bool {
    match &expr.kind {
        TypedExprKind::Symbol(sym) => sym == "_" || sym == "else" || sym == ":else",
        TypedExprKind::Literal(Literal::Keyword(kw)) => kw == "else",
        _ => false,
    }
}

fn is_cond_default_test(expr: &TypedExpr) -> bool {
    match &expr.kind {
        TypedExprKind::Symbol(sym) => sym == "_" || sym == "else" || sym == ":else",
        TypedExprKind::Literal(Literal::Keyword(kw)) => kw == "else",
        _ => false,
    }
}

fn is_recur_call(expr: &TypedExpr) -> bool {
    if let TypedExprKind::Call { callee, .. } = &expr.kind {
        if let TypedExprKind::Symbol(sym) = &callee.kind {
            return sym == "recur";
        }
    }
    false
}

fn split_defs(exprs: &[TypedExpr]) -> (Vec<Defn>, Vec<Def>, Vec<TypedExpr>) {
    let mut defs = Vec::new();
    let mut values = Vec::new();
    let mut main_exprs = Vec::new();
    for expr in exprs {
        if let Some(def) = extract_defn(expr) {
            defs.push(def);
        } else if let Some(def) = extract_def(expr) {
            values.push(def);
        } else {
            main_exprs.push(expr.clone());
        }
    }
    (defs, values, main_exprs)
}

#[derive(Clone, Debug)]
struct Defn {
    raw_name: String,
    name: String,
    params: Vec<DefnParam>,
    body: Vec<TypedExpr>,
    ret_ty: Type,
}

#[derive(Clone, Debug)]
struct DefnParam {
    name: String,
    ident: String,
    ty: Type,
    is_rest: bool,
}

#[derive(Clone, Debug)]
struct Def {
    name: String,
    ident: String,
    ty: Type,
    value: TypedExpr,
}

fn extract_defn(expr: &TypedExpr) -> Option<Defn> {
    if let TypedExprKind::Call { callee, args } = &expr.kind {
        if let TypedExprKind::Symbol(sym) = &callee.kind {
            if (sym == "defn" || sym == "defn-") && args.len() >= 3 {
                let (raw_name, name) = match &args[0].kind {
                    TypedExprKind::Symbol(s) => (s.clone(), sanitize_ident(s)),
                    _ => return None,
                };
                let params = match &args[1].kind {
                    TypedExprKind::Vector(vec) => collect_defn_params(vec),
                    _ => Vec::new(),
                };
                let body = args.iter().skip(2).cloned().collect();
                let ret_ty = match &expr.ty {
                    Type::Func(_, _, boxed) => (*boxed.clone()).clone(),
                    other => other.clone(),
                };
                return Some(Defn {
                    raw_name,
                    name,
                    params,
                    body,
                    ret_ty,
                });
            }
        }
    }
    None
}

fn collect_defn_params(items: &[TypedExpr]) -> Vec<DefnParam> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        match &items[idx].kind {
            TypedExprKind::Symbol(sym) if sym == "&" => {
                if let Some(next) = items.get(idx + 1) {
                    if let TypedExprKind::Symbol(name) = &next.kind {
                        out.push(DefnParam {
                            name: name.clone(),
                            ident: sanitize_ident(name),
                            ty: next.ty.clone(),
                            is_rest: true,
                        });
                    }
                }
                break;
            }
            TypedExprKind::Symbol(name) => {
                out.push(DefnParam {
                    name: name.clone(),
                    ident: sanitize_ident(name),
                    ty: items[idx].ty.clone(),
                    is_rest: false,
                });
            }
            _ => {}
        }
        idx += 1;
    }
    out
}

fn find_main_defn(defs: &[Defn]) -> Option<&Defn> {
    defs.iter()
        .find(|def| def.raw_name == "-main" || def.raw_name == "main")
}

fn extract_def(expr: &TypedExpr) -> Option<Def> {
    let TypedExprKind::Call { callee, args } = &expr.kind else {
        return None;
    };
    let TypedExprKind::Symbol(sym) = &callee.kind else {
        return None;
    };
    if sym != "def" && sym != "def-" {
        return None;
    }
    let Some(first) = args.first() else {
        return None;
    };
    let name = match &first.kind {
        TypedExprKind::Symbol(s) => s.clone(),
        _ => return None,
    };
    let value = def_value_expr(args)?;
    Some(Def {
        name: name.clone(),
        ident: format!("__def_{}", sanitize_ident(&name)),
        ty: value.ty.clone(),
        value: value.clone(),
    })
}

fn def_value_expr(args: &[TypedExpr]) -> Option<&TypedExpr> {
    if args.len() < 2 {
        return None;
    }
    let mut idx = 1;
    if idx + 1 < args.len() {
        if matches!(args[idx].kind, TypedExprKind::Literal(Literal::Str(_))) {
            idx += 1;
        }
    }
    if idx + 1 < args.len() && matches!(args[idx].kind, TypedExprKind::Map(_)) {
        idx += 1;
    }
    args.get(idx)
}

fn render_cargo_toml(crate_name: &str, clove_core_path: &Path) -> String {
    format!(
        "[package]\nname = \"{name}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nclove-core = {{ path = \"{core_path}\" }}\n\n[workspace]\nmembers = []\n",
        name = crate_name,
        core_path = clove_core_path.display()
    )
}

fn render_runtime_opts(source_path: Option<&Path>, working_dir: Option<&Path>) -> String {
    let mut out = String::new();
    out.push_str("fn __clove_runtime_opts() -> EvalOptions {\n");
    out.push_str("    let mut opts = EvalOptions::default();\n");
    if let Some(path) = source_path {
        let literal = format!("{:?}", path.to_string_lossy());
        out.push_str(&format!(
            "    opts.source_name = Some({}.to_string());\n",
            literal
        ));
    }
    if let Some(path) = working_dir {
        let literal = format!("{:?}", path.to_string_lossy());
        out.push_str(&format!(
            "    opts.working_dir = Some(std::path::PathBuf::from({}));\n",
            literal
        ));
    }
    out.push_str("    opts\n}\n");
    out
}

fn render_main_rs(
    funcs: &[Defn],
    defs: &[Def],
    main_exprs: &[TypedExpr],
    source_path: Option<&Path>,
    working_dir: Option<&Path>,
    global_fns: &HashSet<String>,
    global_vars: &HashMap<String, String>,
) -> String {
    let mut out = String::new();
    out.push_str("use clove_core::ast::{Key, Value};\n");
    out.push_str("use clove_core::options::EvalOptions;\n");
    out.push_str("use clove_core::runtime::RuntimeCtx;\n");
    if !defs.is_empty() {
        out.push_str("use std::sync::OnceLock;\n");
    }
    out.push_str(&render_runtime_opts(source_path, working_dir));
    let base_ctx = RenderCtx::with_globals(global_fns, global_vars);
    for def in defs {
        out.push_str(&render_def_static(def));
    }
    for def in funcs {
        out.push_str(&render_fn(def, &base_ctx));
        out.push('\n');
    }
    out.push_str("fn main() {\n");
    out.push_str("    let __runtime = RuntimeCtx::new(__clove_runtime_opts(), &[]);\n");
    out.push_str("    __runtime.with_current_ctx(|_| {\n");
    for def in defs {
        out.push_str("        let _ = ");
        out.push_str(&render_def_init(def, &base_ctx));
        out.push_str(";\n");
    }
    if !main_exprs.is_empty() {
        let ctx = base_ctx.clone();
        // Emit the last expression as the result
        let mut last_expr = String::new();
        for (idx, expr) in main_exprs.iter().enumerate() {
            let rendered = render_expr(expr, &ctx);
            if idx + 1 == main_exprs.len() {
                last_expr = rendered;
            } else {
                out.push_str("        let _ = ");
                out.push_str(&rendered);
                out.push_str(";\n");
            }
        }
        out.push_str("        let result = ");
        out.push_str(&last_expr);
        out.push_str(";\n        println!(\"{:?}\", result);\n");
    } else if let Some(main_def) = find_main_defn(funcs) {
        if main_def.params.is_empty() {
            out.push_str(&format!("        let _ = {}();\n", main_def.name));
        } else if main_def.params.len() == 1 && main_def.params[0].is_rest {
            if let Some(arg_expr) = render_main_rest_args(&main_def.params[0].ty) {
                out.push_str(&format!(
                    "        let _ = {}({});\n",
                    main_def.name, arg_expr
                ));
            }
        }
    }
    out.push_str("    });\n");
    out.push_str("    clove_core::profiler::print_report_if_enabled();\n");
    out.push_str("}\n");
    out
}

fn render_main_rest_args(param_ty: &Type) -> Option<String> {
    match param_ty {
        Type::Vector(inner) => match inner.as_ref() {
            Type::Prim(PrimType::Str) => {
                Some("std::env::args().skip(1).collect::<Vec<String>>()".into())
            }
            Type::Any | Type::Var(_) => {
                Some("std::env::args().skip(1).map(Value::String).collect::<Vec<Value>>()".into())
            }
            _ => None,
        },
        Type::Any | Type::Var(_) => Some(format!(
            "Value::Vector(clove_core::ast::Vector::from({}))",
            "std::env::args().skip(1).map(Value::String).collect::<Vec<Value>>()"
        )),
        _ => None,
    }
}

fn render_fn(def: &Defn, base_ctx: &RenderCtx) -> String {
    let mut ctx = base_ctx.clone();
    for param in &def.params {
        ctx = ctx.with_binding(&param.name, &param.ident, &param.ty);
    }
    let mut out = String::new();
    out.push_str("fn ");
    out.push_str(&def.name);
    out.push('(');
    for (i, param) in def.params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.ident);
        out.push_str(": ");
        out.push_str(&rust_type(&param.ty));
    }
    out.push(')');
    out.push_str(" -> ");
    out.push_str(&rust_type(&def.ret_ty));
    out.push_str(" {\n");
    // body: return the last expression
    let mut last_expr = String::new();
    for (idx, expr) in def.body.iter().enumerate() {
        let rendered = render_expr(expr, &ctx);
        if idx + 1 == def.body.len() {
            last_expr = rendered;
        } else {
            out.push_str("    let _ = ");
            out.push_str(&rendered);
            out.push_str(";\n");
        }
    }
    out.push_str("    ");
    if matches!(def.ret_ty, Type::Any | Type::Var(_)) {
        out.push_str(&render_value_expr(&def.body.last().unwrap(), &ctx));
    } else {
        out.push_str(&last_expr);
    }
    out.push_str("\n}\n");
    out
}

fn render_def_static(def: &Def) -> String {
    format!(
        "static {}: OnceLock<{}> = OnceLock::new();\n",
        def.ident,
        rust_type(&def.ty)
    )
}

fn render_def_init(def: &Def, ctx: &RenderCtx) -> String {
    let value = render_expr(&def.value, ctx);
    format!("{}.set({})", def.ident, value)
}

fn profile_label_for_expr(expr: &TypedExpr, ctx: &RenderCtx) -> Option<String> {
    let TypedExprKind::Call { callee, .. } = &expr.kind else {
        return None;
    };
    let TypedExprKind::Symbol(name) = &callee.kind else {
        return None;
    };
    if ctx.is_local(name) {
        return None;
    }
    Some(name.clone())
}

fn wrap_profile(label: &str, expr: &str) -> String {
    format!(
        "{{ let _prof = clove_core::profiler::enter({label}); let __out = {expr}; __out }}",
        label = format!("{:?}", label),
        expr = expr
    )
}

fn render_expr(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let (mut rendered, already_option) = render_expr_inner(expr, ctx);
    if matches!(expr.ty, Type::Option(_)) && !already_option {
        rendered = format!("Some({})", rendered);
    }
    if let Some(label) = profile_label_for_expr(expr, ctx) {
        rendered = wrap_profile(&label, &rendered);
    }
    rendered
}

fn render_expr_inner(expr: &TypedExpr, ctx: &RenderCtx) -> (String, bool) {
    if expr_needs_runtime(expr, ctx) && runtime_fallback_supported(expr, ctx) {
        return (
            render_runtime_fallback(expr, ctx),
            matches!(expr.ty, Type::Option(_)),
        );
    }
    match &expr.kind {
        TypedExprKind::Literal(lit) => {
            let literal_as_value = matches!(&expr.ty, Type::Any | Type::Var(_))
                || matches!(&expr.ty, Type::Option(inner) if matches!(inner.as_ref(), Type::Any | Type::Var(_)));
            match lit {
                Literal::Int(n) => {
                    if literal_as_value {
                        (format!("Value::Int({}i64)", n), false)
                    } else {
                        (format!("{}i64", n), false)
                    }
                }
                Literal::Float(f) => {
                    if literal_as_value {
                        (format!("Value::Float({}f64)", f), false)
                    } else {
                        (format!("{}f64", f), false)
                    }
                }
                Literal::Bool(b) => {
                    if literal_as_value {
                        (format!("Value::Bool({})", b), false)
                    } else {
                        (b.to_string(), false)
                    }
                }
                Literal::Str(s) => {
                    if is_regex_type(&expr.ty) {
                        return (
                            format!(
                                "clove_core::ast::RegexValue::new({:?})\
                                 .unwrap_or_else(|err| panic!(\"invalid regex: {{}}\", err))",
                                s
                            ),
                            false,
                        );
                    }
                    if literal_as_value {
                        (format!("Value::String(String::from({:?}))", s), false)
                    } else {
                        (format!("String::from({:?})", s), false)
                    }
                }
                Literal::Keyword(s) => {
                    let symbol = format!(":{}", s);
                    if literal_as_value {
                        (format!("Value::Symbol(String::from({:?}))", symbol), false)
                    } else if is_symbol_type(&expr.ty) {
                        (format!("String::from({:?})", symbol), false)
                    } else {
                        (format!("String::from({:?})", s), false)
                    }
                }
                Literal::Nil => {
                    if matches!(expr.ty, Type::Option(_)) {
                        ("None".to_string(), true)
                    } else if literal_as_value {
                        ("Value::Nil".to_string(), false)
                    } else {
                        ("()".to_string(), false)
                    }
                }
            }
        }
        TypedExprKind::Symbol(s) => {
            if let Some(local_ty) = ctx.local_ty(s) {
                if matches!(local_ty, Type::Any | Type::Var(_))
                    && !matches!(expr.ty, Type::Any | Type::Var(_))
                {
                    let rendered = format!("({}).clone()", render_symbol(s));
                    let conv = render_value_to_rust("__v", &expr.ty);
                    return (
                        format!("{{ let __v = {}; {} }}", rendered, conv),
                        matches!(expr.ty, Type::Option(_)),
                    );
                }
            }
            if let Some(ident) = ctx.global_var_ident(s) {
                return (
                    format!(
                        "{}.get().expect(\"def {} not initialized\").clone()",
                        ident, s
                    ),
                    matches!(expr.ty, Type::Option(_)),
                );
            }
            (render_symbol(s), matches!(expr.ty, Type::Option(_)))
        }
        TypedExprKind::Call { callee, args } => {
            if is_regex_type(&callee.ty) {
                return (render_regex_call(callee, args, ctx), false);
            }
            if let TypedExprKind::Symbol(name) = &callee.kind {
                match name.as_str() {
                    "+" if args.len() == 2 => {
                        return (
                            render_variadic_op(args, "+", "0i64", "0f64", &expr.ty, ctx),
                            false,
                        );
                    }
                    "-" if args.len() == 2 => {
                        return (render_minus(args, &expr.ty, ctx), false);
                    }
                    "*" if args.len() == 2 => {
                        return (
                            render_variadic_op(args, "*", "1i64", "1f64", &expr.ty, ctx),
                            false,
                        );
                    }
                    "/" if args.len() == 2 => {
                        return (render_div(args, &expr.ty, ctx), false);
                    }
                    "=" if args.len() == 2 => {
                        return (render_eq(args, ctx), false);
                    }
                    "not=" if args.len() == 2 => {
                        return (render_not_eq(args, ctx), false);
                    }
                    "and" => return (render_variadic_and(args, ctx), false),
                    "or" => return (render_variadic_or(args, ctx), false),
                    "not" if args.len() == 1 => {
                        return (format!("!{}", render_expr(&args[0], ctx)), false);
                    }
                    "cond" => {
                        let expect_option = matches!(expr.ty, Type::Option(_));
                        let expect_value = matches!(expr.ty, Type::Any | Type::Var(_));
                        if cond_supported(args) {
                            return (
                                render_cond(args, expect_option, expect_value, ctx),
                                expect_option,
                            );
                        }
                    }
                    "when" => {
                        let expect_option = matches!(expr.ty, Type::Option(_));
                        let expect_value = matches!(expr.ty, Type::Any | Type::Var(_));
                        if args.len() >= 2 {
                            return (
                                render_when(args, expect_option, expect_value, false, ctx),
                                expect_option,
                            );
                        }
                    }
                    "when-not" => {
                        let expect_option = matches!(expr.ty, Type::Option(_));
                        let expect_value = matches!(expr.ty, Type::Any | Type::Var(_));
                        if args.len() >= 2 {
                            return (
                                render_when(args, expect_option, expect_value, true, ctx),
                                expect_option,
                            );
                        }
                    }
                    "ns" | "deftype" | "defenum" => {
                        return ("()".to_string(), false);
                    }
                    "as->" => {
                        return (render_as_thread(args, ctx), false);
                    }
                    "println" | "print" | "prn" | "puts" => {
                        return (render_print_call(name, args, ctx), false);
                    }
                    "pr-str" => {
                        return (render_pr_str(args, ctx), false);
                    }
                    "match" => {
                        if match_supported(args) {
                            return (render_match(args, ctx), false);
                        }
                    }
                    "try" => {
                        let expect_option = matches!(expr.ty, Type::Option(_));
                        if try_supported(args) {
                            return (render_try(args, expect_option, ctx), expect_option);
                        }
                    }
                    "throw" => {
                        if throw_supported(args) {
                            return (render_throw(args, ctx), false);
                        }
                    }
                    "map" => {
                        if map_supported(args, ctx) {
                            return (render_map(args, ctx), false);
                        }
                    }
                    "map-indexed" => {
                        if map_indexed_supported(args, ctx) {
                            return (render_map_indexed(args, ctx), false);
                        }
                    }
                    "filter" => {
                        if filter_supported(args, ctx) {
                            return (render_filter(args, ctx), false);
                        }
                    }
                    "pfilter" => {
                        if pfilter_supported(args, ctx) {
                            return (render_pfilter(args, ctx), false);
                        }
                    }
                    "pmap" => {
                        if pmap_supported(args, ctx) {
                            return (render_pmap(args, ctx), false);
                        }
                    }
                    "doseq" | "each" => {
                        if doseq_supported(args) {
                            return (render_doseq(args, ctx), false);
                        }
                    }
                    "reduce" => {
                        if reduce_supported(args, ctx) {
                            return (render_reduce(args, ctx), false);
                        }
                    }
                    "update" => {
                        if update_supported(args, ctx) {
                            return (render_update(args, ctx), false);
                        }
                    }
                    "vector" | "list" => {
                        return (render_vector_ctor(args, ctx), false);
                    }
                    "hash-map" => {
                        return (render_hash_map_ctor(args, ctx), false);
                    }
                    "hash-set" => {
                        return (render_hash_set_ctor(args, ctx), false);
                    }
                    "loop" => {
                        return (render_loop_expr(args, ctx), false);
                    }
                    ">" if args.len() == 2 => {
                        return (render_numeric_cmp(args, ">", ctx), false);
                    }
                    "<" if args.len() == 2 => {
                        return (render_numeric_cmp(args, "<", ctx), false);
                    }
                    ">=" if args.len() == 2 => {
                        return (render_numeric_cmp(args, ">=", ctx), false);
                    }
                    "<=" if args.len() == 2 => {
                        return (render_numeric_cmp(args, "<=", ctx), false);
                    }
                    "inc" if args.len() == 1 => {
                        return (render_inc_dec(args, &expr.ty, true, ctx), false);
                    }
                    "str" => {
                        return (render_format_call(args, "", ctx), false);
                    }
                    "includes?" if args.len() == 2 => {
                        return (render_includes(args, ctx), false);
                    }
                    "string::includes?" | "str::includes?" if args.len() == 2 => {
                        return (render_string_includes(args, ctx), false);
                    }
                    "glob" | "fs::glob" | "io::glob" | "std::glob" if args.len() == 1 => {
                        return (render_glob_eager(args, ctx), false);
                    }
                    "glob*" | "fs::glob*" | "io::glob*" | "std::glob*" if args.len() == 1 => {
                        return (render_glob(args, ctx), false);
                    }
                    "json::read-file" if args.len() == 1 => {
                        return (render_json_read_file(args, ctx), false);
                    }
                    "abs" if args.len() == 1 => {
                        return (format!("{}.abs()", render_expr(&args[0], ctx)), false);
                    }
                    "max" => return (render_extremum(args, true, ctx), false),
                    "min" => return (render_extremum(args, false, ctx), false),
                    "count" if args.len() == 1 => {
                        return (render_count(&args[0], ctx), false);
                    }
                    "empty?" if args.len() == 1 => {
                        return (render_empty(&args[0], ctx), false);
                    }
                    "nth" if args.len() == 2 => {
                        return (render_nth(&args[0], &args[1], ctx), true);
                    }
                    "into" if args.len() == 2 => {
                        return (render_into(&args[0], &args[1], ctx), false);
                    }
                    "assoc" if args.len() >= 3 && args.len() % 2 == 1 => {
                        return (render_assoc_multi(&args[0], &args[1..], ctx), false);
                    }
                    "dissoc" if args.len() == 2 => {
                        return (render_dissoc(&args[0], &args[1], ctx), false);
                    }
                    "get" if args.len() == 2 => {
                        return (render_get(&args[0], &args[1], None, ctx), true);
                    }
                    "get" if args.len() == 3 => {
                        return (render_get(&args[0], &args[1], Some(&args[2]), ctx), false);
                    }
                    "contains?" if args.len() == 2 => {
                        return (render_contains(&args[0], &args[1], ctx), false);
                    }
                    "subs" => {
                        return (render_subs(args, ctx), false);
                    }
                    "union" | "std::union" | "set::union" => {
                        return (render_union(args, ctx), false)
                    }
                    "intersection" | "std::intersection" | "set::intersection" => {
                        return (render_intersection(args, ctx), false)
                    }
                    "difference" | "std::difference" | "set::difference" => {
                        return (render_difference(args, ctx), false)
                    }
                    "first" if args.len() == 1 => {
                        return (render_first(&args[0], ctx), true);
                    }
                    "rest" if args.len() == 1 => {
                        return (render_rest(&args[0], ctx), false);
                    }
                    "conj" if args.len() == 2 => {
                        return (render_conj(&args[0], &args[1], &expr.ty, ctx), false);
                    }
                    "disj" if args.len() >= 1 => {
                        return (render_disj(&args[0], &args[1..], &expr.ty, ctx), false);
                    }
                    _ => {}
                }
            }
            let target = render_expr(callee, ctx);
            let rendered_args = render_call_args(callee, args, ctx);
            (
                format!("{}({})", target, rendered_args.join(", ")),
                matches!(expr.ty, Type::Option(_)),
            )
        }
        TypedExprKind::Vector(items) => {
            let rendered: Vec<String> = items.iter().map(|e| render_expr(e, ctx)).collect();
            (format!("vec![{}]", rendered.join(", ")), false)
        }
        TypedExprKind::Map(map) => (render_record_map(map, ctx), false),
        TypedExprKind::Let { bindings, body } => (
            render_let(bindings, body, ctx),
            matches!(expr.ty, Type::Option(_)),
        ),
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let expect_option = matches!(expr.ty, Type::Option(_));
            let expect_value = matches!(expr.ty, Type::Any | Type::Var(_));
            (
                render_if(
                    cond,
                    then_branch,
                    else_branch,
                    expect_option,
                    expect_value,
                    ctx,
                ),
                expect_option,
            )
        }
        TypedExprKind::Do(items) => (render_do(items, ctx), matches!(expr.ty, Type::Option(_))),
        TypedExprKind::Fn { params, body } => (
            render_closure(params, body, &expr.ty, ctx),
            matches!(expr.ty, Type::Option(_)),
        ),
        _ => ("unimplemented!()".to_string(), false),
    }
}

fn render_apply_fn(
    func: &TypedExpr,
    arg_exprs: &[String],
    arg_tys: &[Type],
    ctx: &RenderCtx,
) -> String {
    if let TypedExprKind::Symbol(name) = &func.kind {
        if ctx.is_local(name) || ctx.is_global_fn(name) {
            return format!("{}({})", render_symbol(name), arg_exprs.join(", "));
        }
        if let Some(rendered) = render_builtin_apply(name, arg_exprs, arg_tys) {
            return rendered;
        }
    }
    let target = render_expr(func, ctx);
    format!("({})({})", target, arg_exprs.join(", "))
}

fn render_call_args(callee: &TypedExpr, args: &[TypedExpr], ctx: &RenderCtx) -> Vec<String> {
    let argc = args.len();
    let expected = match &callee.ty {
        Type::Func(params, rest, _) => Some((params.clone(), rest.as_deref().cloned())),
        Type::Overloaded(cands) => cands.iter().find_map(|cand| match cand {
            Type::Func(params, rest, _) if callable_arity_matches(cand, argc) => {
                Some((params.clone(), rest.as_deref().cloned()))
            }
            _ => None,
        }),
        _ => None,
    };
    args.iter()
        .enumerate()
        .map(|(idx, arg)| {
            let expected_ty = expected.as_ref().and_then(|(params, rest)| {
                if idx < params.len() {
                    Some(&params[idx])
                } else {
                    rest.as_ref()
                }
            });
            render_arg_with_expected(arg, expected_ty, ctx)
        })
        .collect()
}

fn render_arg_with_expected(arg: &TypedExpr, expected: Option<&Type>, ctx: &RenderCtx) -> String {
    let rendered = render_expr(arg, ctx);
    let Some(expected_ty) = expected else {
        return rendered;
    };
    if matches!(arg.ty, Type::Any | Type::Var(_)) && matches!(expected_ty, Type::Any | Type::Var(_))
    {
        if let TypedExprKind::Symbol(name) = &arg.kind {
            return format!("{}.clone()", render_symbol(name));
        }
    }
    if matches!(arg.ty, Type::Any | Type::Var(_))
        && !matches!(expected_ty, Type::Any | Type::Var(_))
    {
        let conv = render_value_to_rust("__v", expected_ty);
        return format!("{{ let __v = ({}).clone(); {} }}", rendered, conv);
    }
    if let TypedExprKind::Symbol(name) = &arg.kind {
        if should_clone_symbol_arg(&arg.ty) {
            return format!("{}.clone()", render_symbol(name));
        }
    }
    rendered
}

fn should_clone_symbol_arg(ty: &Type) -> bool {
    !matches!(
        ty,
        Type::Prim(PrimType::Int)
            | Type::Prim(PrimType::Float)
            | Type::Prim(PrimType::Bool)
            | Type::Prim(PrimType::Nil)
            | Type::Func(_, _, _)
            | Type::Overloaded(_)
            | Type::Opaque(_)
    )
}

fn render_try(args: &[TypedExpr], expect_option: bool, ctx: &RenderCtx) -> String {
    let Some((body, finally_body)) = parse_try_finally(args) else {
        return "unimplemented!()".into();
    };
    let body_exprs: Vec<TypedExpr> = body.into_iter().cloned().collect();
    let finally_exprs: Vec<TypedExpr> = finally_body.into_iter().cloned().collect();
    let body_s = if expect_option {
        render_option_block(&body_exprs, ctx)
    } else {
        render_block(&body_exprs, ctx)
    };
    let finally_s = format!("{{ let _ = {}; }}", render_block(&finally_exprs, ctx));
    let mut out = String::from("{");
    out.push_str(" struct __CloveFinallyGuard<F: FnOnce()> { f: Option<F> }");
    out.push_str(
        " impl<F: FnOnce()> Drop for __CloveFinallyGuard<F> { fn drop(&mut self) { if let Some(f) = self.f.take() { f(); } } }",
    );
    out.push_str(&format!(
        " let mut __guard = __CloveFinallyGuard {{ f: Some(|| {}) }};",
        finally_s
    ));
    out.push_str(&format!(" let __result = {};", body_s));
    out.push_str(" if let Some(__f) = __guard.f.take() { __f(); }");
    out.push_str(" __result }");
    out
}

fn render_throw(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 1 {
        return "panic!(\"throw expects one argument\")".into();
    }
    let value = render_expr(&args[0], ctx);
    format!("panic!(\"{{:?}}\", {})", value)
}

fn render_builtin_apply(name: &str, arg_exprs: &[String], arg_tys: &[Type]) -> Option<String> {
    let result_is_int = arg_tys.iter().all(is_int_type);
    match name {
        "+" => match arg_exprs.len() {
            0 => Some(if result_is_int { "0i64" } else { "0f64" }.into()),
            1 => Some(arg_exprs[0].clone()),
            _ => {
                let parts: Vec<String> = arg_exprs
                    .iter()
                    .zip(arg_tys)
                    .map(|(expr, ty)| render_number_as_f64(expr, ty))
                    .collect();
                let joined = parts.join(" + ");
                if result_is_int {
                    Some(format!("({}) as i64", joined))
                } else {
                    Some(joined)
                }
            }
        },
        "-" => match arg_exprs.len() {
            0 => Some(if result_is_int { "0i64" } else { "0f64" }.into()),
            1 => {
                let expr = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                if result_is_int {
                    Some(format!("(-{}) as i64", expr))
                } else {
                    Some(format!("-{}", expr))
                }
            }
            _ => {
                let parts: Vec<String> = arg_exprs
                    .iter()
                    .zip(arg_tys)
                    .map(|(expr, ty)| render_number_as_f64(expr, ty))
                    .collect();
                let joined = parts.join(" - ");
                if result_is_int {
                    Some(format!("({}) as i64", joined))
                } else {
                    Some(joined)
                }
            }
        },
        "*" => match arg_exprs.len() {
            0 => Some(if result_is_int { "1i64" } else { "1f64" }.into()),
            1 => Some(arg_exprs[0].clone()),
            _ => {
                let parts: Vec<String> = arg_exprs
                    .iter()
                    .zip(arg_tys)
                    .map(|(expr, ty)| render_number_as_f64(expr, ty))
                    .collect();
                let joined = parts.join(" * ");
                if result_is_int {
                    Some(format!("({}) as i64", joined))
                } else {
                    Some(joined)
                }
            }
        },
        "/" => match arg_exprs.len() {
            0 => Some("1f64".into()),
            1 => Some(format!(
                "1f64 / {}",
                render_number_as_f64(&arg_exprs[0], &arg_tys[0])
            )),
            _ => {
                let parts: Vec<String> = arg_exprs
                    .iter()
                    .zip(arg_tys)
                    .map(|(expr, ty)| render_number_as_f64(expr, ty))
                    .collect();
                Some(parts.join(" / "))
            }
        },
        "=" => {
            if arg_exprs.len() == 2 {
                Some(format!("{} == {}", arg_exprs[0], arg_exprs[1]))
            } else {
                None
            }
        }
        "not=" => {
            if arg_exprs.len() == 2 {
                Some(format!("{} != {}", arg_exprs[0], arg_exprs[1]))
            } else {
                None
            }
        }
        ">" | "<" | ">=" | "<=" => {
            if arg_exprs.len() == 2 {
                let left = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                let right = render_number_as_f64(&arg_exprs[1], &arg_tys[1]);
                Some(format!("{} {} {}", left, name, right))
            } else {
                None
            }
        }
        "inc" => {
            if arg_exprs.len() == 1 {
                let expr = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                if result_is_int {
                    Some(format!("({} + 1.0) as i64", expr))
                } else {
                    Some(format!("{} + 1.0", expr))
                }
            } else {
                None
            }
        }
        "dec" => {
            if arg_exprs.len() == 1 {
                let expr = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                if result_is_int {
                    Some(format!("({} - 1.0) as i64", expr))
                } else {
                    Some(format!("{} - 1.0", expr))
                }
            } else {
                None
            }
        }
        "max" => {
            if arg_exprs.len() == 2 {
                let left_cmp = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                let right_cmp = render_number_as_f64(&arg_exprs[1], &arg_tys[1]);
                Some(format!(
                    "if {ac} > {bc} {{ {a} }} else {{ {b} }}",
                    ac = left_cmp,
                    bc = right_cmp,
                    a = arg_exprs[0],
                    b = arg_exprs[1]
                ))
            } else {
                None
            }
        }
        "min" => {
            if arg_exprs.len() == 2 {
                let left_cmp = render_number_as_f64(&arg_exprs[0], &arg_tys[0]);
                let right_cmp = render_number_as_f64(&arg_exprs[1], &arg_tys[1]);
                Some(format!(
                    "if {ac} < {bc} {{ {a} }} else {{ {b} }}",
                    ac = left_cmp,
                    bc = right_cmp,
                    a = arg_exprs[0],
                    b = arg_exprs[1]
                ))
            } else {
                None
            }
        }
        "vector" | "list" => Some(format!("vec![{}]", arg_exprs.join(", "))),
        _ => None,
    }
}

fn render_runtime_fallback(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let source_literal = format!("{:?}", expr.source);
    let mut out = String::from("{");
    out.push_str(" let mut __src = String::new();");
    if ctx.locals.is_empty() {
        out.push_str(&format!(" __src.push_str({});", source_literal));
    } else {
        out.push_str(" __src.push_str(\"(let [\");");
        for local in &ctx.locals {
            if local.name == "&" || local.ident == "_" {
                continue;
            }
            let name_lit = format!("{:?}", local.name);
            let val_expr = render_value_from_var(&local.ident, &local.ty);
            out.push_str(&format!(
                " __src.push_str({}); __src.push(' '); __src.push_str(&({}).to_string()); __src.push(' ');",
                name_lit,
                val_expr
            ));
        }
        out.push_str(" __src.push_str(\"] \");");
        out.push_str(&format!(" __src.push_str({});", source_literal));
        out.push_str(" __src.push(')');");
    }
    out.push_str(" let __value = RuntimeCtx::try_with_current(|ctx| ctx.eval_source(&__src))");
    out.push_str(
        ".unwrap_or_else(|| { let __ctx = RuntimeCtx::new(__clove_runtime_opts(), &[]); __ctx.eval_source(&__src) });",
    );
    out.push_str(
        " let __value = __value.unwrap_or_else(|err| panic!(\"runtime fallback failed: {}\", err));",
    );
    out.push_str(&format!(" {} ", render_value_to_rust("__value", &expr.ty)));
    out.push_str(" }");
    out
}

fn format_placeholder(ty: &Type) -> &'static str {
    match ty {
        Type::Prim(PrimType::Str)
        | Type::Prim(PrimType::Int)
        | Type::Prim(PrimType::Float)
        | Type::Prim(PrimType::Bool)
        | Type::Prim(PrimType::Nil)
        | Type::Any
        | Type::Var(_) => "{}",
        _ => "{:?}",
    }
}

fn render_format_call(args: &[TypedExpr], sep: &str, ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "String::new()".into();
    }
    let mut fmt = String::new();
    for (idx, arg) in args.iter().enumerate() {
        if idx > 0 {
            fmt.push_str(sep);
        }
        fmt.push_str(format_placeholder(&arg.ty));
    }
    let fmt_literal = format!("{:?}", fmt);
    let rendered_args: Vec<String> = args.iter().map(|e| render_expr(e, ctx)).collect();
    format!("format!({}, {})", fmt_literal, rendered_args.join(", "))
}

fn render_print_call(name: &str, args: &[TypedExpr], ctx: &RenderCtx) -> String {
    let macro_name = if name == "print" { "print" } else { "println" };
    if args.is_empty() {
        return if macro_name == "print" {
            "print!(\"\")".into()
        } else {
            "println!()".into()
        };
    }
    let rendered = render_format_call(args, " ", ctx);
    format!("{macro_name}!(\"{}\", {rendered})", "{}")
}

fn render_match(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 3 {
        return "unimplemented!()".into();
    }
    let target = render_expr(&args[0], ctx);
    let mut idx = 1;
    let mut out = String::new();
    let mut has_branch = false;
    let mut default_expr: Option<String> = None;
    while idx + 1 < args.len() {
        let pat = &args[idx];
        let expr = &args[idx + 1];
        if is_default_match_pattern(pat) {
            default_expr = Some(format!("{{ {} }}", render_expr(expr, ctx)));
            break;
        }
        let cond = format!("{} == {}", target, render_expr(pat, ctx));
        let body = format!("{{ {} }}", render_expr(expr, ctx));
        if !has_branch {
            out = format!("if {} {}", cond, body);
            has_branch = true;
        } else {
            out = format!("{} else if {} {}", out, cond, body);
        }
        idx += 2;
    }
    if let Some(default_body) = default_expr {
        if has_branch {
            format!("{} else {}", out, default_body)
        } else {
            default_body
        }
    } else if has_branch {
        format!("{} else {{ panic!(\"match failed\") }}", out)
    } else {
        "unimplemented!()".into()
    }
}

fn render_record_map(map: &HashMap<String, TypedExpr>, ctx: &RenderCtx) -> String {
    let mut out = String::from("{ let mut m = std::collections::HashMap::new();");
    for (k, v) in map {
        out.push_str(&format!(
            " m.insert(String::from({}), {});",
            format!("{:?}", k),
            render_value_expr(v, ctx)
        ));
    }
    out.push_str(" m }");
    out
}

fn render_value_expr(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    match &expr.ty {
        Type::Prim(_) => render_value_from_var(&render_expr(expr, ctx), &expr.ty),
        Type::Any | Type::Var(_) | Type::Record(_) => {
            render_value_from_var(&render_expr(expr, ctx), &expr.ty)
        }
        Type::Opaque(_) if is_regex_type(&expr.ty) => {
            render_value_from_var(&render_expr(expr, ctx), &expr.ty)
        }
        Type::Option(inner) => {
            let rendered = render_expr(expr, ctx);
            let inner_value = render_value_from_var("__inner", inner);
            format!(
                "{{ let __v = {v}; match __v {{ Some(__inner) => {inner_value}, None => Value::Nil }} }}",
                v = rendered
            )
        }
        Type::Vector(inner) => {
            let rendered = render_expr(expr, ctx);
            let elem_value = render_value_from_var("__item", inner);
            format!(
                "{{ let __v = {v}; Value::Vector(__v.into_iter().map(|__item| {elem}).collect()) }}",
                v = rendered,
                elem = elem_value
            )
        }
        Type::Set(inner) => {
            let rendered = render_expr(expr, ctx);
            let elem_value = render_value_from_var("__item", inner);
            format!(
                "{{ let __v = {v}; Value::Set(__v.into_iter().map(|__item| {elem}).collect()) }}",
                v = rendered,
                elem = elem_value
            )
        }
        _ => "Value::Nil".into(),
    }
}

fn render_value_expr_with_ctx(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    if let TypedExprKind::Symbol(name) = &expr.kind {
        if let Some(local_ty) = ctx.local_ty(name) {
            return render_value_from_var(&render_symbol(name), local_ty);
        }
    }
    render_value_expr(expr, ctx)
}

fn render_value_from_var(var: &str, ty: &Type) -> String {
    match ty {
        Type::Prim(PrimType::Int) => format!("Value::Int({})", var),
        Type::Prim(PrimType::Float) => format!("Value::Float({})", var),
        Type::Prim(PrimType::Bool) => format!("Value::Bool({})", var),
        Type::Prim(PrimType::Str) => format!("Value::String({}.clone())", var),
        Type::Prim(PrimType::Nil) => "Value::Nil".into(),
        Type::Opaque(_) if is_regex_type(ty) => format!("Value::Regex({}.clone())", var),
        Type::Opaque(name) if is_symbol_type_name(name) => {
            format!("Value::Symbol({}.clone())", var)
        }
        Type::Any | Type::Var(_) => format!("{}.clone()", var),
        Type::Option(inner) => {
            let inner_value = render_value_from_var("__inner", inner);
            format!(
                "{{ match {v}.clone() {{ Some(__inner) => {inner_value}, None => Value::Nil }} }}",
                v = var
            )
        }
        Type::Vector(inner) => {
            let elem_value = render_value_from_var("__item", inner);
            format!(
                "{{ Value::Vector({v}.clone().into_iter().map(|__item| {elem}).collect()) }}",
                v = var,
                elem = elem_value
            )
        }
        Type::Set(inner) => {
            let elem_value = render_value_from_var("__item", inner);
            format!(
                "{{ Value::Set({v}.clone().into_iter().map(|__item| {elem}).collect()) }}",
                v = var,
                elem = elem_value
            )
        }
        Type::Record(_) => {
            format!(
                "{{ let mut __m = std::collections::HashMap::new(); for (__k, __v) in {v}.clone() {{ __m.insert(Key::Keyword(__k), __v); }} Value::Map(__m.into()) }}",
                v = var
            )
        }
        _ => "Value::Nil".into(),
    }
}

fn render_value_to_rust(var: &str, ty: &Type) -> String {
    match ty {
        Type::Prim(PrimType::Int) => format!(
            "match {v} {{ Value::Int(__n) => __n, other => panic!(\"expected int, got {{}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Float) => format!(
            "match {v} {{ Value::Float(__n) => __n, Value::Int(__n) => __n as f64, other => panic!(\"expected float, got {{}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Bool) => format!(
            "match {v} {{ Value::Bool(__b) => __b, other => panic!(\"expected bool, got {{}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Str) => format!(
            "match {v} {{ Value::String(__s) => __s, other => panic!(\"expected str, got {{}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Nil) => format!(
            "match {v} {{ Value::Nil => (), other => panic!(\"expected nil, got {{}}\", other) }}",
            v = var
        ),
        Type::Option(inner) => {
            let inner_conv = render_value_to_rust("__inner", inner);
            format!(
                "match {v} {{ Value::Nil => None, __inner => Some({inner}) }}",
                v = var,
                inner = inner_conv
            )
        }
        Type::Vector(inner) => {
            let inner_conv = render_value_to_rust("__item", inner);
            format!(
                "match {v} {{ Value::Vector(__items) => __items.into_iter().map(|__item| {inner}).collect::<Vec<_>>(), other => panic!(\"expected vector, got {{}}\", other) }}",
                v = var,
                inner = inner_conv
            )
        }
        Type::Set(inner) => {
            let inner_conv = render_value_to_rust("__item", inner);
            format!(
                "match {v} {{ Value::Set(__items) => __items.into_iter().map(|__item| {inner}).collect::<std::collections::HashSet<_>>(), other => panic!(\"expected set, got {{}}\", other) }}",
                v = var,
                inner = inner_conv
            )
        }
        Type::Map(key_ty, val_ty) => {
            let key_conv = render_key_to_rust("__k", key_ty);
            let val_conv = render_value_to_rust("__v", val_ty);
            format!(
                "match {v} {{ Value::Map(__items) => {{ let mut __out = std::collections::HashMap::new(); for (__k, __v) in __items {{ let __key = {key}; let __val = {val}; __out.insert(__key, __val); }} __out }}, other => panic!(\"expected map, got {{}}\", other) }}",
                v = var,
                key = key_conv,
                val = val_conv
            )
        }
        Type::Record(_) => format!(
            "match {v} {{ Value::Map(__items) => {{ let mut __out = std::collections::HashMap::new(); for (__k, __v) in __items {{ let __key = match __k {{ Key::Keyword(__s) | Key::String(__s) | Key::Symbol(__s) => __s, Key::Number(__n) => __n.to_string(), Key::Bool(__b) => __b.to_string(), }}; __out.insert(__key, __v); }} __out }}, other => panic!(\"expected map, got {{}}\", other) }}",
            v = var
        ),
        Type::Opaque(name) if is_symbol_type_name(name) => format!(
            "match {v} {{ Value::Symbol(__s) => __s, other => panic!(\"expected symbol, got {{}}\", other) }}",
            v = var
        ),
        Type::Any | Type::Var(_) => format!("{v}", v = var),
        _ => "panic!(\"unsupported runtime fallback type\")".into(),
    }
}

fn render_key_to_rust(var: &str, ty: &Type) -> String {
    match ty {
        Type::Prim(PrimType::Str) => format!(
            "match {v} {{ Key::Keyword(__s) | Key::String(__s) | Key::Symbol(__s) => __s, other => panic!(\"expected string key, got {{:?}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Int) => format!(
            "match {v} {{ Key::Number(__n) => __n, other => panic!(\"expected int key, got {{:?}}\", other) }}",
            v = var
        ),
        Type::Prim(PrimType::Bool) => format!(
            "match {v} {{ Key::Bool(__b) => __b, other => panic!(\"expected bool key, got {{:?}}\", other) }}",
            v = var
        ),
        Type::Any | Type::Var(_) => format!("clove_core::eval::key_to_value(&{v})", v = var),
        _ => "panic!(\"unsupported map key type\")".into(),
    }
}

fn rust_type(ty: &Type) -> String {
    match ty {
        Type::Opaque(_) if is_regex_type(ty) => "clove_core::ast::RegexValue".into(),
        Type::Prim(PrimType::Int) => "i64".into(),
        Type::Prim(PrimType::Float) => "f64".into(),
        Type::Prim(PrimType::Bool) => "bool".into(),
        Type::Prim(PrimType::Str) => "String".into(),
        Type::Prim(PrimType::Nil) => "()".into(),
        Type::Vector(inner) => format!("Vec<{}>", rust_type(inner)),
        Type::Option(inner) => format!("Option<{}>", rust_type(inner)),
        Type::Map(k, v) => format!(
            "std::collections::HashMap<{}, {}>",
            rust_type(k),
            rust_type(v)
        ),
        Type::Set(inner) => format!("std::collections::HashSet<{}>", rust_type(inner)),
        Type::Record(_) => "std::collections::HashMap<String, Value>".into(),
        Type::Any | Type::Var(_) => "Value".into(),
        _ => "String".into(), // fallback
    }
}

fn is_regex_type(ty: &Type) -> bool {
    matches!(ty, Type::Opaque(name) if name.ends_with("Regex"))
}

fn is_symbol_type(ty: &Type) -> bool {
    match ty {
        Type::Opaque(name) => is_symbol_type_name(name),
        Type::Option(inner) => is_symbol_type(inner),
        _ => false,
    }
}

fn is_symbol_type_name(name: &str) -> bool {
    name == "core::Symbol"
}

fn render_symbol(name: &str) -> String {
    if is_builtin_symbol(name) {
        name.to_string()
    } else {
        sanitize_ident(name)
    }
}

fn is_builtin_symbol(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "="
            | "not="
            | "and"
            | "or"
            | "not"
            | "<"
            | ">"
            | "<="
            | ">="
            | "inc"
            | "str"
    )
}

fn sanitize_ident(raw: &str) -> String {
    let mut out = String::new();
    for (idx, ch) in raw.chars().enumerate() {
        let c = if ch.is_ascii_alphanumeric() || ch == '_' {
            ch.to_ascii_lowercase()
        } else {
            '_'
        };
        if idx == 0 && !(c.is_ascii_alphabetic() || c == '_') {
            out.push('_');
        }
        out.push(c);
    }
    if out.is_empty() {
        "_".into()
    } else {
        out
    }
}

fn render_block(exprs: &[TypedExpr], ctx: &RenderCtx) -> String {
    if exprs.is_empty() {
        return "{ () }".into();
    }
    let mut out = String::from("{");
    for (idx, e) in exprs.iter().enumerate() {
        if idx + 1 == exprs.len() {
            out.push_str(&format!(" {}", render_expr(e, ctx)));
        } else {
            out.push_str(&format!(" let _ = {};", render_expr(e, ctx)));
        }
    }
    out.push_str(" }");
    out
}

fn render_option_block(exprs: &[TypedExpr], ctx: &RenderCtx) -> String {
    if exprs.is_empty() {
        return "{ None }".into();
    }
    let mut out = String::from("{");
    for (idx, e) in exprs.iter().enumerate() {
        if idx + 1 == exprs.len() {
            out.push_str(&format!(" {}", render_expr_as_option(e, ctx)));
        } else {
            out.push_str(&format!(" let _ = {};", render_expr(e, ctx)));
        }
    }
    out.push_str(" }");
    out
}

fn render_value_block(exprs: &[TypedExpr], ctx: &RenderCtx) -> String {
    if exprs.is_empty() {
        return "{ Value::Nil }".into();
    }
    let mut out = String::from("{");
    for (idx, e) in exprs.iter().enumerate() {
        if idx + 1 == exprs.len() {
            out.push_str(&format!(" {}", render_value_expr(e, ctx)));
        } else {
            out.push_str(&format!(" let _ = {};", render_expr(e, ctx)));
        }
    }
    out.push_str(" }");
    out
}

fn render_expr_as_option(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    if matches!(expr.ty, Type::Option(_)) {
        return render_expr(expr, ctx);
    }
    match &expr.kind {
        TypedExprKind::Literal(Literal::Nil) => "None".into(),
        _ => format!("Some({})", render_expr(expr, ctx)),
    }
}

fn render_let(bindings: &[(String, TypedExpr)], body: &[TypedExpr], ctx: &RenderCtx) -> String {
    let mut out = String::from("{");
    let mut next_ctx = ctx.clone();
    for (name, expr) in bindings {
        let ident = sanitize_ident(name);
        out.push_str(&format!(
            " let {} = {};",
            ident,
            render_expr(expr, &next_ctx)
        ));
        next_ctx = next_ctx.with_binding(name, &ident, &expr.ty);
    }
    if body.is_empty() {
        out.push_str(" () ");
    } else {
        for (idx, expr) in body.iter().enumerate() {
            if idx + 1 == body.len() {
                out.push_str(&format!(" {}", render_expr(expr, &next_ctx)));
            } else {
                out.push_str(&format!(" let _ = {};", render_expr(expr, &next_ctx)));
            }
        }
    }
    out.push_str(" }");
    out
}

fn render_if(
    cond: &TypedExpr,
    then_branch: &[TypedExpr],
    else_branch: &[TypedExpr],
    expect_option: bool,
    expect_value: bool,
    ctx: &RenderCtx,
) -> String {
    let cond_s = render_truthy_expr(cond, ctx);
    if expect_option {
        let then_s = render_option_block(then_branch, ctx);
        let else_s = if else_branch.is_empty() {
            "{ None }".into()
        } else {
            render_option_block(else_branch, ctx)
        };
        return format!("if {} {} else {}", cond_s, then_s, else_s);
    }
    if expect_value {
        let then_s = render_value_block(then_branch, ctx);
        let else_s = if else_branch.is_empty() {
            "{ Value::Nil }".into()
        } else {
            render_value_block(else_branch, ctx)
        };
        return format!("if {} {} else {}", cond_s, then_s, else_s);
    }
    let then_s = render_block(then_branch, ctx);
    let else_s = if else_branch.is_empty() {
        "{ () }".into()
    } else {
        render_block(else_branch, ctx)
    };
    format!("if {} {} else {}", cond_s, then_s, else_s)
}

fn render_when(
    args: &[TypedExpr],
    expect_option: bool,
    expect_value: bool,
    invert: bool,
    ctx: &RenderCtx,
) -> String {
    let cond_expr = render_truthy_expr(&args[0], ctx);
    let cond_s = if invert {
        format!("!({})", cond_expr)
    } else {
        cond_expr
    };
    let then_s = if expect_option {
        render_option_block(&args[1..], ctx)
    } else if expect_value {
        render_value_block(&args[1..], ctx)
    } else {
        render_block(&args[1..], ctx)
    };
    let else_s = if expect_option {
        "{ None }"
    } else if expect_value {
        "{ Value::Nil }"
    } else {
        "{ () }"
    };
    format!("if {} {} else {}", cond_s, then_s, else_s)
}

fn render_cond(
    args: &[TypedExpr],
    expect_option: bool,
    expect_value: bool,
    ctx: &RenderCtx,
) -> String {
    let mut idx = args.len();
    let mut out = if expect_option {
        "{ None }".to_string()
    } else if expect_value {
        "{ Value::Nil }".to_string()
    } else {
        "{ () }".to_string()
    };
    while idx >= 2 {
        let test = &args[idx - 2];
        let expr = &args[idx - 1];
        let branch = if expect_option {
            render_option_block(std::slice::from_ref(expr), ctx)
        } else if expect_value {
            render_value_block(std::slice::from_ref(expr), ctx)
        } else {
            render_block(std::slice::from_ref(expr), ctx)
        };
        if is_cond_default_test(test) {
            out = branch;
        } else {
            let test_s = render_truthy_expr(test, ctx);
            out = format!("if {} {} else {}", test_s, branch, out);
        }
        idx -= 2;
    }
    out
}

fn render_do(items: &[TypedExpr], ctx: &RenderCtx) -> String {
    render_block(items, ctx)
}

fn render_closure(params: &[String], body: &[TypedExpr], ty: &Type, ctx: &RenderCtx) -> String {
    let mut next_ctx = ctx.clone();
    let mut param_tys = Vec::new();
    let mut ret_ty = None;
    let mut rest_ty: Option<Type> = None;
    if let Type::Func(args, rest, ret) = ty {
        param_tys = args.clone();
        rest_ty = rest.as_deref().cloned();
        ret_ty = Some(ret.as_ref().clone());
    }
    let (param_names, rest_idx) = split_params_with_rest(params);
    let params_s: Vec<String> = param_names
        .iter()
        .enumerate()
        .map(|(idx, p)| {
            let ident = sanitize_ident(p);
            let ty = match rest_idx {
                Some(rest_pos) if rest_pos == idx => rest_ty
                    .as_ref()
                    .map(|elem| Type::Vector(Box::new(elem.clone())))
                    .unwrap_or(Type::Any),
                _ => param_tys.get(idx).cloned().unwrap_or(Type::Any),
            };
            format!("{}: {}", ident, rust_type(&ty))
        })
        .collect();
    for (idx, name) in param_names.iter().enumerate() {
        let ident = sanitize_ident(name);
        let ty = match rest_idx {
            Some(rest_pos) if rest_pos == idx => rest_ty
                .as_ref()
                .map(|elem| Type::Vector(Box::new(elem.clone())))
                .unwrap_or(Type::Any),
            _ => param_tys.get(idx).cloned().unwrap_or(Type::Any),
        };
        next_ctx = next_ctx.with_binding(name, &ident, &ty);
    }
    let body_s = render_block(body, &next_ctx);
    if let Some(ret) = ret_ty {
        format!(
            "|{}| -> {} {}",
            params_s.join(", "),
            rust_type(&ret),
            body_s
        )
    } else {
        format!("|{}| {}", params_s.join(", "), body_s)
    }
}

fn split_params_with_rest(params: &[String]) -> (Vec<String>, Option<usize>) {
    let mut out = Vec::new();
    let mut rest_idx = None;
    let mut iter = params.iter();
    while let Some(name) = iter.next() {
        if name == "&" {
            if let Some(rest) = iter.next() {
                rest_idx = Some(out.len());
                out.push(rest.clone());
            }
            break;
        }
        out.push(name.clone());
    }
    (out, rest_idx)
}

fn render_vector_ctor(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    let rendered: Vec<String> = args.iter().map(|e| render_expr(e, ctx)).collect();
    format!("vec![{}]", rendered.join(", "))
}

fn render_hash_map_ctor(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    let mut out = String::from("{ let mut m = std::collections::HashMap::new();");
    let mut iter = args.iter();
    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
        out.push_str(&format!(
            " m.insert({}, {});",
            render_expr(k, ctx),
            render_expr(v, ctx)
        ));
    }
    out.push_str(" m }");
    out
}

fn render_hash_set_ctor(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    let mut out = String::from("{ let mut s = std::collections::HashSet::new();");
    for a in args {
        out.push_str(&format!(" s.insert({});", render_expr(a, ctx)));
    }
    out.push_str(" s }");
    out
}

fn render_map(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 2 {
        return "vec![]".into();
    }
    let func = &args[0];
    let colls = &args[1..];
    let mut out = String::from("{");
    for (idx, coll) in colls.iter().enumerate() {
        out.push_str(&format!(
            " let __c{} = {}.clone();",
            idx,
            render_expr(coll, ctx)
        ));
    }
    out.push_str(" let mut __len = __c0.len();");
    for idx in 1..colls.len() {
        out.push_str(&format!(" __len = __len.min(__c{}.len());", idx));
    }
    out.push_str(" let mut __out = Vec::with_capacity(__len);");
    out.push_str(" for __i in 0..__len {");
    let mut arg_exprs = Vec::new();
    let mut arg_tys = Vec::new();
    for (idx, coll) in colls.iter().enumerate() {
        arg_exprs.push(format!("__c{}[__i].clone()", idx));
        if let Type::Vector(inner) = &coll.ty {
            arg_tys.push((**inner).clone());
        }
    }
    let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
    out.push_str(&format!(" __out.push({});", call));
    out.push_str(" } __out }");
    out
}

fn render_map_indexed(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 2 {
        return "vec![]".into();
    }
    let func = &args[0];
    let coll = &args[1];
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let mut out = String::from("{");
    out.push_str(&format!(" let __c = {}.clone();", render_expr(coll, ctx)));
    out.push_str(" let mut __out = Vec::with_capacity(__c.len());");
    out.push_str(" for (__idx, __item) in __c.iter().cloned().enumerate() {");
    out.push_str(" let __i = __idx as i64;");
    let arg_exprs = vec!["__i".to_string(), "__item".to_string()];
    let arg_tys = vec![Type::Prim(PrimType::Int), elem_ty];
    let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
    out.push_str(&format!(" __out.push({});", call));
    out.push_str(" } __out }");
    out
}

fn render_doseq(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    let Some(bindings_form) = args.first() else {
        return "unimplemented!()".into();
    };
    let body = &args[1..];
    let Some(bindings) = parse_doseq_bindings(bindings_form) else {
        return "unimplemented!()".into();
    };
    let mut out = String::from("{");
    render_doseq_loops(&bindings, 0, ctx, body, &mut out);
    out.push_str(" () }");
    out
}

fn render_doseq_loops(
    bindings: &[(String, &TypedExpr)],
    idx: usize,
    ctx: &RenderCtx,
    body: &[TypedExpr],
    out: &mut String,
) {
    if idx >= bindings.len() {
        for expr in body {
            out.push_str(" let _ = ");
            out.push_str(&render_expr(expr, ctx));
            out.push_str(";");
        }
        return;
    }
    let (name, coll) = &bindings[idx];
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let ident = sanitize_ident(name);
    let coll_name = format!("__c{}", idx);
    let item_name = format!("__item{}", idx);
    out.push_str(&format!(
        " let {} = {}.clone();",
        coll_name,
        render_expr(coll, ctx)
    ));
    out.push_str(&format!(
        " for {} in {}.iter().cloned() {{",
        item_name, coll_name
    ));
    out.push_str(&format!(" let {} = {};", ident, item_name));
    let next_ctx = ctx.with_binding(name, &ident, &elem_ty);
    render_doseq_loops(bindings, idx + 1, &next_ctx, body, out);
    out.push_str(" }");
}

fn render_filter(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 2 {
        return "vec![]".into();
    }
    let func = &args[0];
    let coll = &args[1];
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let ret_ty = callable_return_type(func).unwrap_or(Type::Any);
    let keep_expr = truthy_expr_for_type("__keep", &ret_ty).unwrap_or_else(|| {
        "match __keep { Value::Bool(false) | Value::Nil => false, _ => true }".into()
    });
    let mut out = String::from("{");
    out.push_str(&format!(" let __c = {}.clone();", render_expr(coll, ctx)));
    out.push_str(" let mut __out = Vec::new();");
    out.push_str(" for __item in __c.iter().cloned() {");
    let arg_exprs = vec!["__item.clone()".to_string()];
    let arg_tys = vec![elem_ty];
    let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
    out.push_str(&format!(" let __keep = {};", call));
    out.push_str(&format!(" if {} {{ __out.push(__item); }}", keep_expr));
    out.push_str(" } __out }");
    out
}

fn render_pfilter(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 2 {
        return "Value::Vector(Vec::<Value>::new().into())".into();
    }
    let func = &args[0];
    let coll = &args[1];
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let param_ty = callable_first_param_type(func).unwrap_or(Type::Any);
    let arg_expr = render_pmap_arg(&param_ty, &elem_ty);
    let arg_tys = vec![param_ty];
    let call = render_apply_fn(func, &[arg_expr], &arg_tys, ctx);
    let ret_ty = callable_return_type(func).unwrap_or(Type::Any);
    let keep_expr = truthy_expr_for_type("__keep", &ret_ty).unwrap_or_else(|| {
        "match __keep { Value::Bool(false) | Value::Nil => false, _ => true }".into()
    });
    let limit_expr = if args.len() == 3 {
        if let Some(max) = pmap_max_parallel(&args[2]) {
            format!("{}usize", max)
        } else if let Some(expr) = pmap_opts_value(&args[2]) {
            format!("(({}) as usize)", render_expr(expr, ctx))
        } else {
            "1usize".into()
        }
    } else {
        "std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1)".into()
    };
    let item_value = render_value_from_var("__item", &elem_ty);
    let mut out = String::from("{");
    out.push_str(&format!(
        " let __items = {}.clone();",
        render_expr(coll, ctx)
    ));
    out.push_str(&format!(" let __limit = {}.max(1);", limit_expr));
    out.push_str(" let mut __results: Vec<Option<bool>> = vec![None; __items.len()];");
    out.push_str(" let mut __in_flight: Vec<(usize, std::thread::JoinHandle<bool>)> = Vec::new();");
    out.push_str(" for (__idx, __item) in __items.iter().cloned().enumerate() {");
    out.push_str(&format!(
        " let __handle = std::thread::spawn(move || {{ let __keep = {}; {} }});",
        call, keep_expr
    ));
    out.push_str(" __in_flight.push((__idx, __handle));");
    out.push_str(" if __in_flight.len() >= __limit {");
    out.push_str(" let (__slot, __handle) = __in_flight.remove(0);");
    out.push_str(
        " let __value = __handle.join().unwrap_or_else(|_| panic!(\"pfilter worker panicked\"));",
    );
    out.push_str(" __results[__slot] = Some(__value);");
    out.push_str(" }");
    out.push_str(" }");
    out.push_str(" for (__slot, __handle) in __in_flight {");
    out.push_str(
        " let __value = __handle.join().unwrap_or_else(|_| panic!(\"pfilter worker panicked\"));",
    );
    out.push_str(" __results[__slot] = Some(__value);");
    out.push_str(" }");
    out.push_str(" let mut __out: Vec<Value> = Vec::new();");
    out.push_str(" for (__idx, __item) in __items.iter().cloned().enumerate() {");
    out.push_str(&format!(
        " if __results[__idx].unwrap_or(false) {{ __out.push({}); }}",
        item_value
    ));
    out.push_str(" }");
    out.push_str(" Value::Vector(__out.into()) }");
    out
}

fn render_pmap(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 2 {
        return "Value::Vector(Vec::<Value>::new().into())".into();
    }
    let func = &args[0];
    let coll = &args[1];
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let param_ty = callable_first_param_type(func).unwrap_or(Type::Any);
    let arg_expr = render_pmap_arg(&param_ty, &elem_ty);
    let arg_tys = vec![param_ty];
    let call = render_apply_fn(func, &[arg_expr], &arg_tys, ctx);
    let ret_ty = callable_return_type(func).unwrap_or(Type::Any);
    let ret_value = render_value_from_var("__out", &ret_ty);
    let limit_expr = if args.len() == 3 {
        if let Some(max) = pmap_max_parallel(&args[2]) {
            format!("{}usize", max)
        } else if let Some(expr) = pmap_opts_value(&args[2]) {
            format!("(({}) as usize)", render_expr(expr, ctx))
        } else {
            "1usize".into()
        }
    } else {
        "std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1)".into()
    };
    let mut out = String::from("{");
    out.push_str(&format!(
        " let __items = {}.clone();",
        render_expr(coll, ctx)
    ));
    out.push_str(&format!(" let __limit = {}.max(1);", limit_expr));
    out.push_str(" let mut __results: Vec<Option<Value>> = vec![None; __items.len()];");
    out.push_str(
        " let mut __in_flight: Vec<(usize, std::thread::JoinHandle<Value>)> = Vec::new();",
    );
    out.push_str(" for (__idx, __item) in __items.into_iter().enumerate() {");
    out.push_str(&format!(
        " let __handle = std::thread::spawn(move || {{ let __out = {}; {} }});",
        call, ret_value
    ));
    out.push_str(" __in_flight.push((__idx, __handle));");
    out.push_str(" if __in_flight.len() >= __limit {");
    out.push_str(" let (__slot, __handle) = __in_flight.remove(0);");
    out.push_str(
        " let __value = __handle.join().unwrap_or_else(|_| panic!(\"pmap worker panicked\"));",
    );
    out.push_str(" __results[__slot] = Some(__value);");
    out.push_str(" }");
    out.push_str(" }");
    out.push_str(" for (__slot, __handle) in __in_flight {");
    out.push_str(
        " let __value = __handle.join().unwrap_or_else(|_| panic!(\"pmap worker panicked\"));",
    );
    out.push_str(" __results[__slot] = Some(__value);");
    out.push_str(" }");
    out.push_str(
        " let __collected: Vec<Value> = __results\
         .into_iter()\
         .map(|v| v.unwrap_or(Value::Nil))\
         .collect();",
    );
    out.push_str(" Value::Vector(__collected.into()) }");
    out
}

fn render_pmap_arg(param_ty: &Type, elem_ty: &Type) -> String {
    match (param_ty, elem_ty) {
        (Type::Any | Type::Var(_), _) => render_value_from_var("__item", elem_ty),
        (Type::Prim(_) | Type::Option(_), Type::Any | Type::Var(_)) => {
            let conv = render_value_to_rust("__v", param_ty);
            format!("{{ let __v = __item.clone(); {} }}", conv)
        }
        _ => "__item".to_string(),
    }
}

fn render_as_thread(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 3 {
        return "unimplemented!()".into();
    }
    let TypedExprKind::Symbol(name) = &args[1].kind else {
        return "unimplemented!()".into();
    };
    let ident = sanitize_ident(name);
    let mut current_ty = args[0].ty.clone();
    let mut out = String::from("{");
    out.push_str(&format!(
        " let mut {} = {};",
        ident,
        render_expr(&args[0], ctx)
    ));
    let mut step_ctx = ctx.with_binding(name, &ident, &current_ty);
    for step in args.iter().skip(2) {
        let rendered = render_expr(step, &step_ctx);
        out.push_str(&format!(" {} = {};", ident, rendered));
        current_ty = step.ty.clone();
        step_ctx = ctx.with_binding(name, &ident, &current_ty);
    }
    out.push_str(&format!(" {} }}", ident));
    out
}

fn render_pr_str(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "String::new()".into();
    }
    let mut out = String::from("{ let mut __parts: Vec<String> = Vec::new();");
    for arg in args {
        let value = render_value_expr(arg, ctx);
        out.push_str(&format!(" __parts.push(({}).to_string());", value));
    }
    out.push_str(" __parts.join(\" \") }");
    out
}

fn render_string_includes(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 2 {
        return "false".into();
    }
    let s = render_expr(&args[0], ctx);
    let sub = render_expr(&args[1], ctx);
    format!("({}).contains(&{})", s, sub)
}

fn render_includes(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 2 {
        return "false".into();
    }
    if matches!(args[0].ty, Type::Prim(PrimType::Str)) {
        return render_string_includes(args, ctx);
    }
    let mut out = String::from("{");
    out.push_str(&format!(
        " let __target = {};",
        render_value_expr_with_ctx(&args[0], ctx)
    ));
    out.push_str(&format!(
        " let __needle = {};",
        render_value_expr_with_ctx(&args[1], ctx)
    ));
    out.push_str(
        " clove_core::builtins::includes_value(&__target, &__needle)\
         .unwrap_or_else(|err| panic!(\"includes? failed: {}\", err))",
    );
    out.push_str(" }");
    out
}

fn render_glob(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 1 {
        return "vec![]".into();
    }
    let mut out = String::from("{");
    out.push_str(&format!(" let __pattern = {};", render_expr(&args[0], ctx)));
    out.push_str(
        " let __paths = clove_core::builtins::glob_paths(&__pattern)\
         .unwrap_or_else(|err| panic!(\"glob* failed: {}\", err));",
    );
    out.push_str(" __paths }");
    out
}

fn render_glob_eager(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 1 {
        return "vec![]".into();
    }
    let mut out = String::from("{");
    out.push_str(&format!(" let __pattern = {};", render_expr(&args[0], ctx)));
    out.push_str(
        " let __paths = clove_core::builtins::glob_paths_eager(&__pattern)\
         .unwrap_or_else(|err| panic!(\"glob failed: {}\", err));",
    );
    out.push_str(" __paths }");
    out
}

fn render_json_read_file(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 1 {
        return "vec![]".into();
    }
    let mut out = String::from("{");
    out.push_str(" let __items = clove_core::builtins::json_read_file_vec(&");
    out.push_str(&render_expr(&args[0], ctx));
    out.push_str(
        ")\
         .unwrap_or_else(|err| panic!(\"json::read-file failed: {}\", err));",
    );
    out.push_str(" __items }");
    out
}

fn render_regex_call(callee: &TypedExpr, args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "Value::Nil".into();
    }
    let regex_expr = render_expr(callee, ctx);
    let text_expr = if matches!(args[0].ty, Type::Prim(PrimType::Str)) {
        render_expr(&args[0], ctx)
    } else {
        format!("({}).to_string()", render_value_expr(&args[0], ctx))
    };
    let default_expr = if args.len() >= 2 {
        format!("Some({})", render_value_expr(&args[1], ctx))
    } else {
        "None".into()
    };
    let mut out = String::from("{");
    out.push_str(&format!(" let __regex = {};", regex_expr));
    out.push_str(&format!(" let __text = {};", text_expr));
    out.push_str(&format!(
        " let __default: Option<Value> = {};",
        default_expr
    ));
    out.push_str(" let __caps = __regex.regex.captures(&__text);");
    out.push_str(" match __caps {");
    out.push_str(" Some(__caps) => {");
    out.push_str(" match __caps.get(0) {");
    out.push_str(" Some(__full) => {");
    out.push_str(
        " if __full.start() != 0 || __full.end() != __text.len() {\
             __default.unwrap_or(Value::Nil)\
         } else if __caps.len() == 1 {\
             Value::String(__full.as_str().to_string())\
         } else {\
             let mut __items: Vec<Value> = Vec::new();\
             for __idx in 0..__caps.len() {\
                 match __caps.get(__idx) {\
                     Some(__m) => __items.push(Value::String(__m.as_str().to_string())),\
                     None => __items.push(Value::Nil),\
                 }\
             }\
             Value::Vector(__items.into_iter().collect())\
         }",
    );
    out.push_str(" }");
    out.push_str(" None => __default.unwrap_or(Value::Nil),");
    out.push_str(" }");
    out.push_str(" }");
    out.push_str(" None => __default.unwrap_or(Value::Nil),");
    out.push_str(" }");
    out.push_str(" }");
    out
}

fn render_reduce(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() != 2 && args.len() != 3 {
        return "unimplemented!()".into();
    }
    let func = &args[0];
    let (init, coll) = if args.len() == 2 {
        (None, &args[1])
    } else {
        (Some(&args[1]), &args[2])
    };
    let elem_ty = match &coll.ty {
        Type::Vector(inner) => (**inner).clone(),
        _ => Type::Any,
    };
    let acc_ty = init
        .map(|expr| expr.ty.clone())
        .unwrap_or_else(|| elem_ty.clone());
    let mut out = String::from("{");
    out.push_str(&format!(" let __c = {}.clone();", render_expr(coll, ctx)));
    if init.is_none() {
        let empty_expr = render_reduce_empty(func, &acc_ty);
        out.push_str(&format!(" if __c.is_empty() {{ {} }} else {{", empty_expr));
        out.push_str(" let mut __acc = __c[0].clone();");
        out.push_str(" for __item in __c.iter().skip(1).cloned() {");
        let arg_exprs = vec!["__acc".to_string(), "__item".to_string()];
        let arg_tys = vec![acc_ty.clone(), elem_ty.clone()];
        let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
        out.push_str(&format!(" __acc = {};", call));
        out.push_str(" } __acc }");
    } else {
        out.push_str(&format!(
            " let mut __acc = {};",
            render_expr(init.unwrap(), ctx)
        ));
        out.push_str(" for __item in __c.iter().cloned() {");
        let arg_exprs = vec!["__acc".to_string(), "__item".to_string()];
        let arg_tys = vec![acc_ty.clone(), elem_ty];
        let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
        out.push_str(&format!(" __acc = {};", call));
        out.push_str(" } __acc");
    }
    out.push_str(" }");
    out
}

fn render_reduce_empty(func: &TypedExpr, acc_ty: &Type) -> String {
    let TypedExprKind::Symbol(name) = &func.kind else {
        return "panic!(\"reduce on empty collection\")".into();
    };
    match name.as_str() {
        "+" | "-" => Some(match acc_ty {
            Type::Prim(PrimType::Float) => "0f64".to_string(),
            _ => "0i64".to_string(),
        }),
        "*" => Some(match acc_ty {
            Type::Prim(PrimType::Float) => "1f64".to_string(),
            _ => "1i64".to_string(),
        }),
        _ => None,
    }
    .unwrap_or_else(|| "panic!(\"reduce on empty collection\")".into())
}

fn render_update(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() < 3 {
        return "unimplemented!()".into();
    }
    let map_expr = &args[0];
    let key_expr = &args[1];
    let func = &args[2];
    let extra = &args[3..];
    let (value_ty, is_option) = match &map_expr.ty {
        Type::Map(_, val) => ((**val).clone(), false),
        Type::Option(inner) => match inner.as_ref() {
            Type::Map(_, val) => ((**val).clone(), true),
            _ => (Type::Any, false),
        },
        _ => (Type::Any, false),
    };
    let mut out = String::from("{");
    if is_option {
        out.push_str(&format!(
            " let mut __m = match {} {{ Some(__m) => __m.clone(), None => std::collections::HashMap::new() }};",
            render_expr(map_expr, ctx)
        ));
    } else {
        out.push_str(&format!(
            " let mut __m = {}.clone();",
            render_expr(map_expr, ctx)
        ));
    }
    out.push_str(&format!(" let __k = {};", render_expr(key_expr, ctx)));
    let default_expr = default_value_for_update(&value_ty);
    out.push_str(&format!(
        " let __current = __m.get(&__k).cloned().unwrap_or_else(|| {});",
        default_expr
    ));
    let mut arg_exprs = Vec::with_capacity(1 + extra.len());
    arg_exprs.push("__current".to_string());
    let mut arg_tys = Vec::with_capacity(1 + extra.len());
    arg_tys.push(value_ty);
    for arg in extra {
        arg_exprs.push(render_expr(arg, ctx));
        arg_tys.push(arg.ty.clone());
    }
    let call = render_apply_fn(func, &arg_exprs, &arg_tys, ctx);
    out.push_str(&format!(" let __new = {};", call));
    out.push_str(" __m.insert(__k, __new); __m }");
    out
}

fn default_value_for_update(ty: &Type) -> String {
    match ty {
        Type::Option(_) => "None".into(),
        Type::Prim(PrimType::Int) => "0i64".into(),
        Type::Prim(PrimType::Float) => "0f64".into(),
        Type::Prim(PrimType::Bool) => "false".into(),
        Type::Prim(PrimType::Str) => "String::new()".into(),
        Type::Prim(PrimType::Nil) => "()".into(),
        Type::Vector(_) => "vec![]".into(),
        Type::Map(_, _) | Type::Record(_) => "std::collections::HashMap::new()".into(),
        _ => "panic!(\"update on missing key\")".into(),
    }
}

fn truthy_expr_for_type(var: &str, ty: &Type) -> Option<String> {
    match ty {
        Type::Prim(PrimType::Bool) => Some(var.to_string()),
        Type::Prim(PrimType::Nil) => Some("false".into()),
        Type::Prim(PrimType::Int) | Type::Prim(PrimType::Float) | Type::Prim(PrimType::Str) => {
            Some("true".into())
        }
        Type::Option(inner) => match inner.as_ref() {
            Type::Prim(PrimType::Bool) => Some(format!("matches!({}, Some(true))", var)),
            _ => Some(format!("{}.is_some()", var)),
        },
        Type::Vector(_)
        | Type::Map(_, _)
        | Type::Set(_)
        | Type::Record(_)
        | Type::Opaque(_)
        | Type::Func(_, _, _)
        | Type::Mut(_) => Some("true".into()),
        Type::Any | Type::Var(_) | Type::Overloaded(_) | Type::Tuple(_) => None,
    }
}

fn render_truthy_expr(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    if let Some(truthy) = truthy_expr_for_type("__cond", &expr.ty) {
        let rendered = render_expr(expr, ctx);
        return format!("{{ let __cond = {}; {} }}", rendered, truthy);
    }
    let rendered = render_value_expr(expr, ctx);
    format!(
        "match {v} {{ Value::Bool(false) | Value::Nil => false, _ => true }}",
        v = rendered
    )
}

fn render_loop_expr(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "loop { }".into();
    }
    let (var_names, var_idents, inits, var_tys) = match &args[0].kind {
        TypedExprKind::Vector(vec) => {
            let mut names = Vec::new();
            let mut idents = Vec::new();
            let mut vals = Vec::new();
            let mut tys = Vec::new();
            let mut iter = vec.iter();
            while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                if let TypedExprKind::Symbol(name) = &k.kind {
                    names.push(name.clone());
                    idents.push(sanitize_ident(name));
                    vals.push(render_expr(v, ctx));
                    tys.push(v.ty.clone());
                }
            }
            (names, idents, vals, tys)
        }
        _ => return "unimplemented!()".into(),
    };
    let body_exprs: Vec<TypedExpr> = args.iter().skip(1).cloned().collect();
    let mut out = String::from("{");
    for (n, v) in var_idents.iter().zip(inits.iter()) {
        out.push_str(&format!(" let mut {} = {};", n, v));
    }
    out.push_str(" loop {");
    let mut next_ctx = ctx.clone();
    for ((name, ident), ty) in var_names.iter().zip(var_idents.iter()).zip(var_tys.iter()) {
        next_ctx = next_ctx.with_binding(name, ident, ty);
    }
    for (idx, expr) in body_exprs.iter().enumerate() {
        if let Some(assign) = render_recur(expr, &var_idents, &next_ctx) {
            out.push_str(&assign);
            out.push_str(" continue;");
            continue;
        }
        if idx + 1 == body_exprs.len() {
            out.push_str(" ");
            out.push_str(&render_tail_expr(expr, &var_idents, &next_ctx));
        } else {
            out.push_str(" let _ = ");
            out.push_str(&render_expr(expr, &next_ctx));
            out.push_str(";");
        }
    }
    out.push_str(" }");
    out.push_str(" }");
    out
}

fn render_recur(expr: &TypedExpr, vars: &[String], ctx: &RenderCtx) -> Option<String> {
    if let TypedExprKind::Call { callee, args } = &expr.kind {
        if let TypedExprKind::Symbol(sym) = &callee.kind {
            if sym == "recur" && args.len() == vars.len() {
                let mut out = String::new();
                let temps: Vec<String> = (0..args.len()).map(|i| format!("__t{}", i)).collect();
                out.push_str(" { let (");
                out.push_str(&temps.join(", "));
                out.push_str(") = (");
                for (idx, val) in args.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&render_expr(val, ctx));
                }
                out.push_str(");");
                for (name, tmp) in vars.iter().zip(temps.iter()) {
                    out.push_str(&format!(" {} = {};", name, tmp));
                }
                out.push_str(" }");
                return Some(out);
            }
        }
    }
    None
}

fn render_tail_expr(expr: &TypedExpr, vars: &[String], ctx: &RenderCtx) -> String {
    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Symbol(sym) = &callee.kind {
                if sym == "recur" && args.len() == vars.len() {
                    if let Some(assign) = render_recur(expr, vars, ctx) {
                        return format!("{{{} continue;}}", assign);
                    }
                }
            }
            format!("break {};", render_expr(expr, ctx))
        }
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_s = render_expr(cond, ctx);
            let then_s = render_tail_block(then_branch, vars, ctx);
            let else_s = render_tail_block(else_branch, vars, ctx);
            format!("if {} {} else {}", cond_s, then_s, else_s)
        }
        TypedExprKind::Do(items) => render_tail_block(items, vars, ctx),
        _ => format!("break {};", render_expr(expr, ctx)),
    }
}

fn render_tail_block(exprs: &[TypedExpr], vars: &[String], ctx: &RenderCtx) -> String {
    if exprs.is_empty() {
        return "{ break (); }".into();
    }
    let mut out = String::from("{");
    for (idx, e) in exprs.iter().enumerate() {
        if idx + 1 == exprs.len() {
            out.push_str(&render_tail_expr(e, vars, ctx));
        } else {
            out.push_str(&format!(" let _ = {};", render_expr(e, ctx)));
        }
    }
    out.push_str(" }");
    out
}

fn render_variadic_and(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    match args.len() {
        0 => "true".into(),
        1 => render_expr(&args[0], ctx),
        _ => {
            let mut out = String::new();
            for (idx, arg) in args.iter().enumerate() {
                if idx > 0 {
                    out.push_str(" && ");
                }
                out.push_str(&render_expr(arg, ctx));
            }
            out
        }
    }
}

fn render_variadic_or(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    match args.len() {
        0 => "false".into(),
        1 => render_expr(&args[0], ctx),
        _ => {
            let mut out = String::new();
            for (idx, arg) in args.iter().enumerate() {
                if idx > 0 {
                    out.push_str(" || ");
                }
                out.push_str(&render_expr(arg, ctx));
            }
            out
        }
    }
}

fn render_variadic_op(
    args: &[TypedExpr],
    op: &str,
    identity_int: &str,
    identity_float: &str,
    result_ty: &Type,
    ctx: &RenderCtx,
) -> String {
    match args.len() {
        0 => {
            if is_int_type(result_ty) {
                identity_int.to_string()
            } else {
                identity_float.to_string()
            }
        }
        1 => render_expr(&args[0], ctx),
        _ => {
            let parts: Vec<String> = args.iter().map(|e| render_expr_as_f64(e, ctx)).collect();
            let joined = parts.join(&format!(" {} ", op));
            if is_int_type(result_ty) {
                format!("({}) as i64", joined)
            } else {
                joined
            }
        }
    }
}

fn render_minus(args: &[TypedExpr], result_ty: &Type, ctx: &RenderCtx) -> String {
    match args.len() {
        0 => {
            if is_int_type(result_ty) {
                "0i64".into()
            } else {
                "0f64".into()
            }
        }
        1 => {
            let as_f64 = render_expr_as_f64(&args[0], ctx);
            if is_int_type(result_ty) {
                format!("(-{}) as i64", as_f64)
            } else {
                format!("-{}", as_f64)
            }
        }
        _ => {
            let parts: Vec<String> = args.iter().map(|e| render_expr_as_f64(e, ctx)).collect();
            let joined = parts.join(" - ");
            if is_int_type(result_ty) {
                format!("({}) as i64", joined)
            } else {
                joined
            }
        }
    }
}

fn render_div(args: &[TypedExpr], _result_ty: &Type, ctx: &RenderCtx) -> String {
    match args.len() {
        0 => "1f64".into(),
        1 => format!("1f64 / {}", render_expr_as_f64(&args[0], ctx)),
        _ => args
            .iter()
            .map(|e| render_expr_as_f64(e, ctx))
            .collect::<Vec<_>>()
            .join(" / "),
    }
}

fn render_numeric_cmp(args: &[TypedExpr], op: &str, ctx: &RenderCtx) -> String {
    if args.len() != 2 {
        return "false".into();
    }
    let left_f = render_expr_as_f64(&args[0], ctx);
    let right_f = render_expr_as_f64(&args[1], ctx);
    format!("{} {} {}", left_f, op, right_f)
}

fn render_inc_dec(args: &[TypedExpr], result_ty: &Type, is_inc: bool, ctx: &RenderCtx) -> String {
    if args.len() != 1 {
        return if is_int_type(result_ty) {
            "0i64".into()
        } else {
            "0f64".into()
        };
    }
    let as_f64 = render_expr_as_f64(&args[0], ctx);
    let op = if is_inc { "+" } else { "-" };
    if is_int_type(result_ty) {
        format!("({} {} 1.0) as i64", as_f64, op)
    } else {
        format!("{} {} 1.0", as_f64, op)
    }
}

fn render_eq(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    match args.len() {
        0 | 1 => "true".into(),
        _ => {
            let mut parts = Vec::new();
            for pair in args.windows(2) {
                parts.push(format!(
                    "{} == {}",
                    render_expr(&pair[0], ctx),
                    render_expr(&pair[1], ctx)
                ));
            }
            parts.join(" && ")
        }
    }
}

fn render_not_eq(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    match args.len() {
        0 | 1 => "false".into(),
        _ => format!("!({})", render_eq(args, ctx)),
    }
}

fn render_extremum(args: &[TypedExpr], is_max: bool, ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "0i64".into();
    }
    let mut acc = render_expr(&args[0], ctx);
    let mut acc_ty = args[0].ty.clone();
    for a in args.iter().skip(1) {
        let cur = render_expr(a, ctx);
        let acc_cmp = render_number_as_f64(&acc, &acc_ty);
        let cur_cmp = render_number_as_f64(&cur, &a.ty);
        let cmp_expr = if is_max {
            format!("{} > {}", acc_cmp, cur_cmp)
        } else {
            format!("{} < {}", acc_cmp, cur_cmp)
        };
        acc = format!("if {} {{ {} }} else {{ {} }}", cmp_expr, acc, cur);
        acc_ty = a.ty.clone();
    }
    acc
}

fn render_first(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let vec_s = render_expr(expr, ctx);
    format!(
        "{{ let __v = {v}.clone(); __v.first().cloned() }}",
        v = vec_s
    )
}

fn render_rest(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let vec_s = render_expr(expr, ctx);
    format!(
        "{{ let __v = {v}.clone(); if __v.len() > 0 {{ __v[1..].to_vec() }} else {{ vec![] }} }}",
        v = vec_s
    )
}

fn render_conj(
    coll_expr: &TypedExpr,
    item_expr: &TypedExpr,
    result_ty: &Type,
    ctx: &RenderCtx,
) -> String {
    let any_ty = Type::Any;
    let (is_set, target_elem_ty) = match result_ty {
        Type::Set(inner) => (true, inner.as_ref()),
        _ => match &coll_expr.ty {
            Type::Set(inner) => (true, inner.as_ref()),
            _ => (false, &any_ty),
        },
    };
    if is_set {
        let needs_convert = matches!(&coll_expr.ty, Type::Set(inner) if matches!(inner.as_ref(), Type::Any | Type::Var(_)))
            && !matches!(target_elem_ty, Type::Any | Type::Var(_));
        let coll_rendered = render_expr(coll_expr, ctx);
        let coll_rendered = if needs_convert {
            let conv = render_value_to_rust("__item", target_elem_ty);
            format!(
                "{{ let __v = {c}.clone(); __v.into_iter().map(|__item| {conv}).collect::<std::collections::HashSet<_>>() }}",
                c = coll_rendered,
                conv = conv
            )
        } else {
            format!("{c}.clone()", c = coll_rendered)
        };
        let item_rendered = if matches!(target_elem_ty, Type::Any | Type::Var(_)) {
            render_value_expr(item_expr, ctx)
        } else if matches!(item_expr.ty, Type::Any | Type::Var(_)) {
            let item_value = render_value_expr(item_expr, ctx);
            let conv = render_value_to_rust("__v", target_elem_ty);
            format!(
                "{{ let __v = {val}; {conv} }}",
                val = item_value,
                conv = conv
            )
        } else {
            render_expr(item_expr, ctx)
        };
        return format!(
            "{{ let mut __s = {s}; __s.insert({it}); __s }}",
            s = coll_rendered,
            it = item_rendered
        );
    }
    format!(
        "{{ let mut __v = {v}.clone(); __v.push({it}); __v }}",
        v = render_expr(coll_expr, ctx),
        it = render_expr(item_expr, ctx)
    )
}

fn render_disj(
    set_expr: &TypedExpr,
    items: &[TypedExpr],
    result_ty: &Type,
    ctx: &RenderCtx,
) -> String {
    let any_ty = Type::Any;
    let (is_set, target_elem_ty) = match result_ty {
        Type::Set(inner) => (true, inner.as_ref()),
        _ => match &set_expr.ty {
            Type::Set(inner) => (true, inner.as_ref()),
            _ => (false, &any_ty),
        },
    };
    if !is_set {
        return format!("{c}.clone()", c = render_expr(set_expr, ctx));
    }
    let needs_convert = matches!(&set_expr.ty, Type::Set(inner) if matches!(inner.as_ref(), Type::Any | Type::Var(_)))
        && !matches!(target_elem_ty, Type::Any | Type::Var(_));
    let coll_rendered = render_expr(set_expr, ctx);
    let coll_rendered = if needs_convert {
        let conv = render_value_to_rust("__item", target_elem_ty);
        format!(
            "{{ let __v = {c}.clone(); __v.into_iter().map(|__item| {conv}).collect::<std::collections::HashSet<_>>() }}",
            c = coll_rendered,
            conv = conv
        )
    } else {
        format!("{c}.clone()", c = coll_rendered)
    };
    let mut out = format!("{{ let mut __s = {s};", s = coll_rendered);
    for item in items {
        let item_rendered = if matches!(target_elem_ty, Type::Any | Type::Var(_)) {
            render_value_expr(item, ctx)
        } else if matches!(item.ty, Type::Any | Type::Var(_)) {
            let item_value = render_value_expr(item, ctx);
            let conv = render_value_to_rust("__v", target_elem_ty);
            format!(
                "{{ let __v = {val}; {conv} }}",
                val = item_value,
                conv = conv
            )
        } else {
            render_expr(item, ctx)
        };
        out.push_str(&format!(" __s.remove(&{});", item_rendered));
    }
    out.push_str(" __s }");
    out
}

fn render_count(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    format!(
        "{{ let __v = {v}.clone(); i64::try_from(__v.len()).unwrap() }}",
        v = render_expr(expr, ctx)
    )
}

fn render_empty(expr: &TypedExpr, ctx: &RenderCtx) -> String {
    format!(
        "{{ let __v = {v}.clone(); __v.is_empty() }}",
        v = render_expr(expr, ctx)
    )
}

fn render_nth(vec_expr: &TypedExpr, idx_expr: &TypedExpr, ctx: &RenderCtx) -> String {
    format!(
        "{{ let __v = {v}.clone(); let __i = {i}; if __i < 0 {{ None }} else {{ let __u = __i as usize; __v.get(__u).cloned() }} }}",
        v = render_expr(vec_expr, ctx),
        i = render_expr(idx_expr, ctx)
    )
}

fn render_into(dst_expr: &TypedExpr, src_expr: &TypedExpr, ctx: &RenderCtx) -> String {
    format!(
        "{{ let mut __d = {d}.clone(); __d.extend({s}.clone()); __d }}",
        d = render_expr(dst_expr, ctx),
        s = render_expr(src_expr, ctx)
    )
}

fn render_record_key_expr(key_expr: &TypedExpr, ctx: &RenderCtx) -> String {
    match &key_expr.kind {
        TypedExprKind::Literal(Literal::Keyword(name)) => format!("String::from({:?})", name),
        TypedExprKind::Literal(Literal::Str(s)) => format!("String::from({:?})", s),
        TypedExprKind::Symbol(sym) if sym.starts_with(':') => {
            format!("String::from({:?})", sym.trim_start_matches(':'))
        }
        _ => render_expr(key_expr, ctx),
    }
}

fn record_key_name(key_expr: &TypedExpr) -> Option<String> {
    match &key_expr.kind {
        TypedExprKind::Literal(Literal::Keyword(name)) => Some(name.clone()),
        TypedExprKind::Literal(Literal::Str(s)) => Some(s.clone()),
        TypedExprKind::Symbol(sym) if sym.starts_with(':') => {
            Some(sym.trim_start_matches(':').to_string())
        }
        _ => None,
    }
}

fn render_assoc(
    map_expr: &TypedExpr,
    key_expr: &TypedExpr,
    val_expr: &TypedExpr,
    ctx: &RenderCtx,
) -> String {
    let (key_rendered, val_rendered) = match map_expr.ty {
        Type::Record(_) => (
            render_record_key_expr(key_expr, ctx),
            render_value_expr(val_expr, ctx),
        ),
        _ => (render_expr(key_expr, ctx), render_expr(val_expr, ctx)),
    };
    format!(
        "{{ let mut __m = {m}.clone(); __m.insert({k}, {v}); __m }}",
        m = render_expr(map_expr, ctx),
        k = key_rendered,
        v = val_rendered
    )
}

fn render_assoc_multi(map_expr: &TypedExpr, args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.len() == 2 {
        return render_assoc(map_expr, &args[0], &args[1], ctx);
    }
    let mut out = format!(
        "{{ let mut __m = {};",
        render_assoc(map_expr, &args[0], &args[1], ctx)
    );
    let is_record = matches!(map_expr.ty, Type::Record(_));
    let mut idx = 2;
    while idx + 1 < args.len() {
        let key_expr = &args[idx];
        let val_expr = &args[idx + 1];
        let key_rendered = if is_record {
            render_record_key_expr(key_expr, ctx)
        } else {
            render_expr(key_expr, ctx)
        };
        let val_rendered = if is_record {
            render_value_expr(val_expr, ctx)
        } else {
            render_expr(val_expr, ctx)
        };
        out.push_str(&format!(
            " __m.insert({k}, {v});",
            k = key_rendered,
            v = val_rendered
        ));
        idx += 2;
    }
    out.push_str(" __m }");
    out
}

fn render_dissoc(map_expr: &TypedExpr, key_expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let key_rendered = match map_expr.ty {
        Type::Record(_) => render_record_key_expr(key_expr, ctx),
        _ => render_expr(key_expr, ctx),
    };
    format!(
        "{{ let mut __m = {m}.clone(); __m.remove(&{k}); __m }}",
        m = render_expr(map_expr, ctx),
        k = key_rendered
    )
}

fn render_get(
    map_expr: &TypedExpr,
    key_expr: &TypedExpr,
    default_expr: Option<&TypedExpr>,
    ctx: &RenderCtx,
) -> String {
    let (key_is_value, val_is_value) = match &map_expr.ty {
        Type::Map(key_ty, val_ty) => (
            matches!(key_ty.as_ref(), Type::Any | Type::Var(_)),
            matches!(val_ty.as_ref(), Type::Any | Type::Var(_)),
        ),
        Type::Option(inner) => match inner.as_ref() {
            Type::Map(key_ty, val_ty) => (
                matches!(key_ty.as_ref(), Type::Any | Type::Var(_)),
                matches!(val_ty.as_ref(), Type::Any | Type::Var(_)),
            ),
            _ => (false, false),
        },
        _ => (false, false),
    };
    let is_record = matches!(&map_expr.ty, Type::Record(_))
        || matches!(
            &map_expr.ty,
            Type::Option(inner) if matches!(inner.as_ref(), Type::Record(_))
        );
    let key_rendered = if is_record {
        render_record_key_expr(key_expr, ctx)
    } else if key_is_value {
        render_value_expr_with_ctx(key_expr, ctx)
    } else {
        render_expr(key_expr, ctx)
    };
    match &map_expr.ty {
        Type::Record(fields) => {
            let key_name = record_key_name(key_expr);
            if let Some(name) = key_name
                .as_ref()
                .and_then(|name| fields.get(name).map(|_| name))
            {
                let map_rendered = render_expr(map_expr, ctx);
                let key_rendered = render_record_key_expr(key_expr, ctx);
                let field_ty = fields.get(name).unwrap_or(&Type::Any);
                let conv = render_value_to_rust("__v", field_ty);
                if let Some(def) = default_expr {
                    let def_rendered = render_value_expr_with_ctx(def, ctx);
                    return format!(
                        "{{ let __m = {m}.clone(); let __v = __m.get(&{k}).cloned().unwrap_or_else(|| {def}); {conv} }}",
                        m = map_rendered,
                        k = key_rendered,
                        def = def_rendered,
                        conv = conv
                    );
                }
                return format!(
                    "{{ let __m = {m}.clone(); let __v = __m.get(&{k}).cloned().unwrap_or_else(|| panic!(\"record missing field :{name}\")); {conv} }}",
                    m = map_rendered,
                    k = key_rendered,
                    name = name,
                    conv = conv
                );
            }
            render_get_value(map_expr, key_expr, default_expr, ctx)
        }
        Type::Map(_, _) => {
            let map_rendered = render_expr(map_expr, ctx);
            if let Some(def) = default_expr {
                let def_rendered = if val_is_value {
                    render_value_expr_with_ctx(def, ctx)
                } else {
                    render_expr(def, ctx)
                };
                return format!(
                    "{{ let __m = {m}.clone(); __m.get(&{k}).cloned().unwrap_or_else(|| {def}) }}",
                    m = map_rendered,
                    k = key_rendered,
                    def = def_rendered
                );
            }
            format!(
                "{{ let __m = {m}.clone(); __m.get(&{k}).cloned() }}",
                m = map_rendered,
                k = key_rendered
            )
        }
        Type::Option(inner) if matches!(inner.as_ref(), Type::Map(_, _) | Type::Record(_)) => {
            let map_rendered = render_expr(map_expr, ctx);
            if let Some(def) = default_expr {
                let def_rendered = if val_is_value {
                    render_value_expr_with_ctx(def, ctx)
                } else {
                    render_expr(def, ctx)
                };
                return format!(
                    "{{ let __m = {m}; match __m {{ Some(__inner) => __inner.get(&{k}).cloned().unwrap_or_else(|| {def}), None => {def} }} }}",
                    m = map_rendered,
                    k = key_rendered,
                    def = def_rendered
                );
            }
            format!(
                "{{ let __m = {m}; match __m {{ Some(__inner) => __inner.get(&{k}).cloned(), None => None }} }}",
                m = map_rendered,
                k = key_rendered
            )
        }
        _ => render_get_value(map_expr, key_expr, default_expr, ctx),
    }
}

fn render_get_value(
    map_expr: &TypedExpr,
    key_expr: &TypedExpr,
    default_expr: Option<&TypedExpr>,
    ctx: &RenderCtx,
) -> String {
    let mut out = String::from("{");
    out.push_str(&format!(
        " let __target = {};",
        render_value_expr_with_ctx(map_expr, ctx)
    ));
    out.push_str(&format!(
        " let __key = {};",
        render_value_expr_with_ctx(key_expr, ctx)
    ));
    if let Some(def) = default_expr {
        out.push_str(&format!(
            " let __default = {};",
            render_value_expr_with_ctx(def, ctx)
        ));
        out.push_str(
            " let __value = clove_core::builtins::get_value(&__target, &__key, Some(&__default))\
             .unwrap_or_else(|err| panic!(\"get failed: {}\", err));",
        );
    } else {
        out.push_str(
            " let __value = clove_core::builtins::get_value(&__target, &__key, None)\
             .unwrap_or_else(|err| panic!(\"get failed: {}\", err));",
        );
    }
    out.push_str(" __value }");
    out
}

fn render_contains(map_expr: &TypedExpr, key_expr: &TypedExpr, ctx: &RenderCtx) -> String {
    let expr_s = render_expr(map_expr, ctx);
    let key_s = match map_expr.ty {
        Type::Record(_) => render_record_key_expr(key_expr, ctx),
        _ => render_expr(key_expr, ctx),
    };
    match &map_expr.ty {
        Type::Map(_, _) | Type::Record(_) => {
            format!(
                "{{ let __m = {m}.clone(); __m.contains_key(&{k}) }}",
                m = expr_s,
                k = key_s
            )
        }
        _ => {
            format!(
                "{{ let __m = {m}.clone(); __m.contains(&{k}) }}",
                m = expr_s,
                k = key_s
            )
        }
    }
}

fn render_subs(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    match args.len() {
        2 => format!(
            "{{ let __s = {s}.clone(); let __start = {st}; __s.chars().skip(__start as usize).collect::<String>() }}",
            s = render_expr(&args[0], ctx),
            st = render_expr(&args[1], ctx)
        ),
        3 => format!(
            "{{ let __s = {s}.clone(); let __start = {st} as usize; let __end = {en} as usize; __s.chars().skip(__start).take(__end.saturating_sub(__start)).collect::<String>() }}",
            s = render_expr(&args[0], ctx),
            st = render_expr(&args[1], ctx),
            en = render_expr(&args[2], ctx)
        ),
        _ => "String::new()".into(),
    }
}

fn render_union(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "std::collections::HashSet::new()".into();
    }
    let mut out = String::from(
        "{ let mut __acc: std::collections::HashSet<_> = std::collections::HashSet::new();",
    );
    for a in args {
        let vec_s = render_expr(a, ctx);
        out.push_str(&format!(
            " for __x in {v}.clone() {{ __acc.insert(__x); }}",
            v = vec_s
        ));
    }
    out.push_str(" __acc }");
    out
}

fn render_intersection(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "std::collections::HashSet::new()".into();
    }
    let first = render_expr(&args[0], ctx);
    let mut out = String::from("{ let mut __acc: std::collections::HashSet<_> = { let mut s = std::collections::HashSet::new();");
    out.push_str(&format!(
        " for __x in {v}.clone() {{ s.insert(__x); }} s }};",
        v = first
    ));
    for other in args.iter().skip(1) {
        let v = render_expr(other, ctx);
        out.push_str(&format!(
            " let __set = {{ let mut s = std::collections::HashSet::new(); for __x in {o}.clone() {{ s.insert(__x); }} s }}; __acc.retain(|x| __set.contains(x));",
            o = v
        ));
    }
    out.push_str(" __acc }");
    out
}

fn render_difference(args: &[TypedExpr], ctx: &RenderCtx) -> String {
    if args.is_empty() {
        return "std::collections::HashSet::new()".into();
    }
    let first = render_expr(&args[0], ctx);
    let mut out = String::from("{ let mut __acc: std::collections::HashSet<_> = { let mut s = std::collections::HashSet::new();");
    out.push_str(&format!(
        " for __x in {v}.clone() {{ s.insert(__x); }} s }};",
        v = first
    ));
    for other in args.iter().skip(1) {
        let v = render_expr(other, ctx);
        out.push_str(&format!(
            " for __x in {o}.clone() {{ __acc.remove(&__x); }}",
            o = v
        ));
    }
    out.push_str(" __acc }");
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use clove_core::reader::{Reader, ReaderOptions};
    use clove_core::typed_ir::build_typed_exprs;
    use clove_core::typing::infer::infer_forms;

    fn typed_exprs(src: &str) -> Vec<TypedExpr> {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().expect("read forms");
        let infer = infer_forms(&forms).expect("infer forms");
        build_typed_exprs(&infer)
    }

    #[test]
    fn hm_codegen_try_finally_is_supported() {
        let exprs = typed_exprs("(try 1 (finally 2))");
        assert!(!requires_dynamic_fallback(&exprs));
    }

    #[test]
    fn hm_codegen_throw_is_supported() {
        let exprs = typed_exprs("(throw 1)");
        assert!(!requires_dynamic_fallback(&exprs));
    }

    #[test]
    fn hm_codegen_try_with_catch_needs_runtime() {
        let exprs = typed_exprs("(try 1 (catch e 2))");
        let global_fns = collect_global_fns(&exprs);
        let global_vars = collect_global_vars(&exprs);
        let ctx = RenderCtx::with_globals(&global_fns, &global_vars);
        assert!(expr_needs_runtime(&exprs[0], &ctx));
    }
}
