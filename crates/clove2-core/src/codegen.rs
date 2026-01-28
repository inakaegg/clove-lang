use std::collections::HashMap;

use crate::aliases;
use crate::ast::{Expr, ExprKind, Literal};
use crate::builtins;
use crate::syntax::{AstExpr, Binding, Param, TopLevel};
use crate::types::Type;
use crate::use_directive::MutMode;

#[derive(Clone, PartialEq, Eq, Debug)]
enum TypedKind {
    Int,
    Float,
    Bool,
    Str,
    Keyword,
    Symbol,
    Vec(Box<TypedKind>),
    Map(Box<TypedKind>, Box<TypedKind>),
    Optional(Box<TypedKind>),
    Union2(Box<TypedKind>, Box<TypedKind>),
}

fn typed_debug(message: &str) {
    if std::env::var("CLOVE2_TYPED_DEBUG").is_ok() {
        eprintln!("[typed-codegen] {}", message);
    }
}

fn rust_ident(name: &str) -> String {
    let mut out = String::from("v_");
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
        } else {
            out.push('_');
            out.push_str(&format!("{:x}", ch as u32));
            out.push('_');
        }
    }
    out
}

struct TypedExpr {
    code: String,
    kind: TypedKind,
    is_symbol: bool,
}

#[derive(Clone)]
struct TypedFnSig {
    params: Vec<TypedKind>,
    rest: Option<TypedKind>,
    ret: TypedKind,
}

struct InlineExpr {
    prelude: String,
    expr: String,
    kind: TypedKind,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum KeyLiteralKind {
    Str,
    Keyword,
    Symbol,
}

#[derive(Clone)]
struct KeyLiteralDef {
    name: String,
    kind: KeyLiteralKind,
    value: String,
}

#[derive(Clone)]
struct RegexLiteralDef {
    name: String,
    pattern: String,
}

struct TypedContext {
    counter: usize,
    key_literal_idx: usize,
    key_literals: Vec<KeyLiteralDef>,
    key_literal_map: HashMap<(KeyLiteralKind, String), String>,
    regex_literal_idx: usize,
    regex_literals: Vec<RegexLiteralDef>,
    regex_literal_map: HashMap<String, String>,
}

impl TypedContext {
    fn new() -> Self {
        Self {
            counter: 0,
            key_literal_idx: 0,
            key_literals: Vec::new(),
            key_literal_map: HashMap::new(),
            regex_literal_idx: 0,
            regex_literals: Vec::new(),
            regex_literal_map: HashMap::new(),
        }
    }

    fn temp(&mut self, prefix: &str) -> String {
        let name = format!("{}_{}", prefix, self.counter);
        self.counter += 1;
        name
    }

    fn key_literal(&mut self, kind: KeyLiteralKind, value: &str) -> String {
        let key = (kind, value.to_string());
        if let Some(existing) = self.key_literal_map.get(&key) {
            return existing.clone();
        }
        let name = format!("KEY_{}", self.key_literal_idx);
        self.key_literal_idx += 1;
        self.key_literal_map.insert(key, name.clone());
        self.key_literals.push(KeyLiteralDef {
            name: name.clone(),
            kind,
            value: value.to_string(),
        });
        name
    }

    fn regex_literal(&mut self, pattern: &str) -> String {
        if let Some(existing) = self.regex_literal_map.get(pattern) {
            return existing.clone();
        }
        let name = format!("REGEX_{}", self.regex_literal_idx);
        self.regex_literal_idx += 1;
        self.regex_literal_map
            .insert(pattern.to_string(), name.clone());
        self.regex_literals.push(RegexLiteralDef {
            name: name.clone(),
            pattern: pattern.to_string(),
        });
        name
    }
}

fn typed_kind_to_rust(kind: &TypedKind) -> Option<String> {
    match kind {
        TypedKind::Int => Some("i64".to_string()),
        TypedKind::Float => Some("f64".to_string()),
        TypedKind::Bool => Some("bool".to_string()),
        TypedKind::Str => Some("String".to_string()),
        TypedKind::Keyword => Some("String".to_string()),
        TypedKind::Symbol => Some("String".to_string()),
        TypedKind::Vec(inner) => typed_kind_to_rust(inner).map(|inner| format!("Vec<{}>", inner)),
        TypedKind::Map(_, value) => {
            typed_kind_to_rust(value).map(|value| format!("BTreeMap<Key, {}>", value))
        }
        TypedKind::Optional(inner) => {
            typed_kind_to_rust(inner).map(|inner| format!("Option<{}>", inner))
        }
        TypedKind::Union2(left, right) => {
            let left = typed_kind_to_rust(left)?;
            let right = typed_kind_to_rust(right)?;
            Some(format!("Union2<{}, {}>", left, right))
        }
    }
}

fn typed_kind_from_type(ty: &Type) -> Option<TypedKind> {
    match ty {
        Type::Int => Some(TypedKind::Int),
        Type::Float => Some(TypedKind::Float),
        Type::Number => Some(TypedKind::Float),
        Type::Bool => Some(TypedKind::Bool),
        Type::Str => Some(TypedKind::Str),
        Type::Keyword => Some(TypedKind::Keyword),
        Type::Symbol => Some(TypedKind::Symbol),
        Type::Vec(inner) => {
            typed_kind_from_type(inner).map(|inner| TypedKind::Vec(Box::new(inner)))
        }
        Type::Map(key, value) => {
            let key_kind = typed_kind_from_type(key)?;
            let value_kind = typed_kind_from_type(value)?;
            Some(TypedKind::Map(Box::new(key_kind), Box::new(value_kind)))
        }
        Type::Union(items) => {
            let mut has_nil = false;
            let mut others = Vec::new();
            for item in items {
                if matches!(item, Type::Nil) {
                    has_nil = true;
                } else {
                    others.push(item);
                }
            }
            match others.len() {
                0 => None,
                1 => {
                    let inner = typed_kind_from_type(others[0])?;
                    if has_nil {
                        Some(TypedKind::Optional(Box::new(inner)))
                    } else {
                        Some(inner)
                    }
                }
                2 => {
                    let left = typed_kind_from_type(others[0])?;
                    let right = typed_kind_from_type(others[1])?;
                    let union = TypedKind::Union2(Box::new(left), Box::new(right));
                    if has_nil {
                        Some(TypedKind::Optional(Box::new(union)))
                    } else {
                        Some(union)
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn typed_kind_is_copy(kind: &TypedKind) -> bool {
    matches!(kind, TypedKind::Int | TypedKind::Float | TypedKind::Bool)
}

fn emit_typed_truthy_check(ctx: &mut TypedContext, cond: &TypedExpr) -> (String, String) {
    let cond_var = ctx.temp("cond");
    let mut prelude = String::new();
    let cond_expr = if cond.is_symbol {
        cond.code.clone()
    } else {
        prelude.push_str(&format!(
            "let {cond_var} = {cond};\n",
            cond_var = cond_var,
            cond = cond.code
        ));
        cond_var
    };
    let check = match &cond.kind {
        TypedKind::Bool => cond_expr.clone(),
        TypedKind::Optional(inner) => {
            if matches!(**inner, TypedKind::Bool) {
                format!("matches!({cond_expr}, Some(true))", cond_expr = cond_expr)
            } else {
                format!("{cond_expr}.is_some()", cond_expr = cond_expr)
            }
        }
        _ => "true".to_string(),
    };
    (prelude, check)
}

fn emit_typed_truthy_check_from_ast(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    cond: &AstExpr,
) -> Option<(String, String)> {
    let AstExpr::Call { callee, args } = cond else {
        return None;
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return None;
    };
    let canonical = aliases::resolve_alias(sym);
    match canonical {
        "re-find" => emit_typed_regex_truthy(ctx, env, fns, locals, args, false),
        "re-seq" => emit_typed_regex_truthy(ctx, env, fns, locals, args, false),
        "re-matches" => emit_typed_regex_truthy(ctx, env, fns, locals, args, true),
        _ => None,
    }
}

fn emit_typed_regex_truthy(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
    full_match: bool,
) -> Option<(String, String)> {
    if args.len() != 2 {
        return None;
    }
    let pattern_ast = &args[0];
    let text = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if text.kind != TypedKind::Str {
        return None;
    }
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let mut prelude = String::new();
    if text.is_symbol {
        prelude.push_str(&format!(
            "let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        ));
    } else {
        prelude.push_str(&format!(
            "let {text_var} = {text};\nlet {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        ));
    }
    let regex_var = ctx.temp("re");
    match pattern_ast {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let regex_expr = emit_static_regex_expr(ctx, pattern, "regex expects regex");
            prelude.push_str(&format!(
                "let {regex_var} = {regex_expr};\n",
                regex_var = regex_var,
                regex_expr = regex_expr
            ));
        }
        _ => {
            let pattern = emit_typed_expr(ctx, env, fns, locals, pattern_ast)?;
            if pattern.kind != TypedKind::Str {
                return None;
            }
            let pat_var = ctx.temp("pat");
            let pat_ref = ctx.temp("pat_ref");
            if pattern.is_symbol {
                prelude.push_str(&format!(
                    "let {pat_ref} = {pattern}.as_str();\n",
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            } else {
                prelude.push_str(&format!(
                    "let {pat_var} = {pattern};\nlet {pat_ref} = {pat_var}.as_str();\n",
                    pat_var = pat_var,
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            }
            prelude.push_str(&format!(
                "let {regex_var} = Regex::new({pat_ref}).expect(\"regex expects regex\");\n",
                regex_var = regex_var,
                pat_ref = pat_ref
            ));
        }
    }
    let check = if full_match {
        let m_var = ctx.temp("m");
        prelude.push_str(&format!(
            "let {m_var} = {regex_var}.find({text_ref});\n",
            m_var = m_var,
            regex_var = regex_var,
            text_ref = text_ref
        ));
        format!(
            "{m_var}.map(|m| m.start() == 0 && m.end() == {text_ref}.len()).unwrap_or(false)",
            m_var = m_var,
            text_ref = text_ref
        )
    } else {
        format!("{regex_var}.is_match({text_ref})", regex_var = regex_var, text_ref = text_ref)
    };
    Some((prelude, check))
}

#[derive(Clone)]
struct NativeFnDef {
    name: String,
    params: Vec<Param>,
    body: Vec<AstExpr>,
}

struct CodegenContext {
    native_fns: Vec<NativeFnDef>,
    fn_counter: usize,
}

impl CodegenContext {
    fn new() -> Self {
        Self {
            native_fns: Vec::new(),
            fn_counter: 0,
        }
    }

    fn add_native_fn(&mut self, params: &[Param], body: &[AstExpr]) -> String {
        let name = format!("native_fn_{}", self.fn_counter);
        self.fn_counter += 1;
        self.native_fns.push(NativeFnDef {
            name: name.clone(),
            params: params.to_vec(),
            body: body.to_vec(),
        });
        name
    }
}

pub fn emit_rust_program(items: &[TopLevel], default_mut_mode: MutMode) -> Result<String, String> {
    if let Some(out) = emit_typed_program(items) {
        return Ok(out);
    }
    let mut ctx = CodegenContext::new();
    let mut main_body = String::new();
    for item in items {
        main_body.push_str(&emit_top_level_exec(&mut ctx, item)?);
    }
    let mut out = String::new();
    out.push_str("#![allow(unused_imports)]\n");
    out.push_str("#![allow(unused_assignments)]\n");
    out.push_str("use clove2_core::ast::{Expr, ExprKind, Literal, Span};\n");
    out.push_str("use clove2_core::error::Clove2Error;\n");
    out.push_str("use clove2_core::eval::{is_truthy, NativeEnv, Runtime};\n");
    out.push_str("use clove2_core::syntax::{AstExpr, Param};\n");
    out.push_str("use clove2_core::use_directive::MutMode;\n");
    out.push_str("use clove2_core::value::{Key, Value};\n");
    out.push_str("use std::collections::BTreeMap;\n");
    let mut idx = 0usize;
    while idx < ctx.native_fns.len() {
        let def = ctx.native_fns[idx].clone();
        out.push_str(&emit_native_function(&mut ctx, &def)?);
        out.push('\n');
        idx += 1;
    }
    out.push_str("fn main() {\n");
    out.push_str("    let mut runtime = Runtime::new();\n");
    let mode_line = match default_mut_mode {
        MutMode::Mut => "    runtime.set_default_mut_mode(MutMode::Mut);\n",
        MutMode::Imut => "    runtime.set_default_mut_mode(MutMode::Imut);\n",
    };
    out.push_str(mode_line);
    out.push_str(&main_body);
    out.push_str("}\n");
    Ok(out)
}

fn emit_typed_program(items: &[TopLevel]) -> Option<String> {
    let mut ctx = TypedContext::new();
    let mut env: HashMap<String, TypedKind> = HashMap::new();
    let mut fns: HashMap<String, TypedFnSig> = HashMap::new();
    let mut fn_defs: Vec<String> = Vec::new();
    let mut main_body = String::new();
    for item in items {
        match item {
            TopLevel::Def { name, value, .. } => {
                let locals: HashMap<String, TypedKind> = HashMap::new();
                let typed = match emit_typed_expr(&mut ctx, &env, &fns, &locals, value) {
                    Some(typed) => typed,
                    None => {
                        typed_debug(&format!("def {} failed: {:?}", name, value));
                        return None;
                    }
                };
                let rust_ty = typed_kind_to_rust(&typed.kind)?;
                main_body.push_str("    let ");
                main_body.push_str(&rust_ident(name));
                main_body.push_str(": ");
                main_body.push_str(&rust_ty);
                main_body.push_str(" = ");
                main_body.push_str(&typed.code);
                main_body.push_str(";\n");
                env.insert(name.clone(), typed.kind.clone());
            }
            TopLevel::Defn {
                name,
                params,
                ret,
                body,
                ..
            } => {
                let defn = match emit_typed_defn(&mut ctx, &env, &fns, name, params, ret, body) {
                    Some(defn) => defn,
                    None => {
                        typed_debug(&format!("defn {} failed", name));
                        return None;
                    }
                };
                fns.insert(name.clone(), defn.sig);
                fn_defs.push(defn.code);
            }
            TopLevel::Expr { expr, .. } => {
                if is_use_form(expr) {
                    continue;
                }
                let locals: HashMap<String, TypedKind> = HashMap::new();
                let stmt = match emit_typed_stmt(&mut ctx, &env, &fns, &locals, expr) {
                    Some(stmt) => stmt,
                    None => {
                        typed_debug(&format!("expr failed: {:?}", expr));
                        return None;
                    }
                };
                main_body.push_str("    ");
                main_body.push_str(&stmt);
                main_body.push('\n');
            }
            TopLevel::DefType { .. } | TopLevel::DefForeign { .. } => {
                typed_debug("def-type/def-foreign not supported in typed codegen");
                return None;
            }
        }
    }
    let mut out = String::new();
    out.push_str("#![allow(unused_imports)]\n");
    out.push_str("use std::collections::BTreeMap;\n");
    out.push_str("use clove2_core::value::Key;\n");
    out.push_str("use clove2_core::Regex;\n");
    out.push_str("#[derive(Clone, Debug)]\n");
    out.push_str("enum Union2<A, B> {\n    A(A),\n    B(B),\n}\n");
    if !ctx.key_literals.is_empty() {
        out.push_str("thread_local! {\n");
        for def in &ctx.key_literals {
            out.push_str(&format!(
                "    static {name}: std::cell::OnceCell<Key> = std::cell::OnceCell::new();\n",
                name = def.name
            ));
        }
        out.push_str("}\n");
    }
    if !ctx.regex_literals.is_empty() {
        for def in &ctx.regex_literals {
            out.push_str(&format!(
                "static {name}: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();\n",
                name = def.name
            ));
        }
    }
    for def in fn_defs {
        out.push_str(&def);
        out.push('\n');
    }
    out.push_str("fn main() {\n");
    out.push_str(&main_body);
    out.push_str("}\n");
    Some(out)
}

fn emit_typed_stmt(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    expr: &AstExpr,
) -> Option<String> {
    let AstExpr::Call { callee, args } = expr else {
        return None;
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return None;
    };
    let canonical = aliases::resolve_alias(sym);
    if canonical != "println" || args.len() != 1 {
        return None;
    }
    let typed = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    match typed.kind {
        TypedKind::Int
        | TypedKind::Float
        | TypedKind::Bool
        | TypedKind::Str
        | TypedKind::Keyword
        | TypedKind::Symbol => Some(format!("println!(\"{{}}\", {});", typed.code)),
        TypedKind::Vec(_)
        | TypedKind::Map(_, _)
        | TypedKind::Optional(_)
        | TypedKind::Union2(_, _) => Some(format!("println!(\"{{:?}}\", {});", typed.code)),
    }
}

fn emit_typed_expr(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    expr: &AstExpr,
) -> Option<TypedExpr> {
    match expr {
        AstExpr::Literal(Literal::Int(value)) => Some(TypedExpr {
            code: format!("{}i64", value),
            kind: TypedKind::Int,
            is_symbol: false,
        }),
        AstExpr::Literal(Literal::Float(value)) => Some(TypedExpr {
            code: format!("{}f64", value),
            kind: TypedKind::Float,
            is_symbol: false,
        }),
        AstExpr::Literal(Literal::Nil) => None,
        AstExpr::Literal(Literal::Bool(value)) => Some(TypedExpr {
            code: format!("{}", value),
            kind: TypedKind::Bool,
            is_symbol: false,
        }),
        AstExpr::Literal(Literal::Str(value)) => Some(TypedExpr {
            code: format!("String::from({})", emit_string_literal(value)),
            kind: TypedKind::Str,
            is_symbol: false,
        }),
        AstExpr::Keyword(value) => Some(TypedExpr {
            code: format!("String::from({})", emit_string_literal(value)),
            kind: TypedKind::Keyword,
            is_symbol: false,
        }),
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => emit_typed_if(ctx, env, fns, locals, cond, then_expr, else_expr.as_deref()),
        AstExpr::Let { bindings, body } => emit_typed_let(ctx, env, fns, locals, bindings, body),
        AstExpr::Symbol(sym) => {
            if let Some(kind) = locals.get(sym).or_else(|| env.get(sym)) {
                Some(TypedExpr {
                    code: rust_ident(sym),
                    kind: kind.clone(),
                    is_symbol: true,
                })
            } else {
                typed_debug(&format!("symbol not found: {}", sym));
                None
            }
        }
        AstExpr::Vector(items) => emit_typed_vector(ctx, env, fns, locals, items),
        AstExpr::Map(entries) => emit_typed_map_literal(ctx, env, fns, locals, entries),
        AstExpr::Call { callee, args } => {
            let AstExpr::Symbol(sym) = callee.as_ref() else {
                return None;
            };
            let canonical = aliases::resolve_alias(sym);
            if canonical == "contains?" {
                typed_debug("emit_typed_expr contains?");
            }
            if canonical == "apply" {
                typed_debug("emit_typed_expr apply");
            }
            let rewritten_args = rewrite_binding_sugar(canonical, args);
            let args = rewritten_args.as_deref().unwrap_or(args);
            match canonical {
                "do" => emit_typed_do(ctx, env, fns, locals, args),
                "mut" | "imut" => emit_typed_do(ctx, env, fns, locals, args),
                "range" => emit_typed_range(ctx, env, fns, locals, args),
                "map" => emit_typed_map(ctx, env, fns, locals, args),
                "filter" => emit_typed_filter(ctx, env, fns, locals, args),
                "reduce" => emit_typed_reduce(ctx, env, fns, locals, args),
                "apply" => emit_typed_apply(ctx, env, fns, locals, args),
                "count" => emit_typed_count(ctx, env, fns, locals, args),
                "assoc" => emit_typed_assoc(ctx, env, fns, locals, args),
                "get" => emit_typed_get(ctx, env, fns, locals, args),
                "get-in" => emit_typed_get_in(ctx, env, fns, locals, args),
                "nth" => emit_typed_nth(ctx, env, fns, locals, args),
                "assoc-in" => emit_typed_assoc_in(ctx, env, fns, locals, args),
                "update-in" => emit_typed_update_in(ctx, env, fns, locals, args),
                "identity" => emit_typed_identity(ctx, env, fns, locals, args),
                "some" => emit_typed_some(ctx, env, fns, locals, args),
                "every?" => emit_typed_every(ctx, env, fns, locals, args),
                "not-every?" => emit_typed_not_every(ctx, env, fns, locals, args),
                "contains?" => emit_typed_contains(ctx, env, fns, locals, args),
                "rest" => emit_typed_rest(ctx, env, fns, locals, args),
                "conj" => emit_typed_conj(ctx, env, fns, locals, args),
                "empty?" => emit_typed_empty_pred(ctx, env, fns, locals, args),
                "hash-map" => emit_typed_hash_map(ctx, env, fns, locals, args),
                "interleave" => emit_typed_interleave(ctx, env, fns, locals, args),
                "sort" => emit_typed_sort(ctx, env, fns, locals, args),
                "sort-by" => emit_typed_sort_by(ctx, env, fns, locals, args),
                "split" => emit_typed_split(ctx, env, fns, locals, args),
                "re-find" => {
                    typed_debug("emit_typed_expr re-find");
                    emit_typed_re_find(ctx, env, fns, locals, args)
                }
                "re-matches" => {
                    typed_debug("emit_typed_expr re-matches");
                    emit_typed_re_matches(ctx, env, fns, locals, args)
                }
                "re-seq" => {
                    typed_debug("emit_typed_expr re-seq");
                    emit_typed_re_seq(ctx, env, fns, locals, args)
                }
                "replace" => emit_typed_replace(ctx, env, fns, locals, args, false),
                "replace-first" => emit_typed_replace(ctx, env, fns, locals, args, true),
                "join" => emit_typed_join(ctx, env, fns, locals, args),
                "vector" | "list" => emit_typed_vector_call(ctx, env, fns, locals, args),
                "remove" => emit_typed_remove(ctx, env, fns, locals, args),
                "drop-while" => emit_typed_drop_while(ctx, env, fns, locals, args),
                "keep" => emit_typed_keep(ctx, env, fns, locals, args),
                "keep-indexed" => emit_typed_keep_indexed(ctx, env, fns, locals, args),
                "when" => emit_typed_when(ctx, env, fns, locals, args),
                "+" | "-" | "*" => emit_typed_arith(ctx, env, fns, locals, canonical, args),
                "/" | "mod" | "rem" => emit_typed_arith(ctx, env, fns, locals, canonical, args),
                "inc" | "dec" => emit_typed_unary_arith(ctx, env, fns, locals, canonical, args),
                "bit-and" | "bit-or" | "bit-xor" => {
                    emit_typed_bitwise(ctx, env, fns, locals, canonical, args)
                }
                "bit-not" => emit_typed_bit_not(ctx, env, fns, locals, args),
                "bit-shift-left" | "bit-shift-right" => {
                    emit_typed_bit_shift(ctx, env, fns, locals, canonical, args)
                }
                "min" | "max" => emit_typed_min_max(ctx, env, fns, locals, canonical, args),
                "<" | ">" | "<=" | ">=" | "=" => {
                    emit_typed_compare(ctx, env, fns, locals, canonical, args)
                }
                "compare" => emit_typed_compare_fn(ctx, env, fns, locals, args),
                "even?" | "odd?" => emit_typed_parity(ctx, env, fns, locals, canonical, args),
                "not" => emit_typed_not(ctx, env, fns, locals, args),
                _ => emit_typed_call_function(ctx, env, fns, locals, canonical, args),
            }
        }
        _ => None,
    }
}

struct TypedFnDef {
    sig: TypedFnSig,
    code: String,
}

fn emit_typed_defn(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    name: &str,
    params: &[Param],
    ret: &Option<Type>,
    body: &[AstExpr],
) -> Option<TypedFnDef> {
    let mut locals: HashMap<String, TypedKind> = HashMap::new();
    let mut param_kinds = Vec::new();
    let mut rest_kind: Option<TypedKind> = None;
    let mut rust_params = Vec::new();
    for param in params {
        let ty = param.ty.as_ref()?;
        let kind = typed_kind_from_type(ty)?;
        let rust_ty = typed_kind_to_rust(&kind)?;
        let rust_param = rust_ident(&param.name);
        rust_params.push(format!("{}: {}", rust_param, rust_ty));
        if param.rest {
            if rest_kind.is_some() {
                return None;
            }
            if !matches!(kind, TypedKind::Vec(_)) {
                return None;
            }
            rest_kind = Some(kind.clone());
        } else {
            param_kinds.push(kind.clone());
        }
        locals.insert(param.name.clone(), kind);
    }
    let body_expr = emit_typed_block_expr(ctx, env, fns, &locals, body)?;
    let ret_kind = match ret {
        Some(ty) => {
            let annotated = typed_kind_from_type(ty)?;
            if annotated != body_expr.kind {
                return None;
            }
            annotated
        }
        None => body_expr.kind,
    };
    let rust_ret = typed_kind_to_rust(&ret_kind)?;
    let params_joined = rust_params.join(", ");
    let rust_name = rust_ident(name);
    let code = format!(
        "fn {name}({params}) -> {ret} {{\n{body}\n}}\n",
        name = rust_name,
        params = params_joined,
        ret = rust_ret,
        body = indent(&body_expr.code, 4)
    );
    let sig = TypedFnSig {
        params: param_kinds,
        rest: rest_kind,
        ret: ret_kind,
    };
    Some(TypedFnDef { sig, code })
}

fn emit_typed_block_expr(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    body: &[AstExpr],
) -> Option<TypedExpr> {
    let (last, rest) = body.split_last()?;
    if rest.is_empty() {
        return emit_typed_expr(ctx, env, fns, locals, last);
    }
    let mut out = String::new();
    out.push_str("{\n");
    for expr in rest {
        let typed = emit_typed_expr(ctx, env, fns, locals, expr)?;
        out.push_str("    let _ = ");
        out.push_str(&typed.code);
        out.push_str(";\n");
    }
    let last_expr = emit_typed_expr(ctx, env, fns, locals, last)?;
    out.push_str("    ");
    out.push_str(&last_expr.code);
    out.push_str("\n}");
    Some(TypedExpr {
        code: out,
        kind: last_expr.kind,
        is_symbol: false,
    })
}

fn emit_typed_vector(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    items: &[AstExpr],
) -> Option<TypedExpr> {
    let mut elem_kind: Option<TypedKind> = None;
    let mut codes = Vec::new();
    for item in items {
        let typed = emit_typed_expr(ctx, env, fns, locals, item)?;
        if let Some(existing) = &elem_kind {
            if existing != &typed.kind {
                return None;
            }
        } else {
            elem_kind = Some(typed.kind.clone());
        }
        codes.push(typed.code);
    }
    let elem_kind = elem_kind?;
    let code = format!("vec![{}]", codes.join(", "));
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_vector_call(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.is_empty() {
        typed_debug("vector/list: empty args not supported in typed codegen");
        return None;
    }
    let mut elem_kind: Option<TypedKind> = None;
    let mut codes = Vec::new();
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        if let Some(existing) = &elem_kind {
            if existing != &typed.kind {
                typed_debug("vector/list: element kind mismatch");
                return None;
            }
        } else {
            elem_kind = Some(typed.kind.clone());
        }
        codes.push(typed.code);
    }
    let elem_kind = elem_kind?;
    let code = format!("vec![{}]", codes.join(", "));
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_static_key_expr(ctx: &mut TypedContext, kind: KeyLiteralKind, value: &str) -> String {
    let name = ctx.key_literal(kind, value);
    let literal = emit_string_literal(value);
    let ctor = match kind {
        KeyLiteralKind::Str => format!("Key::Str(std::rc::Rc::<str>::from({}))", literal),
        KeyLiteralKind::Keyword => format!("Key::Keyword(std::rc::Rc::<str>::from({}))", literal),
        KeyLiteralKind::Symbol => format!("Key::Symbol(std::rc::Rc::<str>::from({}))", literal),
    };
    format!(
        "{name}.with(|cell| cell.get_or_init(|| {ctor}).clone())",
        name = name,
        ctor = ctor
    )
}

fn emit_static_regex_expr(ctx: &mut TypedContext, pattern: &str, message: &str) -> String {
    let name = ctx.regex_literal(pattern);
    let literal = emit_string_literal(pattern);
    format!(
        "{name}.get_or_init(|| Regex::new({literal}).expect({message}))",
        name = name,
        literal = literal,
        message = emit_string_literal(message)
    )
}

fn emit_key_expr(ctx: &mut TypedContext, key_ast: &AstExpr, key: &TypedExpr) -> Option<String> {
    match key_ast {
        AstExpr::Keyword(value) => {
            return Some(emit_static_key_expr(ctx, KeyLiteralKind::Keyword, value));
        }
        AstExpr::Literal(Literal::Str(value)) => {
            return Some(emit_static_key_expr(ctx, KeyLiteralKind::Str, value));
        }
        _ => {}
    }
    match key.kind {
        TypedKind::Str => {
            if key.is_symbol {
                Some(format!(
                    "Key::Str(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            } else {
                Some(format!(
                    "Key::Str(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            }
        }
        TypedKind::Keyword => {
            if key.is_symbol {
                Some(format!(
                    "Key::Keyword(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            } else {
                Some(format!(
                    "Key::Keyword(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            }
        }
        TypedKind::Symbol => {
            if key.is_symbol {
                Some(format!(
                    "Key::Symbol(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            } else {
                Some(format!(
                    "Key::Symbol(std::rc::Rc::<str>::from({}.as_str()))",
                    key.code
                ))
            }
        }
        TypedKind::Int => Some(format!("Key::Int({})", key.code)),
        TypedKind::Bool => Some(format!("Key::Bool({})", key.code)),
        _ => None,
    }
}

fn emit_key_from_typed(kind: &TypedKind, expr: &str, is_symbol: bool) -> Option<String> {
    match kind {
        TypedKind::Str => Some(format!(
            "Key::Str(std::rc::Rc::<str>::from({}.as_str()))",
            expr
        )),
        TypedKind::Keyword => Some(format!(
            "Key::Keyword(std::rc::Rc::<str>::from({}.as_str()))",
            expr
        )),
        TypedKind::Symbol => Some(format!(
            "Key::Symbol(std::rc::Rc::<str>::from({}.as_str()))",
            expr
        )),
        TypedKind::Int => Some(format!("Key::Int({})", expr)),
        TypedKind::Bool => Some(format!("Key::Bool({})", expr)),
        TypedKind::Union2(_, _)
        | TypedKind::Optional(_)
        | TypedKind::Vec(_)
        | TypedKind::Map(_, _) => {
            let _ = is_symbol;
            None
        }
        TypedKind::Float => None,
    }
}

fn emit_value_expr_for_insert(value: &TypedExpr) -> String {
    if value.is_symbol && !typed_kind_is_copy(&value.kind) {
        format!("{}.clone()", value.code)
    } else {
        value.code.clone()
    }
}

fn emit_typed_map_literal(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    entries: &[(AstExpr, AstExpr)],
) -> Option<TypedExpr> {
    let mut key_kind: Option<TypedKind> = None;
    let mut value_kind: Option<TypedKind> = None;
    let mut pairs = Vec::new();
    for (key_expr, value_expr) in entries {
        let key_typed = emit_typed_expr(ctx, env, fns, locals, key_expr)?;
        let val_typed = emit_typed_expr(ctx, env, fns, locals, value_expr)?;
        if let Some(existing) = &key_kind {
            if existing != &key_typed.kind {
                return None;
            }
        } else {
            key_kind = Some(key_typed.kind.clone());
        }
        if let Some(existing) = &value_kind {
            if existing != &val_typed.kind {
                return None;
            }
        } else {
            value_kind = Some(val_typed.kind.clone());
        }
        pairs.push((key_expr, key_typed, val_typed));
    }
    let key_kind = key_kind?;
    let value_kind = value_kind?;
    let map_type = TypedKind::Map(Box::new(key_kind.clone()), Box::new(value_kind.clone()));
    let value_rust = typed_kind_to_rust(&value_kind)?;
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&format!(
        "    let mut map: BTreeMap<Key, {value_rust}> = BTreeMap::new();\n",
        value_rust = value_rust
    ));
    for (key_ast, key_typed, val_typed) in pairs {
        let key_expr = emit_key_expr(ctx, key_ast, &key_typed)?;
        let value_expr = emit_value_expr_for_insert(&val_typed);
        out.push_str("    map.insert(");
        out.push_str(&key_expr);
        out.push_str(", ");
        out.push_str(&value_expr);
        out.push_str(");\n");
    }
    out.push_str("    map\n}");
    Some(TypedExpr {
        code: out,
        kind: map_type,
        is_symbol: false,
    })
}

fn emit_typed_if(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    cond: &AstExpr,
    then_expr: &AstExpr,
    else_expr: Option<&AstExpr>,
) -> Option<TypedExpr> {
    let (cond_prelude, cond_check) =
        if let Some(pair) = emit_typed_truthy_check_from_ast(ctx, env, fns, locals, cond) {
            pair
        } else {
            let cond_expr = emit_typed_expr(ctx, env, fns, locals, cond)?;
            emit_typed_truthy_check(ctx, &cond_expr)
        };
    let then_expr = emit_typed_expr(ctx, env, fns, locals, then_expr)?;
    if let Some(else_expr) = else_expr {
        let else_expr = emit_typed_expr(ctx, env, fns, locals, else_expr)?;
        if then_expr.kind != else_expr.kind {
            return None;
        }
        let code = format!(
            "{{\n{cond_prelude}    if {cond_check} {{ {then_code} }} else {{ {else_code} }}\n}}",
            cond_prelude = indent(&cond_prelude, 4),
            cond_check = cond_check,
            then_code = then_expr.code,
            else_code = else_expr.code
        );
        return Some(TypedExpr {
            code,
            kind: then_expr.kind,
            is_symbol: false,
        });
    }
    let (then_code, result_kind) = match then_expr.kind {
        TypedKind::Optional(inner) => (then_expr.code, TypedKind::Optional(inner)),
        other => (
            format!("Some({})", then_expr.code),
            TypedKind::Optional(Box::new(other)),
        ),
    };
    let code = format!(
        "{{\n{cond_prelude}    if {cond_check} {{ {then_code} }} else {{ None }}\n}}",
        cond_prelude = indent(&cond_prelude, 4),
        cond_check = cond_check,
        then_code = then_code
    );
    Some(TypedExpr {
        code,
        kind: result_kind,
        is_symbol: false,
    })
}

fn emit_typed_let(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    bindings: &[crate::syntax::Binding],
    body: &[AstExpr],
) -> Option<TypedExpr> {
    let mut scoped = locals.clone();
    let mut out = String::new();
    out.push_str("{\n");
    for binding in bindings {
        let typed = emit_typed_expr(ctx, env, fns, &scoped, &binding.value)?;
        scoped.insert(binding.name.clone(), typed.kind.clone());
        out.push_str("    let ");
        out.push_str(&rust_ident(&binding.name));
        out.push_str(" = ");
        out.push_str(&typed.code);
        out.push_str(";\n");
    }
    let body_expr = emit_typed_block_expr(ctx, env, fns, &scoped, body)?;
    out.push_str(&indent(&body_expr.code, 4));
    out.push_str("\n}");
    Some(TypedExpr {
        code: out,
        kind: body_expr.kind,
        is_symbol: false,
    })
}

fn emit_typed_do(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    emit_typed_block_expr(ctx, env, fns, locals, args)
}

fn emit_typed_when(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 2 {
        return None;
    }
    let (cond_prelude, cond_check) =
        if let Some(pair) = emit_typed_truthy_check_from_ast(ctx, env, fns, locals, &args[0]) {
            pair
        } else {
            let cond_expr = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
            emit_typed_truthy_check(ctx, &cond_expr)
        };
    let body_expr = emit_typed_block_expr(ctx, env, fns, locals, &args[1..])?;
    let (body_code, result_kind) = match body_expr.kind {
        TypedKind::Optional(inner) => (body_expr.code, TypedKind::Optional(inner)),
        other => (
            format!("Some({})", body_expr.code),
            TypedKind::Optional(Box::new(other)),
        ),
    };
    let code = format!(
        "{{\n{cond_prelude}    if {cond_check} {{ {body_code} }} else {{ None }}\n}}",
        cond_prelude = indent(&cond_prelude, 4),
        cond_check = cond_check,
        body_code = body_code
    );
    Some(TypedExpr {
        code,
        kind: result_kind,
        is_symbol: false,
    })
}

fn emit_typed_call_function(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    name: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    let sig = fns.get(name)?;
    if sig.rest.is_none() && args.len() != sig.params.len() {
        return None;
    }
    if sig.rest.is_some() && args.len() < sig.params.len() {
        return None;
    }
    let mut arg_codes = Vec::new();
    for (idx, param_kind) in sig.params.iter().enumerate() {
        let typed = emit_typed_expr(ctx, env, fns, locals, &args[idx])?;
        if &typed.kind != param_kind {
            return None;
        }
        arg_codes.push(typed.code);
    }
    if let Some(rest_kind) = &sig.rest {
        let TypedKind::Vec(rest_inner) = rest_kind else {
            return None;
        };
        let mut rest_codes = Vec::new();
        for arg in &args[sig.params.len()..] {
            let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
            if &typed.kind != rest_inner.as_ref() {
                return None;
            }
            rest_codes.push(typed.code);
        }
        arg_codes.push(format!("vec![{}]", rest_codes.join(", ")));
    }
    Some(TypedExpr {
        code: format!("{}({})", rust_ident(name), arg_codes.join(", ")),
        kind: sig.ret.clone(),
        is_symbol: false,
    })
}

fn emit_typed_unary_inline(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    func: &AstExpr,
    param_kind: &TypedKind,
    value_var: &str,
) -> Option<InlineExpr> {
    match func {
        AstExpr::Symbol(sym) => {
            let canonical = aliases::resolve_alias(sym);
            match canonical {
                "inc" => {
                    if param_kind != &TypedKind::Int && param_kind != &TypedKind::Float {
                        return None;
                    }
                    let op = if param_kind == &TypedKind::Float {
                        "+ 1.0"
                    } else {
                        "+ 1"
                    };
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!("{value_var} {op}", value_var = value_var, op = op),
                        kind: param_kind.clone(),
                    });
                }
                "dec" => {
                    if param_kind != &TypedKind::Int && param_kind != &TypedKind::Float {
                        return None;
                    }
                    let op = if param_kind == &TypedKind::Float {
                        "- 1.0"
                    } else {
                        "- 1"
                    };
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!("{value_var} {op}", value_var = value_var, op = op),
                        kind: param_kind.clone(),
                    });
                }
                "even?" => {
                    if param_kind != &TypedKind::Int {
                        return None;
                    }
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!("{value_var} % 2 == 0", value_var = value_var),
                        kind: TypedKind::Bool,
                    });
                }
                "odd?" => {
                    if param_kind != &TypedKind::Int {
                        return None;
                    }
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!("{value_var} % 2 != 0", value_var = value_var),
                        kind: TypedKind::Bool,
                    });
                }
                "identity" => {
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: value_var.to_string(),
                        kind: param_kind.clone(),
                    });
                }
                "count" => {
                    if matches!(param_kind, TypedKind::Vec(_))
                        || matches!(param_kind, TypedKind::Map(_, _))
                    {
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!("{value}.len() as i64", value = value_var),
                            kind: TypedKind::Int,
                        });
                    }
                    if param_kind == &TypedKind::Str {
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!("{value}.chars().count() as i64", value = value_var),
                            kind: TypedKind::Int,
                        });
                    }
                    return None;
                }
                "first" => {
                    if let TypedKind::Vec(inner) = param_kind {
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!("{value}.first().cloned()", value = value_var),
                            kind: TypedKind::Optional(Box::new((**inner).clone())),
                        });
                    }
                    if param_kind == &TypedKind::Str {
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!(
                                "{value}.chars().next().map(|ch| ch.to_string())",
                                value = value_var
                            ),
                            kind: TypedKind::Optional(Box::new(TypedKind::Str)),
                        });
                    }
                    return None;
                }
                _ => {
                    if let Some(sig) = fns.get(canonical) {
                        if sig.params.len() != 1 || sig.rest.is_some() {
                            return None;
                        }
                        if &sig.params[0] != param_kind {
                            return None;
                        }
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!(
                                "{name}({value})",
                                name = rust_ident(canonical),
                                value = value_var
                            ),
                            kind: sig.ret.clone(),
                        });
                    }
                }
            }
            None
        }
        AstExpr::Fn { params, body, .. } => {
            if params.len() != 1 || params[0].rest {
                typed_debug("unary_inline: fn expects single param");
                return None;
            }
            let mut inline_locals = locals.clone();
            let param = &params[0];
            let annotated_kind = match &param.ty {
                Some(ty) => typed_kind_from_type(ty)?,
                None => param_kind.clone(),
            };
            if &annotated_kind != param_kind {
                typed_debug("unary_inline: param kind mismatch");
                return None;
            }
            inline_locals.insert(param.name.clone(), annotated_kind.clone());
            let body_expr = match emit_typed_block_expr(ctx, env, fns, &inline_locals, body) {
                Some(expr) => expr,
                None => {
                    typed_debug("unary_inline: body expr failed");
                    return None;
                }
            };
            Some(InlineExpr {
                prelude: format!(
                    "let {name} = {value};\n",
                    name = rust_ident(&param.name),
                    value = value_var
                ),
                expr: body_expr.code,
                kind: body_expr.kind,
            })
        }
        _ => None,
    }
}

fn emit_typed_binary_inline(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    func: &AstExpr,
    param_kind: &TypedKind,
    left_var: &str,
    right_var: &str,
) -> Option<InlineExpr> {
    match func {
        AstExpr::Symbol(sym) => {
            let canonical = aliases::resolve_alias(sym);
            match canonical {
                "+" | "-" | "*" => {
                    if param_kind != &TypedKind::Int && param_kind != &TypedKind::Float {
                        return None;
                    }
                    let op = canonical;
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!(
                            "{left} {op} {right}",
                            left = left_var,
                            op = op,
                            right = right_var
                        ),
                        kind: param_kind.clone(),
                    });
                }
                "<" | ">" | "<=" | ">=" => {
                    if param_kind != &TypedKind::Int && param_kind != &TypedKind::Float {
                        return None;
                    }
                    let op = canonical;
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!(
                            "{left} {op} {right}",
                            left = left_var,
                            op = op,
                            right = right_var
                        ),
                        kind: TypedKind::Bool,
                    });
                }
                "=" | "!=" | "not=" => {
                    if !matches!(
                        param_kind,
                        TypedKind::Int
                            | TypedKind::Float
                            | TypedKind::Bool
                            | TypedKind::Str
                            | TypedKind::Keyword
                            | TypedKind::Symbol
                    ) {
                        return None;
                    }
                    let op = if canonical == "=" { "==" } else { "!=" };
                    return Some(InlineExpr {
                        prelude: String::new(),
                        expr: format!(
                            "{left} {op} {right}",
                            left = left_var,
                            op = op,
                            right = right_var
                        ),
                        kind: TypedKind::Bool,
                    });
                }
                _ => {
                    if let Some(sig) = fns.get(canonical) {
                        if sig.params.len() != 2 || sig.rest.is_some() {
                            return None;
                        }
                        if &sig.params[0] != param_kind || &sig.params[1] != param_kind {
                            return None;
                        }
                        return Some(InlineExpr {
                            prelude: String::new(),
                            expr: format!(
                                "{name}({left}, {right})",
                                name = rust_ident(canonical),
                                left = left_var,
                                right = right_var
                            ),
                            kind: sig.ret.clone(),
                        });
                    }
                }
            }
            None
        }
        AstExpr::Fn { params, body, .. } => {
            if params.len() != 2 || params.iter().any(|p| p.rest) {
                return None;
            }
            let mut inline_locals = locals.clone();
            let left_param = &params[0];
            let right_param = &params[1];
            let left_kind = match &left_param.ty {
                Some(ty) => typed_kind_from_type(ty)?,
                None => param_kind.clone(),
            };
            let right_kind = match &right_param.ty {
                Some(ty) => typed_kind_from_type(ty)?,
                None => param_kind.clone(),
            };
            if &left_kind != param_kind || &right_kind != param_kind {
                return None;
            }
            inline_locals.insert(left_param.name.clone(), left_kind.clone());
            inline_locals.insert(right_param.name.clone(), right_kind.clone());
            let body_expr = emit_typed_block_expr(ctx, env, fns, &inline_locals, body)?;
            Some(InlineExpr {
                prelude: format!(
                    "let {left_name} = {left_val};\nlet {right_name} = {right_val};\n",
                    left_name = rust_ident(&left_param.name),
                    left_val = left_var,
                    right_name = rust_ident(&right_param.name),
                    right_val = right_var
                ),
                expr: body_expr.code,
                kind: body_expr.kind,
            })
        }
        _ => None,
    }
}

fn emit_typed_binary_inline_mixed(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    func: &AstExpr,
    left_kind: &TypedKind,
    right_kind: &TypedKind,
    left_var: &str,
    right_var: &str,
) -> Option<InlineExpr> {
    match func {
        AstExpr::Symbol(sym) => {
            let canonical = aliases::resolve_alias(sym);
            if let Some(sig) = fns.get(canonical) {
                if sig.params.len() != 2 || sig.rest.is_some() {
                    return None;
                }
                if &sig.params[0] != left_kind || &sig.params[1] != right_kind {
                    return None;
                }
                return Some(InlineExpr {
                    prelude: String::new(),
                    expr: format!(
                        "{name}({left}, {right})",
                        name = rust_ident(canonical),
                        left = left_var,
                        right = right_var
                    ),
                    kind: sig.ret.clone(),
                });
            }
            None
        }
        AstExpr::Fn { params, body, .. } => {
            if params.len() != 2 || params.iter().any(|p| p.rest) {
                return None;
            }
            let mut inline_locals = locals.clone();
            let left_param = &params[0];
            let right_param = &params[1];
            let left_param_kind = match &left_param.ty {
                Some(ty) => typed_kind_from_type(ty)?,
                None => left_kind.clone(),
            };
            let right_param_kind = match &right_param.ty {
                Some(ty) => typed_kind_from_type(ty)?,
                None => right_kind.clone(),
            };
            if &left_param_kind != left_kind || &right_param_kind != right_kind {
                return None;
            }
            inline_locals.insert(left_param.name.clone(), left_param_kind.clone());
            inline_locals.insert(right_param.name.clone(), right_param_kind.clone());
            let body_expr = emit_typed_block_expr(ctx, env, fns, &inline_locals, body)?;
            Some(InlineExpr {
                prelude: format!(
                    "let {left_name} = {left_val};\nlet {right_name} = {right_val};\n",
                    left_name = rust_ident(&left_param.name),
                    left_val = left_var,
                    right_name = rust_ident(&right_param.name),
                    right_val = right_var
                ),
                expr: body_expr.code,
                kind: body_expr.kind,
            })
        }
        _ => None,
    }
}

fn emit_typed_arith(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.is_empty() {
        return match op {
            "+" => Some(TypedExpr {
                code: "0i64".to_string(),
                kind: TypedKind::Int,
                is_symbol: false,
            }),
            "*" => Some(TypedExpr {
                code: "1i64".to_string(),
                kind: TypedKind::Int,
                is_symbol: false,
            }),
            _ => None,
        };
    }
    let mut typed_args = Vec::new();
    let mut result_kind = if op == "/" {
        TypedKind::Float
    } else {
        TypedKind::Int
    };
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        match typed.kind {
            TypedKind::Int => {}
            TypedKind::Float => result_kind = TypedKind::Float,
            _ => return None,
        }
        typed_args.push(typed);
    }
    let mut codes = Vec::new();
    for typed in typed_args {
        let code = if result_kind == TypedKind::Float && typed.kind == TypedKind::Int {
            format!("({} as f64)", typed.code)
        } else {
            typed.code
        };
        codes.push(format!("({})", code));
    }
    let code = match op {
        "+" => codes.join(" + "),
        "*" => codes.join(" * "),
        "-" => {
            if codes.len() == 1 {
                format!("-{}", codes[0])
            } else {
                codes.join(" - ")
            }
        }
        "/" => {
            if codes.len() == 1 {
                format!("1.0 / {}", codes[0])
            } else {
                codes.join(" / ")
            }
        }
        "mod" | "rem" => {
            if result_kind != TypedKind::Int || codes.len() != 2 {
                return None;
            }
            format!("{} % {}", codes[0], codes[1])
        }
        _ => return None,
    };
    Some(TypedExpr {
        code: format!("({})", code),
        kind: result_kind,
        is_symbol: false,
    })
}

fn emit_typed_bitwise(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 2 {
        return None;
    }
    let mut codes = Vec::new();
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        if typed.kind != TypedKind::Int {
            return None;
        }
        codes.push(format!("({})", typed.code));
    }
    let rust_op = match op {
        "bit-and" => "&",
        "bit-or" => "|",
        "bit-xor" => "^",
        _ => return None,
    };
    let code = codes.join(&format!(" {} ", rust_op));
    Some(TypedExpr {
        code: format!("({})", code),
        kind: TypedKind::Int,
        is_symbol: false,
    })
}

fn emit_typed_bit_not(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let typed = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    if typed.kind != TypedKind::Int {
        return None;
    }
    Some(TypedExpr {
        code: format!("!({})", typed.code),
        kind: TypedKind::Int,
        is_symbol: false,
    })
}

fn emit_typed_bit_shift(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let left = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let right = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if left.kind != TypedKind::Int || right.kind != TypedKind::Int {
        return None;
    }
    let rust_op = match op {
        "bit-shift-left" => "<<",
        "bit-shift-right" => ">>",
        _ => return None,
    };
    Some(TypedExpr {
        code: format!("({} {} {})", left.code, rust_op, right.code),
        kind: TypedKind::Int,
        is_symbol: false,
    })
}

fn emit_typed_min_max(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.is_empty() {
        return None;
    }
    let mut typed_args = Vec::new();
    let mut result_kind = TypedKind::Int;
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        match typed.kind {
            TypedKind::Int => {}
            TypedKind::Float => result_kind = TypedKind::Float,
            _ => return None,
        }
        typed_args.push(typed);
    }
    let rust_ty = typed_kind_to_rust(&result_kind)?;
    let acc_var = ctx.temp("acc");
    let mut out = String::new();
    out.push_str("{\n");
    let first = typed_args.remove(0);
    let first_expr = if result_kind == TypedKind::Float && first.kind == TypedKind::Int {
        format!("({} as f64)", first.code)
    } else {
        first.code
    };
    out.push_str(&format!(
        "    let mut {acc_var}: {rust_ty} = {first_expr};\n",
        acc_var = acc_var,
        rust_ty = rust_ty,
        first_expr = first_expr
    ));
    for typed in typed_args {
        let value_var = ctx.temp("v");
        let value_expr = if result_kind == TypedKind::Float && typed.kind == TypedKind::Int {
            format!("({} as f64)", typed.code)
        } else {
            typed.code
        };
        out.push_str(&format!(
            "    let {value_var}: {rust_ty} = {value_expr};\n",
            value_var = value_var,
            rust_ty = rust_ty,
            value_expr = value_expr
        ));
        let cmp = if op == "max" { ">" } else { "<" };
        out.push_str(&format!(
            "    if {value_var} {cmp} {acc_var} {{ {acc_var} = {value_var}; }}\n",
            value_var = value_var,
            cmp = cmp,
            acc_var = acc_var
        ));
    }
    out.push_str(&format!("    {acc_var}\n}}", acc_var = acc_var));
    Some(TypedExpr {
        code: out,
        kind: result_kind,
        is_symbol: false,
    })
}

fn emit_typed_unary_arith(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let typed = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let one = match typed.kind {
        TypedKind::Int => "1i64",
        TypedKind::Float => "1.0",
        _ => return None,
    };
    let code = match op {
        "inc" => format!("{} + {}", typed.code, one),
        "dec" => format!("{} - {}", typed.code, one),
        _ => return None,
    };
    Some(TypedExpr {
        code,
        kind: typed.kind,
        is_symbol: false,
    })
}

fn emit_typed_compare(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 2 {
        return None;
    }
    let mut typed_args = Vec::new();
    let mut result_kind = TypedKind::Int;
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        match typed.kind {
            TypedKind::Int => {}
            TypedKind::Float => result_kind = TypedKind::Float,
            _ => return None,
        }
        typed_args.push(typed);
    }
    let mut codes = Vec::new();
    for typed in typed_args {
        let code = if result_kind == TypedKind::Float && typed.kind == TypedKind::Int {
            format!("({} as f64)", typed.code)
        } else {
            typed.code
        };
        codes.push(code);
    }
    let mut parts = Vec::new();
    let rust_op = match op {
        "=" => "==",
        _ => op,
    };
    for pair in codes.windows(2) {
        parts.push(format!("{} {} {}", pair[0], rust_op, pair[1]));
    }
    let code = parts.join(" && ");
    Some(TypedExpr {
        code,
        kind: TypedKind::Bool,
        is_symbol: false,
    })
}

fn emit_typed_compare_fn(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let left = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let right = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let left_var = ctx.temp("left");
    let right_var = ctx.temp("right");
    let mut prelude = String::new();
    let left_expr = if left.is_symbol {
        left.code
    } else {
        prelude.push_str(&format!(
            "let {left_var} = {left};\n",
            left_var = left_var,
            left = left.code
        ));
        left_var.clone()
    };
    let right_expr = if right.is_symbol {
        right.code
    } else {
        prelude.push_str(&format!(
            "let {right_var} = {right};\n",
            right_var = right_var,
            right = right.code
        ));
        right_var.clone()
    };
    let ord_expr = match (&left.kind, &right.kind) {
        (TypedKind::Int, TypedKind::Int) => {
            format!("{left}.cmp(&{right})", left = left_expr, right = right_expr)
        }
        (TypedKind::Float, TypedKind::Float) => format!(
            "{left}.partial_cmp(&{right}).unwrap_or(std::cmp::Ordering::Equal)",
            left = left_expr,
            right = right_expr
        ),
        (TypedKind::Int, TypedKind::Float) | (TypedKind::Float, TypedKind::Int) => format!(
            "({left} as f64).partial_cmp(&({right} as f64)).unwrap_or(std::cmp::Ordering::Equal)",
            left = left_expr,
            right = right_expr
        ),
        (TypedKind::Bool, TypedKind::Bool) => {
            format!("{left}.cmp(&{right})", left = left_expr, right = right_expr)
        }
        (TypedKind::Str, TypedKind::Str)
        | (TypedKind::Keyword, TypedKind::Keyword)
        | (TypedKind::Symbol, TypedKind::Symbol) => {
            format!("{left}.cmp(&{right})", left = left_expr, right = right_expr)
        }
        _ => return None,
    };
    let code = format!(
        "{{\n{prelude}    let ord = {ord_expr};\n    match ord {{\n        std::cmp::Ordering::Less => -1,\n        std::cmp::Ordering::Equal => 0,\n        std::cmp::Ordering::Greater => 1,\n    }}\n}}",
        prelude = indent(&prelude, 4),
        ord_expr = ord_expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Int,
        is_symbol: false,
    })
}

fn emit_typed_parity(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    op: &str,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let typed = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    if typed.kind != TypedKind::Int {
        return None;
    }
    let expr = match op {
        "even?" => format!("{} % 2 == 0", typed.code),
        "odd?" => format!("{} % 2 != 0", typed.code),
        _ => return None,
    };
    Some(TypedExpr {
        code: expr,
        kind: TypedKind::Bool,
        is_symbol: false,
    })
}

fn emit_typed_not(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let typed = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    if typed.kind != TypedKind::Bool {
        return None;
    }
    Some(TypedExpr {
        code: format!("!({})", typed.code),
        kind: TypedKind::Bool,
        is_symbol: false,
    })
}

fn emit_typed_range(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.is_empty() || args.len() > 3 {
        return None;
    }
    let mut values = Vec::new();
    for arg in args {
        let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
        if typed.kind != TypedKind::Int {
            return None;
        }
        values.push(typed.code);
    }
    let (start, end, step) = if values.len() == 1 {
        ("0i64".to_string(), values[0].clone(), "1i64".to_string())
    } else if values.len() == 2 {
        (values[0].clone(), values[1].clone(), "1i64".to_string())
    } else {
        (values[0].clone(), values[1].clone(), values[2].clone())
    };
    let start_var = ctx.temp("start");
    let end_var = ctx.temp("end");
    let step_var = ctx.temp("step");
    let value_var = ctx.temp("value");
    let out_var = ctx.temp("out");
    let code = format!(
        "{{\n        let {start_var} = {start};\n        let {end_var} = {end};\n        let {step_var} = {step};\n        if {step_var} == 0 {{\n            panic!(\"range expects non-zero step\");\n        }}\n        let mut {out_var}: Vec<i64> = Vec::new();\n        if {step_var} > 0 {{\n            let mut {value_var} = {start_var};\n            while {value_var} < {end_var} {{\n                {out_var}.push({value_var});\n                {value_var} += {step_var};\n            }}\n        }} else {{\n            let mut {value_var} = {start_var};\n            while {value_var} > {end_var} {{\n                {out_var}.push({value_var});\n                {value_var} += {step_var};\n            }}\n        }}\n        {out_var}\n    }}",
        start_var = start_var,
        end_var = end_var,
        step_var = step_var,
        value_var = value_var,
        out_var = out_var,
        start = start,
        end = end,
        step = step
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(TypedKind::Int)),
        is_symbol: false,
    })
}

fn emit_typed_map(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        typed_debug("map: expected 2 args");
        return None;
    }
    let coll = match emit_typed_expr(ctx, env, fns, locals, &args[1]) {
        Some(expr) => expr,
        None => {
            typed_debug("map: collection expr failed");
            return None;
        }
    };
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        typed_debug("map: collection is not Vec");
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let inline =
        match emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var) {
            Some(expr) => expr,
            None => {
                typed_debug("map: inline unary failed");
                return None;
            }
        };
    let output_kind = TypedKind::Vec(Box::new(inline.kind));
    let out_elem_ty = typed_kind_to_rust(match &output_kind {
        TypedKind::Vec(inner) => inner.as_ref(),
        _ => return None,
    })?;
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::with_capacity({coll_ref}.len());\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            {out_var}.push({inline_expr});\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: output_kind,
        is_symbol: false,
    })
}

fn emit_typed_filter(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    if inline.kind != TypedKind::Bool {
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            if {inline_expr} {{\n                {out_var}.push({value_var});\n            }}\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_some(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    match coll.kind.clone() {
        TypedKind::Vec(elem_kind) => {
            let elem_kind = *elem_kind;
            let coll_var = ctx.temp("coll");
            let coll_ref = ctx.temp("coll_ref");
            let out_var = ctx.temp("out");
            let value_var = ctx.temp("value");
            let cand_var = ctx.temp("cand");
            let inline =
                emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
            let inline_kind = inline.kind.clone();
            let inline_prelude = inline.prelude.clone();
            let inline_expr = inline.expr.clone();
            let result_kind = match &inline_kind {
                TypedKind::Optional(inner) => TypedKind::Optional(inner.clone()),
                other => TypedKind::Optional(Box::new(other.clone())),
            };
            let out_ty = typed_kind_to_rust(&result_kind)?;
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let mut body = String::new();
            body.push_str(&format!(
                "let {cand_var} = {inline_expr};\n",
                cand_var = cand_var,
                inline_expr = inline_expr
            ));
            match &inline_kind {
                TypedKind::Bool => {
                    body.push_str(&format!(
                        "if {cand_var} {{\n    {out_var} = Some({cand_var});\n    break;\n}}\n",
                        cand_var = cand_var,
                        out_var = out_var
                    ));
                }
                TypedKind::Optional(_) => {
                    body.push_str(&format!(
                        "if let Some(value) = {cand_var} {{\n    {out_var} = Some(value);\n    break;\n}}\n",
                        cand_var = cand_var,
                        out_var = out_var
                    ));
                }
                _ => {
                    body.push_str(&format!(
                        "{out_var} = Some({cand_var});\nbreak;\n",
                        out_var = out_var,
                        cand_var = cand_var
                    ));
                }
            }
            let code = format!(
                "{{\n{coll_bind}        let mut {out_var}: {out_ty} = None;\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}{body}        }}\n        {out_var}\n    }}",
                coll_bind = coll_bind,
                coll_ref = coll_ref,
                out_var = out_var,
                out_ty = out_ty,
                value_var = value_var,
                inline_prelude = indent(&inline_prelude, 12),
                body = indent(&body, 12)
            );
            Some(TypedExpr {
                code,
                kind: result_kind,
                is_symbol: false,
            })
        }
        TypedKind::Str => {
            let coll_var = ctx.temp("text");
            let coll_ref = ctx.temp("text_ref");
            let out_var = ctx.temp("out");
            let value_var = ctx.temp("value");
            let cand_var = ctx.temp("cand");
            let inline = emit_typed_unary_inline(
                ctx,
                env,
                fns,
                locals,
                &args[0],
                &TypedKind::Str,
                &value_var,
            )?;
            let inline_kind = inline.kind.clone();
            let inline_prelude = inline.prelude.clone();
            let inline_expr = inline.expr.clone();
            let result_kind = match &inline_kind {
                TypedKind::Optional(inner) => TypedKind::Optional(inner.clone()),
                other => TypedKind::Optional(Box::new(other.clone())),
            };
            let out_ty = typed_kind_to_rust(&result_kind)?;
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = {coll}.as_str();\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = {coll_var}.as_str();\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let mut body = String::new();
            body.push_str(&format!(
                "let {cand_var} = {inline_expr};\n",
                cand_var = cand_var,
                inline_expr = inline_expr
            ));
            match &inline_kind {
                TypedKind::Bool => {
                    body.push_str(&format!(
                        "if {cand_var} {{\n    {out_var} = Some({cand_var});\n    break;\n}}\n",
                        cand_var = cand_var,
                        out_var = out_var
                    ));
                }
                TypedKind::Optional(_) => {
                    body.push_str(&format!(
                        "if let Some(value) = {cand_var} {{\n    {out_var} = Some(value);\n    break;\n}}\n",
                        cand_var = cand_var,
                        out_var = out_var
                    ));
                }
                _ => {
                    body.push_str(&format!(
                        "{out_var} = Some({cand_var});\nbreak;\n",
                        out_var = out_var,
                        cand_var = cand_var
                    ));
                }
            }
            let code = format!(
                "{{\n{coll_bind}        let mut {out_var}: {out_ty} = None;\n        for ch in {coll_ref}.chars() {{\n            let {value_var} = ch.to_string();\n{inline_prelude}{body}        }}\n        {out_var}\n    }}",
                coll_bind = coll_bind,
                coll_ref = coll_ref,
                out_var = out_var,
                out_ty = out_ty,
                value_var = value_var,
                inline_prelude = indent(&inline_prelude, 12),
                body = indent(&body, 12)
            );
            Some(TypedExpr {
                code,
                kind: result_kind,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_remove(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    if inline.kind != TypedKind::Bool {
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            if !{inline_expr} {{\n                {out_var}.push({value_var});\n            }}\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_drop_while(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    if inline.kind != TypedKind::Bool {
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
    let dropping_var = ctx.temp("dropping");
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        let mut {dropping_var} = true;\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n            if {dropping_var} {{\n{inline_prelude}                if {inline_expr} {{\n                    continue;\n                }}\n                {dropping_var} = false;\n            }}\n            {out_var}.push({value_var});\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        dropping_var = dropping_var,
        inline_prelude = indent(&inline.prelude, 16),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_keep(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    let TypedKind::Optional(inner_kind) = inline.kind else {
        return None;
    };
    let out_elem_ty = typed_kind_to_rust(&inner_kind)?;
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let keep_var = ctx.temp("keep");
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            let {keep_var} = {inline_expr};\n            if let Some(value) = {keep_var} {{\n                {out_var}.push(value);\n            }}\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr,
        keep_var = keep_var
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(inner_kind),
        is_symbol: false,
    })
}

fn emit_typed_keep_indexed(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let idx_var = ctx.temp("idx");
    let value_var = ctx.temp("value");
    let inline = emit_typed_binary_inline_mixed(
        ctx,
        env,
        fns,
        locals,
        &args[0],
        &TypedKind::Int,
        &elem_kind,
        &idx_var,
        &value_var,
    )?;
    let TypedKind::Optional(inner_kind) = inline.kind else {
        return None;
    };
    let out_elem_ty = typed_kind_to_rust(&inner_kind)?;
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let keep_var = ctx.temp("keep");
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        for ({idx_var}, {value_var}) in {coll_ref}.iter().enumerate() {{\n            let {idx_var} = {idx_var} as i64;\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            let {keep_var} = {inline_expr};\n            if let Some(value) = {keep_var} {{\n                {out_var}.push(value);\n            }}\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        out_var = out_var,
        idx_var = idx_var,
        value_var = value_var,
        out_elem_ty = out_elem_ty,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr,
        keep_var = keep_var
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(inner_kind),
        is_symbol: false,
    })
}

fn emit_typed_reduce(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 3 {
        typed_debug("reduce: expected 3 args");
        return None;
    }
    if let Some(expr) = emit_typed_reduce_fused(ctx, env, fns, locals, args) {
        return Some(expr);
    }
    let init = match emit_typed_expr(ctx, env, fns, locals, &args[1]) {
        Some(expr) => expr,
        None => {
            typed_debug("reduce: init expr failed");
            return None;
        }
    };
    let coll = match emit_typed_expr(ctx, env, fns, locals, &args[2]) {
        Some(expr) => expr,
        None => {
            typed_debug("reduce: collection expr failed");
            return None;
        }
    };
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        typed_debug("reduce: collection is not Vec");
        return None;
    };
    let elem_kind = *elem_kind;
    if init.kind != elem_kind {
        typed_debug("reduce: init kind mismatch");
        return None;
    }
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let acc_var = ctx.temp("acc");
    let value_var = ctx.temp("value");
    let init_kind = init.kind.clone();
    let init_code = init.code.clone();
    let inline = match emit_typed_binary_inline(
        ctx, env, fns, locals, &args[0], &init.kind, &acc_var, &value_var,
    ) {
        Some(expr) => expr,
        None => {
            typed_debug("reduce: inline binary failed");
            return None;
        }
    };
    if inline.kind != init.kind {
        typed_debug("reduce: inline kind mismatch");
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let code = format!(
        "{{\n{coll_bind}        let mut {acc_var} = {init};\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            {acc_var} = {inline_expr};\n        }}\n        {acc_var}\n    }}",
        coll_bind = coll_bind,
        acc_var = acc_var,
        value_var = value_var,
        coll_ref = coll_ref,
        init = init_code,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: init_kind,
        is_symbol: false,
    })
}

fn emit_typed_apply(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    typed_debug("apply: begin");
    if args.len() < 2 {
        typed_debug("apply: expects at least 2 args");
        return None;
    }
    let callee = &args[0];
    let fixed_args = &args[1..args.len() - 1];
    let tail_expr = &args[args.len() - 1];
    let tail = emit_typed_expr(ctx, env, fns, locals, tail_expr)?;
    let TypedKind::Vec(tail_elem_kind) = tail.kind.clone() else {
        typed_debug("apply: tail is not Vec");
        return None;
    };
    let tail_elem_kind = *tail_elem_kind;
    let (tail_bind, tail_ref) = if tail.is_symbol {
        (
            format!("        let tail_ref = &{};\n", tail.code),
            "tail_ref".to_string(),
        )
    } else {
        (
            format!(
                "        let tail = {};\n        let tail_ref = &tail;\n",
                tail.code
            ),
            "tail_ref".to_string(),
        )
    };

    if let AstExpr::Symbol(sym) = callee {
        let canonical = aliases::resolve_alias(sym);
        if canonical == "+" {
            let mut result_kind = match tail_elem_kind {
                TypedKind::Int | TypedKind::Float => tail_elem_kind.clone(),
                _ => return None,
            };
            let mut fixed_codes = Vec::new();
            for arg in fixed_args {
                let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
                match typed.kind {
                    TypedKind::Int => {}
                    TypedKind::Float => result_kind = TypedKind::Float,
                    _ => return None,
                }
                fixed_codes.push((typed.code, typed.kind));
            }
            let rust_ty = typed_kind_to_rust(&result_kind)?;
            let mut out = String::new();
            out.push_str("{\n");
            out.push_str(&tail_bind);
            let zero = if result_kind == TypedKind::Float {
                "0.0f64"
            } else {
                "0i64"
            };
            out.push_str(&format!(
                "        let mut acc: {rust_ty} = {zero};\n",
                rust_ty = rust_ty,
                zero = zero
            ));
            for (code, kind) in fixed_codes {
                let val = if result_kind == TypedKind::Float && kind == TypedKind::Int {
                    format!("({} as f64)", code)
                } else {
                    code
                };
                out.push_str(&format!("        acc += {val};\n", val = val));
            }
            let iter_expr = if result_kind == TypedKind::Float {
                format!(
                    "({tail_ref}).iter().map(|v| (*v) as f64)",
                    tail_ref = tail_ref
                )
            } else {
                format!("({tail_ref}).iter().cloned()", tail_ref = tail_ref)
            };
            out.push_str(&format!(
                "        for v in {iter_expr} {{\n            acc += v;\n        }}\n        acc\n    }}",
                iter_expr = iter_expr
            ));
            return Some(TypedExpr {
                code: out,
                kind: result_kind,
                is_symbol: false,
            });
        }

        if canonical == "vector" || canonical == "list" {
            let mut elem_kind: Option<TypedKind> = Some(tail_elem_kind.clone());
            let mut fixed_codes = Vec::new();
            for arg in fixed_args {
                let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
                if let Some(existing) = &elem_kind {
                    if existing != &typed.kind {
                        typed_debug("apply vector/list: element kind mismatch");
                        return None;
                    }
                }
                elem_kind = Some(typed.kind.clone());
                fixed_codes.push(typed.code);
            }
            let elem_kind = elem_kind?;
            let rust_ty = typed_kind_to_rust(&elem_kind)?;
            let mut out = String::new();
            out.push_str("{\n");
            out.push_str(&tail_bind);
            out.push_str(&format!(
                "        let mut out: Vec<{ty}> = Vec::with_capacity({fixed} + {tail}.len());\n",
                ty = rust_ty,
                fixed = fixed_codes.len(),
                tail = tail_ref
            ));
            for code in fixed_codes {
                out.push_str(&format!("        out.push({code});\n", code = code));
            }
            out.push_str(&format!(
                "        out.extend({tail}.iter().cloned());\n        out\n    }}",
                tail = tail_ref
            ));
            return Some(TypedExpr {
                code: out,
                kind: TypedKind::Vec(Box::new(elem_kind)),
                is_symbol: false,
            });
        }

        if let Some(sig) = fns.get(canonical) {
            let mut fixed_codes = Vec::new();
            for arg in fixed_args {
                let typed = emit_typed_expr(ctx, env, fns, locals, arg)?;
                fixed_codes.push((typed.code, typed.kind));
            }
            let params_len = sig.params.len();
            if sig.rest.is_none() {
                if fixed_codes.len() > params_len {
                    typed_debug("apply: too many fixed args");
                    return None;
                }
                let needed = params_len - fixed_codes.len();
                if needed > 0 && sig.params[fixed_codes.len()] != tail_elem_kind {
                    typed_debug("apply: tail element kind mismatch");
                    return None;
                }
                for (idx, (_code, kind)) in fixed_codes.iter().enumerate() {
                    if &sig.params[idx] != kind {
                        typed_debug("apply: fixed arg kind mismatch");
                        return None;
                    }
                }
                let mut out = String::new();
                out.push_str("{\n");
                out.push_str(&tail_bind);
                out.push_str(&format!(
                    "        if {tail}.len() != {needed} {{\n            panic!(\"apply expects {params_len} arguments\");\n        }}\n",
                    tail = tail_ref,
                    needed = needed,
                    params_len = params_len
                ));
                let mut call_args = Vec::new();
                for (code, _) in fixed_codes.iter() {
                    call_args.push(code.clone());
                }
                for idx in 0..needed {
                    call_args.push(format!("{tail}[{idx}].clone()", tail = tail_ref, idx = idx));
                }
                out.push_str(&format!(
                    "        {name}({args})\n    }}",
                    name = rust_ident(canonical),
                    args = call_args.join(", ")
                ));
                return Some(TypedExpr {
                    code: out,
                    kind: sig.ret.clone(),
                    is_symbol: false,
                });
            }

            let rest_kind = sig.rest.as_ref()?;
            let TypedKind::Vec(rest_elem) = rest_kind else {
                return None;
            };
            let rest_elem = rest_elem.as_ref().clone();
            if rest_elem != tail_elem_kind {
                typed_debug("apply: rest elem mismatch");
                return None;
            }

            let mut out = String::new();
            out.push_str("{\n");
            out.push_str(&tail_bind);

            let fixed_len = fixed_codes.len();
            let mut call_args = Vec::new();
            let mut extras = Vec::new();
            let needed = if fixed_len >= params_len {
                0
            } else {
                params_len - fixed_len
            };
            out.push_str(&format!(
                "        if {tail}.len() < {needed} {{\n            panic!(\"apply expects at least {params_len} arguments\");\n        }}\n",
                tail = tail_ref,
                needed = needed,
                params_len = params_len
            ));
            for (idx, (code, kind)) in fixed_codes.iter().enumerate() {
                if idx < params_len {
                    if &sig.params[idx] != kind {
                        typed_debug("apply: fixed param mismatch");
                        return None;
                    }
                    call_args.push(code.clone());
                } else {
                    if kind != &rest_elem {
                        typed_debug("apply: fixed rest mismatch");
                        return None;
                    }
                    extras.push(code.clone());
                }
            }
            for idx in 0..needed {
                call_args.push(format!("{tail}[{idx}].clone()", tail = tail_ref, idx = idx));
            }
            let rest_var = ctx.temp("rest");
            out.push_str(&format!(
                "        let mut {rest_var}: Vec<{rest_ty}> = Vec::new();\n",
                rest_var = rest_var,
                rest_ty = typed_kind_to_rust(&rest_elem)?
            ));
            for code in extras {
                out.push_str(&format!(
                    "        {rest_var}.push({code});\n",
                    rest_var = rest_var,
                    code = code
                ));
            }
            out.push_str(&format!(
                "        {rest_var}.extend({tail}.iter().skip({skip}).cloned());\n",
                rest_var = rest_var,
                tail = tail_ref,
                skip = needed
            ));
            call_args.push(rest_var.clone());
            out.push_str(&format!(
                "        {name}({args})\n    }}",
                name = rust_ident(canonical),
                args = call_args.join(", ")
            ));
            return Some(TypedExpr {
                code: out,
                kind: sig.ret.clone(),
                is_symbol: false,
            });
        }
    }
    None
}

#[derive(Clone)]
enum ReduceOp {
    Map(AstExpr),
    Filter(AstExpr),
}

fn collect_reduce_ops(expr: &AstExpr) -> (AstExpr, Vec<ReduceOp>) {
    let mut ops = Vec::new();
    let mut cur = expr.clone();
    loop {
        let AstExpr::Call { callee, args } = &cur else {
            break;
        };
        let AstExpr::Symbol(sym) = callee.as_ref() else {
            break;
        };
        let canonical = crate::aliases::resolve_alias(sym);
        if args.len() == 2 && (canonical == "map" || canonical == "filter") {
            let func_expr = args[0].clone();
            if canonical == "map" {
                ops.push(ReduceOp::Map(func_expr));
            } else {
                ops.push(ReduceOp::Filter(func_expr));
            }
            cur = args[1].clone();
            continue;
        }
        break;
    }
    ops.reverse();
    (cur, ops)
}

fn emit_range_bounds(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<(TypedExpr, TypedExpr, TypedExpr)> {
    let one = TypedExpr {
        code: "1i64".to_string(),
        kind: TypedKind::Int,
        is_symbol: false,
    };
    let zero = TypedExpr {
        code: "0i64".to_string(),
        kind: TypedKind::Int,
        is_symbol: false,
    };
    match args.len() {
        1 => {
            let end = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
            if end.kind != TypedKind::Int {
                return None;
            }
            Some((zero, end, one))
        }
        2 => {
            let start = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
            let end = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
            if start.kind != TypedKind::Int || end.kind != TypedKind::Int {
                return None;
            }
            Some((start, end, one))
        }
        3 => {
            let start = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
            let end = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
            let step = emit_typed_expr(ctx, env, fns, locals, &args[2])?;
            if start.kind != TypedKind::Int
                || end.kind != TypedKind::Int
                || step.kind != TypedKind::Int
            {
                return None;
            }
            Some((start, end, step))
        }
        _ => None,
    }
}

fn emit_typed_reduce_fused(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    let init = match emit_typed_expr(ctx, env, fns, locals, &args[1]) {
        Some(expr) => expr,
        None => {
            typed_debug("reduce_fused: init expr failed");
            return None;
        }
    };
    let (base_expr, ops) = collect_reduce_ops(&args[2]);
    if ops.is_empty() {
        typed_debug("reduce_fused: no ops");
        return None;
    }

    let mut base_loop = String::new();
    let mut base_kind = TypedKind::Int;
    let value_var = ctx.temp("value");

    let mut range_loop = None;
    if let AstExpr::Call { callee, args } = &base_expr {
        if let AstExpr::Symbol(sym) = callee.as_ref() {
            let canonical = crate::aliases::resolve_alias(sym);
            if canonical == "range" {
                if let Some((start, end, step)) = emit_range_bounds(ctx, env, fns, locals, args) {
                    let start_var = ctx.temp("start");
                    let end_var = ctx.temp("end");
                    let step_var = ctx.temp("step");
                    base_kind = TypedKind::Int;
                    base_loop = format!(
                        "        let {start_var} = {start};\n        let {end_var} = {end};\n        let {step_var} = {step};\n        if {step_var} == 0 {{\n            panic!(\"range expects non-zero step\");\n        }}\n        if {step_var} > 0 {{\n            let mut {value_var} = {start_var};\n            while {value_var} < {end_var} {{\n{body}                {value_var} += {step_var};\n            }}\n        }} else {{\n            let mut {value_var} = {start_var};\n            while {value_var} > {end_var} {{\n{body}                {value_var} += {step_var};\n            }}\n        }}\n",
                        start_var = start_var,
                        end_var = end_var,
                        step_var = step_var,
                        start = start.code,
                        end = end.code,
                        step = step.code,
                        value_var = value_var,
                        body = "{{body}}\n",
                    );
                    range_loop = Some(());
                }
            }
        }
    }

    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    if range_loop.is_none() {
        let coll = match emit_typed_expr(ctx, env, fns, locals, &base_expr) {
            Some(expr) => expr,
            None => {
                typed_debug("reduce_fused: base expr failed");
                return None;
            }
        };
        let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
            typed_debug("reduce_fused: base expr is not Vec");
            return None;
        };
        base_kind = *elem_kind;
        let coll_bind = if coll.is_symbol {
            format!(
                "        let {coll_ref} = &{coll};\n",
                coll_ref = coll_ref,
                coll = coll.code
            )
        } else {
            format!(
                "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                coll_var = coll_var,
                coll_ref = coll_ref,
                coll = coll.code
            )
        };
        base_loop = format!(
            "{coll_bind}        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{body}        }}\n",
            coll_bind = coll_bind,
            value_var = value_var,
            coll_ref = coll_ref,
            body = "{{body}}\n",
        );
    }

    let mut cur_kind = base_kind.clone();
    let mut cur_var = value_var.clone();
    let mut loop_body = String::new();
    for op in ops.iter() {
        match op {
            ReduceOp::Map(func) => {
                let inline =
                    emit_typed_unary_inline(ctx, env, fns, locals, func, &cur_kind, &cur_var)?;
                let next_var = ctx.temp("tmp");
                loop_body.push_str(&indent(&inline.prelude, 12));
                loop_body.push_str(&format!(
                    "            let {next_var} = {expr};\n",
                    next_var = next_var,
                    expr = inline.expr
                ));
                cur_var = next_var;
                cur_kind = inline.kind;
            }
            ReduceOp::Filter(func) => {
                let inline =
                    emit_typed_unary_inline(ctx, env, fns, locals, func, &cur_kind, &cur_var)?;
                if inline.kind != TypedKind::Bool {
                    return None;
                }
                loop_body.push_str(&indent(&inline.prelude, 12));
                loop_body.push_str(&format!(
                    "            if !({expr}) {{\n                continue;\n            }}\n",
                    expr = inline.expr
                ));
            }
        }
    }

    let acc_var = ctx.temp("acc");
    if init.kind != cur_kind {
        return None;
    }
    let inline = emit_typed_binary_inline(
        ctx, env, fns, locals, &args[0], &init.kind, &acc_var, &cur_var,
    )?;
    if inline.kind != init.kind {
        return None;
    }
    loop_body.push_str(&indent(&inline.prelude, 12));
    loop_body.push_str(&format!(
        "            {acc_var} = {expr};\n",
        acc_var = acc_var,
        expr = inline.expr
    ));

    let code = format!(
        "{{\n        let mut {acc_var} = {init};\n{loop}\n        {acc_var}\n    }}",
        acc_var = acc_var,
        init = init.code,
        loop = base_loop.replace("{body}", &loop_body),
    );
    Some(TypedExpr {
        code,
        kind: init.kind,
        is_symbol: false,
    })
}

fn emit_typed_identity(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    emit_typed_expr(ctx, env, fns, locals, &args[0])
}

fn emit_typed_rest(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let out_var = ctx.temp("out");
    let value_var = ctx.temp("value");
    let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let code = format!(
        "{{\n{coll_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::new();\n        for {value_var} in {coll_ref}.iter().skip(1) {{\n            let {value_var} = {value_var}.clone();\n            {out_var}.push({value_var});\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        out_var = out_var,
        out_elem_ty = out_elem_ty,
        coll_ref = coll_ref,
        value_var = value_var
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_conj(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let vec_ty = typed_kind_to_rust(&TypedKind::Vec(Box::new(elem_kind.clone())))?;
    let vec_var = ctx.temp("vec");
    let mut out = String::new();
    out.push_str("{\n");
    if coll.is_symbol {
        out.push_str(&format!(
            "    let mut {vec_var}: {vec_ty} = {coll}.clone();\n",
            vec_var = vec_var,
            vec_ty = vec_ty,
            coll = coll.code
        ));
    } else {
        out.push_str(&format!(
            "    let mut {vec_var}: {vec_ty} = {coll};\n",
            vec_var = vec_var,
            vec_ty = vec_ty,
            coll = coll.code
        ));
    }
    for arg in &args[1..] {
        let value = emit_typed_expr(ctx, env, fns, locals, arg)?;
        if value.kind != elem_kind {
            return None;
        }
        let value_expr = emit_value_expr_for_insert(&value);
        out.push_str(&format!(
            "    {vec_var}.push({value});\n",
            vec_var = vec_var,
            value = value_expr
        ));
    }
    out.push_str(&format!("    {vec_var}\n}}", vec_var = vec_var));
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_empty_pred(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    match coll.kind {
        TypedKind::Vec(_) | TypedKind::Map(_, _) | TypedKind::Str => {
            let coll_var = ctx.temp("coll");
            let code = if coll.is_symbol {
                format!("{}.is_empty()", coll.code)
            } else {
                format!(
                    "{{\n    let {coll_var} = {coll};\n    {coll_var}.is_empty()\n}}",
                    coll_var = coll_var,
                    coll = coll.code
                )
            };
            Some(TypedExpr {
                code,
                kind: TypedKind::Bool,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_hash_map(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.is_empty() || args.len() % 2 != 0 {
        return None;
    }
    let mut key_kind: Option<TypedKind> = None;
    let mut value_kind: Option<TypedKind> = None;
    let mut pairs = Vec::new();
    for pair in args.chunks(2) {
        let key_ast = &pair[0];
        let key_typed = emit_typed_expr(ctx, env, fns, locals, key_ast)?;
        let val_typed = emit_typed_expr(ctx, env, fns, locals, &pair[1])?;
        if let Some(existing) = &key_kind {
            if existing != &key_typed.kind {
                return None;
            }
        } else {
            key_kind = Some(key_typed.kind.clone());
        }
        if let Some(existing) = &value_kind {
            if existing != &val_typed.kind {
                return None;
            }
        } else {
            value_kind = Some(val_typed.kind.clone());
        }
        pairs.push((key_ast, key_typed, val_typed));
    }
    let key_kind = key_kind?;
    let value_kind = value_kind?;
    let map_type = TypedKind::Map(Box::new(key_kind.clone()), Box::new(value_kind.clone()));
    let value_rust = typed_kind_to_rust(&value_kind)?;
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&format!(
        "    let mut map: BTreeMap<Key, {value_rust}> = BTreeMap::new();\n",
        value_rust = value_rust
    ));
    for (key_ast, key_typed, val_typed) in pairs {
        let key_expr = emit_key_expr(ctx, key_ast, &key_typed)?;
        let value_expr = emit_value_expr_for_insert(&val_typed);
        out.push_str("    map.insert(");
        out.push_str(&key_expr);
        out.push_str(", ");
        out.push_str(&value_expr);
        out.push_str(");\n");
    }
    out.push_str("    map\n}");
    Some(TypedExpr {
        code: out,
        kind: map_type,
        is_symbol: false,
    })
}

fn emit_typed_interleave(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let left = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let right = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    match (left.kind.clone(), right.kind.clone()) {
        (TypedKind::Vec(left_elem), TypedKind::Vec(right_elem)) => {
            if left_elem != right_elem {
                return None;
            }
            let elem_kind = *left_elem;
            let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
            let left_var = ctx.temp("left");
            let left_ref = ctx.temp("left_ref");
            let right_var = ctx.temp("right");
            let right_ref = ctx.temp("right_ref");
            let len_var = ctx.temp("len");
            let out_var = ctx.temp("out");
            let left_bind = if left.is_symbol {
                format!(
                    "        let {left_ref} = &{left};\n",
                    left_ref = left_ref,
                    left = left.code
                )
            } else {
                format!(
                    "        let {left_var} = {left};\n        let {left_ref} = &{left_var};\n",
                    left_var = left_var,
                    left_ref = left_ref,
                    left = left.code
                )
            };
            let right_bind = if right.is_symbol {
                format!(
                    "        let {right_ref} = &{right};\n",
                    right_ref = right_ref,
                    right = right.code
                )
            } else {
                format!(
                    "        let {right_var} = {right};\n        let {right_ref} = &{right_var};\n",
                    right_var = right_var,
                    right_ref = right_ref,
                    right = right.code
                )
            };
            let code = format!(
                "{{\n{left_bind}{right_bind}        let {len_var} = {left_ref}.len().min({right_ref}.len());\n        let mut {out_var}: Vec<{out_elem_ty}> = Vec::with_capacity({len_var} * 2);\n        for idx in 0..{len_var} {{\n            {out_var}.push({left_ref}[idx].clone());\n            {out_var}.push({right_ref}[idx].clone());\n        }}\n        {out_var}\n    }}",
                left_bind = left_bind,
                right_bind = right_bind,
                len_var = len_var,
                left_ref = left_ref,
                right_ref = right_ref,
                out_var = out_var,
                out_elem_ty = out_elem_ty
            );
            Some(TypedExpr {
                code,
                kind: TypedKind::Vec(Box::new(elem_kind)),
                is_symbol: false,
            })
        }
        (TypedKind::Str, TypedKind::Str) => {
            let left_var = ctx.temp("left");
            let left_ref = ctx.temp("left_ref");
            let right_var = ctx.temp("right");
            let right_ref = ctx.temp("right_ref");
            let left_chars = ctx.temp("left_chars");
            let right_chars = ctx.temp("right_chars");
            let len_var = ctx.temp("len");
            let out_var = ctx.temp("out");
            let left_bind = if left.is_symbol {
                format!(
                    "        let {left_ref} = &{left};\n",
                    left_ref = left_ref,
                    left = left.code
                )
            } else {
                format!(
                    "        let {left_var} = {left};\n        let {left_ref} = &{left_var};\n",
                    left_var = left_var,
                    left_ref = left_ref,
                    left = left.code
                )
            };
            let right_bind = if right.is_symbol {
                format!(
                    "        let {right_ref} = &{right};\n",
                    right_ref = right_ref,
                    right = right.code
                )
            } else {
                format!(
                    "        let {right_var} = {right};\n        let {right_ref} = &{right_var};\n",
                    right_var = right_var,
                    right_ref = right_ref,
                    right = right.code
                )
            };
            let code = format!(
                "{{\n{left_bind}{right_bind}        let {left_chars}: Vec<char> = {left_ref}.chars().collect();\n        let {right_chars}: Vec<char> = {right_ref}.chars().collect();\n        let {len_var} = {left_chars}.len().min({right_chars}.len());\n        let mut {out_var} = String::new();\n        for idx in 0..{len_var} {{\n            {out_var}.push({left_chars}[idx]);\n            {out_var}.push({right_chars}[idx]);\n        }}\n        {out_var}\n    }}",
                left_bind = left_bind,
                right_bind = right_bind,
                left_chars = left_chars,
                right_chars = right_chars,
                len_var = len_var,
                out_var = out_var,
                left_ref = left_ref,
                right_ref = right_ref
            );
            Some(TypedExpr {
                code,
                kind: TypedKind::Str,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_sort(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 && args.len() != 2 {
        return None;
    }
    let coll_arg = if args.len() == 2 { &args[1] } else { &args[0] };
    let coll = emit_typed_expr(ctx, env, fns, locals, coll_arg)?;
    match coll.kind.clone() {
        TypedKind::Str => {
            if args.len() != 1 {
                return None;
            }
            let text_var = ctx.temp("text");
            let text_ref = ctx.temp("text_ref");
            let chars_var = ctx.temp("chars");
            let out_var = ctx.temp("out");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {text_ref} = {text}.as_str();\n",
                    text_ref = text_ref,
                    text = coll.code
                )
            } else {
                format!(
                    "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
                    text_var = text_var,
                    text_ref = text_ref,
                    text = coll.code
                )
            };
            let code = format!(
                "{{\n{coll_bind}        let mut {chars_var}: Vec<char> = {text_ref}.chars().collect();\n        {chars_var}.sort();\n        let {out_var}: String = {chars_var}.into_iter().collect();\n        {out_var}\n    }}",
                coll_bind = coll_bind,
                text_ref = text_ref,
                chars_var = chars_var,
                out_var = out_var
            );
            return Some(TypedExpr {
                code,
                kind: TypedKind::Str,
                is_symbol: false,
            });
        }
        TypedKind::Vec(elem_kind) => {
            let elem_kind = *elem_kind;
            let is_orderable = matches!(
                elem_kind,
                TypedKind::Int
                    | TypedKind::Float
                    | TypedKind::Bool
                    | TypedKind::Str
                    | TypedKind::Keyword
                    | TypedKind::Symbol
            );
            if !is_orderable {
                return None;
            }
            let out_var = ctx.temp("out");
            let coll_init = if coll.is_symbol {
                format!("{}.clone()", coll.code)
            } else {
                coll.code
            };
            let sort_body = if args.len() == 1 {
                if elem_kind == TypedKind::Float {
                    format!(
                        "        {out_var}.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));\n",
                        out_var = out_var
                    )
                } else {
                    format!("        {out_var}.sort();\n", out_var = out_var)
                }
            } else {
                let inline = emit_typed_binary_inline(
                    ctx, env, fns, locals, &args[0], &elem_kind, "left", "right",
                )?;
                let inline_kind = inline.kind.clone();
                let inline_prelude = inline.prelude.clone();
                let inline_expr = inline.expr.clone();
                if !matches!(
                    inline_kind,
                    TypedKind::Bool | TypedKind::Int | TypedKind::Float
                ) {
                    return None;
                }
                let left_expr = if typed_kind_is_copy(&elem_kind) {
                    "*left_ref".to_string()
                } else {
                    "left_ref.clone()".to_string()
                };
                let right_expr = if typed_kind_is_copy(&elem_kind) {
                    "*right_ref".to_string()
                } else {
                    "right_ref.clone()".to_string()
                };
                let cmp_expr = format!(
                    "{{\n    let left = {left_expr};\n    let right = {right_expr};\n{inline_prelude}    {inline_expr}\n}}",
                    left_expr = left_expr,
                    right_expr = right_expr,
                    inline_prelude = indent(&inline_prelude, 4),
                    inline_expr = inline_expr
                );
                let rev_expr = format!(
                    "{{\n    let left = {right_expr};\n    let right = {left_expr};\n{inline_prelude}    {inline_expr}\n}}",
                    left_expr = left_expr,
                    right_expr = right_expr,
                    inline_prelude = indent(&inline_prelude, 4),
                    inline_expr = inline_expr
                );
                let ordering_expr = match inline_kind {
                    TypedKind::Bool => format!(
                        "if {cmp} {{\n                std::cmp::Ordering::Less\n            }} else if {rev} {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                        cmp = cmp_expr,
                        rev = rev_expr
                    ),
                    TypedKind::Int => format!(
                        "if {cmp} < 0 {{\n                std::cmp::Ordering::Less\n            }} else if {cmp} > 0 {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                        cmp = cmp_expr
                    ),
                    TypedKind::Float => format!(
                        "if {cmp} < 0.0 {{\n                std::cmp::Ordering::Less\n            }} else if {cmp} > 0.0 {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                        cmp = cmp_expr
                    ),
                    _ => return None,
                };
                format!(
                    "        {out_var}.sort_by(|left_ref, right_ref| {{\n            {ordering}\n        }});\n",
                    out_var = out_var,
                    ordering = ordering_expr
                )
            };
            let out_elem_ty = typed_kind_to_rust(&elem_kind)?;
            let code = format!(
                "{{\n        let mut {out_var}: Vec<{out_elem_ty}> = {coll_init};\n{sort_body}        {out_var}\n    }}",
                out_var = out_var,
                out_elem_ty = out_elem_ty,
                coll_init = coll_init,
                sort_body = sort_body
            );
            return Some(TypedExpr {
                code,
                kind: TypedKind::Vec(Box::new(elem_kind)),
                is_symbol: false,
            });
        }
        _ => None,
    }
}

fn emit_typed_sort_by(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 && args.len() != 3 {
        return None;
    }
    let coll_arg = if args.len() == 3 { &args[2] } else { &args[1] };
    let coll = emit_typed_expr(ctx, env, fns, locals, coll_arg)?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let value_var = ctx.temp("value");
    let key_inline =
        emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    let key_inline_kind = key_inline.kind.clone();
    let key_inline_prelude = key_inline.prelude.clone();
    let key_inline_expr = key_inline.expr.clone();
    let key_kind = match &key_inline_kind {
        TypedKind::Optional(_) => return None,
        other => other.clone(),
    };
    let key_is_orderable = matches!(
        key_kind,
        TypedKind::Int
            | TypedKind::Float
            | TypedKind::Bool
            | TypedKind::Str
            | TypedKind::Keyword
            | TypedKind::Symbol
    );
    if !key_is_orderable {
        return None;
    }
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let keyed_var = ctx.temp("keyed");
    let key_var = ctx.temp("key");
    let out_var = ctx.temp("out");
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let key_ty = typed_kind_to_rust(&key_kind)?;
    let value_ty = typed_kind_to_rust(&elem_kind)?;
    let cmp_block = if args.len() == 3 {
        let inline =
            emit_typed_binary_inline(ctx, env, fns, locals, &args[1], &key_kind, "left", "right")?;
        let inline_kind = inline.kind.clone();
        let inline_prelude = inline.prelude.clone();
        let inline_expr = inline.expr.clone();
        if !matches!(
            inline_kind,
            TypedKind::Bool | TypedKind::Int | TypedKind::Float
        ) {
            return None;
        }
        let left_expr = if typed_kind_is_copy(&key_kind) {
            "*left_key".to_string()
        } else {
            "left_key.clone()".to_string()
        };
        let right_expr = if typed_kind_is_copy(&key_kind) {
            "*right_key".to_string()
        } else {
            "right_key.clone()".to_string()
        };
        let cmp_expr = format!(
            "{{\n    let left_key = &left_ref.0;\n    let right_key = &right_ref.0;\n    let left = {left_expr};\n    let right = {right_expr};\n{inline_prelude}    {inline_expr}\n}}",
            left_expr = left_expr,
            right_expr = right_expr,
            inline_prelude = indent(&inline_prelude, 4),
            inline_expr = inline_expr
        );
        let rev_expr = format!(
            "{{\n    let left_key = &right_ref.0;\n    let right_key = &left_ref.0;\n    let left = {left_expr};\n    let right = {right_expr};\n{inline_prelude}    {inline_expr}\n}}",
            left_expr = left_expr,
            right_expr = right_expr,
            inline_prelude = indent(&inline_prelude, 4),
            inline_expr = inline_expr
        );
        match inline_kind {
            TypedKind::Bool => format!(
                "if {cmp} {{\n                std::cmp::Ordering::Less\n            }} else if {rev} {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                cmp = cmp_expr,
                rev = rev_expr
            ),
            TypedKind::Int => format!(
                "if {cmp} < 0 {{\n                std::cmp::Ordering::Less\n            }} else if {cmp} > 0 {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                cmp = cmp_expr
            ),
            TypedKind::Float => format!(
                "if {cmp} < 0.0 {{\n                std::cmp::Ordering::Less\n            }} else if {cmp} > 0.0 {{\n                std::cmp::Ordering::Greater\n            }} else {{\n                std::cmp::Ordering::Equal\n            }}",
                cmp = cmp_expr
            ),
            _ => return None,
        }
    } else if key_kind == TypedKind::Float {
        "left_ref.0.partial_cmp(&right_ref.0).unwrap_or(std::cmp::Ordering::Equal)".to_string()
    } else {
        "left_ref.0.cmp(&right_ref.0)".to_string()
    };
    let code = format!(
        "{{\n{coll_bind}        let mut {keyed_var}: Vec<({key_ty}, {value_ty})> = Vec::with_capacity({coll_ref}.len());\n        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            let {key_var} = {inline_expr};\n            {keyed_var}.push(({key_var}, {value_var}));\n        }}\n        {keyed_var}.sort_by(|left_ref, right_ref| {{\n            {cmp_block}\n        }});\n        let mut {out_var}: Vec<{value_ty}> = Vec::with_capacity({keyed_var}.len());\n        for (_, value) in {keyed_var}.into_iter() {{\n            {out_var}.push(value);\n        }}\n        {out_var}\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        keyed_var = keyed_var,
        key_ty = key_ty,
        value_ty = value_ty,
        value_var = value_var,
        inline_prelude = indent(&key_inline_prelude, 12),
        inline_expr = key_inline_expr,
        key_var = key_var,
        cmp_block = cmp_block,
        out_var = out_var
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Vec(Box::new(elem_kind)),
        is_symbol: false,
    })
}

fn emit_typed_split(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 && args.len() != 3 {
        return None;
    }
    let text = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    if text.kind != TypedKind::Str {
        return None;
    }
    let limit_expr = if args.len() == 3 {
        let limit = emit_typed_expr(ctx, env, fns, locals, &args[2])?;
        if limit.kind != TypedKind::Int {
            return None;
        }
        Some(limit.code)
    } else {
        None
    };
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let out_var = ctx.temp("out");
    let limit_var = ctx.temp("limit");
    let coll_bind = if text.is_symbol {
        format!(
            "        let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        )
    } else {
        format!(
            "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        )
    };
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&coll_bind);
    let has_limit = limit_expr.is_some();
    if let Some(limit_code) = &limit_expr {
        out.push_str(&format!(
            "        let {limit_var} = {limit_code};\n",
            limit_var = limit_var,
            limit_code = limit_code
        ));
    }
    out.push_str(&format!(
        "        let mut {out_var}: Vec<String> = Vec::new();\n",
        out_var = out_var
    ));
    match &args[1] {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let sep_var = ctx.temp("re");
            let regex_expr = emit_static_regex_expr(ctx, pattern, "split expects regex");
            out.push_str(&format!(
                "        let {sep_var} = {regex_expr};\n",
                sep_var = sep_var,
                regex_expr = regex_expr
            ));
            if has_limit {
                out.push_str(&format!(
                    "        if {limit_var} > 0 {{\n            for part in {sep_var}.splitn({text_ref}, {limit_var} as usize) {{\n                {out_var}.push(part.to_string());\n            }}\n        }} else {{\n            for part in {sep_var}.split({text_ref}) {{\n                {out_var}.push(part.to_string());\n            }}\n        }}\n",
                    limit_var = limit_var,
                    sep_var = sep_var,
                    text_ref = text_ref,
                    out_var = out_var
                ));
            } else {
                out.push_str(&format!(
                    "        for part in {sep_var}.split({text_ref}) {{\n            {out_var}.push(part.to_string());\n        }}\n",
                    sep_var = sep_var,
                    text_ref = text_ref,
                    out_var = out_var
                ));
            }
        }
        _ => {
            let sep = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
            if sep.kind != TypedKind::Str {
                return None;
            }
            let sep_var = ctx.temp("sep");
            let sep_ref = ctx.temp("sep_ref");
            if sep.is_symbol {
                out.push_str(&format!(
                    "        let {sep_ref} = {sep}.as_str();\n",
                    sep_ref = sep_ref,
                    sep = sep.code
                ));
            } else {
                out.push_str(&format!(
                    "        let {sep_var} = {sep};\n        let {sep_ref} = {sep_var}.as_str();\n",
                    sep_var = sep_var,
                    sep_ref = sep_ref,
                    sep = sep.code
                ));
            }
            out.push_str(&format!(
                "        if {sep_ref}.is_empty() {{\n            for ch in {text_ref}.chars() {{\n                {out_var}.push(ch.to_string());\n            }}\n        }} else {{\n",
                sep_ref = sep_ref,
                text_ref = text_ref,
                out_var = out_var
            ));
            if has_limit {
                out.push_str(&format!(
                    "            if {limit_var} > 0 {{\n                for part in {text_ref}.splitn({limit_var} as usize, {sep_ref}) {{\n                    {out_var}.push(part.to_string());\n                }}\n            }} else {{\n                for part in {text_ref}.split({sep_ref}) {{\n                    {out_var}.push(part.to_string());\n                }}\n            }}\n",
                    limit_var = limit_var,
                    text_ref = text_ref,
                    sep_ref = sep_ref,
                    out_var = out_var
                ));
            } else {
                out.push_str(&format!(
                    "            for part in {text_ref}.split({sep_ref}) {{\n                {out_var}.push(part.to_string());\n            }}\n",
                    text_ref = text_ref,
                    sep_ref = sep_ref,
                    out_var = out_var
                ));
            }
            out.push_str("        }\n");
        }
    }
    out.push_str(&format!("        {out_var}\n    }}", out_var = out_var));
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Vec(Box::new(TypedKind::Str)),
        is_symbol: false,
    })
}

fn emit_typed_re_find(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let pattern_ast = &args[0];
    let text = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if text.kind != TypedKind::Str {
        typed_debug("re-find: text is not Str");
        return None;
    }
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let coll_bind = if text.is_symbol {
        format!(
            "        let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        )
    } else {
        format!(
            "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        )
    };
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&coll_bind);
    let regex_var = ctx.temp("re");
    match pattern_ast {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let regex_expr = emit_static_regex_expr(ctx, pattern, "re-find expects regex");
            out.push_str(&format!(
                "        let {regex_var} = {regex_expr};\n",
                regex_var = regex_var,
                regex_expr = regex_expr
            ));
        }
        _ => {
            let pattern = emit_typed_expr(ctx, env, fns, locals, pattern_ast)?;
            if pattern.kind != TypedKind::Str {
                typed_debug("re-find: pattern is not Str");
                return None;
            }
            let pat_var = ctx.temp("pat");
            let pat_ref = ctx.temp("pat_ref");
            if pattern.is_symbol {
                out.push_str(&format!(
                    "        let {pat_ref} = {pattern}.as_str();\n",
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            } else {
                out.push_str(&format!(
                    "        let {pat_var} = {pattern};\n        let {pat_ref} = {pat_var}.as_str();\n",
                    pat_var = pat_var,
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            }
            out.push_str(&format!(
                "        let {regex_var} = Regex::new({pat_ref}).expect(\"re-find expects regex\");\n",
                regex_var = regex_var,
                pat_ref = pat_ref
            ));
        }
    }
    let match_var = ctx.temp("match");
    let items_var = ctx.temp("items");
    out.push_str(&format!(
        "        let {match_var} = {regex_var}.captures({text_ref}).map(|caps| {{\n            if caps.len() == 1 {{\n                Union2::A(caps.get(0).map(|m| m.as_str().to_string()).unwrap_or_default())\n            }} else {{\n                let mut {items_var}: Vec<Option<String>> = Vec::with_capacity(caps.len());\n                for idx in 0..caps.len() {{\n                    {items_var}.push(caps.get(idx).map(|m| m.as_str().to_string()));\n                }}\n                Union2::B({items_var})\n            }}\n        }});\n        {match_var}\n    }}",
        match_var = match_var,
        regex_var = regex_var,
        text_ref = text_ref,
        items_var = items_var
    ));
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Optional(Box::new(TypedKind::Union2(
            Box::new(TypedKind::Str),
            Box::new(TypedKind::Vec(Box::new(TypedKind::Optional(Box::new(
                TypedKind::Str,
            ))))),
        ))),
        is_symbol: false,
    })
}

fn emit_typed_re_matches(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let pattern_ast = &args[0];
    let text = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if text.kind != TypedKind::Str {
        typed_debug("re-matches: text is not Str");
        return None;
    }
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let coll_bind = if text.is_symbol {
        format!(
            "        let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        )
    } else {
        format!(
            "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        )
    };
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&coll_bind);
    let regex_var = ctx.temp("re");
    match pattern_ast {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let regex_expr = emit_static_regex_expr(ctx, pattern, "re-matches expects regex");
            out.push_str(&format!(
                "        let {regex_var} = {regex_expr};\n",
                regex_var = regex_var,
                regex_expr = regex_expr
            ));
        }
        _ => {
            let pattern = emit_typed_expr(ctx, env, fns, locals, pattern_ast)?;
            if pattern.kind != TypedKind::Str {
                typed_debug("re-matches: pattern is not Str");
                return None;
            }
            let pat_var = ctx.temp("pat");
            let pat_ref = ctx.temp("pat_ref");
            if pattern.is_symbol {
                out.push_str(&format!(
                    "        let {pat_ref} = {pattern}.as_str();\n",
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            } else {
                out.push_str(&format!(
                    "        let {pat_var} = {pattern};\n        let {pat_ref} = {pat_var}.as_str();\n",
                    pat_var = pat_var,
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            }
            out.push_str(&format!(
                "        let {regex_var} = Regex::new({pat_ref}).expect(\"re-matches expects regex\");\n",
                regex_var = regex_var,
                pat_ref = pat_ref
            ));
        }
    }
    let match_var = ctx.temp("match");
    let items_var = ctx.temp("items");
    out.push_str(&format!(
        "        let {match_var} = {regex_var}.captures({text_ref}).and_then(|caps| {{\n            if let Some(full) = caps.get(0) {{\n                if full.start() == 0 && full.end() == {text_ref}.len() {{\n                    if caps.len() == 1 {{\n                        return Some(Union2::A(full.as_str().to_string()));\n                    }}\n                    let mut {items_var}: Vec<Option<String>> = Vec::with_capacity(caps.len());\n                    for idx in 0..caps.len() {{\n                        {items_var}.push(caps.get(idx).map(|m| m.as_str().to_string()));\n                    }}\n                    return Some(Union2::B({items_var}));\n                }}\n            }}\n            None\n        }});\n        {match_var}\n    }}",
        match_var = match_var,
        regex_var = regex_var,
        text_ref = text_ref,
        items_var = items_var
    ));
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Optional(Box::new(TypedKind::Union2(
            Box::new(TypedKind::Str),
            Box::new(TypedKind::Vec(Box::new(TypedKind::Optional(Box::new(
                TypedKind::Str,
            ))))),
        ))),
        is_symbol: false,
    })
}

fn emit_typed_re_seq(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let pattern_ast = &args[0];
    let text = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if text.kind != TypedKind::Str {
        typed_debug("re-seq: text is not Str");
        return None;
    }
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let coll_bind = if text.is_symbol {
        format!(
            "        let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        )
    } else {
        format!(
            "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        )
    };
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&coll_bind);
    let regex_var = ctx.temp("re");
    match pattern_ast {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let regex_expr = emit_static_regex_expr(ctx, pattern, "re-seq expects regex");
            out.push_str(&format!(
                "        let {regex_var} = {regex_expr};\n",
                regex_var = regex_var,
                regex_expr = regex_expr
            ));
        }
        _ => {
            let pattern = emit_typed_expr(ctx, env, fns, locals, pattern_ast)?;
            if pattern.kind != TypedKind::Str {
                typed_debug("re-seq: pattern is not Str");
                return None;
            }
            let pat_var = ctx.temp("pat");
            let pat_ref = ctx.temp("pat_ref");
            if pattern.is_symbol {
                out.push_str(&format!(
                    "        let {pat_ref} = {pattern}.as_str();\n",
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            } else {
                out.push_str(&format!(
                    "        let {pat_var} = {pattern};\n        let {pat_ref} = {pat_var}.as_str();\n",
                    pat_var = pat_var,
                    pat_ref = pat_ref,
                    pattern = pattern.code
                ));
            }
            out.push_str(&format!(
                "        let {regex_var} = Regex::new({pat_ref}).expect(\"re-seq expects regex\");\n",
                regex_var = regex_var,
                pat_ref = pat_ref
            ));
        }
    }
    let out_var = ctx.temp("out");
    let items_var = ctx.temp("items");
    out.push_str(&format!(
        "        let mut {out_var}: Vec<Union2<String, Vec<Option<String>>>> = Vec::new();\n        for caps in {regex_var}.captures_iter({text_ref}) {{\n            if caps.len() == 1 {{\n                {out_var}.push(Union2::A(caps.get(0).map(|m| m.as_str().to_string()).unwrap_or_default()));\n            }} else {{\n                let mut {items_var}: Vec<Option<String>> = Vec::with_capacity(caps.len());\n                for idx in 0..caps.len() {{\n                    {items_var}.push(caps.get(idx).map(|m| m.as_str().to_string()));\n                }}\n                {out_var}.push(Union2::B({items_var}));\n            }}\n        }}\n        {out_var}\n    }}",
        out_var = out_var,
        regex_var = regex_var,
        text_ref = text_ref,
        items_var = items_var
    ));
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Vec(Box::new(TypedKind::Union2(
            Box::new(TypedKind::Str),
            Box::new(TypedKind::Vec(Box::new(TypedKind::Optional(Box::new(
                TypedKind::Str,
            ))))),
        ))),
        is_symbol: false,
    })
}

fn emit_typed_replace(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
    first_only: bool,
) -> Option<TypedExpr> {
    if first_only {
        if args.len() != 3 {
            return None;
        }
    } else if args.len() != 2 && args.len() != 3 {
        return None;
    }

    if args.len() == 2 {
        let smap = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
        let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
        let TypedKind::Map(_, value_kind) = smap.kind.clone() else {
            return None;
        };
        match coll.kind.clone() {
            TypedKind::Vec(elem_kind) => {
                let elem_kind = *elem_kind;
                let out_elem_kind = if elem_kind == *value_kind {
                    elem_kind.clone()
                } else {
                    TypedKind::Union2(Box::new(elem_kind.clone()), Box::new((*value_kind).clone()))
                };
                let out_elem_ty = typed_kind_to_rust(&out_elem_kind)?;
                let coll_var = ctx.temp("coll");
                let coll_ref = ctx.temp("coll_ref");
                let map_var = ctx.temp("map");
                let map_ref = ctx.temp("map_ref");
                let out_var = ctx.temp("out");
                let item_var = ctx.temp("item");
                let key_var = ctx.temp("key");
                let repl_var = ctx.temp("repl");
                let coll_bind = if coll.is_symbol {
                    format!(
                        "        let {coll_ref} = &{coll};\n",
                        coll_ref = coll_ref,
                        coll = coll.code
                    )
                } else {
                    format!(
                        "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                        coll_var = coll_var,
                        coll_ref = coll_ref,
                        coll = coll.code
                    )
                };
                let map_bind = if smap.is_symbol {
                    format!(
                        "        let {map_ref} = &{smap};\n",
                        map_ref = map_ref,
                        smap = smap.code
                    )
                } else {
                    format!(
                        "        let {map_var} = {smap};\n        let {map_ref} = &{map_var};\n",
                        map_var = map_var,
                        map_ref = map_ref,
                        smap = smap.code
                    )
                };
                let key_expr = emit_key_from_typed(&elem_kind, &item_var, true)?;
                let wrap_item = if elem_kind == *value_kind {
                    item_var.clone()
                } else {
                    format!("Union2::A({})", item_var)
                };
                let repl_expr = if typed_kind_is_copy(&value_kind) {
                    format!("*{repl_var}", repl_var = repl_var)
                } else {
                    format!("{repl_var}.clone()", repl_var = repl_var)
                };
                let wrap_repl = if elem_kind == *value_kind {
                    repl_expr.clone()
                } else {
                    format!("Union2::B({})", repl_expr)
                };
                let code = format!(
                    "{{\n{coll_bind}{map_bind}        let mut {out_var}: Vec<{out_elem_ty}> = Vec::with_capacity({coll_ref}.len());\n        for {item_var} in {coll_ref}.iter() {{\n            let {item_var} = {item_var}.clone();\n            let {key_var} = {key_expr};\n            if let Some({repl_var}) = {map_ref}.get(&{key_var}) {{\n                {out_var}.push({wrap_repl});\n            }} else {{\n                {out_var}.push({wrap_item});\n            }}\n        }}\n        {out_var}\n    }}",
                    coll_bind = coll_bind,
                    map_bind = map_bind,
                    out_var = out_var,
                    out_elem_ty = out_elem_ty,
                    coll_ref = coll_ref,
                    item_var = item_var,
                    key_var = key_var,
                    key_expr = key_expr,
                    repl_var = repl_var,
                    map_ref = map_ref,
                    wrap_repl = wrap_repl,
                    wrap_item = wrap_item
                );
                return Some(TypedExpr {
                    code,
                    kind: TypedKind::Vec(Box::new(out_elem_kind)),
                    is_symbol: false,
                });
            }
            TypedKind::Map(key_kind, value_kind_coll) => {
                let out_key_kind = if *key_kind == *value_kind {
                    *key_kind.clone()
                } else {
                    TypedKind::Union2(Box::new(*key_kind.clone()), Box::new((*value_kind).clone()))
                };
                let out_kind = TypedKind::Map(Box::new(out_key_kind), value_kind_coll.clone());
                let out_val_ty = typed_kind_to_rust(&value_kind_coll)?;
                let map_var = ctx.temp("map");
                let map_ref = ctx.temp("map_ref");
                let coll_var = ctx.temp("coll");
                let coll_ref = ctx.temp("coll_ref");
                let out_var = ctx.temp("out");
                let key_var = ctx.temp("key");
                let val_var = ctx.temp("value");
                let repl_var = ctx.temp("repl");
                let repl_val = ctx.temp("repl_val");
                let map_bind = if smap.is_symbol {
                    format!(
                        "        let {map_ref} = &{smap};\n",
                        map_ref = map_ref,
                        smap = smap.code
                    )
                } else {
                    format!(
                        "        let {map_var} = {smap};\n        let {map_ref} = &{map_var};\n",
                        map_var = map_var,
                        map_ref = map_ref,
                        smap = smap.code
                    )
                };
                let coll_bind = if coll.is_symbol {
                    format!(
                        "        let {coll_ref} = &{coll};\n",
                        coll_ref = coll_ref,
                        coll = coll.code
                    )
                } else {
                    format!(
                        "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                        coll_var = coll_var,
                        coll_ref = coll_ref,
                        coll = coll.code
                    )
                };
                let repl_assign = if typed_kind_is_copy(&value_kind) {
                    format!(
                        "let {repl_val} = *{repl_var};\n",
                        repl_val = repl_val,
                        repl_var = repl_var
                    )
                } else {
                    format!(
                        "let {repl_val} = {repl_var}.clone();\n",
                        repl_val = repl_val,
                        repl_var = repl_var
                    )
                };
                let key_expr = emit_key_from_typed(&value_kind, &repl_val, true)?;
                let code = format!(
                    "{{\n{map_bind}{coll_bind}        let mut {out_var}: BTreeMap<Key, {out_val_ty}> = BTreeMap::new();\n        for ({key_var}, {val_var}) in {coll_ref}.iter() {{\n            if let Some({repl_var}) = {map_ref}.get({key_var}) {{\n                {repl_assign}                let new_key = {key_expr};\n                {out_var}.insert(new_key, {val_var}.clone());\n            }} else {{\n                {out_var}.insert({key_var}.clone(), {val_var}.clone());\n            }}\n        }}\n        {out_var}\n    }}",
                    map_bind = map_bind,
                    coll_bind = coll_bind,
                    out_var = out_var,
                    out_val_ty = out_val_ty,
                    key_var = key_var,
                    val_var = val_var,
                    coll_ref = coll_ref,
                    repl_var = repl_var,
                    map_ref = map_ref,
                    repl_assign = indent(&repl_assign, 12),
                    key_expr = key_expr
                );
                return Some(TypedExpr {
                    code,
                    kind: out_kind,
                    is_symbol: false,
                });
            }
            _ => return None,
        }
    }

    let text = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    if text.kind != TypedKind::Str {
        return None;
    }
    let from_ast = &args[1];
    let to = emit_typed_expr(ctx, env, fns, locals, &args[2])?;
    if to.kind != TypedKind::Str {
        return None;
    }
    let text_var = ctx.temp("text");
    let text_ref = ctx.temp("text_ref");
    let to_var = ctx.temp("to");
    let to_ref = ctx.temp("to_ref");
    let text_bind = if text.is_symbol {
        format!(
            "        let {text_ref} = {text}.as_str();\n",
            text_ref = text_ref,
            text = text.code
        )
    } else {
        format!(
            "        let {text_var} = {text};\n        let {text_ref} = {text_var}.as_str();\n",
            text_var = text_var,
            text_ref = text_ref,
            text = text.code
        )
    };
    let to_bind = if to.is_symbol {
        format!(
            "        let {to_ref} = {to}.as_str();\n",
            to_ref = to_ref,
            to = to.code
        )
    } else {
        format!(
            "        let {to_var} = {to};\n        let {to_ref} = {to_var}.as_str();\n",
            to_var = to_var,
            to_ref = to_ref,
            to = to.code
        )
    };
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&text_bind);
    out.push_str(&to_bind);
    match from_ast {
        AstExpr::Literal(Literal::Regex(pattern)) => {
            let regex_var = ctx.temp("re");
            let regex_expr = emit_static_regex_expr(ctx, pattern, "replace expects regex");
            out.push_str(&format!(
                "        let {regex_var} = {regex_expr};\n",
                regex_var = regex_var,
                regex_expr = regex_expr
            ));
            let call = if first_only { "replace" } else { "replace_all" };
            out.push_str(&format!(
                "        {regex_var}.{call}({text_ref}, {to_ref}).to_string()\n    }}",
                regex_var = regex_var,
                call = call,
                text_ref = text_ref,
                to_ref = to_ref
            ));
        }
        _ => {
            let from = emit_typed_expr(ctx, env, fns, locals, from_ast)?;
            if from.kind != TypedKind::Str {
                return None;
            }
            let from_var = ctx.temp("from");
            let from_ref = ctx.temp("from_ref");
            if from.is_symbol {
                out.push_str(&format!(
                    "        let {from_ref} = {from}.as_str();\n",
                    from_ref = from_ref,
                    from = from.code
                ));
            } else {
                out.push_str(&format!(
                    "        let {from_var} = {from};\n        let {from_ref} = {from_var}.as_str();\n",
                    from_var = from_var,
                    from_ref = from_ref,
                    from = from.code
                ));
            }
            if first_only {
                out.push_str(&format!(
                    "        {text_ref}.replacen({from_ref}, {to_ref}, 1)\n    }}",
                    text_ref = text_ref,
                    from_ref = from_ref,
                    to_ref = to_ref
                ));
            } else {
                out.push_str(&format!(
                    "        {text_ref}.replace({from_ref}, {to_ref})\n    }}",
                    text_ref = text_ref,
                    from_ref = from_ref,
                    to_ref = to_ref
                ));
            }
        }
    }
    Some(TypedExpr {
        code: out,
        kind: TypedKind::Str,
        is_symbol: false,
    })
}

fn emit_typed_join(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 && args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    if *elem_kind != TypedKind::Str {
        return None;
    }
    let sep_expr = if args.len() == 2 {
        let sep = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
        if sep.kind != TypedKind::Str {
            return None;
        }
        Some(sep)
    } else {
        None
    };
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let sep_var = ctx.temp("sep");
    let sep_ref = ctx.temp("sep_ref");
    let sep_bind = if let Some(sep) = sep_expr {
        if sep.is_symbol {
            format!(
                "        let {sep_ref} = {sep}.as_str();\n",
                sep_ref = sep_ref,
                sep = sep.code
            )
        } else {
            format!(
                "        let {sep_var} = {sep};\n        let {sep_ref} = {sep_var}.as_str();\n",
                sep_var = sep_var,
                sep_ref = sep_ref,
                sep = sep.code
            )
        }
    } else {
        format!("        let {sep_ref} = \"\";\n", sep_ref = sep_ref)
    };
    let code = format!(
        "{{\n{coll_bind}{sep_bind}        {coll_ref}.join({sep_ref})\n    }}",
        coll_bind = coll_bind,
        sep_bind = sep_bind,
        coll_ref = coll_ref,
        sep_ref = sep_ref
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Str,
        is_symbol: false,
    })
}

fn emit_typed_count(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let is_vec = matches!(coll.kind, TypedKind::Vec(_));
    let is_map = matches!(coll.kind, TypedKind::Map(_, _));
    let is_str = coll.kind == TypedKind::Str;
    if !is_vec && !is_map && !is_str {
        return None;
    }
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let count_expr = if is_str {
        format!("{coll_ref}.chars().count() as i64", coll_ref = coll_ref)
    } else {
        format!("{coll_ref}.len() as i64", coll_ref = coll_ref)
    };
    let code = format!(
        "{{\n{coll_bind}        {count_expr}\n    }}",
        coll_bind = coll_bind,
        count_expr = count_expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Int,
        is_symbol: false,
    })
}

fn emit_typed_get(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 && args.len() != 3 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let key = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let default_expr = if args.len() == 3 {
        Some(emit_typed_expr(ctx, env, fns, locals, &args[2])?)
    } else {
        None
    };
    match coll.kind {
        TypedKind::Vec(elem_kind) => {
            if key.kind != TypedKind::Int {
                return None;
            }
            if let Some(default) = &default_expr {
                if default.kind != *elem_kind {
                    return None;
                }
            }
            let coll_var = ctx.temp("coll");
            let coll_ref = ctx.temp("coll_ref");
            let key_var = ctx.temp("idx");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let key_bind = if key.is_symbol {
                String::new()
            } else {
                format!(
                    "        let {key_var} = {key};\n",
                    key_var = key_var,
                    key = key.code
                )
            };
            let key_expr = if key.is_symbol { key.code } else { key_var };
            let has_default = default_expr.is_some();
            let expr = if let Some(ref default) = default_expr {
                format!(
                    "{{\n{coll_bind}{key_bind}        {coll_ref}.get({key_expr} as usize).cloned().unwrap_or({default})\n    }}",
                    coll_bind = coll_bind,
                    key_bind = key_bind,
                    coll_ref = coll_ref,
                    key_expr = key_expr,
                    default = default.code
                )
            } else {
                format!(
                    "{{\n{coll_bind}{key_bind}        {coll_ref}.get({key_expr} as usize).cloned()\n    }}",
                    coll_bind = coll_bind,
                    key_bind = key_bind,
                    coll_ref = coll_ref,
                    key_expr = key_expr
                )
            };
            let result_kind = if has_default {
                *elem_kind
            } else {
                TypedKind::Optional(elem_kind)
            };
            Some(TypedExpr {
                code: expr,
                kind: result_kind,
                is_symbol: false,
            })
        }
        TypedKind::Map(key_kind, value_kind) => {
            if key.kind != *key_kind {
                return None;
            }
            if let Some(default) = &default_expr {
                if default.kind != *value_kind {
                    return None;
                }
            }
            let coll_var = ctx.temp("map");
            let coll_ref = ctx.temp("map_ref");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let key_expr = emit_key_expr(ctx, &args[1], &key)?;
            let has_default = default_expr.is_some();
            let expr = if let Some(ref default) = default_expr {
                format!(
                    "{{\n{coll_bind}        {coll_ref}.get(&{key}).cloned().unwrap_or({default})\n    }}",
                    coll_bind = coll_bind,
                    coll_ref = coll_ref,
                    key = key_expr,
                    default = default.code
                )
            } else {
                format!(
                    "{{\n{coll_bind}        {coll_ref}.get(&{key}).cloned()\n    }}",
                    coll_bind = coll_bind,
                    coll_ref = coll_ref,
                    key = key_expr
                )
            };
            let result_kind = if has_default {
                *value_kind
            } else {
                TypedKind::Optional(value_kind)
            };
            Some(TypedExpr {
                code: expr,
                kind: result_kind,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_assoc(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 3 || (args.len() - 1) % 2 != 0 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    match coll.kind {
        TypedKind::Map(key_kind, value_kind) => {
            let map_ty = typed_kind_to_rust(&TypedKind::Map(key_kind.clone(), value_kind.clone()))?;
            let map_var = ctx.temp("map");
            let mut out = String::new();
            out.push_str("{\n");
            if coll.is_symbol {
                out.push_str(&format!(
                    "    let mut {map_var}: {map_ty} = {coll}.clone();\n",
                    map_var = map_var,
                    map_ty = map_ty,
                    coll = coll.code
                ));
            } else {
                out.push_str(&format!(
                    "    let mut {map_var}: {map_ty} = {coll};\n",
                    map_var = map_var,
                    map_ty = map_ty,
                    coll = coll.code
                ));
            }
            let mut idx = 1usize;
            while idx + 1 < args.len() {
                let key = emit_typed_expr(ctx, env, fns, locals, &args[idx])?;
                if key.kind != *key_kind {
                    return None;
                }
                let val = emit_typed_expr(ctx, env, fns, locals, &args[idx + 1])?;
                if val.kind != *value_kind {
                    return None;
                }
                let key_expr = emit_key_expr(ctx, &args[idx], &key)?;
                let value_expr = emit_value_expr_for_insert(&val);
                out.push_str("    ");
                out.push_str(&map_var);
                out.push_str(".insert(");
                out.push_str(&key_expr);
                out.push_str(", ");
                out.push_str(&value_expr);
                out.push_str(");\n");
                idx += 2;
            }
            out.push_str("    ");
            out.push_str(&map_var);
            out.push_str("\n}");
            Some(TypedExpr {
                code: out,
                kind: TypedKind::Map(key_kind, value_kind),
                is_symbol: false,
            })
        }
        TypedKind::Vec(elem_kind) => {
            let elem_kind = *elem_kind;
            let vec_ty = typed_kind_to_rust(&TypedKind::Vec(Box::new(elem_kind.clone())))?;
            let vec_var = ctx.temp("vec");
            let mut out = String::new();
            out.push_str("{\n");
            if coll.is_symbol {
                out.push_str(&format!(
                    "    let mut {vec_var}: {vec_ty} = {coll}.clone();\n",
                    vec_var = vec_var,
                    vec_ty = vec_ty,
                    coll = coll.code
                ));
            } else {
                out.push_str(&format!(
                    "    let mut {vec_var}: {vec_ty} = {coll};\n",
                    vec_var = vec_var,
                    vec_ty = vec_ty,
                    coll = coll.code
                ));
            }
            let mut idx = 1usize;
            while idx + 1 < args.len() {
                let key = emit_typed_expr(ctx, env, fns, locals, &args[idx])?;
                if key.kind != TypedKind::Int {
                    return None;
                }
                let val = emit_typed_expr(ctx, env, fns, locals, &args[idx + 1])?;
                if val.kind != elem_kind {
                    return None;
                }
                let val_expr = emit_value_expr_for_insert(&val);
                let key_var = ctx.temp("idx");
                out.push_str(&format!(
                    "    let {key_var} = {key};\n",
                    key_var = key_var,
                    key = key.code
                ));
                out.push_str(&format!(
                    "    if {key_var} < 0 {{ panic!(\"assoc expects non-negative index\"); }}\n",
                    key_var = key_var
                ));
                out.push_str(&format!(
                    "    let {key_var} = {key_var} as usize;\n",
                    key_var = key_var
                ));
                out.push_str(&format!(
                    "    if {key_var} == {vec_var}.len() {{ {vec_var}.push({val}); }} else if {key_var} < {vec_var}.len() {{ {vec_var}[{key_var}] = {val}; }} else {{ panic!(\"assoc index out of bounds\"); }}\n",
                    key_var = key_var,
                    vec_var = vec_var,
                    val = val_expr
                ));
                idx += 2;
            }
            out.push_str("    ");
            out.push_str(&vec_var);
            out.push_str("\n}");
            Some(TypedExpr {
                code: out,
                kind: TypedKind::Vec(Box::new(elem_kind)),
                is_symbol: false,
            })
        }
        _ => None,
    }
}

struct MapPath {
    keys: Vec<String>,
    maps: Vec<TypedKind>,
    final_value: TypedKind,
}

fn collect_map_path(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    mut current: TypedKind,
    path: &[AstExpr],
) -> Option<MapPath> {
    if path.is_empty() {
        return None;
    }
    let mut keys = Vec::new();
    let mut maps = Vec::new();
    for (idx, step) in path.iter().enumerate() {
        let TypedKind::Map(key_kind, value_kind) = current else {
            return None;
        };
        maps.push(TypedKind::Map(key_kind.clone(), value_kind.clone()));
        let key = emit_typed_expr(ctx, env, fns, locals, step)?;
        if key.kind != *key_kind {
            return None;
        }
        let key_expr = emit_key_expr(ctx, step, &key)?;
        keys.push(key_expr);
        if idx + 1 == path.len() {
            return Some(MapPath {
                keys,
                maps,
                final_value: *value_kind,
            });
        }
        current = *value_kind;
    }
    None
}

fn emit_typed_get_in(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 && args.len() != 3 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let AstExpr::Vector(path_items) = &args[1] else {
        return None;
    };
    let default_expr = if args.len() == 3 {
        Some(emit_typed_expr(ctx, env, fns, locals, &args[2])?)
    } else {
        None
    };
    let mut current = coll.kind.clone();
    let mut steps = Vec::new();
    for step in path_items {
        match current.clone() {
            TypedKind::Map(key_kind, value_kind) => {
                let key = emit_typed_expr(ctx, env, fns, locals, step)?;
                if key.kind != *key_kind {
                    return None;
                }
                let key_expr = emit_key_expr(ctx, step, &key)?;
                steps.push(format!(
                    ".and_then(|value| value.get(&{key}).cloned())",
                    key = key_expr
                ));
                current = *value_kind;
            }
            TypedKind::Vec(elem_kind) => {
                let key = emit_typed_expr(ctx, env, fns, locals, step)?;
                if key.kind != TypedKind::Int {
                    return None;
                }
                steps.push(format!(
                    ".and_then(|value| value.get({idx} as usize).cloned())",
                    idx = key.code
                ));
                current = *elem_kind;
            }
            _ => return None,
        }
    }
    if let Some(default) = &default_expr {
        if default.kind != current {
            return None;
        }
    }
    let coll_var = ctx.temp("coll");
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_var} = {coll}.clone();\n",
            coll_var = coll_var,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n",
            coll_var = coll_var,
            coll = coll.code
        )
    };
    let mut expr = format!("Some({coll_var})", coll_var = coll_var);
    for step in steps {
        expr.push_str(&step);
    }
    let has_default = default_expr.is_some();
    let expr = if let Some(ref default) = default_expr {
        format!(
            "{expr}.unwrap_or({default})",
            expr = expr,
            default = default.code
        )
    } else {
        expr
    };
    let result_kind = if has_default {
        current
    } else {
        TypedKind::Optional(Box::new(current))
    };
    let code = format!(
        "{{\n{coll_bind}    {expr}\n}}",
        coll_bind = coll_bind,
        expr = expr
    );
    Some(TypedExpr {
        code,
        kind: result_kind,
        is_symbol: false,
    })
}

fn emit_typed_every(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    if inline.kind != TypedKind::Bool {
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let code = format!(
        "{{\n{coll_bind}        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            if !{inline_expr} {{\n                return false;\n            }}\n        }}\n        true\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        value_var = value_var,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Bool,
        is_symbol: false,
    })
}

fn emit_typed_not_every(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    let TypedKind::Vec(elem_kind) = coll.kind.clone() else {
        return None;
    };
    let elem_kind = *elem_kind;
    let coll_var = ctx.temp("coll");
    let coll_ref = ctx.temp("coll_ref");
    let value_var = ctx.temp("value");
    let inline = emit_typed_unary_inline(ctx, env, fns, locals, &args[0], &elem_kind, &value_var)?;
    if inline.kind != TypedKind::Bool {
        return None;
    }
    let coll_bind = if coll.is_symbol {
        format!(
            "        let {coll_ref} = &{coll};\n",
            coll_ref = coll_ref,
            coll = coll.code
        )
    } else {
        format!(
            "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
            coll_var = coll_var,
            coll_ref = coll_ref,
            coll = coll.code
        )
    };
    let code = format!(
        "{{\n{coll_bind}        for {value_var} in {coll_ref}.iter() {{\n            let {value_var} = {value_var}.clone();\n{inline_prelude}            if !{inline_expr} {{\n                return true;\n            }}\n        }}\n        false\n    }}",
        coll_bind = coll_bind,
        coll_ref = coll_ref,
        value_var = value_var,
        inline_prelude = indent(&inline.prelude, 12),
        inline_expr = inline.expr
    );
    Some(TypedExpr {
        code,
        kind: TypedKind::Bool,
        is_symbol: false,
    })
}

fn emit_typed_contains(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 {
        return None;
    }
    let coll = match emit_typed_expr(ctx, env, fns, locals, &args[0]) {
        Some(coll) => coll,
        None => {
            typed_debug("contains? coll failed");
            return None;
        }
    };
    let key = match emit_typed_expr(ctx, env, fns, locals, &args[1]) {
        Some(key) => key,
        None => {
            typed_debug("contains? key failed");
            return None;
        }
    };
    typed_debug(&format!(
        "contains? coll={:?} key={:?}",
        coll.kind, key.kind
    ));
    match coll.kind.clone() {
        TypedKind::Vec(_) => {
            if key.kind != TypedKind::Int {
                return None;
            }
            let coll_var = ctx.temp("coll");
            let coll_ref = ctx.temp("coll_ref");
            let key_var = ctx.temp("idx");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let key_bind = if key.is_symbol {
                String::new()
            } else {
                format!(
                    "        let {key_var} = {key};\n",
                    key_var = key_var,
                    key = key.code
                )
            };
            let key_expr = if key.is_symbol { key.code } else { key_var };
            let code = format!(
                "{{\n{coll_bind}{key_bind}        if {key_expr} < 0 {{ false }} else {{ ({key_expr} as usize) < {coll_ref}.len() }}\n    }}",
                coll_bind = coll_bind,
                key_bind = key_bind,
                key_expr = key_expr,
                coll_ref = coll_ref
            );
            Some(TypedExpr {
                code,
                kind: TypedKind::Bool,
                is_symbol: false,
            })
        }
        TypedKind::Map(key_kind, _) => {
            if key.kind != *key_kind {
                return None;
            }
            let coll_var = ctx.temp("map");
            let coll_ref = ctx.temp("map_ref");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let key_expr = emit_key_expr(ctx, &args[1], &key)?;
            let code = format!(
                "{{\n{coll_bind}        {coll_ref}.contains_key(&{key})\n    }}",
                coll_bind = coll_bind,
                coll_ref = coll_ref,
                key = key_expr
            );
            Some(TypedExpr {
                code,
                kind: TypedKind::Bool,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_nth(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 2 && args.len() != 3 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let idx = emit_typed_expr(ctx, env, fns, locals, &args[1])?;
    if idx.kind != TypedKind::Int {
        return None;
    }
    let default_expr = if args.len() == 3 {
        Some(emit_typed_expr(ctx, env, fns, locals, &args[2])?)
    } else {
        None
    };
    let idx_var = ctx.temp("idx");
    match coll.kind.clone() {
        TypedKind::Vec(elem_kind) => {
            if let Some(default) = &default_expr {
                if default.kind != *elem_kind {
                    return None;
                }
            }
            let coll_var = ctx.temp("coll");
            let coll_ref = ctx.temp("coll_ref");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let default_code = if let Some(default) = default_expr {
                default.code
            } else {
                "panic!(\"nth index out of range\")".to_string()
            };
            let code = format!(
                "{{\n{coll_bind}        let {idx_var} = {idx};\n        if {idx_var} < 0 {{ panic!(\"nth expects non-negative Int index\"); }}\n        let {idx_var} = {idx_var} as usize;\n        if {idx_var} < {coll_ref}.len() {{ {coll_ref}[{idx_var}].clone() }} else {{ {default_code} }}\n    }}",
                coll_bind = coll_bind,
                idx_var = idx_var,
                idx = idx.code,
                coll_ref = coll_ref,
                default_code = default_code
            );
            Some(TypedExpr {
                code,
                kind: *elem_kind,
                is_symbol: false,
            })
        }
        TypedKind::Str => {
            if let Some(default) = &default_expr {
                if default.kind != TypedKind::Str {
                    return None;
                }
            }
            let coll_var = ctx.temp("coll");
            let coll_ref = ctx.temp("coll_ref");
            let chars_var = ctx.temp("chars");
            let coll_bind = if coll.is_symbol {
                format!(
                    "        let {coll_ref} = &{coll};\n",
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            } else {
                format!(
                    "        let {coll_var} = {coll};\n        let {coll_ref} = &{coll_var};\n",
                    coll_var = coll_var,
                    coll_ref = coll_ref,
                    coll = coll.code
                )
            };
            let default_code = if let Some(default) = default_expr {
                default.code
            } else {
                "panic!(\"nth index out of range\")".to_string()
            };
            let code = format!(
                "{{\n{coll_bind}        let {idx_var} = {idx};\n        if {idx_var} < 0 {{ panic!(\"nth expects non-negative Int index\"); }}\n        let {idx_var} = {idx_var} as usize;\n        let {chars_var}: Vec<char> = {coll_ref}.chars().collect();\n        if {idx_var} < {chars_var}.len() {{ {chars_var}[{idx_var}].to_string() }} else {{ {default_code} }}\n    }}",
                coll_bind = coll_bind,
                idx_var = idx_var,
                idx = idx.code,
                chars_var = chars_var,
                coll_ref = coll_ref,
                default_code = default_code
            );
            Some(TypedExpr {
                code,
                kind: TypedKind::Str,
                is_symbol: false,
            })
        }
        _ => None,
    }
}

fn emit_typed_assoc_in(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() != 3 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let AstExpr::Vector(path_items) = &args[1] else {
        return None;
    };
    let path = collect_map_path(ctx, env, fns, locals, coll.kind.clone(), path_items)?;
    let value = emit_typed_expr(ctx, env, fns, locals, &args[2])?;
    if value.kind != path.final_value {
        return None;
    }
    let map0_ty = typed_kind_to_rust(&path.maps[0])?;
    let map0_var = ctx.temp("map0");
    let mut out = String::new();
    out.push_str("{\n");
    if coll.is_symbol {
        out.push_str(&format!(
            "    let mut {map0_var}: {map0_ty} = {coll}.clone();\n",
            map0_var = map0_var,
            map0_ty = map0_ty,
            coll = coll.code
        ));
    } else {
        out.push_str(&format!(
            "    let mut {map0_var}: {map0_ty} = {coll};\n",
            map0_var = map0_var,
            map0_ty = map0_ty,
            coll = coll.code
        ));
    }
    let mut map_vars = vec![map0_var.clone()];
    for idx in 1..path.keys.len() {
        let map_ty = typed_kind_to_rust(&path.maps[idx])?;
        let prev = &map_vars[idx - 1];
        let next = ctx.temp(&format!("map{idx}"));
        let key_expr = &path.keys[idx - 1];
        out.push_str(&format!(
            "    let mut {next}: {map_ty} = {prev}.get(&{key}).cloned().unwrap_or_else(|| BTreeMap::new());\n",
            next = next,
            map_ty = map_ty,
            prev = prev,
            key = key_expr
        ));
        map_vars.push(next);
    }
    let last_map = map_vars.last().unwrap();
    let last_key = path.keys.last().unwrap();
    let value_expr = emit_value_expr_for_insert(&value);
    out.push_str(&format!(
        "    {last_map}.insert({last_key}, {value});\n",
        last_map = last_map,
        last_key = last_key,
        value = value_expr
    ));
    for idx in (1..map_vars.len()).rev() {
        let child = &map_vars[idx];
        let parent = &map_vars[idx - 1];
        let key_expr = &path.keys[idx - 1];
        out.push_str(&format!(
            "    {parent}.insert({key}, {child});\n",
            parent = parent,
            key = key_expr,
            child = child
        ));
    }
    out.push_str("    ");
    out.push_str(&map0_var);
    out.push_str("\n}");
    Some(TypedExpr {
        code: out,
        kind: path.maps[0].clone(),
        is_symbol: false,
    })
}

fn emit_typed_update_in(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    args: &[AstExpr],
) -> Option<TypedExpr> {
    if args.len() < 3 {
        return None;
    }
    let coll = emit_typed_expr(ctx, env, fns, locals, &args[0])?;
    let AstExpr::Vector(path_items) = &args[1] else {
        return None;
    };
    let path = collect_map_path(ctx, env, fns, locals, coll.kind.clone(), path_items)?;
    let expected_kind = path.final_value.clone();
    let current_kind = match &expected_kind {
        TypedKind::Optional(_) => expected_kind.clone(),
        other => TypedKind::Optional(Box::new(other.clone())),
    };
    let map0_ty = typed_kind_to_rust(&path.maps[0])?;
    let map0_var = ctx.temp("map0");
    let current_var = ctx.temp("current");
    let updated_var = ctx.temp("updated");
    let mut out = String::new();
    out.push_str("{\n");
    if coll.is_symbol {
        out.push_str(&format!(
            "    let mut {map0_var}: {map0_ty} = {coll}.clone();\n",
            map0_var = map0_var,
            map0_ty = map0_ty,
            coll = coll.code
        ));
    } else {
        out.push_str(&format!(
            "    let mut {map0_var}: {map0_ty} = {coll};\n",
            map0_var = map0_var,
            map0_ty = map0_ty,
            coll = coll.code
        ));
    }
    let mut map_vars = vec![map0_var.clone()];
    for idx in 1..path.keys.len() {
        let map_ty = typed_kind_to_rust(&path.maps[idx])?;
        let prev = &map_vars[idx - 1];
        let next = ctx.temp(&format!("map{idx}"));
        let key_expr = &path.keys[idx - 1];
        out.push_str(&format!(
            "    let mut {next}: {map_ty} = {prev}.get(&{key}).cloned().unwrap_or_else(|| BTreeMap::new());\n",
            next = next,
            map_ty = map_ty,
            prev = prev,
            key = key_expr
        ));
        map_vars.push(next);
    }
    let last_map = map_vars.last().unwrap();
    let last_key = path.keys.last().unwrap();
    let current_expr = if matches!(path.final_value, TypedKind::Optional(_)) {
        format!(
            "{last_map}.get(&{last_key}).cloned().flatten()",
            last_map = last_map,
            last_key = last_key
        )
    } else {
        format!(
            "{last_map}.get(&{last_key}).cloned()",
            last_map = last_map,
            last_key = last_key
        )
    };
    out.push_str(&format!(
        "    let {current_var} = {current_expr};\n",
        current_var = current_var,
        current_expr = current_expr
    ));
    let (apply_prelude, apply_expr) = emit_typed_update_apply(
        ctx,
        env,
        fns,
        locals,
        &args[2],
        &current_kind,
        &current_var,
        &args[3..],
        &expected_kind,
    )?;
    if !apply_prelude.is_empty() {
        out.push_str(&indent(&apply_prelude, 4));
    }
    out.push_str(&format!(
        "    let {updated_var} = {apply_expr};\n",
        updated_var = updated_var,
        apply_expr = apply_expr
    ));
    out.push_str(&format!(
        "    {last_map}.insert({last_key}, {updated_var});\n",
        last_map = last_map,
        last_key = last_key,
        updated_var = updated_var
    ));
    for idx in (1..map_vars.len()).rev() {
        let child = &map_vars[idx];
        let parent = &map_vars[idx - 1];
        let key_expr = &path.keys[idx - 1];
        out.push_str(&format!(
            "    {parent}.insert({key}, {child});\n",
            parent = parent,
            key = key_expr,
            child = child
        ));
    }
    out.push_str("    ");
    out.push_str(&map0_var);
    out.push_str("\n}");
    Some(TypedExpr {
        code: out,
        kind: path.maps[0].clone(),
        is_symbol: false,
    })
}

fn emit_typed_update_apply(
    ctx: &mut TypedContext,
    env: &HashMap<String, TypedKind>,
    fns: &HashMap<String, TypedFnSig>,
    locals: &HashMap<String, TypedKind>,
    func: &AstExpr,
    current_kind: &TypedKind,
    current_var: &str,
    extra_args: &[AstExpr],
    expected_kind: &TypedKind,
) -> Option<(String, String)> {
    if extra_args.len() > 1 {
        return None;
    }
    if extra_args.is_empty() {
        let inline =
            emit_typed_unary_inline(ctx, env, fns, locals, func, current_kind, current_var)?;
        if inline.kind != *expected_kind {
            return None;
        }
        return Some((inline.prelude, inline.expr));
    }
    let arg_typed = emit_typed_expr(ctx, env, fns, locals, &extra_args[0])?;
    if let AstExpr::Symbol(sym) = func {
        let canonical = aliases::resolve_alias(sym);
        if canonical == "conj" {
            let TypedKind::Vec(elem_kind) = current_kind else {
                return None;
            };
            if arg_typed.kind != **elem_kind {
                return None;
            }
            let ret_kind = TypedKind::Vec(elem_kind.clone());
            if ret_kind != *expected_kind {
                return None;
            }
            let vec_var = ctx.temp("vec");
            let mut prelude = String::new();
            prelude.push_str(&format!(
                "let mut {vec_var} = {current};\n",
                vec_var = vec_var,
                current = current_var
            ));
            prelude.push_str(&format!(
                "{vec_var}.push({arg});\n",
                vec_var = vec_var,
                arg = arg_typed.code
            ));
            return Some((prelude, vec_var));
        }
    }
    if arg_typed.kind != *current_kind {
        return None;
    }
    let arg_var = ctx.temp("arg");
    let mut prelude = String::new();
    prelude.push_str(&format!(
        "let {arg_var} = {arg};\n",
        arg_var = arg_var,
        arg = arg_typed.code
    ));
    let inline = emit_typed_binary_inline(
        ctx,
        env,
        fns,
        locals,
        func,
        current_kind,
        current_var,
        &arg_var,
    )?;
    if inline.kind != *expected_kind {
        return None;
    }
    prelude.push_str(&inline.prelude);
    Some((prelude, inline.expr))
}

fn emit_top_level_exec(ctx: &mut CodegenContext, item: &TopLevel) -> Result<String, String> {
    match item {
        TopLevel::Def { name, value, .. } => {
            if let Some(value_expr) = emit_const_value(value) {
                Ok(format!(
                    "    let value = {};\n    runtime.set_global({}, value.clone());\n",
                    value_expr,
                    emit_string_literal(name)
                ))
            } else {
                let mut expr_ctx = ExprContext::new();
                let expr = emit_native_expr(ctx, &mut expr_ctx, value)?;
                Ok(format!(
                    "    let value = match runtime.with_native_env(|env| {{ {} }}) {{\n        Ok(value) => value,\n        Err(err) => {{\n            eprintln!(\"{{}}\", err);\n            std::process::exit(1);\n        }}\n    }};\n    runtime.set_global({}, value.clone());\n",
                    expr,
                    emit_string_literal(name)
                ))
            }
        }
        TopLevel::Defn {
            name, params, body, ..
        } => {
            let native_name = ctx.add_native_fn(params, body);
            Ok(format!(
                "    let func = runtime.define_native_function({});\n    runtime.set_global({}, func);\n",
                native_name,
                emit_string_literal(name)
            ))
        }
        TopLevel::DefType { .. } => Ok(String::new()),
        TopLevel::DefForeign { decl, .. } => Ok(format!(
            "    runtime.set_global({}, Value::Builtin({}));\n",
            emit_string_literal(&decl.name),
            emit_string(&format!("__foreign::{}", decl.name))
        )),
        TopLevel::Expr { expr, .. } => {
            if is_use_form(expr) {
                return Ok(String::new());
            }
            let mut expr_ctx = ExprContext::new();
            let expr = emit_native_expr(ctx, &mut expr_ctx, expr)?;
            Ok(format!(
                "    if let Err(err) = runtime.with_native_env(|env| {{ {} }}) {{\n        eprintln!(\"{{}}\", err);\n        std::process::exit(1);\n    }}\n",
                expr
            ))
        }
    }
}

struct ExprContext {
    scopes: Vec<HashMap<String, String>>,
    use_counts: Vec<HashMap<String, Option<usize>>>,
    local_counter: usize,
}

impl ExprContext {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            use_counts: vec![HashMap::new()],
            local_counter: 0,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.use_counts.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.use_counts.pop();
    }

    fn bind(&mut self, name: &str, local: String) {
        self.bind_with_uses(name, local, None);
    }

    fn bind_with_uses(&mut self, name: &str, local: String, uses: Option<usize>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), local);
        }
        if let Some(scope) = self.use_counts.last_mut() {
            scope.insert(name.to_string(), uses);
        }
    }

    fn resolve(&self, name: &str) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(local) = scope.get(name) {
                return Some(local.clone());
            }
        }
        None
    }

    fn consume_use(&mut self, name: &str) -> Option<bool> {
        for scope in self.use_counts.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                let Some(remaining) = entry.as_mut() else {
                    return Some(false);
                };
                if *remaining <= 1 {
                    if *remaining > 0 {
                        *remaining -= 1;
                    }
                    return Some(true);
                }
                *remaining -= 1;
                return Some(false);
            }
        }
        None
    }

    fn new_local(&mut self) -> String {
        let name = format!("local_{}", self.local_counter);
        self.local_counter += 1;
        name
    }
}

fn emit_native_function(codegen: &mut CodegenContext, def: &NativeFnDef) -> Result<String, String> {
    let mut expr_ctx = ExprContext::new();
    let mut out = String::new();
    out.push_str(&format!(
        "fn {}(env: &mut NativeEnv, args: Vec<Value>) -> Result<Value, Clove2Error> {{\n",
        def.name
    ));

    let mut has_rest = false;
    let mut fixed_count = 0usize;
    for param in &def.params {
        if param.rest {
            has_rest = true;
            break;
        }
        fixed_count += 1;
    }
    if has_rest {
        out.push_str(&format!(
            "    if args.len() < {} {{\n        return Err(Clove2Error::new({}));\n    }}\n",
            fixed_count,
            emit_string_literal(&format!(
                "native function expects at least {} arguments",
                fixed_count
            ))
        ));
    } else {
        out.push_str(&format!(
            "    if args.len() != {} {{\n        return Err(Clove2Error::new({}));\n    }}\n",
            fixed_count,
            emit_string_literal(&format!(
                "native function expects {} arguments",
                fixed_count
            ))
        ));
    }

    let body_refs: Vec<&AstExpr> = def.body.iter().collect();
    let mut param_use_counts: HashMap<String, usize> = HashMap::new();
    for param in &def.params {
        let count = count_symbol_uses_in_exprs(&body_refs, &param.name);
        param_use_counts.insert(param.name.clone(), count);
    }
    for (idx, param) in def.params.iter().enumerate() {
        if param.rest {
            let local = format!("param_{}", idx);
            let uses = param_use_counts.get(&param.name).copied();
            expr_ctx.bind_with_uses(&param.name, local.clone(), uses);
            out.push_str(&format!(
                "    let {} = Value::vec(args[{}..].to_vec());\n",
                local, fixed_count
            ));
            break;
        } else {
            let local = format!("param_{}", idx);
            let uses = param_use_counts.get(&param.name).copied();
            expr_ctx.bind_with_uses(&param.name, local.clone(), uses);
            out.push_str(&format!("    let {} = args[{}].clone();\n", local, idx));
        }
    }

    let body = emit_native_block(codegen, &mut expr_ctx, &def.body)?;
    out.push_str(&indent(&body, 4));
    out.push_str("\n}\n");
    Ok(out)
}

fn emit_native_block(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    body: &[AstExpr],
) -> Result<String, String> {
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str("    let mut last = Value::Nil;\n");
    for expr in body {
        let expr_code = emit_native_expr(codegen, ctx, expr)?;
        out.push_str("    last = ");
        out.push_str(&expr_code);
        out.push_str("?;\n");
    }
    out.push_str("    Ok::<Value, Clove2Error>(last)\n");
    out.push_str("}");
    Ok(out)
}

fn emit_native_expr(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    expr: &AstExpr,
) -> Result<String, String> {
    match expr {
        AstExpr::Literal(lit) => Ok(format!(
            "Ok::<Value, Clove2Error>({})",
            emit_value_literal(lit)
        )),
        AstExpr::Symbol(sym) => {
            if let Some(local) = ctx.resolve(sym) {
                let move_value = ctx.consume_use(sym).unwrap_or(false);
                if move_value {
                    Ok(format!("Ok::<Value, Clove2Error>({})", local))
                } else {
                    Ok(format!("Ok::<Value, Clove2Error>({}.clone())", local))
                }
            } else if builtins::is_builtin(sym) || sym == "fs::delete" {
                Ok(format!(
                    "Ok::<Value, Clove2Error>(Value::Builtin({}))",
                    emit_string(sym)
                ))
            } else {
                Ok(format!(
                    "env.get_global({}).ok_or_else(|| Clove2Error::new({}))",
                    emit_string_literal(sym),
                    emit_string_literal(&format!("unknown symbol: {}", sym))
                ))
            }
        }
        AstExpr::Keyword(sym) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Keyword({}))",
            emit_string(sym)
        )),
        AstExpr::Vector(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_native_expr(codegen, ctx, item)?;
                out.push_str("    items.push(");
                out.push_str(&item_expr);
                out.push_str("?);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::vec(items))\n}");
            Ok(out)
        }
        AstExpr::Set(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_native_expr(codegen, ctx, item)?;
                let local = ctx.new_local();
                out.push_str("    let ");
                out.push_str(&local);
                out.push_str(" = ");
                out.push_str(&item_expr);
                out.push_str("?;\n");
                out.push_str("    if !items.iter().any(|existing| existing == &");
                out.push_str(&local);
                out.push_str(") {\n        items.push(");
                out.push_str(&local);
                out.push_str(");\n    }\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::Set(items))\n}");
            Ok(out)
        }
        AstExpr::Map(entries) => {
            let mut out = String::new();
            out.push_str("{\n    let mut args = Vec::new();\n");
            for (key_expr, val_expr) in entries {
                let key = emit_native_expr(codegen, ctx, key_expr)?;
                let val = emit_native_expr(codegen, ctx, val_expr)?;
                out.push_str("    args.push(");
                out.push_str(&key);
                out.push_str("?);\n");
                out.push_str("    args.push(");
                out.push_str(&val);
                out.push_str("?);\n");
            }
            out.push_str("    env.apply_builtin(\"hash-map\", args)\n}");
            Ok(out)
        }
        AstExpr::Quote(expr) => emit_quote_expr(expr),
        AstExpr::ForeignBlock { .. } => Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("foreign block is not supported in native codegen")
        )),
        AstExpr::Fn { params, body, .. } => {
            let name = codegen.add_native_fn(params, body);
            Ok(format!(
                "Ok::<Value, Clove2Error>(env.define_native_function({}))",
                name
            ))
        }
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => {
            let cond_expr = emit_native_expr(codegen, ctx, cond)?;
            let then_expr = emit_native_expr(codegen, ctx, then_expr)?;
            let else_expr = match else_expr {
                Some(expr) => emit_native_expr(codegen, ctx, expr)?,
                None => "Ok::<Value, Clove2Error>(Value::Nil)".to_string(),
            };
            Ok(format!(
                "{{\n    let cond = {}?;\n    if is_truthy(&cond) {{\n        {}\n    }} else {{\n        {}\n    }}\n}}",
                cond_expr, then_expr, else_expr
            ))
        }
        AstExpr::Let { bindings, body } => {
            ctx.push_scope();
            let mut out = String::new();
            out.push_str("{\n");
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
            for binding in bindings {
                let value = emit_native_expr(codegen, ctx, &binding.value)?;
                let local = ctx.new_local();
                out.push_str("    let ");
                out.push_str(&local);
                out.push_str(" = ");
                out.push_str(&value);
                out.push_str("?;\n");
                if binding.name.starts_with("__loop__") {
                    out.push_str("    env.define_global(");
                    out.push_str(&emit_string_literal(&binding.name));
                    out.push_str(", ");
                    out.push_str(&local);
                    out.push_str(".clone())?;\n");
                }
                let uses = binding_use_counts.get(&binding.name).copied();
                ctx.bind_with_uses(&binding.name, local, uses);
            }
            let body_expr = emit_native_block(codegen, ctx, body)?;
            out.push_str(&indent(&body_expr, 4));
            out.push_str("\n}");
            ctx.pop_scope();
            Ok(out)
        }
        AstExpr::SetVar { name, value } => {
            let value_expr = emit_native_expr(codegen, ctx, value)?;
            Ok(format!(
                "{{\n    let value = {}?;\n    env.set_global({}, value.clone())?;\n    Ok::<Value, Clove2Error>(value)\n}}",
                value_expr,
                emit_string_literal(name)
            ))
        }
        AstExpr::Call { callee, args } => emit_native_call(codegen, ctx, callee, args),
    }
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
        AstExpr::Literal(_)
        | AstExpr::Keyword(_)
        | AstExpr::ForeignBlock { .. }
        | AstExpr::Quote(_) => 0,
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

fn emit_native_call(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    callee: &AstExpr,
    args: &[AstExpr],
) -> Result<String, String> {
    if let AstExpr::Symbol(sym) = callee {
        let canonical = aliases::resolve_alias(sym);
        let rewritten_args = rewrite_binding_sugar(canonical, args);
        let args = rewritten_args.as_deref().unwrap_or(args);
        if canonical == "comment" {
            return Ok("Ok::<Value, Clove2Error>(Value::Nil)".to_string());
        }
        if canonical == "try" {
            return emit_native_try(codegen, ctx, args);
        }
        if canonical == "throw" {
            return emit_native_throw(codegen, ctx, args);
        }
        if canonical == "do" {
            return emit_native_do(codegen, ctx, args);
        }
        if canonical == "mut" {
            return emit_native_mut(codegen, ctx, MutMode::Mut, args);
        }
        if canonical == "imut" {
            return emit_native_mut(codegen, ctx, MutMode::Imut, args);
        }
        if canonical == "range" {
            return emit_native_range(codegen, ctx, args);
        }
        if canonical == "map" {
            return emit_native_map(codegen, ctx, args);
        }
        if canonical == "filter" {
            return emit_native_filter(codegen, ctx, args);
        }
        if canonical == "take" {
            return emit_native_take(codegen, ctx, args);
        }
        if canonical == "take-while" {
            return emit_native_take_while(codegen, ctx, args);
        }
        if canonical == "when" {
            return emit_native_when(codegen, ctx, args);
        }
        if canonical == "and" || canonical == "or" {
            return emit_native_and_or(codegen, ctx, canonical, args);
        }
        if canonical == "expect" || canonical == "as" {
            let args_expr = emit_native_args_with_type_first(codegen, ctx, args)?;
            return Ok(format!(
                "{{\n    let args = {}?;\n    env.apply_builtin({}, args)\n}}",
                args_expr,
                emit_string_literal(canonical)
            ));
        }
        if canonical == "update-in" && args.len() >= 3 {
            if let AstExpr::Symbol(name) = &args[0] {
                if ctx.resolve(name).is_none() {
                    let path_expr = emit_native_expr(codegen, ctx, &args[1])?;
                    let func_expr = emit_native_expr(codegen, ctx, &args[2])?;
                    let mut extra_lines = String::new();
                    for extra in &args[3..] {
                        let extra_expr = emit_native_expr(codegen, ctx, extra)?;
                        extra_lines.push_str("    extras.push(");
                        extra_lines.push_str(&extra_expr);
                        extra_lines.push_str("?);\n");
                    }
                    return Ok(format!(
                        "{{\n    let path = {path}?;\n    let func = {func}?;\n    let mut extras = Vec::new();\n{extras}    env.update_in_global({name}, path, func, extras)\n}}",
                        path = path_expr,
                        func = func_expr,
                        extras = extra_lines,
                        name = emit_string_literal(name)
                    ));
                }
            }
        }
        if builtins::is_builtin(canonical) || canonical == "fs::delete" {
            let args_expr = emit_native_args(codegen, ctx, args)?;
            return Ok(format!(
                "{{\n    let args = {}?;\n    env.apply_builtin({}, args)\n}}",
                args_expr,
                emit_string_literal(canonical)
            ));
        }
    }
    let func_expr = emit_native_expr(codegen, ctx, callee)?;
    let args_expr = emit_native_args(codegen, ctx, args)?;
    Ok(format!(
        "{{\n    let func = {}?;\n    let args = {}?;\n    env.call_value(&func, args)\n}}",
        func_expr, args_expr
    ))
}

fn rewrite_binding_sugar(name: &str, args: &[AstExpr]) -> Option<Vec<AstExpr>> {
    if !is_binding_sugar_target(name) {
        return None;
    }
    if args.len() != 2 {
        return None;
    }
    let AstExpr::Vector(bindings) = &args[0] else {
        return None;
    };
    if bindings.len() != 2 {
        return None;
    }
    let AstExpr::Symbol(name_sym) = &bindings[0] else {
        return None;
    };
    let param = Param {
        name: name_sym.clone(),
        ty: None,
        rest: false,
    };
    let func_expr = AstExpr::Fn {
        params: vec![param],
        ret: None,
        body: vec![args[1].clone()],
    };
    Some(vec![func_expr, bindings[1].clone()])
}

fn is_binding_sugar_target(name: &str) -> bool {
    matches!(
        name,
        "map"
            | "pmap"
            | "filter"
            | "pfilter"
            | "remove"
            | "keep"
            | "some"
            | "every?"
            | "not-any?"
            | "not-every?"
            | "take-while"
            | "drop-while"
            | "split-with"
            | "partition-by"
            | "group-by"
            | "run!"
            | "sort-by"
    )
}

fn emit_native_range(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() || args.len() > 3 {
        return Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("range expects 1 to 3 arguments")
        ));
    }
    let mut out = String::new();
    out.push_str("{\n");
    let mut vars = Vec::new();
    for expr in args {
        let value_var = ctx.new_local();
        let expr_code = emit_native_expr(codegen, ctx, expr)?;
        out.push_str("    let ");
        out.push_str(&value_var);
        out.push_str(" = ");
        out.push_str(&expr_code);
        out.push_str("?;\n");
        let int_var = ctx.new_local();
        out.push_str("    let ");
        out.push_str(&int_var);
        out.push_str(" = match ");
        out.push_str(&value_var);
        out.push_str(" {\n");
        out.push_str("        Value::Int(value) => value,\n");
        out.push_str("        Value::Float(value) => value as i64,\n");
        out.push_str("        Value::Str(value) => value.trim().parse::<i64>().map_err(|_| Clove2Error::new(\"range expects int arguments\"))?,\n");
        out.push_str(
            "        _ => return Err(Clove2Error::new(\"range expects int arguments\")),\n",
        );
        out.push_str("    };\n");
        vars.push(int_var);
    }
    let (start_var, end_var, step_var) = if vars.len() == 1 {
        ("0".to_string(), vars[0].clone(), "1".to_string())
    } else if vars.len() == 2 {
        (vars[0].clone(), vars[1].clone(), "1".to_string())
    } else {
        (vars[0].clone(), vars[1].clone(), vars[2].clone())
    };
    out.push_str("    if ");
    out.push_str(&step_var);
    out.push_str(
        " == 0 {\n        return Err(Clove2Error::new(\"range expects non-zero step\"));\n    }\n",
    );
    out.push_str("    let mut out = Vec::new();\n");
    out.push_str("    if ");
    out.push_str(&step_var);
    out.push_str(" > 0 {\n");
    out.push_str("        let mut value = ");
    out.push_str(&start_var);
    out.push_str(";\n        while value < ");
    out.push_str(&end_var);
    out.push_str(" {\n            out.push(Value::Int(value));\n            value += ");
    out.push_str(&step_var);
    out.push_str(";\n        }\n    } else {\n");
    out.push_str("        let mut value = ");
    out.push_str(&start_var);
    out.push_str(";\n        while value > ");
    out.push_str(&end_var);
    out.push_str(" {\n            out.push(Value::Int(value));\n            value += ");
    out.push_str(&step_var);
    out.push_str(";\n        }\n    }\n");
    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
    Ok(out)
}

fn emit_native_map(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.len() < 2 {
        return Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("map expects at least 2 arguments")
        ));
    }
    if args.len() == 2 {
        if let AstExpr::Vector(bindings) = &args[0] {
            if bindings.len() == 2 {
                if let AstExpr::Symbol(name_sym) = &bindings[0] {
                    let param = Param {
                        name: name_sym.clone(),
                        ty: None,
                        rest: false,
                    };
                    let func_expr = AstExpr::Fn {
                        params: vec![param],
                        ret: None,
                        body: vec![args[1].clone()],
                    };
                    return emit_native_map_impl(
                        codegen,
                        ctx,
                        func_expr,
                        vec![bindings[1].clone()],
                    );
                }
            }
        }
    }
    let func_expr = args[0].clone();
    let coll_exprs = args[1..].to_vec();
    emit_native_map_impl(codegen, ctx, func_expr, coll_exprs)
}

fn emit_native_map_impl(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    func_expr: AstExpr,
    coll_exprs: Vec<AstExpr>,
) -> Result<String, String> {
    let mut out = String::new();
    out.push_str("{\n");
    let func_code = emit_native_expr(codegen, ctx, &func_expr)?;
    let func_var = ctx.new_local();
    out.push_str("    let ");
    out.push_str(&func_var);
    out.push_str(" = ");
    out.push_str(&func_code);
    out.push_str("?;\n");
    let mut coll_vars = Vec::new();
    for coll in coll_exprs {
        let coll_code = emit_native_expr(codegen, ctx, &coll)?;
        let coll_var = ctx.new_local();
        out.push_str("    let ");
        out.push_str(&coll_var);
        out.push_str(" = ");
        out.push_str(&coll_code);
        out.push_str("?;\n");
        coll_vars.push(coll_var);
    }
    let items_var = ctx.new_local();
    let min_var = ctx.new_local();
    out.push_str("    let mut ");
    out.push_str(&items_var);
    out.push_str(" = Vec::new();\n");
    out.push_str("    let mut ");
    out.push_str(&min_var);
    out.push_str(" = usize::MAX;\n");
    for coll_var in &coll_vars {
        let items_name = ctx.new_local();
        out.push_str("    let ");
        out.push_str(&items_name);
        out.push_str(" = ");
        out.push_str(&emit_into_vec_items(coll_var, "map"));
        out.push_str(";\n");
        out.push_str("    ");
        out.push_str(&min_var);
        out.push_str(" = ");
        out.push_str(&min_var);
        out.push_str(".min(");
        out.push_str(&items_name);
        out.push_str(".len());\n");
        out.push_str("    ");
        out.push_str(&items_var);
        out.push_str(".push(");
        out.push_str(&items_name);
        out.push_str(");\n");
    }
    out.push_str("    if ");
    out.push_str(&min_var);
    out.push_str(" == usize::MAX {\n        ");
    out.push_str(&min_var);
    out.push_str(" = 0;\n    }\n");
    let result_var = ctx.new_local();
    let out_var = ctx.new_local();
    out.push_str("    let ");
    out.push_str(&result_var);
    out.push_str(" = if ");
    out.push_str(&items_var);
    out.push_str(".len() == 1 {\n");
    out.push_str("        let items = ");
    out.push_str(&items_var);
    out.push_str(".pop().unwrap_or_default();\n");
    out.push_str("        let mut ");
    out.push_str(&out_var);
    out.push_str(" = Vec::with_capacity(items.len());\n");
    out.push_str("        for item in items {\n");
    out.push_str("            ");
    out.push_str(&out_var);
    out.push_str(".push(env.call_value(&");
    out.push_str(&func_var);
    out.push_str(", vec![item])?);\n");
    out.push_str("        }\n");
    out.push_str("        Value::vec(");
    out.push_str(&out_var);
    out.push_str(")\n    } else {\n");
    out.push_str("        let mut ");
    out.push_str(&out_var);
    out.push_str(" = Vec::with_capacity(");
    out.push_str(&min_var);
    out.push_str(");\n");
    out.push_str("        for idx in 0..");
    out.push_str(&min_var);
    out.push_str(" {\n            let mut call_args = Vec::with_capacity(");
    out.push_str(&items_var);
    out.push_str(".len());\n            for items in ");
    out.push_str(&items_var);
    out.push_str(".iter_mut() {\n                let value = std::mem::replace(&mut items[idx], Value::Nil);\n                call_args.push(value);\n            }\n            ");
    out.push_str(&out_var);
    out.push_str(".push(env.call_value(&");
    out.push_str(&func_var);
    out.push_str(", call_args)?);\n        }\n");
    out.push_str("        Value::vec(");
    out.push_str(&out_var);
    out.push_str(")\n    };\n");
    out.push_str("    Ok::<Value, Clove2Error>(");
    out.push_str(&result_var);
    out.push_str(")\n}");
    Ok(out)
}

fn emit_native_filter(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.len() != 2 {
        return Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("filter expects 2 arguments")
        ));
    }
    let (func_expr, coll_expr) = if let AstExpr::Vector(bindings) = &args[0] {
        if bindings.len() == 2 {
            if let AstExpr::Symbol(name_sym) = &bindings[0] {
                let param = Param {
                    name: name_sym.clone(),
                    ty: None,
                    rest: false,
                };
                let func_expr = AstExpr::Fn {
                    params: vec![param],
                    ret: None,
                    body: vec![args[1].clone()],
                };
                (func_expr, bindings[1].clone())
            } else {
                (args[0].clone(), args[1].clone())
            }
        } else {
            (args[0].clone(), args[1].clone())
        }
    } else {
        (args[0].clone(), args[1].clone())
    };
    let mut out = String::new();
    out.push_str("{\n");
    let func_code = emit_native_expr(codegen, ctx, &func_expr)?;
    let func_var = ctx.new_local();
    out.push_str("    let ");
    out.push_str(&func_var);
    out.push_str(" = ");
    out.push_str(&func_code);
    out.push_str("?;\n");
    let coll_code = emit_native_expr(codegen, ctx, &coll_expr)?;
    let coll_var = ctx.new_local();
    out.push_str("    let ");
    out.push_str(&coll_var);
    out.push_str(" = ");
    out.push_str(&coll_code);
    out.push_str("?;\n");
    let items_var = ctx.new_local();
    out.push_str("    let ");
    out.push_str(&items_var);
    out.push_str(" = ");
    out.push_str(&emit_into_vec_items(&coll_var, "filter"));
    out.push_str(";\n");
    let out_var = ctx.new_local();
    out.push_str("    let mut ");
    out.push_str(&out_var);
    out.push_str(" = Vec::new();\n");
    out.push_str("    for item in ");
    out.push_str(&items_var);
    out.push_str(" {\n        let keep = env.call_value(&");
    out.push_str(&func_var);
    out.push_str(", vec![item.clone()])?;\n        if is_truthy(&keep) {\n            ");
    out.push_str(&out_var);
    out.push_str(".push(item);\n        }\n    }\n");
    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(");
    out.push_str(&out_var);
    out.push_str("))\n}");
    Ok(out)
}

fn emit_into_vec_items(value_name: &str, name: &str) -> String {
    format!(
        "match {value} {{\n        Value::Vec(items) => match std::rc::Rc::try_unwrap(items) {{\n            Ok(items) => items,\n            Err(items) => items.as_ref().clone(),\n        }},\n        Value::List(items) | Value::Set(items) => items,\n        Value::Str(text) => text.chars().map(|ch| Value::Str(ch.to_string())).collect(),\n        _ => return Err(Clove2Error::new({err})),\n    }}",
        value = value_name,
        err = emit_string_literal(&format!("{} expects vector, list, set, or string", name))
    )
}

fn emit_native_take(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.len() != 2 {
        return Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("take expects 2 arguments")
        ));
    }
    let count_expr = emit_native_expr(codegen, ctx, &args[0])?;
    let count_var = ctx.new_local();
    let count_val = ctx.new_local();
    let count_usize = ctx.new_local();
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str("    let ");
    out.push_str(&count_var);
    out.push_str(" = ");
    out.push_str(&count_expr);
    out.push_str("?;\n");
    out.push_str("    let ");
    out.push_str(&count_val);
    out.push_str(" = match ");
    out.push_str(&count_var);
    out.push_str(" {\n        Value::Int(value) => value,\n        _ => return Err(Clove2Error::new(\"take expects Int count\")),\n    };\n");
    out.push_str("    let ");
    out.push_str(&count_usize);
    out.push_str(" = if ");
    out.push_str(&count_val);
    out.push_str(" < 0 { 0 } else { ");
    out.push_str(&count_val);
    out.push_str(" as usize };\n");
    out.push_str("    if ");
    out.push_str(&count_usize);
    out.push_str(
        " == 0 {\n        return Ok::<Value, Clove2Error>(Value::vec(Vec::new()));\n    }\n",
    );

    if let AstExpr::Call { callee, args } = &args[1] {
        if let AstExpr::Symbol(sym) = callee.as_ref() {
            match sym.as_str() {
                "range" if args.is_empty() => {
                    out.push_str("    let mut out = Vec::with_capacity(");
                    out.push_str(&count_usize);
                    out.push_str(");\n");
                    out.push_str("    for idx in 0..");
                    out.push_str(&count_usize);
                    out.push_str(" {\n        out.push(Value::Int(idx as i64));\n    }\n");
                    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
                    return Ok(out);
                }
                "repeat" if args.len() == 1 => {
                    let value_expr = emit_native_expr(codegen, ctx, &args[0])?;
                    let value_var = ctx.new_local();
                    out.push_str("    let ");
                    out.push_str(&value_var);
                    out.push_str(" = ");
                    out.push_str(&value_expr);
                    out.push_str("?;\n");
                    out.push_str("    let mut out = Vec::with_capacity(");
                    out.push_str(&count_usize);
                    out.push_str(");\n");
                    out.push_str("    for _ in 0..");
                    out.push_str(&count_usize);
                    out.push_str(" {\n        out.push(");
                    out.push_str(&value_var);
                    out.push_str(".clone());\n    }\n");
                    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
                    return Ok(out);
                }
                "repeatedly" if args.len() == 1 => {
                    let func_expr = emit_native_expr(codegen, ctx, &args[0])?;
                    let func_var = ctx.new_local();
                    out.push_str("    let ");
                    out.push_str(&func_var);
                    out.push_str(" = ");
                    out.push_str(&func_expr);
                    out.push_str("?;\n");
                    out.push_str("    let mut out = Vec::with_capacity(");
                    out.push_str(&count_usize);
                    out.push_str(");\n");
                    out.push_str("    for _ in 0..");
                    out.push_str(&count_usize);
                    out.push_str(" {\n        out.push(env.call_value(&");
                    out.push_str(&func_var);
                    out.push_str(", Vec::new())?);\n    }\n");
                    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
                    return Ok(out);
                }
                "iterate" if args.len() == 2 => {
                    let func_expr = emit_native_expr(codegen, ctx, &args[0])?;
                    let init_expr = emit_native_expr(codegen, ctx, &args[1])?;
                    let func_var = ctx.new_local();
                    let current_var = ctx.new_local();
                    out.push_str("    let ");
                    out.push_str(&func_var);
                    out.push_str(" = ");
                    out.push_str(&func_expr);
                    out.push_str("?;\n");
                    out.push_str("    let mut ");
                    out.push_str(&current_var);
                    out.push_str(" = ");
                    out.push_str(&init_expr);
                    out.push_str("?;\n");
                    out.push_str("    let mut out = Vec::with_capacity(");
                    out.push_str(&count_usize);
                    out.push_str(");\n");
                    out.push_str("    for _ in 0..");
                    out.push_str(&count_usize);
                    out.push_str(" {\n        out.push(");
                    out.push_str(&current_var);
                    out.push_str(".clone());\n        let next = env.call_value(&");
                    out.push_str(&func_var);
                    out.push_str(", vec![");
                    out.push_str(&current_var);
                    out.push_str(".clone()])?;\n        ");
                    out.push_str(&current_var);
                    out.push_str(" = next;\n    }\n");
                    out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
                    return Ok(out);
                }
                _ => {}
            }
        }
    }

    let args_expr = emit_native_args(codegen, ctx, args)?;
    out.push_str("    let args = ");
    out.push_str(&args_expr);
    out.push_str("?;\n    env.apply_builtin(\"take\", args)\n}");
    Ok(out)
}

fn emit_native_take_while(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.len() != 2 {
        return Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("take-while expects 2 arguments")
        ));
    }
    if let AstExpr::Call {
        callee,
        args: iter_args,
    } = &args[1]
    {
        if let AstExpr::Symbol(sym) = callee.as_ref() {
            if sym == "iterate" && iter_args.len() == 2 {
                let func_expr = emit_native_expr(codegen, ctx, &args[0])?;
                let iter_expr = emit_native_expr(codegen, ctx, &iter_args[0])?;
                let init_expr = emit_native_expr(codegen, ctx, &iter_args[1])?;
                let func_var = ctx.new_local();
                let iter_var = ctx.new_local();
                let current_var = ctx.new_local();
                let mut out = String::new();
                out.push_str("{\n");
                out.push_str("    let ");
                out.push_str(&func_var);
                out.push_str(" = ");
                out.push_str(&func_expr);
                out.push_str("?;\n");
                out.push_str("    let ");
                out.push_str(&iter_var);
                out.push_str(" = ");
                out.push_str(&iter_expr);
                out.push_str("?;\n");
                out.push_str("    let mut ");
                out.push_str(&current_var);
                out.push_str(" = ");
                out.push_str(&init_expr);
                out.push_str("?;\n");
                out.push_str("    let mut out = Vec::new();\n");
                out.push_str("    loop {\n        let keep = env.call_value(&");
                out.push_str(&func_var);
                out.push_str(", vec![");
                out.push_str(&current_var);
                out.push_str(".clone()])?;\n        if !is_truthy(&keep) {\n            break;\n        }\n        out.push(");
                out.push_str(&current_var);
                out.push_str(".clone());\n        let next = env.call_value(&");
                out.push_str(&iter_var);
                out.push_str(", vec![");
                out.push_str(&current_var);
                out.push_str(".clone()])?;\n        ");
                out.push_str(&current_var);
                out.push_str(" = next;\n    }\n");
                out.push_str("    Ok::<Value, Clove2Error>(Value::vec(out))\n}");
                return Ok(out);
            }
        }
    }
    let args_expr = emit_native_args(codegen, ctx, args)?;
    Ok(format!(
        "{{\n    let args = {}?;\n    env.apply_builtin(\"take-while\", args)\n}}",
        args_expr
    ))
}

fn emit_native_and_or(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    op: &str,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() {
        if op == "and" {
            return Ok("Ok::<Value, Clove2Error>(Value::Bool(true))".to_string());
        }
        return Ok("Ok::<Value, Clove2Error>(Value::Nil)".to_string());
    }
    let mut out = String::new();
    out.push_str("{\n    let mut last = Value::Nil;\n");
    let label = if op == "and" { "and" } else { "or" };
    out.push_str(&format!("    '{}: {{\n", label));
    for expr in args {
        let expr_code = emit_native_expr(codegen, ctx, expr)?;
        out.push_str("        let value = ");
        out.push_str(&expr_code);
        out.push_str("?;\n        if ");
        if op == "and" {
            out.push_str("!is_truthy(&value)");
        } else {
            out.push_str("is_truthy(&value)");
        }
        out.push_str(" {\n            last = value;\n            break '");
        out.push_str(label);
        out.push_str(";\n        }\n        last = value;\n");
    }
    out.push_str("    }\n    Ok::<Value, Clove2Error>(last)\n}");
    Ok(out)
}

fn emit_native_throw(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.len() != 1 {
        return Err("throw expects 1 argument".to_string());
    }
    let value_expr = emit_native_expr(codegen, ctx, &args[0])?;
    Ok(format!(
        "{{\n    let value = {}?;\n    Err(Clove2Error::thrown(value))\n}}",
        value_expr
    ))
}

struct TryCatchClause {
    name: String,
    body: Vec<AstExpr>,
}

fn parse_try_bindings(items: &[AstExpr]) -> Result<Vec<(String, AstExpr)>, String> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        let AstExpr::Symbol(name_sym) = &items[idx] else {
            return Err("try binding name must be symbol".to_string());
        };
        let mut name = name_sym.clone();
        if name.ends_with(':') {
            name = name.trim_end_matches(':').to_string();
            idx += 1;
            if idx >= items.len() {
                return Err("try binding missing type".to_string());
            }
        }
        idx += 1;
        let value_expr = items
            .get(idx)
            .ok_or_else(|| "try binding missing value".to_string())?;
        out.push((name, value_expr.clone()));
        idx += 1;
    }
    Ok(out)
}

fn parse_try_catch_clause(expr: &AstExpr) -> Result<Option<TryCatchClause>, String> {
    let AstExpr::Call { callee, args } = expr else {
        return Ok(None);
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return Ok(None);
    };
    if sym == "catch" {
        if args.len() < 3 {
            return Err("catch expects type, name, and body".to_string());
        }
        let AstExpr::Symbol(name_sym) = &args[1] else {
            return Err("catch expects symbol name".to_string());
        };
        let body: Vec<AstExpr> = args.iter().skip(2).cloned().collect();
        if body.is_empty() {
            return Err("catch expects body".to_string());
        }
        return Ok(Some(TryCatchClause {
            name: name_sym.clone(),
            body,
        }));
    }
    if sym == "err" {
        if args.is_empty() {
            return Err("err expects body".to_string());
        }
        let (name, body_start) = if args.len() == 1 {
            ("?".to_string(), 0)
        } else {
            let AstExpr::Symbol(name_sym) = &args[0] else {
                return Err("err expects symbol name".to_string());
            };
            (name_sym.clone(), 1)
        };
        let body: Vec<AstExpr> = args.iter().skip(body_start).cloned().collect();
        if body.is_empty() {
            return Err("err expects body".to_string());
        }
        return Ok(Some(TryCatchClause { name, body }));
    }
    Ok(None)
}

fn parse_try_finally_clause(expr: &AstExpr) -> Result<Option<Vec<AstExpr>>, String> {
    let AstExpr::Call { callee, args } = expr else {
        return Ok(None);
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return Ok(None);
    };
    if sym != "finally" && sym != "fin" {
        return Ok(None);
    }
    if args.is_empty() {
        return Err("finally expects body".to_string());
    }
    Ok(Some(args.clone()))
}

fn is_try_handler_expr(expr: &AstExpr) -> bool {
    matches!(expr, AstExpr::Fn { .. } | AstExpr::Symbol(_))
}

fn emit_native_try(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() {
        return Err("try expects body".to_string());
    }
    let mut idx = 0;
    let mut bindings = Vec::new();
    if let Some(AstExpr::Vector(items)) = args.get(0) {
        bindings = parse_try_bindings(items)?;
        idx = 1;
    }
    let mut body: Vec<AstExpr> = Vec::new();
    let mut catches: Vec<TryCatchClause> = Vec::new();
    let mut finally_body: Option<Vec<AstExpr>> = None;
    for arg in args.iter().skip(idx) {
        if let Some(catch) = parse_try_catch_clause(arg)? {
            catches.push(catch);
            continue;
        }
        if let Some(fin) = parse_try_finally_clause(arg)? {
            finally_body = Some(fin);
            continue;
        }
        body.push(arg.clone());
    }
    let mut on_error: Option<AstExpr> = None;
    let mut on_finally: Option<AstExpr> = None;
    if catches.is_empty() && finally_body.is_none() {
        if body.len() >= 2 && is_try_handler_expr(body.last().unwrap()) {
            if is_try_handler_expr(&body[body.len() - 2]) {
                on_finally = body.pop();
                on_error = body.pop();
            } else {
                on_error = body.pop();
            }
        }
    }
    if body.is_empty() {
        return Err("try expects body".to_string());
    }

    ctx.push_scope();
    let mut out = String::new();
    out.push_str("{\n");
    for (name, value_expr) in bindings {
        let local = ctx.new_local();
        let value_code = emit_native_expr(codegen, ctx, &value_expr)?;
        out.push_str("    let ");
        out.push_str(&local);
        out.push_str(" = ");
        out.push_str(&value_code);
        out.push_str("?;\n");
        ctx.bind(&name, local);
    }
    let body_code = emit_native_block(codegen, ctx, &body)?;
    out.push_str("    let result = (|| -> Result<Value, Clove2Error> ");
    out.push_str(&body_code);
    out.push_str(")();\n");
    out.push_str("    let result = match result {\n");
    out.push_str("        Ok(value) => Ok::<Value, Clove2Error>(value),\n");
    out.push_str("        Err(err) => {\n");
    out.push_str(
        "            let err_value = if let Some(value) = err.thrown_value() {\n                value.clone()\n            } else {\n                Value::Str(err.to_string())\n            };\n",
    );
    if let Some(catch) = catches.first() {
        ctx.push_scope();
        let local = ctx.new_local();
        ctx.bind(&catch.name, local.clone());
        out.push_str("            let ");
        out.push_str(&local);
        out.push_str(" = err_value.clone();\n");
        let catch_code = emit_native_block(codegen, ctx, &catch.body)?;
        ctx.pop_scope();
        out.push_str(&indent(&catch_code, 12));
        out.push_str("\n");
    } else if let Some(handler) = on_error {
        if let AstExpr::Fn { params, body, .. } = handler {
            if params.len() != 1 {
                return Err("try on-error handler expects 1 argument".to_string());
            }
            ctx.push_scope();
            let local = ctx.new_local();
            ctx.bind(&params[0].name, local.clone());
            out.push_str("            let ");
            out.push_str(&local);
            out.push_str(" = err_value.clone();\n");
            let handler_code = emit_native_block(codegen, ctx, &body)?;
            ctx.pop_scope();
            out.push_str(&indent(&handler_code, 12));
            out.push_str("\n");
        } else {
            let handler_code = emit_native_expr(codegen, ctx, &handler)?;
            out.push_str("            let handler = ");
            out.push_str(&handler_code);
            out.push_str("?\n");
            out.push_str("                .clone();\n");
            out.push_str("            env.call_value(&handler, vec![err_value])\n");
        }
    } else {
        out.push_str("            Err(err)\n");
    }
    out.push_str("        }\n");
    out.push_str("    };\n");
    if let Some(fin_body) = finally_body {
        let fin_code = emit_native_block(codegen, ctx, &fin_body)?;
        out.push_str("    let _ = ");
        out.push_str(&fin_code);
        out.push_str("?;\n");
    } else if let Some(handler) = on_finally {
        if let AstExpr::Fn { params, body, .. } = handler {
            if !params.is_empty() {
                return Err("try on-finally handler expects 0 arguments".to_string());
            }
            let handler_code = emit_native_block(codegen, ctx, &body)?;
            out.push_str("    let _ = ");
            out.push_str(&handler_code);
            out.push_str("?;\n");
        } else {
            let handler_code = emit_native_expr(codegen, ctx, &handler)?;
            out.push_str("    let _ = {\n        let handler = ");
            out.push_str(&handler_code);
            out.push_str("?;\n        env.call_value(&handler, Vec::new())\n    }?;\n");
        }
    }
    out.push_str("    result\n");
    out.push_str("}");
    ctx.pop_scope();
    Ok(out)
}

fn emit_native_do(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() {
        return Ok("Ok::<Value, Clove2Error>(Value::Nil)".to_string());
    }
    let mut out = String::new();
    out.push_str("{\n    let mut last = Value::Nil;\n");
    for expr in args {
        let expr_code = emit_native_expr(codegen, ctx, expr)?;
        out.push_str("    last = ");
        out.push_str(&expr_code);
        out.push_str("?;\n");
    }
    out.push_str("    Ok::<Value, Clove2Error>(last)\n}");
    Ok(out)
}

fn emit_native_mut(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    mode: MutMode,
    args: &[AstExpr],
) -> Result<String, String> {
    let body = emit_native_do(codegen, ctx, args)?;
    let mut out = String::new();
    out.push_str("{\n    env.push_mut_mode(");
    match mode {
        MutMode::Mut => out.push_str("MutMode::Mut"),
        MutMode::Imut => out.push_str("MutMode::Imut"),
    }
    out.push_str(");\n    let result = ");
    out.push_str(&body);
    out.push_str(";\n    env.pop_mut_mode();\n    result\n}\n");
    Ok(out)
}

fn emit_native_when(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() {
        return Ok("Ok::<Value, Clove2Error>(Value::Nil)".to_string());
    }
    let cond = emit_native_expr(codegen, ctx, &args[0])?;
    let body = if args.len() > 1 {
        emit_native_do(codegen, ctx, &args[1..])?
    } else {
        "Ok::<Value, Clove2Error>(Value::Nil)".to_string()
    };
    Ok(format!(
        "{{\n    let cond = {}?;\n    if !is_truthy(&cond) {{\n        Ok::<Value, Clove2Error>(Value::Nil)\n    }} else {{\n        {}\n    }}\n}}",
        cond, body
    ))
}

fn emit_native_args(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    let mut out = String::new();
    out.push_str("{\n    let mut args = Vec::new();\n");
    for expr in args {
        let expr_code = emit_native_expr(codegen, ctx, expr)?;
        out.push_str("    args.push(");
        out.push_str(&expr_code);
        out.push_str("?);\n");
    }
    out.push_str("    Ok::<Vec<Value>, Clove2Error>(args)\n}");
    Ok(out)
}

fn emit_native_args_with_type_first(
    codegen: &mut CodegenContext,
    ctx: &mut ExprContext,
    args: &[AstExpr],
) -> Result<String, String> {
    if args.is_empty() {
        return emit_native_args(codegen, ctx, args);
    }
    let mut out = String::new();
    out.push_str("{\n    let mut args = Vec::new();\n");
    for (idx, expr) in args.iter().enumerate() {
        let expr_code = if idx == 0 {
            emit_type_value(expr)?
        } else {
            emit_native_expr(codegen, ctx, expr)?
        };
        out.push_str("    args.push(");
        out.push_str(&expr_code);
        out.push_str("?);\n");
    }
    out.push_str("    Ok::<Vec<Value>, Clove2Error>(args)\n}");
    Ok(out)
}

fn emit_type_value(expr: &AstExpr) -> Result<String, String> {
    match expr {
        AstExpr::Symbol(sym) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Symbol({}))",
            emit_string(sym)
        )),
        AstExpr::Keyword(sym) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Keyword({}))",
            emit_string(sym)
        )),
        AstExpr::Literal(Literal::Str(value)) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Str({}))",
            emit_string(value)
        )),
        AstExpr::Vector(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_type_value(item)?;
                out.push_str("    items.push(");
                out.push_str(&item_expr);
                out.push_str("?);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::vec(items))\n}");
            Ok(out)
        }
        AstExpr::Map(entries) => {
            let mut out = String::new();
            out.push_str("{\n    let mut map = BTreeMap::new();\n");
            for (key_expr, value_expr) in entries {
                let key = emit_type_key(key_expr)?;
                let value = emit_type_value(value_expr)?;
                out.push_str("    map.insert(");
                out.push_str(&key);
                out.push_str(", ");
                out.push_str(&value);
                out.push_str("?);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::map(map))\n}");
            Ok(out)
        }
        _ => Err("type expression must be symbol".to_string()),
    }
}

fn emit_type_key(expr: &AstExpr) -> Result<String, String> {
    match expr {
        AstExpr::Keyword(sym) => Ok(format!(
            "Key::Keyword(std::rc::Rc::<str>::from({}))",
            emit_string_literal(sym)
        )),
        AstExpr::Symbol(sym) => Ok(format!(
            "Key::Symbol(std::rc::Rc::<str>::from({}))",
            emit_string_literal(sym)
        )),
        AstExpr::Literal(Literal::Str(value)) => Ok(format!(
            "Key::Str(std::rc::Rc::<str>::from({}))",
            emit_string_literal(value)
        )),
        _ => Err("type expression map key must be keyword or symbol".to_string()),
    }
}

fn emit_quote_expr(expr: &Expr) -> Result<String, String> {
    match &expr.kind {
        ExprKind::Literal(lit) => Ok(format!(
            "Ok::<Value, Clove2Error>({})",
            emit_value_literal(lit)
        )),
        ExprKind::Symbol(sym) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Symbol({}))",
            emit_string(sym)
        )),
        ExprKind::Keyword(sym) => Ok(format!(
            "Ok::<Value, Clove2Error>(Value::Keyword({}))",
            emit_string(sym)
        )),
        ExprKind::Vector(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_quote_expr(item)?;
                out.push_str("    items.push(");
                out.push_str(&item_expr);
                out.push_str("?);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::vec(items))\n}");
            Ok(out)
        }
        ExprKind::Set(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_quote_expr(item)?;
                out.push_str("    let value = ");
                out.push_str(&item_expr);
                out.push_str("?;\n    if !items.iter().any(|existing| existing == &value) {\n        items.push(value);\n    }\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::Set(items))\n}");
            Ok(out)
        }
        ExprKind::Map(entries) => {
            let mut out = String::new();
            out.push_str("{\n    let mut map = BTreeMap::new();\n");
            for (key_expr, val_expr) in entries {
                let key = emit_quote_expr(key_expr)?;
                let val = emit_quote_expr(val_expr)?;
                out.push_str("    let key_value = ");
                out.push_str(&key);
                out.push_str("?;\n    let key = clove2_core::eval::value_to_key(&key_value)?;\n    let value = ");
                out.push_str(&val);
                out.push_str("?;\n    map.insert(key, value);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::map(map))\n}");
            Ok(out)
        }
        ExprKind::List(items) => {
            let mut out = String::new();
            out.push_str("{\n    let mut items = Vec::new();\n");
            for item in items {
                let item_expr = emit_quote_expr(item)?;
                out.push_str("    items.push(");
                out.push_str(&item_expr);
                out.push_str("?);\n");
            }
            out.push_str("    Ok::<Value, Clove2Error>(Value::List(items))\n}");
            Ok(out)
        }
        ExprKind::ForeignBlock { .. } => Ok(format!(
            "Err(Clove2Error::new({}))",
            emit_string_literal("foreign block is not supported in quote")
        )),
    }
}

fn indent(input: &str, spaces: usize) -> String {
    let pad = " ".repeat(spaces);
    input
        .lines()
        .map(|line| format!("{}{}", pad, line))
        .collect::<Vec<_>>()
        .join("\n")
}
fn emit_value_literal(lit: &Literal) -> String {
    match lit {
        Literal::Nil => "Value::Nil".to_string(),
        Literal::Bool(value) => format!("Value::Bool({})", value),
        Literal::Int(value) => format!("Value::Int({})", value),
        Literal::Float(value) => format!("Value::Float({:?})", value),
        Literal::Str(value) => format!("Value::Str({})", emit_string(value)),
        Literal::Regex(value) => format!("Value::Regex({})", emit_string(value)),
    }
}

fn emit_string(value: &str) -> String {
    format!("{}.to_string()", emit_string_literal(value))
}

fn emit_string_literal(value: &str) -> String {
    format!("{:?}", value)
}

fn emit_const_value(expr: &AstExpr) -> Option<String> {
    match expr {
        AstExpr::Literal(lit) => Some(match lit {
            Literal::Nil => "Value::Nil".to_string(),
            Literal::Bool(value) => format!("Value::Bool({})", value),
            Literal::Int(value) => format!("Value::Int({})", value),
            Literal::Float(value) => format!("Value::Float({:?})", value),
            Literal::Str(value) => format!("Value::Str({})", emit_string(value)),
            Literal::Regex(value) => format!("Value::Regex({})", emit_string(value)),
        }),
        AstExpr::Keyword(sym) => Some(format!("Value::Keyword({})", emit_string(sym))),
        AstExpr::Vector(items) => {
            let mut values = Vec::new();
            for item in items {
                values.push(emit_const_value(item)?);
            }
            Some(format!("Value::vec(vec![{}])", values.join(", ")))
        }
        AstExpr::Map(entries) => {
            let mut inserts = Vec::new();
            for (key_expr, value_expr) in entries {
                let key = emit_const_key(key_expr)?;
                let value = emit_const_value(value_expr)?;
                inserts.push(format!("map.insert({}, {});", key, value));
            }
            let mut out = String::new();
            out.push_str("{\n        let mut map = BTreeMap::new();\n");
            for line in inserts {
                out.push_str("        ");
                out.push_str(&line);
                out.push('\n');
            }
            out.push_str("        Value::map(map)\n    }");
            Some(out)
        }
        _ => None,
    }
}

fn emit_const_key(expr: &AstExpr) -> Option<String> {
    match expr {
        AstExpr::Literal(Literal::Str(value)) => Some(format!(
            "Key::Str(std::rc::Rc::<str>::from({}))",
            emit_string_literal(value)
        )),
        AstExpr::Literal(Literal::Bool(value)) => Some(format!("Key::Bool({})", value)),
        AstExpr::Literal(Literal::Int(value)) => Some(format!("Key::Int({})", value)),
        AstExpr::Keyword(sym) => Some(format!(
            "Key::Keyword(std::rc::Rc::<str>::from({}))",
            emit_string_literal(sym)
        )),
        _ => None,
    }
}

fn is_use_form(expr: &AstExpr) -> bool {
    matches!(
        expr,
        AstExpr::Call { callee, .. } if matches!(callee.as_ref(), AstExpr::Symbol(sym) if sym == "use")
    )
}
