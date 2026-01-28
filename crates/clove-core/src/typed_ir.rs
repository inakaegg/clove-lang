//! Simple typed IR to represent HM inference results.
//! Not wired to codegen yet, but used as an intermediate representation for `--opt hm`.

use std::collections::HashMap;

use crate::ast::{
    desugar_interpolated_regex, desugar_interpolated_string, Form, FormKind, MapItem, Span,
};
use crate::compiler::APPLY_SYM;
use crate::eval::expand_short_fn_to_list;
use crate::fn_meta::{self, SubjectPos};
use crate::form_source;
use crate::reader::{
    OOP_AS_SYM, OOP_BARE_SYM, OOP_DOT_STAGE_SYM, OOP_INDEX_SYM, OOP_LET_SYM, OOP_NIL_SAFE_SYM,
    OOP_SYNTAX_SYM,
};
use crate::symbols::canonical_symbol_name;
use crate::typing::hm::{PrimType, Type};
use crate::typing::infer::InferResult;

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
    pub span: Span,
    pub source: String,
}

#[derive(Clone, Debug)]
pub enum TypedExprKind {
    Literal(Literal),
    Symbol(String),
    Call {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },
    Vector(Vec<TypedExpr>),
    Map(HashMap<String, TypedExpr>),
    Fn {
        params: Vec<String>,
        body: Vec<TypedExpr>,
    },
    Let {
        bindings: Vec<(String, TypedExpr)>,
        body: Vec<TypedExpr>,
    },
    If {
        cond: Box<TypedExpr>,
        then_branch: Vec<TypedExpr>,
        else_branch: Vec<TypedExpr>,
    },
    Do(Vec<TypedExpr>),
    Unknown,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Keyword(String),
    Nil,
}

/// Mini IR for a simple instruction list (scaffolding for codegen).
#[derive(Clone, Debug)]
pub enum CodegenOp {
    ConstInt(i64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstStr(String),
    ConstNil,
    LoadLocal(String),
    Call {
        target: String,
        args: Vec<CodegenOp>,
    },
    Unknown,
}

#[derive(Clone, Debug)]
pub struct CodegenFunction {
    pub name: String,
    pub params: Vec<String>,
    pub ret_ty: Type,
    pub body: Vec<CodegenOp>,
}

/// Convert top-level defn entries in TypedExpr into CodegenFunction.
pub fn lower_typed_exprs_to_funcs(exprs: &[TypedExpr]) -> Vec<CodegenFunction> {
    let mut out = Vec::new();
    for expr in exprs {
        if let TypedExprKind::Call { callee, args } = &expr.kind {
            if let TypedExprKind::Symbol(sym) = &callee.kind {
                if sym == "defn" && args.len() >= 3 {
                    let name = match &args[0].kind {
                        TypedExprKind::Symbol(s) => s.clone(),
                        _ => continue,
                    };
                    let params = match &args[1].kind {
                        TypedExprKind::Vector(vec) => vec
                            .iter()
                            .filter_map(|e| match &e.kind {
                                TypedExprKind::Symbol(s) => Some(s.clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    let body_ops = args
                        .iter()
                        .skip(2)
                        .flat_map(|b| lower_expr_to_ops(b))
                        .collect();
                    out.push(CodegenFunction {
                        name,
                        params,
                        ret_ty: expr.ty.clone(),
                        body: body_ops,
                    });
                }
            }
        }
    }
    out
}

fn lower_expr_to_ops(expr: &TypedExpr) -> Vec<CodegenOp> {
    match &expr.kind {
        TypedExprKind::Literal(Literal::Int(n)) => vec![CodegenOp::ConstInt(*n)],
        TypedExprKind::Literal(Literal::Float(n)) => vec![CodegenOp::ConstFloat(*n)],
        TypedExprKind::Literal(Literal::Bool(b)) => vec![CodegenOp::ConstBool(*b)],
        TypedExprKind::Literal(Literal::Str(s)) => vec![CodegenOp::ConstStr(s.clone())],
        TypedExprKind::Literal(Literal::Nil) => vec![CodegenOp::ConstNil],
        TypedExprKind::Symbol(name) => vec![CodegenOp::LoadLocal(name.clone())],
        TypedExprKind::Call { callee, args } => {
            let mut lowered_args = Vec::new();
            for a in args {
                lowered_args.extend(lower_expr_to_ops(a));
            }
            let target = match &callee.kind {
                TypedExprKind::Symbol(s) => s.clone(),
                _ => "<unknown>".into(),
            };
            vec![CodegenOp::Call {
                target,
                args: lowered_args,
            }]
        }
        _ => vec![CodegenOp::Unknown],
    }
}

/// Build TypedExpr from inference results.
pub fn build_typed_exprs(result: &InferResult) -> Vec<TypedExpr> {
    result
        .forms
        .iter()
        .map(|(form, _)| {
            let ty = result
                .type_map
                .get(&form.span.index)
                .cloned()
                .unwrap_or(Type::Any);
            form_to_typed_expr(form, &result.type_map, ty)
        })
        .collect()
}

fn desugar_oop_chain(form: &Form) -> Option<Form> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    let Some(FormKind::Symbol(sym)) = items.first().map(|item| &item.kind) else {
        return None;
    };
    if sym != OOP_SYNTAX_SYM {
        return None;
    }
    if items.len() < 3 {
        return None;
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
                    let (next_base, start_nil_safe) = resolve_oop_nil_safe_start(current);
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
            let tmp_name = format!("__oop_let{}_{}", form.span.index, oop_let_counter);
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
            let tmp_name = format!("__oop_let{}_{}", form.span.index, oop_let_counter);
            oop_let_counter += 1;
            current = build_oop_let_stash_form(stage.span, &sym_name, current, tmp_name);
            processed_stages += 1;
            continue;
        }
        let Some(next) = build_oop_stage_form(stage, current) else {
            return None;
        };
        current = next;
        processed_stages += 1;
    }
    if nil_safe_active {
        if nil_safe_stages.is_empty() {
            return None;
        }
        current = build_oop_nil_safe_chain(current, &nil_safe_stages)?;
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
    Some(current)
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

fn resolve_oop_nil_safe_start(base: Form) -> (Form, bool) {
    (base, true)
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
    let FormKind::List(items) = &stage.kind else {
        return None;
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
        if !matches!(placeholder.kind, FormKind::Symbol(_)) {
            return None;
        }
        let mut binds = Vec::with_capacity(2);
        binds.push(placeholder);
        binds.push(base);
        let let_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("let".into()), stage.span),
                Form::new(FormKind::Vector(binds), stage.span),
                body,
            ]),
            stage.span,
        );
        return Some(let_form);
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

fn form_to_typed_expr(form: &Form, type_map: &HashMap<usize, Type>, ty: Type) -> TypedExpr {
    if let Some(lowered) = desugar_interpolated_string(form) {
        return form_to_typed_expr(&lowered, type_map, ty);
    }
    if let Some(lowered) = desugar_interpolated_regex(form) {
        return form_to_typed_expr(&lowered, type_map, ty);
    }
    if let Some(lowered) = desugar_oop_chain(form) {
        let lowered_ty = type_map.get(&lowered.span.index).cloned().unwrap_or(ty);
        return form_to_typed_expr(&lowered, type_map, lowered_ty);
    }
    let source = fallback_source(form);
    let kind = match &form.kind {
        FormKind::Int(n) => TypedExprKind::Literal(Literal::Int(*n)),
        FormKind::Float(n) => TypedExprKind::Literal(Literal::Float(*n)),
        FormKind::Bool(b) => TypedExprKind::Literal(Literal::Bool(*b)),
        FormKind::String(s) => TypedExprKind::Literal(Literal::Str(s.clone())),
        FormKind::Regex { pattern, .. } => TypedExprKind::Literal(Literal::Str(pattern.clone())),
        FormKind::Nil => TypedExprKind::Literal(Literal::Nil),
        FormKind::Keyword(name) => TypedExprKind::Literal(Literal::Keyword(name.clone())),
        FormKind::ShortFn(body) => {
            let expanded = expand_short_fn_to_list(body, form.span);
            return form_to_typed_expr(&expanded, type_map, ty);
        }
        FormKind::Symbol(s) => {
            let (base, hint) = symbol_name_and_hint(form, s);
            let ty = if matches!(ty, Type::Any) {
                hint.unwrap_or(ty)
            } else {
                ty
            };
            return TypedExpr {
                kind: TypedExprKind::Symbol(base),
                ty,
                span: form.span,
                source,
            };
        }
        FormKind::Vector(items) => {
            let vals = items
                .iter()
                .map(|f| {
                    let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                    form_to_typed_expr(f, type_map, inner_ty)
                })
                .collect();
            TypedExprKind::Vector(vals)
        }
        FormKind::Set(items) => {
            let mut args = Vec::with_capacity(items.len());
            for f in items {
                let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                args.push(form_to_typed_expr(f, type_map, inner_ty));
            }
            let callee = TypedExpr {
                kind: TypedExprKind::Symbol("hash-set".into()),
                ty: Type::Any,
                span: form.span,
                source: "hash-set".into(),
            };
            TypedExprKind::Call {
                callee: Box::new(callee),
                args,
            }
        }
        FormKind::Map(entries) => {
            let mut map = HashMap::new();
            let mut has_spread = false;
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        if let FormKind::Keyword(name) = &k.kind {
                            let inner_ty =
                                type_map.get(&v.span.index).cloned().unwrap_or(Type::Any);
                            map.insert(name.clone(), form_to_typed_expr(v, type_map, inner_ty));
                        }
                    }
                    MapItem::Spread(_) => {
                        has_spread = true;
                    }
                }
            }
            if has_spread {
                TypedExprKind::Unknown
            } else {
                TypedExprKind::Map(map)
            }
        }
        FormKind::List(items) => list_to_typed_expr(items, form.span, type_map),
        _ => TypedExprKind::Unknown,
    };
    TypedExpr {
        kind,
        ty,
        span: form.span,
        source,
    }
}

fn fallback_source(form: &Form) -> String {
    let lowered = desugar_form_for_fallback(form);
    form_source::form_to_source(&lowered)
}

fn desugar_form_for_fallback(form: &Form) -> Form {
    if let Some(lowered) = desugar_interpolated_string(form) {
        return desugar_form_for_fallback(&lowered);
    }
    if let Some(lowered) = desugar_interpolated_regex(form) {
        return desugar_form_for_fallback(&lowered);
    }
    if let Some(lowered) = desugar_oop_chain(form) {
        return desugar_form_for_fallback(&lowered);
    }
    match &form.kind {
        FormKind::List(items) => clone_form_with_kind(
            form,
            FormKind::List(items.iter().map(desugar_form_for_fallback).collect()),
        ),
        FormKind::Vector(items) => clone_form_with_kind(
            form,
            FormKind::Vector(items.iter().map(desugar_form_for_fallback).collect()),
        ),
        FormKind::Set(items) => clone_form_with_kind(
            form,
            FormKind::Set(items.iter().map(desugar_form_for_fallback).collect()),
        ),
        FormKind::ShortFn(body) => clone_form_with_kind(
            form,
            FormKind::ShortFn(body.iter().map(desugar_form_for_fallback).collect()),
        ),
        FormKind::Map(entries) => {
            let lowered = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => MapItem::KeyValue(
                        desugar_form_for_fallback(k),
                        desugar_form_for_fallback(v),
                    ),
                    MapItem::Spread(value) => MapItem::Spread(desugar_form_for_fallback(value)),
                })
                .collect();
            clone_form_with_kind(form, FormKind::Map(lowered))
        }
        _ => form.clone(),
    }
}

fn clone_form_with_kind(form: &Form, kind: FormKind) -> Form {
    Form {
        kind,
        span: form.span,
        type_hint: form.type_hint.clone(),
    }
}

/// Render TypedExpr values to a simple human-readable format.
pub fn format_typed_exprs(exprs: &[TypedExpr]) -> String {
    let mut out = String::new();
    for expr in exprs {
        fmt_expr(expr, 0, &mut out);
        out.push('\n');
    }
    out
}

fn fmt_expr(expr: &TypedExpr, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);
    out.push_str(&format!("({:?} ", expr.ty));
    match &expr.kind {
        TypedExprKind::Literal(lit) => {
            out.push_str(&format!("{:?}", lit));
        }
        TypedExprKind::Symbol(s) => {
            out.push_str(&format!("symbol {}", s));
        }
        TypedExprKind::Call { callee, args } => {
            out.push_str("call\n");
            fmt_expr(callee, indent + 1, out);
            for arg in args {
                out.push('\n');
                fmt_expr(arg, indent + 1, out);
            }
            out.push_str(&pad);
        }
        TypedExprKind::Vector(items) => {
            out.push_str("vector\n");
            for (i, it) in items.iter().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                fmt_expr(it, indent + 1, out);
            }
            out.push_str(&pad);
        }
        TypedExprKind::Map(map) => {
            out.push_str("map");
            for (i, (k, v)) in map.iter().enumerate() {
                out.push('\n');
                out.push_str(&format!("{}  :{}", pad, k));
                out.push('\n');
                fmt_expr(v, indent + 2, out);
                if i + 1 == map.len() {
                    out.push('\n');
                }
            }
            out.push_str(&pad);
        }
        TypedExprKind::Fn { params, body } => {
            out.push_str(&format!("fn params={:?}", params));
            for b in body {
                out.push('\n');
                fmt_expr(b, indent + 1, out);
            }
            out.push_str(&pad);
        }
        TypedExprKind::Let { bindings, body } => {
            out.push_str("let");
            for (k, v) in bindings {
                out.push('\n');
                out.push_str(&format!("{}  {} =", pad, k));
                out.push('\n');
                fmt_expr(v, indent + 2, out);
            }
            if !body.is_empty() {
                out.push('\n');
                out.push_str(&format!("{}  body:", pad));
                for b in body {
                    out.push('\n');
                    fmt_expr(b, indent + 2, out);
                }
            }
            out.push_str(&pad);
        }
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            out.push_str("if");
            out.push('\n');
            fmt_expr(cond, indent + 1, out);
            for t in then_branch {
                out.push('\n');
                fmt_expr(t, indent + 1, out);
            }
            if !else_branch.is_empty() {
                out.push('\n');
                out.push_str(&format!("{}  else:", pad));
                for e in else_branch {
                    out.push('\n');
                    fmt_expr(e, indent + 2, out);
                }
            }
            out.push_str(&pad);
        }
        TypedExprKind::Do(items) => {
            out.push_str("do");
            for it in items {
                out.push('\n');
                fmt_expr(it, indent + 1, out);
            }
            out.push_str(&pad);
        }
        TypedExprKind::Unknown => out.push_str("unknown"),
    }
    out.push(')');
}

fn base_symbol_name(name: &str) -> String {
    if let Some(idx) = name.find('<') {
        name[..idx].to_string()
    } else {
        name.to_string()
    }
}

fn symbol_name_and_hint(form: &Form, raw: &str) -> (String, Option<Type>) {
    if let Some(idx) = raw.find('<') {
        if let Some(close) = raw.rfind('>') {
            if close > idx {
                let base = raw[..idx].to_string();
                let annot = &raw[idx + 1..close];
                if let Ok(kind) = crate::types::TypeKind::parse(annot) {
                    if let Some(ty) = convert_kind(&kind) {
                        return (base, Some(ty));
                    }
                }
                return (base, None);
            }
        }
    }
    let hint = form.type_hint.as_ref().and_then(|h| convert_kind(&h.kind));
    (raw.to_string(), hint)
}

fn convert_kind(kind: &crate::types::TypeKind) -> Option<Type> {
    use crate::types::TypeKind as K;
    Some(match kind {
        K::Int => Type::Prim(PrimType::Int),
        K::Float => Type::Prim(PrimType::Float),
        K::Bool => Type::Prim(PrimType::Bool),
        K::Str => Type::Prim(PrimType::Str),
        K::Nil => Type::Prim(PrimType::Nil),
        K::Any => Type::Any,
        K::Vector(inner) => Type::Vector(Box::new(convert_kind(inner)?)),
        K::Tuple(_) => Type::Any,
        K::Map(k, v) => Type::Map(Box::new(convert_kind(k)?), Box::new(convert_kind(v)?)),
        K::Set(inner) => Type::Set(Box::new(convert_kind(inner)?)),
        K::Option(inner) => Type::Option(Box::new(convert_kind(inner)?)),
        K::Mut(inner) => Type::Mut(Box::new(convert_kind(inner)?)),
        K::Record(fields) => {
            let mut m = std::collections::HashMap::new();
            for (k, v) in fields {
                m.insert(k.clone(), convert_kind(v)?);
            }
            Type::Record(m)
        }
        K::Union(_) => Type::Any,
        K::Function { .. } => return None,
        K::Named(_) => return None,
    })
}

fn list_to_typed_expr(
    items: &[Form],
    _span: Span,
    type_map: &HashMap<usize, Type>,
) -> TypedExprKind {
    if items.is_empty() {
        return TypedExprKind::Unknown;
    }
    let head_sym = match &items[0].kind {
        FormKind::Symbol(s) => s.as_str(),
        _ => "",
    };
    match head_sym {
        "let" if items.len() >= 3 => {
            let mut binds = Vec::new();
            if let FormKind::Vector(vec) = &items[1].kind {
                let mut idx = 0;
                while idx + 1 < vec.len() {
                    if let FormKind::Symbol(name) = &vec[idx].kind {
                        let v = &vec[idx + 1];
                        let inner_ty = type_map.get(&v.span.index).cloned().unwrap_or(Type::Any);
                        binds.push((
                            base_symbol_name(name),
                            form_to_typed_expr(v, type_map, inner_ty),
                        ));
                    }
                    idx += 2;
                }
            }
            let body = items
                .iter()
                .skip(2)
                .map(|f| {
                    let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                    form_to_typed_expr(f, type_map, inner_ty)
                })
                .collect();
            TypedExprKind::Let {
                bindings: binds,
                body,
            }
        }
        "fn" if items.len() >= 3 => {
            let mut params = Vec::new();
            if let FormKind::Vector(vec) = &items[1].kind {
                for p in vec {
                    if let FormKind::Symbol(name) = &p.kind {
                        params.push(base_symbol_name(name));
                    }
                }
            }
            let body = items
                .iter()
                .skip(2)
                .map(|f| {
                    let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                    form_to_typed_expr(f, type_map, inner_ty)
                })
                .collect();
            TypedExprKind::Fn { params, body }
        }
        "if" if items.len() >= 3 => {
            let cond_ty = type_map
                .get(&items[1].span.index)
                .cloned()
                .unwrap_or(Type::Any);
            let then_ty = type_map
                .get(&items[2].span.index)
                .cloned()
                .unwrap_or(Type::Any);
            let cond = Box::new(form_to_typed_expr(&items[1], type_map, cond_ty));
            let then_branch = vec![form_to_typed_expr(&items[2], type_map, then_ty)];
            let else_branch = if items.len() > 3 {
                let else_ty = type_map
                    .get(&items[3].span.index)
                    .cloned()
                    .unwrap_or(Type::Any);
                vec![form_to_typed_expr(&items[3], type_map, else_ty)]
            } else {
                Vec::new()
            };
            TypedExprKind::If {
                cond,
                then_branch,
                else_branch,
            }
        }
        "do" => {
            let body = items
                .iter()
                .skip(1)
                .map(|f| {
                    let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                    form_to_typed_expr(f, type_map, inner_ty)
                })
                .collect();
            TypedExprKind::Do(body)
        }
        _ => {
            let callee_ty = type_map
                .get(&items[0].span.index)
                .cloned()
                .unwrap_or(Type::Any);
            let callee = Box::new(form_to_typed_expr(&items[0], type_map, callee_ty));
            let args = items
                .iter()
                .skip(1)
                .map(|f| {
                    let inner_ty = type_map.get(&f.span.index).cloned().unwrap_or(Type::Any);
                    form_to_typed_expr(f, type_map, inner_ty)
                })
                .collect();
            TypedExprKind::Call { callee, args }
        }
    }
}
