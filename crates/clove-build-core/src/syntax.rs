use std::collections::BTreeMap;

use crate::aliases;
use crate::ast::{Expr, ExprKind, Literal, Span};
use crate::error::Clove2Error;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub enum AstExpr {
    Literal(Literal),
    Symbol(String),
    Keyword(String),
    Vector(Vec<AstExpr>),
    Set(Vec<AstExpr>),
    Map(Vec<(AstExpr, AstExpr)>),
    Quote(Box<Expr>),
    ForeignBlock {
        tag: String,
        code: String,
    },
    Fn {
        params: Vec<Param>,
        ret: Option<Type>,
        body: Vec<AstExpr>,
    },
    If {
        cond: Box<AstExpr>,
        then_expr: Box<AstExpr>,
        else_expr: Option<Box<AstExpr>>,
    },
    Let {
        bindings: Vec<Binding>,
        body: Vec<AstExpr>,
    },
    SetVar {
        name: String,
        value: Box<AstExpr>,
    },
    Call {
        callee: Box<AstExpr>,
        args: Vec<AstExpr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub name: String,
    pub ty: Option<Type>,
    pub value: AstExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Option<Type>,
    pub rest: bool,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Def {
        name: String,
        ty: Option<Type>,
        value: AstExpr,
        span: Span,
    },
    Defn {
        name: String,
        params: Vec<Param>,
        ret: Option<Type>,
        body: Vec<AstExpr>,
        span: Span,
    },
    DefType {
        name: String,
        fields: BTreeMap<String, Type>,
        span: Span,
    },
    DefForeign {
        decl: ForeignDecl,
        span: Span,
    },
    Expr {
        expr: AstExpr,
        span: Span,
    },
}

impl PartialEq for TopLevel {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                TopLevel::Def {
                    name, ty, value, ..
                },
                TopLevel::Def {
                    name: o_name,
                    ty: o_ty,
                    value: o_value,
                    ..
                },
            ) => name == o_name && ty == o_ty && value == o_value,
            (
                TopLevel::Defn {
                    name,
                    params,
                    ret,
                    body,
                    ..
                },
                TopLevel::Defn {
                    name: o_name,
                    params: o_params,
                    ret: o_ret,
                    body: o_body,
                    ..
                },
            ) => name == o_name && params == o_params && ret == o_ret && body == o_body,
            (
                TopLevel::DefType { name, fields, .. },
                TopLevel::DefType {
                    name: o_name,
                    fields: o_fields,
                    ..
                },
            ) => name == o_name && fields == o_fields,
            (TopLevel::DefForeign { decl, .. }, TopLevel::DefForeign { decl: o_decl, .. }) => {
                decl == o_decl
            }
            (TopLevel::Expr { expr, .. }, TopLevel::Expr { expr: o_expr, .. }) => expr == o_expr,
            _ => false,
        }
    }
}

impl Eq for TopLevel {}

#[derive(Clone, Debug, PartialEq)]
pub struct ForeignDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub options: ForeignOptions,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ForeignOptions {
    pub file: Option<String>,
    pub code: Option<ForeignCode>,
    pub entry: Option<String>,
    pub lang: Option<String>,
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForeignCode {
    String(String),
    Block { tag: String, code: String },
}

pub fn parse_forms(forms: &[Expr]) -> Result<Vec<TopLevel>, Clove2Error> {
    forms.iter().map(parse_top_level).collect()
}

fn parse_top_level(form: &Expr) -> Result<TopLevel, Clove2Error> {
    if let ExprKind::List(items) = &form.kind {
        if let Some(Expr {
            kind: ExprKind::Symbol(head),
            ..
        }) = items.first()
        {
            let head_sym = aliases::resolve_alias(head);
            match head_sym {
                "def" => return parse_def(items, &form.span),
                "defn" => return parse_defn(items, &form.span),
                "deftype" => return parse_deftype(items, &form.span),
                "def-foreign" | "def-interop" | "interop" => {
                    return parse_def_foreign(items, &form.span)
                }
                _ => {}
            }
        }
    }
    Ok(TopLevel::Expr {
        expr: lower_expr(form)?,
        span: form.span.clone(),
    })
}

fn parse_def(items: &[Expr], span: &Span) -> Result<TopLevel, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("def expects name and value"));
    }
    let (name, ty, consumed) = parse_typed_name(items, 1)?;
    if consumed != 1 && consumed != 2 {
        return Err(Clove2Error::new("invalid def name"));
    }
    let value_index = 1 + consumed;
    if value_index != items.len() - 1 {
        return Err(Clove2Error::new("def expects name and value"));
    }
    let value = items
        .get(value_index)
        .ok_or_else(|| Clove2Error::new("def missing value"))?;
    let value = lower_expr(value)?;
    Ok(TopLevel::Def {
        name,
        ty,
        value,
        span: span.clone(),
    })
}

fn parse_defn(items: &[Expr], span: &Span) -> Result<TopLevel, Clove2Error> {
    if items.len() < 4 {
        return Err(Clove2Error::new("defn expects name, params, and body"));
    }
    let name = parse_symbol(
        items
            .get(1)
            .ok_or_else(|| Clove2Error::new("defn expects name and params"))?,
    )?;
    if name.ends_with(':') {
        return Err(Clove2Error::new("defn name cannot be typed"));
    }
    let params_expr = items
        .get(2)
        .ok_or_else(|| Clove2Error::new("defn expects params vector"))?;
    let params = parse_params(params_expr)?;
    let mut idx = 3;
    let mut ret = None;
    if let Some(Expr {
        kind: ExprKind::Symbol(sym),
        ..
    }) = items.get(idx)
    {
        if sym == "->" {
            let ty_expr = items
                .get(idx + 1)
                .ok_or_else(|| Clove2Error::new("defn missing return type"))?;
            ret = Some(parse_type_expr(ty_expr)?);
            idx += 2;
        }
    }
    if idx >= items.len() {
        return Err(Clove2Error::new("defn missing body"));
    }
    let body = items[idx..]
        .iter()
        .map(lower_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(TopLevel::Defn {
        name,
        params,
        ret,
        body,
        span: span.clone(),
    })
}

fn parse_def_foreign(items: &[Expr], span: &Span) -> Result<TopLevel, Clove2Error> {
    if items.len() < 5 {
        return Err(Clove2Error::new(
            "def-foreign expects name, options, and signature",
        ));
    }
    let name = parse_symbol(
        items
            .get(1)
            .ok_or_else(|| Clove2Error::new("def-foreign expects name"))?,
    )?;
    let mut idx = 2;
    let mut options = ForeignOptions::default();
    while idx < items.len() {
        if matches!(
            items.get(idx),
            Some(Expr {
                kind: ExprKind::Vector(_),
                ..
            })
        ) {
            break;
        }
        let key = items
            .get(idx)
            .ok_or_else(|| Clove2Error::new("def-foreign missing option key"))?;
        let key = parse_option_key(key)?;
        let value = items
            .get(idx + 1)
            .ok_or_else(|| Clove2Error::new("def-foreign missing option value"))?;
        apply_option(&mut options, &key, value)?;
        idx += 2;
    }
    let params_expr = items
        .get(idx)
        .ok_or_else(|| Clove2Error::new("def-foreign missing params"))?;
    let params = parse_params(params_expr)?;
    if params.iter().any(|param| param.rest) {
        return Err(Clove2Error::new(
            "def-foreign does not support rest parameters",
        ));
    }
    idx += 1;
    let arrow = items
        .get(idx)
        .ok_or_else(|| Clove2Error::new("def-foreign missing return type"))?;
    let ExprKind::Symbol(sym) = &arrow.kind else {
        return Err(Clove2Error::new("def-foreign expects -> return type"));
    };
    if sym != "->" {
        return Err(Clove2Error::new("def-foreign expects -> return type"));
    }
    let ret_expr = items
        .get(idx + 1)
        .ok_or_else(|| Clove2Error::new("def-foreign missing return type"))?;
    let ret = parse_type_expr(ret_expr)?;
    if options.file.is_none() && options.code.is_none() {
        return Err(Clove2Error::new("def-foreign expects :file or :code"));
    }
    if matches!(options.code, Some(ForeignCode::String(_))) && options.lang.is_none() {
        return Err(Clove2Error::new("def-foreign :code string requires :lang"));
    }
    if options.entry.is_none() {
        options.entry = Some(name.clone());
    }
    Ok(TopLevel::DefForeign {
        decl: ForeignDecl {
            name,
            params,
            ret,
            options,
        },
        span: span.clone(),
    })
}

fn parse_deftype(items: &[Expr], span: &Span) -> Result<TopLevel, Clove2Error> {
    if items.len() != 3 {
        return Err(Clove2Error::new("deftype expects name and field map"));
    }
    let name = parse_symbol(
        items
            .get(1)
            .ok_or_else(|| Clove2Error::new("deftype expects name"))?,
    )?;
    let ExprKind::Map(entries) = &items
        .get(2)
        .ok_or_else(|| Clove2Error::new("deftype expects field map"))?
        .kind
    else {
        return Err(Clove2Error::new("deftype expects field map"));
    };
    let mut fields = BTreeMap::new();
    for (key, val) in entries {
        let field = parse_field_name(key)?;
        let ty = parse_type_expr(val)?;
        if fields.contains_key(&field) {
            return Err(Clove2Error::new("deftype has duplicate field"));
        }
        fields.insert(field, ty);
    }
    Ok(TopLevel::DefType {
        name,
        fields,
        span: span.clone(),
    })
}

fn lower_expr(expr: &Expr) -> Result<AstExpr, Clove2Error> {
    match &expr.kind {
        ExprKind::Literal(lit) => Ok(AstExpr::Literal(lit.clone())),
        ExprKind::Symbol(sym) => Ok(AstExpr::Symbol(sym.clone())),
        ExprKind::Keyword(sym) => Ok(AstExpr::Keyword(sym.clone())),
        ExprKind::Vector(items) => {
            let items = items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(AstExpr::Vector(items))
        }
        ExprKind::Set(items) => {
            let items = items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(AstExpr::Set(items))
        }
        ExprKind::Map(entries) => {
            let mut out = Vec::new();
            for (k, v) in entries {
                out.push((lower_expr(k)?, lower_expr(v)?));
            }
            Ok(AstExpr::Map(out))
        }
        ExprKind::ForeignBlock { tag, code } => Ok(AstExpr::ForeignBlock {
            tag: tag.clone(),
            code: code.clone(),
        }),
        ExprKind::List(items) => parse_list_expr(items),
    }
}

fn parse_list_expr(items: &[Expr]) -> Result<AstExpr, Clove2Error> {
    let Some(head) = items.first() else {
        return Err(Clove2Error::new("empty list is not allowed"));
    };
    if let ExprKind::Symbol(sym) = &head.kind {
        let head_sym = aliases::resolve_alias(sym);
        match head_sym {
            "quote" => {
                if items.len() != 2 {
                    return Err(Clove2Error::new("quote expects 1 argument"));
                }
                return Ok(AstExpr::Quote(Box::new(items[1].clone())));
            }
            "if" => return parse_if(items),
            "if-not" => return lower_expr(&rewrite_if_not(items)?),
            "let" => return parse_let(items),
            "loop" => return lower_expr(&rewrite_loop(items)?),
            "set!" => return parse_set_var(items),
            "if-let" => return lower_expr(&rewrite_if_let(items)?),
            "if-some" => return lower_expr(&rewrite_if_some(items)?),
            "when-let" => return lower_expr(&rewrite_when_let(items)?),
            "when-not" => return lower_expr(&rewrite_when_not(items)?),
            "fn" => return parse_fn(items),
            "cond" => return lower_expr(&rewrite_cond(items)?),
            "condp" => return lower_expr(&rewrite_condp(items)?),
            "->" => return lower_expr(&rewrite_thread(items, false, "->")?),
            "->>" => return lower_expr(&rewrite_thread(items, true, "->>")?),
            "as->" => return lower_expr(&rewrite_as_thread(items)?),
            "for" => return lower_expr(&rewrite_for(items)?),
            "doseq" => return lower_expr(&rewrite_doseq(items)?),
            "dotimes" => return lower_expr(&rewrite_dotimes(items)?),
            "each" => return lower_expr(&rewrite_each(items)?),
            "doto" => return lower_expr(&rewrite_doto(items)?),
            "cond->" => return lower_expr(&rewrite_cond_thread(items, false, "cond->")?),
            "cond->>" => return lower_expr(&rewrite_cond_thread(items, true, "cond->>")?),
            "some->" => return lower_expr(&rewrite_some_thread(items, false, "some->")?),
            "some->>" => return lower_expr(&rewrite_some_thread(items, true, "some->>")?),
            "while" => return lower_expr(&rewrite_while(items)?),
            _ => {}
        }
    }
    let callee = lower_expr(head)?;
    let args = items[1..]
        .iter()
        .map(lower_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(AstExpr::Call {
        callee: Box::new(callee),
        args,
    })
}

fn rewrite_if_not(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 || items.len() > 4 {
        return Err(Clove2Error::new("if-not expects 2 or 3 arguments"));
    }
    let cond = Expr::list(vec![Expr::symbol("not"), items[1].clone()]);
    let then_expr = items[2].clone();
    let mut out = vec![Expr::symbol("if"), cond, then_expr];
    if let Some(else_expr) = items.get(3) {
        out.push(else_expr.clone());
    }
    Ok(Expr::list(out))
}

fn rewrite_when_not(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 2 {
        return Err(Clove2Error::new("when-not expects condition and body"));
    }
    let mut out = Vec::with_capacity(items.len() + 1);
    out.push(Expr::symbol("when"));
    out.push(Expr::list(vec![Expr::symbol("not"), items[1].clone()]));
    out.extend(items.iter().skip(2).cloned());
    Ok(Expr::list(out))
}

fn rewrite_if_let(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 || items.len() > 4 {
        return Err(Clove2Error::new(
            "if-let expects bindings and 2 or 3 arguments",
        ));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("if-let expects bindings vector"))?;
    let (bindings_expr, name) = parse_single_binding(bindings_expr, "if-let")?;
    let then_expr = items[2].clone();
    let else_expr = items
        .get(3)
        .cloned()
        .unwrap_or_else(|| Expr::literal(Literal::Nil));
    let tmp_name = format!("__iflet__{}", head_span_start(items));
    let inner_if = Expr::list(vec![
        Expr::symbol("if"),
        Expr::symbol(tmp_name.clone()),
        Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(name), Expr::symbol(tmp_name.clone())]),
            then_expr,
        ]),
        else_expr,
    ]);
    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![
            Expr::symbol(tmp_name),
            binding_value_expr(&bindings_expr)?,
        ]),
        inner_if,
    ]))
}

fn rewrite_when_let(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("when-let expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("when-let expects bindings vector"))?;
    let (bindings_expr, name) = parse_single_binding(bindings_expr, "when-let")?;
    let tmp_name = format!("__whenlet__{}", head_span_start(items));
    let mut body = Vec::with_capacity(items.len() - 1);
    body.push(Expr::symbol("do"));
    body.extend(items.iter().skip(2).cloned());
    let inner_if = Expr::list(vec![
        Expr::symbol("if"),
        Expr::symbol(tmp_name.clone()),
        Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(name), Expr::symbol(tmp_name.clone())]),
            Expr::list(body),
        ]),
        Expr::literal(Literal::Nil),
    ]);
    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![
            Expr::symbol(tmp_name),
            binding_value_expr(&bindings_expr)?,
        ]),
        inner_if,
    ]))
}

fn rewrite_if_some(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 || items.len() > 4 {
        return Err(Clove2Error::new(
            "if-some expects bindings and 2 or 3 arguments",
        ));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("if-some expects bindings vector"))?;
    let (bindings_expr, name) = parse_single_binding(bindings_expr, "if-some")?;
    let then_expr = items[2].clone();
    let else_expr = items
        .get(3)
        .cloned()
        .unwrap_or_else(|| Expr::literal(Literal::Nil));
    let tmp_name = format!("__ifsome__{}", head_span_start(items));
    let inner_if = Expr::list(vec![
        Expr::symbol("if"),
        Expr::list(vec![Expr::symbol("some?"), Expr::symbol(tmp_name.clone())]),
        Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(name), Expr::symbol(tmp_name.clone())]),
            then_expr,
        ]),
        else_expr,
    ]);
    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![
            Expr::symbol(tmp_name),
            binding_value_expr(&bindings_expr)?,
        ]),
        inner_if,
    ]))
}

fn rewrite_cond(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("cond expects test and expr pairs"));
    }
    let clauses = &items[1..];
    if clauses.len() % 2 != 0 {
        return Err(Clove2Error::new("cond expects even number of forms"));
    }
    let mut out = Expr::literal(Literal::Nil);
    let mut idx = clauses.len();
    while idx >= 2 {
        let expr = clauses[idx - 1].clone();
        let test = clauses[idx - 2].clone();
        if is_else_keyword(&test) {
            if idx != clauses.len() {
                return Err(Clove2Error::new("cond :else must be last"));
            }
            out = expr;
        } else {
            out = Expr::list(vec![Expr::symbol("if"), test, expr, out]);
        }
        idx -= 2;
    }
    Ok(out)
}

fn rewrite_condp(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 4 {
        return Err(Clove2Error::new(
            "condp expects predicate, expr, and clauses",
        ));
    }
    let pred_expr = items[1].clone();
    let target_expr = items[2].clone();
    let clauses = &items[3..];

    enum Clause {
        Normal { test: Expr, expr: Expr },
        Shift { test: Expr, func: Expr },
        Else { expr: Expr },
    }

    let mut parsed = Vec::new();
    let mut idx = 0;
    while idx < clauses.len() {
        let test = clauses[idx].clone();
        if is_else_marker(&test) {
            if idx + 1 >= clauses.len() {
                return Err(Clove2Error::new("condp :else missing body"));
            }
            if idx + 2 != clauses.len() {
                return Err(Clove2Error::new("condp :else must be last"));
            }
            parsed.push(Clause::Else {
                expr: clauses[idx + 1].clone(),
            });
            idx += 2;
            continue;
        }
        if idx + 1 >= clauses.len() {
            return Err(Clove2Error::new("condp expects test and expr pairs"));
        }
        if is_shift_marker(&clauses[idx + 1]) {
            if idx + 2 >= clauses.len() {
                return Err(Clove2Error::new("condp missing :>> body"));
            }
            parsed.push(Clause::Shift {
                test,
                func: clauses[idx + 2].clone(),
            });
            idx += 3;
        } else {
            parsed.push(Clause::Normal {
                test,
                expr: clauses[idx + 1].clone(),
            });
            idx += 2;
        }
    }

    let pred_name = format!("__condp_pred_{}", head_span_start(items));
    let expr_name = format!("__condp_expr_{}", head_span_start(items));
    let mut out = Expr::literal(Literal::Nil);
    for (idx, clause) in parsed.into_iter().rev().enumerate() {
        match clause {
            Clause::Else { expr } => out = expr,
            Clause::Normal { test, expr } => {
                out = Expr::list(vec![
                    Expr::symbol("if"),
                    Expr::list(vec![
                        Expr::symbol(&pred_name),
                        test,
                        Expr::symbol(&expr_name),
                    ]),
                    expr,
                    out,
                ]);
            }
            Clause::Shift { test, func } => {
                let tmp_name = format!("__condp__{}_{}", head_span_start(items), idx);
                out = Expr::list(vec![
                    Expr::symbol("let"),
                    Expr::vector(vec![
                        Expr::symbol(&tmp_name),
                        Expr::list(vec![
                            Expr::symbol(&pred_name),
                            test,
                            Expr::symbol(&expr_name),
                        ]),
                    ]),
                    Expr::list(vec![
                        Expr::symbol("if"),
                        Expr::symbol(tmp_name.clone()),
                        Expr::list(vec![func, Expr::symbol(tmp_name)]),
                        out,
                    ]),
                ]);
            }
        }
    }

    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![Expr::symbol(pred_name), pred_expr]),
        Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(expr_name), target_expr]),
            out,
        ]),
    ]))
}

fn rewrite_cond_thread(items: &[Expr], insert_last: bool, name: &str) -> Result<Expr, Clove2Error> {
    if items.len() < 2 {
        return Err(Clove2Error::new(format!(
            "{} expects at least 1 argument",
            name
        )));
    }
    let clauses = &items[2..];
    if clauses.len() % 2 != 0 {
        return Err(Clove2Error::new(format!(
            "{} expects even number of forms",
            name
        )));
    }
    let mut expr = items[1].clone();
    for (idx, pair) in clauses.chunks(2).enumerate() {
        let test = pair[0].clone();
        let step = pair[1].clone();
        let tmp_name = format!("__condthread_{}_{}", head_span_start(items), idx);
        let threaded = thread_step(Expr::symbol(&tmp_name), &step, insert_last)?;
        expr = Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(&tmp_name), expr]),
            Expr::list(vec![
                Expr::symbol("if"),
                test,
                threaded,
                Expr::symbol(tmp_name),
            ]),
        ]);
    }
    Ok(expr)
}

fn rewrite_as_thread(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 4 {
        return Err(Clove2Error::new("as-> expects value, name, and forms"));
    }
    let init = items[1].clone();
    let name = parse_symbol(&items[2])?;
    if name.ends_with(':') {
        return Err(Clove2Error::new("as-> name cannot be typed"));
    }
    let forms = &items[3..];
    let mut out = forms
        .last()
        .cloned()
        .ok_or_else(|| Clove2Error::new("as-> expects at least 1 form"))?;
    for form in forms.iter().rev().skip(1) {
        out = Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(name.clone()), form.clone()]),
            out,
        ]);
    }
    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![Expr::symbol(name), init]),
        out,
    ]))
}

fn rewrite_for(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("for expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("for expects bindings vector"))?;
    let ExprKind::Vector(bindings) = &bindings_expr.kind else {
        return Err(Clove2Error::new("for expects bindings vector"));
    };
    let body = if items.len() == 3 {
        items[2].clone()
    } else {
        let mut out = Vec::with_capacity(items.len() - 1);
        out.push(Expr::symbol("do"));
        out.extend_from_slice(&items[2..]);
        Expr::list(out)
    };

    #[derive(Clone)]
    struct ForStep {
        name: Expr,
        coll: Expr,
        lets: Vec<Expr>,
        whens: Vec<Expr>,
        whiles: Vec<Expr>,
    }

    let mut steps = Vec::new();
    let mut idx = 0;
    while idx < bindings.len() {
        let name_expr = bindings
            .get(idx)
            .ok_or_else(|| Clove2Error::new("for binding missing name"))?;
        let ExprKind::Symbol(_) = &name_expr.kind else {
            return Err(Clove2Error::new("for binding name must be a symbol"));
        };
        idx += 1;
        let coll_expr = bindings
            .get(idx)
            .ok_or_else(|| Clove2Error::new("for binding missing collection"))?;
        idx += 1;

        let mut step = ForStep {
            name: name_expr.clone(),
            coll: coll_expr.clone(),
            lets: Vec::new(),
            whens: Vec::new(),
            whiles: Vec::new(),
        };

        while idx < bindings.len() {
            let ExprKind::Keyword(key) = &bindings[idx].kind else {
                break;
            };
            match key.as_str() {
                "let" => {
                    idx += 1;
                    let ExprKind::Vector(ref let_items) = bindings
                        .get(idx)
                        .ok_or_else(|| Clove2Error::new("for :let expects bindings vector"))?
                        .kind
                    else {
                        return Err(Clove2Error::new("for :let expects bindings vector"));
                    };
                    step.lets.extend(let_items.clone());
                    idx += 1;
                }
                "when" => {
                    idx += 1;
                    let expr = bindings
                        .get(idx)
                        .ok_or_else(|| Clove2Error::new("for :when expects expression"))?;
                    step.whens.push(expr.clone());
                    idx += 1;
                }
                "while" => {
                    idx += 1;
                    let expr = bindings
                        .get(idx)
                        .ok_or_else(|| Clove2Error::new("for :while expects expression"))?;
                    step.whiles.push(expr.clone());
                    idx += 1;
                }
                _ => {
                    return Err(Clove2Error::new(
                        "for accepts :let, :when, or :while modifiers",
                    ));
                }
            }
        }
        steps.push(step);
    }

    if steps.is_empty() {
        return Err(Clove2Error::new("for expects at least one binding"));
    }

    fn wrap_in_let(bindings: &[Expr], body: Expr) -> Expr {
        if bindings.is_empty() {
            return body;
        }
        Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(bindings.to_vec()),
            body,
        ])
    }

    fn join_with_and(exprs: &[Expr]) -> Expr {
        if exprs.len() == 1 {
            return exprs[0].clone();
        }
        let mut out = Vec::with_capacity(exprs.len() + 1);
        out.push(Expr::symbol("and"));
        out.extend_from_slice(exprs);
        Expr::list(out)
    }

    fn predicate_fn(name: &Expr, lets: &[Expr], preds: &[Expr]) -> Expr {
        let pred_expr = join_with_and(preds);
        let body = wrap_in_let(lets, pred_expr);
        Expr::list(vec![
            Expr::symbol("fn"),
            Expr::vector(vec![name.clone()]),
            body,
        ])
    }

    fn apply_modifiers(step: &ForStep) -> Expr {
        let mut coll = step.coll.clone();
        if !step.whiles.is_empty() {
            let pred = predicate_fn(&step.name, &step.lets, &step.whiles);
            coll = Expr::list(vec![Expr::symbol("take-while"), pred, coll]);
        }
        coll
    }

    fn expr_mentions_any(expr: &Expr, names: &std::collections::HashSet<String>) -> bool {
        match &expr.kind {
            ExprKind::Symbol(sym) => names.contains(sym),
            ExprKind::List(items) => {
                if let Some(Expr {
                    kind: ExprKind::Symbol(sym),
                    ..
                }) = items.first()
                {
                    if sym == "quote" {
                        return false;
                    }
                }
                items.iter().any(|item| expr_mentions_any(item, names))
            }
            ExprKind::Vector(items) | ExprKind::Set(items) => {
                items.iter().any(|item| expr_mentions_any(item, names))
            }
            ExprKind::Map(entries) => entries
                .iter()
                .any(|(k, v)| expr_mentions_any(k, names) || expr_mentions_any(v, names)),
            ExprKind::Literal(_) | ExprKind::ForeignBlock { .. } | ExprKind::Keyword(_) => false,
        }
    }

    fn ensure_no_outer_refs(
        exprs: &[Expr],
        outer_names: &std::collections::HashSet<String>,
        label: &str,
    ) -> Result<(), Clove2Error> {
        if outer_names.is_empty() {
            return Ok(());
        }
        for expr in exprs {
            if expr_mentions_any(expr, outer_names) {
                return Err(Clove2Error::new(format!(
                    "for {} cannot reference outer bindings in native mode",
                    label
                )));
            }
        }
        Ok(())
    }

    fn build_steps(
        steps: &[ForStep],
        outer_names: &[Expr],
        body: Expr,
    ) -> Result<Expr, Clove2Error> {
        let (head, tail) = steps.split_first().expect("for steps not empty");
        let outer_set: std::collections::HashSet<String> = outer_names
            .iter()
            .filter_map(|expr| match &expr.kind {
                ExprKind::Symbol(sym) => Some(sym.clone()),
                _ => None,
            })
            .collect();
        ensure_no_outer_refs(&head.whiles, &outer_set, ":while")?;
        let coll = apply_modifiers(head);
        let mut params = Vec::with_capacity(outer_names.len() + 1);
        params.extend_from_slice(outer_names);
        params.push(head.name.clone());
        let inner = if tail.is_empty() {
            body
        } else {
            build_steps(tail, &params, body)?
        };
        let when_expr = if head.whens.is_empty() {
            inner
        } else {
            let pred = join_with_and(&head.whens);
            Expr::list(vec![
                Expr::symbol("if"),
                pred,
                Expr::vector(vec![inner]),
                Expr::vector(Vec::new()),
            ])
        };
        let body_expr = wrap_in_let(&head.lets, when_expr);
        let func = Expr::list(vec![
            Expr::symbol("fn"),
            Expr::vector(params.clone()),
            body_expr,
        ]);
        let map_sym = if tail.is_empty() && head.whens.is_empty() {
            "map"
        } else {
            "mapcat"
        };
        if outer_names.is_empty() {
            return Ok(Expr::list(vec![Expr::symbol(map_sym), func, coll]));
        }
        let coll_sym = Expr::symbol("__for_coll");
        let count_sym = Expr::symbol("__for_count");
        let count_expr = Expr::list(vec![Expr::symbol("count"), coll_sym.clone()]);
        let mut map_args = Vec::with_capacity(2 + outer_names.len());
        map_args.push(Expr::symbol(map_sym));
        map_args.push(func);
        for name in outer_names {
            map_args.push(Expr::list(vec![
                Expr::symbol("repeat"),
                count_sym.clone(),
                name.clone(),
            ]));
        }
        map_args.push(coll_sym.clone());
        let map_expr = Expr::list(map_args);
        Ok(Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![coll_sym.clone(), coll, count_sym.clone(), count_expr]),
            map_expr,
        ]))
    }

    build_steps(&steps, &[], body)
}

fn rewrite_doseq(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("doseq expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("doseq expects bindings vector"))?;
    let ExprKind::Vector(_) = &bindings_expr.kind else {
        return Err(Clove2Error::new("doseq expects bindings vector"));
    };
    let body = if items.len() == 3 {
        items[2].clone()
    } else {
        let mut out = Vec::with_capacity(items.len() - 1);
        out.push(Expr::symbol("do"));
        out.extend_from_slice(&items[2..]);
        Expr::list(out)
    };
    let iter_body = Expr::list(vec![Expr::symbol("do"), body, Expr::literal(Literal::Nil)]);
    let loop_expr = Expr::list(vec![Expr::symbol("for"), bindings_expr.clone(), iter_body]);
    Ok(Expr::list(vec![
        Expr::symbol("do"),
        loop_expr,
        Expr::literal(Literal::Nil),
    ]))
}

fn rewrite_dotimes(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("dotimes expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("dotimes expects bindings vector"))?;
    let ExprKind::Vector(bindings) = &bindings_expr.kind else {
        return Err(Clove2Error::new("dotimes expects bindings vector"));
    };
    if bindings.len() != 2 {
        return Err(Clove2Error::new("dotimes expects [name count]"));
    }
    let name_expr = bindings
        .get(0)
        .ok_or_else(|| Clove2Error::new("dotimes expects name"))?;
    let ExprKind::Symbol(_) = &name_expr.kind else {
        return Err(Clove2Error::new("dotimes expects symbol name"));
    };
    let count_expr = bindings
        .get(1)
        .ok_or_else(|| Clove2Error::new("dotimes expects count"))?;
    let range_expr = Expr::list(vec![Expr::symbol("range"), count_expr.clone()]);
    let doseq_bindings = Expr::vector(vec![name_expr.clone(), range_expr]);
    let mut doseq_items = Vec::with_capacity(items.len());
    doseq_items.push(Expr::symbol("doseq"));
    doseq_items.push(doseq_bindings);
    doseq_items.extend_from_slice(&items[2..]);
    rewrite_doseq(&doseq_items)
}

fn rewrite_each(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("each expects bindings and body"));
    }
    let (bindings, body_expr, coll_expr) = match &items[1].kind {
        ExprKind::Vector(items_vec) => {
            if items_vec.len() != 2 {
                return Err(Clove2Error::new("each expects [name coll]"));
            }
            let name_expr = items_vec
                .get(0)
                .ok_or_else(|| Clove2Error::new("each expects name"))?;
            let ExprKind::Symbol(_) = &name_expr.kind else {
                return Err(Clove2Error::new("each expects symbol name"));
            };
            let coll_expr = items_vec
                .get(1)
                .ok_or_else(|| Clove2Error::new("each expects collection"))?;
            let body = if items.len() == 3 {
                items[2].clone()
            } else {
                let mut out = Vec::with_capacity(items.len() - 1);
                out.push(Expr::symbol("do"));
                out.extend_from_slice(&items[2..]);
                Expr::list(out)
            };
            (
                Expr::vector(vec![name_expr.clone(), coll_expr.clone()]),
                body,
                coll_expr.clone(),
            )
        }
        _ => {
            if items.len() != 3 {
                return Err(Clove2Error::new("each expects (each f coll)"));
            }
            let func = items[1].clone();
            let coll = items[2].clone();
            let name = Expr::symbol("__each_item");
            let body = Expr::list(vec![func, name.clone()]);
            (Expr::vector(vec![name, coll.clone()]), body, coll)
        }
    };
    let doseq_expr = Expr::list(vec![Expr::symbol("doseq"), bindings, body_expr]);
    Ok(Expr::list(vec![Expr::symbol("do"), doseq_expr, coll_expr]))
}

fn rewrite_doto(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("doto expects target and forms"));
    }
    let target = items[1].clone();
    let tmp_name = format!("__doto__{}", head_span_start(items));
    let target_sym = Expr::symbol(&tmp_name);
    let mut body = Vec::with_capacity(items.len());
    body.push(Expr::symbol("do"));
    for form in items.iter().skip(2) {
        body.push(doto_step(&target_sym, form)?);
    }
    body.push(Expr::symbol(tmp_name.clone()));
    Ok(Expr::list(vec![
        Expr::symbol("let"),
        Expr::vector(vec![Expr::symbol(tmp_name), target]),
        Expr::list(body),
    ]))
}

fn doto_step(target: &Expr, form: &Expr) -> Result<Expr, Clove2Error> {
    match &form.kind {
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(Clove2Error::new("doto form cannot be empty"));
            }
            let mut out = items.clone();
            out.insert(1, target.clone());
            Ok(Expr::list(out))
        }
        _ => Ok(Expr::list(vec![form.clone(), target.clone()])),
    }
}

fn rewrite_loop(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("loop expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("loop expects bindings vector"))?;
    let ExprKind::Vector(bindings) = &bindings_expr.kind else {
        return Err(Clove2Error::new("loop expects bindings vector"));
    };
    if bindings.len() % 2 != 0 {
        return Err(Clove2Error::new("loop bindings must be even"));
    }
    let loop_name = format!("__loop__{}", head_span_start(items));
    let mut params = Vec::new();
    let mut init_args = Vec::new();
    let mut arity = 0usize;
    let mut idx = 0;
    while idx < bindings.len() {
        let name_expr = bindings
            .get(idx)
            .ok_or_else(|| Clove2Error::new("loop binding missing name"))?
            .clone();
        let ExprKind::Symbol(name) = &name_expr.kind else {
            return Err(Clove2Error::new("loop binding name must be symbol"));
        };
        if name.ends_with(':') {
            let ty_expr = bindings
                .get(idx + 1)
                .ok_or_else(|| Clove2Error::new("loop binding missing type"))?;
            params.push(name_expr);
            params.push(ty_expr.clone());
            idx += 2;
        } else {
            params.push(name_expr);
            idx += 1;
        }
        let value_expr = bindings
            .get(idx)
            .ok_or_else(|| Clove2Error::new("loop binding missing value"))?;
        init_args.push(value_expr.clone());
        idx += 1;
        arity += 1;
    }
    let mut body = Vec::new();
    for expr in items.iter().skip(2) {
        body.push(expr.clone());
    }
    let mut rewritten = Vec::with_capacity(body.len());
    for expr in &body {
        rewritten.push(rewrite_recur(expr, &loop_name, arity)?);
    }
    let loop_body = if rewritten.len() == 1 {
        rewritten[0].clone()
    } else {
        let mut out = Vec::with_capacity(rewritten.len() + 1);
        out.push(Expr::symbol("do"));
        out.extend(rewritten);
        Expr::list(out)
    };
    let loop_fn = Expr::list(vec![Expr::symbol("fn"), Expr::vector(params), loop_body]);
    let bindings = Expr::vector(vec![Expr::symbol(&loop_name), loop_fn]);
    let mut call_items = Vec::with_capacity(init_args.len() + 1);
    call_items.push(Expr::symbol(&loop_name));
    call_items.extend(init_args);
    let call_expr = Expr::list(call_items);
    Ok(Expr::list(vec![Expr::symbol("let"), bindings, call_expr]))
}

fn rewrite_recur(expr: &Expr, target: &str, arity: usize) -> Result<Expr, Clove2Error> {
    match &expr.kind {
        ExprKind::List(items) => {
            if items.is_empty() {
                return Ok(expr.clone());
            }
            if let ExprKind::Symbol(sym) = &items[0].kind {
                if sym == "recur" {
                    if items.len() - 1 != arity {
                        return Err(Clove2Error::new(format!(
                            "recur expects {} argument(s)",
                            arity
                        )));
                    }
                    let mut out = Vec::with_capacity(items.len());
                    out.push(Expr::symbol(target));
                    out.extend(items.iter().skip(1).cloned());
                    return Ok(Expr::list(out));
                }
                if matches!(sym.as_str(), "fn" | "loop" | "quote") {
                    return Ok(expr.clone());
                }
            }
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(rewrite_recur(item, target, arity)?);
            }
            Ok(Expr::list(out))
        }
        ExprKind::Vector(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(rewrite_recur(item, target, arity)?);
            }
            Ok(Expr::vector(out))
        }
        ExprKind::Set(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(rewrite_recur(item, target, arity)?);
            }
            Ok(Expr::new(ExprKind::Set(out), expr.span.clone()))
        }
        ExprKind::Map(entries) => {
            let mut out = Vec::with_capacity(entries.len());
            for (key, value) in entries {
                out.push((
                    rewrite_recur(key, target, arity)?,
                    rewrite_recur(value, target, arity)?,
                ));
            }
            Ok(Expr::new(ExprKind::Map(out), expr.span.clone()))
        }
        _ => Ok(expr.clone()),
    }
}

fn rewrite_while(items: &[Expr]) -> Result<Expr, Clove2Error> {
    if items.len() < 2 {
        return Err(Clove2Error::new("while expects condition and body"));
    }
    if items.len() == 2 {
        return Ok(Expr::list(vec![Expr::symbol("do")]));
    }
    let cond = items[1].clone();
    let mut when_items = Vec::with_capacity(items.len() + 1);
    when_items.push(Expr::symbol("when"));
    when_items.push(cond);
    when_items.extend(items.iter().skip(2).cloned());
    when_items.push(Expr::list(vec![Expr::symbol("recur")]));
    Ok(Expr::list(vec![
        Expr::symbol("loop"),
        Expr::vector(Vec::new()),
        Expr::list(when_items),
    ]))
}

fn rewrite_thread(items: &[Expr], insert_last: bool, name: &str) -> Result<Expr, Clove2Error> {
    if items.len() < 2 {
        return Err(Clove2Error::new(format!(
            "{} expects at least 1 argument",
            name
        )));
    }
    let mut expr = items[1].clone();
    for step in items.iter().skip(2) {
        expr = thread_step(expr, step, insert_last)?;
    }
    Ok(expr)
}

fn rewrite_some_thread(items: &[Expr], insert_last: bool, name: &str) -> Result<Expr, Clove2Error> {
    if items.len() < 2 {
        return Err(Clove2Error::new(format!(
            "{} expects at least 1 argument",
            name
        )));
    }
    let mut expr = items[1].clone();
    for (idx, step) in items.iter().skip(2).enumerate() {
        let tmp_name = format!("__somethread_{}_{}", head_span_start(items), idx);
        let threaded = thread_step(Expr::symbol(&tmp_name), step, insert_last)?;
        expr = Expr::list(vec![
            Expr::symbol("let"),
            Expr::vector(vec![Expr::symbol(&tmp_name), expr]),
            Expr::list(vec![
                Expr::symbol("if"),
                Expr::list(vec![Expr::symbol("some?"), Expr::symbol(&tmp_name)]),
                threaded,
                Expr::literal(Literal::Nil),
            ]),
        ]);
    }
    Ok(expr)
}

fn thread_step(current: Expr, step: &Expr, insert_last: bool) -> Result<Expr, Clove2Error> {
    match &step.kind {
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(Clove2Error::new("thread step cannot be empty list"));
            }
            let mut out = items.clone();
            if insert_last {
                out.push(current);
            } else {
                out.insert(1, current);
            }
            Ok(Expr::list(out))
        }
        _ => Ok(Expr::list(vec![step.clone(), current])),
    }
}

fn parse_single_binding(bindings_expr: &Expr, form: &str) -> Result<(Expr, String), Clove2Error> {
    let ExprKind::Vector(items) = &bindings_expr.kind else {
        return Err(Clove2Error::new(format!(
            "{} expects bindings vector",
            form
        )));
    };
    if items.len() < 2 {
        return Err(Clove2Error::new(format!(
            "{} expects a single binding",
            form
        )));
    }
    let (name, _ty, consumed) = parse_typed_name(items, 0)?;
    let value_index = consumed;
    if value_index >= items.len() || value_index != items.len() - 1 {
        return Err(Clove2Error::new(format!(
            "{} expects exactly one binding pair",
            form
        )));
    }
    Ok((bindings_expr.clone(), name))
}

fn binding_value_expr(bindings_expr: &Expr) -> Result<Expr, Clove2Error> {
    let ExprKind::Vector(items) = &bindings_expr.kind else {
        return Err(Clove2Error::new("bindings must be a vector"));
    };
    let (_name, _ty, consumed) = parse_typed_name(items, 0)?;
    let value_index = consumed;
    items
        .get(value_index)
        .cloned()
        .ok_or_else(|| Clove2Error::new("binding missing value"))
}

fn is_else_keyword(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Keyword(sym) if sym == "else")
}

fn is_else_marker(expr: &Expr) -> bool {
    matches!(
        &expr.kind,
        ExprKind::Keyword(sym) if sym == "else"
    ) || matches!(
        &expr.kind,
        ExprKind::Symbol(sym) if sym == "_"
    )
}

fn is_shift_marker(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Keyword(sym) if sym == ">>")
}

fn head_span_start(items: &[Expr]) -> usize {
    items
        .first()
        .map(|expr| expr.span.start)
        .unwrap_or_default()
}

fn parse_fn(items: &[Expr]) -> Result<AstExpr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("fn expects params and body"));
    }
    let params_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("fn expects params vector"))?;
    let params = parse_params(params_expr)?;
    let mut idx = 2;
    let mut ret = None;
    if let Some(Expr {
        kind: ExprKind::Symbol(sym),
        ..
    }) = items.get(idx)
    {
        if sym == "->" {
            let ty_expr = items
                .get(idx + 1)
                .ok_or_else(|| Clove2Error::new("fn missing return type"))?;
            ret = Some(parse_type_expr(ty_expr)?);
            idx += 2;
        }
    }
    if idx >= items.len() {
        return Err(Clove2Error::new("fn missing body"));
    }
    let body = items[idx..]
        .iter()
        .map(lower_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(AstExpr::Fn { params, ret, body })
}

fn parse_if(items: &[Expr]) -> Result<AstExpr, Clove2Error> {
    if items.len() < 3 || items.len() > 4 {
        return Err(Clove2Error::new("if expects 2 or 3 arguments"));
    }
    let cond = lower_expr(&items[1])?;
    let then_expr = lower_expr(&items[2])?;
    let else_expr = if items.len() == 4 {
        Some(Box::new(lower_expr(&items[3])?))
    } else {
        None
    };
    Ok(AstExpr::If {
        cond: Box::new(cond),
        then_expr: Box::new(then_expr),
        else_expr,
    })
}

fn parse_let(items: &[Expr]) -> Result<AstExpr, Clove2Error> {
    if items.len() < 3 {
        return Err(Clove2Error::new("let expects bindings and body"));
    }
    let bindings_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("let expects bindings vector"))?;
    let bindings = parse_bindings(bindings_expr)?;
    let body = items[2..]
        .iter()
        .map(lower_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(AstExpr::Let { bindings, body })
}

fn parse_set_var(items: &[Expr]) -> Result<AstExpr, Clove2Error> {
    if items.len() != 3 {
        return Err(Clove2Error::new("set! expects name and value"));
    }
    let name_expr = items
        .get(1)
        .ok_or_else(|| Clove2Error::new("set! expects name"))?;
    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(Clove2Error::new("set! expects symbol name"));
    };
    let value_expr = items
        .get(2)
        .ok_or_else(|| Clove2Error::new("set! expects value"))?;
    let value = lower_expr(value_expr)?;
    Ok(AstExpr::SetVar {
        name: name.clone(),
        value: Box::new(value),
    })
}

fn parse_params(expr: &Expr) -> Result<Vec<Param>, Clove2Error> {
    let ExprKind::Vector(items) = &expr.kind else {
        return Err(Clove2Error::new("params must be a vector"));
    };
    let mut out = Vec::new();
    let mut idx = 0;
    let mut rest_seen = false;
    while idx < items.len() {
        if let ExprKind::Symbol(sym) = &items[idx].kind {
            if sym == "&" {
                if rest_seen {
                    return Err(Clove2Error::new("only one rest parameter is allowed"));
                }
                rest_seen = true;
                idx += 1;
                if idx >= items.len() {
                    return Err(Clove2Error::new("rest parameter is missing name"));
                }
                let (name, ty, consumed) = parse_typed_name(items, idx)?;
                if idx + consumed != items.len() {
                    return Err(Clove2Error::new("rest parameter must be last"));
                }
                out.push(Param {
                    name,
                    ty,
                    rest: true,
                });
                idx += consumed;
                continue;
            }
        }
        let (name, ty, consumed) = parse_typed_name(items, idx)?;
        out.push(Param {
            name,
            ty,
            rest: false,
        });
        idx += consumed;
    }
    Ok(out)
}

fn parse_bindings(expr: &Expr) -> Result<Vec<Binding>, Clove2Error> {
    let ExprKind::Vector(items) = &expr.kind else {
        return Err(Clove2Error::new("bindings must be a vector"));
    };
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        let (name, ty, consumed) = parse_typed_name(items, idx)?;
        idx += consumed;
        let value = items
            .get(idx)
            .ok_or_else(|| Clove2Error::new("binding missing value"))?;
        let value = lower_expr(value)?;
        idx += 1;
        out.push(Binding { name, ty, value });
    }
    Ok(out)
}

fn parse_typed_name(
    items: &[Expr],
    start: usize,
) -> Result<(String, Option<Type>, usize), Clove2Error> {
    let name_expr = items
        .get(start)
        .ok_or_else(|| Clove2Error::new("missing name"))?;
    let ExprKind::Symbol(name) = &name_expr.kind else {
        return Err(Clove2Error::new("name must be a symbol"));
    };
    if name.ends_with(':') {
        let trimmed = name.trim_end_matches(':');
        if trimmed.is_empty() {
            return Err(Clove2Error::new("invalid typed name"));
        }
        let ty_expr = items
            .get(start + 1)
            .ok_or_else(|| Clove2Error::new("missing type after ':'"))?;
        let ty = parse_type_expr(ty_expr)?;
        return Ok((trimmed.to_string(), Some(ty), 2));
    }
    Ok((name.clone(), None, 1))
}

fn parse_type_expr(expr: &Expr) -> Result<Type, Clove2Error> {
    match &expr.kind {
        ExprKind::Symbol(sym) => Type::parse(sym),
        ExprKind::Keyword(sym) => Type::parse(sym),
        ExprKind::Vector(items) => {
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
        ExprKind::Map(entries) => {
            if entries.is_empty() {
                return Err(Clove2Error::new(
                    "map type expression expects at least 1 entry",
                ));
            }
            let mut fields: BTreeMap<String, Type> = BTreeMap::new();
            let mut all_keyword = true;
            let mut open = false;
            for (key, value) in entries {
                match &key.kind {
                    ExprKind::Keyword(name) => {
                        fields.insert(name.clone(), parse_type_expr(value)?);
                    }
                    ExprKind::Symbol(sym) if sym == ".." || sym == "..." => {
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

fn parse_symbol(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Symbol(sym) => Ok(sym.clone()),
        _ => Err(Clove2Error::new("expected symbol")),
    }
}

fn parse_field_name(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Keyword(sym) => Ok(sym.clone()),
        ExprKind::Symbol(sym) => Ok(sym.clone()),
        ExprKind::Literal(Literal::Str(value)) => Ok(value.clone()),
        _ => Err(Clove2Error::new("field name must be keyword or symbol")),
    }
}

fn parse_option_key(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Keyword(sym) => Ok(sym.clone()),
        ExprKind::Symbol(sym) => Ok(sym.clone()),
        _ => Err(Clove2Error::new("option key must be keyword or symbol")),
    }
}

fn apply_option(options: &mut ForeignOptions, key: &str, value: &Expr) -> Result<(), Clove2Error> {
    match key {
        "file" => {
            options.file = Some(parse_string(value)?);
        }
        "code" => {
            options.code = Some(parse_code(value)?);
        }
        "entry" => {
            options.entry = Some(parse_symbol_or_string(value)?);
        }
        "lang" => {
            options.lang = Some(parse_symbol_or_string(value)?);
        }
        "from" => {
            options.from = Some(parse_symbol_or_string(value)?);
        }
        "to" => {
            options.to = Some(parse_symbol_or_string(value)?);
        }
        _ => return Err(Clove2Error::new("unknown def-foreign option")),
    }
    Ok(())
}

fn parse_string(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Literal(Literal::Str(value)) => Ok(value.clone()),
        _ => Err(Clove2Error::new("expected string literal")),
    }
}

fn parse_symbol_or_string(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Symbol(sym) => Ok(sym.clone()),
        ExprKind::Keyword(sym) => Ok(sym.clone()),
        ExprKind::Literal(Literal::Str(value)) => Ok(value.clone()),
        _ => Err(Clove2Error::new("expected symbol, keyword, or string")),
    }
}

fn parse_code(expr: &Expr) -> Result<ForeignCode, Clove2Error> {
    match &expr.kind {
        ExprKind::Literal(Literal::Str(value)) => Ok(ForeignCode::String(value.clone())),
        ExprKind::ForeignBlock { tag, code } => Ok(ForeignCode::Block {
            tag: tag.clone(),
            code: code.clone(),
        }),
        _ => Err(Clove2Error::new("expected code string or foreign block")),
    }
}
