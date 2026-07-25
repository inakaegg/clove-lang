use crate::ast::{Literal, Span};
use crate::builtins::is_builtin;
use crate::error::Clove2Error;
use crate::syntax::{
    AstExpr, Binding as SynBinding, ForeignDecl, Param as SynParam, TopLevel as SynTopLevel,
};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TopLevel {
    Def {
        name: String,
        ty: Option<Type>,
        value: Expr,
        span: Span,
    },
    FnDef {
        name: String,
        params: Vec<Param>,
        ret: Option<Type>,
        body: Expr,
        span: Span,
    },
    DefType {
        name: String,
        fields: Vec<(String, Type)>,
        span: Span,
    },
    DefForeign {
        decl: ForeignDecl,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Option<Type>,
    pub rest: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Effect {
    Pure,
    Alloc,
    IO,
    MayThrow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ownership {
    Borrowed,
    Owned,
    Unique,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Mutability {
    Imut,
    Mut,
    MutHard,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LoweringMode {
    NativePreferred,
    DynamicFallbackRequired,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
    pub span: Span,
    pub effect: Effect,
    pub ownership: Ownership,
    pub mutability: Mutability,
    pub lowering: LoweringMode,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Const(Literal),
    Var(String),
    Keyword(String),
    VectorLit(Vec<Expr>),
    MapLit(Vec<(Expr, Expr)>),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Let {
        bindings: Vec<Binding>,
        body: Box<Expr>,
    },
    Do(Vec<Expr>),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    BuiltinCall {
        name: String,
        args: Vec<Expr>,
    },
    Lambda {
        params: Vec<Param>,
        ret: Option<Type>,
        body: Box<Expr>,
    },
}

pub fn lower_program(top_levels: &[SynTopLevel]) -> Result<Program, Clove2Error> {
    Ok(Program {
        top_levels: top_levels
            .iter()
            .map(lower_top_level)
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn lower_top_level(top: &SynTopLevel) -> Result<TopLevel, Clove2Error> {
    match top {
        SynTopLevel::Def {
            name,
            ty,
            value,
            span,
        } => Ok(TopLevel::Def {
            name: name.clone(),
            ty: ty.clone(),
            value: lower_expr(value)?,
            span: span.clone(),
        }),
        SynTopLevel::Defn {
            name,
            params,
            ret,
            body,
            span,
        } => Ok(TopLevel::FnDef {
            name: name.clone(),
            params: params.iter().map(lower_param).collect(),
            ret: ret.clone(),
            body: lower_body(body)?,
            span: span.clone(),
        }),
        SynTopLevel::DefType { name, fields, span } => Ok(TopLevel::DefType {
            name: name.clone(),
            fields: fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            span: span.clone(),
        }),
        SynTopLevel::DefForeign { decl, span } => Ok(TopLevel::DefForeign {
            decl: decl.clone(),
            span: span.clone(),
        }),
        SynTopLevel::Expr { expr, span } => Ok(TopLevel::Expr {
            expr: lower_expr(expr)?,
            span: span.clone(),
        }),
    }
}

fn lower_expr(expr: &AstExpr) -> Result<Expr, Clove2Error> {
    let kind = match expr {
        AstExpr::Literal(lit) => ExprKind::Const(lit.clone()),
        AstExpr::Symbol(name) => ExprKind::Var(name.clone()),
        AstExpr::Keyword(name) => ExprKind::Keyword(name.clone()),
        AstExpr::Do(items) => ExprKind::Do(
            items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        AstExpr::Vector(items) => ExprKind::VectorLit(
            items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        AstExpr::Map(entries) => ExprKind::MapLit(
            entries
                .iter()
                .map(|(k, v)| Ok((lower_expr(k)?, lower_expr(v)?)))
                .collect::<Result<Vec<_>, Clove2Error>>()?,
        ),
        AstExpr::Fn { params, ret, body } => ExprKind::Lambda {
            params: params.iter().map(lower_param).collect(),
            ret: ret.clone(),
            body: Box::new(lower_body(body)?),
        },
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => ExprKind::If {
            cond: Box::new(lower_expr(cond)?),
            then_expr: Box::new(lower_expr(then_expr)?),
            else_expr: Box::new(match else_expr {
                Some(expr) => lower_expr(expr)?,
                None => Expr::new(ExprKind::Const(Literal::Nil), Span::default()),
            }),
        },
        AstExpr::Let { bindings, body } => ExprKind::Let {
            bindings: bindings
                .iter()
                .map(lower_binding)
                .collect::<Result<Vec<_>, _>>()?,
            body: Box::new(lower_body(body)?),
        },
        AstExpr::Call { callee, args } => {
            let callee = lower_expr(callee)?;
            let args = args.iter().map(lower_expr).collect::<Result<Vec<_>, _>>()?;
            match &callee.kind {
                ExprKind::Var(name) if is_builtin(name) => ExprKind::BuiltinCall {
                    name: name.clone(),
                    args,
                },
                _ => ExprKind::Call {
                    callee: Box::new(callee),
                    args,
                },
            }
        }
        AstExpr::Set(_) => return Err(Clove2Error::new("set is not supported in typed IR yet")),
        AstExpr::Quote(_) => {
            return Err(Clove2Error::new("quote is not supported in typed IR yet"))
        }
        AstExpr::ForeignBlock { .. } => {
            return Err(Clove2Error::new(
                "foreign block is not supported in typed IR yet",
            ))
        }
        AstExpr::SetVar { .. } => {
            return Err(Clove2Error::new("set! is not supported in typed IR yet"))
        }
    };
    Ok(Expr::from_kind(kind, expr_span(expr)))
}

fn lower_body(body: &[AstExpr]) -> Result<Expr, Clove2Error> {
    let items = body.iter().map(lower_expr).collect::<Result<Vec<_>, _>>()?;
    match items.len() {
        0 => Err(Clove2Error::new("body must not be empty")),
        1 => Ok(items.into_iter().next().expect("single item exists")),
        _ => Ok(Expr::from_kind(ExprKind::Do(items), Span::default())),
    }
}

fn lower_binding(binding: &SynBinding) -> Result<Binding, Clove2Error> {
    Ok(Binding {
        name: binding.name.clone(),
        ty: binding.ty.clone(),
        value: lower_expr(&binding.value)?,
    })
}

fn lower_param(param: &SynParam) -> Param {
    Param {
        name: param.name.clone(),
        ty: param.ty.clone(),
        rest: param.rest,
    }
}

fn expr_span(expr: &AstExpr) -> Span {
    match expr {
        AstExpr::Literal(_) => Span::default(),
        AstExpr::Symbol(_) => Span::default(),
        AstExpr::Keyword(_) => Span::default(),
        AstExpr::Do(_) => Span::default(),
        AstExpr::Vector(_) => Span::default(),
        AstExpr::Set(_) => Span::default(),
        AstExpr::Map(_) => Span::default(),
        AstExpr::Quote(expr) => expr.span.clone(),
        AstExpr::ForeignBlock { .. } => Span::default(),
        AstExpr::Fn { .. } => Span::default(),
        AstExpr::If { .. } => Span::default(),
        AstExpr::Let { .. } => Span::default(),
        AstExpr::SetVar { .. } => Span::default(),
        AstExpr::Call { .. } => Span::default(),
    }
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self::from_kind(kind, span)
    }

    pub fn from_kind(kind: ExprKind, span: Span) -> Self {
        let ty = infer_shallow_type(&kind);
        let effect = infer_effect(&kind);
        let ownership = infer_ownership(&kind);
        let lowering = infer_lowering(&kind);
        Self {
            kind,
            ty,
            span,
            effect,
            ownership,
            mutability: Mutability::Mut,
            lowering,
        }
    }
}

fn infer_shallow_type(kind: &ExprKind) -> Option<Type> {
    match kind {
        ExprKind::Const(Literal::Nil) => Some(Type::Nil),
        ExprKind::Const(Literal::Bool(_)) => Some(Type::Bool),
        ExprKind::Const(Literal::Int(_)) => Some(Type::Int),
        ExprKind::Const(Literal::Float(_)) => Some(Type::Float),
        ExprKind::Const(Literal::Str(_)) => Some(Type::Str),
        ExprKind::Const(Literal::Regex(_)) => Some(Type::Named("Regex".to_string())),
        ExprKind::Keyword(_) => Some(Type::Keyword),
        ExprKind::VectorLit(items) => {
            let mut item_tys = items.iter().filter_map(|item| item.ty.clone());
            let first = item_tys.next()?;
            if item_tys.all(|ty| ty == first) {
                Some(Type::Vec(Box::new(first)))
            } else {
                None
            }
        }
        ExprKind::Lambda { params, ret, .. } => Some(Type::Function {
            params: params
                .iter()
                .map(|param| param.ty.clone().unwrap_or(Type::Any))
                .collect(),
            rest: params
                .iter()
                .find(|param| param.rest)
                .and_then(|param| param.ty.clone())
                .map(Box::new),
            ret: Box::new(ret.clone().unwrap_or(Type::Any)),
        }),
        _ => None,
    }
}

fn infer_effect(kind: &ExprKind) -> Effect {
    match kind {
        ExprKind::Const(_) | ExprKind::Var(_) | ExprKind::Keyword(_) | ExprKind::Lambda { .. } => {
            Effect::Pure
        }
        ExprKind::VectorLit(items) => {
            combine_effects(items.iter().map(|expr| &expr.effect), Effect::Alloc)
        }
        ExprKind::MapLit(entries) => combine_effects(
            entries.iter().flat_map(|(k, v)| [&k.effect, &v.effect]),
            Effect::Alloc,
        ),
        ExprKind::If {
            cond,
            then_expr,
            else_expr,
        } => combine_effects(
            [&cond.effect, &then_expr.effect, &else_expr.effect].into_iter(),
            Effect::Pure,
        ),
        ExprKind::Let { bindings, body } => combine_effects(
            bindings
                .iter()
                .map(|binding| &binding.value.effect)
                .chain(std::iter::once(&body.effect)),
            Effect::Pure,
        ),
        ExprKind::Do(items) => combine_effects(items.iter().map(|expr| &expr.effect), Effect::Pure),
        ExprKind::BuiltinCall { name, args } => {
            let base = builtin_effect(name);
            combine_effects(args.iter().map(|expr| &expr.effect), base)
        }
        ExprKind::Call { callee, args } => combine_effects(
            std::iter::once(&callee.effect).chain(args.iter().map(|expr| &expr.effect)),
            Effect::MayThrow,
        ),
    }
}

fn infer_ownership(kind: &ExprKind) -> Ownership {
    match kind {
        ExprKind::Const(Literal::Nil)
        | ExprKind::Const(Literal::Bool(_))
        | ExprKind::Const(Literal::Int(_))
        | ExprKind::Const(Literal::Float(_))
        | ExprKind::Keyword(_)
        | ExprKind::Var(_) => Ownership::Borrowed,
        ExprKind::Const(Literal::Str(_))
        | ExprKind::Const(Literal::Regex(_))
        | ExprKind::VectorLit(_)
        | ExprKind::MapLit(_)
        | ExprKind::Lambda { .. } => Ownership::Owned,
        ExprKind::If {
            then_expr,
            else_expr,
            ..
        } => {
            if then_expr.ownership == Ownership::Unique && else_expr.ownership == Ownership::Unique
            {
                Ownership::Unique
            } else if then_expr.ownership == Ownership::Owned
                || else_expr.ownership == Ownership::Owned
            {
                Ownership::Owned
            } else {
                Ownership::Borrowed
            }
        }
        ExprKind::Let { .. }
        | ExprKind::Do(_)
        | ExprKind::BuiltinCall { .. }
        | ExprKind::Call { .. } => body_ownership(kind),
    }
}

fn body_ownership(kind: &ExprKind) -> Ownership {
    match kind {
        ExprKind::Let { body, .. } => body.ownership.clone(),
        ExprKind::Do(items) => items
            .last()
            .map(|expr| expr.ownership.clone())
            .unwrap_or(Ownership::Borrowed),
        ExprKind::BuiltinCall { .. } | ExprKind::Call { .. } => Ownership::Owned,
        _ => Ownership::Borrowed,
    }
}

fn infer_lowering(kind: &ExprKind) -> LoweringMode {
    match kind {
        ExprKind::Call { .. } => LoweringMode::DynamicFallbackRequired,
        ExprKind::BuiltinCall { .. }
        | ExprKind::Const(_)
        | ExprKind::Var(_)
        | ExprKind::Keyword(_)
        | ExprKind::VectorLit(_)
        | ExprKind::MapLit(_)
        | ExprKind::If { .. }
        | ExprKind::Let { .. }
        | ExprKind::Do(_)
        | ExprKind::Lambda { .. } => LoweringMode::NativePreferred,
    }
}

fn builtin_effect(name: &str) -> Effect {
    match name {
        "println" | "print" | "prn" | "slurp" | "spit" | "repl" | "debug" | "break" => Effect::IO,
        "runtime-error" | "expect" => Effect::MayThrow,
        "range" | "map" | "filter" | "keep" | "reduce" | "assoc" | "assoc-in" | "update"
        | "update-in" | "conj" | "vector" | "hash-map" | "list" | "sort" | "sort-by" | "split"
        | "join" | "replace" | "replace-first" | "shuffle" | "partition" | "partition-all"
        | "partition-by" | "repeat" | "repeatedly" | "merge" | "merge-with" => Effect::Alloc,
        _ => Effect::Pure,
    }
}

fn combine_effects<'a>(effects: impl Iterator<Item = &'a Effect>, base: Effect) -> Effect {
    effects.fold(base, |acc, effect| effect_max(acc, effect.clone()))
}

fn effect_max(a: Effect, b: Effect) -> Effect {
    use Effect::{Alloc, MayThrow, Pure, IO};
    match (a, b) {
        (MayThrow, _) | (_, MayThrow) => MayThrow,
        (IO, _) | (_, IO) => IO,
        (Alloc, _) | (_, Alloc) => Alloc,
        _ => Pure,
    }
}
