use std::path::PathBuf;

use clove_build_core::ast::Literal;
use clove_build_core::reader::read_all;
use clove_build_core::syntax::parse_forms;
use clove_build_core::typed_ir::{
    lower_program, Expr as IrExpr, ExprKind as IrExprKind, Program as IrProgram,
    TopLevel as IrTopLevel,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontProgram {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopLevel {
    Def { name: String, value: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Nil,
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    Keyword(String),
    Map(Vec<(Expr, Expr)>),
    Vector(Vec<Expr>),
    Do(Vec<Expr>),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Let {
        bindings: Vec<(String, Expr)>,
        body: Box<Expr>,
    },
    Lambda1 {
        param: String,
        body: Box<Expr>,
    },
    Lambda2 {
        param1: String,
        param2: String,
        body: Box<Expr>,
    },
    Lambda3 {
        param1: String,
        param2: String,
        param3: String,
        body: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontError {
    pub message: String,
}

impl std::fmt::Display for FrontError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for FrontError {}

pub fn parse_source(src: &SourceFile) -> Result<FrontProgram, FrontError> {
    let ir = parse_typed_ir_source(src)?;
    let top_levels = ir
        .top_levels
        .iter()
        .map(lower_top_level)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(FrontProgram { top_levels })
}

pub fn parse_typed_ir_source(src: &SourceFile) -> Result<IrProgram, FrontError> {
    let forms = read_all(&src.text).map_err(|err| FrontError {
        message: format!("read error: {}", err),
    })?;
    let syntax = parse_forms(&forms).map_err(|err| FrontError {
        message: format!("syntax error: {}", err),
    })?;
    lower_program(&syntax).map_err(|err| FrontError {
        message: format!("lower error: {}", err),
    })
}

fn lower_top_level(top: &IrTopLevel) -> Result<TopLevel, FrontError> {
    match top {
        IrTopLevel::Def { name, value, .. } => Ok(TopLevel::Def {
            name: name.clone(),
            value: lower_expr(value)?,
        }),
        IrTopLevel::FnDef {
            name, params, body, ..
        } => {
            let body = lower_expr(body)?;
            Ok(TopLevel::Def {
                name: name.clone(),
                value: make_lambda_from_names(
                    &params.iter().map(|p| p.name.clone()).collect::<Vec<_>>(),
                    body,
                )?,
            })
        }
        IrTopLevel::Expr { expr, .. } => Ok(TopLevel::Expr(lower_expr(expr)?)),
        IrTopLevel::DefType { .. } => Err(FrontError {
            message: "deftype is not supported in phase2 C front yet".to_string(),
        }),
        IrTopLevel::DefForeign { .. } => Err(FrontError {
            message: "def-foreign is not supported in phase2 C front yet".to_string(),
        }),
    }
}

fn lower_expr(expr: &IrExpr) -> Result<Expr, FrontError> {
    match &expr.kind {
        IrExprKind::Const(Literal::Nil) => Ok(Expr::Nil),
        IrExprKind::Const(Literal::Int(v)) => Ok(Expr::Int(*v)),
        IrExprKind::Const(Literal::Bool(v)) => Ok(Expr::Bool(*v)),
        IrExprKind::Const(Literal::Str(v)) => Ok(Expr::Str(v.clone())),
        IrExprKind::Const(other) => Err(FrontError {
            message: format!("unsupported literal in phase2 C front: {:?}", other),
        }),
        IrExprKind::Var(sym) => Ok(Expr::Symbol(sym.clone())),
        IrExprKind::Keyword(name) => Ok(Expr::Keyword(name.clone())),
        IrExprKind::Do(items) => Ok(Expr::Do(
            items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        IrExprKind::VectorLit(items) => Ok(Expr::Vector(
            items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        IrExprKind::MapLit(entries) => Ok(Expr::Map(
            entries
                .iter()
                .map(|(k, v)| Ok((lower_expr(k)?, lower_expr(v)?)))
                .collect::<Result<Vec<_>, FrontError>>()?,
        )),
        IrExprKind::Lambda { params, body, .. } => {
            let body = lower_expr(body)?;
            let names = params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
            make_lambda_from_names(&names, body)
        }
        IrExprKind::If {
            cond,
            then_expr,
            else_expr,
        } => Ok(Expr::If {
            cond: Box::new(lower_expr(cond)?),
            then_expr: Box::new(lower_expr(then_expr)?),
            else_expr: Box::new(lower_expr(else_expr)?),
        }),
        IrExprKind::Let { bindings, body } => Ok(Expr::Let {
            bindings: bindings
                .iter()
                .map(|binding| Ok((binding.name.clone(), lower_expr(&binding.value)?)))
                .collect::<Result<Vec<_>, FrontError>>()?,
            body: Box::new(lower_expr(body)?),
        }),
        IrExprKind::BuiltinCall { name, args } => {
            let args = args.iter().map(lower_expr).collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::Call {
                callee: name.clone(),
                args,
            })
        }
        IrExprKind::Call { callee, args } => {
            let callee = lower_symbol_callee(callee)?;
            let args = args.iter().map(lower_expr).collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::Call { callee, args })
        }
    }
}

fn lower_symbol_callee(callee: &IrExpr) -> Result<String, FrontError> {
    match &callee.kind {
        IrExprKind::Var(name) => Ok(name.clone()),
        IrExprKind::BuiltinCall { name, .. } => Ok(name.clone()),
        _ => Err(FrontError {
            message: "non-symbol callee is not supported in phase2 C front yet".to_string(),
        }),
    }
}

fn make_lambda_from_names(params: &[String], body: Expr) -> Result<Expr, FrontError> {
    match params.len() {
        1 => Ok(Expr::Lambda1 {
            param: params[0].clone(),
            body: Box::new(body),
        }),
        2 => Ok(Expr::Lambda2 {
            param1: params[0].clone(),
            param2: params[1].clone(),
            body: Box::new(body),
        }),
        3 => Ok(Expr::Lambda3 {
            param1: params[0].clone(),
            param2: params[1].clone(),
            param3: params[2].clone(),
            body: Box::new(body),
        }),
        _ => Err(FrontError {
            message: "lambda currently supports one to three params".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_source, Expr, SourceFile, TopLevel};
    use std::path::PathBuf;

    fn src(text: &str) -> SourceFile {
        SourceFile {
            path: PathBuf::from("x.clv"),
            text: text.to_string(),
        }
    }

    #[test]
    fn parse_pipeline_def() {
        let program = parse_source(&src("(def total (reduce + 0 (map inc (range 0 10))))"))
            .expect("parse should succeed");
        assert_eq!(program.top_levels.len(), 1);
        let TopLevel::Def { name, value } = &program.top_levels[0] else {
            panic!("expected def");
        };
        assert_eq!(name, "total");
        let Expr::Call { callee, .. } = value else {
            panic!("expected call");
        };
        assert_eq!(callee, "reduce");
    }

    #[test]
    fn parse_defn_as_lambda() {
        let program = parse_source(&src("(defn step [x] (+ x 1))")).expect("parse should succeed");
        let TopLevel::Def { name, value } = &program.top_levels[0] else {
            panic!("expected def");
        };
        assert_eq!(name, "step");
        let Expr::Lambda1 { param, .. } = value else {
            panic!("expected lambda");
        };
        assert_eq!(param, "x");
    }

    #[test]
    fn parse_defn_two_params() {
        let program =
            parse_source(&src("(defn step2 [i x] (+ i x))")).expect("parse should succeed");
        let TopLevel::Def { name, value } = &program.top_levels[0] else {
            panic!("expected def");
        };
        assert_eq!(name, "step2");
        let Expr::Lambda2 { param1, param2, .. } = value else {
            panic!("expected lambda2");
        };
        assert_eq!(param1, "i");
        assert_eq!(param2, "x");
    }

    #[test]
    fn parse_defn_three_params() {
        let program =
            parse_source(&src("(defn rf [acc k v] (+ acc v))")).expect("parse should succeed");
        let TopLevel::Def { name, value } = &program.top_levels[0] else {
            panic!("expected def");
        };
        assert_eq!(name, "rf");
        let Expr::Lambda3 {
            param1,
            param2,
            param3,
            ..
        } = value
        else {
            panic!("expected lambda3");
        };
        assert_eq!(param1, "acc");
        assert_eq!(param2, "k");
        assert_eq!(param3, "v");
    }

    #[test]
    fn parse_map_and_keyword() {
        let program = parse_source(&src("(def m {:a 1 :b 2})")).expect("parse should succeed");
        let TopLevel::Def { value, .. } = &program.top_levels[0] else {
            panic!("expected def");
        };
        let Expr::Map(entries) = value else {
            panic!("expected map");
        };
        assert_eq!(entries.len(), 2);
        assert!(matches!(&entries[0].0, Expr::Keyword(k) if k == "a"));
        assert!(matches!(&entries[1].0, Expr::Keyword(k) if k == "b"));
    }

    #[test]
    fn parse_let_as_front_let() {
        let program = parse_source(&src("(let [x 1 y 2] (+ x y))")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Let { bindings, body }) = &program.top_levels[0] else {
            panic!("expected let expression");
        };
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].0, "x");
        assert_eq!(bindings[1].0, "y");
        assert!(matches!(bindings[0].1, Expr::Int(1)));
        assert!(matches!(bindings[1].1, Expr::Int(2)));
        assert!(matches!(body.as_ref(), Expr::Call { callee, .. } if callee == "+"));
    }

    #[test]
    fn parse_if_not_via_syntax_rewrite() {
        let program = parse_source(&src("(if-not false 10 20)")).expect("parse should succeed");
        let TopLevel::Expr(Expr::If {
            cond,
            then_expr,
            else_expr,
        }) = &program.top_levels[0]
        else {
            panic!("expected if expression");
        };
        assert!(matches!(cond.as_ref(), Expr::Call { callee, .. } if callee == "not"));
        assert!(matches!(then_expr.as_ref(), Expr::Int(10)));
        assert!(matches!(else_expr.as_ref(), Expr::Int(20)));
    }

    #[test]
    fn parse_defn_multi_body_as_do() {
        let program =
            parse_source(&src("(defn f [x] (println x) (+ x 1))")).expect("parse should succeed");
        let TopLevel::Def { value, .. } = &program.top_levels[0] else {
            panic!("expected def");
        };
        let Expr::Lambda1 { body, .. } = value else {
            panic!("expected lambda1");
        };
        let Expr::Do(items) = body.as_ref() else {
            panic!("expected do body");
        };
        assert_eq!(items.len(), 2);
        assert!(matches!(&items[0], Expr::Call { callee, .. } if callee == "println"));
        assert!(matches!(&items[1], Expr::Call { callee, .. } if callee == "+"));
    }

    #[test]
    fn parse_when_let_rewrite_contains_do() {
        let program =
            parse_source(&src("(when-let [x 2] (println x) x)")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Let { body, .. }) = &program.top_levels[0] else {
            panic!("expected outer let");
        };
        let Expr::If { then_expr, .. } = body.as_ref() else {
            panic!("expected if");
        };
        let Expr::Let { body, .. } = then_expr.as_ref() else {
            panic!("expected inner let");
        };
        let Expr::Do(items) = body.as_ref() else {
            panic!("expected do");
        };
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn parse_dotimes_rewrite_uses_run() {
        let program =
            parse_source(&src("(dotimes [i 3] (println i))")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Call { callee, .. }) = &program.top_levels[0] else {
            panic!("expected run! call");
        };
        assert_eq!(callee, "run!");
    }

    #[test]
    fn parse_each_rewrite_returns_coll_after_run() {
        let program =
            parse_source(&src("(each [x [1 2]] (println x))")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Let { body, .. }) = &program.top_levels[0] else {
            panic!("expected let");
        };
        let Expr::Do(items) = body.as_ref() else {
            panic!("expected do");
        };
        assert!(matches!(&items[0], Expr::Call { callee, .. } if callee == "run!"));
    }

    #[test]
    fn parse_direct_comp_call_rewrite() {
        let program = parse_source(&src("((comp inc inc) 1)")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Call { callee, args }) = &program.top_levels[0] else {
            panic!("expected call");
        };
        assert_eq!(callee, "__comp-call");
        assert!(matches!(
            args.as_slice(),
            [Expr::Vector(funcs), Expr::Vector(call_args)]
                if funcs.len() == 2 && call_args == &[Expr::Int(1)]
        ));
    }

    #[test]
    fn parse_direct_juxt_call_rewrite() {
        let program = parse_source(&src("((juxt inc dec) 10)")).expect("parse should succeed");
        let TopLevel::Expr(Expr::Call { callee, args }) = &program.top_levels[0] else {
            panic!("expected call");
        };
        assert_eq!(callee, "__juxt-call");
        assert!(matches!(
            args.as_slice(),
            [Expr::Vector(funcs), Expr::Vector(call_args)]
                if funcs.len() == 2 && call_args == &[Expr::Int(10)]
        ));
    }
}
