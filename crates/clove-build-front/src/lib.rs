use std::path::PathBuf;

use clove_build_core::ast::{Expr as SExpr, ExprKind, Literal};
use clove_build_core::reader::read_all;

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
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    Keyword(String),
    Map(Vec<(Expr, Expr)>),
    Vector(Vec<Expr>),
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
    let forms = read_all(&src.text).map_err(|err| FrontError {
        message: format!("read error: {}", err),
    })?;
    let top_levels = forms
        .iter()
        .map(lower_top_level)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(FrontProgram { top_levels })
}

fn lower_top_level(expr: &SExpr) -> Result<TopLevel, FrontError> {
    if let ExprKind::List(items) = &expr.kind {
        if let Some(head) = items.first() {
            if let ExprKind::Symbol(sym) = &head.kind {
                match sym.as_str() {
                    "def" => return lower_def(items),
                    "defn" => return lower_defn(items),
                    _ => {}
                }
            }
        }
    }
    Ok(TopLevel::Expr(lower_expr(expr)?))
}

fn lower_def(items: &[SExpr]) -> Result<TopLevel, FrontError> {
    if items.len() != 3 {
        return Err(FrontError {
            message: "def expects name and value".to_string(),
        });
    }
    let name = expect_symbol(&items[1], "def name")?;
    let value = lower_expr(&items[2])?;
    Ok(TopLevel::Def { name, value })
}

fn lower_defn(items: &[SExpr]) -> Result<TopLevel, FrontError> {
    if items.len() < 4 {
        return Err(FrontError {
            message: "defn expects name, params, and body".to_string(),
        });
    }
    let name = expect_symbol(&items[1], "defn name")?;
    let params = match &items[2].kind {
        ExprKind::Vector(v) => v,
        _ => {
            return Err(FrontError {
                message: "defn params must be vector".to_string(),
            });
        }
    };
    if !(1..=3).contains(&params.len()) {
        return Err(FrontError {
            message: "defn currently supports one to three params".to_string(),
        });
    }
    let param1 = expect_symbol(&params[0], "defn param1")?;
    let param2 = if params.len() >= 2 {
        Some(expect_symbol(&params[1], "defn param2")?)
    } else {
        None
    };
    let body_index = if items.len() >= 6 {
        if let ExprKind::Symbol(sym) = &items[3].kind {
            if sym == "->" {
                5
            } else {
                3
            }
        } else {
            3
        }
    } else {
        3
    };
    if body_index >= items.len() {
        return Err(FrontError {
            message: "defn missing body".to_string(),
        });
    }
    if items.len() != body_index + 1 {
        return Err(FrontError {
            message: "defn currently supports a single body form".to_string(),
        });
    }
    let body = lower_expr(&items[body_index])?;
    Ok(TopLevel::Def {
        name,
        value: make_lambda(params, param1, param2, body)?,
    })
}

fn lower_expr(expr: &SExpr) -> Result<Expr, FrontError> {
    match &expr.kind {
        ExprKind::Literal(Literal::Int(v)) => Ok(Expr::Int(*v)),
        ExprKind::Literal(Literal::Bool(v)) => Ok(Expr::Bool(*v)),
        ExprKind::Literal(Literal::Str(v)) => Ok(Expr::Str(v.clone())),
        ExprKind::Literal(other) => Err(FrontError {
            message: format!("unsupported literal in phase2 C front: {:?}", other),
        }),
        ExprKind::Symbol(sym) => Ok(Expr::Symbol(sym.clone())),
        ExprKind::Vector(items) => {
            let elems = items
                .iter()
                .map(lower_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::Vector(elems))
        }
        ExprKind::List(items) => lower_list(items),
        ExprKind::Keyword(name) => Ok(Expr::Keyword(name.clone())),
        ExprKind::Set(_) => Err(FrontError {
            message: "set is not supported yet".to_string(),
        }),
        ExprKind::Map(entries) => {
            let mut out = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                out.push((lower_expr(k)?, lower_expr(v)?));
            }
            Ok(Expr::Map(out))
        }
        ExprKind::ForeignBlock { .. } => Err(FrontError {
            message: "foreign block is not supported yet".to_string(),
        }),
    }
}

fn lower_list(items: &[SExpr]) -> Result<Expr, FrontError> {
    if items.is_empty() {
        return Err(FrontError {
            message: "empty list is not allowed".to_string(),
        });
    }
    let callee = expect_symbol(&items[0], "call head")?;
    if callee == "fn" {
        if items.len() != 3 {
            return Err(FrontError {
                message: "fn currently expects [x] body".to_string(),
            });
        }
        let params = match &items[1].kind {
            ExprKind::Vector(v) => v,
            _ => {
                return Err(FrontError {
                    message: "fn params must be vector".to_string(),
                });
            }
        };
        if !(1..=3).contains(&params.len()) {
            return Err(FrontError {
                message: "fn currently supports one to three params".to_string(),
            });
        }
        let param1 = expect_symbol(&params[0], "fn param1")?;
        let param2 = if params.len() >= 2 {
            Some(expect_symbol(&params[1], "fn param2")?)
        } else {
            None
        };
        let body = lower_expr(&items[2])?;
        return make_lambda(params, param1, param2, body);
    }

    let args = items[1..]
        .iter()
        .map(lower_expr)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::Call { callee, args })
}

fn expect_symbol(expr: &SExpr, what: &str) -> Result<String, FrontError> {
    if let ExprKind::Symbol(sym) = &expr.kind {
        Ok(sym.clone())
    } else {
        Err(FrontError {
            message: format!("{} must be symbol", what),
        })
    }
}

fn make_lambda(
    params: &[SExpr],
    param1: String,
    param2: Option<String>,
    body: Expr,
) -> Result<Expr, FrontError> {
    match params.len() {
        1 => Ok(Expr::Lambda1 {
            param: param1,
            body: Box::new(body),
        }),
        2 => Ok(Expr::Lambda2 {
            param1,
            param2: param2.expect("param2 exists when params.len() == 2"),
            body: Box::new(body),
        }),
        3 => Ok(Expr::Lambda3 {
            param1,
            param2: param2.expect("param2 exists when params.len() == 3"),
            param3: expect_symbol(&params[2], "lambda param3")?,
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
    fn reject_defn_with_multiple_body_forms_instead_of_dropping_them() {
        let err = parse_source(&src("(defn step [x] (+ x 1) (+ x 100))"))
            .expect_err("multi-form body must not compile with changed semantics");
        assert!(err
            .to_string()
            .contains("defn currently supports a single body form"));
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
}
