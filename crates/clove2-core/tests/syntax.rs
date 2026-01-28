use std::collections::BTreeMap;

use clove2_core::ast::{Expr, ExprKind, Literal};
use clove2_core::reader::read_all;
use clove2_core::syntax::{
    parse_forms, AstExpr, Binding, ForeignCode, ForeignDecl, ForeignOptions, Param, TopLevel,
};
use clove2_core::types::Type;

fn list_symbols(expr: &Expr) -> Vec<String> {
    match &expr.kind {
        ExprKind::List(items) => items
            .iter()
            .map(|item| match &item.kind {
                ExprKind::Symbol(sym) => sym.clone(),
                _ => "<non-symbol>".to_string(),
            })
            .collect(),
        _ => Vec::new(),
    }
}

#[test]
fn parse_def_and_defn() {
    let forms = read_all("(def x: Int 1)\n(defn add [a: Int b: Int] -> Int (+ a b))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "x".to_string(),
            ty: Some(Type::Int),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
    assert_eq!(
        ast[1],
        TopLevel::Defn {
            name: "add".to_string(),
            params: vec![
                Param {
                    name: "a".to_string(),
                    ty: Some(Type::Int),
                    rest: false,
                },
                Param {
                    name: "b".to_string(),
                    ty: Some(Type::Int),
                    rest: false,
                },
            ],
            ret: Some(Type::Int),
            body: vec![AstExpr::Call {
                callee: Box::new(AstExpr::Symbol("+".to_string())),
                args: vec![
                    AstExpr::Symbol("a".to_string()),
                    AstExpr::Symbol("b".to_string()),
                ],
            }],
            span: Default::default(),
        }
    );
}

#[test]
fn parse_def_aliases() {
    let forms = read_all("(-def x 1)\n(defn- add [a: Int] -> Int (+ a 1))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "x".to_string(),
            ty: None,
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
    assert_eq!(
        ast[1],
        TopLevel::Defn {
            name: "add".to_string(),
            params: vec![Param {
                name: "a".to_string(),
                ty: Some(Type::Int),
                rest: false,
            }],
            ret: Some(Type::Int),
            body: vec![AstExpr::Call {
                callee: Box::new(AstExpr::Symbol("+".to_string())),
                args: vec![
                    AstExpr::Symbol("a".to_string()),
                    AstExpr::Literal(Literal::Int(1)),
                ],
            }],
            span: Default::default(),
        }
    );
}

#[test]
fn read_call_sugar_and_compose_sugar() {
    let forms = read_all("str(\"a\", \"b\")\n(. inc str name)\n(inc str name .)").unwrap();
    match &forms[0].kind {
        ExprKind::List(items) => {
            assert!(matches!(&items[0].kind, ExprKind::Symbol(sym) if sym == "str"));
            assert!(matches!(&items[1].kind, ExprKind::Literal(Literal::Str(_))));
            assert!(matches!(&items[2].kind, ExprKind::Literal(Literal::Str(_))));
        }
        _ => panic!("call sugar should read as list"),
    }
    let head_dot_syms = list_symbols(&forms[1]);
    assert_eq!(head_dot_syms, vec!["comp", "name", "str", "inc"]);
    let tail_dot_syms = list_symbols(&forms[2]);
    assert_eq!(tail_dot_syms, vec!["comp", "inc", "str", "name"]);
}

#[test]
fn parse_vector_type_sugar() {
    let forms = read_all("(def xs: [Int] 1)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "xs".to_string(),
            ty: Some(Type::Vec(Box::new(Type::Int))),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
}

#[test]
fn parse_tuple_and_shape_types() {
    let forms = read_all("(def pair: [Int Str] 1)\n(def cfg: {:port Int :host Str} 1)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "pair".to_string(),
            ty: Some(Type::Tuple(vec![Type::Int, Type::Str])),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
    let mut fields = BTreeMap::new();
    fields.insert("port".to_string(), Type::Int);
    fields.insert("host".to_string(), Type::Str);
    assert_eq!(
        ast[1],
        TopLevel::Def {
            name: "cfg".to_string(),
            ty: Some(Type::shape(fields)),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
}

#[test]
fn parse_open_shape_type() {
    let forms = read_all("(def cfg: {:port Int ..} 1)\n(def cfg2: {:port Int ...} 1)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let mut fields = BTreeMap::new();
    fields.insert("port".to_string(), Type::Int);
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "cfg".to_string(),
            ty: Some(Type::open_shape(fields.clone())),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
    assert_eq!(
        ast[1],
        TopLevel::Def {
            name: "cfg2".to_string(),
            ty: Some(Type::open_shape(fields)),
            value: AstExpr::Literal(Literal::Int(1)),
            span: Default::default(),
        }
    );
}

#[test]
fn parse_let_and_if() {
    let forms = read_all("(let [x: Int 1 y 2] (if true x y))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Expr {
            expr: AstExpr::Let {
                bindings: vec![
                    Binding {
                        name: "x".to_string(),
                        ty: Some(Type::Int),
                        value: AstExpr::Literal(Literal::Int(1)),
                    },
                    Binding {
                        name: "y".to_string(),
                        ty: None,
                        value: AstExpr::Literal(Literal::Int(2)),
                    },
                ],
                body: vec![AstExpr::If {
                    cond: Box::new(AstExpr::Literal(Literal::Bool(true))),
                    then_expr: Box::new(AstExpr::Symbol("x".to_string())),
                    else_expr: Some(Box::new(AstExpr::Symbol("y".to_string()))),
                }],
            },
            span: Default::default(),
        }
    );
}

#[test]
fn parse_expr_type_annotation() {
    let forms = read_all("(def total (+ 1 2): Int)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::Def {
            name: "total".to_string(),
            ty: None,
            value: AstExpr::Call {
                callee: Box::new(AstExpr::Symbol("as".to_string())),
                args: vec![
                    AstExpr::Symbol("Int".to_string()),
                    AstExpr::Call {
                        callee: Box::new(AstExpr::Symbol("+".to_string())),
                        args: vec![
                            AstExpr::Literal(Literal::Int(1)),
                            AstExpr::Literal(Literal::Int(2)),
                        ],
                    },
                ],
            },
            span: Default::default(),
        }
    );
}

#[test]
fn parse_def_foreign() {
    let forms = read_all(
        "(def-foreign sha1\n  :file \"ruby/crypto.rb\"\n  :entry \"Crypto.sha1\"\n  [s: Str] -> Str)",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::DefForeign {
            decl: ForeignDecl {
                name: "sha1".to_string(),
                params: vec![Param {
                    name: "s".to_string(),
                    ty: Some(Type::Str),
                    rest: false,
                }],
                ret: Type::Str,
                options: ForeignOptions {
                    file: Some("ruby/crypto.rb".to_string()),
                    code: None,
                    entry: Some("Crypto.sha1".to_string()),
                    lang: None,
                    from: None,
                    to: None,
                },
            },
            span: Default::default(),
        }
    );
    let forms = read_all(
        "(def-foreign sha1\n  :code $rb{puts 1}\n  :entry Crypto.sha1\n  [s: Str] -> Str)",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::DefForeign {
            decl: ForeignDecl {
                name: "sha1".to_string(),
                params: vec![Param {
                    name: "s".to_string(),
                    ty: Some(Type::Str),
                    rest: false,
                }],
                ret: Type::Str,
                options: ForeignOptions {
                    file: None,
                    code: Some(ForeignCode::Block {
                        tag: "rb".to_string(),
                        code: "puts 1".to_string(),
                    }),
                    entry: Some("Crypto.sha1".to_string()),
                    lang: None,
                    from: None,
                    to: None,
                },
            },
            span: Default::default(),
        }
    );

    let forms = read_all("(def-foreign sha1 :file \"x.rb\" [s: Str] -> Str)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::DefForeign {
            decl: ForeignDecl {
                name: "sha1".to_string(),
                params: vec![Param {
                    name: "s".to_string(),
                    ty: Some(Type::Str),
                    rest: false,
                }],
                ret: Type::Str,
                options: ForeignOptions {
                    file: Some("x.rb".to_string()),
                    code: None,
                    entry: Some("sha1".to_string()),
                    lang: None,
                    from: None,
                    to: None,
                },
            },
            span: Default::default(),
        }
    );
}

#[test]
fn parse_deftype() {
    let forms = read_all("(deftype Config {:port Int :host Str})").unwrap();
    let ast = parse_forms(&forms).unwrap();
    match &ast[0] {
        TopLevel::DefType { name, fields, .. } => {
            assert_eq!(name, "Config");
            assert_eq!(fields.get("port"), Some(&Type::Int));
            assert_eq!(fields.get("host"), Some(&Type::Str));
        }
        other => panic!("expected deftype, got {:?}", other),
    }
}

#[test]
fn parse_fn() {
    let forms = read_all("(fn [x: Int] -> Int x)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    match &ast[0] {
        TopLevel::Expr {
            expr: AstExpr::Fn { params, ret, .. },
            ..
        } => {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].ty, Some(Type::Int));
            assert_eq!(ret.as_ref(), Some(&Type::Int));
        }
        other => panic!("expected fn, got {:?}", other),
    }
}

#[test]
fn def_foreign_requires_file_or_code() {
    let forms = read_all("(def-foreign sha1 [s: Str] -> Str)").unwrap();
    let err = parse_forms(&forms).unwrap_err();
    assert!(err
        .to_string()
        .contains("def-foreign expects :file or :code"));
}

#[test]
fn def_foreign_code_string_requires_lang() {
    let forms = read_all("(def-foreign sha1 :code \"puts 1\" [s: Str] -> Str)").unwrap();
    let err = parse_forms(&forms).unwrap_err();
    assert!(err
        .to_string()
        .contains("def-foreign :code string requires :lang"));
}

#[test]
fn def_foreign_code_string_with_lang() {
    let forms = read_all("(def-foreign sha1 :code \"puts 1\" :lang :rb [s: Str] -> Str)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    assert_eq!(
        ast[0],
        TopLevel::DefForeign {
            decl: ForeignDecl {
                name: "sha1".to_string(),
                params: vec![Param {
                    name: "s".to_string(),
                    ty: Some(Type::Str),
                    rest: false,
                }],
                ret: Type::Str,
                options: ForeignOptions {
                    file: None,
                    code: Some(ForeignCode::String("puts 1".to_string())),
                    entry: Some("sha1".to_string()),
                    lang: Some("rb".to_string()),
                    from: None,
                    to: None,
                },
            },
            span: Default::default(),
        }
    );
}

#[test]
fn parse_rest_param() {
    let forms = read_all("(defn sum [a: Int & rest: Vec<Int>] -> Int a)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    match &ast[0] {
        TopLevel::Defn { params, .. } => {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "a");
            assert_eq!(params[0].rest, false);
            assert_eq!(params[1].name, "rest");
            assert_eq!(params[1].rest, true);
            assert_eq!(params[1].ty, Some(Type::Vec(Box::new(Type::Int))));
        }
        other => panic!("expected defn, got {:?}", other),
    }
}

#[test]
fn def_foreign_rejects_rest_param() {
    let forms = read_all("(def-foreign sum :file \"x.rb\" [& rest: Vec<Int>] -> Int)").unwrap();
    let err = parse_forms(&forms).unwrap_err();
    assert!(err
        .to_string()
        .contains("def-foreign does not support rest parameters"));
}

#[test]
fn rest_param_must_be_last() {
    let forms = read_all("(defn f [a: Int & rest: Vec<Int> b: Int] -> Int a)").unwrap();
    let err = parse_forms(&forms).unwrap_err();
    assert!(err.to_string().contains("rest parameter must be last"));
}
