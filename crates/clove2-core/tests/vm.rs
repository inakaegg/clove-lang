use clove2_core::ast::{Literal, Span};
use clove2_core::syntax::{AstExpr, TopLevel};
use clove2_core::use_directive::MutMode;
use clove2_core::value::Value;
use clove2_core::vm::run_program_vm;

#[test]
fn vm_add_ints() {
    let expr = AstExpr::Call {
        callee: Box::new(AstExpr::Symbol("+".to_string())),
        args: vec![
            AstExpr::Literal(Literal::Int(1)),
            AstExpr::Literal(Literal::Int(2)),
            AstExpr::Literal(Literal::Int(3)),
        ],
    };
    let program = vec![TopLevel::Expr {
        expr,
        span: Span::default(),
    }];
    let out = run_program_vm(&program, MutMode::Mut).expect("vm run should succeed");
    assert_eq!(out, Value::Int(6));
}

#[test]
fn vm_let_if() {
    let expr = AstExpr::Let {
        bindings: vec![clove2_core::syntax::Binding {
            name: "a".to_string(),
            ty: None,
            value: AstExpr::Literal(Literal::Int(10)),
        }],
        body: vec![AstExpr::If {
            cond: Box::new(AstExpr::Call {
                callee: Box::new(AstExpr::Symbol(">".to_string())),
                args: vec![
                    AstExpr::Symbol("a".to_string()),
                    AstExpr::Literal(Literal::Int(5)),
                ],
            }),
            then_expr: Box::new(AstExpr::Literal(Literal::Int(1))),
            else_expr: Some(Box::new(AstExpr::Literal(Literal::Int(0)))),
        }],
    };
    let program = vec![TopLevel::Expr {
        expr,
        span: Span::default(),
    }];
    let out = run_program_vm(&program, MutMode::Mut).expect("vm run should succeed");
    assert_eq!(out, Value::Int(1));
}

#[test]
fn vm_map_reduce() {
    let expr = AstExpr::Call {
        callee: Box::new(AstExpr::Symbol("reduce".to_string())),
        args: vec![
            AstExpr::Symbol("+".to_string()),
            AstExpr::Literal(Literal::Int(0)),
            AstExpr::Call {
                callee: Box::new(AstExpr::Symbol("map".to_string())),
                args: vec![
                    AstExpr::Symbol("inc".to_string()),
                    AstExpr::Call {
                        callee: Box::new(AstExpr::Symbol("range".to_string())),
                        args: vec![AstExpr::Literal(Literal::Int(5))],
                    },
                ],
            },
        ],
    };
    let program = vec![TopLevel::Expr {
        expr,
        span: Span::default(),
    }];
    let out = run_program_vm(&program, MutMode::Mut).expect("vm run should succeed");
    assert_eq!(out, Value::Int(15));
}
