use clove_build_core::reader::read_all;
use clove_build_core::syntax::parse_forms;
use clove_build_core::typed_ir::{lower_program, Effect, ExprKind, LoweringMode, TopLevel};

fn lower(text: &str) -> clove_build_core::typed_ir::Program {
    let forms = read_all(text).expect("read should succeed");
    let syntax = parse_forms(&forms).expect("syntax should succeed");
    lower_program(&syntax).expect("typed ir lowering should succeed")
}

#[test]
fn lower_if_not_rewrite_into_if_and_builtin_not() {
    let program = lower("(if-not false 10 20)");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    let ExprKind::If {
        cond,
        then_expr,
        else_expr,
    } = &expr.kind
    else {
        panic!("expected if");
    };
    assert!(matches!(
        cond.kind,
        ExprKind::BuiltinCall { ref name, .. } if name == "not"
    ));
    assert!(matches!(then_expr.kind, ExprKind::Const(_)));
    assert!(matches!(else_expr.kind, ExprKind::Const(_)));
}

#[test]
fn lower_defn_into_fn_def_with_do_body() {
    let program = lower("(defn step [x] (println x) (+ x 1))");
    let TopLevel::FnDef {
        name, params, body, ..
    } = &program.top_levels[0]
    else {
        panic!("expected fn def");
    };
    assert_eq!(name, "step");
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].name, "x");
    let ExprKind::Do(items) = &body.kind else {
        panic!("expected do body");
    };
    assert_eq!(items.len(), 2);
    assert!(matches!(
        items[0].kind,
        ExprKind::BuiltinCall { ref name, .. } if name == "println"
    ));
    assert!(matches!(
        items[1].kind,
        ExprKind::BuiltinCall { ref name, .. } if name == "+"
    ));
}

#[test]
fn lower_let_preserves_bindings_and_body() {
    let program = lower("(let [x 1 y 2] (+ x y))");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    let ExprKind::Let { bindings, body } = &expr.kind else {
        panic!("expected let");
    };
    assert_eq!(bindings.len(), 2);
    assert_eq!(bindings[0].name, "x");
    assert_eq!(bindings[1].name, "y");
    assert!(matches!(bindings[0].value.kind, ExprKind::Const(_)));
    assert!(matches!(
        body.kind,
        ExprKind::BuiltinCall { ref name, .. } if name == "+"
    ));
}

#[test]
fn builtin_calls_are_marked_native_preferred() {
    let program = lower("(reduce + 0 (range 0 10))");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    assert!(matches!(
        expr.kind,
        ExprKind::BuiltinCall { ref name, .. } if name == "reduce"
    ));
    assert_eq!(expr.lowering, LoweringMode::NativePreferred);
}

#[test]
fn metadata_marks_io_builtin_effect() {
    let program = lower("(println 1)");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    assert_eq!(expr.effect, Effect::IO);
}

#[test]
fn non_builtin_symbol_call_requires_dynamic_fallback() {
    let program = lower("(let [f inc] (f 1))");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    let ExprKind::Let { body, .. } = &expr.kind else {
        panic!("expected let");
    };
    assert!(matches!(body.kind, ExprKind::Call { .. }));
    assert_eq!(body.lowering, LoweringMode::DynamicFallbackRequired);
}

#[test]
fn lower_when_let_rewrite_produces_do_body() {
    let program = lower("(when-let [x 2] (println x) x)");
    let TopLevel::Expr { expr, .. } = &program.top_levels[0] else {
        panic!("expected expr");
    };
    let ExprKind::Let { body, .. } = &expr.kind else {
        panic!("expected let");
    };
    let ExprKind::If { then_expr, .. } = &body.kind else {
        panic!("expected if");
    };
    let ExprKind::Let { body, .. } = &then_expr.kind else {
        panic!("expected inner let");
    };
    let ExprKind::Do(items) = &body.kind else {
        panic!("expected do body");
    };
    assert_eq!(items.len(), 2);
}
