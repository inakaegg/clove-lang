use clove_core::ast::Value;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use std::sync::Arc;

fn ctx() -> Arc<RuntimeCtx> {
    RuntimeCtx::new(EvalOptions::default(), &[])
}

#[test]
fn with_redefs_accepts_value_vector_and_restores() {
    let ctx = ctx();
    let value = ctx
        .eval_source(
            "(do (def foo 1)\n                 (let [b [(quote foo) 2]]\n                   [(with-redefs b foo) foo]))",
        )
        .expect("with-redefs should work with value vector");
    let items = match value {
        Value::Vector(v) => v,
        other => panic!("expected vector result, got {}", other.type_name()),
    };
    assert_eq!(items.len(), 2);
    assert_eq!(items[0], Value::Int(2));
    assert_eq!(items[1], Value::Int(1));
}

#[test]
fn with_redefs_fn_works_with_value_vector() {
    let ctx = ctx();
    let value = ctx
        .eval_source(
            "(do (def foo (fn [] 1))\n                 (def g (fn [] (foo)))\n                 (let [b [(quote foo) (fn [] 2)]]\n                   [((with-redefs-fn b g)) (g)]))",
        )
        .expect("with-redefs-fn should work with value vector");
    let items = match value {
        Value::Vector(v) => v,
        other => panic!("expected vector result, got {}", other.type_name()),
    };
    assert_eq!(items.len(), 2);
    assert_eq!(items[0], Value::Int(2));
    assert_eq!(items[1], Value::Int(1));
}
