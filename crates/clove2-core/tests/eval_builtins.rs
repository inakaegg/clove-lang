use clove2_core::eval::run_str;
use clove2_core::value::Value;

#[test]
fn comp_uses_comp_call() {
    let value = run_str("((comp inc inc) 1)").expect("comp should evaluate");
    assert_eq!(value, Value::Int(3));
}

#[test]
fn juxt_uses_juxt_call() {
    let value = run_str("((juxt inc dec) 5)").expect("juxt should evaluate");
    assert_eq!(value, Value::vec(vec![Value::Int(6), Value::Int(4)]));
}

#[test]
fn puts_alias_resolves() {
    let value = run_str("(puts \"alias\")").expect("puts should evaluate");
    assert_eq!(value, Value::Nil);
}
