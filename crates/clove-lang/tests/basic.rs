use clove_core::{error::CloveError, eval_source_with_engines, options::EvalOptions};

fn eval_source(src: &str) -> Result<clove_core::ast::Value, CloveError> {
    // Use this helper for cases where the Python engine is enough (to avoid Ruby initialization).
    eval_source_with_engines(src, EvalOptions::default(), &clove_python::engines())
}

#[test]
fn map_and_get_assoc() {
    let out = eval_source("(assoc {:a 1} :b 2)").unwrap();
    assert!(out.to_string().contains(":a 1"));
    assert!(out.to_string().contains(":b 2"));

    let out = eval_source("(get {:a 1} :a)").unwrap();
    assert_eq!(out.to_string(), "1");

    let out = eval_source("(get {:a 1} :missing 42)").unwrap();
    assert_eq!(out.to_string(), "42");
}

#[test]
fn set_literal() {
    let out = eval_source("#{1 2 3}").unwrap();
    assert_eq!(out.to_string(), "#{1 2 3}");
}

#[test]
fn arithmetic_core_funcs() {
    let out = eval_source("(* 2 3 4)").unwrap();
    assert_eq!(out.to_string(), "24");

    let out = eval_source("(/ 20 2 2)").unwrap();
    assert_eq!(out.to_string(), "5");
}

#[test]
fn py_block_uses_env() {
    let out = eval_source("(let [x 2] (py \"x = x + 3\nx\"))").unwrap();
    assert_eq!(out.to_string(), "5");

    let out = eval_source("(py \"1 + 2\" )").unwrap();
    assert_eq!(out.to_string(), "3");
}

#[test]
fn error_with_span() {
    let err = eval_source("(+ x 1)").unwrap_err();
    let msg = format!("{}", err).to_lowercase();
    assert!(msg.contains("unbound symbol") || msg.contains("unbound"));
}

#[test]
fn map_and_reduce_with_functions() {
    let out = eval_source("(map inc [1 2 3])").unwrap();
    assert_eq!(out.to_string(), "[2 3 4]");

    let out = eval_source("(reduce (fn [acc x] (+ acc x)) [1 2 3 4])").unwrap();
    assert_eq!(out.to_string(), "10");

    let out = eval_source("(reduce (fn [acc x] (+ acc x)) 10 [1 2])").unwrap();
    assert_eq!(out.to_string(), "13");
}
