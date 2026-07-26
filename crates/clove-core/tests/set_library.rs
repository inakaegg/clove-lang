//! The bundled `set` library must work with the symbols it can actually see.
//!
//! `clove_set.clv` loads before `clove_std.clv` and lives in its own namespace, so a
//! reference to a std-level function such as `every?` is unbound at call time. That broke
//! `set::subset?` / `set::superset?` for every caller, and was only caught once the doc
//! example test stopped truncating its run.

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

#[test]
fn subset_and_superset_are_callable() {
    assert_eq!(eval("(set::subset? #{1} #{1 2})"), Value::Bool(true));
    assert_eq!(eval("(set::subset? #{1 3} #{1 2})"), Value::Bool(false));
    assert_eq!(eval("(set::subset? #{} #{1})"), Value::Bool(true));
    assert_eq!(eval("(set::superset? #{1 2} #{1})"), Value::Bool(true));
    assert_eq!(eval("(set::superset? #{1} #{1 2})"), Value::Bool(false));
    assert_eq!(eval("(set::superset? #{1} #{})"), Value::Bool(true));
}

#[test]
fn subset_accepts_vectors_like_the_other_set_functions() {
    assert_eq!(eval("(set::subset? [1] [1 2])"), Value::Bool(true));
    assert_eq!(eval("(set::superset? [1 2] [2])"), Value::Bool(true));
}
