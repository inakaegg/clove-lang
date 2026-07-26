//! `source` は書いたとおりのソースを返す。内部形式を漏らさない。
//!
//! コンパイラは通常の呼び出しを `(__apply f args..)` へ下ろす。`source` は
//! 下ろした後のフォームを文字列化していたため、`(defn f [x] (+ x 1))` が
//! `(defn f [x] (__apply + x 1))` として返っていた。

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

fn source_of(src: &str) -> String {
    match eval(src) {
        Value::String(s) => s,
        other => panic!("expected source string, got {other:?}"),
    }
}

#[test]
fn source_of_defn_has_no_internal_apply() {
    assert_eq!(
        source_of("(defn f [x] (+ x 1)) (source 'f)"),
        "(defn f [x] (+ x 1))"
    );
}

#[test]
fn source_of_nested_calls_has_no_internal_apply() {
    let out = source_of("(defn g [x] (if (> x 0) (inc x) (dec x))) (source 'g)");
    assert!(!out.contains("__apply"), "internal form leaked: {out}");
    assert_eq!(out, "(defn g [x] (if (> x 0) (inc x) (dec x)))");
}

#[test]
fn source_of_def_has_no_internal_apply() {
    assert_eq!(source_of("(def a (+ 1 2)) (source 'a)"), "(def a (+ 1 2))");
}

#[test]
fn source_of_unknown_symbol_is_nil() {
    assert_eq!(eval("(source 'no-such-symbol-here)"), Value::Nil);
}
