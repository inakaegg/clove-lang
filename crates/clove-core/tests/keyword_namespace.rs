//! 名前空間付きキーワードの区切りは `::`。
//!
//! `/` 区切りは言語から削除されており、リーダーは `foo/bar` を拒否する。
//! `(keyword "ns" "name")` が `/` 区切りを作ると、作った値をソースへ書き戻せない。
//! phase2 の評価器 (`clove-build-core`) は以前から `::` を使っている。

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

#[test]
fn keyword_with_namespace_uses_double_colon() {
    assert_eq!(
        eval(r#"(keyword "ns" "name")"#),
        Value::Symbol(":ns::name".into())
    );
}

#[test]
fn namespaced_keyword_round_trips_through_the_reader() {
    assert_eq!(
        eval(r#"(= (keyword "ns" "name") :ns::name)"#),
        Value::Bool(true)
    );
    // str はキーワードの `:` を落とす。読み戻せる表記は pr-str 側。
    assert_eq!(
        eval(r#"(str (keyword "ns" "name"))"#),
        Value::String("ns::name".into())
    );
    assert_eq!(
        eval(r#"(pr-str (keyword "ns" "name"))"#),
        Value::String(":ns::name".into())
    );
}

#[test]
fn keyword_accepts_symbols_and_leading_colons() {
    assert_eq!(
        eval(r#"(keyword ":ns" ":name")"#),
        Value::Symbol(":ns::name".into())
    );
    assert_eq!(
        eval("(keyword 'ns 'name)"),
        Value::Symbol(":ns::name".into())
    );
}

#[test]
fn keyword_does_not_double_the_separator() {
    // 名前側がすでに区切りから始まっているときは足さない
    assert_eq!(
        eval(r#"(keyword "ns" "::name")"#),
        Value::Symbol(":ns::name".into())
    );
}

#[test]
fn single_argument_keyword_is_unchanged() {
    assert_eq!(eval(r#"(keyword "name")"#), Value::Symbol(":name".into()));
    assert_eq!(eval(r#"(keyword ":name")"#), Value::Symbol(":name".into()));
}
