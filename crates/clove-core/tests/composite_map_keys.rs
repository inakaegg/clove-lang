//! `Key` に収まらない複合値をマップのキーにしたときの表現。
//!
//! `Key` は keyword / symbol / string / number / bool しか持たないため、マップや
//! ベクタをキーにすると文字列へ落とされる。以前はその文字列が Rust の Debug 表記
//! （`{Keyword("a"): 1}` や `[1, 2]`）で、
//!
//! - 内部表現が値として利用者に見えてしまう（`set::index` の出力で露出）
//! - 等しいマップでも書いた順で別のキーになり、引けなくなる
//!
//! という2つの問題があった。複合キーを `Key` で表せるようにするのが本来の直しだが、
//! ここでは少なくとも Clove の表記で、順序に依存しない形にする。

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

fn rendered(src: &str) -> String {
    match eval(&format!("(pr-str {})", src)) {
        Value::String(s) => s,
        other => panic!("expected string, got {other:?}"),
    }
}

#[test]
fn composite_keys_do_not_leak_rust_debug_output() {
    for src in [
        "(assoc {} {:a 1} :x)",
        "(assoc {} [1 2] :v)",
        "(assoc {} #{1 2} :s)",
        "(set::index #{{:a 1 :b 2} {:a 1 :b 3}} [:a])",
    ] {
        let out = rendered(src);
        for leaked in ["Keyword(", "Symbol(", "String(", "1, 2"] {
            assert!(
                !out.contains(leaked),
                "{src} leaked Rust debug output ({leaked}): {out}"
            );
        }
    }
}

#[test]
fn composite_keys_use_clove_notation() {
    // マップの表示では Clove 構文のまま出る。
    assert_eq!(rendered("(assoc {} {:a 1} :x)"), "{{:a 1} :x}");
    assert_eq!(rendered("(assoc {} [1 2] :v)"), "{[1 2] :v}");
}

/// キーを値として取り出す経路でも、複合値は文字列に落ちない。
/// 落とすと `{:a 1}` と `"{:a 1}"` が同じ値になってしまう。
#[test]
fn composite_keys_survive_as_values() {
    assert_eq!(rendered("(keys (assoc {} {:a 1} :x))"), "[{:a 1}]");
    assert_eq!(rendered("(keys (assoc {} [1 2] :v))"), "[[1 2]]");
    assert_eq!(
        rendered(r#"(keys (assoc (assoc {} {:a 1} :x) "{:a 1}" :y))"#),
        r#"[{:a 1} "{:a 1}"]"#
    );
    // 取り出したキーで引き直せる
    assert_eq!(
        eval("(let [m (assoc {} {:a 1} :x)] (get m (first (keys m))))"),
        Value::Symbol(":x".into())
    );
}

/// 複合キーは同じ表記の文字列キーと別物。`Key::String` へ落とすと
/// `{:a 1}` と `"{:a 1}"` が同じキーへ潰れる。
#[test]
fn composite_keys_do_not_collide_with_string_keys() {
    assert_eq!(
        eval(r#"(count (assoc (assoc {} {:a 1} :x) "{:a 1}" :y))"#),
        Value::Int(2)
    );
    assert_eq!(
        eval(r#"(count (assoc (assoc {} [1 2] :x) "[1 2]" :y))"#),
        Value::Int(2)
    );
    assert_eq!(
        eval(r#"(get (assoc (assoc {} {:a 1} :x) "{:a 1}" :y) {:a 1})"#),
        Value::Symbol(":x".into())
    );
    assert_eq!(
        eval(r#"(get (assoc (assoc {} {:a 1} :x) "{:a 1}" :y) "{:a 1}")"#),
        Value::Symbol(":y".into())
    );
}

#[test]
fn equal_maps_are_the_same_key_regardless_of_written_order() {
    assert_eq!(
        eval("(get (assoc {} {:a 1 :b 2} :x) {:b 2 :a 1})"),
        Value::Symbol(":x".into())
    );
    assert_eq!(
        eval("(count (assoc (assoc {} {:a 1 :b 2} :x) {:b 2 :a 1} :y))"),
        Value::Int(1)
    );
}

#[test]
fn equal_sets_are_the_same_key_regardless_of_written_order() {
    assert_eq!(
        eval("(get (assoc {} #{1 2} :s) #{2 1})"),
        Value::Symbol(":s".into())
    );
}

#[test]
fn vector_keys_keep_their_order() {
    assert_eq!(
        eval("(get (assoc {} [1 2] :v) [1 2])"),
        Value::Symbol(":v".into())
    );
    assert_eq!(eval("(get (assoc {} [1 2] :v) [2 1])"), Value::Nil);
}
