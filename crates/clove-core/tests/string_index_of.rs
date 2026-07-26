//! `index-of` / `last-index-of` の添字は文字単位。
//!
//! `subs` と `count` が文字単位なので、byte offset を返すと
//! `(subs s (index-of s x))` が壊れる。さらに `start` / `end` が多バイト文字の
//! 途中を指すとスライスで panic していた。

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

#[test]
fn index_of_returns_char_index() {
    assert_eq!(eval(r#"(index-of "aéb" "b")"#), Value::Int(2));
    assert_eq!(eval(r#"(index-of "日本語" "語")"#), Value::Int(2));
    assert_eq!(eval(r#"(index-of "aéb" "z")"#), Value::Nil);
}

#[test]
fn last_index_of_returns_char_index() {
    assert_eq!(eval(r#"(last-index-of "aéb" "b")"#), Value::Int(2));
    assert_eq!(eval(r#"(last-index-of "éaéa" "a")"#), Value::Int(3));
    // docの例
    assert_eq!(eval(r#"(last-index-of "banana" "na")"#), Value::Int(4));
    assert_eq!(eval(r#"(last-index-of "banana" "z")"#), Value::Nil);
}

#[test]
fn index_of_result_feeds_subs() {
    // subs は文字単位。index-of が byte offset を返すと空文字列になる。
    assert_eq!(
        eval(r#"(subs "aéb" (index-of "aéb" "b"))"#),
        Value::String("b".into())
    );
}

#[test]
fn index_of_start_is_a_char_index() {
    assert_eq!(eval(r#"(index-of "aéba" "a" 1)"#), Value::Int(3));
    assert_eq!(eval(r#"(index-of "abcabc" "b" 3)"#), Value::Int(4));
    // start が文字数と等しいときは空文字列だけが見つかる
    assert_eq!(eval(r#"(index-of "abc" "" 3)"#), Value::Int(3));
    assert_eq!(eval(r#"(index-of "abc" "" 4)"#), Value::Nil);
    assert_eq!(eval(r#"(index-of "abc" "c" 3)"#), Value::Nil);
}

#[test]
fn last_index_of_end_is_an_exclusive_char_index() {
    assert_eq!(eval(r#"(last-index-of "banana" "na" 4)"#), Value::Int(2));
    assert_eq!(eval(r#"(last-index-of "aéb" "b" 3)"#), Value::Int(2));
    assert_eq!(eval(r#"(last-index-of "aéb" "b" 2)"#), Value::Nil);
    // end が文字数を超えても全体を見るだけ
    assert_eq!(eval(r#"(last-index-of "aéb" "b" 99)"#), Value::Int(2));
    // end = 0 は nil（phase2 の eval と同じ挙動。Clojure の 0 とは異なる）
    assert_eq!(eval(r#"(last-index-of "abc" "" 0)"#), Value::Nil);
}

#[test]
fn char_index_inside_a_multibyte_char_does_not_panic() {
    // 以前は s[start..] / s[..end] が char 境界外で panic していた。
    assert_eq!(eval(r#"(index-of "é" "x" 1)"#), Value::Nil);
    assert_eq!(eval(r#"(last-index-of "é" "x" 1)"#), Value::Nil);
    assert_eq!(eval(r#"(index-of "éé" "é" 1)"#), Value::Int(1));
    assert_eq!(eval(r#"(last-index-of "éé" "é" 1)"#), Value::Int(0));
}
