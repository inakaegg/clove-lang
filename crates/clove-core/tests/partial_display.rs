//! `partial` の表示は部分適用の内容を出す。
//!
//! `clove_std.clv` が `partial` を素の lambda で再定義していたため、Rust側の
//! `core::partial` が持つ表示（`#<partial + args=[1] remaining=any>`）が使われず
//! `#<lambda>` になっていた。std は `{:subject-pos 1}` を付けるために定義を持つが、
//! 中身は core へ委譲すればよい。

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
fn partial_shows_the_captured_arguments() {
    assert_eq!(
        rendered("(partial + 1)"),
        "#<partial + args=[1] remaining=any>"
    );
    assert_eq!(
        rendered("(partial + 1 2)"),
        "#<partial + args=[1 2] remaining=any>"
    );
}

#[test]
fn partial_still_applies_the_captured_arguments() {
    assert_eq!(eval("((partial + 1) 2)"), Value::Int(3));
    assert_eq!(eval("((partial + 1 2) 3)"), Value::Int(6));
    assert_eq!(
        eval(r#"((partial str "a") "b")"#),
        Value::String("ab".into())
    );
    // 追加引数なしで呼べること
    assert_eq!(eval("((partial + 1 2))"), Value::Int(3));
}

#[test]
fn partial_composes_with_higher_order_functions() {
    // `*` は引数位置では展開マーカーとして読まれるので、関数として渡すときは
    // 修飾名を使う（docs/language/reader_syntax.md の spread の項）。
    assert_eq!(
        eval("(vec (map (partial core::* 2) [1 2 3]))"),
        Value::Vector(vec![Value::Int(2), Value::Int(4), Value::Int(6)].into())
    );
    assert_eq!(
        eval("(vec (map (partial + 10) [1 2 3]))"),
        Value::Vector(vec![Value::Int(11), Value::Int(12), Value::Int(13)].into())
    );
}
