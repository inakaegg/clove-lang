//! OOP例の自動生成は、変換して成立する形だけを出す。
//!
//! 成立しない形まで出すと doc例テストへ「動かないOOP例」が流れる。
//! `resolve_subject_pos` は fn_meta を引くので、ランタイムを作って
//! builtin のメタ情報が登録された状態で確かめる。

use clove_core::doc_examples::try_gen_oop_example;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

fn with_runtime<F: FnOnce()>(body: F) {
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    ctx.with_current_ctx(|_| body());
}

#[test]
fn keyword_receivers_are_not_converted() {
    with_runtime(|| {
        // 主語が :a だと `:a.hash-map(1 :b 2)` になり、索引適用として読まれる
        for example in [
            "(hash-map :a 1 :b 2) ; => {:a 1 :b 2}",
            "(keyword? :a) ; => true",
            "(sorted-map :b 2 :a 1) ; => {:a 1 :b 2}",
        ] {
            assert_eq!(
                try_gen_oop_example(example, Some("core")),
                None,
                "{example}"
            );
        }
    });
}

#[test]
fn pattern_coll_sugar_is_not_converted() {
    with_runtime(|| {
        // 主語 (odd? x) を動かすと [x coll] の束縛の外へ出る
        assert_eq!(
            try_gen_oop_example(
                "(vec (filter [x [1 2 3 4]] (odd? x))) ; => [1 3]",
                Some("core")
            ),
            None
        );
    });
}

#[test]
fn doto_body_is_left_alone() {
    with_runtime(|| {
        // doto は評価前のフォームを順に適用するので、中を書き換えると別物になる。
        // 周りの呼び出しは変換してよい。
        let out = try_gen_oop_example(
            "(let [a (atom 0)] (doto a (swap! inc)) (deref a)) ; => 1",
            Some("core"),
        )
        .expect("surrounding calls are still converted");
        assert!(
            out.contains("(doto a (swap! inc))"),
            "doto body was rewritten: {out}"
        );
    });
}

#[test]
fn expression_heads_are_left_alone() {
    with_runtime(|| {
        // 先頭を OOP チェーンにすると呼び出し全体の読まれ方が変わる
        assert_eq!(
            try_gen_oop_example("((constantly 42) :ignored) ; => 42", Some("core")),
            None
        );
    });
}

#[test]
fn ordinary_calls_are_still_converted() {
    with_runtime(|| {
        assert_eq!(
            try_gen_oop_example("(count [1 2 3]) ; => 3", Some("core")).as_deref(),
            Some("[1 2 3].count() ; => 3")
        );
    });
}
