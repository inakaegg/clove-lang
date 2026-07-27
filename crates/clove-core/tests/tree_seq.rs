//! `tree-seq` は前順（pre-order）の深さ優先で、遅延して1ノードずつ返す。
//!
//! doc例は深さ1しか見ていないため、多段のネストで順序が崩れても気づけない。

use clove_core::ast::Value;
use clove_core::eval_source;

fn eval(src: &str) -> Value {
    eval_source(src, None).unwrap_or_else(|err| panic!("{src} failed: {err}"))
}

#[test]
fn tree_seq_walks_depth_first_in_pre_order() {
    // 1 -[2 -[4 5], 3]
    let src = r#"
      (let [tree {:v 1 :kids [{:v 2 :kids [{:v 4} {:v 5}]} {:v 3}]}
            branch? #(contains? % :kids)
            kids #(get % :kids [])]
        (vec (map :v (tree-seq branch? kids tree))))
    "#;
    assert_eq!(
        eval(src),
        Value::Vector(
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(4),
                Value::Int(5),
                Value::Int(3)
            ]
            .into()
        )
    );
}

#[test]
fn tree_seq_is_lazy_enough_to_take_from_an_infinite_tree() {
    // 子を無限に持つ木でも take した分しか展開しない。
    let src = r#"
      (let [branch? (fn [n] true)
            kids (fn [n] [(* n 2) (+ (* n 2) 1)])]
        (vec (take 5 (tree-seq branch? kids 1))))
    "#;
    assert_eq!(
        eval(src),
        Value::Vector(
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(4),
                Value::Int(8),
                Value::Int(16)
            ]
            .into()
        )
    );
}
