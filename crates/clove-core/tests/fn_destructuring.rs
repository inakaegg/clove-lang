use clove_core::eval_source;

#[test]
fn vector_destructuring_in_fn_params_binds_values() {
    let src = "(do (defn head-tail [[h & t]] {:head h :tail t}) (head-tail [1 2 3]))";
    let out = eval_source(src, None).expect("eval head-tail");
    let expected = eval_source("{:head 1 :tail [2 3]}", None).expect("literal map");
    assert_eq!(out, expected);
}

#[test]
fn map_destructuring_in_fn_params_supports_keys_and_as() {
    let src = "(do (defn pick-coords [{:keys [x y] :as whole}] [x y whole]) (pick-coords {:x 10 :y 20 :z 30}))";
    let out = eval_source(src, None).expect("eval pick-coords");
    let expected = eval_source("[10 20 {:x 10 :y 20 :z 30}]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn map_literal_accepts_shorthand_entries() {
    let src = "(let [a 1 b 2] {:a, :b})";
    let out = eval_source(src, None).expect("eval shorthand map");
    let expected = eval_source("{:a 1 :b 2}", None).expect("literal map");
    assert_eq!(out, expected);
}

#[test]
fn map_destructuring_accepts_shorthand_keys() {
    let src = "(do (defn pick [{a, b}] [a b]) (pick {a: 10 b: 20}))";
    let out = eval_source(src, None).expect("eval pick");
    let expected = eval_source("[10 20]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn map_destructuring_shorthand_allows_as_binding() {
    let src = "(let [{:a, :as whole} {:a 1 :b 2}] [a whole])";
    let out = eval_source(src, None).expect("eval shorthand with as");
    let expected = eval_source("[1 {:a 1 :b 2}]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn map_destructuring_shorthand_without_trailing_comma_before_as() {
    let src = "(let [{:a :as whole} {:a 1 :b 2}] [a whole])";
    let out = eval_source(src, None).expect("eval shorthand without comma");
    let expected = eval_source("[1 {:a 1 :b 2}]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn map_destructuring_allows_commas_as_whitespace_for_special_keys() {
    let src = "(let [{:keys [a] , :as m} {:a 10 :b 20}] [a m])";
    let out = eval_source(src, None).expect("eval destructuring with comma");
    let expected = eval_source("[10 {:a 10 :b 20}]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn type_destructuring_in_let_binds_fields() {
    let src =
        "(do (deftype Foo {:x Int :y Int}) (let [(Foo {:x x :y y}) (Foo {:x 1 :y 2})] [x y]))";
    let out = eval_source(src, None).expect("eval type destructuring");
    let expected = eval_source("[1 2]", None).expect("literal vector");
    assert_eq!(out, expected);
}

#[test]
fn rest_args_allow_destructuring_pattern() {
    let src = "(do (defn rest-two [first & [a b]] [first a b]) (rest-two :x 1))";
    let out = eval_source(src, None).expect("eval rest-two");
    let expected = eval_source("[:x 1 nil]", None).expect("literal vector");
    assert_eq!(out, expected);
}
