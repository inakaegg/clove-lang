use clove_core::eval_source;

#[test]
fn nth_multi_vector() {
    let out = eval_source("(nth [10 20 30 40] [1 3])", None).expect("nth multi");
    let expected = eval_source("[20 40]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn nth_multi_duplicates() {
    let out = eval_source("(nth [10 20 30] [1 1 2])", None).expect("nth duplicates");
    let expected = eval_source("[20 20 30]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn nth_multi_default() {
    let out = eval_source("(nth [10 20 30] [0 99 2] nil)", None).expect("nth default");
    let expected = eval_source("[10 nil 30]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn nth_multi_out_of_bounds_errors() {
    eval_source("(nth [10] [0 1])", None).expect_err("nth should error");
}

#[test]
fn nth_multi_nil_coll() {
    let out = eval_source("(nth nil [1 2])", None).expect("nth nil coll");
    let expected = eval_source("[nil nil]", None).expect("expected vector");
    assert_eq!(out, expected);

    let out = eval_source("(nth nil [1 2] 0)", None).expect("nth nil coll default");
    let expected = eval_source("[0 0]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn nth_multi_seq() {
    let out = eval_source("(nth (range) [0 3 5])", None).expect("nth seq");
    let expected = eval_source("[0 3 5]", None).expect("expected vector");
    assert_eq!(out, expected);
}
