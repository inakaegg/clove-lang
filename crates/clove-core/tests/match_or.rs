use clove_core::eval_source;

#[test]
fn match_or_accepts_commas_and_pipes() {
    let cases = [
        ("(match 2 1,2,3 true _ false)", "true"),
        ("(match 2 1 | 2 | 3 true _ false)", "true"),
        ("(match 3 1,2 | 3 :hit _ :miss)", ":hit"),
    ];
    for (src, expected_src) in cases {
        let out = eval_source(src, None).expect("eval match or");
        let expected = eval_source(expected_src, None).expect("expected value");
        assert_eq!(out, expected, "{}", src);
    }
}

#[test]
fn match_or_rejects_trailing_separator() {
    let err = eval_source("(match 1 1 |)", None).expect_err("match trailing separator");
    assert!(
        err.to_string()
            .contains("match pattern expects expression after separator"),
        "unexpected error: {}",
        err
    );
}
