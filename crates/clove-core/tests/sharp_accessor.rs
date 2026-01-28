use clove_core::eval_source;

#[test]
fn sharp_accessor_maps_examples() {
    let cases = [
        ("(map #0 [[1 2] [3 4]])", "[1 3]"),
        ("(map #0.1 [[[0 9]] [[8 7]]])", "[9 7]"),
        ("(map #[1] [[0 9] [8 7]])", "[9 7]"),
        ("(map #[0][1] [[[0 9]] [[8 7]]])", "[9 7]"),
        ("(map #[0 1] [[[0 9]] [[8 7]]])", "[9 7]"),
    ];
    for (src, expected_src) in cases {
        let out = eval_source(src, None).expect("eval sharp accessor");
        let expected = eval_source(expected_src, None).expect("expected value");
        assert_eq!(out, expected, "{}", src);
    }
}

#[test]
fn sharp_accessor_v2_examples() {
    let cases = [
        ("(map #[0] [[0 10] [1 11]])", "[0 1]"),
        ("(map #[0,1] [[0 10] [1 11]])", "[[0 10] [1 11]]"),
        ("(map #[0 1] [[[1 2] [3 4]]])", "[2]"),
        ("(map #[0 1, 1 0] [[[1 2] [3 4]]])", "[[2 3]]"),
        ("(map #[:a || 99] [{:a 1} {}])", "[1 99]"),
        ("(map #[:a || 99] [{:a nil} {}])", "[99 99]"),
        (
            "(map #[\"name\",\"age\"] [{\"name\" \"a\" \"age\" 1}])",
            "[[\"a\" 1]]",
        ),
        ("(map #:name:first [{:name {:first \"x\"}}])", "[\"x\"]"),
        ("(map #\"aaa\".\"bbb\" [{\"aaa\" {\"bbb\" 1}}])", "[1]"),
        ("(map #:aaa.\"bbb\" [{:aaa {\"bbb\" 1}}])", "[1]"),
        ("(map #\"aaa\".0 [{\"aaa\" [9 8 7]}])", "[9]"),
        ("(map #\"aaa\".[0,1] [{\"aaa\" [9 8 7]}])", "[[9 8]]"),
    ];
    for (src, expected_src) in cases {
        let out = eval_source(src, None).expect("eval sharp accessor v2");
        let expected = eval_source(expected_src, None).expect("expected value");
        assert_eq!(out, expected, "{}", src);
    }
}

#[test]
fn sharp_accessor_invalid_tag_errors() {
    let err = eval_source("#foo", None).expect_err("invalid sharp accessor");
    assert!(
        err.to_string().contains("unknown reader tag"),
        "unexpected error: {}",
        err
    );
}
