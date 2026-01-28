use clove_core::eval_source;

#[test]
fn path_sanitize_filename_examples() {
    let cases = [
        ("(path::sanitize \"a/b:c*?.txt\")", "\"a_b_c__.txt\""),
        ("(path::sanitize \"..\")", "\"_\""),
        ("(path::sanitize-filename \"CON\")", "\"_CON\""),
    ];
    for (src, expected_src) in cases {
        let out = eval_source(src, None).expect("eval sanitize");
        let expected = eval_source(expected_src, None).expect("expected value");
        assert_eq!(out, expected, "{}", src);
    }
}

#[test]
fn path_sanitize_path_examples() {
    let cases = [
        ("(path::sanitize-path \"a/../b\")", "\"b\""),
        ("(path::sanitize-path \"/etc/passwd\")", "\"etc/passwd\""),
    ];
    for (src, expected_src) in cases {
        let out = eval_source(src, None).expect("eval sanitize-path");
        let expected = eval_source(expected_src, None).expect("expected value");
        assert_eq!(out, expected, "{}", src);
    }
}

#[test]
fn path_slugify_examples() {
    let out = eval_source("(path::slugify \"Hello, World!\")", None).expect("eval slugify");
    let expected = eval_source("\"hello-world\"", None).expect("expected value");
    assert_eq!(out, expected);
}
