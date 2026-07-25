use clove_build_core::ast::{Expr, Literal};
use clove_build_core::reader::read_all;

#[test]
fn read_list_and_vector() {
    let forms = read_all("(def x 1)\n[true nil 3.5]").unwrap();
    assert_eq!(
        forms,
        vec![
            Expr::list(vec![
                Expr::symbol("def"),
                Expr::symbol("x"),
                Expr::literal(Literal::Int(1)),
            ]),
            Expr::vector(vec![
                Expr::literal(Literal::Bool(true)),
                Expr::literal(Literal::Nil),
                Expr::literal(Literal::Float(3.5)),
            ]),
        ]
    );
}

#[test]
fn read_map_and_keyword() {
    let forms = read_all("{:port 80 :host \"localhost\"}").unwrap();
    assert_eq!(
        forms,
        vec![Expr::map(vec![
            (Expr::keyword("port"), Expr::literal(Literal::Int(80)),),
            (
                Expr::keyword("host"),
                Expr::literal(Literal::Str("localhost".to_string())),
            ),
        ])]
    );
}

#[test]
fn read_comments_and_escape() {
    let forms = read_all("; comment\n\"a\\n\"").unwrap();
    assert_eq!(forms, vec![Expr::literal(Literal::Str("a\n".to_string()))]);
}

#[test]
fn read_foreign_block() {
    let forms = read_all("$rb{puts 1}").unwrap();
    assert_eq!(forms, vec![Expr::foreign_block("rb", "puts 1")]);
}

#[test]
fn read_expr_type_annotation() {
    let forms = read_all("[1 2]: [Int]").unwrap();
    assert_eq!(
        forms,
        vec![Expr::list(vec![
            Expr::symbol("as"),
            Expr::vector(vec![Expr::symbol("Int")]),
            Expr::vector(vec![
                Expr::literal(Literal::Int(1)),
                Expr::literal(Literal::Int(2)),
            ]),
        ])]
    );
}

#[test]
fn rejects_slash_namespace_separator() {
    // `/` was abolished as a namespace separator (TASK/DONE/名前空間.md, 2025-12-17).
    // The phase1 reader errors at parse time; phase2 must agree.
    for src in [
        "(println 'foo/bar)",
        "(println :foo/bar)",
        "(name :foo/bar)",
    ] {
        let err = read_all(src)
            .expect_err(&format!("{src} must be rejected"))
            .to_string();
        assert!(
            err.contains("namespace separator '/' has been removed"),
            "{src} should report the '/' abolition, got: {err}"
        );
    }
}

#[test]
fn keeps_slash_as_division_operator() {
    // The bare `/` symbol stays valid: it is the division operator.
    let forms = read_all("(/ 6 2)").unwrap();
    assert_eq!(
        forms,
        vec![Expr::list(vec![
            Expr::symbol("/"),
            Expr::literal(Literal::Int(6)),
            Expr::literal(Literal::Int(2)),
        ])]
    );
}

#[test]
fn keeps_regex_literal_with_slashes() {
    // Regex literals must not be caught by the `/` rejection.
    read_all(r#"(re-find /a+/ "aa")"#).expect("regex literal should still read");
}

#[test]
fn reports_unsupported_syntax_as_unsupported_not_malformed() {
    // These are valid Clove (phase1 accepts them); phase2 simply has not
    // implemented them. A bare "unsupported reader tag" / "expected token"
    // reads like a syntax error, which sent readers looking for a typo.
    let cases = [
        (r#"(def c #json{"h":1})"#, "#json"),
        ("(def c #yaml{\n  h: 1\n})", "#yaml"),
        (r#"(re-find #/a+/ "aa")"#, "#/"),
        ("(println ${1 + 2})", "${"),
    ];
    for (src, needle) in cases {
        let err = read_all(src)
            .expect_err(&format!("{src} is not supported by phase2 yet"))
            .to_string();
        assert!(
            err.contains(needle),
            "error for {src:?} should name {needle:?}, got: {err}"
        );
        assert!(
            err.contains("not supported"),
            "error for {src:?} should say it is unsupported, got: {err}"
        );
    }
}
