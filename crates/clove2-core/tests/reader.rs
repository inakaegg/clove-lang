use clove2_core::ast::{Expr, Literal};
use clove2_core::reader::read_all;

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
