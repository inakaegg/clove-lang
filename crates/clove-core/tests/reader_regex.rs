#[path = "common/mod.rs"]
mod common;

use clove_core::ast::{Form, FormKind, InterpolatedPart, MapItem, RegexDelim};

fn contains_regex(form: &Form) -> bool {
    match &form.kind {
        FormKind::Regex { .. } => true,
        FormKind::List(items)
        | FormKind::Vector(items)
        | FormKind::Set(items)
        | FormKind::ShortFn(items) => items.iter().any(contains_regex),
        FormKind::Map(items) => items.iter().any(|item| match item {
            MapItem::KeyValue(k, v) => contains_regex(k) || contains_regex(v),
            MapItem::Spread(value) => contains_regex(value),
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => contains_regex(expr),
            InterpolatedPart::Text(_) => false,
        }),
        FormKind::InterpolatedRegex { .. } => true,
        _ => false,
    }
}

#[test]
fn reader_does_not_treat_division_as_regex_literal() {
    let source = "(let [x (/ 10 2) y (/ 20 4)] (+ x y))";
    let forms = common::parse_forms(source);
    assert!(
        forms.iter().all(|form| !contains_regex(form)),
        "division should not be parsed as regex literal"
    );
}

#[test]
fn reader_parses_hash_slash_regex_literal() {
    let forms = common::parse_forms("#/ /");
    assert!(matches!(
        &forms[0].kind,
        FormKind::Regex { pattern, delim }
            if pattern == " " && matches!(delim, RegexDelim::HashSlash)
    ));
}

#[test]
fn reader_parses_hash_slash_regex_literal_with_digits() {
    let forms = common::parse_forms(r#"#/(\d+)/"#);
    assert!(matches!(
        &forms[0].kind,
        FormKind::Regex { pattern, delim }
            if pattern == r#"(\d+)"# && matches!(delim, RegexDelim::HashSlash)
    ));
}

#[test]
fn reader_parses_interpolated_regex_literal() {
    let forms = common::parse_forms("/foo#{bar}baz/");
    match &forms[0].kind {
        FormKind::InterpolatedRegex { parts, delim } => {
            assert!(matches!(delim, RegexDelim::Slash));
            assert_eq!(parts.len(), 3);
            match &parts[0] {
                InterpolatedPart::Text(text) => assert_eq!(text, "foo"),
                other => panic!("expected regex text part, got {:?}", other),
            }
            match &parts[1] {
                InterpolatedPart::Expr(expr) => match &expr.kind {
                    FormKind::Symbol(sym) => assert_eq!(sym, "bar"),
                    other => panic!("unexpected expr kind: {:?}", other),
                },
                other => panic!("expected regex expr part, got {:?}", other),
            }
            match &parts[2] {
                InterpolatedPart::Text(text) => assert_eq!(text, "baz"),
                other => panic!("expected regex text part, got {:?}", other),
            }
        }
        other => panic!("expected interpolated regex, got {:?}", other),
    }
}
