use clove_core::ast::{FormKind, InterpolatedPart, Value};
use clove_core::eval_source;
use clove_core::formatter::{format_source, FormatOptions};
use clove_core::reader::Reader;

#[test]
fn reader_parses_interpolated_parts() {
    let mut reader = Reader::new("\"hello #{name}!\"");
    let forms = reader.read_all().expect("parse interpolation");
    let form = forms.into_iter().next().expect("one form");
    match form.kind {
        FormKind::InterpolatedString(parts) => {
            assert_eq!(parts.len(), 3);
            match &parts[0] {
                InterpolatedPart::Text(text) => assert_eq!(text, "hello "),
                other => panic!("expected text part, got {:?}", other),
            }
            match &parts[1] {
                InterpolatedPart::Expr(expr) => match &expr.kind {
                    FormKind::Symbol(sym) => assert_eq!(sym, "name"),
                    other => panic!("unexpected expr kind: {:?}", other),
                },
                other => panic!("expected expr part, got {:?}", other),
            }
            match &parts[2] {
                InterpolatedPart::Text(text) => assert_eq!(text, "!"),
                other => panic!("expected trailing text, got {:?}", other),
            }
        }
        other => panic!("expected interpolated string, got {:?}", other),
    }
}

#[test]
fn eval_interpolated_string_builds_text() {
    let val = eval_source(
        "(let [name \"World\" n 3] \"hello #{name} #{(+ n 1)}!\")",
        None,
    )
    .expect("eval interpolated string");
    match val {
        Value::String(s) => assert_eq!(s, "hello World 4!"),
        other => panic!("expected string value, got {:?}", other),
    }
}

#[test]
fn formatting_preserves_interpolation_and_escapes_literal_marker() {
    let formatted = format_source("\"hi #{name}\"", FormatOptions::default()).expect("format");
    let parsed = Reader::new(&formatted)
        .read_all()
        .expect("parse formatted interpolation");
    assert!(matches!(parsed[0].kind, FormKind::InterpolatedString(_)));

    let escaped_src = "\"\\#{not-interp}\"";
    let escaped_formatted =
        format_source(escaped_src, FormatOptions::default()).expect("format escaped");
    let escaped_parsed = Reader::new(&escaped_formatted)
        .read_all()
        .expect("parse escaped formatted");
    match &escaped_parsed[0].kind {
        FormKind::String(text) => assert_eq!(text, "#{not-interp}"),
        FormKind::InterpolatedString(_) => {
            panic!("escaped interpolation marker should stay literal")
        }
        other => panic!("unexpected form {:?}", other),
    }
}

#[test]
fn formatting_allows_escaped_quotes_inside_interpolation() {
    let src = "\"value #{(str \\\"x\\\" i1)}\"";
    let formatted = format_source(src, FormatOptions::default())
        .expect("format interpolation with escaped quotes");
    assert_eq!(formatted, "\"value #{(str \"x\" i1)}\"\n");
}
