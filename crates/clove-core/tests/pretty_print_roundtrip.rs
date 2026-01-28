use clove_core::pretty_print::{pretty_print_source, PrettyOptions};

#[path = "common/mod.rs"]
mod common;

use common::{assert_forms_equivalent, parse_forms, read_example};

#[test]
fn pretty_print_roundtrip_snake_state() {
    let source = read_example("snake_state.clv");
    let original_forms = parse_forms(&source);
    let printed =
        pretty_print_source(&source, PrettyOptions::default()).expect("pretty print failed");
    let printed_forms = parse_forms(&printed);
    assert_forms_equivalent(&original_forms, &printed_forms, "pretty_print snake_state");
}

#[test]
fn pretty_print_is_idempotent() {
    let source = read_example("snake_state.clv");
    let options = PrettyOptions::default();
    let printed_once = pretty_print_source(&source, options.clone()).expect("pretty print failed");
    let printed_twice =
        pretty_print_source(&printed_once, options).expect("second pretty print failed");
    assert_eq!(
        printed_once, printed_twice,
        "pretty_print should be idempotent"
    );
}

#[test]
fn pretty_print_output_is_parseable() {
    for file in ["snake_state.clv", "snake_terminal.clv"] {
        let source = read_example(file);
        let printed =
            pretty_print_source(&source, PrettyOptions::default()).expect("pretty print failed");
        parse_forms(&printed);
    }
}
