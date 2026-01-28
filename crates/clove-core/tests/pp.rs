use clove_core::ast::Value;
use clove_core::eval_source;
use clove_core::reader::Reader;

fn read_all(source: &str) {
    let mut reader = Reader::new(source);
    reader.read_all().expect("pp-str output should parse");
}

#[test]
fn pp_str_output_is_parseable() {
    let out = eval_source("(pp-str {:a 1 :b [1 2 3]})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    read_all(&text);
}

#[test]
fn pp_str_formats_multiline_maps() {
    let out = eval_source("(pp-str {:a 1 :b 2 :c 3 :d 4})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    assert!(text.contains('\n'), "pp-str should include a newline");
    let has_indented = text
        .lines()
        .skip(1)
        .any(|line| line.starts_with(" :b") || line.starts_with("  :b"));
    assert!(has_indented, "pp-str should indent map entries");
}

#[test]
fn pp_str_limits_remain_parseable() {
    let out = eval_source("(pp-str {:a 1 :b 2 :c 3} {:max-items 1})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    assert!(text.contains(":__more__ true"));
    read_all(&text);

    let out = eval_source("(pp-str [1 2 3 4] {:max-items 2})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    assert!(text.contains("..."));
    read_all(&text);

    let out = eval_source("(pp-str {:a {:b {:c 1}}} {:max-depth 1})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    assert!(text.contains(":__more__ true"));
    read_all(&text);
}

#[test]
fn pp_str_stringifies_non_literal_values() {
    let out = eval_source("(pp-str {:cache (atom 1)})", None).expect("pp-str eval");
    let Value::String(text) = out else {
        panic!("pp-str should return a string");
    };
    assert!(
        text.contains("\"#atom<"),
        "pp-str should stringify atom values"
    );
    read_all(&text);
}
