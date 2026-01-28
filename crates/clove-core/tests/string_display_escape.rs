use clove_core::ast::HashMap;
use clove_core::ast::{Key, Value};

#[test]
fn value_string_display_escapes_basic() {
    assert_eq!(Value::String("a\"b".into()).to_string(), "\"a\\\"b\"");
    assert_eq!(Value::String("a\\b".into()).to_string(), "\"a\\\\b\"");
    assert_eq!(Value::String("a\nb".into()).to_string(), "\"a\\nb\"");
}

#[test]
fn value_string_display_escapes_interpolation_marker() {
    assert_eq!(Value::String("#{x}".into()).to_string(), "\"\\#{x}\"");
}

#[test]
fn map_key_string_display_escapes() {
    let mut map = HashMap::new();
    map.insert(Key::String("a\"b".into()), Value::Nil);
    let rendered = Value::Map(map).to_string();
    assert_eq!(rendered, "{\"a\\\"b\" nil}");
}
