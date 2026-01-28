use std::collections::BTreeMap;
use std::rc::Rc;

use clove2_core::type_check::{as_type, expect_type, matches_type};
use clove2_core::types::Type;
use clove2_core::value::{Key, Value};

#[test]
fn matches_optional_union() {
    let ty = Type::Union(vec![Type::Int, Type::Nil]);
    assert!(matches_type(&Value::Int(3), &ty));
    assert!(matches_type(&Value::Nil, &ty));
    assert!(!matches_type(&Value::Bool(true), &ty));
}

#[test]
fn matches_object_closed() {
    let mut fields = BTreeMap::new();
    fields.insert("port".to_string(), Type::Int);
    fields.insert("host".to_string(), Type::Str);
    let ty = Type::Object(fields);

    let mut map = BTreeMap::new();
    map.insert(Key::Str(Rc::from("port")), Value::Int(8080));
    map.insert(
        Key::Str(Rc::from("host")),
        Value::Str("localhost".to_string()),
    );
    assert!(matches_type(&Value::map(map.clone()), &ty));

    map.insert(Key::Str(Rc::from("extra")), Value::Bool(true));
    assert!(!matches_type(&Value::map(map), &ty));
}

#[test]
fn expect_and_as() {
    let ty = Type::Number;
    let val = Value::Float(1.5);
    assert!(expect_type(&val, &ty).is_ok());
    assert!(as_type(&val, &ty).is_some());

    let bad = Value::Str("x".to_string());
    assert!(expect_type(&bad, &ty).is_err());
    assert!(as_type(&bad, &ty).is_none());
}
