use std::collections::BTreeMap;
use std::rc::Rc;

use crate::error::Clove2Error;
use crate::types::Type;
use crate::value::{Key, Value};

pub fn matches_type(value: &Value, ty: &Type) -> bool {
    match ty {
        Type::Any | Type::Dyn | Type::Named(_) => true,
        Type::DynOf(inner) => matches_type(value, inner),
        Type::Nil => matches!(value, Value::Nil),
        Type::Bool => matches!(value, Value::Bool(_)),
        Type::Int => matches!(value, Value::Int(_)),
        Type::Float => matches!(value, Value::Float(_)),
        Type::Number => matches!(value, Value::Int(_) | Value::Float(_)),
        Type::Str => matches!(value, Value::Str(_) | Value::Regex(_)),
        Type::Keyword => matches!(value, Value::Keyword(_)),
        Type::Symbol => matches!(value, Value::Symbol(_)),
        Type::Vec(inner) => match value {
            Value::Vec(items) => items.iter().all(|item| matches_type(item, inner)),
            Value::List(items) | Value::Set(items) => {
                items.iter().all(|item| matches_type(item, inner))
            }
            _ => false,
        },
        Type::Tuple(items) => match value {
            Value::Vec(values) => {
                if values.len() != items.len() {
                    return false;
                }
                values
                    .iter()
                    .zip(items.iter())
                    .all(|(value, ty)| matches_type(value, ty))
            }
            _ => false,
        },
        Type::Map(key, val) => match value {
            Value::Map(map) => map
                .iter()
                .all(|(k, v)| matches_key_type(k, key) && matches_type(v, val)),
            _ => false,
        },
        Type::Shape(shape) => match value {
            Value::Map(map) => matches_shape_fields(map, &shape.fields, shape.open),
            _ => false,
        },
        Type::Object(fields) => match value {
            Value::Map(map) => matches_object_fields(map, fields),
            _ => false,
        },
        Type::Union(items) => items.iter().any(|item| matches_type(value, item)),
        Type::Function { .. } => {
            matches!(
                value,
                Value::Function(_)
                    | Value::NativeFunction(_)
                    | Value::Builtin(_)
                    | Value::Partial { .. }
            )
        }
    }
}

pub fn expect_type<'a>(value: &'a Value, ty: &Type) -> Result<&'a Value, Clove2Error> {
    if matches_type(value, ty) {
        Ok(value)
    } else {
        Err(Clove2Error::new(format!(
            "type mismatch: expected {}, got {}",
            ty,
            value_kind(value)
        )))
    }
}

pub fn as_type<'a>(value: &'a Value, ty: &Type) -> Option<&'a Value> {
    if matches_type(value, ty) {
        Some(value)
    } else {
        None
    }
}

fn matches_key_type(key: &Key, ty: &Type) -> bool {
    match ty {
        Type::Union(items) => items.iter().any(|item| matches_key_type(key, item)),
        Type::Str => matches!(key, Key::Str(_)),
        Type::Keyword => matches!(key, Key::Keyword(_)),
        Type::Symbol => matches!(key, Key::Symbol(_)),
        Type::Bool => matches!(key, Key::Bool(_)),
        Type::Int => matches!(key, Key::Int(_)),
        Type::Any | Type::Dyn | Type::Named(_) => true,
        Type::DynOf(inner) => matches_key_type(key, inner),
        _ => false,
    }
}

fn matches_object_fields(map: &BTreeMap<Key, Value>, fields: &BTreeMap<String, Type>) -> bool {
    for (field, ty) in fields {
        let key = Key::Str(Rc::from(field.as_str()));
        let val = map.get(&key).unwrap_or(&Value::Nil);
        if !matches_type(val, ty) {
            return false;
        }
    }

    for key in map.keys() {
        let Key::Str(name) = key else {
            return false;
        };
        if !fields.contains_key(name.as_ref()) {
            return false;
        }
    }

    true
}

fn matches_shape_fields(
    map: &BTreeMap<Key, Value>,
    fields: &BTreeMap<String, Type>,
    open: bool,
) -> bool {
    for (field, ty) in fields {
        let key = Key::Keyword(Rc::from(field.as_str()));
        let val = map.get(&key).unwrap_or(&Value::Nil);
        if !matches_type(val, ty) {
            return false;
        }
    }

    for key in map.keys() {
        let Key::Keyword(name) = key else {
            return false;
        };
        if !open && !fields.contains_key(name.as_ref()) {
            return false;
        }
    }

    true
}

fn value_kind(value: &Value) -> &'static str {
    match value {
        Value::Nil => "Nil",
        Value::Bool(_) => "Bool",
        Value::Int(_) => "Int",
        Value::Float(_) => "Float",
        Value::Str(_) => "Str",
        Value::Regex(_) => "Regex",
        Value::Keyword(_) => "Keyword",
        Value::Symbol(_) => "Symbol",
        Value::Vec(_) => "Vec",
        Value::List(_) => "List",
        Value::Set(_) => "Set",
        Value::Map(_) => "Map",
        Value::Function(_)
        | Value::NativeFunction(_)
        | Value::Builtin(_)
        | Value::Partial { .. } => "Fn",
    }
}
