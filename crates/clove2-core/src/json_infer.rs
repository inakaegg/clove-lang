use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::error::Clove2Error;
use crate::types::Type;

#[derive(Clone, Debug)]
pub struct JsonInference {
    pub ty: Type,
    pub schema: Value,
}

#[derive(Clone, Debug)]
pub struct JsonSnapshot {
    pub version: u32,
    pub source: String,
    pub content_hash: Option<String>,
    pub ty: Type,
    pub schema: Value,
}

pub fn infer_json_type(value: &Value) -> Type {
    infer_json_type_with_limit(value, 2)
}

pub fn infer_json_schema(value: &Value) -> JsonInference {
    let ty = infer_json_type_with_limit(value, 2);
    let schema = schema_from_type(&ty);
    JsonInference { ty, schema }
}

pub fn snapshot_path(source_path: &Path) -> PathBuf {
    let canonical =
        std::fs::canonicalize(source_path).unwrap_or_else(|_| source_path.to_path_buf());
    let mut hasher = Sha256::new();
    hasher.update(canonical.as_os_str().to_string_lossy().as_bytes());
    let hash = hasher.finalize();
    let hash_hex = format!("{:x}", hash);
    PathBuf::from("target")
        .join("clove2")
        .join("snapshots")
        .join("json")
        .join(format!("{}.schema.json", hash_hex))
}

pub fn write_snapshot(
    source_path: &Path,
    content: &str,
    inference: &JsonInference,
) -> Result<PathBuf, Clove2Error> {
    let snapshot_path = snapshot_path(source_path);
    let content_hash = content_hash(content);
    if let Some(parent) = snapshot_path.parent() {
        std::fs::create_dir_all(parent).map_err(|err| {
            Clove2Error::new(format!(
                "failed to create snapshot dir {}: {}",
                parent.display(),
                err
            ))
        })?;
    }
    let mut root = serde_json::Map::new();
    root.insert("version".to_string(), Value::Number(1.into()));
    root.insert(
        "source".to_string(),
        Value::String(source_path.to_string_lossy().to_string()),
    );
    root.insert("content_hash".to_string(), Value::String(content_hash));
    root.insert("type".to_string(), inference.schema.clone());
    let snapshot_json = Value::Object(root);
    let snapshot_text = serde_json::to_string_pretty(&snapshot_json).map_err(|err| {
        Clove2Error::new(format!(
            "failed to serialize snapshot {}: {}",
            snapshot_path.display(),
            err
        ))
    })?;
    std::fs::write(&snapshot_path, snapshot_text).map_err(|err| {
        Clove2Error::new(format!(
            "failed to write snapshot {}: {}",
            snapshot_path.display(),
            err
        ))
    })?;
    Ok(snapshot_path)
}

pub fn schema_from_type(ty: &Type) -> Value {
    match ty {
        Type::Any => kind_object("any"),
        Type::Nil => kind_object("nil"),
        Type::Bool => kind_object("bool"),
        Type::Int => kind_object("int"),
        Type::Float => kind_object("float"),
        Type::Number => kind_object("number"),
        Type::Str => kind_object("str"),
        Type::Keyword => kind_object("keyword"),
        Type::Symbol => kind_object("symbol"),
        Type::Vec(inner) => {
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("vec".to_string()));
            map.insert("item".to_string(), schema_from_type(inner));
            Value::Object(map)
        }
        Type::Tuple(items) => {
            let item_union = Type::union(items.clone());
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("vec".to_string()));
            map.insert("item".to_string(), schema_from_type(&item_union));
            Value::Object(map)
        }
        Type::Map(key, value) => {
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("map".to_string()));
            map.insert("key".to_string(), schema_from_type(key));
            map.insert("value".to_string(), schema_from_type(value));
            Value::Object(map)
        }
        Type::Shape(shape) => {
            let mut field_map = serde_json::Map::new();
            for (key, value) in &shape.fields {
                field_map.insert(key.clone(), schema_from_type(value));
            }
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("object".to_string()));
            map.insert("fields".to_string(), Value::Object(field_map));
            if shape.open {
                map.insert("open".to_string(), Value::Bool(true));
            }
            Value::Object(map)
        }
        Type::Object(fields) => {
            let mut field_map = serde_json::Map::new();
            for (key, value) in fields {
                field_map.insert(key.clone(), schema_from_type(value));
            }
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("object".to_string()));
            map.insert("fields".to_string(), Value::Object(field_map));
            Value::Object(map)
        }
        Type::Union(items) => {
            let items = items.iter().map(schema_from_type).collect();
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("union".to_string()));
            map.insert("items".to_string(), Value::Array(items));
            Value::Object(map)
        }
        Type::Dyn => kind_object("dyn"),
        Type::DynOf(inner) => {
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("dyn".to_string()));
            map.insert("inner".to_string(), schema_from_type(inner));
            Value::Object(map)
        }
        Type::Named(name) => {
            let mut map = serde_json::Map::new();
            map.insert("kind".to_string(), Value::String("named".to_string()));
            map.insert("name".to_string(), Value::String(name.clone()));
            Value::Object(map)
        }
        Type::Function { params, rest, ret } => {
            let mut map = serde_json::Map::new();
            let params = params.iter().map(schema_from_type).collect();
            map.insert("kind".to_string(), Value::String("fn".to_string()));
            map.insert("params".to_string(), Value::Array(params));
            if let Some(rest) = rest {
                map.insert("rest".to_string(), schema_from_type(rest));
            }
            map.insert("ret".to_string(), schema_from_type(ret));
            Value::Object(map)
        }
    }
}

pub fn schema_to_type(schema: &Value) -> Result<Type, Clove2Error> {
    let Value::Object(map) = schema else {
        return Err(Clove2Error::new("schema must be an object"));
    };
    let kind = map
        .get("kind")
        .and_then(Value::as_str)
        .ok_or_else(|| Clove2Error::new("schema missing kind"))?;

    match kind {
        "any" => Ok(Type::Any),
        "nil" => Ok(Type::Nil),
        "bool" => Ok(Type::Bool),
        "int" => Ok(Type::Int),
        "float" => Ok(Type::Float),
        "number" => Ok(Type::Number),
        "str" => Ok(Type::Str),
        "keyword" => Ok(Type::Keyword),
        "symbol" => Ok(Type::Symbol),
        "vec" => {
            let item = map
                .get("item")
                .ok_or_else(|| Clove2Error::new("vec schema missing item"))?;
            let inner = schema_to_type(item)?;
            Ok(Type::Vec(Box::new(inner)))
        }
        "map" => {
            let key = map
                .get("key")
                .ok_or_else(|| Clove2Error::new("map schema missing key"))?;
            let value = map
                .get("value")
                .ok_or_else(|| Clove2Error::new("map schema missing value"))?;
            let key_ty = schema_to_type(key)?;
            let val_ty = schema_to_type(value)?;
            Ok(Type::Map(Box::new(key_ty), Box::new(val_ty)))
        }
        "object" => {
            let fields = map
                .get("fields")
                .ok_or_else(|| Clove2Error::new("object schema missing fields"))?;
            let Value::Object(fields_map) = fields else {
                return Err(Clove2Error::new("object fields must be an object"));
            };
            let mut out = BTreeMap::new();
            for (key, val) in fields_map {
                out.insert(key.clone(), schema_to_type(val)?);
            }
            Ok(Type::Object(out))
        }
        "union" => {
            let items = map
                .get("items")
                .ok_or_else(|| Clove2Error::new("union schema missing items"))?;
            let Value::Array(items) = items else {
                return Err(Clove2Error::new("union items must be an array"));
            };
            let mut out = Vec::new();
            for item in items {
                out.push(schema_to_type(item)?);
            }
            Ok(Type::Union(out))
        }
        "dyn" => {
            if let Some(inner) = map.get("inner") {
                Ok(Type::DynOf(Box::new(schema_to_type(inner)?)))
            } else {
                Ok(Type::Dyn)
            }
        }
        "named" => {
            let name = map
                .get("name")
                .and_then(Value::as_str)
                .ok_or_else(|| Clove2Error::new("named schema missing name"))?;
            Ok(Type::Named(name.to_string()))
        }
        "fn" => {
            let params = map
                .get("params")
                .ok_or_else(|| Clove2Error::new("fn schema missing params"))?;
            let Value::Array(params) = params else {
                return Err(Clove2Error::new("fn params must be an array"));
            };
            let mut out = Vec::new();
            for param in params {
                out.push(schema_to_type(param)?);
            }
            let rest = if let Some(rest) = map.get("rest") {
                Some(Box::new(schema_to_type(rest)?))
            } else {
                None
            };
            let ret = map
                .get("ret")
                .ok_or_else(|| Clove2Error::new("fn schema missing ret"))?;
            let ret = schema_to_type(ret)?;
            Ok(Type::Function {
                params: out,
                rest,
                ret: Box::new(ret),
            })
        }
        other => Err(Clove2Error::new(format!("unknown schema kind: {}", other))),
    }
}

pub fn read_snapshot(path: &Path) -> Result<JsonSnapshot, Clove2Error> {
    let content = std::fs::read_to_string(path).map_err(|err| {
        Clove2Error::new(format!(
            "failed to read snapshot {}: {}",
            path.display(),
            err
        ))
    })?;
    let value: Value = serde_json::from_str(&content).map_err(|err| {
        Clove2Error::new(format!(
            "failed to parse snapshot {}: {}",
            path.display(),
            err
        ))
    })?;
    let Value::Object(map) = value else {
        return Err(Clove2Error::new("snapshot must be an object"));
    };
    let version = map
        .get("version")
        .and_then(Value::as_u64)
        .ok_or_else(|| Clove2Error::new("snapshot missing version"))?;
    let source = map
        .get("source")
        .and_then(Value::as_str)
        .ok_or_else(|| Clove2Error::new("snapshot missing source"))?
        .to_string();
    let schema = map
        .get("type")
        .ok_or_else(|| Clove2Error::new("snapshot missing type"))?;
    let ty = schema_to_type(schema)?;
    let content_hash = map
        .get("content_hash")
        .and_then(Value::as_str)
        .map(|value| value.to_string());
    Ok(JsonSnapshot {
        version: version as u32,
        source,
        content_hash,
        ty,
        schema: schema.clone(),
    })
}

pub fn read_snapshot_type(path: &Path) -> Result<Type, Clove2Error> {
    read_snapshot(path).map(|snapshot| snapshot.ty)
}

pub fn validate_json_value(value: &Value, ty: &Type) -> bool {
    match ty {
        Type::Any | Type::Dyn => true,
        Type::DynOf(inner) => validate_json_value(value, inner),
        Type::Named(_) => true,
        Type::Nil => matches!(value, Value::Null),
        Type::Bool => matches!(value, Value::Bool(_)),
        Type::Int => matches!(value, Value::Number(num) if num.is_i64() || num.is_u64()),
        Type::Float => matches!(value, Value::Number(num) if num.is_f64()),
        Type::Number => matches!(value, Value::Number(_)),
        Type::Str | Type::Keyword | Type::Symbol => matches!(value, Value::String(_)),
        Type::Vec(inner) => match value {
            Value::Array(items) => items.iter().all(|item| validate_json_value(item, inner)),
            _ => false,
        },
        Type::Tuple(items) => match value {
            Value::Array(values) => {
                let item_union = Type::union(items.clone());
                values
                    .iter()
                    .all(|item| validate_json_value(item, &item_union))
            }
            _ => false,
        },
        Type::Map(key, val) => match value {
            Value::Object(map) => {
                if !matches!(**key, Type::Str | Type::Keyword) {
                    return false;
                }
                map.values().all(|item| validate_json_value(item, val))
            }
            _ => false,
        },
        Type::Shape(shape) => match value {
            Value::Object(map) => {
                for (key, ty) in &shape.fields {
                    let field_value = map.get(key).unwrap_or(&Value::Null);
                    if !validate_json_value(field_value, ty) {
                        return false;
                    }
                }
                for key in map.keys() {
                    if !shape.open && !shape.fields.contains_key(key) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        Type::Object(fields) => match value {
            Value::Object(map) => {
                for (key, ty) in fields {
                    let field_value = map.get(key).unwrap_or(&Value::Null);
                    if !validate_json_value(field_value, ty) {
                        return false;
                    }
                }
                for key in map.keys() {
                    if !fields.contains_key(key) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        },
        Type::Union(items) => items.iter().any(|item| validate_json_value(value, item)),
        Type::Function { .. } => false,
    }
}

fn infer_json_type_with_limit(value: &Value, union_limit: usize) -> Type {
    match value {
        Value::Null => Type::Nil,
        Value::Bool(_) => Type::Bool,
        Value::Number(num) => {
            if num.is_i64() || num.is_u64() {
                Type::Int
            } else {
                Type::Float
            }
        }
        Value::String(_) => Type::Str,
        Value::Array(items) => {
            if items.is_empty() {
                return Type::Vec(Box::new(Type::Dyn));
            }
            let mut iter = items.iter();
            let Some(first) = iter.next() else {
                return Type::Vec(Box::new(Type::Dyn));
            };
            let mut acc = infer_json_type_with_limit(first, union_limit);
            for item in iter {
                let next = infer_json_type_with_limit(item, union_limit);
                acc = merge_types(acc, next, union_limit, MergeContext::ArrayItem);
            }
            Type::Vec(Box::new(acc))
        }
        Value::Object(map) => {
            let mut fields = BTreeMap::new();
            for (key, val) in map {
                fields.insert(key.clone(), infer_json_type_with_limit(val, union_limit));
            }
            Type::Object(fields)
        }
    }
}

fn kind_object(kind: &str) -> Value {
    let mut map = serde_json::Map::new();
    map.insert("kind".to_string(), Value::String(kind.to_string()));
    Value::Object(map)
}

pub(crate) fn content_hash(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}

#[derive(Copy, Clone, Debug)]
enum MergeContext {
    ArrayItem,
    ObjectField,
    Other,
}

fn merge_types(a: Type, b: Type, union_limit: usize, context: MergeContext) -> Type {
    if a == b {
        return a;
    }

    if matches!(a, Type::Dyn) || matches!(b, Type::Dyn) {
        return Type::Dyn;
    }

    match (&a, &b) {
        (Type::Int, Type::Float) | (Type::Float, Type::Int) => Type::Number,
        (Type::Number, Type::Int) | (Type::Number, Type::Float) => Type::Number,
        (Type::Int, Type::Number) | (Type::Float, Type::Number) => Type::Number,
        (Type::Vec(a_item), Type::Vec(b_item)) => {
            let merged = merge_types(
                (**a_item).clone(),
                (**b_item).clone(),
                union_limit,
                MergeContext::ArrayItem,
            );
            Type::Vec(Box::new(merged))
        }
        (Type::Tuple(a_items), Type::Tuple(b_items)) => {
            let a_union = Type::union(a_items.clone());
            let b_union = Type::union(b_items.clone());
            let merged = merge_types(a_union, b_union, union_limit, MergeContext::ArrayItem);
            Type::Vec(Box::new(merged))
        }
        (Type::Vec(a_item), Type::Tuple(b_items)) => {
            let b_union = Type::union(b_items.clone());
            let merged = merge_types(
                (**a_item).clone(),
                b_union,
                union_limit,
                MergeContext::ArrayItem,
            );
            Type::Vec(Box::new(merged))
        }
        (Type::Tuple(a_items), Type::Vec(b_item)) => {
            let a_union = Type::union(a_items.clone());
            let merged = merge_types(
                a_union,
                (**b_item).clone(),
                union_limit,
                MergeContext::ArrayItem,
            );
            Type::Vec(Box::new(merged))
        }
        (Type::Shape(a_shape), Type::Shape(b_shape)) => {
            merge_object_types(&a_shape.fields, &b_shape.fields, union_limit)
        }
        (Type::Shape(a_shape), Type::Object(b_fields))
        | (Type::Object(b_fields), Type::Shape(a_shape)) => {
            merge_object_types(&a_shape.fields, b_fields, union_limit)
        }
        (Type::Object(a_fields), Type::Object(b_fields)) => {
            merge_object_types(a_fields, b_fields, union_limit)
        }
        (Type::Map(a_key, a_val), Type::Map(b_key, b_val)) => {
            let key = merge_types(
                (**a_key).clone(),
                (**b_key).clone(),
                union_limit,
                MergeContext::Other,
            );
            let val = merge_types(
                (**a_val).clone(),
                (**b_val).clone(),
                union_limit,
                MergeContext::Other,
            );
            Type::Map(Box::new(key), Box::new(val))
        }
        _ => merge_union(a, b, union_limit, context),
    }
}

fn merge_union(a: Type, b: Type, union_limit: usize, context: MergeContext) -> Type {
    let mut items = Vec::new();
    push_union_items(&mut items, a);
    push_union_items(&mut items, b);
    normalize_union_items(&mut items);

    if items.len() == 1 {
        return items.remove(0);
    }

    if items.len() > union_limit {
        return match context {
            MergeContext::ArrayItem => Type::Dyn,
            MergeContext::ObjectField => Type::Dyn,
            MergeContext::Other => Type::Dyn,
        };
    }

    Type::Union(items)
}

fn push_union_items(items: &mut Vec<Type>, ty: Type) {
    match ty {
        Type::Union(inner) => {
            for item in inner {
                items.push(item);
            }
        }
        other => items.push(other),
    }
}

fn normalize_union_items(items: &mut Vec<Type>) {
    let mut out: Vec<Type> = Vec::new();
    for item in items.drain(..) {
        if matches!(item, Type::Dyn) {
            out.clear();
            out.push(Type::Dyn);
            break;
        }
        if out.contains(&item) {
            continue;
        }
        out.push(item);
    }

    if out.contains(&Type::Number) {
        out.retain(|t| !matches!(t, Type::Int | Type::Float));
    } else if out.contains(&Type::Int) && out.contains(&Type::Float) {
        out.retain(|t| !matches!(t, Type::Int | Type::Float));
        out.push(Type::Number);
    }

    *items = out;
}

fn merge_object_types(
    a_fields: &BTreeMap<String, Type>,
    b_fields: &BTreeMap<String, Type>,
    union_limit: usize,
) -> Type {
    let mut merged: BTreeMap<String, Type> = BTreeMap::new();
    let mut overflow = false;

    for key in a_fields.keys().chain(b_fields.keys()) {
        if merged.contains_key(key) {
            continue;
        }
        let a_ty = a_fields.get(key);
        let b_ty = b_fields.get(key);
        let combined = match (a_ty, b_ty) {
            (Some(a_ty), Some(b_ty)) => merge_types(
                a_ty.clone(),
                b_ty.clone(),
                union_limit,
                MergeContext::ObjectField,
            ),
            (Some(a_ty), None) | (None, Some(a_ty)) => merge_union(
                a_ty.clone(),
                Type::Nil,
                union_limit,
                MergeContext::ObjectField,
            ),
            (None, None) => Type::Dyn,
        };

        if combined == Type::Dyn {
            overflow = true;
        }
        merged.insert(key.clone(), combined);
    }

    if overflow {
        return Type::Map(Box::new(Type::Str), Box::new(Type::Dyn));
    }

    Type::Object(merged)
}
