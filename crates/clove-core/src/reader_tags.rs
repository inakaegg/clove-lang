use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{Form, FormKind, MapItem, Span};
use crate::error::CloveError;
use crate::reader::{Reader, TagHandler};

pub fn json_tag_handler() -> TagHandler {
    Arc::new(
        |reader: &mut Reader, start: Span| -> Result<Form, CloveError> {
            reader.skip_ws_and_comments();
            let first_char = if reader.eof() {
                None
            } else {
                Some(reader.current_char())
            };

            match first_char {
                Some('{') => {
                    // Object: restore braces for serde_json
                    let content = reader.read_braced_block_content("#json", false)?;
                    let buf = format!("{{{}}}", content);
                    let json: serde_json::Value = serde_json::from_str(&buf).map_err(|e| {
                        CloveError::parse(format!("invalid JSON in #json block: {}", e))
                    })?;
                    Ok(json_value_to_form(&json, start))
                }
                Some('[') => {
                    let raw = reader.read_raw_form_string()?;
                    let json: serde_json::Value = serde_json::from_str(&raw).map_err(|e| {
                        CloveError::parse(format!("invalid JSON in #json block: {}", e))
                    })?;
                    Ok(json_value_to_form(&json, start))
                }
                Some('"') => {
                    let raw = reader.read_raw_form_string()?;
                    let json: serde_json::Value = serde_json::from_str(&raw).map_err(|e| {
                        CloveError::parse(format!("invalid JSON in #json block: {}", e))
                    })?;
                    Ok(json_value_to_form(&json, start))
                }
                _ => reader.parse_err("expected '{', '[', or '\"' after #json"),
            }
        },
    )
}

pub fn yaml_tag_handler() -> TagHandler {
    Arc::new(
        |reader: &mut Reader, start: Span| -> Result<Form, CloveError> {
            reader.skip_ws_and_comments();
            let first_char = if reader.eof() {
                None
            } else {
                Some(reader.current_char())
            };

            match first_char {
                Some('{') => {
                    let content = reader.read_braced_block_content("#yaml", true)?;
                    let json: serde_json::Value = serde_yaml::from_str(&content).map_err(|e| {
                        CloveError::parse(format!("invalid YAML in #yaml block: {}", e))
                    })?;
                    Ok(json_value_to_form(&json, start))
                }
                Some('"') => {
                    let raw = reader.read_raw_form_string()?;
                    let json: serde_json::Value = serde_yaml::from_str(&raw).map_err(|e| {
                        CloveError::parse(format!("invalid YAML in #yaml block: {}", e))
                    })?;
                    Ok(json_value_to_form(&json, start))
                }
                _ => reader.parse_err("expected '{' or '\"' after #yaml"),
            }
        },
    )
}

pub fn builtin_tag_handlers() -> HashMap<String, TagHandler> {
    let mut map = HashMap::new();
    map.insert("json".to_string(), json_tag_handler());
    map.insert("yaml".to_string(), yaml_tag_handler());
    map
}

fn json_value_to_form(value: &serde_json::Value, span: Span) -> Form {
    match value {
        serde_json::Value::Null => Form::new(FormKind::Nil, span),
        serde_json::Value::Bool(b) => Form::new(FormKind::Bool(*b), span),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Form::new(FormKind::Int(i), span)
            } else if let Some(f) = n.as_f64() {
                Form::new(FormKind::Float(f), span)
            } else {
                Form::new(FormKind::String(n.to_string()), span)
            }
        }
        serde_json::Value::String(s) => Form::new(FormKind::String(s.clone()), span),
        serde_json::Value::Array(items) => {
            let forms = items.iter().map(|v| json_value_to_form(v, span)).collect();
            Form::new(FormKind::Vector(forms), span)
        }
        serde_json::Value::Object(map) => {
            let mut entries = Vec::new();
            for (k, v) in map {
                let key_form = Form::new(FormKind::String(k.clone()), span);
                let value_form = json_value_to_form(v, span);
                entries.push(MapItem::KeyValue(key_form, value_form));
            }
            Form::new(FormKind::Map(entries), span)
        }
    }
}
