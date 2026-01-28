use crate::ast::{HashMap, Key, SortedMap, SortedSet, Value, Vector};
use crate::error::CloveError;
use crate::guard;
use crate::string_escape::escape_string_fragment;
use im::HashSet;

pub fn format_value(value: &Value) -> Result<String, CloveError> {
    let mut out = String::new();
    write_value(&mut out, value)?;
    Ok(out)
}

fn write_value(out: &mut String, value: &Value) -> Result<(), CloveError> {
    guard::tick(None)?;
    match value {
        Value::Int(n) => out.push_str(&n.to_string()),
        Value::Float(n) => out.push_str(&format_float(*n)),
        Value::String(s) => {
            out.push('"');
            out.push_str(&escape_string_fragment(s));
            out.push('"');
        }
        Value::Bool(b) => out.push_str(&b.to_string()),
        Value::Nil => out.push_str("nil"),
        Value::List(items) => write_list(out, items)?,
        Value::Vector(items) => write_vector(out, items)?,
        Value::Map(map) => write_map(out, map)?,
        Value::SortedMap(map) => write_sorted_map(out, map)?,
        Value::Set(set) => write_set(out, set)?,
        Value::SortedSet(set) => write_sorted_set(out, set)?,
        Value::MutVector(_)
        | Value::MutMap(_)
        | Value::MutSet(_)
        | Value::TransientVector(_)
        | Value::TransientMap(_)
        | Value::TransientSet(_) => out.push_str(&value.to_string()),
        Value::Regex(_) => out.push_str(&value.to_string()),
        Value::Duration(_) => out.push_str(&value.to_string()),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Atom(_)
        | Value::Chan(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_)
        | Value::Agent(_)
        | Value::Delay(_)
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => out.push_str(&value.to_string()),
        Value::Seq(_) => out.push_str("<seq>"),
        Value::NativeBuf { handle, .. } => {
            out.push_str(&crate::native_buf::format_native_buf_handle(handle));
        }
        Value::Symbol(s) => out.push_str(s),
        Value::Foreign(_) => out.push_str("<foreign>"),
    }
    Ok(())
}

fn write_list(out: &mut String, items: &Vector<Value>) -> Result<(), CloveError> {
    out.push('(');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        write_value(out, item)?;
    }
    out.push(')');
    Ok(())
}

fn write_vector(out: &mut String, items: &Vector<Value>) -> Result<(), CloveError> {
    out.push('[');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        write_value(out, item)?;
    }
    out.push(']');
    Ok(())
}

fn write_map(out: &mut String, map: &HashMap<Key, Value>) -> Result<(), CloveError> {
    out.push('{');
    for (idx, (k, v)) in map.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        out.push_str(&format_key(k));
        out.push(' ');
        write_value(out, v)?;
    }
    out.push('}');
    Ok(())
}

fn write_sorted_map(out: &mut String, map: &SortedMap) -> Result<(), CloveError> {
    out.push('{');
    for (idx, (k, v)) in map.entries.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        out.push_str(&format_key(k));
        out.push(' ');
        write_value(out, v)?;
    }
    out.push('}');
    Ok(())
}

fn write_set(out: &mut String, set: &HashSet<Value>) -> Result<(), CloveError> {
    let mut parts = Vec::new();
    for item in set.iter() {
        parts.push(format_value(item)?);
    }
    parts.sort();
    out.push_str("#{");
    out.push_str(&parts.join(" "));
    out.push('}');
    Ok(())
}

fn write_sorted_set(out: &mut String, set: &SortedSet) -> Result<(), CloveError> {
    out.push_str("#{");
    for (idx, item) in set.entries.iter().enumerate() {
        if idx > 0 {
            out.push(' ');
        }
        write_value(out, item)?;
    }
    out.push('}');
    Ok(())
}

fn format_key(k: &Key) -> String {
    match k {
        Key::Keyword(s) => format!(":{}", s),
        Key::Symbol(s) => format!(":{}", s),
        Key::String(s) => format!("\"{}\"", escape_string_fragment(s)),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

fn format_float(n: f64) -> String {
    if n.fract() == 0.0 {
        format!("{:.1}", n)
    } else {
        n.to_string()
    }
}
