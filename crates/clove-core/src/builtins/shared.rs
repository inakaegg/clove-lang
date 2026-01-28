use crate::ast::{
    is_mut_collection, to_imut_deep, HashMap, Key, SortedMap, SortedSet, TransientKind, Value,
    Vector,
};
use crate::error::CloveError;
use crate::eval::{call_callable, key_to_value, to_key_value_checked};
use crate::guard;
use crate::seq::SeqHandle;
use serde_json::Value as JsonValue;
use std::cmp::Ordering;
use std::collections::HashMap as StdHashMap;

const VALUE_PREVIEW_MAX: usize = 120;

fn truncate_preview(text: String) -> String {
    if text.len() <= VALUE_PREVIEW_MAX {
        return text;
    }
    let mut preview = String::with_capacity(VALUE_PREVIEW_MAX + 3);
    for ch in text.chars() {
        if preview.len() + ch.len_utf8() > VALUE_PREVIEW_MAX {
            preview.push_str("...");
            break;
        }
        preview.push(ch);
    }
    preview
}

pub(crate) fn value_preview(v: &Value) -> String {
    truncate_preview(v.to_string())
}

pub(crate) fn actual_type_with_preview(v: &Value) -> String {
    format!("{} ({})", v.type_name(), value_preview(v))
}

pub(crate) fn type_mismatch_arg(
    expected: impl Into<String>,
    op: &str,
    arg_index: usize,
    actual: &Value,
) -> CloveError {
    CloveError::type_mismatch(
        format!("{} (arg {} to {})", expected.into(), arg_index, op),
        actual_type_with_preview(actual),
    )
}

pub(crate) fn expect_string(
    value: &Value,
    op: &str,
    arg_index: usize,
) -> Result<String, CloveError> {
    match value {
        Value::String(s) => Ok(s.clone()),
        other => Err(type_mismatch_arg("string", op, arg_index, other)),
    }
}

pub(crate) fn expect_path_buf(
    value: &Value,
    op: &str,
    arg_index: usize,
) -> Result<std::path::PathBuf, CloveError> {
    match value {
        Value::String(s) | Value::Symbol(s) => Ok(std::path::PathBuf::from(s)),
        other => Err(type_mismatch_arg(
            "path string or symbol",
            op,
            arg_index,
            other,
        )),
    }
}

pub(crate) fn as_number(v: &Value) -> Result<(f64, bool), CloveError> {
    match v {
        Value::Int(n) => Ok((*n as f64, false)),
        Value::Float(n) => Ok((*n, true)),
        _ => Err(CloveError::type_mismatch(
            "number",
            actual_type_with_preview(v),
        )),
    }
}

pub(crate) fn make_number(value: f64, from_float: bool) -> Value {
    if !from_float && value.fract() == 0.0 {
        Value::Int(value as i64)
    } else {
        Value::Float(value)
    }
}

pub(crate) fn sorted_map_get(map: &SortedMap, key: &Key) -> Result<Option<Value>, CloveError> {
    for (k, v) in &map.entries {
        if compare_keys(&map.comparator, k, key)? == Ordering::Equal {
            return Ok(Some(v.clone()));
        }
    }
    Ok(None)
}

#[allow(dead_code)]
pub(crate) fn sorted_map_insert(
    map: &SortedMap,
    key: Key,
    value: Value,
) -> Result<SortedMap, CloveError> {
    let mut entries = map.entries.clone();
    let mut insert_at = entries.len();
    for (idx, (k, _)) in entries.iter().enumerate() {
        match compare_keys(&map.comparator, k, &key)? {
            Ordering::Equal => {
                entries[idx] = (key, value);
                return Ok(SortedMap {
                    comparator: map.comparator.clone(),
                    entries,
                });
            }
            Ordering::Greater => {
                insert_at = idx;
                break;
            }
            Ordering::Less => {}
        }
    }
    entries.insert(insert_at, (key, value));
    Ok(SortedMap {
        comparator: map.comparator.clone(),
        entries,
    })
}

pub(crate) fn sorted_map_insert_mut(
    map: &mut SortedMap,
    key: Key,
    value: Value,
) -> Result<(), CloveError> {
    let mut insert_at = map.entries.len();
    for (idx, (k, _)) in map.entries.iter().enumerate() {
        match compare_keys(&map.comparator, k, &key)? {
            Ordering::Equal => {
                map.entries[idx] = (key, value);
                return Ok(());
            }
            Ordering::Greater => {
                insert_at = idx;
                break;
            }
            Ordering::Less => {}
        }
    }
    map.entries.insert(insert_at, (key, value));
    Ok(())
}

pub(crate) fn sorted_map_remove(map: &SortedMap, key: &Key) -> Result<SortedMap, CloveError> {
    let mut entries = map.entries.clone();
    for idx in 0..entries.len() {
        if compare_keys(&map.comparator, &entries[idx].0, key)? == Ordering::Equal {
            entries.remove(idx);
            break;
        }
    }
    Ok(SortedMap {
        comparator: map.comparator.clone(),
        entries,
    })
}

pub(crate) fn sorted_set_contains(set: &SortedSet, value: &Value) -> Result<bool, CloveError> {
    for item in &set.entries {
        if compare_values(&set.comparator, item, value)? == Ordering::Equal {
            return Ok(true);
        }
    }
    Ok(false)
}

#[allow(dead_code)]
pub(crate) fn sorted_set_insert(set: &SortedSet, value: Value) -> Result<SortedSet, CloveError> {
    let mut entries = set.entries.clone();
    let mut insert_at = entries.len();
    for (idx, item) in entries.iter().enumerate() {
        match compare_values(&set.comparator, item, &value)? {
            Ordering::Equal => {
                return Ok(SortedSet {
                    comparator: set.comparator.clone(),
                    entries,
                })
            }
            Ordering::Greater => {
                insert_at = idx;
                break;
            }
            Ordering::Less => {}
        }
    }
    entries.insert(insert_at, value);
    Ok(SortedSet {
        comparator: set.comparator.clone(),
        entries,
    })
}

pub(crate) fn sorted_set_insert_mut(set: &mut SortedSet, value: Value) -> Result<(), CloveError> {
    let mut insert_at = set.entries.len();
    for (idx, item) in set.entries.iter().enumerate() {
        match compare_values(&set.comparator, item, &value)? {
            Ordering::Equal => return Ok(()),
            Ordering::Greater => {
                insert_at = idx;
                break;
            }
            Ordering::Less => {}
        }
    }
    set.entries.insert(insert_at, value);
    Ok(())
}

pub(crate) fn sorted_set_remove(set: &SortedSet, value: &Value) -> Result<SortedSet, CloveError> {
    let mut entries = Vec::with_capacity(set.entries.len());
    let mut removed = false;
    for item in &set.entries {
        if !removed && compare_values(&set.comparator, item, value)? == Ordering::Equal {
            removed = true;
            continue;
        }
        entries.push(item.clone());
    }
    Ok(SortedSet {
        comparator: set.comparator.clone(),
        entries,
    })
}

pub(crate) fn sorted_map_from_pairs(
    comparator: Value,
    pairs: &[Value],
    op: &str,
    arg_offset: usize,
) -> Result<SortedMap, CloveError> {
    if pairs.len() % 2 != 0 {
        return super::err(format!("{} expects even number of key/value arguments", op));
    }
    let mut out = SortedMap {
        comparator: Box::new(comparator),
        entries: Vec::new(),
    };
    let mut iter = pairs.iter();
    let mut idx = 0;
    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
        let key = key_from_value_strict(k, op, arg_offset + idx)?;
        sorted_map_insert_mut(&mut out, key, v.clone())?;
        idx += 2;
    }
    Ok(out)
}

pub(crate) fn sorted_map_from_entries(
    comparator: Value,
    entries: &[Value],
    op: &str,
) -> Result<SortedMap, CloveError> {
    let mut out = SortedMap {
        comparator: Box::new(comparator),
        entries: Vec::new(),
    };
    for entry in entries {
        match entry {
            Value::Vector(items) | Value::List(items) if items.len() >= 2 => {
                let key = key_from_value_strict(&items[0], op, 1)?;
                sorted_map_insert_mut(&mut out, key, items[1].clone())?;
            }
            other => {
                return Err(type_mismatch_arg("map entry", op, 1, other));
            }
        }
    }
    Ok(out)
}

pub(crate) fn sorted_map_from_map(
    comparator: Value,
    map: &HashMap<Key, Value>,
) -> Result<SortedMap, CloveError> {
    let mut out = SortedMap {
        comparator: Box::new(comparator),
        entries: Vec::new(),
    };
    for (k, v) in map {
        sorted_map_insert_mut(&mut out, k.clone(), v.clone())?;
    }
    Ok(out)
}

pub(crate) fn sorted_map_from_sorted_map(
    comparator: Value,
    map: &SortedMap,
) -> Result<SortedMap, CloveError> {
    let mut out = SortedMap {
        comparator: Box::new(comparator),
        entries: Vec::new(),
    };
    for (k, v) in &map.entries {
        sorted_map_insert_mut(&mut out, k.clone(), v.clone())?;
    }
    Ok(out)
}

pub(crate) fn sorted_set_from_items(
    comparator: Value,
    items: &[Value],
) -> Result<SortedSet, CloveError> {
    let mut out = SortedSet {
        comparator: Box::new(comparator),
        entries: Vec::new(),
    };
    for item in items {
        if is_mut_collection(item) {
            return super::err("cannot put mutable collection into set; use (imut x)");
        }
        sorted_set_insert_mut(&mut out, item.clone())?;
    }
    Ok(out)
}

pub(crate) fn truthy(v: &Value) -> bool {
    !matches!(v, Value::Nil | Value::Bool(false))
}

pub(crate) fn value_eq(a: &Value, b: &Value) -> bool {
    a == b
}

fn key_from_value_strict(value: &Value, op: &str, arg_index: usize) -> Result<Key, CloveError> {
    match value {
        Value::Symbol(s) => {
            let name = s.strip_prefix(':').unwrap_or(s);
            Ok(Key::Keyword(name.to_string()))
        }
        Value::String(s) => Ok(Key::String(s.clone())),
        Value::Int(n) => Ok(Key::Number(*n)),
        Value::Float(n) => Ok(Key::Number(*n as i64)),
        Value::Bool(b) => Ok(Key::Bool(*b)),
        other => Err(type_mismatch_arg("map key", op, arg_index, other)),
    }
}

pub(crate) fn map_like_to_hashmap(
    value: &Value,
    op: &str,
    arg_index: usize,
) -> Result<HashMap<Key, Value>, CloveError> {
    match value {
        Value::Map(map) => Ok(map.clone()),
        Value::MutMap(handle) => Ok(handle.lock().unwrap_or_else(|e| e.into_inner()).clone()),
        Value::SortedMap(map) => Ok(map.entries.iter().cloned().collect()),
        other => Err(type_mismatch_arg("map", op, arg_index, other)),
    }
}

fn compare_keys(comparator: &Value, left: &Key, right: &Key) -> Result<Ordering, CloveError> {
    compare_values(comparator, &key_to_value(left), &key_to_value(right))
}

fn compare_values(comparator: &Value, left: &Value, right: &Value) -> Result<Ordering, CloveError> {
    let res = call_callable(comparator.clone(), vec![left.clone(), right.clone()])?;
    let (num, _) = as_number(&res)?;
    if num.is_nan() {
        return super::err("comparator returned NaN");
    }
    if num < 0.0 {
        Ok(Ordering::Less)
    } else if num > 0.0 {
        Ok(Ordering::Greater)
    } else {
        Ok(Ordering::Equal)
    }
}

pub(crate) fn num_to_isize(n: f64) -> Result<isize, CloveError> {
    if n.fract() != 0.0 {
        return super::err("index must be an integer");
    }
    Ok(n as isize)
}

fn seq_value_from_handle(handle: SeqHandle) -> Result<Value, CloveError> {
    if handle.peek()?.is_some() {
        Ok(Value::Seq(handle))
    } else {
        Ok(Value::Nil)
    }
}

pub(crate) fn seq_from_value(v: Value) -> Result<Value, CloveError> {
    match seq_handle_from_value(v)? {
        Some(handle) => seq_value_from_handle(handle),
        None => Ok(Value::Nil),
    }
}

pub(crate) fn seq_handle_from_value(v: Value) -> Result<Option<SeqHandle>, CloveError> {
    let handle = match v {
        Value::Nil => return Ok(None),
        Value::Seq(handle) => handle,
        Value::List(items) => SeqHandle::from_iter(items.into_iter()),
        Value::Vector(items) => SeqHandle::from_iter(items.into_iter()),
        Value::Set(items) => SeqHandle::from_iter(items.into_iter()),
        Value::SortedSet(items) => SeqHandle::from_iter(items.entries.into_iter()),
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            SeqHandle::from_iter(items)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            SeqHandle::from_iter(items)
        }
        Value::Map(map) => {
            let pairs = map
                .into_iter()
                .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(&k), v])));
            SeqHandle::from_iter(pairs)
        }
        Value::MutMap(handle) => {
            let map = handle.lock().unwrap_or_else(|e| e.into_inner());
            let pairs: Vec<Value> = map
                .iter()
                .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(k), v.clone()])))
                .collect();
            SeqHandle::from_iter(pairs)
        }
        Value::SortedMap(map) => {
            let pairs = map
                .entries
                .into_iter()
                .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(&k), v])));
            SeqHandle::from_iter(pairs)
        }
        Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_) => {
            return Err(CloveError::runtime(
                "seq does not accept transient collections; use persistent!",
            ))
        }
        other => SeqHandle::from_iter(std::iter::once(other)),
    };
    if handle.peek()?.is_some() {
        Ok(Some(handle))
    } else {
        Ok(None)
    }
}

pub(crate) fn seq_items(v: &Value) -> Result<Vector<Value>, CloveError> {
    match v {
        Value::Nil => Ok(Vector::new()),
        Value::Seq(handle) => handle.collect_all(),
        Value::List(items) => Ok(items.clone()),
        Value::Vector(items) => Ok(items.clone()),
        Value::MutVector(handle) => {
            let items = handle.lock().unwrap_or_else(|e| e.into_inner());
            Ok(items.iter().cloned().collect())
        }
        Value::Set(items) => {
            let mut out = Vector::new();
            for item in items.iter() {
                guard::tick(None)?;
                out.push_back(item.clone());
            }
            Ok(out)
        }
        Value::MutSet(handle) => {
            let items = handle.lock().unwrap_or_else(|e| e.into_inner());
            let mut out = Vector::new();
            for item in items.iter() {
                guard::tick(None)?;
                out.push_back(item.clone());
            }
            Ok(out)
        }
        Value::SortedSet(items) => {
            let mut out = Vector::new();
            for item in items.entries.iter() {
                guard::tick(None)?;
                out.push_back(item.clone());
            }
            Ok(out)
        }
        Value::Map(map) => {
            let mut out = Vector::new();
            for (k, v) in map.iter() {
                guard::tick(None)?;
                out.push_back(Value::Vector(Vector::from(vec![
                    key_to_value(k),
                    v.clone(),
                ])));
            }
            Ok(out)
        }
        Value::MutMap(handle) => {
            let map = handle.lock().unwrap_or_else(|e| e.into_inner());
            let mut out = Vector::new();
            for (k, v) in map.iter() {
                guard::tick(None)?;
                out.push_back(Value::Vector(Vector::from(vec![
                    key_to_value(k),
                    v.clone(),
                ])));
            }
            Ok(out)
        }
        Value::SortedMap(map) => {
            let mut out = Vector::new();
            for (k, v) in map.entries.iter() {
                guard::tick(None)?;
                out.push_back(Value::Vector(Vector::from(vec![
                    key_to_value(k),
                    v.clone(),
                ])));
            }
            Ok(out)
        }
        Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_) => Err(
            CloveError::runtime("seq does not accept transient collections; use persistent!"),
        ),
        other => Ok(Vector::from(vec![other.clone()])),
    }
}

pub(crate) fn seq_is_empty(op: &str, arg_index: usize, v: &Value) -> Result<bool, CloveError> {
    match v {
        Value::Nil => Ok(true),
        Value::Seq(handle) => Ok(handle.peek()?.is_none()),
        Value::List(items) => Ok(items.is_empty()),
        Value::Vector(items) => Ok(items.is_empty()),
        Value::MutVector(handle) => Ok(handle.lock().unwrap_or_else(|e| e.into_inner()).is_empty()),
        Value::Set(items) => Ok(items.is_empty()),
        Value::MutSet(handle) => Ok(handle.lock().unwrap_or_else(|e| e.into_inner()).is_empty()),
        Value::SortedSet(items) => Ok(items.entries.is_empty()),
        Value::Map(items) => Ok(items.is_empty()),
        Value::MutMap(handle) => Ok(handle.lock().unwrap_or_else(|e| e.into_inner()).is_empty()),
        Value::SortedMap(items) => Ok(items.entries.is_empty()),
        Value::TransientVector(handle) => handle.read(op, |kind| match kind {
            TransientKind::Vector(items) => Ok(items.is_empty()),
            _ => Err(CloveError::runtime("transient kind mismatch")),
        }),
        Value::TransientMap(handle) => handle.read(op, |kind| match kind {
            TransientKind::Map(items) => Ok(items.is_empty()),
            _ => Err(CloveError::runtime("transient kind mismatch")),
        }),
        Value::TransientSet(handle) => handle.read(op, |kind| match kind {
            TransientKind::Set(items) => Ok(items.is_empty()),
            _ => Err(CloveError::runtime("transient kind mismatch")),
        }),
        Value::String(s) => Ok(s.is_empty()),
        other => Err(type_mismatch_arg("collection", op, arg_index, other)),
    }
}

pub(crate) fn conj_on_value(coll: Value, item: Value) -> Result<Value, CloveError> {
    match coll {
        Value::List(mut v) => Ok(Value::List({
            v.push_front(to_imut_deep(&item)?);
            v
        })),
        Value::Vector(mut v) => Ok(Value::Vector({
            v.push_back(to_imut_deep(&item)?);
            v
        })),
        Value::Set(mut s) => {
            if is_mut_collection(&item) {
                return super::err("cannot put mutable collection into set; use (imut x)");
            }
            let converted = to_imut_deep(&item)?;
            s.insert(converted);
            Ok(Value::Set(s))
        }
        Value::SortedSet(s) => {
            if is_mut_collection(&item) {
                return super::err("cannot put mutable collection into set; use (imut x)");
            }
            let converted = to_imut_deep(&item)?;
            let mut out = s;
            sorted_set_insert_mut(&mut out, converted)?;
            Ok(Value::SortedSet(out))
        }
        Value::Map(m) => conj_map(&m, std::slice::from_ref(&item)),
        Value::SortedMap(m) => Ok(Value::SortedMap(conj_sorted_map(
            &m,
            std::slice::from_ref(&item),
        )?)),
        Value::Nil => Ok(Value::List(Vector::from(vec![to_imut_deep(&item)?]))),
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => {
            let frozen = to_imut_deep(&coll)?;
            conj_on_value(frozen, item)
        }
        _ => super::err("into/conj target must be a collection"),
    }
}

pub(crate) fn assoc_map(base: &HashMap<Key, Value>, pairs: &[Value]) -> Result<Value, CloveError> {
    if pairs.len() % 2 != 0 {
        return super::err("assoc expects even number of key/value arguments");
    }
    let mut new_map = base.clone();
    let mut iter = pairs.iter();
    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
        let key = to_key_value_checked(k)?;
        let val = to_imut_deep(v)?;
        new_map.insert(key, val);
    }
    Ok(Value::Map(new_map))
}

pub(crate) fn conj_map(base: &HashMap<Key, Value>, items: &[Value]) -> Result<Value, CloveError> {
    let mut new_map = base.clone();
    for item in items {
        match item {
            Value::Vector(v) | Value::List(v) if v.len() == 2 => {
                let key = to_key_value_checked(&v[0])?;
                let val = to_imut_deep(&v[1])?;
                new_map.insert(key, val);
            }
            Value::Map(m) => {
                for (k, v) in m.iter() {
                    new_map.insert(k.clone(), to_imut_deep(v)?);
                }
            }
            Value::SortedMap(m) => {
                for (k, v) in &m.entries {
                    new_map.insert(k.clone(), to_imut_deep(v)?);
                }
            }
            _ => return super::err("conj on map expects map or [k v]"),
        }
    }
    Ok(Value::Map(new_map))
}

pub(crate) fn conj_sorted_map(base: &SortedMap, items: &[Value]) -> Result<SortedMap, CloveError> {
    let mut out = base.clone();
    for item in items {
        match item {
            Value::Vector(v) | Value::List(v) if v.len() == 2 => {
                let key = to_key_value_checked(&v[0])?;
                let val = to_imut_deep(&v[1])?;
                sorted_map_insert_mut(&mut out, key, val)?;
            }
            Value::Map(m) => {
                for (k, v) in m.iter() {
                    sorted_map_insert_mut(&mut out, k.clone(), to_imut_deep(v)?)?;
                }
            }
            Value::SortedMap(m) => {
                for (k, v) in &m.entries {
                    sorted_map_insert_mut(&mut out, k.clone(), to_imut_deep(v)?)?;
                }
            }
            _ => return super::err("conj on map expects map or [k v]"),
        }
    }
    Ok(out)
}

fn append_vector_item(out: &mut Vector<Value>, item: Value) -> Result<(), CloveError> {
    out.push_back(to_imut_deep(&item)?);
    Ok(())
}

fn append_list_item(out: &mut Vector<Value>, item: Value) -> Result<(), CloveError> {
    out.push_front(to_imut_deep(&item)?);
    Ok(())
}

fn append_items_with<F>(
    mut out: Vector<Value>,
    source: Value,
    mut push: F,
) -> Result<Vector<Value>, CloveError>
where
    F: FnMut(&mut Vector<Value>, Value) -> Result<(), CloveError>,
{
    match source {
        Value::List(items) | Value::Vector(items) => {
            for item in items {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::Set(items) => {
            for item in items {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::SortedSet(items) => {
            for item in items.entries {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::Map(map) => {
            for (k, v) in map {
                guard::tick(None)?;
                let item = Value::Vector(Vector::from(vec![key_to_value(&k), v]));
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::MutMap(handle) => {
            let entries: Vec<(Key, Value)> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (k, v) in entries {
                guard::tick(None)?;
                let item = Value::Vector(Vector::from(vec![key_to_value(&k), v]));
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::SortedMap(map) => {
            for (k, v) in map.entries {
                guard::tick(None)?;
                let item = Value::Vector(Vector::from(vec![key_to_value(&k), v]));
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::String(s) => {
            for c in s.chars() {
                guard::tick(None)?;
                push(&mut out, Value::String(c.to_string()))?;
            }
            Ok(out)
        }
        Value::Seq(handle) => {
            let collected = handle.collect_all()?;
            for item in collected {
                guard::tick(None)?;
                push(&mut out, item)?;
            }
            Ok(out)
        }
        Value::Nil => Ok(out),
        other => super::err(format!(
            "into expects collection as second arg, got {}",
            other.type_name()
        )),
    }
}

fn append_map_item(target: &mut HashMap<Key, Value>, item: Value) -> Result<(), CloveError> {
    match item {
        Value::Vector(v) | Value::List(v) if v.len() == 2 => {
            let key = to_key_value_checked(&v[0])?;
            let val = to_imut_deep(&v[1])?;
            target.insert(key, val);
            Ok(())
        }
        Value::Map(m) => {
            for (k, v) in m {
                target.insert(k, to_imut_deep(&v)?);
            }
            Ok(())
        }
        Value::SortedMap(m) => {
            for (k, v) in m.entries {
                target.insert(k, to_imut_deep(&v)?);
            }
            Ok(())
        }
        _ => super::err("conj on map expects map or [k v]"),
    }
}

fn append_into_map(
    mut target: HashMap<Key, Value>,
    source: Value,
) -> Result<HashMap<Key, Value>, CloveError> {
    match source {
        Value::List(items) | Value::Vector(items) => {
            for item in items {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Set(items) => {
            for item in items {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::SortedSet(items) => {
            for item in items.entries {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Map(map) => {
            for (k, v) in map {
                guard::tick(None)?;
                target.insert(k, to_imut_deep(&v)?);
            }
            Ok(target)
        }
        Value::MutMap(handle) => {
            let entries: Vec<(Key, Value)> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (k, v) in entries {
                guard::tick(None)?;
                target.insert(k, to_imut_deep(&v)?);
            }
            Ok(target)
        }
        Value::SortedMap(map) => {
            for (k, v) in map.entries {
                guard::tick(None)?;
                target.insert(k, to_imut_deep(&v)?);
            }
            Ok(target)
        }
        Value::String(s) => {
            for c in s.chars() {
                guard::tick(None)?;
                append_map_item(&mut target, Value::String(c.to_string()))?;
            }
            Ok(target)
        }
        Value::Seq(handle) => {
            let collected = handle.collect_all()?;
            for item in collected {
                guard::tick(None)?;
                append_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Nil => Ok(target),
        other => super::err(format!(
            "into expects collection as second arg, got {}",
            other.type_name()
        )),
    }
}

fn append_sorted_map_item(target: &mut SortedMap, item: Value) -> Result<(), CloveError> {
    match item {
        Value::Vector(v) | Value::List(v) if v.len() == 2 => {
            let key = to_key_value_checked(&v[0])?;
            let val = to_imut_deep(&v[1])?;
            sorted_map_insert_mut(target, key, val)?;
            Ok(())
        }
        Value::Map(m) => {
            for (k, v) in m {
                sorted_map_insert_mut(target, k, to_imut_deep(&v)?)?;
            }
            Ok(())
        }
        Value::SortedMap(m) => {
            for (k, v) in m.entries {
                sorted_map_insert_mut(target, k, to_imut_deep(&v)?)?;
            }
            Ok(())
        }
        _ => super::err("conj on map expects map or [k v]"),
    }
}

fn append_into_sorted_map(mut target: SortedMap, source: Value) -> Result<SortedMap, CloveError> {
    match source {
        Value::List(items) | Value::Vector(items) => {
            for item in items {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Set(items) => {
            for item in items {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::SortedSet(items) => {
            for item in items.entries {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Map(map) => {
            for (k, v) in map {
                guard::tick(None)?;
                sorted_map_insert_mut(&mut target, k, to_imut_deep(&v)?)?;
            }
            Ok(target)
        }
        Value::MutMap(handle) => {
            let entries: Vec<(Key, Value)> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (k, v) in entries {
                guard::tick(None)?;
                sorted_map_insert_mut(&mut target, k, to_imut_deep(&v)?)?;
            }
            Ok(target)
        }
        Value::SortedMap(map) => {
            for (k, v) in map.entries {
                guard::tick(None)?;
                sorted_map_insert_mut(&mut target, k, to_imut_deep(&v)?)?;
            }
            Ok(target)
        }
        Value::String(s) => {
            for c in s.chars() {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, Value::String(c.to_string()))?;
            }
            Ok(target)
        }
        Value::Seq(handle) => {
            let collected = handle.collect_all()?;
            for item in collected {
                guard::tick(None)?;
                append_sorted_map_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Nil => Ok(target),
        other => super::err(format!(
            "into expects collection as second arg, got {}",
            other.type_name()
        )),
    }
}

fn append_sorted_set_item(target: &mut SortedSet, item: Value) -> Result<(), CloveError> {
    if is_mut_collection(&item) {
        return super::err("cannot put mutable collection into set; use (imut x)");
    }
    let converted = to_imut_deep(&item)?;
    sorted_set_insert_mut(target, converted)?;
    Ok(())
}

fn append_into_sorted_set(mut target: SortedSet, source: Value) -> Result<SortedSet, CloveError> {
    match source {
        Value::List(items) | Value::Vector(items) => {
            for item in items {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Set(items) => {
            for item in items {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            for item in items {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::SortedSet(items) => {
            for item in items.entries {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Map(map) => {
            for (k, v) in map {
                guard::tick(None)?;
                append_sorted_set_item(
                    &mut target,
                    Value::Vector(Vector::from(vec![key_to_value(&k), v])),
                )?;
            }
            Ok(target)
        }
        Value::MutMap(handle) => {
            let entries: Vec<(Key, Value)> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            for (k, v) in entries {
                guard::tick(None)?;
                append_sorted_set_item(
                    &mut target,
                    Value::Vector(Vector::from(vec![key_to_value(&k), v])),
                )?;
            }
            Ok(target)
        }
        Value::SortedMap(map) => {
            for (k, v) in map.entries {
                guard::tick(None)?;
                append_sorted_set_item(
                    &mut target,
                    Value::Vector(Vector::from(vec![key_to_value(&k), v])),
                )?;
            }
            Ok(target)
        }
        Value::String(s) => {
            for c in s.chars() {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, Value::String(c.to_string()))?;
            }
            Ok(target)
        }
        Value::Seq(handle) => {
            let collected = handle.collect_all()?;
            for item in collected {
                guard::tick(None)?;
                append_sorted_set_item(&mut target, item)?;
            }
            Ok(target)
        }
        Value::Nil => Ok(target),
        other => super::err(format!(
            "into expects collection as second arg, got {}",
            other.type_name()
        )),
    }
}

fn append_into_slow(target: Value, source: Value) -> Result<Value, CloveError> {
    match source {
        Value::List(items) | Value::Vector(items) => {
            let mut acc = target;
            for item in items {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::MutVector(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            let mut acc = target;
            for item in items {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::Set(items) => {
            let mut acc = target;
            for item in items {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::MutSet(handle) => {
            let items: Vec<Value> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .cloned()
                .collect();
            let mut acc = target;
            for item in items {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::SortedSet(items) => {
            let mut acc = target;
            for item in items.entries {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::Map(map) => {
            let mut acc = target;
            for (k, v) in map {
                guard::tick(None)?;
                acc = conj_on_value(acc, Value::Vector(Vector::from(vec![key_to_value(&k), v])))?;
            }
            Ok(acc)
        }
        Value::MutMap(handle) => {
            let entries: Vec<(Key, Value)> = handle
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            let mut acc = target;
            for (k, v) in entries {
                guard::tick(None)?;
                acc = conj_on_value(acc, Value::Vector(Vector::from(vec![key_to_value(&k), v])))?;
            }
            Ok(acc)
        }
        Value::SortedMap(map) => {
            let mut acc = target;
            for (k, v) in map.entries {
                guard::tick(None)?;
                acc = conj_on_value(acc, Value::Vector(Vector::from(vec![key_to_value(&k), v])))?;
            }
            Ok(acc)
        }
        Value::String(s) => {
            let mut acc = target;
            for c in s.chars() {
                guard::tick(None)?;
                acc = conj_on_value(acc, Value::String(c.to_string()))?;
            }
            Ok(acc)
        }
        Value::Seq(handle) => {
            let mut acc = target;
            let collected = handle.collect_all()?;
            for item in collected {
                guard::tick(None)?;
                acc = conj_on_value(acc, item)?;
            }
            Ok(acc)
        }
        Value::Nil => Ok(target),
        other => super::err(format!(
            "into expects collection as second arg, got {}",
            other.type_name()
        )),
    }
}

pub(crate) fn append_into(target: Value, source: Value) -> Result<Value, CloveError> {
    match target {
        Value::Vector(items) => Ok(Value::Vector(append_items_with(
            items,
            source,
            append_vector_item,
        )?)),
        Value::List(items) => Ok(Value::List(append_items_with(
            items,
            source,
            append_list_item,
        )?)),
        Value::Map(map) => Ok(Value::Map(append_into_map(map, source)?)),
        Value::SortedMap(map) => Ok(Value::SortedMap(append_into_sorted_map(map, source)?)),
        Value::SortedSet(set) => Ok(Value::SortedSet(append_into_sorted_set(set, source)?)),
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => {
            let frozen = to_imut_deep(&target)?;
            append_into(frozen, source)
        }
        other => append_into_slow(other, source),
    }
}

pub(crate) fn key_to_string(key: &Key) -> String {
    match key {
        Key::Keyword(s) => s.clone(),
        Key::Symbol(s) => s.clone(),
        Key::String(s) => s.clone(),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

pub(crate) fn serde_to_value(v: &JsonValue) -> Result<Value, CloveError> {
    match v {
        JsonValue::Null => Ok(Value::Nil),
        JsonValue::Bool(b) => Ok(Value::Bool(*b)),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Value::Int(i))
            } else {
                Ok(Value::Float(n.as_f64().unwrap_or_default()))
            }
        }
        JsonValue::String(s) => Ok(Value::String(s.clone())),
        JsonValue::Array(items) => {
            let mut vec = Vec::with_capacity(items.len());
            for item in items {
                guard::tick(None)?;
                vec.push(serde_to_value(item)?);
            }
            Ok(Value::Vector(Vector::from(vec)))
        }
        JsonValue::Object(map) => {
            let mut out = StdHashMap::with_capacity(map.len());
            for (k, v) in map {
                guard::tick(None)?;
                out.insert(Key::String(k.clone()), serde_to_value(v)?);
            }
            Ok(Value::Map(HashMap::from(out)))
        }
    }
}

pub(crate) fn value_to_serde(v: &Value) -> Result<JsonValue, CloveError> {
    match v {
        Value::Nil => Ok(JsonValue::Null),
        Value::Bool(b) => Ok(JsonValue::Bool(*b)),
        Value::Int(n) => Ok(JsonValue::Number((*n).into())),
        Value::Float(f) => Ok(JsonValue::Number(
            serde_json::Number::from_f64(*f).ok_or_else(|| CloveError::runtime("invalid float"))?,
        )),
        Value::String(s) => Ok(JsonValue::String(s.clone())),
        Value::List(items) | Value::Vector(items) => {
            let mut arr = Vec::with_capacity(items.len());
            for item in items {
                guard::tick(None)?;
                arr.push(value_to_serde(item)?);
            }
            Ok(JsonValue::Array(arr))
        }
        Value::Set(items) => {
            let mut arr = Vec::with_capacity(items.len());
            for item in items {
                guard::tick(None)?;
                arr.push(value_to_serde(item)?);
            }
            Ok(JsonValue::Array(arr))
        }
        Value::SortedSet(items) => {
            let mut arr = Vec::with_capacity(items.entries.len());
            for item in &items.entries {
                guard::tick(None)?;
                arr.push(value_to_serde(item)?);
            }
            Ok(JsonValue::Array(arr))
        }
        Value::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map {
                guard::tick(None)?;
                obj.insert(key_to_string(k), value_to_serde(v)?);
            }
            Ok(JsonValue::Object(obj))
        }
        Value::SortedMap(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in &map.entries {
                guard::tick(None)?;
                obj.insert(key_to_string(k), value_to_serde(v)?);
            }
            Ok(JsonValue::Object(obj))
        }
        Value::Duration(d) => Ok(JsonValue::String(d.pretty())),
        other => super::err(format!(
            "unsupported type for serialization: {}",
            other.type_name()
        )),
    }
}
