use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use crate::ast::Value;
use crate::symbols::canonical_symbol_name;
use once_cell::sync::Lazy;

const DYNAMIC_VARS: &[&str] = &["*out*", "*err*"];

static ROOT_VALUES: Lazy<Mutex<HashMap<String, Value>>> = Lazy::new(|| Mutex::new(HashMap::new()));
static ROOT_EXPLICIT: Lazy<Mutex<HashSet<String>>> = Lazy::new(|| Mutex::new(HashSet::new()));

thread_local! {
    static DYNAMIC_STACK: RefCell<HashMap<String, Vec<Value>>> = RefCell::new(HashMap::new());
}

pub fn is_dynamic_var(name: &str) -> bool {
    let canonical = canonical_symbol_name(name);
    DYNAMIC_VARS.iter().any(|n| canonical.as_ref() == *n)
}

pub fn set_root_value(name: &str, value: Value) {
    if !is_dynamic_var(name) {
        return;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    ROOT_VALUES.lock().unwrap().insert(canonical.clone(), value);
    ROOT_EXPLICIT.lock().unwrap().insert(canonical);
}

pub fn set_root_default(name: &str, value: Value) {
    if !is_dynamic_var(name) {
        return;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    ROOT_VALUES.lock().unwrap().insert(canonical, value);
}

pub fn current_value(name: &str) -> Option<Value> {
    if !is_dynamic_var(name) {
        return None;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    if let Some(v) = DYNAMIC_STACK.with(|cell| {
        cell.borrow()
            .get(&canonical)
            .and_then(|stack| stack.last())
            .cloned()
    }) {
        return Some(v);
    }
    ROOT_VALUES.lock().unwrap().get(&canonical).cloned()
}

pub fn has_dynamic_binding(name: &str) -> bool {
    if !is_dynamic_var(name) {
        return false;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    DYNAMIC_STACK.with(|cell| {
        cell.borrow()
            .get(&canonical)
            .map(|stack| !stack.is_empty())
            .unwrap_or(false)
    })
}

pub fn root_is_explicit(name: &str) -> bool {
    if !is_dynamic_var(name) {
        return false;
    }
    let canonical = canonical_symbol_name(name).into_owned();
    ROOT_EXPLICIT.lock().unwrap().contains(&canonical)
}

pub fn push_bindings(bindings: &[(String, Value)]) -> DynamicGuard {
    DYNAMIC_STACK.with(|cell| {
        let mut map = cell.borrow_mut();
        for (name, val) in bindings {
            let entry = map
                .entry(canonical_symbol_name(name).into_owned())
                .or_insert_with(Vec::new);
            entry.push(val.clone());
        }
    });
    DynamicGuard {
        names: bindings
            .iter()
            .map(|(name, _)| canonical_symbol_name(name).into_owned())
            .collect(),
    }
}

pub struct DynamicGuard {
    names: Vec<String>,
}

impl Drop for DynamicGuard {
    fn drop(&mut self) {
        DYNAMIC_STACK.with(|cell| {
            let mut map = cell.borrow_mut();
            for name in &self.names {
                if let Some(stack) = map.get_mut(name) {
                    stack.pop();
                    if stack.is_empty() {
                        map.remove(name);
                    }
                }
            }
        });
    }
}
