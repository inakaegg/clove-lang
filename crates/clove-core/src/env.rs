use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::ast::Value;
use crate::symbols::canonical_symbol_name;

pub type EnvRef = Arc<RwLock<Env>>;

#[derive(Clone, Debug)]
pub struct Env {
    data: HashMap<String, Value>,
    outer: Option<EnvRef>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            data: HashMap::new(),
            outer: None,
        }
    }
}

impl Env {
    pub fn new_child(outer: EnvRef) -> Self {
        Self {
            data: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, key: &str, value: Value) {
        let canonical = canonical_symbol_name(key);
        self.data.insert(canonical.into_owned(), value);
    }

    pub fn contains_local(&self, key: &str) -> bool {
        let canonical = canonical_symbol_name(key);
        self.data.contains_key(canonical.as_ref())
    }

    pub fn outer_ref(&self) -> Option<EnvRef> {
        self.outer.clone()
    }

    pub fn define_builtin(&mut self, key: &str, value: Value) {
        self.set(key, value);
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        let canonical = canonical_symbol_name(key);
        self.data.remove(canonical.as_ref())
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        let canonical = canonical_symbol_name(key);
        if let Some(v) = self.data.get(canonical.as_ref()) {
            return Some(v.clone());
        }
        if let Some(ref outer) = self.outer {
            return outer.read().unwrap().get(canonical.as_ref());
        }
        None
    }

    pub fn clone_data(&self) -> Vec<(String, Value)> {
        self.data
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    pub fn set_in_chain(&mut self, key: &str, value: Value) -> bool {
        let canonical = canonical_symbol_name(key);
        let name = canonical.as_ref();
        if self.data.contains_key(name) {
            self.data.insert(name.to_string(), value);
            true
        } else if let Some(ref outer) = self.outer {
            if outer.write().unwrap().set_in_chain(name, value.clone()) {
                return true;
            }
            false
        } else {
            self.set(name, value);
            true
        }
    }

    pub fn flatten(&self) -> Vec<(String, Value)> {
        let mut entries = self
            .outer
            .as_ref()
            .map(|o| o.read().unwrap().flatten())
            .unwrap_or_default();
        entries.extend(self.clone_data());
        entries
    }
}

pub fn new_ref(env: Env) -> EnvRef {
    Arc::new(RwLock::new(env))
}
