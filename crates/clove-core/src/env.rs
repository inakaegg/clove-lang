use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock, Weak};

use rustc_hash::FxHashMap;

use crate::ast::Value;
use crate::symbols::canonical_symbol_name;

/// 環境のキーはプログラム中のシンボルで、外部入力ではない。DoS 耐性のための
/// SipHash は要らず、ハッシュ計算が `Env::get` / `set` の実測で目立っていた。
/// 外部入力をキーにする箱（`Value::Map` など）はこれを使わないこと。
type HashMap<K, V> = FxHashMap<K, V>;

pub type EnvRef = Arc<RwLock<Env>>;

static LIVE_ENV_COUNT: AtomicUsize = AtomicUsize::new(0);
static TOTAL_ENV_CREATIONS: AtomicUsize = AtomicUsize::new(0);

/// Called on every environment creation. Relaxed is enough for counters, and this runs
/// once per Clove call frame.
#[inline]
fn count_created() {
    LIVE_ENV_COUNT.fetch_add(1, Ordering::Relaxed);
    TOTAL_ENV_CREATIONS.fetch_add(1, Ordering::Relaxed);
}

#[derive(Debug)]
pub struct Env {
    data: HashMap<String, Value>,
    outer: Option<EnvRef>,
    self_ref: Weak<RwLock<Env>>,
}

impl Default for Env {
    fn default() -> Self {
        count_created();
        Self {
            data: HashMap::default(),
            outer: None,
            self_ref: Weak::new(),
        }
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        count_created();
        Self {
            data: self
                .data
                .iter()
                .map(|(key, value)| (key.clone(), value.clone_with_strong_env()))
                .collect(),
            outer: self.outer.clone(),
            self_ref: Weak::new(),
        }
    }
}

impl Drop for Env {
    fn drop(&mut self) {
        LIVE_ENV_COUNT.fetch_sub(1, Ordering::Relaxed);
    }
}

impl Env {
    pub fn new_child(outer: EnvRef) -> Self {
        count_created();
        Self {
            data: HashMap::default(),
            outer: Some(outer),
            self_ref: Weak::new(),
        }
    }

    pub fn set(&mut self, key: &str, value: Value) {
        let canonical = canonical_symbol_name(key);
        let mut value = value;
        value.downgrade_env_if_same(&self.self_ref);
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
        self.data
            .remove(canonical.as_ref())
            .map(|value| value.clone_with_strong_env())
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        let canonical = canonical_symbol_name(key);
        if let Some(v) = self.data.get(canonical.as_ref()) {
            return Some(v.clone_with_strong_env());
        }
        if let Some(ref outer) = self.outer {
            return outer.read().unwrap().get(canonical.as_ref());
        }
        None
    }

    pub fn clone_data(&self) -> Vec<(String, Value)> {
        self.data
            .iter()
            .map(|(k, v)| (k.clone(), v.clone_with_strong_env()))
            .collect()
    }

    pub fn set_in_chain(&mut self, key: &str, value: Value) -> bool {
        let canonical = canonical_symbol_name(key);
        let name = canonical.as_ref();
        if self.data.contains_key(name) {
            self.set(name, value);
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
    Arc::new_cyclic(|self_ref| {
        let mut env = env;
        env.self_ref = self_ref.clone();
        RwLock::new(env)
    })
}

#[doc(hidden)]
pub fn live_env_count() -> usize {
    LIVE_ENV_COUNT.load(Ordering::Relaxed)
}

/// Environments created since the process started, including ones already dropped.
///
/// Used by the startup-cost tests: work that builds an environment and throws it away is
/// invisible to [`live_env_count`].
#[doc(hidden)]
pub fn total_env_creations() -> usize {
    TOTAL_ENV_CREATIONS.load(Ordering::Relaxed)
}
