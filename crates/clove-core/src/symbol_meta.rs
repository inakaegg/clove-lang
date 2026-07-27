use once_cell::sync::Lazy;
use std::collections::{HashMap as StdHashMap, HashSet};
use std::sync::RwLock;

use crate::ast::{HashMap, Key, Value};
use crate::runtime::RuntimeCtx;
use crate::symbols::canonical_symbol_name;

#[derive(Clone, Debug, Default)]
pub struct SymbolMeta {
    pub doc: Option<String>,
    pub meta: Option<HashMap<Key, Value>>,
    pub source: Option<String>,
}

impl SymbolMeta {
    pub fn is_empty(&self) -> bool {
        self.doc.is_none() && self.meta.is_none() && self.source.is_none()
    }
}

#[derive(Default)]
struct RegistryState {
    entries: StdHashMap<String, SymbolMeta>,
    name_index: StdHashMap<String, HashSet<String>>,
}

#[derive(Default)]
struct RegistryStore {
    global: RegistryState,
    per_runtime: StdHashMap<usize, RegistryState>,
}

static REGISTRY: Lazy<RwLock<RegistryStore>> = Lazy::new(|| RwLock::new(RegistryStore::default()));

fn current_runtime_id() -> Option<usize> {
    RuntimeCtx::try_with_current(|ctx| Ok(ctx.runtime_id())).and_then(|res| res.ok())
}

enum LookupOutcome {
    Found(SymbolMeta),
    Missing,
    Ambiguous,
}

fn split_fqn(symbol: &str) -> Option<(&str, &str)> {
    let idx = symbol.rfind("::")?;
    let ns = &symbol[..idx];
    let local = &symbol[idx + 2..];
    if ns.is_empty() || local.is_empty() {
        None
    } else {
        Some((ns, local))
    }
}

fn lookup_in_state(state: &RegistryState, symbol: &str) -> LookupOutcome {
    if symbol.contains("::") {
        return state
            .entries
            .get(symbol)
            .cloned()
            .map(LookupOutcome::Found)
            .unwrap_or(LookupOutcome::Missing);
    }
    let candidates = match state.name_index.get(symbol) {
        Some(candidates) => candidates,
        None => return LookupOutcome::Missing,
    };
    if candidates.len() != 1 {
        return LookupOutcome::Ambiguous;
    }
    let fqn = match candidates.iter().next() {
        Some(fqn) => fqn,
        None => return LookupOutcome::Missing,
    };
    state
        .entries
        .get(fqn)
        .cloned()
        .map(LookupOutcome::Found)
        .unwrap_or(LookupOutcome::Missing)
}

pub fn register(name: &str, meta: SymbolMeta) {
    if meta.is_empty() {
        return;
    }
    let key = canonical_symbol_name(name).into_owned();
    let bare_key = split_fqn(&key)
        .map(|(_, local)| canonical_symbol_name(local).into_owned())
        .unwrap_or_else(|| key.clone());
    let runtime_id = current_runtime_id();
    let mut guard = REGISTRY.write().unwrap();
    let state = match runtime_id {
        Some(id) => guard.per_runtime.entry(id).or_default(),
        None => &mut guard.global,
    };
    state.entries.insert(key.clone(), meta);
    state
        .name_index
        .entry(bare_key)
        .or_insert_with(HashSet::new)
        .insert(key);
}

pub fn get(name: &str) -> Option<SymbolMeta> {
    let key = canonical_symbol_name(name);
    let runtime_id = current_runtime_id();
    let guard = REGISTRY.read().unwrap();
    if let Some(id) = runtime_id {
        if let Some(state) = guard.per_runtime.get(&id) {
            match lookup_in_state(state, key.as_ref()) {
                LookupOutcome::Found(meta) => return Some(meta),
                LookupOutcome::Ambiguous => return None,
                LookupOutcome::Missing => {}
            }
        }
    }
    match lookup_in_state(&guard.global, key.as_ref()) {
        LookupOutcome::Found(meta) => Some(meta),
        _ => None,
    }
}

pub fn clear_runtime(runtime_id: usize) {
    let mut guard = REGISTRY.write().unwrap();
    guard.per_runtime.remove(&runtime_id);
}

#[cfg(test)]
mod tests {
    use super::*;

    // REGISTRY はプロセス全体で共有され、同じテストバイナリの他のテストも登録する。
    // レジストリを空にすると他のテストの登録まで消えるので、このテスト専用の名前を使う。
    const PROBE: &str = "symbol-meta-bare-name-probe";

    #[test]
    fn bare_name_lookup_is_ambiguous_when_colliding() {
        register(
            &format!("symbol-meta-probe-a::{PROBE}"),
            SymbolMeta {
                doc: Some("a".into()),
                meta: None,
                source: None,
            },
        );
        assert!(get(PROBE).is_some(), "single candidate should resolve");
        register(
            &format!("symbol-meta-probe-b::{PROBE}"),
            SymbolMeta {
                doc: Some("b".into()),
                meta: None,
                source: None,
            },
        );
        assert!(get(&format!("symbol-meta-probe-a::{PROBE}")).is_some());
        assert!(get(&format!("symbol-meta-probe-b::{PROBE}")).is_some());
        assert!(
            get(PROBE).is_none(),
            "ambiguous bare name should not resolve"
        );
    }
}
