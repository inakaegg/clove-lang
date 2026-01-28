use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::ast::Span;
use crate::runtime::RuntimeCtx;
use crate::symbols::canonical_symbol_name;
use crate::types::TypeKind;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SpecialOp {
    IntAdd2,
    FloatAdd2,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SubjectPos {
    Fixed(usize),
    Last,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnOverload {
    pub arg_types: Vec<TypeKind>,
    pub rest: Option<TypeKind>,
    pub ret_type: TypeKind,
    pub special_op: Option<SpecialOp>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnMeta {
    pub name: String,
    pub ns: String,
    pub arglist: Vec<String>,
    pub doc: Option<String>,
    pub overloads: Vec<FnOverload>,
    pub source_file: Option<String>,
    pub source_span: Option<Span>,
    pub subject_pos: Option<SubjectPos>,
}

impl FnMeta {
    pub fn new(ns: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ns: ns.into(),
            arglist: Vec::new(),
            doc: None,
            overloads: Vec::new(),
            source_file: None,
            source_span: None,
            subject_pos: None,
        }
    }

    pub fn fq_name(&self) -> String {
        format!("{}::{}", self.ns, self.name)
    }
}

#[derive(Default)]
struct RegistryState {
    entries: HashMap<String, FnMeta>,
    name_index: HashMap<String, HashSet<String>>,
}

#[derive(Default)]
struct RegistryStore {
    global: RegistryState,
    per_runtime: HashMap<usize, RegistryState>,
}

static REGISTRY: Lazy<RwLock<RegistryStore>> = Lazy::new(|| RwLock::new(RegistryStore::default()));

fn current_runtime_id() -> Option<usize> {
    RuntimeCtx::try_with_current(|ctx| Ok(ctx.runtime_id())).and_then(|res| res.ok())
}

enum LookupOutcome {
    Found(FnMeta),
    Missing,
    Ambiguous,
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

pub fn register(meta: FnMeta) {
    let key = canonical_symbol_name(&meta.fq_name()).into_owned();
    let bare = canonical_symbol_name(&meta.name).into_owned();
    let runtime_id = current_runtime_id();
    let mut guard = REGISTRY.write().unwrap();
    let state = match runtime_id {
        Some(id) => guard
            .per_runtime
            .entry(id)
            .or_insert_with(RegistryState::default),
        None => &mut guard.global,
    };
    state.entries.insert(key.clone(), meta);
    state
        .name_index
        .entry(bare)
        .or_insert_with(HashSet::new)
        .insert(key);
}

pub fn get(symbol: &str) -> Option<FnMeta> {
    let canonical = canonical_symbol_name(symbol);
    let runtime_id = current_runtime_id();
    let guard = REGISTRY.read().unwrap();
    if let Some(id) = runtime_id {
        if let Some(state) = guard.per_runtime.get(&id) {
            match lookup_in_state(state, canonical.as_ref()) {
                LookupOutcome::Found(meta) => return Some(meta),
                LookupOutcome::Ambiguous => return None,
                LookupOutcome::Missing => {}
            }
        }
    }
    match lookup_in_state(&guard.global, canonical.as_ref()) {
        LookupOutcome::Found(meta) => Some(meta),
        _ => None,
    }
}

pub fn all() -> Vec<FnMeta> {
    let runtime_id = current_runtime_id();
    let guard = REGISTRY.read().unwrap();
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    if let Some(id) = runtime_id {
        if let Some(state) = guard.per_runtime.get(&id) {
            for meta in state.entries.values() {
                if seen.insert(meta.fq_name()) {
                    out.push(meta.clone());
                }
            }
        }
    }
    for meta in guard.global.entries.values() {
        if seen.insert(meta.fq_name()) {
            out.push(meta.clone());
        }
    }
    out
}

pub fn clear_runtime(runtime_id: usize) {
    let mut guard = REGISTRY.write().unwrap();
    guard.per_runtime.remove(&runtime_id);
}

#[cfg(test)]
pub fn clear_for_tests() {
    let mut guard = REGISTRY.write().unwrap();
    guard.global.entries.clear();
    guard.global.name_index.clear();
    guard.per_runtime.clear();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_name_lookup_is_ambiguous_when_colliding() {
        clear_for_tests();
        let meta_a = FnMeta::new("ns-a", "foo");
        let meta_b = FnMeta::new("ns-b", "foo");
        register(meta_a);
        assert!(get("foo").is_some(), "single candidate should resolve");
        register(meta_b);
        assert!(get("ns-a::foo").is_some());
        assert!(get("ns-b::foo").is_some());
        assert!(
            get("foo").is_none(),
            "ambiguous bare name should not resolve"
        );
        clear_for_tests();
    }
}
