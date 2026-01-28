use clove_core::ast::{Key, Value};
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use std::sync::Arc;

fn ctx() -> Arc<RuntimeCtx> {
    RuntimeCtx::new(EvalOptions::default(), &[])
}

fn kw(name: &str) -> Key {
    Key::Keyword(name.to_string())
}

#[test]
fn mut_shallow_keeps_nested_collections_immutable() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(mut {:a {:b 1} :v [1 2]})")
        .expect("mut should return mutable map");
    let map_handle = match value {
        Value::MutMap(handle) => handle,
        other => panic!("expected mut map, got {}", other.type_name()),
    };
    let map = map_handle.lock().expect("mut map lock");
    let nested_map = map.get(&kw("a")).expect("missing :a");
    let nested_vec = map.get(&kw("v")).expect("missing :v");
    assert!(matches!(nested_map, Value::Map(_)));
    assert!(matches!(nested_vec, Value::Vector(_)));
}

#[test]
fn assoc_bang_errors_on_immutable() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(assoc! {:a 1} :a 2)")
        .expect_err("assoc! should error on immutable map");
    assert!(
        err.to_string().contains("assoc!"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn assoc_on_mut_returns_immutable_and_does_not_mutate() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a 1}) r (assoc m :a 2)] [m r])")
        .expect("assoc on mut should succeed");
    let items = match value {
        Value::Vector(vec) => vec,
        other => panic!("expected vector, got {}", other.type_name()),
    };
    let m = items.get(0).expect("missing m");
    let r = items.get(1).expect("missing r");
    match m {
        Value::MutMap(handle) => {
            let map = handle.lock().expect("mut map lock");
            assert_eq!(map.get(&kw("a")), Some(&Value::Int(1)));
        }
        other => panic!("expected mut map, got {}", other.type_name()),
    }
    match r {
        Value::Map(map) => {
            assert_eq!(map.get(&kw("a")), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {}", other.type_name()),
    }
}

#[test]
fn imut_deep_freezes_nested_collections() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a (mut {:b 1}) :v (mut [1 2])})] (imut m))")
        .expect("imut should return immutable map");
    let map = match value {
        Value::Map(map) => map,
        other => panic!("expected map, got {}", other.type_name()),
    };
    let nested_map = map.get(&kw("a")).expect("missing :a");
    let nested_vec = map.get(&kw("v")).expect("missing :v");
    assert!(matches!(nested_map, Value::Map(_)));
    assert!(matches!(nested_vec, Value::Vector(_)));
}

#[test]
fn set_rejects_mutable_collections() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(hash-set (mut {:a 1}))")
        .expect_err("hash-set should reject mutable collection");
    assert!(
        err.to_string()
            .contains("cannot put mutable collection into set"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn mut_set_accepts_immutable_collections() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(mut #{ {:a 1} })")
        .expect("mut set should accept immutable collections");
    let set_handle = match value {
        Value::MutSet(handle) => handle,
        other => panic!("expected mut set, got {}", other.type_name()),
    };
    let set = set_handle.lock().expect("mut set lock");
    assert_eq!(set.len(), 1);
    let item = set.iter().next().expect("mut set empty");
    match item {
        Value::Map(map) => {
            assert_eq!(map.get(&kw("a")), Some(&Value::Int(1)));
        }
        other => panic!("expected map in set, got {}", other.type_name()),
    }
}

#[test]
fn conj_bang_mut_set_accepts_immutable_collections() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [s (mut #{})] (conj! s {:a 1}) s)")
        .expect("conj! on mut set should accept immutable collections");
    let set_handle = match value {
        Value::MutSet(handle) => handle,
        other => panic!("expected mut set, got {}", other.type_name()),
    };
    let set = set_handle.lock().expect("mut set lock");
    assert_eq!(set.len(), 1);
    let item = set.iter().next().expect("mut set empty");
    match item {
        Value::Map(map) => {
            assert_eq!(map.get(&kw("a")), Some(&Value::Int(1)));
        }
        other => panic!("expected map in set, got {}", other.type_name()),
    }
}

#[test]
fn conj_bang_mut_set_rejects_mutable_collections() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(let [s (mut #{})] (conj! s (mut {:a 1})))")
        .expect_err("conj! on mut set should reject mutable collections");
    assert!(
        err.to_string()
            .contains("cannot put mutable collection into set"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn conj_bang_transient_set_rejects_mutable_collections() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(let [s (transient #{})] (conj! s (mut {:a 1})))")
        .expect_err("conj! on transient set should reject mutable collections");
    assert!(
        err.to_string()
            .contains("cannot put mutable collection into set"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn conj_bang_mut_vector_works_in_reduce() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(reduce (fn [acc o] (conj! acc o)) (mut []) (mut [:a :b :c]))")
        .expect("conj! reduce should succeed");
    assert!(matches!(value, Value::MutVector(_)));
}

#[test]
fn update_bang_mut_map_updates_in_place() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a 1})] (update! m :a inc) (get m :a))")
        .expect("update! should update mut map");
    assert_eq!(value, Value::Int(2));
}

#[test]
fn update_bang_mut_vector_updates_in_place() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [v (mut [1 2 3])] (update! v 1 (fn [x] (+ x 10))) (get v 1))")
        .expect("update! should update mut vector");
    assert_eq!(value, Value::Int(12));
}

#[test]
fn update_in_bang_mut_map_updates_nested_value() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a {:b 1}})] (update-in! m [:a :b] inc) (get-in m [:a :b]))")
        .expect("update-in! should update nested value");
    assert_eq!(value, Value::Int(2));
}

#[test]
fn update_in_bang_mutates_path_children() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a {:b 1}})] (update-in! m [:a :b] inc) m)")
        .expect("update-in! should mutate path children");
    let map_handle = match value {
        Value::MutMap(handle) => handle,
        other => panic!("expected mut map, got {}", other.type_name()),
    };
    let map = map_handle.lock().expect("mut map lock");
    let nested_map = map.get(&kw("a")).expect("missing :a");
    assert!(matches!(nested_map, Value::MutMap(_)));
}

#[test]
fn update_in_bang_creates_missing_path() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {})] (update-in! m [:a :b] (fn [_] 1)) (get-in m [:a :b]))")
        .expect("update-in! should create missing path");
    assert_eq!(value, Value::Int(1));
}

#[test]
fn assoc_in_bang_mut_map_updates_nested_value() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a {:b 1}})] (assoc-in! m [:a :b] 2) (get-in m [:a :b]))")
        .expect("assoc-in! should update nested value");
    assert_eq!(value, Value::Int(2));
}

#[test]
fn assoc_in_bang_mutates_path_children() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a {:b 1}})] (assoc-in! m [:a :c] 2) (get m :a))")
        .expect("assoc-in! should mutate path children");
    assert!(matches!(value, Value::MutMap(_)));
}

#[test]
fn assoc_in_bang_creates_missing_path() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {})] (assoc-in! m [:a :b] 1) (get-in m [:a :b]))")
        .expect("assoc-in! should create missing path");
    assert_eq!(value, Value::Int(1));
}

#[test]
fn update_in_bang_fast_path_handles_deep_path() {
    let ctx = ctx();
    let value = ctx
        .eval_source(
            "(let [m (mut {:a {:b {:c {:d 1}}}})] (update-in! m [:a :b :c :d] inc) (get-in m [:a :b :c :d]))",
        )
        .expect("update-in! should handle deep path");
    assert_eq!(value, Value::Int(2));
}

#[test]
fn merge_bang_mut_map_merges_entries() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(let [m (mut {:a 1})] (merge! m {:b 2} {:a 3}) [(get m :a) (get m :b)])")
        .expect("merge! should merge into mut map");
    let items = match value {
        Value::Vector(vec) => vec,
        other => panic!("expected vector, got {}", other.type_name()),
    };
    assert_eq!(items.get(0), Some(&Value::Int(3)));
    assert_eq!(items.get(1), Some(&Value::Int(2)));
}
