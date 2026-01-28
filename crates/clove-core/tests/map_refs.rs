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

fn expect_map(value: &Value) -> &clove_core::ast::HashMap<Key, Value> {
    match value {
        Value::Map(map) => map,
        other => panic!("expected map, got {}", other.type_name()),
    }
}

fn map_get<'a>(map: &'a clove_core::ast::HashMap<Key, Value>, key: &Key) -> &'a Value {
    map.get(key).expect("missing map key")
}

#[test]
fn map_ref_relative_keyword() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:pipe {:gap 10 :gap-half (int (/ &^:gap 2))}})")
        .expect("eval map ref keyword");
    let map = expect_map(&value);
    let pipe = expect_map(map_get(map, &kw("pipe")));
    assert_eq!(map_get(pipe, &kw("gap-half")), &Value::Int(5));
}

#[test]
fn map_ref_parent_keyword() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:a {:x 1 :child {:y (+ &^../:x 2)}}})")
        .expect("eval map ref parent");
    let map = expect_map(&value);
    let a = expect_map(map_get(map, &kw("a")));
    let child = expect_map(map_get(a, &kw("child")));
    assert_eq!(map_get(child, &kw("y")), &Value::Int(3));
}

#[test]
fn map_ref_parent_above_root_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:a &^../:x})")
        .expect_err("map ref parent above root should error");
    assert!(
        err.to_string().contains("above root"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_relative_ident_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:pipe {:gap 10 :gap-half (int (/ &gap 2))}})")
        .expect_err("map ref ident should error");
    assert!(
        err.to_string().contains("has been removed"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_root_keyword() {
    let ctx = ctx();
    let value = ctx
        .eval_source(
            "(do (use map-refs true)
               {:screen {:h 100}
                :pipe {:gap 10
                       :gap-half (int (/ &^:gap 2))
                       :gap-max (- &:screen:h &^:gap-half)}})",
        )
        .expect("eval map ref root");
    let map = expect_map(&value);
    let pipe = expect_map(map_get(map, &kw("pipe")));
    assert_eq!(map_get(pipe, &kw("gap-max")), &Value::Int(95));
}

#[test]
fn map_ref_ref_root_alias() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:a 1 :b (+ 2 &ref:a)})")
        .expect("eval map ref root alias");
    let map = expect_map(&value);
    assert_eq!(map_get(map, &kw("b")), &Value::Int(3));
}

#[test]
fn map_ref_ref_root_standalone_with_get() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:gap 10 :gap-half (int (/ (get &ref :gap) 2))})")
        .expect("eval map ref root standalone");
    let map = expect_map(&value);
    assert_eq!(map_get(map, &kw("gap-half")), &Value::Int(5));
}

#[test]
fn map_ref_ref_root_indexer_sugar() {
    let ctx = ctx();
    let value = ctx
        .eval_source(
            "(do (use map-refs true) (use indexer true) {:root {:a {:b 3}} :val &ref[:root :a :b]})",
        )
        .expect("eval map ref root indexer");
    let map = expect_map(&value);
    assert_eq!(map_get(map, &kw("val")), &Value::Int(3));
}

#[test]
fn map_ref_ref_this_alias() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:pipe {:gap 10 :gap-half (int (/ &ref^:gap 2))}})")
        .expect("eval map ref this alias");
    let map = expect_map(&value);
    let pipe = expect_map(map_get(map, &kw("pipe")));
    assert_eq!(map_get(pipe, &kw("gap-half")), &Value::Int(5));
}

#[test]
fn map_ref_ref_parent_alias() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:a {:x 1 :child {:y (+ &ref^../:x 2)}}})")
        .expect("eval map ref parent alias");
    let map = expect_map(&value);
    let a = expect_map(map_get(map, &kw("a")));
    let child = expect_map(map_get(a, &kw("child")));
    assert_eq!(map_get(child, &kw("y")), &Value::Int(3));
}

#[test]
fn map_ref_undefined_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:a &^:missing})")
        .expect_err("undefined map ref should error");
    assert!(
        err.to_string().contains("map ref segment :missing"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_cycle_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:a (+ &^:b 1) :b (+ &^:a 1)})")
        .expect_err("cycle map ref should error");
    assert!(
        err.to_string().contains("cycle"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_keyword_prefers_keyword_key() {
    let ctx = ctx();
    let value = ctx
        .eval_source("(do (use map-refs true) {:gap 1 \"gap\" 2 :val &^:gap})")
        .expect("keyword map ref should resolve");
    let map = expect_map(&value);
    assert_eq!(map_get(map, &kw("val")), &Value::Int(1));
}

#[test]
fn map_ref_explicit_string_segment_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:gap 1 \"gap\" 2 :val &\"gap\"})")
        .expect_err("explicit string segment should error");
    assert!(
        err.to_string().contains("must be standalone")
            || err.to_string().contains("expected keyword segment")
            || err
                .to_string()
                .contains("map ref segment must be a keyword"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_root_syntax_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:a &root.screen.h})")
        .expect_err("map ref root should error");
    assert!(
        err.to_string().contains("'&root' has been removed"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn map_ref_self_dot_is_error() {
    let ctx = ctx();
    let err = ctx
        .eval_source("(do (use map-refs true) {:a &.})")
        .expect_err("map ref &. should error");
    assert!(
        err.to_string().contains("'&.' has been removed"),
        "unexpected error: {}",
        err
    );
}
