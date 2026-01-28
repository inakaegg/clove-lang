use clove_core::ast::{Key, Value};
use clove_core::eval_source;

#[test]
fn dag_defaults_reports_workers_and_buffer() {
    let out = eval_source("(dag::defaults)", None).expect("dag defaults");
    let Value::Map(map) = out else {
        panic!("expected map from dag::defaults");
    };
    let workers = match map.get(&Key::Keyword("workers".into())) {
        Some(Value::Int(n)) => *n,
        other => panic!("workers missing or invalid: {:?}", other),
    };
    let buffer = match map.get(&Key::Keyword("buffer".into())) {
        Some(Value::Int(n)) => *n,
        other => panic!("buffer missing or invalid: {:?}", other),
    };
    assert!(workers >= 1, "workers should be >= 1");
    assert_eq!(buffer, workers.saturating_mul(4));
}

#[test]
fn dag_normalize_opts_accepts_max_parallel() {
    let out = eval_source("(dag::normalize-opts {:max-parallel 2})", None).expect("normalize opts");
    let Value::Map(map) = out else {
        panic!("expected map from dag::normalize-opts");
    };
    assert_eq!(
        map.get(&Key::Keyword("workers".into())),
        Some(&Value::Int(2))
    );
    assert!(!map.contains_key(&Key::Keyword("max-parallel".into())));
}

#[test]
fn dag_pmap_binding_opts_first_works() {
    let out = eval_source("(dag::pmap {:workers 2} [x [1 2 3]] (+ x 1))", None)
        .expect("dag::pmap opts-first");
    let expected = eval_source("[2 3 4]", None).expect("expected vec");
    assert_eq!(out, expected);
}

#[test]
fn dag_pfilter_binding_opts_first_works() {
    let out = eval_source("(dag::pfilter {:workers 2} [x [1 2 3 4]] (odd? x))", None)
        .expect("dag::pfilter opts-first");
    let expected = eval_source("[1 3]", None).expect("expected vec");
    assert_eq!(out, expected);
}

#[test]
fn dag_pmap_skip_drops_errors() {
    let src = r#"
      (dag::pmap {:on-error :skip}
        (fn [x] (if (= x 2) (throw "no") x))
        [1 2 3])
    "#;
    let out = eval_source(src, None).expect("dag::pmap skip");
    let expected = eval_source("[1 3]", None).expect("expected vec");
    assert_eq!(out, expected);
}

#[test]
fn dag_pmap_collect_wraps_errors() {
    let src = r#"
      (dag::pmap {:on-error :collect}
        (fn [x] (if (= x 2) (throw "no") x))
        [1 2])
    "#;
    let out = eval_source(src, None).expect("dag::pmap collect");
    let Value::Vector(items) = out else {
        panic!("expected vector from dag::pmap collect");
    };
    assert_eq!(items.len(), 2);
    let ok = match &items[0] {
        Value::Map(map) => map,
        other => panic!("expected map for ok: {:?}", other),
    };
    assert_eq!(
        ok.get(&Key::Keyword("type".into())),
        Some(&Value::Symbol("dag::Result::Ok".into()))
    );
    let err = match &items[1] {
        Value::Map(map) => map,
        other => panic!("expected map for err: {:?}", other),
    };
    assert_eq!(
        err.get(&Key::Keyword("type".into())),
        Some(&Value::Symbol("dag::Result::Err".into()))
    );
}

#[test]
fn std_pmap_alias_works() {
    let out = eval_source("(std::pmap inc [1 2 3])", None).expect("std::pmap");
    let expected = eval_source("[2 3 4]", None).expect("expected vec");
    assert_eq!(out, expected);
}
