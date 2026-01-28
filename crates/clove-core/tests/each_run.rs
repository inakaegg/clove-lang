use clove_core::eval_source;

#[test]
fn each_binding_returns_receiver() {
    let out = eval_source("(each [x [1 2 3]] x)", None).expect("eval each binding");
    let expected = eval_source("[1 2 3]", None).expect("expected receiver");
    assert_eq!(out, expected);
}

#[test]
fn each_function_returns_receiver_and_runs() {
    let src = r#"
      (let [a (atom 0)
            coll [1 2 3]
            out (each (fn [v] (swap! a + v)) coll)]
        [out @a])
    "#;
    let out = eval_source(src, None).expect("eval each function");
    let expected = eval_source("[[1 2 3] 6]", None).expect("expected receiver and sum");
    assert_eq!(out, expected);
}

#[test]
fn each_oop_method_returns_receiver_and_runs() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [a (atom 0)
              out (range 4).each((fn [v] (swap! a + v)))]
          [out @a]))
    "#;
    let out = eval_source(src, None).expect("eval each oop method");
    let expected = eval_source("[[0 1 2 3] 6]", None).expect("expected receiver and sum");
    assert_eq!(out, expected);
}

#[test]
fn run_returns_nil_and_runs() {
    let src = r#"
      (let [a (atom 0)
            out (run! (fn [v] (swap! a + v)) [1 2 3])]
        [out @a])
    "#;
    let out = eval_source(src, None).expect("eval run!");
    let expected = eval_source("[nil 6]", None).expect("expected nil and sum");
    assert_eq!(out, expected);
}

#[test]
fn map_binding_sugar_binds_pattern() {
    let out = eval_source("(vec (map [x [1 2 3]] (+ x 1)))", None).expect("eval map sugar");
    let expected = eval_source("[2 3 4]", None).expect("expected mapped vec");
    assert_eq!(out, expected);
}

#[test]
fn pmap_binding_sugar_binds_pattern() {
    let out = eval_source("(pmap [x [1 2 3]] (+ x 1))", None).expect("eval pmap sugar");
    let expected = eval_source("[2 3 4]", None).expect("expected mapped vec");
    assert_eq!(out, expected);
}

#[test]
fn pmap_accepts_opts_first() {
    let out = eval_source("(vec (pmap {:max-parallel 2} inc [1 2 3]))", None)
        .expect("eval pmap opts first");
    let expected = eval_source("[2 3 4]", None).expect("expected mapped vec");
    assert_eq!(out, expected);
}

#[test]
fn pmap_binding_sugar_accepts_opts() {
    let out = eval_source("(pmap [x [1 2 3]] {:max-parallel 2} (+ x 1))", None)
        .expect("eval pmap sugar opts");
    let expected = eval_source("[2 3 4]", None).expect("expected mapped vec");
    assert_eq!(out, expected);
}

#[test]
fn pfilter_binding_sugar_binds_pattern() {
    let out = eval_source("(pfilter [x [1 2 3 4]] (odd? x))", None).expect("eval pfilter sugar");
    let expected = eval_source("[1 3]", None).expect("expected filtered vec");
    assert_eq!(out, expected);
}

#[test]
fn filter_binding_sugar_binds_pattern() {
    let out =
        eval_source("(vec (filter [x [1 2 3 4]] (odd? x)))", None).expect("eval filter sugar");
    let expected = eval_source("[1 3]", None).expect("expected filtered vec");
    assert_eq!(out, expected);
}

#[test]
fn remove_binding_sugar_binds_pattern() {
    let out =
        eval_source("(vec (remove [x [1 2 3 4]] (odd? x)))", None).expect("eval remove sugar");
    let expected = eval_source("(vec (remove (fn [x] (odd? x)) [1 2 3 4]))", None)
        .expect("expected remove result");
    assert_eq!(out, expected);
}

#[test]
fn keep_binding_sugar_binds_pattern() {
    let out = eval_source(
        "(vec (keep [x [1 2 3 4]] (if (even? x) (/ x 2) nil)))",
        None,
    )
    .expect("eval keep sugar");
    let expected = eval_source(
        "(vec (keep (fn [x] (if (even? x) (/ x 2) nil)) [1 2 3 4]))",
        None,
    )
    .expect("expected keep result");
    assert_eq!(out, expected);
}

#[test]
fn some_binding_sugar_binds_pattern() {
    let out = eval_source("(some [x [1 3 4 5]] (if (even? x) (* x 10) nil))", None)
        .expect("eval some sugar");
    let expected = eval_source(
        "(some (fn [x] (if (even? x) (* x 10) nil)) [1 3 4 5])",
        None,
    )
    .expect("expected some result");
    assert_eq!(out, expected);
}

#[test]
fn every_binding_sugar_binds_pattern() {
    let out = eval_source("(every? [x [2 4]] (even? x))", None).expect("eval every? sugar");
    let expected =
        eval_source("(every? (fn [x] (even? x)) [2 4])", None).expect("expected every? result");
    assert_eq!(out, expected);
}

#[test]
fn not_any_binding_sugar_binds_pattern() {
    let out = eval_source("(not-any? [x [-1 -2]] (pos? x))", None).expect("eval not-any? sugar");
    let expected = eval_source("(not-any? (fn [x] (pos? x)) [-1 -2])", None)
        .expect("expected not-any? result");
    assert_eq!(out, expected);
}

#[test]
fn not_every_binding_sugar_binds_pattern() {
    let out =
        eval_source("(not-every? [x [-1 0 1]] (neg? x))", None).expect("eval not-every? sugar");
    let expected = eval_source("(not-every? (fn [x] (neg? x)) [-1 0 1])", None)
        .expect("expected not-every? result");
    assert_eq!(out, expected);
}

#[test]
fn take_while_binding_sugar_binds_pattern() {
    let out = eval_source("(vec (take-while [x [0 1 2 3 0]] (< x 3)))", None)
        .expect("eval take-while sugar");
    let expected = eval_source("(vec (take-while (fn [x] (< x 3)) [0 1 2 3 0]))", None)
        .expect("expected take-while result");
    assert_eq!(out, expected);
}

#[test]
fn drop_while_binding_sugar_binds_pattern() {
    let out = eval_source("(vec (drop-while [x [-2 -1 0 1]] (neg? x)))", None)
        .expect("eval drop-while sugar");
    let expected = eval_source("(vec (drop-while (fn [x] (neg? x)) [-2 -1 0 1]))", None)
        .expect("expected drop-while result");
    assert_eq!(out, expected);
}

#[test]
fn split_with_binding_sugar_binds_pattern() {
    let out =
        eval_source("(split-with [x [-2 -1 0 1]] (neg? x))", None).expect("eval split-with sugar");
    let expected = eval_source("(split-with (fn [x] (neg? x)) [-2 -1 0 1])", None)
        .expect("expected split-with result");
    assert_eq!(out, expected);
}

#[test]
fn partition_by_binding_sugar_binds_pattern() {
    let out = eval_source("(partition-by [x [\"a\" \"a\" \"b\"]] x)", None)
        .expect("eval partition-by sugar");
    let expected = eval_source("(partition-by (fn [x] x) [\"a\" \"a\" \"b\"])", None)
        .expect("expected partition-by result");
    assert_eq!(out, expected);
}

#[test]
fn group_by_binding_sugar_binds_pattern() {
    let out = eval_source("(group-by [x [1 2 3 4]] (odd? x))", None).expect("eval group-by sugar");
    let expected = eval_source("(group-by (fn [x] (odd? x)) [1 2 3 4])", None)
        .expect("expected group-by result");
    assert_eq!(out, expected);
}

#[test]
fn run_binding_sugar_binds_pattern() {
    let out = eval_source(
        "(let [a (atom 0)] (run! [x [1 2 3]] (swap! a + x)) @a)",
        None,
    )
    .expect("eval run! sugar");
    let expected = eval_source(
        "(let [a (atom 0)] (run! (fn [x] (swap! a + x)) [1 2 3]) @a)",
        None,
    )
    .expect("expected run! result");
    assert_eq!(out, expected);
}

#[test]
fn sort_by_binding_sugar_binds_pattern() {
    let out = eval_source("(sort-by [s [\"aa\" \"b\" \"ccc\"]] (count s))", None)
        .expect("eval sort-by sugar");
    let expected = eval_source("(sort-by (fn [s] (count s)) [\"aa\" \"b\" \"ccc\"])", None)
        .expect("expected sort-by result");
    assert_eq!(out, expected);
}

#[test]
fn sort_by_binding_sugar_rejects_comparator_form() {
    let out = eval_source("(sort-by [x [3 1 2]] > x)", None);
    assert!(
        out.is_err(),
        "sort-by sugar should not accept comparator form"
    );
}

#[test]
fn map_accepts_regex_callable() {
    let out = eval_source(r#"(vec (map /\d+/ ["1" "a2" "333"]))"#, None).expect("eval regex map");
    let expected = eval_source(r#"["1" nil "333"]"#, None).expect("expected regex map result");
    assert_eq!(out, expected);
}

#[test]
fn interpolated_regex_literal_works() {
    let out = eval_source(r#"(let [target "x"] (/a#{target}b/ "axb"))"#, None)
        .expect("eval interpolated regex");
    let expected = eval_source(r#""axb""#, None).expect("expected regex match");
    assert_eq!(out, expected);
}

#[test]
fn pmap_oop_namespace_segment_resolves() {
    let src = r#"
      (do
        (use oop-syntax true)
        (pmap [s ["{}" "{\"a\":1}"]] s.json::parse))
    "#;
    let out = eval_source(src, None).expect("eval pmap oop namespace segment");
    let expected = eval_source(r#"[{} {"a" 1}]"#, None).expect("expected parsed json");
    assert_eq!(out, expected);
}
