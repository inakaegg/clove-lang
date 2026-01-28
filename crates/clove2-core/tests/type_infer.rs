use std::collections::BTreeMap;
use std::path::PathBuf;

use clove2_core::reader::read_all;
use clove2_core::syntax::parse_forms;
use clove2_core::type_infer::{check_program, infer_program, DiagnosticLevel};
use clove2_core::types::Type;
use clove2_core::use_directive::NativeLevel;

#[test]
fn def_type_mismatch_is_error() {
    let forms = read_all("(def x: Int \"a\")").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags.iter().any(|d| d.level == DiagnosticLevel::Error));
    assert!(diags
        .iter()
        .any(|d| d.message.contains("def x expects Int")));
}

#[test]
fn expect_reports_mismatch() {
    let forms = read_all("(def x (expect Int \"a\"))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("expect expects Int")));
}

#[test]
fn expect_allows_optional_union() {
    let forms = read_all("(def flag true)\n(def x (expect Int (if flag 1 nil)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn as_does_not_error() {
    let forms = read_all("(def x (as Int \"a\"))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn optional_usage_is_error_in_strict() {
    let forms = read_all("(def flag true)\n(def x (int (if flag 1 nil)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("expects non-optional")));
}

#[test]
fn optional_if_condition_is_ok() {
    let forms = read_all("(def flag (if true true nil))\n(def x (if flag 1 2))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn optional_not_is_ok() {
    let forms = read_all("(def flag (if true true nil))\n(def x (not flag))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn optional_map_input_is_error_in_strict() {
    let forms =
        read_all("(def xs (if true [1 2] nil))\n(def ys (map (fn [x: Int] -> Int x) xs))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("map expects non-optional")));
}

#[test]
fn reduce_with_init_infers_int() {
    let forms = read_all("(def xs [1 2 3])\n(def total: Int (reduce + 0 xs))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn warn_on_any_in_warn_mode() {
    let forms = read_all("(def x y)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Warn);
    assert!(diags.iter().any(|d| d.level == DiagnosticLevel::Warning));
}

#[test]
fn use_directive_is_ignored_in_type_check() {
    let forms = read_all("(use mode :native)\n(def x 1)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn debug_helpers_are_no_op() {
    let forms = read_all("(repl (def x 1))\n(debug x)\n(break)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn mut_immut_are_block_like() {
    let forms =
        read_all("(def x (mut (int \"1\") (inc 1)))\n(def y (imut (str 1)))\n(def z (do 1 2 3))")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn infer_builtin_casts() {
    let forms =
        read_all("(def x (int \"1\"))\n(def y (str 1))\n(def z (str))\n(def w (str \"a\" 1))")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn time_bench_checks() {
    let forms = read_all(
        "(def t (time (fn [] 1)))\n(def b (bench 2 (fn [] 1)))\n(def r (get t :result))\n(def s (get b :runs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn json_infer_in_strict() {
    let mut path = std::env::temp_dir();
    let suffix = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    path.push(format!("clove2_test_{}.json", suffix));
    std::fs::write(&path, "{\"port\": 80}").unwrap();
    let forms = read_all(&format!(
        "(def cfg (json::read-file \"{}\" :infer true))",
        path.display()
    ))
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
    let _ = std::fs::remove_file(path);
}

#[test]
fn json_read_string_infer() {
    let forms =
        read_all("(def cfg (json::read-string \"{\\\"port\\\": 80}\" :infer true))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn json_write_helpers() {
    let forms = read_all(
        "(def out (json::write-string {:a 1}))\n(def ok (json::write-file \"a.json\" {:a 1}))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn json_infer_with_type_name() {
    let mut path = std::env::temp_dir();
    let suffix = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    path.push(format!("clove2_test_{}.json", suffix));
    std::fs::write(&path, "{\"port\": 80}").unwrap();
    let forms = read_all(&format!(
        "(def cfg (json::read-file \"{}\" :infer true :type Config))\n(def cfg2: Config cfg)",
        path.display()
    ))
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
    let _ = std::fs::remove_file(path);
}

#[test]
fn json_schema_unknown_type() {
    let forms = read_all("(def cfg (json::read-file \"cfg.json\" :schema Missing))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("unknown schema type")));
}

#[test]
fn json_schema_known_type() {
    let mut path = std::env::temp_dir();
    let suffix = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    path.push(format!("clove2_test_{}.json", suffix));
    std::fs::write(&path, "{\"port\": 80}").unwrap();
    let forms = read_all(&format!(
        "(deftype Config {{:port Int}})\n(def cfg (json::read-file \"{}\" :schema Config))",
        path.display()
    ))
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
    let _ = std::fs::remove_file(path);
}

#[test]
fn json_schema_mismatch() {
    let mut path = std::env::temp_dir();
    let suffix = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    path.push(format!("clove2_test_{}.json", suffix));
    std::fs::write(&path, "{\"port\": \"x\"}").unwrap();
    let forms = read_all(&format!(
        "(deftype Config {{:port Int}})\n(def cfg (json::read-file \"{}\" :schema Config))",
        path.display()
    ))
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags.iter().any(|d| d.message.contains("schema mismatch")));
    let _ = std::fs::remove_file(path);
}

#[test]
fn rest_param_allows_extra_args() {
    let forms =
        read_all("(defn sum [a: Int & rest: Vec<Int>] -> Int a)\n(def x (sum 1 2 3))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn rest_param_type_mismatch_is_error() {
    let forms =
        read_all("(defn sum [a: Int & rest: Vec<Int>] -> Int a)\n(def x (sum 1 \"a\"))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags.iter().any(|d| d.message.contains("sum expects Int")));
}

#[test]
fn apply_with_rest_fn_is_ok() {
    let forms =
        read_all("(defn sum [a: Int & rest: Vec<Int>] -> Int a)\n(def x (apply sum 1 [2 3]))")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn apply_requires_rest_fn_in_strict() {
    let forms =
        read_all("(defn add [a: Int b: Int] -> Int (+ a b))\n(def x (apply add [1 2]))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("apply expects function with rest")));
}

#[test]
fn merge_maps_is_ok() {
    let forms = read_all(
        "(def m (merge {:a 1} {:b 2}))\n(def n (merge-with (fn [x: Int y: Int] -> Int (+ x y)) {:a 1} {:a 2}))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn contains_checks_map_and_vec() {
    let forms = read_all(
        "(def m {:a 1})\n(def v [1 2])\n(def x (contains? m :a))\n(def y (contains? v 1))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn contains_vec_index_type_error() {
    let forms = read_all("(def v [1 2])\n(def x (contains? v \"a\"))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("contains? expects Int index")));
}

#[test]
fn and_or_infers_union() {
    let forms = read_all("(def x (and true 1))\n(def y (or nil \"a\"))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn get_on_object_literal() {
    let forms = read_all(
        "(def cfg {:port 80 :host \"x\"})\n(def p (get cfg :port))\n(def h (get cfg :host))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn defn_call_signature() {
    let forms = read_all("(def x (add 1 2))\n(defn add [a: Int b: Int] -> Int a)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn defn_call_arity_error() {
    let forms = read_all("(defn add [a: Int b: Int] -> Int (+ a b))\n(add 1)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("add expects 2 arguments")));
}

#[test]
fn named_type_assignment() {
    let forms = read_all(
        "(deftype Config {:port Int :host Str})\n(def cfg: Config {:port 80 :host \"x\"})",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn named_type_mismatch() {
    let forms =
        read_all("(deftype Config {:port Int :host Str})\n(def cfg: Config {:port 80})").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags
        .iter()
        .any(|d| d.message.contains("def cfg expects Config")));
}

#[test]
fn map_over_vector() {
    let forms = read_all("(defn inc1 [x: Int] -> Int x)\n(def xs [1 2 3])\n(def ys (map inc1 xs))")
        .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn map_with_builtin_fn() {
    let forms =
        read_all("(def xs [1 2 3])\n(def ys (map inc xs))\n(def zs (map pr-str xs))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn map_with_fn_literal() {
    let forms = read_all("(def xs [1 2 3])\n(def ys (map (fn [x: Int] -> Int x) xs))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn filter_over_vector() {
    let forms = read_all(
        "(defn is-pos [x: Int] -> Bool true)\n(def xs [1 2 3])\n(def ys (filter is-pos xs))\n(def zs (remove is-pos xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn complement_predicate_checks() {
    let forms = read_all(
        "(defn is-pos [x: Int] -> Bool true)\n(def p (complement is-pos))\n(def xs [1 2 3])\n(def ys (filter p xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn pipe_checks() {
    let forms = read_all(
        "(defn inc1 [x: Int] -> Int (+ x 1))\n(defn double1 [x: Int] -> Int (* x 2))\n(def f (pipe inc1 double1))\n(def y (f 3))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn comp_allows_variadic_unary() {
    let forms = read_all("(def f (comp str inc))\n(def y (f 1))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn juxt_allows_variadic_mix() {
    let forms = read_all("(def f (juxt inc dec str))\n(def y (f 10))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn count_and_nth() {
    let forms = read_all("(def xs [1 2 3])\n(def n (count xs))\n(def x (nth xs 0))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn tuple_nth_non_literal_index_unions() {
    let forms =
        read_all("(def pair [1 \"a\"])\n(def idx (if true 0 1))\n(def v (nth pair idx))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let (summary, diags) = infer_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
    assert_eq!(
        summary.values.get("v"),
        Some(&Type::union(vec![Type::Int, Type::Str]))
    );
}

#[test]
fn flow_typing_nil_narrowing() {
    let forms = read_all("(def x (or nil 1))\n(def y (if (nil? x) 0 (+ x 1)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn flow_typing_type_predicate_narrowing() {
    let forms = read_all("(def x (or 1 \"a\"))\n(def y (if (int? x) (+ x 1) 0))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn get_vector_index() {
    let forms = read_all("(def xs [1 2 3])\n(def a (get xs 1))\n(def b (get xs 10 99))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn range_with_step() {
    let forms = read_all("(def xs (range 0 10 2))\n(def ys (range 10 0 -1))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn empty_check_on_collection() {
    let forms = read_all(
        "(def xs [1 2])\n(def m {:a 1})\n(def s \"\")\n(def a (empty? xs))\n(def b (empty? m))\n(def c (empty? s))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn parity_checks() {
    let forms = read_all(
        "(def a (odd? 1))\n(def b (even? 2))\n(def c (zero? 0))\n(def d (pos? 1))\n(def e (neg? -1))\n(def f (mod 5 2))\n(def g (quot 5 2))\n(def h (rem 5 2))\n(def i (bit-and 7 3))\n(def j (bit-or 7 3))\n(def k (bit-xor 7 3))\n(def l (bit-not 7))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn bit_shift_checks() {
    let forms = read_all("(def a (bit-shift-left 1 2))\n(def b (bit-shift-right 8 1))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn nil_check() {
    let forms = read_all(
        "(def a (nil? nil))\n(def b (nil? 1))\n(def c (some? 1))\n(def d (true? true))\n(def e (false? false))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn includes_checks() {
    let forms = read_all(
        "(def xs [1 2])\n(def m {:a 1})\n(def s \"abc\")\n(def a (includes? xs 1))\n(def b (includes? m :a))\n(def c (includes? s \"a\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn every_some_checks() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def a (every? (fn [x: Int] -> Bool true) xs))\n(def b (not-any? (fn [x: Int] -> Bool false) xs))\n(def c (not-every? (fn [x: Int] -> Bool false) xs))\n(def d (some (fn [x: Int] -> Int x) xs))\n(def e (shuffle xs))\n(def f (shuffle \"abc\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn keep_checks() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def a (keep (fn [x: Int] -> Int x) xs))\n(def b (keep-indexed (fn [i: Int x: Int] -> Int x) xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn frequencies_checks() {
    let forms = read_all(
        "(def xs [\"a\" \"b\" \"a\"])\n(def a (frequencies xs))\n(def b (frequencies \"abca\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn name_namespace_checks() {
    let forms = read_all(
        "(def a (name :foo/bar))\n(def b (namespace :foo/bar))\n(def c (name :bar))\n(def d (namespace :bar))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn get_in_checks() {
    let forms = read_all(
        "(def m {:a {:b 1}})\n(def x (get-in m [:a :b]))\n(def y (get-in m [:a :c] 0))\n(def v [[1] [2]])\n(def z (get-in v [1 0]))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn dissoc_checks() {
    let forms = read_all("(def m {:a 1 :b 2})\n(def m2 (imut (dissoc m :a)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn select_keys_check() {
    let forms = read_all("(def m {:a 1 :b 2})\n(def a (select-keys m [:a]))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn assoc_update_in_checks() {
    let forms = read_all(
        "(def m {:a {:b 1}})\n(def m2 (imut (assoc-in m [:a :b] 2)))\n(def m3 (imut (update-in m [:a :b] inc)))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn mut_unique_allows_single_use_def() {
    let forms = read_all("(def m {:a 1})\n(def m2 (update-in m [:a] inc))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn mut_unique_rejects_shared_def() {
    let forms = read_all("(def m {:a 1})\n(def x m)\n(def m2 (update-in m [:a] inc))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(diags.iter().any(|d| d.message.contains("requires unique")));
}

#[test]
fn take_drop_checks() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def ys (take 2 xs))\n(def zs (drop 1 xs))\n(def ys2 (take-last 2 xs))\n(def zs2 (drop-last 1 xs))\n(def s (take 2 \"abcd\"))\n(def t (drop 2 \"abcd\"))\n(def s2 (take-last 2 \"abcd\"))\n(def t2 (drop-last 2 \"abcd\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn partition_checks() {
    let forms = read_all(
        "(def xs [1 2 3 4 5])\n(def a (partition 2 xs))\n(def b (partition 2 1 xs))\n(def c (partition-all 2 xs))\n(def d (partition-all 2 1 xs))\n(def e (partition 2 \"abcd\"))\n(def f (partition-by (fn [x: Int] -> Int x) xs))\n(def g (partition-by (fn [x: Str] -> Str x) \"abcc\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn sort_checks() {
    let forms = read_all(
        "(def xs [3 1 2])\n(def a (sort xs))\n(def b (sort \"cba\"))\n(def c (sort-by (fn [x: Int] -> Int x) xs))\n(def d (sort-by (fn [x: Str] -> Str x) \"cba\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn distinct_dedupe_checks() {
    let forms = read_all(
        "(def xs [1 1 2 2 3])\n(def a (distinct xs))\n(def b (dedupe xs))\n(def c (distinct \"abca\"))\n(def d (dedupe \"abca\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn group_by_checks() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def a (group-by (fn [x: Int] -> Keyword :odd) xs))\n(def b (group-by (fn [x: Str] -> Str x) \"aba\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn zip_checks() {
    let forms = read_all(
        "(def xs [1 2])\n(def ys [\"a\" \"b\"])\n(def a (zip xs ys))\n(def b (zip \"ab\" \"cd\"))\n(def c (zip-with (fn [x: Int y: Str] -> Str y) xs ys))\n(def d (zipmap [:a :b] xs))\n(def e (zipmap \"ab\" ys))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn interpose_interleave_flatten_checks() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def a (interpose 0 xs))\n(def b (interpose \",\" \"ab\"))\n(def c (interleave [1 2] [3 4]))\n(def d (interleave \"ab\" \"cd\"))\n(def e (flatten [[1] [2 3]]))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn reverse_concat_checks() {
    let forms = read_all(
        "(def xs [1 2])\n(def ys (imut (reverse xs)))\n(def zs (concat [1] [2 3]))\n(def s (reverse \"abc\"))\n(def t (concat \"a\" \"b\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn min_max_abs_checks() {
    let forms = read_all("(def a (abs -1))\n(def b (min 3 2 1))\n(def c (max 1 2 3))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn rand_checks() {
    let forms = read_all(
        "(def a (rand))\n(def b (rand 10))\n(def c (rand-int 5))\n(def d (rand-nth [1 2 3]))\n(def e (rand-nth \"abc\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn string_helpers() {
    let forms = read_all(
        "(def a (starts-with? \"abc\" \"a\"))\n(def b (ends-with? \"abc\" \"c\"))\n(def c (trim \" a \"))\n(def d (upper-case \"ab\"))\n(def e (lower-case \"AB\"))\n(def f (split \"a,b\" \",\"))\n(def f2 (split \"a,b,c\" \",\" 2))\n(def g (join \"/\" f))\n(def h (blank? \" \"))\n(def i (replace \"a-b\" \"-\" \"/\"))\n(def j (replace-first \"a-b-b\" \"b\" \"c\"))\n(def k (split-lines \"a\\nb\"))\n(def l (index-of \"abc\" \"b\"))\n(def l2 (index-of \"abcabc\" \"b\" 3))\n(def m (last-index-of \"ababa\" \"ba\"))\n(def m2 (last-index-of \"ababa\" \"ba\" 2))\n(def n (capitalize \"hELLo\"))\n(def o (trim-newline \"x\\n\"))\n(def p (triml \" a\"))\n(def q (trimr \"a \"))\n(def r (reverse-str \"abc\"))\n(def s (lines \"a\\nb\"))\n(def t (subs \"abcd\" 1 3))\n(def u (slurp \"a.txt\"))\n(def v (spit \"a.txt\" \"x\"))\n(def w (format \"x{}y\" 1))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn type_predicates() {
    let forms = read_all(
        "(def a (bool? true))\n(def b (boolean? true))\n(def c (int? 1))\n(def d (integer? 1))\n(def e (float? 1.2))\n(def f (number? 1))\n(def g (str? \"x\"))\n(def h (string? \"x\"))\n(def i (keyword? :k))\n(def j (symbol? (symbol \"s\")))\n(def k (vec? [1]))\n(def l (vector? [1]))\n(def m (map? {:a 1}))\n(def n (fn? +))\n(def o (coll? [1]))\n(def p (sequential? [1]))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn reduce_over_vector() {
    let forms =
        read_all("(defn add [a: Int b: Int] -> Int a)\n(def xs [1 2 3])\n(def s (reduce add xs))")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn reduce_kv_checks() {
    let forms = read_all(
        "(defn step [acc: Int k: Str v: Int] -> Int (+ acc v))\n(def m {\"a\" 1 \"b\" 2})\n(def s (reduce-kv step 0 m))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn first_last_rest() {
    let forms =
        read_all("(def xs [1 2 3])\n(def a (first xs))\n(def b (last xs))\n(def r (rest xs))")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn seq_helpers() {
    let forms = read_all(
        "(def xs [1 2 3])\n(def a (second xs))\n(def b (peek xs))\n(def c (subvec xs 1 3))\n(def d (not-empty xs))\n(def e (not-empty \"x\"))\n(def f (butlast xs))\n(def g (next xs))\n(def h (pop xs))\n(def i (empty xs))\n(def j (seq xs))\n(def k (seq \"abc\"))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn take_drop_while_checks() {
    let forms = read_all(
        "(defn pred [x: Int] -> Bool (< x 3))\n(def xs [1 2 3 4])\n(def a (take-while pred xs))\n(def b (drop-while pred xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn map_indexed_mapcat_checks() {
    let forms = read_all(
        "(defn f [i: Int x: Int] -> Int (+ i x))\n(defn g [x: Int] -> Vec<Int> [x (+ x 1)])\n(def xs [1 2 3])\n(def a (map-indexed f xs))\n(def b (mapcat g xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn map_indexed_allows_rest_fn() {
    let forms = read_all(
        "(defn f [i: Int x: Int & rest: Vec<Int>] -> Int i)\n(def xs [1 2])\n(def ys (map-indexed f xs))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn cons_vec_into_repeat_checks() {
    let forms = read_all(
        "(def xs [1 2])\n(def ys (imut (cons 0 xs)))\n(def zs (vec xs))\n(def ws (imut (into xs [3 4])))\n(def rs (repeat 3 \"a\"))\n(defn f [] -> Int 1)\n(def ts (repeatedly 2 f))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn compare_and_print() {
    let forms =
        read_all("(def a (= 1 2))\n(def b (< 1 2))\n(def c (pr-str 1 \"a\"))\n(println a b c)")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn print_no_args_ok() {
    let forms = read_all("(print)").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn conj_vector() {
    let forms = read_all("(def xs [1 2])\n(def ys (imut (conj xs 3)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn list_vector_hash_map_checks() {
    let forms = read_all(
        "(def xs (list 1 2))\n(def ys (vector 1 2))\n(def m (hash-map :a 1 :b 2))\n(def a (count xs))\n(def b (get m :a))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn assoc_map() {
    let forms = read_all("(def m {:a 1})\n(def m2 (imut (assoc m :b 2)))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn update_map() {
    let forms = read_all(
        "(defn inc1 [x: Int] -> Int x)\n(def m {:a 1})\n(def m2 (imut (update m :a inc1)))",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn keys_and_vals() {
    let forms = read_all("(def m {:a 1 :b 2})\n(def ks (keys m))\n(def vs (vals m))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn infer_tuple_from_vector_literal() {
    let forms = read_all("(def t [1 \"a\"])").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let (summary, diags) = infer_program(&ast, NativeLevel::Strict);
    assert!(
        !diags.iter().any(|d| d.level == DiagnosticLevel::Error),
        "tuple inference errors: {:?}",
        diags
    );
    assert_eq!(
        summary.values.get("t"),
        Some(&Type::Tuple(vec![Type::Int, Type::Str]))
    );
}

#[test]
fn infer_shape_and_missing_key_diag() {
    let forms =
        read_all("(def m {:a 1 :b \"x\"})\n(def ok (get m :a))\n(def ng (get m :c))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let (summary, diags) = infer_program(&ast, NativeLevel::Strict);
    let mut fields = BTreeMap::new();
    fields.insert("a".to_string(), Type::Int);
    fields.insert("b".to_string(), Type::Str);
    assert_eq!(summary.values.get("m"), Some(&Type::shape(fields)));
    assert!(diags
        .iter()
        .any(|d| d.message.contains("get expects key :c")));
}

#[test]
fn open_shape_allows_missing_key() {
    let forms = read_all("(def m: {:a Int ..} {:a 1 :b 2})\n(def v (get m :b))").unwrap();
    let ast = parse_forms(&forms).unwrap();
    let diags = check_program(&ast, NativeLevel::Strict);
    assert!(!diags.iter().any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn bench_phase2_infers_vec_types() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("../../bench/phase2/bench.clv");
    let content = std::fs::read_to_string(&path).unwrap();
    let forms = read_all(&content).unwrap();
    let ast = parse_forms(&forms).unwrap();
    let (summary, diags) = infer_program(&ast, NativeLevel::Strict);
    assert!(
        !diags.iter().any(|d| d.level == DiagnosticLevel::Error),
        "bench.clv type errors: {:?}",
        diags
    );
    let vec_int = Type::Vec(Box::new(Type::Int));
    assert_eq!(summary.values.get("xs"), Some(&vec_int));
    assert_eq!(summary.values.get("ys"), Some(&vec_int));
    assert_eq!(summary.values.get("zs"), Some(&vec_int));
    assert_eq!(summary.values.get("ws"), Some(&vec_int));
    assert_eq!(summary.values.get("total"), Some(&Type::Int));
}
