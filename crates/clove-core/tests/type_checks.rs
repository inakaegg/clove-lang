use clove_core::eval_source;

#[test]
fn instance_predicate_requires_fields_and_match_rejects_forged_tags() {
    let src = r#"(do
  (deftype TypeCheckInstance {:a Int})
  (def ok (TypeCheckInstance a: 1))
  (def missing (dissoc ok :a))
  [(TypeCheckInstance? ok)
   (TypeCheckInstance? missing)
   (instance? 'user::TypeCheckInstance ok)
   (instance? 'user::TypeCheckInstance missing)
   (instance? 'user::TypeCheckInstance {:type 'user::TypeCheckInstance})
   (match {:type 'user::TypeCheckInstance}
     TypeCheckInstance :ok
     _ :ng)])"#;
    let out = eval_source(src, None).expect("eval instance? checks");
    let expected = eval_source("[true false true false false :ng]", None).expect("expected result");
    assert_eq!(out, expected);
}

#[test]
fn match_rejects_forged_enum_variant_tags() {
    let src = r#"(do
  (defenum TypeCheckEnumTag
    TypeCheckEnumTagRunning {:speed Int}
    TypeCheckEnumTagIdle)
  (match {:type 'user::TypeCheckEnumTag::TypeCheckEnumTagRunning}
    TypeCheckEnumTag::TypeCheckEnumTagRunning :ok
    _ :ng))"#;
    let out = eval_source(src, None).expect("eval enum match");
    let expected = eval_source(":ng", None).expect("expected result");
    assert_eq!(out, expected);
}

#[test]
fn deftype_predicate_requires_methods() {
    let src = r#"(do
  (use oop-syntax true)
  (deftype MethodCheck
    (def initial {:x 1})
    (defn step [] (get self :x)))
  (def v (MethodCheck {:x 1}))
  [(MethodCheck? v)
   (MethodCheck? (dissoc v :step))])"#;
    let out = eval_source(src, None).expect("eval method checks");
    let expected = eval_source("[true false]", None).expect("expected result");
    assert_eq!(out, expected);
}

#[test]
fn deftype_predicate_rejects_non_method_values() {
    let src = r#"(do
  (use oop-syntax true)
  (deftype MethodReject
    (def initial {:x 1})
    (defn step [n] n))
  (def v (MethodReject {:x 1}))
  [(MethodReject? (assoc v :step 123))
   (MethodReject? (assoc v :step (fn [x] x)))])"#;
    let out = eval_source(src, None).expect("eval method reject checks");
    let expected = eval_source("[false false]", None).expect("expected result");
    assert_eq!(out, expected);
}

#[test]
fn deftype_predicate_checks_method_signature() {
    let src = r#"(do
  (use oop-syntax true)
  (deftype MethodSig
    (def initial {:x 1})
    (defn step [n: Int] n))
  (def v (MethodSig {:x 1}))
  [(MethodSig? (assoc v :step (method [n: Int] n)))
   (MethodSig? (assoc v :step (method [n: Str] n)))])"#;
    let out = eval_source(src, None).expect("eval method signature checks");
    let expected = eval_source("[true false]", None).expect("expected result");
    assert_eq!(out, expected);
}
