use clove_core::ast::Value;
use clove_core::error::CloveError;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

fn eval_with_vm(src: &str) -> Result<Value, CloveError> {
    let mut opts = EvalOptions::default();
    opts.use_vm = true;
    let ctx = RuntimeCtx::new(opts, &[]);
    ctx.eval_source(src)
}

fn eval_with_eval(src: &str) -> Result<Value, CloveError> {
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    ctx.eval_source(src)
}

fn assert_vm_matches_eval(src: &str) {
    let eval_val = eval_with_eval(src).expect("eval");
    let vm_val = eval_with_vm(src).expect("vm");
    assert_eq!(eval_val.to_string(), vm_val.to_string(), "{}", src);
}

#[test]
fn vm_basic_forms_match_eval() {
    let cases = [
        "(do 1 2 3)",
        "(if true 1 2)",
        "(if false 1)",
        "(let [x 10 y 32] (+ x y))",
        "(let [x 1] (inc x))",
        "(let [x 2 f (fn [y] (+ x y))] (f 3))",
        "(let [f (fn f [n] (if (= n 0) 0 (f (dec n))))] (f 3))",
        "(let [f (fn [x & more] (count more))] (f 1 2 3))",
        "(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))",
        "(+ 1 2 3)",
        "(and true 1 2)",
        "(or false nil 3)",
        "(when true 1 2 3)",
        "(cond false 1 true 2)",
        "(-> 1 inc (+ 2))",
        "(->> 1 (+ 2) inc)",
        "(let [v [1 2 3]] (+ (nth v 0) (nth v 2)))",
        "(let [m {:a 1 :b 2}] (+ (get m :a) (get m :b)))",
        "(let [s #{1 2 3}] (contains? s 2))",
    ];
    for src in cases {
        assert_vm_matches_eval(src);
    }
}

#[test]
fn vm_fallbacks_to_eval_for_special_forms() {
    assert_vm_matches_eval("(do (def x 1) x)");
}

#[test]
fn vm_sees_global_updates_across_fallbacks() {
    assert_vm_matches_eval(
        "(do (def x 1)
             (let [f (fn [] x)] (f))
             (set! x 2)
             (let [f (fn [] x)] (f)))",
    );
}

#[test]
fn vm_defn_forms_match_eval() {
    assert_vm_matches_eval("(do (defn add1 [x] (+ x 1)) (add1 41))");
    assert_vm_matches_eval("(do (def n 1) (defn f [x] (+ x n)) (f 2))");
    assert_vm_matches_eval("(do (defn add2 \"doc\" [x] (+ x 2)) (add2 40))");
    assert_vm_matches_eval("(do (defn add3 {:meta 1} [x] (+ x 3)) (add3 40))");
    assert_vm_matches_eval("(do (defn multi ([x] x) ([x y] (+ x y))) (+ (multi 1) (multi 1 2)))");
    assert_vm_matches_eval("(do (defn with-where [] 1 (where (-defn inner [] 2))) (with-where))");
    assert_vm_matches_eval(
        "(do (defn where-defn [] (where (defn inner [] 2)) (inner)) (where-defn))",
    );
}

#[test]
fn vm_method_forms_match_eval() {
    assert_vm_matches_eval("(let [m (method [x] (+ & x))] (m 10 2))");
}
