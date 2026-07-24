use std::sync::{Arc, Mutex};

use clove_core::ast::Value;
use clove_core::env::live_env_count;
use clove_core::eval_source;

static ENV_COUNT_TEST_LOCK: Mutex<()> = Mutex::new(());

fn run_with_large_stack<T, F>(f: F) -> T
where
    T: Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(f)
        .expect("spawn test thread with larger stack")
        .join()
        .expect("large-stack test thread panicked")
}

fn assert_eval_reclaims_local_envs(source: &str, expected: Value, case: &str) {
    let _guard = ENV_COUNT_TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let before = live_env_count();
    let actual = eval_source(source, None).unwrap_or_else(|error| panic!("{case}: {error}"));
    assert_eq!(actual, expected, "{case}: unexpected result");
    let after = live_env_count();
    let retained = after.saturating_sub(before);
    assert!(
        retained <= 10,
        "{case}: retained {retained} environments (before={before}, after={after})"
    );
}

#[test]
fn let_bound_lambda_does_not_retain_each_call_environment() {
    run_with_large_stack(|| {
        assert_eval_reclaims_local_envs(
            r#"
        (do
          (defn work [x]
            (let [helper (fn [y] (+ y 1))]
              (helper x)))
          (defn run [i n acc]
            (if (< i n)
              (recur (+ i 1) n (+ acc (work i)))
              acc))
          (run 0 1000 0))
        "#,
            Value::Int(500500),
            "let-bound lambda",
        );
    });
}

#[test]
fn local_defn_does_not_retain_each_call_environment() {
    run_with_large_stack(|| {
        assert_eval_reclaims_local_envs(
            r#"
        (do
          (defn work [x]
            (-defn helper [y] (+ y 1))
            (helper x))
          (defn run [i n acc]
            (if (< i n)
              (recur (+ i 1) n (+ acc (work i)))
              acc))
          (run 0 1000 0))
        "#,
            Value::Int(500500),
            "local defn",
        );
    });
}

#[test]
fn escaping_and_recursive_local_closures_remain_callable() {
    run_with_large_stack(|| {
        let _guard = ENV_COUNT_TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let actual = eval_source(
            r#"
        (do
          (defn make-adder [n] (fn [x] (+ x n)))
          (defn escaping-let []
            (let [helper (fn [y] (+ y 1))]
              helper))
          (defn recursive [n]
            (-defn factorial [x]
              (if (zero? x) 1 (* x (factorial (dec x)))))
            (-defn even? [x]
              (if (zero? x) true (odd? (dec x))))
            (-defn odd? [x]
              (if (zero? x) false (even? (dec x))))
            [(factorial n) (even? n)])
          [(let [f (make-adder 3)] (f 4))
           ((escaping-let) 5)
           (recursive 5)])
        "#,
            None,
        )
        .expect("evaluate escaping and recursive closures");

        assert_eq!(
            actual,
            eval_source("[7 6 [120 false]]", None).expect("evaluate expected value")
        );
    });
}

#[test]
fn cloned_lambda_values_share_their_payload() {
    run_with_large_stack(|| {
        let _guard = ENV_COUNT_TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let single = eval_source("(fn [x] (+ x 1))", None).expect("evaluate single lambda");
        let single_clone = single.clone();
        match (&single, &single_clone) {
            (
                Value::Lambda {
                    data: left_data, ..
                },
                Value::Lambda {
                    data: right_data, ..
                },
            ) => assert!(Arc::ptr_eq(left_data, right_data)),
            _ => panic!("expected single lambda values"),
        }

        let multi =
            eval_source("(fn ([x] x) ([x y] (+ x y)))", None).expect("evaluate multi-arity lambda");
        let multi_clone = multi.clone();
        match (&multi, &multi_clone) {
            (
                Value::MultiLambda {
                    data: left_data, ..
                },
                Value::MultiLambda {
                    data: right_data, ..
                },
            ) => assert!(Arc::ptr_eq(left_data, right_data)),
            _ => panic!("expected multi-arity lambda values"),
        }
    });
}
