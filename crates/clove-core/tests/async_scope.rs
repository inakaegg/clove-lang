use clove_core::ast::Value;
use clove_core::eval_source;
use once_cell::sync::Lazy;
use std::sync::Mutex;

static ENV_LOCK: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

struct EnvVarGuard {
    key: &'static str,
    original: Option<Option<String>>,
}

impl EnvVarGuard {
    fn set(key: &'static str, value: Option<&str>) -> Self {
        let original = std::env::var(key).ok();
        match value {
            Some(v) => std::env::set_var(key, v),
            None => std::env::remove_var(key),
        }
        Self {
            key,
            original: Some(original),
        }
    }
}

impl Drop for EnvVarGuard {
    fn drop(&mut self) {
        match self.original.take().flatten() {
            Some(val) => std::env::set_var(self.key, val),
            None => std::env::remove_var(self.key),
        }
    }
}

#[test]
fn cancelled_is_false_outside_scope() {
    let out = eval_source("(async::cancelled?)", None).expect("eval cancelled?");
    assert_eq!(out, Value::Bool(false));
}

#[test]
fn body_completion_cancels_children() {
    let src = "
      (let [events (chan)
            {:keys [await]} (async-scope
                              [(future (fn [] (chan-take! (async::cancel-chan))
                                         (chan-put! events :child)))]
                              (chan-put! events :body)
                              :body)]
        [(await) (chan-take! events) (chan-take! events)])
    ";
    let out = eval_source(src, None).expect("eval async-scope");
    let expected = eval_source("[:body :body :child]", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn cancel_function_triggers_scope_cancellation() {
    let src = "
      (let [{:keys [cancel! await]} (async-scope
                                      [(future (fn [] (chan-take! (async::cancel-chan)) :child))]
                                      (chan-take! (async::cancel-chan))
                                      :body)]
        (cancel!)
        (await))
    ";
    let out = eval_source(src, None).expect("eval cancel!");
    let expected = eval_source(":body", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn promise_like_body_warns_in_default_mode() {
    let _lock = ENV_LOCK.lock().unwrap();
    let _env_guard = EnvVarGuard::set("ASYNC_SCOPE_STRICT", None);
    let src = "(let [{:keys [await]} (async-scope [] (future (fn [] 1)))] (await))";
    let out = eval_source(src, None).expect("eval promise-like body");
    assert!(matches!(out, Value::Future(_)));
}

#[test]
fn promise_like_body_errors_in_strict_mode() {
    let _lock = ENV_LOCK.lock().unwrap();
    let _env_guard = EnvVarGuard::set("ASYNC_SCOPE_STRICT", Some("1"));
    let src = "(let [{:keys [await]} (async-scope [] (future (fn [] 1)))] (await))";
    let err = eval_source(src, None).expect_err("strict mode should error");
    assert!(
        err.to_string()
            .contains("main-body must not return promise-like"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn child_error_triggers_fail_fast_cancel() {
    let src = "
      (let [events (chan)]
        (do
          (try
            (let [{:keys [await]} (async-scope
                                    [(future (fn [] (throw (runtime-error \"boom\"))))
                                     (future (fn [] (chan-take! (async::scope-cancel-chan))
                                                    (chan-put! events :cancelled)))]
                                    (chan-take! (async::scope-cancel-chan)))]
              (await))
            (catch RuntimeError e
              (chan-put! events :err)))
          (let [a (chan-take! events)
                b (chan-take! events)]
            #{a b})))
    ";
    let out = eval_source(src, None).expect("eval fail-fast");
    let set = match out {
        Value::Set(items) => items,
        other => panic!("expected set, got {:?}", other),
    };
    assert!(set.contains(&Value::Symbol(":err".into())));
    assert!(set.contains(&Value::Symbol(":cancelled".into())));
}

#[test]
fn scope_loop_supports_recur() {
    let src = "
      (let [counter (atom 0)
            {:keys [await]} (async-scope []
                                     (async::scope-loop [n 0]
                                       (swap! counter (fn [x] (+ x 1)))
                                       (if (< n 2)
                                         (recur (inc n))
                                         :done)))]
        [(await) (atom-deref counter)])
    ";
    let out = eval_source(src, None).expect("eval scope-loop recur");
    let expected = eval_source("[:done 3]", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn cancelled_propogates_to_future_child() {
    let src = "
      (let [p1 (promise)
            p2 (promise)
            {:keys [await cancel!]} (async-scope
                                      [(future (fn []
                                                 (promise-deliver! p1 (async::cancelled?))
                                                 (async::scope-select [(timeout 20ms)])
                                                 (promise-deliver! p2 (async::cancelled?))))]
                                      (async::scope-select [(timeout 200ms)]))]
        (promise-deref p1)
        (cancel!)
        [ (await)
          (promise-deref p1)
          (promise-deref p2)])
    ";
    let out = eval_source(src, None).expect("eval cancelled? propagation");
    let expected = eval_source("[:cancelled false true]", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn scope_loop_respects_scope_cancel_in_future_child() {
    let src = "
      (let [started (promise)
            {:keys [await]} (async-scope
                              [(future (fn []
                                         (async::scope-loop []
                                           (promise-deliver! started :started)
                                           (let [sel (async::scope-select [(timeout 2ms)])]
                                             (when (not= sel :cancelled)
                                               (recur))))))]
                              (promise-deref started))]
        (await))
    ";
    let out = eval_source(src, None).expect("eval scope-loop cancel in future child");
    let expected = eval_source(":started", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn scope_loop_future_child_produces_events() {
    let src = "
      (let [events (chan 4)
            {:keys [await]} (async-scope
                              [(future (fn []
                                         (async::scope-loop []
                                           (chan-put! events :tick)
                                           (async::scope-select [(timeout 1ms)])
                                           (recur))))]
                              (chan-take! events))]
        (await))
    ";
    let out = eval_source(src, None).expect("eval scope-loop future child events");
    let expected = eval_source(":tick", None).expect("expected");
    assert_eq!(out, expected);
}

#[test]
fn scope_select_returns_cancelled() {
    let src = "
      (let [{:keys [cancel! await]} (async-scope []
                                           (async::scope-select [(timeout 200ms)]))]
        (cancel!)
        (await))
    ";
    let out = eval_source(src, None).expect("eval scope-select");
    assert_eq!(out, Value::Symbol(":cancelled".into()));
}

#[test]
fn async_scope_alias_works() {
    let src = "(let [{:keys [await]} (async::scope [] :ok)] (await))";
    let out = eval_source(src, None).expect("eval async::scope");
    assert_eq!(out, Value::Symbol(":ok".into()));
}
