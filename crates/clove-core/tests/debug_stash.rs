use std::sync::{Mutex, OnceLock};

use clove_core::ast::Value;
use clove_core::env::EnvRef;
use clove_core::error::CloveError;
use clove_core::eval::Evaluator;
use clove_core::options::EvalOptions;
use clove_core::repl;
use clove_core::runtime::RuntimeCtx;
use clove_core::settings::{FeatureToggle, RuntimeFeatureId, MAIN_PACKAGE_ID};

#[derive(Debug)]
struct DebugSnapshot {
    subject: Value,
    subject_alias: Value,
    stash: Value,
    last: Value,
    func: Value,
    func_alias: Value,
    args: Value,
    args_alias: Value,
    call: Value,
    call_alias: Value,
    error: Value,
    error_alias: Value,
}

static DEBUG_REPL_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
static DEBUG_SNAPSHOT: OnceLock<Mutex<Option<DebugSnapshot>>> = OnceLock::new();

fn store_snapshot(snapshot: DebugSnapshot) {
    let slot = DEBUG_SNAPSHOT.get_or_init(|| Mutex::new(None));
    *slot.lock().unwrap() = Some(snapshot);
}

fn take_snapshot() -> Option<DebugSnapshot> {
    let slot = DEBUG_SNAPSHOT.get_or_init(|| Mutex::new(None));
    slot.lock().unwrap().take()
}

fn clear_snapshot() {
    let slot = DEBUG_SNAPSHOT.get_or_init(|| Mutex::new(None));
    *slot.lock().unwrap() = None;
}

fn capture_debug_repl(_evaluator: &Evaluator, env: EnvRef) -> Result<Value, CloveError> {
    let guard = env.read().unwrap();
    let snapshot = DebugSnapshot {
        subject: guard.get("?").unwrap_or(Value::Nil),
        subject_alias: guard.get("?v").unwrap_or(Value::Nil),
        stash: guard.get("*?").unwrap_or(Value::Nil),
        last: guard.get("*1").unwrap_or(Value::Nil),
        func: guard.get("*f").unwrap_or(Value::Nil),
        func_alias: guard.get("?f").unwrap_or(Value::Nil),
        args: guard.get("*args").unwrap_or(Value::Nil),
        args_alias: guard.get("?args").unwrap_or(Value::Nil),
        call: guard.get("*call").unwrap_or(Value::Nil),
        call_alias: guard.get("?call").unwrap_or(Value::Nil),
        error: guard.get("*e").unwrap_or(Value::Nil),
        error_alias: guard.get("?e").unwrap_or(Value::Nil),
    };
    store_snapshot(snapshot);
    Ok(Value::Nil)
}

struct DebugHandlerGuard;

impl DebugHandlerGuard {
    fn install() -> Self {
        repl::set_debug_repl_handler(Some(capture_debug_repl));
        Self
    }
}

impl Drop for DebugHandlerGuard {
    fn drop(&mut self) {
        repl::set_debug_repl_handler(None);
    }
}

#[test]
fn debug_repl_stash_subject_is_first_arg() {
    let _lock = DEBUG_REPL_LOCK
        .get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap();
    clear_snapshot();
    let _handler_guard = DebugHandlerGuard::install();

    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    ctx.settings().assign_feature_toggle(
        FeatureToggle::Runtime(RuntimeFeatureId::ReplOnError),
        MAIN_PACKAGE_ID,
        true,
    );
    let result = ctx.eval_source("(/ 1 0)");
    assert!(result.is_err());

    let snapshot = take_snapshot().expect("debug repl snapshot missing");
    assert_eq!(snapshot.subject, Value::Int(1));
    assert_eq!(snapshot.subject_alias, Value::Int(1));
    assert_eq!(snapshot.stash, Value::Int(1));
    assert_eq!(snapshot.last, Value::Int(1));
    assert!(!matches!(snapshot.func, Value::Nil));
    assert_eq!(snapshot.func, snapshot.func_alias);
    assert_eq!(snapshot.args, snapshot.args_alias);
    assert_eq!(snapshot.call, snapshot.call_alias);
    assert_eq!(snapshot.error, snapshot.error_alias);
    match snapshot.args {
        Value::Vector(vec) => {
            assert_eq!(vec.len(), 2);
            assert_eq!(vec.get(0).cloned(), Some(Value::Int(1)));
            assert_eq!(vec.get(1).cloned(), Some(Value::Int(0)));
        }
        other => panic!("*args is not vector: {}", other.type_name()),
    }
    match snapshot.call {
        Value::Map(_) => {}
        other => panic!("*call is not map: {}", other.type_name()),
    }
    match snapshot.error {
        Value::String(s) => assert!(!s.is_empty()),
        other => panic!("*e is not string: {}", other.type_name()),
    }
}
