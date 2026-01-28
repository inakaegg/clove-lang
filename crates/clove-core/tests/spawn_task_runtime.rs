use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use clove_core::ast::{FnArity, Value};
use clove_core::concurrency::spawn_task;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

fn unique_temp_dir(label: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    std::env::temp_dir().join(format!(
        "clove-test-{}-{}-{}",
        label,
        std::process::id(),
        nanos
    ))
}

#[test]
fn spawn_task_inherits_working_dir() {
    let workdir = unique_temp_dir("spawn-task-dir");
    std::fs::create_dir_all(&workdir).unwrap();
    let mut opts = EvalOptions::default();
    opts.working_dir = Some(workdir.clone());
    let ctx = RuntimeCtx::new(opts, &[]);
    let src = r#"(let [t (spawn (fn [] (path::resolve "child.txt")))] (task-deref t))"#;
    let value = ctx.eval_source(src).unwrap();
    let expected = workdir.join("child.txt");
    assert_eq!(value, Value::String(expected.to_string_lossy().to_string()));
    let _ = std::fs::remove_dir_all(&workdir);
}

#[test]
fn spawn_task_allows_runtime_with_current() {
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let callable = Value::native_fn(FnArity::exact(0), |_args| {
        RuntimeCtx::with_current(|_ctx| Ok(Value::Bool(true)))
    });
    let task = ctx.with_current_ctx(|_ctx| spawn_task(callable));
    let result = task.wait().unwrap();
    assert_eq!(result, Value::Bool(true));
}
