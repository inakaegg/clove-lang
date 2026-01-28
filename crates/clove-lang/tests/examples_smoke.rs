use clove_core::ast::Value;
use clove_core::error::CloveError;
use clove_core::eval::call_callable;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn example_path(rel: &str) -> PathBuf {
    workspace_root().join(rel)
}

fn run_example(rel: &str) -> Result<(), CloveError> {
    let path = example_path(rel);
    let mut opts = EvalOptions::default();
    if let Some(parent) = path.parent() {
        opts.working_dir = Some(parent.to_path_buf());
    }
    opts.source_name = Some(path.to_string_lossy().into_owned());
    let ctx = RuntimeCtx::new(opts, &[]);
    ctx.eval_file(&path)?;
    let _ = call_main(&ctx)?;
    Ok(())
}

fn call_main(ctx: &RuntimeCtx) -> Result<Value, CloveError> {
    let argv: Vec<Value> = Vec::new();
    for ns in ctx.namespace_names() {
        if let Some(env) = ctx.namespace_env(&ns) {
            if let Some(callable) = env.read().unwrap().get("-main") {
                return call_callable(callable, argv.clone());
            }
        }
    }
    if let Some(callable) = ctx.env().read().unwrap().get("-main") {
        return call_callable(callable, argv);
    }
    Err(CloveError::runtime("`-main` not found in example"))
}

#[test]
fn examples_with_main_run_as_smoke() {
    let samples = [
        "examples/config/json_yaml.clv",
        "examples/concurrency/async_scope_nested.clv",
        "examples/concurrency/channels.clv",
        "examples/types/demo.clv",
    ];
    for rel in samples {
        run_example(rel).unwrap_or_else(|e| panic!("{} failed: {}", rel, e));
    }
}
