use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clove_core::ast::Value;
use clove_core::error::CloveError;
use clove_core::foreign::ForeignEngine;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

pub mod deps;
pub mod doc;
mod native_plugins;
mod native_policy;
pub mod pkg;
pub mod plugin;
pub mod repl;
pub mod symbols;

/// Find and call -main in the runtime. Error if not found.
pub fn call_main(ctx: &Arc<RuntimeCtx>, args: &[String]) -> Result<Value, CloveError> {
    ctx.with_current_ctx(|ctx| {
        let default_ns = ctx.default_namespace_name();
        if let Some(env) = ctx.namespace_env(&default_ns) {
            let callable = {
                let guard = env.read().unwrap();
                guard.get("-main")
            };
            if let Some(callable) = callable {
                return ctx.call_main_with_top_level_err_fin(callable, args, env.clone());
            }
        }
        let mut ns_candidates = ctx.namespace_names();
        ns_candidates.sort();
        let mut found = Vec::new();
        for ns_name in ns_candidates {
            if ns_name == default_ns {
                continue;
            }
            if let Some(env) = ctx.namespace_env(&ns_name) {
                let callable = {
                    let guard = env.read().unwrap();
                    guard.get("-main")
                };
                if let Some(callable) = callable {
                    found.push((ns_name, callable, env.clone()));
                }
            }
        }
        if found.len() == 1 {
            let (_ns, callable, env) = found.remove(0);
            return ctx.call_main_with_top_level_err_fin(callable, args, env);
        }
        if found.len() > 1 {
            let names: Vec<String> = found.iter().map(|(ns, _, _)| ns.clone()).collect();
            return Err(CloveError::runtime(format!(
                "multiple `-main` found in namespaces: {}",
                names.join(", ")
            )));
        }
        let env = ctx.env();
        let callable = {
            let guard = env.read().unwrap();
            guard.get("-main")
        };
        if let Some(callable) = callable {
            return ctx.call_main_with_top_level_err_fin(callable, args, env);
        }
        let fallback_ns = ctx.default_namespace_name();
        Err(CloveError::runtime(format!(
            "`-main` not found in any namespace (default: {})",
            fallback_ns
        )))
    })
}

/// Infer new namespaces from the delta against the existing namespace list.
pub fn pick_new_namespace(before: &[String], after: &[String]) -> Option<String> {
    let before_set: HashSet<&str> = before.iter().map(|s| s.as_str()).collect();
    let mut candidates = Vec::new();
    for ns in after {
        if !before_set.contains(ns.as_str()) && !matches!(ns.as_str(), "core" | "async" | "std") {
            candidates.push(ns.clone());
        }
    }
    candidates.sort();
    candidates.pop()
}

pub fn default_engines() -> Vec<Arc<dyn ForeignEngine>> {
    let mut engines: Vec<Arc<dyn ForeignEngine>> = Vec::new();
    #[cfg(feature = "ruby")]
    {
        engines.extend(clove_ruby::engines());
    }
    #[cfg(feature = "python")]
    {
        engines.extend(clove_python::engines());
    }
    engines
}

pub fn install_lang_extras(ctx: &RuntimeCtx) {
    if !ctx.mark_lang_extras_installed() {
        return;
    }
    native_plugins::install(ctx);
}

pub use native_plugins::{set_native_plugin_config, NativePluginConfig};

pub fn create_runtime(opts: EvalOptions, engines: &[Arc<dyn ForeignEngine>]) -> Arc<RuntimeCtx> {
    let ctx = RuntimeCtx::new(opts, engines);
    install_lang_extras(&ctx);
    ctx
}

pub fn eval_source_with_lang_features(
    src: &str,
    opts: EvalOptions,
    engines: &[Arc<dyn ForeignEngine>],
) -> Result<Value, CloveError> {
    let ctx = create_runtime(opts, engines);
    ctx.eval_source(src)
}

/// Evaluate the source and call -main if needed.
/// Set `source_name`/`working_dir` on `opts` beforehand.
pub fn run_source_with_lang_features(
    src: &str,
    opts: EvalOptions,
    engines: &[Arc<dyn ForeignEngine>],
    run_main: bool,
    script_args: &[String],
) -> Result<Value, CloveError> {
    let ctx = create_runtime(opts, engines);
    let before_names = ctx.namespace_names();
    let eval_value = ctx.eval_source(src)?;
    let after_names = ctx.namespace_names();
    let file_path = ctx
        .source_name()
        .as_deref()
        .and_then(|name| resolve_source_path(name));
    let mut default_ns = file_path
        .as_deref()
        .and_then(|path| ctx.namespace_for_path(path));
    if default_ns.is_none() {
        default_ns = pick_new_namespace(&before_names, &after_names);
    }
    if let Some(ns) = default_ns {
        ctx.set_default_namespace_name(ns);
    }
    if run_main {
        call_main(&ctx, script_args)
    } else {
        Ok(eval_value)
    }
}

fn resolve_source_path(raw: &str) -> Option<PathBuf> {
    let path = Path::new(raw);
    if path.is_absolute() {
        Some(path.to_path_buf())
    } else {
        std::env::current_dir().ok().map(|cwd| cwd.join(path))
    }
}
