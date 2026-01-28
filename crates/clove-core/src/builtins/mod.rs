use crate::ast::Value;
use crate::env::{new_ref, Env, EnvRef};
use crate::error::CloveError;

pub(crate) use shared::*;

#[macro_export]
macro_rules! def_builtin {

    ($env:expr, $name:expr, $arity:expr, |$args:ident| $body:block) => {
        $env.define_builtin(
            $name,
            $crate::ast::Value::native_fn_with_name($name, $arity, move |$args: &[$crate::ast::Value]| -> Result<$crate::ast::Value, $crate::error::CloveError> {
                $body
            }),
        );
    };
    ($env:expr, $name:expr, $arity:expr, |$args:ident| $body:expr) => {
        $env.define_builtin(
            $name,
            $crate::ast::Value::native_fn_with_name($name, $arity, move |$args: &[$crate::ast::Value]| -> Result<$crate::ast::Value, $crate::error::CloveError> {
                $body
            }),
        );
    };

}

pub use def_builtin;

pub fn default_env() -> EnvRef {
    let mut env = Env::default();
    install_core(&mut env);
    new_ref(env)
}

pub fn err<T>(msg: impl Into<String>) -> Result<T, CloveError> {
    Err(CloveError::runtime(msg))
}

pub fn glob_paths(pattern: &str) -> Result<Vec<String>, CloveError> {
    fs::glob_paths(pattern)
}

pub fn glob_paths_eager(pattern: &str) -> Result<Vec<String>, CloveError> {
    fs::glob_paths_eager(pattern)
}

pub fn json_read_file_vec(path: &str) -> Result<Vec<Value>, CloveError> {
    let value = json::read_json_file_value(path)?;
    match value {
        Value::Vector(items) | Value::List(items) => Ok(items.into_iter().collect()),
        other => Err(CloveError::runtime(format!(
            "json::read-file expected array, got {}",
            other.type_name()
        ))),
    }
}

pub fn get_value(
    target: &Value,
    key: &Value,
    default: Option<&Value>,
) -> Result<Value, CloveError> {
    seq::get_value(target, key, default)
}

pub fn includes_value(target: &Value, needle: &Value) -> Result<bool, CloveError> {
    seq::includes_value(target, needle)
}

mod concurrency;
pub(crate) mod core;
mod data;
mod duration;
mod functional;
mod math;
pub(crate) mod memo;

mod cli;
mod config;
mod dag;
mod fs;
mod http;
mod io;
mod json;
mod log;
mod native;
mod path;
mod predicates;
mod process;
mod seq;
mod shared;
mod shell;
mod string;
mod term;
mod test;
mod time;
mod walk;
mod yaml;

pub fn install_core_primitives(env: &mut Env) {
    math::install(env);
    core::install(env);
    seq::install_primitives(env);
    string::install(env);
    predicates::install(env);
}

pub fn install_clove_extras(env: &mut Env) {
    duration::install(env);
    functional::install(env);
    seq::install_derived(env);
    memo::install(env);
    walk::install(env);
    concurrency::install(env);
    dag::install(env);
    fs::install(env);
    io::install(env);
    term::install(env);
    path::install(env);
    time::install(env);
    json::install(env);
    yaml::install(env);
    config::install(env);
    http::install(env);
    shell::install(env);
    process::install(env);
    cli::install(env);
    test::install(env);
    log::install(env);
    data::install(env);
    native::install(env);
}

pub fn install_core(env: &mut Env) {
    install_core_primitives(env);
    install_clove_extras(env);
}
