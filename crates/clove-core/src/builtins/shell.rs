use crate::ast::{FnArity, Value};
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "shell::split", FnArity::exact(1), |args| match args {
        [Value::String(s)] => split(s),
        _ => err("shell::split expects string"),
    });
    def_builtin!(env, "shell::escape", FnArity::exact(1), |args| match args {
        [Value::Vector(v)] | [Value::List(v)] => escape(v),
        _ => err("shell::escape expects vector of strings"),
    });
}

fn split(s: &str) -> Result<Value, CloveError> {
    let parts = shell_words::split(s)
        .map_err(|e| CloveError::runtime(format!("shell split error: {}", e)))?;
    Ok(Value::Vector(
        parts.into_iter().map(Value::String).collect(),
    ))
}

fn escape(items: &crate::ast::Vector<Value>) -> Result<Value, CloveError> {
    let mut args = Vec::with_capacity(items.len());
    for v in items {
        match v {
            Value::String(s) => args.push(s.clone()),
            Value::Symbol(s) => args.push(s.clone()),
            _ => return err("shell::escape expects strings"),
        }
    }
    Ok(Value::String(shell_words::join(args.iter())))
}
