use crate::ast::{FnArity, Value};
use crate::builtins::shared::{serde_to_value, value_to_serde};
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use std::fs;
use std::path::Path;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "yaml::parse", FnArity::exact(1), |args| match args {
        [Value::String(s)] => parse_yaml(s),
        _ => err("yaml::parse expects string"),
    });
    def_builtin!(
        env,
        "yaml::generate",
        FnArity::exact(1),
        |args| match args {
            [v] => generate_yaml(v, true),
            _ => err("yaml::generate expects value"),
        }
    );
    def_builtin!(
        env,
        "yaml::generate-pretty",
        FnArity::exact(1),
        |args| match args {
            [v] => generate_yaml(v, true),
            _ => err("yaml::generate-pretty expects value"),
        }
    );
    def_builtin!(
        env,
        "yaml::read-file",
        FnArity::exact(1),
        |args| match args {
            [Value::String(path)] => {
                let content = fs::read_to_string(path)
                    .map_err(|e| CloveError::runtime(format!("yaml::read-file failed: {}", e)))?;
                parse_yaml(&content)
            }
            _ => err("yaml::read-file expects path string"),
        }
    );
    def_builtin!(
        env,
        "yaml::write-file",
        FnArity::exact(2),
        |args| match args {
            [Value::String(path), v] => {
                let yaml = generate_yaml(v, true)?;
                let s = match yaml {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                if let Some(parent) = Path::new(path).parent() {
                    if !parent.as_os_str().is_empty() {
                        fs::create_dir_all(parent).map_err(|e| {
                            CloveError::runtime(format!("yaml::write-file failed: {}", e))
                        })?;
                    }
                }
                fs::write(path, s)
                    .map_err(|e| CloveError::runtime(format!("yaml::write-file failed: {}", e)))?;
                Ok(Value::String(path.clone()))
            }
            _ => err("yaml::write-file expects path and value"),
        }
    );
}

fn parse_yaml(s: &str) -> Result<Value, CloveError> {
    let parsed: serde_yaml::Value = serde_yaml::from_str(s)
        .map_err(|e| CloveError::runtime(format!("yaml parse error: {}", e)))?;
    let json = serde_json::to_value(parsed)
        .map_err(|e| CloveError::runtime(format!("yaml conversion error: {}", e)))?;
    serde_to_value(&json)
}

fn generate_yaml(v: &Value, pretty: bool) -> Result<Value, CloveError> {
    let serde_val = value_to_serde(v)?;
    let out = if pretty {
        serde_yaml::to_string(&serde_val)
    } else {
        serde_yaml::to_string(&serde_val)
    }
    .map_err(|e| CloveError::runtime(format!("yaml generate error: {}", e)))?;
    let beautified = if pretty {
        out.replace("\n-", "\n  -")
    } else {
        out
    };
    Ok(Value::String(beautified))
}
