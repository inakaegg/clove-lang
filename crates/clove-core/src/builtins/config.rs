use crate::ast::{FnArity, HashMap, Key, Value};
use crate::builtins::shared::{serde_to_value, value_to_serde};
use crate::builtins::{def_builtin, err, map_like_to_hashmap};
use crate::error::CloveError;
use configparser::ini::Ini as IniParser;
use std::fs;
use std::path::Path;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "toml::parse", FnArity::exact(1), |args| match args {
        [Value::String(s)] => parse_toml(s),
        _ => err("toml::parse expects string"),
    });
    def_builtin!(
        env,
        "toml::generate",
        FnArity::exact(1),
        |args| match args {
            [v] => generate_toml(v),
            _ => err("toml::generate expects value"),
        }
    );
    def_builtin!(
        env,
        "toml::read-file",
        FnArity::exact(1),
        |args| match args {
            [Value::String(path)] => {
                let content = fs::read_to_string(path)
                    .map_err(|e| CloveError::runtime(format!("toml::read-file failed: {}", e)))?;
                parse_toml(&content)
            }
            _ => err("toml::read-file expects path string"),
        }
    );
    def_builtin!(
        env,
        "toml::write-file",
        FnArity::exact(2),
        |args| match args {
            [Value::String(path), v] => {
                let text = generate_toml(v)?;
                let s = match text {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                if let Some(parent) = Path::new(path).parent() {
                    if !parent.as_os_str().is_empty() {
                        fs::create_dir_all(parent).map_err(|e| {
                            CloveError::runtime(format!("toml::write-file failed: {}", e))
                        })?;
                    }
                }
                fs::write(path, s)
                    .map_err(|e| CloveError::runtime(format!("toml::write-file failed: {}", e)))?;
                Ok(Value::String(path.clone()))
            }
            _ => err("toml::write-file expects path and value"),
        }
    );

    def_builtin!(env, "ini::parse", FnArity::exact(1), |args| match args {
        [Value::String(s)] => parse_ini(s),
        _ => err("ini::parse expects string"),
    });
    def_builtin!(env, "ini::generate", FnArity::exact(1), |args| match args {
        [v] => generate_ini(v),
        _ => err("ini::generate expects value"),
    });
    def_builtin!(env, "env::vars", FnArity::exact(0), |_args| {
        let mut map = HashMap::new();
        for (k, v) in std::env::vars() {
            map.insert(Key::String(k), Value::String(v));
        }
        Ok(Value::Map(map))
    });
    def_builtin!(env, "env::get", FnArity::exact(1), |args| match args {
        [Value::String(k)] => Ok(std::env::var(k).map(Value::String).unwrap_or(Value::Nil)),
        _ => err("env::get expects key string"),
    });
    def_builtin!(env, "env::has?", FnArity::exact(1), |args| match args {
        [Value::String(k)] => Ok(Value::Bool(std::env::var_os(k).is_some())),
        _ => err("env::has? expects key string"),
    });
    def_builtin!(
        env,
        "env::parse-file",
        FnArity::exact(1),
        |args| match args {
            [Value::String(path)] => parse_env_file(path),
            _ => err("env::parse-file expects path string"),
        }
    );
}

fn parse_toml(s: &str) -> Result<Value, CloveError> {
    let toml_val: toml::Value =
        toml::from_str(s).map_err(|e| CloveError::runtime(format!("toml parse error: {}", e)))?;
    let json = serde_json::to_value(toml_val)
        .map_err(|e| CloveError::runtime(format!("toml conversion error: {}", e)))?;
    serde_to_value(&json)
}

fn generate_toml(v: &Value) -> Result<Value, CloveError> {
    let json = value_to_serde(v)?;
    let text = toml::to_string(&json)
        .map_err(|e| CloveError::runtime(format!("toml generate error: {}", e)))?;
    Ok(Value::String(text))
}

fn parse_ini(s: &str) -> Result<Value, CloveError> {
    let mut parser = IniParser::new();
    parser.set_default_section("");
    parser
        .read(s.to_string())
        .map_err(|e| CloveError::runtime(format!("ini parse error: {}", e)))?;
    let mut out = HashMap::new();
    for (section, props) in parser.get_map_ref() {
        let mut kv = HashMap::new();
        for (k, v) in props {
            let val = v.clone().unwrap_or_default();
            kv.insert(crate::ast::Key::String(k.clone()), Value::String(val));
        }
        let key = if section.is_empty() {
            crate::ast::Key::String("".to_string())
        } else {
            crate::ast::Key::String(section.clone())
        };
        out.insert(key, Value::Map(kv));
    }
    Ok(Value::Map(out))
}

fn generate_ini(v: &Value) -> Result<Value, CloveError> {
    let map = map_like_to_hashmap(v, "ini::generate", 1)?;
    let mut ini = IniParser::new();
    let mut default_props: Vec<(String, String)> = Vec::new();
    for (k, v) in map {
        let section = match k {
            crate::ast::Key::String(s) => Some(s.clone()),
            crate::ast::Key::Keyword(s) => Some(s.clone()),
            crate::ast::Key::Symbol(s) => Some(s.clone()),
            crate::ast::Key::Number(n) => Some(n.to_string()),
            crate::ast::Key::Bool(b) => Some(b.to_string()),
        };
        let props = map_like_to_hashmap(&v, "ini::generate", 1)?;
        for (pk, pv) in props {
            let val_str = match pv {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Nil => "".to_string(),
                _ => return err("ini values must be scalar"),
            };
            let key_str = match pk {
                crate::ast::Key::String(s) => s.clone(),
                crate::ast::Key::Keyword(s) => s.clone(),
                crate::ast::Key::Symbol(s) => s.clone(),
                crate::ast::Key::Number(n) => n.to_string(),
                crate::ast::Key::Bool(b) => b.to_string(),
            };
            if let Some(section_name) = section.as_deref() {
                if section_name.is_empty() {
                    default_props.push((key_str, val_str));
                } else {
                    ini.set(section_name, &key_str, Some(val_str));
                }
            }
        }
    }
    let mut out = String::new();
    for (k, v) in default_props {
        out.push_str(&format!("{}={}\n", k, v));
    }
    out.push_str(&ini.writes());
    Ok(Value::String(out))
}

fn parse_env_file(path: &str) -> Result<Value, CloveError> {
    let content = fs::read_to_string(path)
        .map_err(|e| CloveError::runtime(format!("env::parse-file failed: {}", e)))?;
    let mut map = HashMap::new();
    let vars = dotenvy::from_read_iter(content.as_bytes());
    for entry in vars {
        let (k, v) = entry.map_err(|e| CloveError::runtime(format!("env parse error: {}", e)))?;
        map.insert(crate::ast::Key::String(k), Value::String(v));
    }
    Ok(Value::Map(map))
}
