use crate::ast::{FnArity, Value};
use crate::builtins::def_builtin;

pub(crate) fn install(env: &mut crate::env::Env) {
    let entries = [
        ("log::trace", "TRACE", false),
        ("log::debug", "DEBUG", false),
        ("log::info", "INFO", false),
        ("log::warn", "WARN", true),
        ("log::error", "ERROR", true),
    ];
    for (name, label, to_stderr) in entries {
        def_builtin!(env, name, FnArity::at_least(0), |args| {
            let mut parts = vec![label.to_string()];
            for a in args {
                parts.push(a.to_string());
            }
            let line = parts.join(" ");
            if to_stderr {
                eprintln!("{}", line);
            } else {
                println!("{}", line);
            }
            Ok(Value::Nil)
        });
    }
}
