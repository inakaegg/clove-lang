use crate::ast::{FnArity, HashMap, Key, Value, Vector};
use crate::builtins::shared::value_eq;
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use crate::eval::call_callable;
use im::HashMap as ImHashMap;
use once_cell::sync::Lazy;
use std::sync::Mutex;

static TEST_REGISTRY: Lazy<Mutex<ImHashMap<String, Value>>> =
    Lazy::new(|| Mutex::new(ImHashMap::new()));
static TEST_CONTEXT: Lazy<Mutex<Vec<String>>> = Lazy::new(|| Mutex::new(Vec::new()));

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "test::deftest", FnArity::exact(2), |args| match args {
        [name, func] => register_test(name, func),
        _ => err("test::deftest expects name and function"),
    });
    def_builtin!(env, "test::is", FnArity::range(1, 3), |args| match args {
        [val] => assert_truthy(val, None),
        [expected, actual] => assert_equal(expected, actual, None),
        [expected, actual, msg] => assert_equal(expected, actual, Some(msg)),
        _ => err("test::is expects 1 to 3 arguments"),
    });
    def_builtin!(
        env,
        "test::run-tests",
        FnArity::exact(0),
        |_args| run_tests()
    );
    def_builtin!(env, "test::clear!", FnArity::exact(0), |_args| {
        TEST_REGISTRY.lock().unwrap().clear();
        Ok(Value::Bool(true))
    });
    def_builtin!(env, "test::testing", FnArity::exact(2), |args| match args {
        [Value::String(desc), func] => testing(desc, func),
        [Value::Symbol(desc), func] => testing(desc, func),
        _ => err("test::testing expects description string/symbol and function"),
    });
}

fn register_test(name: &Value, func: &Value) -> Result<Value, CloveError> {
    let id = match name {
        Value::String(s) => s.clone(),
        Value::Symbol(s) => s.clone(),
        _ => return err("test name must be string/symbol"),
    };
    match func {
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => {
            TEST_REGISTRY.lock().unwrap().insert(id, func.clone());
            Ok(Value::Bool(true))
        }
        _ => err("test::deftest expects callable"),
    }
}

fn assert_truthy(val: &Value, msg: Option<&Value>) -> Result<Value, CloveError> {
    if super::shared::truthy(val) {
        Ok(Value::Bool(true))
    } else {
        let ctx = current_context();
        let base = msg
            .map(|m| m.to_string())
            .unwrap_or_else(|| "assertion failed".to_string());
        err(format!("{}{}", ctx, base))
    }
}

fn assert_equal(
    expected: &Value,
    actual: &Value,
    msg: Option<&Value>,
) -> Result<Value, CloveError> {
    if value_eq(expected, actual) {
        Ok(Value::Bool(true))
    } else {
        let ctx = current_context();
        let base = msg
            .map(|m| m.to_string())
            .unwrap_or_else(|| format!("expected {}, got {}", expected, actual));
        err(format!("{}{}", ctx, base))
    }
}

fn run_tests() -> Result<Value, CloveError> {
    let registry = TEST_REGISTRY.lock().unwrap().clone();
    let mut results = Vector::new();
    let mut pass = 0;
    let mut fail = 0;
    for (name, func) in registry {
        let res = call_callable(func.clone(), vec![]);
        match res {
            Ok(_) => {
                pass += 1;
                results.push_back(result_entry(&name, "pass", Value::Nil));
            }
            Err(e) => {
                fail += 1;
                results.push_back(result_entry(&name, "fail", Value::String(e.to_string())));
            }
        }
    }
    let mut out = HashMap::new();
    out.insert(Key::Keyword("pass".into()), Value::Int(pass));
    out.insert(Key::Keyword("fail".into()), Value::Int(fail));
    out.insert(Key::Keyword("results".into()), Value::Vector(results));
    Ok(Value::Map(out))
}

fn result_entry(name: &str, status: &str, error: Value) -> Value {
    let mut map = HashMap::new();
    map.insert(Key::Keyword("name".into()), Value::String(name.to_string()));
    map.insert(
        Key::Keyword("status".into()),
        Value::String(status.to_string()),
    );
    map.insert(Key::Keyword("error".into()), error);
    Value::Map(map)
}

fn testing(desc: &str, func: &Value) -> Result<Value, CloveError> {
    match func {
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => {
            {
                let mut ctx = TEST_CONTEXT.lock().unwrap();
                ctx.push(desc.to_string());
            }
            let res = call_callable(func.clone(), vec![]);
            {
                let mut ctx = TEST_CONTEXT.lock().unwrap();
                ctx.pop();
            }
            res
        }
        _ => err("test::testing expects callable"),
    }
}

fn current_context() -> String {
    let ctx = TEST_CONTEXT.lock().unwrap();
    if ctx.is_empty() {
        "".to_string()
    } else {
        format!("[{}] ", ctx.join(" > "))
    }
}
