use std::borrow::Cow;
use std::collections::HashSet as StdHashSet;
use std::sync::Arc;

use pyo3::types::{PyAny, PyDict, PyFloat, PyList, PyModule, PySet, PyString};
use pyo3::{IntoPy, Py, PyObject, PyResult, Python, ToPyObject};

use clove_core::ast::{DurationUnit, Form, HashMap, RegexValue, Span, Value, Vector};
use clove_core::env::EnvRef;
use clove_core::error::CloveError;
use clove_core::eval::to_key_value;
use clove_core::foreign::{ForeignEngine, ForeignValue};
use clove_core::form_to_string::form_to_string;
use im::HashSet;

#[allow(dead_code)]
struct PythonForeignValue {
    value: PyObject,
}

unsafe impl Send for PythonForeignValue {}
unsafe impl Sync for PythonForeignValue {}

pub struct PythonEngine;

impl PythonEngine {
    pub fn new() -> Arc<Self> {
        Arc::new(Self)
    }
}

impl ForeignEngine for PythonEngine {
    fn tag(&self) -> &str {
        "py"
    }

    fn eval_block(&self, code: &str, env: EnvRef, span: Option<Span>) -> Result<Value, CloveError> {
        Python::with_gil(|py| {
            let normalized = normalize_python_block(code);
            let main = PyModule::import(py, "__main__")
                .map_err(|e| CloveError::foreign("py", format_with_span(span, e.to_string())))?;
            let scope = main.dict();
            let mut existing = StdHashSet::new();
            for key in scope.keys().iter() {
                if let Ok(name) = key.extract::<String>() {
                    existing.insert(name);
                }
            }

            let env_snapshot = PyDict::new(py);
            let mut injected: Vec<(String, Option<PyObject>)> = Vec::new();
            let mut tracked = StdHashSet::new();
            for (k, v) in env.read().unwrap().flatten() {
                if valid_ident(&k) && !k.starts_with("__") {
                    if let Ok(pv) = to_python(py, &v) {
                        let prev = scope.get_item(&k).ok().map(|obj| obj.to_object(py));
                        scope
                            .set_item(&k, pv.clone_ref(py))
                            .map_err(|e| e.to_string())
                            .ok();
                        env_snapshot
                            .set_item(&k, pv)
                            .map_err(|e| e.to_string())
                            .ok();
                        injected.push((k.clone(), prev));
                        tracked.insert(k.clone());
                    }
                }
            }
            let prev_env = scope.get_item("__env__").ok().map(|obj| obj.to_object(py));
            scope.set_item("__env__", env_snapshot).ok();
            injected.push(("__env__".into(), prev_env));

            let helper =
                python_block_helper(py).map_err(|e| CloveError::foreign("py", e.to_string()))?;
            let result = helper
                .as_ref(py)
                .call1((normalized.as_ref(), scope))
                .map_err(|e| CloveError::foreign("py", format_python_err(py, e, span)))?;

            let mut updates = Vec::new();
            for (key, val) in scope.iter() {
                let name = match key.extract::<String>() {
                    Ok(n) => n,
                    Err(_) => continue,
                };
                if !valid_ident(&name) || name.starts_with("__") {
                    continue;
                }
                let should_sync = tracked.contains(&name) || !existing.contains(&name);
                if !should_sync {
                    continue;
                }
                if let Ok(v) = from_python(py, val) {
                    updates.push((name, v));
                }
            }
            {
                let mut env_mut = env.write().unwrap();
                for (name, value) in updates {
                    env_mut.set_in_chain(&name, value);
                }
            }

            for (name, prev) in injected {
                if let Some(obj) = prev {
                    scope.set_item(&name, obj).ok();
                } else {
                    scope.del_item(&name).ok();
                }
            }

            from_python(py, result).map_err(|e| CloveError::foreign("py", e))
        })
    }

    fn eval_fallback(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        let src = form_to_string(form, self.tag());
        self.eval_block(&src, env, Some(form.span))
    }

    fn call_symbol(
        &self,
        path: &str,
        args: &[Value],
        span: Option<Span>,
    ) -> Result<Value, CloveError> {
        Python::with_gil(|py| {
            let parts: Vec<&str> = path.split('.').collect();
            if parts.len() < 2 {
                return Err(CloveError::foreign(
                    "py",
                    format_with_span(
                        span,
                        format!("invalid foreign symbol (need receiver.attr): {}", path),
                    ),
                ));
            }
            let method = *parts.last().expect("parts has at least one element");
            let main = PyModule::import(py, "__main__")
                .map_err(|e| CloveError::foreign("py", format_with_span(span, e.to_string())))?;

            let mut current: PyObject = match main.getattr(parts[0]) {
                Ok(obj) => obj.into_py(py),
                Err(_) => PyModule::import(py, parts[0])
                    .map_err(|e| CloveError::foreign("py", format_with_span(span, e.to_string())))?
                    .into_py(py),
            };
            for seg in parts.iter().skip(1).take(parts.len().saturating_sub(2)) {
                current = current
                    .as_ref(py)
                    .getattr(*seg)
                    .map_err(|e| CloveError::foreign("py", format_with_span(span, e.to_string())))?
                    .into_py(py);
            }

            let py_args_vec = args
                .iter()
                .map(|a| to_python(py, a))
                .collect::<PyResult<Vec<_>>>()
                .map_err(|e| CloveError::foreign("py", format_with_span(span, e.to_string())))?;

            let py_args = pyo3::types::PyTuple::new(py, py_args_vec);

            let result = current
                .as_ref(py)
                .call_method1(method, py_args)
                .map_err(|e| CloveError::foreign("py", format_python_err(py, e, span)))?;

            from_python(py, result)
                .map_err(|e| CloveError::foreign("py", format_with_span(span, e)))
        })
    }
}

fn valid_ident(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

const PY_BLOCK_HELPER_SRC: &str = r#"
import ast

def exec_block(src, scope):
    module = ast.parse(src, mode='exec')
    result_expr = None
    if module.body and isinstance(module.body[-1], ast.Expr):
        result_expr = ast.Expression(module.body[-1].value)
        module.body = module.body[:-1]
    exec_module = ast.Module(body=module.body, type_ignores=[])
    if exec_module.body:
        exec(compile(exec_module, '<string>', 'exec'), scope, scope)
    if result_expr is not None:
        return eval(compile(result_expr, '<string>', 'eval'), scope, scope)
    return None
"#;

fn python_block_helper(py: Python<'_>) -> PyResult<Py<PyAny>> {
    let module = PyModule::from_code(
        py,
        PY_BLOCK_HELPER_SRC,
        "clove_py_helper.py",
        "clove_py_helper",
    )?;
    Ok(module.getattr("exec_block")?.into_py(py))
}

fn to_python(py: Python<'_>, val: &Value) -> PyResult<PyObject> {
    match val {
        Value::Int(n) => Ok((*n).into_py(py)),
        Value::Float(n) => Ok(PyFloat::new(py, *n).into()),
        Value::String(s) => Ok(PyString::new(py, s).into()),
        Value::Bool(b) => Ok(b.into_py(py)),
        Value::Nil => Ok(py.None()),
        Value::Duration(d) => Ok(PyFloat::new(py, d.to_unit(DurationUnit::Second)).into()),
        Value::Regex(re) => {
            let re_mod = PyModule::import(py, "re")?;
            re_mod
                .call_method("compile", (re.pattern.as_str(),), None)
                .map(|obj| obj.into())
        }
        Value::Seq(handle) => {
            let items = handle
                .collect_all()
                .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
            let mut elems = Vec::with_capacity(items.len());
            for item in items {
                elems.push(to_python(py, &item)?);
            }
            Ok(PyList::new(py, elems).into())
        }
        Value::List(items) | Value::Vector(items) => {
            let mut elems = Vec::with_capacity(items.len());
            for item in items {
                elems.push(to_python(py, item)?);
            }
            Ok(PyList::new(py, elems).into())
        }
        Value::SortedMap(map) => {
            let dict = PyDict::new(py);
            for (k, v) in &map.entries {
                dict.set_item(key_to_python(py, k)?, to_python(py, v)?)?;
            }
            Ok(dict.into())
        }
        Value::Map(map) => {
            let dict = PyDict::new(py);
            for (k, v) in map {
                dict.set_item(key_to_python(py, k)?, to_python(py, v)?)?;
            }
            Ok(dict.into())
        }
        Value::SortedSet(items) => {
            let mut elems = Vec::with_capacity(items.entries.len());
            for item in &items.entries {
                elems.push(to_python(py, item)?);
            }
            Ok(PySet::new(py, &elems)?.into())
        }
        Value::Set(items) => {
            let mut elems = Vec::with_capacity(items.len());
            for item in items {
                elems.push(to_python(py, item)?);
            }
            Ok(PySet::new(py, &elems)?.into())
        }
        Value::Symbol(s) => Ok(PyString::new(py, s).into()),
        Value::ForeignCallable { .. } => Err(pyo3::exceptions::PyTypeError::new_err(
            "cannot pass foreign callable into py block",
        )),
        Value::Foreign(_) => Err(pyo3::exceptions::PyTypeError::new_err(
            "cannot pass foreign value into py block",
        )),
        Value::NativeBuf { .. } => Err(pyo3::exceptions::PyTypeError::new_err(
            "cannot pass native buffer into py block",
        )),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. } => Err(pyo3::exceptions::PyTypeError::new_err(
            "cannot pass function into py block",
        )),
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => {
            Err(pyo3::exceptions::PyTypeError::new_err(
                "cannot pass mutable collection into py block; use imut",
            ))
        }
        Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_) => {
            Err(pyo3::exceptions::PyTypeError::new_err(
                "cannot pass transient collection into py block; use persistent!",
            ))
        }
        Value::Atom(_)
        | Value::Chan(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_)
        | Value::Agent(_)
        | Value::Delay(_) => Err(pyo3::exceptions::PyTypeError::new_err(
            "cannot pass concurrency handle into py block",
        )),
    }
}

fn key_to_python(py: Python<'_>, k: &clove_core::ast::Key) -> PyResult<PyObject> {
    match k {
        clove_core::ast::Key::Keyword(s) | clove_core::ast::Key::Symbol(s) => {
            Ok(PyString::new(py, s).into())
        }
        clove_core::ast::Key::String(s) => Ok(PyString::new(py, s).into()),
        clove_core::ast::Key::Number(n) => Ok((*n).into_py(py)),
        clove_core::ast::Key::Bool(b) => Ok((*b).into_py(py)),
    }
}

fn from_python(py: Python<'_>, val: &PyAny) -> Result<Value, String> {
    if val.is_none() {
        return Ok(Value::Nil);
    }
    if let Ok(b) = val.extract::<bool>() {
        return Ok(Value::Bool(b));
    }
    if let Ok(i) = val.extract::<i64>() {
        return Ok(Value::Int(i));
    }
    if let Ok(f) = val.downcast::<PyFloat>() {
        return Ok(Value::Float(f.value()));
    }
    if let Ok(s) = val.downcast::<PyString>() {
        return Ok(Value::String(s.to_string_lossy().into_owned()));
    }
    if let Ok(list) = val.downcast::<PyList>() {
        let mut out = Vec::with_capacity(list.len());
        for item in list {
            out.push(from_python(py, item)?);
        }
        return Ok(Value::Vector(Vector::from(out)));
    }
    if let Ok(set) = val.downcast::<PySet>() {
        let mut out: HashSet<Value> = HashSet::new();
        for item in set {
            out.insert(from_python(py, item)?);
        }
        return Ok(Value::Set(out));
    }
    if let Ok(dict) = val.downcast::<PyDict>() {
        let mut out = HashMap::new();
        for (k, v) in dict.iter() {
            let key_val = from_python(py, k)?;
            let map_key = to_key_value(key_val);
            out.insert(map_key, from_python(py, v)?);
        }
        return Ok(Value::Map(out));
    }
    if let Ok(re_mod) = PyModule::import(py, "re") {
        if let Ok(pattern_type) = re_mod.getattr("Pattern") {
            if val.is_instance(pattern_type).unwrap_or(false) {
                if let Ok(pat) = val.getattr("pattern").and_then(|p| p.extract::<String>()) {
                    return RegexValue::new(pat)
                        .map(Value::Regex)
                        .map_err(|e| e.to_string());
                }
            }
        }
    }
    Ok(Value::Foreign(ForeignValue {
        tag: "py".into(),
        data: Arc::new(PythonForeignValue {
            value: val.to_object(py),
        }),
    }))
}

fn format_python_err(_py: Python<'_>, err: pyo3::PyErr, span: Option<Span>) -> String {
    let base = err.to_string();
    if let Some(span) = span {
        format!("unknown:{}:{}: {}", span.line, span.col, base)
    } else {
        base
    }
}

fn format_with_span(span: Option<Span>, msg: String) -> String {
    if let Some(s) = span {
        format!("unknown:{}:{}: {}", s.line, s.col, msg)
    } else {
        msg
    }
}

fn normalize_python_block(code: &str) -> Cow<'_, str> {
    let mut min_indent = usize::MAX;
    let mut has_content = false;
    for line in code.lines() {
        if line.trim().is_empty() {
            continue;
        }
        has_content = true;
        let indent = line.chars().take_while(|c| matches!(c, ' ' | '\t')).count();
        if indent < min_indent {
            min_indent = indent;
        }
        if min_indent == 0 {
            break;
        }
    }
    if !has_content || min_indent == 0 || min_indent == usize::MAX {
        return Cow::Borrowed(code);
    }

    let mut out = String::with_capacity(code.len());
    for line in code.split_inclusive('\n') {
        let (content, newline) = if line.ends_with('\n') {
            (&line[..line.len() - 1], "\n")
        } else {
            (line, "")
        };
        if content.trim().is_empty() {
            out.push_str(content);
            out.push_str(newline);
            continue;
        }
        let mut removed = 0;
        let mut cut = content.len();
        for (idx, ch) in content.char_indices() {
            if removed >= min_indent {
                cut = idx;
                break;
            }
            if matches!(ch, ' ' | '\t') {
                removed += 1;
                continue;
            }
            cut = idx;
            break;
        }
        if removed < min_indent {
            out.push_str(content);
        } else {
            out.push_str(&content[cut..]);
        }
        out.push_str(newline);
    }
    Cow::Owned(out)
}
