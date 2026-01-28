use crate::ast::{FnArity, Key, SortedMap, SortedSet, Value};
use crate::ast::{HashMap, Vector};
use crate::builtins::{def_builtin, err, sorted_map_insert_mut, sorted_set_insert_mut};
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::{call_callable, key_to_value, to_key_value_checked};
use im::HashSet;

pub(crate) fn install(env: &mut Env) {
    // --- walk helpers ---
    def_builtin!(env, "walk::walk", FnArity::exact(3), |args| {
        match args {
            [inner, outer, form] => walk_apply(inner.clone(), outer.clone(), form.clone()),
            _ => err("walk expects inner, outer, form"),
        }
    });
    define_alias(env, "walk", "walk::walk");
    def_builtin!(env, "walk::postwalk", FnArity::exact(2), |args| {
        match args {
            [f, form] => postwalk(f.clone(), form.clone()),
            _ => err("postwalk expects f and form"),
        }
    });
    define_alias(env, "postwalk", "walk::postwalk");
    def_builtin!(env, "walk::prewalk", FnArity::exact(2), |args| {
        match args {
            [f, form] => prewalk(f.clone(), form.clone()),
            _ => err("prewalk expects f and form"),
        }
    });
    define_alias(env, "prewalk", "walk::prewalk");
    def_builtin!(env, "walk::keywordize-keys", FnArity::exact(1), |args| {
        match args {
            [form] => keywordize_keys(form.clone()),
            _ => err("keywordize-keys expects map"),
        }
    });
    define_alias(env, "keywordize-keys", "walk::keywordize-keys");
    def_builtin!(env, "walk::stringify-keys", FnArity::exact(1), |args| {
        match args {
            [form] => stringify_keys(form.clone()),
            _ => err("stringify-keys expects map"),
        }
    });
    define_alias(env, "stringify-keys", "walk::stringify-keys");
}

fn define_alias(env: &mut Env, alias: &str, target: &str) {
    if let Some(value) = env.get(target) {
        env.define_builtin(alias, value);
    }
}

fn walk_apply(inner: Value, outer: Value, form: Value) -> Result<Value, CloveError> {
    let mapped = match form {
        Value::List(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(walk_apply(inner.clone(), outer.clone(), item)?);
            }
            Value::List(out)
        }
        Value::Vector(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(walk_apply(inner.clone(), outer.clone(), item)?);
            }
            Value::Vector(out)
        }
        Value::Set(v) => Value::Set(
            v.iter()
                .map(|item| walk_apply(inner.clone(), outer.clone(), item.clone()))
                .collect::<Result<HashSet<_>, _>>()?,
        ),
        Value::SortedSet(v) => {
            let mut out = SortedSet {
                comparator: v.comparator.clone(),
                entries: Vec::new(),
            };
            for item in &v.entries {
                let mapped = walk_apply(inner.clone(), outer.clone(), item.clone())?;
                sorted_set_insert_mut(&mut out, mapped)?;
            }
            Value::SortedSet(out)
        }
        Value::Map(m) => {
            let mut out = HashMap::new();
            for (k, v) in m {
                let new_k = walk_apply(inner.clone(), outer.clone(), key_to_value(&k))?;
                let new_v = walk_apply(inner.clone(), outer.clone(), v)?;
                out.insert(to_key_value_checked(&new_k)?, new_v);
            }
            Value::Map(out)
        }
        Value::SortedMap(m) => {
            let mut out = SortedMap {
                comparator: m.comparator.clone(),
                entries: Vec::new(),
            };
            for (k, v) in &m.entries {
                let new_k = walk_apply(inner.clone(), outer.clone(), key_to_value(k))?;
                let new_v = walk_apply(inner.clone(), outer.clone(), v.clone())?;
                sorted_map_insert_mut(&mut out, to_key_value_checked(&new_k)?, new_v)?;
            }
            Value::SortedMap(out)
        }
        other => call_callable(inner.clone(), vec![other])?,
    };
    call_callable(outer, vec![mapped])
}

fn postwalk(f: Value, form: Value) -> Result<Value, CloveError> {
    let mapped = match form.clone() {
        Value::List(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(postwalk(f.clone(), item)?);
            }
            Value::List(out)
        }
        Value::Vector(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(postwalk(f.clone(), item)?);
            }
            Value::Vector(out)
        }
        Value::Set(v) => Value::Set(
            v.iter()
                .map(|item| postwalk(f.clone(), item.clone()))
                .collect::<Result<HashSet<_>, _>>()?,
        ),
        Value::SortedSet(v) => {
            let mut out = SortedSet {
                comparator: v.comparator.clone(),
                entries: Vec::new(),
            };
            for item in &v.entries {
                let mapped = postwalk(f.clone(), item.clone())?;
                sorted_set_insert_mut(&mut out, mapped)?;
            }
            Value::SortedSet(out)
        }
        Value::Map(m) => {
            let mut out = HashMap::new();
            for (k, v) in m {
                let new_k = postwalk(f.clone(), key_to_value(&k))?;
                let new_v = postwalk(f.clone(), v)?;
                out.insert(to_key_value_checked(&new_k)?, new_v);
            }
            Value::Map(out)
        }
        Value::SortedMap(m) => {
            let mut out = SortedMap {
                comparator: m.comparator.clone(),
                entries: Vec::new(),
            };
            for (k, v) in &m.entries {
                let new_k = postwalk(f.clone(), key_to_value(k))?;
                let new_v = postwalk(f.clone(), v.clone())?;
                sorted_map_insert_mut(&mut out, to_key_value_checked(&new_k)?, new_v)?;
            }
            Value::SortedMap(out)
        }
        other => other,
    };
    call_callable(f, vec![mapped])
}

fn prewalk(f: Value, form: Value) -> Result<Value, CloveError> {
    let applied = call_callable(f.clone(), vec![form])?;
    let mapped = match applied {
        Value::List(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(prewalk(f.clone(), item)?);
            }
            Value::List(out)
        }
        Value::Vector(v) => {
            let mut out = Vector::new();
            for item in v {
                out.push_back(prewalk(f.clone(), item)?);
            }
            Value::Vector(out)
        }
        Value::Set(v) => Value::Set(
            v.iter()
                .map(|item| prewalk(f.clone(), item.clone()))
                .collect::<Result<HashSet<_>, _>>()?,
        ),
        Value::SortedSet(v) => {
            let mut out = SortedSet {
                comparator: v.comparator.clone(),
                entries: Vec::new(),
            };
            for item in &v.entries {
                let mapped = prewalk(f.clone(), item.clone())?;
                sorted_set_insert_mut(&mut out, mapped)?;
            }
            Value::SortedSet(out)
        }
        Value::Map(m) => {
            let mut out = HashMap::new();
            for (k, v) in m {
                let new_k = prewalk(f.clone(), key_to_value(&k))?;
                let new_v = prewalk(f.clone(), v)?;
                out.insert(to_key_value_checked(&new_k)?, new_v);
            }
            Value::Map(out)
        }
        Value::SortedMap(m) => {
            let mut out = SortedMap {
                comparator: m.comparator.clone(),
                entries: Vec::new(),
            };
            for (k, v) in &m.entries {
                let new_k = prewalk(f.clone(), key_to_value(k))?;
                let new_v = prewalk(f.clone(), v.clone())?;
                sorted_map_insert_mut(&mut out, to_key_value_checked(&new_k)?, new_v)?;
            }
            Value::SortedMap(out)
        }
        other => other,
    };
    Ok(mapped)
}

fn keywordize_keys(form: Value) -> Result<Value, CloveError> {
    let func = Value::native_fn(FnArity::exact(1), |args| match args {
        [Value::Map(map)] => {
            let mut out = HashMap::new();
            for (k, v) in map {
                let new_k = match k {
                    Key::String(s) => Key::Keyword(s.to_string()),
                    other => other.clone(),
                };
                out.insert(new_k, v.clone());
            }
            Ok(Value::Map(out))
        }
        [Value::SortedMap(map)] => {
            let mut out = SortedMap {
                comparator: map.comparator.clone(),
                entries: Vec::new(),
            };
            for (k, v) in &map.entries {
                let new_k = match k {
                    Key::String(s) => Key::Keyword(s.to_string()),
                    other => other.clone(),
                };
                sorted_map_insert_mut(&mut out, new_k, v.clone())?;
            }
            Ok(Value::SortedMap(out))
        }
        [v] => Ok(v.clone()),
        _ => err("keywordize-keys helper expects one arg"),
    });
    postwalk(func, form)
}

fn stringify_keys(form: Value) -> Result<Value, CloveError> {
    let func = Value::native_fn(FnArity::exact(1), |args| match args {
        [Value::Map(map)] => {
            let mut out = HashMap::new();
            for (k, v) in map {
                let new_k = match k {
                    Key::Keyword(s) | Key::Symbol(s) => Key::String(s.to_string()),
                    other => other.clone(),
                };
                out.insert(new_k, v.clone());
            }
            Ok(Value::Map(out))
        }
        [Value::SortedMap(map)] => {
            let mut out = SortedMap {
                comparator: map.comparator.clone(),
                entries: Vec::new(),
            };
            for (k, v) in &map.entries {
                let new_k = match k {
                    Key::Keyword(s) | Key::Symbol(s) => Key::String(s.to_string()),
                    other => other.clone(),
                };
                sorted_map_insert_mut(&mut out, new_k, v.clone())?;
            }
            Ok(Value::SortedMap(out))
        }
        [v] => Ok(v.clone()),
        _ => err("stringify-keys helper expects one arg"),
    });
    postwalk(func, form)
}
