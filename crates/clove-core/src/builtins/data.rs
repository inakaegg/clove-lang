use crate::ast::{FnArity, Key, SortedMap, SortedSet, Value};
use crate::ast::{HashMap, Vector};
use crate::builtins::{def_builtin, err};
use crate::env::Env;
use crate::error::CloveError;
use im::HashSet;

pub(crate) fn install(env: &mut Env) {
    def_builtin!(env, "data::diff", FnArity::exact(2), |args| match args {
        [a, b] => {
            let (only_a, only_b, both) = diff_value(a, b)?;
            Ok(Value::Vector(Vector::from(vec![only_a, only_b, both])))
        }
        _ => err("data::diff expects two arguments"),
    });
}

fn diff_value(a: &Value, b: &Value) -> Result<(Value, Value, Value), CloveError> {
    if a == b {
        return Ok((Value::Nil, Value::Nil, a.clone()));
    }
    match (a, b) {
        (Value::Map(ma), Value::Map(mb)) => Ok(diff_map(ma, mb)?),
        (Value::Map(ma), Value::SortedMap(mb)) => Ok(diff_map(ma, &sorted_map_to_hashmap(mb))?),
        (Value::SortedMap(ma), Value::Map(mb)) => Ok(diff_map(&sorted_map_to_hashmap(ma), mb)?),
        (Value::SortedMap(ma), Value::SortedMap(mb)) => Ok(diff_map(
            &sorted_map_to_hashmap(ma),
            &sorted_map_to_hashmap(mb),
        )?),
        (Value::Set(sa), Value::Set(sb)) => Ok(diff_set(sa, sb)),
        (Value::Set(sa), Value::SortedSet(sb)) => Ok(diff_set(sa, &sorted_set_to_hashset(sb))),
        (Value::SortedSet(sa), Value::Set(sb)) => Ok(diff_set(&sorted_set_to_hashset(sa), sb)),
        (Value::SortedSet(sa), Value::SortedSet(sb)) => Ok(diff_set(
            &sorted_set_to_hashset(sa),
            &sorted_set_to_hashset(sb),
        )),
        _ => Ok((a.clone(), b.clone(), Value::Nil)),
    }
}

fn diff_set(a: &HashSet<Value>, b: &HashSet<Value>) -> (Value, Value, Value) {
    let only_a: HashSet<Value> = a.iter().filter(|v| !b.contains(*v)).cloned().collect();
    let only_b: HashSet<Value> = b.iter().filter(|v| !a.contains(*v)).cloned().collect();
    let both: HashSet<Value> = a.iter().filter(|v| b.contains(*v)).cloned().collect();
    (Value::Set(only_a), Value::Set(only_b), Value::Set(both))
}

fn diff_map(
    a: &HashMap<Key, Value>,
    b: &HashMap<Key, Value>,
) -> Result<(Value, Value, Value), CloveError> {
    let mut only_a = HashMap::new();
    let mut only_b = HashMap::new();
    let mut both = HashMap::new();
    let mut keys: HashSet<Key> = a.keys().cloned().collect();
    keys.extend(b.keys().cloned());
    for k in keys {
        let av = a.get(&k);
        let bv = b.get(&k);
        match (av, bv) {
            (Some(va), Some(vb)) => {
                let (da, db, common) = diff_value(va, vb)?;
                if !matches!(da, Value::Nil) {
                    only_a.insert(k.clone(), da);
                }
                if !matches!(db, Value::Nil) {
                    only_b.insert(k.clone(), db);
                }
                if !matches!(common, Value::Nil) {
                    both.insert(k.clone(), common);
                }
            }
            (Some(va), None) => {
                only_a.insert(k.clone(), va.clone());
            }
            (None, Some(vb)) => {
                only_b.insert(k.clone(), vb.clone());
            }
            (None, None) => {}
        }
    }
    Ok((map_or_nil(only_a), map_or_nil(only_b), map_or_nil(both)))
}

fn map_or_nil(map: HashMap<Key, Value>) -> Value {
    if map.is_empty() {
        Value::Nil
    } else {
        Value::Map(map)
    }
}

fn sorted_map_to_hashmap(map: &SortedMap) -> HashMap<Key, Value> {
    map.entries.iter().cloned().collect()
}

fn sorted_set_to_hashset(set: &SortedSet) -> HashSet<Value> {
    set.entries.iter().cloned().collect()
}
