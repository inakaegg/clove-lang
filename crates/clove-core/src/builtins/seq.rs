use crate::ast::{
    contains_mut_collection, is_mut_collection, to_imut_deep, to_mut_shallow, FnArity, HashMap,
    Key, SortedMap, SortedSet, TransientHandle, TransientKind, Value, Vector,
};
use crate::builtins::{
    actual_type_with_preview, append_into, as_number, assoc_map, conj_map, conj_sorted_map,
    def_builtin, err, expect_string, make_number, num_to_isize, seq_from_value,
    seq_handle_from_value, seq_is_empty, seq_items, sorted_map_get, sorted_map_insert_mut,
    sorted_map_remove, sorted_set_contains, sorted_set_insert_mut, sorted_set_remove,
    type_mismatch_arg, value_eq,
};
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::{call_callable, key_to_value, to_key_value_checked};
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::guard;
use crate::runtime::{
    gather_sequence, gather_string, normalize_index, parse_lookup_request, slice_sequence,
    slice_string, LookupRequest, SequenceKind,
};
use crate::seq::{SeqEngine, SeqHandle};
use crate::types::TypeKind;
use im::HashSet;
use std::collections::HashMap as StdHashMap;
use std::sync::{Arc, Mutex};

pub(crate) fn install_primitives(env: &mut Env) {
    register_primitive_metas();
    // --- Sequence Ops (Primitives) ---
    def_builtin!(env, "first", FnArity::exact(1), |args| {
        match args {
            [Value::List(vs)] => Ok(vs.get(0).cloned().unwrap_or(Value::Nil)),
            [Value::Vector(vs)] => Ok(vs.get(0).cloned().unwrap_or(Value::Nil)),
            [Value::MutVector(handle)] => Ok(handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?
                .get(0)
                .cloned()
                .unwrap_or(Value::Nil)),
            [Value::Seq(handle)] => Ok(handle.peek()?.unwrap_or(Value::Nil)),
            [Value::Map(_)]
            | [Value::SortedMap(_)]
            | [Value::Set(_)]
            | [Value::SortedSet(_)]
            | [Value::MutMap(_)]
            | [Value::MutSet(_)] => {
                if let Some(handle) = seq_handle_from_value(args[0].clone())? {
                    Ok(handle.next()?.unwrap_or(Value::Nil))
                } else {
                    Ok(Value::Nil)
                }
            }
            [Value::Nil] => Ok(Value::Nil),
            [other] => Err(crate::builtins::type_mismatch_arg(
                "collection",
                "first",
                1,
                other,
            )),
            _ => err("first expects list, vector, seq, or nil"),
        }
    });
    def_builtin!(env, "peek", FnArity::exact(1), |args| {
        match args {
            [Value::List(vs)] => Ok(vs.front().cloned().unwrap_or(Value::Nil)),
            [Value::Vector(vs)] => Ok(vs.back().cloned().unwrap_or(Value::Nil)),
            [Value::MutVector(handle)] => Ok(handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?
                .back()
                .cloned()
                .unwrap_or(Value::Nil)),
            [Value::Seq(handle)] => Ok(handle.peek()?.unwrap_or(Value::Nil)),
            [Value::Nil] => Ok(Value::Nil),
            [other] => Err(crate::builtins::type_mismatch_arg(
                "collection",
                "peek",
                1,
                other,
            )),
            _ => err("peek expects list, vector, seq, or nil"),
        }
    });
    def_builtin!(env, "rest", FnArity::exact(1), |args| {
        match args {
            [Value::List(vs)] => Ok(Value::List(vs.iter().skip(1).cloned().collect())),
            [Value::Vector(vs)] => Ok(Value::Vector(vs.iter().skip(1).cloned().collect())),
            [Value::MutVector(handle)] => {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                Ok(Value::Vector(vec.iter().skip(1).cloned().collect()))
            }
            [Value::Seq(handle)] => {
                let _ = handle.next()?;
                if handle.peek()?.is_none() {
                    Ok(Value::List(Vector::new()))
                } else {
                    Ok(Value::Seq(handle.clone()))
                }
            }
            [Value::Nil] => Ok(Value::List(Vector::new())),
            [other] => Err(crate::builtins::type_mismatch_arg(
                "collection",
                "rest",
                1,
                other,
            )),
            _ => err("rest expects list, vector, seq, or nil"),
        }
    });
    def_builtin!(env, "cons", FnArity::exact(2), |args| {
        match args {
            [val, Value::List(list)] => {
                let mut new_list = list.clone();
                new_list.push_front(val.clone());
                Ok(Value::List(new_list))
            }
            [val, Value::Vector(list)] => {
                let mut new_list = list.clone();
                new_list.push_front(val.clone());
                Ok(Value::Vector(new_list))
            }
            [val, Value::MutVector(handle)] => {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                let mut new_list: Vector<Value> = vec.iter().cloned().collect();
                new_list.push_front(val.clone());
                Ok(Value::Vector(new_list))
            }
            [val, Value::Seq(seq)] => {
                let mut collected = seq.collect_all()?;
                collected.push_front(val.clone());
                Ok(Value::List(collected))
            }
            [val, Value::Nil] => Ok(Value::List(Vector::from(vec![val.clone()]))),
            [_, other] => Err(crate::builtins::type_mismatch_arg(
                "list/vector/seq",
                "cons",
                2,
                other,
            )),
            _ => err("cons expects value and list/vector/seq"),
        }
    });
    def_builtin!(env, "conj", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((
                Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_),
                _,
            )) => err("conj does not accept transient collections; use conj!"),
            Some((Value::List(list), rest)) => {
                let mut new_list = deep_imut_vector(list)?;
                for v in rest.iter().rev() {
                    new_list.push_front(to_imut_deep(v)?);
                }
                Ok(Value::List(new_list))
            }
            Some((Value::Vector(vec), rest)) => {
                let mut new_vec = deep_imut_vector(vec)?;
                for v in rest {
                    new_vec.push_back(to_imut_deep(v)?);
                }
                Ok(Value::Vector(new_vec))
            }
            Some((Value::Set(set), rest)) => {
                let mut new_set = deep_imut_set(set)?;
                for v in rest {
                    if is_mut_collection(v) {
                        return err("cannot put mutable collection into set; use (imut x)");
                    }
                    new_set.insert(to_imut_deep(v)?);
                }
                Ok(Value::Set(new_set))
            }
            Some((Value::SortedSet(set), rest)) => {
                let mut out = deep_imut_sorted_set(set)?;
                for v in rest {
                    if is_mut_collection(v) {
                        return err("cannot put mutable collection into set; use (imut x)");
                    }
                    sorted_set_insert_mut(&mut out, to_imut_deep(v)?)?;
                }
                Ok(Value::SortedSet(out))
            }
            Some((Value::Map(map), rest)) => {
                let base = to_imut_deep(&Value::Map(map.clone()))?;
                match base {
                    Value::Map(base_map) => conj_map(&base_map, rest),
                    _ => err("conj expects map"),
                }
            }
            Some((Value::SortedMap(map), rest)) => {
                let base = deep_imut_sorted_map(map)?;
                Ok(Value::SortedMap(conj_sorted_map(&base, rest)?))
            }
            Some((Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_), rest)) => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Vector(vec) => {
                        let mut new_vec = vec;
                        for v in rest {
                            new_vec.push_back(to_imut_deep(v)?);
                        }
                        Ok(Value::Vector(new_vec))
                    }
                    Value::Map(map) => conj_map(&map, rest),
                    Value::Set(set) => {
                        let mut new_set = set;
                        for v in rest {
                            if is_mut_collection(v) {
                                return err("cannot put mutable collection into set; use (imut x)");
                            }
                            new_set.insert(to_imut_deep(v)?);
                        }
                        Ok(Value::Set(new_set))
                    }
                    other => Err(type_mismatch_arg("collection", "conj", 1, &other)),
                }
            }
            Some((Value::Nil, rest)) => conj_map(&HashMap::new(), rest),
            Some((other, _)) => Err(crate::builtins::type_mismatch_arg(
                "collection",
                "conj",
                1,
                other,
            )),
            None => err("conj expects collection as first argument"),
        }
    });
    def_builtin!(env, "mut", FnArity::exact(1), |args| {
        match args {
            [value] => to_mut_shallow(value),
            _ => err("mut expects one argument"),
        }
    });
    def_builtin!(env, "imut", FnArity::exact(1), |args| {
        match args {
            [value] => to_imut_deep(value),
            _ => err("imut expects one argument"),
        }
    });
    def_builtin!(env, "transient", FnArity::exact(1), |args| {
        match args {
            [Value::Vector(vec)] => Ok(Value::TransientVector(TransientHandle::new(
                TransientKind::Vector(vec.clone()),
            ))),
            [Value::Map(map)] => Ok(Value::TransientMap(TransientHandle::new(
                TransientKind::Map(crate::ast::TransientMap::from_persistent(map.clone())),
            ))),
            [Value::Set(set)] => Ok(Value::TransientSet(TransientHandle::new(
                TransientKind::Set(set.clone()),
            ))),
            [other] => Err(type_mismatch_arg("vector/map/set", "transient", 1, other)),
            _ => err("transient expects one argument"),
        }
    });
    def_builtin!(env, "persistent!", FnArity::exact(1), |args| {
        match args {
            [Value::TransientVector(handle)] => match handle.persist("persistent!")? {
                TransientKind::Vector(vec) => Ok(Value::Vector(vec)),
                _ => err("persistent! expected transient vector"),
            },
            [Value::TransientMap(handle)] => match handle.persist("persistent!")? {
                TransientKind::Map(map) => Ok(Value::Map(map.into_persistent())),
                _ => err("persistent! expected transient map"),
            },
            [Value::TransientSet(handle)] => match handle.persist("persistent!")? {
                TransientKind::Set(set) => Ok(Value::Set(set)),
                _ => err("persistent! expected transient set"),
            },
            [other] => Err(type_mismatch_arg(
                "transient collection",
                "persistent!",
                1,
                other,
            )),
            _ => err("persistent! expects one argument"),
        }
    });
    def_builtin!(env, "conj!", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((Value::MutVector(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutVector(handle.clone()));
                }
                let mut vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                for v in rest {
                    vec.push_back(to_mut_shallow(v)?);
                }
                Ok(Value::MutVector(handle.clone()))
            }
            Some((Value::MutMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutMap(handle.clone()));
                }
                let mut map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                for item in rest {
                    match item {
                        Value::Vector(v) | Value::List(v) if v.len() == 2 => {
                            let key = to_key_value_checked(&v[0])?;
                            let val = to_mut_shallow(&v[1])?;
                            map.insert(key, val);
                        }
                        Value::Map(m) => {
                            for (k, v) in m.iter() {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        Value::SortedMap(m) => {
                            for (k, v) in &m.entries {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        Value::MutMap(m) => {
                            let locked = m
                                .lock()
                                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                            for (k, v) in locked.iter() {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        _ => return err("conj! on map expects map or [k v]"),
                    }
                }
                Ok(Value::MutMap(handle.clone()))
            }
            Some((Value::MutSet(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutSet(handle.clone()));
                }
                let mut set = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
                for v in rest {
                    if is_mut_collection(v) {
                        return err("cannot put mutable collection into set; use (imut x)");
                    }
                    let converted = to_imut_deep(v)?;
                    set.insert(converted);
                }
                Ok(Value::MutSet(handle.clone()))
            }
            Some((Value::TransientVector(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientVector(handle.clone()));
                }
                let (_, next) = handle.write("conj!", |kind| match kind {
                    TransientKind::Vector(vec) => {
                        for v in rest {
                            vec.push_back(v.clone());
                        }
                        Ok(())
                    }
                    _ => err("conj! expected transient vector"),
                })?;
                Ok(Value::TransientVector(next))
            }
            Some((Value::TransientMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientMap(handle.clone()));
                }
                let (_, next) = handle.write("conj!", |kind| match kind {
                    TransientKind::Map(map) => {
                        for item in rest {
                            match item {
                                Value::Vector(v) | Value::List(v) if v.len() == 2 => {
                                    map.insert(to_key_value_checked(&v[0])?, v[1].clone());
                                }
                                Value::Map(m) => {
                                    map.extend_map(m);
                                }
                                Value::SortedMap(m) => {
                                    map.extend_sorted_map(m);
                                }
                                _ => return err("conj! on map expects map or [k v]"),
                            }
                        }
                        Ok(())
                    }
                    _ => err("conj! expected transient map"),
                })?;
                Ok(Value::TransientMap(next))
            }
            Some((Value::TransientSet(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientSet(handle.clone()));
                }
                let (_, next) = handle.write("conj!", |kind| match kind {
                    TransientKind::Set(set) => {
                        for v in rest {
                            if is_mut_collection(v) {
                                return err("cannot put mutable collection into set; use (imut x)");
                            }
                            set.insert(to_imut_deep(v)?);
                        }
                        Ok(())
                    }
                    _ => err("conj! expected transient set"),
                })?;
                Ok(Value::TransientSet(next))
            }
            Some((other, _)) => Err(type_mismatch_arg(
                "mutable or transient vector/map/set",
                "conj!",
                1,
                other,
            )),
            None => err("conj! expects collection as first argument"),
        }
    });
    def_builtin!(env, "assoc!", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((Value::MutMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutMap(handle.clone()));
                }
                if rest.len() % 2 != 0 {
                    return err("assoc! expects even number of key/value arguments");
                }
                let mut map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                let mut iter = rest.iter();
                while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                    map.insert(to_key_value_checked(k)?, to_mut_shallow(v)?);
                }
                Ok(Value::MutMap(handle.clone()))
            }
            Some((Value::MutVector(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutVector(handle.clone()));
                }
                let mut vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                assoc_indexed_mut_shallow(&mut vec, rest)?;
                Ok(Value::MutVector(handle.clone()))
            }
            Some((Value::TransientMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientMap(handle.clone()));
                }
                if rest.len() % 2 != 0 {
                    return err("assoc! expects even number of key/value arguments");
                }
                let (_, next) = handle.write("assoc!", |kind| match kind {
                    TransientKind::Map(map) => {
                        let mut iter = rest.iter();
                        while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                            map.insert(to_key_value_checked(k)?, v.clone());
                        }
                        Ok(())
                    }
                    _ => err("assoc! expected transient map"),
                })?;
                Ok(Value::TransientMap(next))
            }
            Some((Value::TransientVector(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientVector(handle.clone()));
                }
                let (_, next) = handle.write("assoc!", |kind| match kind {
                    TransientKind::Vector(vec) => {
                        assoc_indexed_mut(vec, rest)?;
                        Ok(())
                    }
                    _ => err("assoc! expected transient vector"),
                })?;
                Ok(Value::TransientVector(next))
            }
            Some((other, _)) => Err(type_mismatch_arg(
                "mutable or transient map/vector",
                "assoc!",
                1,
                other,
            )),
            None => err("assoc! expects collection as first argument"),
        }
    });
    def_builtin!(env, "update!", FnArity::at_least(3), |args| {
        match args {
            [target, key, func, extra @ ..] => {
                update_value_mut("update!", target, key, func, extra, true)
            }
            _ => err("update! expects map/vector, key, function, and optional args"),
        }
    });
    def_builtin!(env, "update-in!", FnArity::at_least(3), |args| {
        match args {
            [target, path, func, extra @ ..] => {
                update_in_mut_from_value(target.clone(), path, func.clone(), extra)
            }
            _ => {
                err("update-in! expects map/vector, path vector/list, function, and optional args")
            }
        }
    });
    def_builtin!(env, "dissoc!", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((Value::MutMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutMap(handle.clone()));
                }
                let mut map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                for key in rest {
                    map.remove(&to_key_value_checked(key)?);
                }
                Ok(Value::MutMap(handle.clone()))
            }
            Some((Value::TransientMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientMap(handle.clone()));
                }
                let (_, next) = handle.write("dissoc!", |kind| match kind {
                    TransientKind::Map(map) => {
                        for key in rest {
                            map.remove(&to_key_value_checked(key)?);
                        }
                        Ok(())
                    }
                    _ => err("dissoc! expected transient map"),
                })?;
                Ok(Value::TransientMap(next))
            }
            Some((other, _)) => Err(type_mismatch_arg("transient map", "dissoc!", 1, other)),
            None => err("dissoc! expects map and keys"),
        }
    });
    def_builtin!(env, "disj!", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((Value::MutSet(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutSet(handle.clone()));
                }
                let mut set = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
                for v in rest {
                    set.remove(v);
                }
                Ok(Value::MutSet(handle.clone()))
            }
            Some((Value::TransientSet(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::TransientSet(handle.clone()));
                }
                let (_, next) = handle.write("disj!", |kind| match kind {
                    TransientKind::Set(set) => {
                        for v in rest {
                            set.remove(v);
                        }
                        Ok(())
                    }
                    _ => err("disj! expected transient set"),
                })?;
                Ok(Value::TransientSet(next))
            }
            Some((other, _)) => Err(type_mismatch_arg("transient set", "disj!", 1, other)),
            None => err("disj! expects set and values"),
        }
    });
    def_builtin!(env, "pop!", FnArity::exact(1), |args| {
        match args {
            [Value::MutVector(handle)] => {
                let mut vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                if vec.is_empty() {
                    return err("pop! on empty vector");
                }
                let _ = vec.pop_back();
                Ok(Value::MutVector(handle.clone()))
            }
            [Value::TransientVector(handle)] => {
                let (_, next) = handle.write("pop!", |kind| match kind {
                    TransientKind::Vector(vec) => {
                        if vec.is_empty() {
                            return err("pop! on empty vector");
                        }
                        let _ = vec.pop_back();
                        Ok(())
                    }
                    _ => err("pop! expected transient vector"),
                })?;
                Ok(Value::TransientVector(next))
            }
            [other] => Err(type_mismatch_arg(
                "mutable or transient vector",
                "pop!",
                1,
                other,
            )),
            _ => err("pop! expects one argument"),
        }
    });
    def_builtin!(env, "into", FnArity::exact(2), |args| {
        match args {
            [target, source] => {
                let frozen = match target {
                    Value::SortedMap(map) => Value::SortedMap(deep_imut_sorted_map(map)?),
                    Value::SortedSet(set) => Value::SortedSet(deep_imut_sorted_set(set)?),
                    Value::Map(_)
                    | Value::Vector(_)
                    | Value::List(_)
                    | Value::Set(_)
                    | Value::MutMap(_)
                    | Value::MutVector(_)
                    | Value::MutSet(_)
                    | Value::Nil => to_imut_deep(target)?,
                    other => other.clone(),
                };
                append_into(frozen, source.clone())
            }
            _ => err("into expects two arguments"),
        }
    });
    def_builtin!(env, "vec", FnArity::exact(1), |args| {
        match args {
            [Value::Vector(v)] => Ok(Value::Vector(v.clone())),
            [Value::List(v)] => Ok(Value::Vector(v.clone())),
            [Value::MutVector(handle)] => {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                Ok(Value::Vector(vec.iter().cloned().collect()))
            }
            [Value::Set(v)] => Ok(Value::Vector(v.iter().cloned().collect())),
            [Value::SortedSet(v)] => Ok(Value::Vector(v.entries.iter().cloned().collect())),
            [Value::MutSet(handle)] => {
                let set = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
                Ok(Value::Vector(set.iter().cloned().collect()))
            }
            [Value::Map(m)] => Ok(Value::Vector(
                m.iter()
                    .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(k), v.clone()])))
                    .collect(),
            )),
            [Value::SortedMap(m)] => Ok(Value::Vector(
                m.entries
                    .iter()
                    .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(k), v.clone()])))
                    .collect(),
            )),
            [Value::MutMap(handle)] => {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                Ok(Value::Vector(
                    map.iter()
                        .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(k), v.clone()])))
                        .collect(),
                ))
            }
            [Value::String(s)] => Ok(Value::Vector(
                s.chars()
                    .map(|c| Value::String(c.to_string()))
                    .collect::<Vector<_>>(),
            )),
            [Value::Seq(handle)] => Ok(Value::Vector(handle.collect_all()?)),
            [Value::Nil] => Ok(Value::Vector(Vector::new())),
            [other] => Err(crate::builtins::type_mismatch_arg(
                "collection",
                "vec",
                1,
                other,
            )),
            _ => err("vec expects one argument"),
        }
    });
    def_builtin!(env, "nth", FnArity::range(2, 3), |args| {
        let [coll, idx, ..] = args else {
            return err("nth expects (coll idx) or (coll idx default)");
        };
        let default = args.get(2).cloned();
        if is_nth_index_collection(idx) {
            let indices = parse_nth_indices(idx)?;
            return nth_many(coll, &indices, default);
        }
        let idx = parse_nth_index(idx)?;
        nth_one(coll, idx, default)
    });
    def_builtin!(env, "subvec", FnArity::range(2, 3), |args| match args {
        [coll, start] => subvec_slice(coll, start, None),
        [coll, start, end] => subvec_slice(coll, start, Some(end)),
        _ => err("subvec expects collection, start, and optional end"),
    });
    def_builtin!(env, "seq", FnArity::exact(1), |args| {
        match args {
            [value] => seq_from_value(value.clone()),
            _ => err("seq expects one argument"),
        }
    });
    def_builtin!(env, "next", FnArity::exact(1), |args| {
        match args {
            [v] => match seq_handle_from_value(v.clone())? {
                Some(handle) => {
                    let _ = handle.next()?;
                    let remaining = handle.collect_all()?;
                    if remaining.is_empty() {
                        Ok(Value::Nil)
                    } else {
                        Ok(Value::List(remaining))
                    }
                }
                None => Ok(Value::Nil),
            },
            _ => err("next expects one argument"),
        }
    });
    def_builtin!(env, "second", FnArity::exact(1), |args| {
        match args {
            [v] => match seq_handle_from_value(v.clone())? {
                Some(handle) => {
                    let _ = handle.next()?;
                    Ok(handle.next()?.unwrap_or(Value::Nil))
                }
                None => Ok(Value::Nil),
            },
            _ => err("second expects one argument"),
        }
    });
    def_builtin!(env, "ffirst", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let first_val = match seq_handle_from_value(v.clone())? {
                    Some(handle) => handle.next()?.unwrap_or(Value::Nil),
                    None => Value::Nil,
                };
                match seq_handle_from_value(first_val)? {
                    Some(inner) => Ok(inner.next()?.unwrap_or(Value::Nil)),
                    None => Ok(Value::Nil),
                }
            }
            _ => err("ffirst expects one argument"),
        }
    });
    def_builtin!(env, "last", FnArity::exact(1), |args| {
        match args {
            [Value::List(items)] => Ok(items.iter().last().cloned().unwrap_or(Value::Nil)),
            [Value::Vector(items)] => Ok(items.iter().last().cloned().unwrap_or(Value::Nil)),
            [Value::Set(items)] => Ok(items.iter().last().cloned().unwrap_or(Value::Nil)),
            [Value::SortedSet(items)] => Ok(items.entries.last().cloned().unwrap_or(Value::Nil)),
            [Value::Seq(handle)] => {
                let mut last_val: Option<Value> = None;
                while let Some(v) = handle.next()? {
                    last_val = Some(v);
                }
                Ok(last_val.unwrap_or(Value::Nil))
            }
            [Value::Nil] => Ok(Value::Nil),
            [other] => Err(type_mismatch_arg("collection", "last", 1, other)),
            _ => err("last expects list, vector, set, seq, or nil"),
        }
    });
    def_builtin!(env, "reverse", FnArity::exact(1), |args| {
        match args {
            [Value::List(items)] => Ok(Value::List(items.iter().cloned().rev().collect())),
            [Value::Vector(items)] => Ok(Value::Vector(items.iter().cloned().rev().collect())),
            [Value::Set(items)] => {
                let mut tmp: Vec<Value> = items.iter().cloned().collect();
                tmp.reverse();
                Ok(Value::Vector(Vector::from(tmp)))
            }
            [Value::SortedSet(items)] => {
                let mut tmp = items.entries.clone();
                tmp.reverse();
                Ok(Value::Vector(Vector::from(tmp)))
            }
            [Value::Seq(handle)] => {
                let mut items: Vec<Value> = handle.collect_all()?.into_iter().collect();
                items.reverse();
                Ok(Value::Vector(Vector::from(items)))
            }
            [Value::Nil] => Ok(Value::List(Vector::new())),
            [other] => Err(type_mismatch_arg("collection", "reverse", 1, other)),
            _ => err("reverse expects list, vector, set, seq, or nil"),
        }
    });
    def_builtin!(env, "rseq", FnArity::exact(1), |args| match args {
        [Value::Vector(items)] => Ok(Value::Seq(SeqHandle::from_iter(
            items.clone().into_iter().rev(),
        ))),
        [Value::List(items)] => Ok(Value::Seq(SeqHandle::from_iter(
            items.clone().into_iter().rev(),
        ))),
        [Value::SortedSet(items)] => Ok(Value::Seq(SeqHandle::from_iter(
            items.entries.clone().into_iter().rev(),
        ))),
        [Value::SortedMap(map)] => {
            let entries: Vec<Value> = map
                .entries
                .iter()
                .rev()
                .map(|(k, v)| Value::Vector(Vector::from(vec![key_to_value(k), v.clone()])))
                .collect();
            Ok(Value::Seq(SeqHandle::from_iter(entries)))
        }
        [Value::String(s)] => Ok(Value::Seq(SeqHandle::from_iter(
            s.chars()
                .rev()
                .map(|c| Value::String(c.to_string()))
                .collect::<Vec<_>>(),
        ))),
        [Value::Seq(_)] => err("rseq expects a reversible collection, not a 1-pass seq"),
        [Value::Nil] => Ok(Value::Seq(SeqHandle::from_iter(Vec::<Value>::new()))),
        [other] => Err(type_mismatch_arg("reversible collection", "rseq", 1, other,)),
        _ => err("rseq expects vector, list, string, sorted collection, or nil"),
    });
    def_builtin!(env, "butlast", FnArity::exact(1), |args| match args {
        [Value::List(items)] => {
            let mut v: Vector<Value> = items.iter().cloned().collect();
            let _ = v.pop_back();
            Ok(Value::List(v))
        }
        [Value::Vector(items)] => {
            let mut v = items.clone();
            let _ = v.pop_back();
            Ok(Value::Vector(v))
        }
        [Value::Seq(handle)] => {
            let mut collected: Vec<Value> = handle.collect_all()?.into_iter().collect();
            if !collected.is_empty() {
                collected.pop();
            }
            Ok(Value::Vector(Vector::from(collected)))
        }
        [Value::Nil] => Ok(Value::List(Vector::new())),
        [other] => Err(type_mismatch_arg("collection", "butlast", 1, other)),
        _ => err("butlast expects list, vector, seq, or nil"),
    });
    def_builtin!(env, "assoc", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((
                Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_),
                _,
            )) => err("assoc does not accept transient collections; use assoc!"),
            Some((Value::Map(map), rest)) => {
                let base = to_imut_deep(&Value::Map(map.clone()))?;
                match base {
                    Value::Map(base_map) => assoc_map(&base_map, rest),
                    _ => err("assoc expects map"),
                }
            }
            Some((Value::SortedMap(map), rest)) => {
                if rest.len() % 2 != 0 {
                    return err("assoc expects even number of key/value arguments");
                }
                let mut out = deep_imut_sorted_map(map)?;
                let mut iter = rest.iter();
                while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                    sorted_map_insert_mut(&mut out, to_key_value_checked(k)?, to_imut_deep(v)?)?;
                }
                Ok(Value::SortedMap(out))
            }
            Some((Value::Nil, rest)) => assoc_map(&HashMap::new(), rest),
            Some((Value::Vector(vec), rest)) => {
                let base = deep_imut_vector(vec)?;
                assoc_indexed(&base, rest, false)
            }
            Some((Value::List(list), rest)) => {
                let base = deep_imut_vector(list)?;
                assoc_indexed(&base, rest, true)
            }
            Some((Value::MutVector(_), rest)) => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Vector(vec) => assoc_indexed(&vec, rest, false),
                    other => Err(type_mismatch_arg("vector", "assoc", 1, &other)),
                }
            }
            Some((Value::MutMap(_), rest)) => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Map(map) => assoc_map(&map, rest),
                    other => Err(type_mismatch_arg("map", "assoc", 1, &other)),
                }
            }
            Some((other, _)) => Err(type_mismatch_arg("map/vector/list", "assoc", 1, other)),
            _ => err("assoc expects a map/vector/list followed by key/value pairs"),
        }
    });
    def_builtin!(env, "get", FnArity::range(2, 3), |args| {
        match args {
            [target, key] => get_value(target, key, None),
            [target, key, default_val] => get_value(target, key, Some(default_val)),
            _ => err("get expects map/set/seq/string, key, and optional default"),
        }
    });
    def_builtin!(env, "get-in", FnArity::range(2, 3), |args| {
        match args {
            [target, path] => get_in_from_value(target.clone(), path, None),
            [target, path, default_val] => {
                get_in_from_value(target.clone(), path, Some(default_val.clone()))
            }
            _ => err("get-in expects collection, path vector/list, and optional default"),
        }
    });
    def_builtin!(env, "get-in-many", FnArity::range(2, 3), |args| {
        match args {
            [target, paths] => get_in_many_from_value(target, paths, None),
            [target, paths, default_val] => {
                get_in_many_from_value(target, paths, Some(default_val.clone()))
            }
            _ => err("get-in-many expects collection, path vector/list, and optional default"),
        }
    });
    def_builtin!(env, "assoc-in", FnArity::exact(3), |args| {
        match args {
            [target, path, value] => assoc_in_from_value(target.clone(), path, value.clone()),
            _ => err("assoc-in expects map, path vector/list, and value"),
        }
    });
    def_builtin!(env, "assoc-in!", FnArity::exact(3), |args| {
        match args {
            [target, path, value] => assoc_in_mut_from_value(target.clone(), path, value.clone()),
            _ => err("assoc-in! expects mutable map/vector, path vector/list, and value"),
        }
    });
    def_builtin!(env, "update-in", FnArity::at_least(3), |args| {
        match args {
            [target, path, f, extra @ ..] => {
                update_in_from_value(target.clone(), path, f.clone(), extra)
            }
            _ => err("update-in expects map, path vector/list, function, and optional args"),
        }
    });
    def_builtin!(env, "keys", FnArity::exact(1), |args| {
        match args {
            [Value::Map(map)] => {
                let mut ks = Vector::new();
                for k in map.keys() {
                    ks.push_back(key_to_value(k));
                }
                Ok(Value::Vector(ks))
            }
            [Value::SortedMap(map)] => Ok(Value::Vector(
                map.entries.iter().map(|(k, _)| key_to_value(k)).collect(),
            )),
            [Value::MutMap(handle)] => {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                let mut ks = Vector::new();
                for k in map.keys() {
                    ks.push_back(key_to_value(k));
                }
                Ok(Value::Vector(ks))
            }
            [other] => Err(type_mismatch_arg("map", "keys", 1, other)),
            _ => err("keys expects a map"),
        }
    });
    def_builtin!(env, "vals", FnArity::exact(1), |args| {
        match args {
            [Value::Map(map)] => Ok(Value::Vector(map.values().cloned().collect())),
            [Value::SortedMap(map)] => Ok(Value::Vector(
                map.entries.iter().map(|(_, v)| v.clone()).collect(),
            )),
            [Value::MutMap(handle)] => {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                Ok(Value::Vector(map.values().cloned().collect()))
            }
            [other] => Err(type_mismatch_arg("map", "vals", 1, other)),
            _ => err("vals expects a map"),
        }
    });
    def_builtin!(env, "merge", FnArity::at_least(0), |args| {
        let mut out = HashMap::new();
        let mut sorted: Option<SortedMap> = None;
        for (idx, arg) in args.iter().enumerate() {
            match arg {
                Value::Map(m) => {
                    if let Some(current) = sorted.take() {
                        let mut next = current;
                        for (k, v) in m.iter() {
                            sorted_map_insert_mut(&mut next, k.clone(), to_imut_deep(v)?)?;
                        }
                        sorted = Some(next);
                    } else {
                        for (k, v) in m.iter() {
                            out.insert(k.clone(), to_imut_deep(v)?);
                        }
                    }
                }
                Value::MutMap(handle) => {
                    let map = handle
                        .lock()
                        .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                    if let Some(current) = sorted.take() {
                        let mut next = current;
                        for (k, v) in map.iter() {
                            sorted_map_insert_mut(&mut next, k.clone(), to_imut_deep(v)?)?;
                        }
                        sorted = Some(next);
                    } else {
                        for (k, v) in map.iter() {
                            out.insert(k.clone(), to_imut_deep(v)?);
                        }
                    }
                }
                Value::SortedMap(m) => {
                    if let Some(current) = sorted.take() {
                        let mut next = current;
                        for (k, v) in &m.entries {
                            sorted_map_insert_mut(&mut next, k.clone(), to_imut_deep(v)?)?;
                        }
                        sorted = Some(next);
                    } else if out.is_empty() {
                        sorted = Some(deep_imut_sorted_map(m)?);
                    } else {
                        let mut next = SortedMap {
                            comparator: m.comparator.clone(),
                            entries: Vec::new(),
                        };
                        for (k, v) in out.iter() {
                            sorted_map_insert_mut(&mut next, k.clone(), v.clone())?;
                        }
                        for (k, v) in &m.entries {
                            sorted_map_insert_mut(&mut next, k.clone(), to_imut_deep(v)?)?;
                        }
                        sorted = Some(next);
                    }
                }
                Value::Nil => {}
                other => return Err(type_mismatch_arg("map", "merge", idx + 1, other)),
            }
        }
        if let Some(map) = sorted {
            Ok(Value::SortedMap(map))
        } else {
            Ok(Value::Map(out))
        }
    });
    def_builtin!(env, "merge!", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((Value::MutMap(handle), rest)) => {
                if rest.is_empty() {
                    return Ok(Value::MutMap(handle.clone()));
                }
                let mut map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                for (idx, arg) in rest.iter().enumerate() {
                    match arg {
                        Value::Map(m) => {
                            for (k, v) in m.iter() {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        Value::MutMap(m) => {
                            let locked = m
                                .lock()
                                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                            for (k, v) in locked.iter() {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        Value::SortedMap(m) => {
                            for (k, v) in &m.entries {
                                map.insert(k.clone(), to_mut_shallow(v)?);
                            }
                        }
                        other => return Err(type_mismatch_arg("map", "merge!", idx + 2, other)),
                    }
                }
                Ok(Value::MutMap(handle.clone()))
            }
            Some((other, _)) => Err(type_mismatch_arg("mutable map", "merge!", 1, other)),
            None => err("merge! expects mutable map and maps"),
        }
    });
    def_builtin!(env, "dissoc", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((
                Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_),
                _,
            )) => err("dissoc does not accept transient collections; use dissoc!"),
            Some((Value::Map(map), rest)) => {
                let base = to_imut_deep(&Value::Map(map.clone()))?;
                let mut new_map = match base {
                    Value::Map(map) => map,
                    _ => return err("dissoc expects map"),
                };
                for k in rest {
                    new_map.remove(&to_key_value_checked(k)?);
                }
                Ok(Value::Map(new_map))
            }
            Some((Value::SortedMap(map), rest)) => {
                let mut out = deep_imut_sorted_map(map)?;
                for k in rest {
                    out = sorted_map_remove(&out, &to_key_value_checked(k)?)?;
                }
                Ok(Value::SortedMap(out))
            }
            Some((Value::Nil, _)) => Ok(Value::Nil),
            Some((Value::MutMap(_), rest)) => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Map(map) => {
                        let mut out = map;
                        for k in rest {
                            out.remove(&to_key_value_checked(k)?);
                        }
                        Ok(Value::Map(out))
                    }
                    other => Err(type_mismatch_arg("map", "dissoc", 1, &other)),
                }
            }
            Some((other, _)) => Err(type_mismatch_arg("map", "dissoc", 1, other)),
            _ => err("dissoc expects map and keys"),
        }
    });
    def_builtin!(env, "disj", FnArity::at_least(1), |args| {
        match args.split_first() {
            Some((
                Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_),
                _,
            )) => err("disj does not accept transient collections; use disj!"),
            Some((Value::Set(set), rest)) => {
                let mut out = deep_imut_set(set)?;
                for v in rest {
                    out.remove(v);
                }
                Ok(Value::Set(out))
            }
            Some((Value::SortedSet(set), rest)) => {
                let mut out = deep_imut_sorted_set(set)?;
                for v in rest {
                    out = sorted_set_remove(&out, v)?;
                }
                Ok(Value::SortedSet(out))
            }
            Some((Value::Nil, _)) => Ok(Value::Nil),
            Some((Value::MutSet(_), rest)) => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Set(set) => {
                        let mut out = set;
                        for v in rest {
                            out.remove(v);
                        }
                        Ok(Value::Set(out))
                    }
                    other => Err(type_mismatch_arg("set", "disj", 1, &other)),
                }
            }
            Some((other, _)) => Err(type_mismatch_arg("set", "disj", 1, other)),
            _ => err("disj expects set and values"),
        }
    });
    def_builtin!(env, "empty?", FnArity::exact(1), |args| {
        match args {
            [coll] => Ok(Value::Bool(seq_is_empty("empty?", 1, coll)?)),
            _ => err("empty? expects collection"),
        }
    });
    def_builtin!(env, "not-empty", FnArity::exact(1), |args| {
        match args {
            [coll] => {
                if seq_is_empty("not-empty", 1, coll)? {
                    Ok(Value::Nil)
                } else {
                    Ok(coll.clone())
                }
            }
            _ => err("not-empty expects collection"),
        }
    });
    def_builtin!(env, "empty", FnArity::exact(1), |args| match args {
        [Value::List(_)] => Ok(Value::List(Vector::new())),
        [Value::Vector(_)] => Ok(Value::Vector(Vector::new())),
        [Value::MutVector(_)] => Ok(Value::Vector(Vector::new())),
        [Value::Map(_)] => Ok(Value::Map(HashMap::new())),
        [Value::MutMap(_)] => Ok(Value::Map(HashMap::new())),
        [Value::SortedMap(map)] => Ok(Value::SortedMap(SortedMap {
            comparator: map.comparator.clone(),
            entries: Vec::new(),
        })),
        [Value::Set(_)] => Ok(Value::Set(HashSet::new())),
        [Value::MutSet(_)] => Ok(Value::Set(HashSet::new())),
        [Value::SortedSet(set)] => Ok(Value::SortedSet(SortedSet {
            comparator: set.comparator.clone(),
            entries: Vec::new(),
        })),
        [Value::String(_)] => Ok(Value::String(String::new())),
        [Value::Seq(_)] => Ok(Value::Seq(SeqHandle::from_iter(Vec::<Value>::new()))),
        [Value::Nil] => Ok(Value::Nil),
        [other] => Err(type_mismatch_arg("collection", "empty", 1, other)),
        _ => err("empty expects one argument"),
    });
    def_builtin!(env, "count", FnArity::exact(1), |args| {
        match args {
            [Value::List(v)] => Ok(make_number(v.len() as f64, false)),
            [Value::Vector(v)] => Ok(make_number(v.len() as f64, false)),
            [Value::MutVector(handle)] => Ok(make_number(
                handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?
                    .len() as f64,
                false,
            )),
            [Value::Map(m)] => Ok(make_number(m.len() as f64, false)),
            [Value::MutMap(handle)] => Ok(make_number(
                handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?
                    .len() as f64,
                false,
            )),
            [Value::SortedMap(m)] => Ok(make_number(m.entries.len() as f64, false)),
            [Value::Set(s)] => Ok(make_number(s.len() as f64, false)),
            [Value::MutSet(handle)] => Ok(make_number(
                handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?
                    .len() as f64,
                false,
            )),
            [Value::SortedSet(s)] => Ok(make_number(s.entries.len() as f64, false)),
            [Value::TransientVector(handle)] => handle.read("count", |kind| match kind {
                TransientKind::Vector(vec) => Ok(make_number(vec.len() as f64, false)),
                _ => err("count expected transient vector"),
            }),
            [Value::TransientMap(handle)] => handle.read("count", |kind| match kind {
                TransientKind::Map(map) => Ok(make_number(map.len() as f64, false)),
                _ => err("count expected transient map"),
            }),
            [Value::TransientSet(handle)] => handle.read("count", |kind| match kind {
                TransientKind::Set(set) => Ok(make_number(set.len() as f64, false)),
                _ => err("count expected transient set"),
            }),
            [Value::String(s)] => Ok(make_number(s.chars().count() as f64, false)),
            [Value::Seq(handle)] => {
                let mut count = 0usize;
                while let Some(_) = handle.next()? {
                    count += 1;
                }
                Ok(make_number(count as f64, false))
            }
            [Value::Nil] => Ok(make_number(0.0, false)),
            [other] => Err(type_mismatch_arg("collection", "count", 1, other)),
            _ => err("count expects collection"),
        }
    });
    def_builtin!(env, "includes?", FnArity::exact(2), |args| {
        match args {
            [target, needle] => Ok(Value::Bool(includes_value(target, needle)?)),
            _ => err("includes? expects collection and value"),
        }
    });
    def_builtin!(env, "contains?", FnArity::exact(2), |args| {
        match args {
            [Value::Map(map), key] => {
                Ok(Value::Bool(map.contains_key(&to_key_value_checked(key)?)))
            }
            [Value::SortedMap(map), key] => Ok(Value::Bool(
                sorted_map_get(map, &to_key_value_checked(key)?)?.is_some(),
            )),
            [Value::MutMap(handle), key] => {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                Ok(Value::Bool(map.contains_key(&to_key_value_checked(key)?)))
            }
            [Value::Set(set), val] => Ok(Value::Bool(set.contains(val))),
            [Value::SortedSet(set), val] => Ok(Value::Bool(sorted_set_contains(set, val)?)),
            [Value::MutSet(handle), val] => {
                let set = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
                Ok(Value::Bool(set.contains(val)))
            }
            [Value::TransientMap(handle), key] => handle.read("contains?", |kind| match kind {
                TransientKind::Map(map) => {
                    Ok(Value::Bool(map.contains_key(&to_key_value_checked(key)?)))
                }
                _ => err("contains? expected transient map"),
            }),
            [Value::TransientSet(handle), val] => handle.read("contains?", |kind| match kind {
                TransientKind::Set(set) => Ok(Value::Bool(set.contains(val))),
                _ => err("contains? expected transient set"),
            }),
            [Value::Vector(vec), n] => match n {
                Value::Int(idx) => Ok(Value::Bool(
                    usize::try_from(*idx)
                        .ok()
                        .map(|idx| idx < vec.len())
                        .unwrap_or(false),
                )),
                other => Err(contains_index_type_error(other)),
            },
            [Value::MutVector(handle), n] => {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                match n {
                    Value::Int(idx) => Ok(Value::Bool(
                        usize::try_from(*idx)
                            .ok()
                            .map(|idx| idx < vec.len())
                            .unwrap_or(false),
                    )),
                    other => Err(contains_index_type_error(other)),
                }
            }
            [Value::List(list), n] => match n {
                Value::Int(idx) => Ok(Value::Bool(
                    usize::try_from(*idx)
                        .ok()
                        .map(|idx| idx < list.len())
                        .unwrap_or(false),
                )),
                other => Err(contains_index_type_error(other)),
            },
            [Value::TransientVector(handle), n] => handle.read("contains?", |kind| match kind {
                TransientKind::Vector(vec) => match n {
                    Value::Int(idx) => Ok(Value::Bool(
                        usize::try_from(*idx)
                            .ok()
                            .map(|idx| idx < vec.len())
                            .unwrap_or(false),
                    )),
                    other => Err(contains_index_type_error(other)),
                },
                _ => err("contains? expected transient vector"),
            }),
            [other, _] => Err(type_mismatch_arg(
                "map/set/vector/list",
                "contains?",
                1,
                other,
            )),
            _ => err("contains? expects (map/set coll key) or (vector/list coll index)"),
        }
    });

    // --- Map update ---
    def_builtin!(env, "update", FnArity::at_least(3), |args| {
        match args {
            [Value::Map(base), key, f, extra @ ..] => {
                let base = to_imut_deep(&Value::Map(base.clone()))?;
                let mut new_map = match base {
                    Value::Map(map) => map,
                    _ => return err("update expects map"),
                };
                let current = new_map
                    .get(&to_key_value_checked(key)?)
                    .cloned()
                    .unwrap_or(Value::Nil);
                let mut call_args = Vec::with_capacity(1 + extra.len());
                call_args.push(current);
                call_args.extend_from_slice(&extra);
                let new_val = call_callable(f.clone(), call_args)?;
                new_map.insert(to_key_value_checked(key)?, to_imut_deep(&new_val)?);
                Ok(Value::Map(new_map))
            }
            [Value::SortedMap(base), key, f, extra @ ..] => {
                let lookup_key = to_key_value_checked(key)?;
                let base = deep_imut_sorted_map(base)?;
                let current = sorted_map_get(&base, &lookup_key)?.unwrap_or(Value::Nil);
                let mut call_args = Vec::with_capacity(1 + extra.len());
                call_args.push(current);
                call_args.extend_from_slice(&extra);
                let new_val = call_callable(f.clone(), call_args)?;
                let mut new_map = base;
                sorted_map_insert_mut(&mut new_map, lookup_key, to_imut_deep(&new_val)?)?;
                Ok(Value::SortedMap(new_map))
            }
            [Value::MutMap(_), key, f, extra @ ..] => {
                let frozen = to_imut_deep(&args[0])?;
                match frozen {
                    Value::Map(map) => {
                        let current = map
                            .get(&to_key_value_checked(key)?)
                            .cloned()
                            .unwrap_or(Value::Nil);
                        let mut call_args = Vec::with_capacity(1 + extra.len());
                        call_args.push(current);
                        call_args.extend_from_slice(&extra);
                        let new_val = call_callable(f.clone(), call_args)?;
                        let mut new_map = map;
                        new_map.insert(to_key_value_checked(key)?, to_imut_deep(&new_val)?);
                        Ok(Value::Map(new_map))
                    }
                    other => Err(type_mismatch_arg("map", "update", 1, &other)),
                }
            }
            [Value::Nil, key, f, extra @ ..] => {
                let mut new_map = HashMap::new();
                let mut call_args = Vec::with_capacity(1 + extra.len());
                call_args.push(Value::Nil);
                call_args.extend_from_slice(&extra);
                let new_val = call_callable(f.clone(), call_args)?;
                new_map.insert(to_key_value_checked(key)?, to_imut_deep(&new_val)?);
                Ok(Value::Map(new_map))
            }
            [other, ..] => Err(type_mismatch_arg("map", "update", 1, other)),
            _ => err("update expects map, key, function, and optional args"),
        }
    });
}

pub(crate) fn install_derived(env: &mut Env) {
    register_derived_metas();
    def_builtin!(env, "range", FnArity::range(0, 3), |args| {
        match args.len() {
            0 => Ok(Value::Seq(SeqHandle::new(Box::new(InfiniteRangeSeq {
                current: 0,
            })))),
            1 => {
                let (end, ef) = as_number(&args[0])?;
                range(0.0, end, 1.0, ef)
            }
            2 => {
                let (start, sf) = as_number(&args[0])?;
                let (end, ef) = as_number(&args[1])?;
                range(start, end, 1.0, sf || ef)
            }
            3 => {
                let (start, sf) = as_number(&args[0])?;
                let (end, ef) = as_number(&args[1])?;
                let (step, stf) = as_number(&args[2])?;
                range(start, end, step, sf || ef || stf)
            }
            _ => err("range expects 0 to 3 numbers"),
        }
    });
    def_builtin!(env, "repeat", FnArity::range(1, 2), |args| {
        match args {
            [value] => Ok(Value::Seq(SeqHandle::new(Box::new(RepeatSeq {
                value: value.clone(),
            })))),
            [n, value] => {
                let (count, _) = as_number(n)?;
                let count = count.max(0.0) as usize;
                let mut out = Vector::new();
                for _ in 0..count {
                    out.push_back(value.clone());
                }
                Ok(Value::Vector(out))
            }
            _ => err("repeat expects (value) or (count value)"),
        }
    });
    def_builtin!(env, "cycle", FnArity::exact(1), |args| {
        match args {
            [coll] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(Value::Seq(SeqHandle::new(Box::new(CycleSeqEngine {
                        source: Some(handle),
                        buffer: Vec::new(),
                        index: 0,
                        source_done: false,
                    }))))
                } else {
                    Ok(empty_seq_value())
                }
            }
            _ => err("cycle expects one collection"),
        }
    });
    def_builtin!(env, "iterate", FnArity::exact(2), |args| match args {
        [f, seed] => Ok(Value::Seq(SeqHandle::new(Box::new(IterateSeqEngine {
            func: f.clone(),
            current: Some(seed.clone()),
        })))),
        _ => err("iterate expects function and seed value"),
    });
    def_builtin!(env, "repeatedly", FnArity::range(1, 2), |args| match args {
        [f] => Ok(Value::Seq(SeqHandle::new(Box::new(RepeatedlySeqEngine {
            func: f.clone(),
        })))),
        [n, f] => {
            let (count, _) = as_number(n)?;
            let count = count.max(0.0) as usize;
            let mut out = Vector::new();
            for _ in 0..count {
                out.push_back(call_callable(f.clone(), vec![])?);
            }
            Ok(Value::Vector(out))
        }
        _ => err("repeatedly expects (f) or (n f)"),
    });
    def_builtin!(env, "map", FnArity::at_least(2), |args| {
        match args.split_first() {
            Some((f, colls)) if !colls.is_empty() => {
                let has_lazy_input = colls.iter().any(|c| matches!(c, Value::Seq(_)));
                if has_lazy_input {
                    let mut handles = Vec::new();
                    for coll in colls {
                        if let Some(handle) = seq_handle_from_value(coll.clone())? {
                            handles.push(handle);
                        } else {
                            return Ok(empty_seq_value());
                        }
                    }
                    return Ok(Value::Seq(SeqHandle::new(Box::new(MapSeqEngine {
                        func: f.clone(),
                        sources: handles,
                    }))));
                }
                let seqs: Vec<Vector<Value>> =
                    colls.iter().map(seq_items).collect::<Result<_, _>>()?;
                let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
                let mut out = Vector::new();
                for i in 0..min_len {
                    guard::tick(None)?;
                    let mut call_args = Vec::with_capacity(seqs.len());
                    for s in &seqs {
                        call_args.push(s[i].clone());
                    }
                    out.push_back(call_callable(f.clone(), call_args)?);
                }
                Ok(Value::Vector(out))
            }
            _ => err("map expects fn and at least one collection"),
        }
    });
    def_builtin!(env, "map-indexed", FnArity::exact(2), |args| match args {
        [f, coll] => {
            if let Some(handle) = seq_handle_from_value(coll.clone())? {
                Ok(Value::Seq(SeqHandle::new(Box::new(MapIndexedSeqEngine {
                    func: f.clone(),
                    source: handle,
                    idx: 0,
                }))))
            } else {
                let items = seq_items(coll)?;
                let mut out = Vector::new();
                for (i, v) in items.iter().enumerate() {
                    guard::tick(None)?;
                    out.push_back(call_callable(
                        f.clone(),
                        vec![Value::Int(i as i64), v.clone()],
                    )?);
                }
                Ok(Value::Vector(out))
            }
        }
        _ => err("map-indexed expects function and collection"),
    });
    def_builtin!(env, "concat", FnArity::at_least(0), |args| {
        let mut out = Vector::new();
        for coll in args {
            let items = seq_items(coll)?;
            for item in items {
                guard::tick(None)?;
                out.push_back(to_imut_deep(&item)?);
            }
        }
        Ok(Value::Vector(out))
    });
    def_builtin!(env, "mapcat", FnArity::at_least(2), |args| {
        match args.split_first() {
            Some((f, colls)) if !colls.is_empty() => {
                let seqs: Vec<Vector<Value>> =
                    colls.iter().map(seq_items).collect::<Result<_, _>>()?;
                let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
                let mut out = Vector::new();
                for i in 0..min_len {
                    guard::tick(None)?;
                    let mut call_args = Vec::with_capacity(seqs.len());
                    for s in &seqs {
                        call_args.push(s[i].clone());
                    }
                    let produced = call_callable(f.clone(), call_args)?;
                    let items = seq_items(&produced)?;
                    for item in items {
                        guard::tick(None)?;
                        out.push_back(to_imut_deep(&item)?);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("mapcat expects fn and at least one collection"),
        }
    });
    def_builtin!(env, "tree-seq", FnArity::exact(3), |args| match args {
        [branch_fn, children_fn, root] => Ok(Value::Seq(SeqHandle::new(Box::new(
            TreeSeqEngine::new(branch_fn.clone(), children_fn.clone(), root.clone()),
        )))),
        _ => err("tree-seq expects branch?, children-fn, and root"),
    });
    def_builtin!(env, "flatten-into", FnArity::range(2, 3), |args| {
        let (value, acc, depth) = match args {
            [value, acc] => (value.clone(), acc, None),
            [value, acc, depth] => (value.clone(), acc, Some(parse_flatten_depth(depth)?)),
            _ => return err("flatten-into expects value, acc, and optional depth"),
        };
        let base = match acc {
            Value::Vector(_) | Value::MutVector(_) => to_imut_deep(acc)?,
            other => return Err(type_mismatch_arg("vector", "flatten-into", 2, other)),
        };
        let mut out = match base {
            Value::Vector(items) => items,
            other => return Err(type_mismatch_arg("vector", "flatten-into", 2, &other)),
        };
        flatten_into_vector(value, &mut out, depth)?;
        Ok(Value::Vector(out))
    });
    def_builtin!(env, "flatten", FnArity::range(1, 2), |args| {
        let (value, depth) = match args {
            [value] => (value.clone(), None),
            [value, depth] => (value.clone(), Some(parse_flatten_depth(depth)?)),
            _ => return err("flatten expects coll or (coll depth)"),
        };
        let mut out = Vector::new();
        flatten_into_vector(value, &mut out, depth)?;
        Ok(Value::Vector(out))
    });
    def_builtin!(env, "reduce", FnArity::range(2, 3), |args| {
        match args.len() {
            2 => {
                let f = args[0].clone();
                let mut iter = seq_items(&args[1])?.into_iter();
                let mut acc = match iter.next() {
                    Some(first) => first,
                    None => return call_callable(f.clone(), vec![]),
                };
                for item in iter {
                    acc = call_callable(f.clone(), vec![acc, item])?;
                }
                Ok(acc)
            }
            3 => {
                let f = args[0].clone();
                let mut acc = args[1].clone();
                if is_conj_fn(&f) && matches!(acc, Value::Vector(_) | Value::MutVector(_)) {
                    let acc = to_mut_shallow(&acc)?;
                    let seq = seq_items(&args[2])?;
                    if let Value::MutVector(handle) = &acc {
                        let mut vec = handle
                            .lock()
                            .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                        for item in seq {
                            guard::tick(None)?;
                            vec.push_back(to_mut_shallow(&item)?);
                        }
                    }
                    return to_imut_deep(&acc);
                }
                if is_concat_fn(&f)
                    && matches!(acc, Value::Vector(_) | Value::List(_) | Value::MutVector(_))
                {
                    let acc = to_mut_shallow(&acc)?;
                    let seq = seq_items(&args[2])?;
                    if let Value::MutVector(handle) = &acc {
                        let mut vec = handle
                            .lock()
                            .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                        for coll in seq {
                            guard::tick(None)?;
                            let items = seq_items(&coll)?;
                            for item in items {
                                guard::tick(None)?;
                                vec.push_back(to_mut_shallow(&item)?);
                            }
                        }
                    }
                    return to_imut_deep(&acc);
                }
                let seq = seq_items(&args[2])?;
                for item in seq {
                    acc = call_callable(f.clone(), vec![acc, item])?;
                }
                Ok(acc)
            }
            _ => err("reduce expects (reduce fn coll) or (reduce fn init coll)"),
        }
    });
}

fn is_conj_fn(value: &Value) -> bool {
    match value {
        Value::Func(func) => matches!(func.debug_name(), Some("conj") | Some("core::conj")),
        _ => false,
    }
}

fn is_concat_fn(value: &Value) -> bool {
    match value {
        Value::Func(func) => matches!(
            func.debug_name(),
            Some("concat") | Some("std::concat") | Some("core::concat")
        ),
        Value::Lambda {
            name: Some(name), ..
        }
        | Value::MultiLambda {
            name: Some(name), ..
        } => {
            matches!(name.as_str(), "concat" | "std::concat")
        }
        _ => false,
    }
}

fn parse_flatten_depth(value: &Value) -> Result<usize, CloveError> {
    let (num, _) = as_number(value)
        .map_err(|_| CloveError::runtime("flatten expects non-negative integer depth"))?;
    let idx = num_to_isize(num)
        .map_err(|_| CloveError::runtime("flatten expects non-negative integer depth"))?;
    if idx < 0 {
        return err("flatten expects non-negative integer depth");
    }
    Ok(idx as usize)
}

fn is_sequential_value(value: &Value) -> bool {
    matches!(
        value,
        Value::List(_) | Value::Vector(_) | Value::MutVector(_) | Value::Seq(_)
    )
}

struct FlattenEntry {
    value: Value,
    depth: Option<usize>,
}

fn push_sequential_items(
    stack: &mut Vec<FlattenEntry>,
    value: Value,
    depth: Option<usize>,
) -> Result<(), CloveError> {
    match value {
        Value::List(items) | Value::Vector(items) => {
            for item in items.iter().rev() {
                guard::tick(None)?;
                stack.push(FlattenEntry {
                    value: item.clone(),
                    depth,
                });
            }
            Ok(())
        }
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            for item in vec.iter().rev() {
                guard::tick(None)?;
                stack.push(FlattenEntry {
                    value: item.clone(),
                    depth,
                });
            }
            Ok(())
        }
        Value::Seq(handle) => {
            let items = handle.collect_all()?;
            for item in items.iter().rev() {
                guard::tick(None)?;
                stack.push(FlattenEntry {
                    value: item.clone(),
                    depth,
                });
            }
            Ok(())
        }
        other => Err(type_mismatch_arg(
            "sequential collection",
            "flatten",
            1,
            &other,
        )),
    }
}

fn flatten_depth_one_item(item: Value, out: &mut Vector<Value>) -> Result<(), CloveError> {
    if matches!(item, Value::Nil) {
        return Ok(());
    }
    match item {
        Value::List(items) | Value::Vector(items) => {
            for nested in items {
                guard::tick(None)?;
                if matches!(nested, Value::Nil) {
                    continue;
                }
                out.push_back(to_imut_deep(&nested)?);
            }
            Ok(())
        }
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            for nested in vec.iter() {
                guard::tick(None)?;
                if matches!(nested, Value::Nil) {
                    continue;
                }
                out.push_back(to_imut_deep(nested)?);
            }
            Ok(())
        }
        Value::Seq(handle) => {
            let items = handle.collect_all()?;
            for nested in items {
                guard::tick(None)?;
                if matches!(nested, Value::Nil) {
                    continue;
                }
                out.push_back(to_imut_deep(&nested)?);
            }
            Ok(())
        }
        other => {
            out.push_back(to_imut_deep(&other)?);
            Ok(())
        }
    }
}

fn flatten_depth_one(value: Value, out: &mut Vector<Value>) -> Result<(), CloveError> {
    if matches!(value, Value::Nil) {
        return Ok(());
    }
    match value {
        Value::List(items) | Value::Vector(items) => {
            for item in items {
                guard::tick(None)?;
                flatten_depth_one_item(item, out)?;
            }
            Ok(())
        }
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            for item in vec.iter() {
                guard::tick(None)?;
                flatten_depth_one_item(item.clone(), out)?;
            }
            Ok(())
        }
        Value::Seq(handle) => {
            let items = handle.collect_all()?;
            for item in items {
                guard::tick(None)?;
                flatten_depth_one_item(item, out)?;
            }
            Ok(())
        }
        other => {
            out.push_back(to_imut_deep(&other)?);
            Ok(())
        }
    }
}

fn flatten_into_vector(
    value: Value,
    out: &mut Vector<Value>,
    depth: Option<usize>,
) -> Result<(), CloveError> {
    if matches!(depth, Some(1)) {
        return flatten_depth_one(value, out);
    }
    let mut stack = Vec::new();
    if let Some(depth) = depth {
        if is_sequential_value(&value) {
            push_sequential_items(&mut stack, value, Some(depth))?;
        } else {
            stack.push(FlattenEntry {
                value,
                depth: Some(depth),
            });
        }
    } else {
        stack.push(FlattenEntry { value, depth: None });
    }
    while let Some(entry) = stack.pop() {
        guard::tick(None)?;
        let value = entry.value;
        if matches!(value, Value::Nil) {
            continue;
        }
        let can_flatten =
            is_sequential_value(&value) && entry.depth.map_or(true, |depth| depth > 0);
        if can_flatten {
            let next_depth = entry.depth.map(|depth| depth.saturating_sub(1));
            push_sequential_items(&mut stack, value, next_depth)?;
        } else {
            out.push_back(to_imut_deep(&value)?);
        }
    }
    Ok(())
}

fn empty_seq_value() -> Value {
    Value::Seq(SeqHandle::from_iter(std::iter::empty::<Value>()))
}

struct MapSeqEngine {
    func: Value,
    sources: Vec<SeqHandle>,
}

impl SeqEngine for MapSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        let mut args = Vec::with_capacity(self.sources.len());
        for src in &self.sources {
            match src.next()? {
                Some(v) => args.push(v),
                None => return Ok(None),
            }
        }
        let result = call_callable(self.func.clone(), args)?;
        Ok(Some(result))
    }
}

struct MapIndexedSeqEngine {
    func: Value,
    source: SeqHandle,
    idx: i64,
}

impl SeqEngine for MapIndexedSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        if let Some(v) = self.source.next()? {
            let out = call_callable(self.func.clone(), vec![Value::Int(self.idx), v])?;
            self.idx = self.idx.wrapping_add(1);
            Ok(Some(out))
        } else {
            Ok(None)
        }
    }
}

fn truthy_value(v: &Value) -> bool {
    !matches!(v, Value::Nil | Value::Bool(false))
}

struct TreeSeqEngine {
    branch_fn: Value,
    children_fn: Value,
    stack: Vec<Value>,
}

impl TreeSeqEngine {
    fn new(branch_fn: Value, children_fn: Value, root: Value) -> Self {
        Self {
            branch_fn,
            children_fn,
            stack: vec![root],
        }
    }
}

impl SeqEngine for TreeSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        while let Some(node) = self.stack.pop() {
            let should_branch = call_callable(self.branch_fn.clone(), vec![node.clone()])?;
            if truthy_value(&should_branch) {
                let children_val = call_callable(self.children_fn.clone(), vec![node.clone()])?;
                if let Some(handle) = seq_handle_from_value(children_val)? {
                    let mut collected = Vec::new();
                    while let Some(child) = handle.next()? {
                        collected.push(child);
                    }
                    for child in collected.into_iter().rev() {
                        self.stack.push(child);
                    }
                }
            }
            return Ok(Some(node));
        }
        Ok(None)
    }
}

struct InfiniteRangeSeq {
    current: i64,
}

impl SeqEngine for InfiniteRangeSeq {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        let next = self.current;
        self.current = self
            .current
            .checked_add(1)
            .unwrap_or_else(|| self.current.wrapping_add(1));
        Ok(Some(Value::Int(next)))
    }
}

struct IterateSeqEngine {
    func: Value,
    current: Option<Value>,
}

impl SeqEngine for IterateSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        if let Some(cur) = self.current.take() {
            let result = cur.clone();
            let next = call_callable(self.func.clone(), vec![cur])?;
            self.current = Some(next);
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }
}

struct RepeatSeq {
    value: Value,
}

impl SeqEngine for RepeatSeq {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        Ok(Some(self.value.clone()))
    }
}

struct CycleSeqEngine {
    source: Option<SeqHandle>,
    buffer: Vec<Value>,
    index: usize,
    source_done: bool,
}

impl SeqEngine for CycleSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        if !self.source_done {
            if let Some(handle) = &self.source {
                if let Some(v) = handle.next()? {
                    self.buffer.push(v.clone());
                    return Ok(Some(v));
                }
            }
            self.source_done = true;
        }
        if self.buffer.is_empty() {
            return Ok(None);
        }
        let item = self.buffer[self.index].clone();
        self.index = (self.index + 1) % self.buffer.len();
        Ok(Some(item))
    }
}

struct RepeatedlySeqEngine {
    func: Value,
}

impl SeqEngine for RepeatedlySeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        let v = call_callable(self.func.clone(), vec![])?;
        Ok(Some(v))
    }
}

fn range(start: f64, end: f64, step: f64, force_float: bool) -> Result<Value, CloveError> {
    if step == 0.0 {
        return err("range step cannot be zero");
    }
    let mut items = Vector::new();
    let mut cur = start;
    let is_float = force_float || start.fract() != 0.0 || end.fract() != 0.0 || step.fract() != 0.0;
    if step > 0.0 {
        while cur < end {
            items.push_back(make_number(cur, is_float || cur.fract() != 0.0));
            cur += step;
        }
    } else {
        while cur > end {
            items.push_back(make_number(cur, is_float || cur.fract() != 0.0));
            cur += step;
        }
    }
    Ok(Value::Vector(items))
}

fn is_nth_index_collection(value: &Value) -> bool {
    matches!(
        value,
        Value::Vector(_) | Value::List(_) | Value::MutVector(_) | Value::TransientVector(_)
    )
}

fn parse_nth_index(value: &Value) -> Result<isize, CloveError> {
    let (n, _) = as_number(value)?;
    let idx = num_to_isize(n)?;
    if idx < 0 {
        return err("nth index must be non-negative");
    }
    Ok(idx)
}

fn parse_nth_indices_from_iter<'a, I>(values: I) -> Result<Vec<usize>, CloveError>
where
    I: IntoIterator<Item = &'a Value>,
{
    let mut out = Vec::new();
    for val in values {
        let idx = parse_nth_index(val)?;
        out.push(idx as usize);
    }
    Ok(out)
}

fn parse_nth_indices(value: &Value) -> Result<Vec<usize>, CloveError> {
    match value {
        Value::Vector(v) | Value::List(v) => parse_nth_indices_from_iter(v.iter()),
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            parse_nth_indices_from_iter(vec.iter())
        }
        Value::TransientVector(handle) => handle.read("nth", |kind| match kind {
            TransientKind::Vector(vec) => parse_nth_indices_from_iter(vec.iter()),
            _ => err("nth expected transient vector"),
        }),
        _ => err("nth expects number or index collection"),
    }
}

fn nth_one(coll: &Value, idx: isize, default: Option<Value>) -> Result<Value, CloveError> {
    match coll {
        Value::Vector(v) => nth_lookup(v, idx, default),
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            nth_lookup(&vec, idx, default)
        }
        Value::TransientVector(handle) => {
            let default_clone = default.clone();
            handle.read("nth", |kind| match kind {
                TransientKind::Vector(vec) => nth_lookup(vec, idx, default_clone),
                _ => err("nth expected transient vector"),
            })
        }
        Value::List(v) => nth_lookup(v, idx, default),
        Value::String(s) => string_nth(s, idx, default),
        Value::Seq(handle) => seq_nth(handle, idx, default),
        Value::Nil => Ok(default.unwrap_or(Value::Nil)),
        other => Err(type_mismatch_arg("collection", "nth", 1, other)),
    }
}

fn nth_many(coll: &Value, indices: &[usize], default: Option<Value>) -> Result<Value, CloveError> {
    if let Value::Seq(handle) = coll {
        return nth_many_seq(handle, indices, default);
    }
    let mut out = Vector::new();
    for idx in indices {
        out.push_back(nth_one(coll, *idx as isize, default.clone())?);
    }
    Ok(Value::Vector(out))
}

fn nth_many_seq(
    handle: &SeqHandle,
    indices: &[usize],
    default: Option<Value>,
) -> Result<Value, CloveError> {
    if indices.is_empty() {
        return Ok(Value::Vector(Vector::new()));
    }
    let mut positions: StdHashMap<usize, Vec<usize>> = StdHashMap::new();
    for (out_idx, idx) in indices.iter().enumerate() {
        positions.entry(*idx).or_default().push(out_idx);
    }
    let max_idx = *indices.iter().max().unwrap();
    let mut out: Vec<Value> = vec![Value::Nil; indices.len()];
    let mut filled = vec![false; indices.len()];
    let mut current = 0usize;
    while current <= max_idx {
        match handle.next()? {
            Some(val) => {
                if let Some(targets) = positions.remove(&current) {
                    for out_idx in targets {
                        out[out_idx] = val.clone();
                        filled[out_idx] = true;
                    }
                }
            }
            None => {
                if let Some(default_val) = default.as_ref() {
                    for (idx, filled_flag) in filled.iter().enumerate() {
                        if !*filled_flag {
                            out[idx] = default_val.clone();
                        }
                    }
                    return Ok(Value::Vector(out.into()));
                }
                return err("nth index out of bounds");
            }
        }
        current += 1;
    }
    Ok(Value::Vector(out.into()))
}

fn nth_lookup(
    items: &Vector<Value>,
    n: isize,
    default: Option<Value>,
) -> Result<Value, CloveError> {
    if n < 0 {
        return err("nth index must be non-negative");
    }
    let idx = n as usize;
    items
        .get(idx)
        .cloned()
        .or(default)
        .ok_or_else(|| CloveError::runtime("nth index out of bounds"))
}

fn string_nth(s: &str, n: isize, default: Option<Value>) -> Result<Value, CloveError> {
    if n < 0 {
        return err("nth index must be non-negative");
    }
    let idx = n as usize;
    let ch_opt = s.chars().nth(idx);
    match (ch_opt, default) {
        (Some(c), _) => Ok(Value::String(c.to_string())),
        (None, Some(d)) => Ok(d),
        (None, None) => err("nth index out of bounds"),
    }
}

fn parse_subvec_index(idx_val: &Value, len: usize) -> Result<usize, CloveError> {
    let (n, _) = as_number(idx_val)?;
    let idx = num_to_isize(n)?;
    if idx < 0 {
        return err("subvec index must be non-negative");
    }
    let idx = idx as usize;
    if idx > len {
        return err("subvec index out of bounds");
    }
    Ok(idx)
}

fn subvec_slice(coll: &Value, start: &Value, end: Option<&Value>) -> Result<Value, CloveError> {
    let items = seq_items(coll)?;
    let len = items.len();
    let start_idx = parse_subvec_index(start, len)?;
    let end_idx = match end {
        Some(val) => parse_subvec_index(val, len)?,
        None => len,
    };
    if end_idx < start_idx {
        return err("subvec end must be >= start");
    }
    let slice: Vector<Value> = items
        .iter()
        .skip(start_idx)
        .take(end_idx - start_idx)
        .cloned()
        .collect();
    Ok(Value::Vector(slice))
}

fn seq_nth(handle: &SeqHandle, n: isize, default: Option<Value>) -> Result<Value, CloveError> {
    if n < 0 {
        return err("nth index must be non-negative");
    }
    let mut idx: usize = 0;
    while let Some(v) = handle.next()? {
        if idx == n as usize {
            return Ok(v);
        }
        idx += 1;
    }
    match default {
        Some(v) => Ok(v),
        None => err("nth index out of bounds"),
    }
}

fn seq_index_lookup(items: &Vector<Value>, key: &Value) -> Result<Option<Value>, CloveError> {
    let (idx, _) = as_number(key)?;
    let idx = num_to_isize(idx)?;
    if idx < 0 {
        return Ok(None);
    }
    Ok(items.get(idx as usize).cloned())
}

fn value_or_default(result: Option<Value>, default: Option<&Value>) -> Value {
    match result {
        Some(val) => val,
        None => default.cloned().unwrap_or(Value::Nil),
    }
}

fn seq_get_lookup(
    items: &Vector<Value>,
    key: &Value,
    default: Option<&Value>,
    kind: SequenceKind,
) -> Result<Value, CloveError> {
    match parse_lookup_request(key)? {
        LookupRequest::Index(idx) => match normalize_index(idx, items.len(), false) {
            Ok(resolved) => Ok(value_or_default(items.get(resolved).cloned(), default)),
            Err(_) => Ok(value_or_default(None, default)),
        },
        LookupRequest::Range(spec) => match slice_sequence(items, spec, kind) {
            Ok(val) => Ok(val),
            Err(_) => Ok(value_or_default(None, default)),
        },
        LookupRequest::Indexes(indexes) => match gather_sequence(items, indexes, kind) {
            Ok(val) => Ok(val),
            Err(_) => Ok(value_or_default(None, default)),
        },
    }
}

fn string_get_lookup(
    text: &str,
    key: &Value,
    default: Option<&Value>,
) -> Result<Value, CloveError> {
    let chars: Vec<char> = text.chars().collect();
    match parse_lookup_request(key)? {
        LookupRequest::Index(idx) => match normalize_index(idx, chars.len(), false) {
            Ok(resolved) => Ok(value_or_default(
                Some(Value::String(chars[resolved].to_string())),
                default,
            )),
            Err(_) => Ok(value_or_default(None, default)),
        },
        LookupRequest::Range(spec) => match slice_string(&chars, spec) {
            Ok(val) => Ok(val),
            Err(_) => Ok(value_or_default(None, default)),
        },
        LookupRequest::Indexes(indexes) => match gather_string(&chars, indexes) {
            Ok(val) => Ok(val),
            Err(_) => Ok(value_or_default(None, default)),
        },
    }
}

pub(crate) fn get_value(
    target: &Value,
    key: &Value,
    default: Option<&Value>,
) -> Result<Value, CloveError> {
    match target {
        Value::Map(map) => Ok(value_or_default(
            map.get(&to_key_value_checked(key)?).cloned(),
            default,
        )),
        Value::SortedMap(map) => Ok(value_or_default(
            sorted_map_get(map, &to_key_value_checked(key)?)?,
            default,
        )),
        Value::MutMap(handle) => {
            let map = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
            Ok(value_or_default(
                map.get(&to_key_value_checked(key)?).cloned(),
                default,
            ))
        }
        Value::Vector(vec) => seq_get_lookup(vec, key, default, SequenceKind::Vector),
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            seq_get_lookup(&vec, key, default, SequenceKind::Vector)
        }
        Value::List(list) => seq_get_lookup(list, key, default, SequenceKind::List),
        Value::TransientVector(handle) => handle.read("get", |kind| match kind {
            TransientKind::Vector(vec) => seq_get_lookup(vec, key, default, SequenceKind::Vector),
            _ => err("get expected transient vector"),
        }),
        Value::String(text) => string_get_lookup(text, key, default),
        Value::Set(set) => Ok(if set.contains(key) {
            key.clone()
        } else {
            value_or_default(None, default)
        }),
        Value::SortedSet(set) => Ok(if sorted_set_contains(set, key)? {
            key.clone()
        } else {
            value_or_default(None, default)
        }),
        Value::MutSet(handle) => {
            let set = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
            Ok(if set.contains(key) {
                key.clone()
            } else {
                value_or_default(None, default)
            })
        }
        Value::TransientMap(handle) => handle.read("get", |kind| match kind {
            TransientKind::Map(map) => Ok(value_or_default(
                map.get(&to_key_value_checked(key)?).cloned(),
                default,
            )),
            _ => err("get expected transient map"),
        }),
        Value::TransientSet(handle) => handle.read("get", |kind| match kind {
            TransientKind::Set(set) => Ok(if set.contains(key) {
                key.clone()
            } else {
                value_or_default(None, default)
            }),
            _ => err("get expected transient set"),
        }),
        Value::Seq(handle) => {
            let idx = num_to_isize(as_number(key)?.0)?;
            if let Some(default_val) = default {
                seq_nth(handle, idx, Some(default_val.clone()))
            } else {
                seq_nth(handle, idx, None)
                    .map(Some)
                    .map(|res| value_or_default(res, None))
            }
        }
        Value::Nil => Ok(value_or_default(None, default)),
        other => Err(type_mismatch_arg(
            "map/set/vector/list/string/seq/nil",
            "get",
            1,
            other,
        )),
    }
}

pub(crate) fn includes_value(target: &Value, needle: &Value) -> Result<bool, CloveError> {
    match target {
        Value::String(s) => {
            let needle = expect_string(needle, "includes?", 2)?;
            Ok(s.contains(&needle))
        }
        Value::Set(set) => Ok(set.contains(needle)),
        Value::SortedSet(set) => Ok(sorted_set_contains(set, needle)?),
        Value::Map(map) => Ok(map.contains_key(&to_key_value_checked(needle)?)),
        Value::SortedMap(map) => Ok(sorted_map_get(map, &to_key_value_checked(needle)?)?.is_some()),
        Value::MutSet(handle) => {
            let set = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
            Ok(set.contains(needle))
        }
        Value::MutMap(handle) => {
            let map = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
            Ok(map.contains_key(&to_key_value_checked(needle)?))
        }
        Value::TransientSet(handle) => handle.read("includes?", |kind| match kind {
            TransientKind::Set(set) => Ok(set.contains(needle)),
            _ => err("includes? expected transient set"),
        }),
        Value::TransientMap(handle) => handle.read("includes?", |kind| match kind {
            TransientKind::Map(map) => Ok(map.contains_key(&to_key_value_checked(needle)?)),
            _ => err("includes? expected transient map"),
        }),
        Value::Vector(vec) => Ok(vec.iter().any(|item| value_eq(item, needle))),
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            Ok(vec.iter().any(|item| value_eq(item, needle)))
        }
        Value::List(list) => Ok(list.iter().any(|item| value_eq(item, needle))),
        Value::TransientVector(handle) => handle.read("includes?", |kind| match kind {
            TransientKind::Vector(vec) => Ok(vec.iter().any(|item| value_eq(item, needle))),
            _ => err("includes? expected transient vector"),
        }),
        Value::Seq(handle) => {
            let mut found = false;
            while let Some(item) = handle.next()? {
                if value_eq(&item, needle) {
                    found = true;
                    break;
                }
            }
            Ok(found)
        }
        Value::Nil => Ok(false),
        other => Err(type_mismatch_arg(
            "string/set/map/vector/list/seq",
            "includes?",
            1,
            other,
        )),
    }
}

fn path_items<'a>(path: &'a Value, op: &str) -> Result<&'a Vector<Value>, CloveError> {
    match path {
        Value::Vector(items) | Value::List(items) => Ok(items),
        other => Err(type_mismatch_arg("path vector or list", op, 2, other)),
    }
}

fn to_key_ref(value: &Value) -> Result<Key, CloveError> {
    to_key_value_checked(value)
}

fn normalize_get_in_many_paths(paths: &Value) -> Result<Vec<Value>, CloveError> {
    let items = match paths {
        Value::Vector(items) | Value::List(items) => items,
        other => {
            return Err(type_mismatch_arg(
                "path vector or list",
                "get-in-many",
                2,
                other,
            ))
        }
    };
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        match item {
            Value::Vector(_) | Value::List(_) => out.push(item.clone()),
            _ => {
                let mut path = Vector::new();
                path.push_back(item.clone());
                out.push(Value::Vector(path));
            }
        }
    }
    Ok(out)
}

fn is_get_in_many_nth_target(target: &Value) -> bool {
    matches!(
        target,
        Value::Vector(_)
            | Value::List(_)
            | Value::MutVector(_)
            | Value::TransientVector(_)
            | Value::Seq(_)
            | Value::String(_)
            | Value::Nil
    )
}

fn collect_nth_indices_from_paths(paths: &[Value]) -> Result<Option<Vec<usize>>, CloveError> {
    let mut indices = Vec::with_capacity(paths.len());
    for path in paths {
        let items = match path {
            Value::Vector(items) | Value::List(items) => items,
            _ => return Ok(None),
        };
        if items.len() != 1 {
            return Ok(None);
        }
        let idx = parse_nth_index(&items[0])?;
        indices.push(idx as usize);
    }
    Ok(Some(indices))
}

fn get_in_many_from_value(
    target: &Value,
    paths: &Value,
    default: Option<Value>,
) -> Result<Value, CloveError> {
    let paths = normalize_get_in_many_paths(paths)?;
    if paths.is_empty() {
        return Ok(Value::Vector(Vector::new()));
    }
    if is_get_in_many_nth_target(target) {
        if let Some(indices) = collect_nth_indices_from_paths(&paths)? {
            return nth_many(target, &indices, default);
        }
    }
    let mut out = Vector::new();
    for path in paths {
        let value = get_in_from_value(target.clone(), &path, default.clone())?;
        out.push_back(value);
    }
    Ok(Value::Vector(out))
}

fn get_in_from_value(
    target: Value,
    path: &Value,
    default: Option<Value>,
) -> Result<Value, CloveError> {
    let keys = path_items(path, "get-in")?;
    if keys.is_empty() {
        return Ok(target);
    }
    let mut current = target;
    for key in keys.iter() {
        let next = match &current {
            Value::Map(map) => map.get(&to_key_ref(key)?).cloned(),
            Value::SortedMap(map) => sorted_map_get(map, &to_key_ref(key)?)?,
            Value::MutMap(handle) => {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                map.get(&to_key_ref(key)?).cloned()
            }
            Value::Vector(vec) => seq_index_lookup(vec, key)?,
            Value::MutVector(handle) => {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                seq_index_lookup(&vec, key)?
            }
            Value::List(list) => seq_index_lookup(list, key)?,
            Value::Set(set) => {
                if set.contains(key) {
                    Some(key.clone())
                } else {
                    None
                }
            }
            Value::SortedSet(set) => {
                if sorted_set_contains(set, key)? {
                    Some(key.clone())
                } else {
                    None
                }
            }
            Value::MutSet(handle) => {
                let set = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
                if set.contains(key) {
                    Some(key.clone())
                } else {
                    None
                }
            }
            Value::Nil => return Ok(default.clone().unwrap_or(Value::Nil)),
            other => {
                return Err(CloveError::type_mismatch(
                    "collection",
                    actual_type_with_preview(other),
                ))
            }
        };
        match next {
            Some(val) => current = val,
            None => return Ok(default.clone().unwrap_or(Value::Nil)),
        }
    }
    Ok(current)
}

fn get_child_value(op: &str, current: &Value, key: &Value) -> Result<Value, CloveError> {
    match current {
        Value::Map(map) => Ok(map.get(&to_key_ref(key)?).cloned().unwrap_or(Value::Nil)),
        Value::SortedMap(map) => Ok(sorted_map_get(map, &to_key_ref(key)?)?.unwrap_or(Value::Nil)),
        Value::MutMap(handle) => {
            let map = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
            Ok(map.get(&to_key_ref(key)?).cloned().unwrap_or(Value::Nil))
        }
        Value::Vector(vec) => seq_get_lookup(vec, key, None, SequenceKind::Vector),
        Value::MutVector(handle) => {
            let vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            seq_get_lookup(&vec, key, None, SequenceKind::Vector)
        }
        Value::List(list) => seq_get_lookup(list, key, None, SequenceKind::List),
        Value::Set(set) => Ok(if set.contains(key) {
            key.clone()
        } else {
            Value::Nil
        }),
        Value::SortedSet(set) => Ok(if sorted_set_contains(set, key)? {
            key.clone()
        } else {
            Value::Nil
        }),
        Value::MutSet(handle) => {
            let set = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable set lock poisoned"))?;
            Ok(if set.contains(key) {
                key.clone()
            } else {
                Value::Nil
            })
        }
        Value::Nil => Ok(Value::Nil),
        other => Err(type_mismatch_arg("collection", op, 1, other)),
    }
}

fn assoc_value(op: &str, current: Value, key: &Value, value: Value) -> Result<Value, CloveError> {
    match current {
        Value::Map(map) => {
            let pair = [key.clone(), value];
            assoc_map(&map, &pair)
        }
        Value::SortedMap(map) => {
            let map_key = to_key_value_checked(key)?;
            let base = deep_imut_sorted_map(&map)?;
            let mut out = base;
            sorted_map_insert_mut(&mut out, map_key, to_imut_deep(&value)?)?;
            Ok(Value::SortedMap(out))
        }
        Value::Vector(vec) => {
            let pair = [key.clone(), value];
            let base = deep_imut_vector(&vec)?;
            assoc_indexed(&base, &pair, false)
        }
        Value::List(list) => {
            let pair = [key.clone(), value];
            let base = deep_imut_vector(&list)?;
            assoc_indexed(&base, &pair, true)
        }
        Value::Nil => {
            let pair = [key.clone(), value];
            assoc_map(&HashMap::new(), &pair)
        }
        Value::MutMap(_) => {
            let frozen = to_imut_deep(&current)?;
            match frozen {
                Value::Map(map) => {
                    let pair = [key.clone(), value];
                    assoc_map(&map, &pair)
                }
                other => Err(type_mismatch_arg("map", op, 1, &other)),
            }
        }
        Value::MutVector(_) => {
            let frozen = to_imut_deep(&current)?;
            match frozen {
                Value::Vector(vec) => {
                    let pair = [key.clone(), value];
                    assoc_indexed(&vec, &pair, false)
                }
                other => Err(type_mismatch_arg("vector", op, 1, &other)),
            }
        }
        other => Err(type_mismatch_arg("map/vector/list", op, 1, &other)),
    }
}

fn parse_index_value(op: &str, key: &Value) -> Result<usize, CloveError> {
    let (n, _) = as_number(key)?;
    let idx = num_to_isize(n)?;
    if idx < 0 {
        return err(format!("{op} index must be non-negative"));
    }
    Ok(idx as usize)
}

fn assoc_value_mut(
    op: &str,
    current: Value,
    key: &Value,
    value: Value,
) -> Result<Value, CloveError> {
    match current {
        Value::MutMap(handle) => {
            let handle_clone = Arc::clone(&handle);
            {
                let mut map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                let converted = to_mut_shallow(&value)?;
                map.insert(to_key_value_checked(key)?, converted);
            }
            Ok(Value::MutMap(handle_clone))
        }
        Value::MutVector(handle) => {
            let idx = parse_index_value(op, key)?;
            let handle_clone = Arc::clone(&handle);
            {
                let mut vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                if idx > vec.len() {
                    return err(format!("{op} index out of bounds"));
                }
                let converted = to_mut_shallow(&value)?;
                if idx == vec.len() {
                    vec.push_back(converted);
                } else {
                    vec[idx] = converted;
                }
            }
            Ok(Value::MutVector(handle_clone))
        }
        other => Err(type_mismatch_arg("mutable map/vector", op, 1, &other)),
    }
}

fn update_value_mut(
    op: &str,
    target: &Value,
    key: &Value,
    func: &Value,
    extra: &[Value],
    allow_transient: bool,
) -> Result<Value, CloveError> {
    let mut call_args = Vec::with_capacity(1 + extra.len());
    match target {
        Value::MutMap(handle) => {
            let current = {
                let map = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
                map.get(&to_key_ref(key)?).cloned().unwrap_or(Value::Nil)
            };
            call_args.push(current);
            call_args.extend_from_slice(extra);
            let next_val = call_callable(func.clone(), call_args)?;
            let converted = to_mut_shallow(&next_val)?;
            let mut map = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable map lock poisoned"))?;
            map.insert(to_key_value_checked(key)?, converted);
            Ok(Value::MutMap(handle.clone()))
        }
        Value::MutVector(handle) => {
            let idx = parse_index_value(op, key)?;
            let current = {
                let vec = handle
                    .lock()
                    .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
                if idx > vec.len() {
                    return err(format!("{op} index out of bounds"));
                }
                if idx == vec.len() {
                    Value::Nil
                } else {
                    vec[idx].clone()
                }
            };
            call_args.push(current);
            call_args.extend_from_slice(extra);
            let next_val = call_callable(func.clone(), call_args)?;
            let converted = to_mut_shallow(&next_val)?;
            let mut vec = handle
                .lock()
                .map_err(|_| CloveError::runtime("mutable vector lock poisoned"))?;
            if idx > vec.len() {
                return err(format!("{op} index out of bounds"));
            }
            if idx == vec.len() {
                vec.push_back(converted);
            } else {
                vec[idx] = converted;
            }
            Ok(Value::MutVector(handle.clone()))
        }
        Value::TransientMap(handle) if allow_transient => {
            let map_key = to_key_value_checked(key)?;
            let current = handle.read(op, |kind| match kind {
                TransientKind::Map(map) => Ok(map.get(&map_key).cloned().unwrap_or(Value::Nil)),
                _ => err("update! expected transient map"),
            })?;
            call_args.push(current);
            call_args.extend_from_slice(extra);
            let next_val = call_callable(func.clone(), call_args)?;
            let (_, next) = handle.write(op, |kind| match kind {
                TransientKind::Map(map) => {
                    map.insert(map_key, next_val.clone());
                    Ok(())
                }
                _ => err("update! expected transient map"),
            })?;
            Ok(Value::TransientMap(next))
        }
        Value::TransientVector(handle) if allow_transient => {
            let idx = parse_index_value(op, key)?;
            let current = handle.read(op, |kind| match kind {
                TransientKind::Vector(vec) => {
                    if idx > vec.len() {
                        return err(format!("{op} index out of bounds"));
                    }
                    if idx == vec.len() {
                        Ok(Value::Nil)
                    } else {
                        Ok(vec[idx].clone())
                    }
                }
                _ => err("update! expected transient vector"),
            })?;
            call_args.push(current);
            call_args.extend_from_slice(extra);
            let next_val = call_callable(func.clone(), call_args)?;
            let (_, next) = handle.write(op, |kind| match kind {
                TransientKind::Vector(vec) => {
                    if idx > vec.len() {
                        return err(format!("{op} index out of bounds"));
                    }
                    if idx == vec.len() {
                        vec.push_back(next_val.clone());
                    } else {
                        vec[idx] = next_val.clone();
                    }
                    Ok(())
                }
                _ => err("update! expected transient vector"),
            })?;
            Ok(Value::TransientVector(next))
        }
        other => Err(type_mismatch_arg(
            if allow_transient {
                "mutable or transient map/vector"
            } else {
                "mutable map/vector"
            },
            op,
            1,
            other,
        )),
    }
}

fn get_or_make_mut_child(op: &str, parent: &Value, key: &Value) -> Result<Value, CloveError> {
    let child = get_child_value(op, parent, key)?;
    match child {
        Value::Nil => {
            let next = Value::MutMap(Arc::new(Mutex::new(HashMap::new())));
            assoc_value_mut(op, parent.clone(), key, next.clone())?;
            Ok(next)
        }
        Value::MutMap(_) | Value::MutVector(_) => Ok(child),
        Value::Map(_) | Value::SortedMap(_) | Value::Vector(_) => {
            let next = to_mut_shallow(&child)?;
            assoc_value_mut(op, parent.clone(), key, next.clone())?;
            Ok(next)
        }
        other => Err(type_mismatch_arg("map/vector/list", op, 1, &other)),
    }
}

fn assoc_in_mut_from_value(target: Value, path: &Value, value: Value) -> Result<Value, CloveError> {
    let keys = path_items(path, "assoc-in!")?;
    let total = keys.len();
    if total == 0 {
        return to_mut_shallow(&value);
    }

    let ensure_child = |current: &Value, key: &Value| -> Result<Value, CloveError> {
        get_or_make_mut_child("assoc-in!", current, key)
    };

    let assoc_step =
        |current: Value, key: &Value, next: Value| assoc_value_mut("assoc-in!", current, key, next);

    match total {
        1 => assoc_step(target, &keys[0], value),
        2 => {
            let child = ensure_child(&target, &keys[0])?;
            let updated = assoc_step(child, &keys[1], value)?;
            assoc_step(target, &keys[0], updated)
        }
        3 => {
            let child = ensure_child(&target, &keys[0])?;
            let grand = ensure_child(&child, &keys[1])?;
            let updated = assoc_step(grand, &keys[2], value)?;
            let next = assoc_step(child, &keys[1], updated)?;
            assoc_step(target, &keys[0], next)
        }
        4 => {
            let child = ensure_child(&target, &keys[0])?;
            let grand = ensure_child(&child, &keys[1])?;
            let great = ensure_child(&grand, &keys[2])?;
            let updated = assoc_step(great, &keys[3], value)?;
            let next = assoc_step(grand, &keys[2], updated)?;
            let next = assoc_step(child, &keys[1], next)?;
            assoc_step(target, &keys[0], next)
        }
        _ => {
            fn step(
                current: Value,
                keys: &Vector<Value>,
                idx: usize,
                value: Value,
            ) -> Result<Value, CloveError> {
                let key = &keys[idx];
                if idx + 1 == keys.len() {
                    return assoc_value_mut("assoc-in!", current, key, value);
                }
                let next_child = get_or_make_mut_child("assoc-in!", &current, key)?;
                let updated = step(next_child, keys, idx + 1, value)?;
                assoc_value_mut("assoc-in!", current, key, updated)
            }
            step(target, keys, 0, value)
        }
    }
}

fn update_in_mut_from_value(
    target: Value,
    path: &Value,
    func: Value,
    extra: &[Value],
) -> Result<Value, CloveError> {
    let keys = path_items(path, "update-in!")?;
    if keys.is_empty() {
        return Ok(target);
    }
    let total = keys.len();

    let ensure_child = |current: &Value, key: &Value| -> Result<Value, CloveError> {
        get_or_make_mut_child("update-in!", current, key)
    };

    let assoc_step = |current: Value, key: &Value, next: Value| {
        assoc_value_mut("update-in!", current, key, next)
    };

    let update_leaf = |current: Value, key: &Value| {
        update_value_mut("update-in!", &current, key, &func, extra, false)
    };

    match total {
        1 => update_leaf(target, &keys[0]),
        2 => {
            let child = ensure_child(&target, &keys[0])?;
            let updated = update_leaf(child, &keys[1])?;
            assoc_step(target, &keys[0], updated)
        }
        3 => {
            let child = ensure_child(&target, &keys[0])?;
            let grand = ensure_child(&child, &keys[1])?;
            let updated = update_leaf(grand, &keys[2])?;
            let next = assoc_step(child, &keys[1], updated)?;
            assoc_step(target, &keys[0], next)
        }
        4 => {
            let child = ensure_child(&target, &keys[0])?;
            let grand = ensure_child(&child, &keys[1])?;
            let great = ensure_child(&grand, &keys[2])?;
            let updated = update_leaf(great, &keys[3])?;
            let next = assoc_step(grand, &keys[2], updated)?;
            let next = assoc_step(child, &keys[1], next)?;
            assoc_step(target, &keys[0], next)
        }
        _ => {
            fn step(
                current: Value,
                keys: &Vector<Value>,
                idx: usize,
                func: &Value,
                extra: &[Value],
            ) -> Result<Value, CloveError> {
                let key = &keys[idx];
                if idx + 1 == keys.len() {
                    return update_value_mut("update-in!", &current, key, func, extra, false);
                }
                let next_child = get_or_make_mut_child("update-in!", &current, key)?;
                let updated = step(next_child, keys, idx + 1, func, extra)?;
                assoc_value_mut("update-in!", current, key, updated)
            }
            step(target, keys, 0, &func, extra)
        }
    }
}

fn assoc_in_from_value(target: Value, path: &Value, value: Value) -> Result<Value, CloveError> {
    let keys = path_items(path, "assoc-in")?;
    let total = keys.len();
    if total == 0 {
        return Ok(value);
    }

    let get_child = |current: &Value, key: &Value| -> Result<Value, CloveError> {
        let child = get_child_value("assoc-in", current, key)?;
        Ok(if matches!(child, Value::Nil) {
            Value::Map(HashMap::new())
        } else {
            child
        })
    };

    let assoc_step =
        |current: Value, key: &Value, next: Value| assoc_value("assoc-in", current, key, next);

    match total {
        1 => assoc_step(target, &keys[0], value),
        2 => {
            let child = get_child(&target, &keys[0])?;
            let updated = assoc_step(child, &keys[1], value)?;
            assoc_step(target, &keys[0], updated)
        }
        3 => {
            let child = get_child(&target, &keys[0])?;
            let grand = get_child(&child, &keys[1])?;
            let updated = assoc_step(grand, &keys[2], value)?;
            let next = assoc_step(child, &keys[1], updated)?;
            assoc_step(target, &keys[0], next)
        }
        4 => {
            let child = get_child(&target, &keys[0])?;
            let grand = get_child(&child, &keys[1])?;
            let great = get_child(&grand, &keys[2])?;
            let updated = assoc_step(great, &keys[3], value)?;
            let next = assoc_step(grand, &keys[2], updated)?;
            let next = assoc_step(child, &keys[1], next)?;
            assoc_step(target, &keys[0], next)
        }
        _ => {
            fn step(
                current: Value,
                keys: &Vector<Value>,
                idx: usize,
                value: Value,
            ) -> Result<Value, CloveError> {
                if idx >= keys.len() {
                    return Ok(value);
                }
                let key = &keys[idx];
                let child = get_child_value("assoc-in", &current, key)?;
                let next_child = if matches!(child, Value::Nil) {
                    Value::Map(HashMap::new())
                } else {
                    child
                };
                let updated = step(next_child, keys, idx + 1, value)?;
                assoc_value("assoc-in", current, key, updated)
            }
            step(target, keys, 0, value)
        }
    }
}

fn update_in_from_value(
    target: Value,
    path: &Value,
    func: Value,
    extra: &[Value],
) -> Result<Value, CloveError> {
    let keys = path_items(path, "update-in")?;
    let total = keys.len();
    if total == 0 {
        return Ok(target);
    }

    let apply_func = |current: Value| -> Result<Value, CloveError> {
        let mut call_args = Vec::with_capacity(1 + extra.len());
        call_args.push(current);
        call_args.extend_from_slice(extra);
        call_callable(func.clone(), call_args)
    };

    let get_child = |current: &Value, key: &Value| -> Result<Value, CloveError> {
        let child = get_child_value("update-in", current, key)?;
        Ok(if matches!(child, Value::Nil) {
            Value::Map(HashMap::new())
        } else {
            child
        })
    };

    let assoc_step =
        |current: Value, key: &Value, next: Value| assoc_value("update-in", current, key, next);

    let update_leaf = |current: Value, key: &Value| -> Result<Value, CloveError> {
        let current_val = get_child_value("update-in", &current, key)?;
        let next_val = apply_func(current_val)?;
        assoc_step(current, key, next_val)
    };

    match total {
        1 => update_leaf(target, &keys[0]),
        2 => {
            let child = get_child(&target, &keys[0])?;
            let updated = update_leaf(child, &keys[1])?;
            assoc_step(target, &keys[0], updated)
        }
        3 => {
            let child = get_child(&target, &keys[0])?;
            let grand = get_child(&child, &keys[1])?;
            let updated = update_leaf(grand, &keys[2])?;
            let next = assoc_step(child, &keys[1], updated)?;
            assoc_step(target, &keys[0], next)
        }
        4 => {
            let child = get_child(&target, &keys[0])?;
            let grand = get_child(&child, &keys[1])?;
            let great = get_child(&grand, &keys[2])?;
            let updated = update_leaf(great, &keys[3])?;
            let next = assoc_step(grand, &keys[2], updated)?;
            let next = assoc_step(child, &keys[1], next)?;
            assoc_step(target, &keys[0], next)
        }
        _ => {
            fn step(
                current: Value,
                keys: &Vector<Value>,
                idx: usize,
                func: &Value,
                extra: &[Value],
            ) -> Result<Value, CloveError> {
                if idx >= keys.len() {
                    return Ok(current);
                }
                let key = &keys[idx];
                if idx + 1 == keys.len() {
                    let mut call_args = Vec::with_capacity(1 + extra.len());
                    let current_val = get_child_value("update-in", &current, key)?;
                    call_args.push(current_val);
                    call_args.extend_from_slice(extra);
                    let next_val = call_callable(func.clone(), call_args)?;
                    return assoc_value("update-in", current, key, next_val);
                }
                let child = get_child_value("update-in", &current, key)?;
                let next_child = if matches!(child, Value::Nil) {
                    Value::Map(HashMap::new())
                } else {
                    child
                };
                let updated = step(next_child, keys, idx + 1, func, extra)?;
                assoc_value("update-in", current, key, updated)
            }
            step(target, keys, 0, &func, extra)
        }
    }
}

fn register_derived_metas() {
    let mut range_meta = FnMeta::new("core", "range");
    range_meta.arglist.push("[]".into());
    range_meta.arglist.push("[end]".into());
    range_meta.arglist.push("[start end]".into());
    range_meta.arglist.push("[start end step]".into());
    range_meta.doc = Some(
        "Returns numbers from start (inclusive) to end (exclusive) by step. With no bounds it is an infinite lazy sequence starting at 0; bounded arities realize a finite collection."
            .into(),
    );
    range_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    range_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    range_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Int, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    range_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    range_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(range_meta);

    let mut repeat_meta = FnMeta::new("core", "repeat");
    repeat_meta.arglist.push("[x]".into());
    repeat_meta.arglist.push("[n x]".into());
    repeat_meta.doc = Some(
        "Produces copies of a value: one-arg returns an infinite lazy sequence, two-arg returns a vector of n repetitions."
            .into(),
    );
    repeat_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    repeat_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    repeat_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(repeat_meta);

    let mut cycle_meta = FnMeta::new("core", "cycle");
    cycle_meta.arglist.push("[coll]".into());
    cycle_meta.doc = Some("Return an infinite lazy sequence cycling through coll.".into());
    cycle_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    cycle_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(cycle_meta);

    let mut map_meta = FnMeta::new("core", "map");
    map_meta.arglist.push("[f coll & colls]".into());
    map_meta.doc = Some(
        "Applies f to parallel items from each collection, stopping at the shortest; yields a lazy seq when any input is a seq, otherwise an eager vector."
            .into(),
    );
    map_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Named("clove::core::Function".into()),
            TypeKind::Vector(Box::new(TypeKind::Any)),
        ],
        rest: Some(TypeKind::vector(TypeKind::Any)),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    map_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(map_meta);

    let mut concat_meta = FnMeta::new("core", "concat");
    concat_meta.arglist.push("[& colls]".into());
    concat_meta.doc = Some("Eagerly concatenate colls into a single vector.".into());
    concat_meta.overloads.push(FnOverload {
        arg_types: vec![],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Vector(Box::new(TypeKind::Any)),
        special_op: None,
    });
    concat_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(concat_meta);

    let mut mapcat_meta = FnMeta::new("core", "mapcat");
    mapcat_meta.arglist.push("[f coll & colls]".into());
    mapcat_meta.doc =
        Some("Apply f across the provided colls and concat each intermediate result.".into());
    mapcat_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Named("clove::core::Function".into()),
            TypeKind::Any,
        ],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Vector(Box::new(TypeKind::Any)),
        special_op: None,
    });
    mapcat_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(mapcat_meta);

    let mut reduce_meta = FnMeta::new("core", "reduce");
    reduce_meta.arglist.push("[f coll]".into());
    reduce_meta.arglist.push("[f init coll]".into());
    reduce_meta.doc = Some(
        "Combines a collection with f: without init uses the first element or calls f with no args when coll is empty; with init uses it as the starting accumulator."
            .into(),
    );
    reduce_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Vector(Box::new(TypeKind::Any))],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    reduce_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Any,
            TypeKind::Any,
            TypeKind::Vector(Box::new(TypeKind::Any)),
        ],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    reduce_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(reduce_meta);

    let mut flatten_meta = FnMeta::new("core", "flatten");
    flatten_meta.arglist.push("[coll]".into());
    flatten_meta.arglist.push("[coll depth]".into());
    flatten_meta.doc = Some(
        "Flatten nested sequential structures into a vector. If depth is provided, flatten at most depth levels.".into(),
    );
    flatten_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Vector(Box::new(TypeKind::Any)),
        special_op: None,
    });
    flatten_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Vector(Box::new(TypeKind::Any)),
        special_op: None,
    });
    flatten_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(flatten_meta);
}

fn register_primitive_metas() {
    let mut into_meta = FnMeta::new("core", "into");
    into_meta.arglist.push("[to-coll from-coll]".into());
    into_meta.doc =
        Some("Conjoin each value from from-coll onto to-coll, preserving the target type.".into());
    into_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    into_meta.subject_pos = Some(SubjectPos::Fixed(2));
    fn_meta::register(into_meta);

    let mut count_meta = FnMeta::new("core", "count");
    count_meta.arglist.push("[coll]".into());
    count_meta.doc = Some(
        "Returns the number of items in a collection (nil counts as 0); also works for strings and other countable types."
            .into(),
    );
    count_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    count_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(count_meta);

    let mut includes_meta = FnMeta::new("core", "includes?");
    includes_meta.arglist.push("[coll x]".into());
    includes_meta.doc = Some(
        "Return true when coll contains x. For strings, checks substring; for maps/sets, checks keys; for vectors/lists/seqs, checks values. Unlike contains?, which checks keys or indices."
            .into(),
    );
    includes_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    includes_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(includes_meta);

    let mut contains_meta = FnMeta::new("core", "contains?");
    contains_meta.arglist.push("[coll key-or-index]".into());
    contains_meta.doc = Some(
        "True when map/set contains the key; for vector/list it checks index existence. To test membership by value, use includes?."
            .into(),
    );
    contains_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    contains_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(contains_meta);

    let mut disj_meta = FnMeta::new("core", "disj");
    disj_meta.arglist.push("[set & values]".into());
    disj_meta.doc = Some("Return a set with the given values removed (nil stays nil).".into());
    disj_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    disj_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(disj_meta);

    let mut subvec_meta = FnMeta::new("core", "subvec");
    subvec_meta.arglist.push("[coll start]".into());
    subvec_meta.arglist.push("[coll start end]".into());
    subvec_meta.doc = Some(
        "Return a vector containing items from start (inclusive) up to end (exclusive).".into(),
    );
    subvec_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::vector(TypeKind::Any),
        special_op: None,
    });
    subvec_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Int, TypeKind::Int],
        rest: None,
        ret_type: TypeKind::vector(TypeKind::Any),
        special_op: None,
    });
    subvec_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(subvec_meta);

    let mut not_empty_meta = FnMeta::new("core", "not-empty");
    not_empty_meta.arglist.push("[coll]".into());
    not_empty_meta.doc = Some("Return coll when it has items; otherwise return nil.".into());
    not_empty_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    not_empty_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(not_empty_meta);

    let mut mut_meta = FnMeta::new("core", "mut");
    mut_meta.arglist.push("[x]".into());
    mut_meta.doc = Some(
        "Shallow-convert collections to mutable ones, leaving nested collections immutable.".into(),
    );
    mut_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    mut_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(mut_meta);

    let mut imut_meta = FnMeta::new("core", "imut");
    imut_meta.arglist.push("[x]".into());
    imut_meta.doc = Some(
        "Deep-convert collections to immutable ones, freezing any mutable collections.".into(),
    );
    imut_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::Any))],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    imut_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(imut_meta);

    let mut transient_meta = FnMeta::new("core", "transient");
    transient_meta.arglist.push("[coll]".into());
    transient_meta.doc = Some("Create a transient version of a vector, map, or set.".into());
    transient_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    transient_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(transient_meta);

    let mut persistent_meta = FnMeta::new("core", "persistent!");
    persistent_meta.arglist.push("[tcoll]".into());
    persistent_meta.doc =
        Some("Return a persistent collection and invalidate the transient.".into());
    persistent_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    persistent_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(persistent_meta);

    let mut conj_bang_meta = FnMeta::new("core", "conj!");
    conj_bang_meta.arglist.push("[tcoll & xs]".into());
    conj_bang_meta.doc =
        Some("Mutate a mutable or transient collection by conjoining items.".into());
    conj_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::Any))],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    conj_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(conj_bang_meta);

    let mut assoc_bang_meta = FnMeta::new("core", "assoc!");
    assoc_bang_meta.arglist.push("[tcoll k v & kvs]".into());
    assoc_bang_meta.doc =
        Some("Mutate a mutable or transient map/vector by associating entries.".into());
    assoc_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::Any))],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    assoc_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(assoc_bang_meta);

    let mut update_bang_meta = FnMeta::new("core", "update!");
    update_bang_meta.arglist.push("[tcoll k f & args]".into());
    update_bang_meta.doc =
        Some("Mutate a mutable or transient map/vector by updating a value.".into());
    update_bang_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Mut(Box::new(TypeKind::Any)),
            TypeKind::Any,
            TypeKind::Any,
        ],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    update_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(update_bang_meta);

    let mut update_in_bang_meta = FnMeta::new("core", "update-in!");
    update_in_bang_meta.arglist.push("[m path f & args]".into());
    update_in_bang_meta.doc =
        Some("Mutate a mutable map/vector by updating a nested value.".into());
    update_in_bang_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Mut(Box::new(TypeKind::Any)),
            TypeKind::Any,
            TypeKind::Any,
        ],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    update_in_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(update_in_bang_meta);

    let mut assoc_in_bang_meta = FnMeta::new("core", "assoc-in!");
    assoc_in_bang_meta.arglist.push("[m path v]".into());
    assoc_in_bang_meta.doc =
        Some("Mutate a mutable map/vector by associating a nested value.".into());
    assoc_in_bang_meta.overloads.push(FnOverload {
        arg_types: vec![
            TypeKind::Mut(Box::new(TypeKind::Any)),
            TypeKind::Any,
            TypeKind::Any,
        ],
        rest: None,
        ret_type: TypeKind::Mut(Box::new(TypeKind::Any)),
        special_op: None,
    });
    assoc_in_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(assoc_in_bang_meta);

    let mut merge_bang_meta = FnMeta::new("core", "merge!");
    merge_bang_meta.arglist.push("[m & maps]".into());
    merge_bang_meta.doc = Some("Mutate a mutable map by merging in entries.".into());
    merge_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::map(
            TypeKind::Any,
            TypeKind::Any,
        )))],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::map(TypeKind::Any, TypeKind::Any))),
        special_op: None,
    });
    merge_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(merge_bang_meta);

    let mut dissoc_bang_meta = FnMeta::new("core", "dissoc!");
    dissoc_bang_meta.arglist.push("[tmap & keys]".into());
    dissoc_bang_meta.doc = Some("Mutate a mutable or transient map by removing keys.".into());
    dissoc_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::map(
            TypeKind::Any,
            TypeKind::Any,
        )))],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::map(TypeKind::Any, TypeKind::Any))),
        special_op: None,
    });
    dissoc_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(dissoc_bang_meta);

    let mut disj_bang_meta = FnMeta::new("core", "disj!");
    disj_bang_meta.arglist.push("[tset & values]".into());
    disj_bang_meta.doc = Some("Mutate a mutable or transient set by removing values.".into());
    disj_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::set(TypeKind::Any)))],
        rest: Some(TypeKind::Any),
        ret_type: TypeKind::Mut(Box::new(TypeKind::set(TypeKind::Any))),
        special_op: None,
    });
    disj_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(disj_bang_meta);

    let mut pop_bang_meta = FnMeta::new("core", "pop!");
    pop_bang_meta.arglist.push("[tvec]".into());
    pop_bang_meta.doc = Some("Remove the last item from a mutable or transient vector.".into());
    pop_bang_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Mut(Box::new(TypeKind::vector(TypeKind::Any)))],
        rest: None,
        ret_type: TypeKind::Mut(Box::new(TypeKind::vector(TypeKind::Any))),
        special_op: None,
    });
    pop_bang_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(pop_bang_meta);
}

fn contains_index_type_error(actual: &Value) -> CloveError {
    CloveError::type_mismatch(
        "index (Int) for contains? on vector/list; contains? checks index existence. To test membership by value, use includes? (or some).",
        actual_type_with_preview(actual),
    )
}

fn deep_imut_vector(items: &Vector<Value>) -> Result<Vector<Value>, CloveError> {
    if !items.iter().any(contains_mut_collection) {
        return Ok(items.clone());
    }
    let mut out = Vector::new();
    for item in items.iter() {
        out.push_back(to_imut_deep(item)?);
    }
    Ok(out)
}

fn deep_imut_set(items: &HashSet<Value>) -> Result<HashSet<Value>, CloveError> {
    if !items.iter().any(contains_mut_collection) {
        return Ok(items.clone());
    }
    let mut out = HashSet::new();
    for item in items.iter() {
        if is_mut_collection(item) {
            return err("cannot put mutable collection into set; use (imut x)");
        }
        out.insert(to_imut_deep(item)?);
    }
    Ok(out)
}

fn deep_imut_sorted_set(items: &SortedSet) -> Result<SortedSet, CloveError> {
    if !items.entries.iter().any(contains_mut_collection) {
        return Ok(items.clone());
    }
    let mut out = SortedSet {
        comparator: items.comparator.clone(),
        entries: Vec::new(),
    };
    for item in items.entries.iter() {
        if is_mut_collection(item) {
            return err("cannot put mutable collection into set; use (imut x)");
        }
        sorted_set_insert_mut(&mut out, to_imut_deep(item)?)?;
    }
    Ok(out)
}

fn deep_imut_sorted_map(items: &SortedMap) -> Result<SortedMap, CloveError> {
    if !items
        .entries
        .iter()
        .any(|(_, v)| contains_mut_collection(v))
    {
        return Ok(items.clone());
    }
    let mut out = SortedMap {
        comparator: items.comparator.clone(),
        entries: Vec::new(),
    };
    for (k, v) in items.entries.iter() {
        sorted_map_insert_mut(&mut out, k.clone(), to_imut_deep(v)?)?;
    }
    Ok(out)
}

fn assoc_indexed(
    items: &Vector<Value>,
    pairs: &[Value],
    as_list: bool,
) -> Result<Value, CloveError> {
    if pairs.len() % 2 != 0 {
        return err("assoc expects even number of key/value arguments");
    }
    let mut out = items.clone();
    let mut iter = pairs.iter();
    while let (Some(idx_val), Some(value)) = (iter.next(), iter.next()) {
        let (n, _) = as_number(idx_val)?;
        let idx = num_to_isize(n)?;
        if idx < 0 {
            return err("assoc index must be non-negative");
        }
        let idx = idx as usize;
        if idx > out.len() {
            return err("assoc index out of bounds");
        }
        if idx == out.len() {
            out.push_back(to_imut_deep(value)?);
        } else {
            out[idx] = to_imut_deep(value)?;
        }
    }
    Ok(if as_list {
        Value::List(out)
    } else {
        Value::Vector(out)
    })
}

fn assoc_indexed_mut(items: &mut Vector<Value>, pairs: &[Value]) -> Result<(), CloveError> {
    if pairs.len() % 2 != 0 {
        return err("assoc! expects even number of key/value arguments");
    }
    let mut iter = pairs.iter();
    while let (Some(idx_val), Some(value)) = (iter.next(), iter.next()) {
        let (n, _) = as_number(idx_val)?;
        let idx = num_to_isize(n)?;
        if idx < 0 {
            return err("assoc index must be non-negative");
        }
        let idx = idx as usize;
        if idx > items.len() {
            return err("assoc index out of bounds");
        }
        if idx == items.len() {
            items.push_back(value.clone());
        } else {
            items[idx] = value.clone();
        }
    }
    Ok(())
}

fn assoc_indexed_mut_shallow(items: &mut Vector<Value>, pairs: &[Value]) -> Result<(), CloveError> {
    if pairs.len() % 2 != 0 {
        return err("assoc! expects even number of key/value arguments");
    }
    let mut iter = pairs.iter();
    while let (Some(idx_val), Some(value)) = (iter.next(), iter.next()) {
        let (n, _) = as_number(idx_val)?;
        let idx = num_to_isize(n)?;
        if idx < 0 {
            return err("assoc index must be non-negative");
        }
        let idx = idx as usize;
        if idx > items.len() {
            return err("assoc index out of bounds");
        }
        let converted = to_mut_shallow(value)?;
        if idx == items.len() {
            items.push_back(converted);
        } else {
            items[idx] = converted;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::ast::Value;
    use crate::ast::Vector;
    use crate::builtins::default_env;
    use crate::env::EnvRef;
    use crate::eval::call_callable;
    use crate::seq::SeqHandle;

    fn call(env: &EnvRef, name: &str, args: Vec<Value>) -> Value {
        let func = env
            .read()
            .unwrap()
            .get(name)
            .unwrap_or_else(|| panic!("missing builtin {}", name));
        call_callable(func, args).expect("builtin call failed")
    }

    fn call_err(env: &EnvRef, name: &str, args: Vec<Value>) -> String {
        let func = env
            .read()
            .unwrap()
            .get(name)
            .unwrap_or_else(|| panic!("missing builtin {}", name));
        call_callable(func, args)
            .expect_err("builtin call unexpectedly succeeded")
            .to_string()
    }

    #[test]
    fn range_generates_expected_sequence() {
        let env = default_env();
        let res = call(&env, "range", vec![Value::Int(0), Value::Int(3)]);
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![
                Value::Int(0),
                Value::Int(1),
                Value::Int(2)
            ]))
        );
        let res_desc = call(
            &env,
            "range",
            vec![Value::Int(3), Value::Int(0), Value::Int(-1)],
        );
        assert_eq!(
            res_desc,
            Value::Vector(Vector::from(vec![
                Value::Int(3),
                Value::Int(2),
                Value::Int(1)
            ]))
        );
    }

    #[test]
    fn nth_returns_default_when_oob() {
        let env = default_env();
        let res = call(
            &env,
            "nth",
            vec![
                Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)])),
                Value::Int(5),
                Value::Int(99),
            ],
        );
        assert_eq!(res, Value::Int(99));
    }

    #[test]
    fn repeat_returns_value_multiple_times() {
        let env = default_env();
        let repeated = call(&env, "repeat", vec![Value::Int(3), Value::Int(7)]);
        assert_eq!(
            repeated,
            Value::Vector(Vector::from(vec![
                Value::Int(7),
                Value::Int(7),
                Value::Int(7)
            ]))
        );
        let empty = call(
            &env,
            "repeat",
            vec![Value::Int(-5), Value::String("x".into())],
        );
        assert_eq!(empty, Value::Vector(Vector::new()));
    }

    #[test]
    fn subvec_slices_vector_range() {
        let env = default_env();
        let res = call(
            &env,
            "subvec",
            vec![
                Value::Vector(Vector::from(vec![
                    Value::Int(0),
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3),
                ])),
                Value::Int(1),
                Value::Int(3),
            ],
        );
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)]))
        );
    }

    #[test]
    fn subvec_defaults_end_and_works_on_seq() {
        let env = default_env();
        let seq = Value::Seq(SeqHandle::from_iter(
            vec![Value::String("a".into()), Value::String("b".into())].into_iter(),
        ));
        let res = call(&env, "subvec", vec![seq, Value::Int(1)]);
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![Value::String("b".into())]))
        );
    }

    #[test]
    fn count_reports_type_mismatch_with_preview_and_arg() {
        let env = default_env();
        let err = call_err(&env, "count", vec![Value::Int(10)]);
        assert!(
            err.contains("expected collection (arg 1 to count)"),
            "unexpected message: {}",
            err
        );
        assert!(err.contains("number (10)"), "missing preview: {}", err);
    }
}
