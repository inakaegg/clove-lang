use crate::ast::{FnArity, Value};
use crate::builtins::{def_builtin, err, truthy};
use crate::env::Env;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::types::TypeKind;
use std::sync::Arc;

pub(crate) fn install(env: &mut Env) {
    register_predicate_metas();
    // --- Predicates ---
    def_builtin!(env, "not", FnArity::exact(1), |args| {
        match args {
            [v] => Ok(Value::Bool(!truthy(v))),
            _ => err("not expects one argument"),
        }
    });
    def_builtin!(env, "boolean", FnArity::exact(1), |args| {
        match args {
            [v] => Ok(Value::Bool(truthy(v))),
            _ => err("boolean expects one argument"),
        }
    });
    def_builtin!(env, "number?", FnArity::exact(1), |args| {
        match args {
            [Value::Int(_)] | [Value::Float(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("number? expects one argument"),
        }
    });
    def_builtin!(env, "integer?", FnArity::exact(1), |args| {
        match args {
            [Value::Int(_)] => Ok(Value::Bool(true)),
            [Value::Float(v)] => Ok(Value::Bool(v.fract() == 0.0)),
            [_] => Ok(Value::Bool(false)),
            _ => err("integer? expects one argument"),
        }
    });
    def_builtin!(env, "float?", FnArity::exact(1), |args| {
        match args {
            [Value::Float(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("float? expects one argument"),
        }
    });
    def_builtin!(env, "string?", FnArity::exact(1), |args| {
        match args {
            [Value::String(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("string? one arg"),
        }
    });
    def_builtin!(env, "boolean?", FnArity::exact(1), |args| {
        match args {
            [Value::Bool(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("boolean? one arg"),
        }
    });
    def_builtin!(env, "map?", FnArity::exact(1), |args| {
        match args {
            [Value::Map(_)] | [Value::SortedMap(_)] | [Value::MutMap(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("map? one arg"),
        }
    });
    def_builtin!(env, "coll?", FnArity::exact(1), |args| {
        match args {
            [Value::List(_)]
            | [Value::Vector(_)]
            | [Value::Set(_)]
            | [Value::SortedSet(_)]
            | [Value::Map(_)]
            | [Value::SortedMap(_)]
            | [Value::Seq(_)]
            | [Value::MutVector(_)]
            | [Value::MutSet(_)]
            | [Value::MutMap(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("coll? one arg"),
        }
    });
    def_builtin!(env, "vector?", FnArity::exact(1), |args| {
        match args {
            [Value::Vector(_)] | [Value::MutVector(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("vector? one arg"),
        }
    });
    def_builtin!(env, "list?", FnArity::exact(1), |args| {
        match args {
            [Value::List(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("list? one arg"),
        }
    });
    def_builtin!(env, "set?", FnArity::exact(1), |args| {
        match args {
            [Value::Set(_)] | [Value::SortedSet(_)] | [Value::MutSet(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("set? one arg"),
        }
    });
    def_builtin!(env, "sorted?", FnArity::exact(1), |args| {
        match args {
            [Value::SortedMap(_)] | [Value::SortedSet(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("sorted? one arg"),
        }
    });
    def_builtin!(env, "sequential?", FnArity::exact(1), |args| {
        match args {
            [Value::List(_)] | [Value::Vector(_)] | [Value::MutVector(_)] | [Value::Seq(_)] => {
                Ok(Value::Bool(true))
            }
            [_] => Ok(Value::Bool(false)),
            _ => err("sequential? expects one argument"),
        }
    });
    def_builtin!(env, "seq?", FnArity::exact(1), |args| {
        match args {
            [Value::Seq(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("seq? expects one argument"),
        }
    });
    def_builtin!(env, "mut?", FnArity::exact(1), |args| {
        match args {
            [Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("mut? expects one argument"),
        }
    });
    def_builtin!(env, "imut?", FnArity::exact(1), |args| {
        match args {
            [Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)] => Ok(Value::Bool(false)),
            [_] => Ok(Value::Bool(true)),
            _ => err("imut? expects one argument"),
        }
    });
    def_builtin!(env, "identical?", FnArity::exact(2), |args| match args {
        [a, b] => Ok(Value::Bool(value_identical(a, b))),
        _ => err("identical? expects two arguments"),
    });
    def_builtin!(env, "fn?", FnArity::exact(1), |args| {
        match args {
            [Value::Func(_)]
            | [Value::Partial { .. }]
            | [Value::Compose { .. }]
            | [Value::Lambda { .. }]
            | [Value::MultiLambda { .. }]
            | [Value::ForeignCallable { .. }] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("fn? expects one argument"),
        }
    });
    def_builtin!(env, "keyword?", FnArity::exact(1), |args| {
        match args {
            [Value::Symbol(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("keyword? expects one argument"),
        }
    });
    def_builtin!(env, "symbol?", FnArity::exact(1), |args| {
        match args {
            [Value::Symbol(_)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("symbol? expects one argument"),
        }
    });
    def_builtin!(env, "some?", FnArity::exact(1), |args| {
        match args {
            [v] => Ok(Value::Bool(!matches!(v, Value::Nil | Value::Bool(false)))),
            _ => err("some? expects one argument"),
        }
    });
    def_builtin!(env, "nil?", FnArity::exact(1), |args| {
        match args {
            [Value::Nil] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("nil? expects one argument"),
        }
    });
    def_builtin!(env, "true?", FnArity::exact(1), |args| {
        match args {
            [Value::Bool(true)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("true? expects one argument"),
        }
    });
    def_builtin!(env, "false?", FnArity::exact(1), |args| {
        match args {
            [Value::Bool(false)] => Ok(Value::Bool(true)),
            [_] => Ok(Value::Bool(false)),
            _ => err("false? expects one argument"),
        }
    });
}

fn register_predicate_metas() {
    fn bool_type() -> TypeKind {
        TypeKind::Bool
    }

    fn add_simple_pred(name: &str, doc: &str) {
        let mut meta = FnMeta::new("core", name);
        meta.arglist.push("[value]".into());
        meta.doc = Some(doc.into());
        meta.overloads.push(FnOverload {
            arg_types: vec![TypeKind::Any],
            rest: None,
            ret_type: bool_type(),
            special_op: None,
        });
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }

    add_simple_pred("not", "Logical negation of truthiness.");
    add_simple_pred("boolean", "Return true when value is truthy.");
    add_simple_pred("number?", "Return true when value is integer or float.");
    add_simple_pred("integer?", "Return true when value is an integer.");
    add_simple_pred("float?", "Return true when value is a float.");
    add_simple_pred("string?", "Return true when value is a string.");
    add_simple_pred("boolean?", "Return true when value is a boolean.");
    add_simple_pred("map?", "Return true when value is a map.");
    add_simple_pred("coll?", "Return true when value is a collection.");
    add_simple_pred("vector?", "Return true when value is a vector.");
    add_simple_pred("list?", "Return true when value is a list.");
    add_simple_pred("set?", "Return true when value is a set.");
    add_simple_pred("mut?", "Return true when value is a mutable collection.");
    add_simple_pred(
        "imut?",
        "Return true when value is not a mutable collection.",
    );
    add_simple_pred("sorted?", "Return true when value is a sorted map or set.");
    add_simple_pred("sequential?", "Return true when value is sequential.");
    add_simple_pred("seq?", "Return true when value is a seq.");
    add_simple_pred("fn?", "Return true when value is a callable function.");
    add_simple_pred("keyword?", "Return true when value is a keyword symbol.");
    add_simple_pred("symbol?", "Return true when value is a symbol.");
    add_simple_pred("some?", "Return true when value is neither nil nor false.");
    add_simple_pred("nil?", "Return true when value is nil.");
    add_simple_pred("true?", "Return true when value is true.");
    add_simple_pred("false?", "Return true when value is false.");

    let mut identical_meta = FnMeta::new("core", "identical?");
    identical_meta.arglist.push("[a b]".into());
    identical_meta.doc = Some("Return true when references or primitives are identical.".into());
    identical_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: bool_type(),
        special_op: None,
    });
    identical_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(identical_meta);
}

fn value_identical(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Seq(sa), Value::Seq(sb)) => sa.ptr_eq(sb),
        (Value::Func(fa), Value::Func(fb)) => Arc::ptr_eq(fa, fb),
        _ => a == b,
    }
}
