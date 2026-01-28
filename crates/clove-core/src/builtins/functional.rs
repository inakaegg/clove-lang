use crate::ast::Vector;
use crate::ast::{ComposeKind, FnArity, Value};
use crate::builtins::{as_number, def_builtin, err, seq_handle_from_value, seq_items, truthy};
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::call_callable;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::seq::{SeqEngine, SeqHandle};
use crate::types::TypeKind;

pub(crate) fn install(env: &mut Env) {
    register_functional_metas();
    // --- Functional Ops ---
    def_builtin!(env, "apply", FnArity::at_least(2), |args| {
        if args.len() < 2 {
            return err("apply expects at least a function and a collection");
        }
        let callable = args[0].clone();
        let mut flat_args = args[1..args.len() - 1].to_vec();
        match &args[args.len() - 1] {
            Value::List(v) | Value::Vector(v) => {
                flat_args.extend(v.iter().cloned());
            }
            Value::Seq(handle) => {
                flat_args.extend(handle.collect_all()?);
            }
            Value::Nil => {}
            _ => return err("apply expects last argument to be list, vector, seq, or nil"),
        }
        call_callable(callable, flat_args)
    });
    def_builtin!(env, "trampoline", FnArity::at_least(1), |args| {
        if args.is_empty() {
            return err("trampoline expects a function and optional arguments");
        }
        let mut result = call_callable(args[0].clone(), args[1..].to_vec())?;
        while is_callable(&result) {
            result = call_callable(result, vec![])?;
        }
        Ok(result)
    });
    def_builtin!(env, "filter", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(FilterSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        invert: false,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for item in seq {
                    let keep = call_callable(pred.clone(), vec![item.clone()])?;
                    if truthy(&keep) {
                        out.push_back(item);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("filter expects predicate and collection"),
        }
    });
    def_builtin!(env, "remove", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(FilterSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        invert: true,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for item in seq {
                    let keep = call_callable(pred.clone(), vec![item.clone()])?;
                    if !truthy(&keep) {
                        out.push_back(item);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("remove expects predicate and collection"),
        }
    });
    def_builtin!(env, "keep", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(KeepSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        indexed: false,
                        index: 0,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for item in seq {
                    let res = call_callable(pred.clone(), vec![item.clone()])?;
                    if !matches!(res, Value::Nil) {
                        out.push_back(res);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("keep expects predicate and collection"),
        }
    });
    def_builtin!(env, "keep-indexed", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(KeepSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        indexed: true,
                        index: 0,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for (idx, item) in seq.into_iter().enumerate() {
                    let res =
                        call_callable(pred.clone(), vec![Value::Int(idx as i64), item.clone()])?;
                    if !matches!(res, Value::Nil) {
                        out.push_back(res);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("keep-indexed expects predicate and collection"),
        }
    });
    def_builtin!(env, "take", FnArity::exact(2), |args| {
        match args {
            [n, coll @ Value::Seq(_)] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                if n == 0 {
                    return Ok(Value::Vector(Vector::new()));
                }
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(TakeSeqEngine {
                        source: handle,
                        remaining: n,
                    }))
                } else {
                    Ok(Value::Vector(Vector::new()))
                }
            }
            [n, coll] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                let seq = seq_items(coll)?;
                Ok(Value::Vector(seq.into_iter().take(n).collect()))
            }
            _ => err("take expects number and collection"),
        }
    });
    def_builtin!(env, "take-last", FnArity::exact(2), |args| {
        match args {
            [n, coll] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                if n == 0 {
                    return Ok(Value::Vector(Vector::new()));
                }
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    let items = handle.collect_all()?;
                    let len = items.len();
                    if n >= len {
                        Ok(Value::Vector(items))
                    } else {
                        let start = len - n;
                        Ok(Value::Vector(items.into_iter().skip(start).collect()))
                    }
                } else {
                    let items = seq_items(coll)?;
                    let len = items.len();
                    if n >= len {
                        Ok(Value::Vector(items))
                    } else {
                        let start = len - n;
                        Ok(Value::Vector(items.into_iter().skip(start).collect()))
                    }
                }
            }
            _ => err("take-last expects number and collection"),
        }
    });
    def_builtin!(env, "drop-last", FnArity::range(1, 2), |args| {
        let (n, coll) = match args {
            [coll] => (1, coll),
            [n, coll] => {
                let (n, _) = as_number(n)?;
                (n.max(0.0) as usize, coll)
            }
            _ => return err("drop-last expects (coll) or (n coll)"),
        };
        if n == 0 {
            let seq = seq_items(coll)?;
            return Ok(Value::Vector(seq));
        }
        let items = seq_items(coll)?;
        let len = items.len();
        if n >= len {
            Ok(Value::Vector(Vector::new()))
        } else {
            Ok(Value::Vector(items.into_iter().take(len - n).collect()))
        }
    });
    def_builtin!(env, "drop", FnArity::exact(2), |args| {
        match args {
            [n, coll @ Value::Seq(_)] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(DropSeqEngine {
                        source: handle,
                        remaining: n,
                        skipped: false,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [n, coll] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                let seq = seq_items(coll)?;
                Ok(Value::Vector(seq.into_iter().skip(n).collect()))
            }
            _ => err("drop expects number and collection"),
        }
    });
    def_builtin!(env, "take-nth", FnArity::exact(2), |args| {
        match args {
            [n, coll @ Value::Seq(_)] => {
                let (n, _) = as_number(n)?;
                if n <= 0.0 {
                    return err("take-nth expects positive step");
                }
                let step = n as usize;
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(TakeNthSeqEngine {
                        source: handle,
                        step,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [n, coll] => {
                let (n, _) = as_number(n)?;
                if n <= 0.0 {
                    return err("take-nth expects positive step");
                }
                let step = n as usize;
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for (idx, item) in seq.into_iter().enumerate() {
                    if idx % step == 0 {
                        out.push_back(item);
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("take-nth expects number and collection"),
        }
    });
    def_builtin!(env, "take-while", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(TakeWhileSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        done: false,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut out = Vector::new();
                for item in seq {
                    let keep = call_callable(pred.clone(), vec![item.clone()])?;
                    if truthy(&keep) {
                        out.push_back(item);
                    } else {
                        break;
                    }
                }
                Ok(Value::Vector(out))
            }
            _ => err("take-while expects predicate and collection"),
        }
    });
    def_builtin!(env, "drop-while", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    Ok(seq_from_engine(DropWhileSeqEngine {
                        pred: pred.clone(),
                        source: handle,
                        skipping: true,
                    }))
                } else {
                    Ok(empty_seq_value())
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut skipping = true;
                let mut out = Vector::new();
                for item in seq {
                    if skipping {
                        let keep = call_callable(pred.clone(), vec![item.clone()])?;
                        skipping = truthy(&keep);
                        if skipping {
                            continue;
                        }
                    }
                    out.push_back(item);
                }
                Ok(Value::Vector(out))
            }
            _ => err("drop-while expects predicate and collection"),
        }
    });
    def_builtin!(env, "split-at", FnArity::exact(2), |args| {
        match args {
            [n, coll @ Value::Seq(_)] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    let mut first = Vector::new();
                    for _ in 0..n {
                        match handle.next()? {
                            Some(v) => first.push_back(v),
                            None => break,
                        }
                    }
                    Ok(Value::Vector(Vector::from(vec![
                        Value::Vector(first),
                        Value::Seq(handle),
                    ])))
                } else {
                    Ok(Value::Vector(Vector::from(vec![
                        Value::Vector(Vector::new()),
                        Value::Vector(Vector::new()),
                    ])))
                }
            }
            [n, coll] => {
                let (n, _) = as_number(n)?;
                let n = n.max(0.0) as usize;
                let seq = seq_items(coll)?;
                let first: Vector<Value> = seq.iter().cloned().take(n).collect();
                let second: Vector<Value> = seq.into_iter().skip(n).collect();
                Ok(Value::Vector(Vector::from(vec![
                    Value::Vector(first),
                    Value::Vector(second),
                ])))
            }
            _ => err("split-at expects number and collection"),
        }
    });
    def_builtin!(env, "split-with", FnArity::exact(2), |args| {
        match args {
            [pred, coll @ Value::Seq(_)] => {
                if let Some(handle) = seq_handle_from_value(coll.clone())? {
                    let mut prefix = Vector::new();
                    let mut tail_first = None;
                    loop {
                        let Some(item) = handle.next()? else {
                            break;
                        };
                        let keep = call_callable(pred.clone(), vec![item.clone()])?;
                        if truthy(&keep) {
                            prefix.push_back(item);
                        } else {
                            tail_first = Some(item);
                            break;
                        }
                    }
                    let tail = match tail_first {
                        Some(first) => Value::Seq(SeqHandle::new(Box::new(SplitWithTailSeq {
                            first: Some(first),
                            source: handle,
                        }))),
                        None => empty_seq_value(),
                    };
                    Ok(Value::Vector(Vector::from(vec![
                        Value::Vector(prefix),
                        tail,
                    ])))
                } else {
                    Ok(Value::Vector(Vector::from(vec![
                        Value::Vector(Vector::new()),
                        Value::Vector(Vector::new()),
                    ])))
                }
            }
            [pred, coll] => {
                let seq = seq_items(coll)?;
                let mut prefix = Vector::new();
                let mut rest = Vector::new();
                let mut in_prefix = true;
                for item in seq {
                    if in_prefix {
                        let keep = call_callable(pred.clone(), vec![item.clone()])?;
                        if truthy(&keep) {
                            prefix.push_back(item);
                        } else {
                            in_prefix = false;
                            rest.push_back(item);
                        }
                    } else {
                        rest.push_back(item);
                    }
                }
                Ok(Value::Vector(Vector::from(vec![
                    Value::Vector(prefix),
                    Value::Vector(rest),
                ])))
            }
            _ => err("split-with expects predicate and collection"),
        }
    });
    def_builtin!(env, "dorun", FnArity::range(1, 2), |args| {
        match args {
            [coll] => {
                let seq = seq_items(coll)?;
                for _ in seq {}
                Ok(Value::Nil)
            }
            [Value::Int(n), coll] => {
                let count = (*n).max(0) as usize;
                let seq = seq_items(coll)?;
                for (_idx, _item) in seq.into_iter().enumerate().take(count) {}
                Ok(Value::Nil)
            }
            [other, _] => Err(CloveError::type_mismatch("int", other.type_name())),
            _ => err("dorun expects (coll) or (n coll)"),
        }
    });

    def_builtin!(env, "identity", FnArity::exact(1), |args| {
        match args {
            [v] => Ok(v.clone()),
            _ => err("identity expects one argument"),
        }
    });
    def_builtin!(env, "constantly", FnArity::exact(1), |args| {
        match args {
            [v] => Ok(Value::native_fn(FnArity::at_least(0), {
                let captured = v.clone();
                move |_| Ok(captured.clone())
            })),
            _ => err("constantly expects one argument"),
        }
    });
    def_builtin!(env, "partial", FnArity::at_least(1), |args| {
        if args.is_empty() {
            return err("partial expects at least a function");
        }
        let f = args[0].clone();
        let captured = args[1..].to_vec();
        Ok(Value::Partial {
            callable: Box::new(f),
            captured,
            remaining: FnArity::at_least(0),
        })
    });
    def_builtin!(env, "comp", FnArity::at_least(1), |args| {
        if args.is_empty() {
            return err("comp expects at least one function");
        }
        Ok(Value::Compose {
            funcs: args.to_vec(),
            kind: ComposeKind::Comp,
        })
    });
    def_builtin!(env, "pipe", FnArity::at_least(1), |args| {
        if args.is_empty() {
            return err("pipe expects at least one function");
        }
        Ok(Value::Compose {
            funcs: args.to_vec(),
            kind: ComposeKind::Pipe,
        })
    });
    def_builtin!(env, "fnil", FnArity::at_least(2), |args| {
        if args.len() < 2 {
            return err("fnil expects a function and at least one default value");
        }
        if args.len() > 4 {
            return err("fnil supports up to 3 default values");
        }
        let f = args[0].clone();
        let defaults = args[1..].to_vec();
        Ok(Value::native_fn(FnArity::at_least(0), move |call_args| {
            let mut patched = Vec::with_capacity(call_args.len());
            for (idx, arg) in call_args.iter().enumerate() {
                if idx < defaults.len() && matches!(arg, Value::Nil) {
                    patched.push(defaults[idx].clone());
                } else {
                    patched.push(arg.clone());
                }
            }
            call_callable(f.clone(), patched)
        }))
    });
    def_builtin!(env, "some-fn", FnArity::at_least(1), |args| {
        let funcs = args.to_vec();
        Ok(Value::native_fn(FnArity::at_least(0), move |call_args| {
            for f in &funcs {
                let res = call_callable(f.clone(), call_args.to_vec())?;
                if truthy(&res) {
                    return Ok(res);
                }
            }
            Ok(Value::Nil)
        }))
    });
    def_builtin!(env, "not-every?", FnArity::exact(2), |args| match args {
        [pred, coll] => {
            if let Some(handle) = seq_handle_from_value(coll.clone())? {
                while let Some(item) = handle.next()? {
                    let keep = call_callable(pred.clone(), vec![item.clone()])?;
                    if !truthy(&keep) {
                        return Ok(Value::Bool(true));
                    }
                }
                Ok(Value::Bool(false))
            } else {
                Ok(Value::Bool(false))
            }
        }
        _ => err("not-every? expects predicate and collection"),
    });
}

fn seq_from_engine<E: SeqEngine + 'static>(engine: E) -> Value {
    Value::Seq(SeqHandle::new(Box::new(engine)))
}

fn empty_seq_value() -> Value {
    Value::Seq(SeqHandle::from_iter(std::iter::empty::<Value>()))
}

#[allow(clippy::too_many_lines)]
fn register_functional_metas() {
    fn function_type() -> TypeKind {
        TypeKind::Named("clove::core::Function".into())
    }
    fn seq_like() -> TypeKind {
        TypeKind::Named("clove::core::Seq".into())
    }
    fn vec_any() -> TypeKind {
        TypeKind::vector(TypeKind::Any)
    }
    fn bool_type() -> TypeKind {
        TypeKind::Bool
    }

    macro_rules! simple_meta {
        ($name:expr, $arglist:expr, $doc:expr, $args:expr, $ret:expr) => {{
            let mut meta = FnMeta::new("core", $name);
            meta.arglist.push($arglist.into());
            meta.doc = Some($doc.into());
            meta.overloads.push(FnOverload {
                arg_types: $args,
                rest: None,
                ret_type: $ret,
                special_op: None,
            });
            fn_meta::register(meta);
        }};
        ($name:expr, $arglist:expr, $doc:expr, $args:expr, $ret:expr, $subject:expr) => {{
            let mut meta = FnMeta::new("core", $name);
            meta.arglist.push($arglist.into());
            meta.doc = Some($doc.into());
            meta.overloads.push(FnOverload {
                arg_types: $args,
                rest: None,
                ret_type: $ret,
                special_op: None,
            });
            meta.subject_pos = Some($subject);
            fn_meta::register(meta);
        }};
    }

    let mut apply_meta = FnMeta::new("core", "apply");
    apply_meta.arglist.push("[f & args]".into());
    apply_meta.doc = Some("Call f with preceding args plus items of final collection.".into());
    apply_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: Some(vec_any()),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    apply_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(apply_meta);

    let mut trampoline_meta = FnMeta::new("core", "trampoline");
    trampoline_meta.arglist.push("[f & args]".into());
    trampoline_meta.doc = Some(
        "Call f with args; if the result is callable, invoke it repeatedly until a non-function value is produced."
            .into(),
    );
    trampoline_meta.overloads.push(FnOverload {
        arg_types: vec![function_type()],
        rest: Some(vec_any()),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    trampoline_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(trampoline_meta);
    simple_meta!(
        "filter",
        "[pred coll]",
        "Return items where pred returns truthy.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "remove",
        "[pred coll]",
        "Return items where pred is falsey.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "keep",
        "[pred coll]",
        "Apply pred and keep non-nil results.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "keep-indexed",
        "[pred coll]",
        "Like keep but passes index and value.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "take",
        "[n coll]",
        "Take first n items from coll.",
        vec![TypeKind::Int, TypeKind::Any],
        vec_any(),
        SubjectPos::Last
    );
    simple_meta!(
        "take-last",
        "[n coll]",
        "Take last n items from coll (realizes the input).",
        vec![TypeKind::Int, TypeKind::Any],
        vec_any(),
        SubjectPos::Last
    );
    let mut drop_last_meta = FnMeta::new("core", "drop-last");
    drop_last_meta.arglist.push("[coll]".into());
    drop_last_meta.arglist.push("[n coll]".into());
    drop_last_meta.doc = Some("Drop the last n items from coll (realizes the input).".into());
    drop_last_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: vec_any(),
        special_op: None,
    });
    drop_last_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Any],
        rest: None,
        ret_type: vec_any(),
        special_op: None,
    });
    drop_last_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(drop_last_meta);
    simple_meta!(
        "drop",
        "[n coll]",
        "Drop first n items from coll.",
        vec![TypeKind::Int, TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "take-nth",
        "[n coll]",
        "Return every nth item from coll.",
        vec![TypeKind::Int, TypeKind::Any],
        vec_any(),
        SubjectPos::Last
    );
    simple_meta!(
        "take-while",
        "[pred coll]",
        "Take items while pred returns truthy.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "drop-while",
        "[pred coll]",
        "Drop items while pred returns truthy.",
        vec![function_type(), TypeKind::Any],
        seq_like(),
        SubjectPos::Last
    );
    simple_meta!(
        "split-at",
        "[n coll]",
        "Split coll at n, returning [prefix rest].",
        vec![TypeKind::Int, TypeKind::Any],
        vec_any(),
        SubjectPos::Last
    );
    simple_meta!(
        "split-with",
        "[pred coll]",
        "Split coll at first element where pred is falsey.",
        vec![function_type(), TypeKind::Any],
        vec_any(),
        SubjectPos::Last
    );

    let mut dorun_meta = FnMeta::new("core", "dorun");
    dorun_meta.arglist.push("[coll]".into());
    dorun_meta.arglist.push("[n coll]".into());
    dorun_meta.doc = Some("Realize coll (optionally first n items) for side effects.".into());
    dorun_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    dorun_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Nil,
        special_op: None,
    });
    dorun_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(dorun_meta);

    simple_meta!(
        "identity",
        "[value]",
        "Return value unchanged.",
        vec![TypeKind::Any],
        TypeKind::Any,
        SubjectPos::Fixed(1)
    );

    let mut constantly_meta = FnMeta::new("core", "constantly");
    constantly_meta.arglist.push("[value]".into());
    constantly_meta.doc = Some("Return function that always returns value.".into());
    constantly_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: function_type(),
        special_op: None,
    });
    constantly_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(constantly_meta);

    for name in ["partial", "comp", "pipe", "fnil", "some-fn"] {
        let doc = match name {
            "partial" => "Capture leading args and return new function.",
            "comp" => "Compose functions right-to-left.",
            "pipe" => "Compose functions left-to-right.",
            "fnil" => "Return fn that replaces nil args with defaults.",
            "some-fn" => "Return fn that returns first truthy result.",
            _ => "",
        };
        let mut meta = FnMeta::new("core", name);
        meta.arglist.push("[f & more]".into());
        meta.doc = Some(doc.into());
        meta.overloads.push(FnOverload {
            arg_types: vec![function_type()],
            rest: Some(vec_any()),
            ret_type: function_type(),
            special_op: None,
        });
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }

    let mut not_every_meta = FnMeta::new("core", "not-every?");
    not_every_meta.arglist.push("[pred coll]".into());
    not_every_meta.doc = Some("Return true when any item fails pred.".into());
    not_every_meta.overloads.push(FnOverload {
        arg_types: vec![function_type(), TypeKind::Any],
        rest: None,
        ret_type: bool_type(),
        special_op: None,
    });
    not_every_meta.subject_pos = Some(SubjectPos::Last);
    fn_meta::register(not_every_meta);
}

fn is_callable(value: &Value) -> bool {
    matches!(
        value,
        Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. }
    )
}
struct FilterSeqEngine {
    pred: Value,
    source: SeqHandle,
    invert: bool,
}

impl SeqEngine for FilterSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        loop {
            let Some(v) = self.source.next()? else {
                return Ok(None);
            };
            let keep = truthy(&call_callable(self.pred.clone(), vec![v.clone()])?);
            if keep ^ self.invert {
                return Ok(Some(v));
            }
        }
    }
}

struct KeepSeqEngine {
    pred: Value,
    source: SeqHandle,
    indexed: bool,
    index: usize,
}

impl SeqEngine for KeepSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        loop {
            let Some(v) = self.source.next()? else {
                return Ok(None);
            };
            let res = if self.indexed {
                let idx = self.index;
                self.index += 1;
                call_callable(self.pred.clone(), vec![Value::Int(idx as i64), v.clone()])?
            } else {
                call_callable(self.pred.clone(), vec![v.clone()])?
            };
            if !matches!(res, Value::Nil) {
                return Ok(Some(res));
            }
        }
    }
}

struct TakeSeqEngine {
    source: SeqHandle,
    remaining: usize,
}

impl SeqEngine for TakeSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        if self.remaining == 0 {
            return Ok(None);
        }
        match self.source.next()? {
            Some(v) => {
                self.remaining -= 1;
                Ok(Some(v))
            }
            None => Ok(None),
        }
    }
}

struct DropSeqEngine {
    source: SeqHandle,
    remaining: usize,
    skipped: bool,
}

impl SeqEngine for DropSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        if !self.skipped {
            while self.remaining > 0 {
                if self.source.next()?.is_none() {
                    self.skipped = true;
                    return Ok(None);
                }
                self.remaining -= 1;
            }
            self.skipped = true;
        }
        self.source.next()
    }
}

struct TakeNthSeqEngine {
    source: SeqHandle,
    step: usize,
}

impl SeqEngine for TakeNthSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        let Some(v) = self.source.next()? else {
            return Ok(None);
        };
        for _ in 1..self.step {
            if self.source.next()?.is_none() {
                break;
            }
        }
        Ok(Some(v))
    }
}

struct TakeWhileSeqEngine {
    pred: Value,
    source: SeqHandle,
    done: bool,
}

impl SeqEngine for TakeWhileSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        if self.done {
            return Ok(None);
        }
        let Some(v) = self.source.next()? else {
            return Ok(None);
        };
        if truthy(&call_callable(self.pred.clone(), vec![v.clone()])?) {
            Ok(Some(v))
        } else {
            self.done = true;
            Ok(None)
        }
    }
}

struct DropWhileSeqEngine {
    pred: Value,
    source: SeqHandle,
    skipping: bool,
}

impl SeqEngine for DropWhileSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        if self.skipping {
            loop {
                let Some(v) = self.source.next()? else {
                    return Ok(None);
                };
                if truthy(&call_callable(self.pred.clone(), vec![v.clone()])?) {
                    continue;
                } else {
                    self.skipping = false;
                    return Ok(Some(v));
                }
            }
        }
        self.source.next()
    }
}

struct SplitWithTailSeq {
    first: Option<Value>,
    source: SeqHandle,
}

impl SeqEngine for SplitWithTailSeq {
    fn next(&mut self) -> Result<Option<Value>, crate::error::CloveError> {
        if let Some(first) = self.first.take() {
            return Ok(Some(first));
        }
        self.source.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Vector;
    use crate::builtins::default_env;
    use crate::env::EnvRef;
    use crate::error::CloveError;
    use crate::eval::call_callable;

    fn call(env: &EnvRef, name: &str, args: Vec<Value>) -> Value {
        let func = env
            .read()
            .unwrap()
            .get(name)
            .unwrap_or_else(|| panic!("missing builtin {}", name));
        call_callable(func, args).expect("builtin call failed")
    }

    #[test]
    fn apply_spreads_last_collection() {
        let env = default_env();
        let res = call(
            &env,
            "apply",
            vec![
                env.read().unwrap().get("+").unwrap(),
                Value::Int(1),
                Value::Int(2),
                Value::Vector(Vector::from(vec![Value::Int(3), Value::Int(4)])),
            ],
        );
        assert_eq!(res, Value::Int(10));
    }

    #[test]
    fn take_while_stops_on_predicate_failure() {
        let env = default_env();
        let pred = Value::native_fn(FnArity::exact(1), |args| match args {
            [Value::Int(n)] => Ok(Value::Bool(*n < 3)),
            _ => Err(CloveError::runtime("invalid arg")),
        });
        let res = call(
            &env,
            "take-while",
            vec![
                pred,
                Value::Vector(Vector::from(vec![
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3),
                    Value::Int(0),
                ])),
            ],
        );
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)]))
        );
    }

    #[test]
    fn take_last_returns_tail_items() {
        let env = default_env();
        let res = call(
            &env,
            "take-last",
            vec![
                Value::Int(2),
                Value::Vector(Vector::from(vec![
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3),
                ])),
            ],
        );
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![Value::Int(2), Value::Int(3)]))
        );
    }

    #[test]
    fn take_last_handles_seq_and_shorter_length() {
        let env = default_env();
        let seq = Value::Seq(SeqHandle::from_iter(
            vec![Value::Int(5), Value::Int(6)].into_iter(),
        ));
        let res = call(&env, "take-last", vec![Value::Int(5), seq]);
        assert_eq!(
            res,
            Value::Vector(Vector::from(vec![Value::Int(5), Value::Int(6)]))
        );
    }
}
