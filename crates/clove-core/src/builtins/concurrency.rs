use crate::ast::{contains_mut_collection, FnArity, HashMap, Key, Value, Vector};
use crate::builtins::{dag, def_builtin, err, map_like_to_hashmap, type_mismatch_arg};
use crate::concurrency::{
    add_watch_value, current_cancel_ctx, future_from_promise, never_ready_cancel_chan, promise_all,
    promise_all_settled, promise_any, promise_catch, promise_error_value, promise_finally,
    promise_kind_done, promise_like_from_value, promise_race, promise_then, remove_watch_value,
    select_cases, spawn_future, spawn_task, timeout_channel, AgentHandle, AtomHandle, ChanHandle,
    PromiseHandle, PromiseKind, SelectOp, SelectResult,
};
use crate::env::Env;
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::types::TypeKind;
use std::time::Duration;

pub(crate) fn install(env: &mut Env) {
    install_atom_builtins(env);
    install_channel_builtins(env);
    install_timeout_builtins(env);
    install_cancel_context_builtins(env);
    install_task_builtins(env);
    install_future_builtins(env);
    install_promise_builtins(env);
    install_delay_builtins(env);
    install_promise_combinators(env);
    install_agent_builtins(env);
    install_select_builtins(env);
    install_deref_builtin(env);
    install_aliases(env);
    install_fn_meta();
}

fn reject_mut_boundary(op: &str, value: &Value) -> Result<(), CloveError> {
    if contains_mut_collection(value) {
        return Err(CloveError::runtime(format!(
            "{op} cannot accept mutable collection; use (imut x)"
        )));
    }
    Ok(())
}

fn install_fn_meta() {
    fn named(name: &str) -> TypeKind {
        TypeKind::Named(name.to_string())
    }
    fn atom_ty() -> TypeKind {
        named("core::Atom")
    }
    fn chan_ty() -> TypeKind {
        named("core::Chan")
    }
    fn promise_ty() -> TypeKind {
        named("core::Promise")
    }
    fn task_ty() -> TypeKind {
        named("core::Task")
    }
    fn future_ty() -> TypeKind {
        named("core::Future")
    }
    fn agent_ty() -> TypeKind {
        named("core::Agent")
    }
    fn duration_ty() -> TypeKind {
        named("core::Duration")
    }
    fn map_any() -> TypeKind {
        TypeKind::map(TypeKind::Any, TypeKind::Any)
    }
    fn pmap_opts_record() -> TypeKind {
        let mut fields = im::HashMap::new();
        fields.insert("max-parallel".to_string(), TypeKind::Int);
        fields.insert("workers".to_string(), TypeKind::Int);
        fields.insert("chunk".to_string(), TypeKind::Int);
        fields.insert("buffer".to_string(), TypeKind::Int);
        fields.insert("ordered?".to_string(), TypeKind::Bool);
        fields.insert("on-error".to_string(), TypeKind::Any);
        TypeKind::Record(fields)
    }
    fn watchable_ty() -> TypeKind {
        TypeKind::union(vec![
            atom_ty(),
            agent_ty(),
            promise_ty(),
            task_ty(),
            future_ty(),
        ])
    }
    fn promise_like_ty() -> TypeKind {
        TypeKind::union(vec![promise_ty(), task_ty(), future_ty()])
    }
    fn done_target_ty() -> TypeKind {
        TypeKind::union(vec![promise_like_ty(), agent_ty()])
    }
    fn overload(args: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> FnOverload {
        FnOverload {
            arg_types: args,
            rest,
            ret_type: ret,
            special_op: None,
        }
    }
    fn register(name: &str, arglist: &[&str], overloads: Vec<FnOverload>) {
        register_with_subject(name, arglist, overloads, SubjectPos::Fixed(1));
    }

    fn register_with_subject(
        name: &str,
        arglist: &[&str],
        overloads: Vec<FnOverload>,
        subject_pos: SubjectPos,
    ) {
        let (ns, local) = match name.split_once("::") {
            Some((ns, local)) => (ns, local),
            None => ("core", name),
        };
        let mut meta = FnMeta::new(ns, local);
        for arg in arglist {
            meta.arglist.push((*arg).into());
        }
        meta.overloads = overloads;
        meta.subject_pos = Some(subject_pos);
        fn_meta::register(meta);
    }

    register(
        "atom",
        &["[value]", "[value opts]"],
        vec![
            overload(vec![TypeKind::Any], None, atom_ty()),
            overload(vec![TypeKind::Any, map_any()], None, atom_ty()),
        ],
    );
    register(
        "atom?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "atom-deref",
        &["[atom]"],
        vec![overload(vec![atom_ty()], None, TypeKind::Any)],
    );
    register(
        "atom-set!",
        &["[atom value]"],
        vec![overload(
            vec![atom_ty(), TypeKind::Any],
            None,
            TypeKind::Any,
        )],
    );
    register(
        "atom-update!",
        &["[atom f & args]"],
        vec![overload(
            vec![atom_ty(), TypeKind::Any],
            Some(TypeKind::Any),
            TypeKind::Any,
        )],
    );
    register(
        "compare-and-set!",
        &["[atom expected new]"],
        vec![overload(
            vec![atom_ty(), TypeKind::Any, TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register(
        "atom-add-watch",
        &["[atom key f]"],
        vec![overload(
            vec![watchable_ty(), TypeKind::Any, TypeKind::Any],
            None,
            watchable_ty(),
        )],
    );
    register(
        "atom-remove-watch",
        &["[atom key]"],
        vec![overload(
            vec![watchable_ty(), TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register(
        "add-watch",
        &["[value key f]"],
        vec![overload(
            vec![watchable_ty(), TypeKind::Any, TypeKind::Any],
            None,
            watchable_ty(),
        )],
    );
    register(
        "remove-watch",
        &["[value key]"],
        vec![overload(
            vec![watchable_ty(), TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register(
        "atom-validator",
        &["[atom]"],
        vec![overload(vec![atom_ty()], None, TypeKind::Any)],
    );
    register(
        "atom-set-validator!",
        &["[atom validator]"],
        vec![overload(vec![atom_ty(), TypeKind::Any], None, atom_ty())],
    );
    register(
        "chan",
        &["[]", "[capacity]"],
        vec![
            overload(vec![], None, chan_ty()),
            overload(vec![TypeKind::Any], None, chan_ty()),
        ],
    );
    register(
        "chan?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "chan-put!",
        &["[chan value]"],
        vec![overload(
            vec![chan_ty(), TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register(
        "chan-take!",
        &["[chan]"],
        vec![overload(vec![chan_ty()], None, TypeKind::Any)],
    );
    register(
        "chan-close!",
        &["[chan]"],
        vec![overload(vec![chan_ty()], None, TypeKind::Bool)],
    );
    register(
        "chan-closed?",
        &["[chan]"],
        vec![overload(vec![chan_ty()], None, TypeKind::Bool)],
    );
    register(
        "timeout",
        &["[ms-or-duration]"],
        vec![overload(
            vec![TypeKind::union(vec![TypeKind::Int, duration_ty()])],
            None,
            chan_ty(),
        )],
    );
    register(
        "cancelled?",
        &["[]"],
        vec![overload(vec![], None, TypeKind::Bool)],
    );
    register(
        "cancel-chan",
        &["[]"],
        vec![overload(vec![], None, chan_ty())],
    );
    register(
        "spawn",
        &["[f]", "[f opts]"],
        vec![
            overload(vec![TypeKind::Any], None, task_ty()),
            overload(vec![TypeKind::Any, map_any()], None, task_ty()),
        ],
    );
    register(
        "task?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "task-deref",
        &["[task]"],
        vec![overload(vec![task_ty()], None, TypeKind::Any)],
    );
    register(
        "task-done?",
        &["[task]"],
        vec![overload(vec![task_ty()], None, TypeKind::Bool)],
    );
    register(
        "task-cancel!",
        &["[task]"],
        vec![overload(vec![task_ty()], None, TypeKind::Bool)],
    );
    register(
        "future",
        &["[f]", "[f opts]"],
        vec![
            overload(vec![TypeKind::Any], None, future_ty()),
            overload(vec![TypeKind::Any, map_any()], None, future_ty()),
        ],
    );
    register(
        "future?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "future-deref",
        &["[future]"],
        vec![overload(vec![future_ty()], None, TypeKind::Any)],
    );
    register(
        "future-done?",
        &["[future]"],
        vec![overload(vec![future_ty()], None, TypeKind::Bool)],
    );
    register(
        "future-cancelled?",
        &["[future]"],
        vec![overload(vec![future_ty()], None, TypeKind::Bool)],
    );
    register(
        "future-cancel!",
        &["[future]"],
        vec![overload(vec![future_ty()], None, TypeKind::Bool)],
    );
    register(
        "promise",
        &["[]"],
        vec![overload(vec![], None, promise_ty())],
    );
    register(
        "promise?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "promise-deref",
        &["[promise]"],
        vec![overload(vec![promise_ty()], None, TypeKind::Any)],
    );
    register(
        "promise-deliver!",
        &["[promise value]"],
        vec![overload(
            vec![promise_ty(), TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register(
        "promise-done?",
        &["[promise]"],
        vec![overload(vec![promise_like_ty()], None, TypeKind::Bool)],
    );
    register(
        "promise-error",
        &["[promise]"],
        vec![overload(vec![promise_like_ty()], None, TypeKind::Any)],
    );
    register(
        "deliver",
        &["[promise value]"],
        vec![overload(
            vec![promise_ty(), TypeKind::Any],
            None,
            TypeKind::Bool,
        )],
    );
    register_with_subject(
        "pmap",
        &["[f coll]", "[f coll opts]", "[opts f coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, pmap_opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![pmap_opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
    register_with_subject(
        "pfilter",
        &["[pred coll]", "[pred coll opts]", "[opts pred coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, pmap_opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![pmap_opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
    register(
        "promise-then",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            promise_ty(),
        )],
    );
    register(
        "promise-catch",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            promise_ty(),
        )],
    );
    register(
        "promise-finally",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            promise_ty(),
        )],
    );
    register(
        "promise-all",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, promise_ty())],
    );
    register(
        "promise-all-settled",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, promise_ty())],
    );
    register(
        "promise-race",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, promise_ty())],
    );
    register(
        "promise-any",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, promise_ty())],
    );
    register(
        "future-then",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            future_ty(),
        )],
    );
    register(
        "future-catch",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            future_ty(),
        )],
    );
    register(
        "future-finally",
        &["[promise f]"],
        vec![overload(
            vec![promise_like_ty(), TypeKind::Any],
            None,
            future_ty(),
        )],
    );
    register(
        "future-all",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, future_ty())],
    );
    register(
        "future-any",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, future_ty())],
    );
    register(
        "future-race",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, future_ty())],
    );
    register(
        "future-all-settled",
        &["[coll]"],
        vec![overload(vec![TypeKind::Any], None, future_ty())],
    );
    register(
        "delay?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "realized?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "force",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Any)],
    );
    register(
        "agent",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, agent_ty())],
    );
    register(
        "agent?",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Bool)],
    );
    register(
        "agent-deref",
        &["[agent]"],
        vec![overload(vec![agent_ty()], None, TypeKind::Any)],
    );
    register(
        "agent-send!",
        &["[agent f & args]"],
        vec![overload(
            vec![agent_ty(), TypeKind::Any],
            Some(TypeKind::Any),
            agent_ty(),
        )],
    );
    register(
        "agent-send-io!",
        &["[agent f & args]"],
        vec![overload(
            vec![agent_ty(), TypeKind::Any],
            Some(TypeKind::Any),
            agent_ty(),
        )],
    );
    register(
        "agent-await",
        &["[agent]"],
        vec![overload(vec![agent_ty()], None, TypeKind::Bool)],
    );
    register(
        "agent-done?",
        &["[agent]"],
        vec![overload(vec![agent_ty()], None, TypeKind::Bool)],
    );
    register(
        "agent-error",
        &["[agent]"],
        vec![overload(vec![agent_ty()], None, TypeKind::Any)],
    );
    register(
        "agent-restart!",
        &["[agent value]"],
        vec![overload(vec![agent_ty(), TypeKind::Any], None, agent_ty())],
    );
    register(
        "select",
        &["[cases]", "[cases opts]"],
        vec![
            overload(vec![TypeKind::Any], None, TypeKind::Any),
            overload(vec![TypeKind::Any, map_any()], None, TypeKind::Any),
        ],
    );
    register(
        "scope-select",
        &["[cases]", "[cases opts]"],
        vec![
            overload(vec![TypeKind::Any], None, TypeKind::Any),
            overload(vec![TypeKind::Any, map_any()], None, TypeKind::Any),
        ],
    );
    register(
        "deref",
        &["[value]"],
        vec![overload(vec![TypeKind::Any], None, TypeKind::Any)],
    );
    register(
        "done?",
        &["[value]"],
        vec![overload(vec![done_target_ty()], None, TypeKind::Bool)],
    );
}

fn install_atom_builtins(env: &mut Env) {
    def_builtin!(env, "atom", FnArity::range(1, 2), |args| match args {
        [value] => Ok(Value::Atom(AtomHandle::new(value.clone()))),
        [value, opts] => {
            let opts = map_like_to_hashmap(opts, "atom", 2)?;
            let validator = opts
                .get(&Key::Keyword("validator".into()))
                .cloned()
                .filter(|v| !matches!(v, Value::Nil));
            let handle = AtomHandle::with_validator(value.clone(), validator)?;
            Ok(Value::Atom(handle))
        }
        _ => err("atom expects initial value and optional opts map"),
    });
    def_builtin!(env, "atom?", FnArity::exact(1), |args| match args {
        [Value::Atom(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Atom(_)))),
        _ => err("atom? expects one value"),
    });
    def_builtin!(env, "atom-deref", FnArity::exact(1), |args| match args {
        [Value::Atom(handle)] => Ok(handle.deref()),
        [other] => Err(crate::builtins::type_mismatch_arg(
            "atom",
            "atom-deref",
            1,
            other,
        )),
        _ => err("atom-deref expects atom"),
    });
    def_builtin!(env, "atom-set!", FnArity::exact(2), |args| match args {
        [Value::Atom(handle), value] => handle.set(value.clone()),
        [other, _] => Err(crate::builtins::type_mismatch_arg(
            "atom",
            "atom-set!",
            1,
            other,
        )),
        _ => err("atom-set! expects atom and value"),
    });
    def_builtin!(
        env,
        "atom-update!",
        FnArity::at_least(2),
        |args| match args {
            [Value::Atom(handle), func, rest @ ..] => {
                let extras = rest.iter().cloned().collect::<Vec<_>>();
                handle.update_with_callable(func.clone(), extras)
            }
            [other, ..] => Err(type_mismatch_arg("atom", "atom-update!", 1, other)),
            _ => err("atom-update! expects atom, function, and optional args"),
        }
    );
    def_builtin!(
        env,
        "compare-and-set!",
        FnArity::exact(3),
        |args| match args {
            [Value::Atom(handle), expected, new_value] => Ok(Value::Bool(
                handle.compare_and_set(expected.clone(), new_value.clone())?
            )),
            [other, _, _] => Err(type_mismatch_arg("atom", "compare-and-set!", 1, other)),
            _ => err("compare-and-set! expects atom, expected, and new value"),
        }
    );
    def_builtin!(env, "atom-add-watch", FnArity::exact(3), |args| {
        add_watch_builtin(args, "atom-add-watch")
    });
    def_builtin!(env, "atom-remove-watch", FnArity::exact(2), |args| {
        remove_watch_builtin(args, "atom-remove-watch")
    });
    def_builtin!(env, "add-watch", FnArity::exact(3), |args| {
        add_watch_builtin(args, "add-watch")
    });
    def_builtin!(env, "remove-watch", FnArity::exact(2), |args| {
        remove_watch_builtin(args, "remove-watch")
    });
    def_builtin!(
        env,
        "atom-validator",
        FnArity::exact(1),
        |args| match args {
            [Value::Atom(handle)] => Ok(handle.validator().unwrap_or(Value::Nil)),
            [other] => Err(type_mismatch_arg("atom", "atom-validator", 1, other)),
            _ => err("atom-validator expects atom"),
        }
    );
    def_builtin!(
        env,
        "atom-set-validator!",
        FnArity::exact(2),
        |args| match args {
            [Value::Atom(handle), Value::Nil] => {
                handle.set_validator(None)?;
                Ok(Value::Atom(handle.clone()))
            }
            [Value::Atom(handle), validator] => {
                handle.set_validator(Some(validator.clone()))?;
                Ok(Value::Atom(handle.clone()))
            }
            [other, _] => Err(type_mismatch_arg("atom", "atom-set-validator!", 1, other)),
            _ => err("atom-set-validator! expects atom and validator"),
        }
    );
}

fn install_channel_builtins(env: &mut Env) {
    def_builtin!(env, "chan", FnArity::range(0, 1), |args| {
        let capacity = match args {
            [] => None,
            [Value::Int(n)] if *n >= 0 => Some(*n as usize),
            [Value::Int(_)] => return err("chan capacity must be non-negative integer or nil"),
            [Value::Nil] => None,
            [other] => {
                return Err(type_mismatch_arg(
                    "non-negative integer or nil",
                    "chan",
                    1,
                    other,
                ))
            }
            _ => return err("chan expects optional capacity"),
        };
        Ok(Value::Chan(ChanHandle::new(capacity)))
    });
    def_builtin!(env, "chan?", FnArity::exact(1), |args| match args {
        [Value::Chan(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Chan(_)))),
        _ => err("chan? expects one value"),
    });
    def_builtin!(env, "chan-put!", FnArity::exact(2), |args| match args {
        [Value::Chan(handle), value] => {
            reject_mut_boundary("chan-put!", value)?;
            Ok(Value::Bool(handle.put(value.clone())))
        }
        [other, _] => Err(crate::builtins::type_mismatch_arg(
            "channel",
            "chan-put!",
            1,
            other,
        )),
        _ => err("chan-put! expects channel and value"),
    });
    def_builtin!(env, "chan-take!", FnArity::exact(1), |args| match args {
        [Value::Chan(handle)] => Ok(handle.take().unwrap_or(Value::Nil)),
        [other] => Err(crate::builtins::type_mismatch_arg(
            "channel",
            "chan-take!",
            1,
            other,
        )),
        _ => err("chan-take! expects channel"),
    });
    def_builtin!(env, "chan-close!", FnArity::exact(1), |args| match args {
        [Value::Chan(handle)] => {
            handle.close();
            Ok(Value::Bool(true))
        }
        [other] => Err(crate::builtins::type_mismatch_arg(
            "channel",
            "chan-close!",
            1,
            other,
        )),
        _ => err("chan-close! expects channel"),
    });
    def_builtin!(env, "chan-closed?", FnArity::exact(1), |args| match args {
        [Value::Chan(handle)] => Ok(Value::Bool(handle.is_closed())),
        [other] => Err(crate::builtins::type_mismatch_arg(
            "channel",
            "chan-closed?",
            1,
            other,
        )),
        _ => err("chan-closed? expects channel"),
    });
}

fn install_timeout_builtins(env: &mut Env) {
    def_builtin!(env, "timeout", FnArity::exact(1), |args| {
        match args {
            [Value::Int(ms)] if *ms >= 0 => {
                let chan = timeout_channel(Duration::from_millis(*ms as u64));
                Ok(Value::Chan(chan))
            }
            [Value::Duration(d)] => {
                let millis = d
                    .to_millis_i64()
                    .ok()
                    .ok_or_else(|| CloveError::runtime("timeout expects non-negative duration"))?;
                if millis < 0 {
                    return err("timeout expects non-negative duration");
                }
                let chan = timeout_channel(Duration::from_millis(millis as u64));
                Ok(Value::Chan(chan))
            }
            [Value::Nil] => {
                let chan = timeout_channel(Duration::from_millis(0));
                Ok(Value::Chan(chan))
            }
            [Value::Int(_)] => err("timeout expects non-negative integer or duration"),
            [other] => Err(type_mismatch_arg(
                "non-negative integer, duration, or nil",
                "timeout",
                1,
                other,
            )),
            _ => err("timeout expects non-negative integer or duration"),
        }
    });
}

fn install_cancel_context_builtins(env: &mut Env) {
    def_builtin!(env, "cancelled?", FnArity::exact(0), |args| match args {
        [] => Ok(Value::Bool(
            current_cancel_ctx()
                .as_ref()
                .map(|ctx| ctx.is_cancelled())
                .unwrap_or(false),
        )),
        _ => err("cancelled? takes no arguments"),
    });
    def_builtin!(env, "cancel-chan", FnArity::exact(0), |args| match args {
        [] => Ok(Value::Chan(
            current_cancel_ctx()
                .map(|ctx| ctx.cancel_chan())
                .unwrap_or_else(never_ready_cancel_chan),
        )),
        _ => err("cancel-chan takes no arguments"),
    });
}

fn install_task_builtins(env: &mut Env) {
    def_builtin!(env, "spawn", FnArity::range(1, 2), |args| match args {
        [callable] => Ok(Value::Task(spawn_task(callable.clone()))),
        [callable, Value::Map(_)] | [callable, Value::SortedMap(_)] => {
            Ok(Value::Task(spawn_task(callable.clone())))
        }
        [_, other] => Err(type_mismatch_arg("map", "spawn", 2, other)),
        _ => err("spawn expects callable and optional options map"),
    });
    def_builtin!(env, "task?", FnArity::exact(1), |args| match args {
        [Value::Task(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Task(_)))),
        _ => err("task? expects one value"),
    });
    def_builtin!(env, "task-deref", FnArity::exact(1), |args| match args {
        [Value::Task(handle)] => handle.wait(),
        [other] => Err(type_mismatch_arg("task", "task-deref", 1, other)),
        _ => err("task-deref expects task"),
    });
    def_builtin!(env, "task-done?", FnArity::exact(1), |args| match args {
        [Value::Task(handle)] => {
            Ok(Value::Bool(promise_kind_done(&PromiseKind::Task(
                handle.clone(),
            ))))
        }
        [other] => Err(type_mismatch_arg("task", "task-done?", 1, other)),
        _ => err("task-done? expects task"),
    });
    def_builtin!(env, "task-cancel!", FnArity::exact(1), |args| match args {
        [Value::Task(_)] => Ok(Value::Bool(false)),
        [other] => Err(type_mismatch_arg("task", "task-cancel!", 1, other)),
        _ => err("task-cancel! expects task"),
    });
}

fn install_future_builtins(env: &mut Env) {
    def_builtin!(env, "future", FnArity::range(1, 2), |args| match args {
        [callable] => Ok(Value::Future(spawn_future(callable.clone()))),
        [callable, Value::Map(_)] | [callable, Value::SortedMap(_)] => {
            Ok(Value::Future(spawn_future(callable.clone())))
        }
        [_, other] => Err(type_mismatch_arg("map", "future", 2, other)),
        _ => err("future expects callable"),
    });
    def_builtin!(env, "future?", FnArity::exact(1), |args| match args {
        [Value::Future(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Future(_)))),
        _ => err("future? expects one value"),
    });
    def_builtin!(env, "future-deref", FnArity::exact(1), |args| match args {
        [Value::Future(handle)] => handle.wait(),
        [other] => Err(type_mismatch_arg("future", "future-deref", 1, other)),
        _ => err("future-deref expects future"),
    });
    def_builtin!(env, "future-done?", FnArity::exact(1), |args| match args {
        [Value::Future(handle)] => Ok(Value::Bool(promise_kind_done(&PromiseKind::Future(
            handle.clone(),
        )))),
        [other] => Err(type_mismatch_arg("future", "future-done?", 1, other)),
        _ => err("future-done? expects future"),
    });
    def_builtin!(
        env,
        "future-cancelled?",
        FnArity::exact(1),
        |args| match args {
            [Value::Future(handle)] => Ok(Value::Bool(handle.is_cancelled())),
            [other] => Err(type_mismatch_arg("future", "future-cancelled?", 1, other)),
            _ => err("future-cancelled? expects future"),
        }
    );
    def_builtin!(
        env,
        "future-cancel!",
        FnArity::exact(1),
        |args| match args {
            [Value::Future(handle)] => {
                for _ in 0..5 {
                    if handle.is_done() {
                        return Ok(Value::Bool(false));
                    }
                    std::thread::yield_now();
                    std::thread::sleep(Duration::from_millis(1));
                }
                Ok(Value::Bool(handle.cancel()))
            }
            [other] => Err(type_mismatch_arg("future", "future-cancel!", 1, other)),
            _ => err("future-cancel! expects future"),
        }
    );
}

fn install_promise_builtins(env: &mut Env) {
    def_builtin!(env, "promise", FnArity::exact(0), |args| match args {
        [] => Ok(Value::Promise(PromiseHandle::new())),
        _ => err("promise takes no arguments"),
    });
    def_builtin!(env, "promise?", FnArity::exact(1), |args| match args {
        [Value::Promise(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Promise(_)))),
        _ => err("promise? expects one value"),
    });
    def_builtin!(env, "promise-deref", FnArity::exact(1), |args| match args {
        [Value::Promise(handle)] => handle.wait(),
        [other] => Err(type_mismatch_arg("promise", "promise-deref", 1, other)),
        _ => err("promise-deref expects promise"),
    });
    def_builtin!(
        env,
        "promise-deliver!",
        FnArity::exact(2),
        |args| match args {
            [Value::Promise(handle), value] => {
                reject_mut_boundary("promise-deliver!", value)?;
                Ok(Value::Bool(handle.resolve(Ok(value.clone()))))
            }
            [other, _] => Err(type_mismatch_arg("promise", "promise-deliver!", 1, other,)),
            _ => err("promise-deliver! expects promise and value"),
        }
    );
    def_builtin!(env, "promise-done?", FnArity::exact(1), |args| match args {
        [value] => {
            let kind = expect_promise_like(value, "promise-done?", 1)?;
            Ok(Value::Bool(promise_kind_done(&kind)))
        }
        _ => err("promise-done? expects promise/task/future"),
    });
    def_builtin!(env, "promise-error", FnArity::exact(1), |args| match args {
        [value] => {
            let _ = expect_promise_like(value, "promise-error", 1)?;
            promise_error_value(value)
        }
        _ => err("promise-error expects promise/task/future"),
    });
    def_builtin!(env, "deliver", FnArity::exact(2), |args| match args {
        [Value::Promise(handle), value] => {
            reject_mut_boundary("deliver", value)?;
            Ok(Value::Bool(handle.resolve(Ok(value.clone()))))
        }
        [other, _] => Err(type_mismatch_arg("promise", "deliver", 1, other)),
        _ => err("deliver expects promise and value"),
    });
    def_builtin!(env, "pmap", FnArity::range(2, 3), |args| match args {
        [f, coll] => {
            let opts = dag::normalize_opts_value(None, "pmap", 0)?;
            dag::pmap_execute(f.clone(), coll.clone(), opts)
        }
        [first, second, third] => {
            let (f, coll, opts, opts_arg) = match first {
                Value::Map(_) | Value::MutMap(_) | Value::SortedMap(_) => {
                    (second, third, Some(first), 1)
                }
                _ => (first, second, Some(third), 3),
            };
            let opts = dag::normalize_opts_value(opts, "pmap", opts_arg)?;
            dag::pmap_execute(f.clone(), coll.clone(), opts)
        }
        _ => err("pmap expects function, collection, and optional opts map"),
    });
    def_builtin!(env, "pfilter", FnArity::range(2, 3), |args| match args {
        [pred, coll] => {
            let opts = dag::normalize_opts_value(None, "pfilter", 0)?;
            dag::pfilter_execute(pred.clone(), coll.clone(), opts)
        }
        [first, second, third] => {
            let (pred, coll, opts, opts_arg) = match first {
                Value::Map(_) | Value::MutMap(_) | Value::SortedMap(_) => {
                    (second, third, Some(first), 1)
                }
                _ => (first, second, Some(third), 3),
            };
            let opts = dag::normalize_opts_value(opts, "pfilter", opts_arg)?;
            dag::pfilter_execute(pred.clone(), coll.clone(), opts)
        }
        _ => err("pfilter expects predicate, collection, and optional opts map"),
    });
}

fn install_promise_combinators(env: &mut Env) {
    def_builtin!(env, "promise-then", FnArity::exact(2), |args| match args {
        [source, func] => {
            let kind = expect_promise_like(source, "promise-then", 1)?;
            Ok(Value::Promise(promise_then(kind, func.clone())))
        }
        _ => err("promise-then expects promise and function"),
    });
    def_builtin!(env, "promise-catch", FnArity::exact(2), |args| match args {
        [source, func] => {
            let kind = expect_promise_like(source, "promise-catch", 1)?;
            Ok(Value::Promise(promise_catch(kind, func.clone())))
        }
        _ => err("promise-catch expects promise and function"),
    });
    def_builtin!(
        env,
        "promise-finally",
        FnArity::exact(2),
        |args| match args {
            [source, func] => {
                let kind = expect_promise_like(source, "promise-finally", 1)?;
                Ok(Value::Promise(promise_finally(kind, func.clone())))
            }
            _ => err("promise-finally expects promise and function"),
        }
    );
    def_builtin!(env, "promise-all", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "promise-all")?;
            Ok(Value::Promise(promise_all(promises)))
        }
        _ => err("promise-all expects collection"),
    });
    def_builtin!(
        env,
        "promise-all-settled",
        FnArity::exact(1),
        |args| match args {
            [collection] => {
                let promises = collect_promises(collection, "promise-all-settled")?;
                Ok(Value::Promise(promise_all_settled(promises)))
            }
            _ => err("promise-all-settled expects collection"),
        }
    );
    def_builtin!(env, "promise-race", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "promise-race")?;
            Ok(Value::Promise(promise_race(promises)))
        }
        _ => err("promise-race expects collection"),
    });
    def_builtin!(env, "promise-any", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "promise-any")?;
            Ok(Value::Promise(promise_any(promises)))
        }
        _ => err("promise-any expects collection"),
    });

    def_builtin!(env, "future-then", FnArity::exact(2), |args| match args {
        [source, func] => {
            let kind = expect_promise_like(source, "future-then", 1)?;
            let promise = promise_then(kind, func.clone());
            Ok(Value::Future(future_from_promise(promise)))
        }
        _ => err("future-then expects future or promise and function"),
    });
    def_builtin!(env, "future-catch", FnArity::exact(2), |args| match args {
        [source, func] => {
            let kind = expect_promise_like(source, "future-catch", 1)?;
            let promise = promise_catch(kind, func.clone());
            Ok(Value::Future(future_from_promise(promise)))
        }
        _ => err("future-catch expects future/promise and function"),
    });
    def_builtin!(
        env,
        "future-finally",
        FnArity::exact(2),
        |args| match args {
            [source, func] => {
                let kind = expect_promise_like(source, "future-finally", 1)?;
                let promise = promise_finally(kind, func.clone());
                Ok(Value::Future(future_from_promise(promise)))
            }
            _ => err("future-finally expects future/promise and function"),
        }
    );
    def_builtin!(env, "future-all", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "future-all")?;
            let promise = promise_all(promises);
            Ok(Value::Future(future_from_promise(promise)))
        }
        _ => err("future-all expects collection"),
    });
    def_builtin!(env, "future-any", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "future-any")?;
            let promise = promise_any(promises);
            Ok(Value::Future(future_from_promise(promise)))
        }
        _ => err("future-any expects collection"),
    });
    def_builtin!(env, "future-race", FnArity::exact(1), |args| match args {
        [collection] => {
            let promises = collect_promises(collection, "future-race")?;
            let promise = promise_race(promises);
            Ok(Value::Future(future_from_promise(promise)))
        }
        _ => err("future-race expects collection"),
    });
    def_builtin!(
        env,
        "future-all-settled",
        FnArity::exact(1),
        |args| match args {
            [collection] => {
                let promises = collect_promises(collection, "future-all-settled")?;
                let promise = promise_all_settled(promises);
                Ok(Value::Future(future_from_promise(promise)))
            }
            _ => err("future-all-settled expects collection"),
        }
    );
}

fn install_delay_builtins(env: &mut Env) {
    def_builtin!(env, "delay?", FnArity::exact(1), |args| match args {
        [Value::Delay(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Delay(_)))),
        _ => err("delay? expects one value"),
    });
    def_builtin!(env, "realized?", FnArity::exact(1), |args| match args {
        [Value::Delay(handle)] => Ok(Value::Bool(handle.is_realized())),
        [Value::Nil] => Ok(Value::Bool(false)),
        [_] => Ok(Value::Bool(false)),
        _ => err("realized? expects one value"),
    });
    def_builtin!(env, "force", FnArity::exact(1), |args| match args {
        [Value::Delay(handle)] => handle.force(),
        [other] => Ok(other.clone()),
        _ => err("force expects one value"),
    });
}

fn install_agent_builtins(env: &mut Env) {
    def_builtin!(env, "agent", FnArity::exact(1), |args| match args {
        [initial] => {
            reject_mut_boundary("agent", initial)?;
            Ok(Value::Agent(AgentHandle::new(initial.clone())))
        }
        _ => err("agent expects initial value"),
    });
    def_builtin!(env, "agent?", FnArity::exact(1), |args| match args {
        [Value::Agent(_)] => Ok(Value::Bool(true)),
        [Value::Nil] => Ok(Value::Bool(false)),
        [other] => Ok(Value::Bool(matches!(other, Value::Agent(_)))),
        _ => err("agent? expects one value"),
    });
    def_builtin!(env, "agent-deref", FnArity::exact(1), |args| match args {
        [Value::Agent(handle)] => Ok(handle.state()),
        [other] => Err(type_mismatch_arg("agent", "agent-deref", 1, other)),
        _ => err("agent-deref expects agent"),
    });
    def_builtin!(
        env,
        "agent-send!",
        FnArity::at_least(2),
        |args| match args {
            [Value::Agent(handle), func, rest @ ..] => {
                for item in rest {
                    reject_mut_boundary("agent-send!", item)?;
                }
                let payload = rest.iter().cloned().collect::<Vec<_>>();
                handle.send(func.clone(), payload);
                Ok(Value::Agent(handle.clone()))
            }
            [other, ..] => Err(type_mismatch_arg("agent", "agent-send!", 1, other)),
            _ => err("agent-send! expects agent, function, and args"),
        }
    );
    def_builtin!(
        env,
        "agent-send-io!",
        FnArity::at_least(2),
        |args| match args {
            [Value::Agent(handle), func, rest @ ..] => {
                for item in rest {
                    reject_mut_boundary("agent-send-io!", item)?;
                }
                let payload = rest.iter().cloned().collect::<Vec<_>>();
                handle.send(func.clone(), payload);
                Ok(Value::Agent(handle.clone()))
            }
            [other, ..] => Err(type_mismatch_arg("agent", "agent-send-io!", 1, other)),
            _ => err("agent-send-io! expects agent, function, and args"),
        }
    );
    def_builtin!(env, "agent-await", FnArity::exact(1), |args| match args {
        [Value::Agent(handle)] => {
            handle.await_all();
            Ok(Value::Bool(true))
        }
        [other] => Err(type_mismatch_arg("agent", "agent-await", 1, other)),
        _ => err("agent-await expects agent"),
    });
    def_builtin!(env, "agent-done?", FnArity::exact(1), |args| match args {
        [Value::Agent(handle)] => Ok(Value::Bool(handle.is_idle())),
        [other] => Err(type_mismatch_arg("agent", "agent-done?", 1, other)),
        _ => err("agent-done? expects agent"),
    });
    def_builtin!(env, "agent-error", FnArity::exact(1), |args| match args {
        [Value::Agent(handle)] => match handle.error() {
            Some(err) => {
                let text = err.to_string();
                let msg = text
                    .strip_prefix("Runtime error: ")
                    .unwrap_or(text.as_str())
                    .to_string();
                Ok(Value::String(msg))
            }
            None => Ok(Value::Nil),
        },
        [other] => Err(type_mismatch_arg("agent", "agent-error", 1, other)),
        _ => err("agent-error expects agent"),
    });
    def_builtin!(
        env,
        "agent-restart!",
        FnArity::exact(2),
        |args| match args {
            [Value::Agent(handle), value] => {
                reject_mut_boundary("agent-restart!", value)?;
                handle.restart(value.clone());
                Ok(Value::Agent(handle.clone()))
            }
            [other, _] => Err(type_mismatch_arg("agent", "agent-restart!", 1, other)),
            _ => err("agent-restart! expects agent and value"),
        }
    );
}

fn install_select_builtins(env: &mut Env) {
    def_builtin!(env, "select", FnArity::range(1, 2), |args| match args {
        [cases] => perform_select("select", cases, None, None),
        [cases, opts] => {
            let opts = map_like_to_hashmap(opts, "select", 2)?;
            let timeout = opts
                .get(&Key::Keyword("timeout".into()))
                .and_then(value_to_duration);
            let default = opts.get(&Key::Keyword("default".into())).cloned();
            perform_select("select", cases, timeout, default)
        }
        _ => err("select expects cases and optional opts map"),
    });
    def_builtin!(
        env,
        "scope-select",
        FnArity::range(1, 2),
        |args| match args {
            [cases] => scope_select(cases, None),
            [cases, opts] => {
                let opts = map_like_to_hashmap(opts, "scope-select", 2)?;
                scope_select(cases, Some(&opts))
            }
            _ => err("scope-select expects cases and optional opts map"),
        }
    );
}

fn install_deref_builtin(env: &mut Env) {
    def_builtin!(env, "deref", FnArity::exact(1), |args| match args {
        [value] => deref_value(value),
        _ => err("deref expects one value"),
    });
    def_builtin!(env, "done?", FnArity::exact(1), |args| match args {
        [Value::Agent(handle)] => Ok(Value::Bool(handle.is_idle())),
        [value] => {
            let kind = expect_promise_like(value, "done?", 1)?;
            Ok(Value::Bool(promise_kind_done(&kind)))
        }
        _ => err("done? expects promise/task/future/agent"),
    });
}

fn install_aliases(env: &mut Env) {
    alias(env, "swap!", "atom-update!");
    alias(env, "reset!", "atom-set!");
    alias(env, "set-validator!", "atom-set-validator!");
    alias(env, "validator", "atom-validator");
    alias(env, "get-validator", "atom-validator");
    alias(env, "<!!", "chan-take!");
    alias(env, ">!!", "chan-put!");
    alias(env, "put!", "chan-put!");
    alias(env, "take!", "chan-take!");
    alias(env, "go", "spawn");
    alias(env, "<!", "chan-take!");
    alias(env, ">!", "chan-put!");

    let async_exports = [
        ("chan", "chan"),
        ("chan?", "chan?"),
        ("chan-put!", "chan-put!"),
        ("chan-take!", "chan-take!"),
        ("chan-close!", "chan-close!"),
        ("chan-closed?", "chan-closed?"),
        ("timeout", "timeout"),
        ("cancelled?", "cancelled?"),
        ("cancel-chan", "cancel-chan"),
        ("scope-cancelled?", "cancelled?"),
        ("scope-cancel-chan", "cancel-chan"),
        ("scope-select", "scope-select"),
        ("select", "select"),
        ("go", "spawn"),
        ("<!!", "chan-take!"),
        (">!!", "chan-put!"),
        ("add-watch", "add-watch"),
        ("remove-watch", "remove-watch"),
        ("done?", "done?"),
        ("promise", "promise"),
        ("promise?", "promise?"),
        ("promise-deref", "promise-deref"),
        ("promise-deliver!", "promise-deliver!"),
        ("promise-done?", "promise-done?"),
        ("promise-error", "promise-error"),
        ("promise-then", "promise-then"),
        ("promise-catch", "promise-catch"),
        ("promise-finally", "promise-finally"),
        ("promise-all", "promise-all"),
        ("promise-all-settled", "promise-all-settled"),
        ("promise-race", "promise-race"),
        ("promise-any", "promise-any"),
        ("future", "future"),
        ("future?", "future?"),
        ("future-deref", "future-deref"),
        ("future-done?", "future-done?"),
        ("future-cancel!", "future-cancel!"),
        ("future-then", "future-then"),
        ("future-catch", "future-catch"),
        ("future-finally", "future-finally"),
        ("future-all", "future-all"),
        ("future-any", "future-any"),
        ("future-race", "future-race"),
        ("future-all-settled", "future-all-settled"),
        ("agent", "agent"),
        ("agent?", "agent?"),
        ("agent-deref", "agent-deref"),
        ("agent-send!", "agent-send!"),
        ("agent-send-io!", "agent-send-io!"),
        ("agent-await", "agent-await"),
        ("agent-done?", "agent-done?"),
        ("agent-error", "agent-error"),
        ("agent-restart!", "agent-restart!"),
    ];
    for (name, existing) in async_exports {
        alias_core_async(env, name, existing);
    }
}

fn alias(env: &mut Env, new: &str, existing: &str) {
    if let Some(value) = env.get(existing) {
        env.define_builtin(new, value);
    }
}

fn alias_core_async(env: &mut Env, name: &str, existing: &str) {
    let qualified_colon = format!("async::{}", name);
    alias(env, &qualified_colon, existing);
}

fn add_watch_builtin(args: &[Value], name: &str) -> Result<Value, CloveError> {
    match args {
        [Value::Atom(_)
        | Value::Agent(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_), key, func] => add_watch_value(&args[0], key.clone(), func.clone()),
        [other, ..] => Err(type_mismatch_arg(
            "watchable value (atom/agent/promise/task/future)",
            name,
            1,
            other,
        )),
        _ => err(format!(
            "{name} expects watchable value (atom/agent/promise/task/future), key, and function"
        )),
    }
}

fn remove_watch_builtin(args: &[Value], name: &str) -> Result<Value, CloveError> {
    match args {
        [Value::Atom(_)
        | Value::Agent(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_), key] => remove_watch_value(&args[0], key).map(Value::Bool),
        [other, ..] => Err(type_mismatch_arg(
            "watchable value (atom/agent/promise/task/future)",
            name,
            1,
            other,
        )),
        _ => err(format!(
            "{name} expects watchable value (atom/agent/promise/task/future) and key"
        )),
    }
}

fn expect_promise_like(
    value: &Value,
    op: &str,
    arg_index: usize,
) -> Result<PromiseKind, CloveError> {
    promise_like_from_value(value)
        .ok_or_else(|| type_mismatch_arg("promise/task/future", op, arg_index, value))
}

fn collect_promises(value: &Value, op: &str) -> Result<Vec<PromiseKind>, CloveError> {
    let items = match value {
        Value::Vector(v) => v.iter().collect::<Vec<_>>(),
        Value::List(v) => v.iter().collect::<Vec<_>>(),
        other => return Err(type_mismatch_arg("collection", op, 1, other)),
    };
    items
        .into_iter()
        .map(|item| expect_promise_like(item, op, 1))
        .collect::<Result<Vec<_>, _>>()
}

fn perform_select(
    op: &str,
    cases_value: &Value,
    timeout: Option<Duration>,
    default: Option<Value>,
) -> Result<Value, CloveError> {
    let cases = parse_select_cases(op, cases_value)?;
    let effective_timeout = if default.is_some() {
        Some(Duration::from_millis(0))
    } else {
        timeout
    };
    match select_cases(cases, effective_timeout) {
        Some(SelectResult::Take(value, chan)) => {
            Ok(Value::Vector(Vector::from(vec![value, Value::Chan(chan)])))
        }
        Some(SelectResult::Put(chan)) => Ok(Value::Vector(Vector::from(vec![
            Value::Bool(true),
            Value::Chan(chan),
        ]))),
        None => {
            if let Some(value) = default {
                let pair = Vector::from(vec![value, Value::Nil]);
                Ok(Value::Vector(pair))
            } else {
                Ok(Value::Nil)
            }
        }
    }
}

fn parse_select_cases(op: &str, value: &Value) -> Result<Vec<SelectOp>, CloveError> {
    let entries = match value {
        Value::Vector(items) => items.iter().collect::<Vec<_>>(),
        Value::List(items) => items.iter().collect::<Vec<_>>(),
        other => return Err(type_mismatch_arg("collection", op, 1, other)),
    };
    let mut ops = Vec::new();
    for entry in entries {
        match entry {
            Value::Chan(handle) => ops.push(SelectOp::Take(handle.clone())),
            Value::Vector(items) | Value::List(items) => {
                if items.len() != 3 {
                    return err("select put form must be [:put chan value]");
                }
                let keyword = &items[0];
                let chan = match &items[1] {
                    Value::Chan(handle) => handle.clone(),
                    other => return Err(type_mismatch_arg("channel", op, 1, other)),
                };
                if !matches!(keyword, Value::Symbol(s) if s == ":put" || s == "put") {
                    return err("select vector must start with :put for send operations");
                }
                reject_mut_boundary(op, &items[2])?;
                ops.push(SelectOp::Put(chan, items[2].clone()));
            }
            other => {
                return Err(type_mismatch_arg(
                    "channel or [:put chan value]",
                    op,
                    1,
                    other,
                ))
            }
        }
    }
    Ok(ops)
}

fn scope_select(
    cases_value: &Value,
    opts: Option<&HashMap<Key, Value>>,
) -> Result<Value, CloveError> {
    let cancel_handle = current_cancel_ctx()
        .map(|ctx| ctx.cancel_chan())
        .unwrap_or_else(never_ready_cancel_chan);
    let cancel_chan = Value::Chan(cancel_handle.clone());
    let mut items = match cases_value {
        Value::Vector(values) => values.iter().cloned().collect::<Vec<_>>(),
        Value::List(values) => values.iter().cloned().collect::<Vec<_>>(),
        other => return Err(type_mismatch_arg("collection", "scope-select", 1, other)),
    };
    items.push(cancel_chan.clone());
    let cases = Value::Vector(items.into());
    let timeout = opts
        .and_then(|m| m.get(&Key::Keyword("timeout".into())))
        .and_then(value_to_duration);
    let default = opts
        .and_then(|m| m.get(&Key::Keyword("default".into())))
        .cloned();
    let result = perform_select("scope-select", &cases, timeout, default)?;
    if let Value::Vector(values) = &result {
        if values.len() == 2 {
            if let Value::Chan(ch) = &values[1] {
                if ch.debug_info().id == cancel_handle.debug_info().id {
                    return Ok(Value::Symbol(":cancelled".into()));
                }
            }
        }
    }
    Ok(result)
}

fn value_to_duration(value: &Value) -> Option<Duration> {
    match value {
        Value::Int(ms) => millis_to_duration(*ms),
        Value::Float(ms) if *ms >= 0.0 => millis_to_duration(ms.trunc() as i64),
        Value::Duration(d) => d.to_millis_i64().ok().and_then(millis_to_duration),
        _ => None,
    }
}

fn millis_to_duration(ms: i64) -> Option<Duration> {
    if ms < 0 {
        None
    } else {
        Some(Duration::from_millis(ms as u64))
    }
}

fn deref_value(value: &Value) -> Result<Value, CloveError> {
    match value {
        Value::Atom(handle) => Ok(handle.deref()),
        Value::Promise(handle) => handle.wait(),
        Value::Task(handle) => handle.wait(),
        Value::Future(handle) => handle.wait(),
        Value::Agent(handle) => Ok(handle.state()),
        Value::Delay(handle) => handle.force(),
        other => Err(type_mismatch_arg(
            "derefable value (atom/promise/task/future/agent/delay)",
            "deref",
            1,
            other,
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Value;
    use crate::builtins::default_env;
    use crate::env::EnvRef;
    use crate::eval::call_callable;

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
    fn promise_deref_reports_type_mismatch_with_preview_and_arg() {
        let env = default_env();
        let err = call_err(&env, "promise-deref", vec![Value::Int(1)]);
        assert!(
            err.contains("expected promise (arg 1 to promise-deref)"),
            "unexpected message: {}",
            err
        );
        assert!(err.contains("number (1)"), "missing preview: {}", err);
    }
}
