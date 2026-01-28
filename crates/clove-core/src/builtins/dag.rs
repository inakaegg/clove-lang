use crate::ast::{FnArity, HashMap, Key, Value, Vector};
use crate::builtins::{
    def_builtin, err, map_like_to_hashmap, seq_items, truthy, type_mismatch_arg,
};
use crate::concurrency::spawn_with_current_file;
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::call_callable;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::runtime::{RuntimeCtx, RuntimeGuard, RuntimeRequirementGuard};
use crate::type_registry;
use crate::types::TypeKind;
use crossbeam_channel::{bounded, unbounded, TrySendError};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use std::thread;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DagOnError {
    Throw,
    Cancel,
    Skip,
    Collect,
}

impl DagOnError {
    fn as_keyword(self) -> &'static str {
        match self {
            DagOnError::Throw => "throw",
            DagOnError::Cancel => "cancel",
            DagOnError::Skip => "skip",
            DagOnError::Collect => "collect",
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct DagOpts {
    pub(crate) workers: usize,
    pub(crate) chunk: usize,
    pub(crate) buffer: usize,
    pub(crate) ordered: bool,
    pub(crate) on_error: DagOnError,
}

static DAG_DEFAULTS: OnceLock<DagOpts> = OnceLock::new();
static DAG_DEFAULT_MAP: OnceLock<HashMap<Key, Value>> = OnceLock::new();

pub(crate) fn install(env: &mut Env) {
    def_builtin!(env, "dag::defaults", FnArity::exact(0), |_args| {
        Ok(Value::Map(dag_defaults_map().clone()))
    });
    def_builtin!(
        env,
        "dag::normalize-opts",
        FnArity::exact(1),
        |args| match args {
            [value] => {
                let opts = normalize_opts_value(Some(value), "dag::normalize-opts", 1)?;
                Ok(Value::Map(opts_to_map(&opts)))
            }
            _ => err("dag::normalize-opts expects opts map"),
        }
    );
    def_builtin!(env, "dag::pmap", FnArity::range(2, 3), |args| {
        let (func, coll, opts, opts_arg) = match args {
            [func, coll] => (func, coll, None, 0),
            [first, second, third] => match first {
                Value::Map(_) | Value::MutMap(_) | Value::SortedMap(_) => {
                    (second, third, Some(first), 1)
                }
                _ => (first, second, Some(third), 3),
            },
            _ => return err("dag::pmap expects function, collection, and optional opts map"),
        };
        let opts = normalize_opts_value(opts, "dag::pmap", opts_arg)?;
        pmap_execute(func.clone(), coll.clone(), opts)
    });
    def_builtin!(env, "dag::pfilter", FnArity::range(2, 3), |args| {
        let (pred, coll, opts, opts_arg) = match args {
            [pred, coll] => (pred, coll, None, 0),
            [first, second, third] => match first {
                Value::Map(_) | Value::MutMap(_) | Value::SortedMap(_) => {
                    (second, third, Some(first), 1)
                }
                _ => (first, second, Some(third), 3),
            },
            _ => return err("dag::pfilter expects predicate, collection, and optional opts map"),
        };
        let opts = normalize_opts_value(opts, "dag::pfilter", opts_arg)?;
        if opts.on_error == DagOnError::Collect {
            return err("dag::pfilter :on-error :collect is not supported yet");
        }
        pfilter_execute(pred.clone(), coll.clone(), opts)
    });
    install_fn_meta();
}

fn install_fn_meta() {
    fn map_any() -> TypeKind {
        TypeKind::map(TypeKind::Any, TypeKind::Any)
    }
    fn opts_record() -> TypeKind {
        let mut fields = im::HashMap::new();
        fields.insert("max-parallel".to_string(), TypeKind::Int);
        fields.insert("workers".to_string(), TypeKind::Int);
        fields.insert("chunk".to_string(), TypeKind::Int);
        fields.insert("buffer".to_string(), TypeKind::Int);
        fields.insert("ordered?".to_string(), TypeKind::Bool);
        fields.insert("on-error".to_string(), TypeKind::Any);
        TypeKind::Record(fields)
    }
    fn overload(args: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> FnOverload {
        FnOverload {
            arg_types: args,
            rest,
            ret_type: ret,
            special_op: None,
        }
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

    register_with_subject(
        "dag::defaults",
        &["[]"],
        vec![overload(vec![], None, map_any())],
        SubjectPos::Fixed(1),
    );
    register_with_subject(
        "dag::normalize-opts",
        &["[opts]"],
        vec![overload(vec![map_any()], None, map_any())],
        SubjectPos::Fixed(1),
    );
    register_with_subject(
        "dag::pmap",
        &["[f coll]", "[f coll opts]", "[opts f coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
    register_with_subject(
        "dag::pfilter",
        &["[pred coll]", "[pred coll opts]", "[opts pred coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
    register_with_subject(
        "std::pmap",
        &["[f coll]", "[f coll opts]", "[opts f coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
    register_with_subject(
        "std::pfilter",
        &["[pred coll]", "[pred coll opts]", "[opts pred coll]"],
        vec![
            overload(vec![TypeKind::Any, TypeKind::Any], None, TypeKind::Any),
            overload(
                vec![TypeKind::Any, TypeKind::Any, map_any()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![TypeKind::Any, TypeKind::Any, opts_record()],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![map_any(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
            overload(
                vec![opts_record(), TypeKind::Any, TypeKind::Any],
                None,
                TypeKind::Any,
            ),
        ],
        SubjectPos::Fixed(2),
    );
}

fn dag_defaults_opts() -> DagOpts {
    *DAG_DEFAULTS.get_or_init(|| {
        let workers = thread::available_parallelism()
            .ok()
            .map(|n| n.get())
            .unwrap_or(1)
            .max(1);
        DagOpts {
            workers,
            chunk: 64,
            buffer: workers.saturating_mul(4),
            ordered: true,
            on_error: DagOnError::Throw,
        }
    })
}

fn dag_defaults_map() -> &'static HashMap<Key, Value> {
    DAG_DEFAULT_MAP.get_or_init(|| opts_to_map(&dag_defaults_opts()))
}

fn opts_to_map(opts: &DagOpts) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    map.insert(
        Key::Keyword("workers".into()),
        Value::Int(opts.workers as i64),
    );
    map.insert(Key::Keyword("chunk".into()), Value::Int(opts.chunk as i64));
    map.insert(
        Key::Keyword("buffer".into()),
        Value::Int(opts.buffer as i64),
    );
    map.insert(Key::Keyword("ordered?".into()), Value::Bool(opts.ordered));
    map.insert(
        Key::Keyword("on-error".into()),
        Value::Symbol(format!(":{}", opts.on_error.as_keyword())),
    );
    map
}

pub(crate) fn normalize_opts_value(
    value: Option<&Value>,
    op: &str,
    arg_index: usize,
) -> Result<DagOpts, CloveError> {
    let defaults = dag_defaults_opts();
    let Some(value) = value else {
        return Ok(defaults);
    };
    if matches!(value, Value::Nil) {
        return Ok(defaults);
    }
    let map = map_like_to_hashmap(value, op, arg_index)?;
    normalize_opts_map(&map, op, arg_index)
}

fn normalize_opts_map(
    map: &HashMap<Key, Value>,
    op: &str,
    arg_index: usize,
) -> Result<DagOpts, CloveError> {
    let allowed = [
        "max-parallel",
        "workers",
        "chunk",
        "buffer",
        "ordered?",
        "on-error",
    ];
    for key in map.keys() {
        let Some(name) = key_name(key) else {
            return Err(CloveError::runtime(format!(
                "{op} opts keys must be keywords or symbols"
            )));
        };
        if !allowed.contains(&name) {
            return Err(CloveError::runtime(format!("{op} unknown option :{name}")));
        }
    }
    let workers = parse_opt_int(map, "workers", op, arg_index)?;
    if matches!(workers, Some(n) if n < 1) {
        return Err(CloveError::runtime(format!("{op} :workers must be >= 1")));
    }
    let max_parallel = parse_opt_int(map, "max-parallel", op, arg_index)?;
    if matches!(max_parallel, Some(n) if n < 1) {
        return Err(CloveError::runtime(format!(
            "{op} :max-parallel must be >= 1"
        )));
    }
    if workers.is_some() && max_parallel.is_some() {
        return Err(CloveError::runtime(format!(
            "{op} cannot use :workers and :max-parallel together"
        )));
    }
    let defaults = dag_defaults_opts();
    let workers = workers.or(max_parallel).unwrap_or(defaults.workers as i64);
    let workers = workers as usize;
    let chunk = parse_opt_int(map, "chunk", op, arg_index)?.unwrap_or(defaults.chunk as i64);
    if chunk < 1 {
        return Err(CloveError::runtime(format!("{op} :chunk must be >= 1")));
    }
    let chunk = chunk as usize;
    let buffer =
        parse_opt_int(map, "buffer", op, arg_index)?.unwrap_or((workers.saturating_mul(4)) as i64);
    if buffer < 0 {
        return Err(CloveError::runtime(format!("{op} :buffer must be >= 0")));
    }
    let buffer = buffer as usize;
    let ordered = parse_opt_bool(map, "ordered?", op, arg_index)?.unwrap_or(defaults.ordered);
    let on_error = parse_opt_on_error(map, "on-error", op, arg_index)?.unwrap_or(defaults.on_error);
    Ok(DagOpts {
        workers,
        chunk,
        buffer,
        ordered,
        on_error,
    })
}

fn key_name(key: &Key) -> Option<&str> {
    match key {
        Key::Keyword(name) | Key::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn lookup_key<'a>(map: &'a HashMap<Key, Value>, name: &str) -> Option<&'a Value> {
    map.iter().find_map(|(k, v)| match k {
        Key::Keyword(key) | Key::Symbol(key) if key == name => Some(v),
        _ => None,
    })
}

fn parse_opt_int(
    map: &HashMap<Key, Value>,
    name: &str,
    op: &str,
    arg_index: usize,
) -> Result<Option<i64>, CloveError> {
    let Some(value) = lookup_key(map, name) else {
        return Ok(None);
    };
    match value {
        Value::Int(n) => Ok(Some(*n)),
        other => Err(type_mismatch_arg("int", op, arg_index, other)),
    }
}

fn parse_opt_bool(
    map: &HashMap<Key, Value>,
    name: &str,
    op: &str,
    arg_index: usize,
) -> Result<Option<bool>, CloveError> {
    let Some(value) = lookup_key(map, name) else {
        return Ok(None);
    };
    match value {
        Value::Bool(b) => Ok(Some(*b)),
        other => Err(type_mismatch_arg("bool", op, arg_index, other)),
    }
}

fn parse_opt_on_error(
    map: &HashMap<Key, Value>,
    name: &str,
    op: &str,
    arg_index: usize,
) -> Result<Option<DagOnError>, CloveError> {
    let Some(value) = lookup_key(map, name) else {
        return Ok(None);
    };
    let raw = match value {
        Value::Symbol(sym) | Value::String(sym) => sym.trim_start_matches(':'),
        other => return Err(type_mismatch_arg("keyword", op, arg_index, other)),
    };
    let parsed = match raw {
        "throw" => DagOnError::Throw,
        "cancel" => DagOnError::Cancel,
        "skip" => DagOnError::Skip,
        "collect" => DagOnError::Collect,
        _ => {
            return Err(CloveError::runtime(format!(
                "{op} :on-error must be :throw, :cancel, :skip, or :collect"
            )))
        }
    };
    Ok(Some(parsed))
}

fn dag_result_ok(value: Value) -> Value {
    let mut data = HashMap::new();
    data.insert(Key::Keyword("value".into()), value);
    Value::Map(type_registry::add_type_tag(data, "dag::Result::Ok"))
}

fn dag_result_err(error: Value) -> Value {
    let mut data = HashMap::new();
    data.insert(Key::Keyword("error".into()), error);
    Value::Map(type_registry::add_type_tag(data, "dag::Result::Err"))
}

fn error_to_value(err: &CloveError) -> Value {
    Value::String(err.to_string())
}

#[derive(Debug)]
struct TaskItem {
    idx: usize,
    value: Value,
}

#[derive(Debug)]
enum PmapMsg {
    Ok { idx: usize, value: Value },
    Err { idx: usize, error: CloveError },
}

#[derive(Debug)]
enum PfilterMsg {
    Ok {
        idx: usize,
        keep: bool,
        value: Value,
    },
    Err {
        idx: usize,
        error: CloveError,
    },
}

pub(crate) fn pmap_execute(func: Value, coll: Value, opts: DagOpts) -> Result<Value, CloveError> {
    let items = seq_items(&coll)?;
    let total = items.len();
    if total == 0 {
        return Ok(Value::Vector(Vector::new()));
    }
    let workers = opts.workers.min(total).max(1);
    let (task_tx, task_rx) = bounded::<Vec<TaskItem>>(opts.buffer);
    let (res_tx, res_rx) = unbounded::<PmapMsg>();
    let cancel = Arc::new(AtomicBool::new(false));
    let mut handles = Vec::with_capacity(workers);
    for _ in 0..workers {
        let task_rx = task_rx.clone();
        let res_tx = res_tx.clone();
        let func = func.clone();
        let cancel = cancel.clone();
        let runtime_info =
            RuntimeCtx::current_handle().map(|handle| (handle.clone(), handle.task_guard()));
        let handle = spawn_with_current_file(move || {
            let (_runtime_guard, _task_guard, _require_guard) =
                if let Some((handle, guard)) = runtime_info {
                    (Some(RuntimeGuard::set(handle)), Some(guard), None)
                } else {
                    (None, None, Some(RuntimeRequirementGuard::set(true)))
                };
            while let Ok(chunk) = task_rx.recv() {
                if cancel.load(Ordering::Relaxed) {
                    break;
                }
                for task in chunk {
                    if cancel.load(Ordering::Relaxed) {
                        return;
                    }
                    let result = call_callable(func.clone(), vec![task.value]);
                    let msg = match result {
                        Ok(value) => PmapMsg::Ok {
                            idx: task.idx,
                            value,
                        },
                        Err(error) => PmapMsg::Err {
                            idx: task.idx,
                            error,
                        },
                    };
                    if res_tx.send(msg).is_err() {
                        return;
                    }
                }
            }
        });
        handles.push(handle);
    }
    drop(task_rx);
    drop(res_tx);
    let producer_cancel = cancel.clone();
    let producer_handle = spawn_with_current_file(move || {
        let mut iter = items.into_iter().enumerate();
        loop {
            if producer_cancel.load(Ordering::Relaxed) {
                break;
            }
            let mut chunk_items = Vec::new();
            for _ in 0..opts.chunk {
                if let Some((idx, value)) = iter.next() {
                    chunk_items.push(TaskItem { idx, value });
                } else {
                    break;
                }
            }
            if chunk_items.is_empty() {
                break;
            }
            loop {
                if producer_cancel.load(Ordering::Relaxed) {
                    return;
                }
                match task_tx.try_send(chunk_items) {
                    Ok(()) => break,
                    Err(TrySendError::Full(items)) => {
                        chunk_items = items;
                        thread::yield_now();
                    }
                    Err(TrySendError::Disconnected(_)) => return,
                }
            }
        }
    });
    let mut received = 0usize;
    let mut error = None;
    let mut ordered_results = if opts.ordered {
        Some(vec![None; total])
    } else {
        None
    };
    let mut unordered_results: Vec<Value> = Vec::with_capacity(total);
    while received < total {
        let msg = match res_rx.recv() {
            Ok(msg) => msg,
            Err(_) => break,
        };
        received += 1;
        match msg {
            PmapMsg::Ok { idx, value } => match opts.on_error {
                DagOnError::Collect => {
                    let wrapped = dag_result_ok(value);
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = Some(wrapped);
                    } else {
                        unordered_results.push(wrapped);
                    }
                }
                _ => {
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = Some(value);
                    } else {
                        unordered_results.push(value);
                    }
                }
            },
            PmapMsg::Err { idx, error: err } => match opts.on_error {
                DagOnError::Throw | DagOnError::Cancel => {
                    cancel.store(true, Ordering::Relaxed);
                    error = Some(err);
                    break;
                }
                DagOnError::Skip => {
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = None;
                    }
                }
                DagOnError::Collect => {
                    let wrapped = dag_result_err(error_to_value(&err));
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = Some(wrapped);
                    } else {
                        unordered_results.push(wrapped);
                    }
                }
            },
        }
    }
    cancel.store(true, Ordering::Relaxed);
    drop(res_rx);
    let _ = producer_handle.join();
    for handle in handles {
        let _ = handle.join();
    }
    if let Some(err) = error {
        return Err(err);
    }
    let out = if let Some(results) = ordered_results {
        results.into_iter().filter_map(|v| v).collect::<Vector<_>>()
    } else {
        unordered_results.into_iter().collect::<Vector<_>>()
    };
    Ok(Value::Vector(out))
}

pub(crate) fn pfilter_execute(
    pred: Value,
    coll: Value,
    opts: DagOpts,
) -> Result<Value, CloveError> {
    let items = seq_items(&coll)?;
    let total = items.len();
    if total == 0 {
        return Ok(Value::Vector(Vector::new()));
    }
    let workers = opts.workers.min(total).max(1);
    let (task_tx, task_rx) = bounded::<Vec<TaskItem>>(opts.buffer);
    let (res_tx, res_rx) = unbounded::<PfilterMsg>();
    let cancel = Arc::new(AtomicBool::new(false));
    let mut handles = Vec::with_capacity(workers);
    for _ in 0..workers {
        let task_rx = task_rx.clone();
        let res_tx = res_tx.clone();
        let pred = pred.clone();
        let cancel = cancel.clone();
        let runtime_info =
            RuntimeCtx::current_handle().map(|handle| (handle.clone(), handle.task_guard()));
        let handle = spawn_with_current_file(move || {
            let (_runtime_guard, _task_guard, _require_guard) =
                if let Some((handle, guard)) = runtime_info {
                    (Some(RuntimeGuard::set(handle)), Some(guard), None)
                } else {
                    (None, None, Some(RuntimeRequirementGuard::set(true)))
                };
            while let Ok(chunk) = task_rx.recv() {
                if cancel.load(Ordering::Relaxed) {
                    break;
                }
                for task in chunk {
                    if cancel.load(Ordering::Relaxed) {
                        return;
                    }
                    let result = call_callable(pred.clone(), vec![task.value.clone()]);
                    let msg = match result {
                        Ok(value) => PfilterMsg::Ok {
                            idx: task.idx,
                            keep: truthy(&value),
                            value: task.value,
                        },
                        Err(error) => PfilterMsg::Err {
                            idx: task.idx,
                            error,
                        },
                    };
                    if res_tx.send(msg).is_err() {
                        return;
                    }
                }
            }
        });
        handles.push(handle);
    }
    drop(task_rx);
    drop(res_tx);
    let producer_cancel = cancel.clone();
    let producer_handle = spawn_with_current_file(move || {
        let mut iter = items.into_iter().enumerate();
        loop {
            if producer_cancel.load(Ordering::Relaxed) {
                break;
            }
            let mut chunk_items = Vec::new();
            for _ in 0..opts.chunk {
                if let Some((idx, value)) = iter.next() {
                    chunk_items.push(TaskItem { idx, value });
                } else {
                    break;
                }
            }
            if chunk_items.is_empty() {
                break;
            }
            loop {
                if producer_cancel.load(Ordering::Relaxed) {
                    return;
                }
                match task_tx.try_send(chunk_items) {
                    Ok(()) => break,
                    Err(TrySendError::Full(items)) => {
                        chunk_items = items;
                        thread::yield_now();
                    }
                    Err(TrySendError::Disconnected(_)) => return,
                }
            }
        }
    });
    let mut received = 0usize;
    let mut error = None;
    let mut ordered_results = if opts.ordered {
        Some(vec![None; total])
    } else {
        None
    };
    let mut unordered_results: Vec<Value> = Vec::with_capacity(total);
    while received < total {
        let msg = match res_rx.recv() {
            Ok(msg) => msg,
            Err(_) => break,
        };
        received += 1;
        match msg {
            PfilterMsg::Ok { idx, keep, value } => {
                if keep {
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = Some(value);
                    } else {
                        unordered_results.push(value);
                    }
                }
            }
            PfilterMsg::Err { idx, error: err } => match opts.on_error {
                DagOnError::Throw | DagOnError::Cancel => {
                    cancel.store(true, Ordering::Relaxed);
                    error = Some(err);
                    break;
                }
                DagOnError::Skip => {
                    if let Some(results) = ordered_results.as_mut() {
                        results[idx] = None;
                    }
                }
                DagOnError::Collect => {}
            },
        }
    }
    cancel.store(true, Ordering::Relaxed);
    drop(res_rx);
    let _ = producer_handle.join();
    for handle in handles {
        let _ = handle.join();
    }
    if let Some(err) = error {
        return Err(err);
    }
    let out = if let Some(results) = ordered_results {
        results.into_iter().filter_map(|v| v).collect::<Vector<_>>()
    } else {
        unordered_results.into_iter().collect::<Vector<_>>()
    };
    Ok(Value::Vector(out))
}
