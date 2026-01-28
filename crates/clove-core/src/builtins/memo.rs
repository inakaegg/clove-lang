use crate::ast::Vector;
use crate::ast::{callable_label, FnArity, Value};
use crate::builtins::{def_builtin, err, map_like_to_hashmap};
use crate::env::Env;
use crate::error::CloveError;
use crate::eval::call_callable;
use crate::memo_support::{
    canonical_key_for_args, default_cache_root, derive_lambda_store_name, now_ms,
    resolve_store_path, ttl_value_ms, PersistentStore, SerializationError,
};
use once_cell::sync::OnceCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

pub fn install(env: &mut Env) {
    def_builtin!(env, "memoize", FnArity::exact(1), |args| {
        match args {
            [callable] => build_memoized(callable.clone(), None),
            _ => err("memoize expects a single callable"),
        }
    });
    def_builtin!(env, "memo", FnArity::range(1, 2), |args| {
        match args {
            [callable] => build_memoized(callable.clone(), Some(MemoConfig::default())),
            [callable, opts] => {
                let config = parse_options(opts)?;
                build_memoized(callable.clone(), Some(config))
            }
            _ => err("memo expects callable and optional options map"),
        }
    });
}

#[derive(Clone)]
struct MemoValue {
    value: Value,
    saved_at_ms: i64,
}

struct MemoFn {
    callable: Value,
    entries: Mutex<HashMap<Vector<Value>, Arc<OnceCell<MemoValue>>>>,
    store: Option<Arc<PersistentStore>>,
    ttl_ms: Option<i64>,
}

impl MemoFn {
    fn new(callable: Value, store: Option<Arc<PersistentStore>>, ttl_ms: Option<i64>) -> Self {
        Self {
            callable,
            entries: Mutex::new(HashMap::new()),
            store,
            ttl_ms,
        }
    }

    fn call(&self, args: &[Value]) -> Result<Value, CloveError> {
        let key = Vector::from(args.to_vec());
        loop {
            let cell = {
                let mut guard = self.entries.lock().unwrap();
                guard
                    .entry(key.clone())
                    .or_insert_with(|| Arc::new(OnceCell::new()))
                    .clone()
            };
            if let Some(value) = cell.get() {
                if self.is_expired(value.saved_at_ms) {
                    self.entries.lock().unwrap().remove(&key);
                    continue;
                }
                return Ok(value.value.clone());
            }
            let callable = self.callable.clone();
            let args_vec = args.to_vec();
            let mut store = self.store.clone();
            let disk_key = if let Some(store_ref) = &store {
                match canonical_key_for_args(&args_vec) {
                    Ok(key) => Some(key),
                    Err(SerializationError::Unsupported(msg)) => {
                        store_ref.warn_args_unserializable(msg);
                        store = None;
                        None
                    }
                }
            } else {
                None
            };
            let value_ref = cell.get_or_try_init(|| {
                if let (Some(store_ref), Some(ref key)) = (&store, &disk_key) {
                    match store_ref.try_load(key) {
                        Ok(Some(entry)) => {
                            return Ok::<MemoValue, CloveError>(MemoValue {
                                value: entry.value,
                                saved_at_ms: entry.saved_at_ms,
                            });
                        }
                        Ok(None) => {}
                        Err(err) => {
                            eprintln!(
                                "memo: failed to read cache '{}': {}",
                                store_ref.dir().display(),
                                err
                            );
                        }
                    }
                }
                let value = call_callable(callable.clone(), args_vec.clone())?;
                let memo_value = MemoValue {
                    value: value.clone(),
                    saved_at_ms: now_ms(),
                };
                if let (Some(store_ref), Some(ref key)) = (&store, &disk_key) {
                    if let Err(err) = store_ref.save(key, &memo_value.value) {
                        eprintln!(
                            "memo: failed to persist cache '{}': {}",
                            store_ref.dir().display(),
                            err
                        );
                    }
                }
                Ok(memo_value)
            })?;
            if self.is_expired(value_ref.saved_at_ms) {
                self.entries.lock().unwrap().remove(&key);
                continue;
            }
            return Ok(value_ref.value.clone());
        }
    }

    fn is_expired(&self, saved_at_ms: i64) -> bool {
        match self.ttl_ms {
            Some(ttl) => now_ms().saturating_sub(saved_at_ms) > ttl,
            None => false,
        }
    }
}

fn build_memoized(callable: Value, config: Option<MemoConfig>) -> Result<Value, CloveError> {
    if !is_callable(&callable) {
        return err("memo functions expect a callable");
    }
    let (store, ttl_ms) = match config {
        Some(cfg) => {
            let ttl = cfg.ttl_ms;
            let store = if cfg.store_path.is_some() {
                Some(cfg.into_store(&callable)?)
            } else {
                None
            };
            (store, ttl)
        }
        None => (None, None),
    };
    let label = callable_label(&callable).unwrap_or_else(|| "<fn>".into());
    let debug_name = format!("memoized {}", label);
    let memo_fn = Arc::new(MemoFn::new(callable, store, ttl_ms));
    Ok(Value::native_fn_with_name(
        debug_name,
        FnArity::at_least(0),
        move |args| memo_fn.call(args),
    ))
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

#[derive(Clone)]
struct MemoConfig {
    ttl_ms: Option<i64>,
    store_path: Option<PathBuf>,
}

impl Default for MemoConfig {
    fn default() -> Self {
        Self {
            ttl_ms: None,
            store_path: None,
        }
    }
}

impl MemoConfig {
    fn into_store(&self, callable: &Value) -> Result<Arc<PersistentStore>, CloveError> {
        let path = if let Some(path) = &self.store_path {
            path.clone()
        } else if let Some(auto) = derive_lambda_store_name(callable) {
            default_cache_root().join("auto").join(auto)
        } else {
            return Err(CloveError::runtime(
                "memo requires :store when callable has no stable identity",
            ));
        };
        Ok(Arc::new(PersistentStore::new(path, self.ttl_ms)))
    }
}

fn parse_options(value: &Value) -> Result<MemoConfig, CloveError> {
    use crate::ast::Key;
    let map = map_like_to_hashmap(value, "memo", 2)?;
    let mut config = MemoConfig::default();
    for (key, val) in map.iter() {
        let name = match key {
            Key::Keyword(s) | Key::Symbol(s) => s.as_str(),
            Key::String(s) => s.as_str(),
            _ => continue,
        };
        match name {
            "ttl" => config.ttl_ms = ttl_value_ms(val)?,
            "store" => {
                if let Value::String(path) = val {
                    config.store_path = Some(resolve_store_path(path));
                } else {
                    return err(":store option expects string");
                }
            }
            _ => {}
        }
    }
    Ok(config)
}
