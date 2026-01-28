use std::cell::RefCell;
use std::ffi::c_void;
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use clove_plugin_api::{
    Bool, BuiltinFn, EnvHandle, HostApiV1, ReaderCloseFn, ReaderErrorFn, ReaderReadFn, ValueHandle,
    CLOVE_ARITY_UNBOUNDED, CLOVE_PLUGIN_API_VERSION,
};

use crate::ast::NativeBufTy;
use crate::ast::{FnArity, HashMap, MainThreadBuf, NativeBufHandle, Value, Vector};
use crate::env::EnvRef;
use crate::error::CloveError;
use crate::eval::{call_callable, to_key_value_checked};
use crate::io_reader::{reader_value, ReaderHandle};

thread_local! {
    static HANDLE_POOL: RefCell<Vec<Vec<ValueHandle>>> = RefCell::new(Vec::new());
    static NATIVE_I32BUF_GUARDS: RefCell<Vec<Vec<NativeI32BufGuard>>> = RefCell::new(Vec::new());
}

pub struct EnvHandleBox {
    inner: Arc<EnvHandleInner>,
    handle: EnvHandle,
}

struct EnvHandleInner {
    env: EnvRef,
    alive: AtomicBool,
    runtime_id: usize,
}

// EnvHandleBox holds an immutable pointer; the value is managed by Arc.
unsafe impl Send for EnvHandleBox {}
unsafe impl Sync for EnvHandleBox {}

impl EnvHandleBox {
    pub fn new(env: EnvRef, runtime_id: usize) -> Self {
        let inner = Arc::new(EnvHandleInner {
            env,
            alive: AtomicBool::new(true),
            runtime_id,
        });
        let handle = Arc::into_raw(inner.clone()) as EnvHandle;
        Self { inner, handle }
    }

    pub fn handle(&self) -> EnvHandle {
        self.handle
    }

    pub fn mark_dead(&self) {
        self.inner.alive.store(false, Ordering::SeqCst);
    }
}

impl Drop for EnvHandleBox {
    fn drop(&mut self) {
        self.mark_dead();
        unsafe {
            let _ = Arc::from_raw(self.handle.cast::<EnvHandleInner>());
        }
    }
}

#[derive(Debug)]
enum PluginValue {
    Ok(Value),
    Err(CloveError),
}

#[derive(Clone)]
struct PluginCallData {
    env: EnvHandle,
    func: BuiltinFn,
    user_data: *mut c_void,
    call_lock: Arc<Mutex<()>>,
}

unsafe impl Send for PluginCallData {}
unsafe impl Sync for PluginCallData {}

enum NativeI32BufGuardInner {
    Shared {
        guard: Option<std::sync::MutexGuard<'static, Vec<i32>>>,
        _buf: Arc<Mutex<Vec<i32>>>,
    },
    Main {
        ptr: *mut Vec<i32>,
        buf: Arc<MainThreadBuf<i32>>,
    },
}

struct NativeI32BufGuard {
    inner: NativeI32BufGuardInner,
}

impl NativeI32BufGuard {
    fn new_shared(buf: Arc<Mutex<Vec<i32>>>) -> Result<Self, CloveError> {
        let guard = unsafe {
            let ptr = Arc::as_ptr(&buf);
            let guard = (&*ptr)
                .lock()
                .map_err(|_| CloveError::runtime("native i32 buffer lock poisoned"))?;
            std::mem::transmute::<
                std::sync::MutexGuard<'_, Vec<i32>>,
                std::sync::MutexGuard<'static, Vec<i32>>,
            >(guard)
        };
        Ok(Self {
            inner: NativeI32BufGuardInner::Shared {
                guard: Some(guard),
                _buf: buf,
            },
        })
    }

    fn new_main(buf: Arc<MainThreadBuf<i32>>) -> Result<Self, CloveError> {
        let ptr = buf.begin_borrow("native i32 buffer slice")?;
        Ok(Self {
            inner: NativeI32BufGuardInner::Main { ptr, buf },
        })
    }

    fn as_slice(&self) -> &[i32] {
        match &self.inner {
            NativeI32BufGuardInner::Shared { guard, .. } => {
                guard.as_ref().map(|g| g.as_slice()).unwrap_or(&[])
            }
            NativeI32BufGuardInner::Main { ptr, .. } => unsafe { (&*(*ptr)).as_slice() },
        }
    }
}

impl Drop for NativeI32BufGuard {
    fn drop(&mut self) {
        match &mut self.inner {
            NativeI32BufGuardInner::Shared { guard, .. } => {
                guard.take();
            }
            NativeI32BufGuardInner::Main { buf, .. } => {
                buf.end_borrow();
            }
        }
    }
}

pub fn host_api_v1() -> &'static HostApiV1 {
    &HOST_API_V1
}

fn with_handle_pool<R>(f: impl FnOnce() -> R) -> (R, Vec<ValueHandle>) {
    HANDLE_POOL.with(|cell| cell.borrow_mut().push(Vec::new()));
    let result = f();
    let handles = HANDLE_POOL.with(|cell| cell.borrow_mut().pop().unwrap_or_default());
    (result, handles)
}

fn with_native_i32buf_guard_pool<R>(f: impl FnOnce() -> R) -> (R, Vec<NativeI32BufGuard>) {
    NATIVE_I32BUF_GUARDS.with(|cell| cell.borrow_mut().push(Vec::new()));
    let result = f();
    let guards = NATIVE_I32BUF_GUARDS.with(|cell| cell.borrow_mut().pop().unwrap_or_default());
    (result, guards)
}

fn track_handle(handle: ValueHandle) {
    HANDLE_POOL.with(|cell| {
        if let Some(list) = cell.borrow_mut().last_mut() {
            list.push(handle);
        }
    });
}

fn drop_handle(handle: ValueHandle) {
    if handle.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(handle as *mut PluginValue));
    }
}

fn track_native_i32buf_guard(guard: NativeI32BufGuard) {
    NATIVE_I32BUF_GUARDS.with(|cell| {
        if let Some(list) = cell.borrow_mut().last_mut() {
            list.push(guard);
        }
    });
}

unsafe fn env_from_handle(handle: EnvHandle) -> Option<EnvRef> {
    let inner = handle.cast::<EnvHandleInner>().as_ref()?;
    if !inner.alive.load(Ordering::SeqCst) {
        return None;
    }
    Some(inner.env.clone())
}

fn env_alive(handle: EnvHandle) -> bool {
    let inner = unsafe { handle.cast::<EnvHandleInner>().as_ref() };
    inner
        .map(|state| state.alive.load(Ordering::SeqCst))
        .unwrap_or(false)
}

fn ensure_env_alive_or_err(env: EnvHandle) -> Result<(), ValueHandle> {
    if env_alive(env) {
        Ok(())
    } else {
        Err(env_dead_error(env))
    }
}

fn take_out_ptrs<T>(
    out_ptr: *mut *const T,
    out_len: *mut usize,
) -> Option<(*mut *const T, *mut usize)> {
    if out_ptr.is_null() || out_len.is_null() {
        None
    } else {
        Some((out_ptr, out_len))
    }
}

unsafe fn value_ref<'a>(handle: ValueHandle) -> Option<&'a PluginValue> {
    (handle as *const PluginValue).as_ref()
}

unsafe fn value_mut<'a>(handle: ValueHandle) -> Option<&'a mut PluginValue> {
    (handle as *mut PluginValue).as_mut()
}

fn env_dead_error(handle: EnvHandle) -> ValueHandle {
    let runtime_id =
        unsafe { handle.cast::<EnvHandleInner>().as_ref() }.map(|inner| inner.runtime_id);
    let msg = match runtime_id {
        Some(id) => format!("runtime {} is already dropped", id),
        None => "runtime is already dropped".to_string(),
    };
    make_err(CloveError::runtime(msg))
}

fn make_handle(value: PluginValue) -> ValueHandle {
    let handle = Box::into_raw(Box::new(value)) as ValueHandle;
    track_handle(handle);
    handle
}

fn make_ok(value: Value) -> ValueHandle {
    make_handle(PluginValue::Ok(value))
}

fn make_err(err: CloveError) -> ValueHandle {
    make_handle(PluginValue::Err(err))
}

fn take_handle(handle: ValueHandle) -> Result<Value, CloveError> {
    if handle.is_null() {
        return Err(CloveError::runtime("plugin builtin returned null"));
    }
    let boxed = unsafe { Box::from_raw(handle as *mut PluginValue) };
    match *boxed {
        PluginValue::Ok(value) => Ok(value),
        PluginValue::Err(err) => Err(err),
    }
}

fn call_plugin_fn(call_data: &PluginCallData, args: &[Value]) -> Result<Value, CloveError> {
    let _guard = call_data
        .call_lock
        .lock()
        .map_err(|_| CloveError::runtime("plugin call lock poisoned"))?;
    let mut arg_handles = Vec::with_capacity(args.len());
    for arg in args {
        let handle = Box::into_raw(Box::new(PluginValue::Ok(arg.clone()))) as ValueHandle;
        arg_handles.push(handle);
    }
    let argv_ptr = arg_handles.as_ptr();
    let (ret_handle, handles) = with_handle_pool(|| {
        let (ret_handle, guards) = with_native_i32buf_guard_pool(|| unsafe {
            (call_data.func)(
                host_api_v1() as *const HostApiV1,
                call_data.env,
                args.len(),
                argv_ptr,
                call_data.user_data,
            )
        });
        drop(guards);
        ret_handle
    });
    for handle in handles {
        if handle != ret_handle {
            drop_handle(handle);
        }
    }
    for handle in arg_handles {
        if handle != ret_handle {
            drop_handle(handle);
        }
    }
    take_handle(ret_handle)
}

unsafe extern "C" fn define_fn_utf8(
    env: EnvHandle,
    name_ptr: *const u8,
    name_len: usize,
    arity_min: usize,
    arity_max: usize,
    func: BuiltinFn,
    user_data: *mut c_void,
) -> Bool {
    let env_ref = match env_from_handle(env) {
        Some(env_ref) => env_ref,
        None => return 0,
    };
    if name_ptr.is_null() {
        return 0;
    }
    let name = match std::str::from_utf8(unsafe { std::slice::from_raw_parts(name_ptr, name_len) })
    {
        Ok(value) => value,
        Err(_) => return 0,
    };
    let arity = if arity_max == CLOVE_ARITY_UNBOUNDED {
        FnArity::at_least(arity_min)
    } else if arity_max >= arity_min {
        FnArity::range(arity_min, arity_max)
    } else {
        return 0;
    };
    let env_handle = env;
    let func_ptr = func;
    let name_owned = name.to_string();
    let call_data = PluginCallData {
        env: env_handle,
        func: func_ptr,
        user_data,
        call_lock: Arc::new(Mutex::new(())),
    };
    let value = Value::native_fn_with_name(name_owned.clone(), arity, {
        let call_data = call_data.clone();
        move |args| call_plugin_fn(&call_data, args)
    });
    let mut writer = env_ref.write().unwrap();
    writer.define_builtin(&name_owned, value);
    1
}

unsafe extern "C" fn make_nil(_env: EnvHandle) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Nil)
}

unsafe extern "C" fn make_bool(_env: EnvHandle, value: Bool) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Bool(value != 0))
}

unsafe extern "C" fn make_int(_env: EnvHandle, value: i64) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Int(value))
}

unsafe extern "C" fn make_float(_env: EnvHandle, value: f64) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Float(value))
}

unsafe extern "C" fn make_string_utf8(_env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    if ptr.is_null() {
        return make_ok(Value::Nil);
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    match std::str::from_utf8(slice) {
        Ok(text) => make_ok(Value::String(text.to_string())),
        Err(_) => make_ok(Value::Nil),
    }
}

unsafe extern "C" fn make_bytes(_env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    if ptr.is_null() {
        return make_ok(Value::Nil);
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut vec = Vector::new();
    for b in slice {
        vec.push_back(Value::Int(*b as i64));
    }
    make_ok(Value::Vector(vec))
}

unsafe extern "C" fn make_keyword_utf8(_env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    if ptr.is_null() {
        return make_ok(Value::Nil);
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let text = match std::str::from_utf8(slice) {
        Ok(text) => text,
        Err(_) => return make_ok(Value::Nil),
    };
    let keyword = if text.starts_with(':') {
        text.to_string()
    } else {
        format!(":{}", text)
    };
    make_ok(Value::Symbol(keyword))
}

unsafe extern "C" fn make_vec(_env: EnvHandle) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Vector(Vector::new()))
}

unsafe extern "C" fn vec_push(_env: EnvHandle, vec: ValueHandle, val: ValueHandle) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let vec_ref = unsafe { value_mut(vec) };
    let val_ref = unsafe { value_ref(val) };
    let (vec_ref, val_ref) = match (vec_ref, val_ref) {
        (Some(vec_ref), Some(val_ref)) => (vec_ref, val_ref),
        _ => return 0,
    };
    match (vec_ref, val_ref) {
        (PluginValue::Ok(Value::Vector(items)), PluginValue::Ok(value)) => {
            items.push_back(value.clone());
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn vec_len(_env: EnvHandle, vec: ValueHandle, out: *mut usize) -> Bool {
    let vec_ref = unsafe { value_ref(vec) };
    let out = unsafe { out.as_mut() };
    let (vec_ref, out) = match (vec_ref, out) {
        (Some(vec_ref), Some(out)) => (vec_ref, out),
        _ => return 0,
    };
    match vec_ref {
        PluginValue::Ok(Value::Vector(items)) | PluginValue::Ok(Value::List(items)) => {
            *out = items.len();
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn vec_get(_env: EnvHandle, vec: ValueHandle, idx: usize) -> ValueHandle {
    let vec_ref = unsafe { value_ref(vec) };
    let vec_ref = match vec_ref {
        Some(vec_ref) => vec_ref,
        None => return make_ok(Value::Nil),
    };
    match vec_ref {
        PluginValue::Ok(Value::Vector(items)) | PluginValue::Ok(Value::List(items)) => {
            if let Some(value) = items.get(idx) {
                make_ok(value.clone())
            } else {
                make_ok(Value::Nil)
            }
        }
        _ => make_ok(Value::Nil),
    }
}

unsafe extern "C" fn make_map(_env: EnvHandle) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    make_ok(Value::Map(HashMap::new()))
}

unsafe extern "C" fn map_put(
    _env: EnvHandle,
    map: ValueHandle,
    key: ValueHandle,
    val: ValueHandle,
) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let map_ref = unsafe { value_mut(map) };
    let key_ref = unsafe { value_ref(key) };
    let val_ref = unsafe { value_ref(val) };
    let (map_ref, key_ref, val_ref) = match (map_ref, key_ref, val_ref) {
        (Some(map_ref), Some(key_ref), Some(val_ref)) => (map_ref, key_ref, val_ref),
        _ => return 0,
    };
    match (map_ref, key_ref, val_ref) {
        (PluginValue::Ok(Value::Map(map)), PluginValue::Ok(key_val), PluginValue::Ok(val_val)) => {
            match to_key_value_checked(key_val) {
                Ok(key) => {
                    map.insert(key, val_val.clone());
                    1
                }
                Err(_) => 0,
            }
        }
        _ => 0,
    }
}

unsafe extern "C" fn map_get(_env: EnvHandle, map: ValueHandle, key: ValueHandle) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    let map_ref = unsafe { value_ref(map) };
    let key_ref = unsafe { value_ref(key) };
    let (map_ref, key_ref) = match (map_ref, key_ref) {
        (Some(map_ref), Some(key_ref)) => (map_ref, key_ref),
        _ => return make_ok(Value::Nil),
    };
    match (map_ref, key_ref) {
        (PluginValue::Ok(Value::Map(map)), PluginValue::Ok(key_val)) => {
            match to_key_value_checked(key_val) {
                Ok(key) => {
                    if let Some(value) = map.get(&key) {
                        make_ok(value.clone())
                    } else {
                        make_ok(Value::Nil)
                    }
                }
                Err(_) => make_ok(Value::Nil),
            }
        }
        _ => make_ok(Value::Nil),
    }
}

unsafe extern "C" fn as_int(_env: EnvHandle, v: ValueHandle, out: *mut i64) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let value = unsafe { value_ref(v) };
    let out = unsafe { out.as_mut() };
    let (value, out) = match (value, out) {
        (Some(value), Some(out)) => (value, out),
        _ => return 0,
    };
    match value {
        PluginValue::Ok(Value::Int(n)) => {
            *out = *n;
            1
        }
        PluginValue::Ok(Value::Float(n)) => {
            *out = *n as i64;
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn as_float(_env: EnvHandle, v: ValueHandle, out: *mut f64) -> Bool {
    let value = unsafe { value_ref(v) };
    let out = unsafe { out.as_mut() };
    let (value, out) = match (value, out) {
        (Some(value), Some(out)) => (value, out),
        _ => return 0,
    };
    match value {
        PluginValue::Ok(Value::Int(n)) => {
            *out = *n as f64;
            1
        }
        PluginValue::Ok(Value::Float(n)) => {
            *out = *n;
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn as_bool(_env: EnvHandle, v: ValueHandle, out: *mut Bool) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let value = unsafe { value_ref(v) };
    let out = unsafe { out.as_mut() };
    let (value, out) = match (value, out) {
        (Some(value), Some(out)) => (value, out),
        _ => return 0,
    };
    match value {
        PluginValue::Ok(Value::Bool(b)) => {
            *out = if *b { 1 } else { 0 };
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn as_string_utf8(
    _env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let value = unsafe { value_ref(v) };
    let (out_ptr, out_len) = match take_out_ptrs(out_ptr, out_len) {
        Some((out_ptr, out_len)) => (out_ptr, out_len),
        _ => return 0,
    };
    match value {
        Some(PluginValue::Ok(Value::String(s))) => {
            unsafe {
                *out_ptr = s.as_ptr();
                *out_len = s.len();
            }
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn as_symbol_utf8(
    _env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) -> Bool {
    if !env_alive(_env) {
        return 0;
    }
    let value = unsafe { value_ref(v) };
    let (out_ptr, out_len) = match take_out_ptrs(out_ptr, out_len) {
        Some((out_ptr, out_len)) => (out_ptr, out_len),
        _ => return 0,
    };
    match value {
        Some(PluginValue::Ok(Value::Symbol(s))) => {
            unsafe {
                *out_ptr = s.as_ptr();
                *out_len = s.len();
            }
            1
        }
        _ => 0,
    }
}

unsafe extern "C" fn native_i32buf_slice(
    env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const i32,
    out_len: *mut usize,
) -> Bool {
    if !env_alive(env) {
        return 0;
    }
    let value = unsafe { value_ref(v) };
    let (out_ptr, out_len) = match take_out_ptrs(out_ptr, out_len) {
        Some((out_ptr, out_len)) => (out_ptr, out_len),
        _ => return 0,
    };
    let guard = match value {
        Some(PluginValue::Ok(Value::NativeBuf {
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32(buf),
            ..
        })) => NativeI32BufGuard::new_shared(buf.clone()),
        Some(PluginValue::Ok(Value::NativeBuf {
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32Main(buf),
            ..
        })) => NativeI32BufGuard::new_main(buf.clone()),
        _ => return 0,
    };
    let guard = match guard {
        Ok(guard) => guard,
        Err(_) => return 0,
    };
    let slice = guard.as_slice();
    unsafe {
        *out_ptr = slice.as_ptr();
        *out_len = slice.len();
    }
    track_native_i32buf_guard(guard);
    1
}

unsafe extern "C" fn is_nil(_env: EnvHandle, v: ValueHandle) -> Bool {
    let value = unsafe { value_ref(v) };
    match value {
        Some(PluginValue::Ok(Value::Nil)) => 1,
        _ => 0,
    }
}

unsafe extern "C" fn raise_runtime_error_utf8(
    _env: EnvHandle,
    ptr: *const u8,
    len: usize,
) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(_env) {
        return handle;
    }
    if ptr.is_null() {
        return make_err(CloveError::runtime("plugin runtime error"));
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let message = std::str::from_utf8(slice).unwrap_or("plugin runtime error");
    make_err(CloveError::runtime(message.to_string()))
}

unsafe extern "C" fn make_reader_handle(
    env: EnvHandle,
    read_fn: ReaderReadFn,
    close_fn: ReaderCloseFn,
    error_fn: ReaderErrorFn,
    ctx: *mut c_void,
) -> ValueHandle {
    if let Err(handle) = ensure_env_alive_or_err(env) {
        return handle;
    }
    let handle = ReaderHandle::from_callbacks(read_fn, close_fn, error_fn, ctx);
    make_ok(reader_value(handle))
}

unsafe extern "C" fn call_value(
    env: EnvHandle,
    func: ValueHandle,
    argc: usize,
    argv: *const ValueHandle,
    out_ok: *mut Bool,
) -> ValueHandle {
    if !env_alive(env) {
        if let Some(out_ok) = unsafe { out_ok.as_mut() } {
            *out_ok = 0;
        }
        return env_dead_error(env);
    }
    let func_value = match unsafe { value_ref(func) } {
        Some(PluginValue::Ok(value)) => value.clone(),
        Some(PluginValue::Err(err)) => {
            if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                *out_ok = 0;
            }
            return make_err(err.clone());
        }
        None => {
            if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                *out_ok = 0;
            }
            return make_err(CloveError::runtime("invalid callable handle"));
        }
    };
    let mut args = Vec::with_capacity(argc);
    if argc > 0 && argv.is_null() {
        if let Some(out_ok) = unsafe { out_ok.as_mut() } {
            *out_ok = 0;
        }
        return make_err(CloveError::runtime("call args are missing"));
    }
    for idx in 0..argc {
        let handle = unsafe { *argv.add(idx) };
        let value = match unsafe { value_ref(handle) } {
            Some(PluginValue::Ok(value)) => value.clone(),
            Some(PluginValue::Err(err)) => {
                if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                    *out_ok = 0;
                }
                return make_err(err.clone());
            }
            None => {
                if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                    *out_ok = 0;
                }
                return make_err(CloveError::runtime("invalid argument handle"));
            }
        };
        args.push(value);
    }
    match call_callable(func_value, args) {
        Ok(value) => {
            if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                *out_ok = 1;
            }
            make_ok(value)
        }
        Err(err) => {
            if let Some(out_ok) = unsafe { out_ok.as_mut() } {
                *out_ok = 0;
            }
            make_err(err)
        }
    }
}

static HOST_API_V1: HostApiV1 = HostApiV1 {
    api_version: CLOVE_PLUGIN_API_VERSION,
    size: mem::size_of::<HostApiV1>(),
    define_fn_utf8,
    make_nil,
    make_bool,
    make_int,
    make_float,
    make_string_utf8,
    make_bytes,
    make_keyword_utf8,
    make_vec,
    vec_push,
    vec_len,
    vec_get,
    make_map,
    map_put,
    map_get,
    as_int,
    as_float,
    as_bool,
    as_string_utf8,
    as_symbol_utf8,
    is_nil,
    raise_runtime_error_utf8,
    native_i32buf_slice,
    make_reader_handle,
    call_value,
};
