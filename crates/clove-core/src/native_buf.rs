use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex, Weak};

use crate::ast::{MainThreadBuf, NativeBufHandle, NativeBufTy, Value};
use crate::error::CloveError;
use crate::runtime::RuntimeCtx;

#[derive(Clone)]
pub enum NativeBuf {
    I32(Arc<Mutex<Vec<i32>>>),
    I32Main(Arc<MainThreadBuf<i32>>),
    F32(Arc<Mutex<Vec<f32>>>),
    F32Main(Arc<MainThreadBuf<f32>>),
    I64(Arc<Mutex<Vec<i64>>>),
    I64Main(Arc<MainThreadBuf<i64>>),
}

const NATIVE_BUF_CACHE_LIMIT: usize = 256;

thread_local! {
    static I32BUF_CACHE: RefCell<HashMap<u64, Weak<Mutex<Vec<i32>>>>> = RefCell::new(HashMap::new());
    static F32BUF_CACHE: RefCell<HashMap<u64, Weak<Mutex<Vec<f32>>>>> = RefCell::new(HashMap::new());
    static I64BUF_CACHE: RefCell<HashMap<u64, Weak<Mutex<Vec<i64>>>>> = RefCell::new(HashMap::new());
}

thread_local! {
    static I32BUF_GUARDS: RefCell<HashMap<u64, Rc<RefCell<NativeBufBorrow<i32>>>>> =
        RefCell::new(HashMap::new());
    static F32BUF_GUARDS: RefCell<HashMap<u64, Rc<RefCell<NativeBufBorrow<f32>>>>> =
        RefCell::new(HashMap::new());
    static I64BUF_GUARDS: RefCell<HashMap<u64, Rc<RefCell<NativeBufBorrow<i64>>>>> =
        RefCell::new(HashMap::new());
}

struct NativeBufScopedGuard<T: 'static> {
    guard: Option<std::sync::MutexGuard<'static, Vec<T>>>,
    _buf: Arc<Mutex<Vec<T>>>,
}

impl<T: 'static> NativeBufScopedGuard<T> {
    fn new(buf: Arc<Mutex<Vec<T>>>, op: &str) -> Result<Self, CloveError> {
        let guard = unsafe {
            let ptr = Arc::as_ptr(&buf);
            let guard = (&*ptr)
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            std::mem::transmute::<
                std::sync::MutexGuard<'_, Vec<T>>,
                std::sync::MutexGuard<'static, Vec<T>>,
            >(guard)
        };
        Ok(Self {
            guard: Some(guard),
            _buf: buf,
        })
    }

    fn with_mut<R>(
        &mut self,
        op: &str,
        f: impl FnOnce(&mut Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        let mut guard = self
            .guard
            .take()
            .ok_or_else(|| CloveError::runtime(format!("{op} native buffer already borrowed")))?;
        let result = f(&mut guard);
        self.guard = Some(guard);
        result
    }

    fn with_ref<R>(
        &mut self,
        op: &str,
        f: impl FnOnce(&Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        let mut guard = self
            .guard
            .take()
            .ok_or_else(|| CloveError::runtime(format!("{op} native buffer already borrowed")))?;
        let result = f(&guard);
        self.guard = Some(guard);
        result
    }
}

#[derive(Clone)]
pub enum NativeBufAccess<T> {
    Shared(Arc<Mutex<Vec<T>>>),
    Main(Arc<MainThreadBuf<T>>),
}

struct InUseGuard<'a> {
    flag: &'a Cell<bool>,
}

impl<'a> InUseGuard<'a> {
    fn new(flag: &'a Cell<bool>) -> Self {
        Self { flag }
    }
}

impl<'a> Drop for InUseGuard<'a> {
    fn drop(&mut self) {
        self.flag.set(false);
    }
}

struct MainThreadBufBorrow<T> {
    buf: Arc<MainThreadBuf<T>>,
    in_use: Cell<bool>,
}

impl<T> MainThreadBufBorrow<T> {
    fn new(buf: Arc<MainThreadBuf<T>>, op: &str) -> Result<Self, CloveError> {
        let _ = buf.begin_borrow(op)?;
        Ok(Self {
            buf,
            in_use: Cell::new(false),
        })
    }

    fn with_mut<R>(
        &self,
        op: &str,
        f: impl FnOnce(&mut Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        if self.in_use.replace(true) {
            return Err(CloveError::runtime(format!(
                "{op} native buffer already borrowed"
            )));
        }
        let _guard = InUseGuard::new(&self.in_use);
        let ptr = self.buf.data_ptr();
        unsafe { f(&mut *ptr) }
    }

    fn with_ref<R>(
        &self,
        op: &str,
        f: impl FnOnce(&Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        if self.in_use.replace(true) {
            return Err(CloveError::runtime(format!(
                "{op} native buffer already borrowed"
            )));
        }
        let _guard = InUseGuard::new(&self.in_use);
        let ptr = self.buf.data_ptr();
        unsafe { f(&*ptr) }
    }
}

impl<T> Drop for MainThreadBufBorrow<T> {
    fn drop(&mut self) {
        self.buf.end_borrow();
    }
}

enum NativeBufBorrow<T: 'static> {
    Shared(NativeBufScopedGuard<T>),
    Main(MainThreadBufBorrow<T>),
}

impl<T: 'static> NativeBufBorrow<T> {
    fn with_mut<R>(
        &mut self,
        op: &str,
        f: impl FnOnce(&mut Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        match self {
            NativeBufBorrow::Shared(guard) => guard.with_mut(op, f),
            NativeBufBorrow::Main(guard) => guard.with_mut(op, f),
        }
    }

    fn with_ref<R>(
        &mut self,
        op: &str,
        f: impl FnOnce(&Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        match self {
            NativeBufBorrow::Shared(guard) => guard.with_ref(op, f),
            NativeBufBorrow::Main(guard) => guard.with_ref(op, f),
        }
    }
}

fn cache_insert_i32(id: u64, buf: &Arc<Mutex<Vec<i32>>>) {
    I32BUF_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if cache.len() >= NATIVE_BUF_CACHE_LIMIT {
            cache.clear();
        }
        cache.insert(id, Arc::downgrade(buf));
    });
}

fn cache_insert_f32(id: u64, buf: &Arc<Mutex<Vec<f32>>>) {
    F32BUF_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if cache.len() >= NATIVE_BUF_CACHE_LIMIT {
            cache.clear();
        }
        cache.insert(id, Arc::downgrade(buf));
    });
}

fn cache_insert_i64(id: u64, buf: &Arc<Mutex<Vec<i64>>>) {
    I64BUF_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if cache.len() >= NATIVE_BUF_CACHE_LIMIT {
            cache.clear();
        }
        cache.insert(id, Arc::downgrade(buf));
    });
}

fn cache_get_i32(id: u64) -> Option<Arc<Mutex<Vec<i32>>>> {
    I32BUF_CACHE.with(|cache| cache.borrow().get(&id).and_then(|weak| weak.upgrade()))
}

fn cache_get_f32(id: u64) -> Option<Arc<Mutex<Vec<f32>>>> {
    F32BUF_CACHE.with(|cache| cache.borrow().get(&id).and_then(|weak| weak.upgrade()))
}

fn cache_get_i64(id: u64) -> Option<Arc<Mutex<Vec<i64>>>> {
    I64BUF_CACHE.with(|cache| cache.borrow().get(&id).and_then(|weak| weak.upgrade()))
}

#[derive(Default)]
pub struct NativeBufRegistry {
    next_id: u64,
    map: HashMap<u64, NativeBuf>,
}

impl NativeBufRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, buf: NativeBuf) -> u64 {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        self.map.insert(id, buf);
        id
    }

    pub fn get(&self, id: u64) -> Option<&NativeBuf> {
        self.map.get(&id)
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }
}

pub fn native_buf_type_name(ty: NativeBufTy) -> &'static str {
    match ty {
        NativeBufTy::I32 => "NativeI32Buf",
        NativeBufTy::F32 => "NativeF32Buf",
        NativeBufTy::I64 => "NativeI64Buf",
    }
}

pub fn native_buf_label(ty: NativeBufTy) -> &'static str {
    match ty {
        NativeBufTy::I32 => "i32buf",
        NativeBufTy::F32 => "f32buf",
        NativeBufTy::I64 => "i64buf",
    }
}

pub fn format_native_buf(ty: NativeBufTy, id: u64) -> String {
    let label = native_buf_label(ty);
    let (len, cap) = native_buf_len_cap(id, ty)
        .map(|(len, cap)| (len.to_string(), cap.to_string()))
        .unwrap_or_else(|| ("?".into(), "?".into()));
    format!("#<native/{} len={} cap={}>", label, len, cap)
}

pub fn format_native_buf_handle(handle: &NativeBufHandle) -> String {
    let label = native_buf_label(handle.ty());
    let (len, cap) = match handle {
        NativeBufHandle::I32(buf) => buf
            .try_lock()
            .map(|guard| (guard.len(), guard.capacity()))
            .ok(),
        NativeBufHandle::I32Main(buf) => buf
            .with_ref("format-native-buf", |guard| {
                Ok((guard.len(), guard.capacity()))
            })
            .ok(),
        NativeBufHandle::F32(buf) => buf
            .try_lock()
            .map(|guard| (guard.len(), guard.capacity()))
            .ok(),
        NativeBufHandle::F32Main(buf) => buf
            .with_ref("format-native-buf", |guard| {
                Ok((guard.len(), guard.capacity()))
            })
            .ok(),
        NativeBufHandle::I64(buf) => buf
            .try_lock()
            .map(|guard| (guard.len(), guard.capacity()))
            .ok(),
        NativeBufHandle::I64Main(buf) => buf
            .with_ref("format-native-buf", |guard| {
                Ok((guard.len(), guard.capacity()))
            })
            .ok(),
    }
    .map(|(len, cap)| (len.to_string(), cap.to_string()))
    .unwrap_or_else(|| ("?".into(), "?".into()));
    format!("#<native/{} len={} cap={}>", label, len, cap)
}

pub fn expect_native_buf(value: &Value, expected: NativeBufTy) -> Result<u64, CloveError> {
    match value {
        Value::NativeBuf { id, ty, .. } if *ty == expected => Ok(*id),
        Value::NativeBuf { ty, .. } => Err(CloveError::type_mismatch(
            native_buf_type_name(expected),
            native_buf_type_name(*ty),
        )),
        other => Err(CloveError::type_mismatch(
            native_buf_type_name(expected),
            other.type_name(),
        )),
    }
}

pub fn expect_i32buf(value: &Value) -> Result<u64, CloveError> {
    expect_native_buf(value, NativeBufTy::I32)
}

pub fn expect_f32buf(value: &Value) -> Result<u64, CloveError> {
    expect_native_buf(value, NativeBufTy::F32)
}

pub fn expect_i64buf(value: &Value) -> Result<u64, CloveError> {
    expect_native_buf(value, NativeBufTy::I64)
}

pub fn expect_i32buf_handle(value: &Value) -> Result<(u64, NativeBufAccess<i32>), CloveError> {
    match value {
        Value::NativeBuf {
            id,
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32(buf),
        } => Ok((*id, NativeBufAccess::Shared(buf.clone()))),
        Value::NativeBuf {
            id,
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32Main(buf),
        } => Ok((*id, NativeBufAccess::Main(buf.clone()))),
        Value::NativeBuf { ty, .. } => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::I32),
            native_buf_type_name(*ty),
        )),
        other => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::I32),
            other.type_name(),
        )),
    }
}

pub fn expect_f32buf_handle(value: &Value) -> Result<(u64, NativeBufAccess<f32>), CloveError> {
    match value {
        Value::NativeBuf {
            id,
            ty: NativeBufTy::F32,
            handle: NativeBufHandle::F32(buf),
        } => Ok((*id, NativeBufAccess::Shared(buf.clone()))),
        Value::NativeBuf {
            id,
            ty: NativeBufTy::F32,
            handle: NativeBufHandle::F32Main(buf),
        } => Ok((*id, NativeBufAccess::Main(buf.clone()))),
        Value::NativeBuf { ty, .. } => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::F32),
            native_buf_type_name(*ty),
        )),
        other => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::F32),
            other.type_name(),
        )),
    }
}

pub fn expect_i64buf_handle(value: &Value) -> Result<(u64, NativeBufAccess<i64>), CloveError> {
    match value {
        Value::NativeBuf {
            id,
            ty: NativeBufTy::I64,
            handle: NativeBufHandle::I64(buf),
        } => Ok((*id, NativeBufAccess::Shared(buf.clone()))),
        Value::NativeBuf {
            id,
            ty: NativeBufTy::I64,
            handle: NativeBufHandle::I64Main(buf),
        } => Ok((*id, NativeBufAccess::Main(buf.clone()))),
        Value::NativeBuf { ty, .. } => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::I64),
            native_buf_type_name(*ty),
        )),
        other => Err(CloveError::type_mismatch(
            native_buf_type_name(NativeBufTy::I64),
            other.type_name(),
        )),
    }
}

pub fn begin_i32buf_guard(id: u64, buf: NativeBufAccess<i32>, op: &str) -> Result<(), CloveError> {
    I32BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.contains_key(&id) {
            return Err(CloveError::runtime(format!(
                "{op} native i32 buffer already locked"
            )));
        }
        let guard = match buf {
            NativeBufAccess::Shared(buf) => {
                NativeBufBorrow::Shared(NativeBufScopedGuard::new(buf, op)?)
            }
            NativeBufAccess::Main(buf) => NativeBufBorrow::Main(MainThreadBufBorrow::new(buf, op)?),
        };
        guards.insert(id, Rc::new(RefCell::new(guard)));
        Ok(())
    })
}

pub fn end_i32buf_guard(id: u64, op: &str) -> Result<(), CloveError> {
    I32BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.remove(&id).is_some() {
            Ok(())
        } else {
            Err(CloveError::runtime(format!(
                "{op} native i32 buffer was not locked"
            )))
        }
    })
}

pub fn with_i32buf_mut<R>(
    id: u64,
    buf: NativeBufAccess<i32>,
    op: &str,
    f: impl FnOnce(&mut Vec<i32>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = I32BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native i32 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_mut(op, f);
        return result;
    }
    let f = f.expect("native i32 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let mut guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&mut guard)
        }
        NativeBufAccess::Main(buf) => buf.with_mut(op, f),
    }
}

pub fn with_i32buf_ref<R>(
    id: u64,
    buf: NativeBufAccess<i32>,
    op: &str,
    f: impl FnOnce(&Vec<i32>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = I32BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native i32 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_ref(op, f);
        return result;
    }
    let f = f.expect("native i32 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&guard)
        }
        NativeBufAccess::Main(buf) => buf.with_ref(op, f),
    }
}

pub fn begin_f32buf_guard(id: u64, buf: NativeBufAccess<f32>, op: &str) -> Result<(), CloveError> {
    F32BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.contains_key(&id) {
            return Err(CloveError::runtime(format!(
                "{op} native f32 buffer already locked"
            )));
        }
        let guard = match buf {
            NativeBufAccess::Shared(buf) => {
                NativeBufBorrow::Shared(NativeBufScopedGuard::new(buf, op)?)
            }
            NativeBufAccess::Main(buf) => NativeBufBorrow::Main(MainThreadBufBorrow::new(buf, op)?),
        };
        guards.insert(id, Rc::new(RefCell::new(guard)));
        Ok(())
    })
}

pub fn end_f32buf_guard(id: u64, op: &str) -> Result<(), CloveError> {
    F32BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.remove(&id).is_some() {
            Ok(())
        } else {
            Err(CloveError::runtime(format!(
                "{op} native f32 buffer was not locked"
            )))
        }
    })
}

pub fn with_f32buf_mut<R>(
    id: u64,
    buf: NativeBufAccess<f32>,
    op: &str,
    f: impl FnOnce(&mut Vec<f32>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = F32BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native f32 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_mut(op, f);
        return result;
    }
    let f = f.expect("native f32 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let mut guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&mut guard)
        }
        NativeBufAccess::Main(buf) => buf.with_mut(op, f),
    }
}

pub fn with_f32buf_ref<R>(
    id: u64,
    buf: NativeBufAccess<f32>,
    op: &str,
    f: impl FnOnce(&Vec<f32>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = F32BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native f32 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_ref(op, f);
        return result;
    }
    let f = f.expect("native f32 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&guard)
        }
        NativeBufAccess::Main(buf) => buf.with_ref(op, f),
    }
}

pub fn begin_i64buf_guard(id: u64, buf: NativeBufAccess<i64>, op: &str) -> Result<(), CloveError> {
    I64BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.contains_key(&id) {
            return Err(CloveError::runtime(format!(
                "{op} native i64 buffer already locked"
            )));
        }
        let guard = match buf {
            NativeBufAccess::Shared(buf) => {
                NativeBufBorrow::Shared(NativeBufScopedGuard::new(buf, op)?)
            }
            NativeBufAccess::Main(buf) => NativeBufBorrow::Main(MainThreadBufBorrow::new(buf, op)?),
        };
        guards.insert(id, Rc::new(RefCell::new(guard)));
        Ok(())
    })
}

pub fn end_i64buf_guard(id: u64, op: &str) -> Result<(), CloveError> {
    I64BUF_GUARDS.with(|guards| {
        let mut guards = guards.borrow_mut();
        if guards.remove(&id).is_some() {
            Ok(())
        } else {
            Err(CloveError::runtime(format!(
                "{op} native i64 buffer was not locked"
            )))
        }
    })
}

pub fn with_i64buf_mut<R>(
    id: u64,
    buf: NativeBufAccess<i64>,
    op: &str,
    f: impl FnOnce(&mut Vec<i64>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = I64BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native i64 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_mut(op, f);
        return result;
    }
    let f = f.expect("native i64 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let mut guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&mut guard)
        }
        NativeBufAccess::Main(buf) => buf.with_mut(op, f),
    }
}

pub fn with_i64buf_ref<R>(
    id: u64,
    buf: NativeBufAccess<i64>,
    op: &str,
    f: impl FnOnce(&Vec<i64>) -> Result<R, CloveError>,
) -> Result<R, CloveError> {
    let mut f = Some(f);
    let entry = I64BUF_GUARDS.with(|guards| guards.borrow().get(&id).cloned());
    if let Some(entry) = entry {
        let f = f.take().expect("native i64 buffer closure missing");
        let mut guard = entry.borrow_mut();
        let result = guard.with_ref(op, f);
        return result;
    }
    let f = f.expect("native i64 buffer closure missing");
    match buf {
        NativeBufAccess::Shared(buf) => {
            let guard = buf
                .lock()
                .map_err(|_| CloveError::runtime(format!("{op} lock poisoned")))?;
            f(&guard)
        }
        NativeBufAccess::Main(buf) => buf.with_ref(op, f),
    }
}

pub fn lock_i32buf(ctx: &RuntimeCtx, id: u64) -> Result<Arc<Mutex<Vec<i32>>>, CloveError> {
    if let Some(buf) = cache_get_i32(id) {
        return Ok(buf);
    }
    let buf = ctx.with_native_bufs(|bufs| match bufs.get(id) {
        Some(NativeBuf::I32(buf)) => Ok(buf.clone()),
        Some(NativeBuf::I32Main(_)) => {
            Err(CloveError::runtime("native i32 buffer is main-thread only"))
        }
        Some(_) => Err(CloveError::runtime("native i32 buffer type mismatch")),
        None => Err(CloveError::runtime("native i32 buffer not found")),
    })?;
    cache_insert_i32(id, &buf);
    Ok(buf)
}

pub fn lock_f32buf(ctx: &RuntimeCtx, id: u64) -> Result<Arc<Mutex<Vec<f32>>>, CloveError> {
    if let Some(buf) = cache_get_f32(id) {
        return Ok(buf);
    }
    let buf = ctx.with_native_bufs(|bufs| match bufs.get(id) {
        Some(NativeBuf::F32(buf)) => Ok(buf.clone()),
        Some(NativeBuf::F32Main(_)) => {
            Err(CloveError::runtime("native f32 buffer is main-thread only"))
        }
        Some(_) => Err(CloveError::runtime("native f32 buffer type mismatch")),
        None => Err(CloveError::runtime("native f32 buffer not found")),
    })?;
    cache_insert_f32(id, &buf);
    Ok(buf)
}

pub fn lock_i64buf(ctx: &RuntimeCtx, id: u64) -> Result<Arc<Mutex<Vec<i64>>>, CloveError> {
    if let Some(buf) = cache_get_i64(id) {
        return Ok(buf);
    }
    let buf = ctx.with_native_bufs(|bufs| match bufs.get(id) {
        Some(NativeBuf::I64(buf)) => Ok(buf.clone()),
        Some(NativeBuf::I64Main(_)) => {
            Err(CloveError::runtime("native i64 buffer is main-thread only"))
        }
        Some(_) => Err(CloveError::runtime("native i64 buffer type mismatch")),
        None => Err(CloveError::runtime("native i64 buffer not found")),
    })?;
    cache_insert_i64(id, &buf);
    Ok(buf)
}

fn native_buf_len_cap(id: u64, ty: NativeBufTy) -> Option<(usize, usize)> {
    RuntimeCtx::try_with_current(|ctx| {
        ctx.with_native_bufs(|bufs| match bufs.get(id) {
            Some(NativeBuf::I32(buf)) if ty == NativeBufTy::I32 => {
                let guard = buf
                    .lock()
                    .map_err(|_| CloveError::runtime("native i32 buffer lock poisoned"))?;
                Ok((guard.len(), guard.capacity()))
            }
            Some(NativeBuf::I32Main(buf)) if ty == NativeBufTy::I32 => buf
                .with_ref("native i32 buffer len/cap", |guard| {
                    Ok((guard.len(), guard.capacity()))
                }),
            Some(NativeBuf::F32(buf)) if ty == NativeBufTy::F32 => {
                let guard = buf
                    .lock()
                    .map_err(|_| CloveError::runtime("native f32 buffer lock poisoned"))?;
                Ok((guard.len(), guard.capacity()))
            }
            Some(NativeBuf::F32Main(buf)) if ty == NativeBufTy::F32 => buf
                .with_ref("native f32 buffer len/cap", |guard| {
                    Ok((guard.len(), guard.capacity()))
                }),
            Some(NativeBuf::I64(buf)) if ty == NativeBufTy::I64 => {
                let guard = buf
                    .lock()
                    .map_err(|_| CloveError::runtime("native i64 buffer lock poisoned"))?;
                Ok((guard.len(), guard.capacity()))
            }
            Some(NativeBuf::I64Main(buf)) if ty == NativeBufTy::I64 => buf
                .with_ref("native i64 buffer len/cap", |guard| {
                    Ok((guard.len(), guard.capacity()))
                }),
            _ => Err(CloveError::runtime("native buffer not found")),
        })
    })
    .and_then(|res| res.ok())
}
