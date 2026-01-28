use crate::ast::{HashMap, Key, Value, Vector};
use crate::error::CloveError;
use crate::eval::{call_callable, current_file_name, set_current_file};
use crate::runtime::{RuntimeCtx, RuntimeGuard, RuntimeRequirementGuard};
use crossbeam_channel::{self, Receiver, Select, Sender};
use once_cell::sync::Lazy;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap as StdHashMap;
use std::panic::{self, AssertUnwindSafe};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, Weak};
use std::thread;
use std::time::Duration;

#[derive(Clone)]
pub struct AtomHandle {
    inner: Arc<AtomInner>,
}

fn panic_payload_message(payload: Box<dyn Any + Send>) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg.clone()
    } else {
        "panic occurred".to_string()
    }
}

fn call_callable_guarded(callable: Value, args: Vec<Value>) -> Result<Value, CloveError> {
    match panic::catch_unwind(AssertUnwindSafe(|| call_callable(callable, args))) {
        Ok(result) => result,
        Err(payload) => Err(CloveError::runtime(format!(
            "panic: {}",
            panic_payload_message(payload)
        ))),
    }
}

pub(crate) struct AtomInner {
    value: Mutex<AtomValue>,
    validator: Mutex<Option<Value>>,
    watches: Mutex<StdHashMap<Value, Value>>,
}

#[derive(Clone)]
struct AtomValue {
    value: Value,
    version: usize,
}

impl AtomHandle {
    pub fn new(initial: Value) -> Self {
        Self::with_validator(initial, None).expect("validatorless atom creation cannot fail")
    }

    pub fn with_validator(initial: Value, validator: Option<Value>) -> Result<Self, CloveError> {
        let handle = Self {
            inner: Arc::new(AtomInner {
                value: Mutex::new(AtomValue {
                    value: initial,
                    version: 0,
                }),
                validator: Mutex::new(None),
                watches: Mutex::new(StdHashMap::new()),
            }),
        };
        if let Some(func) = validator {
            handle.set_validator(Some(func))?;
        }
        Ok(handle)
    }

    pub fn deref(&self) -> Value {
        self.inner.value.lock().unwrap().value.clone()
    }

    pub fn set(&self, value: Value) -> Result<Value, CloveError> {
        let target = value;
        self.replace_with(move |_| Ok(target.clone()))
    }

    pub fn update_with_callable(
        &self,
        func: Value,
        extra_args: Vec<Value>,
    ) -> Result<Value, CloveError> {
        let extras = extra_args;
        self.replace_with(move |current| {
            let mut args = Vec::with_capacity(1 + extras.len());
            args.push(current);
            args.extend(extras.iter().cloned());
            call_callable(func.clone(), args)
        })
    }

    pub fn compare_and_set(&self, expected: Value, new_value: Value) -> Result<bool, CloveError> {
        loop {
            let snapshot = {
                let guard = self.inner.value.lock().unwrap();
                guard.clone()
            };
            if snapshot.value != expected {
                return Ok(false);
            }
            Self::enforce_validator(self.validator(), &new_value)?;
            if self.try_commit(snapshot.version, new_value.clone()) {
                self.fire_watches(snapshot.value, new_value.clone())?;
                return Ok(true);
            }
        }
    }

    pub fn add_watch(&self, key: Value, func: Value) {
        self.inner.watches.lock().unwrap().insert(key, func);
    }

    pub fn remove_watch(&self, key: &Value) -> bool {
        self.inner.watches.lock().unwrap().remove(key).is_some()
    }

    pub fn validator(&self) -> Option<Value> {
        self.inner.validator.lock().unwrap().clone()
    }

    pub fn set_validator(&self, validator: Option<Value>) -> Result<(), CloveError> {
        if let Some(func) = &validator {
            let current = self.deref();
            Self::enforce_validator(Some(func.clone()), &current)?;
        }
        *self.inner.validator.lock().unwrap() = validator;
        Ok(())
    }

    fn replace_with<F>(&self, func: F) -> Result<Value, CloveError>
    where
        F: Fn(Value) -> Result<Value, CloveError>,
    {
        loop {
            let snapshot = {
                let guard = self.inner.value.lock().unwrap();
                guard.clone()
            };
            let candidate = func(snapshot.value.clone())?;
            Self::enforce_validator(self.validator(), &candidate)?;
            if self.try_commit(snapshot.version, candidate.clone()) {
                self.fire_watches(snapshot.value, candidate.clone())?;
                return Ok(candidate);
            }
        }
    }

    fn try_commit(&self, expected_version: usize, next: Value) -> bool {
        let mut guard = self.inner.value.lock().unwrap();
        if guard.version != expected_version {
            return false;
        }
        guard.value = next;
        guard.version = guard.version.wrapping_add(1);
        true
    }

    fn enforce_validator(validator: Option<Value>, candidate: &Value) -> Result<(), CloveError> {
        if let Some(func) = validator {
            let result = call_callable(func.clone(), vec![candidate.clone()])?;
            if !value_truthy(&result) {
                return Err(CloveError::runtime("atom validator rejected the new value"));
            }
        }
        Ok(())
    }

    fn fire_watches(&self, old_value: Value, new_value: Value) -> Result<(), CloveError> {
        let watchers = self.inner.watches.lock().unwrap();
        if watchers.is_empty() {
            return Ok(());
        }
        let entries: Vec<(Value, Value)> = watchers
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        drop(watchers);
        let atom_value = Value::Atom(self.clone());
        let mut first_error = None;
        for (key, callback) in entries {
            let mut args = Vec::with_capacity(4);
            args.push(key.clone());
            args.push(atom_value.clone());
            args.push(old_value.clone());
            args.push(new_value.clone());
            if let Err(err) = call_callable(callback.clone(), args) {
                if first_error.is_none() {
                    first_error = Some(err);
                }
            }
        }
        if let Some(err) = first_error {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub(crate) fn ptr(&self) -> *const AtomInner {
        Arc::as_ptr(&self.inner)
    }
}

#[derive(Clone)]
pub struct DelayHandle {
    inner: Arc<DelayInner>,
}

pub(crate) struct DelayInner {
    state: Mutex<DelayState>,
    cond: Condvar,
}

#[derive(Clone)]
enum DelayState {
    Pending(Value),
    Running,
    Realized(Result<Value, CloveError>),
}

impl DelayHandle {
    pub fn new(thunk: Value) -> Self {
        Self {
            inner: Arc::new(DelayInner {
                state: Mutex::new(DelayState::Pending(thunk)),
                cond: Condvar::new(),
            }),
        }
    }

    pub fn force(&self) -> Result<Value, CloveError> {
        loop {
            let mut guard = self.inner.state.lock().unwrap();
            match &*guard {
                DelayState::Pending(thunk) => {
                    let thunk = thunk.clone();
                    *guard = DelayState::Running;
                    drop(guard);
                    let result = call_callable(thunk, vec![]);
                    let mut guard = self.inner.state.lock().unwrap();
                    *guard = DelayState::Realized(result.clone());
                    self.inner.cond.notify_all();
                    return result;
                }
                DelayState::Running => {
                    drop(self.inner.cond.wait(guard).unwrap());
                    continue;
                }
                DelayState::Realized(result) => {
                    return result.clone();
                }
            }
        }
    }

    pub fn is_realized(&self) -> bool {
        matches!(*self.inner.state.lock().unwrap(), DelayState::Realized(_))
    }

    pub(crate) fn ptr(&self) -> *const DelayInner {
        Arc::as_ptr(&self.inner)
    }
}

#[derive(Clone)]
pub struct ChanHandle {
    inner: Arc<ChanInner>,
}

pub(crate) struct ChanInner {
    sender: Mutex<Option<Sender<Value>>>,
    receiver: Receiver<Value>,
    closed: AtomicBool,
    id: usize,
    capacity: Option<usize>,
}

#[derive(Clone, Copy, Debug)]
pub struct ChanDebugInfo {
    pub id: usize,
    pub capacity: Option<usize>,
    pub len: usize,
    pub closed: bool,
}

static NEXT_CHAN_ID: AtomicUsize = AtomicUsize::new(1);

impl ChanHandle {
    pub fn new(capacity: Option<usize>) -> Self {
        let (sender, receiver) = match capacity {
            Some(0) => crossbeam_channel::bounded(0),
            Some(cap) => crossbeam_channel::bounded(cap),
            None => crossbeam_channel::unbounded(),
        };
        let id = NEXT_CHAN_ID.fetch_add(1, Ordering::SeqCst);
        let inner = ChanInner {
            sender: Mutex::new(Some(sender)),
            receiver,
            closed: AtomicBool::new(false),
            id,
            capacity,
        };
        Self {
            inner: Arc::new(inner),
        }
    }

    pub fn put(&self, value: Value) -> bool {
        let sender = self.inner.sender.lock().unwrap().clone();
        if let Some(sender) = sender {
            sender.send(value).is_ok()
        } else {
            false
        }
    }

    pub fn take(&self) -> Option<Value> {
        self.inner.receiver.recv().ok()
    }

    pub fn close(&self) {
        self.inner.sender.lock().unwrap().take();
        self.inner.closed.store(true, Ordering::SeqCst);
    }

    pub fn is_closed(&self) -> bool {
        self.inner.closed.load(Ordering::SeqCst)
    }

    pub fn debug_info(&self) -> ChanDebugInfo {
        ChanDebugInfo {
            id: self.inner.id,
            capacity: self.inner.capacity,
            len: self.inner.receiver.len(),
            closed: self.is_closed(),
        }
    }

    pub(crate) fn receiver_clone(&self) -> Receiver<Value> {
        self.inner.receiver.clone()
    }

    pub(crate) fn sender_clone(&self) -> Option<Sender<Value>> {
        self.inner.sender.lock().unwrap().clone()
    }

    pub(crate) fn ptr(&self) -> *const ChanInner {
        Arc::as_ptr(&self.inner)
    }
}

pub struct CancelContext {
    cancelled: AtomicBool,
    cancel_chan: ChanHandle,
    children: Mutex<Vec<Weak<CancelContext>>>,
}

impl CancelContext {
    pub fn new(parent: Option<Arc<CancelContext>>) -> Arc<CancelContext> {
        let ctx = Arc::new(CancelContext {
            cancelled: AtomicBool::new(false),
            cancel_chan: ChanHandle::new(None),
            children: Mutex::new(Vec::new()),
        });
        if let Some(parent_ctx) = parent {
            parent_ctx
                .children
                .lock()
                .unwrap()
                .push(Arc::downgrade(&ctx));
            if parent_ctx.is_cancelled() {
                ctx.cancel();
            }
        }
        ctx
    }

    pub fn cancel(&self) {
        if self.cancelled.swap(true, Ordering::SeqCst) {
            return;
        }
        self.cancel_chan.close();
        let mut guard = self.children.lock().unwrap();
        let mut alive = Vec::new();
        for child in guard.drain(..) {
            if let Some(ctx) = child.upgrade() {
                ctx.cancel();
                alive.push(Arc::downgrade(&ctx));
            }
        }
        *guard = alive;
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::SeqCst)
    }

    pub fn cancel_chan(&self) -> ChanHandle {
        self.cancel_chan.clone()
    }
}

thread_local! {
    static CURRENT_CANCEL_CTX: RefCell<Option<Arc<CancelContext>>> = RefCell::new(None);
}

struct CancelCtxGuard {
    prev: Option<Option<Arc<CancelContext>>>,
}

impl Drop for CancelCtxGuard {
    fn drop(&mut self) {
        if let Some(prev) = self.prev.take() {
            CURRENT_CANCEL_CTX.with(|cell| {
                cell.replace(prev);
            });
        }
    }
}

pub fn current_cancel_ctx() -> Option<Arc<CancelContext>> {
    CURRENT_CANCEL_CTX.with(|cell| cell.borrow().clone())
}

pub fn with_cancel_ctx<R>(ctx: Option<Arc<CancelContext>>, f: impl FnOnce() -> R) -> R {
    let prev = CURRENT_CANCEL_CTX.with(|cell| cell.replace(ctx));
    let guard = CancelCtxGuard { prev: Some(prev) };
    let result = f();
    drop(guard);
    result
}

static NEVER_READY_CANCEL_CHAN: Lazy<ChanHandle> = Lazy::new(|| ChanHandle::new(None));

pub fn never_ready_cancel_chan() -> ChanHandle {
    NEVER_READY_CANCEL_CHAN.clone()
}

#[derive(Clone)]
pub struct PromiseHandle {
    state: Arc<PromiseState>,
}

pub(crate) struct PromiseState {
    result: Mutex<Option<Result<Value, CloveError>>>,
    cond: Condvar,
    cancelled: AtomicBool,
    watches: Mutex<StdHashMap<Value, PromiseWatch>>,
}

#[derive(Clone)]
struct PromiseWatch {
    target: Value,
    callback: Value,
}

#[derive(Clone, Debug)]
pub enum PromiseStateView {
    Pending,
    Fulfilled(Value),
    Rejected(String),
    Cancelled,
}

impl PromiseHandle {
    pub fn new() -> Self {
        Self {
            state: Arc::new(PromiseState {
                result: Mutex::new(None),
                cond: Condvar::new(),
                cancelled: AtomicBool::new(false),
                watches: Mutex::new(StdHashMap::new()),
            }),
        }
    }

    pub fn resolve(&self, result: Result<Value, CloveError>) -> bool {
        let mut guard = self.state.result.lock().unwrap();
        if guard.is_some() {
            return false;
        }
        *guard = Some(result);
        self.state.cancelled.store(false, Ordering::SeqCst);
        let snapshot = guard.clone();
        self.state.cond.notify_all();
        drop(guard);
        if let Some(res) = snapshot {
            self.fire_watches(Value::Nil, promise_settled_value(&res));
        }
        true
    }

    pub fn wait(&self) -> Result<Value, CloveError> {
        let mut guard = self.state.result.lock().unwrap();
        while guard.is_none() {
            guard = self.state.cond.wait(guard).unwrap();
        }
        guard.clone().unwrap()
    }

    pub fn is_done(&self) -> bool {
        self.state.result.lock().unwrap().is_some()
    }

    pub fn current(&self) -> Option<Result<Value, CloveError>> {
        self.state.result.lock().unwrap().clone()
    }

    pub fn cancel(&self) -> bool {
        let mut guard = self.state.result.lock().unwrap();
        if guard.is_some() {
            return false;
        }
        *guard = Some(Err(CloveError::runtime("future cancelled")));
        self.state.cancelled.store(true, Ordering::SeqCst);
        let snapshot = guard.clone();
        self.state.cond.notify_all();
        drop(guard);
        if let Some(res) = snapshot {
            self.fire_watches(Value::Nil, promise_settled_value(&res));
        }
        true
    }

    pub fn is_cancelled(&self) -> bool {
        self.state.cancelled.load(Ordering::SeqCst)
    }

    pub fn debug_state(&self) -> PromiseStateView {
        let snapshot = self.state.result.lock().unwrap().clone();
        if self.is_cancelled() {
            return PromiseStateView::Cancelled;
        }
        match snapshot {
            None => PromiseStateView::Pending,
            Some(Ok(value)) => PromiseStateView::Fulfilled(value),
            Some(Err(err)) => PromiseStateView::Rejected(err.to_string()),
        }
    }

    pub(crate) fn ptr(&self) -> *const PromiseState {
        Arc::as_ptr(&self.state)
    }

    pub fn add_watch(&self, target: Value, key: Value, func: Value) {
        self.state.watches.lock().unwrap().insert(
            key,
            PromiseWatch {
                target,
                callback: func,
            },
        );
    }

    pub fn remove_watch(&self, key: &Value) -> bool {
        self.state.watches.lock().unwrap().remove(key).is_some()
    }

    fn fire_watches(&self, old_value: Value, new_value: Value) {
        let watchers = self.state.watches.lock().unwrap();
        if watchers.is_empty() {
            return;
        }
        let entries: Vec<(Value, PromiseWatch)> = watchers
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        drop(watchers);
        let mut first_error = None;
        for (key, watcher) in entries {
            let mut args = Vec::with_capacity(4);
            args.push(key.clone());
            args.push(watcher.target.clone());
            args.push(old_value.clone());
            args.push(new_value.clone());
            if let Err(err) = call_callable(watcher.callback.clone(), args) {
                if first_error.is_none() {
                    first_error = Some(err);
                }
            }
        }
        if let Some(err) = first_error {
            eprintln!("promise watch error: {}", err);
        }
    }
}

#[derive(Clone)]
pub struct TaskHandle {
    promise: PromiseHandle,
}

impl TaskHandle {
    pub fn new(promise: PromiseHandle) -> Self {
        Self { promise }
    }

    pub fn promise(&self) -> &PromiseHandle {
        &self.promise
    }

    pub fn is_done(&self) -> bool {
        self.promise.is_done()
    }

    pub fn cancel(&self) -> bool {
        self.promise.cancel()
    }

    pub fn is_cancelled(&self) -> bool {
        self.promise.is_cancelled()
    }

    pub fn wait(&self) -> Result<Value, CloveError> {
        self.promise.wait()
    }

    pub(crate) fn ptr(&self) -> *const PromiseState {
        self.promise.ptr()
    }

    pub fn add_watch(&self, key: Value, func: Value) {
        self.promise.add_watch(Value::Task(self.clone()), key, func);
    }

    pub fn remove_watch(&self, key: &Value) -> bool {
        self.promise.remove_watch(key)
    }

    pub fn current(&self) -> Option<Result<Value, CloveError>> {
        self.promise.current()
    }

    pub fn debug_state(&self) -> PromiseStateView {
        self.promise.debug_state()
    }
}

pub(crate) fn spawn_with_current_file<F, T>(f: F) -> thread::JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let file = current_file_name();
    let cancel_ctx = current_cancel_ctx();
    thread::spawn(move || {
        set_current_file(file);
        with_cancel_ctx(cancel_ctx, f)
    })
}

pub fn spawn_task(thunk: Value) -> TaskHandle {
    let promise = PromiseHandle::new();
    let runner_promise = promise.clone();
    let cancel_ctx = current_cancel_ctx();
    let runtime_info =
        RuntimeCtx::current_handle().map(|handle| (handle.clone(), handle.task_guard()));
    spawn_with_current_file(move || {
        let (_runtime_guard, _task_guard, _require_guard) =
            if let Some((handle, guard)) = runtime_info {
                (Some(RuntimeGuard::set(handle)), Some(guard), None)
            } else {
                (None, None, Some(RuntimeRequirementGuard::set(true)))
            };
        let ctx_for_error = cancel_ctx.clone();
        let result = call_callable_guarded(thunk, Vec::new());
        if result.is_err() {
            if let Some(ctx) = ctx_for_error {
                ctx.cancel();
            }
        }
        runner_promise.resolve(result);
    });
    TaskHandle::new(promise)
}

pub fn task_from_promise(promise: PromiseHandle) -> TaskHandle {
    TaskHandle::new(promise)
}

#[derive(Clone)]
pub struct FutureHandle {
    task: TaskHandle,
}

impl FutureHandle {
    pub fn new(task: TaskHandle) -> Self {
        Self { task }
    }

    pub fn task(&self) -> &TaskHandle {
        &self.task
    }

    pub fn wait(&self) -> Result<Value, CloveError> {
        self.task.wait()
    }

    pub fn is_done(&self) -> bool {
        self.task.is_done()
    }

    pub fn cancel(&self) -> bool {
        self.task.cancel()
    }

    pub fn is_cancelled(&self) -> bool {
        self.task.is_cancelled()
    }

    pub(crate) fn ptr(&self) -> *const PromiseState {
        self.task.ptr()
    }

    pub fn add_watch(&self, key: Value, func: Value) {
        self.task
            .promise()
            .add_watch(Value::Future(self.clone()), key, func);
    }

    pub fn remove_watch(&self, key: &Value) -> bool {
        self.task.promise().remove_watch(key)
    }

    pub fn current(&self) -> Option<Result<Value, CloveError>> {
        self.task.current()
    }

    pub fn debug_state(&self) -> PromiseStateView {
        self.task.debug_state()
    }
}

pub fn spawn_future(thunk: Value) -> FutureHandle {
    FutureHandle::new(spawn_task(thunk))
}

pub fn future_from_promise(promise: PromiseHandle) -> FutureHandle {
    FutureHandle::new(task_from_promise(promise))
}

#[derive(Clone)]
pub struct AgentHandle {
    state: Arc<Mutex<Value>>,
    sender: Sender<AgentMessage>,
    pending: Arc<AtomicUsize>,
    processing: Arc<AtomicBool>,
    cond: Arc<(Mutex<()>, Condvar)>,
    error: Arc<Mutex<Option<CloveError>>>,
    watches: Arc<Mutex<StdHashMap<Value, Value>>>,
}

struct AgentMessage {
    func: Value,
    args: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct AgentDebugInfo {
    pub state: Value,
    pub pending: usize,
    pub processing: bool,
    pub error: Option<CloveError>,
}

impl AgentHandle {
    pub fn new(initial: Value) -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let state = Arc::new(Mutex::new(initial));
        let pending = Arc::new(AtomicUsize::new(0));
        let processing = Arc::new(AtomicBool::new(false));
        let cond = Arc::new((Mutex::new(()), Condvar::new()));
        let error = Arc::new(Mutex::new(None));
        let watches = Arc::new(Mutex::new(StdHashMap::new()));
        let handle = Self {
            state: state.clone(),
            sender,
            pending: pending.clone(),
            processing: processing.clone(),
            cond: cond.clone(),
            error: error.clone(),
            watches: watches.clone(),
        };
        start_agent_worker(handle.clone(), receiver);
        handle
    }

    pub fn state(&self) -> Value {
        self.state.lock().unwrap().clone()
    }

    pub fn add_watch(&self, key: Value, func: Value) {
        self.watches.lock().unwrap().insert(key, func);
    }

    pub fn remove_watch(&self, key: &Value) -> bool {
        self.watches.lock().unwrap().remove(key).is_some()
    }

    fn fire_watches(&self, old_value: Value, new_value: Value) {
        let watchers = self.watches.lock().unwrap();
        if watchers.is_empty() {
            return;
        }
        let entries: Vec<(Value, Value)> = watchers
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        drop(watchers);
        let agent_value = Value::Agent(self.clone());
        let mut first_error = None;
        for (key, callback) in entries {
            let mut args = Vec::with_capacity(4);
            args.push(key.clone());
            args.push(agent_value.clone());
            args.push(old_value.clone());
            args.push(new_value.clone());
            if let Err(err) = call_callable(callback.clone(), args) {
                if first_error.is_none() {
                    first_error = Some(err);
                }
            }
        }
        if let Some(err) = first_error {
            eprintln!("agent watch error: {}", err);
        }
    }

    pub fn send(&self, func: Value, args: Vec<Value>) -> bool {
        self.pending.fetch_add(1, Ordering::SeqCst);
        if self.sender.send(AgentMessage { func, args }).is_err() {
            self.pending.fetch_sub(1, Ordering::SeqCst);
            return false;
        }
        true
    }

    pub fn await_all(&self) {
        let (lock, cvar) = &*self.cond;
        let mut guard = lock.lock().unwrap();
        while self.pending.load(Ordering::SeqCst) > 0 || self.processing.load(Ordering::SeqCst) {
            guard = cvar.wait(guard).unwrap();
        }
    }

    pub fn is_idle(&self) -> bool {
        self.pending.load(Ordering::SeqCst) == 0 && !self.processing.load(Ordering::SeqCst)
    }

    pub fn error(&self) -> Option<CloveError> {
        self.error.lock().unwrap().clone()
    }

    pub fn debug_info(&self) -> AgentDebugInfo {
        AgentDebugInfo {
            state: self.state(),
            pending: self.pending.load(Ordering::SeqCst),
            processing: self.processing.load(Ordering::SeqCst),
            error: self.error(),
        }
    }

    pub fn restart(&self, value: Value) {
        let previous = self.state();
        *self.state.lock().unwrap() = value.clone();
        self.error.lock().unwrap().take();
        self.fire_watches(previous, value);
        let (_, cvar) = &*self.cond;
        cvar.notify_all();
    }

    pub(crate) fn ptr(&self) -> *const Mutex<Value> {
        Arc::as_ptr(&self.state)
    }
}

fn start_agent_worker(handle: AgentHandle, receiver: Receiver<AgentMessage>) {
    spawn_with_current_file(move || {
        while let Ok(message) = receiver.recv() {
            if handle.error.lock().unwrap().is_some() {
                handle.pending.fetch_sub(1, Ordering::SeqCst);
                continue;
            }
            handle.processing.store(true, Ordering::SeqCst);
            let previous_state = handle.state.lock().unwrap().clone();
            let mut args = Vec::with_capacity(1 + message.args.len());
            args.push(previous_state.clone());
            args.extend(message.args);
            let result = call_callable_guarded(message.func, args);
            match result {
                Ok(next) => {
                    *handle.state.lock().unwrap() = next.clone();
                    handle.fire_watches(previous_state, next);
                }
                Err(err) => {
                    *handle.error.lock().unwrap() = Some(err);
                    handle.fire_watches(previous_state.clone(), previous_state);
                }
            }
            handle.pending.fetch_sub(1, Ordering::SeqCst);
            handle.processing.store(false, Ordering::SeqCst);
            let (_, cvar) = &*handle.cond;
            cvar.notify_all();
        }
        let (_, cvar) = &*handle.cond;
        cvar.notify_all();
    });
}

#[derive(Clone)]
pub enum PromiseKind {
    Promise(PromiseHandle),
    Task(TaskHandle),
    Future(FutureHandle),
}

impl PromiseKind {
    pub fn wait(&self) -> Result<Value, CloveError> {
        match self {
            PromiseKind::Promise(handle) => handle.wait(),
            PromiseKind::Task(handle) => handle.wait(),
            PromiseKind::Future(handle) => handle.wait(),
        }
    }

    pub fn promise(&self) -> PromiseHandle {
        match self {
            PromiseKind::Promise(handle) => handle.clone(),
            PromiseKind::Task(handle) => handle.promise().clone(),
            PromiseKind::Future(handle) => handle.task().promise().clone(),
        }
    }

    pub fn is_done(&self) -> bool {
        match self {
            PromiseKind::Promise(handle) => handle.is_done(),
            PromiseKind::Task(handle) => handle.is_done(),
            PromiseKind::Future(handle) => handle.is_done(),
        }
    }

    pub fn is_cancelled(&self) -> bool {
        match self {
            PromiseKind::Promise(handle) => handle.is_cancelled(),
            PromiseKind::Task(handle) => handle.is_cancelled(),
            PromiseKind::Future(handle) => handle.is_cancelled(),
        }
    }

    pub fn current(&self) -> Option<Result<Value, CloveError>> {
        match self {
            PromiseKind::Promise(handle) => handle.current(),
            PromiseKind::Task(handle) => handle.current(),
            PromiseKind::Future(handle) => handle.current(),
        }
    }
}

pub fn promise_from_value(value: Value) -> PromiseHandle {
    let handle = PromiseHandle::new();
    handle.resolve(Ok(value));
    handle
}

pub fn promise_from_error(err: CloveError) -> PromiseHandle {
    let handle = PromiseHandle::new();
    handle.resolve(Err(err));
    handle
}

pub fn promise_like_from_value(value: &Value) -> Option<PromiseKind> {
    match value {
        Value::Promise(handle) => Some(PromiseKind::Promise(handle.clone())),
        Value::Task(handle) => Some(PromiseKind::Task(handle.clone())),
        Value::Future(handle) => Some(PromiseKind::Future(handle.clone())),
        _ => None,
    }
}

pub fn promise_then(source: PromiseKind, on_success: Value) -> PromiseHandle {
    let target = PromiseHandle::new();
    let target_clone = target.clone();
    spawn_with_current_file(move || match source.wait() {
        Ok(value) => match call_callable_guarded(on_success.clone(), vec![value]) {
            Ok(res) => settle_nested(res, target_clone.clone()),
            Err(err) => {
                target_clone.resolve(Err(err));
            }
        },
        Err(err) => {
            target_clone.resolve(Err(err));
        }
    });
    target
}

pub fn promise_catch(source: PromiseKind, on_error: Value) -> PromiseHandle {
    let target = PromiseHandle::new();
    let target_clone = target.clone();
    spawn_with_current_file(move || match source.wait() {
        Ok(value) => {
            target_clone.resolve(Ok(value));
        }
        Err(err) => match call_callable_guarded(on_error.clone(), vec![error_to_value(&err)]) {
            Ok(res) => settle_nested(res, target_clone.clone()),
            Err(handler_err) => {
                target_clone.resolve(Err(handler_err));
            }
        },
    });
    target
}

pub fn promise_finally(source: PromiseKind, on_finally: Value) -> PromiseHandle {
    let target = PromiseHandle::new();
    let target_clone = target.clone();
    let finalizer = on_finally.clone();
    spawn_with_current_file(move || {
        let outcome = source.wait();
        match call_callable_guarded(finalizer, Vec::new()) {
            Ok(_) => match outcome {
                Ok(value) => {
                    target_clone.resolve(Ok(value));
                }
                Err(err) => {
                    target_clone.resolve(Err(err));
                }
            },
            Err(err) => {
                target_clone.resolve(Err(err));
            }
        }
    });
    target
}

pub fn promise_all(sources: Vec<PromiseKind>) -> PromiseHandle {
    let target = PromiseHandle::new();
    let target_clone = target.clone();
    thread::spawn(move || {
        let mut results = Vec::with_capacity(sources.len());
        for src in sources {
            match src.wait() {
                Ok(value) => results.push(value),
                Err(err) => {
                    target_clone.resolve(Err(err));
                    return;
                }
            }
        }
        target_clone.resolve(Ok(Value::Vector(Vector::from(results))));
    });
    target
}

pub fn promise_all_settled(sources: Vec<PromiseKind>) -> PromiseHandle {
    let target = PromiseHandle::new();
    let target_clone = target.clone();
    thread::spawn(move || {
        let mut results = Vec::with_capacity(sources.len());
        for src in sources {
            let entry = match src.wait() {
                Ok(value) => settled_entry(":fulfilled", Some(value)),
                Err(err) => settled_entry(":rejected", Some(Value::String(err.to_string()))),
            };
            results.push(entry);
        }
        target_clone.resolve(Ok(Value::Vector(Vector::from(results))));
    });
    target
}

pub fn promise_race(sources: Vec<PromiseKind>) -> PromiseHandle {
    race_like(sources, RaceMode::First)
}

pub fn promise_any(sources: Vec<PromiseKind>) -> PromiseHandle {
    race_like(sources, RaceMode::Fulfilled)
}

enum RaceMode {
    First,
    Fulfilled,
}

fn race_like(sources: Vec<PromiseKind>, mode: RaceMode) -> PromiseHandle {
    let target = PromiseHandle::new();
    if sources.is_empty() {
        target.resolve(Err(CloveError::runtime("no promises provided")));
        return target;
    }
    let total = sources.len();
    let (tx, rx) = crossbeam_channel::unbounded();
    for (idx, src) in sources.into_iter().enumerate() {
        let tx_clone = tx.clone();
        thread::spawn(move || {
            let _ = tx_clone.send((idx, src.wait()));
        });
    }
    let target_clone = target.clone();
    thread::spawn(move || match mode {
        RaceMode::First => {
            let mut best: Option<(usize, Result<Value, CloveError>)> = None;
            for _ in 0..total {
                match rx.recv() {
                    Ok((idx, res)) => {
                        if idx == 0 {
                            target_clone.resolve(res);
                            return;
                        }
                        if best.as_ref().map_or(true, |(best_idx, _)| idx < *best_idx) {
                            best = Some((idx, res));
                        }
                    }
                    Err(_) => break,
                }
            }
            if let Some((_, res)) = best {
                target_clone.resolve(res);
            }
        }
        RaceMode::Fulfilled => {
            let mut last_err = None;
            for _ in 0..total {
                match rx.recv() {
                    Ok((_, Ok(value))) => {
                        target_clone.resolve(Ok(value));
                        return;
                    }
                    Ok((_, Err(err))) => last_err = Some(err),
                    Err(_) => break,
                }
            }
            if let Some(err) = last_err {
                target_clone.resolve(Err(err));
            }
        }
    });
    target
}

fn promise_settled_value(result: &Result<Value, CloveError>) -> Value {
    match result {
        Ok(value) => settled_entry(":fulfilled", Some(value.clone())),
        Err(err) => settled_entry(":rejected", Some(Value::String(err.to_string()))),
    }
}

fn settled_entry(status: &str, value: Option<Value>) -> Value {
    let mut map = HashMap::new();
    map.insert(Key::Keyword("status".into()), Value::Symbol(status.into()));
    if let Some(v) = value {
        let key = if status == ":rejected" {
            "error"
        } else {
            "value"
        };
        map.insert(Key::Keyword(key.into()), v);
    }
    Value::Map(map)
}

fn settle_nested(result: Value, target: PromiseHandle) {
    if let Some(kind) = promise_like_from_value(&result) {
        let promise = kind.promise();
        thread::spawn(move || {
            let _ = target.resolve(promise.wait());
        });
    } else {
        target.resolve(Ok(result));
    }
}

fn error_to_value(err: &CloveError) -> Value {
    Value::String(err.to_string())
}

#[derive(Clone)]
pub enum SelectOp {
    Take(ChanHandle),
    Put(ChanHandle, Value),
}

pub enum SelectResult {
    Take(Value, ChanHandle),
    Put(ChanHandle),
}

pub fn select_cases(cases: Vec<SelectOp>, timeout: Option<Duration>) -> Option<SelectResult> {
    if cases.is_empty() {
        return None;
    }
    let mut select = Select::new();
    let mut registrations = Vec::with_capacity(cases.len());
    for case in cases {
        match case {
            SelectOp::Take(handle) => {
                let receiver = Box::new(handle.receiver_clone());
                let receiver_ptr = Box::into_raw(receiver);
                let index = select.recv(unsafe { &*receiver_ptr });
                registrations.push((index, SelectKind::Take(handle, receiver_ptr)));
            }
            SelectOp::Put(handle, value) => {
                if let Some(sender) = handle.sender_clone() {
                    let sender_box = Box::new(sender);
                    let sender_ptr = Box::into_raw(sender_box);
                    let index = select.send(unsafe { &*sender_ptr });
                    registrations.push((index, SelectKind::Put(handle, sender_ptr, value)));
                }
            }
        }
    }
    let oper = match timeout {
        Some(dur) => match select.select_timeout(dur) {
            Ok(op) => op,
            Err(_) => return None,
        },
        None => select.select(),
    };
    let (_, kind) = registrations
        .into_iter()
        .find(|(idx, _)| *idx == oper.index())?;
    let mut owned_kind = kind;
    match &mut owned_kind {
        SelectKind::Take(handle, receiver_ptr) => {
            let recv_ref = unsafe { &**receiver_ptr };
            match oper.recv(recv_ref) {
                Ok(value) => Some(SelectResult::Take(value, handle.clone())),
                Err(_) => Some(SelectResult::Take(Value::Nil, handle.clone())),
            }
        }
        SelectKind::Put(handle, sender_ptr, value) => {
            let send_ref = unsafe { &**sender_ptr };
            let payload = std::mem::replace(value, Value::Nil);
            let _ = oper.send(send_ref, payload);
            Some(SelectResult::Put(handle.clone()))
        }
    }
}

enum SelectKind {
    Take(ChanHandle, *mut Receiver<Value>),
    Put(ChanHandle, *mut Sender<Value>, Value),
}

impl Drop for SelectKind {
    fn drop(&mut self) {
        unsafe {
            match self {
                SelectKind::Take(_, receiver_ptr) => {
                    let _ = Box::from_raw(*receiver_ptr);
                }
                SelectKind::Put(_, sender_ptr, _) => {
                    let _ = Box::from_raw(*sender_ptr);
                }
            }
        }
    }
}

pub fn timeout_channel(duration: Duration) -> ChanHandle {
    let chan = ChanHandle::new(Some(0));
    let clone = chan.clone();
    thread::spawn(move || {
        thread::sleep(duration);
        let _ = clone.put(Value::Nil);
        clone.close();
    });
    chan
}

pub fn done_value(value: &Value) -> Result<bool, CloveError> {
    match value {
        Value::Agent(handle) => Ok(handle.is_idle()),
        _ => {
            let kind = promise_like_from_value(value).ok_or_else(|| {
                CloveError::type_mismatch("promise/task/future", value.type_name())
            })?;
            Ok(promise_kind_done(&kind))
        }
    }
}

pub fn promise_error_value(value: &Value) -> Result<Value, CloveError> {
    let kind = promise_like_from_value(value)
        .ok_or_else(|| CloveError::type_mismatch("promise/task/future", value.type_name()))?;
    promise_error_for_kind(&kind)
}

pub fn add_watch_value(target: &Value, key: Value, func: Value) -> Result<Value, CloveError> {
    match target {
        Value::Atom(handle) => {
            handle.add_watch(key, func);
            Ok(Value::Atom(handle.clone()))
        }
        Value::Agent(handle) => {
            handle.add_watch(key, func);
            Ok(Value::Agent(handle.clone()))
        }
        Value::Promise(handle) => {
            handle.add_watch(target.clone(), key, func);
            Ok(target.clone())
        }
        Value::Task(handle) => {
            handle.add_watch(key, func);
            Ok(Value::Task(handle.clone()))
        }
        Value::Future(handle) => {
            handle.add_watch(key, func);
            Ok(Value::Future(handle.clone()))
        }
        other => Err(CloveError::type_mismatch(
            "watchable value (atom/agent/promise/task/future)",
            other.type_name(),
        )),
    }
}

pub fn remove_watch_value(target: &Value, key: &Value) -> Result<bool, CloveError> {
    match target {
        Value::Atom(handle) => Ok(handle.remove_watch(key)),
        Value::Agent(handle) => Ok(handle.remove_watch(key)),
        Value::Promise(handle) => Ok(handle.remove_watch(key)),
        Value::Task(handle) => Ok(handle.remove_watch(key)),
        Value::Future(handle) => Ok(handle.remove_watch(key)),
        other => Err(CloveError::type_mismatch(
            "watchable value (atom/agent/promise/task/future)",
            other.type_name(),
        )),
    }
}

pub(crate) fn promise_kind_done(kind: &PromiseKind) -> bool {
    for _ in 0..5 {
        if kind.is_done() {
            return true;
        }
        std::thread::yield_now();
        std::thread::sleep(Duration::from_millis(1));
    }
    kind.is_done()
}

fn promise_error_for_kind(kind: &PromiseKind) -> Result<Value, CloveError> {
    let mut current = kind.current();
    for _ in 0..5 {
        if let Some(res) = current {
            return Ok(match res {
                Ok(_) => Value::Nil,
                Err(err) => Value::String(err.to_string()),
            });
        }
        std::thread::yield_now();
        current = kind.current();
        std::thread::sleep(Duration::from_millis(1));
    }
    Ok(Value::Nil)
}

fn value_truthy(value: &Value) -> bool {
    !matches!(value, Value::Nil | Value::Bool(false))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FnArity, Span, Value};
    use crate::error::format_error;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn spawn_future_inherits_current_file_for_error_reporting() {
        let previous = current_file_name();
        set_current_file(Some("TEST.clv".into()));

        let callable = Value::native_fn(FnArity::exact(0), |_| {
            Err(CloveError::runtime("boom")
                .with_span(Span {
                    line: 1,
                    col: 1,
                    index: 0,
                })
                .with_file(current_file_name()))
        });
        let future = spawn_future(callable);
        let err = future.wait().unwrap_err();
        let lines = format_error(&err);

        set_current_file(previous);
        assert!(
            lines.iter().any(|line| line.contains("TEST.clv:1:1")),
            "expected file name to appear in error output: {:?}",
            lines
        );
    }

    #[test]
    fn spawn_task_panics_do_not_block_wait() {
        let callable = Value::native_fn(FnArity::exact(0), |_| {
            panic!("boom");
        });
        let task = spawn_task(callable);
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let result = task.wait();
            let _ = tx.send(result);
        });
        let result = rx
            .recv_timeout(Duration::from_secs(1))
            .expect("task wait timed out");
        assert!(result.is_err(), "panic should become an error");
    }

    #[test]
    fn agent_panics_do_not_block_await_all() {
        let agent = AgentHandle::new(Value::Int(0));
        let callable = Value::native_fn(FnArity::exact(1), |_| {
            panic!("boom");
        });
        let (tx, rx) = mpsc::channel();
        let agent_clone = agent.clone();
        thread::spawn(move || {
            agent_clone.await_all();
            let _ = tx.send(());
        });
        assert!(agent.send(callable, Vec::new()));
        rx.recv_timeout(Duration::from_secs(1))
            .expect("agent await_all timed out");
        assert!(agent.error().is_some(), "panic should set agent error");
    }
}
