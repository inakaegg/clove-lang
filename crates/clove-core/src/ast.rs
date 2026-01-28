use std::cell::{Cell, UnsafeCell};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap as StdHashMap;
use std::collections::HashSet as StdHashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
use std::thread;
use std::thread::ThreadId;

use crate::concurrency::{
    AgentHandle, AtomHandle, ChanHandle, DelayHandle, FutureHandle, PromiseHandle,
    PromiseStateView, TaskHandle,
};
pub use crate::cow::{HashMap, Vector};
use crate::error::CloveError;
use crate::foreign::ForeignEngine;
use crate::seq::SeqHandle;
use crate::settings::RuntimeSettings;
use crate::string_escape::escape_string_fragment;
use crate::symbols::canonical_symbol_name;
use im::HashSet;
use regex::Regex;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Form {
    pub kind: FormKind,
    pub span: Span,
    pub type_hint: Option<crate::types::TypeHint>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolatedPart {
    Text(String),
    Expr(Form),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FormKind {
    Symbol(String),
    Keyword(String),
    Int(i64),
    Float(f64),
    String(String),
    InterpolatedString(Vec<InterpolatedPart>),
    Bool(bool),
    Nil,
    Duration(DurationValue),
    ShortFn(Vec<Form>),
    List(Vec<Form>),
    Vector(Vec<Form>),
    Map(Vec<MapItem>),
    Set(Vec<Form>),
    Regex {
        pattern: String,
        delim: RegexDelim,
    },
    InterpolatedRegex {
        parts: Vec<InterpolatedPart>,
        delim: RegexDelim,
    },
    ForeignBlock {
        tag: String,
        code: String,
    },
    ForeignRaw {
        tag: Option<String>,
        code: String,
    },
    ForeignSymbol {
        tag: Option<String>,
        path: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MapItem {
    KeyValue(Form, Form),
    Spread(Form),
}

impl Form {
    pub fn new(kind: FormKind, span: Span) -> Self {
        Self {
            kind,
            span,
            type_hint: None,
        }
    }

    pub fn with_type_hint(mut self, hint: crate::types::TypeHint) -> Self {
        self.type_hint = Some(hint);
        self
    }
}

impl Eq for FormKind {}

impl Hash for FormKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            FormKind::Symbol(s) | FormKind::Keyword(s) | FormKind::String(s) => s.hash(state),
            FormKind::Int(n) => n.hash(state),
            FormKind::Float(f) => f.to_bits().hash(state),
            FormKind::Bool(b) => b.hash(state),
            FormKind::Nil => {}
            FormKind::InterpolatedString(parts) => parts.hash(state),
            FormKind::Duration(d) => d.hash(state),
            FormKind::ShortFn(body) => body.hash(state),
            FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
                items.hash(state)
            }
            FormKind::Map(entries) => entries.hash(state),
            FormKind::Regex { pattern, delim } => {
                pattern.hash(state);
                delim.hash(state);
            }
            FormKind::InterpolatedRegex { parts, delim } => {
                parts.hash(state);
                delim.hash(state);
            }
            FormKind::ForeignBlock { tag, code } => {
                tag.hash(state);
                code.hash(state);
            }
            FormKind::ForeignRaw { tag, code } => {
                tag.hash(state);
                code.hash(state);
            }
            FormKind::ForeignSymbol { tag, path } => {
                tag.hash(state);
                path.hash(state);
            }
        }
    }
}

pub fn desugar_interpolated_string(form: &Form) -> Option<Form> {
    let parts = match &form.kind {
        FormKind::InterpolatedString(parts) => parts,
        _ => return None,
    };
    Some(interpolated_parts_to_str_form(parts, form.span))
}

pub fn desugar_interpolated_regex(form: &Form) -> Option<Form> {
    let parts = match &form.kind {
        FormKind::InterpolatedRegex { parts, .. } => parts,
        _ => return None,
    };
    let mut items = Vec::with_capacity(2);
    items.push(Form::new(
        FormKind::Symbol("re-pattern".to_string()),
        form.span,
    ));
    items.push(interpolated_parts_to_str_form(parts, form.span));
    Some(Form::new(FormKind::List(items), form.span))
}

fn interpolated_parts_to_str_form(parts: &[InterpolatedPart], span: Span) -> Form {
    let mut items = Vec::with_capacity(parts.len() + 1);
    items.push(Form::new(FormKind::Symbol("str".to_string()), span));
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => {
                items.push(Form::new(FormKind::String(text.clone()), span));
            }
            InterpolatedPart::Expr(expr) => {
                items.push(expr.clone());
            }
        }
    }
    Form::new(FormKind::List(items), span)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegexDelim {
    Slash,
    Hash,
    HashSlash,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DurationUnit {
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Year,
}

impl DurationUnit {
    pub fn suffix(&self) -> &'static str {
        match self {
            DurationUnit::Millisecond => "ms",
            DurationUnit::Second => "s",
            DurationUnit::Minute => "m",
            DurationUnit::Hour => "h",
            DurationUnit::Day => "d",
            DurationUnit::Week => "w",
            DurationUnit::Year => "y",
        }
    }

    pub fn nanos_per_unit(&self) -> i128 {
        const NANOS_PER_MS: i128 = 1_000_000;
        match self {
            DurationUnit::Millisecond => NANOS_PER_MS,
            DurationUnit::Second => NANOS_PER_MS * 1_000,
            DurationUnit::Minute => NANOS_PER_MS * 60_000,
            DurationUnit::Hour => NANOS_PER_MS * 3_600_000,
            DurationUnit::Day => NANOS_PER_MS * 86_400_000,
            DurationUnit::Week => NANOS_PER_MS * 604_800_000,
            DurationUnit::Year => NANOS_PER_MS * 31_536_000_000,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DurationValue {
    nanos: i128,
}

impl DurationValue {
    pub fn from_nanos(nanos: i128) -> Result<Self, CloveError> {
        if nanos < 0 {
            return Err(CloveError::runtime("duration cannot be negative"));
        }
        Ok(Self { nanos })
    }

    pub fn from_unit(magnitude: i128, unit: DurationUnit) -> Result<Self, CloveError> {
        let nanos = magnitude
            .checked_mul(unit.nanos_per_unit())
            .ok_or_else(|| CloveError::runtime("duration literal overflowed"))?;
        Self::from_nanos(nanos)
    }

    pub fn as_nanos(&self) -> i128 {
        self.nanos
    }

    pub fn to_unit(&self, unit: DurationUnit) -> f64 {
        self.nanos as f64 / unit.nanos_per_unit() as f64
    }

    pub fn to_millis_i64(&self) -> Result<i64, CloveError> {
        const NANOS_PER_MS: i128 = 1_000_000;
        let millis = self.nanos / NANOS_PER_MS;
        if millis > i64::MAX as i128 {
            return Err(CloveError::runtime("duration too large"));
        }
        Ok(millis as i64)
    }

    pub fn pretty(&self) -> String {
        for unit in [
            DurationUnit::Year,
            DurationUnit::Week,
            DurationUnit::Day,
            DurationUnit::Hour,
            DurationUnit::Minute,
            DurationUnit::Second,
            DurationUnit::Millisecond,
        ] {
            let nanos_per = unit.nanos_per_unit();
            if self.nanos % nanos_per == 0 {
                let magnitude = self.nanos / nanos_per;
                return format!("{}{}", magnitude, unit.suffix());
            }
        }
        format!("{}ns", self.nanos)
    }
}

impl fmt::Display for DurationValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FnArity {
    min: usize,
    max: Option<usize>,
}

impl FnArity {
    pub fn new(min: usize, max: Option<usize>) -> Self {
        if let Some(max_val) = max {
            assert!(min <= max_val, "min arity cannot exceed max arity");
        }
        Self { min, max }
    }

    pub fn exact(count: usize) -> Self {
        Self::new(count, Some(count))
    }

    pub fn at_least(min: usize) -> Self {
        Self::new(min, None)
    }

    pub fn range(min: usize, max: usize) -> Self {
        Self::new(min, Some(max))
    }

    pub fn min(&self) -> usize {
        self.min
    }

    pub fn max(&self) -> Option<usize> {
        self.max
    }

    pub fn remaining_after(&self, provided: usize) -> Self {
        debug_assert!(provided <= self.min);
        let min = self.min - provided;
        let max = self.max.map(|max_val| {
            debug_assert!(provided <= max_val);
            max_val - provided
        });
        Self { min, max }
    }
}

pub struct NativeFn {
    func: Box<dyn Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync>,
    arity: FnArity,
    debug_name: Option<Arc<str>>,
}

impl NativeFn {
    pub fn new(
        arity: FnArity,
        func: impl Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync + 'static,
    ) -> Self {
        Self {
            func: Box::new(func),
            arity,
            debug_name: None,
        }
    }

    pub fn with_name(
        arity: FnArity,
        name: impl Into<String>,
        func: impl Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync + 'static,
    ) -> Self {
        let mut nf = Self::new(arity, func);
        let canonical = canonical_symbol_name(&name.into()).into_owned();
        nf.debug_name = Some(canonical.into());
        nf
    }

    pub fn call(&self, args: &[Value]) -> Result<Value, CloveError> {
        (self.func)(args)
    }

    pub fn arity(&self) -> FnArity {
        self.arity
    }

    pub fn debug_name(&self) -> Option<&str> {
        self.debug_name.as_deref()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ComposeKind {
    Comp,
    Pipe,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NativeBufTy {
    I32,
    F32,
    I64,
}

struct BorrowFlagGuard<'a> {
    flag: &'a Cell<bool>,
}

impl<'a> BorrowFlagGuard<'a> {
    fn new(flag: &'a Cell<bool>) -> Self {
        Self { flag }
    }
}

impl<'a> Drop for BorrowFlagGuard<'a> {
    fn drop(&mut self) {
        self.flag.set(false);
    }
}

pub struct MainThreadBuf<T> {
    owner: ThreadId,
    data: UnsafeCell<Vec<T>>,
    borrowed: Cell<bool>,
}

unsafe impl<T: Send> Send for MainThreadBuf<T> {}
unsafe impl<T: Send> Sync for MainThreadBuf<T> {}

impl<T> MainThreadBuf<T> {
    pub fn new(vec: Vec<T>) -> Self {
        Self {
            owner: thread::current().id(),
            data: UnsafeCell::new(vec),
            borrowed: Cell::new(false),
        }
    }

    pub(crate) fn assert_owner(&self, op: &str) -> Result<(), CloveError> {
        if thread::current().id() != self.owner {
            return Err(CloveError::runtime(format!(
                "{op} main-thread buffer accessed from another thread"
            )));
        }
        Ok(())
    }

    pub(crate) fn data_ptr(&self) -> *mut Vec<T> {
        self.data.get()
    }

    pub fn begin_borrow(&self, op: &str) -> Result<*mut Vec<T>, CloveError> {
        self.assert_owner(op)?;
        if self.borrowed.replace(true) {
            return Err(CloveError::runtime(format!(
                "{op} native buffer already borrowed"
            )));
        }
        Ok(self.data.get())
    }

    pub fn end_borrow(&self) {
        self.borrowed.set(false);
    }

    pub fn with_mut<R>(
        &self,
        op: &str,
        f: impl FnOnce(&mut Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        self.assert_owner(op)?;
        if self.borrowed.replace(true) {
            return Err(CloveError::runtime(format!(
                "{op} native buffer already borrowed"
            )));
        }
        let _guard = BorrowFlagGuard::new(&self.borrowed);
        let result = unsafe { f(&mut *self.data.get()) };
        result
    }

    pub fn with_ref<R>(
        &self,
        op: &str,
        f: impl FnOnce(&Vec<T>) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        self.assert_owner(op)?;
        if self.borrowed.replace(true) {
            return Err(CloveError::runtime(format!(
                "{op} native buffer already borrowed"
            )));
        }
        let _guard = BorrowFlagGuard::new(&self.borrowed);
        let result = unsafe { f(&*self.data.get()) };
        result
    }
}

#[derive(Clone)]
pub enum NativeBufHandle {
    I32(Arc<Mutex<Vec<i32>>>),
    I32Main(Arc<MainThreadBuf<i32>>),
    F32(Arc<Mutex<Vec<f32>>>),
    F32Main(Arc<MainThreadBuf<f32>>),
    I64(Arc<Mutex<Vec<i64>>>),
    I64Main(Arc<MainThreadBuf<i64>>),
}

impl NativeBufHandle {
    pub fn ty(&self) -> NativeBufTy {
        match self {
            NativeBufHandle::I32(_) | NativeBufHandle::I32Main(_) => NativeBufTy::I32,
            NativeBufHandle::F32(_) | NativeBufHandle::F32Main(_) => NativeBufTy::F32,
            NativeBufHandle::I64(_) | NativeBufHandle::I64Main(_) => NativeBufTy::I64,
        }
    }
}

#[derive(Clone)]
pub struct TransientHandle {
    state: Arc<Mutex<TransientState>>,
    epoch: u64,
}

#[derive(Debug)]
pub enum TransientKind {
    Vector(Vector<Value>),
    Map(TransientMap),
    Set(HashSet<Value>),
}

#[derive(Debug)]
pub enum TransientMap {
    Persistent(HashMap<Key, Value>),
    Builder(StdHashMap<Key, Value>),
}

pub enum TransientMapIter<'a> {
    Persistent(crate::cow::MapIter<'a, Key, Value>),
    Builder(std::collections::hash_map::Iter<'a, Key, Value>),
}

impl<'a> Iterator for TransientMapIter<'a> {
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TransientMapIter::Persistent(iter) => iter.next(),
            TransientMapIter::Builder(iter) => iter.next(),
        }
    }
}

impl TransientMap {
    pub fn from_persistent(map: HashMap<Key, Value>) -> Self {
        Self::Persistent(map)
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Persistent(map) => map.len(),
            Self::Builder(map) => map.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains_key(&self, key: &Key) -> bool {
        match self {
            Self::Persistent(map) => map.contains_key(key),
            Self::Builder(map) => map.contains_key(key),
        }
    }

    pub fn get(&self, key: &Key) -> Option<&Value> {
        match self {
            Self::Persistent(map) => map.get(key),
            Self::Builder(map) => map.get(key),
        }
    }

    pub fn insert(&mut self, key: Key, value: Value) {
        self.ensure_builder().insert(key, value);
    }

    pub fn remove(&mut self, key: &Key) {
        self.ensure_builder().remove(key);
    }

    pub fn extend_map(&mut self, map: &HashMap<Key, Value>) {
        let out = self.ensure_builder();
        for (k, v) in map.iter() {
            out.insert(k.clone(), v.clone());
        }
    }

    pub fn extend_sorted_map(&mut self, map: &SortedMap) {
        let out = self.ensure_builder();
        for (k, v) in &map.entries {
            out.insert(k.clone(), v.clone());
        }
    }

    pub fn iter(&self) -> TransientMapIter<'_> {
        match self {
            Self::Persistent(map) => TransientMapIter::Persistent(map.iter()),
            Self::Builder(map) => TransientMapIter::Builder(map.iter()),
        }
    }

    pub fn into_persistent(self) -> HashMap<Key, Value> {
        match self {
            Self::Persistent(map) => map,
            Self::Builder(map) => HashMap::from(map),
        }
    }

    fn ensure_builder(&mut self) -> &mut StdHashMap<Key, Value> {
        let next = match self {
            Self::Persistent(map) => {
                let mut out = StdHashMap::with_capacity(map.len());
                for (k, v) in map.iter() {
                    out.insert(k.clone(), v.clone());
                }
                out
            }
            Self::Builder(_) => return self.as_builder_mut(),
        };
        *self = Self::Builder(next);
        self.as_builder_mut()
    }

    fn as_builder_mut(&mut self) -> &mut StdHashMap<Key, Value> {
        match self {
            Self::Builder(map) => map,
            Self::Persistent(_) => unreachable!("builder missing"),
        }
    }
}

#[derive(Debug)]
pub struct TransientState {
    kind: Option<TransientKind>,
    alive: bool,
    epoch: u64,
    owner: ThreadId,
}

impl TransientHandle {
    pub fn new(kind: TransientKind) -> Self {
        Self {
            state: Arc::new(Mutex::new(TransientState {
                kind: Some(kind),
                alive: true,
                epoch: 0,
                owner: thread::current().id(),
            })),
            epoch: 0,
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.state, &other.state)
    }

    pub fn ptr(&self) -> *const () {
        Arc::as_ptr(&self.state) as *const ()
    }

    pub fn read<R>(
        &self,
        op: &str,
        f: impl FnOnce(&TransientKind) -> Result<R, CloveError>,
    ) -> Result<R, CloveError> {
        let guard = self.guard(op)?;
        let kind = guard
            .kind
            .as_ref()
            .ok_or_else(|| CloveError::runtime("transient is not alive"))?;
        f(kind)
    }

    pub fn write<R>(
        &self,
        op: &str,
        f: impl FnOnce(&mut TransientKind) -> Result<R, CloveError>,
    ) -> Result<(R, Self), CloveError> {
        let mut guard = self.guard(op)?;
        guard.epoch = guard.epoch.saturating_add(1);
        let next_epoch = guard.epoch;
        let kind = guard
            .kind
            .as_mut()
            .ok_or_else(|| CloveError::runtime("transient is not alive"))?;
        let out = f(kind)?;
        Ok((out, self.with_epoch(next_epoch)))
    }

    pub fn persist(&self, op: &str) -> Result<TransientKind, CloveError> {
        let mut guard = self.guard(op)?;
        guard.alive = false;
        guard
            .kind
            .take()
            .ok_or_else(|| CloveError::runtime("transient already persisted"))
    }

    fn with_epoch(&self, epoch: u64) -> Self {
        Self {
            state: Arc::clone(&self.state),
            epoch,
        }
    }

    fn guard(&self, op: &str) -> Result<std::sync::MutexGuard<'_, TransientState>, CloveError> {
        let guard = self
            .state
            .lock()
            .map_err(|_| CloveError::runtime("transient lock poisoned"))?;
        if !guard.alive {
            return Err(CloveError::runtime(format!(
                "{} on transient after persistent!",
                op
            )));
        }
        if guard.owner != thread::current().id() {
            return Err(CloveError::runtime(format!(
                "{} on transient from another thread",
                op
            )));
        }
        if guard.epoch != self.epoch {
            return Err(CloveError::runtime(format!(
                "{} on stale transient; use returned value",
                op
            )));
        }
        Ok(guard)
    }
}

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    List(Vector<Value>),
    Vector(Vector<Value>),
    Map(HashMap<Key, Value>),
    SortedMap(SortedMap),
    Set(HashSet<Value>),
    SortedSet(SortedSet),
    MutVector(Arc<Mutex<Vector<Value>>>),
    MutMap(Arc<Mutex<HashMap<Key, Value>>>),
    MutSet(Arc<Mutex<HashSet<Value>>>),
    TransientVector(TransientHandle),
    TransientMap(TransientHandle),
    TransientSet(TransientHandle),
    Regex(RegexValue),
    Duration(DurationValue),
    Func(Arc<NativeFn>),
    Partial {
        callable: Box<Value>,
        captured: Vec<Value>,
        remaining: FnArity,
    },
    Compose {
        funcs: Vec<Value>,
        kind: ComposeKind,
    },
    Atom(AtomHandle),
    Chan(ChanHandle),
    Promise(PromiseHandle),
    Task(TaskHandle),
    Future(FutureHandle),
    Agent(AgentHandle),
    Delay(DelayHandle),
    NativeBuf {
        id: u64,
        ty: NativeBufTy,
        handle: NativeBufHandle,
    },
    Seq(SeqHandle),
    Lambda {
        params: Vec<String>,
        rest: Option<String>,
        body: Vec<Form>,
        local_defns: Vec<LocalDefn>,
        env: crate::env::EnvRef,
        engines: Vec<Arc<dyn ForeignEngine>>,
        auto_fallback: bool,
        call_wrappers: Arc<StdHashSet<String>>,
        settings: RuntimeSettings,
        meta: Option<HashMap<Key, Value>>,
        doc: Option<String>,
        name: Option<String>,
        inferred_type: Option<crate::types::TypeKind>,
        recur_id: usize,
    },
    MultiLambda {
        clauses: Vec<LambdaClause>,
        env: crate::env::EnvRef,
        engines: Vec<Arc<dyn ForeignEngine>>,
        auto_fallback: bool,
        call_wrappers: Arc<StdHashSet<String>>,
        settings: RuntimeSettings,
        meta: Option<HashMap<Key, Value>>,
        doc: Option<String>,
        name: Option<String>,
        inferred_type: Option<crate::types::TypeKind>,
    },
    Symbol(String),
    Foreign(crate::foreign::ForeignValue),
    ForeignCallable {
        tag: String,
        path: String,
        engine: Arc<dyn ForeignEngine>,
        span: Option<Span>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LocalDefnClause {
    pub params: Vec<String>,
    pub rest: Option<String>,
    pub body: Vec<Form>,
    pub local_defns: Vec<LocalDefn>,
    pub inferred_type: Option<crate::types::TypeKind>,
    pub recur_id: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LocalDefn {
    pub name: String,
    pub doc: Option<String>,
    pub meta: Option<HashMap<Key, Value>>,
    pub clauses: Vec<LocalDefnClause>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LambdaClause {
    pub params: Vec<String>,
    pub rest: Option<String>,
    pub body: Vec<Form>,
    pub local_defns: Vec<LocalDefn>,
    pub recur_id: usize,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Key {
    Keyword(String),
    Symbol(String),
    String(String),
    Number(i64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub struct SortedMap {
    pub comparator: Box<Value>,
    pub entries: Vec<(Key, Value)>,
}

#[derive(Clone, Debug)]
pub struct SortedSet {
    pub comparator: Box<Value>,
    pub entries: Vec<Value>,
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) | Value::Float(_) => "number",
            Value::String(_) => "str",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::List(_) => "list",
            Value::Vector(_) => "vector",
            Value::Map(_) | Value::SortedMap(_) => "map",
            Value::Set(_) | Value::SortedSet(_) => "set",
            Value::MutVector(_) => "mut-vector",
            Value::MutMap(_) => "mut-map",
            Value::MutSet(_) => "mut-set",
            Value::TransientVector(_) => "transient-vector",
            Value::TransientMap(_) => "transient-map",
            Value::TransientSet(_) => "transient-set",
            Value::Regex(_) => "regex",
            Value::Duration(_) => "duration",
            Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. } => "function",
            Value::Atom(_) => "atom",
            Value::Chan(_) => "channel",
            Value::Promise(_) => "promise",
            Value::Task(_) => "task",
            Value::Future(_) => "future",
            Value::Agent(_) => "agent",
            Value::Delay(_) => "delay",
            Value::NativeBuf { ty, .. } => match ty {
                NativeBufTy::I32 => "native/i32buf",
                NativeBufTy::F32 => "native/f32buf",
                NativeBufTy::I64 => "native/i64buf",
            },
            Value::Seq(_) => "seq",
            Value::Symbol(_) => "symbol",
            Value::Foreign(_) => "foreign",
            Value::ForeignCallable { .. } => "function",
        }
    }

    pub fn native_fn(
        arity: FnArity,
        func: impl Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync + 'static,
    ) -> Self {
        Value::Func(Arc::new(NativeFn::new(arity, func)))
    }

    pub fn native_fn_with_name(
        name: impl Into<String>,
        arity: FnArity,
        func: impl Fn(&[Value]) -> Result<Value, CloveError> + Send + Sync + 'static,
    ) -> Self {
        Value::Func(Arc::new(NativeFn::with_name(arity, name, func)))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Map(a), Value::SortedMap(b)) => map_entries_equal_map(a, &b.entries),
            (Value::SortedMap(a), Value::Map(b)) => map_entries_equal_map(b, &a.entries),
            (Value::SortedMap(a), Value::SortedMap(b)) => sorted_map_entries_equal(a, b),
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::Set(a), Value::SortedSet(b)) => set_entries_equal_set(a, &b.entries),
            (Value::SortedSet(a), Value::Set(b)) => set_entries_equal_set(b, &a.entries),
            (Value::SortedSet(a), Value::SortedSet(b)) => sorted_set_entries_equal(a, b),
            (Value::MutVector(a), Value::MutVector(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == right
            }
            (Value::MutVector(a), Value::Vector(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == *b
            }
            (Value::Vector(a), Value::MutVector(b)) => {
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                *a == right
            }
            (Value::MutMap(a), Value::MutMap(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == right
            }
            (Value::MutMap(a), Value::Map(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == *b
            }
            (Value::Map(a), Value::MutMap(b)) => {
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                *a == right
            }
            (Value::MutMap(a), Value::SortedMap(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                map_entries_equal_map(&left, &b.entries)
            }
            (Value::SortedMap(a), Value::MutMap(b)) => {
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                map_entries_equal_map(&right, &a.entries)
            }
            (Value::MutSet(a), Value::MutSet(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == right
            }
            (Value::MutSet(a), Value::Set(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                left == *b
            }
            (Value::Set(a), Value::MutSet(b)) => {
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                *a == right
            }
            (Value::MutSet(a), Value::SortedSet(b)) => {
                let left = a.lock().unwrap_or_else(|e| e.into_inner()).clone();
                set_entries_equal_set(&left, &b.entries)
            }
            (Value::SortedSet(a), Value::MutSet(b)) => {
                let right = b.lock().unwrap_or_else(|e| e.into_inner()).clone();
                set_entries_equal_set(&right, &a.entries)
            }
            (Value::TransientVector(a), Value::TransientVector(b)) => {
                a.ptr_eq(b) && a.epoch == b.epoch
            }
            (Value::TransientMap(a), Value::TransientMap(b)) => a.ptr_eq(b) && a.epoch == b.epoch,
            (Value::TransientSet(a), Value::TransientSet(b)) => a.ptr_eq(b) && a.epoch == b.epoch,
            (Value::Regex(a), Value::Regex(b)) => a == b,
            (Value::Duration(a), Value::Duration(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Func(a), Value::Func(b)) => Arc::ptr_eq(a, b),
            (
                Value::Partial {
                    callable: ac,
                    captured: aa,
                    remaining: ar,
                },
                Value::Partial {
                    callable: bc,
                    captured: ba,
                    remaining: br,
                },
            ) => ac == bc && aa == ba && ar == br,
            (
                Value::Compose {
                    funcs: af,
                    kind: ak,
                },
                Value::Compose {
                    funcs: bf,
                    kind: bk,
                },
            ) => ak == bk && af == bf,
            (Value::Atom(a), Value::Atom(b)) => a.ptr() == b.ptr(),
            (Value::Chan(a), Value::Chan(b)) => a.ptr() == b.ptr(),
            (Value::Promise(a), Value::Promise(b)) => a.ptr() == b.ptr(),
            (Value::Task(a), Value::Task(b)) => a.ptr() == b.ptr(),
            (Value::Future(a), Value::Future(b)) => a.ptr() == b.ptr(),
            (Value::Agent(a), Value::Agent(b)) => a.ptr() == b.ptr(),
            (Value::Delay(a), Value::Delay(b)) => a.ptr() == b.ptr(),
            (
                Value::NativeBuf {
                    id: aid, ty: aty, ..
                },
                Value::NativeBuf {
                    id: bid, ty: bty, ..
                },
            ) => aid == bid && aty == bty,
            (Value::Seq(a), Value::Seq(b)) => a.ptr_eq(b),
            (
                Value::Lambda {
                    params: ap,
                    rest: ar,
                    body: ab,
                    local_defns: alocal_defns,
                    env: ae,
                    engines: aeng,
                    auto_fallback: af,
                    call_wrappers: ac,
                    settings: aset,
                    meta: ameta,
                    doc: adoc,
                    name: aname,
                    inferred_type: ainferred,
                    recur_id: arid,
                },
                Value::Lambda {
                    params: bp,
                    rest: br,
                    body: bb,
                    local_defns: blocal_defns,
                    env: be,
                    engines: beng,
                    auto_fallback: bf,
                    call_wrappers: bc,
                    settings: bset,
                    meta: bmeta,
                    doc: bdoc,
                    name: bname,
                    inferred_type: binferred,
                    recur_id: brid,
                },
            ) => {
                ap == bp
                    && ar == br
                    && ab == bb
                    && alocal_defns == blocal_defns
                    && af == bf
                    && ameta == bmeta
                    && adoc == bdoc
                    && aname == bname
                    && Arc::ptr_eq(ae, be)
                    && aeng.len() == beng.len()
                    && aeng.iter().zip(beng.iter()).all(|(a, b)| Arc::ptr_eq(a, b))
                    && Arc::ptr_eq(ac, bc)
                    && aset == bset
                    && ainferred == binferred
                    && arid == brid
            }
            (
                Value::MultiLambda {
                    clauses: ac,
                    env: ae,
                    engines: aeng,
                    auto_fallback: af,
                    call_wrappers: acw,
                    settings: aset,
                    meta: ameta,
                    doc: adoc,
                    name: aname,
                    inferred_type: ainferred,
                },
                Value::MultiLambda {
                    clauses: bc,
                    env: be,
                    engines: beng,
                    auto_fallback: bf,
                    call_wrappers: bcw,
                    settings: bset,
                    meta: bmeta,
                    doc: bdoc,
                    name: bname,
                    inferred_type: binferred,
                },
            ) => {
                ac == bc
                    && af == bf
                    && ameta == bmeta
                    && adoc == bdoc
                    && aname == bname
                    && Arc::ptr_eq(ae, be)
                    && aeng.len() == beng.len()
                    && aeng.iter().zip(beng.iter()).all(|(a, b)| Arc::ptr_eq(a, b))
                    && Arc::ptr_eq(acw, bcw)
                    && aset == bset
                    && ainferred == binferred
            }
            (Value::Foreign(a), Value::Foreign(b)) => {
                a.tag == b.tag && Arc::ptr_eq(&a.data, &b.data)
            }
            (
                Value::ForeignCallable {
                    tag: atag,
                    path: apath,
                    engine: aeng,
                    span: aspan,
                },
                Value::ForeignCallable {
                    tag: btag,
                    path: bpath,
                    engine: beng,
                    span: bspan,
                },
            ) => atag == btag && apath == bpath && Arc::ptr_eq(aeng, beng) && aspan == bspan,
            _ => false,
        }
    }
}

impl Eq for Value {}

fn map_entries_equal_map(map: &HashMap<Key, Value>, entries: &[(Key, Value)]) -> bool {
    if map.len() != entries.len() {
        return false;
    }
    for (k, v) in entries {
        match map.get(k) {
            Some(other) if other == v => {}
            _ => return false,
        }
    }
    true
}

fn sorted_map_entries_equal(a: &SortedMap, b: &SortedMap) -> bool {
    if a.entries.len() != b.entries.len() {
        return false;
    }
    let mut map = HashMap::new();
    for (k, v) in &b.entries {
        map.insert(k.clone(), v.clone());
    }
    map_entries_equal_map(&map, &a.entries)
}

fn set_entries_equal_set(set: &HashSet<Value>, entries: &[Value]) -> bool {
    if set.len() != entries.len() {
        return false;
    }
    for v in entries {
        if !set.contains(v) {
            return false;
        }
    }
    true
}

fn sorted_set_entries_equal(a: &SortedSet, b: &SortedSet) -> bool {
    if a.entries.len() != b.entries.len() {
        return false;
    }
    let mut set = HashSet::new();
    for v in &b.entries {
        set.insert(v.clone());
    }
    set_entries_equal_set(&set, &a.entries)
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Map(map) => hash_map_value(state, map.len(), map.iter()),
            Value::SortedMap(map) => hash_map_value(
                state,
                map.entries.len(),
                map.entries.iter().map(|(k, v)| (k, v)),
            ),
            Value::Set(set) => hash_set_value(state, set.len(), set.iter()),
            Value::SortedSet(set) => hash_set_value(state, set.entries.len(), set.entries.iter()),
            Value::MutMap(handle) => {
                let map = handle.lock().unwrap_or_else(|e| e.into_inner());
                hash_map_value(state, map.len(), map.iter());
            }
            Value::MutSet(handle) => {
                let set = handle.lock().unwrap_or_else(|e| e.into_inner());
                hash_set_value(state, set.len(), set.iter());
            }
            _ => {
                std::mem::discriminant(self).hash(state);
                match self {
                    Value::Int(n) => n.hash(state),
                    Value::Float(f) => f.to_bits().hash(state),
                    Value::String(s) => s.hash(state),
                    Value::Bool(b) => b.hash(state),
                    Value::Nil => {}
                    Value::List(v) | Value::Vector(v) => v.hash(state),
                    Value::MutVector(handle) => {
                        let vec = handle.lock().unwrap_or_else(|e| e.into_inner());
                        vec.hash(state);
                    }
                    Value::Regex(r) => r.pattern.hash(state),
                    Value::Duration(d) => d.hash(state),
                    Value::Func(f) => Arc::as_ptr(f).hash(state),
                    Value::Partial {
                        callable,
                        captured,
                        remaining,
                    } => {
                        callable.hash(state);
                        captured.hash(state);
                        remaining.hash(state);
                    }
                    Value::Compose { funcs, kind } => {
                        funcs.hash(state);
                        kind.hash(state);
                    }
                    Value::Atom(handle) => (handle.ptr() as usize).hash(state),
                    Value::Chan(handle) => (handle.ptr() as usize).hash(state),
                    Value::Promise(handle) => (handle.ptr() as usize).hash(state),
                    Value::Task(handle) => (handle.ptr() as usize).hash(state),
                    Value::Future(handle) => (handle.ptr() as usize).hash(state),
                    Value::Agent(handle) => (handle.ptr() as usize).hash(state),
                    Value::Delay(handle) => (handle.ptr() as usize).hash(state),
                    Value::NativeBuf { id, ty, .. } => {
                        id.hash(state);
                        ty.hash(state);
                    }
                    Value::Seq(handle) => (handle.ptr() as usize).hash(state),
                    Value::TransientVector(handle)
                    | Value::TransientMap(handle)
                    | Value::TransientSet(handle) => {
                        handle.ptr().hash(state);
                        handle.epoch.hash(state);
                    }
                    Value::Lambda {
                        params,
                        rest,
                        body,
                        local_defns,
                        env,
                        engines,
                        auto_fallback,
                        call_wrappers,
                        settings,
                        meta,
                        doc,
                        name,
                        inferred_type,
                        recur_id,
                    } => {
                        params.hash(state);
                        rest.hash(state);
                        body.hash(state);
                        local_defns.hash(state);
                        Arc::as_ptr(env).hash(state);
                        engines.iter().for_each(|e| Arc::as_ptr(e).hash(state));
                        auto_fallback.hash(state);
                        Arc::as_ptr(call_wrappers).hash(state);
                        settings.hash(state);
                        meta.hash(state);
                        doc.hash(state);
                        name.hash(state);
                        inferred_type.hash(state);
                        recur_id.hash(state);
                    }
                    Value::MultiLambda {
                        clauses,
                        env,
                        engines,
                        auto_fallback,
                        call_wrappers,
                        settings,
                        meta,
                        doc,
                        name,
                        inferred_type,
                    } => {
                        clauses.hash(state);
                        Arc::as_ptr(env).hash(state);
                        engines.iter().for_each(|e| Arc::as_ptr(e).hash(state));
                        auto_fallback.hash(state);
                        Arc::as_ptr(call_wrappers).hash(state);
                        settings.hash(state);
                        meta.hash(state);
                        doc.hash(state);
                        name.hash(state);
                        inferred_type.hash(state);
                    }
                    Value::Symbol(s) => s.hash(state),
                    Value::Foreign(fv) => {
                        fv.tag.hash(state);
                        Arc::as_ptr(&fv.data).hash(state);
                    }
                    Value::ForeignCallable {
                        tag,
                        path,
                        engine,
                        span,
                    } => {
                        tag.hash(state);
                        path.hash(state);
                        Arc::as_ptr(engine).hash(state);
                        span.hash(state);
                    }
                    Value::Map(_)
                    | Value::SortedMap(_)
                    | Value::Set(_)
                    | Value::SortedSet(_)
                    | Value::MutMap(_)
                    | Value::MutSet(_) => {}
                }
            }
        }
    }
}

fn hash_map_value<'a, H: Hasher>(
    state: &mut H,
    len: usize,
    iter: impl Iterator<Item = (&'a Key, &'a Value)>,
) {
    1u8.hash(state);
    let combined = hash_unordered(iter.map(|(k, v)| hash_entry(k, v)), len);
    combined.hash(state);
}

fn hash_set_value<'a, H: Hasher>(state: &mut H, len: usize, iter: impl Iterator<Item = &'a Value>) {
    2u8.hash(state);
    let combined = hash_unordered(iter.map(hash_value), len);
    combined.hash(state);
}

fn hash_entry(key: &Key, value: &Value) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    value.hash(&mut hasher);
    hasher.finish()
}

fn hash_value(value: &Value) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn hash_unordered(values: impl Iterator<Item = u64>, len: usize) -> u64 {
    let mut acc = 0u64;
    for v in values {
        acc ^= mix_hash(v);
    }
    acc ^ mix_hash(len as u64)
}

fn mix_hash(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9E3779B97F4A7C15);
    value = (value ^ (value >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94D049BB133111EB);
    value ^ (value >> 31)
}

fn pretty_symbol(name: &str) -> String {
    canonical_symbol_name(name).into_owned()
}

pub fn callable_label(callable: &Value) -> Option<String> {
    match callable {
        Value::Lambda { name, .. } | Value::MultiLambda { name, .. } => Some(
            name.clone()
                .map(|n| pretty_symbol(&n))
                .unwrap_or_else(|| "<lambda>".into()),
        ),
        Value::Func(func) => func.debug_name().map(pretty_symbol),
        Value::ForeignCallable { tag, path, .. } => Some(format!("{}::{}", tag, path)),
        Value::Partial { callable, .. } => {
            let base = callable_label(callable).unwrap_or_else(|| "<fn>".into());
            Some(format!("partial {}", base))
        }
        Value::Compose { funcs, kind } => {
            let head = match kind {
                ComposeKind::Comp => "comp",
                ComposeKind::Pipe => "pipe",
            };
            let parts: Vec<String> = funcs
                .iter()
                .map(|f| callable_label(f).unwrap_or_else(|| "<fn>".into()))
                .collect();
            Some(format!("{} [{}]", head, parts.join(" ")))
        }
        _ => None,
    }
}

fn format_fn_arity(arity: FnArity) -> String {
    match (arity.min(), arity.max()) {
        (0, None) => "any".into(),
        (min, Some(max)) if min == max => max.to_string(),
        (min, Some(max)) => format!("{}..{}", min, max),
        (min, None) => format!("{}+", min),
    }
}

fn format_callable_value(callable: &Value) -> String {
    match callable {
        Value::Lambda { name, .. } | Value::MultiLambda { name, .. } => match name {
            Some(n) => format!("#<fn {}>", pretty_symbol(n)),
            None => "#<lambda>".into(),
        },
        Value::Func(func) => {
            if let Some(name) = func.debug_name() {
                format!("#<fn {}>", pretty_symbol(name))
            } else {
                "#<native-fn>".into()
            }
        }
        Value::Partial {
            callable,
            captured,
            remaining,
        } => {
            let base = callable_label(callable).unwrap_or_else(|| "<fn>".into());
            let captured_repr = if captured.is_empty() {
                String::new()
            } else {
                let rendered: Vec<String> = captured.iter().map(|v| v.to_string()).collect();
                format!(" args=[{}]", rendered.join(" "))
            };
            format!(
                "#<partial {}{} remaining={}>",
                base,
                captured_repr,
                format_fn_arity(*remaining)
            )
        }
        Value::Compose { funcs, kind } => {
            let parts: Vec<String> = funcs
                .iter()
                .map(|f| callable_label(f).unwrap_or_else(|| "<fn>".into()))
                .collect();
            let head = match kind {
                ComposeKind::Comp => "comp",
                ComposeKind::Pipe => "pipe",
            };
            format!("#<{} [{}]>", head, parts.join(" "))
        }
        Value::ForeignCallable { tag, path, .. } => format!("#<foreign {}::{}>", tag, path),
        _ => "#<fn>".into(),
    }
}

fn escape_regex_slash(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len());
    let mut backslash_run = 0usize;
    for ch in pattern.chars() {
        if ch == '\\' {
            backslash_run += 1;
            out.push(ch);
            continue;
        }
        if ch == '/' && backslash_run % 2 == 0 {
            out.push('\\');
        }
        out.push(ch);
        backslash_run = 0;
    }
    out
}

fn format_chan(handle: &ChanHandle) -> String {
    let info = handle.debug_info();
    let capacity = match info.capacity {
        Some(cap) => cap.to_string(),
        None => "unbounded".into(),
    };
    format!(
        "#<chan id={} cap={} len={} closed={}>",
        info.id, capacity, info.len, info.closed
    )
}

fn format_async_handle(label: &str, state: PromiseStateView) -> String {
    match state {
        PromiseStateView::Pending => format!("#<{} state=pending>", label),
        PromiseStateView::Fulfilled(value) => {
            format!("#<{} state=fulfilled value={}>", label, value)
        }
        PromiseStateView::Rejected(err) => {
            format!("#<{} state=rejected error={}>", label, err)
        }
        PromiseStateView::Cancelled => format!("#<{} state=cancelled>", label),
    }
}

fn format_agent(handle: &AgentHandle) -> String {
    let info = handle.debug_info();
    let mut status = Vec::new();
    status.push(format!("pending={}", info.pending));
    status.push(format!("processing={}", info.processing));
    if let Some(err) = info.error {
        status.push(format!("error={}", err));
    }
    let suffix = if status.is_empty() {
        String::new()
    } else {
        format!(" {}", status.join(" "))
    };
    format!("#<agent state={}{}>", info.state, suffix)
}

fn format_delay(handle: &DelayHandle) -> String {
    if handle.is_realized() {
        "#<delay state=realized>".into()
    } else {
        "#<delay state=pending>".into()
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", escape_string_fragment(s)),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::List(items) => f.debug_list().entries(items).finish(),
            Value::Vector(items) => f.debug_list().entries(items).finish(),
            Value::Map(map) => f.debug_map().entries(map.iter()).finish(),
            Value::SortedMap(map) => f
                .debug_map()
                .entries(map.entries.iter().map(|(k, v)| (k, v)))
                .finish(),
            Value::Set(set) => {
                let mut elems: Vec<String> = set.iter().map(|v| format!("{:?}", v)).collect();
                elems.sort();
                f.debug_set().entries(elems.iter()).finish()
            }
            Value::SortedSet(set) => f.debug_set().entries(set.entries.iter()).finish(),
            Value::MutVector(handle) => {
                let vec = handle.lock().unwrap_or_else(|e| e.into_inner());
                let items: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
                write!(f, "#<mut [{}]>", items.join(" "))
            }
            Value::MutMap(handle) => {
                let map = handle.lock().unwrap_or_else(|e| e.into_inner());
                let parts: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("{} {}", format_key(k), v))
                    .collect();
                write!(f, "#<mut {{{}}}>", parts.join(" "))
            }
            Value::MutSet(handle) => {
                let set = handle.lock().unwrap_or_else(|e| e.into_inner());
                let mut parts: Vec<String> = set.iter().map(|v| v.to_string()).collect();
                parts.sort();
                write!(f, "#<mut #{{{}}}>", parts.join(" "))
            }
            Value::TransientVector(_) => write!(f, "#<transient-vector>"),
            Value::TransientMap(_) => write!(f, "#<transient-map>"),
            Value::TransientSet(_) => write!(f, "#<transient-set>"),
            Value::Regex(re) => {
                let pattern = escape_regex_slash(&re.pattern);
                write!(f, "#/{}/", pattern)
            }
            Value::Duration(d) => write!(f, "{}", d),
            Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. } => write!(f, "{}", format_callable_value(self)),
            Value::Atom(handle) => write!(f, "#atom<{}>", handle.deref()),
            Value::Chan(handle) => write!(f, "{}", format_chan(handle)),
            Value::Promise(handle) => {
                write!(
                    f,
                    "{}",
                    format_async_handle("promise", handle.debug_state())
                )
            }
            Value::Task(handle) => {
                write!(f, "{}", format_async_handle("task", handle.debug_state()))
            }
            Value::Future(handle) => {
                write!(f, "{}", format_async_handle("future", handle.debug_state()))
            }
            Value::Agent(handle) => write!(f, "{}", format_agent(handle)),
            Value::Delay(handle) => write!(f, "{}", format_delay(handle)),
            Value::NativeBuf { handle, .. } => {
                write!(f, "{}", crate::native_buf::format_native_buf_handle(handle))
            }
            Value::Seq(_) => write!(f, "<seq>"),
            Value::Symbol(s) => write!(f, ":{}", s),
            Value::Foreign(_) => write!(f, "<foreign>"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", format_float(*n)),
            Value::String(s) => write!(f, "\"{}\"", escape_string_fragment(s)),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                write!(f, "({})", parts.join(" "))
            }
            Value::Vector(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", parts.join(" "))
            }
            Value::Map(map) => {
                let parts: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("{} {}", format_key(k), v))
                    .collect();
                write!(f, "{{{}}}", parts.join(" "))
            }
            Value::SortedMap(map) => {
                let parts: Vec<String> = map
                    .entries
                    .iter()
                    .map(|(k, v)| format!("{} {}", format_key(k), v))
                    .collect();
                write!(f, "{{{}}}", parts.join(" "))
            }
            Value::Set(set) => {
                let mut parts: Vec<String> = set.iter().map(|v| v.to_string()).collect();
                parts.sort();
                write!(f, "#{{{}}}", parts.join(" "))
            }
            Value::SortedSet(set) => {
                let parts: Vec<String> = set.entries.iter().map(|v| v.to_string()).collect();
                write!(f, "#{{{}}}", parts.join(" "))
            }
            Value::MutVector(handle) => {
                let vec = handle.lock().unwrap_or_else(|e| e.into_inner());
                let parts: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
                write!(f, "#<mut [{}]>", parts.join(" "))
            }
            Value::MutMap(handle) => {
                let map = handle.lock().unwrap_or_else(|e| e.into_inner());
                let parts: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("{} {}", format_key(k), v))
                    .collect();
                write!(f, "#<mut {{{}}}>", parts.join(" "))
            }
            Value::MutSet(handle) => {
                let set = handle.lock().unwrap_or_else(|e| e.into_inner());
                let mut parts: Vec<String> = set.iter().map(|v| v.to_string()).collect();
                parts.sort();
                write!(f, "#<mut #{{{}}}>", parts.join(" "))
            }
            Value::TransientVector(_) => write!(f, "#<transient-vector>"),
            Value::TransientMap(_) => write!(f, "#<transient-map>"),
            Value::TransientSet(_) => write!(f, "#<transient-set>"),
            Value::Regex(re) => {
                let pattern = escape_regex_slash(&re.pattern);
                write!(f, "#/{}/", pattern)
            }
            Value::Duration(d) => write!(f, "{}", d),
            Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. } => write!(f, "{}", format_callable_value(self)),
            Value::Atom(handle) => write!(f, "#atom<{}>", handle.deref()),
            Value::Chan(handle) => write!(f, "{}", format_chan(handle)),
            Value::Promise(handle) => {
                write!(
                    f,
                    "{}",
                    format_async_handle("promise", handle.debug_state())
                )
            }
            Value::Task(handle) => {
                write!(f, "{}", format_async_handle("task", handle.debug_state()))
            }
            Value::Future(handle) => {
                write!(f, "{}", format_async_handle("future", handle.debug_state()))
            }
            Value::Agent(handle) => write!(f, "{}", format_agent(handle)),
            Value::Delay(handle) => write!(f, "{}", format_delay(handle)),
            Value::NativeBuf { handle, .. } => {
                write!(f, "{}", crate::native_buf::format_native_buf_handle(handle))
            }
            Value::Seq(_) => write!(f, "<seq>"),
            Value::Symbol(s) => {
                if s.starts_with(':') || s.contains("::") {
                    write!(f, "{}", s)
                } else {
                    write!(f, "{}", s)
                }
            }
            Value::Foreign(_) => write!(f, "<foreign>"),
        }
    }
}

pub fn is_mut_collection(value: &Value) -> bool {
    matches!(
        value,
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)
    )
}

pub fn contains_mut_collection(value: &Value) -> bool {
    match value {
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => true,
        Value::Vector(items) | Value::List(items) => items.iter().any(contains_mut_collection),
        Value::Map(map) => map.values().any(contains_mut_collection),
        Value::SortedMap(map) => map.entries.iter().any(|(_, v)| contains_mut_collection(v)),
        Value::Set(set) => set.iter().any(contains_mut_collection),
        Value::SortedSet(set) => set.entries.iter().any(contains_mut_collection),
        _ => false,
    }
}

pub fn to_mut_deep(value: &Value) -> Result<Value, CloveError> {
    match value {
        Value::MutVector(handle) => Ok(Value::MutVector(Arc::clone(handle))),
        Value::MutMap(handle) => Ok(Value::MutMap(Arc::clone(handle))),
        Value::MutSet(handle) => Ok(Value::MutSet(Arc::clone(handle))),
        Value::List(items) => {
            let mut out = Vector::new();
            for item in items.iter() {
                out.push_back(to_mut_deep(item)?);
            }
            Ok(Value::List(out))
        }
        Value::Vector(items) => {
            let mut out = Vector::new();
            for item in items.iter() {
                out.push_back(to_mut_deep(item)?);
            }
            Ok(Value::MutVector(Arc::new(Mutex::new(out))))
        }
        Value::Map(map) => {
            let mut out = HashMap::new();
            for (k, v) in map.iter() {
                out.insert(k.clone(), to_mut_deep(v)?);
            }
            Ok(Value::MutMap(Arc::new(Mutex::new(out))))
        }
        Value::SortedMap(map) => {
            let mut out = HashMap::new();
            for (k, v) in map.entries.iter() {
                out.insert(k.clone(), to_mut_deep(v)?);
            }
            Ok(Value::MutMap(Arc::new(Mutex::new(out))))
        }
        Value::Set(set) => {
            let mut out = HashSet::new();
            for item in set.iter() {
                if is_mut_collection(item) {
                    return Err(CloveError::runtime(
                        "cannot put mutable collection into set; use (imut x)",
                    ));
                }
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::MutSet(Arc::new(Mutex::new(out))))
        }
        Value::SortedSet(set) => {
            let mut out = HashSet::new();
            for item in set.entries.iter() {
                if is_mut_collection(item) {
                    return Err(CloveError::runtime(
                        "cannot put mutable collection into set; use (imut x)",
                    ));
                }
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::MutSet(Arc::new(Mutex::new(out))))
        }
        other => Ok(other.clone()),
    }
}

pub fn to_imut_deep(value: &Value) -> Result<Value, CloveError> {
    if !matches!(
        value,
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)
    ) && !contains_mut_collection(value)
    {
        return Ok(value.clone());
    }

    match value {
        Value::MutVector(handle) => {
            let vec = handle.lock().unwrap_or_else(|e| e.into_inner());
            let mut out = Vector::new();
            for item in vec.iter() {
                out.push_back(to_imut_deep(item)?);
            }
            Ok(Value::Vector(out))
        }
        Value::MutMap(handle) => {
            let map = handle.lock().unwrap_or_else(|e| e.into_inner());
            let mut out = HashMap::new();
            for (k, v) in map.iter() {
                out.insert(k.clone(), to_imut_deep(v)?);
            }
            Ok(Value::Map(out))
        }
        Value::MutSet(handle) => {
            let set = handle.lock().unwrap_or_else(|e| e.into_inner());
            let mut out = HashSet::new();
            for item in set.iter() {
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::Set(out))
        }
        Value::List(items) => {
            let mut out = Vector::new();
            for item in items.iter() {
                out.push_back(to_imut_deep(item)?);
            }
            Ok(Value::List(out))
        }
        Value::Vector(items) => {
            let mut out = Vector::new();
            for item in items.iter() {
                out.push_back(to_imut_deep(item)?);
            }
            Ok(Value::Vector(out))
        }
        Value::Map(map) => {
            let mut out = HashMap::new();
            for (k, v) in map.iter() {
                out.insert(k.clone(), to_imut_deep(v)?);
            }
            Ok(Value::Map(out))
        }
        Value::SortedMap(map) => {
            let mut out = HashMap::new();
            for (k, v) in map.entries.iter() {
                out.insert(k.clone(), to_imut_deep(v)?);
            }
            Ok(Value::Map(out))
        }
        Value::Set(set) => {
            let mut out = HashSet::new();
            for item in set.iter() {
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::Set(out))
        }
        Value::SortedSet(set) => {
            let mut out = HashSet::new();
            for item in set.entries.iter() {
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::Set(out))
        }
        other => Ok(other.clone()),
    }
}

pub fn to_mut_shallow(value: &Value) -> Result<Value, CloveError> {
    match value {
        Value::MutVector(handle) => Ok(Value::MutVector(Arc::clone(handle))),
        Value::MutMap(handle) => Ok(Value::MutMap(Arc::clone(handle))),
        Value::MutSet(handle) => Ok(Value::MutSet(Arc::clone(handle))),
        Value::Vector(items) => Ok(Value::MutVector(Arc::new(Mutex::new(items.clone())))),
        Value::Map(map) => Ok(Value::MutMap(Arc::new(Mutex::new(map.clone())))),
        Value::SortedMap(map) => {
            let mut out = HashMap::new();
            for (k, v) in map.entries.iter() {
                out.insert(k.clone(), v.clone());
            }
            Ok(Value::MutMap(Arc::new(Mutex::new(out))))
        }
        Value::Set(set) => {
            let mut out = HashSet::new();
            for item in set.iter() {
                if is_mut_collection(item) {
                    return Err(CloveError::runtime(
                        "cannot put mutable collection into set; use (imut x)",
                    ));
                }
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::MutSet(Arc::new(Mutex::new(out))))
        }
        Value::SortedSet(set) => {
            let mut out = HashSet::new();
            for item in set.entries.iter() {
                if is_mut_collection(item) {
                    return Err(CloveError::runtime(
                        "cannot put mutable collection into set; use (imut x)",
                    ));
                }
                out.insert(to_imut_deep(item)?);
            }
            Ok(Value::MutSet(Arc::new(Mutex::new(out))))
        }
        other => Ok(other.clone()),
    }
}

fn format_key(k: &Key) -> String {
    match k {
        Key::Keyword(s) => format!(":{}", s),
        Key::Symbol(s) => format!(":{}", s),
        Key::String(s) => format!("\"{}\"", escape_string_fragment(s)),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

fn format_float(n: f64) -> String {
    if n.fract() == 0.0 {
        format!("{:.1}", n)
    } else {
        n.to_string()
    }
}

#[derive(Clone)]
pub struct RegexValue {
    pub pattern: String,
    pub regex: Regex,
}

impl RegexValue {
    pub fn new(pattern: impl Into<String>) -> Result<Self, CloveError> {
        let pattern = pattern.into();
        let regex = Regex::new(&pattern)
            .map_err(|e| CloveError::runtime(format!("invalid regex: {}", e)))?;
        Ok(Self { pattern, regex })
    }
}

impl PartialEq for RegexValue {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}

impl fmt::Debug for RegexValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#\"{}\"", self.pattern)
    }
}
