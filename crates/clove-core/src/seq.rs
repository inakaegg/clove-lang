use crate::ast::{Value, Vector};
use crate::error::CloveError;
use crate::guard;
use std::sync::{Arc, Mutex};

/// Engine trait for single-pass sequences.
/// `next` returns the next element, and `Ok(None)` at the end.
pub trait SeqEngine: Send + 'static {
    fn next(&mut self) -> Result<Option<Value>, CloveError>;
}

struct SeqState {
    engine: Box<dyn SeqEngine>,
    buffered: Option<Value>,
    exhausted: bool,
}

/// Handle for a 1-pass lazy sequence.
/// Cloning shares internal progress.
#[derive(Clone)]
pub struct SeqHandle {
    inner: Arc<Mutex<SeqState>>,
}

impl SeqHandle {
    pub fn new(engine: Box<dyn SeqEngine>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(SeqState {
                engine,
                buffered: None,
                exhausted: false,
            })),
        }
    }

    pub fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: Send + 'static,
    {
        SeqHandle::new(Box::new(IterSeq {
            iter: iter.into_iter(),
        }))
    }

    /// Peek without consuming the head element. None if empty.
    pub fn peek(&self) -> Result<Option<Value>, CloveError> {
        let mut guard = self.inner.lock().unwrap();
        if let Some(v) = guard.buffered.clone() {
            return Ok(Some(v));
        }
        if guard.exhausted {
            return Ok(None);
        }
        let next = guard.engine.next()?;
        match next {
            Some(v) => {
                guard.buffered = Some(v.clone());
                Ok(Some(v))
            }
            None => {
                guard.exhausted = true;
                Ok(None)
            }
        }
    }

    /// Advance by one element. None if empty.
    pub fn next(&self) -> Result<Option<Value>, CloveError> {
        guard::tick(None)?;
        let mut guard = self.inner.lock().unwrap();
        if let Some(v) = guard.buffered.take() {
            return Ok(Some(v));
        }
        if guard.exhausted {
            return Ok(None);
        }
        let next = guard.engine.next()?;
        if next.is_none() {
            guard.exhausted = true;
        }
        Ok(next)
    }

    pub fn collect_all(&self) -> Result<Vector<Value>, CloveError> {
        let mut out = Vector::new();
        while let Some(v) = self.next()? {
            out.push_back(v);
        }
        Ok(out)
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    pub fn ptr(&self) -> *const () {
        Arc::as_ptr(&self.inner) as *const ()
    }
}

struct IterSeq<I>
where
    I: Iterator<Item = Value> + Send + 'static,
{
    iter: I,
}

impl<I> SeqEngine for IterSeq<I>
where
    I: Iterator<Item = Value> + Send + 'static,
{
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        Ok(self.iter.next())
    }
}
