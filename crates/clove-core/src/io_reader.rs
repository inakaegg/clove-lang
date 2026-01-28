use std::ffi::c_void;
use std::io::{self, Read};
use std::sync::{Arc, Mutex};

use clove_plugin_api::{ReaderCloseFn, ReaderErrorFn, ReaderReadFn};

use crate::ast::Value;
use crate::foreign::ForeignValue;

pub const IO_READER_TAG: &str = "io::reader";

#[derive(Clone)]
pub struct ReaderHandle {
    inner: Arc<Mutex<ReaderState>>,
}

struct ReaderState {
    reader: Option<Box<dyn Read + Send>>,
}

impl ReaderHandle {
    pub fn new<R>(reader: R) -> Self
    where
        R: Read + Send + 'static,
    {
        Self::new_boxed(Box::new(reader))
    }

    pub fn new_boxed(reader: Box<dyn Read + Send>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(ReaderState {
                reader: Some(reader),
            })),
        }
    }

    pub fn from_callbacks(
        read_fn: ReaderReadFn,
        close_fn: ReaderCloseFn,
        error_fn: ReaderErrorFn,
        ctx: *mut c_void,
    ) -> Self {
        let reader = CallbackReader::new(read_fn, close_fn, error_fn, ctx);
        Self::new(reader)
    }

    pub fn close(&self) -> io::Result<()> {
        let mut guard = self
            .inner
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "io reader lock poisoned"))?;
        if guard.reader.is_some() {
            guard.reader = None;
        }
        Ok(())
    }
}

impl Read for ReaderHandle {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut guard = self
            .inner
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "io reader lock poisoned"))?;
        let reader = guard
            .reader
            .as_mut()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "io reader is already closed"))?;
        reader.read(buf)
    }
}

pub fn reader_value(handle: ReaderHandle) -> Value {
    Value::Foreign(ForeignValue {
        tag: IO_READER_TAG.to_string(),
        data: Arc::new(handle),
    })
}

pub fn reader_from_value(value: &Value) -> Option<ReaderHandle> {
    match value {
        Value::Foreign(fv) if fv.tag == IO_READER_TAG => {
            fv.data.downcast_ref::<ReaderHandle>().cloned()
        }
        _ => None,
    }
}

struct CallbackReader {
    read_fn: ReaderReadFn,
    close_fn: ReaderCloseFn,
    error_fn: ReaderErrorFn,
    ctx: *mut c_void,
    closed: bool,
}

impl CallbackReader {
    fn new(
        read_fn: ReaderReadFn,
        close_fn: ReaderCloseFn,
        error_fn: ReaderErrorFn,
        ctx: *mut c_void,
    ) -> Self {
        Self {
            read_fn,
            close_fn,
            error_fn,
            ctx,
            closed: false,
        }
    }

    fn close(&mut self) {
        if self.closed {
            return;
        }
        if let Some(close_fn) = self.close_fn {
            unsafe {
                close_fn(self.ctx);
            }
        }
        self.closed = true;
    }

    fn error_message(&self) -> Option<String> {
        let error_fn = self.error_fn?;
        let mut ptr: *const u8 = std::ptr::null();
        let mut len: usize = 0;
        unsafe {
            error_fn(self.ctx, &mut ptr as *mut _, &mut len as *mut _);
        }
        if ptr.is_null() || len == 0 {
            return None;
        }
        let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
        std::str::from_utf8(slice).ok().map(|s| s.to_string())
    }
}

impl Read for CallbackReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.closed {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "io reader is already closed",
            ));
        }
        let result = unsafe { (self.read_fn)(self.ctx, buf.as_mut_ptr(), buf.len()) };
        if result >= 0 {
            Ok(result as usize)
        } else {
            let msg = self
                .error_message()
                .unwrap_or_else(|| "io reader read failed".to_string());
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    }
}

impl Drop for CallbackReader {
    fn drop(&mut self) {
        self.close();
    }
}

unsafe impl Send for CallbackReader {}
