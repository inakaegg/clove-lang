use crate::ast::Span;
use crate::error::CloveError;

#[derive(Debug)]
pub enum VmError {
    Unsupported { span: Span, reason: String },
    Runtime(CloveError),
}

impl VmError {
    pub fn unsupported(span: Span, reason: impl Into<String>) -> Self {
        VmError::Unsupported {
            span,
            reason: reason.into(),
        }
    }

    pub fn runtime(err: CloveError) -> Self {
        VmError::Runtime(err)
    }

    pub fn is_unsupported(&self) -> bool {
        matches!(self, VmError::Unsupported { .. })
    }
}
