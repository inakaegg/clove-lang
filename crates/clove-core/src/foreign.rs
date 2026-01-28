use std::any::Any;
use std::sync::Arc;

use crate::ast::{Form, Span, Value};
use crate::env::EnvRef;
use crate::error::CloveError;

/// Engine that can evaluate foreign blocks (e.g., Ruby/Python/Lua).
pub trait ForeignEngine: Send + Sync {
    /// Tag such as "rb" or "py".
    fn tag(&self) -> &str;

    /// Evaluate a foreign code block with access to the current environment.
    fn eval_block(&self, code: &str, env: EnvRef, span: Option<Span>) -> Result<Value, CloveError>;

    /// Evaluate S-expression form by converting to this engine's source (used for auto-fallback).
    fn eval_fallback(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError>;

    /// Call foreign callable referenced by path (e.g., "Foo.bar") with already-evaluated args.
    fn call_symbol(
        &self,
        path: &str,
        args: &[Value],
        span: Option<Span>,
    ) -> Result<Value, CloveError>;
}

/// Foreign value wrapper (for engines that need to round-trip opaque values).
#[derive(Clone)]
pub struct ForeignValue {
    pub tag: String,
    pub data: Arc<dyn Any + Send + Sync>,
}
