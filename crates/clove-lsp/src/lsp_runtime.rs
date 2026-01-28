use std::sync::{Arc, Mutex};

use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

pub struct LspRuntime {
    ctx: Mutex<Arc<RuntimeCtx>>,
}

impl LspRuntime {
    pub fn new() -> Self {
        Self {
            ctx: Mutex::new(RuntimeCtx::new(EvalOptions::default(), &[])),
        }
    }

    pub fn with_current<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Arc<RuntimeCtx>) -> R,
    {
        let guard = self.ctx.lock().expect("failed to lock LSP runtime");
        guard.with_current_ctx(f)
    }
}
