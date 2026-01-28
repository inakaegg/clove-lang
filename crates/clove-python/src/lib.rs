pub mod engine;

use std::sync::Arc;

use clove_core::foreign::ForeignEngine;

pub fn engines() -> Vec<Arc<dyn ForeignEngine>> {
    vec![engine::PythonEngine::new()]
}

pub use clove_core::*;
