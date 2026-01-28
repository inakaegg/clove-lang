use std::fs;
use std::sync::{Arc, Mutex};

use clove_core::ast::{Form, Span, Value};
use clove_core::env::EnvRef;
use clove_core::error::CloveError;
use clove_core::eval_source_with_engines;
use clove_core::foreign::ForeignEngine;
use clove_core::options::EvalOptions;

#[derive(Clone)]
struct EchoEngine {
    calls: Arc<Mutex<Vec<String>>>,
}

impl ForeignEngine for EchoEngine {
    fn tag(&self) -> &str {
        "rb"
    }

    fn eval_block(
        &self,
        code: &str,
        _env: EnvRef,
        _span: Option<Span>,
    ) -> Result<Value, CloveError> {
        self.calls.lock().unwrap().push(code.to_string());
        Ok(Value::String(code.to_string()))
    }

    fn eval_fallback(&self, _form: &Form, _env: EnvRef) -> Result<Value, CloveError> {
        Err(CloveError::runtime("fallback not supported"))
    }

    fn call_symbol(
        &self,
        _path: &str,
        _args: &[Value],
        _span: Option<Span>,
    ) -> Result<Value, CloveError> {
        Err(CloveError::runtime("call_symbol not supported"))
    }
}

#[test]
fn example_oop_chain_runs() {
    let base = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = base.join("../../examples/seq/oop_method_chain.clv");
    let src = fs::read_to_string(&path).expect("read example");
    clove_core::eval_source(&src, None).expect("run oop example");
}

#[test]
fn example_ruby_suffix_runs() {
    let calls = Arc::new(Mutex::new(Vec::new()));
    let engine = Arc::new(EchoEngine {
        calls: calls.clone(),
    });
    let base = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = base.join("../../examples/interop/ruby_suffix_chain.clv");
    let src = fs::read_to_string(&path).expect("read example");
    let opts = EvalOptions {
        source_name: Some(path.to_string_lossy().into_owned()),
        ..Default::default()
    };
    let out = eval_source_with_engines(&src, opts, &[engine]).expect("run ruby example");
    assert!(matches!(
        out,
        Value::String(_) | Value::Nil | Value::Vector(_)
    ));
    let recorded = calls.lock().unwrap();
    assert!(!recorded.is_empty(), "ruby engine was not invoked");
}
