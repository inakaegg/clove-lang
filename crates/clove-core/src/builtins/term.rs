// File: crates/clove-core/src/builtins/term.rs

use crate::ast::{FnArity, Value};
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use crate::eval::call_callable;

use std::time::Duration;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, is_raw_mode_enabled};

pub(crate) fn install(env: &mut crate::env::Env) {
    // (term::raw-mode! true)  /  (term::raw-mode! false)
    def_builtin!(
        env,
        "term::raw-mode!",
        FnArity::exact(1),
        |args| match args {
            [Value::Bool(on)] => {
                if *on {
                    enable_raw_mode()
                        .map_err(|e| CloveError::runtime(format!("term::raw-mode!: {}", e)))?;
                } else {
                    disable_raw_mode()
                        .map_err(|e| CloveError::runtime(format!("term::raw-mode!: {}", e)))?;
                }
                Ok(Value::Nil)
            }
            _ => err("term::raw-mode! expects boolean"),
        }
    );

    // (term::read-key)          ; blocking
    // (term::read-key 120)      ; 120ms timeout, nil if nothing arrives
    // (term::read-key 16ms)     ; same with Duration
    def_builtin!(
        env,
        "term::read-key",
        FnArity::range(0, 1),
        |args| read_key(args)
    );

    // (term::with-raw-mode f)   ; run f in raw mode (always restore on exit)
    def_builtin!(
        env,
        "term::with-raw-mode",
        FnArity::exact(1),
        |args| match args {
            [f] => with_raw_mode(f),
            _ => err("term::with-raw-mode expects function"),
        }
    );
}

fn read_key(args: &[Value]) -> Result<Value, CloveError> {
    let timeout = match args {
        [] => None,
        [Value::Int(ms)] if *ms >= 0 => Some(Duration::from_millis(*ms as u64)),
        [Value::Duration(dur)] => {
            let millis = dur.to_millis_i64()?;
            let millis_u64 = u64::try_from(millis)
                .map_err(|_| CloveError::runtime("term::read-key timeout is too large"))?;
            Some(Duration::from_millis(millis_u64))
        }
        [_] => return err("term::read-key expects non-negative integer or duration timeout"),
        _ => return err("term::read-key expects 0 or 1 arguments"),
    };

    if let Some(timeout) = timeout {
        let ok = event::poll(timeout)
            .map_err(|e| CloveError::runtime(format!("term::read-key poll failed: {}", e)))?;
        if !ok {
            // timeout
            return Ok(Value::Nil);
        }
    }

    loop {
        let ev = event::read()
            .map_err(|e| CloveError::runtime(format!("term::read-key failed: {}", e)))?;
        if let Event::Key(key) = ev {
            if let Some(v) = key_to_value(key) {
                return Ok(v);
            }
        }
        // Skip other events and continue.
    }
}

fn key_to_value(key: KeyEvent) -> Option<Value> {
    use KeyCode::*;

    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);

    let s = match key.code {
        // Single-character keys like "w".
        Char(c) if ctrl => format!("ctrl-{}", c.to_ascii_lowercase()),
        Char(c) => c.to_string(),
        // Arrow keys are "up" / "down" / "left" / "right".
        Up => "up".to_string(),
        Down => "down".to_string(),
        Left => "left".to_string(),
        Right => "right".to_string(),
        Esc => "esc".to_string(),
        Enter => "enter".to_string(),
        // Ignore everything else for now and read again.
        _ => return None,
    };
    Some(Value::String(s))
}

fn with_raw_mode(f: &Value) -> Result<Value, CloveError> {
    let was_raw = is_raw_mode_enabled()
        .map_err(|e| CloveError::runtime(format!("term::with-raw-mode: {}", e)))?;

    if !was_raw {
        enable_raw_mode()
            .map_err(|e| CloveError::runtime(format!("term::with-raw-mode: {}", e)))?;
    }

    // Call Clove function with [] args
    let result = call_callable(f.clone(), Vec::new());

    // OFF (restore only if it wasn't raw; ignore errors for now).
    if !was_raw {
        let _ = disable_raw_mode();
    }

    result
}
