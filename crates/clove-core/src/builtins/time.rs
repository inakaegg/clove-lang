use crate::ast::{DurationValue, FnArity, Value};
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use chrono::{DateTime, TimeZone, Utc};
use std::fmt::Write;
use std::time::Duration;

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "time::now", FnArity::exact(0), |_args| Ok(Value::Int(
        now_millis()
    )));
    def_builtin!(env, "time::instant", FnArity::exact(1), |args| match args {
        [Value::Int(ms)] => Ok(Value::Int(*ms)),
        [Value::Float(f)] => Ok(Value::Float(*f)),
        _ => err("time::instant expects epoch millis (int/float)"),
    });
    def_builtin!(env, "time::parse", FnArity::exact(1), |args| match args {
        [Value::String(s)] => parse_time(s),
        _ => err("time::parse expects string"),
    });
    def_builtin!(env, "time::format", FnArity::exact(2), |args| match args {
        [inst, Value::String(fmt)] => format_time(inst, fmt),
        _ => err("time::format expects instant and format string"),
    });
    def_builtin!(env, "time::plus", FnArity::exact(2), |args| match args {
        [inst, Value::Duration(dur)] => plus(inst, dur, true),
        _ => err("time::plus expects instant and duration"),
    });
    def_builtin!(env, "time::minus", FnArity::exact(2), |args| match args {
        [inst, Value::Duration(dur)] => plus(inst, dur, false),
        _ => err("time::minus expects instant and duration"),
    });
    def_builtin!(env, "time::between", FnArity::exact(2), |args| match args {
        [a, b] => between(a, b),
        _ => err("time::between expects two instants"),
    });
    def_builtin!(env, "time::sleep", FnArity::exact(1), |args| match args {
        [Value::Duration(d)] => {
            let millis = d.to_millis_i64()?;
            let mut remaining = millis.max(0) as u64;
            while remaining > 0 {
                if crate::interrupt::is_interrupted() {
                    return Err(CloveError::runtime("execution interrupted"));
                }
                let chunk = remaining.min(100);
                std::thread::sleep(Duration::from_millis(chunk));
                remaining -= chunk;
            }
            if crate::interrupt::is_interrupted() {
                Err(CloveError::runtime("execution interrupted"))
            } else {
                Ok(Value::Bool(true))
            }
        }
        _ => err("time::sleep expects duration"),
    });
}

fn now_millis() -> i64 {
    let now = chrono::Utc::now();
    now.timestamp_millis()
}

fn parse_time(s: &str) -> Result<Value, CloveError> {
    let dt: DateTime<Utc> = s
        .parse()
        .or_else(|_| DateTime::parse_from_rfc3339(s).map(|d| d.with_timezone(&Utc)))
        .map_err(|e| CloveError::runtime(format!("time::parse failed: {}", e)))?;
    Ok(Value::Int(dt.timestamp_millis()))
}

fn format_time(inst: &Value, fmt: &str) -> Result<Value, CloveError> {
    let millis = instant_millis(inst)?;
    let dt = Utc
        .timestamp_millis_opt(millis)
        .single()
        .ok_or_else(|| CloveError::runtime("invalid instant"))?;
    let mut buf = String::new();
    write!(&mut buf, "{}", dt.format(fmt))
        .map_err(|e| CloveError::runtime(format!("time::format failed: {}", e)))?;
    Ok(Value::String(buf))
}

fn plus(inst: &Value, dur: &DurationValue, add: bool) -> Result<Value, CloveError> {
    let millis = instant_millis(inst)?;
    let delta = dur.to_millis_i64()?;
    let result = if add { millis + delta } else { millis - delta };
    Ok(Value::Int(result))
}

fn between(a: &Value, b: &Value) -> Result<Value, CloveError> {
    let a_ms = instant_millis(a)?;
    let b_ms = instant_millis(b)?;
    let diff = (b_ms - a_ms).abs() as i128;
    let nanos = diff * 1_000_000;
    let dur = DurationValue::from_nanos(nanos)?;
    Ok(Value::Duration(dur))
}

fn instant_millis(value: &Value) -> Result<i64, CloveError> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Float(f) => Ok(*f as i64),
        _ => err("instant must be int/float millis"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_time_returns_error_on_invalid_spec() {
        let inst = Value::Int(0);
        let result = format_time(&inst, "%24H");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(
            msg.contains("time::format failed"),
            "unexpected error message: {}",
            msg
        );
    }

    #[test]
    fn format_time_formats_valid_spec() {
        let inst = Value::Int(0);
        let result = format_time(&inst, "%H").unwrap();
        assert_eq!(result, Value::String("00".to_string()));
    }
}
