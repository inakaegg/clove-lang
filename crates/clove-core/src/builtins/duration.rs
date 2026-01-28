use crate::ast::{DurationUnit, DurationValue, FnArity, Value};
use crate::builtins::{def_builtin, err};
use crate::env::Env;
use crate::error::CloveError;

struct UnitSpec {
    symbol_name: &'static str,
    constructor_name: &'static str,
    keyword_aliases: &'static [&'static str],
    unit: DurationUnit,
}

const UNIT_SPECS: &[UnitSpec] = &[
    UnitSpec {
        symbol_name: "ms",
        constructor_name: "duration-ms",
        keyword_aliases: &["ms", "millisecond", "milliseconds"],
        unit: DurationUnit::Millisecond,
    },
    UnitSpec {
        symbol_name: "sec",
        constructor_name: "duration-sec",
        keyword_aliases: &["sec", "second", "seconds", "s"],
        unit: DurationUnit::Second,
    },
    UnitSpec {
        symbol_name: "min",
        constructor_name: "duration-min",
        keyword_aliases: &["min", "minute", "minutes", "m"],
        unit: DurationUnit::Minute,
    },
    UnitSpec {
        symbol_name: "hour",
        constructor_name: "duration-hour",
        keyword_aliases: &["hour", "hours", "h"],
        unit: DurationUnit::Hour,
    },
    UnitSpec {
        symbol_name: "day",
        constructor_name: "duration-day",
        keyword_aliases: &["day", "days", "d"],
        unit: DurationUnit::Day,
    },
    UnitSpec {
        symbol_name: "week",
        constructor_name: "duration-week",
        keyword_aliases: &["week", "weeks", "w"],
        unit: DurationUnit::Week,
    },
    UnitSpec {
        symbol_name: "year",
        constructor_name: "duration-year",
        keyword_aliases: &["year", "years", "y"],
        unit: DurationUnit::Year,
    },
];

pub(crate) fn install(env: &mut Env) {
    for spec in UNIT_SPECS {
        install_unit(env, spec.symbol_name, spec.unit);
    }
    for spec in UNIT_SPECS {
        install_constructor(env, spec.constructor_name, spec.unit);
    }
    install_duration(env);
}

fn install_unit(env: &mut Env, name: &'static str, unit: DurationUnit) {
    if env.get(name).is_some() {
        // Do not override existing symbols (e.g., numeric min).
        return;
    }
    let func = Value::native_fn(FnArity::exact(1), move |args| {
        convert_duration(name, unit, args)
    });
    env.define_builtin(name, func);
}

fn install_constructor(env: &mut Env, name: &'static str, unit: DurationUnit) {
    let func = Value::native_fn(FnArity::exact(1), move |args| match args {
        [value] => duration_from_number(name, value, unit),
        _ => err(format!("{} expects a numeric magnitude", name)),
    });
    env.define_builtin(name, func);
}

fn install_duration(env: &mut Env) {
    def_builtin!(env, "duration", FnArity::exact(2), |args| match args {
        [value, unit_value] => {
            let unit = determine_unit(unit_value).ok_or_else(|| {
                CloveError::runtime(
                    "duration expects a unit keyword/string such as :ms, :sec, :min, :hour",
                )
            })?;
            duration_from_number("duration", value, unit)
        }
        _ => err("duration expects a magnitude and a unit keyword"),
    });
}

fn convert_duration(name: &str, unit: DurationUnit, args: &[Value]) -> Result<Value, CloveError> {
    match args {
        [Value::Duration(d)] => Ok(float_to_value(d.to_unit(unit))),
        _ => err(format!("{} expects a duration literal", name)),
    }
}

fn duration_from_number(
    name: &str,
    value: &Value,
    unit: DurationUnit,
) -> Result<Value, CloveError> {
    let duration = match value {
        Value::Int(n) => DurationValue::from_unit(*n as i128, unit),
        Value::Float(f) => float_to_duration_value(*f, unit),
        _ => return err(format!("{} expects a numeric magnitude", name)),
    }?;
    Ok(Value::Duration(duration))
}

fn float_to_duration_value(value: f64, unit: DurationUnit) -> Result<DurationValue, CloveError> {
    if !value.is_finite() {
        return Err(CloveError::runtime("duration magnitude must be finite"));
    }
    if value < 0.0 {
        return Err(CloveError::runtime("duration cannot be negative"));
    }
    let nanos_per_unit = unit.nanos_per_unit() as f64;
    let nanos = value * nanos_per_unit;
    if !nanos.is_finite() || nanos > i128::MAX as f64 {
        return Err(CloveError::runtime("duration magnitude overflowed"));
    }
    let rounded = nanos.round() as i128;
    DurationValue::from_nanos(rounded)
}

fn determine_unit(value: &Value) -> Option<DurationUnit> {
    match value {
        Value::Symbol(sym) => lookup_unit(sym),
        Value::String(text) => lookup_unit(text),
        _ => None,
    }
}

fn lookup_unit(text: &str) -> Option<DurationUnit> {
    if text.is_empty() {
        return None;
    }
    let trimmed = text.trim();
    let without_colon = trimmed.trim_start_matches(':');
    let tail = without_colon
        .rsplit('/')
        .next()
        .unwrap_or(without_colon)
        .to_ascii_lowercase();
    for spec in UNIT_SPECS {
        if spec
            .keyword_aliases
            .iter()
            .any(|alias| *alias == tail.as_str())
        {
            return Some(spec.unit);
        }
    }
    None
}

fn float_to_value(value: f64) -> Value {
    if value.fract() == 0.0 && value <= i64::MAX as f64 && value >= i64::MIN as f64 {
        Value::Int(value as i64)
    } else {
        Value::Float(value)
    }
}
