use crate::ast::{FnArity, Value};
use crate::builtins::{as_number, def_builtin, err, make_number, value_eq};
use crate::env::Env;
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SpecialOp, SubjectPos};
use crate::ir::{
    FAST_FLOAT_ADD_SYM, FAST_FLOAT_DEC_SYM, FAST_FLOAT_INC_SYM, FAST_INT_ADD_SYM, FAST_INT_DEC_SYM,
    FAST_INT_INC_SYM,
};
use crate::types::TypeKind;

pub(crate) fn install(env: &mut Env) {
    register_math_metas();
    env.define_builtin("math::PI", Value::Float(std::f64::consts::PI));
    env.define_builtin("math::E", Value::Float(std::f64::consts::E));
    // --- Arithmetic ---
    def_builtin!(env, "+", FnArity::at_least(0), |args| {
        if let Some(Value::String(first)) = args.first() {
            let mut buf = first.clone();
            for (idx, arg) in args.iter().enumerate().skip(1) {
                let Value::String(s) = arg else {
                    return err(format!("+ expects string (arg {})", idx + 1));
                };
                buf.push_str(s);
            }
            return Ok(Value::String(buf));
        }
        arith("+", args, 0.0, |a, b| a + b, false)
    });
    def_builtin!(env, FAST_INT_ADD_SYM, FnArity::exact(2), |args| {
        if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
            let value = (*a as f64) + (*b as f64);
            Ok(make_number(value, value.fract() != 0.0))
        } else {
            arith("+", args, 0.0, |a, b| a + b, false)
        }
    });
    def_builtin!(env, FAST_FLOAT_ADD_SYM, FnArity::exact(2), |args| {
        if let (Value::Float(a), Value::Float(b)) = (&args[0], &args[1]) {
            Ok(Value::Float(a + b))
        } else {
            arith("+", args, 0.0, |a, b| a + b, true)
        }
    });
    def_builtin!(env, FAST_INT_INC_SYM, FnArity::exact(1), |args| {
        if let Value::Int(n) = args[0] {
            let value = (n as f64) + 1.0;
            Ok(make_number(value, value.fract() != 0.0))
        } else {
            err("fast-inc-int expects int")
        }
    });
    def_builtin!(env, FAST_FLOAT_INC_SYM, FnArity::exact(1), |args| {
        if let Value::Float(n) = args[0] {
            Ok(Value::Float(n + 1.0))
        } else {
            err("fast-inc-float expects float")
        }
    });
    def_builtin!(env, FAST_INT_DEC_SYM, FnArity::exact(1), |args| {
        if let Value::Int(n) = args[0] {
            let value = (n as f64) - 1.0;
            Ok(make_number(value, value.fract() != 0.0))
        } else {
            err("fast-dec-int expects int")
        }
    });
    def_builtin!(env, FAST_FLOAT_DEC_SYM, FnArity::exact(1), |args| {
        if let Value::Float(n) = args[0] {
            Ok(Value::Float(n - 1.0))
        } else {
            err("fast-dec-float expects float")
        }
    });
    def_builtin!(env, "-", FnArity::at_least(0), |args| {
        if args.is_empty() {
            Ok(make_number(0.0, false))
        } else if args.len() == 1 {
            let (n, is_float) = as_number(&args[0])?;
            Ok(make_number(-n, is_float || (-n).fract() != 0.0))
        } else {
            arith("-", args, 0.0, |a, b| a - b, false)
        }
    });
    def_builtin!(env, "*", FnArity::at_least(0), |args| {
        if args.len() == 2 {
            if let (Value::String(s), Value::Int(n)) = (&args[0], &args[1]) {
                if *n < 0 {
                    return err("* expects non-negative repeat count");
                }
                return Ok(Value::String(s.repeat(*n as usize)));
            }
        }
        arith("*", args, 1.0, |a, b| a * b, false)
    });
    def_builtin!(env, "/", FnArity::at_least(1), |args| {
        if args
            .iter()
            .any(|v| matches!(v, Value::Func(func) if func.debug_name() == Some("/")))
        {
            return err(
                "'/': got function '/' as an argument; if you meant a regex literal, use #/.../",
            );
        }
        match args {
            [] => err("/ expects at least one number"),
            [x] => {
                let (n, _) = as_number(x)?;
                if n == 0.0 {
                    return err("division by zero");
                }
                Ok(make_number(1.0 / n, true))
            }
            _ => {
                let (mut acc, first_float) = as_number(&args[0])?;
                let mut acc_float = first_float;
                let mut acc_int = if first_float {
                    None
                } else if let Value::Int(n) = args[0] {
                    Some(n as i128)
                } else {
                    None
                };
                for val in &args[1..] {
                    let (n, is_float) = as_number(val)?;
                    if n == 0.0 {
                        return err("division by zero");
                    }
                    acc /= n;
                    acc_float = acc_float || is_float || acc.fract() != 0.0;
                    if let (Some(current), Value::Int(divisor)) = (acc_int, val) {
                        if *divisor == 0 {
                            return err("division by zero");
                        }
                        if current % (*divisor as i128) == 0 {
                            acc_int = Some(current / (*divisor as i128));
                        } else {
                            acc_int = None;
                        }
                    } else {
                        acc_int = None;
                    }
                }
                if let Some(int_val) = acc_int {
                    if let Ok(narrow) = i64::try_from(int_val) {
                        return Ok(Value::Int(narrow));
                    }
                }
                Ok(make_number(acc, acc_float))
            }
        }
    });

    // --- Comparison ---
    def_builtin!(env, ">", FnArity::at_least(0), |args| {
        cmp_chain(">", args, |a, b| a > b)
    });
    def_builtin!(env, "<", FnArity::at_least(0), |args| {
        cmp_chain("<", args, |a, b| a < b)
    });
    def_builtin!(env, ">=", FnArity::at_least(0), |args| {
        cmp_chain(">=", args, |a, b| a >= b)
    });
    def_builtin!(env, "<=", FnArity::at_least(0), |args| {
        cmp_chain("<=", args, |a, b| a <= b)
    });

    def_builtin!(env, "=", FnArity::at_least(0), |args| {
        if args.len() < 2 {
            return Ok(Value::Bool(true));
        }
        let first = &args[0];
        for other in &args[1..] {
            if !value_eq(first, other) {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    });
    def_builtin!(env, "not=", FnArity::at_least(0), |args| {
        if args.len() < 2 {
            return Ok(Value::Bool(false));
        }
        let first = &args[0];
        for other in &args[1..] {
            if !value_eq(first, other) {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    });

    // --- Math Utils ---
    def_builtin!(env, "inc", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, is_float) = as_number(v)?;
                Ok(make_number(n + 1.0, is_float || (n + 1.0).fract() != 0.0))
            }
            _ => err("inc expects one number"),
        }
    });
    def_builtin!(env, "dec", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, is_float) = as_number(v)?;
                Ok(make_number(n - 1.0, is_float || (n - 1.0).fract() != 0.0))
            }
            _ => err("dec expects one number"),
        }
    });
    def_builtin!(env, "abs", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, is_float) = as_number(v)?;
                let val = n.abs();
                Ok(make_number(val, is_float || val.fract() != 0.0))
            }
            _ => err("abs expects one number"),
        }
    });
    def_builtin!(env, "zero?", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, _) = as_number(v)?;
                Ok(Value::Bool(n == 0.0))
            }
            _ => err("zero? expects one number"),
        }
    });
    def_builtin!(env, "pos?", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, _) = as_number(v)?;
                Ok(Value::Bool(n > 0.0))
            }
            _ => err("pos? expects one number"),
        }
    });
    def_builtin!(env, "neg?", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, _) = as_number(v)?;
                Ok(Value::Bool(n < 0.0))
            }
            _ => err("neg? expects one number"),
        }
    });
    def_builtin!(env, "even?", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, _) = as_number(v)?;
                Ok(Value::Bool(n % 2.0 == 0.0))
            }
            _ => err("even? expects one number"),
        }
    });
    def_builtin!(env, "odd?", FnArity::exact(1), |args| {
        match args {
            [v] => {
                let (n, _) = as_number(v)?;
                Ok(Value::Bool(n % 2.0 != 0.0))
            }
            _ => err("odd? expects one number"),
        }
    });
    def_builtin!(env, "mod", FnArity::exact(2), |args| {
        match args {
            [a, b] => {
                let (a, af) = as_number(a)?;
                let (b, _) = as_number(b)?;
                if b == 0.0 {
                    err("mod by zero")
                } else {
                    let val = a.rem_euclid(b);
                    Ok(make_number(
                        val,
                        af || val.fract() != 0.0 || b.fract() != 0.0,
                    ))
                }
            }
            _ => err("mod expects two numbers"),
        }
    });

    // --- Bitwise / integer ops ---
    def_builtin!(env, "bit-and", FnArity::at_least(0), |args| {
        let mut acc = if args.is_empty() {
            -1i64
        } else {
            expect_int(&args[0])?
        };
        for v in args.iter().skip(1) {
            acc &= expect_int(v)?;
        }
        Ok(Value::Int(acc))
    });
    def_builtin!(env, "bit-or", FnArity::at_least(0), |args| {
        let mut acc = if args.is_empty() {
            0i64
        } else {
            expect_int(&args[0])?
        };
        for v in args.iter().skip(1) {
            acc |= expect_int(v)?;
        }
        Ok(Value::Int(acc))
    });
    def_builtin!(env, "bit-xor", FnArity::at_least(0), |args| {
        let mut acc = if args.is_empty() {
            0i64
        } else {
            expect_int(&args[0])?
        };
        for v in args.iter().skip(1) {
            acc ^= expect_int(v)?;
        }
        Ok(Value::Int(acc))
    });
    def_builtin!(env, "bit-not", FnArity::exact(1), |args| match args {
        [v] => Ok(Value::Int(!expect_int(v)?)),
        _ => err("bit-not expects one int"),
    });
    def_builtin!(env, "bit-test", FnArity::exact(2), |args| match args {
        [v, n] => {
            let val = expect_int(v)?;
            let bit = expect_int(n)? as u32;
            Ok(Value::Bool(((val as u64) >> bit) & 1 == 1))
        }
        _ => err("bit-test expects value and bit index"),
    });
    def_builtin!(env, "bit-set", FnArity::exact(2), |args| match args {
        [v, n] => {
            let val = expect_int(v)?;
            let bit = expect_int(n)? as u32;
            Ok(Value::Int(val | (1i64 << bit)))
        }
        _ => err("bit-set expects value and bit index"),
    });
    def_builtin!(env, "bit-clear", FnArity::exact(2), |args| match args {
        [v, n] => {
            let val = expect_int(v)?;
            let bit = expect_int(n)? as u32;
            Ok(Value::Int(val & !(1i64 << bit)))
        }
        _ => err("bit-clear expects value and bit index"),
    });
    def_builtin!(env, "bit-flip", FnArity::exact(2), |args| match args {
        [v, n] => {
            let val = expect_int(v)?;
            let bit = expect_int(n)? as u32;
            Ok(Value::Int(val ^ (1i64 << bit)))
        }
        _ => err("bit-flip expects value and bit index"),
    });
    def_builtin!(env, "bit-and-not", FnArity::at_least(2), |args| {
        let mut iter = args.iter();
        let mut acc = expect_int(iter.next().unwrap())?;
        for v in iter {
            acc &= !expect_int(v)?;
        }
        Ok(Value::Int(acc))
    });
    def_builtin!(
        env,
        "bit-shift-left",
        FnArity::exact(2),
        |args| match args {
            [a, b] => {
                let acc = expect_int(a)?;
                let shift = expect_int(b)? as u32;
                Ok(Value::Int(acc.wrapping_shl(shift)))
            }
            _ => err("bit-shift-left expects two ints"),
        }
    );
    def_builtin!(
        env,
        "bit-shift-right",
        FnArity::exact(2),
        |args| match args {
            [a, b] => {
                let acc = expect_int(a)?;
                let shift = expect_int(b)? as u32;
                Ok(Value::Int(acc.wrapping_shr(shift)))
            }
            _ => err("bit-shift-right expects two ints"),
        }
    );
    def_builtin!(
        env,
        "unsigned-bit-shift-right",
        FnArity::exact(2),
        |args| match args {
            [a, b] => {
                let acc = expect_int(a)? as u64;
                let shift = expect_int(b)? as u32;
                Ok(Value::Int((acc >> shift) as i64))
            }
            _ => err("unsigned-bit-shift-right expects two ints"),
        }
    );
    def_builtin!(env, "quot", FnArity::exact(2), |args| match args {
        [a, b] => {
            let x = expect_int(a)?;
            let y = expect_int(b)?;
            if y == 0 {
                return err("quot by zero");
            }
            Ok(Value::Int(x / y))
        }
        _ => err("quot expects two ints"),
    });
    def_builtin!(env, "rem", FnArity::exact(2), |args| match args {
        [a, b] => {
            let x = expect_int(a)?;
            let y = expect_int(b)?;
            if y == 0 {
                return err("rem by zero");
            }
            Ok(Value::Int(x - (x / y) * y))
        }
        _ => err("rem expects two ints"),
    });
    def_builtin!(env, "max", FnArity::at_least(1), |args| {
        let (mut acc, mut acc_float) = as_number(&args[0])?;
        for v in args.iter().skip(1) {
            let (n, nf) = as_number(v)?;
            if n > acc {
                acc = n;
            }
            acc_float |= nf || acc.fract() != 0.0;
        }
        Ok(make_number(acc, acc_float))
    });
    def_builtin!(env, "min", FnArity::at_least(1), |args| {
        let (mut acc, mut acc_float) = as_number(&args[0])?;
        for v in args.iter().skip(1) {
            let (n, nf) = as_number(v)?;
            if n < acc {
                acc = n;
            }
            acc_float |= nf || acc.fract() != 0.0;
        }
        Ok(make_number(acc, acc_float))
    });
    def_builtin!(env, "int", FnArity::exact(1), |args| match args {
        [v] => {
            let (n, _) = as_number(v)?;
            Ok(Value::Int(n as i64))
        }
        _ => err("int expects one number"),
    });
    def_builtin!(env, "long", FnArity::exact(1), |args| match args {
        [v] => {
            let (n, _) = as_number(v)?;
            Ok(Value::Int(n as i64))
        }
        _ => err("long expects one number"),
    });
    def_builtin!(env, "float", FnArity::exact(1), |args| match args {
        [v] => {
            let (n, _) = as_number(v)?;
            Ok(Value::Float(n))
        }
        _ => err("float expects one number"),
    });

    // --- math:: ---
    def_builtin!(env, "math::sin", FnArity::exact(1), |args| {
        math_unary("math::sin", args, |n| n.sin())
    });
    def_builtin!(env, "math::cos", FnArity::exact(1), |args| {
        math_unary("math::cos", args, |n| n.cos())
    });
    def_builtin!(env, "math::tan", FnArity::exact(1), |args| {
        math_unary("math::tan", args, |n| n.tan())
    });
    def_builtin!(env, "math::asin", FnArity::exact(1), |args| {
        math_unary("math::asin", args, |n| n.asin())
    });
    def_builtin!(env, "math::acos", FnArity::exact(1), |args| {
        math_unary("math::acos", args, |n| n.acos())
    });
    def_builtin!(env, "math::atan", FnArity::exact(1), |args| {
        math_unary("math::atan", args, |n| n.atan())
    });
    def_builtin!(env, "math::atan2", FnArity::exact(2), |args| {
        math_binary("math::atan2", args, |y, x| y.atan2(x))
    });
    def_builtin!(env, "math::exp", FnArity::exact(1), |args| {
        math_unary("math::exp", args, |n| n.exp())
    });
    def_builtin!(env, "math::log", FnArity::exact(1), |args| {
        math_unary("math::log", args, |n| n.ln())
    });
    def_builtin!(env, "math::log10", FnArity::exact(1), |args| {
        math_unary("math::log10", args, |n| n.log10())
    });
    def_builtin!(env, "math::expm1", FnArity::exact(1), |args| {
        math_unary("math::expm1", args, |n| n.exp_m1())
    });
    def_builtin!(env, "math::log1p", FnArity::exact(1), |args| {
        math_unary("math::log1p", args, |n| n.ln_1p())
    });
    def_builtin!(env, "math::pow", FnArity::exact(2), |args| {
        math_binary("math::pow", args, |base, exp| base.powf(exp))
    });
    def_builtin!(env, "math::sqrt", FnArity::exact(1), |args| {
        math_unary("math::sqrt", args, |n| n.sqrt())
    });
    def_builtin!(env, "math::cbrt", FnArity::exact(1), |args| {
        math_unary("math::cbrt", args, |n| n.cbrt())
    });
    def_builtin!(env, "math::hypot", FnArity::exact(2), |args| {
        math_binary("math::hypot", args, |x, y| x.hypot(y))
    });
    def_builtin!(env, "math::floor", FnArity::exact(1), |args| {
        math_unary("math::floor", args, |n| n.floor())
    });
    def_builtin!(env, "math::ceil", FnArity::exact(1), |args| {
        math_unary("math::ceil", args, |n| n.ceil())
    });
    def_builtin!(env, "math::round", FnArity::exact(1), |args| {
        math_round("math::round", args)
    });
    def_builtin!(env, "math::rint", FnArity::exact(1), |args| {
        math_unary("math::rint", args, round_ties_even)
    });
    def_builtin!(env, "math::to-radians", FnArity::exact(1), |args| {
        math_unary("math::to-radians", args, |n| n.to_radians())
    });
    def_builtin!(env, "math::to-degrees", FnArity::exact(1), |args| {
        math_unary("math::to-degrees", args, |n| n.to_degrees())
    });
}

fn arith(
    op_name: &str,
    args: &[Value],
    init: f64,
    op_fn: impl Fn(f64, f64) -> f64,
    force_float: bool,
) -> Result<Value, CloveError> {
    let (mut acc, mut acc_float) = if args.is_empty() {
        (init, force_float || init.fract() != 0.0)
    } else {
        as_number(&args[0]).map_err(|err| match err {
            CloveError::TypeMismatch {
                actual, context, ..
            } => CloveError::TypeMismatch {
                expected: format!("number (arg 1 to {})", op_name),
                actual,
                context,
            },
            other => other,
        })?
    };
    acc_float |= force_float;
    for (idx, val) in args.iter().enumerate().skip(1) {
        let (n, is_float) = as_number(val).map_err(|err| match err {
            CloveError::TypeMismatch {
                actual, context, ..
            } => CloveError::TypeMismatch {
                expected: format!("number (arg {} to {})", idx + 1, op_name),
                actual,
                context,
            },
            other => other,
        })?;
        acc = op_fn(acc, n);
        acc_float = acc_float || is_float || acc.fract() != 0.0;
    }
    Ok(make_number(acc, acc_float))
}

fn expect_int(value: &Value) -> Result<i64, CloveError> {
    match value {
        Value::Int(i) => Ok(*i),
        other => Err(CloveError::type_mismatch(
            "int",
            crate::builtins::shared::actual_type_with_preview(other),
        )),
    }
}

fn cmp_chain(
    op: &str,
    args: &[Value],
    cmp: impl Fn(f64, f64) -> bool,
) -> Result<Value, CloveError> {
    if args.len() < 2 {
        return Ok(Value::Bool(true));
    }
    let (mut last, _) = as_number(&args[0]).map_err(|err| match err {
        CloveError::TypeMismatch {
            actual, context, ..
        } => CloveError::TypeMismatch {
            expected: format!("number (arg 1 to {})", op),
            actual,
            context,
        },
        other => other,
    })?;
    for (idx, val) in args.iter().enumerate().skip(1) {
        let (n, _) = as_number(val).map_err(|err| match err {
            CloveError::TypeMismatch {
                actual, context, ..
            } => CloveError::TypeMismatch {
                expected: format!("number (arg {} to {})", idx + 1, op),
                actual,
                context,
            },
            other => other,
        })?;
        if !cmp(last, n) {
            return Ok(Value::Bool(false));
        }
        last = n;
    }
    Ok(Value::Bool(true))
}

fn math_unary(op: &str, args: &[Value], f: impl Fn(f64) -> f64) -> Result<Value, CloveError> {
    match args {
        [v] => {
            let (n, _) = as_number(v)?;
            Ok(Value::Float(f(n)))
        }
        _ => err(format!("{} expects one number", op)),
    }
}

fn math_binary(op: &str, args: &[Value], f: impl Fn(f64, f64) -> f64) -> Result<Value, CloveError> {
    match args {
        [a, b] => {
            let (x, _) = as_number(a)?;
            let (y, _) = as_number(b)?;
            Ok(Value::Float(f(x, y)))
        }
        _ => err(format!("{} expects two numbers", op)),
    }
}

fn math_round(op: &str, args: &[Value]) -> Result<Value, CloveError> {
    match args {
        [v] => {
            let (n, _) = as_number(v)?;
            Ok(Value::Int(n.round() as i64))
        }
        _ => err(format!("{} expects one number", op)),
    }
}

fn round_ties_even(value: f64) -> f64 {
    if !value.is_finite() {
        return value;
    }
    let floor = value.floor();
    let diff = value - floor;
    if diff < 0.5 {
        floor
    } else if diff > 0.5 {
        floor + 1.0
    } else if (floor / 2.0).fract() == 0.0 {
        floor
    } else {
        floor + 1.0
    }
}

fn register_math_metas() {
    let mut plus = FnMeta::new("core", "+");
    plus.arglist.push("[& nums]".into());
    plus.doc = Some(
        "Adds the given numbers left-to-right; zero arguments yield 0 and a single argument is returned unchanged."
            .into(),
    );
    plus.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    plus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    plus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    plus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Int],
        rest: Some(TypeKind::vector(TypeKind::Int)),
        ret_type: TypeKind::Int,
        special_op: Some(SpecialOp::IntAdd2),
    });
    plus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Float,
        special_op: Some(SpecialOp::FloatAdd2),
    });
    plus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: Some(TypeKind::vector(TypeKind::Any)),
        ret_type: TypeKind::Any,
        special_op: None,
    });
    plus.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(plus);

    let mut minus = FnMeta::new("core", "-");
    minus.arglist.push("[x & ys]".into());
    minus.doc = Some(
        "Negates a single argument or subtracts each subsequent number from the first argument."
            .into(),
    );
    minus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    minus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    minus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Int],
        rest: Some(TypeKind::vector(TypeKind::Int)),
        ret_type: TypeKind::Int,
        special_op: None,
    });
    minus.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Float,
        special_op: None,
    });
    minus.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(minus);

    let mut mul = FnMeta::new("core", "*");
    mul.arglist.push("[& nums]".into());
    mul.doc = Some(
        "Multiplies the arguments starting from 1; returns 1 when called with no arguments.".into(),
    );
    mul.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    mul.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    mul.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    mul.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int, TypeKind::Int],
        rest: Some(TypeKind::vector(TypeKind::Int)),
        ret_type: TypeKind::Int,
        special_op: None,
    });
    mul.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Float,
        special_op: None,
    });
    mul.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(mul);

    let mut div = FnMeta::new("core", "/");
    div.arglist.push("[x & ys]".into());
    div.doc = Some(
        "Divides the first argument by each following number; one argument returns its reciprocal and division by zero is an error."
            .into(),
    );
    div.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    div.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Float,
        special_op: None,
    });
    div.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(div);

    let mut lt = FnMeta::new("core", "<");
    lt.arglist.push("[& nums]".into());
    lt.doc = Some(
        "Returns true when the arguments are in strictly increasing order (true for zero or one argument)."
            .into(),
    );
    lt.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lt.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lt.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lt.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(lt);

    let mut lte = FnMeta::new("core", "<=");
    lte.arglist.push("[& nums]".into());
    lte.doc = Some(
        "Returns true when the arguments are non-decreasing (true for zero or one argument)."
            .into(),
    );
    lte.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lte.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lte.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    lte.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(lte);

    let mut inc = FnMeta::new("core", "inc");
    inc.arglist.push("[x]".into());
    inc.doc = Some("Returns the argument increased by 1, preserving int/float shape.".into());
    inc.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    inc.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    inc.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(inc);

    let mut dec = FnMeta::new("core", "dec");
    dec.arglist.push("[x]".into());
    dec.doc = Some("Returns the argument decreased by 1, preserving int/float shape.".into());
    dec.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    dec.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    dec.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(dec);

    let mut abs = FnMeta::new("core", "abs");
    abs.arglist.push("[x]".into());
    abs.doc = Some("Returns the absolute value, preserving int/float shape.".into());
    abs.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Int],
        rest: None,
        ret_type: TypeKind::Int,
        special_op: None,
    });
    abs.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Float,
        special_op: None,
    });
    abs.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(abs);

    let mut mod_meta = FnMeta::new("core", "mod");
    mod_meta.arglist.push("[num div]".into());
    mod_meta.doc = Some("Remainder with Euclidean semantics; divisor zero raises an error.".into());
    mod_meta.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Any,
        special_op: None,
    });
    mod_meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(mod_meta);

    let mut gt = FnMeta::new("core", ">");
    gt.arglist.push("[& nums]".into());
    gt.doc = Some("True when arguments are strictly decreasing (true for zero or one arg).".into());
    gt.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gt.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gt.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gt.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(gt);

    let mut gte = FnMeta::new("core", ">=");
    gte.arglist.push("[& nums]".into());
    gte.doc = Some(
        "True when arguments never increase (allows equals; true for zero or one arg).".into(),
    );
    gte.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gte.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gte.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float, TypeKind::Float],
        rest: Some(TypeKind::vector(TypeKind::Float)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    gte.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(gte);

    let mut eq = FnMeta::new("core", "=");
    eq.arglist.push("[& values]".into());
    eq.doc = Some(
        "Returns true when all arguments are equal; zero or one argument returns true.".into(),
    );
    eq.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    eq.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    eq.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: Some(TypeKind::vector(TypeKind::Any)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    eq.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(eq);

    let mut neq = FnMeta::new("core", "not=");
    neq.arglist.push("[& values]".into());
    neq.doc = Some(
        "Negated equality: returns true when some argument differs; false for zero or one arg."
            .into(),
    );
    neq.overloads.push(FnOverload {
        arg_types: vec![],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    neq.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    neq.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Any, TypeKind::Any],
        rest: Some(TypeKind::vector(TypeKind::Any)),
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    neq.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(neq);

    let mut zero = FnMeta::new("core", "zero?");
    zero.arglist.push("[x]".into());
    zero.doc = Some("Returns true when the number is exactly zero.".into());
    zero.overloads.push(FnOverload {
        arg_types: vec![TypeKind::Float],
        rest: None,
        ret_type: TypeKind::Bool,
        special_op: None,
    });
    zero.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(zero);

    fn num_type() -> TypeKind {
        TypeKind::union(vec![TypeKind::Int, TypeKind::Float])
    }
    fn register_math_unary(name: &str, arglist: &str, doc: &str, ret_type: TypeKind) {
        let mut meta = FnMeta::new("math", name);
        meta.arglist.push(arglist.into());
        meta.doc = Some(doc.into());
        meta.overloads.push(FnOverload {
            arg_types: vec![num_type()],
            rest: None,
            ret_type,
            special_op: None,
        });
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }
    fn register_math_binary(name: &str, arglist: &str, doc: &str, ret_type: TypeKind) {
        let mut meta = FnMeta::new("math", name);
        meta.arglist.push(arglist.into());
        meta.doc = Some(doc.into());
        meta.overloads.push(FnOverload {
            arg_types: vec![num_type(), num_type()],
            rest: None,
            ret_type,
            special_op: None,
        });
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }

    register_math_unary(
        "sin",
        "[x]",
        "Return the sine of x (radians).",
        TypeKind::Float,
    );
    register_math_unary(
        "cos",
        "[x]",
        "Return the cosine of x (radians).",
        TypeKind::Float,
    );
    register_math_unary(
        "tan",
        "[x]",
        "Return the tangent of x (radians).",
        TypeKind::Float,
    );
    register_math_unary(
        "asin",
        "[x]",
        "Return the arcsine of x (radians).",
        TypeKind::Float,
    );
    register_math_unary(
        "acos",
        "[x]",
        "Return the arccosine of x (radians).",
        TypeKind::Float,
    );
    register_math_unary(
        "atan",
        "[x]",
        "Return the arctangent of x (radians).",
        TypeKind::Float,
    );
    register_math_binary(
        "atan2",
        "[y x]",
        "Return the arctangent of y/x, using the signs to determine the quadrant.",
        TypeKind::Float,
    );
    register_math_unary("exp", "[x]", "Return e raised to x.", TypeKind::Float);
    register_math_unary(
        "log",
        "[x]",
        "Return the natural logarithm of x.",
        TypeKind::Float,
    );
    register_math_unary(
        "log10",
        "[x]",
        "Return the base-10 logarithm of x.",
        TypeKind::Float,
    );
    register_math_unary(
        "expm1",
        "[x]",
        "Return exp(x) - 1 with better precision for small x.",
        TypeKind::Float,
    );
    register_math_unary(
        "log1p",
        "[x]",
        "Return log(1 + x) with better precision for small x.",
        TypeKind::Float,
    );
    register_math_binary(
        "pow",
        "[base exp]",
        "Return base raised to exp.",
        TypeKind::Float,
    );
    register_math_unary(
        "sqrt",
        "[x]",
        "Return the square root of x.",
        TypeKind::Float,
    );
    register_math_unary("cbrt", "[x]", "Return the cube root of x.", TypeKind::Float);
    register_math_binary(
        "hypot",
        "[x y]",
        "Return sqrt(x^2 + y^2) without undue overflow or underflow.",
        TypeKind::Float,
    );
    register_math_unary(
        "floor",
        "[x]",
        "Return the largest integer <= x as a float.",
        TypeKind::Float,
    );
    register_math_unary(
        "ceil",
        "[x]",
        "Return the smallest integer >= x as a float.",
        TypeKind::Float,
    );
    register_math_unary(
        "round",
        "[x]",
        "Round to the nearest integer (ties away from zero).",
        TypeKind::Int,
    );
    register_math_unary(
        "rint",
        "[x]",
        "Round to the nearest integer using ties-to-even.",
        TypeKind::Float,
    );
    register_math_unary(
        "to-radians",
        "[degrees]",
        "Convert degrees to radians.",
        TypeKind::Float,
    );
    register_math_unary(
        "to-degrees",
        "[radians]",
        "Convert radians to degrees.",
        TypeKind::Float,
    );
}
