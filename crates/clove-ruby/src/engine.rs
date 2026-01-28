use std::sync::Arc;

use magnus::try_convert::TryConvert;
use magnus::value::ReprValue;
use magnus::IntoValue;
use magnus::{
    embed, Error as RubyError, RArray, RClass, RHash, RRegexp, Ruby, Symbol, Value as RubyVal,
};

use clove_core::ast::{DurationUnit, Form, HashMap, Key, RegexValue, Value, Vector};
use clove_core::env::EnvRef;
use clove_core::error::CloveError;
use clove_core::eval::to_key_value;
use clove_core::foreign::ForeignEngine;
use clove_core::form_to_string::form_to_string;
use im::HashSet;
use std::sync::Once;

pub struct RubyEngine;

impl RubyEngine {
    pub fn new() -> Arc<Self> {
        static INIT: Once = Once::new();
        static mut _CLEANUP: Option<embed::Cleanup> = None;
        INIT.call_once(|| unsafe {
            _CLEANUP = Some(embed::init());
            set_ruby_encoding();
        });
        Arc::new(Self)
    }
}

impl ForeignEngine for RubyEngine {
    fn tag(&self) -> &str {
        "rb"
    }

    fn eval_block(
        &self,
        code: &str,
        env: EnvRef,
        span: Option<clove_core::ast::Span>,
    ) -> Result<Value, CloveError> {
        let ruby = Ruby::get()
            .map_err(|e| CloveError::foreign("rb", format!("ruby not available: {}", e)))?;

        let binding = ruby
            .eval::<RubyVal>("binding()")
            .map_err(|e| CloveError::foreign("rb", e.to_string()))?;

        let env_hash = ruby.hash_new();
        for (k, v) in env.read().unwrap().flatten() {
            if valid_ident(&k) && !k.starts_with("__") {
                if let Ok(rv) = to_ruby(&ruby, &v) {
                    env_hash.aset::<&str, _>(&k, rv).ok();
                    binding
                        .funcall::<_, _, RubyVal>("local_variable_set", (k.as_str(), rv))
                        .ok();
                }
            }
        }
        let _ = binding.funcall::<_, _, RubyVal>("local_variable_set", ("__env__", env_hash));
        let wrapped_src = format!(
      "begin\n{}\nrescue Exception => e\n  raise e, e.full_message(highlight: false), e.backtrace\nend",
      code
    );
        let (filename, line) = match span {
            Some(s) => (
                format!("ruby block (line {})", s.line),
                s.line.saturating_sub(1) as i64,
            ),
            None => ("__clj_rb_block__".to_string(), 1),
        };
        let result: RubyVal = binding
            .funcall("eval", (wrapped_src, filename, line))
            .map_err(|e| CloveError::foreign("rb", e.to_string()))?;

        let locals: Result<RArray, _> = binding.funcall("local_variables", ());
        if let Ok(names) = locals {
            for name_val in names.into_iter() {
                let sym = match Symbol::try_convert(name_val) {
                    Ok(s) => s,
                    Err(_) => continue,
                };
                let key_name: std::borrow::Cow<'_, str> = match sym.name() {
                    Ok(n) => n,
                    Err(_) => continue,
                };
                if !valid_ident(&key_name) || key_name.starts_with("__") {
                    continue;
                }
                if let Ok(v) = binding.funcall::<_, _, RubyVal>("local_variable_get", (sym,)) {
                    if let Ok(val) = from_ruby_value(v) {
                        env.write().unwrap().set_in_chain(&key_name, val);
                    }
                }
            }
        }
        from_ruby_value(result).map_err(|e| CloveError::foreign("rb", e.to_string()))
    }

    fn eval_fallback(&self, form: &Form, env: EnvRef) -> Result<Value, CloveError> {
        let src = form_to_string(form, self.tag());
        self.eval_block(&src, env, Some(form.span))
    }

    fn call_symbol(
        &self,
        path: &str,
        args: &[Value],
        span: Option<clove_core::ast::Span>,
    ) -> Result<Value, CloveError> {
        let ruby = Ruby::get()
            .map_err(|e| CloveError::foreign("rb", format!("ruby not available: {}", e)))?;

        let parts: Vec<&str> = path.split('.').collect();
        if parts.len() < 2 {
            return Err(CloveError::foreign(
                "rb",
                format!("invalid foreign symbol (need receiver.method): {}", path),
            ));
        }
        let method = *parts
            .last()
            .expect("parts has at least 2 elements so last exists");
        let mut recv: RubyVal = ruby
            .eval(parts[0])
            .map_err(|e| CloveError::foreign("rb", format_with_span(span, e.to_string())))?;
        for seg in parts.iter().skip(1).take(parts.len().saturating_sub(2)) {
            recv = recv
                .funcall::<_, _, RubyVal>("const_get", (seg.to_string(),))
                .map_err(|e| CloveError::foreign("rb", format_with_span(span, e.to_string())))?;
        }

        let ruby_args: Vec<RubyVal> = args
            .iter()
            .map(|a| {
                to_ruby(&ruby, a)
                    .map_err(|e| CloveError::foreign("rb", format_with_span(span, e.to_string())))
            })
            .collect::<Result<_, _>>()?;

        let result: RubyVal = recv
            .funcall(method, ruby_args.as_slice())
            .map_err(|e| CloveError::foreign("rb", format_with_span(span, e.to_string())))?;

        from_ruby_value(result)
            .map_err(|e| CloveError::foreign("rb", format_with_span(span, e.to_string())))
    }
}

fn valid_ident(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn to_ruby(ruby: &Ruby, val: &Value) -> Result<magnus::Value, RubyError> {
    match val {
        Value::Int(n) => Ok(ruby.integer_from_i64(*n).into_value_with(ruby)),
        Value::Float(n) => Ok(ruby.float_from_f64(*n).into_value_with(ruby)),
        Value::String(s) => Ok(ruby.str_new(s).into_value_with(ruby)),
        Value::Bool(b) => Ok(b.into_value_with(ruby)),
        Value::Nil => Ok(ruby.qnil().into_value_with(ruby)),
        Value::Duration(d) => Ok(ruby
            .float_from_f64(d.to_unit(DurationUnit::Second))
            .into_value_with(ruby)),
        Value::Regex(re) => {
            let re_class: RClass = ruby.eval("Regexp").map_err(|e| e)?;
            let obj: RubyVal = re_class.funcall("new", (re.pattern.as_str(),))?;
            Ok(obj)
        }
        Value::Seq(handle) => {
            let collected = handle
                .collect_all()
                .map_err(|e| RubyError::new(ruby.exception_runtime_error(), e.to_string()))?;
            let ary = ruby.ary_new();
            for item in collected {
                ary.push(to_ruby(ruby, &item)?)?;
            }
            Ok(ary.into_value_with(ruby))
        }
        Value::List(items) | Value::Vector(items) => {
            let ary = ruby.ary_new();
            for item in items {
                ary.push(to_ruby(ruby, item)?)?;
            }
            Ok(ary.into_value_with(ruby))
        }
        Value::SortedMap(map) => {
            let h = ruby.hash_new();
            for (k, v) in &map.entries {
                let key_val = key_to_ruby(ruby, k)?;
                let val_val = to_ruby(ruby, v)?;
                h.aset(key_val, val_val)?;
            }
            Ok(h.into_value_with(ruby))
        }
        Value::Map(map) => {
            let h = ruby.hash_new();
            for (k, v) in map {
                let key_val = key_to_ruby(ruby, k)?;
                let val_val = to_ruby(ruby, v)?;
                h.aset(key_val, val_val)?;
            }
            Ok(h.into_value_with(ruby))
        }
        Value::SortedSet(items) => {
            let ary = ruby.ary_new();
            for item in &items.entries {
                ary.push(to_ruby(ruby, item)?)?;
            }
            let set_class: magnus::RClass = ruby.eval("require 'set'; Set").map_err(|e| e)?;
            let set_obj: magnus::Value = set_class.funcall("new", (ary,))?;
            Ok(set_obj)
        }
        Value::Set(items) => {
            let ary = ruby.ary_new();
            for item in items {
                ary.push(to_ruby(ruby, item)?)?;
            }
            let set_class: magnus::RClass = ruby.eval("require 'set'; Set").map_err(|e| e)?;
            let set_obj: magnus::Value = set_class.funcall("new", (ary,))?;
            Ok(set_obj)
        }
        Value::Symbol(s) => Ok(ruby.intern(s).into_value_with(ruby)),
        Value::ForeignCallable { .. } => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass foreign callable into rb block",
        )),
        Value::Foreign(_) => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass foreign value into rb",
        )),
        Value::NativeBuf { .. } => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass native buffer into rb",
        )),
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. } => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass function into rb block",
        )),
        Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_) => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass mutable collection into rb block; use imut",
        )),
        Value::TransientVector(_) | Value::TransientMap(_) | Value::TransientSet(_) => {
            Err(RubyError::new(
                ruby.exception_arg_error(),
                "cannot pass transient collection into rb block; use persistent!",
            ))
        }
        Value::Atom(_)
        | Value::Chan(_)
        | Value::Promise(_)
        | Value::Task(_)
        | Value::Future(_)
        | Value::Agent(_)
        | Value::Delay(_) => Err(RubyError::new(
            ruby.exception_arg_error(),
            "cannot pass concurrency handle into rb block",
        )),
    }
}

fn key_to_ruby(ruby: &Ruby, k: &Key) -> Result<magnus::Value, RubyError> {
    match k {
        Key::Keyword(s) | Key::Symbol(s) => Ok(ruby.intern(s).into_value_with(ruby)),
        Key::String(s) => Ok(ruby.str_new(s).into_value_with(ruby)),
        Key::Number(n) => Ok(ruby.integer_from_i64(*n).into_value_with(ruby)),
        Key::Bool(b) => Ok(b.into_value_with(ruby)),
    }
}

fn ruby_key_to_key(val: RubyVal) -> Result<Key, RubyError> {
    if let Ok(sym) = Symbol::try_convert(val) {
        let name: std::borrow::Cow<'_, str> = sym.name()?;
        return Ok(Key::Keyword(name.into_owned()));
    }
    if let Ok(s) = String::try_convert(val) {
        return Ok(Key::String(s));
    }
    if let Ok(i) = i64::try_convert(val) {
        return Ok(Key::Number(i));
    }
    if let Ok(b) = bool::try_convert(val) {
        return Ok(Key::Bool(b));
    }
    let value = from_ruby_value(val)?;
    Ok(to_key_value(value))
}

fn from_ruby_value(val: magnus::Value) -> Result<Value, RubyError> {
    if val.is_nil() {
        return Ok(Value::Nil);
    }
    if let Ok(f) = f64::try_convert(val) {
        if let Ok(is_int) = val.funcall::<_, _, bool>("integer?", ()) {
            if is_int {
                return Ok(Value::Int(f as i64));
            }
        }
        return Ok(Value::Float(f));
    }
    if let Ok(i) = i64::try_convert(val) {
        return Ok(Value::Int(i));
    }
    if let Ok(s) = String::try_convert(val) {
        return Ok(Value::String(s));
    }
    if let Ok(sym) = Symbol::try_convert(val) {
        let name: std::borrow::Cow<'_, str> = sym.name()?;
        return Ok(Value::Symbol(name.into_owned()));
    }
    if let Ok(array) = RArray::try_convert(val) {
        let mut out = Vec::with_capacity(array.len());
        for item in array.into_iter() {
            out.push(from_ruby_value(item)?);
        }
        return Ok(Value::Vector(Vector::from(out)));
    }
    if let Ok(hash) = RHash::try_convert(val) {
        let mut out = HashMap::new();
        hash.foreach(|k: RubyVal, v: RubyVal| {
            let map_key = ruby_key_to_key(k)?;
            out.insert(map_key, from_ruby_value(v)?);
            Ok(magnus::r_hash::ForEach::Continue)
        })?;
        return Ok(Value::Map(out));
    }
    if let Ok(ruby) = Ruby::get() {
        if let Ok(set_class) =
            ruby.eval::<RClass>("begin; require 'set'; Set; rescue LoadError; nil; end")
        {
            if val.is_kind_of(set_class) {
                let ary: RArray = val.funcall("to_a", ())?;
                let mut out = HashSet::new();
                for item in ary.into_iter() {
                    out.insert(from_ruby_value(item)?);
                }
                return Ok(Value::Set(out));
            }
        }
    }
    if let Ok(re) = RRegexp::try_convert(val) {
        let pat: String = re.funcall("source", ())?;
        return RegexValue::new(pat).map(Value::Regex).map_err(|e| {
            let ruby = Ruby::get().unwrap();
            RubyError::new(ruby.exception_arg_error(), e.to_string())
        });
    }
    if let Ok(b) = bool::try_convert(val) {
        return Ok(Value::Bool(b));
    }
    let ruby = Ruby::get().unwrap();
    Err(RubyError::new(
        ruby.exception_type_error(),
        "unsupported ruby value",
    ))
}

fn set_ruby_encoding() {
    if let Ok(ruby) = Ruby::get() {
        let _ = ruby.eval::<RubyVal>(
            r#"
            Encoding.default_external = Encoding::UTF_8
            Encoding.default_internal = Encoding::UTF_8
            STDOUT.set_encoding(Encoding::UTF_8) if STDOUT.respond_to?(:set_encoding)
            STDERR.set_encoding(Encoding::UTF_8) if STDERR.respond_to?(:set_encoding)
            "#,
        );
    }
}

fn format_with_span(span: Option<clove_core::ast::Span>, msg: String) -> String {
    if let Some(s) = span {
        format!("unknown:{}:{}: {}", s.line, s.col, msg)
    } else {
        msg
    }
}
