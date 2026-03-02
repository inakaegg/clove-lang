use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::time::Instant;

use rand::seq::SliceRandom;
use rand::Rng;
use regex::Regex;

use crate::aliases;
use crate::ast::Expr;
use crate::builtins;
use crate::error::Clove2Error;
use crate::reader::read_all;
use crate::syntax::{parse_forms, AstExpr, Binding, Param, TopLevel};
use crate::type_check::{as_type, expect_type};
use crate::types::Type;
use crate::use_directive::{parse_use_directives, MutMode};
use crate::value::{Key, Value};

type EnvRef = Rc<Env>;

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    static REGEX_CACHE: RefCell<HashMap<String, Regex>> = RefCell::new(HashMap::new());
}

#[derive(Debug, Default)]
struct Env {
    parent: Option<EnvRef>,
    values: RefCell<HashMap<String, Value>>,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(Env {
            parent: None,
            values: RefCell::new(HashMap::new()),
        })
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(Env {
            parent: Some(parent),
            values: RefCell::new(HashMap::new()),
        })
    }

    fn set(&self, name: &str, value: Value) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.values.borrow().get(name) {
            return Some(value.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }
        None
    }

    fn with_value_ref<F, R>(&self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&Value) -> R,
    {
        {
            let values = self.values.borrow();
            if let Some(value) = values.get(name) {
                return Some(f(value));
            }
        }
        if let Some(parent) = &self.parent {
            return parent.with_value_ref(name, f);
        }
        None
    }

    fn with_value_mut<F, R>(&self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut Value) -> R,
    {
        {
            let mut values = self.values.borrow_mut();
            if let Some(value) = values.get_mut(name) {
                return Some(f(value));
            }
        }
        if let Some(parent) = &self.parent {
            return parent.with_value_mut(name, f);
        }
        None
    }
}

fn count_map_ptr_in_env(env: &EnvRef, target: &Rc<BTreeMap<Key, Value>>) -> usize {
    let mut total = 0;
    let mut current = Some(env.clone());
    while let Some(node) = current {
        let values = node.values.borrow();
        for value in values.values() {
            let mut map_stack = HashSet::new();
            let mut vec_stack = HashSet::new();
            total += count_map_ptr_in_value(value, target, &mut map_stack, &mut vec_stack);
        }
        current = node.parent.clone();
    }
    total
}

fn map_unique_for_symbol(env: &EnvRef, name: &str) -> Option<bool> {
    let map_ref = env.with_value_ref(name, |value| match value {
        Value::Map(map) => Some(map.clone()),
        _ => None,
    })??;
    Some(count_map_ptr_in_env(env, &map_ref) <= 1)
}

fn count_map_ptr_in_value(
    value: &Value,
    target: &Rc<BTreeMap<Key, Value>>,
    map_stack: &mut HashSet<*const BTreeMap<Key, Value>>,
    vec_stack: &mut HashSet<*const Vec<Value>>,
) -> usize {
    match value {
        Value::Map(map) => {
            let mut count = 0;
            if Rc::ptr_eq(map, target) {
                count += 1;
            }
            let ptr = Rc::as_ptr(map);
            if !map_stack.insert(ptr) {
                return count;
            }
            for inner in map.values() {
                count += count_map_ptr_in_value(inner, target, map_stack, vec_stack);
            }
            map_stack.remove(&ptr);
            count
        }
        Value::Vec(items) => {
            let ptr = Rc::as_ptr(items);
            if !vec_stack.insert(ptr) {
                return 0;
            }
            let mut count = 0;
            for item in items.iter() {
                count += count_map_ptr_in_value(item, target, map_stack, vec_stack);
            }
            vec_stack.remove(&ptr);
            count
        }
        Value::List(items) | Value::Set(items) => {
            let mut count = 0;
            for item in items.iter() {
                count += count_map_ptr_in_value(item, target, map_stack, vec_stack);
            }
            count
        }
        Value::Partial(partial) => {
            let mut count = count_map_ptr_in_value(&partial.func, target, map_stack, vec_stack);
            for arg in partial.args.iter() {
                count += count_map_ptr_in_value(arg, target, map_stack, vec_stack);
            }
            count
        }
        _ => 0,
    }
}

#[derive(Clone, Debug)]
struct Function {
    params: Vec<String>,
    rest: Option<String>,
    body: Vec<AstExpr>,
    env: EnvRef,
}

pub type NativeFunction = fn(&mut NativeEnv, Vec<Value>) -> Result<Value, Clove2Error>;

struct Evaluator {
    functions: Vec<Function>,
    native_functions: Vec<NativeFunction>,
    global_env: Option<EnvRef>,
    mut_mode: MutMode,
    mut_mode_stack: Vec<MutMode>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self {
            functions: Vec::new(),
            native_functions: Vec::new(),
            global_env: None,
            mut_mode: MutMode::Mut,
            mut_mode_stack: Vec::new(),
        }
    }
}

pub struct NativeEnv<'a> {
    eval: &'a mut Evaluator,
}

impl<'a> NativeEnv<'a> {
    pub fn apply_builtin(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        self.eval.eval_builtin_value(name, args)
    }

    pub fn call_value(&mut self, func: &Value, args: Vec<Value>) -> Result<Value, Clove2Error> {
        self.eval.call_value(func, args)
    }

    pub fn define_native_function(&mut self, func: NativeFunction) -> Value {
        let id = self.eval.define_native_function(func);
        Value::NativeFunction(id)
    }

    pub fn push_mut_mode(&mut self, mode: MutMode) {
        self.eval.push_mut_mode(mode);
    }

    pub fn pop_mut_mode(&mut self) {
        self.eval.pop_mut_mode();
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.eval.get_global(name)
    }

    pub fn set_global(&mut self, name: &str, value: Value) -> Result<(), Clove2Error> {
        self.eval.set_global_value(name, value)
    }

    pub fn define_global(&mut self, name: &str, value: Value) -> Result<(), Clove2Error> {
        self.eval.define_global_value(name, value)
    }

    pub fn update_in_global(
        &mut self,
        name: &str,
        path: Value,
        func: Value,
        extras: Vec<Value>,
    ) -> Result<Value, Clove2Error> {
        self.eval.update_in_global_values(name, path, func, extras)
    }
}

pub struct Runtime {
    eval: Evaluator,
    env: EnvRef,
}

impl Runtime {
    pub fn new() -> Self {
        let env = Env::new();
        let mut eval = Evaluator::default();
        eval.set_global_env(env.clone());
        Self { eval, env }
    }

    pub fn set_default_mut_mode(&mut self, mode: MutMode) {
        self.eval.set_mut_mode(mode);
    }

    pub fn set_global(&mut self, name: &str, value: Value) {
        self.env.set(name, value);
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.env.get(name)
    }

    pub fn eval_expr(&mut self, expr: &AstExpr) -> Result<Value, Clove2Error> {
        self.eval.eval_expr(expr, self.env.clone())
    }

    pub fn define_function(&mut self, params: Vec<Param>, body: Vec<AstExpr>) -> Value {
        let func_id = self.eval.define_function(&params, &body, self.env.clone());
        Value::Function(func_id)
    }

    pub fn define_native_function(&mut self, func: NativeFunction) -> Value {
        let id = self.eval.define_native_function(func);
        Value::NativeFunction(id)
    }

    pub fn with_native_env<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut NativeEnv) -> R,
    {
        let mut env = NativeEnv {
            eval: &mut self.eval,
        };
        f(&mut env)
    }
}

#[derive(Copy, Clone, Debug)]
enum ValueType {
    Bool,
    Int,
    Float,
    Number,
    Str,
    Keyword,
    Symbol,
    Vec,
    Map,
    Fn,
}

impl Evaluator {
    fn mut_mode(&self) -> MutMode {
        self.mut_mode
    }

    fn set_mut_mode(&mut self, mode: MutMode) {
        self.mut_mode = mode;
    }

    fn push_mut_mode(&mut self, mode: MutMode) {
        self.mut_mode_stack.push(self.mut_mode);
        self.mut_mode = mode;
    }

    fn pop_mut_mode(&mut self) {
        if let Some(prev) = self.mut_mode_stack.pop() {
            self.mut_mode = prev;
        }
    }

    fn vec_for_update<'a>(
        &self,
        items: &'a mut Rc<Vec<Value>>,
        name: &str,
    ) -> Result<&'a mut Vec<Value>, Clove2Error> {
        match self.mut_mode() {
            MutMode::Mut => Rc::get_mut(items).ok_or_else(|| {
                Clove2Error::new(format!("{} requires unique vector in mut mode", name))
            }),
            MutMode::Imut => Ok(Rc::make_mut(items)),
        }
    }

    fn map_for_update<'a>(
        &self,
        map: &'a mut Rc<BTreeMap<Key, Value>>,
        name: &str,
    ) -> Result<&'a mut BTreeMap<Key, Value>, Clove2Error> {
        match self.mut_mode() {
            MutMode::Mut => Rc::get_mut(map).ok_or_else(|| {
                Clove2Error::new(format!("{} requires unique map in mut mode", name))
            }),
            MutMode::Imut => Ok(Rc::make_mut(map)),
        }
    }

    fn map_for_update_with_unique<'a>(
        &self,
        map: &'a mut Rc<BTreeMap<Key, Value>>,
        name: &str,
        map_unique: bool,
    ) -> Result<&'a mut BTreeMap<Key, Value>, Clove2Error> {
        match self.mut_mode() {
            MutMode::Imut => Ok(Rc::make_mut(map)),
            MutMode::Mut => {
                let is_unique = Rc::strong_count(map) == 1;
                if is_unique {
                    return Ok(Rc::get_mut(map).expect("unique map"));
                }
                if map_unique {
                    return Ok(Rc::make_mut(map));
                }
                Err(Clove2Error::new(format!(
                    "{} requires unique map in mut mode",
                    name
                )))
            }
        }
    }
    fn set_global_env(&mut self, env: EnvRef) {
        self.global_env = Some(env);
    }

    fn define_global_value(&mut self, name: &str, value: Value) -> Result<(), Clove2Error> {
        let env = self
            .global_env
            .clone()
            .ok_or_else(|| Clove2Error::new("global env is not initialized"))?;
        env.set(name, value);
        Ok(())
    }

    fn define_native_function(&mut self, func: NativeFunction) -> usize {
        let id = self.native_functions.len();
        self.native_functions.push(func);
        id
    }

    fn get_global(&self, name: &str) -> Option<Value> {
        let env = self.global_env.as_ref()?;
        env.get(name)
    }

    fn set_global_value(&mut self, name: &str, value: Value) -> Result<(), Clove2Error> {
        let env = self
            .global_env
            .as_ref()
            .ok_or_else(|| Clove2Error::new("global env is not initialized"))?;
        if env.get(name).is_none() {
            return Err(Clove2Error::new(format!(
                "set! expects existing global var: {}",
                name
            )));
        }
        env.set(name, value);
        Ok(())
    }

    fn eval_program(&mut self, items: &[TopLevel]) -> Result<Value, Clove2Error> {
        let env = Env::new();
        self.set_global_env(env.clone());
        let mut last = Value::Nil;
        for item in items {
            if matches!(self.mut_mode(), MutMode::Mut) {
                // Avoid keeping internal references across top-level forms in mut mode.
                last = Value::Nil;
            }
            match item {
                TopLevel::Def { name, value, .. } => {
                    let value = self.eval_expr(value, env.clone())?;
                    env.set(name, value.clone());
                    last = value;
                }
                TopLevel::Defn {
                    name, params, body, ..
                } => {
                    let func_id = self.define_function(params, body, env.clone());
                    let value = Value::Function(func_id);
                    env.set(name, value.clone());
                    last = value;
                }
                TopLevel::DefType { .. } => {}
                TopLevel::DefForeign { decl, .. } => {
                    let value = Value::Builtin(format!("__foreign::{}", decl.name));
                    env.set(&decl.name, value.clone());
                    last = value;
                }
                TopLevel::Expr { expr, .. } => {
                    if is_use_form(expr) {
                        continue;
                    }
                    last = self.eval_expr(expr, env.clone())?;
                }
            }
        }
        Ok(last)
    }

    fn define_function(&mut self, params: &[Param], body: &[AstExpr], env: EnvRef) -> usize {
        let mut names = Vec::new();
        let mut rest = None;
        for param in params {
            if param.rest {
                rest = Some(param.name.clone());
            } else {
                names.push(param.name.clone());
            }
        }
        let func = Function {
            params: names,
            rest,
            body: body.to_vec(),
            env,
        };
        let id = self.functions.len();
        self.functions.push(func);
        id
    }

    fn eval_expr(&mut self, expr: &AstExpr, env: EnvRef) -> Result<Value, Clove2Error> {
        match expr {
            AstExpr::Literal(lit) => Ok(match lit {
                crate::ast::Literal::Nil => Value::Nil,
                crate::ast::Literal::Bool(value) => Value::Bool(*value),
                crate::ast::Literal::Int(value) => Value::Int(*value),
                crate::ast::Literal::Float(value) => Value::Float(*value),
                crate::ast::Literal::Str(value) => Value::Str(value.clone()),
                crate::ast::Literal::Regex(value) => Value::Regex(value.clone()),
            }),
            AstExpr::Symbol(sym) => match env.get(sym) {
                Some(value) => Ok(value),
                None => {
                    let canonical = aliases::resolve_alias(sym);
                    if builtins::is_builtin(canonical) || is_extra_builtin(canonical) {
                        Ok(Value::Builtin(canonical.to_string()))
                    } else {
                        Err(Clove2Error::new(format!("unknown symbol: {}", sym)))
                    }
                }
            },
            AstExpr::Keyword(sym) => Ok(Value::Keyword(sym.clone())),
            AstExpr::Vector(items) => {
                let mut out = Vec::new();
                for item in items {
                    out.push(self.eval_expr(item, env.clone())?);
                }
                Ok(Value::vec(out))
            }
            AstExpr::Set(items) => {
                let mut out = Vec::new();
                for item in items {
                    let value = self.eval_expr(item, env.clone())?;
                    if !out.iter().any(|existing| existing == &value) {
                        out.push(value);
                    }
                }
                Ok(Value::Set(out))
            }
            AstExpr::Map(entries) => {
                let mut out = BTreeMap::new();
                for (key_expr, val_expr) in entries {
                    let key_val = self.eval_expr(key_expr, env.clone())?;
                    let key = value_to_key(&key_val)?;
                    let value = self.eval_expr(val_expr, env.clone())?;
                    out.insert(key, value);
                }
                Ok(Value::map(out))
            }
            AstExpr::Quote(expr) => self.eval_quote(expr),
            AstExpr::ForeignBlock { .. } => {
                Err(Clove2Error::new("foreign block is not supported in eval"))
            }
            AstExpr::Fn { params, body, .. } => {
                let func_id = self.define_function(params, body, env);
                Ok(Value::Function(func_id))
            }
            AstExpr::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.eval_expr(cond, env.clone())?;
                if is_truthy(&cond_val) {
                    self.eval_expr(then_expr, env)
                } else if let Some(else_expr) = else_expr {
                    self.eval_expr(else_expr, env)
                } else {
                    Ok(Value::Nil)
                }
            }
            AstExpr::Let { bindings, body } => {
                let child = Env::child(env);
                for Binding { name, value, .. } in bindings {
                    let value = self.eval_expr(value, child.clone())?;
                    child.set(name, value);
                }
                let mut last = Value::Nil;
                for expr in body {
                    last = self.eval_expr(expr, child.clone())?;
                }
                Ok(last)
            }
            AstExpr::SetVar { name, value } => {
                let value = self.eval_expr(value, env)?;
                self.set_global_value(name, value.clone())?;
                Ok(value)
            }
            AstExpr::Call { callee, args } => {
                if let AstExpr::Symbol(sym) = callee.as_ref() {
                    let canonical = aliases::resolve_alias(sym);
                    if canonical == "comment" {
                        return Ok(Value::Nil);
                    }
                    if canonical == "try" {
                        return self.eval_try(args, env);
                    }
                    if canonical == "throw" {
                        return self.eval_throw(args, env);
                    }
                    if canonical == "err" || canonical == "fin" {
                        return Err(Clove2Error::new("err/fin must appear at end of try body"));
                    }
                    if matches!(canonical, "mut" | "imut" | "do") {
                        if canonical == "mut" {
                            self.push_mut_mode(MutMode::Mut);
                        } else if canonical == "imut" {
                            self.push_mut_mode(MutMode::Imut);
                        }
                        let mut last = Value::Nil;
                        for arg in args {
                            last = self.eval_expr(arg, env.clone())?;
                        }
                        if canonical == "mut" || canonical == "imut" {
                            self.pop_mut_mode();
                        }
                        return Ok(last);
                    }
                    if canonical == "and" {
                        if args.is_empty() {
                            return Ok(Value::Bool(true));
                        }
                        let mut last = Value::Nil;
                        for arg in args {
                            let value = self.eval_expr(arg, env.clone())?;
                            if !is_truthy(&value) {
                                return Ok(value);
                            }
                            last = value;
                        }
                        return Ok(last);
                    }
                    if canonical == "when" {
                        if args.is_empty() {
                            return Ok(Value::Nil);
                        }
                        let cond_val = self.eval_expr(&args[0], env.clone())?;
                        if !is_truthy(&cond_val) {
                            return Ok(Value::Nil);
                        }
                        let mut last = Value::Nil;
                        for arg in args.iter().skip(1) {
                            last = self.eval_expr(arg, env.clone())?;
                        }
                        return Ok(last);
                    }
                    if canonical == "or" {
                        if args.is_empty() {
                            return Ok(Value::Nil);
                        }
                        let mut last = Value::Nil;
                        for arg in args {
                            let value = self.eval_expr(arg, env.clone())?;
                            if is_truthy(&value) {
                                return Ok(value);
                            }
                            last = value;
                        }
                        return Ok(last);
                    }
                    if builtins::is_builtin(canonical) || is_extra_builtin(canonical) {
                        return self.eval_builtin(canonical, args, env);
                    }
                }
                let callee_val = self.eval_expr(callee, env.clone())?;
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.eval_expr(arg, env.clone())?);
                }
                self.call_value(&callee_val, arg_vals)
            }
        }
    }

    fn eval_quote(&self, expr: &Expr) -> Result<Value, Clove2Error> {
        match &expr.kind {
            crate::ast::ExprKind::Literal(lit) => Ok(match lit {
                crate::ast::Literal::Nil => Value::Nil,
                crate::ast::Literal::Bool(value) => Value::Bool(*value),
                crate::ast::Literal::Int(value) => Value::Int(*value),
                crate::ast::Literal::Float(value) => Value::Float(*value),
                crate::ast::Literal::Str(value) => Value::Str(value.clone()),
                crate::ast::Literal::Regex(value) => Value::Regex(value.clone()),
            }),
            crate::ast::ExprKind::Symbol(sym) => Ok(Value::Symbol(sym.clone())),
            crate::ast::ExprKind::Keyword(sym) => Ok(Value::Keyword(sym.clone())),
            crate::ast::ExprKind::Vector(items) => {
                let mut out = Vec::new();
                for item in items {
                    out.push(self.eval_quote(item)?);
                }
                Ok(Value::vec(out))
            }
            crate::ast::ExprKind::Set(items) => {
                let mut out = Vec::new();
                for item in items {
                    let value = self.eval_quote(item)?;
                    if !out.iter().any(|existing| existing == &value) {
                        out.push(value);
                    }
                }
                Ok(Value::Set(out))
            }
            crate::ast::ExprKind::Map(entries) => {
                let mut out = BTreeMap::new();
                for (key_expr, value_expr) in entries {
                    let key_val = self.eval_quote(key_expr)?;
                    let key = value_to_key(&key_val)?;
                    let value = self.eval_quote(value_expr)?;
                    out.insert(key, value);
                }
                Ok(Value::map(out))
            }
            crate::ast::ExprKind::List(items) => {
                let mut out = Vec::new();
                for item in items {
                    out.push(self.eval_quote(item)?);
                }
                Ok(Value::List(out))
            }
            crate::ast::ExprKind::ForeignBlock { .. } => {
                Err(Clove2Error::new("quote does not support foreign block"))
            }
        }
    }

    fn call_function(&mut self, func_id: usize, args: Vec<Value>) -> Result<Value, Clove2Error> {
        let func = self
            .functions
            .get(func_id)
            .cloned()
            .ok_or_else(|| Clove2Error::new("unknown function"))?;
        if let Some(rest_name) = &func.rest {
            if args.len() < func.params.len() {
                return Err(Clove2Error::new("function arity mismatch"));
            }
            let (fixed_args, rest_args) = args.split_at(func.params.len());
            let child = Env::child(func.env.clone());
            for (name, arg) in func.params.iter().zip(fixed_args.iter()) {
                child.set(name, arg.clone());
            }
            child.set(rest_name, Value::vec(rest_args.to_vec()));
            let mut last = Value::Nil;
            for expr in &func.body {
                last = self.eval_expr(expr, child.clone())?;
            }
            return Ok(last);
        }
        if func.params.len() != args.len() {
            return Err(Clove2Error::new("function arity mismatch"));
        }
        let child = Env::child(func.env.clone());
        for (name, arg) in func.params.iter().zip(args.into_iter()) {
            child.set(name, arg);
        }
        let mut last = Value::Nil;
        for expr in &func.body {
            last = self.eval_expr(expr, child.clone())?;
        }
        Ok(last)
    }

    fn eval_builtin(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        match name {
            "println" | "print" | "prn" => {
                let mut parts = Vec::new();
                for arg in args {
                    let value = self.eval_expr(arg, env.clone())?;
                    if name == "prn" {
                        parts.push(format_pr_value(&value));
                    } else {
                        parts.push(format_value(&value));
                    }
                }
                let out = parts.join(" ");
                if name == "print" {
                    print!("{}", out);
                } else {
                    println!("{}", out);
                }
                Ok(Value::Nil)
            }
            "pr-str" => {
                let mut parts = Vec::new();
                for arg in args {
                    let value = self.eval_expr(arg, env.clone())?;
                    parts.push(format_pr_value(&value));
                }
                Ok(Value::Str(parts.join(" ")))
            }
            "+" | "-" | "*" | "/" | "inc" | "dec" => self.eval_numeric(name, args, env),
            "mod" | "quot" | "rem" => self.eval_int_op(name, args, env),
            "bit-and" | "bit-or" | "bit-xor" | "bit-not" => self.eval_bit_op(name, args, env),
            "bit-shift-left" | "bit-shift-right" => self.eval_bit_shift(name, args, env),
            "=" | "!=" | "not=" | "<" | ">" | "<=" | ">=" => self.eval_compare(name, args, env),
            "compare" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("compare expects 2 arguments"));
                }
                let left = self.eval_expr(&args[0], env.clone())?;
                let right = self.eval_expr(&args[1], env)?;
                let ord = compare_values_ord(&left, &right);
                let out = match ord {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                };
                Ok(Value::Int(out))
            }
            "not" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("not expects 1 argument"));
                }
                let value = self.eval_expr(&args[0], env)?;
                Ok(Value::Bool(!is_truthy(&value)))
            }
            "str" => {
                let mut out = String::new();
                for arg in args {
                    let value = self.eval_expr(arg, env.clone())?;
                    out.push_str(&format_str_value(&value));
                }
                Ok(Value::Str(out))
            }
            "int" => self.eval_cast_int(args, env),
            "float" => self.eval_cast_float(args, env),
            "bool" => self.eval_cast_bool(args, env),
            "keyword" => self.eval_cast_keyword(args, env),
            "symbol" => self.eval_cast_symbol(args, env),
            "name" => self.eval_name(args, env),
            "namespace" => self.eval_namespace(args, env),
            "gensym" => self.eval_gensym(args, env),
            "get" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(Clove2Error::new("get expects 2 or 3 arguments"));
                }
                let map_val = self.eval_expr(&args[0], env.clone())?;
                let key_val = self.eval_expr(&args[1], env.clone())?;
                match map_val {
                    Value::Map(map) => {
                        let key = value_to_key(&key_val)?;
                        if let Some(value) = map.get(&key) {
                            Ok(value.clone())
                        } else if args.len() == 3 {
                            self.eval_expr(&args[2], env)
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    Value::Vec(items) => {
                        let idx = match key_val {
                            Value::Int(value) if value >= 0 => value as usize,
                            _ => return Err(Clove2Error::new("get expects Int index")),
                        };
                        if idx < items.len() {
                            Ok(items[idx].clone())
                        } else if args.len() == 3 {
                            self.eval_expr(&args[2], env)
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    _ => Err(Clove2Error::new("get expects map or vector")),
                }
            }
            "count" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("count expects 1 argument"));
                }
                let value = self.eval_expr(&args[0], env)?;
                match value {
                    Value::Vec(items) => Ok(Value::Int(items.len() as i64)),
                    Value::List(items) => Ok(Value::Int(items.len() as i64)),
                    Value::Set(items) => Ok(Value::Int(items.len() as i64)),
                    Value::Map(map) => Ok(Value::Int(map.len() as i64)),
                    Value::Str(value) => Ok(Value::Int(value.chars().count() as i64)),
                    _ => Err(Clove2Error::new("count expects collection")),
                }
            }
            "empty?" => self.eval_empty(args, env),
            "odd?" => self.eval_parity(args, env, true),
            "even?" => self.eval_parity(args, env, false),
            "zero?" => self.eval_num_pred(args, env, "zero?"),
            "pos?" => self.eval_num_pred(args, env, "pos?"),
            "neg?" => self.eval_num_pred(args, env, "neg?"),
            "nil?" => self.eval_is_nil(args, env),
            "bool?" => self.eval_is_type(args, env, ValueType::Bool, "bool?"),
            "boolean?" => self.eval_is_type(args, env, ValueType::Bool, "boolean?"),
            "int?" => self.eval_is_type(args, env, ValueType::Int, "int?"),
            "integer?" => self.eval_is_type(args, env, ValueType::Int, "integer?"),
            "float?" => self.eval_is_type(args, env, ValueType::Float, "float?"),
            "number?" => self.eval_is_type(args, env, ValueType::Number, "number?"),
            "str?" => self.eval_is_type(args, env, ValueType::Str, "str?"),
            "string?" => self.eval_is_type(args, env, ValueType::Str, "string?"),
            "keyword?" => self.eval_is_type(args, env, ValueType::Keyword, "keyword?"),
            "symbol?" => self.eval_is_type(args, env, ValueType::Symbol, "symbol?"),
            "vec?" => self.eval_is_type(args, env, ValueType::Vec, "vec?"),
            "vector?" => self.eval_is_type(args, env, ValueType::Vec, "vector?"),
            "map?" => self.eval_is_type(args, env, ValueType::Map, "map?"),
            "fn?" => self.eval_is_type(args, env, ValueType::Fn, "fn?"),
            "some?" => self.eval_some(args, env),
            "true?" => self.eval_true(args, env),
            "false?" => self.eval_false(args, env),
            "coll?" => self.eval_is_coll(args, env),
            "sequential?" => self.eval_is_sequential(args, env),
            "instance?" => self.eval_instance(args, env),
            "range" => self.eval_range(args, env),
            "rand" => self.eval_rand(args, env),
            "rand-int" => self.eval_rand_int(args, env),
            "rand-nth" => self.eval_rand_nth(args, env),
            "identity" => self.eval_identity(args, env),
            "constantly" => self.eval_constantly(args, env),
            "partial" => self.eval_partial(args, env),
            "complement" => self.eval_complement(args, env),
            "pipe" => self.eval_pipe(args, env),
            "comp" => self.eval_comp(args, env),
            "juxt" => self.eval_juxt(args, env),
            "__comp-call" => {
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr(arg, env.clone())?);
                }
                self.eval_comp_call_values(values)
            }
            "__juxt-call" => {
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr(arg, env.clone())?);
                }
                self.eval_juxt_call_values(values)
            }
            "abs" => self.eval_abs(args, env),
            "min" => self.eval_minmax("min", args, env),
            "max" => self.eval_minmax("max", args, env),
            "starts-with?" => self.eval_starts_ends_with("starts-with?", args, env),
            "ends-with?" => self.eval_starts_ends_with("ends-with?", args, env),
            "trim" => self.eval_trim(args, env),
            "triml" => self.eval_trim_start(args, env),
            "trimr" => self.eval_trim_end(args, env),
            "upper-case" => self.eval_upper_lower("upper-case", args, env),
            "lower-case" => self.eval_upper_lower("lower-case", args, env),
            "reverse-str" => self.eval_reverse_str(args, env),
            "split" => self.eval_split(args, env),
            "join" => self.eval_join(args, env),
            "blank?" => self.eval_blank(args, env),
            "format" => self.eval_format(args, env),
            "replace" => self.eval_replace(args, env, false),
            "replace-first" => self.eval_replace(args, env, true),
            "re-find" => self.eval_re_find(args, env),
            "re-matches" => self.eval_re_matches(args, env),
            "re-seq" => self.eval_re_seq(args, env),
            "split-lines" => self.eval_split_lines(args, env, false),
            "lines" => self.eval_split_lines(args, env, true),
            "index-of" => self.eval_index_of(args, env, false),
            "last-index-of" => self.eval_index_of(args, env, true),
            "capitalize" => self.eval_capitalize(args, env),
            "trim-newline" => self.eval_trim_newline(args, env),
            "subs" => self.eval_subs(args, env),
            "slurp" => self.eval_slurp(args, env),
            "spit" => self.eval_spit(args, env),
            "fs::delete" => self.eval_fs_delete(args, env),
            "time" => self.eval_time(args, env),
            "bench" => self.eval_bench(args, env),
            "runtime-error" => self.eval_runtime_error(args, env),
            "nth" => self.eval_nth(args, env),
            "seq" => self.eval_seq(args, env),
            "first" => self.eval_first(args, env),
            "second" => self.eval_second(args, env),
            "last" => self.eval_last(args, env),
            "peek" => self.eval_peek(args, env),
            "rest" => self.eval_rest(args, env),
            "map" => self.eval_map(args, env),
            "map-indexed" => self.eval_map_indexed(args, env),
            "mapcat" => self.eval_mapcat(args, env),
            "filter" => self.eval_filter(args, env),
            "remove" => self.eval_remove(args, env),
            "dorun" => self.eval_dorun(args, env),
            "keep" => self.eval_keep(args, env),
            "keep-indexed" => self.eval_keep_indexed(args, env),
            "every?" => self.eval_every(args, env),
            "not-every?" => self.eval_not_every(args, env),
            "not-any?" => self.eval_not_any(args, env),
            "some" => self.eval_some_call(args, env),
            "shuffle" => self.eval_shuffle(args, env),
            "take-while" => self.eval_take_while(args, env),
            "drop-while" => self.eval_drop_while(args, env),
            "partition" => self.eval_partition(args, env, false),
            "partition-all" => self.eval_partition(args, env, true),
            "partition-by" => self.eval_partition_by(args, env),
            "sort" => self.eval_sort(args, env),
            "sort-by" => self.eval_sort_by(args, env),
            "distinct" => self.eval_distinct(args, env),
            "dedupe" => self.eval_dedupe(args, env),
            "group-by" => self.eval_group_by(args, env),
            "zip" => self.eval_zip(args, env),
            "zip-with" => self.eval_zip_with(args, env),
            "zipmap" => self.eval_zipmap(args, env),
            "interpose" => self.eval_interpose(args, env),
            "interleave" => self.eval_interleave(args, env),
            "flatten" => self.eval_flatten(args, env),
            "reduce" => self.eval_reduce(args, env),
            "reduce-kv" => self.eval_reduce_kv(args, env),
            "apply" => self.eval_apply(args, env),
            "merge" => self.eval_merge(args, env),
            "merge-with" => self.eval_merge_with(args, env),
            "contains?" => self.eval_contains(args, env),
            "includes?" => self.eval_includes(args, env),
            "get-in" => self.eval_get_in(args, env),
            "dissoc" => self.eval_dissoc(args, env),
            "assoc-in" => self.eval_assoc_in(args, env),
            "update-in" => self.eval_update_in(args, env),
            "take" => self.eval_take(args, env),
            "drop" => self.eval_drop(args, env),
            "take-last" => self.eval_take_last(args, env),
            "drop-last" => self.eval_drop_last(args, env),
            "reverse" => self.eval_reverse(args, env),
            "subvec" => self.eval_subvec(args, env),
            "concat" => self.eval_concat(args, env),
            "conj" => self.eval_conj(args, env),
            "cons" => self.eval_cons(args, env),
            "list" => self.eval_list(args, env),
            "vector" => self.eval_vector(args, env),
            "hash-map" => self.eval_hash_map(args, env),
            "vec" => self.eval_vec(args, env),
            "into" => self.eval_into(args, env),
            "assoc" => self.eval_assoc(args, env),
            "update" => self.eval_update(args, env),
            "keys" => self.eval_keys(args, env),
            "vals" => self.eval_vals(args, env),
            "select-keys" => self.eval_select_keys(args, env),
            "frequencies" => self.eval_frequencies(args, env),
            "not-empty" => self.eval_not_empty(args, env),
            "repeat" => self.eval_repeat(args, env),
            "repeatedly" => self.eval_repeatedly(args, env),
            "iterate" => self.eval_iterate(args, env),
            "butlast" => self.eval_butlast(args, env),
            "next" => self.eval_next(args, env),
            "pop" => self.eval_pop(args, env),
            "empty" => self.eval_empty_value(args, env),
            "expect" => self.eval_expect(args, env),
            "as" => self.eval_as(args, env),
            "json::read-file" => self.eval_json_read_file(args, env),
            "json::read-string" => self.eval_json_read_string(args, env),
            "json::write-string" => self.eval_json_write_string(args, env),
            "json::write-file" => self.eval_json_write_file(args, env),
            "repl" => {
                if args.len() > 1 {
                    return Err(Clove2Error::new("repl expects 0 or 1 argument"));
                }
                if args.is_empty() {
                    Ok(Value::Nil)
                } else {
                    self.eval_expr(&args[0], env)
                }
            }
            "debug" | "break" => Ok(Value::Nil),
            _ => Err(Clove2Error::new(format!("unknown builtin: {}", name))),
        }
    }

    fn eval_numeric(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return match name {
                "+" => Ok(Value::Int(0)),
                "*" => Ok(Value::Int(1)),
                "-" => Ok(Value::Int(0)),
                "/" => Ok(Value::Int(1)),
                _ => Err(Clove2Error::new("numeric op expects arguments")),
            };
        }
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        let has_float = values.iter().any(|v| matches!(v, Value::Float(_)));
        if name == "inc" || name == "dec" {
            if values.len() != 1 {
                return Err(Clove2Error::new("inc/dec expects 1 argument"));
            }
        }
        let mut acc = match values.first().unwrap() {
            Value::Int(v) => *v as f64,
            Value::Float(v) => *v,
            other => {
                return Err(Clove2Error::new(format!(
                    "numeric op expects number, got {}",
                    format_value(other)
                )))
            }
        };
        if name == "inc" {
            acc += 1.0;
        } else if name == "dec" {
            acc -= 1.0;
        } else if values.len() == 1 {
            match name {
                "-" => acc = -acc,
                "/" => acc = 1.0 / acc,
                _ => {}
            }
        } else {
            for value in values.iter().skip(1) {
                let next = match value {
                    Value::Int(v) => *v as f64,
                    Value::Float(v) => *v,
                    other => {
                        return Err(Clove2Error::new(format!(
                            "numeric op expects number, got {}",
                            format_value(other)
                        )))
                    }
                };
                match name {
                    "+" => acc += next,
                    "-" => acc -= next,
                    "*" => acc *= next,
                    "/" => acc /= next,
                    _ => {}
                }
            }
        }
        if name == "/" {
            if has_float {
                Ok(Value::Float(acc))
            } else if acc.fract() == 0.0 {
                Ok(Value::Int(acc as i64))
            } else {
                Ok(Value::Float(acc))
            }
        } else if has_float {
            Ok(Value::Float(acc))
        } else {
            Ok(Value::Int(acc as i64))
        }
    }

    fn eval_bit_op(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_bit_op_values(name, values)
    }

    fn eval_bit_shift(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_bit_shift_values(name, values)
    }

    fn eval_int_op(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new(format!("{} expects 2 arguments", name)));
        }
        let left = self.eval_expr(&args[0], env.clone())?;
        let right = self.eval_expr(&args[1], env)?;
        self.eval_int_op_values(name, vec![left, right])
    }

    fn eval_abs(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("abs expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Int(value) => Ok(Value::Int(value.abs())),
            Value::Float(value) => Ok(Value::Float(value.abs())),
            _ => Err(Clove2Error::new("abs expects number")),
        }
    }

    fn eval_minmax(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new(format!(
                "{} expects at least 1 argument",
                name
            )));
        }
        let mut values = Vec::new();
        let mut has_float = false;
        for arg in args {
            let value = self.eval_expr(arg, env.clone())?;
            match value {
                Value::Int(value) => values.push(value as f64),
                Value::Float(value) => {
                    has_float = true;
                    values.push(value);
                }
                _ => {
                    return Err(Clove2Error::new(format!(
                        "{} expects numeric arguments",
                        name
                    )))
                }
            }
        }
        let mut iter = values.into_iter();
        let Some(first) = iter.next() else {
            return Err(Clove2Error::new(format!(
                "{} expects at least 1 argument",
                name
            )));
        };
        let mut out = first;
        for value in iter {
            if name == "min" {
                if value < out {
                    out = value;
                }
            } else if value > out {
                out = value;
            }
        }
        if has_float {
            Ok(Value::Float(out))
        } else {
            Ok(Value::Int(out as i64))
        }
    }

    fn eval_starts_ends_with(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new(format!("{} expects 2 arguments", name)));
        }
        let text = self.eval_expr(&args[0], env.clone())?;
        let prefix = self.eval_expr(&args[1], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let Value::Str(prefix) = prefix else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let ok = if name == "starts-with?" {
            text.starts_with(&prefix)
        } else {
            text.ends_with(&prefix)
        };
        Ok(Value::Bool(ok))
    }

    fn eval_trim(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("trim expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new("trim expects string"));
        };
        Ok(Value::Str(value.trim().to_string()))
    }

    fn eval_trim_start(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("triml expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new("triml expects string"));
        };
        Ok(Value::Str(value.trim_start().to_string()))
    }

    fn eval_trim_end(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("trimr expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new("trimr expects string"));
        };
        Ok(Value::Str(value.trim_end().to_string()))
    }

    fn eval_upper_lower(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let out = if name == "upper-case" {
            value.to_uppercase()
        } else {
            value.to_lowercase()
        };
        Ok(Value::Str(out))
    }

    fn eval_reverse_str(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("reverse-str expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new("reverse-str expects string"));
        };
        Ok(Value::Str(value.chars().rev().collect()))
    }

    fn eval_split(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("split expects 2 or 3 arguments"));
        }
        let text = self.eval_expr(&args[0], env.clone())?;
        let sep = self.eval_expr(&args[1], env.clone())?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("split expects string"));
        };
        let limit = if args.len() == 3 {
            let limit_val = self.eval_expr(&args[2], env)?;
            let Value::Int(limit) = limit_val else {
                return Err(Clove2Error::new("split expects Int limit"));
            };
            if limit > 0 {
                Some(limit as usize)
            } else {
                None
            }
        } else {
            None
        };
        let items = split_text(&text, &sep, limit)?;
        Ok(Value::vec(items))
    }

    fn eval_join(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("join expects 1 or 2 arguments"));
        }
        let (coll_expr, sep) = if args.len() == 2 {
            let sep_val = self.eval_expr(&args[1], env.clone())?;
            let Value::Str(sep) = sep_val else {
                return Err(Clove2Error::new("join expects string separator"));
            };
            (&args[0], sep)
        } else {
            (&args[0], String::new())
        };
        let coll = self.eval_expr(coll_expr, env)?;
        let items = match &coll {
            Value::Vec(items) => items.as_ref(),
            Value::List(items) => items.as_slice(),
            _ => return Err(Clove2Error::new("join expects vector of strings")),
        };
        let mut out = String::new();
        for (idx, item) in items.iter().enumerate() {
            let Value::Str(item) = item else {
                return Err(Clove2Error::new("join expects vector of strings"));
            };
            if idx > 0 {
                out.push_str(&sep);
            }
            out.push_str(item);
        }
        Ok(Value::Str(out))
    }

    fn eval_format(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("format expects at least 1 argument"));
        }
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_format_values(values)
    }

    fn eval_blank(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("blank? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let Value::Str(value) = value else {
            return Err(Clove2Error::new("blank? expects string"));
        };
        Ok(Value::Bool(value.trim().is_empty()))
    }

    fn eval_replace(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        first_only: bool,
    ) -> Result<Value, Clove2Error> {
        if first_only {
            if args.len() != 3 {
                return Err(Clove2Error::new("replace-first expects 3 arguments"));
            }
            let text = self.eval_expr(&args[0], env.clone())?;
            let from = self.eval_expr(&args[1], env.clone())?;
            let to = self.eval_expr(&args[2], env)?;
            let Value::Str(text) = text else {
                return Err(Clove2Error::new("replace-first expects string"));
            };
            let Value::Str(to) = to else {
                return Err(Clove2Error::new("replace-first expects string replacement"));
            };
            let out = replace_in_text(&text, &from, &to, true)?;
            return Ok(Value::Str(out));
        }

        if args.len() == 2 {
            let smap = self.eval_expr(&args[0], env.clone())?;
            let coll = self.eval_expr(&args[1], env)?;
            return replace_collection(&smap, &coll);
        }
        if args.len() == 3 {
            let text = self.eval_expr(&args[0], env.clone())?;
            let from = self.eval_expr(&args[1], env.clone())?;
            let to = self.eval_expr(&args[2], env)?;
            let Value::Str(text) = text else {
                return Err(Clove2Error::new("replace expects string"));
            };
            let Value::Str(to) = to else {
                return Err(Clove2Error::new("replace expects string replacement"));
            };
            let out = replace_in_text(&text, &from, &to, false)?;
            return Ok(Value::Str(out));
        }
        Err(Clove2Error::new("replace expects 2 or 3 arguments"))
    }

    fn eval_re_find(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-find expects 2 arguments"));
        }
        let pattern = self.eval_expr(&args[0], env.clone())?;
        let text = self.eval_expr(&args[1], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("re-find expects string"));
        };
        let regex = resolve_regex_value(&pattern)?;
        Ok(regex
            .captures(&text)
            .map(|caps| capture_to_value(&caps))
            .unwrap_or(Value::Nil))
    }

    fn eval_re_matches(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-matches expects 2 arguments"));
        }
        let pattern = self.eval_expr(&args[0], env.clone())?;
        let text = self.eval_expr(&args[1], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("re-matches expects string"));
        };
        let regex = resolve_regex_value(&pattern)?;
        let matched = regex.captures(&text).and_then(|caps| {
            caps.get(0).and_then(|full| {
                if full.start() == 0 && full.end() == text.len() {
                    Some(capture_to_value(&caps))
                } else {
                    None
                }
            })
        });
        Ok(matched.unwrap_or(Value::Nil))
    }

    fn eval_re_seq(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-seq expects 2 arguments"));
        }
        let pattern = self.eval_expr(&args[0], env.clone())?;
        let text = self.eval_expr(&args[1], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("re-seq expects string"));
        };
        let regex = resolve_regex_value(&pattern)?;
        let mut items = Vec::new();
        for caps in regex.captures_iter(&text) {
            items.push(capture_to_value(&caps));
        }
        Ok(Value::list(items))
    }

    fn eval_split_lines(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        keep_ends: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("split-lines expects 1 argument"));
        }
        let text = self.eval_expr(&args[0], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("split-lines expects string"));
        };
        let items = if keep_ends {
            split_lines_keep_ends(&text)
        } else {
            text.lines()
                .map(|line| Value::Str(line.to_string()))
                .collect()
        };
        Ok(Value::vec(items))
    }

    fn eval_index_of(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        last: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("index-of expects 2 or 3 arguments"));
        }
        let text = self.eval_expr(&args[0], env.clone())?;
        let needle = self.eval_expr(&args[1], env.clone())?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("index-of expects string"));
        };
        let Value::Str(needle) = needle else {
            return Err(Clove2Error::new("index-of expects string needle"));
        };
        let len = text.chars().count();
        let out = if last {
            let end = if args.len() == 3 {
                let end_val = self.eval_expr(&args[2], env)?;
                let Value::Int(end) = end_val else {
                    return Err(Clove2Error::new("last-index-of expects Int end"));
                };
                if end < 0 {
                    return Err(Clove2Error::new("last-index-of expects non-negative end"));
                }
                end as usize
            } else {
                len
            };
            if end == 0 {
                None
            } else {
                let end = end.min(len);
                let end_byte = char_byte_index(&text, end);
                let slice = &text[..end_byte];
                slice.rfind(&needle).map(|idx| {
                    let prefix = &text[..idx];
                    prefix.chars().count()
                })
            }
        } else {
            let start = if args.len() == 3 {
                let start_val = self.eval_expr(&args[2], env)?;
                let Value::Int(start) = start_val else {
                    return Err(Clove2Error::new("index-of expects Int start"));
                };
                if start < 0 {
                    return Err(Clove2Error::new("index-of expects non-negative start"));
                }
                start as usize
            } else {
                0
            };
            if start > len {
                return Ok(Value::Nil);
            }
            let start_byte = char_byte_index(&text, start);
            let slice = &text[start_byte..];
            slice.find(&needle).map(|idx| {
                let prefix = &text[..start_byte + idx];
                prefix.chars().count()
            })
        };
        match out {
            Some(idx) => Ok(Value::Int(idx as i64)),
            None => Ok(Value::Nil),
        }
    }

    fn eval_capitalize(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("capitalize expects 1 argument"));
        }
        let text = self.eval_expr(&args[0], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("capitalize expects string"));
        };
        let mut chars = text.chars();
        let Some(first) = chars.next() else {
            return Ok(Value::Str(text));
        };
        let mut out = String::new();
        out.extend(first.to_uppercase());
        let rest: String = chars.collect();
        out.push_str(&rest.to_lowercase());
        Ok(Value::Str(out))
    }

    fn eval_trim_newline(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("trim-newline expects 1 argument"));
        }
        let text = self.eval_expr(&args[0], env)?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("trim-newline expects string"));
        };
        let out = text.trim_end_matches(&['\n', '\r'][..]).to_string();
        Ok(Value::Str(out))
    }

    fn eval_subs(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("subs expects 2 or 3 arguments"));
        }
        let text = self.eval_expr(&args[0], env.clone())?;
        let start = self.eval_expr(&args[1], env.clone())?;
        let Value::Str(text) = text else {
            return Err(Clove2Error::new("subs expects string"));
        };
        let Value::Int(start) = start else {
            return Err(Clove2Error::new("subs expects int start"));
        };
        let end = if args.len() == 3 {
            let end = self.eval_expr(&args[2], env)?;
            let Value::Int(end) = end else {
                return Err(Clove2Error::new("subs expects int end"));
            };
            end
        } else {
            text.chars().count() as i64
        };
        if start < 0 || end < 0 {
            return Err(Clove2Error::new("subs expects non-negative indices"));
        }
        let len = text.chars().count() as i64;
        if start > end || end > len {
            return Err(Clove2Error::new("subs index out of range"));
        }
        let start_idx = char_byte_index(&text, start as usize);
        let end_idx = char_byte_index(&text, end as usize);
        Ok(Value::Str(text[start_idx..end_idx].to_string()))
    }

    fn eval_slurp(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("slurp expects 1 argument"));
        }
        let path_val = self.eval_expr(&args[0], env)?;
        let Value::Str(path) = path_val else {
            return Err(Clove2Error::new("slurp expects string path"));
        };
        let resolved = normalize_path(&path);
        let content = fs::read_to_string(&resolved)
            .map_err(|err| Clove2Error::new(format!("failed to read {}: {}", resolved, err)))?;
        Ok(Value::Str(content))
    }

    fn eval_spit(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("spit expects 2 arguments"));
        }
        let path_val = self.eval_expr(&args[0], env.clone())?;
        let content_val = self.eval_expr(&args[1], env)?;
        let Value::Str(path) = path_val else {
            return Err(Clove2Error::new("spit expects string path"));
        };
        let Value::Str(content) = content_val else {
            return Err(Clove2Error::new("spit expects string content"));
        };
        let resolved = normalize_path(&path);
        fs::write(&resolved, content)
            .map_err(|err| Clove2Error::new(format!("failed to write {}: {}", resolved, err)))?;
        Ok(Value::Str(path))
    }

    fn eval_fs_delete(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("fs::delete expects 1 argument"));
        }
        let path_val = self.eval_expr(&args[0], env)?;
        let Value::Str(path) = path_val else {
            return Err(Clove2Error::new("fs::delete expects string path"));
        };
        let resolved = normalize_path(&path);
        remove_path(&resolved)?;
        Ok(Value::Str(path))
    }

    fn eval_time(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("time expects 1 argument"));
        }
        let func = self.eval_expr(&args[0], env)?;
        self.eval_time_values(vec![func])
    }

    fn eval_throw(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("throw expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Err(Clove2Error::thrown(value))
    }

    fn eval_try(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("try expects body"));
        }
        let mut idx = 0;
        let mut local_env = env.clone();
        if let Some(AstExpr::Vector(items)) = args.get(0) {
            let bindings = parse_try_bindings(items)?;
            let child = Env::child(env.clone());
            for (name, value_expr) in bindings {
                let value = self.eval_expr(&value_expr, child.clone())?;
                child.set(&name, value);
            }
            local_env = child;
            idx = 1;
        }
        let mut body: Vec<AstExpr> = Vec::new();
        let mut catches: Vec<TryCatchClause> = Vec::new();
        let mut err_clause: Option<TryCatchClause> = None;
        let mut finally_body: Option<Vec<AstExpr>> = None;
        for expr in args.iter().skip(idx) {
            if let Some(catch) = parse_try_catch_clause(expr)? {
                catches.push(catch);
                continue;
            }
            if let Some(err) = parse_try_err_clause(expr)? {
                err_clause = Some(err);
                continue;
            }
            if let Some(fin) = parse_try_finally_clause(expr)? {
                finally_body = Some(fin);
                continue;
            }
            body.push(expr.clone());
        }
        let mut on_error: Option<AstExpr> = None;
        let mut on_finally: Option<AstExpr> = None;
        if catches.is_empty() && err_clause.is_none() && finally_body.is_none() {
            if body.len() >= 2 && is_try_handler_expr(body.last().unwrap()) {
                if is_try_handler_expr(&body[body.len() - 2]) {
                    on_finally = body.pop();
                    on_error = body.pop();
                } else {
                    on_error = body.pop();
                }
            }
        }
        if body.is_empty() {
            return Err(Clove2Error::new("try expects body"));
        }
        let result = eval_body(self, &body, local_env.clone());
        let out = match result {
            Ok(value) => Ok(value),
            Err(err) => {
                let err_value = err
                    .thrown_value()
                    .cloned()
                    .unwrap_or_else(|| Value::Str(err.to_string()));
                if let Some(clause) = catches.first() {
                    let child = Env::child(local_env.clone());
                    child.set(&clause.name, err_value);
                    eval_body(self, &clause.body, child)
                } else if let Some(clause) = err_clause {
                    let child = Env::child(local_env.clone());
                    child.set(&clause.name, err_value);
                    eval_body(self, &clause.body, child)
                } else if let Some(handler) = on_error {
                    let handler_value = self.eval_expr(&handler, local_env.clone())?;
                    self.call_value(&handler_value, vec![err_value])
                } else {
                    Err(err)
                }
            }
        };
        if let Some(fin) = finally_body {
            let _ = eval_body(self, &fin, local_env)?;
        } else if let Some(handler) = on_finally {
            let handler_value = self.eval_expr(&handler, local_env.clone())?;
            let _ = self.call_value(&handler_value, Vec::new())?;
        }
        out
    }

    fn eval_bench(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("bench expects 2 arguments"));
        }
        let iterations = self.eval_expr(&args[0], env.clone())?;
        let func = self.eval_expr(&args[1], env)?;
        self.eval_bench_values(vec![iterations, func])
    }

    fn eval_runtime_error(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new(
                "runtime-error expects at least 1 argument",
            ));
        }
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_runtime_error_values(values)
    }

    fn eval_time_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("time expects 1 argument"));
        }
        let func = args[0].clone();
        let start = Instant::now();
        let value = self.call_value(&func, Vec::new())?;
        let elapsed = start.elapsed();
        Ok(Value::map(measurement_result(value, 1, elapsed)))
    }

    fn eval_bench_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("bench expects 2 arguments"));
        }
        let Value::Int(runs) = args[0] else {
            return Err(Clove2Error::new("bench expects Int iterations"));
        };
        if runs <= 0 {
            return Err(Clove2Error::new("bench expects positive iterations"));
        }
        let func = args[1].clone();
        let start = Instant::now();
        let mut last = Value::Nil;
        for _ in 0..runs {
            last = self.call_value(&func, Vec::new())?;
        }
        let elapsed = start.elapsed();
        Ok(Value::map(measurement_result(last, runs, elapsed)))
    }

    fn eval_runtime_error_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new(
                "runtime-error expects at least 1 argument",
            ));
        }
        let parts: Vec<String> = args
            .iter()
            .map(|value| match value {
                Value::Str(text) => text.clone(),
                other => format_value(other),
            })
            .collect();
        Err(Clove2Error::thrown(Value::Str(parts.join(" "))))
    }

    fn eval_compare(
        &mut self,
        name: &str,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Ok(Value::Bool(true));
        }
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        let mut ok = true;
        for pair in values.windows(2) {
            let left = &pair[0];
            let right = &pair[1];
            ok &= compare_values(name, left, right)?;
        }
        Ok(Value::Bool(ok))
    }

    fn eval_cast_int(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("int expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Int(value) => Ok(Value::Int(value)),
            Value::Float(value) => Ok(Value::Int(value as i64)),
            Value::Str(value) => value
                .trim()
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| Clove2Error::new("int parse failed")),
            _ => Err(Clove2Error::new("int expects number or string")),
        }
    }

    fn eval_cast_float(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("float expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Int(value) => Ok(Value::Float(value as f64)),
            Value::Float(value) => Ok(Value::Float(value)),
            Value::Str(value) => value
                .trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| Clove2Error::new("float parse failed")),
            _ => Err(Clove2Error::new("float expects number or string")),
        }
    }

    fn eval_cast_bool(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("bool expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Bool(value) => Ok(Value::Bool(value)),
            Value::Str(value) => match value.trim() {
                "true" => Ok(Value::Bool(true)),
                "false" => Ok(Value::Bool(false)),
                _ => Err(Clove2Error::new("bool parse failed")),
            },
            _ => Err(Clove2Error::new("bool expects bool or string")),
        }
    }

    fn eval_cast_keyword(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("keyword expects 1 or 2 arguments"));
        }
        if args.len() == 2 {
            let ns_val = self.eval_expr(&args[0], env.clone())?;
            let name_val = self.eval_expr(&args[1], env)?;
            let ns = keyword_part(&ns_val)?;
            let name = keyword_part(&name_val)?;
            return Ok(Value::Keyword(format!("{}::{}", ns, name)));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Keyword(value) => Ok(Value::Keyword(value)),
            Value::Str(value) => Ok(Value::Keyword(value)),
            Value::Symbol(value) => Ok(Value::Keyword(value)),
            _ => Err(Clove2Error::new(
                "keyword expects string, symbol, or keyword",
            )),
        }
    }

    fn eval_empty(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("empty? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Vec(items) => Ok(Value::Bool(items.is_empty())),
            Value::List(items) => Ok(Value::Bool(items.is_empty())),
            Value::Set(items) => Ok(Value::Bool(items.is_empty())),
            Value::Map(map) => Ok(Value::Bool(map.is_empty())),
            Value::Str(value) => Ok(Value::Bool(value.is_empty())),
            _ => Err(Clove2Error::new("empty? expects collection")),
        }
    }

    fn eval_not_empty(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("not-empty expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Vec(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Vec(items))
                }
            }
            Value::List(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items))
                }
            }
            Value::Set(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Set(items))
                }
            }
            Value::Map(map) => {
                if map.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Map(map))
                }
            }
            Value::Str(value) => {
                if value.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Str(value))
                }
            }
            _ => Err(Clove2Error::new("not-empty expects collection")),
        }
    }

    fn eval_parity(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        odd: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("odd?/even? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Int(value) => Ok(Value::Bool((value % 2 != 0) == odd)),
            _ => Err(Clove2Error::new("odd?/even? expects int")),
        }
    }

    fn eval_num_pred(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        name: &str,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let value = self.eval_expr(&args[0], env)?;
        let number = match value {
            Value::Int(value) => value as f64,
            Value::Float(value) => value,
            _ => return Err(Clove2Error::new(format!("{} expects number", name))),
        };
        let ok = match name {
            "zero?" => number == 0.0,
            "pos?" => number > 0.0,
            "neg?" => number < 0.0,
            _ => false,
        };
        Ok(Value::Bool(ok))
    }

    fn eval_is_nil(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("nil? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(matches!(value, Value::Nil)))
    }

    fn eval_is_type(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        ty: ValueType,
        name: &str,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(match ty {
            ValueType::Bool => matches!(value, Value::Bool(_)),
            ValueType::Int => matches!(value, Value::Int(_)),
            ValueType::Float => matches!(value, Value::Float(_)),
            ValueType::Number => matches!(value, Value::Int(_) | Value::Float(_)),
            ValueType::Str => matches!(value, Value::Str(_)),
            ValueType::Keyword => matches!(value, Value::Keyword(_)),
            ValueType::Symbol => matches!(value, Value::Symbol(_)),
            ValueType::Vec => matches!(value, Value::Vec(_)),
            ValueType::Map => matches!(value, Value::Map(_)),
            ValueType::Fn => {
                matches!(
                    value,
                    Value::Function(_)
                        | Value::NativeFunction(_)
                        | Value::Builtin(_)
                        | Value::Partial(_)
                )
            }
        }))
    }

    fn eval_is_coll(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("coll? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(matches!(
            value,
            Value::Vec(_) | Value::List(_) | Value::Set(_) | Value::Map(_) | Value::Str(_)
        )))
    }

    fn eval_is_sequential(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("sequential? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(matches!(value, Value::Vec(_) | Value::List(_))))
    }

    fn eval_instance(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("instance? expects 2 arguments"));
        }
        let type_val = self.eval_expr(&args[0], env.clone())?;
        let value = self.eval_expr(&args[1], env)?;
        let type_name = normalize_type_name(&type_val)?;
        Ok(Value::Bool(matches_type_name(&type_name, &value)))
    }

    fn eval_some(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("some? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(!matches!(value, Value::Nil)))
    }

    fn eval_true(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("true? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(matches!(value, Value::Bool(true))))
    }

    fn eval_false(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("false? expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(Value::Bool(matches!(value, Value::Bool(false))))
    }

    fn eval_cast_symbol(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("symbol expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Symbol(value) => Ok(Value::Symbol(value)),
            Value::Str(value) => Ok(Value::Symbol(value)),
            Value::Keyword(value) => Ok(Value::Symbol(value)),
            _ => Err(Clove2Error::new(
                "symbol expects string, symbol, or keyword",
            )),
        }
    }

    fn eval_name(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("name expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let name = name_from_value(&value)?;
        Ok(Value::Str(name))
    }

    fn eval_namespace(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("namespace expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let namespace = namespace_from_value(&value)?;
        Ok(match namespace {
            Some(value) => Value::Str(value),
            None => Value::Nil,
        })
    }

    fn eval_gensym(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_gensym_values(values)
    }

    fn eval_nth(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 2 || args.len() > 3 {
            return Err(Clove2Error::new("nth expects 2 or 3 arguments"));
        }
        let coll_val = self.eval_expr(&args[0], env.clone())?;
        let idx_val = self.eval_expr(&args[1], env.clone())?;
        let default_val = if args.len() == 3 {
            Some(self.eval_expr(&args[2], env)?)
        } else {
            None
        };
        let items = values_to_vec_items(&coll_val, "nth")?;
        match idx_val {
            Value::Int(value) if value >= 0 => {
                let idx = value as usize;
                if idx < items.len() {
                    Ok(items[idx].clone())
                } else {
                    Ok(default_val.unwrap_or(Value::Nil))
                }
            }
            Value::Vec(indices) => {
                let mut out = Vec::with_capacity(indices.len());
                for idx_val in indices.iter() {
                    let Value::Int(value) = idx_val else {
                        return Err(Clove2Error::new("nth expects non-negative Int index"));
                    };
                    if *value < 0 {
                        return Err(Clove2Error::new("nth expects non-negative Int index"));
                    }
                    let idx = *value as usize;
                    if idx < items.len() {
                        out.push(items[idx].clone());
                    } else {
                        out.push(default_val.clone().unwrap_or(Value::Nil));
                    }
                }
                Ok(Value::vec(out))
            }
            Value::List(indices) => {
                let mut out = Vec::with_capacity(indices.len());
                for idx_val in indices.iter() {
                    let Value::Int(value) = idx_val else {
                        return Err(Clove2Error::new("nth expects non-negative Int index"));
                    };
                    if *value < 0 {
                        return Err(Clove2Error::new("nth expects non-negative Int index"));
                    }
                    let idx = *value as usize;
                    if idx < items.len() {
                        out.push(items[idx].clone());
                    } else {
                        out.push(default_val.clone().unwrap_or(Value::Nil));
                    }
                }
                Ok(Value::vec(out))
            }
            _ => Err(Clove2Error::new("nth expects non-negative Int index")),
        }
    }

    fn eval_range(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_range_values(values)
    }

    fn eval_rand(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_rand_values(values)
    }

    fn eval_rand_int(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_rand_int_values(values)
    }

    fn eval_rand_nth(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::new();
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_rand_nth_values(values)
    }

    fn eval_seq(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("seq expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Nil => Ok(Value::Nil),
            Value::Vec(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items.as_ref().clone()))
                }
            }
            Value::List(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items))
                }
            }
            Value::Set(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items))
                }
            }
            Value::Str(text) => {
                if text.is_empty() {
                    Ok(Value::Nil)
                } else {
                    let items = text.chars().map(|ch| Value::Str(ch.to_string())).collect();
                    Ok(Value::List(items))
                }
            }
            Value::Map(map) => {
                if map.is_empty() {
                    Ok(Value::Nil)
                } else {
                    let mut out = Vec::with_capacity(map.len());
                    for (key, value) in map.iter() {
                        out.push(Value::vec(vec![key_to_value(key), value.clone()]));
                    }
                    Ok(Value::List(out))
                }
            }
            _ => Err(Clove2Error::new("seq expects collection")),
        }
    }

    fn eval_identity(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("identity expects 1 argument"));
        }
        self.eval_expr(&args[0], env)
    }

    fn eval_constantly(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("constantly expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        Ok(self.make_constantly(value))
    }

    fn eval_partial(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("partial expects at least 1 argument"));
        }
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_partial_values(values)
    }

    fn eval_complement(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("complement expects 1 argument"));
        }
        let func = self.eval_expr(&args[0], env)?;
        Ok(self.make_complement(func))
    }

    fn eval_comp(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut funcs = Vec::with_capacity(args.len());
        for arg in args {
            funcs.push(self.eval_expr(arg, env.clone())?);
        }
        Ok(self.make_comp(funcs))
    }

    fn eval_pipe(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut funcs = Vec::with_capacity(args.len());
        for arg in args {
            funcs.push(self.eval_expr(arg, env.clone())?);
        }
        Ok(self.make_pipe(funcs))
    }

    fn eval_juxt(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut funcs = Vec::with_capacity(args.len());
        for arg in args {
            funcs.push(self.eval_expr(arg, env.clone())?);
        }
        Ok(self.make_juxt(funcs))
    }

    fn eval_first(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("first expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => Ok(items.first().cloned().unwrap_or(Value::Nil)),
            Value::List(items) => Ok(items.first().cloned().unwrap_or(Value::Nil)),
            Value::Set(items) => Ok(items.first().cloned().unwrap_or(Value::Nil)),
            _ => Err(Clove2Error::new("first expects collection")),
        }
    }

    fn eval_second(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("second expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        let items =
            vec_items_ref(&vec_val).ok_or_else(|| Clove2Error::new("second expects collection"))?;
        Ok(items.get(1).cloned().unwrap_or(Value::Nil))
    }

    fn eval_last(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("last expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
            Value::List(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
            Value::Set(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
            _ => Err(Clove2Error::new("last expects collection")),
        }
    }

    fn eval_peek(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("peek expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
            Value::List(items) => Ok(items.first().cloned().unwrap_or(Value::Nil)),
            Value::Set(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
            _ => Err(Clove2Error::new("peek expects collection")),
        }
    }

    fn eval_rest(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("rest expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => {
                let rest = if items.len() <= 1 {
                    Vec::new()
                } else {
                    items[1..].to_vec()
                };
                Ok(Value::vec(rest))
            }
            Value::List(items) => {
                let rest = if items.len() <= 1 {
                    Vec::new()
                } else {
                    items[1..].to_vec()
                };
                Ok(Value::List(rest))
            }
            _ => Err(Clove2Error::new("rest expects collection")),
        }
    }

    fn eval_binding_sugar(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        name: &str,
    ) -> Option<Result<(Value, Value), Clove2Error>> {
        if args.len() != 2 {
            return None;
        }
        let AstExpr::Vector(bindings) = &args[0] else {
            return None;
        };
        if bindings.len() != 2 {
            return Some(Err(Clove2Error::new(format!(
                "{} binding form expects [name coll]",
                name
            ))));
        }
        let AstExpr::Symbol(name_sym) = &bindings[0] else {
            return Some(Err(Clove2Error::new(format!(
                "{} binding form expects symbol name",
                name
            ))));
        };
        let param = Param {
            name: name_sym.clone(),
            ty: None,
            rest: false,
        };
        let body = vec![args[1].clone()];
        let func_id = self.define_function(&[param], &body, env.clone());
        let func = Value::Function(func_id);
        let coll = self.eval_expr(&bindings[1], env);
        Some(coll.map(|coll| (func, coll)))
    }

    fn eval_map(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "map") {
            let (func, coll) = result?;
            return self.eval_map_values(vec![func, coll]);
        }
        if args.len() < 2 {
            return Err(Clove2Error::new("map expects at least 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let mut colls = Vec::with_capacity(args.len() - 1);
        for arg in &args[1..] {
            colls.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_map_multi(func, colls)
    }

    fn eval_map_multi(&mut self, func: Value, colls: Vec<Value>) -> Result<Value, Clove2Error> {
        if colls.is_empty() {
            return Err(Clove2Error::new("map expects collection arguments"));
        }
        let mut coll_items = Vec::new();
        let mut min_len = usize::MAX;
        for coll in &colls {
            let items = values_to_vec_items(coll, "map")?;
            min_len = min_len.min(items.len());
            coll_items.push(items);
        }
        if min_len == usize::MAX {
            min_len = 0;
        }
        let mut out = Vec::with_capacity(min_len);
        for idx in 0..min_len {
            let mut args = Vec::with_capacity(colls.len());
            for items in &coll_items {
                args.push(items[idx].clone());
            }
            out.push(self.call_value(&func, args)?);
        }
        Ok(Value::vec(out))
    }

    fn comparator_order(
        &mut self,
        cmp: &Value,
        left: &Value,
        right: &Value,
    ) -> Result<std::cmp::Ordering, Clove2Error> {
        use std::cmp::Ordering;
        let result = self.call_value(cmp, vec![left.clone(), right.clone()])?;
        match result {
            Value::Bool(true) => Ok(Ordering::Less),
            Value::Bool(false) => {
                let reverse = self.call_value(cmp, vec![right.clone(), left.clone()])?;
                if matches!(reverse, Value::Bool(true)) {
                    Ok(Ordering::Greater)
                } else {
                    Ok(Ordering::Equal)
                }
            }
            Value::Int(value) => Ok(value.cmp(&0)),
            Value::Float(value) => {
                if value < 0.0 {
                    Ok(Ordering::Less)
                } else if value > 0.0 {
                    Ok(Ordering::Greater)
                } else {
                    Ok(Ordering::Equal)
                }
            }
            _ => Err(Clove2Error::new("sort comparator expects bool or number")),
        }
    }

    fn sort_with_cmp(&mut self, items: &mut Vec<Value>, cmp: &Value) -> Result<(), Clove2Error> {
        for i in 1..items.len() {
            let mut j = i;
            while j > 0 {
                let ord = self.comparator_order(cmp, &items[j - 1], &items[j])?;
                if ord == std::cmp::Ordering::Greater {
                    items.swap(j - 1, j);
                    j -= 1;
                } else {
                    break;
                }
            }
        }
        Ok(())
    }

    fn eval_map_indexed(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("map-indexed expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "map-indexed")?;
        let mut out = Vec::new();
        for (idx, item) in items.into_iter().enumerate() {
            let idx_val = Value::Int(idx as i64);
            out.push(self.call_value(&func, vec![idx_val, item])?);
        }
        Ok(Value::vec(out))
    }

    fn eval_mapcat(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "mapcat") {
            let (func, coll) = result?;
            return self.eval_mapcat_values(vec![func, coll]);
        }
        if args.len() < 2 {
            return Err(Clove2Error::new("mapcat expects at least 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let mut colls = Vec::with_capacity(args.len() - 1);
        for arg in &args[1..] {
            colls.push(self.eval_expr(arg, env.clone())?);
        }
        let mut coll_items = Vec::new();
        let mut min_len = usize::MAX;
        for coll in &colls {
            let items = values_to_vec_items(coll, "mapcat")?;
            min_len = min_len.min(items.len());
            coll_items.push(items);
        }
        if min_len == usize::MAX {
            min_len = 0;
        }
        let mut out = Vec::new();
        for idx in 0..min_len {
            let mut call_args = Vec::with_capacity(colls.len());
            for items in &coll_items {
                call_args.push(items[idx].clone());
            }
            let mapped = self.call_value(&func, call_args)?;
            let values = values_to_vec_items(&mapped, "mapcat")?;
            out.extend(values);
        }
        Ok(Value::vec(out))
    }

    fn eval_filter(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "filter") {
            let (func, coll) = result?;
            return self.eval_filter_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("filter expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "filter")?;
        let mut out = Vec::new();
        for item in items {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if is_truthy(&keep) {
                out.push(item);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_remove(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "remove") {
            let (func, coll) = result?;
            return self.eval_remove_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("remove expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        self.eval_remove_values(vec![func, vec_val])
    }

    fn eval_dorun(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_dorun_values(values)
    }

    fn eval_keep(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "keep") {
            let (func, coll) = result?;
            return self.eval_keep_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("keep expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "keep")?;
        let mut out = Vec::new();
        for item in items {
            let value = self.call_value(&func, vec![item])?;
            if !matches!(value, Value::Nil) {
                out.push(value);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_keep_indexed(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("keep-indexed expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "keep-indexed")?;
        let mut out = Vec::new();
        for (idx, item) in items.into_iter().enumerate() {
            let value = self.call_value(&func, vec![Value::Int(idx as i64), item])?;
            if !matches!(value, Value::Nil) {
                out.push(value);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_every(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "every?") {
            let (func, coll) = result?;
            return self.eval_every_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("every? expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        self.eval_every_values(vec![func, vec_val])
    }

    fn eval_not_every(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "not-every?") {
            let (func, coll) = result?;
            return self.eval_not_every_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("not-every? expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        self.eval_not_every_values(vec![func, vec_val])
    }

    fn eval_not_any(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "not-any?") {
            let (func, coll) = result?;
            return self.eval_not_any_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("not-any? expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        self.eval_not_any_values(vec![func, vec_val])
    }

    fn eval_some_call(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "some") {
            let (func, coll) = result?;
            return self.eval_some_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("some expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        self.eval_some_values(vec![func, vec_val])
    }

    fn eval_shuffle(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("shuffle expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        self.eval_shuffle_values(vec![value])
    }

    fn eval_take_while(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "take-while") {
            let (func, coll) = result?;
            return self.eval_take_while_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("take-while expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        if let AstExpr::Call {
            callee,
            args: iter_args,
        } = &args[1]
        {
            if let AstExpr::Symbol(sym) = callee.as_ref() {
                if sym == "iterate" && iter_args.len() == 2 {
                    let iter_func = self.eval_expr(&iter_args[0], env.clone())?;
                    let mut current = self.eval_expr(&iter_args[1], env.clone())?;
                    let mut out = Vec::new();
                    loop {
                        let keep = self.call_value(&func, vec![current.clone()])?;
                        if !is_truthy(&keep) {
                            break;
                        }
                        out.push(current.clone());
                        current = self.call_value(&iter_func, vec![current])?;
                    }
                    return Ok(Value::vec(out));
                }
            }
        }
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "take-while")?;
        let mut out = Vec::new();
        for item in items {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if is_truthy(&keep) {
                out.push(item);
            } else {
                break;
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_drop_while(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "drop-while") {
            let (func, coll) = result?;
            return self.eval_drop_while_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("drop-while expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let vec_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&vec_val, "drop-while")?;
        let mut start = items.len();
        for (idx, item) in items.iter().enumerate() {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if !is_truthy(&keep) {
                start = idx;
                break;
            }
        }
        let out = if start >= items.len() {
            Vec::new()
        } else {
            items[start..].to_vec()
        };
        Ok(Value::vec(out))
    }

    fn eval_partition(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
        allow_incomplete: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("partition expects 2 or 3 arguments"));
        }
        let n_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Int(size) = n_val else {
            return Err(Clove2Error::new("partition expects Int size"));
        };
        if size <= 0 {
            return Err(Clove2Error::new("partition expects positive size"));
        }
        let step = if args.len() == 3 {
            let step_val = self.eval_expr(&args[1], env.clone())?;
            let Value::Int(step) = step_val else {
                return Err(Clove2Error::new("partition expects Int step"));
            };
            if step <= 0 {
                return Err(Clove2Error::new("partition expects positive step"));
            }
            step as usize
        } else {
            size as usize
        };
        let coll_expr_idx = if args.len() == 3 { 2 } else { 1 };
        let coll_val = self.eval_expr(&args[coll_expr_idx], env)?;
        self.eval_partition_values(
            vec![Value::Int(size), Value::Int(step as i64), coll_val],
            allow_incomplete,
        )
    }

    fn eval_partition_by(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "partition-by") {
            let (func, coll) = result?;
            return self.eval_partition_by_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("partition-by expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let coll_val = self.eval_expr(&args[1], env)?;
        let items = values_to_vec_items(&coll_val, "partition-by")?;
        let mut out = Vec::new();
        let mut current = Vec::new();
        let mut last_key: Option<Value> = None;
        for item in items {
            let key = self.call_value(&func, vec![item.clone()])?;
            let is_same = match &last_key {
                Some(prev) => prev == &key,
                None => true,
            };
            if !is_same && !current.is_empty() {
                out.push(Value::vec(current));
                current = Vec::new();
            }
            current.push(item);
            last_key = Some(key);
        }
        if !current.is_empty() {
            out.push(Value::vec(current));
        }
        Ok(Value::vec(out))
    }

    fn eval_sort(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("sort expects 1 or 2 arguments"));
        }
        if args.len() == 2 {
            let cmp = self.eval_expr(&args[0], env.clone())?;
            let coll_val = self.eval_expr(&args[1], env)?;
            return self.eval_sort_values(vec![cmp, coll_val]);
        }
        let coll_val = self.eval_expr(&args[0], env)?;
        self.eval_sort_values(vec![coll_val])
    }

    fn eval_sort_by(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "sort-by") {
            let (func, coll) = result?;
            return self.eval_sort_by_values(vec![func, coll]);
        }
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("sort-by expects 2 or 3 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        if args.len() == 3 {
            let cmp = self.eval_expr(&args[1], env.clone())?;
            let coll_val = self.eval_expr(&args[2], env)?;
            return self.eval_sort_by_values(vec![func, cmp, coll_val]);
        }
        let coll_val = self.eval_expr(&args[1], env)?;
        self.eval_sort_by_values(vec![func, coll_val])
    }

    fn eval_distinct(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("distinct expects 1 argument"));
        }
        let coll_val = self.eval_expr(&args[0], env)?;
        self.eval_distinct_values(vec![coll_val])
    }

    fn eval_dedupe(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("dedupe expects 1 argument"));
        }
        let coll_val = self.eval_expr(&args[0], env)?;
        self.eval_dedupe_values(vec![coll_val])
    }

    fn eval_group_by(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if let Some(result) = self.eval_binding_sugar(args, env.clone(), "group-by") {
            let (func, coll) = result?;
            return self.eval_group_by_values(vec![func, coll]);
        }
        if args.len() != 2 {
            return Err(Clove2Error::new("group-by expects 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let coll_val = self.eval_expr(&args[1], env)?;
        self.eval_group_by_values(vec![func, coll_val])
    }

    fn eval_zip(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("zip expects 2 arguments"));
        }
        let left = self.eval_expr(&args[0], env.clone())?;
        let right = self.eval_expr(&args[1], env)?;
        self.eval_zip_values(vec![left, right])
    }

    fn eval_zip_with(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("zip-with expects 3 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let left = self.eval_expr(&args[1], env.clone())?;
        let right = self.eval_expr(&args[2], env)?;
        self.eval_zip_with_values(vec![func, left, right])
    }

    fn eval_zipmap(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("zipmap expects 2 arguments"));
        }
        let keys = self.eval_expr(&args[0], env.clone())?;
        let vals = self.eval_expr(&args[1], env)?;
        self.eval_zipmap_values(vec![keys, vals])
    }

    fn eval_interpose(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("interpose expects 2 arguments"));
        }
        let sep = self.eval_expr(&args[0], env.clone())?;
        let coll = self.eval_expr(&args[1], env)?;
        self.eval_interpose_values(vec![sep, coll])
    }

    fn eval_interleave(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("interleave expects 2 arguments"));
        }
        let left = self.eval_expr(&args[0], env.clone())?;
        let right = self.eval_expr(&args[1], env)?;
        self.eval_interleave_values(vec![left, right])
    }

    fn eval_flatten(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("flatten expects 1 or 2 arguments"));
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let depth = if args.len() == 2 {
            let depth_val = self.eval_expr(&args[1], env)?;
            let Value::Int(depth) = depth_val else {
                return Err(Clove2Error::new("flatten expects Int depth"));
            };
            if depth < 0 {
                return Err(Clove2Error::new("flatten expects non-negative depth"));
            }
            Some(depth as usize)
        } else {
            None
        };
        let mut out = Vec::new();
        flatten_value(&coll, depth, &mut out)?;
        Ok(Value::vec(out))
    }

    fn eval_reduce(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("reduce expects 2 or 3 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let (init, vec_val) = if args.len() == 3 {
            (
                Some(self.eval_expr(&args[1], env.clone())?),
                self.eval_expr(&args[2], env)?,
            )
        } else {
            (None, self.eval_expr(&args[1], env)?)
        };
        let items = values_to_vec_items(&vec_val, "reduce")?;
        let mut iter = items.into_iter();
        let mut acc = match init {
            Some(value) => value,
            None => match iter.next() {
                Some(value) => value,
                None => return Ok(Value::Nil),
            },
        };
        for item in iter {
            acc = self.call_value(&func, vec![acc, item])?;
        }
        Ok(acc)
    }

    fn eval_reduce_kv(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("reduce-kv expects 3 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let mut acc = self.eval_expr(&args[1], env.clone())?;
        let map_val = self.eval_expr(&args[2], env)?;
        let Value::Map(map) = map_val else {
            return Err(Clove2Error::new("reduce-kv expects map"));
        };
        for (key, value) in map.iter() {
            acc = self.call_value(&func, vec![acc, key_to_value(key), value.clone()])?;
        }
        Ok(acc)
    }

    fn eval_apply(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("apply expects at least 2 arguments"));
        }
        let func = self.eval_expr(&args[0], env.clone())?;
        let mut call_args = Vec::new();
        for arg in &args[1..args.len() - 1] {
            call_args.push(self.eval_expr(arg, env.clone())?);
        }
        let tail = self.eval_expr(&args[args.len() - 1], env)?;
        let Value::Vec(items) = tail else {
            return Err(Clove2Error::new("apply expects vector as last argument"));
        };
        call_args.extend(items.as_ref().iter().cloned());
        self.call_value(&func, call_args)
    }

    fn eval_merge(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("merge expects at least 1 argument"));
        }
        let mut out: BTreeMap<Key, Value> = BTreeMap::new();
        for arg in args {
            let value = self.eval_expr(arg, env.clone())?;
            let Value::Map(map) = value else {
                return Err(Clove2Error::new("merge expects map"));
            };
            for (key, val) in map.iter() {
                out.insert(key.clone(), val.clone());
            }
        }
        Ok(Value::map(out))
    }

    fn eval_merge_with(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("merge-with expects at least 2 arguments"));
        }
        let mut values = Vec::with_capacity(args.len());
        values.push(self.eval_expr(&args[0], env.clone())?);
        for arg in &args[1..] {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_merge_with_values(values)
    }

    fn eval_contains(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("contains? expects 2 arguments"));
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let key = self.eval_expr(&args[1], env)?;
        match coll {
            Value::Map(map) => {
                let key = value_to_key(&key)?;
                Ok(Value::Bool(map.contains_key(&key)))
            }
            Value::Vec(items) => {
                let idx = match key {
                    Value::Int(value) if value >= 0 => value as usize,
                    _ => return Err(Clove2Error::new("contains? expects Int index")),
                };
                Ok(Value::Bool(idx < items.len()))
            }
            Value::Set(items) => Ok(Value::Bool(items.iter().any(|v| v == &key))),
            _ => Err(Clove2Error::new("contains? expects map or vector")),
        }
    }

    fn eval_get_in(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("get-in expects 2 or 3 arguments"));
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let path_val = self.eval_expr(&args[1], env.clone())?;
        let default_val = if args.len() == 3 {
            self.eval_expr(&args[2], env)?
        } else {
            Value::Nil
        };
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("get-in expects vector path"));
        };
        self.eval_get_in_value(coll, &path, &default_val)
    }

    fn eval_get_in_value(
        &mut self,
        mut value: Value,
        path: &[Value],
        default_val: &Value,
    ) -> Result<Value, Clove2Error> {
        if path.is_empty() {
            return Ok(value);
        }
        for key in path {
            match value {
                Value::Map(map) => {
                    let key = value_to_key(key)?;
                    if let Some(next) = map.get(&key) {
                        value = next.clone();
                    } else {
                        return Ok(default_val.clone());
                    }
                }
                Value::Vec(items) => {
                    let idx = match key {
                        Value::Int(value) if *value >= 0 => *value as usize,
                        _ => {
                            return Err(Clove2Error::new(
                                "get-in expects Int index for vector path",
                            ))
                        }
                    };
                    if idx >= items.len() {
                        return Ok(default_val.clone());
                    }
                    value = items[idx].clone();
                }
                _ => {
                    return Ok(default_val.clone());
                }
            }
        }
        Ok(value)
    }

    fn eval_dissoc(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("dissoc expects map and keys"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                let mut keys = Vec::with_capacity(args.len() - 1);
                for key_expr in &args[1..] {
                    let key_val = self.eval_expr(key_expr, env.clone())?;
                    keys.push(value_to_key(&key_val)?);
                }
                let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                if let Some(result) =
                    env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                        let Value::Map(map) = value else {
                            return Err(Clove2Error::new("dissoc expects map"));
                        };
                        let map_ref = self.map_for_update_with_unique(map, "dissoc", map_unique)?;
                        for key in &keys {
                            map_ref.remove(key);
                        }
                        Ok(Value::Map(map.clone()))
                    })
                {
                    return result;
                }
            }
        }
        let map_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Map(mut map) = map_val else {
            return Err(Clove2Error::new("dissoc expects map"));
        };
        let map_ref = self.map_for_update(&mut map, "dissoc")?;
        for key_expr in &args[1..] {
            let key_val = self.eval_expr(key_expr, env.clone())?;
            let key = value_to_key(&key_val)?;
            map_ref.remove(&key);
        }
        Ok(Value::Map(map))
    }

    fn eval_assoc_in(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("assoc-in expects map, path, and value"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                if env.with_value_ref(name, |_| ()).is_some() {
                    let path_val = self.eval_expr(&args[1], env.clone())?;
                    let value = self.eval_expr(&args[2], env.clone())?;
                    let Value::Vec(path) = path_val else {
                        return Err(Clove2Error::new("assoc-in expects vector path"));
                    };
                    let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                    if let Some(result) =
                        env.with_value_mut(name, |slot| -> Result<Value, Clove2Error> {
                            if path.is_empty() {
                                return Ok(value.clone());
                            }
                            match slot {
                                Value::Map(map) => {
                                    let map_ref = self
                                        .map_for_update_with_unique(map, "assoc-in", map_unique)?;
                                    self.assoc_in_place_map(map_ref, &path, value.clone())?;
                                    Ok(Value::Map(map.clone()))
                                }
                                Value::Nil => {
                                    let mut new_map = BTreeMap::new();
                                    self.assoc_in_place_map(&mut new_map, &path, value.clone())?;
                                    let new_val = Value::Map(Rc::new(new_map));
                                    *slot = new_val.clone();
                                    Ok(new_val)
                                }
                                _ => Err(Clove2Error::new("assoc-in expects map")),
                            }
                        })
                    {
                        return result;
                    }
                }
            }
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let path_val = self.eval_expr(&args[1], env.clone())?;
        let value = self.eval_expr(&args[2], env)?;
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("assoc-in expects vector path"));
        };
        self.eval_assoc_in_value(coll, &path, value)
    }

    fn eval_assoc_in_value(
        &mut self,
        coll: Value,
        path: &[Value],
        value: Value,
    ) -> Result<Value, Clove2Error> {
        if path.is_empty() {
            return Ok(value);
        }
        let key = value_to_key(&path[0])?;
        let mut map = match coll {
            Value::Map(map) => map,
            Value::Nil => Rc::new(BTreeMap::new()),
            _ => return Err(Clove2Error::new("assoc-in expects map")),
        };
        if matches!(self.mut_mode(), MutMode::Mut) {
            let map_ref = self.map_for_update(&mut map, "assoc-in")?;
            self.assoc_in_place_map(map_ref, path, value)?;
            return Ok(Value::Map(map));
        }
        let next_value = if path.len() == 1 {
            value
        } else {
            let current = map.get(&key).cloned().unwrap_or(Value::Nil);
            self.eval_assoc_in_value(current, &path[1..], value)?
        };
        self.map_for_update(&mut map, "assoc-in")?
            .insert(key, next_value);
        Ok(Value::Map(map))
    }

    fn get_in_ref_value(
        &self,
        value: &Value,
        path: &[Value],
        default: &Value,
    ) -> Result<Value, Clove2Error> {
        let mut current = value;
        for key in path {
            match current {
                Value::Map(map) => {
                    let key = value_to_key(key)?;
                    match map.get(&key) {
                        Some(next) => current = next,
                        None => return Ok(default.clone()),
                    }
                }
                Value::Vec(items) => {
                    let idx = match key {
                        Value::Int(value) if *value >= 0 => *value as usize,
                        _ => {
                            return Err(Clove2Error::new(
                                "get-in expects Int index for vector path",
                            ))
                        }
                    };
                    if idx >= items.len() {
                        return Ok(default.clone());
                    }
                    current = &items[idx];
                }
                _ => return Ok(default.clone()),
            }
        }
        Ok(current.clone())
    }

    fn assoc_in_place_map(
        &self,
        map: &mut BTreeMap<Key, Value>,
        path: &[Value],
        value: Value,
    ) -> Result<(), Clove2Error> {
        if path.is_empty() {
            return Ok(());
        }
        let key = value_to_key(&path[0])?;
        if path.len() == 1 {
            map.insert(key, value);
            return Ok(());
        }
        match map.get_mut(&key) {
            Some(entry) => match entry {
                Value::Map(inner) => {
                    let inner_map = Rc::get_mut(inner).ok_or_else(|| {
                        Clove2Error::new("assoc-in requires unique map in mut mode")
                    })?;
                    self.assoc_in_place_map(inner_map, &path[1..], value)
                }
                Value::Nil => {
                    let mut next = BTreeMap::new();
                    self.assoc_in_place_map(&mut next, &path[1..], value)?;
                    *entry = Value::Map(Rc::new(next));
                    Ok(())
                }
                _ => Err(Clove2Error::new("assoc-in expects map")),
            },
            None => {
                let mut next = BTreeMap::new();
                self.assoc_in_place_map(&mut next, &path[1..], value)?;
                map.insert(key, Value::Map(Rc::new(next)));
                Ok(())
            }
        }
    }

    fn take_in_value_map(
        &self,
        map: &mut BTreeMap<Key, Value>,
        path: &[Value],
    ) -> Result<Value, Clove2Error> {
        if path.is_empty() {
            return Ok(Value::Nil);
        }
        let key = value_to_key(&path[0])?;
        if path.len() == 1 {
            return Ok(map.remove(&key).unwrap_or(Value::Nil));
        }
        let Some(entry) = map.get_mut(&key) else {
            return Ok(Value::Nil);
        };
        match entry {
            Value::Map(inner) => {
                let inner_map = Rc::get_mut(inner)
                    .ok_or_else(|| Clove2Error::new("update-in requires unique map in mut mode"))?;
                self.take_in_value_map(inner_map, &path[1..])
            }
            Value::Nil => Ok(Value::Nil),
            _ => Err(Clove2Error::new("update-in expects map")),
        }
    }

    fn eval_update_in(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 3 {
            return Err(Clove2Error::new(
                "update-in expects map, path, and function",
            ));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                if env.with_value_ref(name, |_| ()).is_some() {
                    let path_val = self.eval_expr(&args[1], env.clone())?;
                    let Value::Vec(path) = path_val else {
                        return Err(Clove2Error::new("update-in expects vector path"));
                    };
                    let func = self.eval_expr(&args[2], env.clone())?;
                    let mut extra_vals = Vec::new();
                    for extra in &args[3..] {
                        extra_vals.push(self.eval_expr(extra, env.clone())?);
                    }
                    let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                    if let Some(result) =
                        env.with_value_mut(name, |slot| -> Result<Value, Clove2Error> {
                            if path.is_empty() {
                                let mut call_args = Vec::with_capacity(1 + extra_vals.len());
                                call_args.push(slot.clone());
                                call_args.extend(extra_vals.iter().cloned());
                                let updated = self.call_value(&func, call_args)?;
                                *slot = updated.clone();
                                return Ok(updated);
                            }
                            match slot {
                                Value::Map(map) => {
                                    let map_ref = self.map_for_update_with_unique(
                                        map,
                                        "update-in",
                                        map_unique,
                                    )?;
                                    let current = self.take_in_value_map(map_ref, &path)?;
                                    let mut call_args = Vec::with_capacity(1 + extra_vals.len());
                                    call_args.push(current);
                                    call_args.extend(extra_vals.iter().cloned());
                                    let updated = self.call_value(&func, call_args)?;
                                    self.assoc_in_place_map(map_ref, &path, updated.clone())?;
                                    Ok(Value::Map(map.clone()))
                                }
                                Value::Nil => {
                                    let mut new_map = BTreeMap::new();
                                    let mut call_args = Vec::with_capacity(1 + extra_vals.len());
                                    call_args.push(Value::Nil);
                                    call_args.extend(extra_vals.iter().cloned());
                                    let updated = self.call_value(&func, call_args)?;
                                    self.assoc_in_place_map(&mut new_map, &path, updated.clone())?;
                                    let new_val = Value::Map(Rc::new(new_map));
                                    *slot = new_val.clone();
                                    Ok(new_val)
                                }
                                _ => Err(Clove2Error::new("update-in expects map")),
                            }
                        })
                    {
                        return result;
                    }
                }
            }
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let path_val = self.eval_expr(&args[1], env.clone())?;
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("update-in expects vector path"));
        };
        let func = self.eval_expr(&args[2], env.clone())?;
        let mut call_args = Vec::new();
        let mut extra_vals = Vec::new();
        for extra in &args[3..] {
            extra_vals.push(self.eval_expr(extra, env.clone())?);
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if path.is_empty() {
                call_args.push(coll);
                call_args.extend(extra_vals);
                let updated = self.call_value(&func, call_args)?;
                return Ok(updated);
            }
            match coll {
                Value::Map(mut map) => {
                    let map_ref = self.map_for_update(&mut map, "update-in")?;
                    let current = self.take_in_value_map(map_ref, &path)?;
                    call_args.push(current);
                    call_args.extend(extra_vals);
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(map_ref, &path, updated)?;
                    Ok(Value::Map(map))
                }
                Value::Nil => {
                    let mut new_map = BTreeMap::new();
                    call_args.push(Value::Nil);
                    call_args.extend(extra_vals);
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(&mut new_map, &path, updated)?;
                    Ok(Value::Map(Rc::new(new_map)))
                }
                _ => Err(Clove2Error::new("update-in expects map")),
            }
        } else {
            let current = self.eval_get_in_value(coll.clone(), &path, &Value::Nil)?;
            call_args.push(current);
            call_args.extend(extra_vals);
            let updated = self.call_value(&func, call_args)?;
            self.eval_assoc_in_value(coll, &path, updated)
        }
    }

    fn update_in_global_values(
        &mut self,
        name: &str,
        path_val: Value,
        func: Value,
        extras: Vec<Value>,
    ) -> Result<Value, Clove2Error> {
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("update-in expects vector path"));
        };
        let env = self
            .global_env
            .clone()
            .ok_or_else(|| Clove2Error::new("global env is not initialized"))?;
        let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
        let mut values = env.values.borrow_mut();
        let Some(slot) = values.get_mut(name) else {
            return Err(Clove2Error::new(format!("unknown symbol: {}", name)));
        };
        let mut slot_value = std::mem::replace(slot, Value::Nil);
        drop(values);
        let (new_slot, result) = if path.is_empty() {
            let mut call_args = Vec::with_capacity(1 + extras.len());
            call_args.push(slot_value.clone());
            call_args.extend(extras.iter().cloned());
            let updated = self.call_value(&func, call_args)?;
            (updated.clone(), updated)
        } else {
            match &mut slot_value {
                Value::Map(map) => {
                    let map_ref = self.map_for_update_with_unique(map, "update-in", map_unique)?;
                    let current = self.take_in_value_map(map_ref, &path)?;
                    let mut call_args = Vec::with_capacity(1 + extras.len());
                    call_args.push(current);
                    call_args.extend(extras.iter().cloned());
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(map_ref, &path, updated.clone())?;
                    (Value::Map(map.clone()), Value::Map(map.clone()))
                }
                Value::Nil => {
                    let mut new_map = BTreeMap::new();
                    let mut call_args = Vec::with_capacity(1 + extras.len());
                    call_args.push(Value::Nil);
                    call_args.extend(extras.iter().cloned());
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(&mut new_map, &path, updated.clone())?;
                    let new_val = Value::Map(Rc::new(new_map));
                    (new_val.clone(), new_val)
                }
                _ => return Err(Clove2Error::new("update-in expects map")),
            }
        };
        let mut values = env.values.borrow_mut();
        if let Some(slot) = values.get_mut(name) {
            *slot = new_slot;
        } else {
            values.insert(name.to_string(), new_slot);
        }
        Ok(result)
    }

    fn eval_take(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("take expects 2 arguments"));
        }
        let count_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Int(count) = count_val else {
            return Err(Clove2Error::new("take expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        if count == 0 {
            return Ok(Value::vec(Vec::new()));
        }
        if let AstExpr::Call {
            callee,
            args: gen_args,
        } = &args[1]
        {
            if let AstExpr::Symbol(sym) = callee.as_ref() {
                match sym.as_str() {
                    "range" if gen_args.is_empty() => {
                        let items = (0..count).map(|v| Value::Int(v as i64)).collect();
                        return Ok(Value::vec(items));
                    }
                    "repeat" if gen_args.len() == 1 => {
                        let value = self.eval_expr(&gen_args[0], env.clone())?;
                        return Ok(Value::vec(vec![value; count]));
                    }
                    "repeatedly" if gen_args.len() == 1 => {
                        let func = self.eval_expr(&gen_args[0], env.clone())?;
                        let mut out = Vec::with_capacity(count);
                        for _ in 0..count {
                            out.push(self.call_value(&func, Vec::new())?);
                        }
                        return Ok(Value::vec(out));
                    }
                    "iterate" if gen_args.len() == 2 => {
                        let func = self.eval_expr(&gen_args[0], env.clone())?;
                        let mut current = self.eval_expr(&gen_args[1], env.clone())?;
                        let mut out = Vec::with_capacity(count);
                        for _ in 0..count {
                            out.push(current.clone());
                            current = self.call_value(&func, vec![current])?;
                        }
                        return Ok(Value::vec(out));
                    }
                    _ => {}
                }
            }
        }
        let coll = self.eval_expr(&args[1], env)?;
        match coll {
            Value::Vec(items) => Ok(Value::vec(
                items.as_ref().iter().take(count).cloned().collect(),
            )),
            Value::List(items) => Ok(Value::vec(items.into_iter().take(count).collect())),
            Value::Str(value) => {
                let out: String = value.chars().take(count).collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("take expects vector or string")),
        }
    }

    fn eval_drop(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("drop expects 2 arguments"));
        }
        let count_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Int(count) = count_val else {
            return Err(Clove2Error::new("drop expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        let coll = self.eval_expr(&args[1], env)?;
        match coll {
            Value::Vec(items) => Ok(Value::vec(
                items.as_ref().iter().skip(count).cloned().collect(),
            )),
            Value::List(items) => Ok(Value::vec(items.into_iter().skip(count).collect())),
            Value::Str(value) => {
                let out: String = value.chars().skip(count).collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("drop expects vector or string")),
        }
    }

    fn eval_take_last(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("take-last expects 2 arguments"));
        }
        let count_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Int(count) = count_val else {
            return Err(Clove2Error::new("take-last expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        let coll = self.eval_expr(&args[1], env)?;
        match coll {
            Value::Vec(items) => {
                let len = items.len();
                let start = len.saturating_sub(count);
                Ok(Value::vec(items[start..].to_vec()))
            }
            Value::Str(value) => {
                let chars: Vec<char> = value.chars().collect();
                let len = chars.len();
                let start = len.saturating_sub(count);
                let out: String = chars[start..].iter().collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("take-last expects vector or string")),
        }
    }

    fn eval_drop_last(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("drop-last expects 1 or 2 arguments"));
        }
        let (count, coll_expr) = if args.len() == 1 {
            (1, &args[0])
        } else {
            let count_val = self.eval_expr(&args[0], env.clone())?;
            let Value::Int(count) = count_val else {
                return Err(Clove2Error::new("drop-last expects Int count"));
            };
            (count, &args[1])
        };
        let count = if count < 0 { 0 } else { count as usize };
        let coll = self.eval_expr(coll_expr, env)?;
        match coll {
            Value::Vec(items) => {
                let len = items.len();
                let end = len.saturating_sub(count);
                Ok(Value::vec(items[..end].to_vec()))
            }
            Value::List(items) => {
                let len = items.len();
                let end = len.saturating_sub(count);
                Ok(Value::List(items[..end].to_vec()))
            }
            Value::Str(value) => {
                let chars: Vec<char> = value.chars().collect();
                let len = chars.len();
                let end = len.saturating_sub(count);
                let out: String = chars[..end].iter().collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("drop-last expects vector or string")),
        }
    }

    fn eval_reverse(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("reverse expects 1 argument"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                if let Some(result) =
                    env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                        match value {
                            Value::Vec(items) => {
                                let items_ref = self.vec_for_update(items, "reverse")?;
                                items_ref.reverse();
                                Ok(Value::Vec(items.clone()))
                            }
                            Value::List(items) => {
                                items.reverse();
                                Ok(Value::List(items.clone()))
                            }
                            Value::Str(value) => {
                                let out: String = value.chars().rev().collect();
                                Ok(Value::Str(out))
                            }
                            _ => Err(Clove2Error::new("reverse expects collection or string")),
                        }
                    })
                {
                    return result;
                }
            }
        }
        let coll = self.eval_expr(&args[0], env)?;
        match coll {
            Value::Vec(mut items) => {
                let items_ref = self.vec_for_update(&mut items, "reverse")?;
                items_ref.reverse();
                Ok(Value::Vec(items))
            }
            Value::List(mut items) => {
                items.reverse();
                Ok(Value::List(items))
            }
            Value::Str(value) => {
                let out: String = value.chars().rev().collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("reverse expects collection or string")),
        }
    }

    fn eval_subvec(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("subvec expects 2 or 3 arguments"));
        }
        let vec_val = self.eval_expr(&args[0], env.clone())?;
        let start_val = self.eval_expr(&args[1], env.clone())?;
        let Value::Vec(items) = vec_val else {
            return Err(Clove2Error::new("subvec expects vector"));
        };
        let Value::Int(start) = start_val else {
            return Err(Clove2Error::new("subvec expects Int start"));
        };
        let end = if args.len() == 3 {
            let end_val = self.eval_expr(&args[2], env)?;
            let Value::Int(end) = end_val else {
                return Err(Clove2Error::new("subvec expects Int end"));
            };
            end
        } else {
            items.len() as i64
        };
        if start < 0 || end < 0 {
            return Err(Clove2Error::new("subvec expects non-negative indices"));
        }
        let start = start as usize;
        let end = end as usize;
        if start > end || end > items.len() {
            return Err(Clove2Error::new("subvec index out of range"));
        }
        Ok(Value::vec(items[start..end].to_vec()))
    }

    fn eval_concat(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("concat expects at least 1 argument"));
        }
        let mut vec_items: Vec<Value> = Vec::new();
        let mut str_items = String::new();
        let mut mode: Option<&str> = None;
        for arg in args {
            let value = self.eval_expr(arg, env.clone())?;
            match value {
                Value::Vec(_) | Value::List(_) | Value::Set(_) => {
                    if let Some(kind) = mode {
                        if kind != "vec" {
                            return Err(Clove2Error::new("concat expects same type"));
                        }
                    } else {
                        mode = Some("vec");
                    }
                    let items = into_vec_items(value, "concat")?;
                    vec_items.extend(items);
                }
                Value::Str(value) => {
                    if let Some(kind) = mode {
                        if kind != "str" {
                            return Err(Clove2Error::new("concat expects same type"));
                        }
                    } else {
                        mode = Some("str");
                    }
                    str_items.push_str(&value);
                }
                _ => return Err(Clove2Error::new("concat expects vector or string")),
            }
        }
        match mode {
            Some("vec") => Ok(Value::vec(vec_items)),
            Some("str") => Ok(Value::Str(str_items)),
            _ => Err(Clove2Error::new("concat expects vector or string")),
        }
    }

    fn eval_includes(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("includes? expects 2 arguments"));
        }
        let coll = self.eval_expr(&args[0], env.clone())?;
        let item = self.eval_expr(&args[1], env)?;
        match coll {
            Value::Vec(items) => Ok(Value::Bool(items.iter().any(|v| v == &item))),
            Value::List(items) => Ok(Value::Bool(items.iter().any(|v| v == &item))),
            Value::Set(items) => Ok(Value::Bool(items.iter().any(|v| v == &item))),
            Value::Map(map) => {
                let key = value_to_key(&item)?;
                Ok(Value::Bool(map.contains_key(&key)))
            }
            Value::Str(value) => match item {
                Value::Str(item) => Ok(Value::Bool(value.contains(&item))),
                _ => Err(Clove2Error::new("includes? expects string item for string")),
            },
            Value::Nil => Ok(Value::Bool(false)),
            _ => Err(Clove2Error::new("includes? expects collection")),
        }
    }

    fn eval_conj(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("conj expects at least 2 arguments"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                let mut values = Vec::with_capacity(args.len() - 1);
                for arg in &args[1..] {
                    values.push(self.eval_expr(arg, env.clone())?);
                }
                if let Some(result) =
                    env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                        match value {
                            Value::Vec(items) => {
                                let items_ref = self.vec_for_update(items, "conj")?;
                                for value in &values {
                                    items_ref.push(value.clone());
                                }
                                Ok(Value::Vec(items.clone()))
                            }
                            Value::List(items) => {
                                for value in &values {
                                    items.insert(0, value.clone());
                                }
                                Ok(Value::List(items.clone()))
                            }
                            Value::Set(items) => {
                                for value in &values {
                                    if !items.iter().any(|existing| existing == value) {
                                        items.push(value.clone());
                                    }
                                }
                                Ok(Value::Set(items.clone()))
                            }
                            _ => Err(Clove2Error::new("conj expects collection")),
                        }
                    })
                {
                    return result;
                }
            }
        }
        let vec_val = self.eval_expr(&args[0], env.clone())?;
        match vec_val {
            Value::Vec(mut items) => {
                let items_ref = self.vec_for_update(&mut items, "conj")?;
                for arg in &args[1..] {
                    items_ref.push(self.eval_expr(arg, env.clone())?);
                }
                Ok(Value::Vec(items))
            }
            Value::List(mut items) => {
                for arg in &args[1..] {
                    let value = self.eval_expr(arg, env.clone())?;
                    items.insert(0, value);
                }
                Ok(Value::List(items))
            }
            Value::Set(mut items) => {
                for arg in &args[1..] {
                    let value = self.eval_expr(arg, env.clone())?;
                    if !items.iter().any(|existing| existing == &value) {
                        items.push(value);
                    }
                }
                Ok(Value::Set(items))
            }
            _ => Err(Clove2Error::new("conj expects collection")),
        }
    }

    fn eval_cons(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("cons expects 2 arguments"));
        }
        let item = self.eval_expr(&args[0], env.clone())?;
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[1] {
                if let Some(result) =
                    env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                        match value {
                            Value::Vec(items) => {
                                let items_ref = self.vec_for_update(items, "cons")?;
                                items_ref.insert(0, item.clone());
                                Ok(Value::List(items_ref.clone()))
                            }
                            Value::List(items) => {
                                items.insert(0, item.clone());
                                Ok(Value::List(items.clone()))
                            }
                            _ => Err(Clove2Error::new("cons expects collection")),
                        }
                    })
                {
                    return result;
                }
            }
        }
        let coll_val = self.eval_expr(&args[1], env)?;
        match coll_val {
            Value::Vec(mut items) => {
                let items_ref = self.vec_for_update(&mut items, "cons")?;
                items_ref.insert(0, item);
                Ok(Value::List(items_ref.clone()))
            }
            Value::List(mut items) => {
                items.insert(0, item);
                Ok(Value::List(items))
            }
            _ => Err(Clove2Error::new("cons expects collection")),
        }
    }

    fn eval_list(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            out.push(self.eval_expr(arg, env.clone())?);
        }
        Ok(Value::List(out))
    }

    fn eval_vector(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            out.push(self.eval_expr(arg, env.clone())?);
        }
        Ok(Value::vec(out))
    }

    fn eval_hash_map(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() % 2 != 0 {
            return Err(Clove2Error::new("hash-map expects even arguments"));
        }
        let mut out = BTreeMap::new();
        for pair in args.chunks(2) {
            let key_val = self.eval_expr(&pair[0], env.clone())?;
            let val = self.eval_expr(&pair[1], env.clone())?;
            let key = value_to_key(&key_val)?;
            out.insert(key, val);
        }
        Ok(Value::map(out))
    }

    fn eval_vec(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("vec expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Vec(items) => Ok(Value::Vec(items)),
            Value::List(items) => Ok(Value::vec(items)),
            Value::Set(items) => Ok(Value::vec(items)),
            Value::Map(map) => {
                let mut out = Vec::with_capacity(map.len());
                for (key, value) in map.iter() {
                    out.push(Value::vec(vec![key_to_value(key), value.clone()]));
                }
                Ok(Value::vec(out))
            }
            Value::Str(text) => Ok(Value::vec(
                text.chars().map(|ch| Value::Str(ch.to_string())).collect(),
            )),
            _ => Err(Clove2Error::new("vec expects collection")),
        }
    }

    fn eval_into(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("into expects 2 arguments"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                if env.with_value_ref(name, |_| ()).is_some() {
                    let source = self.eval_expr(&args[1], env.clone())?;
                    let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                    if let Some(result) =
                        env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                            match value {
                                Value::Vec(items) => {
                                    let items_ref = self.vec_for_update(items, "into")?;
                                    let values = values_to_vec_items(&source, "into")?;
                                    items_ref.extend(values);
                                    Ok(Value::Vec(items.clone()))
                                }
                                Value::List(items) => {
                                    let values = values_to_vec_items(&source, "into")?;
                                    items.extend(values);
                                    Ok(Value::List(items.clone()))
                                }
                                Value::Set(items) => {
                                    let values = values_to_vec_items(&source, "into")?;
                                    for value in values {
                                        if !items.iter().any(|existing| existing == &value) {
                                            items.push(value);
                                        }
                                    }
                                    Ok(Value::Set(items.clone()))
                                }
                                Value::Map(map) => {
                                    let values = values_to_vec_items(&source, "into")?;
                                    let map_ref =
                                        self.map_for_update_with_unique(map, "into", map_unique)?;
                                    for value in values {
                                        let Value::Vec(pair) = value else {
                                            return Err(Clove2Error::new(
                                                "into expects [key value] pairs",
                                            ));
                                        };
                                        let pair = pair.as_ref();
                                        if pair.len() != 2 {
                                            return Err(Clove2Error::new(
                                                "into expects [key value] pairs",
                                            ));
                                        }
                                        let key = value_to_key(&pair[0])?;
                                        map_ref.insert(key, pair[1].clone());
                                    }
                                    Ok(Value::Map(map.clone()))
                                }
                                _ => Err(Clove2Error::new("into expects collection target")),
                            }
                        })
                    {
                        return result;
                    }
                }
            }
        }
        let target = self.eval_expr(&args[0], env.clone())?;
        let source = self.eval_expr(&args[1], env)?;
        match target {
            Value::Vec(items) => {
                let mut items = items;
                let items_ref = self.vec_for_update(&mut items, "into")?;
                let values = values_to_vec_items(&source, "into")?;
                items_ref.extend(values);
                Ok(Value::Vec(items))
            }
            Value::List(mut items) => {
                let values = values_to_vec_items(&source, "into")?;
                items.extend(values);
                Ok(Value::List(items))
            }
            Value::Set(mut items) => {
                let values = values_to_vec_items(&source, "into")?;
                for value in values {
                    if !items.iter().any(|existing| existing == &value) {
                        items.push(value);
                    }
                }
                Ok(Value::Set(items))
            }
            Value::Map(mut map) => {
                let values = values_to_vec_items(&source, "into")?;
                let map_ref = self.map_for_update(&mut map, "into")?;
                for value in values {
                    let Value::Vec(pair) = value else {
                        return Err(Clove2Error::new("into expects [key value] pairs"));
                    };
                    let pair = pair.as_ref();
                    if pair.len() != 2 {
                        return Err(Clove2Error::new("into expects [key value] pairs"));
                    }
                    let key = value_to_key(&pair[0])?;
                    map_ref.insert(key, pair[1].clone());
                }
                Ok(Value::Map(map))
            }
            _ => Err(Clove2Error::new("into expects collection target")),
        }
    }

    fn eval_assoc(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 3 || (args.len() - 1) % 2 != 0 {
            return Err(Clove2Error::new("assoc expects map and key/value pairs"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                let mut pairs = Vec::with_capacity((args.len() - 1) / 2);
                for pair in args[1..].chunks(2) {
                    let key_val = self.eval_expr(&pair[0], env.clone())?;
                    let value = self.eval_expr(&pair[1], env.clone())?;
                    pairs.push((key_val, value));
                }
                let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                if let Some(result) =
                    env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                        match value {
                            Value::Map(map) => {
                                let map_ref =
                                    self.map_for_update_with_unique(map, "assoc", map_unique)?;
                                for (key_val, value) in &pairs {
                                    let key = value_to_key(key_val)?;
                                    map_ref.insert(key, value.clone());
                                }
                                Ok(Value::Map(map.clone()))
                            }
                            Value::Vec(items) => {
                                let items_ref = self.vec_for_update(items, "assoc")?;
                                for (key_val, value) in &pairs {
                                    let Value::Int(idx) = key_val else {
                                        return Err(Clove2Error::new(
                                            "assoc expects Int index for vector",
                                        ));
                                    };
                                    if *idx < 0 {
                                        return Err(Clove2Error::new(
                                            "assoc expects non-negative index",
                                        ));
                                    }
                                    let idx = *idx as usize;
                                    if idx < items_ref.len() {
                                        items_ref[idx] = value.clone();
                                    } else if idx == items_ref.len() {
                                        items_ref.push(value.clone());
                                    } else {
                                        return Err(Clove2Error::new("assoc index out of range"));
                                    }
                                }
                                Ok(Value::Vec(items.clone()))
                            }
                            _ => Err(Clove2Error::new("assoc expects map or vector")),
                        }
                    })
                {
                    return result;
                }
            }
        }
        let map_val = self.eval_expr(&args[0], env.clone())?;
        match map_val {
            Value::Map(mut map) => {
                let map_ref = self.map_for_update(&mut map, "assoc")?;
                for pair in args[1..].chunks(2) {
                    let key_val = self.eval_expr(&pair[0], env.clone())?;
                    let value = self.eval_expr(&pair[1], env.clone())?;
                    let key = value_to_key(&key_val)?;
                    map_ref.insert(key, value);
                }
                Ok(Value::Map(map))
            }
            Value::Vec(mut items) => {
                let items_ref = self.vec_for_update(&mut items, "assoc")?;
                for pair in args[1..].chunks(2) {
                    let key_val = self.eval_expr(&pair[0], env.clone())?;
                    let value = self.eval_expr(&pair[1], env.clone())?;
                    let Value::Int(idx) = key_val else {
                        return Err(Clove2Error::new("assoc expects Int index for vector"));
                    };
                    if idx < 0 {
                        return Err(Clove2Error::new("assoc expects non-negative index"));
                    }
                    let idx = idx as usize;
                    if idx < items_ref.len() {
                        items_ref[idx] = value;
                    } else if idx == items_ref.len() {
                        items_ref.push(value);
                    } else {
                        return Err(Clove2Error::new("assoc index out of range"));
                    }
                }
                Ok(Value::Vec(items))
            }
            _ => Err(Clove2Error::new("assoc expects map or vector")),
        }
    }

    fn eval_repeat(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("repeat expects 2 arguments"));
        }
        let count_val = self.eval_expr(&args[0], env.clone())?;
        let value = self.eval_expr(&args[1], env)?;
        let Value::Int(count) = count_val else {
            return Err(Clove2Error::new("repeat expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        let mut out = Vec::with_capacity(count);
        for _ in 0..count {
            out.push(value.clone());
        }
        Ok(Value::vec(out))
    }

    fn eval_repeatedly(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("repeatedly expects 2 arguments"));
        }
        let count_val = self.eval_expr(&args[0], env.clone())?;
        let func = self.eval_expr(&args[1], env)?;
        let Value::Int(count) = count_val else {
            return Err(Clove2Error::new("repeatedly expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        let mut out = Vec::with_capacity(count);
        for _ in 0..count {
            out.push(self.call_value(&func, Vec::new())?);
        }
        Ok(Value::vec(out))
    }

    fn eval_iterate(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.eval_expr(arg, env.clone())?);
        }
        self.eval_iterate_values(values)
    }

    fn eval_iterate_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        let (count, func, init) = match args.len() {
            2 => (1024i64, args[0].clone(), args[1].clone()),
            3 => {
                let Value::Int(count) = args[0] else {
                    return Err(Clove2Error::new("iterate expects Int count"));
                };
                (count, args[1].clone(), args[2].clone())
            }
            _ => return Err(Clove2Error::new("iterate expects 2 or 3 arguments")),
        };
        let count = if count < 0 { 0 } else { count as usize };
        let mut out = Vec::with_capacity(count);
        let mut current = init;
        for _ in 0..count {
            out.push(current.clone());
            current = self.call_value(&func, vec![current])?;
        }
        Ok(Value::vec(out))
    }

    fn eval_butlast(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("butlast expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => {
                if items.len() <= 1 {
                    Ok(Value::vec(Vec::new()))
                } else {
                    Ok(Value::vec(items[..items.len() - 1].to_vec()))
                }
            }
            Value::List(items) => {
                if items.len() <= 1 {
                    Ok(Value::List(Vec::new()))
                } else {
                    Ok(Value::List(items[..items.len() - 1].to_vec()))
                }
            }
            _ => Err(Clove2Error::new("butlast expects collection")),
        }
    }

    fn eval_next(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("next expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => {
                if items.len() <= 1 {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items[1..].to_vec()))
                }
            }
            Value::List(items) => {
                if items.len() <= 1 {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items[1..].to_vec()))
                }
            }
            _ => Err(Clove2Error::new("next expects collection")),
        }
    }

    fn eval_pop(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("pop expects 1 argument"));
        }
        let vec_val = self.eval_expr(&args[0], env)?;
        match vec_val {
            Value::Vec(items) => {
                if items.is_empty() {
                    return Err(Clove2Error::new("pop expects non-empty vector"));
                }
                Ok(Value::vec(items[..items.len() - 1].to_vec()))
            }
            Value::List(items) => {
                if items.is_empty() {
                    return Err(Clove2Error::new("pop expects non-empty list"));
                }
                Ok(Value::List(items[1..].to_vec()))
            }
            _ => Err(Clove2Error::new("pop expects collection")),
        }
    }

    fn eval_empty_value(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("empty expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        match value {
            Value::Nil => Ok(Value::Nil),
            Value::Vec(_) => Ok(Value::vec(Vec::new())),
            Value::List(_) => Ok(Value::List(Vec::new())),
            Value::Set(_) => Ok(Value::Set(Vec::new())),
            Value::Map(_) => Ok(Value::map(BTreeMap::new())),
            Value::Str(_) => Ok(Value::Str(String::new())),
            _ => Err(Clove2Error::new("empty expects collection")),
        }
    }

    fn eval_update(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() < 3 {
            return Err(Clove2Error::new("update expects map, key, and function"));
        }
        if matches!(self.mut_mode(), MutMode::Mut) {
            if let AstExpr::Symbol(name) = &args[0] {
                let key_val = self.eval_expr(&args[1], env.clone())?;
                let key = value_to_key(&key_val)?;
                let func = self.eval_expr(&args[2], env.clone())?;
                let mut extra_vals = Vec::new();
                for extra in &args[3..] {
                    extra_vals.push(self.eval_expr(extra, env.clone())?);
                }
                let map_unique = map_unique_for_symbol(&env, name).unwrap_or(false);
                if let Some(current) =
                    env.with_value_ref(name, |value| -> Result<Value, Clove2Error> {
                        let Value::Map(map) = value else {
                            return Err(Clove2Error::new("update expects map"));
                        };
                        Ok(map.get(&key).cloned().unwrap_or(Value::Nil))
                    })
                {
                    let mut call_args = Vec::with_capacity(1 + extra_vals.len());
                    call_args.push(current?);
                    call_args.extend(extra_vals);
                    let updated = self.call_value(&func, call_args)?;
                    if let Some(result) =
                        env.with_value_mut(name, |value| -> Result<Value, Clove2Error> {
                            let Value::Map(map) = value else {
                                return Err(Clove2Error::new("update expects map"));
                            };
                            self.map_for_update_with_unique(map, "update", map_unique)?
                                .insert(key, updated);
                            Ok(Value::Map(map.clone()))
                        })
                    {
                        return result;
                    }
                }
            }
        }
        let map_val = self.eval_expr(&args[0], env.clone())?;
        let Value::Map(mut map) = map_val else {
            return Err(Clove2Error::new("update expects map"));
        };
        let key_val = self.eval_expr(&args[1], env.clone())?;
        let key = value_to_key(&key_val)?;
        let func = self.eval_expr(&args[2], env.clone())?;
        let mut call_args = Vec::new();
        call_args.push(map.get(&key).cloned().unwrap_or(Value::Nil));
        for extra in &args[3..] {
            call_args.push(self.eval_expr(extra, env.clone())?);
        }
        let updated = self.call_value(&func, call_args)?;
        self.map_for_update(&mut map, "update")?
            .insert(key, updated);
        Ok(Value::Map(map))
    }

    fn eval_keys(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("keys expects 1 argument"));
        }
        let map_val = self.eval_expr(&args[0], env)?;
        let Value::Map(map) = map_val else {
            return Err(Clove2Error::new("keys expects map"));
        };
        let mut out = Vec::new();
        for key in map.keys() {
            out.push(key_to_value(key));
        }
        Ok(Value::vec(out))
    }

    fn eval_vals(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("vals expects 1 argument"));
        }
        let map_val = self.eval_expr(&args[0], env)?;
        let Value::Map(map) = map_val else {
            return Err(Clove2Error::new("vals expects map"));
        };
        let mut out = Vec::new();
        for value in map.values() {
            out.push(value.clone());
        }
        Ok(Value::vec(out))
    }

    fn eval_select_keys(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("select-keys expects map and keys"));
        }
        let map_val = self.eval_expr(&args[0], env.clone())?;
        let keys_val = self.eval_expr(&args[1], env)?;
        let Value::Map(map) = map_val else {
            return Err(Clove2Error::new("select-keys expects map"));
        };
        let Value::Vec(keys) = keys_val else {
            return Err(Clove2Error::new("select-keys expects vector keys"));
        };
        let mut out: BTreeMap<Key, Value> = BTreeMap::new();
        for key in keys.as_ref().iter() {
            let key = value_to_key(&key)?;
            if let Some(value) = map.get(&key) {
                out.insert(key, value.clone());
            }
        }
        Ok(Value::map(out))
    }

    fn eval_frequencies(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("frequencies expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        self.eval_frequencies_values(vec![value])
    }

    fn eval_expect(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("expect expects 2 arguments"));
        }
        let ty = parse_type_from_ast(&args[0])?;
        let value = self.eval_expr(&args[1], env)?;
        let _ = expect_type(&value, &ty)?;
        Ok(value)
    }

    fn eval_as(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("as expects 2 arguments"));
        }
        let ty = parse_type_from_ast(&args[0])?;
        let value = self.eval_expr(&args[1], env)?;
        if as_type(&value, &ty).is_some() {
            Ok(value)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_json_read_file(&mut self, args: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("json::read-file expects path"));
        }
        let path_val = self.eval_expr(&args[0], env)?;
        let Value::Str(path) = path_val else {
            return Err(Clove2Error::new("json::read-file expects string path"));
        };
        let mut schema = None;
        let mut validate = false;
        if args.len() > 1 {
            if (args.len() - 1) % 2 != 0 {
                return Err(Clove2Error::new("json::read-file expects keyword options"));
            }
            for pair in args[1..].chunks(2) {
                let key = match &pair[0] {
                    AstExpr::Keyword(name) => name.as_str(),
                    AstExpr::Symbol(name) => name.as_str(),
                    _ => {
                        return Err(Clove2Error::new(
                            "json::read-file option key must be keyword or symbol",
                        ))
                    }
                };
                match key {
                    "schema" | "type" => {
                        schema = Some(parse_type_from_ast(&pair[1])?);
                    }
                    "validate" => {
                        if let AstExpr::Literal(crate::ast::Literal::Bool(value)) = &pair[1] {
                            validate = *value;
                        } else {
                            return Err(Clove2Error::new("json::read-file :validate expects bool"));
                        }
                    }
                    "infer" => {}
                    _ => return Err(Clove2Error::new("json::read-file has unknown option")),
                }
            }
        }
        let resolved = normalize_path(&path);
        let content = fs::read_to_string(&resolved)
            .map_err(|err| Clove2Error::new(format!("failed to read {}: {}", resolved, err)))?;
        let value: serde_json::Value = serde_json::from_str(&content)
            .map_err(|err| Clove2Error::new(format!("invalid json {}: {}", resolved, err)))?;
        let out = json_to_value(&value);
        if validate {
            if let Some(schema) = schema {
                if !crate::type_check::matches_type(&out, &schema) {
                    return Err(Clove2Error::new(format!(
                        "json::read-file validation failed: {}",
                        schema
                    )));
                }
            }
        }
        Ok(out)
    }

    fn eval_json_read_string(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("json::read-string expects json string"));
        }
        let input_val = self.eval_expr(&args[0], env)?;
        let Value::Str(input) = input_val else {
            return Err(Clove2Error::new("json::read-string expects string input"));
        };
        let mut schema = None;
        let mut validate = false;
        if args.len() > 1 {
            if (args.len() - 1) % 2 != 0 {
                return Err(Clove2Error::new(
                    "json::read-string expects keyword options",
                ));
            }
            for pair in args[1..].chunks(2) {
                let key = match &pair[0] {
                    AstExpr::Keyword(name) => name.as_str(),
                    AstExpr::Symbol(name) => name.as_str(),
                    _ => {
                        return Err(Clove2Error::new(
                            "json::read-string option key must be keyword or symbol",
                        ))
                    }
                };
                match key {
                    "schema" | "type" => {
                        schema = Some(parse_type_from_ast(&pair[1])?);
                    }
                    "validate" => {
                        if let AstExpr::Literal(crate::ast::Literal::Bool(value)) = &pair[1] {
                            validate = *value;
                        } else {
                            return Err(Clove2Error::new(
                                "json::read-string :validate expects bool",
                            ));
                        }
                    }
                    "infer" => {}
                    _ => return Err(Clove2Error::new("json::read-string has unknown option")),
                }
            }
        }
        let value: serde_json::Value = serde_json::from_str(&input)
            .map_err(|err| Clove2Error::new(format!("invalid json: {}", err)))?;
        let out = json_to_value(&value);
        if validate {
            if let Some(schema) = schema {
                if !crate::type_check::matches_type(&out, &schema) {
                    return Err(Clove2Error::new(format!(
                        "json::read-string validation failed: {}",
                        schema
                    )));
                }
            }
        }
        Ok(out)
    }

    fn eval_json_write_string(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("json::write-string expects 1 argument"));
        }
        let value = self.eval_expr(&args[0], env)?;
        let json_value = value_to_json(&value)?;
        let out = serde_json::to_string(&json_value)
            .map_err(|err| Clove2Error::new(format!("json::write-string failed: {}", err)))?;
        Ok(Value::Str(out))
    }

    fn eval_json_write_file(
        &mut self,
        args: &[AstExpr],
        env: EnvRef,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("json::write-file expects 2 arguments"));
        }
        let path_val = self.eval_expr(&args[0], env.clone())?;
        let value = self.eval_expr(&args[1], env)?;
        let Value::Str(path) = path_val else {
            return Err(Clove2Error::new("json::write-file expects string path"));
        };
        let json_value = value_to_json(&value)?;
        let out = serde_json::to_string(&json_value)
            .map_err(|err| Clove2Error::new(format!("json::write-file failed: {}", err)))?;
        let resolved = normalize_path(&path);
        fs::write(&resolved, out)
            .map_err(|err| Clove2Error::new(format!("json::write-file failed: {}", err)))?;
        Ok(Value::Str(path))
    }

    fn call_value(&mut self, func: &Value, args: Vec<Value>) -> Result<Value, Clove2Error> {
        match func {
            Value::Function(id) => self.call_function(*id, args),
            Value::NativeFunction(id) => {
                let Some(func) = self.native_functions.get(*id).copied() else {
                    return Err(Clove2Error::new("native function not found"));
                };
                let mut env = NativeEnv { eval: self };
                func(&mut env, args)
            }
            Value::Builtin(name) => self.eval_builtin_value(name, args),
            Value::Partial(partial) => {
                let mut call_args = partial.args.clone();
                call_args.extend(args);
                self.call_value(&partial.func, call_args)
            }
            Value::Set(items) => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("set expects 1 argument"));
                }
                let item = &args[0];
                if items.iter().any(|val| val == item) {
                    Ok(item.clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            Value::Map(map) => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("map expects 1 argument"));
                }
                let key = value_to_key(&args[0])?;
                Ok(map.get(&key).cloned().unwrap_or(Value::Nil))
            }
            Value::Regex(pattern) => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("regex expects 1 argument"));
                }
                let Value::Str(text) = &args[0] else {
                    return Err(Clove2Error::new("regex expects string"));
                };
                let regex = resolve_regex_value(&Value::Regex(pattern.clone()))?;
                let matched = regex.captures(text).and_then(|caps| {
                    caps.get(0).and_then(|full| {
                        if full.start() == 0 && full.end() == text.len() {
                            Some(capture_to_value(&caps))
                        } else {
                            None
                        }
                    })
                });
                Ok(matched.unwrap_or(Value::Nil))
            }
            _ => Err(Clove2Error::new("expected function")),
        }
    }

    fn eval_builtin_value(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if let Some(name) = name.strip_prefix("__foreign::") {
            return Err(Clove2Error::new(format!(
                "foreign call is not supported yet: {}",
                name
            )));
        }
        match name {
            "println" | "print" | "prn" => {
                let parts: Vec<String> = args
                    .iter()
                    .map(|value| {
                        if name == "prn" {
                            format_pr_value(value)
                        } else {
                            format_value(value)
                        }
                    })
                    .collect();
                let out = parts.join(" ");
                if name == "print" {
                    print!("{}", out);
                } else {
                    println!("{}", out);
                }
                Ok(Value::Nil)
            }
            "pr-str" => {
                let parts: Vec<String> = args.iter().map(format_pr_value).collect();
                Ok(Value::Str(parts.join(" ")))
            }
            "+" | "-" | "*" | "/" | "inc" | "dec" => self.eval_numeric_values(name, args),
            "mod" | "quot" | "rem" => self.eval_int_op_values(name, args),
            "bit-and" | "bit-or" | "bit-xor" | "bit-not" => self.eval_bit_op_values(name, args),
            "bit-shift-left" | "bit-shift-right" => self.eval_bit_shift_values(name, args),
            "=" | "!=" | "not=" | "<" | ">" | "<=" | ">=" => self.eval_compare_values(name, args),
            "compare" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("compare expects 2 arguments"));
                }
                let ord = compare_values_ord(&args[0], &args[1]);
                let out = match ord {
                    std::cmp::Ordering::Less => -1,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                };
                Ok(Value::Int(out))
            }
            "not" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("not expects 1 argument"));
                }
                Ok(Value::Bool(!is_truthy(&args[0])))
            }
            "nil?" | "some?" | "true?" | "false?" | "bool?" | "boolean?" | "int?" | "integer?"
            | "float?" | "number?" | "str?" | "string?" | "keyword?" | "symbol?" | "vec?"
            | "vector?" | "map?" | "fn?" | "coll?" | "sequential?" => {
                self.eval_pred_value(name, args)
            }
            "instance?" => self.eval_instance_values(args),
            "odd?" | "even?" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("odd?/even? expects 1 argument"));
                }
                let Value::Int(value) = args[0] else {
                    return Err(Clove2Error::new("odd?/even? expects int"));
                };
                let odd = name == "odd?";
                Ok(Value::Bool((value % 2 != 0) == odd))
            }
            "zero?" | "pos?" | "neg?" => self.eval_num_pred_values(name, args),
            "str" => {
                let mut out = String::new();
                for arg in &args {
                    out.push_str(&format_str_value(arg));
                }
                Ok(Value::Str(out))
            }
            "time" => self.eval_time_values(args),
            "bench" => self.eval_bench_values(args),
            "runtime-error" => self.eval_runtime_error_values(args),
            "repl" => {
                if args.len() > 1 {
                    return Err(Clove2Error::new("repl expects 0 or 1 argument"));
                }
                if args.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(args[0].clone())
                }
            }
            "debug" | "break" => Ok(Value::Nil),
            "int" => self.eval_cast_int_value(args),
            "float" => self.eval_cast_float_value(args),
            "bool" => self.eval_cast_bool_value(args),
            "keyword" => self.eval_cast_keyword_value(args),
            "symbol" => self.eval_cast_symbol_value(args),
            "name" => self.eval_name_value(args),
            "namespace" => self.eval_namespace_value(args),
            "gensym" => self.eval_gensym_values(args),
            "count" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("count expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => Ok(Value::Int(items.len() as i64)),
                    Value::List(items) => Ok(Value::Int(items.len() as i64)),
                    Value::Set(items) => Ok(Value::Int(items.len() as i64)),
                    Value::Map(map) => Ok(Value::Int(map.len() as i64)),
                    Value::Str(value) => Ok(Value::Int(value.chars().count() as i64)),
                    _ => Err(Clove2Error::new("count expects collection")),
                }
            }
            "range" => self.eval_range_values(args),
            "rand" => self.eval_rand_values(args),
            "rand-int" => self.eval_rand_int_values(args),
            "rand-nth" => self.eval_rand_nth_values(args),
            "identity" => self.eval_identity_values(args),
            "constantly" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("constantly expects 1 argument"));
                }
                Ok(self.make_constantly(args[0].clone()))
            }
            "partial" => self.eval_partial_values(args),
            "complement" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("complement expects 1 argument"));
                }
                Ok(self.make_complement(args[0].clone()))
            }
            "pipe" => {
                let funcs = args;
                Ok(self.make_pipe(funcs))
            }
            "comp" => {
                let funcs = args;
                Ok(self.make_comp(funcs))
            }
            "juxt" => {
                let funcs = args;
                Ok(self.make_juxt(funcs))
            }
            "__comp-call" => self.eval_comp_call_values(args),
            "__juxt-call" => self.eval_juxt_call_values(args),
            "every?" => self.eval_every_values(args),
            "not-every?" => self.eval_not_every_values(args),
            "not-any?" => self.eval_not_any_values(args),
            "some" => self.eval_some_values(args),
            "shuffle" => self.eval_shuffle_values(args),
            "sort" => self.eval_sort_values(args),
            "sort-by" => self.eval_sort_by_values(args),
            "distinct" => self.eval_distinct_values(args),
            "dedupe" => self.eval_dedupe_values(args),
            "group-by" => self.eval_group_by_values(args),
            "zip" => self.eval_zip_values(args),
            "zip-with" => self.eval_zip_with_values(args),
            "zipmap" => self.eval_zipmap_values(args),
            "interpose" => self.eval_interpose_values(args),
            "interleave" => self.eval_interleave_values(args),
            "flatten" => self.eval_flatten_values(args),
            "remove" => self.eval_remove_values(args),
            "dorun" => self.eval_dorun_values(args),
            "format" => self.eval_format_values(args),
            "abs" => self.eval_abs_value(args),
            "min" => self.eval_minmax_values("min", args),
            "max" => self.eval_minmax_values("max", args),
            "starts-with?" => self.eval_starts_ends_with_values("starts-with?", args),
            "ends-with?" => self.eval_starts_ends_with_values("ends-with?", args),
            "trim" => self.eval_trim_values("trim", args),
            "triml" => self.eval_trim_values("triml", args),
            "trimr" => self.eval_trim_values("trimr", args),
            "upper-case" => self.eval_upper_lower_values("upper-case", args),
            "lower-case" => self.eval_upper_lower_values("lower-case", args),
            "reverse-str" => self.eval_reverse_str_values(args),
            "split" => self.eval_split_values(args),
            "join" => self.eval_join_values(args),
            "blank?" => self.eval_blank_values(args),
            "replace" => self.eval_replace_values(args, false),
            "replace-first" => self.eval_replace_values(args, true),
            "re-find" => self.eval_re_find_values(args),
            "re-matches" => self.eval_re_matches_values(args),
            "re-seq" => self.eval_re_seq_values(args),
            "split-lines" => self.eval_split_lines_values(args, false),
            "lines" => self.eval_split_lines_values(args, true),
            "index-of" => self.eval_index_of_values(args, false),
            "last-index-of" => self.eval_index_of_values(args, true),
            "capitalize" => self.eval_capitalize_values(args),
            "trim-newline" => self.eval_trim_newline_values(args),
            "subs" => self.eval_subs_values(args),
            "slurp" => self.eval_slurp_values(args),
            "spit" => self.eval_spit_values(args),
            "fs::delete" => self.eval_fs_delete_values(args),
            "map" => self.eval_map_values(args),
            "map-indexed" => self.eval_map_indexed_values(args),
            "mapcat" => self.eval_mapcat_values(args),
            "filter" => self.eval_filter_values(args),
            "keep" => self.eval_keep_values(args),
            "keep-indexed" => self.eval_keep_indexed_values(args),
            "take-while" => self.eval_take_while_values(args),
            "drop-while" => self.eval_drop_while_values(args),
            "partition" => self.eval_partition_values(args, false),
            "partition-all" => self.eval_partition_values(args, true),
            "partition-by" => self.eval_partition_by_values(args),
            "reduce" => self.eval_reduce_values(args),
            "reduce-kv" => self.eval_reduce_kv_values(args),
            "take" => self.eval_take_values(args),
            "drop" => self.eval_drop_values(args),
            "take-last" => self.eval_take_last_values(args),
            "drop-last" => self.eval_drop_last_values(args),
            "reverse" => self.eval_reverse_values(args),
            "subvec" => self.eval_subvec_values(args),
            "concat" => self.eval_concat_values(args),
            "includes?" => self.eval_includes_values(args),
            "get-in" => self.eval_get_in_values(args),
            "assoc-in" => self.eval_assoc_in_values(args),
            "update" => self.eval_update_values(args),
            "update-in" => self.eval_update_in_values(args),
            "dissoc" => self.eval_dissoc_values(args),
            "empty?" => self.eval_empty_pred_values(args),
            "not-empty" => self.eval_not_empty_values(args),
            "json::read-file" => self.eval_json_read_file_values(args),
            "json::read-string" => self.eval_json_read_string_values(args),
            "json::write-string" => self.eval_json_write_string_values(args),
            "json::write-file" => self.eval_json_write_file_values(args),
            "expect" => self.eval_expect_values(args),
            "as" => self.eval_as_values(args),
            "seq" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("seq expects 1 argument"));
                }
                match &args[0] {
                    Value::Nil => Ok(Value::Nil),
                    Value::Vec(items) => {
                        if items.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::List(items.as_ref().clone()))
                        }
                    }
                    Value::List(items) => {
                        if items.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::List(items.clone()))
                        }
                    }
                    Value::Set(items) => {
                        if items.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::List(items.clone()))
                        }
                    }
                    Value::Str(text) => {
                        if text.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            let items = text.chars().map(|ch| Value::Str(ch.to_string())).collect();
                            Ok(Value::List(items))
                        }
                    }
                    Value::Map(map) => {
                        if map.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            let mut out = Vec::with_capacity(map.len());
                            for (key, value) in map.iter() {
                                out.push(Value::vec(vec![key_to_value(key), value.clone()]));
                            }
                            Ok(Value::List(out))
                        }
                    }
                    _ => Err(Clove2Error::new("seq expects collection")),
                }
            }
            "first" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("first expects 1 argument"));
                }
                let items = vec_items_ref(&args[0])
                    .ok_or_else(|| Clove2Error::new("first expects collection"))?;
                Ok(items.first().cloned().unwrap_or(Value::Nil))
            }
            "second" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("second expects 1 argument"));
                }
                let items = vec_items_ref(&args[0])
                    .ok_or_else(|| Clove2Error::new("second expects collection"))?;
                Ok(items.get(1).cloned().unwrap_or(Value::Nil))
            }
            "last" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("last expects 1 argument"));
                }
                let items = vec_items_ref(&args[0])
                    .ok_or_else(|| Clove2Error::new("last expects collection"))?;
                Ok(items.last().cloned().unwrap_or(Value::Nil))
            }
            "peek" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("peek expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
                    Value::List(items) => Ok(items.first().cloned().unwrap_or(Value::Nil)),
                    Value::Set(items) => Ok(items.last().cloned().unwrap_or(Value::Nil)),
                    _ => Err(Clove2Error::new("peek expects collection")),
                }
            }
            "rest" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("rest expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => {
                        let rest = if items.len() <= 1 {
                            Vec::new()
                        } else {
                            items[1..].to_vec()
                        };
                        Ok(Value::vec(rest))
                    }
                    Value::List(items) => {
                        let rest = if items.len() <= 1 {
                            Vec::new()
                        } else {
                            items[1..].to_vec()
                        };
                        Ok(Value::List(rest))
                    }
                    _ => Err(Clove2Error::new("rest expects collection")),
                }
            }
            "butlast" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("butlast expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => {
                        if items.len() <= 1 {
                            Ok(Value::vec(Vec::new()))
                        } else {
                            Ok(Value::vec(items[..items.len() - 1].to_vec()))
                        }
                    }
                    Value::List(items) => {
                        if items.len() <= 1 {
                            Ok(Value::List(Vec::new()))
                        } else {
                            Ok(Value::List(items[..items.len() - 1].to_vec()))
                        }
                    }
                    _ => Err(Clove2Error::new("butlast expects collection")),
                }
            }
            "next" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("next expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => {
                        if items.len() <= 1 {
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::List(items[1..].to_vec()))
                        }
                    }
                    Value::List(items) => {
                        if items.len() <= 1 {
                            Ok(Value::Nil)
                        } else {
                            Ok(Value::List(items[1..].to_vec()))
                        }
                    }
                    _ => Err(Clove2Error::new("next expects collection")),
                }
            }
            "pop" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("pop expects 1 argument"));
                }
                match &args[0] {
                    Value::Vec(items) => {
                        if items.is_empty() {
                            return Err(Clove2Error::new("pop expects non-empty vector"));
                        }
                        Ok(Value::vec(items[..items.len() - 1].to_vec()))
                    }
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err(Clove2Error::new("pop expects non-empty list"));
                        }
                        Ok(Value::List(items[1..].to_vec()))
                    }
                    _ => Err(Clove2Error::new("pop expects collection")),
                }
            }
            "empty" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("empty expects 1 argument"));
                }
                match &args[0] {
                    Value::Nil => Ok(Value::Nil),
                    Value::Vec(_) => Ok(Value::vec(Vec::new())),
                    Value::List(_) => Ok(Value::List(Vec::new())),
                    Value::Set(_) => Ok(Value::Set(Vec::new())),
                    Value::Map(_) => Ok(Value::map(BTreeMap::new())),
                    Value::Str(_) => Ok(Value::Str(String::new())),
                    _ => Err(Clove2Error::new("empty expects collection")),
                }
            }
            "apply" => {
                if args.len() < 2 {
                    return Err(Clove2Error::new("apply expects at least 2 arguments"));
                }
                let func = args
                    .first()
                    .ok_or_else(|| Clove2Error::new("apply expects function"))?;
                let tail = args
                    .last()
                    .ok_or_else(|| Clove2Error::new("apply expects args"))?;
                let items = values_to_vec_items(tail, "apply")?;
                let mut call_args = Vec::new();
                for arg in args.iter().skip(1).take(args.len() - 2) {
                    call_args.push(arg.clone());
                }
                call_args.extend(items);
                self.call_value(func, call_args)
            }
            "merge" => {
                if args.is_empty() {
                    return Err(Clove2Error::new("merge expects at least 1 argument"));
                }
                let mut out = BTreeMap::new();
                for arg in args {
                    let Value::Map(map) = arg else {
                        return Err(Clove2Error::new("merge expects map"));
                    };
                    for (key, val) in map.iter() {
                        out.insert(key.clone(), val.clone());
                    }
                }
                Ok(Value::map(out))
            }
            "merge-with" => self.eval_merge_with_values(args),
            "contains?" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("contains? expects 2 arguments"));
                }
                match &args[0] {
                    Value::Map(map) => {
                        let key = value_to_key(&args[1])?;
                        Ok(Value::Bool(map.contains_key(&key)))
                    }
                    Value::Vec(items) => {
                        let idx = match args[1] {
                            Value::Int(value) if value >= 0 => value as usize,
                            _ => return Err(Clove2Error::new("contains? expects Int index")),
                        };
                        Ok(Value::Bool(idx < items.len()))
                    }
                    Value::Set(items) => Ok(Value::Bool(items.iter().any(|v| v == &args[1]))),
                    _ => Err(Clove2Error::new("contains? expects map or vector")),
                }
            }
            "conj" => {
                if args.len() < 2 {
                    return Err(Clove2Error::new("conj expects at least 2 arguments"));
                }
                let mut iter = args.into_iter();
                let first = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("conj expects collection"))?;
                match first {
                    Value::Vec(mut items) => {
                        let items_ref = self.vec_for_update(&mut items, "conj")?;
                        for item in iter {
                            items_ref.push(item);
                        }
                        Ok(Value::Vec(items))
                    }
                    Value::List(mut items) => {
                        for item in iter {
                            items.insert(0, item);
                        }
                        Ok(Value::List(items))
                    }
                    Value::Set(mut items) => {
                        for item in iter {
                            if !items.iter().any(|existing| existing == &item) {
                                items.push(item);
                            }
                        }
                        Ok(Value::Set(items))
                    }
                    _ => Err(Clove2Error::new("conj expects collection")),
                }
            }
            "cons" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("cons expects 2 arguments"));
                }
                let mut iter = args.into_iter();
                let item = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("cons expects item"))?;
                let coll = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("cons expects collection"))?;
                match coll {
                    Value::Vec(mut items) => {
                        let items_ref = self.vec_for_update(&mut items, "cons")?;
                        items_ref.insert(0, item);
                        Ok(Value::List(items_ref.clone()))
                    }
                    Value::List(mut items) => {
                        items.insert(0, item);
                        Ok(Value::List(items))
                    }
                    _ => Err(Clove2Error::new("cons expects collection")),
                }
            }
            "list" => Ok(Value::List(args)),
            "vector" => Ok(Value::vec(args)),
            "hash-map" => self.eval_hash_map_values(args),
            "vec" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("vec expects 1 argument"));
                }
                match args[0].clone() {
                    Value::Vec(items) => Ok(Value::Vec(items)),
                    Value::List(items) => Ok(Value::vec(items)),
                    Value::Set(items) => Ok(Value::vec(items)),
                    Value::Map(map) => {
                        let mut out = Vec::with_capacity(map.len());
                        for (key, value) in map.iter() {
                            out.push(Value::vec(vec![key_to_value(key), value.clone()]));
                        }
                        Ok(Value::vec(out))
                    }
                    Value::Str(text) => Ok(Value::vec(
                        text.chars().map(|ch| Value::Str(ch.to_string())).collect(),
                    )),
                    _ => Err(Clove2Error::new("vec expects collection")),
                }
            }
            "into" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("into expects 2 arguments"));
                }
                let mut iter = args.into_iter();
                let target = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("into expects 2 arguments"))?;
                let source = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("into expects 2 arguments"))?;
                match target {
                    Value::Vec(mut items) => {
                        let values = values_to_vec_items(&source, "into")?;
                        let items_ref = self.vec_for_update(&mut items, "into")?;
                        items_ref.extend(values);
                        Ok(Value::Vec(items))
                    }
                    Value::List(mut items) => {
                        let values = values_to_vec_items(&source, "into")?;
                        items.extend(values);
                        Ok(Value::List(items))
                    }
                    Value::Set(mut items) => {
                        let values = values_to_vec_items(&source, "into")?;
                        for value in values {
                            if !items.iter().any(|existing| existing == &value) {
                                items.push(value);
                            }
                        }
                        Ok(Value::Set(items))
                    }
                    Value::Map(mut map) => {
                        let values = values_to_vec_items(&source, "into")?;
                        let map_ref = self.map_for_update(&mut map, "into")?;
                        for value in values {
                            let Value::Vec(pair) = value else {
                                return Err(Clove2Error::new("into expects [key value] pairs"));
                            };
                            let pair = pair.as_ref();
                            if pair.len() != 2 {
                                return Err(Clove2Error::new("into expects [key value] pairs"));
                            }
                            let key = value_to_key(&pair[0])?;
                            map_ref.insert(key, pair[1].clone());
                        }
                        Ok(Value::Map(map))
                    }
                    _ => Err(Clove2Error::new("into expects collection target")),
                }
            }
            "repeat" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("repeat expects 2 arguments"));
                }
                let Value::Int(count) = args[0] else {
                    return Err(Clove2Error::new("repeat expects Int count"));
                };
                let count = if count < 0 { 0 } else { count as usize };
                let mut out = Vec::with_capacity(count);
                for _ in 0..count {
                    out.push(args[1].clone());
                }
                Ok(Value::vec(out))
            }
            "repeatedly" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("repeatedly expects 2 arguments"));
                }
                let Value::Int(count) = args[0] else {
                    return Err(Clove2Error::new("repeatedly expects Int count"));
                };
                let count = if count < 0 { 0 } else { count as usize };
                let func = args
                    .get(1)
                    .ok_or_else(|| Clove2Error::new("repeatedly expects function"))?;
                let mut out = Vec::with_capacity(count);
                for _ in 0..count {
                    out.push(self.call_value(func, Vec::new())?);
                }
                Ok(Value::vec(out))
            }
            "iterate" => self.eval_iterate_values(args),
            "assoc" => {
                if args.len() < 3 || (args.len() - 1) % 2 != 0 {
                    return Err(Clove2Error::new("assoc expects map and key/value pairs"));
                }
                let mut iter = args.into_iter();
                let target = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("assoc expects map and key/value pairs"))?;
                let rest: Vec<Value> = iter.collect();
                match target {
                    Value::Map(mut map) => {
                        let map_ref = self.map_for_update(&mut map, "assoc")?;
                        for pair in rest.chunks(2) {
                            let key = value_to_key(&pair[0])?;
                            map_ref.insert(key, pair[1].clone());
                        }
                        Ok(Value::Map(map))
                    }
                    Value::Vec(mut items) => {
                        let items_ref = self.vec_for_update(&mut items, "assoc")?;
                        for pair in rest.chunks(2) {
                            let Value::Int(idx) = pair[0] else {
                                return Err(Clove2Error::new("assoc expects Int index for vector"));
                            };
                            if idx < 0 {
                                return Err(Clove2Error::new("assoc expects non-negative index"));
                            }
                            let idx = idx as usize;
                            if idx < items_ref.len() {
                                items_ref[idx] = pair[1].clone();
                            } else if idx == items_ref.len() {
                                items_ref.push(pair[1].clone());
                            } else {
                                return Err(Clove2Error::new("assoc index out of range"));
                            }
                        }
                        Ok(Value::Vec(items))
                    }
                    _ => Err(Clove2Error::new("assoc expects map or vector")),
                }
            }
            "keys" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("keys expects 1 argument"));
                }
                let Value::Map(map) = &args[0] else {
                    return Err(Clove2Error::new("keys expects map"));
                };
                Ok(Value::vec(map.keys().map(key_to_value).collect()))
            }
            "vals" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("vals expects 1 argument"));
                }
                let Value::Map(map) = &args[0] else {
                    return Err(Clove2Error::new("vals expects map"));
                };
                Ok(Value::vec(map.values().cloned().collect()))
            }
            "select-keys" => {
                if args.len() != 2 {
                    return Err(Clove2Error::new("select-keys expects map and keys"));
                }
                let Value::Map(map) = &args[0] else {
                    return Err(Clove2Error::new("select-keys expects map"));
                };
                let Value::Vec(keys) = &args[1] else {
                    return Err(Clove2Error::new("select-keys expects vector keys"));
                };
                let mut out = BTreeMap::new();
                for key in keys.as_ref().iter() {
                    let key = value_to_key(key)?;
                    if let Some(value) = map.get(&key) {
                        out.insert(key, value.clone());
                    }
                }
                Ok(Value::map(out))
            }
            "frequencies" => self.eval_frequencies_values(args),
            "get" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(Clove2Error::new("get expects 2 or 3 arguments"));
                }
                match &args[0] {
                    Value::Map(map) => {
                        let key = value_to_key(&args[1])?;
                        if let Some(value) = map.get(&key) {
                            Ok(value.clone())
                        } else if args.len() == 3 {
                            Ok(args[2].clone())
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    Value::Vec(items) => {
                        let idx = match args[1] {
                            Value::Int(value) if value >= 0 => value as usize,
                            _ => return Err(Clove2Error::new("get expects Int index")),
                        };
                        if idx < items.len() {
                            Ok(items[idx].clone())
                        } else if args.len() == 3 {
                            Ok(args[2].clone())
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    _ => Err(Clove2Error::new("get expects map or vector")),
                }
            }
            "nth" => {
                if args.len() < 2 || args.len() > 3 {
                    return Err(Clove2Error::new("nth expects 2 or 3 arguments"));
                }
                let items = values_to_vec_items(&args[0], "nth")?;
                let default_val = if args.len() == 3 {
                    Some(args[2].clone())
                } else {
                    None
                };
                match &args[1] {
                    Value::Int(value) if *value >= 0 => {
                        let idx = *value as usize;
                        if idx < items.len() {
                            Ok(items[idx].clone())
                        } else {
                            Ok(default_val.unwrap_or(Value::Nil))
                        }
                    }
                    Value::Vec(indices) => {
                        let mut out = Vec::with_capacity(indices.len());
                        for idx_val in indices.as_ref().iter() {
                            let Value::Int(value) = idx_val else {
                                return Err(Clove2Error::new("nth expects non-negative Int index"));
                            };
                            if *value < 0 {
                                return Err(Clove2Error::new("nth expects non-negative Int index"));
                            }
                            let idx = *value as usize;
                            if idx < items.len() {
                                out.push(items[idx].clone());
                            } else {
                                out.push(default_val.clone().unwrap_or(Value::Nil));
                            }
                        }
                        Ok(Value::vec(out))
                    }
                    Value::List(indices) => {
                        let mut out = Vec::with_capacity(indices.len());
                        for idx_val in indices.iter() {
                            let Value::Int(value) = idx_val else {
                                return Err(Clove2Error::new("nth expects non-negative Int index"));
                            };
                            if *value < 0 {
                                return Err(Clove2Error::new("nth expects non-negative Int index"));
                            }
                            let idx = *value as usize;
                            if idx < items.len() {
                                out.push(items[idx].clone());
                            } else {
                                out.push(default_val.clone().unwrap_or(Value::Nil));
                            }
                        }
                        Ok(Value::vec(out))
                    }
                    _ => Err(Clove2Error::new("nth expects non-negative Int index")),
                }
            }
            _ => Err(Clove2Error::new("builtin cannot be used as value")),
        }
    }

    fn make_constantly(&mut self, value: Value) -> Value {
        let name = format!("__constantly_{}", self.functions.len());
        let env = Env::new();
        env.set(&name, value);
        let params = vec![Param {
            name: "__args".to_string(),
            ty: None,
            rest: true,
        }];
        let body = vec![AstExpr::Symbol(name)];
        let func_id = self.define_function(&params, &body, env);
        Value::Function(func_id)
    }

    fn make_partial(&mut self, func: Value, pre_args: Vec<Value>) -> Value {
        let desc = partial_desc(&func, &pre_args);
        Value::Partial(Box::new(crate::value::PartialValue {
            func,
            args: pre_args,
            desc,
        }))
    }

    fn make_complement(&mut self, func: Value) -> Value {
        let func_name = format!("__complement_func_{}", self.functions.len());
        let rest_name = "__args".to_string();
        let env = Env::new();
        env.set(&func_name, func);

        let apply_call = AstExpr::Call {
            callee: Box::new(AstExpr::Symbol("apply".to_string())),
            args: vec![
                AstExpr::Symbol(func_name),
                AstExpr::Symbol(rest_name.clone()),
            ],
        };
        let not_call = AstExpr::Call {
            callee: Box::new(AstExpr::Symbol("not".to_string())),
            args: vec![apply_call],
        };
        let params = vec![Param {
            name: rest_name,
            ty: None,
            rest: true,
        }];
        let body = vec![not_call];
        let func_id = self.define_function(&params, &body, env);
        Value::Function(func_id)
    }

    fn make_comp(&mut self, funcs: Vec<Value>) -> Value {
        if funcs.is_empty() {
            return Value::Builtin("identity".to_string());
        }
        if funcs.len() == 1 {
            return funcs[0].clone();
        }
        let funcs_name = format!("__comp_funcs_{}", self.functions.len());
        let rest_name = "__args".to_string();
        let env = Env::new();
        env.set(&funcs_name, Value::vec(funcs));

        let call = AstExpr::Call {
            callee: Box::new(AstExpr::Symbol("__comp-call".to_string())),
            args: vec![
                AstExpr::Symbol(funcs_name),
                AstExpr::Symbol(rest_name.clone()),
            ],
        };
        let params = vec![Param {
            name: rest_name,
            ty: None,
            rest: true,
        }];
        let body = vec![call];
        let func_id = self.define_function(&params, &body, env);
        Value::Function(func_id)
    }

    fn make_pipe(&mut self, funcs: Vec<Value>) -> Value {
        let reversed: Vec<Value> = funcs.into_iter().rev().collect();
        self.make_comp(reversed)
    }

    fn make_juxt(&mut self, funcs: Vec<Value>) -> Value {
        if funcs.is_empty() {
            return self.make_constantly(Value::vec(Vec::new()));
        }
        let funcs_name = format!("__juxt_funcs_{}", self.functions.len());
        let rest_name = "__args".to_string();
        let env = Env::new();
        env.set(&funcs_name, Value::vec(funcs));

        let call = AstExpr::Call {
            callee: Box::new(AstExpr::Symbol("__juxt-call".to_string())),
            args: vec![
                AstExpr::Symbol(funcs_name),
                AstExpr::Symbol(rest_name.clone()),
            ],
        };
        let params = vec![Param {
            name: rest_name,
            ty: None,
            rest: true,
        }];
        let body = vec![call];
        let func_id = self.define_function(&params, &body, env);
        Value::Function(func_id)
    }

    fn eval_numeric_values(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return match name {
                "+" => Ok(Value::Int(0)),
                "*" => Ok(Value::Int(1)),
                "-" => Ok(Value::Int(0)),
                "/" => Ok(Value::Int(1)),
                _ => Err(Clove2Error::new("numeric op expects arguments")),
            };
        }
        let has_float = args.iter().any(|v| matches!(v, Value::Float(_)));
        if name == "inc" || name == "dec" {
            if args.len() != 1 {
                return Err(Clove2Error::new("inc/dec expects 1 argument"));
            }
        }
        let mut acc = match args.first().unwrap() {
            Value::Int(v) => *v as f64,
            Value::Float(v) => *v,
            other => {
                return Err(Clove2Error::new(format!(
                    "numeric op expects number, got {}",
                    format_value(other)
                )))
            }
        };
        if name == "inc" {
            acc += 1.0;
        } else if name == "dec" {
            acc -= 1.0;
        } else if args.len() == 1 {
            match name {
                "-" => acc = -acc,
                "/" => acc = 1.0 / acc,
                _ => {}
            }
        } else {
            for value in args.iter().skip(1) {
                let next = match value {
                    Value::Int(v) => *v as f64,
                    Value::Float(v) => *v,
                    other => {
                        return Err(Clove2Error::new(format!(
                            "numeric op expects number, got {}",
                            format_value(other)
                        )))
                    }
                };
                match name {
                    "+" => acc += next,
                    "-" => acc -= next,
                    "*" => acc *= next,
                    "/" => acc /= next,
                    _ => {}
                }
            }
        }
        if name == "/" {
            if has_float {
                Ok(Value::Float(acc))
            } else if acc.fract() == 0.0 {
                Ok(Value::Int(acc as i64))
            } else {
                Ok(Value::Float(acc))
            }
        } else if has_float {
            Ok(Value::Float(acc))
        } else {
            Ok(Value::Int(acc as i64))
        }
    }

    fn eval_int_op_values(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new(format!("{} expects 2 arguments", name)));
        }
        let Value::Int(left) = args[0] else {
            return Err(Clove2Error::new(format!("{} expects int arguments", name)));
        };
        let Value::Int(right) = args[1] else {
            return Err(Clove2Error::new(format!("{} expects int arguments", name)));
        };
        if right == 0 {
            return Err(Clove2Error::new(format!(
                "{} expects non-zero divisor",
                name
            )));
        }
        let out = match name {
            "mod" => ((left % right) + right) % right,
            "quot" => left / right,
            "rem" => left % right,
            _ => return Err(Clove2Error::new(format!("unknown int op: {}", name))),
        };
        Ok(Value::Int(out))
    }

    fn eval_bit_op_values(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        match name {
            "bit-not" => {
                if args.len() != 1 {
                    return Err(Clove2Error::new("bit-not expects 1 argument"));
                }
                let Value::Int(value) = args[0] else {
                    return Err(Clove2Error::new("bit-not expects int"));
                };
                Ok(Value::Int(!value))
            }
            "bit-and" | "bit-or" | "bit-xor" => {
                if args.is_empty() {
                    return Ok(Value::Int(match name {
                        "bit-and" => -1,
                        "bit-or" => 0,
                        "bit-xor" => 0,
                        _ => 0,
                    }));
                }
                let mut iter = args.into_iter();
                let Value::Int(mut acc) = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("bit op expects args"))?
                else {
                    return Err(Clove2Error::new(format!("{} expects int arguments", name)));
                };
                for arg in iter {
                    let Value::Int(value) = arg else {
                        return Err(Clove2Error::new(format!("{} expects int arguments", name)));
                    };
                    match name {
                        "bit-and" => acc &= value,
                        "bit-or" => acc |= value,
                        "bit-xor" => acc ^= value,
                        _ => {}
                    }
                }
                Ok(Value::Int(acc))
            }
            _ => Err(Clove2Error::new("unknown bit op")),
        }
    }

    fn eval_bit_shift_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new(format!("{} expects 2 arguments", name)));
        }
        let Value::Int(value) = args[0] else {
            return Err(Clove2Error::new(format!("{} expects int arguments", name)));
        };
        let Value::Int(shift) = args[1] else {
            return Err(Clove2Error::new(format!("{} expects int arguments", name)));
        };
        if shift < 0 || shift > 63 {
            return Err(Clove2Error::new(format!("{} expects shift in 0..63", name)));
        }
        let shift = shift as u32;
        let out = match name {
            "bit-shift-left" => value << shift,
            "bit-shift-right" => value >> shift,
            _ => return Err(Clove2Error::new("unknown bit shift op")),
        };
        Ok(Value::Int(out))
    }

    fn eval_num_pred_values(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let number = match args[0] {
            Value::Int(value) => value as f64,
            Value::Float(value) => value,
            _ => return Err(Clove2Error::new(format!("{} expects number", name))),
        };
        let ok = match name {
            "zero?" => number == 0.0,
            "pos?" => number > 0.0,
            "neg?" => number < 0.0,
            _ => false,
        };
        Ok(Value::Bool(ok))
    }

    fn eval_pred_value(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let value = &args[0];
        let ok = match name {
            "nil?" => matches!(value, Value::Nil),
            "some?" => !matches!(value, Value::Nil),
            "true?" => matches!(value, Value::Bool(true)),
            "false?" => matches!(value, Value::Bool(false)),
            "bool?" | "boolean?" => matches!(value, Value::Bool(_)),
            "int?" | "integer?" => matches!(value, Value::Int(_)),
            "float?" => matches!(value, Value::Float(_)),
            "number?" => matches!(value, Value::Int(_) | Value::Float(_)),
            "str?" | "string?" => matches!(value, Value::Str(_)),
            "keyword?" => matches!(value, Value::Keyword(_)),
            "symbol?" => matches!(value, Value::Symbol(_)),
            "vec?" | "vector?" => matches!(value, Value::Vec(_)),
            "map?" => matches!(value, Value::Map(_)),
            "fn?" => matches!(
                value,
                Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_)
            ),
            "coll?" => matches!(
                value,
                Value::Vec(_) | Value::List(_) | Value::Set(_) | Value::Map(_) | Value::Str(_)
            ),
            "sequential?" => matches!(value, Value::Vec(_) | Value::List(_)),
            _ => return Err(Clove2Error::new(format!("unknown predicate: {}", name))),
        };
        Ok(Value::Bool(ok))
    }

    fn eval_instance_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("instance? expects 2 arguments"));
        }
        let type_name = normalize_type_name(&args[0])?;
        Ok(Value::Bool(matches_type_name(&type_name, &args[1])))
    }

    fn eval_compare_values(&mut self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Ok(Value::Bool(true));
        }
        let mut ok = true;
        for pair in args.windows(2) {
            let left = &pair[0];
            let right = &pair[1];
            ok &= compare_values(name, left, right)?;
        }
        Ok(Value::Bool(ok))
    }

    fn eval_range_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("range expects 1 to 3 arguments"));
        }
        let (start, end, step) = if args.len() == 1 {
            (0, value_to_int(&args[0])?, 1)
        } else if args.len() == 2 {
            (value_to_int(&args[0])?, value_to_int(&args[1])?, 1)
        } else {
            (
                value_to_int(&args[0])?,
                value_to_int(&args[1])?,
                value_to_int(&args[2])?,
            )
        };
        if step == 0 {
            return Err(Clove2Error::new("range expects non-zero step"));
        }
        let mut out = Vec::new();
        if step > 0 {
            let mut value = start;
            while value < end {
                out.push(Value::Int(value));
                value += step;
            }
        } else {
            let mut value = start;
            while value > end {
                out.push(Value::Int(value));
                value += step;
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_rand_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        let mut rng = rand::thread_rng();
        match args.len() {
            0 => Ok(Value::Float(rng.gen::<f64>())),
            1 => {
                let upper = value_to_number(&args[0], "rand")?;
                Ok(Value::Float(rng.gen::<f64>() * upper))
            }
            _ => Err(Clove2Error::new(
                "rand expects zero or one numeric argument",
            )),
        }
    }

    fn eval_rand_int_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("rand-int expects 1 argument"));
        }
        let Value::Int(upper) = args[0] else {
            return Err(Clove2Error::new("rand-int expects Int argument"));
        };
        if upper <= 0 {
            return Err(Clove2Error::new("rand-int expects positive Int"));
        }
        let mut rng = rand::thread_rng();
        Ok(Value::Int(rng.gen_range(0..upper)))
    }

    fn eval_rand_nth_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("rand-nth expects 1 argument"));
        }
        let mut rng = rand::thread_rng();
        match &args[0] {
            Value::Vec(items) => {
                if items.is_empty() {
                    return Err(Clove2Error::new("rand-nth expects non-empty vector"));
                }
                let idx = rng.gen_range(0..items.len());
                Ok(items[idx].clone())
            }
            Value::Str(text) => {
                let chars: Vec<char> = text.chars().collect();
                if chars.is_empty() {
                    return Err(Clove2Error::new("rand-nth expects non-empty string"));
                }
                let idx = rng.gen_range(0..chars.len());
                Ok(Value::Str(chars[idx].to_string()))
            }
            _ => Err(Clove2Error::new("rand-nth expects vector or string")),
        }
    }

    fn eval_format_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("format expects at least 1 argument"));
        }
        let Value::Str(template) = &args[0] else {
            return Err(Clove2Error::new("format expects string template"));
        };
        let mut out = String::new();
        let mut idx = 0;
        let mut arg_idx = 1;
        while let Some(pos) = template[idx..].find('{') {
            let abs = idx + pos;
            out.push_str(&template[idx..abs]);
            if template[abs..].starts_with("{}") {
                if arg_idx >= args.len() {
                    return Err(Clove2Error::new("format placeholder count mismatch"));
                }
                out.push_str(&format_value(&args[arg_idx]));
                arg_idx += 1;
                idx = abs + 2;
            } else if template[abs..].starts_with("{:?}") {
                if arg_idx >= args.len() {
                    return Err(Clove2Error::new("format placeholder count mismatch"));
                }
                out.push_str(&format_value(&args[arg_idx]));
                arg_idx += 1;
                idx = abs + 4;
            } else {
                return Err(Clove2Error::new("format expects {} or {:?} placeholders"));
            }
        }
        out.push_str(&template[idx..]);
        if arg_idx != args.len() {
            return Err(Clove2Error::new("format placeholder count mismatch"));
        }
        Ok(Value::Str(out))
    }

    fn eval_identity_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("identity expects 1 argument"));
        }
        Ok(args[0].clone())
    }

    fn eval_partial_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("partial expects at least 1 argument"));
        }
        let func = args[0].clone();
        let pre_args = args.into_iter().skip(1).collect::<Vec<_>>();
        Ok(self.make_partial(func, pre_args))
    }

    fn eval_comp_call_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("__comp-call expects 2 arguments"));
        }
        let Value::Vec(funcs) = &args[0] else {
            return Err(Clove2Error::new("__comp-call expects function vector"));
        };
        let Value::Vec(call_args) = &args[1] else {
            return Err(Clove2Error::new("__comp-call expects arg vector"));
        };
        if funcs.is_empty() {
            return Ok(call_args.first().cloned().unwrap_or(Value::Nil));
        }
        let call_args = call_args.as_ref().clone();
        let mut acc = self.call_value(funcs.last().unwrap(), call_args.clone())?;
        for func in funcs.iter().rev().skip(1) {
            acc = self.call_value(func, vec![acc])?;
        }
        Ok(acc)
    }

    fn eval_juxt_call_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("__juxt-call expects 2 arguments"));
        }
        let Value::Vec(funcs) = &args[0] else {
            return Err(Clove2Error::new("__juxt-call expects function vector"));
        };
        let Value::Vec(call_args) = &args[1] else {
            return Err(Clove2Error::new("__juxt-call expects arg vector"));
        };
        let call_args = call_args.as_ref().clone();
        let mut out = Vec::with_capacity(funcs.len());
        for func in funcs.iter() {
            out.push(self.call_value(func, call_args.clone())?);
        }
        Ok(Value::vec(out))
    }

    fn eval_every_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("every? expects 2 arguments"));
        }
        let func = &args[0];
        let items = values_to_vec_items(&args[1], "every?")?;
        for item in items {
            let value = self.call_value(func, vec![item.clone()])?;
            if !is_truthy(&value) {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }

    fn eval_not_every_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("not-every? expects 2 arguments"));
        }
        let func = &args[0];
        let items = values_to_vec_items(&args[1], "not-every?")?;
        for item in items {
            let value = self.call_value(func, vec![item.clone()])?;
            if !is_truthy(&value) {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    }

    fn eval_remove_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("remove expects 2 arguments"));
        }
        let func = &args[0];
        let items = values_to_vec_items(&args[1], "remove")?;
        let mut out = Vec::new();
        for item in items {
            let keep = self.call_value(func, vec![item.clone()])?;
            if !is_truthy(&keep) {
                out.push(item.clone());
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_dorun_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        let (limit, coll) = match args.as_slice() {
            [coll] => (None, coll),
            [Value::Int(limit), coll] => {
                let limit = if *limit < 0 { 0 } else { *limit as usize };
                (Some(limit), coll)
            }
            [other, _] => {
                return Err(Clove2Error::new(format!(
                    "dorun expects Int count, got {}",
                    value_type_name(other)
                )))
            }
            _ => return Err(Clove2Error::new("dorun expects 1 or 2 arguments")),
        };
        let items = values_to_vec_items(coll, "dorun")?;
        if let Some(limit) = limit {
            for _ in items.into_iter().take(limit) {}
        } else {
            for _ in items {}
        }
        Ok(Value::Nil)
    }

    fn eval_not_any_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("not-any? expects 2 arguments"));
        }
        let func = &args[0];
        let items = values_to_vec_items(&args[1], "not-any?")?;
        for item in items {
            let value = self.call_value(func, vec![item.clone()])?;
            if is_truthy(&value) {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    }

    fn eval_merge_with_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("merge-with expects at least 2 arguments"));
        }
        let func = args[0].clone();
        let mut out: BTreeMap<Key, Value> = BTreeMap::new();
        for arg in args.into_iter().skip(1) {
            let Value::Map(map) = arg else {
                return Err(Clove2Error::new("merge-with expects map"));
            };
            for (key, val) in map.iter() {
                if let Some(current) = out.get(key) {
                    let merged = self.call_value(&func, vec![current.clone(), val.clone()])?;
                    out.insert(key.clone(), merged);
                } else {
                    out.insert(key.clone(), val.clone());
                }
            }
        }
        Ok(Value::map(out))
    }

    fn eval_some_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("some expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("some expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("some expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "some")?;
        for item in items {
            let value = self.call_value(&func, vec![item])?;
            if is_truthy(&value) {
                return Ok(value);
            }
        }
        Ok(Value::Nil)
    }

    fn eval_shuffle_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("shuffle expects 1 argument"));
        }
        let mut rng = rand::thread_rng();
        match &args[0] {
            Value::Vec(items) => {
                let mut out = items.as_ref().clone();
                out.shuffle(&mut rng);
                Ok(Value::vec(out))
            }
            Value::Str(text) => {
                let mut chars: Vec<char> = text.chars().collect();
                chars.shuffle(&mut rng);
                Ok(Value::Str(chars.into_iter().collect()))
            }
            _ => Err(Clove2Error::new("shuffle expects vector or string")),
        }
    }

    fn eval_frequencies_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("frequencies expects 1 argument"));
        }
        let mut out = BTreeMap::new();
        let mut iter = args.into_iter();
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("frequencies expects 1 argument"))?;
        let items = into_vec_items(vec_val, "frequencies")?;
        for item in items {
            let key = value_to_key(&item)?;
            let count = out
                .get(&key)
                .and_then(|value| match value {
                    Value::Int(count) => Some(*count),
                    _ => None,
                })
                .unwrap_or(0);
            out.insert(key, Value::Int(count + 1));
        }
        Ok(Value::map(out))
    }

    fn eval_hash_map_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() % 2 != 0 {
            return Err(Clove2Error::new("hash-map expects even arguments"));
        }
        let mut out = BTreeMap::new();
        for pair in args.chunks(2) {
            let key = value_to_key(&pair[0])?;
            out.insert(key, pair[1].clone());
        }
        Ok(Value::map(out))
    }

    fn eval_partition_values(
        &self,
        args: Vec<Value>,
        allow_incomplete: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("partition expects 2 or 3 arguments"));
        }
        let args_len = args.len();
        let mut iter = args.into_iter();
        let size_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("partition expects size"))?;
        let Value::Int(size) = size_val else {
            return Err(Clove2Error::new("partition expects Int size"));
        };
        if size <= 0 {
            return Err(Clove2Error::new("partition expects positive size"));
        }
        let (step, coll) = if args_len == 3 {
            let step_val = iter
                .next()
                .ok_or_else(|| Clove2Error::new("partition expects step"))?;
            let Value::Int(step) = step_val else {
                return Err(Clove2Error::new("partition expects Int step"));
            };
            if step <= 0 {
                return Err(Clove2Error::new("partition expects positive step"));
            }
            (
                step as usize,
                iter.next()
                    .ok_or_else(|| Clove2Error::new("partition expects collection"))?,
            )
        } else {
            (
                size as usize,
                iter.next()
                    .ok_or_else(|| Clove2Error::new("partition expects collection"))?,
            )
        };
        let size = size as usize;
        let items: Vec<Value> = match coll {
            Value::Vec(items) => match std::rc::Rc::try_unwrap(items) {
                Ok(items) => items,
                Err(items) => items.as_ref().clone(),
            },
            Value::Str(text) => text.chars().map(|ch| Value::Str(ch.to_string())).collect(),
            _ => return Err(Clove2Error::new("partition expects vector or string")),
        };
        let mut out = Vec::new();
        let mut idx = 0;
        while idx < items.len() {
            let end = idx + size;
            if end > items.len() {
                if allow_incomplete {
                    let chunk = items[idx..].to_vec();
                    out.push(Value::vec(chunk));
                }
                break;
            }
            let chunk = items[idx..end].to_vec();
            out.push(Value::vec(chunk));
            idx += step;
        }
        Ok(Value::vec(out))
    }

    fn eval_sort_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("sort expects 1 or 2 arguments"));
        }
        let args_len = args.len();
        let mut iter = args.into_iter();
        let (cmp, coll) = if args_len == 2 {
            let cmp = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort expects comparator"))?;
            let coll = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort expects collection"))?;
            (Some(cmp), coll)
        } else {
            let coll = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort expects collection"))?;
            (None, coll)
        };
        if cmp.is_none() {
            if let Value::Str(text) = coll {
                let mut chars: Vec<char> = text.chars().collect();
                chars.sort();
                return Ok(Value::Str(chars.into_iter().collect()));
            }
        }
        let mut items = into_vec_items(coll, "sort")?;
        if let Some(cmp) = &cmp {
            self.sort_with_cmp(&mut items, cmp)?;
        } else {
            items.sort_by(compare_values_ord);
        }
        Ok(Value::vec(items))
    }

    fn eval_sort_by_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("sort-by expects 2 or 3 arguments"));
        }
        let args_len = args.len();
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("sort-by expects function"))?;
        let (cmp, coll) = if args_len == 3 {
            let cmp = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort-by expects comparator"))?;
            let coll = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort-by expects collection"))?;
            (Some(cmp), coll)
        } else {
            let coll = iter
                .next()
                .ok_or_else(|| Clove2Error::new("sort-by expects collection"))?;
            (None, coll)
        };
        let items = into_vec_items(coll, "sort-by")?;
        let mut keyed: Vec<(Value, Value)> = Vec::with_capacity(items.len());
        for item in items {
            let key = self.call_value(&func, vec![item.clone()])?;
            keyed.push((key, item));
        }
        if let Some(cmp) = &cmp {
            for i in 1..keyed.len() {
                let mut j = i;
                while j > 0 {
                    let ord = self.comparator_order(cmp, &keyed[j - 1].0, &keyed[j].0)?;
                    if ord == std::cmp::Ordering::Greater {
                        keyed.swap(j - 1, j);
                        j -= 1;
                    } else {
                        break;
                    }
                }
            }
        } else {
            keyed.sort_by(|a, b| compare_values_ord(&a.0, &b.0));
        }
        let out: Vec<Value> = keyed.into_iter().map(|(_, item)| item).collect();
        Ok(Value::vec(out))
    }

    fn eval_distinct_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("distinct expects 1 argument"));
        }
        match &args[0] {
            Value::Vec(items) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut out = Vec::new();
                for item in items.as_ref().iter() {
                    if !seen.iter().any(|v| v == item) {
                        seen.push(item.clone());
                        out.push(item.clone());
                    }
                }
                Ok(Value::vec(out))
            }
            Value::Str(text) => {
                let mut seen: Vec<char> = Vec::new();
                let mut out = String::new();
                for ch in text.chars() {
                    if !seen.contains(&ch) {
                        seen.push(ch);
                        out.push(ch);
                    }
                }
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("distinct expects vector or string")),
        }
    }

    fn eval_dedupe_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("dedupe expects 1 argument"));
        }
        match &args[0] {
            Value::Vec(items) => {
                let mut out = Vec::new();
                let mut last: Option<Value> = None;
                for item in items.as_ref().iter() {
                    let is_dup = match &last {
                        Some(prev) => prev == item,
                        None => false,
                    };
                    if !is_dup {
                        out.push(item.clone());
                        last = Some(item.clone());
                    }
                }
                Ok(Value::vec(out))
            }
            Value::Str(text) => {
                let mut out = String::new();
                let mut last: Option<char> = None;
                for ch in text.chars() {
                    if last != Some(ch) {
                        out.push(ch);
                        last = Some(ch);
                    }
                }
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("dedupe expects vector or string")),
        }
    }

    fn eval_group_by_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("group-by expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("group-by expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("group-by expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "group-by")?;
        let mut out: BTreeMap<Key, Value> = BTreeMap::new();
        for item in items {
            let key_val = self.call_value(&func, vec![item.clone()])?;
            let key = value_to_key(&key_val)?;
            let entry = out.entry(key).or_insert_with(|| Value::vec(Vec::new()));
            let Value::Vec(values) = entry else {
                return Err(Clove2Error::new("group-by internal error"));
            };
            let values = self.vec_for_update(values, "group-by")?;
            values.push(item);
        }
        Ok(Value::map(out))
    }

    fn eval_zip_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("zip expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let left_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zip expects 2 arguments"))?;
        let right_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zip expects 2 arguments"))?;
        let left_items = into_vec_items(left_val, "zip")?;
        let right_items = into_vec_items(right_val, "zip")?;
        let len = left_items.len().min(right_items.len());
        let mut out = Vec::with_capacity(len);
        for (left, right) in left_items
            .into_iter()
            .zip(right_items.into_iter())
            .take(len)
        {
            out.push(Value::vec(vec![left, right]));
        }
        Ok(Value::vec(out))
    }

    fn eval_zip_with_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("zip-with expects 3 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zip-with expects 3 arguments"))?;
        let left_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zip-with expects 3 arguments"))?;
        let right_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zip-with expects 3 arguments"))?;
        let left_items = into_vec_items(left_val, "zip-with")?;
        let right_items = into_vec_items(right_val, "zip-with")?;
        let len = left_items.len().min(right_items.len());
        let mut out = Vec::with_capacity(len);
        for (left, right) in left_items
            .into_iter()
            .zip(right_items.into_iter())
            .take(len)
        {
            let value = self.call_value(&func, vec![left, right])?;
            out.push(value);
        }
        Ok(Value::vec(out))
    }

    fn eval_zipmap_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("zipmap expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let keys_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zipmap expects 2 arguments"))?;
        let vals_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("zipmap expects 2 arguments"))?;
        let keys = into_vec_items(keys_val, "zipmap")?;
        let vals = into_vec_items(vals_val, "zipmap")?;
        let len = keys.len().min(vals.len());
        let mut out = BTreeMap::new();
        for (key_value, value) in keys.into_iter().zip(vals.into_iter()).take(len) {
            let key = value_to_key(&key_value)?;
            out.insert(key, value);
        }
        Ok(Value::map(out))
    }

    fn eval_interpose_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("interpose expects 2 arguments"));
        }
        let sep = &args[0];
        match &args[1] {
            Value::Vec(items) => {
                if items.is_empty() {
                    return Ok(Value::vec(Vec::new()));
                }
                let mut out = Vec::with_capacity(items.len() * 2 - 1);
                for (idx, item) in items.iter().enumerate() {
                    if idx > 0 {
                        out.push(sep.clone());
                    }
                    out.push(item.clone());
                }
                Ok(Value::vec(out))
            }
            Value::Str(text) => {
                let Value::Str(sep_str) = sep else {
                    return Err(Clove2Error::new(
                        "interpose expects string separator for string input",
                    ));
                };
                let mut out = String::new();
                for (idx, ch) in text.chars().enumerate() {
                    if idx > 0 {
                        out.push_str(sep_str);
                    }
                    out.push(ch);
                }
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("interpose expects vector or string")),
        }
    }

    fn eval_interleave_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("interleave expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let left_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("interleave expects 2 arguments"))?;
        let right_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("interleave expects 2 arguments"))?;
        if let (Value::Str(left), Value::Str(right)) = (&left_val, &right_val) {
            let left_chars: Vec<char> = left.chars().collect();
            let right_chars: Vec<char> = right.chars().collect();
            let len = left_chars.len().min(right_chars.len());
            let mut out = String::new();
            for idx in 0..len {
                out.push(left_chars[idx]);
                out.push(right_chars[idx]);
            }
            return Ok(Value::Str(out));
        }
        let left_items = into_vec_items(left_val, "interleave")?;
        let right_items = into_vec_items(right_val, "interleave")?;
        let len = left_items.len().min(right_items.len());
        let mut out = Vec::with_capacity(len * 2);
        for (left, right) in left_items
            .into_iter()
            .zip(right_items.into_iter())
            .take(len)
        {
            out.push(left);
            out.push(right);
        }
        Ok(Value::vec(out))
    }

    fn eval_flatten_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("flatten expects 1 or 2 arguments"));
        }
        let depth = if args.len() == 2 {
            let Value::Int(depth) = args[1] else {
                return Err(Clove2Error::new("flatten expects Int depth"));
            };
            if depth < 0 {
                return Err(Clove2Error::new("flatten expects non-negative depth"));
            }
            Some(depth as usize)
        } else {
            None
        };
        let mut out = Vec::new();
        flatten_value(&args[0], depth, &mut out)?;
        Ok(Value::vec(out))
    }

    fn eval_abs_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("abs expects 1 argument"));
        }
        match &args[0] {
            Value::Int(value) => Ok(Value::Int(value.abs())),
            Value::Float(value) => Ok(Value::Float(value.abs())),
            _ => Err(Clove2Error::new("abs expects number")),
        }
    }

    fn eval_minmax_values(&self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new(format!(
                "{} expects at least 1 argument",
                name
            )));
        }
        let mut values = Vec::new();
        let mut has_float = false;
        for arg in args {
            match arg {
                Value::Int(value) => values.push(value as f64),
                Value::Float(value) => {
                    has_float = true;
                    values.push(value);
                }
                _ => {
                    return Err(Clove2Error::new(format!(
                        "{} expects numeric arguments",
                        name
                    )))
                }
            }
        }
        let mut iter = values.into_iter();
        let Some(first) = iter.next() else {
            return Err(Clove2Error::new(format!(
                "{} expects at least 1 argument",
                name
            )));
        };
        let mut out = first;
        for value in iter {
            if name == "min" {
                if value < out {
                    out = value;
                }
            } else if value > out {
                out = value;
            }
        }
        if has_float {
            Ok(Value::Float(out))
        } else {
            Ok(Value::Int(out as i64))
        }
    }

    fn eval_starts_ends_with_values(
        &self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new(format!("{} expects 2 arguments", name)));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let Value::Str(prefix) = &args[1] else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let ok = if name == "starts-with?" {
            text.starts_with(prefix)
        } else {
            text.ends_with(prefix)
        };
        Ok(Value::Bool(ok))
    }

    fn eval_trim_values(&self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let Value::Str(value) = &args[0] else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let out = match name {
            "trim" => value.trim().to_string(),
            "triml" => value.trim_start().to_string(),
            "trimr" => value.trim_end().to_string(),
            _ => return Err(Clove2Error::new("unknown trim op")),
        };
        Ok(Value::Str(out))
    }

    fn eval_upper_lower_values(&self, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new(format!("{} expects 1 argument", name)));
        }
        let Value::Str(value) = &args[0] else {
            return Err(Clove2Error::new(format!("{} expects string", name)));
        };
        let out = if name == "upper-case" {
            value.to_uppercase()
        } else {
            value.to_lowercase()
        };
        Ok(Value::Str(out))
    }

    fn eval_reverse_str_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("reverse-str expects 1 argument"));
        }
        let Value::Str(value) = &args[0] else {
            return Err(Clove2Error::new("reverse-str expects string"));
        };
        Ok(Value::Str(value.chars().rev().collect()))
    }

    fn eval_split_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("split expects 2 or 3 arguments"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("split expects string"));
        };
        let limit = if args.len() == 3 {
            let Value::Int(limit) = args[2] else {
                return Err(Clove2Error::new("split expects Int limit"));
            };
            if limit > 0 {
                Some(limit as usize)
            } else {
                None
            }
        } else {
            None
        };
        let items = split_text(text, &args[1], limit)?;
        Ok(Value::vec(items))
    }

    fn eval_join_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("join expects 1 or 2 arguments"));
        }
        let (coll, sep) = if args.len() == 2 {
            let Value::Str(sep) = &args[1] else {
                return Err(Clove2Error::new("join expects string separator"));
            };
            (&args[0], sep.as_str())
        } else {
            (&args[0], "")
        };
        let items = match coll {
            Value::Vec(items) => items,
            Value::List(items) => items,
            _ => return Err(Clove2Error::new("join expects vector of strings")),
        };
        let mut out = String::new();
        for (idx, item) in items.iter().enumerate() {
            let Value::Str(item) = item else {
                return Err(Clove2Error::new("join expects vector of strings"));
            };
            if idx > 0 {
                out.push_str(sep);
            }
            out.push_str(item);
        }
        Ok(Value::Str(out))
    }

    fn eval_blank_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("blank? expects 1 argument"));
        }
        let Value::Str(value) = &args[0] else {
            return Err(Clove2Error::new("blank? expects string"));
        };
        Ok(Value::Bool(value.trim().is_empty()))
    }

    fn eval_replace_values(
        &self,
        args: Vec<Value>,
        first_only: bool,
    ) -> Result<Value, Clove2Error> {
        if first_only {
            if args.len() != 3 {
                return Err(Clove2Error::new("replace-first expects 3 arguments"));
            }
            let Value::Str(text) = &args[0] else {
                return Err(Clove2Error::new("replace-first expects string"));
            };
            let Value::Str(to) = &args[2] else {
                return Err(Clove2Error::new("replace-first expects string replacement"));
            };
            let out = replace_in_text(text, &args[1], to, true)?;
            return Ok(Value::Str(out));
        }

        if args.len() == 2 {
            return replace_collection(&args[0], &args[1]);
        }
        if args.len() == 3 {
            let Value::Str(text) = &args[0] else {
                return Err(Clove2Error::new("replace expects string"));
            };
            let Value::Str(to) = &args[2] else {
                return Err(Clove2Error::new("replace expects string replacement"));
            };
            let out = replace_in_text(text, &args[1], to, false)?;
            return Ok(Value::Str(out));
        }
        Err(Clove2Error::new("replace expects 2 or 3 arguments"))
    }

    fn eval_re_find_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-find expects 2 arguments"));
        }
        let Value::Str(text) = &args[1] else {
            return Err(Clove2Error::new("re-find expects string"));
        };
        let regex = resolve_regex_value(&args[0])?;
        Ok(regex
            .captures(text)
            .map(|caps| capture_to_value(&caps))
            .unwrap_or(Value::Nil))
    }

    fn eval_re_matches_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-matches expects 2 arguments"));
        }
        let Value::Str(text) = &args[1] else {
            return Err(Clove2Error::new("re-matches expects string"));
        };
        let regex = resolve_regex_value(&args[0])?;
        let matched = regex.captures(text).and_then(|caps| {
            caps.get(0).and_then(|full| {
                if full.start() == 0 && full.end() == text.len() {
                    Some(capture_to_value(&caps))
                } else {
                    None
                }
            })
        });
        Ok(matched.unwrap_or(Value::Nil))
    }

    fn eval_re_seq_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("re-seq expects 2 arguments"));
        }
        let Value::Str(text) = &args[1] else {
            return Err(Clove2Error::new("re-seq expects string"));
        };
        let regex = resolve_regex_value(&args[0])?;
        let mut items = Vec::new();
        for caps in regex.captures_iter(text) {
            items.push(capture_to_value(&caps));
        }
        Ok(Value::list(items))
    }

    fn eval_split_lines_values(
        &self,
        args: Vec<Value>,
        keep_ends: bool,
    ) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("split-lines expects 1 argument"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("split-lines expects string"));
        };
        let items = if keep_ends {
            split_lines_keep_ends(text)
        } else {
            text.lines()
                .map(|line| Value::Str(line.to_string()))
                .collect()
        };
        Ok(Value::vec(items))
    }

    fn eval_index_of_values(&self, args: Vec<Value>, last: bool) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("index-of expects 2 or 3 arguments"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("index-of expects string"));
        };
        let Value::Str(needle) = &args[1] else {
            return Err(Clove2Error::new("index-of expects string needle"));
        };
        let len = text.chars().count();
        let out = if last {
            let end = if args.len() == 3 {
                let Value::Int(end) = args[2] else {
                    return Err(Clove2Error::new("last-index-of expects Int end"));
                };
                if end < 0 {
                    return Err(Clove2Error::new("last-index-of expects non-negative end"));
                }
                end as usize
            } else {
                len
            };
            if end == 0 {
                None
            } else {
                let end = end.min(len);
                let end_byte = char_byte_index(text, end);
                let slice = &text[..end_byte];
                slice.rfind(needle).map(|idx| {
                    let prefix = &text[..idx];
                    prefix.chars().count()
                })
            }
        } else {
            let start = if args.len() == 3 {
                let Value::Int(start) = args[2] else {
                    return Err(Clove2Error::new("index-of expects Int start"));
                };
                if start < 0 {
                    return Err(Clove2Error::new("index-of expects non-negative start"));
                }
                start as usize
            } else {
                0
            };
            if start > len {
                return Ok(Value::Nil);
            }
            let start_byte = char_byte_index(text, start);
            let slice = &text[start_byte..];
            slice.find(needle).map(|idx| {
                let prefix = &text[..start_byte + idx];
                prefix.chars().count()
            })
        };
        match out {
            Some(idx) => Ok(Value::Int(idx as i64)),
            None => Ok(Value::Nil),
        }
    }

    fn eval_capitalize_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("capitalize expects 1 argument"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("capitalize expects string"));
        };
        let mut chars = text.chars();
        let Some(first) = chars.next() else {
            return Ok(Value::Str(text.clone()));
        };
        let mut out = String::new();
        out.extend(first.to_uppercase());
        let rest: String = chars.collect();
        out.push_str(&rest.to_lowercase());
        Ok(Value::Str(out))
    }

    fn eval_trim_newline_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("trim-newline expects 1 argument"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("trim-newline expects string"));
        };
        let out = text.trim_end_matches(&['\n', '\r'][..]).to_string();
        Ok(Value::Str(out))
    }

    fn eval_subs_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("subs expects 2 or 3 arguments"));
        }
        let Value::Str(text) = &args[0] else {
            return Err(Clove2Error::new("subs expects string"));
        };
        let Value::Int(start) = args[1] else {
            return Err(Clove2Error::new("subs expects Int start"));
        };
        let end = if args.len() == 3 {
            let Value::Int(end) = args[2] else {
                return Err(Clove2Error::new("subs expects Int end"));
            };
            end
        } else {
            text.chars().count() as i64
        };
        if start < 0 || end < 0 {
            return Err(Clove2Error::new("subs expects non-negative indices"));
        }
        let len = text.chars().count() as i64;
        if start > end || end > len {
            return Err(Clove2Error::new("subs index out of range"));
        }
        let start_idx = char_byte_index(text, start as usize);
        let end_idx = char_byte_index(text, end as usize);
        Ok(Value::Str(text[start_idx..end_idx].to_string()))
    }

    fn eval_slurp_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("slurp expects 1 argument"));
        }
        let Value::Str(path) = &args[0] else {
            return Err(Clove2Error::new("slurp expects string path"));
        };
        let resolved = normalize_path(path);
        let content = fs::read_to_string(&resolved)
            .map_err(|err| Clove2Error::new(format!("failed to read {}: {}", resolved, err)))?;
        Ok(Value::Str(content))
    }

    fn eval_spit_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("spit expects 2 arguments"));
        }
        let Value::Str(path) = &args[0] else {
            return Err(Clove2Error::new("spit expects string path"));
        };
        let Value::Str(content) = &args[1] else {
            return Err(Clove2Error::new("spit expects string content"));
        };
        let resolved = normalize_path(path);
        fs::write(&resolved, content)
            .map_err(|err| Clove2Error::new(format!("failed to write {}: {}", resolved, err)))?;
        Ok(Value::Str(path.clone()))
    }

    fn eval_fs_delete_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("fs::delete expects 1 argument"));
        }
        let Value::Str(path) = &args[0] else {
            return Err(Clove2Error::new("fs::delete expects string path"));
        };
        let resolved = normalize_path(path);
        remove_path(&resolved)?;
        Ok(Value::Str(path.clone()))
    }

    fn eval_map_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("map expects at least 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("map expects at least 2 arguments"))?;
        let mut coll_items = Vec::new();
        let mut min_len = usize::MAX;
        for coll in iter {
            let items = into_vec_items(coll, "map")?;
            min_len = min_len.min(items.len());
            coll_items.push(items);
        }
        if min_len == usize::MAX {
            min_len = 0;
        }
        if coll_items.len() == 1 {
            let items = coll_items.pop().unwrap_or_default();
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(self.call_value(&func, vec![item])?);
            }
            return Ok(Value::vec(out));
        }
        let mut out = Vec::with_capacity(min_len);
        for idx in 0..min_len {
            let mut call_args = Vec::with_capacity(coll_items.len());
            for items in coll_items.iter_mut() {
                let value = std::mem::replace(&mut items[idx], Value::Nil);
                call_args.push(value);
            }
            out.push(self.call_value(&func, call_args)?);
        }
        Ok(Value::vec(out))
    }

    fn eval_map_indexed_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("map-indexed expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("map-indexed expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("map-indexed expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "map-indexed")?;
        let mut out = Vec::with_capacity(items.len());
        for (idx, item) in items.into_iter().enumerate() {
            out.push(self.call_value(&func, vec![Value::Int(idx as i64), item])?);
        }
        Ok(Value::vec(out))
    }

    fn eval_mapcat_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("mapcat expects at least 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("mapcat expects at least 2 arguments"))?;
        let mut coll_items = Vec::new();
        let mut min_len = usize::MAX;
        for coll in iter {
            let items = into_vec_items(coll, "mapcat")?;
            min_len = min_len.min(items.len());
            coll_items.push(items);
        }
        if min_len == usize::MAX {
            min_len = 0;
        }
        let mut out = Vec::new();
        if coll_items.len() == 1 {
            let items = coll_items.pop().unwrap_or_default();
            for item in items {
                let mapped = self.call_value(&func, vec![item])?;
                let values = into_vec_items(mapped, "mapcat")?;
                out.extend(values);
            }
            return Ok(Value::vec(out));
        }
        for idx in 0..min_len {
            let mut call_args = Vec::with_capacity(coll_items.len());
            for items in coll_items.iter_mut() {
                let value = std::mem::replace(&mut items[idx], Value::Nil);
                call_args.push(value);
            }
            let mapped = self.call_value(&func, call_args)?;
            let values = into_vec_items(mapped, "mapcat")?;
            out.extend(values);
        }
        Ok(Value::vec(out))
    }

    fn eval_filter_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("filter expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("filter expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("filter expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "filter")?;
        let mut out = Vec::new();
        for item in items {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if is_truthy(&keep) {
                out.push(item);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_keep_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("keep expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("keep expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("keep expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "keep")?;
        let mut out = Vec::new();
        for item in items {
            let value = self.call_value(&func, vec![item])?;
            if !matches!(value, Value::Nil) {
                out.push(value);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_keep_indexed_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("keep-indexed expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("keep-indexed expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("keep-indexed expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "keep-indexed")?;
        let mut out = Vec::new();
        for (idx, item) in items.into_iter().enumerate() {
            let value = self.call_value(&func, vec![Value::Int(idx as i64), item])?;
            if !matches!(value, Value::Nil) {
                out.push(value);
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_take_while_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("take-while expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("take-while expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("take-while expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "take-while")?;
        let mut out = Vec::new();
        for item in items {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if is_truthy(&keep) {
                out.push(item);
            } else {
                break;
            }
        }
        Ok(Value::vec(out))
    }

    fn eval_drop_while_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("drop-while expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("drop-while expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("drop-while expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "drop-while")?;
        let mut start = items.len();
        for (idx, item) in items.iter().enumerate() {
            let keep = self.call_value(&func, vec![item.clone()])?;
            if !is_truthy(&keep) {
                start = idx;
                break;
            }
        }
        let out = if start >= items.len() {
            Vec::new()
        } else {
            items[start..].to_vec()
        };
        Ok(Value::vec(out))
    }

    fn eval_partition_by_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("partition-by expects 2 arguments"));
        }
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("partition-by expects 2 arguments"))?;
        let vec_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("partition-by expects 2 arguments"))?;
        let items = into_vec_items(vec_val, "partition-by")?;
        let mut out = Vec::new();
        let mut current = Vec::new();
        let mut last_key: Option<Value> = None;
        for item in items {
            let key = self.call_value(&func, vec![item.clone()])?;
            let is_same = match &last_key {
                Some(prev) => prev == &key,
                None => true,
            };
            if !is_same && !current.is_empty() {
                out.push(Value::vec(current));
                current = Vec::new();
            }
            current.push(item);
            last_key = Some(key);
        }
        if !current.is_empty() {
            out.push(Value::vec(current));
        }
        Ok(Value::vec(out))
    }

    fn eval_reduce_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("reduce expects 2 or 3 arguments"));
        }
        let args_len = args.len();
        let mut iter = args.into_iter();
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("reduce expects 2 or 3 arguments"))?;
        let (init, vec_val) = if args_len == 3 {
            let init = iter
                .next()
                .ok_or_else(|| Clove2Error::new("reduce expects init"))?;
            let vec_val = iter
                .next()
                .ok_or_else(|| Clove2Error::new("reduce expects collection"))?;
            (Some(init), vec_val)
        } else {
            let vec_val = iter
                .next()
                .ok_or_else(|| Clove2Error::new("reduce expects collection"))?;
            (None, vec_val)
        };
        let items = into_vec_items(vec_val, "reduce")?;
        let mut iter = items.into_iter();
        let mut acc = match init {
            Some(value) => value,
            None => match iter.next() {
                Some(value) => value,
                None => return Ok(Value::Nil),
            },
        };
        for item in iter {
            acc = self.call_value(&func, vec![acc, item])?;
        }
        Ok(acc)
    }

    fn eval_reduce_kv_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("reduce-kv expects 3 arguments"));
        }
        let func = &args[0];
        let mut acc = args[1].clone();
        let Value::Map(map) = &args[2] else {
            return Err(Clove2Error::new("reduce-kv expects map"));
        };
        for (key, value) in map.iter() {
            acc = self.call_value(func, vec![acc, key_to_value(key), value.clone()])?;
        }
        Ok(acc)
    }

    fn eval_take_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("take expects 2 arguments"));
        }
        let Value::Int(count) = args[0] else {
            return Err(Clove2Error::new("take expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        match &args[1] {
            Value::Vec(items) => Ok(Value::vec(items.iter().take(count).cloned().collect())),
            Value::List(items) => Ok(Value::vec(items.iter().take(count).cloned().collect())),
            Value::Str(value) => {
                let out: String = value.chars().take(count).collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("take expects vector or string")),
        }
    }

    fn eval_drop_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("drop expects 2 arguments"));
        }
        let Value::Int(count) = args[0] else {
            return Err(Clove2Error::new("drop expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        match &args[1] {
            Value::Vec(items) => Ok(Value::vec(items.iter().skip(count).cloned().collect())),
            Value::List(items) => Ok(Value::vec(items.iter().skip(count).cloned().collect())),
            Value::Str(value) => {
                let out: String = value.chars().skip(count).collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("drop expects vector or string")),
        }
    }

    fn eval_take_last_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("take-last expects 2 arguments"));
        }
        let Value::Int(count) = args[0] else {
            return Err(Clove2Error::new("take-last expects Int count"));
        };
        let count = if count < 0 { 0 } else { count as usize };
        match &args[1] {
            Value::Vec(items) => {
                let len = items.len();
                let start = len.saturating_sub(count);
                Ok(Value::vec(items[start..].to_vec()))
            }
            Value::Str(value) => {
                let chars: Vec<char> = value.chars().collect();
                let len = chars.len();
                let start = len.saturating_sub(count);
                let out: String = chars[start..].iter().collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("take-last expects vector or string")),
        }
    }

    fn eval_drop_last_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("drop-last expects 1 or 2 arguments"));
        }
        let (count, coll) = if args.len() == 1 {
            (1, &args[0])
        } else {
            let Value::Int(count) = args[0] else {
                return Err(Clove2Error::new("drop-last expects Int count"));
            };
            (count, &args[1])
        };
        let count = if count < 0 { 0 } else { count as usize };
        match coll {
            Value::Vec(items) => {
                let len = items.len();
                let end = len.saturating_sub(count);
                Ok(Value::vec(items[..end].to_vec()))
            }
            Value::List(items) => {
                let len = items.len();
                let end = len.saturating_sub(count);
                Ok(Value::List(items[..end].to_vec()))
            }
            Value::Str(value) => {
                let chars: Vec<char> = value.chars().collect();
                let len = chars.len();
                let end = len.saturating_sub(count);
                let out: String = chars[..end].iter().collect();
                Ok(Value::Str(out))
            }
            _ => Err(Clove2Error::new("drop-last expects vector or string")),
        }
    }

    fn eval_reverse_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("reverse expects 1 argument"));
        }
        match &args[0] {
            Value::Vec(items) => {
                let mut out = items.as_ref().clone();
                out.reverse();
                Ok(Value::vec(out))
            }
            Value::List(items) => {
                let mut out = items.clone();
                out.reverse();
                Ok(Value::List(out))
            }
            Value::Str(value) => Ok(Value::Str(value.chars().rev().collect())),
            _ => Err(Clove2Error::new("reverse expects collection or string")),
        }
    }

    fn eval_subvec_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("subvec expects 2 or 3 arguments"));
        }
        let Value::Vec(items) = &args[0] else {
            return Err(Clove2Error::new("subvec expects vector"));
        };
        let Value::Int(start) = args[1] else {
            return Err(Clove2Error::new("subvec expects Int start"));
        };
        let end = if args.len() == 3 {
            let Value::Int(end) = args[2] else {
                return Err(Clove2Error::new("subvec expects Int end"));
            };
            end
        } else {
            items.len() as i64
        };
        if start < 0 || end < 0 {
            return Err(Clove2Error::new("subvec expects non-negative indices"));
        }
        let start = start as usize;
        let end = end as usize;
        if start > end || end > items.len() {
            return Err(Clove2Error::new("subvec index out of range"));
        }
        Ok(Value::vec(items[start..end].to_vec()))
    }

    fn eval_concat_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("concat expects at least 1 argument"));
        }
        let mut vec_items: Vec<Value> = Vec::new();
        let mut str_items = String::new();
        let mut mode: Option<&str> = None;
        for value in args {
            match value {
                Value::Vec(_) | Value::List(_) | Value::Set(_) => {
                    if let Some(kind) = mode {
                        if kind != "vec" {
                            return Err(Clove2Error::new("concat expects same type"));
                        }
                    } else {
                        mode = Some("vec");
                    }
                    let items = into_vec_items(value, "concat")?;
                    vec_items.extend(items);
                }
                Value::Str(value) => {
                    if let Some(kind) = mode {
                        if kind != "str" {
                            return Err(Clove2Error::new("concat expects same type"));
                        }
                    } else {
                        mode = Some("str");
                    }
                    str_items.push_str(&value);
                }
                _ => return Err(Clove2Error::new("concat expects vector or string")),
            }
        }
        match mode {
            Some("vec") => Ok(Value::vec(vec_items)),
            Some("str") => Ok(Value::Str(str_items)),
            _ => Err(Clove2Error::new("concat expects vector or string")),
        }
    }

    fn eval_includes_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("includes? expects 2 arguments"));
        }
        match &args[0] {
            Value::Vec(items) => Ok(Value::Bool(items.iter().any(|v| v == &args[1]))),
            Value::List(items) => Ok(Value::Bool(items.iter().any(|v| v == &args[1]))),
            Value::Set(items) => Ok(Value::Bool(items.iter().any(|v| v == &args[1]))),
            Value::Map(map) => {
                let key = value_to_key(&args[1])?;
                Ok(Value::Bool(map.contains_key(&key)))
            }
            Value::Str(value) => match &args[1] {
                Value::Str(item) => Ok(Value::Bool(value.contains(item))),
                _ => Err(Clove2Error::new("includes? expects string item for string")),
            },
            Value::Nil => Ok(Value::Bool(false)),
            _ => Err(Clove2Error::new("includes? expects collection")),
        }
    }

    fn eval_get_in_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 && args.len() != 3 {
            return Err(Clove2Error::new("get-in expects 2 or 3 arguments"));
        }
        let coll = args[0].clone();
        let path_val = args[1].clone();
        let default_val = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::Nil
        };
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("get-in expects vector path"));
        };
        self.eval_get_in_value(coll, &path, &default_val)
    }

    fn eval_assoc_in_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 3 {
            return Err(Clove2Error::new("assoc-in expects map, path, and value"));
        }
        let mut iter = args.into_iter();
        let coll = iter
            .next()
            .ok_or_else(|| Clove2Error::new("assoc-in expects map, path, and value"))?;
        let path_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("assoc-in expects map, path, and value"))?;
        let value = iter
            .next()
            .ok_or_else(|| Clove2Error::new("assoc-in expects map, path, and value"))?;
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("assoc-in expects vector path"));
        };
        self.eval_assoc_in_value(coll, path.as_ref(), value)
    }

    fn eval_update_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 3 {
            return Err(Clove2Error::new("update expects map, key, and function"));
        }
        let mut iter = args.into_iter();
        let first = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update expects map, key, and function"))?;
        let Value::Map(mut map) = first else {
            return Err(Clove2Error::new("update expects map"));
        };
        let key_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update expects map, key, and function"))?;
        let key = value_to_key(&key_val)?;
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update expects map, key, and function"))?;
        let mut call_args = Vec::new();
        call_args.push(map.get(&key).cloned().unwrap_or(Value::Nil));
        for extra in iter {
            call_args.push(extra);
        }
        let updated = self.call_value(&func, call_args)?;
        self.map_for_update(&mut map, "update")?
            .insert(key, updated);
        Ok(Value::Map(map))
    }

    fn eval_update_in_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 3 {
            return Err(Clove2Error::new(
                "update-in expects map, path, and function",
            ));
        }
        let mut iter = args.into_iter();
        let coll = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update-in expects map, path, and function"))?;
        let path_val = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update-in expects map, path, and function"))?;
        let Value::Vec(path) = path_val else {
            return Err(Clove2Error::new("update-in expects vector path"));
        };
        let func = iter
            .next()
            .ok_or_else(|| Clove2Error::new("update-in expects map, path, and function"))?;
        let extras: Vec<Value> = iter.collect();
        if matches!(self.mut_mode(), MutMode::Mut) {
            if path.is_empty() {
                let mut call_args = Vec::with_capacity(1 + extras.len());
                call_args.push(coll);
                call_args.extend(extras.iter().cloned());
                return self.call_value(&func, call_args);
            }
            match coll {
                Value::Map(mut map) => {
                    let map_ref = self.map_for_update(&mut map, "update-in")?;
                    let current = self.take_in_value_map(map_ref, path.as_ref())?;
                    let mut call_args = Vec::with_capacity(1 + extras.len());
                    call_args.push(current);
                    call_args.extend(extras.iter().cloned());
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(map_ref, path.as_ref(), updated.clone())?;
                    Ok(Value::Map(map))
                }
                Value::Nil => {
                    let mut new_map = BTreeMap::new();
                    let mut call_args = Vec::with_capacity(1 + extras.len());
                    call_args.push(Value::Nil);
                    call_args.extend(extras.iter().cloned());
                    let updated = self.call_value(&func, call_args)?;
                    self.assoc_in_place_map(&mut new_map, path.as_ref(), updated.clone())?;
                    Ok(Value::Map(Rc::new(new_map)))
                }
                _ => Err(Clove2Error::new("update-in expects map")),
            }
        } else {
            let current = self.eval_get_in_value(coll.clone(), path.as_ref(), &Value::Nil)?;
            let mut call_args = Vec::with_capacity(1 + extras.len());
            call_args.push(current);
            call_args.extend(extras.iter().cloned());
            let updated = self.call_value(&func, call_args)?;
            self.eval_assoc_in_value(coll, path.as_ref(), updated)
        }
    }

    fn eval_dissoc_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() < 2 {
            return Err(Clove2Error::new("dissoc expects map and keys"));
        }
        let mut iter = args.into_iter();
        let first = iter
            .next()
            .ok_or_else(|| Clove2Error::new("dissoc expects map and keys"))?;
        let Value::Map(mut map) = first else {
            return Err(Clove2Error::new("dissoc expects map"));
        };
        let map_ref = self.map_for_update(&mut map, "dissoc")?;
        for key in iter {
            let key = value_to_key(&key)?;
            map_ref.remove(&key);
        }
        Ok(Value::Map(map))
    }

    fn eval_empty_pred_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("empty? expects 1 argument"));
        }
        match &args[0] {
            Value::Vec(items) => Ok(Value::Bool(items.is_empty())),
            Value::List(items) => Ok(Value::Bool(items.is_empty())),
            Value::Set(items) => Ok(Value::Bool(items.is_empty())),
            Value::Map(map) => Ok(Value::Bool(map.is_empty())),
            Value::Str(value) => Ok(Value::Bool(value.is_empty())),
            _ => Err(Clove2Error::new("empty? expects collection")),
        }
    }

    fn eval_not_empty_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("not-empty expects 1 argument"));
        }
        match &args[0] {
            Value::Vec(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Vec(items.clone()))
                }
            }
            Value::List(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(items.clone()))
                }
            }
            Value::Set(items) => {
                if items.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Set(items.clone()))
                }
            }
            Value::Map(map) => {
                if map.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Map(map.clone()))
                }
            }
            Value::Str(value) => {
                if value.is_empty() {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Str(value.clone()))
                }
            }
            _ => Err(Clove2Error::new("not-empty expects collection")),
        }
    }

    fn eval_json_read_file_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("json::read-file expects path"));
        }
        let Value::Str(path) = &args[0] else {
            return Err(Clove2Error::new("json::read-file expects string path"));
        };
        let mut schema = None;
        let mut validate = false;
        if args.len() > 1 {
            if (args.len() - 1) % 2 != 0 {
                return Err(Clove2Error::new("json::read-file expects keyword options"));
            }
            for pair in args[1..].chunks(2) {
                let key = json_option_key(&pair[0])?;
                match key {
                    "schema" | "type" => {
                        schema = Some(parse_type_from_value(&pair[1])?);
                    }
                    "validate" => {
                        let Value::Bool(value) = pair[1] else {
                            return Err(Clove2Error::new("json::read-file :validate expects bool"));
                        };
                        validate = value;
                    }
                    "infer" => {}
                    _ => return Err(Clove2Error::new("json::read-file has unknown option")),
                }
            }
        }
        let resolved = normalize_path(path);
        let content = fs::read_to_string(&resolved)
            .map_err(|err| Clove2Error::new(format!("failed to read {}: {}", resolved, err)))?;
        let value: serde_json::Value = serde_json::from_str(&content)
            .map_err(|err| Clove2Error::new(format!("invalid json {}: {}", resolved, err)))?;
        let out = json_to_value(&value);
        if validate {
            if let Some(schema) = schema {
                if !crate::type_check::matches_type(&out, &schema) {
                    return Err(Clove2Error::new(format!(
                        "json::read-file validation failed: {}",
                        schema
                    )));
                }
            }
        }
        Ok(out)
    }

    fn eval_json_read_string_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.is_empty() {
            return Err(Clove2Error::new("json::read-string expects json string"));
        }
        let Value::Str(input) = &args[0] else {
            return Err(Clove2Error::new("json::read-string expects string input"));
        };
        let mut schema = None;
        let mut validate = false;
        if args.len() > 1 {
            if (args.len() - 1) % 2 != 0 {
                return Err(Clove2Error::new(
                    "json::read-string expects keyword options",
                ));
            }
            for pair in args[1..].chunks(2) {
                let key = json_option_key(&pair[0])?;
                match key {
                    "schema" | "type" => {
                        schema = Some(parse_type_from_value(&pair[1])?);
                    }
                    "validate" => {
                        let Value::Bool(value) = pair[1] else {
                            return Err(Clove2Error::new(
                                "json::read-string :validate expects bool",
                            ));
                        };
                        validate = value;
                    }
                    "infer" => {}
                    _ => return Err(Clove2Error::new("json::read-string has unknown option")),
                }
            }
        }
        let value: serde_json::Value = serde_json::from_str(input)
            .map_err(|err| Clove2Error::new(format!("invalid json: {}", err)))?;
        let out = json_to_value(&value);
        if validate {
            if let Some(schema) = schema {
                if !crate::type_check::matches_type(&out, &schema) {
                    return Err(Clove2Error::new(format!(
                        "json::read-string validation failed: {}",
                        schema
                    )));
                }
            }
        }
        Ok(out)
    }

    fn eval_json_write_string_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("json::write-string expects 1 argument"));
        }
        let json_value = value_to_json(&args[0])?;
        let out = serde_json::to_string(&json_value)
            .map_err(|err| Clove2Error::new(format!("json::write-string failed: {}", err)))?;
        Ok(Value::Str(out))
    }

    fn eval_json_write_file_values(&mut self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("json::write-file expects 2 arguments"));
        }
        let Value::Str(path) = &args[0] else {
            return Err(Clove2Error::new("json::write-file expects string path"));
        };
        let json_value = value_to_json(&args[1])?;
        let out = serde_json::to_string(&json_value)
            .map_err(|err| Clove2Error::new(format!("json::write-file failed: {}", err)))?;
        let resolved = normalize_path(path);
        fs::write(&resolved, out)
            .map_err(|err| Clove2Error::new(format!("json::write-file failed: {}", err)))?;
        Ok(Value::Str(path.clone()))
    }

    fn eval_expect_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("expect expects 2 arguments"));
        }
        let ty = parse_type_from_value(&args[0])?;
        let value = args[1].clone();
        let _ = expect_type(&value, &ty)?;
        Ok(value)
    }

    fn eval_as_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 2 {
            return Err(Clove2Error::new("as expects 2 arguments"));
        }
        let ty = parse_type_from_value(&args[0])?;
        let value = args[1].clone();
        if as_type(&value, &ty).is_some() {
            Ok(value)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_cast_int_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("int expects 1 argument"));
        }
        match &args[0] {
            Value::Int(value) => Ok(Value::Int(*value)),
            Value::Float(value) => Ok(Value::Int(*value as i64)),
            Value::Str(value) => value
                .trim()
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| Clove2Error::new("int parse failed")),
            _ => Err(Clove2Error::new("int expects number or string")),
        }
    }

    fn eval_cast_float_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("float expects 1 argument"));
        }
        match &args[0] {
            Value::Int(value) => Ok(Value::Float(*value as f64)),
            Value::Float(value) => Ok(Value::Float(*value)),
            Value::Str(value) => value
                .trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| Clove2Error::new("float parse failed")),
            _ => Err(Clove2Error::new("float expects number or string")),
        }
    }

    fn eval_cast_bool_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("bool expects 1 argument"));
        }
        match &args[0] {
            Value::Bool(value) => Ok(Value::Bool(*value)),
            Value::Str(value) => match value.trim() {
                "true" => Ok(Value::Bool(true)),
                "false" => Ok(Value::Bool(false)),
                _ => Err(Clove2Error::new("bool parse failed")),
            },
            _ => Err(Clove2Error::new("bool expects bool or string")),
        }
    }

    fn eval_cast_keyword_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 && args.len() != 2 {
            return Err(Clove2Error::new("keyword expects 1 or 2 arguments"));
        }
        if args.len() == 2 {
            let ns = keyword_part(&args[0])?;
            let name = keyword_part(&args[1])?;
            return Ok(Value::Keyword(format!("{}::{}", ns, name)));
        }
        match &args[0] {
            Value::Keyword(value) => Ok(Value::Keyword(value.clone())),
            Value::Str(value) => Ok(Value::Keyword(value.clone())),
            Value::Symbol(value) => Ok(Value::Keyword(value.clone())),
            _ => Err(Clove2Error::new(
                "keyword expects string, symbol, or keyword",
            )),
        }
    }

    fn eval_cast_symbol_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("symbol expects 1 argument"));
        }
        match &args[0] {
            Value::Symbol(value) => Ok(Value::Symbol(value.clone())),
            Value::Str(value) => Ok(Value::Symbol(value.clone())),
            Value::Keyword(value) => Ok(Value::Symbol(value.clone())),
            _ => Err(Clove2Error::new(
                "symbol expects string, symbol, or keyword",
            )),
        }
    }

    fn eval_name_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("name expects 1 argument"));
        }
        let name = name_from_value(&args[0])?;
        Ok(Value::Str(name))
    }

    fn eval_namespace_value(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        if args.len() != 1 {
            return Err(Clove2Error::new("namespace expects 1 argument"));
        }
        let namespace = namespace_from_value(&args[0])?;
        Ok(match namespace {
            Some(value) => Value::Str(value),
            None => Value::Nil,
        })
    }

    fn eval_gensym_values(&self, args: Vec<Value>) -> Result<Value, Clove2Error> {
        let prefix = match args.as_slice() {
            [] => "G__".to_string(),
            [Value::Str(value)] => value.clone(),
            [Value::Symbol(value)] => value.clone(),
            [Value::Keyword(value)] => value.clone(),
            [other] => {
                return Err(Clove2Error::new(format!(
                    "gensym expects string or symbol, got {}",
                    value_type_name(other)
                )))
            }
            _ => return Err(Clove2Error::new("gensym expects 0 or 1 argument")),
        };
        let id = GENSYM_COUNTER.fetch_add(1, AtomicOrdering::SeqCst);
        Ok(Value::Symbol(format!("{}{}", prefix, id)))
    }
}

#[derive(Clone, Debug)]
struct TryCatchClause {
    name: String,
    body: Vec<AstExpr>,
}

fn parse_try_bindings(items: &[AstExpr]) -> Result<Vec<(String, AstExpr)>, Clove2Error> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        let AstExpr::Symbol(name_sym) = &items[idx] else {
            return Err(Clove2Error::new("try binding name must be symbol"));
        };
        let mut name = name_sym.clone();
        if name.ends_with(':') {
            name = name.trim_end_matches(':').to_string();
            idx += 1;
            if idx >= items.len() {
                return Err(Clove2Error::new("try binding missing type"));
            }
        }
        idx += 1;
        let value_expr = items
            .get(idx)
            .ok_or_else(|| Clove2Error::new("try binding missing value"))?;
        out.push((name, value_expr.clone()));
        idx += 1;
    }
    Ok(out)
}

fn parse_try_catch_clause(expr: &AstExpr) -> Result<Option<TryCatchClause>, Clove2Error> {
    let AstExpr::Call { callee, args } = expr else {
        return Ok(None);
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return Ok(None);
    };
    if sym != "catch" {
        return Ok(None);
    }
    if args.len() < 3 {
        return Err(Clove2Error::new("catch expects type, name, and body"));
    }
    let AstExpr::Symbol(name_sym) = &args[1] else {
        return Err(Clove2Error::new("catch expects symbol name"));
    };
    let body: Vec<AstExpr> = args.iter().skip(2).cloned().collect();
    if body.is_empty() {
        return Err(Clove2Error::new("catch expects body"));
    }
    Ok(Some(TryCatchClause {
        name: name_sym.clone(),
        body,
    }))
}

fn parse_try_err_clause(expr: &AstExpr) -> Result<Option<TryCatchClause>, Clove2Error> {
    let AstExpr::Call { callee, args } = expr else {
        return Ok(None);
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return Ok(None);
    };
    if sym != "err" {
        return Ok(None);
    }
    if args.is_empty() {
        return Err(Clove2Error::new("err expects body"));
    }
    let (name, body_start) = if args.len() == 1 {
        ("?".to_string(), 0)
    } else if let AstExpr::Symbol(name_sym) = &args[0] {
        (name_sym.clone(), 1)
    } else {
        ("?".to_string(), 0)
    };
    let body: Vec<AstExpr> = args.iter().skip(body_start).cloned().collect();
    if body.is_empty() {
        return Err(Clove2Error::new("err expects body"));
    }
    Ok(Some(TryCatchClause { name, body }))
}

fn parse_try_finally_clause(expr: &AstExpr) -> Result<Option<Vec<AstExpr>>, Clove2Error> {
    let AstExpr::Call { callee, args } = expr else {
        return Ok(None);
    };
    let AstExpr::Symbol(sym) = callee.as_ref() else {
        return Ok(None);
    };
    if sym != "finally" && sym != "fin" {
        return Ok(None);
    }
    if args.is_empty() {
        return Err(Clove2Error::new("finally expects body"));
    }
    Ok(Some(args.clone()))
}

fn is_try_handler_expr(expr: &AstExpr) -> bool {
    matches!(expr, AstExpr::Fn { .. } | AstExpr::Symbol(_))
}

fn eval_body(eval: &mut Evaluator, body: &[AstExpr], env: EnvRef) -> Result<Value, Clove2Error> {
    let mut last = Value::Nil;
    for expr in body {
        last = eval.eval_expr(expr, env.clone())?;
    }
    Ok(last)
}

fn name_from_value(value: &Value) -> Result<String, Clove2Error> {
    match value {
        Value::Keyword(name) | Value::Symbol(name) => {
            let (_, tail) = split_namespace(name);
            Ok(tail.to_string())
        }
        Value::Str(value) => Ok(value.clone()),
        _ => Err(Clove2Error::new("name expects keyword, symbol, or string")),
    }
}

fn keyword_part(value: &Value) -> Result<String, Clove2Error> {
    match value {
        Value::Str(value) => Ok(value.clone()),
        Value::Symbol(value) | Value::Keyword(value) => Ok(value.clone()),
        _ => Err(Clove2Error::new(
            "keyword expects string, symbol, or keyword",
        )),
    }
}

fn namespace_from_value(value: &Value) -> Result<Option<String>, Clove2Error> {
    match value {
        Value::Keyword(name) | Value::Symbol(name) => {
            let (ns, _) = split_namespace(name);
            Ok(ns.map(|value| value.to_string()))
        }
        _ => Err(Clove2Error::new("namespace expects keyword or symbol")),
    }
}

fn split_namespace(raw: &str) -> (Option<&str>, &str) {
    if let Some(idx) = raw.find('/') {
        let (ns, rest) = raw.split_at(idx);
        let name = &rest[1..];
        if ns.is_empty() {
            (None, name)
        } else {
            (Some(ns), name)
        }
    } else {
        (None, raw)
    }
}

fn strip_type_namespace(raw: &str) -> &str {
    let raw = raw.trim_start_matches(':');
    if let Some(idx) = raw.rfind("::") {
        return &raw[idx + 2..];
    }
    if let Some(idx) = raw.rfind('/') {
        return &raw[idx + 1..];
    }
    raw
}

fn normalize_type_name(value: &Value) -> Result<String, Clove2Error> {
    let raw = match value {
        Value::Keyword(name) | Value::Symbol(name) | Value::Str(name) => name.as_str(),
        _ => return Err(Clove2Error::new("instance? expects type name")),
    };
    Ok(strip_type_namespace(raw).to_string())
}

fn matches_type_name(type_name: &str, value: &Value) -> bool {
    match type_name {
        "Nil" => matches!(value, Value::Nil),
        "Bool" => matches!(value, Value::Bool(_)),
        "Int" => matches!(value, Value::Int(_)),
        "Float" => matches!(value, Value::Float(_)),
        "Number" => matches!(value, Value::Int(_) | Value::Float(_)),
        "Str" | "String" => matches!(value, Value::Str(_)),
        "Keyword" => matches!(value, Value::Keyword(_)),
        "Symbol" => matches!(value, Value::Symbol(_)),
        "Vec" | "Vector" => matches!(value, Value::Vec(_)),
        "List" => matches!(value, Value::List(_)),
        "Set" => matches!(value, Value::Set(_)),
        "Map" => matches!(value, Value::Map(_)),
        "Regex" => matches!(value, Value::Regex(_)),
        _ => false,
    }
}

fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Nil => "Nil",
        Value::Bool(_) => "Bool",
        Value::Int(_) => "Int",
        Value::Float(_) => "Float",
        Value::Str(_) => "Str",
        Value::Regex(_) => "Regex",
        Value::Keyword(_) => "Keyword",
        Value::Symbol(_) => "Symbol",
        Value::Vec(_) => "Vec",
        Value::List(_) => "List",
        Value::Set(_) => "Set",
        Value::Map(_) => "Map",
        Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_) | Value::Partial(_) => {
            "Fn"
        }
    }
}

fn compare_values(name: &str, left: &Value, right: &Value) -> Result<bool, Clove2Error> {
    match name {
        "=" => Ok(left == right),
        "!=" | "not=" => Ok(left != right),
        "<" | ">" | "<=" | ">=" => {
            let (l, r) = match (left, right) {
                (Value::Int(a), Value::Int(b)) => (*a as f64, *b as f64),
                (Value::Float(a), Value::Float(b)) => (*a, *b),
                (Value::Int(a), Value::Float(b)) => (*a as f64, *b),
                (Value::Float(a), Value::Int(b)) => (*a, *b as f64),
                _ => return Err(Clove2Error::new("comparison expects numeric arguments")),
            };
            Ok(match name {
                "<" => l < r,
                ">" => l > r,
                "<=" => l <= r,
                ">=" => l >= r,
                _ => false,
            })
        }
        _ => Err(Clove2Error::new("unknown compare op")),
    }
}

fn compare_values_ord(left: &Value, right: &Value) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    match (left, right) {
        (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
        (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b).unwrap_or(Ordering::Equal),
        (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)).unwrap_or(Ordering::Equal),
        (Value::Str(a), Value::Str(b)) => a.cmp(b),
        (Value::Keyword(a), Value::Keyword(b)) => a.cmp(b),
        (Value::Symbol(a), Value::Symbol(b)) => a.cmp(b),
        _ => Ordering::Equal,
    }
}

pub fn value_to_key(value: &Value) -> Result<Key, Clove2Error> {
    match value {
        Value::Keyword(name) => Ok(Key::Keyword(Rc::from(name.as_str()))),
        Value::Str(name) => Ok(Key::Str(Rc::from(name.as_str()))),
        Value::Symbol(name) => Ok(Key::Symbol(Rc::from(name.as_str()))),
        Value::Bool(value) => Ok(Key::Bool(*value)),
        Value::Int(value) => Ok(Key::Int(*value)),
        _ => Err(Clove2Error::new(
            "map key must be keyword, string, symbol, bool, or int",
        )),
    }
}

fn normalize_path(path: &str) -> String {
    if path.contains("::") {
        path.replace("::", "/")
    } else {
        path.to_string()
    }
}

fn is_extra_builtin(name: &str) -> bool {
    matches!(name, "fs::delete")
}

fn remove_path(path: &str) -> Result<(), Clove2Error> {
    let meta = fs::metadata(path)
        .map_err(|err| Clove2Error::new(format!("fs::delete failed: {}", err)))?;
    if meta.is_dir() {
        fs::remove_dir_all(path)
            .map_err(|err| Clove2Error::new(format!("fs::delete failed: {}", err)))?;
    } else {
        fs::remove_file(path)
            .map_err(|err| Clove2Error::new(format!("fs::delete failed: {}", err)))?;
    }
    Ok(())
}

fn split_text(text: &str, sep: &Value, limit: Option<usize>) -> Result<Vec<Value>, Clove2Error> {
    match sep {
        Value::Str(sep) => {
            if sep.is_empty() {
                return Ok(text.chars().map(|ch| Value::Str(ch.to_string())).collect());
            }
            let iter: Box<dyn Iterator<Item = &str>> = if let Some(limit) = limit {
                Box::new(text.splitn(limit, sep))
            } else {
                Box::new(text.split(sep))
            };
            Ok(iter.map(|item| Value::Str(item.to_string())).collect())
        }
        Value::Regex(pattern) => {
            let regex = Regex::new(pattern)
                .map_err(|err| Clove2Error::new(format!("split expects regex: {}", err)))?;
            let iter: Box<dyn Iterator<Item = &str>> = if let Some(limit) = limit {
                Box::new(regex.splitn(text, limit))
            } else {
                Box::new(regex.split(text))
            };
            Ok(iter.map(|item| Value::Str(item.to_string())).collect())
        }
        _ => Err(Clove2Error::new("split expects string or regex")),
    }
}

fn replace_in_text(
    text: &str,
    from: &Value,
    to: &str,
    first_only: bool,
) -> Result<String, Clove2Error> {
    match from {
        Value::Str(needle) => {
            if first_only {
                Ok(text.replacen(needle, to, 1))
            } else {
                Ok(text.replace(needle, to))
            }
        }
        Value::Regex(pattern) => {
            let regex = Regex::new(pattern)
                .map_err(|err| Clove2Error::new(format!("replace expects regex: {}", err)))?;
            if first_only {
                Ok(regex.replace(text, to).to_string())
            } else {
                Ok(regex.replace_all(text, to).to_string())
            }
        }
        _ => Err(Clove2Error::new("replace expects string or regex")),
    }
}

fn replace_collection(smap: &Value, coll: &Value) -> Result<Value, Clove2Error> {
    let Value::Map(map) = smap else {
        return Err(Clove2Error::new("replace expects map"));
    };
    match coll {
        Value::Vec(items) => {
            let mut out = Vec::new();
            for item in items.as_ref().iter() {
                if let Ok(key) = value_to_key(item) {
                    if let Some(repl) = map.get(&key) {
                        out.push(repl.clone());
                        continue;
                    }
                }
                out.push(item.clone());
            }
            Ok(Value::vec(out))
        }
        Value::List(items) | Value::Set(items) => {
            let mut out = Vec::new();
            for item in items {
                if let Ok(key) = value_to_key(item) {
                    if let Some(repl) = map.get(&key) {
                        out.push(repl.clone());
                        continue;
                    }
                }
                out.push(item.clone());
            }
            Ok(Value::vec(out))
        }
        Value::Map(coll_map) => {
            let mut out = BTreeMap::new();
            for (key, value) in coll_map.iter() {
                if let Some(repl) = map.get(key) {
                    let new_key = value_to_key(repl)?;
                    out.insert(new_key, value.clone());
                } else {
                    out.insert(key.clone(), value.clone());
                }
            }
            Ok(Value::map(out))
        }
        _ => Err(Clove2Error::new("replace expects collection")),
    }
}

fn resolve_regex_value(value: &Value) -> Result<Regex, Clove2Error> {
    match value {
        Value::Regex(pattern) | Value::Str(pattern) => REGEX_CACHE.with(|cache| {
            if let Some(regex) = cache.borrow().get(pattern) {
                return Ok(regex.clone());
            }
            let regex =
                Regex::new(pattern).map_err(|err| Clove2Error::new(format!("invalid regex: {}", err)))?;
            cache.borrow_mut().insert(pattern.clone(), regex.clone());
            Ok(regex)
        }),
        _ => Err(Clove2Error::new("regex expects string or regex")),
    }
}

fn capture_to_value(captures: &regex::Captures) -> Value {
    if captures.len() == 1 {
        return Value::Str(
            captures
                .get(0)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default(),
        );
    }
    let mut items = Vec::with_capacity(captures.len());
    for idx in 0..captures.len() {
        match captures.get(idx) {
            Some(m) => items.push(Value::Str(m.as_str().to_string())),
            None => items.push(Value::Nil),
        }
    }
    Value::vec(items)
}

fn split_lines_keep_ends(text: &str) -> Vec<Value> {
    let mut items = Vec::new();
    let mut start = 0;
    let mut iter = text.char_indices().peekable();
    while let Some((idx, ch)) = iter.next() {
        if ch == '\n' {
            let end = idx + ch.len_utf8();
            items.push(Value::Str(text[start..end].to_string()));
            start = end;
            continue;
        }
        if ch == '\r' {
            let mut end = idx + ch.len_utf8();
            if let Some(&(next_idx, next_ch)) = iter.peek() {
                if next_ch == '\n' {
                    iter.next();
                    end = next_idx + next_ch.len_utf8();
                }
            }
            items.push(Value::Str(text[start..end].to_string()));
            start = end;
        }
    }
    if start < text.len() {
        items.push(Value::Str(text[start..].to_string()));
    }
    items
}

fn vec_items_ref(value: &Value) -> Option<&[Value]> {
    match value {
        Value::Vec(items) => Some(items.as_ref()),
        Value::List(items) | Value::Set(items) => Some(items.as_slice()),
        _ => None,
    }
}

fn values_to_vec_items(value: &Value, name: &str) -> Result<Vec<Value>, Clove2Error> {
    match value {
        Value::Vec(items) => Ok(items.as_ref().clone()),
        Value::List(items) => Ok(items.clone()),
        Value::Set(items) => Ok(items.clone()),
        Value::Str(text) => Ok(text.chars().map(|ch| Value::Str(ch.to_string())).collect()),
        _ => Err(Clove2Error::new(format!(
            "{} expects vector, list, set, or string",
            name
        ))),
    }
}

fn into_vec_items(value: Value, name: &str) -> Result<Vec<Value>, Clove2Error> {
    match value {
        Value::Vec(items) => match std::rc::Rc::try_unwrap(items) {
            Ok(items) => Ok(items),
            Err(items) => Ok(items.as_ref().clone()),
        },
        Value::List(items) | Value::Set(items) => Ok(items),
        Value::Str(text) => Ok(text.chars().map(|ch| Value::Str(ch.to_string())).collect()),
        _ => Err(Clove2Error::new(format!(
            "{} expects vector, list, set, or string",
            name
        ))),
    }
}

fn measurement_result(
    last_value: Value,
    runs: i64,
    elapsed: std::time::Duration,
) -> BTreeMap<Key, Value> {
    let mut out = BTreeMap::new();
    let elapsed_ms = elapsed.as_secs_f64() * 1000.0;
    out.insert(
        Key::Keyword(Rc::from("elapsed-ms")),
        Value::Float(elapsed_ms),
    );
    out.insert(Key::Keyword(Rc::from("result")), last_value);
    out.insert(Key::Keyword(Rc::from("runs")), Value::Int(runs));
    if runs > 0 {
        let avg_ms = elapsed_ms / runs as f64;
        out.insert(Key::Keyword(Rc::from("avg-ms")), Value::Float(avg_ms));
    }
    out
}

fn flatten_value(
    value: &Value,
    depth: Option<usize>,
    out: &mut Vec<Value>,
) -> Result<(), Clove2Error> {
    if matches!(value, Value::Nil) {
        return Ok(());
    }
    if let Some(items) = vec_items_ref(value) {
        for item in items {
            flatten_nested(item, depth, out)?;
        }
    } else {
        out.push(value.clone());
    }
    Ok(())
}

fn flatten_nested(
    value: &Value,
    depth: Option<usize>,
    out: &mut Vec<Value>,
) -> Result<(), Clove2Error> {
    if matches!(value, Value::Nil) {
        return Ok(());
    }
    match depth {
        None => {
            if let Some(items) = vec_items_ref(value) {
                for item in items {
                    flatten_nested(item, None, out)?;
                }
            } else {
                out.push(value.clone());
            }
        }
        Some(0) => out.push(value.clone()),
        Some(depth) => {
            if let Some(items) = vec_items_ref(value) {
                let next_depth = depth.saturating_sub(1);
                for item in items {
                    flatten_nested(item, Some(next_depth), out)?;
                }
            } else {
                out.push(value.clone());
            }
        }
    }
    Ok(())
}

fn format_pr_value(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Bool(value) => value.to_string(),
        Value::Int(value) => value.to_string(),
        Value::Float(value) => format_float(*value),
        Value::Str(value) => format!("\"{}\"", escape_string(value)),
        Value::Regex(value) => format!("/{}/", escape_string(value)),
        Value::Keyword(value) => format!(":{}", value),
        Value::Symbol(value) => value.clone(),
        Value::Vec(items) => {
            let parts: Vec<String> = items.iter().map(format_pr_value).collect();
            format!("[{}]", parts.join(" "))
        }
        Value::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else {
                let parts: Vec<String> = items.iter().map(format_pr_value).collect();
                format!("({})", parts.join(" "))
            }
        }
        Value::Set(items) => {
            let mut parts: Vec<String> = items.iter().map(format_pr_value).collect();
            parts.sort();
            format!("#{{{}}}", parts.join(" "))
        }
        Value::Map(map) => {
            let mut parts = Vec::new();
            for (key, value) in map.iter() {
                parts.push(format!("{} {}", format_pr_key(key), format_pr_value(value)));
            }
            format!("{{{}}}", parts.join(" "))
        }
        Value::Partial(partial) => partial.desc.clone(),
        Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_) => "<fn>".to_string(),
    }
}

fn format_pr_key(key: &Key) -> String {
    match key {
        Key::Str(value) => format!("\"{}\"", escape_string(value.as_ref())),
        Key::Keyword(value) => format!(":{}", value.as_ref()),
        Key::Symbol(value) => value.to_string(),
        Key::Bool(value) => value.to_string(),
        Key::Int(value) => value.to_string(),
    }
}

fn format_str_value(value: &Value) -> String {
    match value {
        Value::Keyword(value) => value.clone(),
        _ => format_value(value),
    }
}

fn escape_string(value: &str) -> String {
    let mut out = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Bool(value) => value.to_string(),
        Value::Int(value) => value.to_string(),
        Value::Float(value) => format_float(*value),
        Value::Str(value) => value.clone(),
        Value::Regex(value) => format!("/{}/", value),
        Value::Keyword(value) => format!(":{}", value),
        Value::Symbol(value) => value.clone(),
        Value::Vec(items) => {
            let parts: Vec<String> = items.iter().map(format_pr_value).collect();
            format!("[{}]", parts.join(" "))
        }
        Value::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else {
                let parts: Vec<String> = items.iter().map(format_pr_value).collect();
                format!("({})", parts.join(" "))
            }
        }
        Value::Set(items) => {
            let mut parts: Vec<String> = items.iter().map(format_pr_value).collect();
            parts.sort();
            format!("#{{{}}}", parts.join(" "))
        }
        Value::Map(map) => {
            let mut parts = Vec::new();
            for (key, value) in map.iter() {
                parts.push(format!("{} {}", format_pr_key(key), format_pr_value(value)));
            }
            format!("{{{}}}", parts.join(" "))
        }
        Value::Partial(partial) => partial.desc.clone(),
        Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_) => "<fn>".to_string(),
    }
}

fn char_byte_index(text: &str, idx: usize) -> usize {
    if idx == 0 {
        return 0;
    }
    if idx == text.chars().count() {
        return text.len();
    }
    text.char_indices()
        .nth(idx)
        .map(|(offset, _)| offset)
        .unwrap_or(text.len())
}

fn key_to_value(key: &Key) -> Value {
    match key {
        Key::Str(value) => Value::Str(value.to_string()),
        Key::Keyword(value) => Value::Keyword(value.to_string()),
        Key::Symbol(value) => Value::Symbol(value.to_string()),
        Key::Bool(value) => Value::Bool(*value),
        Key::Int(value) => Value::Int(*value),
    }
}

fn format_float(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{:.1}", value)
    } else {
        value.to_string()
    }
}

fn partial_desc(func: &Value, pre_args: &[Value]) -> String {
    let name = match func {
        Value::Builtin(name) => name.clone(),
        Value::Partial(_) => "fn".to_string(),
        Value::Function(_) | Value::NativeFunction(_) => "fn".to_string(),
        _ => "fn".to_string(),
    };
    let args = pre_args
        .iter()
        .map(format_value)
        .collect::<Vec<_>>()
        .join(" ");
    format!("#<partial {} args=[{}] remaining=any>", name, args)
}

pub fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(value) => *value,
        Value::Nil => false,
        _ => true,
    }
}

fn is_use_form(expr: &AstExpr) -> bool {
    matches!(
        expr,
        AstExpr::Call {
            callee,
            ..
        } if matches!(callee.as_ref(), AstExpr::Symbol(sym) if sym == "use")
    )
}

fn parse_type_from_ast(expr: &AstExpr) -> Result<Type, Clove2Error> {
    match expr {
        AstExpr::Symbol(sym) => Type::parse(sym),
        AstExpr::Keyword(sym) => Type::parse(sym),
        AstExpr::Vector(items) => {
            if items.is_empty() {
                return Err(Clove2Error::new(
                    "vector type expression expects at least 1 element",
                ));
            }
            if items.len() == 1 {
                let inner = parse_type_from_ast(&items[0])?;
                return Ok(Type::Vec(Box::new(inner)));
            }
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(parse_type_from_ast(item)?);
            }
            Ok(Type::Tuple(out))
        }
        AstExpr::Map(entries) => {
            if entries.is_empty() {
                return Err(Clove2Error::new(
                    "map type expression expects at least 1 entry",
                ));
            }
            let mut fields: BTreeMap<String, Type> = BTreeMap::new();
            let mut all_keyword = true;
            let mut open = false;
            for (key, value) in entries {
                match key {
                    AstExpr::Keyword(name) => {
                        fields.insert(name.clone(), parse_type_from_ast(value)?);
                    }
                    AstExpr::Symbol(sym) if sym == ".." || sym == "..." => {
                        if open {
                            return Err(Clove2Error::new("shape open marker is duplicated"));
                        }
                        open = true;
                    }
                    _ => {
                        all_keyword = false;
                        break;
                    }
                }
            }
            if all_keyword {
                return Ok(if open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                });
            }
            if entries.len() != 1 {
                return Err(Clove2Error::new(
                    "map type expression expects 1 entry or keyword shape",
                ));
            }
            let (key, value) = &entries[0];
            let key_ty = parse_type_from_ast(key)?;
            let value_ty = parse_type_from_ast(value)?;
            Ok(Type::Map(Box::new(key_ty), Box::new(value_ty)))
        }
        _ => Err(Clove2Error::new("type expression must be symbol")),
    }
}

fn parse_type_from_value(value: &Value) -> Result<Type, Clove2Error> {
    match value {
        Value::Symbol(sym) => Type::parse(sym),
        Value::Keyword(sym) => Type::parse(sym),
        Value::Str(sym) => Type::parse(sym),
        Value::Vec(items) => {
            if items.is_empty() {
                return Err(Clove2Error::new(
                    "vector type expression expects at least 1 element",
                ));
            }
            if items.len() == 1 {
                let inner = parse_type_from_value(&items[0])?;
                return Ok(Type::Vec(Box::new(inner)));
            }
            let mut out = Vec::with_capacity(items.len());
            for item in items.as_ref() {
                out.push(parse_type_from_value(item)?);
            }
            Ok(Type::Tuple(out))
        }
        Value::Map(entries) => {
            if entries.is_empty() {
                return Err(Clove2Error::new(
                    "map type expression expects at least 1 entry",
                ));
            }
            let mut fields: BTreeMap<String, Type> = BTreeMap::new();
            let mut all_keyword = true;
            let mut open = false;
            for (key, value) in entries.iter() {
                match key {
                    Key::Keyword(name) => {
                        fields.insert(name.to_string(), parse_type_from_value(value)?);
                    }
                    Key::Symbol(sym) if sym.as_ref() == ".." || sym.as_ref() == "..." => {
                        if open {
                            return Err(Clove2Error::new("shape open marker is duplicated"));
                        }
                        open = true;
                    }
                    _ => {
                        all_keyword = false;
                        break;
                    }
                }
            }
            if all_keyword {
                return Ok(if open {
                    Type::open_shape(fields)
                } else {
                    Type::shape(fields)
                });
            }
            if entries.len() != 1 {
                return Err(Clove2Error::new(
                    "map type expression expects 1 entry or keyword shape",
                ));
            }
            let (key, value) = entries.iter().next().unwrap();
            let key_ty = parse_type_from_key(key)?;
            let value_ty = parse_type_from_value(value)?;
            Ok(Type::Map(Box::new(key_ty), Box::new(value_ty)))
        }
        _ => Err(Clove2Error::new("type expression must be symbol")),
    }
}

fn parse_type_from_key(key: &Key) -> Result<Type, Clove2Error> {
    match key {
        Key::Symbol(sym) => Type::parse(sym.as_ref()),
        Key::Keyword(sym) => Type::parse(sym.as_ref()),
        Key::Str(sym) => Type::parse(sym.as_ref()),
        _ => Err(Clove2Error::new("type expression must be symbol")),
    }
}

fn json_option_key(value: &Value) -> Result<&str, Clove2Error> {
    match value {
        Value::Keyword(name) => Ok(name.as_str()),
        Value::Symbol(name) => Ok(name.as_str()),
        _ => Err(Clove2Error::new(
            "json option key must be keyword or symbol",
        )),
    }
}

fn json_to_value(value: &serde_json::Value) -> Value {
    match value {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(value) => Value::Bool(*value),
        serde_json::Value::Number(value) => {
            if let Some(num) = value.as_i64() {
                Value::Int(num)
            } else if let Some(num) = value.as_f64() {
                Value::Float(num)
            } else {
                Value::Nil
            }
        }
        serde_json::Value::String(value) => Value::Str(value.clone()),
        serde_json::Value::Array(items) => Value::vec(items.iter().map(json_to_value).collect()),
        serde_json::Value::Object(map) => {
            let mut out = BTreeMap::new();
            for (key, value) in map {
                out.insert(Key::Str(Rc::from(key.as_str())), json_to_value(value));
            }
            Value::map(out)
        }
    }
}

fn value_to_json(value: &Value) -> Result<serde_json::Value, Clove2Error> {
    match value {
        Value::Nil => Ok(serde_json::Value::Null),
        Value::Bool(value) => Ok(serde_json::Value::Bool(*value)),
        Value::Int(value) => Ok(serde_json::Value::Number((*value).into())),
        Value::Float(value) => serde_json::Number::from_f64(*value)
            .map(serde_json::Value::Number)
            .ok_or_else(|| Clove2Error::new("json::write-string expects finite number")),
        Value::Str(value) => Ok(serde_json::Value::String(value.clone())),
        Value::Keyword(value) => Ok(serde_json::Value::String(format!(":{}", value))),
        Value::Symbol(value) => Ok(serde_json::Value::String(value.clone())),
        Value::Regex(value) => Ok(serde_json::Value::String(format!("/{}/", value))),
        Value::Vec(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items.as_ref().iter() {
                out.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        Value::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        Value::Set(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        Value::Map(map) => {
            let mut out = serde_json::Map::new();
            for (key, val) in map.iter() {
                out.insert(key_to_json_string(key), value_to_json(val)?);
            }
            Ok(serde_json::Value::Object(out))
        }
        Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_) | Value::Partial(_) => {
            Err(Clove2Error::new(
                "json::write-string cannot serialize function",
            ))
        }
    }
}

fn key_to_json_string(key: &Key) -> String {
    match key {
        Key::Str(value) => value.to_string(),
        Key::Keyword(value) => format!(":{}", value.as_ref()),
        Key::Symbol(value) => value.to_string(),
        Key::Bool(value) => value.to_string(),
        Key::Int(value) => value.to_string(),
    }
}

fn value_to_number(value: &Value, name: &str) -> Result<f64, Clove2Error> {
    match value {
        Value::Int(value) => Ok(*value as f64),
        Value::Float(value) => Ok(*value),
        _ => Err(Clove2Error::new(format!("{} expects number", name))),
    }
}

fn value_to_int(value: &Value) -> Result<i64, Clove2Error> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Float(value) => Ok(*value as i64),
        Value::Str(value) => value
            .trim()
            .parse::<i64>()
            .map_err(|_| Clove2Error::new("range expects int arguments")),
        _ => Err(Clove2Error::new("range expects int arguments")),
    }
}

pub fn run_str(source: &str) -> Result<Value, Clove2Error> {
    let forms = read_all(source)?;
    let (use_cfg, _) = parse_use_directives(&forms)?;
    let ast = parse_forms(&forms)?;
    let mut eval = Evaluator::default();
    eval.set_mut_mode(use_cfg.default_mut_mode());
    eval.eval_program(&ast)
}

pub fn run_program(items: &[TopLevel]) -> Result<Value, Clove2Error> {
    let mut eval = Evaluator::default();
    eval.eval_program(items)
}

pub fn run_program_with_mut_mode(items: &[TopLevel], mode: MutMode) -> Result<Value, Clove2Error> {
    let mut eval = Evaluator::default();
    eval.set_mut_mode(mode);
    eval.eval_program(items)
}

pub fn apply_builtin_value(
    name: &str,
    args: Vec<Value>,
    mode: MutMode,
) -> Result<Value, Clove2Error> {
    let mut eval = Evaluator::default();
    eval.set_mut_mode(mode);
    eval.eval_builtin_value(name, args)
}

pub fn run_file(path: &str) -> Result<Value, Clove2Error> {
    let content = fs::read_to_string(path)
        .map_err(|err| Clove2Error::new(format!("failed to read {}: {}", path, err)))?;
    run_str(&content)
}
