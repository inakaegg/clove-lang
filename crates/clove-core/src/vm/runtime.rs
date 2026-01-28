use std::sync::Arc;

use crate::ast::{FnArity, NativeFn, Span, Value};
use crate::builtins::core::{doc_set, meta_lookup, meta_set};
use crate::cow::{HashMap as CowHashMap, Vector};
use crate::dynamic_vars;
use crate::env::{new_ref, Env, EnvRef};
use crate::error::CloveError;
use crate::eval::{
    call_callable, call_native_func, current_file_name, register_vm_multi_arity, Evaluator,
};
use crate::profiler;
use crate::runtime::{apply_builtin, RuntimeCtx};
use crate::vm::bytecode::{BuiltinId, Capture, FunctionPrototype, Instruction};
use crate::vm::compiler::CompiledChunk;
use crate::vm::error::VmError;
use crate::vm::profiler as vm_profiler;
use im::HashSet;

pub fn run_chunk(
    compiled: &CompiledChunk,
    evaluator: &Evaluator,
    env: EnvRef,
) -> Result<Value, VmError> {
    run_vm(
        compiled.chunk.code(),
        compiled.chunk.constants(),
        compiled.chunk.functions(),
        compiled.chunk.globals(),
        evaluator,
        env,
        Vec::new(),
        current_file_name().unwrap_or_else(|| "<chunk>".to_string()),
    )
}

pub fn run_chunk_with_locals(
    compiled: &CompiledChunk,
    evaluator: &Evaluator,
    env: EnvRef,
    locals: Vec<Value>,
) -> Result<Value, VmError> {
    run_vm(
        compiled.chunk.code(),
        compiled.chunk.constants(),
        compiled.chunk.functions(),
        compiled.chunk.globals(),
        evaluator,
        env,
        slots_from_values(locals),
        current_file_name().unwrap_or_else(|| "<chunk>".to_string()),
    )
}

pub(crate) fn run_prototype(
    proto: &FunctionPrototype,
    evaluator: &Evaluator,
    env: EnvRef,
    locals: Vec<Value>,
) -> Result<Value, VmError> {
    run_vm(
        proto.chunk.code(),
        proto.chunk.constants(),
        proto.chunk.functions(),
        proto.chunk.globals(),
        evaluator,
        env,
        slots_from_values(locals),
        proto.name.clone().unwrap_or_else(|| "vm-fn".to_string()),
    )
}

fn run_vm(
    code: &[Instruction],
    constants: &[Value],
    functions: &[Arc<FunctionPrototype>],
    globals: &[String],
    evaluator: &Evaluator,
    env: EnvRef,
    locals: Vec<LocalSlot>,
    label: String,
) -> Result<Value, VmError> {
    let _guard = profiler::enter("vm.run");
    let _vm_guard = vm_profiler::enter_chunk(&label);
    let mut vm = Vm {
        evaluator,
        env,
        code,
        constants,
        functions,
        globals,
        ip: 0,
        stack: Vec::new(),
        locals_len: locals.len(),
        frame_base: 0,
        locals,
        global_cache: vec![None; globals.len()],
    };
    vm.run()
}

#[derive(Clone)]
enum LocalSlot {
    Value(Value),
    I64(i64),
    F64(f64),
}

impl LocalSlot {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Int(n) => LocalSlot::I64(n),
            Value::Float(n) => LocalSlot::F64(n),
            other => LocalSlot::Value(other),
        }
    }

    fn to_value(&self) -> Value {
        match self {
            LocalSlot::Value(value) => value.clone(),
            LocalSlot::I64(n) => Value::Int(*n),
            LocalSlot::F64(n) => Value::Float(*n),
        }
    }

    fn into_value(self) -> Value {
        match self {
            LocalSlot::Value(value) => value,
            LocalSlot::I64(n) => Value::Int(n),
            LocalSlot::F64(n) => Value::Float(n),
        }
    }

    fn as_i64(&self) -> Option<i64> {
        match self {
            LocalSlot::I64(n) => Some(*n),
            LocalSlot::Value(Value::Int(n)) => Some(*n),
            _ => None,
        }
    }

    fn as_f64(&self) -> Option<f64> {
        match self {
            LocalSlot::F64(n) => Some(*n),
            LocalSlot::I64(n) => Some(*n as f64),
            LocalSlot::Value(Value::Float(n)) => Some(*n),
            LocalSlot::Value(Value::Int(n)) => Some(*n as f64),
            _ => None,
        }
    }

    fn as_f64_strict(&self) -> Option<f64> {
        match self {
            LocalSlot::F64(n) => Some(*n),
            LocalSlot::Value(Value::Float(n)) => Some(*n),
            _ => None,
        }
    }

    fn is_float(&self) -> bool {
        matches!(self, LocalSlot::F64(_) | LocalSlot::Value(Value::Float(_)))
    }
}

fn slots_from_values(values: Vec<Value>) -> Vec<LocalSlot> {
    values.into_iter().map(LocalSlot::from_value).collect()
}

struct Vm<'a> {
    evaluator: &'a Evaluator,
    env: EnvRef,
    code: &'a [Instruction],
    constants: &'a [Value],
    functions: &'a [Arc<FunctionPrototype>],
    globals: &'a [String],
    ip: usize,
    stack: Vec<LocalSlot>,
    locals: Vec<LocalSlot>,
    locals_len: usize,
    frame_base: usize,
    global_cache: Vec<Option<GlobalCacheEntry>>,
}

impl<'a> Vm<'a> {
    fn push_value(&mut self, value: Value) {
        self.stack.push(LocalSlot::from_value(value));
    }

    fn push_slot(&mut self, slot: LocalSlot) {
        self.stack.push(slot);
    }

    fn pop_slot(&mut self, span: Span) -> Result<LocalSlot, VmError> {
        self.stack
            .pop()
            .ok_or_else(|| self.runtime_error(span, "stack underflow"))
    }

    fn run(&mut self) -> Result<Value, VmError> {
        while self.ip < self.code.len() {
            let instr = self.code[self.ip].clone();
            self.ip += 1;
            vm_profiler::count_op(&instr);
            match instr {
                Instruction::Const(idx, _span) => {
                    let value = self
                        .constants
                        .get(idx)
                        .cloned()
                        .ok_or_else(|| self.runtime_error(_span, "invalid constant index"))?;
                    self.push_value(value);
                }
                Instruction::ConstI64(value, _span) => {
                    self.push_slot(LocalSlot::I64(value));
                }
                Instruction::ConstF64(value, _span) => {
                    self.push_slot(LocalSlot::F64(value));
                }
                Instruction::ConstBool(value, _span) => {
                    self.push_slot(LocalSlot::Value(Value::Bool(value)));
                }
                Instruction::ConstNil(_span) => {
                    self.push_slot(LocalSlot::Value(Value::Nil));
                }
                Instruction::LoadLocal(idx, span) => {
                    let idx = self.frame_base + idx;
                    let slot = self
                        .locals
                        .get(idx)
                        .filter(|_| idx < self.locals_len)
                        .cloned()
                        .ok_or_else(|| self.runtime_error(span, "undefined local"))?;
                    self.push_slot(slot);
                }
                Instruction::StoreLocal(idx, span) => {
                    let slot = self.pop_slot(span)?;
                    let idx = self.frame_base + idx;
                    if idx >= self.locals.len() {
                        self.locals.resize(idx + 1, LocalSlot::Value(Value::Nil));
                    }
                    self.locals[idx] = slot;
                    if idx + 1 > self.locals_len {
                        self.locals_len = idx + 1;
                    }
                }
                Instruction::LoadGlobalId(id, span) => {
                    let value = self.load_global_id(id, span)?;
                    self.push_value(value);
                }
                Instruction::DefGlobalId(id, is_private, span) => {
                    let value = self.pop(span)?;
                    let name = self
                        .globals
                        .get(id)
                        .ok_or_else(|| self.runtime_error(span, "invalid global id"))?;
                    {
                        let mut writer = self.env.write().unwrap();
                        writer.set(name, value.clone());
                    }
                    let version = RuntimeCtx::try_with_current(|ctx| {
                        ctx.bump_global_version(name, &self.env);
                        if is_private {
                            ctx.mark_private_name(&self.env, name);
                        }
                        ctx.clear_imported_flag(&self.env, name);
                        Ok(ctx.global_version(name, &self.env))
                    })
                    .and_then(Result::ok)
                    .unwrap_or(0);
                    if let Some(slot) = self.global_cache.get_mut(id) {
                        *slot = Some(GlobalCacheEntry {
                            version,
                            value: value.clone(),
                        });
                    }
                    self.push_value(value);
                }
                Instruction::DefnGlobalId(id, is_private, form, span) => {
                    let value = self.pop(span)?;
                    let name = self
                        .globals
                        .get(id)
                        .ok_or_else(|| self.runtime_error(span, "invalid global id"))?;
                    let eval_value = self
                        .evaluator
                        .eval(&form, self.env.clone())
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    if let Value::Map(meta_map) = meta_lookup(&eval_value) {
                        meta_set(value.clone(), Value::Map(meta_map));
                    }
                    match &eval_value {
                        Value::Lambda { doc, .. } | Value::MultiLambda { doc, .. } => {
                            if let Some(doc) = doc.clone() {
                                doc_set(value.clone(), doc);
                            }
                        }
                        _ => {}
                    }
                    {
                        let mut writer = self.env.write().unwrap();
                        writer.set(name, value.clone());
                    }
                    let version = RuntimeCtx::try_with_current(|ctx| {
                        ctx.bump_global_version(name, &self.env);
                        if is_private {
                            ctx.mark_private_name(&self.env, name);
                        }
                        ctx.clear_imported_flag(&self.env, name);
                        Ok(ctx.global_version(name, &self.env))
                    })
                    .and_then(Result::ok)
                    .unwrap_or(0);
                    if let Some(slot) = self.global_cache.get_mut(id) {
                        *slot = Some(GlobalCacheEntry {
                            version,
                            value: value.clone(),
                        });
                    }
                    self.push_value(value);
                }
                Instruction::DefLocalForm(id, form, span) => {
                    let name = self
                        .globals
                        .get(id)
                        .ok_or_else(|| self.runtime_error(span, "invalid global id"))?;
                    let (local_name, value) = self
                        .evaluator
                        .eval_local_defn_value(&form, self.env.clone())
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    {
                        let mut writer = self.env.write().unwrap();
                        writer.set(&local_name, value.clone());
                    }
                    if local_name == *name {
                        let version = self.global_version(name);
                        if let Some(slot) = self.global_cache.get_mut(id) {
                            *slot = Some(GlobalCacheEntry { version, value });
                        }
                    }
                }
                Instruction::DefLocalId(id, span) => {
                    let value = self.pop(span)?;
                    let name = self
                        .globals
                        .get(id)
                        .ok_or_else(|| self.runtime_error(span, "invalid global id"))?;
                    {
                        let mut writer = self.env.write().unwrap();
                        writer.set(name, value.clone());
                    }
                    let version = self.global_version(name);
                    if let Some(slot) = self.global_cache.get_mut(id) {
                        *slot = Some(GlobalCacheEntry { version, value });
                    }
                }
                Instruction::MakeClosure(func_idx, captures, span) => {
                    let proto = self
                        .functions
                        .get(func_idx)
                        .ok_or_else(|| self.runtime_error(span, "invalid function index"))?;
                    let captured_env = self.capture_env(&captures, span)?;
                    let callable = make_vm_function(proto.clone(), captured_env);
                    self.push_value(callable);
                }
                Instruction::MakeMultiClosure(func_indices, captures, name, span) => {
                    let mut protos = Vec::with_capacity(func_indices.len());
                    for idx in func_indices {
                        let proto = self
                            .functions
                            .get(idx)
                            .ok_or_else(|| self.runtime_error(span, "invalid function index"))?;
                        protos.push(proto.clone());
                    }
                    let captured_env = self.capture_env(&captures, span)?;
                    let callable = make_vm_multi_function(protos, captured_env, name);
                    self.push_value(callable);
                }
                Instruction::AttachMeta(meta_idx, span) => {
                    let value = self.pop(span)?;
                    let meta = self
                        .constants
                        .get(meta_idx)
                        .cloned()
                        .ok_or_else(|| self.runtime_error(span, "invalid constant index"))?;
                    meta_set(value.clone(), meta);
                    self.push_value(value);
                }
                Instruction::AttachDoc(doc_idx, span) => {
                    let value = self.pop(span)?;
                    let doc_value = self
                        .constants
                        .get(doc_idx)
                        .cloned()
                        .ok_or_else(|| self.runtime_error(span, "invalid constant index"))?;
                    let doc = match doc_value {
                        Value::String(text) => text,
                        _ => {
                            return Err(self.runtime_error(span, "doc must be string"));
                        }
                    };
                    doc_set(value.clone(), doc);
                    self.push_value(value);
                }
                Instruction::Dup(span) => {
                    let slot = self
                        .stack
                        .last()
                        .cloned()
                        .ok_or_else(|| self.runtime_error(span, "stack underflow"))?;
                    self.push_slot(slot);
                }
                Instruction::MakeVector(count, span) => {
                    let mut values = Vec::with_capacity(count);
                    for _ in 0..count {
                        values.push(self.pop(span)?);
                    }
                    values.reverse();
                    let mut vec = Vector::new();
                    for value in values {
                        vec.push_back(value);
                    }
                    self.push_value(Value::Vector(vec));
                }
                Instruction::MakeSet(count, span) => {
                    let mut values = Vec::with_capacity(count);
                    for _ in 0..count {
                        values.push(self.pop(span)?);
                    }
                    values.reverse();
                    let mut set = HashSet::new();
                    for value in values {
                        set.insert(value);
                    }
                    self.push_value(Value::Set(set));
                }
                Instruction::MakeMap(keys, span) => {
                    let mut values = Vec::with_capacity(keys.len());
                    for _ in 0..keys.len() {
                        values.push(self.pop(span)?);
                    }
                    values.reverse();
                    let mut map = CowHashMap::new();
                    for (key, value) in keys.iter().cloned().zip(values) {
                        map.insert(key, value);
                    }
                    self.push_value(Value::Map(map));
                }
                Instruction::TruncateLocals(count, _span) => {
                    let new_len = self.frame_base + count;
                    if new_len > self.locals.len() {
                        self.locals.resize(new_len, LocalSlot::Value(Value::Nil));
                    }
                    self.locals_len = new_len;
                }
                Instruction::Jump(target, _span) => {
                    self.ip = target;
                }
                Instruction::JumpIfFalse(target, span) => {
                    let cond = self.pop(span)?;
                    if !is_truthy(&cond) {
                        self.ip = target;
                    }
                }
                Instruction::JumpIfFalseBool(target, span) => {
                    let cond_slot = self.pop_slot(span)?;
                    let is_true = match cond_slot {
                        LocalSlot::Value(Value::Bool(value)) => value,
                        LocalSlot::Value(Value::Nil) => false,
                        other => is_truthy(&other.into_value()),
                    };
                    if !is_true {
                        self.ip = target;
                    }
                }
                Instruction::Call0(span) => {
                    let callable = self.pop(span)?;
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, &[]),
                        _ => call_callable(callable, Vec::new()),
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Call1(span) => {
                    let arg0 = self.pop(span)?;
                    let callable = self.pop(span)?;
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, std::slice::from_ref(&arg0)),
                        _ => call_callable(callable, vec![arg0]),
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Call2(span) => {
                    let arg1 = self.pop(span)?;
                    let arg0 = self.pop(span)?;
                    let callable = self.pop(span)?;
                    let args = [arg0, arg1];
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, &args),
                        other => {
                            let args = Vec::from(args);
                            call_callable(other, args)
                        }
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Call3(span) => {
                    let arg2 = self.pop(span)?;
                    let arg1 = self.pop(span)?;
                    let arg0 = self.pop(span)?;
                    let callable = self.pop(span)?;
                    let args = [arg0, arg1, arg2];
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, &args),
                        other => {
                            let args = Vec::from(args);
                            call_callable(other, args)
                        }
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Call4(span) => {
                    let arg3 = self.pop(span)?;
                    let arg2 = self.pop(span)?;
                    let arg1 = self.pop(span)?;
                    let arg0 = self.pop(span)?;
                    let callable = self.pop(span)?;
                    let args = [arg0, arg1, arg2, arg3];
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, &args),
                        other => {
                            let args = Vec::from(args);
                            call_callable(other, args)
                        }
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Call(argc, span) => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.pop(span)?);
                    }
                    args.reverse();
                    let callable = self.pop(span)?;
                    let result = match callable {
                        Value::Func(func) => call_native_func(&func, &args),
                        _ => call_callable(callable, args),
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::Apply(argc, span) => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.pop(span)?);
                    }
                    args.reverse();
                    let head = self.pop(span)?;
                    if let Some(result) = self.try_fast_apply(&head, &args) {
                        self.push_value(result);
                        continue;
                    }
                    let result = if let Value::Func(func) = &head {
                        call_native_func(func, &args)
                    } else if matches!(
                        &head,
                        Value::Partial { .. }
                            | Value::Compose { .. }
                            | Value::Lambda { .. }
                            | Value::MultiLambda { .. }
                            | Value::ForeignCallable { .. }
                    ) {
                        call_callable(head, args)
                    } else {
                        let mut apply_args = Vec::with_capacity(argc + 1);
                        apply_args.push(head);
                        apply_args.extend(args);
                        apply_builtin(&apply_args)
                    }
                    .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::OopIndex(span) => {
                    let key = self.pop(span)?;
                    let target = self.pop(span)?;
                    let result = self
                        .evaluator
                        .oop_index_value(target, key, self.env.clone(), span)
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::OopMethod(argc, span) => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.pop(span)?);
                    }
                    args.reverse();
                    let name_val = self.pop(span)?;
                    let target = self.pop(span)?;
                    let result = self
                        .evaluator
                        .oop_method_value(target, name_val, args, self.env.clone(), span)
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                    self.push_value(result);
                }
                Instruction::AddI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Add) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        let value = (a as f64) + (b as f64);
                        self.push_slot(LocalSlot::I64(value as i64));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Add,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::AddF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Add) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if lhs_slot.is_float() || rhs_slot.is_float() {
                        if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                            self.push_slot(LocalSlot::F64(a + b));
                            continue;
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Add,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::SubI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Sub) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        let value = (a as f64) - (b as f64);
                        self.push_slot(LocalSlot::I64(value as i64));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Sub,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::SubF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Sub) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if lhs_slot.is_float() || rhs_slot.is_float() {
                        if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                            self.push_slot(LocalSlot::F64(a - b));
                            continue;
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Sub,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::MulI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Mul) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        let value = (a as f64) * (b as f64);
                        self.push_slot(LocalSlot::I64(value as i64));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Mul,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::MulF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Mul) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if lhs_slot.is_float() || rhs_slot.is_float() {
                        if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                            self.push_slot(LocalSlot::F64(a * b));
                            continue;
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Mul,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::DivF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Div) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if lhs_slot.is_float() || rhs_slot.is_float() {
                        if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                            if b != 0.0 {
                                self.push_slot(LocalSlot::F64(a / b));
                                continue;
                            }
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Div,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::EqI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Eq) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        self.push_value(Value::Bool(a == b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Eq,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::EqF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Eq) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_f64_strict(), rhs_slot.as_f64_strict())
                    {
                        self.push_value(Value::Bool(a == b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Eq,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::LtI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Lt) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        self.push_value(Value::Bool((a as f64) < (b as f64)));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Lt,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::LtF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Lt) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                        self.push_value(Value::Bool(a < b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Lt,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::LeI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Le) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        self.push_value(Value::Bool((a as f64) <= (b as f64)));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Le,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::LeF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Le) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                        self.push_value(Value::Bool(a <= b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Le,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::GtI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Gt) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        self.push_value(Value::Bool((a as f64) > (b as f64)));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Gt,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::GtF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Gt) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                        self.push_value(Value::Bool(a > b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Gt,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::GeI64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Ge) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_i64(), rhs_slot.as_i64()) {
                        self.push_value(Value::Bool((a as f64) >= (b as f64)));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Ge,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::GeF64(global_id, span) => {
                    let rhs_slot = self.pop_slot(span)?;
                    let lhs_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Ge) {
                        let result = call_callable(
                            callable,
                            vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        )
                        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let (Some(a), Some(b)) = (lhs_slot.as_f64(), rhs_slot.as_f64()) {
                        self.push_value(Value::Bool(a >= b));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Ge,
                        callable,
                        vec![lhs_slot.into_value(), rhs_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::IncI64(global_id, span) => {
                    let value_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Inc) {
                        let result = call_callable(callable, vec![value_slot.into_value()])
                            .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let Some(n) = value_slot.as_i64() {
                        let next = (n as f64) + 1.0;
                        self.push_slot(LocalSlot::I64(next as i64));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Inc,
                        callable,
                        vec![value_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::IncF64(global_id, span) => {
                    let value_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Inc) {
                        let result = call_callable(callable, vec![value_slot.into_value()])
                            .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if value_slot.is_float() {
                        if let Some(value) = value_slot.as_f64_strict() {
                            self.push_slot(LocalSlot::F64(value + 1.0));
                            continue;
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Inc,
                        callable,
                        vec![value_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::DecI64(global_id, span) => {
                    let value_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Dec) {
                        let result = call_callable(callable, vec![value_slot.into_value()])
                            .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if let Some(n) = value_slot.as_i64() {
                        let next = (n as f64) - 1.0;
                        self.push_slot(LocalSlot::I64(next as i64));
                        continue;
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Dec,
                        callable,
                        vec![value_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::DecF64(global_id, span) => {
                    let value_slot = self.pop_slot(span)?;
                    let callable = self.load_global_id(global_id, span)?;
                    if !is_builtin_value(&callable, BuiltinId::Dec) {
                        let result = call_callable(callable, vec![value_slot.into_value()])
                            .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
                        self.push_value(result);
                        continue;
                    }
                    if value_slot.is_float() {
                        if let Some(value) = value_slot.as_f64_strict() {
                            self.push_slot(LocalSlot::F64(value - 1.0));
                            continue;
                        }
                    }
                    let result = self.call_builtin_with_callable(
                        BuiltinId::Dec,
                        callable,
                        vec![value_slot.into_value()],
                        span,
                    )?;
                    self.push_value(result);
                }
                Instruction::CallBuiltin(builtin_id, argc, global_id, span) => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.pop(span)?);
                    }
                    args.reverse();
                    let callable = self.load_global_id(global_id, span)?;
                    let result =
                        self.call_builtin_with_callable(builtin_id, callable, args, span)?;
                    self.push_value(result);
                }
                Instruction::Pop(span) => {
                    let _ = self.pop(span)?;
                }
                Instruction::Return(span) => {
                    let value = self.pop(span)?;
                    return Ok(value);
                }
            }
        }
        Err(self.runtime_error(
            Span {
                line: 0,
                col: 0,
                index: 0,
            },
            "vm ended without return",
        ))
    }

    fn pop(&mut self, span: Span) -> Result<Value, VmError> {
        Ok(self.pop_slot(span)?.into_value())
    }

    fn runtime_error(&self, span: Span, msg: &str) -> VmError {
        let err = CloveError::runtime(msg.to_string());
        VmError::runtime(self.decorate_error(err, span))
    }

    fn decorate_error(&self, err: CloveError, span: Span) -> CloveError {
        err.with_span(span)
            .with_file(current_file_name())
            .with_env(self.env.clone())
    }

    fn load_global_id(&mut self, id: usize, span: Span) -> Result<Value, VmError> {
        let name = self
            .globals
            .get(id)
            .ok_or_else(|| self.runtime_error(span, "invalid global id"))?;
        if dynamic_vars::is_dynamic_var(name) {
            return self.resolve_symbol(name, span);
        }
        let version = self.global_version(name);
        if let Some(entry) = self.global_cache.get(id).and_then(|entry| entry.as_ref()) {
            if entry.version == version {
                return Ok(entry.value.clone());
            }
        }
        let value = self.resolve_symbol(name, span)?;
        if let Some(slot) = self.global_cache.get_mut(id) {
            *slot = Some(GlobalCacheEntry {
                version,
                value: value.clone(),
            });
        }
        Ok(value)
    }

    fn resolve_symbol(&self, name: &str, span: Span) -> Result<Value, VmError> {
        self.evaluator
            .resolve_symbol_value(name, span, &self.env)
            .map_err(|err| VmError::runtime(self.decorate_error(err, span)))
    }

    fn global_version(&self, name: &str) -> u64 {
        RuntimeCtx::try_with_current(|ctx| Ok(ctx.global_version(name, &self.env)))
            .and_then(Result::ok)
            .unwrap_or(0)
    }

    fn try_fast_builtin(&self, builtin_id: BuiltinId, args: &[Value]) -> Option<Value> {
        match builtin_id {
            BuiltinId::Add => fast_add(args),
            BuiltinId::Sub => fast_sub(args),
            BuiltinId::Mul => fast_mul(args),
            BuiltinId::Div => fast_div(args),
            BuiltinId::Eq => fast_eq(args),
            BuiltinId::Lt => fast_cmp(args, |a, b| a < b),
            BuiltinId::Le => fast_cmp(args, |a, b| a <= b),
            BuiltinId::Gt => fast_cmp(args, |a, b| a > b),
            BuiltinId::Ge => fast_cmp(args, |a, b| a >= b),
            BuiltinId::Inc => fast_inc(args),
            BuiltinId::Dec => fast_dec(args),
        }
    }

    fn call_builtin_with_callable(
        &self,
        builtin_id: BuiltinId,
        callable: Value,
        args: Vec<Value>,
        span: Span,
    ) -> Result<Value, VmError> {
        let result = match self.try_fast_builtin(builtin_id, &args) {
            Some(value) if is_builtin_value(&callable, builtin_id) => Ok(value),
            _ => call_callable(callable, args),
        }
        .map_err(|err| VmError::runtime(self.decorate_error(err, span)))?;
        Ok(result)
    }

    fn try_fast_apply(&self, head: &Value, args: &[Value]) -> Option<Value> {
        let builtin_id = builtin_id_for_value(head)?;
        if !is_builtin_value(head, builtin_id) {
            return None;
        }
        self.try_fast_builtin(builtin_id, args)
    }

    fn capture_env(&self, captures: &[Capture], span: Span) -> Result<EnvRef, VmError> {
        let child = new_ref(Env::new_child(self.env.clone()));
        {
            let mut writer = child.write().unwrap();
            for capture in captures {
                if capture.index >= self.locals_len {
                    return Err(self.runtime_error(span, "invalid capture"));
                }
                let value = self
                    .locals
                    .get(capture.index)
                    .map(LocalSlot::to_value)
                    .ok_or_else(|| self.runtime_error(span, "invalid capture"))?;
                writer.set(&capture.name, value);
            }
        }
        Ok(child)
    }
}

#[derive(Clone)]
struct GlobalCacheEntry {
    version: u64,
    value: Value,
}

#[derive(Clone)]
struct VmFunction {
    proto: Arc<FunctionPrototype>,
    env: EnvRef,
}

impl VmFunction {
    fn call(&self, args: &[Value]) -> Result<Value, CloveError> {
        let locals = pack_vm_args(args, self.proto.param_count, self.proto.has_rest)?;
        let env = if self.proto.needs_env || self.proto.name.is_some() {
            let child = new_ref(Env::new_child(self.env.clone()));
            if let Some(name) = &self.proto.name {
                let callable = make_vm_function(self.proto.clone(), self.env.clone());
                child.write().unwrap().set(name, callable);
            }
            child
        } else {
            self.env.clone()
        };
        RuntimeCtx::with_current(|ctx| ctx.run_vm_prototype(&self.proto, env, locals))
    }
}

fn make_vm_function(proto: Arc<FunctionPrototype>, env: EnvRef) -> Value {
    let arity = if proto.has_rest {
        FnArity::at_least(proto.param_count)
    } else {
        FnArity::exact(proto.param_count)
    };
    let name = proto.name.clone().unwrap_or_else(|| "vm-fn".to_string());
    let func = Arc::new(VmFunction { proto, env });
    Value::native_fn_with_name(name, arity, move |args| func.call(args))
}

fn make_vm_multi_function(
    protos: Vec<Arc<FunctionPrototype>>,
    env: EnvRef,
    name: Option<String>,
) -> Value {
    let arities: Vec<(usize, bool)> = protos
        .iter()
        .map(|proto| (proto.param_count, proto.has_rest))
        .collect();
    let (min_args, max_args) = multi_arity_bounds(&arities);
    let arity = FnArity::new(min_args, max_args);
    let label = name.clone().unwrap_or_else(|| "vm-fn".to_string());
    let protos = Arc::new(protos);
    let env = env.clone();
    let func = Arc::new_cyclic(|weak| {
        let weak_self = weak.clone();
        let protos = protos.clone();
        let env = env.clone();
        let name = name.clone();
        NativeFn::with_name(arity, label.clone(), move |args| {
            let arg_len = args.len();
            let proto = select_multi_proto(&protos, arg_len)
                .ok_or_else(|| multi_arity_error(&protos, arg_len))?;
            let locals = pack_vm_args(args, proto.param_count, proto.has_rest)?;
            let env = if proto.needs_env || name.is_some() {
                let child = new_ref(Env::new_child(env.clone()));
                if let Some(name) = &name {
                    if let Some(func) = weak_self.upgrade() {
                        child.write().unwrap().set(name, Value::Func(func));
                    }
                }
                child
            } else {
                env.clone()
            };
            RuntimeCtx::with_current(|ctx| ctx.run_vm_prototype(proto, env, locals))
        })
    });
    register_vm_multi_arity(&func, arities);
    Value::Func(func)
}

fn select_multi_proto(
    protos: &[Arc<FunctionPrototype>],
    provided: usize,
) -> Option<&Arc<FunctionPrototype>> {
    for proto in protos {
        if !proto.has_rest && provided == proto.param_count {
            return Some(proto);
        }
        if proto.has_rest && provided >= proto.param_count {
            return Some(proto);
        }
    }
    None
}

fn multi_arity_bounds(clauses: &[(usize, bool)]) -> (usize, Option<usize>) {
    let mut min_args = usize::MAX;
    let mut max_args: Option<usize> = None;
    for (param_count, has_rest) in clauses {
        if *param_count < min_args {
            min_args = *param_count;
        }
        if !*has_rest {
            max_args = Some(max_args.map_or(*param_count, |current| current.max(*param_count)));
        } else {
            max_args = None;
            break;
        }
    }
    if min_args == usize::MAX {
        min_args = 0;
    }
    (min_args, max_args)
}

fn multi_arity_error(protos: &[Arc<FunctionPrototype>], provided: usize) -> CloveError {
    let clauses: Vec<(usize, bool)> = protos
        .iter()
        .map(|proto| (proto.param_count, proto.has_rest))
        .collect();
    let (min_args, max_args) = multi_arity_bounds(&clauses);
    match max_args {
        Some(max) => CloveError::arity(format!(
            "expected between {} and {} args, got {}",
            min_args, max, provided
        )),
        None => CloveError::arity(format!(
            "expected at least {} args, got {}",
            min_args, provided
        )),
    }
}

fn is_builtin_value(value: &Value, builtin_id: BuiltinId) -> bool {
    match value {
        Value::Func(func) => func.debug_name() == Some(builtin_id.name()),
        _ => false,
    }
}

fn builtin_id_for_value(value: &Value) -> Option<BuiltinId> {
    match value {
        Value::Func(func) => func.debug_name().and_then(builtin_id_for_name),
        _ => None,
    }
}

fn builtin_id_for_name(name: &str) -> Option<BuiltinId> {
    match name {
        "+" => Some(BuiltinId::Add),
        "-" => Some(BuiltinId::Sub),
        "*" => Some(BuiltinId::Mul),
        "/" => Some(BuiltinId::Div),
        "=" => Some(BuiltinId::Eq),
        "<" => Some(BuiltinId::Lt),
        "<=" => Some(BuiltinId::Le),
        ">" => Some(BuiltinId::Gt),
        ">=" => Some(BuiltinId::Ge),
        "inc" => Some(BuiltinId::Inc),
        "dec" => Some(BuiltinId::Dec),
        _ => None,
    }
}

fn number_value(value: &Value) -> Option<(f64, bool)> {
    match value {
        Value::Int(n) => Some((*n as f64, false)),
        Value::Float(n) => Some((*n, true)),
        _ => None,
    }
}

fn make_number(value: f64, from_float: bool) -> Value {
    if !from_float && value.fract() == 0.0 {
        Value::Int(value as i64)
    } else {
        Value::Float(value)
    }
}

fn fast_add(args: &[Value]) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    let (a, a_float) = number_value(&args[0])?;
    let (b, b_float) = number_value(&args[1])?;
    let value = a + b;
    let is_float = a_float || b_float || value.fract() != 0.0;
    Some(make_number(value, is_float))
}

fn fast_sub(args: &[Value]) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    let (a, a_float) = number_value(&args[0])?;
    let (b, b_float) = number_value(&args[1])?;
    let value = a - b;
    let is_float = a_float || b_float || value.fract() != 0.0;
    Some(make_number(value, is_float))
}

fn fast_mul(args: &[Value]) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    let (a, a_float) = number_value(&args[0])?;
    let (b, b_float) = number_value(&args[1])?;
    let value = a * b;
    let is_float = a_float || b_float || value.fract() != 0.0;
    Some(make_number(value, is_float))
}

fn fast_div(args: &[Value]) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    let (a, a_float) = number_value(&args[0])?;
    let (b, b_float) = number_value(&args[1])?;
    if b == 0.0 {
        return None;
    }
    let value = a / b;
    let is_float = a_float || b_float || value.fract() != 0.0;
    Some(make_number(value, is_float))
}

fn fast_eq(args: &[Value]) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Some(Value::Bool(a == b)),
        (Value::Float(a), Value::Float(b)) => Some(Value::Bool(a == b)),
        _ => None,
    }
}

fn fast_cmp(args: &[Value], cmp: impl Fn(f64, f64) -> bool) -> Option<Value> {
    if args.len() != 2 {
        return None;
    }
    let (a, _) = number_value(&args[0])?;
    let (b, _) = number_value(&args[1])?;
    Some(Value::Bool(cmp(a, b)))
}

fn fast_inc(args: &[Value]) -> Option<Value> {
    if args.len() != 1 {
        return None;
    }
    let (value, is_float) = number_value(&args[0])?;
    let next = value + 1.0;
    let needs_float = is_float || next.fract() != 0.0;
    Some(make_number(next, needs_float))
}

fn fast_dec(args: &[Value]) -> Option<Value> {
    if args.len() != 1 {
        return None;
    }
    let (value, is_float) = number_value(&args[0])?;
    let next = value - 1.0;
    let needs_float = is_float || next.fract() != 0.0;
    Some(make_number(next, needs_float))
}

fn pack_vm_args(
    args: &[Value],
    param_count: usize,
    has_rest: bool,
) -> Result<Vec<Value>, CloveError> {
    if has_rest {
        if args.len() < param_count {
            return Err(CloveError::arity(format!(
                "expected at least {} args, got {}",
                param_count,
                args.len()
            )));
        }
        let mut locals = Vec::with_capacity(param_count + 1);
        locals.extend(args.iter().take(param_count).cloned());
        let rest_values: Vector<_> = args.iter().skip(param_count).cloned().collect();
        locals.push(Value::List(rest_values));
        Ok(locals)
    } else {
        if args.len() != param_count {
            return Err(CloveError::arity(format!(
                "expected {} args, got {}",
                param_count,
                args.len()
            )));
        }
        Ok(args.to_vec())
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Nil | Value::Bool(false))
}
