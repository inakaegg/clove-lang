use std::collections::BTreeMap;
use std::collections::HashMap;
use std::time::Instant;

use crate::ast::Literal;
use crate::builtins;
use crate::error::Clove2Error;
use crate::eval::{apply_builtin_value, value_to_key};
use crate::syntax::{AstExpr, Binding, Param, TopLevel};
use crate::types::Type;
use crate::use_directive::MutMode;
use crate::value::{Key, Value};
use regex::Regex;

pub mod profiler;

#[derive(Debug, Clone)]
enum Instruction {
    PushInt(i64),
    PushBool(bool),
    PushStr(String),
    PushKeyword(String),
    PushFloat(f64),
    PushRegex(String),
    PushNil,
    PushFunction(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    LoadGlobalSlot(usize),
    StoreGlobalSlot(usize),
    LoadGlobalDynamic(String),
    Map,
    Reduce(usize),
    MakeVec(usize),
    MakeSet(usize),
    MakeMap(usize),
    Pop,
    AddInt,
    AddIntN(usize),
    MinNumberN(usize),
    MaxNumberN(usize),
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    EqInt,
    LtInt,
    GtInt,
    LeInt,
    GeInt,
    Contains,
    Not,
    BoolToInt(i64, i64),
    JumpIfFalse(usize),
    Jump(usize),
    Conj,
    Nth,
    Interleave,
    Split,
    Join,
    CallBuiltin1(String),
    CallBuiltin2(String),
    Call(usize),
}

#[derive(Debug, Clone)]
struct VmFunction {
    params: Vec<String>,
    local_count: usize,
    code: Vec<Instruction>,
}

#[derive(Debug, Clone)]
struct VmProgram {
    code: Vec<Instruction>,
    functions: Vec<VmFunction>,
    local_count: usize,
    global_names: Vec<String>,
}

struct VmState {
    globals: Vec<Value>,
    global_map: HashMap<String, usize>,
    global_versions: Vec<u64>,
    global_cache_versions: Vec<u64>,
    global_cache_values: Vec<VmValue>,
    functions: Vec<VmFunction>,
    mut_mode: MutMode,
}

#[derive(Clone)]
enum VmValue {
    Value(Value),
    Int(i64),
    Bool(bool),
    Iter(VmIter),
}

#[derive(Clone)]
enum IterBase {
    Vec(std::rc::Rc<Vec<Value>>),
    Range { start: i64, end: i64, len: usize },
}

#[derive(Clone)]
enum IterOp {
    Map(Value),
    Filter(Value),
    FilterNot(Value),
    Keep(Value),
    KeepIndexed(Value),
    DropWhile(Value),
}

#[derive(Clone)]
struct VmIter {
    base: IterBase,
    ops: Vec<IterOp>,
}

struct NativeCacheEntry {
    func: VmFunction,
    frame: VmFrame,
}

fn call_value_arity1_cached(
    state: &mut VmState,
    func: &Value,
    arg: Value,
    cache: &mut [Option<NativeCacheEntry>],
    cache_idx: usize,
) -> Result<Value, Clove2Error> {
    if let Value::NativeFunction(id) = func {
        if let Some(entry) = cache.get_mut(cache_idx).and_then(|slot| slot.as_mut()) {
            if entry.func.params.len() == 1 {
                entry.frame.stack.clear();
                entry.frame.set_local_slot(0, value_to_vmvalue(arg))?;
                return run_code(&entry.func.code, state, &mut entry.frame);
            }
        }
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                frame.set_local_slot(0, value_to_vmvalue(arg))?;
                let value = run_code(&func_def.code, state, &mut frame)?;
                cache[cache_idx] = Some(NativeCacheEntry {
                    func: func_def,
                    frame,
                });
                return Ok(value);
            }
        }
    }
    call_value_arity1(state, func, arg)
}

fn call_value_arity2_cached(
    state: &mut VmState,
    func: &Value,
    arg1: Value,
    arg2: Value,
    cache: &mut [Option<NativeCacheEntry>],
    cache_idx: usize,
) -> Result<Value, Clove2Error> {
    if let Value::NativeFunction(id) = func {
        if let Some(entry) = cache.get_mut(cache_idx).and_then(|slot| slot.as_mut()) {
            if entry.func.params.len() == 2 {
                entry.frame.stack.clear();
                entry.frame.set_local_slot(0, value_to_vmvalue(arg1))?;
                entry.frame.set_local_slot(1, value_to_vmvalue(arg2))?;
                return run_code(&entry.func.code, state, &mut entry.frame);
            }
        }
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 2 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                frame.set_local_slot(0, value_to_vmvalue(arg1))?;
                frame.set_local_slot(1, value_to_vmvalue(arg2))?;
                let value = run_code(&func_def.code, state, &mut frame)?;
                cache[cache_idx] = Some(NativeCacheEntry {
                    func: func_def,
                    frame,
                });
                return Ok(value);
            }
        }
    }
    call_value_arity2(state, func, arg1, arg2)
}

fn call_vm_arity1_cached(
    state: &mut VmState,
    func: &Value,
    arg: VmValue,
    cache: &mut [Option<NativeCacheEntry>],
    cache_idx: usize,
) -> Result<VmValue, Clove2Error> {
    if let Value::NativeFunction(id) = func {
        if let Some(entry) = cache.get_mut(cache_idx).and_then(|slot| slot.as_mut()) {
            if entry.func.params.len() == 1 {
                entry.frame.stack.clear();
                entry.frame.set_local_slot(0, arg)?;
                let value = run_code(&entry.func.code, state, &mut entry.frame)?;
                return Ok(value_to_vmvalue(value));
            }
        }
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                frame.set_local_slot(0, arg)?;
                let value = run_code(&func_def.code, state, &mut frame)?;
                cache[cache_idx] = Some(NativeCacheEntry {
                    func: func_def,
                    frame,
                });
                return Ok(value_to_vmvalue(value));
            }
        }
    }
    if let Value::Builtin(name) = func {
        if let Some(result) = call_builtin_1_vm(state, name, &arg) {
            return result;
        }
    }
    if let Value::Builtin(name) = func {
        if let Some(result) = call_builtin_1_vm(state, name, &arg) {
            return result;
        }
    }
    let value = vmvalue_into_value(state, arg)?;
    match func {
        Value::Builtin(name) => Ok(value_to_vmvalue(call_builtin_1(state, name, value)?)),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_vm_arity2_cached(
    state: &mut VmState,
    func: &Value,
    arg1: VmValue,
    arg2: VmValue,
    cache: &mut [Option<NativeCacheEntry>],
    cache_idx: usize,
) -> Result<VmValue, Clove2Error> {
    if let Value::NativeFunction(id) = func {
        if let Some(entry) = cache.get_mut(cache_idx).and_then(|slot| slot.as_mut()) {
            if entry.func.params.len() == 2 {
                entry.frame.stack.clear();
                entry.frame.set_local_slot(0, arg1)?;
                entry.frame.set_local_slot(1, arg2)?;
                let value = run_code(&entry.func.code, state, &mut entry.frame)?;
                return Ok(value_to_vmvalue(value));
            }
        }
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 2 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                frame.set_local_slot(0, arg1)?;
                frame.set_local_slot(1, arg2)?;
                let value = run_code(&func_def.code, state, &mut frame)?;
                cache[cache_idx] = Some(NativeCacheEntry {
                    func: func_def,
                    frame,
                });
                return Ok(value_to_vmvalue(value));
            }
        }
    }
    if let Value::Builtin(name) = func {
        if let Some(result) = call_builtin_2_vm(state, name, &arg1, &arg2) {
            return result;
        }
    }
    if let Value::Builtin(name) = func {
        if let Some(result) = call_builtin_2_vm(state, name, &arg1, &arg2) {
            return result;
        }
    }
    let value1 = vmvalue_into_value(state, arg1)?;
    let value2 = vmvalue_into_value(state, arg2)?;
    match func {
        Value::Builtin(name) => Ok(value_to_vmvalue(call_builtin_2(
            state, name, value1, value2,
        )?)),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

impl VmIter {
    fn from_value(value: Value) -> Result<Self, Clove2Error> {
        match value {
            Value::Vec(items) => Ok(Self {
                base: IterBase::Vec(items),
                ops: Vec::new(),
            }),
            _ => Err(Clove2Error::new("vm: expected iterable value")),
        }
    }

    fn map(mut self, func: Value) -> Self {
        self.ops.push(IterOp::Map(func));
        self
    }

    fn filter(mut self, func: Value) -> Self {
        self.ops.push(IterOp::Filter(func));
        self
    }

    fn remove(mut self, func: Value) -> Self {
        self.ops.push(IterOp::FilterNot(func));
        self
    }

    fn keep(mut self, func: Value) -> Self {
        self.ops.push(IterOp::Keep(func));
        self
    }

    fn keep_indexed(mut self, func: Value) -> Self {
        self.ops.push(IterOp::KeepIndexed(func));
        self
    }

    fn drop_while(mut self, func: Value) -> Self {
        self.ops.push(IterOp::DropWhile(func));
        self
    }

    fn for_each<F>(&self, state: &mut VmState, mut f: F) -> Result<(), Clove2Error>
    where
        F: FnMut(&mut VmState, Value) -> Result<(), Clove2Error>,
    {
        if self.ops.len() == 1 {
            match &self.ops[0] {
                IterOp::Map(func) => {
                    let mut native_cache: Vec<Option<NativeCacheEntry>> = Vec::with_capacity(1);
                    native_cache.push(None);
                    match &self.base {
                        IterBase::Vec(items) => {
                            for item in items.iter() {
                                let vm_value = call_vm_arity1_cached(
                                    state,
                                    func,
                                    value_to_vmvalue(item.clone()),
                                    &mut native_cache,
                                    0,
                                )?;
                                let value = vmvalue_into_value(state, vm_value)?;
                                f(state, value)?;
                            }
                        }
                        IterBase::Range { start, end, .. } => {
                            let mut cur = *start;
                            while cur < *end {
                                let vm_value = call_vm_arity1_cached(
                                    state,
                                    func,
                                    VmValue::Int(cur),
                                    &mut native_cache,
                                    0,
                                )?;
                                let value = vmvalue_into_value(state, vm_value)?;
                                f(state, value)?;
                                cur += 1;
                            }
                        }
                    }
                    return Ok(());
                }
                _ => {}
            }
        }
        let mut native_cache: Vec<Option<NativeCacheEntry>> =
            (0..self.ops.len()).map(|_| None).collect();
        let mut drop_states = vec![true; self.ops.len()];
        match &self.base {
            IterBase::Vec(items) => {
                for (idx, item) in items.iter().enumerate() {
                    if let Some(vm_value) = self.apply_ops(
                        state,
                        value_to_vmvalue(item.clone()),
                        idx,
                        &mut drop_states,
                        &mut native_cache,
                    )? {
                        let value = vmvalue_into_value(state, vm_value)?;
                        f(state, value)?;
                    }
                }
            }
            IterBase::Range { start, end, .. } => {
                let mut cur = *start;
                let mut idx = 0usize;
                while cur < *end {
                    if let Some(vm_value) = self.apply_ops(
                        state,
                        VmValue::Int(cur),
                        idx,
                        &mut drop_states,
                        &mut native_cache,
                    )? {
                        let value = vmvalue_into_value(state, vm_value)?;
                        f(state, value)?;
                    }
                    cur += 1;
                    idx += 1;
                }
            }
        }
        Ok(())
    }

    fn for_each_vmvalue<F>(&self, state: &mut VmState, mut f: F) -> Result<(), Clove2Error>
    where
        F: FnMut(&mut VmState, VmValue) -> Result<(), Clove2Error>,
    {
        if self.ops.len() == 1 {
            match &self.ops[0] {
                IterOp::Map(func) => {
                    let mut native_cache: Vec<Option<NativeCacheEntry>> = Vec::with_capacity(1);
                    native_cache.push(None);
                    match &self.base {
                        IterBase::Vec(items) => {
                            for item in items.iter() {
                                let vm_value = call_vm_arity1_cached(
                                    state,
                                    func,
                                    value_to_vmvalue(item.clone()),
                                    &mut native_cache,
                                    0,
                                )?;
                                f(state, vm_value)?;
                            }
                        }
                        IterBase::Range { start, end, .. } => {
                            let mut cur = *start;
                            while cur < *end {
                                let vm_value = call_vm_arity1_cached(
                                    state,
                                    func,
                                    VmValue::Int(cur),
                                    &mut native_cache,
                                    0,
                                )?;
                                f(state, vm_value)?;
                                cur += 1;
                            }
                        }
                    }
                    return Ok(());
                }
                _ => {}
            }
        }
        let mut native_cache: Vec<Option<NativeCacheEntry>> =
            (0..self.ops.len()).map(|_| None).collect();
        let mut drop_states = vec![true; self.ops.len()];
        match &self.base {
            IterBase::Vec(items) => {
                for (idx, item) in items.iter().enumerate() {
                    if let Some(vm_value) = self.apply_ops(
                        state,
                        value_to_vmvalue(item.clone()),
                        idx,
                        &mut drop_states,
                        &mut native_cache,
                    )? {
                        f(state, vm_value)?;
                    }
                }
            }
            IterBase::Range { start, end, .. } => {
                let mut cur = *start;
                let mut idx = 0usize;
                while cur < *end {
                    if let Some(vm_value) = self.apply_ops(
                        state,
                        VmValue::Int(cur),
                        idx,
                        &mut drop_states,
                        &mut native_cache,
                    )? {
                        f(state, vm_value)?;
                    }
                    cur += 1;
                    idx += 1;
                }
            }
        }
        Ok(())
    }

    fn collect(&self, state: &mut VmState) -> Result<Vec<Value>, Clove2Error> {
        let mut out = Vec::new();
        self.for_each(state, |_, value| {
            out.push(value);
            Ok(())
        })?;
        Ok(out)
    }

    fn apply_ops(
        &self,
        state: &mut VmState,
        mut value: VmValue,
        index: usize,
        drop_states: &mut [bool],
        native_cache: &mut [Option<NativeCacheEntry>],
    ) -> Result<Option<VmValue>, Clove2Error> {
        for (idx, op) in self.ops.iter().enumerate() {
            match op {
                IterOp::Map(func) => {
                    value = call_vm_arity1_cached(state, func, value, native_cache, idx)?;
                }
                IterOp::Filter(func) => {
                    let keep =
                        call_vm_arity1_cached(state, func, value.clone(), native_cache, idx)?;
                    if !is_truthy_vm(state, &keep)? {
                        return Ok(None);
                    }
                }
                IterOp::FilterNot(func) => {
                    let keep =
                        call_vm_arity1_cached(state, func, value.clone(), native_cache, idx)?;
                    if is_truthy_vm(state, &keep)? {
                        return Ok(None);
                    }
                }
                IterOp::Keep(func) => {
                    let keep_val = call_vm_arity1_cached(state, func, value, native_cache, idx)?;
                    if is_nil_vm(&keep_val) {
                        return Ok(None);
                    }
                    value = keep_val;
                }
                IterOp::KeepIndexed(func) => {
                    let keep_val = call_vm_arity2_cached(
                        state,
                        func,
                        VmValue::Int(index as i64),
                        value,
                        native_cache,
                        idx,
                    )?;
                    if is_nil_vm(&keep_val) {
                        return Ok(None);
                    }
                    value = keep_val;
                }
                IterOp::DropWhile(func) => {
                    if drop_states[idx] {
                        let drop =
                            call_vm_arity1_cached(state, func, value.clone(), native_cache, idx)?;
                        if is_truthy_vm(state, &drop)? {
                            return Ok(None);
                        }
                        drop_states[idx] = false;
                    }
                }
            }
        }
        Ok(Some(value))
    }

    fn reduce_int_fast(&self, init: Option<i64>) -> Result<Option<i64>, Clove2Error> {
        if self.ops.iter().any(|op| matches!(op, IterOp::Keep(_) | IterOp::KeepIndexed(_))) {
            return Ok(None);
        }
        #[derive(Copy, Clone)]
        enum IntMap {
            Identity,
            Inc,
            Dec,
        }
        #[derive(Copy, Clone)]
        enum IntFilter {
            Even,
            Odd,
        }
        enum FastKind {
            None,
            Map(IntMap),
            Filter { filter: IntFilter, invert: bool },
            MapFilter {
                map: IntMap,
                filter: IntFilter,
                invert: bool,
            },
            FilterMap {
                filter: IntFilter,
                invert: bool,
                map: IntMap,
            },
        }
        fn map_kind(func: &Value) -> Option<IntMap> {
            match func {
                Value::Builtin(name) => match name.as_str() {
                    "identity" => Some(IntMap::Identity),
                    "inc" => Some(IntMap::Inc),
                    "dec" => Some(IntMap::Dec),
                    _ => None,
                },
                _ => None,
            }
        }
        fn filter_kind(func: &Value) -> Option<IntFilter> {
            match func {
                Value::Builtin(name) => match name.as_str() {
                    "even?" => Some(IntFilter::Even),
                    "odd?" => Some(IntFilter::Odd),
                    _ => None,
                },
                _ => None,
            }
        }
        let fast_kind = match self.ops.as_slice() {
            [] => FastKind::None,
            [IterOp::Map(func)] => {
                let map = match map_kind(func) {
                    Some(map) => map,
                    None => return Ok(None),
                };
                FastKind::Map(map)
            }
            [IterOp::Filter(func)] => {
                let filter = match filter_kind(func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                FastKind::Filter {
                    filter,
                    invert: false,
                }
            }
            [IterOp::FilterNot(func)] => {
                let filter = match filter_kind(func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                FastKind::Filter { filter, invert: true }
            }
            [IterOp::Map(map_func), IterOp::Filter(filter_func)] => {
                let map = match map_kind(map_func) {
                    Some(map) => map,
                    None => return Ok(None),
                };
                let filter = match filter_kind(filter_func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                FastKind::MapFilter {
                    map,
                    filter,
                    invert: false,
                }
            }
            [IterOp::Map(map_func), IterOp::FilterNot(filter_func)] => {
                let map = match map_kind(map_func) {
                    Some(map) => map,
                    None => return Ok(None),
                };
                let filter = match filter_kind(filter_func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                FastKind::MapFilter { map, filter, invert: true }
            }
            [IterOp::Filter(filter_func), IterOp::Map(map_func)] => {
                let filter = match filter_kind(filter_func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                let map = match map_kind(map_func) {
                    Some(map) => map,
                    None => return Ok(None),
                };
                FastKind::FilterMap { filter, invert: false, map }
            }
            [IterOp::FilterNot(filter_func), IterOp::Map(map_func)] => {
                let filter = match filter_kind(filter_func) {
                    Some(filter) => filter,
                    None => return Ok(None),
                };
                let map = match map_kind(map_func) {
                    Some(map) => map,
                    None => return Ok(None),
                };
                FastKind::FilterMap { filter, invert: true, map }
            }
            _ => return Ok(None),
        };
        let mut acc = init;
        let mut processed_any = false;
        let apply_map = |map: IntMap, value: i64| match map {
            IntMap::Identity => value,
            IntMap::Inc => value + 1,
            IntMap::Dec => value - 1,
        };
        let apply_filter = |filter: IntFilter, value: i64| match filter {
            IntFilter::Even => value % 2 == 0,
            IntFilter::Odd => value % 2 != 0,
        };
        match &self.base {
            IterBase::Range { start, end, .. } => {
                let mut cur = *start;
                while cur < *end {
                    let mut value = cur;
                    let mut keep = true;
                    match fast_kind {
                        FastKind::None => {}
                        FastKind::Map(map) => value = apply_map(map, value),
                        FastKind::Filter { filter, invert } => {
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                        }
                        FastKind::MapFilter {
                            map,
                            filter,
                            invert,
                        } => {
                            value = apply_map(map, value);
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                        }
                        FastKind::FilterMap {
                            filter,
                            invert,
                            map,
                        } => {
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                            if keep {
                                value = apply_map(map, value);
                            }
                        }
                    }
                    if keep {
                        processed_any = true;
                        if let Some(acc_value) = acc.as_mut() {
                            *acc_value += value;
                        } else {
                            acc = Some(value);
                        }
                    }
                    cur += 1;
                }
            }
            IterBase::Vec(items) => {
                for item in items.iter() {
                    let Value::Int(value) = item else {
                        return Ok(None);
                    };
                    let mut value = *value;
                    let mut keep = true;
                    match fast_kind {
                        FastKind::None => {}
                        FastKind::Map(map) => value = apply_map(map, value),
                        FastKind::Filter { filter, invert } => {
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                        }
                        FastKind::MapFilter {
                            map,
                            filter,
                            invert,
                        } => {
                            value = apply_map(map, value);
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                        }
                        FastKind::FilterMap {
                            filter,
                            invert,
                            map,
                        } => {
                            keep = apply_filter(filter, value);
                            if invert {
                                keep = !keep;
                            }
                            if keep {
                                value = apply_map(map, value);
                            }
                        }
                    }
                    if keep {
                        processed_any = true;
                        if let Some(acc_value) = acc.as_mut() {
                            *acc_value += value;
                        } else {
                            acc = Some(value);
                        }
                    }
                }
            }
        }
        if acc.is_some() {
            return Ok(acc);
        }
        if init.is_some() {
            return Ok(init);
        }
        if !processed_any {
            return Err(Clove2Error::new("vm: reduce on empty collection"));
        }
        Ok(acc)
    }
}

#[derive(Clone)]
struct CompileEnv {
    scopes: Vec<HashMap<String, usize>>,
    next_local: usize,
    globals: HashMap<String, usize>,
    global_names: Vec<String>,
}

impl CompileEnv {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            next_local: 0,
            globals: HashMap::new(),
            global_names: Vec::new(),
        }
    }

    fn with_globals_from(other: &CompileEnv) -> Self {
        Self {
            scopes: Vec::new(),
            next_local: 0,
            globals: other.globals.clone(),
            global_names: other.global_names.clone(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str) -> usize {
        let slot = self.next_local;
        self.next_local += 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), slot);
        }
        slot
    }

    fn resolve(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(slot) = scope.get(name) {
                return Some(*slot);
            }
        }
        None
    }

    fn define_global(&mut self, name: &str) -> usize {
        if let Some(slot) = self.globals.get(name) {
            return *slot;
        }
        let slot = self.global_names.len();
        self.globals.insert(name.to_string(), slot);
        self.global_names.push(name.to_string());
        slot
    }

    fn resolve_global(&self, name: &str) -> Option<usize> {
        self.globals.get(name).copied()
    }
}

impl VmState {
    fn new(functions: Vec<VmFunction>, global_names: Vec<String>, mut_mode: MutMode) -> Self {
        let mut globals = vec![Value::Nil; global_names.len()];
        let mut global_map = HashMap::new();
        let global_versions = vec![0u64; global_names.len()];
        let mut global_cache_versions = vec![u64::MAX; global_names.len()];
        let mut global_cache_values = vec![VmValue::Value(Value::Nil); global_names.len()];
        for (idx, name) in global_names.iter().enumerate() {
            global_map.insert(name.clone(), idx);
            if builtins::BUILTIN_NAMES.contains(&name.as_str()) {
                globals[idx] = Value::Builtin(name.clone());
                global_cache_values[idx] = VmValue::Value(Value::Builtin(name.clone()));
                global_cache_versions[idx] = 0;
            }
        }
        Self {
            globals,
            global_map,
            global_versions,
            global_cache_versions,
            global_cache_values,
            functions,
            mut_mode,
        }
    }
}

fn compile_program(items: &[TopLevel]) -> Result<VmProgram, Clove2Error> {
    let mut code = Vec::new();
    let mut functions = Vec::new();
    let mut env = CompileEnv::new();
    for name in builtins::BUILTIN_NAMES {
        env.define_global(name);
    }
    let mut first = true;
    for item in items {
        if !first {
            code.push(Instruction::Pop);
        }
        first = false;
        match item {
            TopLevel::Expr { expr, .. } => {
                compile_expr(&mut code, &mut functions, &mut env, expr)?;
            }
            TopLevel::Def { name, value, .. } => {
                let slot = env.define_global(name);
                compile_expr(&mut code, &mut functions, &mut env, value)?;
                code.push(Instruction::StoreGlobalSlot(slot));
                code.push(Instruction::PushNil);
            }
            TopLevel::Defn {
                name,
                params,
                ret,
                body,
                ..
            } => {
                let slot = env.define_global(name);
                let func_id = compile_fn(&mut functions, params, ret, body, &env)?;
                code.push(Instruction::PushFunction(func_id));
                code.push(Instruction::StoreGlobalSlot(slot));
                code.push(Instruction::PushNil);
            }
            _ => return Err(Clove2Error::new("vm: top-level form not supported yet")),
        }
    }
    Ok(VmProgram {
        code,
        functions,
        local_count: env.next_local,
        global_names: env.global_names,
    })
}

fn compile_fn(
    functions: &mut Vec<VmFunction>,
    params: &[Param],
    ret: &Option<Type>,
    body: &[AstExpr],
    globals: &CompileEnv,
) -> Result<usize, Clove2Error> {
    if ret.is_some() {
        return Err(Clove2Error::new("vm: typed fn not supported yet"));
    }
    let mut code = Vec::new();
    let mut env = CompileEnv::with_globals_from(globals);
    env.push_scope();
    for param in params {
        env.define(&param.name);
    }
    for (idx, expr) in body.iter().enumerate() {
        compile_expr(&mut code, functions, &mut env, expr)?;
        if idx + 1 != body.len() {
            code.push(Instruction::Pop);
        }
    }
    let params = params.iter().map(|param| param.name.clone()).collect();
    let func = VmFunction {
        params,
        local_count: env.next_local,
        code,
    };
    functions.push(func);
    Ok(functions.len() - 1)
}

fn compile_expr(
    out: &mut Vec<Instruction>,
    functions: &mut Vec<VmFunction>,
    env: &mut CompileEnv,
    expr: &AstExpr,
) -> Result<(), Clove2Error> {
    match expr {
        AstExpr::Literal(lit) => match lit {
            Literal::Int(value) => {
                out.push(Instruction::PushInt(*value));
                Ok(())
            }
            Literal::Bool(value) => {
                out.push(Instruction::PushBool(*value));
                Ok(())
            }
            Literal::Str(value) => {
                out.push(Instruction::PushStr(value.clone()));
                Ok(())
            }
            Literal::Float(value) => {
                out.push(Instruction::PushFloat(*value));
                Ok(())
            }
            Literal::Regex(value) => {
                out.push(Instruction::PushRegex(value.clone()));
                Ok(())
            }
            Literal::Nil => {
                out.push(Instruction::PushNil);
                Ok(())
            }
        },
        AstExpr::Keyword(name) => {
            out.push(Instruction::PushKeyword(name.clone()));
            Ok(())
        }
        AstExpr::Vector(items) => {
            for item in items {
                compile_expr(out, functions, env, item)?;
            }
            out.push(Instruction::MakeVec(items.len()));
            Ok(())
        }
        AstExpr::Set(items) => {
            for item in items {
                compile_expr(out, functions, env, item)?;
            }
            out.push(Instruction::MakeSet(items.len()));
            Ok(())
        }
        AstExpr::Map(entries) => {
            for (key, value) in entries {
                compile_expr(out, functions, env, key)?;
                compile_expr(out, functions, env, value)?;
            }
            out.push(Instruction::MakeMap(entries.len()));
            Ok(())
        }
        AstExpr::Symbol(name) => {
            if let Some(slot) = env.resolve(name) {
                out.push(Instruction::LoadLocal(slot));
            } else if let Some(slot) = env.resolve_global(name) {
                out.push(Instruction::LoadGlobalSlot(slot));
            } else {
                out.push(Instruction::LoadGlobalDynamic(name.clone()));
            }
            Ok(())
        }
        AstExpr::Fn { params, ret, body } => {
            let func_id = compile_fn(functions, params, ret, body, env)?;
            out.push(Instruction::PushFunction(func_id));
            Ok(())
        }
        AstExpr::Let { bindings, body } => {
            env.push_scope();
            compile_bindings(out, functions, env, bindings)?;
            for (idx, expr) in body.iter().enumerate() {
                compile_expr(out, functions, env, expr)?;
                if idx + 1 != body.len() {
                    out.push(Instruction::Pop);
                }
            }
            env.pop_scope();
            Ok(())
        }
        AstExpr::If {
            cond,
            then_expr,
            else_expr,
        } => {
            compile_expr(out, functions, env, cond)?;
            if let AstExpr::Literal(Literal::Int(then_val)) = then_expr.as_ref() {
                if let Some(else_expr) = else_expr.as_ref() {
                    if let AstExpr::Literal(Literal::Int(else_val)) = else_expr.as_ref() {
                        out.push(Instruction::BoolToInt(*then_val, *else_val));
                        return Ok(());
                    }
                }
            }
            let jf_idx = out.len();
            out.push(Instruction::JumpIfFalse(0));
            compile_expr(out, functions, env, then_expr)?;
            let j_idx = out.len();
            out.push(Instruction::Jump(0));
            let else_start = out.len();
            if let Some(expr) = else_expr.as_ref() {
                compile_expr(out, functions, env, expr)?;
            } else {
                out.push(Instruction::PushNil);
            }
            let end = out.len();
            out[jf_idx] = Instruction::JumpIfFalse(else_start);
            out[j_idx] = Instruction::Jump(end);
            Ok(())
        }
        AstExpr::Call { callee, args } => {
            if let AstExpr::Symbol(sym) = callee.as_ref() {
                let canonical = crate::aliases::resolve_alias(sym);
                match canonical {
                    "when" => {
                        if args.is_empty() {
                            out.push(Instruction::PushNil);
                            return Ok(());
                        }
                        let cond = &args[0];
                        compile_expr(out, functions, env, cond)?;
                        let jf_idx = out.len();
                        out.push(Instruction::JumpIfFalse(0));
                        if args.len() == 1 {
                            out.push(Instruction::PushNil);
                        } else {
                            for (idx, expr) in args.iter().skip(1).enumerate() {
                                compile_expr(out, functions, env, expr)?;
                                if idx + 2 != args.len() {
                                    out.push(Instruction::Pop);
                                }
                            }
                        }
                        let end = out.len();
                        out[jf_idx] = Instruction::JumpIfFalse(end);
                        return Ok(());
                    }
                    "apply" => {
                        if args.len() < 2 {
                            return Err(Clove2Error::new("vm: apply expects 2+ args"));
                        }
                        if let AstExpr::Symbol(apply_callee) = &args[0] {
                            let apply_callee = crate::aliases::resolve_alias(apply_callee);
                            if apply_callee == "+" {
                                if let Some(AstExpr::Vector(items)) = args.last() {
                                    for arg in args.iter().skip(1).take(args.len() - 2) {
                                        compile_expr(out, functions, env, arg)?;
                                    }
                                    for item in items {
                                        compile_expr(out, functions, env, item)?;
                                    }
                                    let count = (args.len() - 2) + items.len();
                                    if count == 0 {
                                        out.push(Instruction::PushInt(0));
                                        return Ok(());
                                    }
                                    out.push(Instruction::AddIntN(count));
                                    return Ok(());
                                }
                            }
                        }
                    }
                    "+" | "-" | "*" | "/" | "mod" => {
                        if args.is_empty() {
                            return Err(Clove2Error::new("vm: call requires args"));
                        }
                        if canonical == "+" && args.len() > 1 {
                            for arg in args.iter() {
                                compile_expr(out, functions, env, arg)?;
                            }
                            out.push(Instruction::AddIntN(args.len()));
                            return Ok(());
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        for arg in args.iter().skip(1) {
                            compile_expr(out, functions, env, arg)?;
                            match canonical {
                                "+" => out.push(Instruction::AddInt),
                                "-" => out.push(Instruction::SubInt),
                                "*" => out.push(Instruction::MulInt),
                                "/" => out.push(Instruction::DivInt),
                                "mod" => out.push(Instruction::ModInt),
                                _ => {}
                            }
                        }
                        return Ok(());
                    }
                    "<" | ">" | "<=" | ">=" | "=" => {
                        if args.len() != 2 {
                            return Err(Clove2Error::new("vm: comparison expects 2 args"));
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        compile_expr(out, functions, env, &args[1])?;
                        match canonical {
                            "<" => out.push(Instruction::LtInt),
                            ">" => out.push(Instruction::GtInt),
                            "<=" => out.push(Instruction::LeInt),
                            ">=" => out.push(Instruction::GeInt),
                            "=" => out.push(Instruction::EqInt),
                            _ => {}
                        }
                        return Ok(());
                    }
                    "not" => {
                        if args.len() != 1 {
                            return Err(Clove2Error::new("vm: not expects 1 arg"));
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        out.push(Instruction::Not);
                        return Ok(());
                    }
                    "max" => {
                        if args.is_empty() {
                            return Err(Clove2Error::new("vm: max expects 1+ args"));
                        }
                        for arg in args.iter() {
                            compile_expr(out, functions, env, arg)?;
                        }
                        out.push(Instruction::MaxNumberN(args.len()));
                        return Ok(());
                    }
                    "min" => {
                        if args.is_empty() {
                            return Err(Clove2Error::new("vm: min expects 1+ args"));
                        }
                        for arg in args.iter() {
                            compile_expr(out, functions, env, arg)?;
                        }
                        out.push(Instruction::MinNumberN(args.len()));
                        return Ok(());
                    }
                    "contains?" => {
                        if args.len() != 2 {
                            return Err(Clove2Error::new("vm: contains? expects 2 args"));
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        compile_expr(out, functions, env, &args[1])?;
                        out.push(Instruction::Contains);
                        return Ok(());
                    }
                    "conj" => {
                        if args.len() == 2 {
                            compile_expr(out, functions, env, &args[0])?;
                            compile_expr(out, functions, env, &args[1])?;
                            out.push(Instruction::Conj);
                            return Ok(());
                        }
                    }
                    "nth" => {
                        if args.len() == 2 {
                            compile_expr(out, functions, env, &args[0])?;
                            compile_expr(out, functions, env, &args[1])?;
                            out.push(Instruction::Nth);
                            return Ok(());
                        }
                    }
                    "interleave" => {
                        if args.len() == 2 {
                            compile_expr(out, functions, env, &args[0])?;
                            compile_expr(out, functions, env, &args[1])?;
                            out.push(Instruction::Interleave);
                            return Ok(());
                        }
                    }
                    "split" => {
                        if args.len() == 2 {
                            compile_expr(out, functions, env, &args[0])?;
                            compile_expr(out, functions, env, &args[1])?;
                            out.push(Instruction::Split);
                            return Ok(());
                        }
                    }
                    "join" => {
                        if args.len() == 2 {
                            compile_expr(out, functions, env, &args[0])?;
                            compile_expr(out, functions, env, &args[1])?;
                            out.push(Instruction::Join);
                            return Ok(());
                        }
                    }
                    "hash-map" => {
                        if args.len() % 2 == 0 {
                            for arg in args.iter() {
                                compile_expr(out, functions, env, arg)?;
                            }
                            out.push(Instruction::MakeMap(args.len() / 2));
                            return Ok(());
                        }
                    }
                    "map" => {
                        if args.len() != 2 {
                            return Err(Clove2Error::new("vm: map expects 2 args"));
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        compile_expr(out, functions, env, &args[1])?;
                        out.push(Instruction::Map);
                        return Ok(());
                    }
                    "reduce" => {
                        if args.len() < 2 || args.len() > 3 {
                            return Err(Clove2Error::new("vm: reduce expects 2 or 3 args"));
                        }
                        compile_expr(out, functions, env, &args[0])?;
                        if args.len() == 3 {
                            compile_expr(out, functions, env, &args[1])?;
                            compile_expr(out, functions, env, &args[2])?;
                        } else {
                            compile_expr(out, functions, env, &args[1])?;
                        }
                        out.push(Instruction::Reduce(args.len()));
                        return Ok(());
                    }
                    _ => {
                        if builtins::BUILTIN_NAMES.contains(&canonical) {
                            match args.len() {
                                1 => {
                                    compile_expr(out, functions, env, &args[0])?;
                                    out.push(Instruction::CallBuiltin1(canonical.to_string()));
                                    return Ok(());
                                }
                                2 => {
                                    compile_expr(out, functions, env, &args[0])?;
                                    compile_expr(out, functions, env, &args[1])?;
                                    out.push(Instruction::CallBuiltin2(canonical.to_string()));
                                    return Ok(());
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            compile_expr(out, functions, env, callee)?;
            for arg in args {
                compile_expr(out, functions, env, arg)?;
            }
            out.push(Instruction::Call(args.len()));
            Ok(())
        }
        _ => Err(Clove2Error::new("vm: expr not supported yet")),
    }
}

fn compile_bindings(
    out: &mut Vec<Instruction>,
    functions: &mut Vec<VmFunction>,
    env: &mut CompileEnv,
    bindings: &[Binding],
) -> Result<(), Clove2Error> {
    for binding in bindings {
        compile_expr(out, functions, env, &binding.value)?;
        let slot = env.define(&binding.name);
        out.push(Instruction::StoreLocal(slot));
    }
    Ok(())
}

struct VmFrame {
    locals: Vec<VmValue>,
    stack: Vec<VmValue>,
}

impl VmFrame {
    fn new(local_count: usize) -> Self {
        Self {
            locals: vec![VmValue::Value(Value::Nil); local_count],
            stack: Vec::new(),
        }
    }

    fn set_local_slot(&mut self, slot: usize, value: VmValue) -> Result<(), Clove2Error> {
        if slot >= self.locals.len() {
            return Err(Clove2Error::new("vm: local slot out of range"));
        }
        self.locals[slot] = value;
        Ok(())
    }

    fn get_local_slot(&self, slot: usize) -> Result<VmValue, Clove2Error> {
        self.locals
            .get(slot)
            .cloned()
            .ok_or_else(|| Clove2Error::new("vm: local slot out of range"))
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Nil | Value::Bool(false))
}

fn is_nil_vm(value: &VmValue) -> bool {
    matches!(value, VmValue::Value(Value::Nil))
}

fn is_truthy_vm(state: &mut VmState, value: &VmValue) -> Result<bool, Clove2Error> {
    Ok(match value {
        VmValue::Bool(value) => *value,
        VmValue::Value(Value::Bool(value)) => *value,
        VmValue::Value(Value::Nil) => false,
        _ => is_truthy(&vmvalue_into_value(state, value.clone())?),
    })
}

fn prepare_reuse_frame(func: &VmFunction) -> Result<VmFrame, Clove2Error> {
    let frame = VmFrame::new(func.local_count);
    Ok(frame)
}

fn pop_vm_value(stack: &mut Vec<VmValue>) -> Result<VmValue, Clove2Error> {
    stack
        .pop()
        .ok_or_else(|| Clove2Error::new("vm: stack underflow"))
}

fn vmvalue_into_value(state: &mut VmState, value: VmValue) -> Result<Value, Clove2Error> {
    match value {
        VmValue::Value(value) => Ok(value),
        VmValue::Int(value) => Ok(Value::Int(value)),
        VmValue::Bool(value) => Ok(Value::Bool(value)),
        VmValue::Iter(iter) => Ok(Value::vec(iter.collect(state)?)),
    }
}

fn pop_value(state: &mut VmState, stack: &mut Vec<VmValue>) -> Result<Value, Clove2Error> {
    let value = pop_vm_value(stack)?;
    vmvalue_into_value(state, value)
}

fn pop_int(_state: &mut VmState, stack: &mut Vec<VmValue>) -> Result<i64, Clove2Error> {
    match pop_vm_value(stack)? {
        VmValue::Int(value) => Ok(value),
        VmValue::Value(Value::Int(value)) => Ok(value),
        other => Err(Clove2Error::new(format!(
            "vm: expected int, got {}",
            vmvalue_kind(&other)
        ))),
    }
}

fn pop_truthy(state: &mut VmState, stack: &mut Vec<VmValue>) -> Result<bool, Clove2Error> {
    match pop_vm_value(stack)? {
        VmValue::Bool(value) => Ok(value),
        other => Ok(is_truthy(&vmvalue_into_value(state, other)?)),
    }
}

fn value_to_vmvalue(value: Value) -> VmValue {
    match value {
        Value::Int(value) => VmValue::Int(value),
        Value::Bool(value) => VmValue::Bool(value),
        value => VmValue::Value(value),
    }
}

fn vmvalue_kind(value: &VmValue) -> &'static str {
    match value {
        VmValue::Value(_) => "value",
        VmValue::Int(_) => "int",
        VmValue::Bool(_) => "bool",
        VmValue::Iter(_) => "iter",
    }
}

fn vmvalue_into_iter(value: VmValue) -> Result<VmIter, Clove2Error> {
    match value {
        VmValue::Iter(iter) => Ok(iter),
        VmValue::Value(value) => VmIter::from_value(value),
        VmValue::Int(value) => VmIter::from_value(Value::Int(value)),
        VmValue::Bool(value) => VmIter::from_value(Value::Bool(value)),
    }
}

fn vm_args_to_values(state: &mut VmState, args: Vec<VmValue>) -> Result<Vec<Value>, Clove2Error> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        out.push(vmvalue_into_value(state, arg)?);
    }
    Ok(out)
}

fn vmvalue_into_int(name: &str, arg: VmValue) -> Result<i64, Clove2Error> {
    match arg {
        VmValue::Int(value) => Ok(value),
        VmValue::Value(Value::Int(value)) => Ok(value),
        _ => Err(Clove2Error::new(format!("vm: {name} expects int"))),
    }
}

fn call_builtin_vm(
    state: &mut VmState,
    name: &str,
    mut args: Vec<VmValue>,
) -> Result<VmValue, Clove2Error> {
    match name {
        "identity" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: identity expects 1 argument"));
            }
            Ok(args.pop().unwrap())
        }
        "count" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: count expects 1 argument"));
            }
            let arg = args.pop().unwrap();
            match arg {
                VmValue::Iter(iter) => {
                    let mut count = 0i64;
                    iter.for_each(state, |_, _| {
                        count += 1;
                        Ok(())
                    })?;
                    Ok(VmValue::Value(Value::Int(count)))
                }
                other => {
                    let value = vmvalue_into_value(state, other)?;
                    Ok(VmValue::Value(call_builtin(state, "count", vec![value])?))
                }
            }
        }
        "empty?" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: empty? expects 1 argument"));
            }
            let arg = args.pop().unwrap();
            match arg {
                VmValue::Iter(iter) => {
                    let mut any = false;
                    iter.for_each(state, |_, _| {
                        any = true;
                        Ok(())
                    })?;
                    Ok(VmValue::Value(Value::Bool(!any)))
                }
                other => {
                    let value = vmvalue_into_value(state, other)?;
                    Ok(VmValue::Value(call_builtin(state, "empty?", vec![value])?))
                }
            }
        }
        "range" => {
            let args = vm_args_to_values(state, args)?;
            let (start, end) = match args.as_slice() {
                [Value::Int(end)] => (0, *end),
                [Value::Int(start), Value::Int(end)] => (*start, *end),
                _ => return Err(Clove2Error::new("vm: range expects int arguments")),
            };
            Ok(VmValue::Iter(VmIter {
                base: IterBase::Range {
                    start,
                    end,
                    len: (end - start).max(0) as usize,
                },
                ops: Vec::new(),
            }))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(Clove2Error::new("vm: apply expects at least 2 arguments"));
            }
            fn add_int(value: i64, acc_int: &mut i64, acc_float: &mut f64, use_float: &mut bool) {
                if *use_float {
                    *acc_float += value as f64;
                } else {
                    *acc_int += value;
                }
            }
            fn add_float(value: f64, acc_int: &mut i64, acc_float: &mut f64, use_float: &mut bool) {
                if !*use_float {
                    *acc_float = *acc_int as f64;
                    *use_float = true;
                }
                *acc_float += value;
            }
            let try_apply_add = |state: &mut VmState, args: &[VmValue]| -> Option<VmValue> {
                let callee = args.first()?;
                let VmValue::Value(Value::Builtin(name)) = callee else {
                    return None;
                };
                if name != "+" {
                    return None;
                }
                let tail = args.last()?;
                let fixed = &args[1..args.len().saturating_sub(1)];
                let mut acc_int: i64 = 0;
                let mut acc_float: f64 = 0.0;
                let mut use_float = false;
                for arg in fixed {
                    match arg {
                        VmValue::Int(value) => {
                            add_int(*value, &mut acc_int, &mut acc_float, &mut use_float)
                        }
                        VmValue::Value(Value::Int(value)) => {
                            add_int(*value, &mut acc_int, &mut acc_float, &mut use_float)
                        }
                        VmValue::Value(Value::Float(value)) => {
                            add_float(*value, &mut acc_int, &mut acc_float, &mut use_float)
                        }
                        _ => return None,
                    }
                }
                match tail {
                    VmValue::Iter(iter) => {
                        let mut ok = true;
                        let result = iter.for_each_vmvalue(state, |_, value| {
                            match value {
                                VmValue::Int(value) => {
                                    add_int(value, &mut acc_int, &mut acc_float, &mut use_float)
                                }
                                VmValue::Value(Value::Int(value)) => {
                                    add_int(value, &mut acc_int, &mut acc_float, &mut use_float)
                                }
                                VmValue::Value(Value::Float(value)) => {
                                    add_float(value, &mut acc_int, &mut acc_float, &mut use_float)
                                }
                                _ => {
                                    ok = false;
                                }
                            }
                            Ok(())
                        });
                        if result.is_err() || !ok {
                            return None;
                        }
                    }
                    VmValue::Value(Value::Vec(items)) => {
                        for value in items.iter() {
                            match value {
                                Value::Int(value) => {
                                    add_int(*value, &mut acc_int, &mut acc_float, &mut use_float)
                                }
                                Value::Float(value) => {
                                    add_float(*value, &mut acc_int, &mut acc_float, &mut use_float)
                                }
                                _ => return None,
                            }
                        }
                    }
                    _ => return None,
                }
                if use_float {
                    Some(VmValue::Value(Value::Float(acc_float)))
                } else {
                    Some(VmValue::Int(acc_int))
                }
            };
            if let Some(value) = try_apply_add(state, &args) {
                return Ok(value);
            }
            let args = vm_args_to_values(state, args)?;
            Ok(VmValue::Value(call_builtin(state, "apply", args)?))
        }
        "map" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: map expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: map expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.map(func)))
        }
        "filter" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: filter expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: filter expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.filter(func)))
        }
        "remove" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: remove expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: remove expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.remove(func)))
        }
        "drop-while" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: drop-while expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: drop-while expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.drop_while(func)))
        }
        "keep" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: keep expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: keep expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.keep(func)))
        }
        "keep-indexed" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: keep-indexed expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: keep-indexed expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            Ok(VmValue::Iter(iter.keep_indexed(func)))
        }
        "some" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: some expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: some expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            let mut native_cache: Vec<Option<NativeCacheEntry>> = vec![None];
            let mut found = Value::Nil;
            iter.for_each(state, |state, value| {
                let res = call_value_arity1_cached(state, &func, value, &mut native_cache, 0)?;
                if is_truthy(&res) {
                    found = res;
                    return Err(Clove2Error::new("vm: _some_break"));
                }
                Ok(())
            })
            .or_else(|err| {
                if err.message() == Some("vm: _some_break") {
                    Ok(())
                } else {
                    Err(err)
                }
            })?;
            Ok(VmValue::Value(found))
        }
        "every?" | "not-every?" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: every? expects 2 arguments"));
            }
            let coll = args.pop().unwrap();
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: every? expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            let negated = name == "not-every?";
            let mut native_cache: Vec<Option<NativeCacheEntry>> = vec![None];
            let mut ok = !negated;
            iter.for_each(state, |state, value| {
                let res = call_value_arity1_cached(state, &func, value, &mut native_cache, 0)?;
                let truthy = is_truthy(&res);
                if negated {
                    if !truthy {
                        ok = true;
                        return Err(Clove2Error::new("vm: _every_break"));
                    }
                    Ok(())
                } else {
                    if !truthy {
                        ok = false;
                        return Err(Clove2Error::new("vm: _every_break"));
                    }
                    Ok(())
                }
            })
            .or_else(|err| {
                if err.message() == Some("vm: _every_break") {
                    Ok(())
                } else {
                    Err(err)
                }
            })?;
            Ok(VmValue::Value(Value::Bool(ok)))
        }
        "reduce" => {
            if args.len() < 2 || args.len() > 3 {
                return Err(Clove2Error::new("vm: reduce expects 2 or 3 arguments"));
            }
            let coll = args.pop().unwrap();
            let init = if args.len() == 2 {
                Some(args.pop().unwrap())
            } else {
                None
            };
            let func = match args.pop().unwrap() {
                VmValue::Value(value) => value,
                _ => return Err(Clove2Error::new("vm: reduce expects function")),
            };
            let iter = vmvalue_into_iter(coll)?;
            if let Value::Builtin(name) = &func {
                if name == "+" {
                    let init_int = match &init {
                        Some(VmValue::Int(value)) => Some(*value),
                        Some(VmValue::Value(Value::Int(value))) => Some(*value),
                        Some(_) => None,
                        None => None,
                    };
                    if init.is_none() || init_int.is_some() {
                        let iter_fast = iter.clone();
                        let mut acc_int = init_int;
                        let mut ok = true;
                        iter_fast
                            .for_each_vmvalue(state, |_, item| match item {
                                VmValue::Int(value) => {
                                    if let Some(acc) = acc_int.as_mut() {
                                        *acc += value;
                                    } else {
                                        acc_int = Some(value);
                                    }
                                    Ok(())
                                }
                                VmValue::Value(Value::Int(value)) => {
                                    if let Some(acc) = acc_int.as_mut() {
                                        *acc += value;
                                    } else {
                                        acc_int = Some(value);
                                    }
                                    Ok(())
                                }
                                _ => {
                                    ok = false;
                                    Err(Clove2Error::new("vm: _reduce_int_break"))
                                }
                            })
                            .or_else(|err| {
                                if err.message() == Some("vm: _reduce_int_break") {
                                    Ok(())
                                } else {
                                    Err(err)
                                }
                            })?;
                        if ok {
                            if let Some(acc) = acc_int {
                                return Ok(VmValue::Int(acc));
                            }
                            return Err(Clove2Error::new("vm: reduce on empty collection"));
                        }
                    }
                }
            }
            let mut acc = if let Some(init) = init {
                Some(vmvalue_into_value(state, init)?)
            } else {
                None
            };
            iter.for_each(state, |state, item| {
                if acc.is_none() {
                    acc = Some(item);
                    return Ok(());
                }
                let cur = acc.take().expect("acc must exist");
                acc = Some(call_value_arity2(state, &func, cur, item)?);
                Ok(())
            })?;
            acc.map(VmValue::Value)
                .ok_or_else(|| Clove2Error::new("vm: reduce on empty collection"))
        }
        "bit-and" | "bit-or" | "bit-xor" => {
            if args.len() < 2 {
                return Err(Clove2Error::new("vm: bit op expects 2+ arguments"));
            }
            let op = name;
            let mut iter = args.into_iter();
            let mut acc = vmvalue_into_int(op, iter.next().unwrap())?;
            for arg in iter {
                let value = vmvalue_into_int(op, arg)?;
                acc = match op {
                    "bit-and" => acc & value,
                    "bit-or" => acc | value,
                    "bit-xor" => acc ^ value,
                    _ => acc,
                };
            }
            Ok(VmValue::Int(acc))
        }
        "bit-not" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: bit-not expects 1 argument"));
            }
            let value = vmvalue_into_int("bit-not", args.pop().unwrap())?;
            Ok(VmValue::Int(!value))
        }
        "bit-shift-left" | "bit-shift-right" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: bit-shift expects 2 arguments"));
            }
            let right = vmvalue_into_int(name, args.pop().unwrap())?;
            let left = vmvalue_into_int(name, args.pop().unwrap())?;
            let value = match name {
                "bit-shift-left" => left << right,
                "bit-shift-right" => left >> right,
                _ => left,
            };
            Ok(VmValue::Int(value))
        }
        "max" => {
            if args.is_empty() {
                return Err(Clove2Error::new("vm: max expects 1+ arguments"));
            }
            let mut iter = args.into_iter();
            let first = iter.next().unwrap();
            let mut use_float = false;
            let mut acc_int = 0i64;
            let mut acc_float = 0.0f64;
            match first {
                VmValue::Int(value) => acc_int = value,
                VmValue::Value(Value::Int(value)) => acc_int = value,
                VmValue::Value(Value::Float(value)) => {
                    use_float = true;
                    acc_float = value;
                }
                _ => return Err(Clove2Error::new("vm: max expects number")),
            }
            for arg in iter {
                match arg {
                    VmValue::Int(value) => {
                        if use_float {
                            let v = value as f64;
                            if v > acc_float {
                                acc_float = v;
                            }
                        } else if value > acc_int {
                            acc_int = value;
                        }
                    }
                    VmValue::Value(Value::Int(value)) => {
                        if use_float {
                            let v = value as f64;
                            if v > acc_float {
                                acc_float = v;
                            }
                        } else if value > acc_int {
                            acc_int = value;
                        }
                    }
                    VmValue::Value(Value::Float(value)) => {
                        if !use_float {
                            use_float = true;
                            acc_float = acc_int as f64;
                        }
                        if value > acc_float {
                            acc_float = value;
                        }
                    }
                    _ => return Err(Clove2Error::new("vm: max expects number")),
                }
            }
            if use_float {
                Ok(VmValue::Value(Value::Float(acc_float)))
            } else {
                Ok(VmValue::Int(acc_int))
            }
        }
        _ => {
            let args = vm_args_to_values(state, args)?;
            Ok(VmValue::Value(call_builtin(state, name, args)?))
        }
    }
}

fn call_value_vm(
    state: &mut VmState,
    callee: VmValue,
    args: Vec<VmValue>,
) -> Result<VmValue, Clove2Error> {
    let callee = match callee {
        VmValue::Value(value) => value,
        _ => return Err(Clove2Error::new("vm: call target is not callable")),
    };
    match callee {
        Value::NativeFunction(id) => {
            let args = vm_args_to_values(state, args)?;
            Ok(VmValue::Value(call_vm_function(state, id, args)?))
        }
        Value::Builtin(name) => call_builtin_vm(state, &name, args),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_value_vm_arity1(
    state: &mut VmState,
    callee: VmValue,
    arg: VmValue,
) -> Result<VmValue, Clove2Error> {
    let callee = match callee {
        VmValue::Value(value) => value,
        _ => return Err(Clove2Error::new("vm: call target is not callable")),
    };
    match callee {
        Value::NativeFunction(id) => {
            let arg = vmvalue_into_value(state, arg)?;
            Ok(VmValue::Value(call_vm_function_arity1(state, id, arg)?))
        }
        Value::Builtin(name) => {
            if let Some(result) = call_builtin_1_vm(state, &name, &arg) {
                return result;
            }
            let arg = vmvalue_into_value(state, arg)?;
            Ok(VmValue::Value(call_builtin_1(state, &name, arg)?))
        }
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_value_vm_arity2(
    state: &mut VmState,
    callee: VmValue,
    arg1: VmValue,
    arg2: VmValue,
) -> Result<VmValue, Clove2Error> {
    let callee = match callee {
        VmValue::Value(value) => value,
        _ => return Err(Clove2Error::new("vm: call target is not callable")),
    };
    match callee {
        Value::NativeFunction(id) => {
            let arg1 = vmvalue_into_value(state, arg1)?;
            let arg2 = vmvalue_into_value(state, arg2)?;
            Ok(VmValue::Value(call_vm_function_arity2(
                state, id, arg1, arg2,
            )?))
        }
        Value::Builtin(name) => {
            if let Some(result) = call_builtin_2_vm(state, &name, &arg1, &arg2) {
                return result;
            }
            let arg1 = vmvalue_into_value(state, arg1)?;
            let arg2 = vmvalue_into_value(state, arg2)?;
            Ok(VmValue::Value(call_builtin_2(state, &name, arg1, arg2)?))
        }
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn run_code(
    code: &[Instruction],
    state: &mut VmState,
    frame: &mut VmFrame,
) -> Result<Value, Clove2Error> {
    if profiler::is_enabled() {
        return run_code_profiled(code, state, frame);
    }
    run_code_fast(code, state, frame)
}

fn run_code_fast(
    code: &[Instruction],
    state: &mut VmState,
    frame: &mut VmFrame,
) -> Result<Value, Clove2Error> {
    let mut ip = 0usize;
    while ip < code.len() {
        match &code[ip] {
            Instruction::PushInt(value) => frame.stack.push(VmValue::Int(*value)),
            Instruction::PushBool(value) => frame.stack.push(VmValue::Bool(*value)),
            Instruction::PushStr(value) => {
                frame.stack.push(VmValue::Value(Value::Str(value.clone())))
            }
            Instruction::PushKeyword(value) => {
                frame.stack
                    .push(VmValue::Value(Value::Keyword(value.clone())))
            }
            Instruction::PushFloat(value) => {
                frame.stack.push(VmValue::Value(Value::Float(*value)))
            }
            Instruction::PushRegex(value) => {
                frame.stack
                    .push(VmValue::Value(Value::Regex(value.clone())))
            }
            Instruction::PushNil => frame.stack.push(VmValue::Value(Value::Nil)),
            Instruction::PushFunction(id) => {
                frame.stack.push(VmValue::Value(Value::NativeFunction(*id)))
            }
            Instruction::LoadLocal(slot) => {
                let value = frame.get_local_slot(*slot)?;
                frame.stack.push(value);
            }
            Instruction::StoreLocal(slot) => {
                let value = pop_vm_value(&mut frame.stack)?;
                frame.set_local_slot(*slot, value)?;
            }
            Instruction::LoadGlobalSlot(slot) => {
                let idx = *slot;
                if idx >= state.globals.len() {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
                let version = state.global_versions[idx];
                if state.global_cache_versions[idx] != version {
                    let value = state.globals[idx].clone();
                    state.global_cache_values[idx] = value_to_vmvalue(value);
                    state.global_cache_versions[idx] = version;
                }
                frame.stack.push(state.global_cache_values[idx].clone());
            }
            Instruction::StoreGlobalSlot(slot) => {
                let value = pop_value(state, &mut frame.stack)?;
                let idx = *slot;
                if let Some(target) = state.globals.get_mut(idx) {
                    *target = value.clone();
                    state.global_versions[idx] = state.global_versions[idx].saturating_add(1);
                    state.global_cache_values[idx] = value_to_vmvalue(value);
                    state.global_cache_versions[idx] = state.global_versions[idx];
                } else {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
            }
            Instruction::LoadGlobalDynamic(name) => {
                let slot = state
                    .global_map
                    .get(name)
                    .copied()
                    .ok_or_else(|| Clove2Error::new("vm: unknown symbol"))?;
                if slot >= state.globals.len() {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
                let version = state.global_versions[slot];
                if state.global_cache_versions[slot] != version {
                    let value = state.globals[slot].clone();
                    state.global_cache_values[slot] = value_to_vmvalue(value);
                    state.global_cache_versions[slot] = version;
                }
                frame.stack.push(state.global_cache_values[slot].clone());
            }
            Instruction::Map => {
                let coll = pop_vm_value(&mut frame.stack)?;
                let func = pop_vm_value(&mut frame.stack)?;
                let func = match func {
                    VmValue::Value(value) => value,
                    _ => return Err(Clove2Error::new("vm: map expects function")),
                };
                let iter = vmvalue_into_iter(coll)?;
                frame.stack.push(VmValue::Iter(iter.map(func)));
            }
            Instruction::Reduce(argc) => {
                if *argc < 2 || *argc > 3 {
                    return Err(Clove2Error::new("vm: reduce expects 2 or 3 args"));
                }
                let coll = pop_vm_value(&mut frame.stack)?;
                let init = if *argc == 3 {
                    Some(pop_vm_value(&mut frame.stack)?)
                } else {
                    None
                };
                let func = pop_vm_value(&mut frame.stack)?;
                let func = match func {
                    VmValue::Value(value) => value,
                    _ => return Err(Clove2Error::new("vm: reduce expects function")),
                };
                let iter = vmvalue_into_iter(coll)?;
                if let Value::Builtin(name) = &func {
                    if name == "+" {
                        let init_int = match &init {
                            Some(VmValue::Int(value)) => Some(*value),
                            Some(VmValue::Value(Value::Int(value))) => Some(*value),
                            Some(_) => None,
                            None => None,
                        };
                        if init.is_none() || init_int.is_some() {
                            if let Some(acc) = iter.reduce_int_fast(init_int)? {
                                frame.stack.push(VmValue::Int(acc));
                                ip += 1;
                                continue;
                            }
                            let iter_fast = iter.clone();
                            let mut acc_int = init_int;
                            let mut ok = true;
                            iter_fast
                                .for_each_vmvalue(state, |_, item| match item {
                                    VmValue::Int(value) => {
                                        if let Some(acc) = acc_int.as_mut() {
                                            *acc += value;
                                        } else {
                                            acc_int = Some(value);
                                        }
                                        Ok(())
                                    }
                                    VmValue::Value(Value::Int(value)) => {
                                        if let Some(acc) = acc_int.as_mut() {
                                            *acc += value;
                                        } else {
                                            acc_int = Some(value);
                                        }
                                        Ok(())
                                    }
                                    _ => {
                                        ok = false;
                                        Err(Clove2Error::new("vm: _reduce_int_break"))
                                    }
                                })
                                .or_else(|err| {
                                    if err.message() == Some("vm: _reduce_int_break") {
                                        Ok(())
                                    } else {
                                        Err(err)
                                    }
                                })?;
                            if ok {
                                if let Some(acc) = acc_int {
                                    frame.stack.push(VmValue::Int(acc));
                                    ip += 1;
                                    continue;
                                }
                                return Err(Clove2Error::new("vm: reduce on empty collection"));
                            }
                        }
                    }
                }
                let mut acc = if let Some(init) = init {
                    Some(vmvalue_into_value(state, init)?)
                } else {
                    None
                };
                iter.for_each(state, |state, item| {
                    if acc.is_none() {
                        acc = Some(item);
                        return Ok(());
                    }
                    let value = call_value_arity2_cached(
                        state,
                        &func,
                        acc.take().unwrap(),
                        item,
                        &mut vec![None],
                        0,
                    )?;
                    acc = Some(value);
                    Ok(())
                })?;
                let value =
                    acc.ok_or_else(|| Clove2Error::new("vm: reduce on empty collection"))?;
                frame.stack.push(value_to_vmvalue(value));
            }
            Instruction::MakeVec(count) => {
                let mut items = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    items.push(vmvalue_into_value(state, value)?);
                }
                items.reverse();
                frame.stack.push(VmValue::Value(Value::vec(items)));
            }
            Instruction::MakeSet(count) => {
                let mut items = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    items.push(vmvalue_into_value(state, value)?);
                }
                items.reverse();
                frame.stack.push(VmValue::Value(Value::set(items)));
            }
            Instruction::MakeMap(count) => {
                let mut map = BTreeMap::new();
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    let key = pop_vm_value(&mut frame.stack)?;
                    let value = vmvalue_into_value(state, value)?;
                    let key_value = vmvalue_into_value(state, key)?;
                    let key = value_to_key(&key_value)?;
                    map.insert(key, value);
                }
                frame.stack.push(VmValue::Value(Value::map(map)));
            }
            Instruction::Pop => {
                pop_vm_value(&mut frame.stack)?;
            }
            Instruction::AddInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a + b));
            }
            Instruction::AddIntN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: add expects args"));
                }
                let mut acc = 0i64;
                for _ in 0..*count {
                    acc += pop_int(state, &mut frame.stack)?;
                }
                frame.stack.push(VmValue::Int(acc));
            }
            Instruction::MaxNumberN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: max expects args"));
                }
                let mut use_float = false;
                let mut acc_int = 0i64;
                let mut acc_float = 0.0f64;
                let mut initialized = false;
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    match value {
                        VmValue::Int(v) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf > acc_float {
                                    acc_float = vf;
                                }
                            } else if v > acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Int(v)) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf > acc_float {
                                    acc_float = vf;
                                }
                            } else if v > acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Float(v)) => {
                            if !use_float {
                                use_float = true;
                                acc_float = if initialized { acc_int as f64 } else { v };
                                initialized = true;
                            }
                            if v > acc_float {
                                acc_float = v;
                            }
                        }
                        _ => return Err(Clove2Error::new("vm: max expects number")),
                    }
                }
                if !initialized {
                    return Err(Clove2Error::new("vm: max expects args"));
                }
                if use_float {
                    frame.stack.push(VmValue::Value(Value::Float(acc_float)));
                } else {
                    frame.stack.push(VmValue::Int(acc_int));
                }
            }
            Instruction::MinNumberN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: min expects args"));
                }
                let mut use_float = false;
                let mut acc_int = 0i64;
                let mut acc_float = 0.0f64;
                let mut initialized = false;
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    match value {
                        VmValue::Int(v) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf < acc_float {
                                    acc_float = vf;
                                }
                            } else if v < acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Int(v)) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf < acc_float {
                                    acc_float = vf;
                                }
                            } else if v < acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Float(v)) => {
                            if !use_float {
                                use_float = true;
                                acc_float = if initialized { acc_int as f64 } else { v };
                                initialized = true;
                            }
                            if v < acc_float {
                                acc_float = v;
                            }
                        }
                        _ => return Err(Clove2Error::new("vm: min expects number")),
                    }
                }
                if !initialized {
                    return Err(Clove2Error::new("vm: min expects args"));
                }
                if use_float {
                    frame.stack.push(VmValue::Value(Value::Float(acc_float)));
                } else {
                    frame.stack.push(VmValue::Int(acc_int));
                }
            }
            Instruction::SubInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a - b));
            }
            Instruction::MulInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a * b));
            }
            Instruction::DivInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                if b == 0 {
                    return Err(Clove2Error::new("vm: division by zero"));
                }
                frame.stack.push(VmValue::Int(a / b));
            }
            Instruction::ModInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                if b == 0 {
                    return Err(Clove2Error::new("vm: division by zero"));
                }
                frame.stack.push(VmValue::Int(a % b));
            }
            Instruction::EqInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a == b));
            }
            Instruction::LtInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a < b));
            }
            Instruction::GtInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a > b));
            }
            Instruction::LeInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a <= b));
            }
            Instruction::GeInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a >= b));
            }
            Instruction::Contains => {
                let key = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let result = match coll {
                    VmValue::Value(Value::Vec(items)) => {
                        let idx = match key {
                            VmValue::Int(value) => value,
                            VmValue::Value(Value::Int(value)) => value,
                            _ => return Err(Clove2Error::new("vm: contains? expects int index")),
                        } as isize;
                        idx >= 0 && (idx as usize) < items.len()
                    }
                    VmValue::Value(Value::Map(map)) => {
                        let key_value = vmvalue_into_value(state, key)?;
                        let key = value_to_key(&key_value)?;
                        map.contains_key(&key)
                    }
                    VmValue::Iter(iter) => {
                        let idx = match key {
                            VmValue::Int(value) => value,
                            VmValue::Value(Value::Int(value)) => value,
                            _ => return Err(Clove2Error::new("vm: contains? expects int index")),
                        } as isize;
                        if idx < 0 {
                            false
                        } else if iter.ops.is_empty() {
                            match iter.base {
                                IterBase::Range { len, .. } => (idx as usize) < len,
                                IterBase::Vec(items) => (idx as usize) < items.len(),
                            }
                        } else {
                            let items = iter.collect(state)?;
                            (idx as usize) < items.len()
                        }
                    }
                    _ => return Err(Clove2Error::new("vm: contains? expects vector/map")),
                };
                frame.stack.push(VmValue::Bool(result));
            }
            Instruction::Not => {
                let value = pop_truthy(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(!value));
            }
            Instruction::BoolToInt(then_val, else_val) => {
                let cond = pop_truthy(state, &mut frame.stack)?;
                let value = if cond { *then_val } else { *else_val };
                frame.stack.push(VmValue::Int(value));
            }
            Instruction::JumpIfFalse(target) => {
                let cond = pop_truthy(state, &mut frame.stack)?;
                if !cond {
                    ip = *target;
                    continue;
                }
            }
            Instruction::Jump(target) => {
                ip = *target;
                continue;
            }
            Instruction::Conj => {
                let value = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: conj expects vector"));
                };
                let value = match value {
                    VmValue::Int(value) => Value::Int(value),
                    VmValue::Bool(value) => Value::Bool(value),
                    VmValue::Value(value) => value,
                    VmValue::Iter(_) => return Err(Clove2Error::new("vm: conj expects value")),
                };
                let mut out = items.to_vec();
                out.push(value);
                frame.stack.push(VmValue::Value(Value::vec(out)));
            }
            Instruction::Nth => {
                let idx = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: nth expects vector"));
                };
                let idx = match idx {
                    VmValue::Int(value) => value,
                    VmValue::Value(Value::Int(value)) => value,
                    _ => return Err(Clove2Error::new("vm: nth expects int index")),
                } as isize;
                if idx < 0 || (idx as usize) >= items.len() {
                    return Err(Clove2Error::new("vm: nth out of range"));
                }
                frame
                    .stack
                    .push(VmValue::Value(items[idx as usize].clone()));
            }
            Instruction::Interleave => {
                let right = pop_vm_value(&mut frame.stack)?;
                let left = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(left_items)) = left else {
                    return Err(Clove2Error::new("vm: interleave expects vector"));
                };
                let VmValue::Value(Value::Vec(right_items)) = right else {
                    return Err(Clove2Error::new("vm: interleave expects vector"));
                };
                let len = left_items.len().min(right_items.len());
                let mut out = Vec::with_capacity(len * 2);
                for (l, r) in left_items.iter().zip(right_items.iter()).take(len) {
                    out.push(l.clone());
                    out.push(r.clone());
                }
                frame.stack.push(VmValue::Value(Value::vec(out)));
            }
            Instruction::Split => {
                let sep = pop_vm_value(&mut frame.stack)?;
                let text = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Str(text)) = text else {
                    return Err(Clove2Error::new("vm: split expects string"));
                };
                let VmValue::Value(Value::Str(sep)) = sep else {
                    return Err(Clove2Error::new("vm: split expects string separator"));
                };
                let parts: Vec<Value> = text
                    .split(&sep)
                    .map(|part| Value::Str(part.to_string()))
                    .collect();
                frame.stack.push(VmValue::Value(Value::vec(parts)));
            }
            Instruction::Join => {
                let sep = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Str(sep)) = sep else {
                    return Err(Clove2Error::new("vm: join expects string separator"));
                };
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: join expects vector of strings"));
                };
                let mut out = String::new();
                for (idx, item) in items.iter().enumerate() {
                    let Value::Str(item) = item else {
                        return Err(Clove2Error::new("vm: join expects vector of strings"));
                    };
                    if idx > 0 {
                        out.push_str(&sep);
                    }
                    out.push_str(item);
                }
                frame.stack.push(VmValue::Value(Value::Str(out)));
            }
            Instruction::CallBuiltin1(name) => {
                let arg = pop_vm_value(&mut frame.stack)?;
                if let Some(result) = call_builtin_1_vm(state, name, &arg) {
                    frame.stack.push(result?);
                } else {
                    let arg = vmvalue_into_value(state, arg)?;
                    let value = call_builtin_1(state, name, arg)?;
                    frame.stack.push(value_to_vmvalue(value));
                }
            }
            Instruction::CallBuiltin2(name) => {
                let arg2 = pop_vm_value(&mut frame.stack)?;
                let arg1 = pop_vm_value(&mut frame.stack)?;
                if let Some(result) = call_builtin_2_vm(state, name, &arg1, &arg2) {
                    frame.stack.push(result?);
                } else {
                    let arg1 = vmvalue_into_value(state, arg1)?;
                    let arg2 = vmvalue_into_value(state, arg2)?;
                    let value = call_builtin_2(state, name, arg1, arg2)?;
                    frame.stack.push(value_to_vmvalue(value));
                }
            }
            Instruction::Call(argc) => {
                match *argc {
                    1 => {
                        let arg = pop_vm_value(&mut frame.stack)?;
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm_arity1(state, callee, arg)?;
                        frame.stack.push(value);
                    }
                    2 => {
                        let arg2 = pop_vm_value(&mut frame.stack)?;
                        let arg1 = pop_vm_value(&mut frame.stack)?;
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm_arity2(state, callee, arg1, arg2)?;
                        frame.stack.push(value);
                    }
                    _ => {
                        let mut args = Vec::with_capacity(*argc);
                        for _ in 0..*argc {
                            args.push(pop_vm_value(&mut frame.stack)?);
                        }
                        args.reverse();
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm(state, callee, args)?;
                        frame.stack.push(value);
                    }
                }
            }
        }
        ip += 1;
    }
    let value = frame.stack.pop().unwrap_or(VmValue::Value(Value::Nil));
    vmvalue_into_value(state, value)
}

fn run_code_profiled(
    code: &[Instruction],
    state: &mut VmState,
    frame: &mut VmFrame,
) -> Result<Value, Clove2Error> {
    let mut ip = 0usize;
    while ip < code.len() {
        let instr = &code[ip];
        let label = instr_label(instr);
        let start = Instant::now();
        match instr {
            Instruction::PushInt(value) => frame.stack.push(VmValue::Int(*value)),
            Instruction::PushBool(value) => frame.stack.push(VmValue::Bool(*value)),
            Instruction::PushStr(value) => {
                frame.stack.push(VmValue::Value(Value::Str(value.clone())))
            }
            Instruction::PushKeyword(value) => {
                frame.stack
                    .push(VmValue::Value(Value::Keyword(value.clone())))
            }
            Instruction::PushFloat(value) => {
                frame.stack.push(VmValue::Value(Value::Float(*value)))
            }
            Instruction::PushRegex(value) => {
                frame.stack
                    .push(VmValue::Value(Value::Regex(value.clone())))
            }
            Instruction::PushNil => frame.stack.push(VmValue::Value(Value::Nil)),
            Instruction::PushFunction(id) => {
                frame.stack.push(VmValue::Value(Value::NativeFunction(*id)))
            }
            Instruction::LoadLocal(slot) => {
                let value = frame.get_local_slot(*slot)?;
                frame.stack.push(value);
            }
            Instruction::StoreLocal(slot) => {
                let value = pop_vm_value(&mut frame.stack)?;
                frame.set_local_slot(*slot, value)?;
            }
            Instruction::LoadGlobalSlot(slot) => {
                let idx = *slot;
                if idx >= state.globals.len() {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
                let version = state.global_versions[idx];
                if state.global_cache_versions[idx] != version {
                    let value = state.globals[idx].clone();
                    state.global_cache_values[idx] = value_to_vmvalue(value);
                    state.global_cache_versions[idx] = version;
                }
                frame.stack.push(state.global_cache_values[idx].clone());
            }
            Instruction::StoreGlobalSlot(slot) => {
                let value = pop_value(state, &mut frame.stack)?;
                let idx = *slot;
                if let Some(target) = state.globals.get_mut(idx) {
                    *target = value.clone();
                    state.global_versions[idx] = state.global_versions[idx].saturating_add(1);
                    state.global_cache_values[idx] = value_to_vmvalue(value);
                    state.global_cache_versions[idx] = state.global_versions[idx];
                } else {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
            }
            Instruction::LoadGlobalDynamic(name) => {
                let slot = state
                    .global_map
                    .get(name)
                    .copied()
                    .ok_or_else(|| Clove2Error::new("vm: unknown symbol"))?;
                if slot >= state.globals.len() {
                    return Err(Clove2Error::new("vm: global slot out of range"));
                }
                let version = state.global_versions[slot];
                if state.global_cache_versions[slot] != version {
                    let value = state.globals[slot].clone();
                    state.global_cache_values[slot] = value_to_vmvalue(value);
                    state.global_cache_versions[slot] = version;
                }
                frame.stack.push(state.global_cache_values[slot].clone());
            }
            Instruction::Map => {
                let coll = pop_vm_value(&mut frame.stack)?;
                let func = pop_vm_value(&mut frame.stack)?;
                let func = match func {
                    VmValue::Value(value) => value,
                    _ => return Err(Clove2Error::new("vm: map expects function")),
                };
                let iter = vmvalue_into_iter(coll)?;
                frame.stack.push(VmValue::Iter(iter.map(func)));
            }
            Instruction::Reduce(argc) => {
                if *argc < 2 || *argc > 3 {
                    return Err(Clove2Error::new("vm: reduce expects 2 or 3 args"));
                }
                let coll = pop_vm_value(&mut frame.stack)?;
                let init = if *argc == 3 {
                    Some(pop_vm_value(&mut frame.stack)?)
                } else {
                    None
                };
                let func = pop_vm_value(&mut frame.stack)?;
                let func = match func {
                    VmValue::Value(value) => value,
                    _ => return Err(Clove2Error::new("vm: reduce expects function")),
                };
                let iter = vmvalue_into_iter(coll)?;
                if let Value::Builtin(name) = &func {
                    if name == "+" {
                        let init_int = match &init {
                            Some(VmValue::Int(value)) => Some(*value),
                            Some(VmValue::Value(Value::Int(value))) => Some(*value),
                            Some(_) => None,
                            None => None,
                        };
                        if init.is_none() || init_int.is_some() {
                            if let Some(acc) = iter.reduce_int_fast(init_int)? {
                                frame.stack.push(VmValue::Int(acc));
                                let elapsed = start.elapsed().as_nanos();
                                profiler::record_op_label(label, elapsed);
                                ip += 1;
                                continue;
                            }
                            let iter_fast = iter.clone();
                            let mut acc_int = init_int;
                            let mut ok = true;
                            iter_fast
                                .for_each_vmvalue(state, |_, item| match item {
                                    VmValue::Int(value) => {
                                        if let Some(acc) = acc_int.as_mut() {
                                            *acc += value;
                                        } else {
                                            acc_int = Some(value);
                                        }
                                        Ok(())
                                    }
                                    VmValue::Value(Value::Int(value)) => {
                                        if let Some(acc) = acc_int.as_mut() {
                                            *acc += value;
                                        } else {
                                            acc_int = Some(value);
                                        }
                                        Ok(())
                                    }
                                    _ => {
                                        ok = false;
                                        Err(Clove2Error::new("vm: _reduce_int_break"))
                                    }
                                })
                                .or_else(|err| {
                                    if err.message() == Some("vm: _reduce_int_break") {
                                        Ok(())
                                    } else {
                                        Err(err)
                                    }
                                })?;
                            if ok {
                                if let Some(acc) = acc_int {
                                    frame.stack.push(VmValue::Int(acc));
                                    let elapsed = start.elapsed().as_nanos();
                                    profiler::record_op_label(label, elapsed);
                                    ip += 1;
                                    continue;
                                }
                                return Err(Clove2Error::new("vm: reduce on empty collection"));
                            }
                        }
                    }
                }
                let mut acc = if let Some(init) = init {
                    Some(vmvalue_into_value(state, init)?)
                } else {
                    None
                };
                let mut native_cache: Vec<Option<NativeCacheEntry>> = vec![None];
                iter.for_each(state, |state, item| {
                    if acc.is_none() {
                        acc = Some(item);
                        return Ok(());
                    }
                    let value = call_value_arity2_cached(
                        state,
                        &func,
                        acc.take().unwrap(),
                        item,
                        &mut native_cache,
                        0,
                    )?;
                    acc = Some(value);
                    Ok(())
                })?;
                let value =
                    acc.ok_or_else(|| Clove2Error::new("vm: reduce on empty collection"))?;
                frame.stack.push(value_to_vmvalue(value));
            }
            Instruction::MakeVec(count) => {
                let mut items = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    items.push(vmvalue_into_value(state, value)?);
                }
                items.reverse();
                frame.stack.push(VmValue::Value(Value::vec(items)));
            }
            Instruction::MakeSet(count) => {
                let mut items = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    items.push(vmvalue_into_value(state, value)?);
                }
                items.reverse();
                frame.stack.push(VmValue::Value(Value::set(items)));
            }
            Instruction::MakeMap(count) => {
                let mut map = BTreeMap::new();
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    let key = pop_vm_value(&mut frame.stack)?;
                    let value = vmvalue_into_value(state, value)?;
                    let key_value = vmvalue_into_value(state, key)?;
                    let key = value_to_key(&key_value)?;
                    map.insert(key, value);
                }
                frame.stack.push(VmValue::Value(Value::map(map)));
            }
            Instruction::Pop => {
                pop_vm_value(&mut frame.stack)?;
            }
            Instruction::AddInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a + b));
            }
            Instruction::AddIntN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: add expects args"));
                }
                let mut acc = 0i64;
                for _ in 0..*count {
                    acc += pop_int(state, &mut frame.stack)?;
                }
                frame.stack.push(VmValue::Int(acc));
            }
            Instruction::MaxNumberN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: max expects args"));
                }
                let mut use_float = false;
                let mut acc_int = 0i64;
                let mut acc_float = 0.0f64;
                let mut initialized = false;
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    match value {
                        VmValue::Int(v) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf > acc_float {
                                    acc_float = vf;
                                }
                            } else if v > acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Int(v)) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf > acc_float {
                                    acc_float = vf;
                                }
                            } else if v > acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Float(v)) => {
                            if !use_float {
                                use_float = true;
                                acc_float = if initialized { acc_int as f64 } else { v };
                                initialized = true;
                            }
                            if v > acc_float {
                                acc_float = v;
                            }
                        }
                        _ => return Err(Clove2Error::new("vm: max expects number")),
                    }
                }
                if !initialized {
                    return Err(Clove2Error::new("vm: max expects args"));
                }
                if use_float {
                    frame.stack.push(VmValue::Value(Value::Float(acc_float)));
                } else {
                    frame.stack.push(VmValue::Int(acc_int));
                }
            }
            Instruction::MinNumberN(count) => {
                if *count == 0 {
                    return Err(Clove2Error::new("vm: min expects args"));
                }
                let mut use_float = false;
                let mut acc_int = 0i64;
                let mut acc_float = 0.0f64;
                let mut initialized = false;
                for _ in 0..*count {
                    let value = pop_vm_value(&mut frame.stack)?;
                    match value {
                        VmValue::Int(v) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf < acc_float {
                                    acc_float = vf;
                                }
                            } else if v < acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Int(v)) => {
                            if !initialized {
                                acc_int = v;
                                initialized = true;
                            } else if use_float {
                                let vf = v as f64;
                                if vf < acc_float {
                                    acc_float = vf;
                                }
                            } else if v < acc_int {
                                acc_int = v;
                            }
                        }
                        VmValue::Value(Value::Float(v)) => {
                            if !use_float {
                                use_float = true;
                                acc_float = if initialized { acc_int as f64 } else { v };
                                initialized = true;
                            }
                            if v < acc_float {
                                acc_float = v;
                            }
                        }
                        _ => return Err(Clove2Error::new("vm: min expects number")),
                    }
                }
                if !initialized {
                    return Err(Clove2Error::new("vm: min expects args"));
                }
                if use_float {
                    frame.stack.push(VmValue::Value(Value::Float(acc_float)));
                } else {
                    frame.stack.push(VmValue::Int(acc_int));
                }
            }

            Instruction::SubInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a - b));
            }
            Instruction::MulInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Int(a * b));
            }
            Instruction::DivInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                if b == 0 {
                    return Err(Clove2Error::new("vm: division by zero"));
                }
                frame.stack.push(VmValue::Int(a / b));
            }
            Instruction::ModInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                if b == 0 {
                    return Err(Clove2Error::new("vm: division by zero"));
                }
                frame.stack.push(VmValue::Int(a % b));
            }
            Instruction::EqInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a == b));
            }
            Instruction::LtInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a < b));
            }
            Instruction::GtInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a > b));
            }
            Instruction::LeInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a <= b));
            }
            Instruction::GeInt => {
                let b = pop_int(state, &mut frame.stack)?;
                let a = pop_int(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(a >= b));
            }
            Instruction::Contains => {
                let key = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let result = match coll {
                    VmValue::Value(Value::Vec(items)) => {
                        let idx = match key {
                            VmValue::Int(value) => value,
                            VmValue::Value(Value::Int(value)) => value,
                            _ => return Err(Clove2Error::new("vm: contains? expects int index")),
                        } as isize;
                        idx >= 0 && (idx as usize) < items.len()
                    }
                    VmValue::Value(Value::Map(map)) => {
                        let key_value = vmvalue_into_value(state, key)?;
                        let key = value_to_key(&key_value)?;
                        map.contains_key(&key)
                    }
                    VmValue::Iter(iter) => {
                        let idx = match key {
                            VmValue::Int(value) => value,
                            VmValue::Value(Value::Int(value)) => value,
                            _ => return Err(Clove2Error::new("vm: contains? expects int index")),
                        } as isize;
                        if idx < 0 {
                            false
                        } else if iter.ops.is_empty() {
                            match iter.base {
                                IterBase::Range { len, .. } => (idx as usize) < len,
                                IterBase::Vec(items) => (idx as usize) < items.len(),
                            }
                        } else {
                            let items = iter.collect(state)?;
                            (idx as usize) < items.len()
                        }
                    }
                    _ => return Err(Clove2Error::new("vm: contains? expects vector/map")),
                };
                frame.stack.push(VmValue::Bool(result));
            }
            Instruction::Not => {
                let value = pop_truthy(state, &mut frame.stack)?;
                frame.stack.push(VmValue::Bool(!value));
            }
            Instruction::BoolToInt(then_val, else_val) => {
                let cond = pop_truthy(state, &mut frame.stack)?;
                let value = if cond { *then_val } else { *else_val };
                frame.stack.push(VmValue::Int(value));
            }
            Instruction::JumpIfFalse(target) => {
                let cond = pop_truthy(state, &mut frame.stack)?;
                if !cond {
                    ip = *target;
                    let elapsed = start.elapsed().as_nanos();
                    profiler::record_op_label(label, elapsed);
                    continue;
                }
            }
            Instruction::Jump(target) => {
                ip = *target;
                let elapsed = start.elapsed().as_nanos();
                profiler::record_op_label(label, elapsed);
                continue;
            }
            Instruction::Conj => {
                let value = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: conj expects vector"));
                };
                let value = match value {
                    VmValue::Int(value) => Value::Int(value),
                    VmValue::Bool(value) => Value::Bool(value),
                    VmValue::Value(value) => value,
                    VmValue::Iter(_) => return Err(Clove2Error::new("vm: conj expects value")),
                };
                let mut out = items.to_vec();
                out.push(value);
                frame.stack.push(VmValue::Value(Value::vec(out)));
            }
            Instruction::Nth => {
                let idx = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: nth expects vector"));
                };
                let idx = match idx {
                    VmValue::Int(value) => value,
                    VmValue::Value(Value::Int(value)) => value,
                    _ => return Err(Clove2Error::new("vm: nth expects int index")),
                } as isize;
                if idx < 0 || (idx as usize) >= items.len() {
                    return Err(Clove2Error::new("vm: nth out of range"));
                }
                frame
                    .stack
                    .push(VmValue::Value(items[idx as usize].clone()));
            }
            Instruction::Interleave => {
                let right = pop_vm_value(&mut frame.stack)?;
                let left = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Vec(left_items)) = left else {
                    return Err(Clove2Error::new("vm: interleave expects vector"));
                };
                let VmValue::Value(Value::Vec(right_items)) = right else {
                    return Err(Clove2Error::new("vm: interleave expects vector"));
                };
                let len = left_items.len().min(right_items.len());
                let mut out = Vec::with_capacity(len * 2);
                for (l, r) in left_items.iter().zip(right_items.iter()).take(len) {
                    out.push(l.clone());
                    out.push(r.clone());
                }
                frame.stack.push(VmValue::Value(Value::vec(out)));
            }
            Instruction::Split => {
                let sep = pop_vm_value(&mut frame.stack)?;
                let text = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Str(text)) = text else {
                    return Err(Clove2Error::new("vm: split expects string"));
                };
                let VmValue::Value(Value::Str(sep)) = sep else {
                    return Err(Clove2Error::new("vm: split expects string separator"));
                };
                let parts: Vec<Value> = text
                    .split(&sep)
                    .map(|part| Value::Str(part.to_string()))
                    .collect();
                frame.stack.push(VmValue::Value(Value::vec(parts)));
            }
            Instruction::Join => {
                let sep = pop_vm_value(&mut frame.stack)?;
                let coll = pop_vm_value(&mut frame.stack)?;
                let VmValue::Value(Value::Str(sep)) = sep else {
                    return Err(Clove2Error::new("vm: join expects string separator"));
                };
                let VmValue::Value(Value::Vec(items)) = coll else {
                    return Err(Clove2Error::new("vm: join expects vector of strings"));
                };
                let mut out = String::new();
                for (idx, item) in items.iter().enumerate() {
                    let Value::Str(item) = item else {
                        return Err(Clove2Error::new("vm: join expects vector of strings"));
                    };
                    if idx > 0 {
                        out.push_str(&sep);
                    }
                    out.push_str(item);
                }
                frame.stack.push(VmValue::Value(Value::Str(out)));
            }
            Instruction::CallBuiltin1(name) => {
                let arg = pop_vm_value(&mut frame.stack)?;
                if let Some(result) = call_builtin_1_vm(state, name, &arg) {
                    frame.stack.push(result?);
                } else {
                    let arg = vmvalue_into_value(state, arg)?;
                    let value = call_builtin_1(state, name, arg)?;
                    frame.stack.push(value_to_vmvalue(value));
                }
            }
            Instruction::CallBuiltin2(name) => {
                let arg2 = pop_vm_value(&mut frame.stack)?;
                let arg1 = pop_vm_value(&mut frame.stack)?;
                if let Some(result) = call_builtin_2_vm(state, name, &arg1, &arg2) {
                    frame.stack.push(result?);
                } else {
                    let arg1 = vmvalue_into_value(state, arg1)?;
                    let arg2 = vmvalue_into_value(state, arg2)?;
                    let value = call_builtin_2(state, name, arg1, arg2)?;
                    frame.stack.push(value_to_vmvalue(value));
                }
            }
            Instruction::Call(argc) => {
                match *argc {
                    1 => {
                        let arg = pop_vm_value(&mut frame.stack)?;
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm_arity1(state, callee, arg)?;
                        frame.stack.push(value);
                    }
                    2 => {
                        let arg2 = pop_vm_value(&mut frame.stack)?;
                        let arg1 = pop_vm_value(&mut frame.stack)?;
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm_arity2(state, callee, arg1, arg2)?;
                        frame.stack.push(value);
                    }
                    _ => {
                        let mut args = Vec::with_capacity(*argc);
                        for _ in 0..*argc {
                            args.push(pop_vm_value(&mut frame.stack)?);
                        }
                        args.reverse();
                        let callee = pop_vm_value(&mut frame.stack)?;
                        let value = call_value_vm(state, callee, args)?;
                        frame.stack.push(value);
                    }
                }
            }
        }
        let elapsed = start.elapsed().as_nanos();
        profiler::record_op_label(label, elapsed);
        ip += 1;
    }
    let value = frame.stack.pop().unwrap_or(VmValue::Value(Value::Nil));
    vmvalue_into_value(state, value)
}

fn instr_label(instr: &Instruction) -> &'static str {
    match instr {
        Instruction::PushInt(_) => "PushInt",
        Instruction::PushBool(_) => "PushBool",
        Instruction::PushStr(_) => "PushStr",
        Instruction::PushKeyword(_) => "PushKeyword",
        Instruction::PushFloat(_) => "PushFloat",
        Instruction::PushRegex(_) => "PushRegex",
        Instruction::PushNil => "PushNil",
        Instruction::PushFunction(_) => "PushFunction",
        Instruction::LoadLocal(_) => "LoadLocal",
        Instruction::StoreLocal(_) => "StoreLocal",
        Instruction::LoadGlobalSlot(_) => "LoadGlobal",
        Instruction::StoreGlobalSlot(_) => "StoreGlobal",
        Instruction::LoadGlobalDynamic(_) => "LoadGlobalDyn",
        Instruction::Map => "Map",
        Instruction::Reduce(_) => "Reduce",
        Instruction::MakeVec(_) => "MakeVec",
        Instruction::MakeSet(_) => "MakeSet",
        Instruction::MakeMap(_) => "MakeMap",
        Instruction::Pop => "Pop",
        Instruction::AddInt => "AddInt",
        Instruction::AddIntN(_) => "AddIntN",
        Instruction::MinNumberN(_) => "MinNumberN",
        Instruction::MaxNumberN(_) => "MaxNumberN",
        Instruction::SubInt => "SubInt",
        Instruction::MulInt => "MulInt",
        Instruction::DivInt => "DivInt",
        Instruction::ModInt => "ModInt",
        Instruction::EqInt => "EqInt",
        Instruction::LtInt => "LtInt",
        Instruction::GtInt => "GtInt",
        Instruction::LeInt => "LeInt",
        Instruction::GeInt => "GeInt",
        Instruction::Contains => "Contains",
        Instruction::Not => "Not",
        Instruction::BoolToInt(_, _) => "BoolToInt",
        Instruction::JumpIfFalse(_) => "JumpIfFalse",
        Instruction::Jump(_) => "Jump",
        Instruction::Conj => "Conj",
        Instruction::Nth => "Nth",
        Instruction::Interleave => "Interleave",
        Instruction::Split => "Split",
        Instruction::Join => "Join",
        Instruction::CallBuiltin1(_) => "CallBuiltin1",
        Instruction::CallBuiltin2(_) => "CallBuiltin2",
        Instruction::Call(_) => "Call",
    }
}

fn call_value(state: &mut VmState, callee: Value, args: Vec<Value>) -> Result<Value, Clove2Error> {
    match callee {
        Value::NativeFunction(id) => call_vm_function(state, id, args),
        Value::Builtin(name) => call_builtin(state, &name, args),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_value_arity1(
    state: &mut VmState,
    callee: &Value,
    arg: Value,
) -> Result<Value, Clove2Error> {
    match callee {
        Value::NativeFunction(id) => call_vm_function_arity1(state, *id, arg),
        Value::Builtin(name) => call_builtin_1(state, name, arg),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_value_arity2(
    state: &mut VmState,
    callee: &Value,
    arg1: Value,
    arg2: Value,
) -> Result<Value, Clove2Error> {
    match callee {
        Value::NativeFunction(id) => call_vm_function_arity2(state, *id, arg1, arg2),
        Value::Builtin(name) => call_builtin_2(state, name, arg1, arg2),
        _ => Err(Clove2Error::new("vm: call target is not callable")),
    }
}

fn call_vm_function(
    state: &mut VmState,
    id: usize,
    args: Vec<Value>,
) -> Result<Value, Clove2Error> {
    let func = state
        .functions
        .get(id)
        .ok_or_else(|| Clove2Error::new("vm: function not found"))?
        .clone();
    if args.len() != func.params.len() {
        return Err(Clove2Error::new("vm: arity mismatch"));
    }
    let mut frame = VmFrame::new(func.local_count);
    for (idx, value) in args.into_iter().enumerate() {
        frame.set_local_slot(idx, value_to_vmvalue(value))?;
    }
    run_code(&func.code, state, &mut frame)
}

fn call_vm_function_arity1(
    state: &mut VmState,
    id: usize,
    arg: Value,
) -> Result<Value, Clove2Error> {
    let func = state
        .functions
        .get(id)
        .ok_or_else(|| Clove2Error::new("vm: function not found"))?
        .clone();
    if func.params.len() != 1 {
        return Err(Clove2Error::new("vm: arity mismatch"));
    }
    let mut frame = VmFrame::new(func.local_count);
    frame.set_local_slot(0, value_to_vmvalue(arg))?;
    run_code(&func.code, state, &mut frame)
}

fn call_vm_function_arity2(
    state: &mut VmState,
    id: usize,
    arg1: Value,
    arg2: Value,
) -> Result<Value, Clove2Error> {
    let func = state
        .functions
        .get(id)
        .ok_or_else(|| Clove2Error::new("vm: function not found"))?
        .clone();
    if func.params.len() != 2 {
        return Err(Clove2Error::new("vm: arity mismatch"));
    }
    let mut frame = VmFrame::new(func.local_count);
    frame.set_local_slot(0, value_to_vmvalue(arg1))?;
    frame.set_local_slot(1, value_to_vmvalue(arg2))?;
    run_code(&func.code, state, &mut frame)
}

fn call_builtin(state: &mut VmState, name: &str, args: Vec<Value>) -> Result<Value, Clove2Error> {
    match name {
        "inc" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: inc expects 1 argument"));
            }
            match &args[0] {
                Value::Int(value) => Ok(Value::Int(value + 1)),
                Value::Float(value) => Ok(Value::Float(value + 1.0)),
                _ => Err(Clove2Error::new("vm: inc expects number")),
            }
        }
        "dec" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: dec expects 1 argument"));
            }
            match &args[0] {
                Value::Int(value) => Ok(Value::Int(value - 1)),
                Value::Float(value) => Ok(Value::Float(value - 1.0)),
                _ => Err(Clove2Error::new("vm: dec expects number")),
            }
        }
        "even?" | "odd?" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: even?/odd? expects 1 argument"));
            }
            let Value::Int(value) = args[0] else {
                return Err(Clove2Error::new("vm: even?/odd? expects int"));
            };
            let odd = name == "odd?";
            Ok(Value::Bool((value % 2 != 0) == odd))
        }
        "identity" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: identity expects 1 argument"));
            }
            Ok(args[0].clone())
        }
        "count" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: count expects 1 argument"));
            }
            match &args[0] {
                Value::Vec(items) => Ok(Value::Int(items.len() as i64)),
                Value::Map(map) => Ok(Value::Int(map.len() as i64)),
                Value::Str(value) => Ok(Value::Int(value.chars().count() as i64)),
                _ => Err(Clove2Error::new("vm: count expects collection")),
            }
        }
        "range" => {
            let (start, end) = match args.as_slice() {
                [Value::Int(end)] => (0, *end),
                [Value::Int(start), Value::Int(end)] => (*start, *end),
                _ => return Err(Clove2Error::new("vm: range expects int arguments")),
            };
            let mut out = Vec::new();
            let mut cur = start;
            while cur < end {
                out.push(Value::Int(cur));
                cur += 1;
            }
            Ok(Value::vec(out))
        }
        "contains?" => {
            if args.len() != 2 {
                return Err(Clove2Error::new("vm: contains? expects 2 arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::Vec(items), Value::Int(idx)) => {
                    let idx = *idx as isize;
                    let ok = idx >= 0 && (idx as usize) < items.len();
                    Ok(Value::Bool(ok))
                }
                (Value::Map(map), key) => {
                    let key = value_to_key(key)?;
                    Ok(Value::Bool(map.contains_key(&key)))
                }
                _ => Err(Clove2Error::new("vm: contains? expects vector/map")),
            }
        }
        "map" => vm_map(state, args),
        "filter" => vm_filter(state, args),
        "reduce" => vm_reduce(state, args),
        "some" => vm_some(state, args),
        "every?" => vm_every(state, args, false),
        "not-every?" => vm_every(state, args, true),
        "remove" => vm_remove(state, args),
        "drop-while" => vm_drop_while(state, args),
        "keep" => vm_keep(state, args),
        "keep-indexed" => vm_keep_indexed(state, args),
        "rest" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: rest expects 1 argument"));
            }
            match &args[0] {
                Value::Vec(items) => {
                    if items.is_empty() {
                        Ok(Value::vec(Vec::new()))
                    } else {
                        Ok(Value::vec(items[1..].to_vec()))
                    }
                }
                _ => Err(Clove2Error::new("vm: rest expects vector")),
            }
        }
        "empty?" => {
            if args.len() != 1 {
                return Err(Clove2Error::new("vm: empty? expects 1 argument"));
            }
            match &args[0] {
                Value::Vec(items) => Ok(Value::Bool(items.is_empty())),
                Value::Map(map) => Ok(Value::Bool(map.is_empty())),
                Value::Str(value) => Ok(Value::Bool(value.is_empty())),
                _ => Err(Clove2Error::new("vm: empty? expects collection")),
            }
        }
        "hash-map" => {
            if args.len() % 2 != 0 {
                return Err(Clove2Error::new(
                    "vm: hash-map expects even number of arguments",
                ));
            }
            let mut map = BTreeMap::new();
            let mut iter = args.into_iter();
            while let Some(key) = iter.next() {
                let value = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("vm: hash-map expects value"))?;
                let key = value_to_key(&key)?;
                map.insert(key, value);
            }
            Ok(Value::map(map))
        }
        "conj" => {
            if args.len() < 2 {
                return Err(Clove2Error::new("vm: conj expects 2+ arguments"));
            }
            match &args[0] {
                Value::Vec(items) => {
                    let mut out = items.to_vec();
                    for arg in args.iter().skip(1) {
                        out.push(arg.clone());
                    }
                    Ok(Value::vec(out))
                }
                _ => apply_builtin_value(name, args, state.mut_mode),
            }
        }
        "nth" => {
            if args.len() < 2 || args.len() > 3 {
                return Err(Clove2Error::new("vm: nth expects 2 or 3 arguments"));
            }
            let Value::Vec(items) = &args[0] else {
                return Err(Clove2Error::new("vm: nth expects vector"));
            };
            let Value::Int(idx) = args[1] else {
                return Err(Clove2Error::new("vm: nth expects int index"));
            };
            let idx = idx as isize;
            if idx < 0 || (idx as usize) >= items.len() {
                if args.len() == 3 {
                    return Ok(args[2].clone());
                }
                return Err(Clove2Error::new("vm: nth out of range"));
            }
            Ok(items[idx as usize].clone())
        }
        "get" => vm_get(args),
        "assoc" => vm_assoc(args),
        "get-in" => vm_get_in(args),
        "assoc-in" => vm_assoc_in(args),
        "update-in" => vm_update_in(state, args),
        "interleave" => vm_interleave(args),
        "sort" => vm_sort(args),
        "sort-by" => vm_sort_by(state, args),
        "split" => vm_split(args),
        "join" => vm_join(args),
        _ => apply_builtin_value(name, args, state.mut_mode),
    }
}

fn call_builtin_1(state: &mut VmState, name: &str, arg: Value) -> Result<Value, Clove2Error> {
    match name {
        "inc" => match arg {
            Value::Int(value) => Ok(Value::Int(value + 1)),
            Value::Float(value) => Ok(Value::Float(value + 1.0)),
            _ => Err(Clove2Error::new("vm: inc expects number")),
        },
        "dec" => match arg {
            Value::Int(value) => Ok(Value::Int(value - 1)),
            Value::Float(value) => Ok(Value::Float(value - 1.0)),
            _ => Err(Clove2Error::new("vm: dec expects number")),
        },
        "identity" => Ok(arg),
        "even?" | "odd?" => {
            let Value::Int(value) = arg else {
                return Err(Clove2Error::new("vm: even?/odd? expects int"));
            };
            let odd = name == "odd?";
            Ok(Value::Bool((value % 2 != 0) == odd))
        }
        "not" => Ok(Value::Bool(!is_truthy(&arg))),
        _ => call_builtin(state, name, vec![arg]),
    }
}


fn call_builtin_1_vm(
    state: &mut VmState,
    name: &str,
    arg: &VmValue,
) -> Option<Result<VmValue, Clove2Error>> {
    let result = match name {
        "inc" => match arg {
            VmValue::Int(value) => Ok(VmValue::Int(value + 1)),
            VmValue::Value(Value::Int(value)) => Ok(VmValue::Int(value + 1)),
            VmValue::Value(Value::Float(value)) => {
                Ok(VmValue::Value(Value::Float(value + 1.0)))
            }
            _ => Err(Clove2Error::new("vm: inc expects number")),
        },
        "dec" => match arg {
            VmValue::Int(value) => Ok(VmValue::Int(value - 1)),
            VmValue::Value(Value::Int(value)) => Ok(VmValue::Int(value - 1)),
            VmValue::Value(Value::Float(value)) => {
                Ok(VmValue::Value(Value::Float(value - 1.0)))
            }
            _ => Err(Clove2Error::new("vm: dec expects number")),
        },
        "identity" => Ok(arg.clone()),
        "count" => match arg {
            VmValue::Iter(iter) => {
                let mut count = 0i64;
                match iter.for_each(state, |_, _| {
                    count += 1;
                    Ok(())
                }) {
                    Ok(()) => Ok(VmValue::Int(count)),
                    Err(err) => Err(err),
                }
            }
            VmValue::Value(Value::Vec(items)) => Ok(VmValue::Int(items.len() as i64)),
            VmValue::Value(Value::Map(map)) => Ok(VmValue::Int(map.len() as i64)),
            VmValue::Value(Value::Str(value)) => Ok(VmValue::Int(value.len() as i64)),
            _ => Err(Clove2Error::new("vm: count expects collection")),
        },
        "empty?" => match arg {
            VmValue::Iter(iter) => {
                let mut any = false;
                match iter.for_each(state, |_, _| {
                    any = true;
                    Ok(())
                }) {
                    Ok(()) => Ok(VmValue::Bool(!any)),
                    Err(err) => Err(err),
                }
            }
            VmValue::Value(Value::Vec(items)) => Ok(VmValue::Bool(items.is_empty())),
            VmValue::Value(Value::Map(map)) => Ok(VmValue::Bool(map.is_empty())),
            VmValue::Value(Value::Str(value)) => Ok(VmValue::Bool(value.is_empty())),
            _ => Err(Clove2Error::new("vm: empty? expects collection")),
        },
        "rest" => match arg {
            VmValue::Value(Value::Vec(items)) => {
                if items.is_empty() {
                    Ok(VmValue::Value(Value::vec(Vec::new())))
                } else {
                    Ok(VmValue::Value(Value::vec(items[1..].to_vec())))
                }
            }
            _ => Err(Clove2Error::new("vm: rest expects vector")),
        },
        "even?" | "odd?" => match arg {
            VmValue::Int(value) => {
                let odd = name == "odd?";
                Ok(VmValue::Bool((value % 2 != 0) == odd))
            }
            VmValue::Value(Value::Int(value)) => {
                let odd = name == "odd?";
                Ok(VmValue::Bool((value % 2 != 0) == odd))
            }
            _ => Err(Clove2Error::new("vm: even?/odd? expects int")),
        },
        "not" => is_truthy_vm(state, arg).map(|value| VmValue::Bool(!value)),
        _ => return None,
    };
    Some(result)
}

fn call_builtin_2(
    state: &mut VmState,
    name: &str,
    arg1: Value,
    arg2: Value,
) -> Result<Value, Clove2Error> {
    match name {
        "+" => match (arg1, arg2) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
            _ => Err(Clove2Error::new("vm: + expects numbers")),
        },
        "-" => match (arg1, arg2) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
            _ => Err(Clove2Error::new("vm: - expects numbers")),
        },
        "*" => match (arg1, arg2) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
            _ => Err(Clove2Error::new("vm: * expects numbers")),
        },
        "/" => match (arg1, arg2) {
            (Value::Int(_), Value::Int(0)) | (Value::Float(_), Value::Float(0.0)) => {
                Err(Clove2Error::new("vm: division by zero"))
            }
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / b as f64)),
            _ => Err(Clove2Error::new("vm: / expects numbers")),
        },
        "mod" => match (arg1, arg2) {
            (Value::Int(_), Value::Int(0)) => Err(Clove2Error::new("vm: division by zero")),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
            _ => Err(Clove2Error::new("vm: mod expects int")),
        },
        _ => call_builtin(state, name, vec![arg1, arg2]),
    }
}


fn call_builtin_2_vm(
    _state: &mut VmState,
    name: &str,
    arg1: &VmValue,
    arg2: &VmValue,
) -> Option<Result<VmValue, Clove2Error>> {
    let result = match name {
        "+" => match (arg1, arg2) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a + b)),
            (VmValue::Int(a), VmValue::Value(Value::Int(b))) => Ok(VmValue::Int(a + b)),
            (VmValue::Value(Value::Int(a)), VmValue::Int(b)) => Ok(VmValue::Int(a + b)),
            (VmValue::Value(Value::Int(a)), VmValue::Value(Value::Int(b))) => {
                Ok(VmValue::Int(a + b))
            }
            _ => return None,
        },
        "-" => match (arg1, arg2) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a - b)),
            (VmValue::Int(a), VmValue::Value(Value::Int(b))) => Ok(VmValue::Int(a - b)),
            (VmValue::Value(Value::Int(a)), VmValue::Int(b)) => Ok(VmValue::Int(a - b)),
            (VmValue::Value(Value::Int(a)), VmValue::Value(Value::Int(b))) => {
                Ok(VmValue::Int(a - b))
            }
            _ => return None,
        },
        "*" => match (arg1, arg2) {
            (VmValue::Int(a), VmValue::Int(b)) => Ok(VmValue::Int(a * b)),
            (VmValue::Int(a), VmValue::Value(Value::Int(b))) => Ok(VmValue::Int(a * b)),
            (VmValue::Value(Value::Int(a)), VmValue::Int(b)) => Ok(VmValue::Int(a * b)),
            (VmValue::Value(Value::Int(a)), VmValue::Value(Value::Int(b))) => {
                Ok(VmValue::Int(a * b))
            }
            _ => return None,
        },
        "conj" => match arg1 {
            VmValue::Value(Value::Vec(items)) => {
                let mut out = items.to_vec();
                let value = match arg2 {
                    VmValue::Int(value) => Value::Int(*value),
                    VmValue::Value(value) => value.clone(),
                    _ => return None,
                };
                out.push(value);
                Ok(VmValue::Value(Value::vec(out)))
            }
            _ => return None,
        },
        "nth" => match (arg1, arg2) {
            (VmValue::Value(Value::Vec(items)), VmValue::Int(idx)) => {
                let idx = *idx as isize;
                if idx < 0 || (idx as usize) >= items.len() {
                    return Some(Err(Clove2Error::new("vm: nth out of range")));
                }
                Ok(VmValue::Value(items[idx as usize].clone()))
            }
            (VmValue::Value(Value::Vec(items)), VmValue::Value(Value::Int(idx))) => {
                let idx = *idx as isize;
                if idx < 0 || (idx as usize) >= items.len() {
                    return Some(Err(Clove2Error::new("vm: nth out of range")));
                }
                Ok(VmValue::Value(items[idx as usize].clone()))
            }
            _ => return None,
        },
        "interleave" => match (arg1, arg2) {
            (VmValue::Value(Value::Vec(left)), VmValue::Value(Value::Vec(right))) => {
                let len = left.len().min(right.len());
                let mut out = Vec::with_capacity(len * 2);
                for (l, r) in left.iter().zip(right.iter()).take(len) {
                    out.push(l.clone());
                    out.push(r.clone());
                }
                Ok(VmValue::Value(Value::vec(out)))
            }
            _ => return None,
        },
        _ => return None,
    };
    Some(result)
}

fn vm_map(state: &mut VmState, mut args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: map expects 2 arguments"));
    }
    let coll = args.pop().unwrap();
    let func = args.pop().unwrap();
    let Value::Vec(items) = coll else {
        return Err(Clove2Error::new("vm: map expects vector"));
    };
    let mut out = Vec::with_capacity(items.len());
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    out.push(value);
                }
                return Ok(Value::vec(out));
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        out.push(value);
    }
    Ok(Value::vec(out))
}

fn vm_filter(state: &mut VmState, mut args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: filter expects 2 arguments"));
    }
    let coll = args.pop().unwrap();
    let func = args.pop().unwrap();
    let Value::Vec(items) = coll else {
        return Err(Clove2Error::new("vm: filter expects vector"));
    };
    let mut out = Vec::new();
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if is_truthy(&value) {
                        out.push(item.clone());
                    }
                }
                return Ok(Value::vec(out));
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        if is_truthy(&value) {
            out.push(item.clone());
        }
    }
    Ok(Value::vec(out))
}

fn vm_reduce(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    match args.as_slice() {
        [func, Value::Vec(items)] => {
            if items.is_empty() {
                return Err(Clove2Error::new("vm: reduce on empty collection"));
            }
            if let Value::NativeFunction(id) = func {
                if let Some(func_def) = state.functions.get(*id).cloned() {
                    if func_def.params.len() == 2 {
                        let mut frame = prepare_reuse_frame(&func_def)?;
                        let mut acc = items[0].clone();
                        for item in items.iter().skip(1) {
                            frame.stack.clear();
                            frame.set_local_slot(0, value_to_vmvalue(acc))?;
                            frame.set_local_slot(1, value_to_vmvalue(item.clone()))?;
                            acc = run_code(&func_def.code, state, &mut frame)?;
                        }
                        return Ok(acc);
                    }
                }
            }
            let mut acc = items[0].clone();
            for item in items.iter().skip(1) {
                acc = call_value_arity2(state, func, acc, item.clone())?;
            }
            Ok(acc)
        }
        [func, init, Value::Vec(items)] => {
            if let Value::NativeFunction(id) = func {
                if let Some(func_def) = state.functions.get(*id).cloned() {
                    if func_def.params.len() == 2 {
                        let mut frame = prepare_reuse_frame(&func_def)?;
                        let mut acc = init.clone();
                        for item in items.iter() {
                            frame.stack.clear();
                            frame.set_local_slot(0, value_to_vmvalue(acc))?;
                            frame.set_local_slot(1, value_to_vmvalue(item.clone()))?;
                            acc = run_code(&func_def.code, state, &mut frame)?;
                        }
                        return Ok(acc);
                    }
                }
            }
            let mut acc = init.clone();
            for item in items.iter() {
                acc = call_value_arity2(state, func, acc, item.clone())?;
            }
            Ok(acc)
        }
        _ => Err(Clove2Error::new(
            "vm: reduce expects (f coll) or (f init coll)",
        )),
    }
}

fn vm_some(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: some expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: some expects vector"));
    };
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if is_truthy(&value) {
                        return Ok(value);
                    }
                }
                return Ok(Value::Nil);
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }
    Ok(Value::Nil)
}

fn vm_every(state: &mut VmState, args: Vec<Value>, negated: bool) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: every? expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: every? expects vector"));
    };
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    let ok = is_truthy(&value);
                    if negated {
                        if !ok {
                            return Ok(Value::Bool(true));
                        }
                    } else if !ok {
                        return Ok(Value::Bool(false));
                    }
                }
                return Ok(Value::Bool(!negated));
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        let ok = is_truthy(&value);
        if negated {
            if !ok {
                return Ok(Value::Bool(true));
            }
        } else if !ok {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(!negated))
}

fn vm_remove(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: remove expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: remove expects vector"));
    };
    let mut out = Vec::new();
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if !is_truthy(&value) {
                        out.push(item.clone());
                    }
                }
                return Ok(Value::vec(out));
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        if !is_truthy(&value) {
            out.push(item.clone());
        }
    }
    Ok(Value::vec(out))
}

fn vm_drop_while(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: drop-while expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: drop-while expects vector"));
    };
    let mut idx = 0usize;
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                while idx < items.len() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(items[idx].clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if !is_truthy(&value) {
                        break;
                    }
                    idx += 1;
                }
                return Ok(Value::vec(items[idx..].to_vec()));
            }
        }
    }
    while idx < items.len() {
        let value = call_value_arity1(state, &func, items[idx].clone())?;
        if !is_truthy(&value) {
            break;
        }
        idx += 1;
    }
    Ok(Value::vec(items[idx..].to_vec()))
}

fn vm_keep(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: keep expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: keep expects vector"));
    };
    let mut out = Vec::new();
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 1 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for item in items.iter() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if !matches!(value, Value::Nil) {
                        out.push(value);
                    }
                }
                return Ok(Value::vec(out));
            }
        }
    }
    for item in items.iter() {
        let value = call_value_arity1(state, &func, item.clone())?;
        if !matches!(value, Value::Nil) {
            out.push(value);
        }
    }
    Ok(Value::vec(out))
}

fn vm_keep_indexed(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: keep-indexed expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: keep-indexed expects vector"));
    };
    let mut out = Vec::new();
    if let Value::NativeFunction(id) = &func {
        if let Some(func_def) = state.functions.get(*id).cloned() {
            if func_def.params.len() == 2 {
                let mut frame = prepare_reuse_frame(&func_def)?;
                for (idx, item) in items.iter().enumerate() {
                    frame.stack.clear();
                    frame.set_local_slot(0, value_to_vmvalue(Value::Int(idx as i64)))?;
                    frame.set_local_slot(1, value_to_vmvalue(item.clone()))?;
                    let value = run_code(&func_def.code, state, &mut frame)?;
                    if !matches!(value, Value::Nil) {
                        out.push(value);
                    }
                }
                return Ok(Value::vec(out));
            }
        }
    }
    for (idx, item) in items.iter().enumerate() {
        let value = call_value_arity2(state, &func, Value::Int(idx as i64), item.clone())?;
        if !matches!(value, Value::Nil) {
            out.push(value);
        }
    }
    Ok(Value::vec(out))
}

#[derive(Clone, Copy, PartialEq)]
enum SortKind {
    Number,
    Str,
    Keyword,
    Bool,
}

fn sort_kind_of(value: &Value) -> Option<SortKind> {
    match value {
        Value::Int(_) | Value::Float(_) => Some(SortKind::Number),
        Value::Str(_) => Some(SortKind::Str),
        Value::Keyword(_) => Some(SortKind::Keyword),
        Value::Bool(_) => Some(SortKind::Bool),
        _ => None,
    }
}

fn compare_by_kind(kind: SortKind, left: &Value, right: &Value) -> std::cmp::Ordering {
    match kind {
        SortKind::Number => {
            let l = match left {
                Value::Int(v) => *v as f64,
                Value::Float(v) => *v,
                _ => 0.0,
            };
            let r = match right {
                Value::Int(v) => *v as f64,
                Value::Float(v) => *v,
                _ => 0.0,
            };
            l.partial_cmp(&r).unwrap_or(std::cmp::Ordering::Equal)
        }
        SortKind::Str => match (left, right) {
            (Value::Str(a), Value::Str(b)) => a.cmp(b),
            _ => std::cmp::Ordering::Equal,
        },
        SortKind::Keyword => match (left, right) {
            (Value::Keyword(a), Value::Keyword(b)) => a.cmp(b),
            _ => std::cmp::Ordering::Equal,
        },
        SortKind::Bool => match (left, right) {
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            _ => std::cmp::Ordering::Equal,
        },
    }
}

fn vm_sort(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 1 {
        return Err(Clove2Error::new("vm: sort expects 1 argument"));
    }
    let Value::Vec(items) = &args[0] else {
        return Err(Clove2Error::new("vm: sort expects vector"));
    };
    if items.is_empty() {
        return Ok(Value::vec(Vec::new()));
    }
    let kind =
        sort_kind_of(&items[0]).ok_or_else(|| Clove2Error::new("vm: sort unsupported type"))?;
    for item in items.iter().skip(1) {
        if sort_kind_of(item) != Some(kind) {
            return Err(Clove2Error::new("vm: sort expects uniform comparable type"));
        }
    }
    let mut out = items.to_vec();
    out.sort_by(|a, b| compare_by_kind(kind, a, b));
    Ok(Value::vec(out))
}

fn vm_sort_by(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: sort-by expects 2 arguments"));
    }
    let func = args[0].clone();
    let Value::Vec(items) = &args[1] else {
        return Err(Clove2Error::new("vm: sort-by expects vector"));
    };
    let mut keyed = Vec::with_capacity(items.len());
    let mut native_cache: Vec<Option<NativeCacheEntry>> = vec![None];
    for item in items.iter() {
        let key = call_value_arity1_cached(state, &func, item.clone(), &mut native_cache, 0)?;
        keyed.push((key, item.clone()));
    }
    if keyed.is_empty() {
        return Ok(Value::vec(Vec::new()));
    }
    let kind = sort_kind_of(&keyed[0].0)
        .ok_or_else(|| Clove2Error::new("vm: sort-by unsupported key type"))?;
    for (key, _) in keyed.iter().skip(1) {
        if sort_kind_of(key) != Some(kind) {
            return Err(Clove2Error::new(
                "vm: sort-by expects uniform comparable keys",
            ));
        }
    }
    keyed.sort_by(|a, b| compare_by_kind(kind, &a.0, &b.0));
    let out = keyed.into_iter().map(|(_, v)| v).collect::<Vec<_>>();
    Ok(Value::vec(out))
}

fn vm_interleave(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: interleave expects 2 arguments"));
    }
    let mut iter = args.into_iter();
    let left = iter.next().unwrap();
    let right = iter.next().unwrap();
    if let (Value::Str(left), Value::Str(right)) = (&left, &right) {
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
    let Value::Vec(left_items) = left else {
        return Err(Clove2Error::new("vm: interleave expects vector or string"));
    };
    let Value::Vec(right_items) = right else {
        return Err(Clove2Error::new("vm: interleave expects vector or string"));
    };
    let len = left_items.len().min(right_items.len());
    let mut out = Vec::with_capacity(len * 2);
    for (l, r) in left_items.iter().zip(right_items.iter()).take(len) {
        out.push(l.clone());
        out.push(r.clone());
    }
    Ok(Value::vec(out))
}

fn vm_split(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 && args.len() != 3 {
        return Err(Clove2Error::new("vm: split expects 2 or 3 arguments"));
    }
    let text = &args[0];
    let sep = &args[1];
    let Value::Str(text) = text else {
        return Err(Clove2Error::new("vm: split expects string"));
    };
    let limit = if args.len() == 3 {
        let Value::Int(limit) = args[2] else {
            return Err(Clove2Error::new("vm: split expects Int limit"));
        };
        if limit > 0 {
            Some(limit as usize)
        } else {
            None
        }
    } else {
        None
    };
    let items = match sep {
        Value::Str(sep) => {
            if sep.is_empty() {
                text.chars().map(|ch| Value::Str(ch.to_string())).collect()
            } else if let Some(limit) = limit {
                text.splitn(limit, sep)
                    .map(|item| Value::Str(item.to_string()))
                    .collect()
            } else {
                text.split(sep)
                    .map(|item| Value::Str(item.to_string()))
                    .collect()
            }
        }
        Value::Regex(pattern) => {
            let regex = Regex::new(pattern)
                .map_err(|err| Clove2Error::new(format!("vm: split expects regex: {}", err)))?;
            if let Some(limit) = limit {
                regex
                    .splitn(text, limit)
                    .map(|item| Value::Str(item.to_string()))
                    .collect()
            } else {
                regex
                    .split(text)
                    .map(|item| Value::Str(item.to_string()))
                    .collect()
            }
        }
        _ => return Err(Clove2Error::new("vm: split expects string or regex")),
    };
    Ok(Value::vec(items))
}

fn vm_join(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 1 && args.len() != 2 {
        return Err(Clove2Error::new("vm: join expects 1 or 2 arguments"));
    }
    let (coll, sep) = if args.len() == 2 {
        let Value::Str(sep) = &args[1] else {
            return Err(Clove2Error::new("vm: join expects string separator"));
        };
        (&args[0], sep.clone())
    } else {
        (&args[0], String::new())
    };
    let Value::Vec(items) = coll else {
        return Err(Clove2Error::new("vm: join expects vector of strings"));
    };
    let mut out = String::new();
    for (idx, item) in items.iter().enumerate() {
        let Value::Str(item) = item else {
            return Err(Clove2Error::new("vm: join expects vector of strings"));
        };
        if idx > 0 {
            out.push_str(&sep);
        }
        out.push_str(item);
    }
    Ok(Value::Str(out))
}

fn vm_get(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() < 2 || args.len() > 3 {
        return Err(Clove2Error::new("vm: get expects 2 or 3 arguments"));
    }
    match &args[0] {
        Value::Map(map) => {
            let key = value_to_key(&args[1])?;
            Ok(map.get(&key).cloned().unwrap_or_else(|| {
                if args.len() == 3 {
                    args[2].clone()
                } else {
                    Value::Nil
                }
            }))
        }
        Value::Vec(items) => {
            let Value::Int(idx) = args[1] else {
                return Err(Clove2Error::new("vm: get expects int index"));
            };
            let idx = idx as isize;
            if idx < 0 || (idx as usize) >= items.len() {
                if args.len() == 3 {
                    Ok(args[2].clone())
                } else {
                    Ok(Value::Nil)
                }
            } else {
                Ok(items[idx as usize].clone())
            }
        }
        _ => Err(Clove2Error::new("vm: get expects map/vector")),
    }
}

fn vm_assoc(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() < 3 || args.len() % 2 == 0 {
        return Err(Clove2Error::new("vm: assoc expects key/value pairs"));
    }
    match &args[0] {
        Value::Map(map) => {
            let mut out: BTreeMap<Key, Value> = map.as_ref().clone();
            let mut iter = args.into_iter().skip(1);
            while let Some(key) = iter.next() {
                let value = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("vm: assoc expects value"))?;
                let key = value_to_key(&key)?;
                out.insert(key, value);
            }
            Ok(Value::map(out))
        }
        Value::Vec(items) => {
            let mut out = items.to_vec();
            let mut iter = args.into_iter().skip(1);
            while let Some(key) = iter.next() {
                let value = iter
                    .next()
                    .ok_or_else(|| Clove2Error::new("vm: assoc expects value"))?;
                let Value::Int(idx) = key else {
                    return Err(Clove2Error::new("vm: assoc expects int index for vector"));
                };
                let idx = idx as isize;
                if idx < 0 || (idx as usize) >= out.len() {
                    return Err(Clove2Error::new("vm: assoc index out of range"));
                }
                out[idx as usize] = value;
            }
            Ok(Value::vec(out))
        }
        _ => Err(Clove2Error::new("vm: assoc expects map/vector")),
    }
}

fn vm_get_in(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 2 {
        return Err(Clove2Error::new("vm: get-in expects 2 arguments"));
    }
    let mut cur = args[0].clone();
    let Value::Vec(path) = &args[1] else {
        return Err(Clove2Error::new("vm: get-in expects vector path"));
    };
    for key in path.iter() {
        cur = vm_get(vec![cur, key.clone()])?;
        if matches!(cur, Value::Nil) {
            break;
        }
    }
    Ok(cur)
}

fn vm_assoc_in(args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() != 3 {
        return Err(Clove2Error::new("vm: assoc-in expects 3 arguments"));
    }
    let Value::Vec(path) = &args[1] else {
        return Err(Clove2Error::new("vm: assoc-in expects vector path"));
    };
    if path.is_empty() {
        return Err(Clove2Error::new("vm: assoc-in expects non-empty path"));
    }
    let mut cur = args[0].clone();
    let mut stack = Vec::new();
    for key in path.iter().take(path.len() - 1) {
        stack.push((cur.clone(), key.clone()));
        cur = vm_get(vec![cur, key.clone()])?;
        if matches!(cur, Value::Nil) {
            cur = Value::map(BTreeMap::new());
        }
    }
    let last_key = path.last().unwrap().clone();
    cur = vm_assoc(vec![cur, last_key, args[2].clone()])?;
    while let Some((container, key)) = stack.pop() {
        cur = vm_assoc(vec![container, key, cur])?;
    }
    Ok(cur)
}

fn vm_update_in(state: &mut VmState, args: Vec<Value>) -> Result<Value, Clove2Error> {
    if args.len() < 3 {
        return Err(Clove2Error::new("vm: update-in expects 3+ arguments"));
    }
    let target = args[0].clone();
    let path = args[1].clone();
    let func = args[2].clone();
    let mut rest = args[3..].to_vec();
    let current = vm_get_in(vec![target.clone(), path.clone()])?;
    let updated = if rest.is_empty() {
        call_value_arity1(state, &func, current)?
    } else if rest.len() == 1 {
        call_value_arity2(state, &func, current, rest.remove(0))?
    } else {
        let mut call_args = vec![current];
        call_args.append(&mut rest);
        call_value(state, func, call_args)?
    };
    vm_assoc_in(vec![target, path, updated])
}

pub fn run_program_vm(items: &[TopLevel], mut_mode: MutMode) -> Result<Value, Clove2Error> {
    let program = compile_program(items)?;
    let mut state = VmState::new(program.functions, program.global_names, mut_mode);
    let mut frame = VmFrame::new(program.local_count);
    run_code(&program.code, &mut state, &mut frame)
}
