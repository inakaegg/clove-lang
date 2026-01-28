use std::sync::Arc;

use crate::ast::{Form, Key, Span, Value};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    Inc,
    Dec,
}

impl BuiltinId {
    pub fn name(self) -> &'static str {
        match self {
            BuiltinId::Add => "+",
            BuiltinId::Sub => "-",
            BuiltinId::Mul => "*",
            BuiltinId::Div => "/",
            BuiltinId::Eq => "=",
            BuiltinId::Lt => "<",
            BuiltinId::Le => "<=",
            BuiltinId::Gt => ">",
            BuiltinId::Ge => ">=",
            BuiltinId::Inc => "inc",
            BuiltinId::Dec => "dec",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Capture {
    pub index: usize,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct FunctionPrototype {
    pub chunk: Chunk,
    pub param_count: usize,
    pub has_rest: bool,
    pub name: Option<String>,
    pub needs_env: bool,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Const(usize, Span),
    ConstI64(i64, Span),
    ConstF64(f64, Span),
    ConstBool(bool, Span),
    ConstNil(Span),
    LoadLocal(usize, Span),
    StoreLocal(usize, Span),
    LoadGlobalId(usize, Span),
    DefGlobalId(usize, bool, Span),
    DefnGlobalId(usize, bool, Form, Span),
    DefLocalForm(usize, Form, Span),
    DefLocalId(usize, Span),
    AddI64(usize, Span),
    AddF64(usize, Span),
    SubI64(usize, Span),
    SubF64(usize, Span),
    MulI64(usize, Span),
    MulF64(usize, Span),
    DivF64(usize, Span),
    EqI64(usize, Span),
    EqF64(usize, Span),
    LtI64(usize, Span),
    LtF64(usize, Span),
    LeI64(usize, Span),
    LeF64(usize, Span),
    GtI64(usize, Span),
    GtF64(usize, Span),
    GeI64(usize, Span),
    GeF64(usize, Span),
    IncI64(usize, Span),
    IncF64(usize, Span),
    DecI64(usize, Span),
    DecF64(usize, Span),
    MakeClosure(usize, Vec<Capture>, Span),
    MakeMultiClosure(Vec<usize>, Vec<Capture>, Option<String>, Span),
    AttachMeta(usize, Span),
    AttachDoc(usize, Span),
    Dup(Span),
    MakeVector(usize, Span),
    MakeSet(usize, Span),
    MakeMap(Vec<Key>, Span),
    TruncateLocals(usize, Span),
    Jump(usize, Span),
    JumpIfFalse(usize, Span),
    JumpIfFalseBool(usize, Span),
    Call0(Span),
    Call1(Span),
    Call2(Span),
    Call3(Span),
    Call4(Span),
    Call(usize, Span),
    Apply(usize, Span),
    OopIndex(Span),
    OopMethod(usize, Span),
    CallBuiltin(BuiltinId, usize, usize, Span),
    Pop(Span),
    Return(Span),
}

impl Instruction {
    pub fn span(&self) -> Span {
        match self {
            Instruction::Const(_, span)
            | Instruction::ConstI64(_, span)
            | Instruction::ConstF64(_, span)
            | Instruction::ConstBool(_, span)
            | Instruction::ConstNil(span)
            | Instruction::LoadLocal(_, span)
            | Instruction::StoreLocal(_, span)
            | Instruction::LoadGlobalId(_, span)
            | Instruction::DefGlobalId(_, _, span)
            | Instruction::DefnGlobalId(_, _, _, span)
            | Instruction::DefLocalForm(_, _, span)
            | Instruction::DefLocalId(_, span)
            | Instruction::AddI64(_, span)
            | Instruction::AddF64(_, span)
            | Instruction::SubI64(_, span)
            | Instruction::SubF64(_, span)
            | Instruction::MulI64(_, span)
            | Instruction::MulF64(_, span)
            | Instruction::DivF64(_, span)
            | Instruction::EqI64(_, span)
            | Instruction::EqF64(_, span)
            | Instruction::LtI64(_, span)
            | Instruction::LtF64(_, span)
            | Instruction::LeI64(_, span)
            | Instruction::LeF64(_, span)
            | Instruction::GtI64(_, span)
            | Instruction::GtF64(_, span)
            | Instruction::GeI64(_, span)
            | Instruction::GeF64(_, span)
            | Instruction::IncI64(_, span)
            | Instruction::IncF64(_, span)
            | Instruction::DecI64(_, span)
            | Instruction::DecF64(_, span)
            | Instruction::MakeClosure(_, _, span)
            | Instruction::MakeMultiClosure(_, _, _, span)
            | Instruction::AttachMeta(_, span)
            | Instruction::AttachDoc(_, span)
            | Instruction::Dup(span)
            | Instruction::MakeVector(_, span)
            | Instruction::MakeSet(_, span)
            | Instruction::MakeMap(_, span)
            | Instruction::TruncateLocals(_, span)
            | Instruction::Jump(_, span)
            | Instruction::JumpIfFalse(_, span)
            | Instruction::JumpIfFalseBool(_, span)
            | Instruction::Call0(span)
            | Instruction::Call1(span)
            | Instruction::Call2(span)
            | Instruction::Call3(span)
            | Instruction::Call4(span)
            | Instruction::Call(_, span)
            | Instruction::Apply(_, span)
            | Instruction::OopIndex(span)
            | Instruction::OopMethod(_, span)
            | Instruction::CallBuiltin(_, _, _, span)
            | Instruction::Pop(span)
            | Instruction::Return(span) => *span,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Chunk {
    code: Vec<Instruction>,
    constants: Vec<Value>,
    functions: Vec<Arc<FunctionPrototype>>,
    globals: Vec<String>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn code(&self) -> &[Instruction] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn functions(&self) -> &[Arc<FunctionPrototype>] {
        &self.functions
    }

    pub fn globals(&self) -> &[String] {
        &self.globals
    }

    pub fn add_const(&mut self, value: Value) -> usize {
        let idx = self.constants.len();
        self.constants.push(value);
        idx
    }

    pub fn add_function(&mut self, proto: FunctionPrototype) -> usize {
        let idx = self.functions.len();
        self.functions.push(Arc::new(proto));
        idx
    }

    pub fn add_global(&mut self, name: String) -> usize {
        let idx = self.globals.len();
        self.globals.push(name);
        idx
    }

    pub fn push(&mut self, instr: Instruction) -> usize {
        let idx = self.code.len();
        self.code.push(instr);
        idx
    }

    pub fn patch_jump(&mut self, at: usize, target: usize) -> Result<(), String> {
        match self.code.get_mut(at) {
            Some(Instruction::Jump(dst, _)) => {
                *dst = target;
                Ok(())
            }
            Some(Instruction::JumpIfFalse(dst, _)) => {
                *dst = target;
                Ok(())
            }
            Some(Instruction::JumpIfFalseBool(dst, _)) => {
                *dst = target;
                Ok(())
            }
            _ => Err(format!("invalid jump patch at {}", at)),
        }
    }
}
