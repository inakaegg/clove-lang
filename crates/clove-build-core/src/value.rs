use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Key {
    Str(Rc<str>),
    Keyword(Rc<str>),
    Symbol(Rc<str>),
    Bool(bool),
    Int(i64),
}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        use Key::*;
        let rank = |key: &Key| match key {
            Str(_) => 0,
            Keyword(_) => 1,
            Symbol(_) => 2,
            Bool(_) => 3,
            Int(_) => 4,
        };
        let left_rank = rank(self);
        let right_rank = rank(other);
        if left_rank != right_rank {
            return left_rank.cmp(&right_rank);
        }
        match (self, other) {
            (Str(a), Str(b)) => a.as_ref().cmp(b.as_ref()),
            (Keyword(a), Keyword(b)) => a.as_ref().cmp(b.as_ref()),
            (Symbol(a), Symbol(b)) => a.as_ref().cmp(b.as_ref()),
            (Bool(a), Bool(b)) => b.cmp(a),
            (Int(a), Int(b)) => a.cmp(b),
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PartialValue {
    pub func: Value,
    pub args: Vec<Value>,
    pub desc: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Regex(String),
    Keyword(String),
    Symbol(String),
    Vec(Rc<Vec<Value>>),
    List(Vec<Value>),
    Set(Vec<Value>),
    Map(Rc<BTreeMap<Key, Value>>),
    Function(usize),
    NativeFunction(usize),
    Builtin(String),
    Partial(Box<PartialValue>),
}

impl Value {
    pub fn vec(items: Vec<Value>) -> Self {
        Value::Vec(Rc::new(items))
    }

    pub fn list(items: Vec<Value>) -> Self {
        Value::List(items)
    }

    pub fn set(items: Vec<Value>) -> Self {
        Value::Set(items)
    }

    pub fn map(items: BTreeMap<Key, Value>) -> Self {
        Value::Map(Rc::new(items))
    }
}
