use std::fmt;

use crate::value::Value;

#[derive(Debug)]
pub enum Clove2ErrorKind {
    Message(String),
    Thrown(Value),
}

#[derive(Debug)]
pub struct Clove2Error {
    kind: Clove2ErrorKind,
}

impl Clove2Error {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            kind: Clove2ErrorKind::Message(message.into()),
        }
    }

    pub fn thrown(value: Value) -> Self {
        Self {
            kind: Clove2ErrorKind::Thrown(value),
        }
    }

    pub fn thrown_value(&self) -> Option<&Value> {
        match &self.kind {
            Clove2ErrorKind::Thrown(value) => Some(value),
            _ => None,
        }
    }

    pub fn message(&self) -> Option<&str> {
        match &self.kind {
            Clove2ErrorKind::Message(message) => Some(message.as_str()),
            _ => None,
        }
    }
}

impl fmt::Display for Clove2Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            Clove2ErrorKind::Message(message) => write!(f, "{}", message),
            Clove2ErrorKind::Thrown(value) => write!(f, "throw: {:?}", value),
        }
    }
}

impl std::error::Error for Clove2Error {}
