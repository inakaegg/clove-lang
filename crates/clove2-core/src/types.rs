use std::collections::BTreeMap;
use std::fmt;

use crate::error::Clove2Error;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ShapeType {
    pub fields: BTreeMap<String, Type>,
    pub open: bool,
}

impl ShapeType {
    pub fn new(fields: BTreeMap<String, Type>, open: bool) -> Self {
        Self { fields, open }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Nil,
    Bool,
    Int,
    Float,
    Number,
    Str,
    Keyword,
    Symbol,
    Vec(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Object(BTreeMap<String, Type>),
    Tuple(Vec<Type>),
    Shape(ShapeType),
    Union(Vec<Type>),
    Dyn,
    DynOf(Box<Type>),
    Named(String),
    Function {
        params: Vec<Type>,
        rest: Option<Box<Type>>,
        ret: Box<Type>,
    },
}

impl Type {
    pub fn is_optional(&self) -> bool {
        matches!(self, Type::Union(items) if items.iter().any(|t| *t == Type::Nil))
    }

    pub fn union(items: Vec<Type>) -> Type {
        union_types(items)
    }

    pub fn union_two(a: Type, b: Type) -> Type {
        union_types(vec![a, b])
    }

    pub fn shape(fields: BTreeMap<String, Type>) -> Type {
        Type::Shape(ShapeType::new(fields, false))
    }

    pub fn open_shape(fields: BTreeMap<String, Type>) -> Type {
        Type::Shape(ShapeType::new(fields, true))
    }

    pub fn parse(input: &str) -> Result<Self, Clove2Error> {
        let mut parser = TypeParser::new(input);
        let ty = parser.parse_type()?;
        parser.skip_ws();
        if !parser.is_end() {
            return Err(parser.err("unexpected trailing input"));
        }
        Ok(ty)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "Any"),
            Type::Nil => write!(f, "Nil"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Number => write!(f, "Number"),
            Type::Str => write!(f, "Str"),
            Type::Keyword => write!(f, "Keyword"),
            Type::Symbol => write!(f, "Symbol"),
            Type::Vec(inner) => write!(f, "Vec<{}>", inner),
            Type::Map(key, value) => write!(f, "Map<{}, {}>", key, value),
            Type::Object(fields) => {
                write!(f, "Object{{")?;
                let mut first = true;
                for (key, ty) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}: {}", key, ty)?;
                }
                write!(f, "}}")
            }
            Type::Tuple(items) => {
                let mut first = true;
                write!(f, "[")?;
                for item in items {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Type::Shape(shape) => {
                let mut first = true;
                write!(f, "{{")?;
                for (key, ty) in &shape.fields {
                    if !first {
                        write!(f, " ")?;
                    }
                    first = false;
                    write!(f, ":{} {}", key, ty)?;
                }
                if shape.open {
                    if !shape.fields.is_empty() {
                        write!(f, " ")?;
                    }
                    write!(f, "..")?;
                }
                write!(f, "}}")
            }
            Type::Union(items) => {
                let mut first = true;
                for item in items {
                    if !first {
                        write!(f, "|")?;
                    }
                    first = false;
                    write!(f, "{}", item)?;
                }
                Ok(())
            }
            Type::Dyn => write!(f, "Dyn"),
            Type::DynOf(inner) => write!(f, "Dyn<{}>", inner),
            Type::Named(name) => write!(f, "{}", name),
            Type::Function { params, rest, ret } => {
                let mut parts: Vec<String> = params.iter().map(|t| t.to_string()).collect();
                if let Some(rest_ty) = rest {
                    parts.push("&".to_string());
                    parts.push(rest_ty.to_string());
                }
                write!(f, "[{}] -> {}", parts.join(" "), ret)
            }
        }
    }
}

struct TypeParser {
    chars: Vec<char>,
    pos: usize,
}

impl TypeParser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn parse_type(&mut self) -> Result<Type, Clove2Error> {
        self.parse_union()
    }

    fn parse_union(&mut self) -> Result<Type, Clove2Error> {
        let mut items = vec![self.parse_primary()?];
        loop {
            self.skip_ws();
            if !self.consume_char('|') {
                break;
            }
            items.push(self.parse_primary()?);
        }
        Ok(union_types(items))
    }

    fn parse_primary(&mut self) -> Result<Type, Clove2Error> {
        self.skip_ws();
        let (mut ty, ident) = if self.consume_char('[') {
            (self.parse_function()?, None)
        } else {
            let ident = self.parse_ident()?;
            let ty = match ident.as_str() {
                "Any" => Type::Any,
                "Nil" => Type::Nil,
                "Bool" => Type::Bool,
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Number" => Type::Number,
                "Str" => Type::Str,
                "Keyword" => Type::Keyword,
                "Symbol" => Type::Symbol,
                "Key" => union_types(vec![Type::Str, Type::Keyword]),
                "Dyn" => Type::Dyn,
                "Vec" => {
                    let inner = self.parse_angle_type()?;
                    Type::Vec(Box::new(inner))
                }
                "Map" => {
                    let (key, value) = self.parse_angle_pair()?;
                    Type::Map(Box::new(key), Box::new(value))
                }
                "DynOf" => {
                    let inner = self.parse_angle_type()?;
                    Type::DynOf(Box::new(inner))
                }
                "Object" => {
                    self.skip_ws();
                    self.expect_char('{')?;
                    let fields = self.parse_object_fields()?;
                    Type::Object(fields)
                }
                other => Type::Named(other.to_string()),
            };
            (ty, Some(ident))
        };

        self.skip_ws();
        if self.consume_char('<') {
            if ident.as_deref() == Some("Dyn") {
                let inner = self.parse_union()?;
                self.skip_ws();
                self.expect_char('>')?;
                ty = Type::DynOf(Box::new(inner));
            } else {
                return Err(self.err(format!(
                    "unexpected generic for {}",
                    ident.unwrap_or_else(|| "type".to_string())
                )));
            }
        }

        self.skip_ws();
        if self.consume_char('?') {
            ty = Type::union_two(ty, Type::Nil);
        }

        Ok(ty)
    }

    fn parse_function(&mut self) -> Result<Type, Clove2Error> {
        let mut params = Vec::new();
        let mut rest = None;
        loop {
            self.skip_ws();
            if self.consume_char(']') {
                break;
            }
            if self.consume_char('&') {
                if rest.is_some() {
                    return Err(self.err("only one rest type is allowed"));
                }
                let rest_ty = self.parse_union()?;
                rest = Some(Box::new(rest_ty));
                self.skip_ws();
                self.expect_char(']')?;
                break;
            }
            let ty = self.parse_union()?;
            params.push(ty);
            self.skip_ws();
            if self.consume_char(']') {
                break;
            }
        }
        self.skip_ws();
        if !self.consume_str("->") {
            return Err(self.err("expected '->' for function type"));
        }
        let ret = self.parse_union()?;
        Ok(Type::Function {
            params,
            rest,
            ret: Box::new(ret),
        })
    }

    fn parse_angle_type(&mut self) -> Result<Type, Clove2Error> {
        self.skip_ws();
        self.expect_char('<')?;
        let ty = self.parse_union()?;
        self.skip_ws();
        self.expect_char('>')?;
        Ok(ty)
    }

    fn parse_angle_pair(&mut self) -> Result<(Type, Type), Clove2Error> {
        self.skip_ws();
        self.expect_char('<')?;
        let key = self.parse_union()?;
        self.skip_ws();
        self.expect_char(',')?;
        let value = self.parse_union()?;
        self.skip_ws();
        self.expect_char('>')?;
        Ok((key, value))
    }

    fn parse_object_fields(&mut self) -> Result<BTreeMap<String, Type>, Clove2Error> {
        let mut fields = BTreeMap::new();
        loop {
            self.skip_ws();
            if self.consume_char('}') {
                break;
            }
            let name = self.parse_ident()?;
            self.skip_ws();
            self.expect_char(':')?;
            let ty = self.parse_union()?;
            fields.insert(name, ty);
            self.skip_ws();
            if self.consume_char(',') {
                continue;
            }
            self.skip_ws();
            self.expect_char('}')?;
            break;
        }
        Ok(fields)
    }

    fn parse_ident(&mut self) -> Result<String, Clove2Error> {
        self.skip_ws();
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                self.pos += 1;
            } else {
                break;
            }
        }
        if self.pos == start {
            return Err(self.err("expected identifier"));
        }
        Ok(self.chars[start..self.pos].iter().collect())
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn consume_char(&mut self, expected: char) -> bool {
        if self.peek_char() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn consume_str(&mut self, expected: &str) -> bool {
        let start = self.pos;
        for ch in expected.chars() {
            if self.peek_char() == Some(ch) {
                self.pos += 1;
            } else {
                self.pos = start;
                return false;
            }
        }
        true
    }

    fn expect_char(&mut self, expected: char) -> Result<(), Clove2Error> {
        if self.consume_char(expected) {
            Ok(())
        } else {
            Err(self.err(format!("expected '{}'", expected)))
        }
    }

    fn is_end(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn err(&self, msg: impl Into<String>) -> Clove2Error {
        Clove2Error::new(msg.into())
    }
}

fn union_types(items: Vec<Type>) -> Type {
    let mut out = Vec::new();
    for item in items {
        match item {
            Type::Union(inner) => out.extend(inner),
            other => out.push(other),
        }
    }
    if out.iter().any(|t| matches!(t, Type::Any)) {
        return Type::Any;
    }
    if out.iter().any(|t| matches!(t, Type::Dyn)) {
        return Type::Dyn;
    }
    let mut unique: Vec<Type> = Vec::new();
    for item in out {
        if !unique.contains(&item) {
            unique.push(item);
        }
    }

    if unique.contains(&Type::Number) {
        unique.retain(|t| !matches!(t, Type::Int | Type::Float));
    } else if unique.contains(&Type::Int) && unique.contains(&Type::Float) {
        unique.retain(|t| !matches!(t, Type::Int | Type::Float));
        unique.push(Type::Number);
    }

    if unique.len() == 1 {
        unique.remove(0)
    } else {
        Type::Union(unique)
    }
}
