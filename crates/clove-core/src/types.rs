use std::fmt;

use im::HashMap;

use crate::error::CloveError;
use crate::type_registry::{self, FieldSchema, PrimitiveType, TypeEntry};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    Str,
    Nil,
    Any,
    Vector(Box<TypeKind>),
    Tuple(Vec<TypeKind>),
    Map(Box<TypeKind>, Box<TypeKind>),
    Set(Box<TypeKind>),
    Option(Box<TypeKind>),
    Mut(Box<TypeKind>),
    Record(HashMap<String, TypeKind>),
    Union(Vec<TypeKind>),
    Function {
        params: Vec<TypeKind>,
        rest: Option<Box<TypeKind>>,
        ret: Box<TypeKind>,
    },
    Named(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TypeHintStyle {
    Postfix,
    Return,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeHint {
    pub kind: TypeKind,
    pub from_syntax: bool,
    pub style: TypeHintStyle,
}

impl TypeHint {
    pub fn new(kind: TypeKind, from_syntax: bool) -> Self {
        Self {
            kind,
            from_syntax,
            style: TypeHintStyle::Postfix,
        }
    }

    pub fn return_hint(kind: TypeKind, from_syntax: bool) -> Self {
        Self {
            kind,
            from_syntax,
            style: TypeHintStyle::Return,
        }
    }

    pub fn with_style(mut self, style: TypeHintStyle) -> Self {
        self.style = style;
        self
    }
}

#[derive(Debug, Clone)]
pub struct TypeParseError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for TypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at position {}", self.message, self.position)
    }
}

impl std::error::Error for TypeParseError {}

impl TypeParseError {
    fn new(message: impl Into<String>, position: usize) -> Self {
        Self {
            message: message.into(),
            position,
        }
    }
}

impl TypeKind {
    pub fn any() -> Self {
        TypeKind::Any
    }

    pub fn vector(inner: TypeKind) -> Self {
        TypeKind::Vector(Box::new(inner))
    }

    pub fn option(inner: TypeKind) -> Self {
        TypeKind::Option(Box::new(inner))
    }

    pub fn map(key: TypeKind, value: TypeKind) -> Self {
        TypeKind::Map(Box::new(key), Box::new(value))
    }

    pub fn set(inner: TypeKind) -> Self {
        TypeKind::Set(Box::new(inner))
    }

    pub fn function(params: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> Self {
        TypeKind::Function {
            params,
            rest: rest.map(Box::new),
            ret: Box::new(ret),
        }
    }

    pub fn union(types: Vec<TypeKind>) -> Self {
        use std::collections::HashSet;

        let mut flat = Vec::new();
        for ty in types {
            match ty {
                TypeKind::Union(members) => {
                    flat.extend(members);
                }
                other => flat.push(other),
            }
        }
        if flat.iter().any(|ty| matches!(ty, TypeKind::Any)) {
            return TypeKind::Any;
        }
        let mut seen = HashSet::new();
        let mut uniq = Vec::new();
        for ty in flat {
            if seen.insert(ty.clone()) {
                uniq.push(ty);
            }
        }
        if uniq.len() == 1 {
            return uniq.remove(0);
        }
        uniq.sort_by(|a, b| a.describe().cmp(&b.describe()));
        TypeKind::Union(uniq)
    }

    pub fn named(name: impl Into<String>) -> Self {
        TypeKind::Named(name.into())
    }

    pub fn to_registry_name(&self) -> Option<String> {
        match self {
            TypeKind::Int => Some("clove::core::Int".into()),
            TypeKind::Float => Some("clove::core::Float".into()),
            TypeKind::Bool => Some("clove::core::Bool".into()),
            TypeKind::Str => Some("clove::core::Str".into()),
            TypeKind::Nil => Some("clove::core::Nil".into()),
            TypeKind::Any => None,
            TypeKind::Vector(_) => Some("clove::core::Vector".into()),
            TypeKind::Tuple(_) => None,
            TypeKind::Map(_, _) => Some("clove::core::Map".into()),
            TypeKind::Set(_) => Some("clove::core::Set".into()),
            TypeKind::Option(_) => None,
            TypeKind::Mut(_) => None,
            TypeKind::Record(_) => None,
            TypeKind::Union(_) => None,
            TypeKind::Function { .. } => Some("clove::core::Function".into()),
            TypeKind::Named(name) => Some(name.clone()),
        }
    }

    pub fn from_registry_name(name: &str) -> Self {
        match name {
            "Int" | "Integer" | "clove::core::Int" | "clove::core::Integer" => TypeKind::Int,
            "Float" | "clove::core::Float" => TypeKind::Float,
            "Bool" | "clove::core::Bool" => TypeKind::Bool,
            "Str" | "clove::core::Str" => TypeKind::Str,
            // Backward compatibility: treat legacy String as Str.
            "String" | "clove::core::String" => TypeKind::Str,
            "Nil" | "clove::core::Nil" => TypeKind::Nil,
            "Symbol"
            | "Sym"
            | "Keyword"
            | "Kw"
            | "clove::core::Symbol"
            | "clove::core::Keyword" => TypeKind::named("clove::core::Symbol"),
            "Regex" | "Re" | "clove::core::Regex" => TypeKind::named("clove::core::Regex"),
            "Vector" | "Vec" | "clove::core::Vector" => TypeKind::vector(TypeKind::Any),
            "Set" | "clove::core::Set" => TypeKind::set(TypeKind::Any),
            other => TypeKind::Named(other.to_string()),
        }
    }

    pub fn from_keyword(keyword: &str) -> Option<Self> {
        match keyword.trim_start_matches(':') {
            "int" => Some(TypeKind::Int),
            "float" => Some(TypeKind::Float),
            "str" | "string" => Some(TypeKind::Str),
            "bool" => Some(TypeKind::Bool),
            "nil" => Some(TypeKind::Nil),
            "any" => Some(TypeKind::Any),
            "option" => Some(TypeKind::Option(Box::new(TypeKind::Any))),
            _ => None,
        }
    }

    pub fn to_keyword(&self) -> Option<String> {
        match self {
            TypeKind::Int => Some(":int".into()),
            TypeKind::Float => Some(":float".into()),
            TypeKind::Str => Some(":str".into()),
            TypeKind::Bool => Some(":bool".into()),
            TypeKind::Nil => Some(":nil".into()),
            TypeKind::Any => Some(":any".into()),
            TypeKind::Option(_) => None,
            _ => None,
        }
    }

    pub fn from_registry_entry(name: &str) -> Option<Self> {
        let entry = type_registry::get_type_entry(name)?;
        Some(match entry {
            TypeEntry::Primitive(meta) => {
                Self::from_registry_name(&format!("{}::{}", meta.namespace, meta.name))
            }
            TypeEntry::Product(meta) => {
                let mut fields = HashMap::new();
                for field in &meta.fields {
                    let field_kind = match &field.schema {
                        FieldSchema::Primitive(prim) => Self::from_primitive_type(*prim),
                        FieldSchema::TypeRef(name) => Self::named(name.clone()),
                        FieldSchema::TypeExpr(kind) => kind.clone(),
                    };
                    fields.insert(field.name.clone(), field_kind);
                }
                TypeKind::Record(fields)
            }
            TypeEntry::Sum(meta) => TypeKind::named(format!("{}::{}", meta.namespace, meta.name)),
            TypeEntry::Alias(meta) => TypeKind::named(format!("{}::{}", meta.namespace, meta.name)),
        })
    }

    pub fn from_primitive_type(kind: PrimitiveType) -> Self {
        match kind {
            PrimitiveType::Int => TypeKind::Int,
            PrimitiveType::Float => TypeKind::Float,
            PrimitiveType::Str => TypeKind::Str,
            PrimitiveType::Bool => TypeKind::Bool,
            PrimitiveType::Nil => TypeKind::Nil,
            PrimitiveType::Vector => TypeKind::vector(TypeKind::Any),
            PrimitiveType::Map => TypeKind::map(TypeKind::Any, TypeKind::Any),
            PrimitiveType::List => TypeKind::named("clove::core::List"),
            PrimitiveType::Set => TypeKind::set(TypeKind::Any),
            PrimitiveType::Seq => TypeKind::named("clove::core::Seq"),
            PrimitiveType::Symbol => TypeKind::named("clove::core::Symbol"),
            PrimitiveType::Regex => TypeKind::named("clove::core::Regex"),
            PrimitiveType::Duration => TypeKind::named("clove::core::Duration"),
            PrimitiveType::Function => TypeKind::named("clove::core::Function"),
            PrimitiveType::Atom => TypeKind::named("clove::core::Atom"),
            PrimitiveType::Chan => TypeKind::named("clove::core::Chan"),
            PrimitiveType::Promise => TypeKind::named("clove::core::Promise"),
            PrimitiveType::Task => TypeKind::named("clove::core::Task"),
            PrimitiveType::Future => TypeKind::named("clove::core::Future"),
            PrimitiveType::Agent => TypeKind::named("clove::core::Agent"),
            PrimitiveType::Foreign => TypeKind::named("clove::core::Foreign"),
        }
    }

    pub fn parse(type_expr: &str) -> Result<Self, TypeParseError> {
        let mut parser = TypeParser::new(type_expr);
        let kind = parser.parse_type()?;
        parser.ensure_end()?;
        Ok(kind)
    }

    pub fn describe(&self) -> String {
        match self {
            TypeKind::Int => "Int".into(),
            TypeKind::Float => "Float".into(),
            TypeKind::Bool => "Bool".into(),
            TypeKind::Str => "Str".into(),
            TypeKind::Nil => "Nil".into(),
            TypeKind::Any => "Any".into(),
            TypeKind::Vector(inner) => format!("[{}]", inner.describe()),
            TypeKind::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|t| t.describe()).collect();
                format!("[{}]", parts.join(" "))
            }
            TypeKind::Map(k, v) => format!("{{{} {}}}", k.describe(), v.describe()),
            TypeKind::Set(inner) => format!("#{{{}}}", inner.describe()),
            TypeKind::Option(inner) => format!("{}?", inner.describe()),
            TypeKind::Mut(inner) => format!("Mut<{}>", inner.describe()),
            TypeKind::Record(fields) => {
                let mut parts: Vec<_> = fields
                    .iter()
                    .map(|(k, v)| format!(":{} {}", k, v.describe()))
                    .collect();
                parts.sort();
                format!("{{{}}}", parts.join(", "))
            }
            TypeKind::Union(types) => types
                .iter()
                .map(|t| t.describe())
                .collect::<Vec<_>>()
                .join("|"),
            TypeKind::Function { params, rest, ret } => {
                let mut parts: Vec<String> = params.iter().map(|t| t.describe()).collect();
                if let Some(rest_ty) = rest.as_ref() {
                    parts.push("&".into());
                    let rest_desc = match rest_ty.as_ref() {
                        TypeKind::Vector(inner) => inner.describe(),
                        other => other.describe(),
                    };
                    parts.push(rest_desc);
                }
                format!("[{}] -> {}", parts.join(" "), ret.describe())
            }
            TypeKind::Named(name) => name
                .strip_prefix("clove::core::")
                .unwrap_or(name)
                .to_string(),
        }
    }
}

impl From<TypeKind> for TypeHint {
    fn from(kind: TypeKind) -> Self {
        TypeHint::new(kind, false)
    }
}

struct TypeParser {
    chars: Vec<char>,
    pos: usize,
}

impl TypeParser {
    fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            pos: 0,
        }
    }

    fn parse_type(&mut self) -> Result<TypeKind, TypeParseError> {
        self.parse_union()
    }

    fn parse_suffixes(&mut self, mut base: TypeKind) -> Result<TypeKind, TypeParseError> {
        loop {
            self.skip_ws();
            if self.consume_str("[]") {
                base = TypeKind::vector(base);
                continue;
            }
            break;
        }
        self.skip_ws();
        if self.consume_char('?') {
            base = TypeKind::option(base);
            self.skip_ws();
            if self.consume_char('?') {
                return Err(self.err("unexpected '?'"));
            }
        }
        Ok(base)
    }

    fn parse_union(&mut self) -> Result<TypeKind, TypeParseError> {
        let mut types = vec![self.parse_postfix()?];
        loop {
            self.skip_ws();
            if !self.consume_char('|') {
                break;
            }
            types.push(self.parse_postfix()?);
        }
        if types.len() == 1 {
            Ok(types.remove(0))
        } else {
            Ok(TypeKind::union(types))
        }
    }

    fn parse_postfix(&mut self) -> Result<TypeKind, TypeParseError> {
        let base = self.parse_primary()?;
        self.parse_suffixes(base)
    }

    fn parse_primary(&mut self) -> Result<TypeKind, TypeParseError> {
        self.skip_ws();
        match self.peek_char() {
            Some('{') => self.parse_braced_type(),
            Some('[') => self.parse_bracket_type(),
            _ => self.parse_named_type(),
        }
    }

    fn parse_bracket_type(&mut self) -> Result<TypeKind, TypeParseError> {
        self.expect_char('[')?;
        self.skip_ws();
        let mut params = Vec::new();
        let mut rest = None;
        if !self.consume_char(']') {
            loop {
                self.skip_ws();
                if self.consume_char('&') {
                    if rest.is_some() {
                        return Err(self.err("unexpected '&'"));
                    }
                    let rest_ty = self.parse_type()?;
                    rest = Some(rest_ty);
                    self.skip_ws();
                    self.expect_char(']')?;
                    break;
                }
                let ty = self.parse_type()?;
                params.push(ty);
                self.skip_ws();
                if self.consume_char(']') {
                    break;
                }
                self.consume_char(',');
            }
        }
        self.skip_ws();
        if self.consume_str("->") {
            let ret = self.parse_type()?;
            return Ok(TypeKind::function(params, rest, ret));
        }
        if rest.is_some() {
            return Err(self.err("rest type is only valid for function types"));
        }
        match params.len() {
            0 => Ok(TypeKind::vector(TypeKind::Any)),
            1 => Ok(TypeKind::vector(params.remove(0))),
            _ => Ok(TypeKind::Tuple(params)),
        }
    }

    fn parse_braced_type(&mut self) -> Result<TypeKind, TypeParseError> {
        self.expect_char('{')?;
        self.skip_ws();
        if self.consume_char('}') {
            return Ok(TypeKind::Record(HashMap::new()));
        }
        if self.peek_char() == Some(':') {
            return self.parse_record_body();
        }
        let key = self.parse_type()?;
        self.skip_ws();
        self.consume_char(',');
        let value = self.parse_type()?;
        self.skip_ws();
        self.expect_char('}')?;
        Ok(TypeKind::map(key, value))
    }

    fn parse_named_type(&mut self) -> Result<TypeKind, TypeParseError> {
        self.skip_ws();
        let mut ident = self.parse_ident()?;
        let mut option_suffix = false;
        if let Some(stripped) = ident.strip_suffix('?') {
            if ident.ends_with("??") {
                return Err(self.err("unexpected '?'"));
            }
            if stripped.is_empty() {
                return Err(self.err("expected identifier"));
            }
            ident = stripped.to_string();
            option_suffix = true;
        }
        let args = if self.consume_char('<') {
            Some(self.parse_generic_args()?)
        } else {
            None
        };
        let mut base = match ident.as_str() {
            "Vector" | "Vec" => {
                if let Some(mut args) = args {
                    if args.len() != 1 {
                        return Err(self.err("Vector expects one type parameter"));
                    }
                    Ok(TypeKind::vector(args.remove(0)))
                } else {
                    Ok(TypeKind::vector(TypeKind::Any))
                }
            }
            "Map" => {
                if let Some(mut args) = args {
                    if args.len() != 2 {
                        return Err(self.err("Map expects two type parameters"));
                    }
                    let value = args.pop().unwrap();
                    let key = args.pop().unwrap();
                    Ok(TypeKind::map(key, value))
                } else {
                    Ok(TypeKind::map(TypeKind::Any, TypeKind::Any))
                }
            }
            "Set" => {
                if let Some(mut args) = args {
                    if args.len() != 1 {
                        return Err(self.err("Set expects one type parameter"));
                    }
                    Ok(TypeKind::set(args.remove(0)))
                } else {
                    Ok(TypeKind::set(TypeKind::Any))
                }
            }
            "Mut" | "mut" => {
                if let Some(mut args) = args {
                    if args.len() != 1 {
                        return Err(self.err("Mut expects one type parameter"));
                    }
                    Ok(TypeKind::Mut(Box::new(args.remove(0))))
                } else {
                    Err(self.err("Mut expects one type parameter"))
                }
            }
            other => Ok(match other {
                "Int" | "int" | "Integer" | "integer" => TypeKind::Int,
                "Float" | "float" => TypeKind::Float,
                "Bool" | "bool" => TypeKind::Bool,
                "Str" | "str" | "String" | "string" => TypeKind::Str,
                "Nil" | "nil" => TypeKind::Nil,
                "Any" | "any" => TypeKind::Any,
                "Symbol" | "symbol" | "Sym" | "sym" | "Keyword" | "keyword" | "Kw" | "kw" => {
                    TypeKind::named("clove::core::Symbol")
                }
                "Regex" | "regex" | "Re" | "re" => TypeKind::named("clove::core::Regex"),
                "Foreign" | "foreign" => TypeKind::named("clove::core::Foreign"),
                _ => {
                    if let Some(args) = args {
                        let inner = args
                            .iter()
                            .map(|t| t.describe())
                            .collect::<Vec<_>>()
                            .join(" ");
                        TypeKind::named(format!("{}<{}>", other, inner))
                    } else {
                        TypeKind::named(other)
                    }
                }
            }),
        }?;
        if option_suffix {
            base = TypeKind::option(base);
        }
        Ok(base)
    }

    fn parse_generic_args(&mut self) -> Result<Vec<TypeKind>, TypeParseError> {
        let mut args = Vec::new();
        loop {
            self.skip_ws();
            if self.consume_char('>') {
                break;
            }
            let ty = self.parse_type()?;
            args.push(ty);
            self.skip_ws();
            if self.consume_char('>') {
                break;
            }
            self.consume_char(',');
        }
        if args.is_empty() {
            return Err(self.err("expected type parameters"));
        }
        Ok(args)
    }

    fn parse_record_body(&mut self) -> Result<TypeKind, TypeParseError> {
        let mut fields = HashMap::new();
        loop {
            self.skip_ws();
            if self.consume_char('}') {
                break;
            }
            let key = self.parse_keyword()?;
            self.skip_ws();
            let value = self.parse_type()?;
            fields.insert(key, value);
            self.skip_ws();
            if self.consume_char('}') {
                break;
            }
            self.consume_char(',');
        }
        Ok(TypeKind::Record(fields))
    }

    fn parse_ident(&mut self) -> Result<String, TypeParseError> {
        self.skip_ws();
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || matches!(ch, ':' | '-' | '_' | '.' | '!' | '?') {
                self.pos += 1;
            } else {
                break;
            }
        }
        if start == self.pos {
            return Err(self.err("expected identifier"));
        }
        Ok(self.chars[start..self.pos].iter().collect())
    }

    fn parse_keyword(&mut self) -> Result<String, TypeParseError> {
        self.skip_ws();
        if !self.consume_char(':') {
            return Err(self.err("expected :keyword"));
        }
        let ident = self.parse_ident()?;
        Ok(ident)
    }

    fn expect_char(&mut self, ch: char) -> Result<(), TypeParseError> {
        self.skip_ws();
        if self.consume_char(ch) {
            Ok(())
        } else {
            Err(self.err(format!("expected '{}'", ch)))
        }
    }

    fn consume_char(&mut self, ch: char) -> bool {
        if self.peek_char() == Some(ch) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn consume_str(&mut self, s: &str) -> bool {
        let pattern: Vec<char> = s.chars().collect();
        if self.chars[self.pos..].starts_with(&pattern) {
            self.pos += pattern.len();
            true
        } else {
            false
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek_char(), Some(c) if c.is_whitespace()) {
            self.pos += 1;
        }
    }

    fn ensure_end(&mut self) -> Result<(), TypeParseError> {
        self.skip_ws();
        if self.pos == self.chars.len() {
            Ok(())
        } else {
            Err(self.err("unexpected trailing characters"))
        }
    }

    fn err(&self, message: impl Into<String>) -> TypeParseError {
        TypeParseError::new(message, self.pos)
    }
}

pub fn parse_type_hint(expr: &str) -> Result<TypeHint, CloveError> {
    let kind = TypeKind::parse(expr)
        .map_err(|err| CloveError::parse(format!("invalid type hint '{}': {}", expr, err)))?;
    Ok(TypeHint::new(kind, true))
}

#[cfg(test)]
mod tests {
    use super::{TypeKind, TypeParseError};

    #[test]
    fn parse_simple_primitive() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Int")?;
        assert_eq!(kind, TypeKind::Int);
        Ok(())
    }

    #[test]
    fn parse_vector_suffix() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Int[][]")?;
        assert_eq!(kind, TypeKind::vector(TypeKind::vector(TypeKind::Int)));
        Ok(())
    }

    #[test]
    fn parse_map_literal() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Map<Int, Str>")?;
        assert_eq!(kind, TypeKind::map(TypeKind::Int, TypeKind::Str));
        // Accept legacy String too (backward compatibility)
        let legacy = TypeKind::parse("Map<Int, String>")?;
        assert_eq!(legacy, TypeKind::map(TypeKind::Int, TypeKind::Str));
        Ok(())
    }

    #[test]
    fn parse_vector_literal_bracket() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("[Int]")?;
        assert_eq!(kind, TypeKind::vector(TypeKind::Int));
        Ok(())
    }

    #[test]
    fn parse_mut_type() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Mut<[Int]>")?;
        assert_eq!(
            kind,
            TypeKind::Mut(Box::new(TypeKind::vector(TypeKind::Int)))
        );
        assert_eq!(kind.describe(), "Mut<[Int]>");
        Ok(())
    }

    #[test]
    fn parse_tuple_literal_bracket() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("[Int Str]")?;
        assert_eq!(kind, TypeKind::Tuple(vec![TypeKind::Int, TypeKind::Str]));
        Ok(())
    }

    #[test]
    fn function_rest_vector_describes_inner_type() {
        let kind = TypeKind::function(vec![], Some(TypeKind::vector(TypeKind::Any)), TypeKind::Str);
        assert_eq!(kind.describe(), "[& Any] -> Str");
    }

    #[test]
    fn parse_map_literal_brace() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("{Int Str}")?;
        assert_eq!(kind, TypeKind::map(TypeKind::Int, TypeKind::Str));
        Ok(())
    }

    #[test]
    fn parse_record_literal() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("{:id Int :name Str}")?;
        if let TypeKind::Record(fields) = kind {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields.get("id"), Some(&TypeKind::Int));
            assert_eq!(fields.get("name"), Some(&TypeKind::Str));
        } else {
            panic!("expected record");
        }
        // Legacy name is still interpreted as Str.
        let legacy = TypeKind::parse("{:id Int :name String}")?;
        if let TypeKind::Record(fields) = legacy {
            assert_eq!(fields.get("name"), Some(&TypeKind::Str));
        } else {
            panic!("expected record");
        }
        Ok(())
    }

    #[test]
    fn parse_option_suffix() -> Result<(), TypeParseError> {
        let simple = TypeKind::parse("Int?")?;
        assert_eq!(simple, TypeKind::option(TypeKind::Int));
        assert_eq!(simple.describe(), "Int?");

        let nested = TypeKind::parse("Int[]?")?;
        assert_eq!(nested, TypeKind::option(TypeKind::vector(TypeKind::Int)));
        assert_eq!(nested.describe(), "[Int]?");
        Ok(())
    }

    #[test]
    fn parse_union() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Int|Str|Int")?;
        assert_eq!(kind, TypeKind::union(vec![TypeKind::Int, TypeKind::Str]));
        assert_eq!(kind.describe(), "Int|Str");
        Ok(())
    }

    #[test]
    fn parse_function_type() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("[Int Float & [Str]] -> Bool")?;
        assert_eq!(
            kind,
            TypeKind::function(
                vec![TypeKind::Int, TypeKind::Float],
                Some(TypeKind::vector(TypeKind::Str)),
                TypeKind::Bool
            )
        );
        assert_eq!(kind.describe(), "[Int Float & Str] -> Bool");
        Ok(())
    }

    #[test]
    fn parse_generic_named_type() -> Result<(), TypeParseError> {
        let kind = TypeKind::parse("Result<Int Str>")?;
        assert_eq!(kind, TypeKind::named("Result<Int Str>"));
        Ok(())
    }

    #[test]
    fn parse_type_aliases() -> Result<(), TypeParseError> {
        assert_eq!(TypeKind::parse("Integer")?, TypeKind::Int);
        assert_eq!(TypeKind::parse("Vec")?, TypeKind::vector(TypeKind::Any));
        assert_eq!(
            TypeKind::parse("Vec<Int>")?,
            TypeKind::vector(TypeKind::Int)
        );
        assert_eq!(
            TypeKind::parse("Sym")?,
            TypeKind::named("clove::core::Symbol")
        );
        assert_eq!(
            TypeKind::parse("Kw")?,
            TypeKind::named("clove::core::Symbol")
        );
        assert_eq!(
            TypeKind::parse("Re")?,
            TypeKind::named("clove::core::Regex")
        );
        Ok(())
    }
}
