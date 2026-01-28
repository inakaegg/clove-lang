#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { start: 0, end: 0 }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Regex(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Symbol(String),
    Keyword(String),
    List(Vec<Expr>),
    Vector(Vec<Expr>),
    Set(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    ForeignBlock { tag: String, code: String },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn literal(literal: Literal) -> Self {
        Self::new(ExprKind::Literal(literal), Span::default())
    }

    pub fn symbol(name: impl Into<String>) -> Self {
        Self::new(ExprKind::Symbol(name.into()), Span::default())
    }

    pub fn keyword(name: impl Into<String>) -> Self {
        Self::new(ExprKind::Keyword(name.into()), Span::default())
    }

    pub fn list(items: Vec<Expr>) -> Self {
        Self::new(ExprKind::List(items), Span::default())
    }

    pub fn vector(items: Vec<Expr>) -> Self {
        Self::new(ExprKind::Vector(items), Span::default())
    }

    pub fn set(items: Vec<Expr>) -> Self {
        Self::new(ExprKind::Set(items), Span::default())
    }

    pub fn map(entries: Vec<(Expr, Expr)>) -> Self {
        Self::new(ExprKind::Map(entries), Span::default())
    }

    pub fn foreign_block(tag: impl Into<String>, code: impl Into<String>) -> Self {
        Self::new(
            ExprKind::ForeignBlock {
                tag: tag.into(),
                code: code.into(),
            },
            Span::default(),
        )
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Expr {}
