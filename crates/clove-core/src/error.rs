use std::fmt;
use std::fs;

use crate::ast::{Span, Value};
use crate::env::EnvRef;
use thiserror::Error;

pub const ERROR_TAG: &str = "\x1b[31m[ERROR]\x1b[0m";
pub const WARN_TAG: &str = "\x1b[33m[WARN]\x1b[0m";

#[derive(Clone, Debug, Default)]
pub struct StackFrame {
    pub function: String,
    pub span: Option<Span>,
    pub file: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct ErrorContext {
    pub span: Option<Span>,
    pub stack: Vec<StackFrame>,
    pub file: Option<String>,
    pub env: Option<EnvRef>,
}

impl ErrorContext {
    fn set_span(&mut self, span: Span) {
        if self.span.is_none() {
            self.span = Some(span);
        }
    }

    fn set_stack(&mut self, stack: Vec<StackFrame>) {
        if self.stack.is_empty() && !stack.is_empty() {
            self.stack = stack;
        }
    }

    fn set_file(&mut self, file: Option<String>) {
        if self.file.is_none() {
            self.file = file;
        }
    }

    fn set_env(&mut self, env: EnvRef) {
        if self.env.is_none() {
            self.env = Some(env);
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeErrorData {
    pub message: String,
    pub context: ErrorContext,
}

impl RuntimeErrorData {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            context: ErrorContext::default(),
        }
    }
}

impl fmt::Display for RuntimeErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone, Debug)]
pub struct GuardErrorData {
    pub message: String,
    pub context: ErrorContext,
}

impl GuardErrorData {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            context: ErrorContext::default(),
        }
    }
}

impl fmt::Display for GuardErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone, Debug)]
pub struct ParseErrorData {
    pub message: String,
    pub context: ErrorContext,
}

impl ParseErrorData {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            context: ErrorContext::default(),
        }
    }
}

impl fmt::Display for ParseErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Error, Debug, Clone)]
pub enum CloveError {
    #[error("{0}")]
    UnboundSymbol(RuntimeErrorData),

    #[error("Message: {0}")]
    Message(RuntimeErrorData),

    #[error("Arity mismatch: {0}")]
    Arity(RuntimeErrorData),

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
        context: ErrorContext,
    },

    #[error("Foreign error ({tag}): {message}")]
    Foreign {
        tag: String,
        message: String,
        context: ErrorContext,
    },

    #[error("Parse error: {0}")]
    Parse(ParseErrorData),

    #[error("Thrown: {0}")]
    Thrown(Value, ErrorContext),

    #[error("Runtime error: {0}")]
    Other(RuntimeErrorData),

    #[error("{0}")]
    Guard(GuardErrorData),

    #[error("internal recur signal")]
    RecurSignal { target: usize, values: Vec<Value> },
}

impl CloveError {
    pub fn unbound_symbol(message: impl Into<String>) -> Self {
        CloveError::UnboundSymbol(RuntimeErrorData::new(message))
    }

    pub fn message(message: impl Into<String>) -> Self {
        CloveError::Message(RuntimeErrorData::new(message))
    }

    pub fn arity(message: impl Into<String>) -> Self {
        CloveError::Arity(RuntimeErrorData::new(message))
    }

    pub fn type_mismatch(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        CloveError::TypeMismatch {
            expected: expected.into(),
            actual: actual.into(),
            context: ErrorContext::default(),
        }
    }

    pub fn foreign(tag: impl Into<String>, message: impl Into<String>) -> Self {
        CloveError::Foreign {
            tag: tag.into(),
            message: message.into(),
            context: ErrorContext::default(),
        }
    }

    pub fn parse(message: impl Into<String>) -> Self {
        CloveError::Parse(ParseErrorData::new(message))
    }

    pub fn runtime(message: impl Into<String>) -> Self {
        CloveError::Other(RuntimeErrorData::new(message))
    }

    pub fn guard(message: impl Into<String>) -> Self {
        CloveError::Guard(GuardErrorData::new(message))
    }

    pub fn with_span(mut self, span: Span) -> Self {
        if let Some(ctx) = self.context_mut() {
            ctx.set_span(span);
        }
        self
    }

    pub fn with_stack(mut self, stack: Vec<StackFrame>) -> Self {
        if let Some(ctx) = self.context_mut() {
            ctx.set_stack(stack);
        }
        self
    }

    pub fn span(&self) -> Option<Span> {
        self.context_ref().and_then(|ctx| ctx.span)
    }

    pub fn with_file(mut self, file: Option<String>) -> Self {
        if let Some(ctx) = self.context_mut() {
            ctx.set_file(file);
        }
        self
    }

    pub fn with_env(mut self, env: EnvRef) -> Self {
        if let Some(ctx) = self.context_mut() {
            ctx.set_env(env);
        }
        self
    }

    pub fn file(&self) -> Option<&str> {
        self.context_ref().and_then(|ctx| ctx.file.as_deref())
    }

    pub fn env(&self) -> Option<EnvRef> {
        self.context_ref().and_then(|ctx| ctx.env.clone())
    }

    pub fn stack(&self) -> &[StackFrame] {
        self.context_ref()
            .map(|ctx| ctx.stack.as_slice())
            .unwrap_or(&[])
    }

    fn context_ref(&self) -> Option<&ErrorContext> {
        match self {
            CloveError::UnboundSymbol(data)
            | CloveError::Message(data)
            | CloveError::Arity(data)
            | CloveError::Other(data) => Some(&data.context),
            CloveError::TypeMismatch { context, .. } | CloveError::Foreign { context, .. } => {
                Some(context)
            }
            CloveError::Parse(data) => Some(&data.context),
            CloveError::Thrown(_, context) => Some(context),
            CloveError::Guard(data) => Some(&data.context),
            CloveError::RecurSignal { .. } => None,
        }
    }

    fn context_mut(&mut self) -> Option<&mut ErrorContext> {
        match self {
            CloveError::UnboundSymbol(data)
            | CloveError::Message(data)
            | CloveError::Arity(data)
            | CloveError::Other(data) => Some(&mut data.context),
            CloveError::TypeMismatch { context, .. } | CloveError::Foreign { context, .. } => {
                Some(context)
            }
            CloveError::Parse(data) => Some(&mut data.context),
            CloveError::Thrown(_, context) => Some(context),
            CloveError::Guard(data) => Some(&mut data.context),
            CloveError::RecurSignal { .. } => None,
        }
    }
}

pub fn format_error(err: &CloveError) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push(format!("{} {}", ERROR_TAG, err));
    let stack = err.stack();
    let err_location = format_error_location(err.file(), err.span());
    let top_location = stack
        .last()
        .and_then(|frame| format_error_location(frame.file.as_deref(), frame.span));
    let best_frame = best_frame_for_error(err.file(), err.span(), stack);
    let mut snippet = err
        .file()
        .zip(err.span())
        .and_then(|(file, span)| format_source_snippet(file, span));
    if stack.is_empty() {
        if let Some(location) = err_location {
            lines.push(format_error_location_line(location, best_frame));
            if let Some(snippet_lines) = snippet.take() {
                lines.extend(snippet_lines);
            }
        }
    } else {
        let mut insert_snippet_after_top = false;
        if let Some(location) = err_location {
            if top_location.as_deref() != Some(location.as_str()) {
                lines.push(format_error_location_line(location, best_frame));
                if let Some(snippet_lines) = snippet.take() {
                    lines.extend(snippet_lines);
                }
            } else {
                insert_snippet_after_top = snippet.is_some();
            }
        }
        let mut is_top = true;
        for frame in stack.iter().rev() {
            let location = format_error_location(frame.file.as_deref(), frame.span)
                .unwrap_or_else(|| "unknown".into());
            if frame.function.is_empty() {
                lines.push(format!("  at {}", location));
            } else {
                lines.push(format!("  at {} in {}", location, frame.function));
            }
            if is_top && insert_snippet_after_top {
                if let Some(snippet_lines) = snippet.take() {
                    lines.extend(snippet_lines);
                }
                insert_snippet_after_top = false;
            }
            is_top = false;
        }
    }
    lines
}

fn format_error_location(file: Option<&str>, span: Option<Span>) -> Option<String> {
    let file_name = file.unwrap_or("unknown");
    match span {
        Some(span) => Some(format!("{}:{}:{}", file_name, span.line, span.col)),
        None => {
            if file.is_some() {
                Some(file_name.to_string())
            } else {
                None
            }
        }
    }
}

fn format_error_location_line(location: String, frame: Option<&StackFrame>) -> String {
    if let Some(frame) = frame {
        if !frame.function.is_empty() {
            return format!("  at {} in {}", location, frame.function);
        }
    }
    format!("  at {}", location)
}

fn best_frame_for_error<'a>(
    file: Option<&str>,
    span: Option<Span>,
    stack: &'a [StackFrame],
) -> Option<&'a StackFrame> {
    if stack.is_empty() {
        return None;
    }
    if let (Some(file), Some(span)) = (file, span) {
        let mut best: Option<&StackFrame> = None;
        for frame in stack.iter().rev() {
            if frame.function.is_empty() {
                continue;
            }
            if frame.file.as_deref() != Some(file) {
                continue;
            }
            if let Some(frame_span) = frame.span {
                if frame_span.line > span.line {
                    continue;
                }
                if best.is_none()
                    || best
                        .and_then(|candidate| candidate.span)
                        .is_some_and(|candidate_span| frame_span.line > candidate_span.line)
                {
                    best = Some(frame);
                }
            } else if best.is_none() {
                best = Some(frame);
            }
        }
        if best.is_some() {
            return best;
        }
    }
    stack
        .iter()
        .rev()
        .find(|frame| !frame.function.is_empty() && !is_internal_frame_name(&frame.function))
        .or_else(|| stack.iter().rev().find(|frame| !frame.function.is_empty()))
}

fn is_internal_frame_name(name: &str) -> bool {
    matches!(
        name,
        "__apply"
            | "<lambda>"
            | "let"
            | "if"
            | "cond"
            | "match"
            | "try"
            | "catch"
            | "finally"
            | "err"
            | "fin"
            | "loop"
            | "do"
            | "where"
            | "with-init"
    )
}

fn format_source_snippet(file: &str, span: Span) -> Option<Vec<String>> {
    if span.line == 0 {
        return None;
    }
    let content = fs::read_to_string(file).ok()?;
    let line = content.lines().nth(span.line.saturating_sub(1))?;
    let mut lines = Vec::new();
    lines.push(format!("  | {}", line));
    let mut marker = String::from("  | ");
    let col = span.col.saturating_sub(1);
    marker.extend(std::iter::repeat(' ').take(col));
    marker.push('^');
    lines.push(marker);
    Some(lines)
}

impl From<String> for CloveError {
    fn from(s: String) -> Self {
        CloveError::runtime(s)
    }
}

impl From<&str> for CloveError {
    fn from(s: &str) -> Self {
        CloveError::runtime(s.to_string())
    }
}
