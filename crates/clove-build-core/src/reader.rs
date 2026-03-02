use crate::ast::{Expr, ExprKind, Literal, Span};
use crate::error::Clove2Error;

pub fn read_all(input: &str) -> Result<Vec<Expr>, Clove2Error> {
    let mut reader = Reader::new(input);
    reader.read_all()
}

struct Reader {
    chars: Vec<char>,
    pos: usize,
}

impl Reader {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn read_all(&mut self) -> Result<Vec<Expr>, Clove2Error> {
        let mut out = Vec::new();
        loop {
            self.skip_ws_and_comments()?;
            if self.is_end() {
                break;
            }
            out.push(self.read_expr()?);
        }
        Ok(out)
    }

    fn read_expr(&mut self) -> Result<Expr, Clove2Error> {
        self.skip_ws_and_comments()?;
        let start = self.pos;
        let ch = self.peek_char().ok_or_else(|| self.err("unexpected EOF"))?;
        let expr = match ch {
            '\'' => {
                self.pos += 1;
                let quoted = self.read_expr()?;
                let end = self.pos;
                let items = vec![Expr::symbol("quote"), quoted];
                Ok(Expr::new(ExprKind::List(items), Span::new(start, end)))
            }
            '#' => {
                self.pos += 1;
                let expr = self.read_sharp(start)?;
                Ok(expr)
            }
            '(' => {
                self.pos += 1;
                let items = self.read_list(')')?;
                let end = self.pos;
                Ok(Expr::new(ExprKind::List(items), Span::new(start, end)))
            }
            '[' => {
                self.pos += 1;
                let items = self.read_list(']')?;
                let end = self.pos;
                Ok(Expr::new(ExprKind::Vector(items), Span::new(start, end)))
            }
            '{' => {
                self.pos += 1;
                let items = self.read_map()?;
                let end = self.pos;
                Ok(Expr::new(ExprKind::Map(items), Span::new(start, end)))
            }
            '"' => {
                self.pos += 1;
                let s = self.read_string()?;
                let end = self.pos;
                Ok(Expr::new(
                    ExprKind::Literal(Literal::Str(s)),
                    Span::new(start, end),
                ))
            }
            '$' => {
                self.pos += 1;
                let kind = self.read_foreign_block()?;
                let end = self.pos;
                Ok(Expr::new(kind, Span::new(start, end)))
            }
            ':' => {
                self.pos += 1;
                let name = self.read_token()?;
                if name.is_empty() {
                    return Err(self.err("empty keyword"));
                }
                let end = self.pos;
                Ok(Expr::new(ExprKind::Keyword(name), Span::new(start, end)))
            }
            '/' => {
                if self.should_read_regex() {
                    self.pos += 1;
                    let pattern = self.read_regex()?;
                    let end = self.pos;
                    Ok(Expr::new(
                        ExprKind::Literal(Literal::Regex(pattern)),
                        Span::new(start, end),
                    ))
                } else {
                    let token = self.read_token()?;
                    let kind = self.token_to_expr(&token)?;
                    let end = self.pos;
                    Ok(Expr::new(kind, Span::new(start, end)))
                }
            }
            _ => {
                let token = self.read_token()?;
                let kind = self.token_to_expr(&token)?;
                let end = self.pos;
                Ok(Expr::new(kind, Span::new(start, end)))
            }
        }?;
        let expr = self.lower_call_sugar(expr, start)?;
        let expr = self.lower_compose_sugar(expr)?;
        self.read_type_annotation(expr, start)
    }

    fn read_type_annotation(&mut self, mut expr: Expr, start: usize) -> Result<Expr, Clove2Error> {
        loop {
            if self.peek_char() != Some(':') {
                break;
            }
            self.pos += 1;
            self.skip_ws_and_comments()?;
            if self.is_end() {
                return Err(self.err("missing type after ':'"));
            }
            let ty_expr = self.read_expr()?;
            let end = self.pos;
            let items = vec![Expr::symbol("as"), ty_expr, expr];
            expr = Expr::new(ExprKind::List(items), Span::new(start, end));
        }
        Ok(expr)
    }

    fn read_list(&mut self, end: char) -> Result<Vec<Expr>, Clove2Error> {
        let mut items = Vec::new();
        loop {
            self.skip_ws_and_comments()?;
            if self.consume_char(end) {
                break;
            }
            if self.is_end() {
                return Err(self.err(format!("unexpected EOF while reading '{}'", end)));
            }
            items.push(self.read_expr()?);
        }
        Ok(items)
    }

    fn read_map(&mut self) -> Result<Vec<(Expr, Expr)>, Clove2Error> {
        let mut entries = Vec::new();
        loop {
            self.skip_ws_and_comments()?;
            if self.consume_char('}') {
                break;
            }
            if self.is_end() {
                return Err(self.err("unexpected EOF while reading map"));
            }
            let key = self.read_expr()?;
            self.skip_ws_and_comments()?;
            if self.consume_char('}') {
                if is_open_shape_marker(&key) {
                    entries.push((key, Expr::literal(Literal::Nil)));
                    break;
                }
                return Err(self.err("map entry is missing value"));
            }
            let value = self.read_expr()?;
            entries.push((key, value));
        }
        Ok(entries)
    }

    fn read_string(&mut self) -> Result<String, Clove2Error> {
        let mut out = String::new();
        while let Some(ch) = self.peek_char() {
            self.pos += 1;
            match ch {
                '"' => return Ok(out),
                '\\' => {
                    let esc = self
                        .peek_char()
                        .ok_or_else(|| self.err("unexpected EOF in string"))?;
                    self.pos += 1;
                    match esc {
                        '\\' => out.push('\\'),
                        '"' => out.push('"'),
                        'n' => out.push('\n'),
                        'r' => out.push('\r'),
                        't' => out.push('\t'),
                        other => {
                            return Err(self.err(format!("unknown escape: \\{}", other)));
                        }
                    }
                }
                other => out.push(other),
            }
        }
        Err(self.err("unexpected EOF in string"))
    }

    fn should_read_regex(&self) -> bool {
        let mut idx = self.pos + 1;
        let Some(first) = self.chars.get(idx).copied() else {
            return false;
        };
        if first.is_whitespace() {
            return false;
        }
        let mut escaped = false;
        while let Some(ch) = self.chars.get(idx).copied() {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '/' {
                return true;
            } else if ch.is_whitespace() {
                return false;
            }
            idx += 1;
        }
        false
    }

    fn read_regex(&mut self) -> Result<String, Clove2Error> {
        let mut out = String::new();
        let mut escaped = false;
        while let Some(ch) = self.peek_char() {
            self.pos += 1;
            if escaped {
                out.push(ch);
                escaped = false;
                continue;
            }
            match ch {
                '\\' => {
                    out.push(ch);
                    escaped = true;
                }
                '/' => return Ok(out),
                other => out.push(other),
            }
        }
        Err(self.err("unexpected EOF in regex literal"))
    }

    fn read_foreign_block(&mut self) -> Result<ExprKind, Clove2Error> {
        let tag = self.read_token()?;
        if tag.is_empty() {
            return Err(self.err("missing foreign tag"));
        }
        self.skip_ws_and_comments()?;
        if !self.consume_char('{') {
            return Err(self.err("foreign block must start with '{'"));
        }
        let mut depth = 1;
        let mut out = String::new();
        while let Some(ch) = self.peek_char() {
            self.pos += 1;
            match ch {
                '{' => {
                    depth += 1;
                    out.push(ch);
                }
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(ExprKind::ForeignBlock { tag, code: out });
                    }
                    out.push(ch);
                }
                other => out.push(other),
            }
        }
        Err(self.err("unexpected EOF in foreign block"))
    }

    fn token_to_expr(&self, token: &str) -> Result<ExprKind, Clove2Error> {
        match token {
            "nil" => Ok(ExprKind::Literal(Literal::Nil)),
            "true" => Ok(ExprKind::Literal(Literal::Bool(true))),
            "false" => Ok(ExprKind::Literal(Literal::Bool(false))),
            _ => {
                if let Some(literal) = parse_number(token) {
                    return Ok(ExprKind::Literal(literal));
                }
                Ok(ExprKind::Symbol(token.to_string()))
            }
        }
    }

    fn read_token(&mut self) -> Result<String, Clove2Error> {
        let start = self.pos;
        let mut angle_depth = 0usize;
        let mut starts_with_ident = None;
        while let Some(ch) = self.peek_char() {
            if starts_with_ident.is_none() {
                starts_with_ident = Some(ch.is_ascii_alphabetic() || ch == '_');
            }
            let in_angle = angle_depth > 0 && matches!(starts_with_ident, Some(true));
            if !in_angle {
                if is_delimiter(ch) {
                    break;
                }
            } else if matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';') {
                break;
            }
            if matches!(starts_with_ident, Some(true)) {
                if ch == '<' {
                    angle_depth += 1;
                } else if ch == '>' && angle_depth > 0 {
                    angle_depth -= 1;
                }
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(self.err("expected token"));
        }
        if angle_depth > 0 {
            return Err(self.err("unclosed generic type"));
        }
        Ok(self.chars[start..self.pos].iter().collect())
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), Clove2Error> {
        loop {
            while let Some(ch) = self.peek_char() {
                if ch.is_whitespace() || ch == ',' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.peek_char() == Some(';') {
                while let Some(ch) = self.peek_char() {
                    self.pos += 1;
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            if self.peek_char() == Some('#') && self.peek_next_char() == Some('_') {
                self.pos += 2;
                self.skip_ws_and_comments()?;
                if self.is_end() {
                    return Err(self.err("unexpected EOF after #_"));
                }
                let _ = self.read_expr()?;
                continue;
            }
            break;
        }
        Ok(())
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

    fn is_end(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn err(&self, msg: impl Into<String>) -> Clove2Error {
        Clove2Error::new(msg.into())
    }

    fn peek_next_char(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn read_sharp(&mut self, start: usize) -> Result<Expr, Clove2Error> {
        let Some(ch) = self.peek_char() else {
            return Err(self.err("unexpected EOF after #"));
        };
        match ch {
            '(' => {
                self.pos += 1;
                let body = self.read_list(')')?;
                self.expand_short_fn(body, start)
            }
            '{' => {
                self.pos += 1;
                let items = self.read_list('}')?;
                let end = self.pos;
                Ok(Expr::new(ExprKind::Set(items), Span::new(start, end)))
            }
            ch if ch.is_ascii_digit() => {
                let idx = self.read_sharp_index()?;
                self.expand_sharp_accessor(idx, start)
            }
            _ => Err(self.err("unsupported reader tag")),
        }
    }

    fn lower_call_sugar(&mut self, base: Expr, start: usize) -> Result<Expr, Clove2Error> {
        if self.peek_char() != Some('(') {
            return Ok(base);
        }
        let ExprKind::Symbol(sym) = &base.kind else {
            return Ok(base);
        };
        if sym == "*" {
            return Ok(base);
        }
        self.pos += 1;
        let args = self.read_list(')')?;
        let end = self.pos;
        let mut items = Vec::with_capacity(args.len() + 1);
        items.push(base);
        items.extend(args);
        let expr = Expr::new(ExprKind::List(items), Span::new(start, end));
        if self.peek_char() == Some('(') {
            return Err(self.err("chained call sugar is not supported yet"));
        }
        Ok(expr)
    }

    fn lower_compose_sugar(&self, base: Expr) -> Result<Expr, Clove2Error> {
        let ExprKind::List(items) = &base.kind else {
            return Ok(base);
        };
        if items.len() < 2 {
            return Ok(base);
        }
        let head_is_dot = matches!(&items[0].kind, ExprKind::Symbol(sym) if sym == ".");
        let tail_is_dot =
            matches!(&items[items.len() - 1].kind, ExprKind::Symbol(sym) if sym == ".");
        if head_is_dot && tail_is_dot {
            return Err(self.err("compose form cannot have '.' at both head and tail"));
        }
        if head_is_dot {
            let mut out = Vec::with_capacity(items.len());
            out.push(Expr::symbol("comp"));
            out.extend(items.iter().skip(1).rev().cloned());
            return Ok(Expr::new(ExprKind::List(out), base.span));
        }
        if tail_is_dot {
            let mut out = Vec::with_capacity(items.len());
            out.push(Expr::symbol("comp"));
            out.extend(items[..items.len() - 1].iter().cloned());
            return Ok(Expr::new(ExprKind::List(out), base.span));
        }
        Ok(base)
    }

    fn read_sharp_index(&mut self) -> Result<i64, Clove2Error> {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.pos += 1;
            } else {
                break;
            }
        }
        if self.pos == start {
            return Err(self.err("sharp accessor expects index"));
        }
        let num: i64 = self.chars[start..self.pos]
            .iter()
            .collect::<String>()
            .parse()
            .map_err(|_| self.err("sharp accessor expects integer index"))?;
        Ok(num)
    }

    fn expand_sharp_accessor(&mut self, index: i64, start: usize) -> Result<Expr, Clove2Error> {
        let arg = Expr::symbol("it__sharp");
        let params = Expr::vector(vec![arg.clone()]);
        let body = Expr::list(vec![
            Expr::symbol("get"),
            arg,
            Expr::literal(Literal::Int(index)),
        ]);
        let items = vec![Expr::symbol("fn"), params, body];
        let end = self.pos;
        Ok(Expr::new(ExprKind::List(items), Span::new(start, end)))
    }

    fn expand_short_fn(&mut self, body: Vec<Expr>, start: usize) -> Result<Expr, Clove2Error> {
        let list_expr = Expr::list(body);
        let info = placeholder_info(&[list_expr.clone()]);
        let names = PlaceholderNames::new(info.max_index, info.has_rest);
        let mut params = Vec::new();
        for name in &names.args {
            params.push(Expr::symbol(name));
        }
        if let Some(rest) = &names.rest {
            params.push(Expr::symbol("&"));
            params.push(Expr::symbol(rest));
        }
        let params = Expr::vector(params);
        let replaced = replace_placeholders(list_expr, &names);
        let items = vec![Expr::symbol("fn"), params, replaced];
        let end = self.pos;
        Ok(Expr::new(ExprKind::List(items), Span::new(start, end)))
    }
}

#[derive(Default, Clone, Copy)]
struct PlaceholderInfo {
    max_index: usize,
    has_rest: bool,
}

impl PlaceholderInfo {
    fn merge(&mut self, other: PlaceholderInfo) {
        self.max_index = self.max_index.max(other.max_index);
        self.has_rest |= other.has_rest;
    }
}

struct PlaceholderNames {
    args: Vec<String>,
    rest: Option<String>,
}

impl PlaceholderNames {
    fn new(max_index: usize, has_rest: bool) -> Self {
        let mut args = Vec::new();
        for idx in 1..=max_index {
            args.push(format!("p{}__auto", idx));
        }
        let rest = if has_rest {
            Some("rest__auto".to_string())
        } else {
            None
        };
        Self { args, rest }
    }

    fn lookup(&self, sym: &str) -> Option<String> {
        if sym == "%&" {
            return self.rest.clone();
        }
        if sym == "%" {
            return self.args.first().cloned();
        }
        if let Some(num) = sym.strip_prefix('%').and_then(|s| s.parse::<usize>().ok()) {
            if num > 0 {
                return self.args.get(num - 1).cloned();
            }
        }
        None
    }
}

fn placeholder_info(exprs: &[Expr]) -> PlaceholderInfo {
    let mut info = PlaceholderInfo::default();
    for expr in exprs {
        info.merge(placeholder_info_expr(expr));
    }
    info
}

fn placeholder_info_expr(expr: &Expr) -> PlaceholderInfo {
    match &expr.kind {
        ExprKind::Symbol(sym) => {
            if sym == "%&" {
                PlaceholderInfo {
                    max_index: 0,
                    has_rest: true,
                }
            } else if sym == "%" {
                PlaceholderInfo {
                    max_index: 1,
                    has_rest: false,
                }
            } else if let Some(num) = sym.strip_prefix('%').and_then(|s| s.parse::<usize>().ok()) {
                if num > 0 {
                    PlaceholderInfo {
                        max_index: num,
                        has_rest: false,
                    }
                } else {
                    PlaceholderInfo::default()
                }
            } else {
                PlaceholderInfo::default()
            }
        }
        ExprKind::List(items) | ExprKind::Vector(items) | ExprKind::Set(items) => {
            let mut info = PlaceholderInfo::default();
            for item in items {
                info.merge(placeholder_info_expr(item));
            }
            info
        }
        ExprKind::Map(entries) => {
            let mut info = PlaceholderInfo::default();
            for (k, v) in entries {
                info.merge(placeholder_info_expr(k));
                info.merge(placeholder_info_expr(v));
            }
            info
        }
        _ => PlaceholderInfo::default(),
    }
}

fn replace_placeholders(expr: Expr, names: &PlaceholderNames) -> Expr {
    match expr.kind {
        ExprKind::Symbol(sym) => {
            if let Some(repl) = names.lookup(&sym) {
                Expr::symbol(repl)
            } else {
                Expr::symbol(sym)
            }
        }
        ExprKind::List(items) => {
            let replaced = items
                .into_iter()
                .map(|item| replace_placeholders(item, names))
                .collect();
            Expr::list(replaced)
        }
        ExprKind::Vector(items) => {
            let replaced = items
                .into_iter()
                .map(|item| replace_placeholders(item, names))
                .collect();
            Expr::vector(replaced)
        }
        ExprKind::Set(items) => {
            let replaced = items
                .into_iter()
                .map(|item| replace_placeholders(item, names))
                .collect();
            Expr::set(replaced)
        }
        ExprKind::Map(entries) => {
            let replaced = entries
                .into_iter()
                .map(|(k, v)| {
                    (
                        replace_placeholders(k, names),
                        replace_placeholders(v, names),
                    )
                })
                .collect();
            Expr::map(replaced)
        }
        ExprKind::Literal(lit) => Expr::literal(lit),
        ExprKind::Keyword(sym) => Expr::keyword(sym),
        ExprKind::ForeignBlock { tag, code } => Expr::foreign_block(tag, code),
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace()
        || ch == ','
        || ch == '('
        || ch == ')'
        || ch == '['
        || ch == ']'
        || ch == '{'
        || ch == '}'
        || ch == '"'
        || ch == ';'
}

fn is_open_shape_marker(expr: &Expr) -> bool {
    matches!(
        &expr.kind,
        ExprKind::Symbol(sym) if sym == ".." || sym == "..."
    )
}

fn parse_number(token: &str) -> Option<Literal> {
    if token.contains('.') || token.contains('e') || token.contains('E') {
        if let Ok(num) = token.parse::<f64>() {
            return Some(Literal::Float(num));
        }
        return None;
    }
    if let Ok(num) = token.parse::<i64>() {
        return Some(Literal::Int(num));
    }
    None
}
