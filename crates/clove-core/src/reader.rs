use crate::ast::{DurationUnit, DurationValue, Form, FormKind, InterpolatedPart, MapItem, Span};
use crate::error::CloveError;
use crate::spread::strip_spread_symbol;
use crate::type_syntax::parse_type_expr_from_forms;
use crate::types::parse_type_hint;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::Arc;

pub const DOT_CHAIN_PLACEHOLDER_PREFIX: &str = "__dot_chain";
pub const INDEX_GET_SYM: &str = "clove.syntax::index-get";
pub const INDEX_GET_IN_SYM: &str = "clove.syntax::index-get-in";
pub const INDEX_GET_MANY_SYM: &str = "clove.syntax::index-get-many";
pub const OOP_DOT_STAGE_SYM: &str = "clove.syntax::oop-dot";
pub const OOP_SYNTAX_SYM: &str = "clove.syntax::oop";
pub const OOP_INDEX_SYM: &str = "clove.syntax::oop-index";
pub const OOP_BARE_SYM: &str = "clove.syntax::oop-bare";
pub const OOP_SEG_SYM: &str = "clove.syntax::oop-seg";
pub const OOP_METHOD_SYM: &str = "clove.syntax::oop-method";
pub const OOP_NIL_SAFE_SYM: &str = "clove.syntax::oop-nil-safe";
pub const OOP_AS_SYM: &str = "clove.syntax::oop-as";
pub const OOP_LET_SYM: &str = "clove.syntax::oop-let";
pub const MAP_REF_SYM: &str = "clove.syntax::map-ref";
pub const INFIX_SYNTAX_SYM: &str = "clove.syntax::infix";
pub const MATCH_OR_SYM: &str = "clove.syntax::match-or";
const RANGE_LITERAL_SYM: &str = "__range_literal";
const SHARP_ACCESSOR_ARG: &str = "__it";

#[derive(Clone, Debug)]
pub struct CommentTrivia {
    pub span: Span,
    pub container: Option<usize>,
    pub text: String,
    pub inline: bool,
    pub after_opening_delim: bool,
    pub trailing_form: Option<usize>,
    pub leading_blank: bool,
}

pub type TagHandler = Arc<dyn Fn(&mut Reader, Span) -> Result<Form, CloveError> + Send + Sync>;

#[derive(Clone)]
pub struct ReaderOptions {
    allowed_foreign_tags: Option<HashSet<String>>,
    pub tag_handlers: HashMap<String, TagHandler>,
    pub source_name: Option<String>,
    pub allow_invalid_type_hints: bool,
    pub start_line: usize,
    pub start_col: usize,
}

impl ReaderOptions {
    pub fn language_defaults(foreign_tags: Vec<String>) -> Self {
        Self {
            allowed_foreign_tags: normalize_foreign_tags(foreign_tags),
            tag_handlers: crate::reader_tags::builtin_tag_handlers(),
            source_name: None,
            allow_invalid_type_hints: false,
            start_line: 1,
            start_col: 1,
        }
    }

    pub fn language_defaults_with_extra(
        foreign_tags: Vec<String>,
        mut extra_handlers: HashMap<String, TagHandler>,
    ) -> Self {
        let mut handlers = crate::reader_tags::builtin_tag_handlers();
        for (name, handler) in extra_handlers.drain() {
            handlers.insert(name, handler);
        }
        Self {
            allowed_foreign_tags: normalize_foreign_tags(foreign_tags),
            tag_handlers: handlers,
            source_name: None,
            allow_invalid_type_hints: false,
            start_line: 1,
            start_col: 1,
        }
    }

    pub fn with_allowed_foreign_tags(mut self, tags: Vec<String>) -> Self {
        self.allowed_foreign_tags = normalize_foreign_tags(tags);
        self
    }

    pub fn allow_all_foreign_tags(mut self) -> Self {
        self.allowed_foreign_tags = None;
        self
    }

    pub fn with_source_name(mut self, name: Option<String>) -> Self {
        self.source_name = name;
        self
    }

    pub fn allow_invalid_type_hints(mut self) -> Self {
        self.allow_invalid_type_hints = true;
        self
    }

    pub fn allowed_foreign_tags(&self) -> Option<impl Iterator<Item = &String>> {
        self.allowed_foreign_tags.as_ref().map(|set| set.iter())
    }
}

impl Default for ReaderOptions {
    fn default() -> Self {
        Self {
            allowed_foreign_tags: None,
            tag_handlers: HashMap::new(),
            source_name: None,
            allow_invalid_type_hints: false,
            start_line: 1,
            start_col: 1,
        }
    }
}

fn normalize_foreign_tags(tags: Vec<String>) -> Option<HashSet<String>> {
    if tags.is_empty() {
        None
    } else {
        Some(tags.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
struct SharpSelector {
    path: Vec<Form>,
    default: Option<Form>,
}

pub struct Reader {
    chars: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
    comments: Vec<CommentTrivia>,
    comma_trailing: BTreeSet<usize>,
    container_stack: Vec<Option<usize>>,
    last_form_stack: Vec<Option<usize>>,
    top_level_blank_lines: BTreeSet<usize>,
    inner_blank_lines: BTreeSet<usize>,
    pending_blank_stack: Vec<bool>,
    since_newline_has_content_stack: Vec<bool>,
    pub options: ReaderOptions,
    last_closed_form: Option<usize>,
    interpolation_depth: usize,
    string_escape_indices: BTreeSet<usize>,
}

impl Reader {
    pub fn new(source: &str) -> Self {
        Self::new_with_options(source, ReaderOptions::default())
    }

    pub fn new_with_options(source: &str, mut options: ReaderOptions) -> Self {
        if options.start_line == 0 {
            options.start_line = 1;
        }
        if options.start_col == 0 {
            options.start_col = 1;
        }
        Self {
            chars: source.chars().collect(),
            index: 0,
            line: options.start_line,
            col: options.start_col,
            comments: Vec::new(),
            comma_trailing: BTreeSet::new(),
            container_stack: vec![None],
            last_form_stack: vec![None],
            top_level_blank_lines: BTreeSet::new(),
            inner_blank_lines: BTreeSet::new(),
            pending_blank_stack: vec![false],
            since_newline_has_content_stack: vec![true],
            options,
            last_closed_form: None,
            interpolation_depth: 0,
            string_escape_indices: BTreeSet::new(),
        }
    }

    pub fn take_comments(&mut self) -> Vec<CommentTrivia> {
        std::mem::take(&mut self.comments)
    }

    pub fn take_top_level_blank_lines(&mut self) -> BTreeSet<usize> {
        std::mem::take(&mut self.top_level_blank_lines)
    }

    pub fn take_inner_blank_lines(&mut self) -> BTreeSet<usize> {
        std::mem::take(&mut self.inner_blank_lines)
    }

    pub fn take_string_escapes(&mut self) -> BTreeSet<usize> {
        std::mem::take(&mut self.string_escape_indices)
    }

    pub fn take_commas(&mut self) -> BTreeSet<usize> {
        std::mem::take(&mut self.comma_trailing)
    }

    pub fn read_all(&mut self) -> Result<Vec<Form>, CloveError> {
        let mut forms = Vec::new();
        self.skip_ws_and_comments();
        while !self.eof() {
            forms.push(self.read_form()?);
            self.skip_ws_and_comments();
        }
        Ok(forms)
    }

    fn read_form(&mut self) -> Result<Form, CloveError> {
        self.skip_ws_and_comments();
        if self.eof() {
            return self.parse_err("unexpected end of input");
        }
        let span = self.current_span();
        self.record_blank_line(span.index);
        let mut form = match self.current_char() {
            '$' => self.read_dollar_form(),
            '@' => self.read_deref_form(span),
            '&' => self.read_map_ref_or_symbol(span),
            '\'' => self.read_quote_form(span),
            '(' => self.read_list(),
            '[' => self.read_vector(),
            '#' => self.read_dispatch(span),
            '{' => self.read_map(span),
            '"' => self.read_string(span),
            ':' => self.read_keyword(span),
            '/' => {
                if self.should_read_regex_literal() {
                    self.read_regex_literal(span, '/', crate::ast::RegexDelim::Slash)
                } else {
                    self.read_atom(span)
                }
            }
            ch => {
                if ch == ')' || ch == ']' {
                    self.parse_err(format!("unexpected closing delimiter '{}'", ch))
                } else {
                    self.read_atom(span)
                }
            }
        }?;

        form = self.lower_call_sugar(form)?;
        form = self.lower_compose_sugar(form)?;
        loop {
            let mut progressed = false;
            while !self.eof() && self.current_char() == '[' {
                if matches!(&form.kind, FormKind::Symbol(sym) if sym == "*") {
                    break;
                }
                let accessor = self.read_indexer_accessor()?;
                form = self.lower_index_sugar(form, accessor)?;
                progressed = true;
            }
            let before = self.index;
            form = self.lower_dot_chain(form)?;
            form = self.lower_oop_chain(form)?;
            if self.index != before {
                progressed = true;
            }
            if !progressed {
                break;
            }
        }
        if let Some(hint) = self.read_postfix_type_hint(form.span)? {
            form = form.with_type_hint(hint);
        }

        Ok(form)
    }

    fn read_postfix_type_hint(
        &mut self,
        span: Span,
    ) -> Result<Option<crate::types::TypeHint>, CloveError> {
        if self.eof() || !matches!(self.current_char(), '<' | ':') {
            return Ok(None);
        }
        if self.current_char() == '<' {
            self.advance(); // consume '<'
            let mut depth = 1usize;
            let mut buf = String::new();
            while !self.eof() {
                let ch = self.current_char();
                match ch {
                    '<' => {
                        depth += 1;
                        buf.push(ch);
                        self.advance();
                    }
                    '>' => {
                        depth -= 1;
                        if depth == 0 {
                            self.advance(); // consume closing '>'
                            break;
                        }
                        buf.push(ch);
                        self.advance();
                    }
                    _ => {
                        buf.push(ch);
                        self.advance();
                    }
                }
            }
            if depth != 0 {
                return self.parse_err("unterminated type annotation");
            }
            let hint = match parse_type_hint(buf.trim()) {
                Ok(hint) => hint,
                Err(err) => {
                    if self.options.allow_invalid_type_hints {
                        return Ok(None);
                    }
                    return Err(err.with_span(span));
                }
            };
            return Ok(Some(hint));
        }
        self.advance(); // consume ':'
        self.skip_ws_and_comments();
        if self.eof() {
            return self.parse_err("expected type annotation after ':'");
        }
        let type_forms = self.read_type_annotation_forms(span)?;
        let hint = match parse_type_expr_from_forms(&type_forms) {
            Ok((kind, _)) => crate::types::TypeHint::new(kind, true),
            Err(err) => {
                if self.options.allow_invalid_type_hints {
                    return Ok(None);
                }
                return Err(err.with_span(span));
            }
        };
        Ok(Some(hint))
    }

    fn read_type_annotation_forms(&mut self, span: Span) -> Result<Vec<Form>, CloveError> {
        let mut forms = Vec::new();
        let first = self.read_form()?;
        let is_vec = matches!(first.kind, FormKind::Vector(_));
        forms.push(first);
        if is_vec {
            self.skip_ws_and_comments();
            if !self.eof() && self.current_char() == '-' && self.peek_char() == Some('>') {
                let arrow = self.read_form()?;
                forms.push(arrow);
                let mut ret = self.read_type_annotation_forms(span)?;
                forms.append(&mut ret);
            }
        }
        if forms.is_empty() {
            return Err(CloveError::parse("expected type annotation").with_span(span));
        }
        Ok(forms)
    }

    fn read_dispatch(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // consume '#'

        match self.current_char() {
            '{' => return self.read_set(start),
            '(' => return self.read_short_fn(start),
            '/' => return self.read_regex_literal(start, '/', crate::ast::RegexDelim::HashSlash),
            ch if ch.is_ascii_digit() || ch == '[' || ch == ':' || ch == '"' => {
                return self.read_sharp_accessor_fn(start)
            }
            '_' => {
                self.advance(); // consume '_'
                self.skip_ws_and_comments();
                self.read_form()?; // read and discard
                return self.read_form();
            }
            _ => {}
        }

        let mut tag_name = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || ch == '{'
                || ch == '['
                || ch == '"'
                || ch == '('
                || ch == ')'
                || ch == '}'
                || ch == ']'
            {
                break;
            }
            tag_name.push(ch);
            self.advance();
        }

        if tag_name.is_empty() {
            return self.parse_err("expected tag name after #");
        }

        let handler = self.options.tag_handlers.get(&tag_name).cloned();
        if let Some(handler) = handler {
            return handler(self, start);
        }

        self.parse_err(format!("unknown reader tag: #{}", tag_name))
    }

    fn read_list(&mut self) -> Result<Form, CloveError> {
        let start = self.current_span();
        self.advance(); // (
        self.container_stack.push(Some(start.index));
        self.last_form_stack.push(None);
        self.pending_blank_stack.push(false);
        self.since_newline_has_content_stack.push(true);
        let mut items: Vec<Form> = Vec::new();
        self.skip_ws_and_comments();
        while !self.eof() && self.current_char() != ')' {
            let form = self.read_form()?;
            self.mark_last_form_index(form.span.index);
            items.push(form);
            self.skip_ws_and_comments();
        }
        if self.eof() {
            self.container_stack.pop();
            self.last_form_stack.pop();
            self.pending_blank_stack.pop();
            self.since_newline_has_content_stack.pop();
            return self.parse_err("unterminated list");
        }
        self.advance(); // )
        self.container_stack.pop();
        self.last_form_stack.pop();
        self.pending_blank_stack.pop();
        self.since_newline_has_content_stack.pop();
        let items = self.rewrite_match_or_patterns(items)?;
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::List(items), start))
    }

    fn rewrite_match_or_patterns(&self, items: Vec<Form>) -> Result<Vec<Form>, CloveError> {
        let Some(FormKind::Symbol(head)) = items.first().map(|form| &form.kind) else {
            return Ok(items);
        };
        if head != "match" || items.len() < 3 {
            return Ok(items);
        }
        let mut out = Vec::with_capacity(items.len());
        out.push(items[0].clone());
        out.push(items[1].clone());
        let mut idx = 2;
        while idx < items.len() {
            let (pattern_form, next_idx) = self.parse_match_or_pattern(&items, idx)?;
            out.push(pattern_form);
            idx = next_idx;
            while idx + 1 < items.len() {
                match &items[idx].kind {
                    FormKind::Keyword(kw) if kw == "when" || kw == "if" || kw == "as" => {
                        out.push(items[idx].clone());
                        out.push(items[idx + 1].clone());
                        idx += 2;
                        continue;
                    }
                    _ => {}
                }
                break;
            }
            if idx >= items.len() {
                return self.parse_err("match clause missing result expression");
            }
            out.push(items[idx].clone());
            idx += 1;
        }
        Ok(out)
    }

    fn parse_match_or_pattern(
        &self,
        items: &[Form],
        start_idx: usize,
    ) -> Result<(Form, usize), CloveError> {
        let mut patterns = Vec::new();
        let mut idx = start_idx;
        let mut expect_pattern = true;
        let mut saw_separator = false;
        while idx < items.len() {
            if expect_pattern {
                if is_match_or_separator(&items[idx]) {
                    return self.parse_err("match pattern expects expression after separator");
                }
                patterns.push(items[idx].clone());
                idx += 1;
                expect_pattern = false;
                continue;
            }
            let Some(last) = patterns.last() else {
                break;
            };
            if self.comma_trailing.contains(&last.span.index) {
                saw_separator = true;
                expect_pattern = true;
                continue;
            }
            if is_match_or_separator(&items[idx]) {
                saw_separator = true;
                idx += 1;
                expect_pattern = true;
                continue;
            }
            break;
        }
        if let Some(last) = patterns.last() {
            if idx >= items.len() && self.comma_trailing.contains(&last.span.index) {
                return self.parse_err("match pattern cannot end with ','");
            }
        }
        if expect_pattern && saw_separator {
            return self.parse_err("match pattern expects expression after separator");
        }
        if patterns.len() == 1 {
            return Ok((patterns[0].clone(), idx));
        }
        let start_span = patterns[0].span;
        let mut or_items = Vec::with_capacity(patterns.len() + 1);
        or_items.push(Form::new(
            FormKind::Symbol(MATCH_OR_SYM.to_string()),
            start_span,
        ));
        or_items.extend(patterns);
        Ok((Form::new(FormKind::List(or_items), start_span), idx))
    }

    fn read_vector(&mut self) -> Result<Form, CloveError> {
        let start = self.current_span();
        self.advance(); // [
        self.container_stack.push(Some(start.index));
        self.last_form_stack.push(None);
        self.pending_blank_stack.push(false);
        self.since_newline_has_content_stack.push(true);
        let mut items = Vec::new();
        self.skip_ws_and_comments();
        while !self.eof() && self.current_char() != ']' {
            let form = self.read_form()?;
            self.mark_last_form_index(form.span.index);
            items.push(form);
            self.skip_ws_and_comments();
        }
        if self.eof() {
            self.container_stack.pop();
            self.last_form_stack.pop();
            self.pending_blank_stack.pop();
            self.since_newline_has_content_stack.pop();
            return self.parse_err("unterminated vector");
        }
        self.advance(); // ]
        self.container_stack.pop();
        self.last_form_stack.pop();
        self.pending_blank_stack.pop();
        self.since_newline_has_content_stack.pop();
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::Vector(items), start))
    }

    fn read_sharp_accessor_fn(&mut self, start: Span) -> Result<Form, CloveError> {
        let mut allows_leading_number = true;
        let mut saw_selector = false;
        let arg_form = Form::new(FormKind::Symbol(SHARP_ACCESSOR_ARG.to_string()), start);
        let mut current = arg_form.clone();
        while !self.eof() {
            let ch = self.current_char();
            if ch == '[' {
                let selectors = self.read_sharp_accessor_selectors()?;
                current = self.apply_sharp_selectors(current, selectors, start)?;
                allows_leading_number = false;
                saw_selector = true;
                continue;
            }
            if ch == '.' {
                if self.peek_char() == Some('[') {
                    self.advance(); // consume '.'
                    let selectors = self.read_sharp_accessor_selectors()?;
                    current = self.apply_sharp_selectors(current, selectors, start)?;
                    allows_leading_number = false;
                    saw_selector = true;
                    continue;
                }
                self.advance(); // consume '.'
                if self.eof() {
                    return self.parse_err("sharp accessor expects selector after '.'");
                }
                let key_span = self.current_span();
                match self.current_char() {
                    ch if ch.is_ascii_digit() => {
                        let num = self.read_sharp_selector_int()?;
                        current = self.apply_sharp_dot_index(current, num, start)?;
                    }
                    ':' => {
                        let key_form = self.read_oop_keyword_segment(key_span)?;
                        let keys = self.expand_sharp_keyword_path_segments(key_form)?;
                        for key in keys {
                            current = self.apply_sharp_dot_key(current, key, start)?;
                        }
                    }
                    '"' => {
                        let key_form = self.read_string(key_span)?;
                        current = self.apply_sharp_dot_key(current, key_form, start)?;
                    }
                    _ => {
                        return self.parse_err("sharp accessor expects selector after '.'");
                    }
                }
                allows_leading_number = false;
                saw_selector = true;
                continue;
            }
            if allows_leading_number && ch.is_ascii_digit() {
                let num = self.read_sharp_selector_int()?;
                current = self.apply_sharp_dot_index(current, num, start)?;
                allows_leading_number = false;
                saw_selector = true;
                continue;
            }
            if !saw_selector && (ch == ':' || ch == '"') {
                let selector = self.read_sharp_inline_selector()?;
                current = self.apply_sharp_selectors(current, vec![selector], start)?;
                allows_leading_number = false;
                saw_selector = true;
                continue;
            }
            break;
        }
        if !saw_selector {
            return self.parse_err("sharp accessor expects selector");
        }

        let params = Form::new(FormKind::Vector(vec![arg_form]), start);
        let fn_items = vec![
            Form::new(FormKind::Symbol("fn".into()), start),
            params,
            current,
        ];
        Ok(Form::new(FormKind::List(fn_items), start))
    }

    fn apply_sharp_dot_index(
        &self,
        current: Form,
        index: i64,
        span: Span,
    ) -> Result<Form, CloveError> {
        let key_form = Form::new(FormKind::Int(index), span);
        self.apply_sharp_dot_key(current, key_form, span)
    }

    fn apply_sharp_dot_key(
        &self,
        current: Form,
        key_form: Form,
        span: Span,
    ) -> Result<Form, CloveError> {
        let stage = self.build_oop_index_stage(key_form);
        if let FormKind::List(items) = &current.kind {
            if matches!(
                items.first().map(|form| &form.kind),
                Some(FormKind::Symbol(sym)) if sym == OOP_SYNTAX_SYM
            ) {
                let mut new_items = items.clone();
                new_items.push(stage);
                return Ok(Form::new(FormKind::List(new_items), current.span));
            }
        }
        let mut items = Vec::with_capacity(3);
        items.push(Form::new(
            FormKind::Symbol(OOP_SYNTAX_SYM.to_string()),
            span,
        ));
        items.push(current);
        items.push(stage);
        Ok(Form::new(FormKind::List(items), span))
    }

    fn read_sharp_selector_int(&mut self) -> Result<i64, CloveError> {
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if ch.is_ascii_digit() || ch == '_' {
                buf.push(ch);
                self.advance();
                continue;
            }
            break;
        }
        if buf.is_empty() {
            return self.parse_err("sharp accessor expects integer index");
        }
        let normalized = strip_numeric_separators(&buf);
        let Ok(num) = normalized.parse::<i64>() else {
            return self.parse_err("sharp accessor expects integer index");
        };
        Ok(num)
    }

    fn read_sharp_accessor_selectors(&mut self) -> Result<Vec<SharpSelector>, CloveError> {
        let accessor = self.read_indexer_accessor()?;
        let accessor_span = accessor.span;
        let items = match accessor.kind {
            FormKind::Vector(items) => items,
            _ => unreachable!(),
        };
        if items.is_empty() {
            return self.parse_err("sharp accessor expects selector");
        }
        let groups = self.split_index_groups(items);
        if groups.is_empty() {
            return self.parse_err("sharp accessor expects selector");
        }
        let mut selectors = Vec::with_capacity(groups.len());
        for group in groups {
            let selector = self.parse_sharp_selector_group(group, accessor_span)?;
            selectors.push(selector);
        }
        Ok(selectors)
    }

    fn read_sharp_inline_selector(&mut self) -> Result<SharpSelector, CloveError> {
        let start = self.current_span();
        let form = match self.current_char() {
            ':' => self.read_oop_keyword_segment(start)?,
            '"' => self.read_string(start)?,
            _ => unreachable!(),
        };
        self.parse_sharp_selector_group(vec![form], start)
    }

    fn parse_sharp_selector_group(
        &self,
        items: Vec<Form>,
        _span: Span,
    ) -> Result<SharpSelector, CloveError> {
        if items.is_empty() {
            return self.parse_err("sharp accessor expects selector");
        }
        let mut default_split = None;
        for (idx, item) in items.iter().enumerate() {
            if matches!(item.kind, FormKind::Symbol(ref sym) if sym == "||") {
                default_split = Some(idx);
                break;
            }
        }
        let (path_items, default_form) = if let Some(idx) = default_split {
            if idx == 0 {
                return self.parse_err("default marker '||' requires preceding path");
            }
            if idx + 1 >= items.len() {
                return self.parse_err("default marker '||' requires a value after it");
            }
            if idx + 2 != items.len() {
                return self
                    .parse_err("default marker '||' expects exactly one expression after it");
            }
            (items[..idx].to_vec(), Some(items[idx + 1].clone()))
        } else {
            (items.clone(), None)
        };
        if path_items.is_empty() {
            return self.parse_err("index expression expects at least one path element");
        }
        let mut expanded = Vec::new();
        for item in path_items {
            let mut segments = self.expand_sharp_keyword_path_segments(item)?;
            expanded.append(&mut segments);
        }
        let normalized = self.normalize_indexer_ranges(expanded)?;
        if normalized.is_empty() {
            return self.parse_err("index expression expects at least one path element");
        }
        Ok(SharpSelector {
            path: normalized,
            default: default_form,
        })
    }

    fn apply_sharp_selectors(
        &self,
        target: Form,
        selectors: Vec<SharpSelector>,
        span: Span,
    ) -> Result<Form, CloveError> {
        if selectors.is_empty() {
            return self.parse_err("sharp accessor expects selector");
        }
        if selectors.len() == 1 {
            let selector = selectors.into_iter().next().unwrap();
            let mut expr = self.build_sharp_index_expr(target, selector.path, span)?;
            if let Some(default_val) = selector.default {
                expr = self.wrap_nil_coalesce(expr, default_val, span);
            }
            return Ok(expr);
        }
        let simple = selectors
            .iter()
            .all(|selector| selector.default.is_none() && selector.path.len() == 1);
        if simple {
            let target_span = target.span;
            let mut keys = Vec::with_capacity(selectors.len());
            for selector in selectors {
                keys.push(selector.path.into_iter().next().unwrap());
            }
            let list_items = vec![
                Form::new(FormKind::Symbol(INDEX_GET_MANY_SYM.into()), span),
                target,
                Form::new(FormKind::Vector(keys), span),
            ];
            return Ok(Form::new(FormKind::List(list_items), target_span));
        }
        let mut values = Vec::with_capacity(selectors.len());
        for selector in selectors {
            let mut expr = self.build_sharp_index_expr(target.clone(), selector.path, span)?;
            if let Some(default_val) = selector.default {
                expr = self.wrap_nil_coalesce(expr, default_val, span);
            }
            values.push(expr);
        }
        Ok(Form::new(FormKind::Vector(values), span))
    }

    fn expand_sharp_keyword_path_segments(&self, item: Form) -> Result<Vec<Form>, CloveError> {
        match &item.kind {
            FormKind::Keyword(name) if name.contains(':') => {
                let mut parts = Vec::new();
                let mut buf = String::new();
                let mut chars = name.chars().peekable();
                let mut saw_split = false;
                while let Some(ch) = chars.next() {
                    if ch == ':' {
                        if chars.peek() == Some(&':') {
                            buf.push(':');
                            buf.push(':');
                            chars.next();
                            continue;
                        }
                        if buf.is_empty() {
                            return self.parse_err("keyword path expects non-empty segment");
                        }
                        parts.push(buf);
                        buf = String::new();
                        saw_split = true;
                        continue;
                    }
                    buf.push(ch);
                }
                if saw_split {
                    if buf.is_empty() {
                        return self.parse_err("keyword path expects non-empty segment");
                    }
                    parts.push(buf);
                    let mut out = Vec::with_capacity(parts.len());
                    for part in parts {
                        out.push(Form::new(FormKind::Keyword(part), item.span));
                    }
                    Ok(out)
                } else {
                    Ok(vec![item])
                }
            }
            _ => Ok(vec![item]),
        }
    }

    fn build_sharp_index_expr(
        &self,
        target: Form,
        path_items: Vec<Form>,
        span: Span,
    ) -> Result<Form, CloveError> {
        if path_items.is_empty() {
            return self.parse_err("index expression expects at least one path element");
        }
        let target_span = target.span;
        let mut list_items = Vec::new();
        if path_items.len() == 1 {
            list_items.push(Form::new(FormKind::Symbol(INDEX_GET_SYM.into()), span));
            list_items.push(target);
            list_items.push(path_items[0].clone());
        } else {
            list_items.push(Form::new(FormKind::Symbol(INDEX_GET_IN_SYM.into()), span));
            list_items.push(target);
            list_items.push(Form::new(FormKind::Vector(path_items), span));
        }
        Ok(Form::new(FormKind::List(list_items), target_span))
    }

    fn wrap_nil_coalesce(&self, expr: Form, default_val: Form, span: Span) -> Form {
        let tmp = Form::new(FormKind::Symbol("__sharp_val".into()), span);
        let binding = Form::new(FormKind::Vector(vec![tmp.clone(), expr]), span);
        let cond = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("nil?".into()), span),
                tmp.clone(),
            ]),
            span,
        );
        let if_form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("if".into()), span),
                cond,
                default_val,
                tmp.clone(),
            ]),
            span,
        );
        Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("let".into()), span),
                binding,
                if_form,
            ]),
            span,
        )
    }

    fn read_indexer_accessor(&mut self) -> Result<Form, CloveError> {
        let start = self.current_span();
        self.advance(); // [
        self.container_stack.push(Some(start.index));
        self.last_form_stack.push(None);
        self.pending_blank_stack.push(false);
        self.since_newline_has_content_stack.push(true);
        let mut items = Vec::new();
        self.skip_ws_and_comments();
        while !self.eof() && self.current_char() != ']' {
            if self.current_char() == '.' && self.peek_char() == Some('.') {
                let span = self.current_span();
                self.advance();
                self.advance();
                let mut symbol = "..";
                if self.current_char() == '.' {
                    self.advance();
                    symbol = "...";
                }
                let form = Form::new(FormKind::Symbol(symbol.into()), span);
                self.mark_last_form_index(form.span.index);
                items.push(form);
            } else {
                let form = self.read_form()?;
                self.mark_last_form_index(form.span.index);
                items.push(form);
            }
            self.skip_ws_and_comments();
        }
        if self.eof() {
            self.container_stack.pop();
            self.last_form_stack.pop();
            self.pending_blank_stack.pop();
            self.since_newline_has_content_stack.pop();
            return self.parse_err("unterminated vector");
        }
        self.advance(); // ]
        self.container_stack.pop();
        self.last_form_stack.pop();
        self.pending_blank_stack.pop();
        self.since_newline_has_content_stack.pop();
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::Vector(items), start))
    }

    fn lower_index_sugar(&self, target: Form, accessor: Form) -> Result<Form, CloveError> {
        let accessor_span = accessor.span;
        let items = match accessor.kind {
            FormKind::Vector(items) => items,
            _ => unreachable!(),
        };
        if items.is_empty() {
            return self.parse_err("index expression expects at least one path element");
        }

        let mut default_split = None;
        for (idx, item) in items.iter().enumerate() {
            if matches!(item.kind, FormKind::Symbol(ref sym) if sym == "||") {
                default_split = Some(idx);
                break;
            }
        }
        let (path_items, default_form) = if let Some(idx) = default_split {
            if idx == 0 {
                return self.parse_err("default marker '||' requires preceding path");
            }
            if idx + 1 >= items.len() {
                return self.parse_err("default marker '||' requires a value after it");
            }
            if idx + 2 != items.len() {
                return self
                    .parse_err("default marker '||' expects exactly one expression after it");
            }
            (items[..idx].to_vec(), Some(items[idx + 1].clone()))
        } else {
            (items.clone(), None)
        };

        let accessor_has_commas = path_items
            .iter()
            .any(|item| self.comma_trailing.contains(&item.span.index));

        if path_items.is_empty() {
            return self.parse_err("index expression expects at least one path element");
        }

        let mut list_items = Vec::new();
        let target_span = target.span;
        if accessor_has_commas {
            let mut grouped = Vec::new();
            let groups = self.split_index_groups(path_items);
            for group in groups {
                let normalized = self.normalize_indexer_ranges(group)?;
                if normalized.is_empty() {
                    return self.parse_err("index expression expects at least one path element");
                }
                if normalized.len() == 1 {
                    grouped.push(normalized.into_iter().next().unwrap());
                } else {
                    grouped.push(Form::new(FormKind::Vector(normalized), accessor_span));
                }
            }
            list_items.push(Form::new(
                FormKind::Symbol(INDEX_GET_MANY_SYM.into()),
                accessor_span,
            ));
            list_items.push(target);
            list_items.push(Form::new(FormKind::Vector(grouped), accessor_span));
            if let Some(default_val) = default_form {
                list_items.push(default_val);
            }
        } else {
            let path_items = self.normalize_indexer_ranges(path_items)?;
            if path_items.is_empty() {
                return self.parse_err("index expression expects at least one path element");
            }
            if path_items.len() == 1 {
                list_items.push(Form::new(
                    FormKind::Symbol(INDEX_GET_SYM.into()),
                    accessor_span,
                ));
                list_items.push(target);
                list_items.push(path_items[0].clone());
                if let Some(default_val) = default_form {
                    list_items.push(default_val);
                }
            } else {
                list_items.push(Form::new(
                    FormKind::Symbol(INDEX_GET_IN_SYM.into()),
                    accessor_span,
                ));
                list_items.push(target);
                list_items.push(Form::new(
                    FormKind::Vector(path_items.clone()),
                    accessor_span,
                ));
                if let Some(default_val) = default_form {
                    list_items.push(default_val);
                }
            }
        }

        Ok(Form::new(FormKind::List(list_items), target_span))
    }

    fn split_index_groups(&self, items: Vec<Form>) -> Vec<Vec<Form>> {
        let mut groups = Vec::new();
        let mut current = Vec::new();
        for item in items {
            let comma_after = self.comma_trailing.contains(&item.span.index);
            current.push(item);
            if comma_after {
                if !current.is_empty() {
                    groups.push(current);
                }
                current = Vec::new();
            }
        }
        if !current.is_empty() {
            groups.push(current);
        }
        groups
    }

    fn normalize_indexer_ranges(&self, items: Vec<Form>) -> Result<Vec<Form>, CloveError> {
        let mut out = Vec::new();
        let mut idx = 0;
        while idx < items.len() {
            let item = items[idx].clone();
            if let FormKind::Symbol(sym) = &item.kind {
                if sym == ".." || sym == "..." {
                    let exclusive = sym == "...";
                    let start = out.pop();
                    let end = if idx + 1 < items.len() {
                        Some(items[idx + 1].clone())
                    } else {
                        None
                    };
                    out.push(self.build_range_form(start, end, exclusive, item.span));
                    idx += if idx + 1 < items.len() { 2 } else { 1 };
                    continue;
                }
                if let Some((start_raw, end_raw, exclusive)) = split_range_symbol(sym) {
                    let mut start = None;
                    if start_raw.is_none() && out.last().is_some() && sym.starts_with("..") {
                        start = out.pop();
                    }
                    if start.is_none() {
                        start = match start_raw {
                            Some(raw) => Some(self.parse_range_bound_form(raw, item.span)?),
                            None => None,
                        };
                    }
                    let end = match end_raw {
                        Some(raw) => Some(self.parse_range_bound_form(raw, item.span)?),
                        None => None,
                    };
                    out.push(self.build_range_form(start, end, exclusive, item.span));
                    idx += 1;
                    continue;
                }
            }
            out.push(item);
            idx += 1;
        }
        Ok(out)
    }

    fn parse_range_bound_form(&self, raw: &str, span: Span) -> Result<Form, CloveError> {
        let mut reader = Reader::new_with_options(raw, self.options.clone());
        let mut forms = reader.read_all()?;
        if forms.len() != 1 {
            let msg = format!("range bound expects single expression: {}", raw);
            return Err(CloveError::parse(msg).with_span(span));
        }
        Ok(forms.remove(0))
    }

    fn build_range_form(
        &self,
        start: Option<Form>,
        end: Option<Form>,
        exclusive: bool,
        span: Span,
    ) -> Form {
        let mut items = Vec::with_capacity(4);
        items.push(Form::new(FormKind::Symbol(RANGE_LITERAL_SYM.into()), span));
        items.push(start.unwrap_or_else(|| Form::new(FormKind::Nil, span)));
        items.push(end.unwrap_or_else(|| Form::new(FormKind::Nil, span)));
        items.push(Form::new(FormKind::Bool(exclusive), span));
        Form::new(FormKind::List(items), span)
    }

    fn lower_call_sugar(&mut self, base: Form) -> Result<Form, CloveError> {
        if self.eof() || self.current_char() != '(' {
            return Ok(base);
        }
        if !matches!(&base.kind, FormKind::Symbol(_)) {
            return Ok(base);
        }
        if matches!(&base.kind, FormKind::Symbol(sym) if sym == "*") {
            return Ok(base);
        }
        let base_span = base.span;
        let args_form = self.read_list()?;
        let FormKind::List(mut items) = args_form.kind else {
            return Ok(base);
        };
        let mut call_items = Vec::with_capacity(items.len() + 1);
        call_items.push(base);
        call_items.append(&mut items);
        let call_form = Form::new(FormKind::List(call_items), base_span);
        if !self.eof() && self.current_char() == '(' {
            return self.parse_err("chained call sugar is not supported yet");
        }
        Ok(call_form)
    }

    fn lower_compose_sugar(&self, base: Form) -> Result<Form, CloveError> {
        let FormKind::List(items) = &base.kind else {
            return Ok(base);
        };
        if items.len() < 2 {
            return Ok(base);
        }
        let head_is_dot = matches!(&items[0].kind, FormKind::Symbol(sym) if sym == ".");
        let tail_is_dot =
            matches!(&items[items.len() - 1].kind, FormKind::Symbol(sym) if sym == ".");
        if head_is_dot && tail_is_dot {
            return self.parse_err("compose form cannot have '.' at both head and tail");
        }
        if head_is_dot {
            let mut out = Vec::with_capacity(items.len());
            out.push(Form::new(FormKind::Symbol("comp".into()), items[0].span));
            out.extend(items.iter().skip(1).rev().cloned());
            return Ok(Form::new(FormKind::List(out), base.span));
        }
        if tail_is_dot {
            let mut out = Vec::with_capacity(items.len());
            out.push(Form::new(
                FormKind::Symbol("comp".into()),
                items[items.len() - 1].span,
            ));
            out.extend(items[..items.len() - 1].iter().cloned());
            return Ok(Form::new(FormKind::List(out), base.span));
        }
        Ok(base)
    }

    fn lower_dot_chain(&mut self, base: Form) -> Result<Form, CloveError> {
        let mut stages = Vec::new();
        loop {
            if self.eof() {
                break;
            }
            if self.current_char() == '.' && self.peek_char() == Some('(') {
                self.advance(); // consume '.'
                let stage = self.read_list()?;
                stages.push(stage);
                continue;
            }
            if self.peek_line_start_dot_chain() {
                self.skip_ws_and_comments();
                if self.current_char() == '.' && self.peek_char() == Some('(') {
                    self.advance(); // consume '.'
                    let stage = self.read_list()?;
                    stages.push(stage);
                    continue;
                }
            }
            break;
        }
        if stages.is_empty() {
            return Ok(base);
        }
        if !self.eof() && matches!(self.current_char(), '.' | ':' | '?') {
            let mut oop_stages = Vec::new();
            for stage in stages {
                oop_stages.push(self.convert_dot_stage_to_oop(stage)?);
            }
            oop_stages.extend(self.read_oop_chain_stages()?);
            if oop_stages
                .last()
                .map(|stage| self.is_oop_nil_safe_marker(stage))
                .unwrap_or(false)
            {
                return self.parse_err("nil-safe chain requires a stage after '?.'");
            }
            let base_span = base.span;
            let base = match base.kind {
                FormKind::List(items) if items.len() == 1 => {
                    if matches!(&items[0].kind, FormKind::Symbol(_)) {
                        Form::new(FormKind::List(items), base_span)
                    } else {
                        items[0].clone()
                    }
                }
                other => Form::new(other, base_span),
            };
            let mut items = Vec::with_capacity(oop_stages.len() + 2);
            items.push(Form::new(
                FormKind::Symbol(OOP_SYNTAX_SYM.to_string()),
                base_span,
            ));
            items.push(base);
            items.extend(oop_stages);
            return Ok(Form::new(FormKind::List(items), base_span));
        }
        self.build_dot_chain(base, stages)
    }

    fn peek_line_start_dot_chain(&self) -> bool {
        let mut idx = self.index;
        let mut saw_newline = false;
        while idx < self.chars.len() {
            let ch = self.chars[idx];
            if ch == ';' {
                while idx < self.chars.len() && self.chars[idx] != '\n' {
                    idx += 1;
                }
                continue;
            }
            if ch == '\n' {
                saw_newline = true;
                idx += 1;
                continue;
            }
            if ch.is_whitespace() || ch == ',' {
                idx += 1;
                continue;
            }
            if !saw_newline {
                return false;
            }
            return ch == '.' && self.chars.get(idx + 1).copied() == Some('(');
        }
        false
    }

    fn build_dot_chain(&self, base: Form, stages: Vec<Form>) -> Result<Form, CloveError> {
        let base_span = base.span;
        let placeholder = format!("{}{}_auto", DOT_CHAIN_PLACEHOLDER_PREFIX, base_span.index);
        let mut chain_items = Vec::with_capacity(stages.len() + 3);
        chain_items.push(Form::new(FormKind::Symbol("as->".into()), base_span));
        chain_items.push(base);
        chain_items.push(Form::new(FormKind::Symbol(placeholder.clone()), base_span));
        for stage in stages {
            if let FormKind::List(stage_items) = &stage.kind {
                if let Some(FormKind::Symbol(head)) = stage_items.first().map(|f| &f.kind) {
                    match head.as_str() {
                        "as" => {
                            if stage_items.len() != 2 {
                                return self.parse_err("dot as stage expects single symbol");
                            }
                            if !matches!(stage_items[1].kind, FormKind::Symbol(_)) {
                                return self.parse_err("dot as stage expects single symbol");
                            }
                            chain_items.push(stage);
                            continue;
                        }
                        "let" => {
                            if stage_items.len() != 2 {
                                return self.parse_err("dot let stage expects single symbol");
                            }
                            let sym = match &stage_items[1].kind {
                                FormKind::Symbol(sym) => sym,
                                _ => {
                                    return self.parse_err("dot let stage expects single symbol");
                                }
                            };
                            if sym.starts_with('*') {
                                return self.parse_err("dot let stage expects non-star symbol");
                            }
                            chain_items.push(stage);
                            continue;
                        }
                        "repl" | "debug" => {
                            let (_converted, used_placeholder) =
                                self.transform_dot_form(&stage, &placeholder)?;
                            if !used_placeholder {
                                if stage_items.len() == 2 {
                                    if let FormKind::Symbol(sym) = &stage_items[1].kind {
                                        if !sym.starts_with('*') {
                                            let let_stage = Form::new(
                                                FormKind::List(vec![
                                                    Form::new(
                                                        FormKind::Symbol("let".into()),
                                                        stage.span,
                                                    ),
                                                    stage_items[1].clone(),
                                                ]),
                                                stage.span,
                                            );
                                            chain_items.push(let_stage);
                                            let repl_stage = Form::new(
                                                FormKind::List(vec![
                                                    Form::new(
                                                        FormKind::Symbol(head.clone()),
                                                        stage.span,
                                                    ),
                                                    Form::new(
                                                        FormKind::Symbol(placeholder.clone()),
                                                        stage.span,
                                                    ),
                                                ]),
                                                stage.span,
                                            );
                                            chain_items.push(repl_stage);
                                            continue;
                                        }
                                    }
                                }
                                let mut repl_items = Vec::with_capacity(stage_items.len() + 1);
                                repl_items
                                    .push(Form::new(FormKind::Symbol(head.clone()), stage.span));
                                if stage_items.len() == 1 {
                                    repl_items.push(Form::new(
                                        FormKind::Symbol(placeholder.clone()),
                                        stage.span,
                                    ));
                                } else {
                                    repl_items.extend(stage_items.iter().skip(1).cloned());
                                }
                                chain_items.push(Form::new(FormKind::List(repl_items), stage.span));
                                continue;
                            }
                        }
                        _ => {}
                    }
                }
            }
            let (converted, used_placeholder) = self.transform_dot_form(&stage, &placeholder)?;
            if !used_placeholder {
                return self.parse_err("dot pipeline stage requires '?' or '*?' placeholder");
            }
            chain_items.push(converted);
        }
        Ok(Form::new(FormKind::List(chain_items), base_span))
    }

    fn convert_dot_stage_to_oop(&self, stage: Form) -> Result<Form, CloveError> {
        if let FormKind::List(stage_items) = &stage.kind {
            if let Some(FormKind::Symbol(head)) = stage_items.first().map(|f| &f.kind) {
                match head.as_str() {
                    "as" => {
                        if stage_items.len() != 2 {
                            return self.parse_err("oop as stage expects single symbol");
                        }
                        let sym = match &stage_items[1].kind {
                            FormKind::Symbol(sym) => sym.clone(),
                            _ => {
                                return self.parse_err("oop as stage expects single symbol");
                            }
                        };
                        let span = stage.span;
                        let stage = Form::new(
                            FormKind::List(vec![
                                Form::new(FormKind::Symbol(OOP_AS_SYM.to_string()), span),
                                Form::new(FormKind::Symbol(sym), span),
                            ]),
                            span,
                        );
                        return Ok(stage);
                    }
                    "let" => {
                        if stage_items.len() != 2 {
                            return self.parse_err("oop let stage expects single symbol");
                        }
                        let sym = match &stage_items[1].kind {
                            FormKind::Symbol(sym) => sym.clone(),
                            _ => {
                                return self.parse_err("oop let stage expects single symbol");
                            }
                        };
                        if sym.starts_with('*') {
                            return self.parse_err("oop let stage expects non-star symbol");
                        }
                        let span = stage.span;
                        let stage = Form::new(
                            FormKind::List(vec![
                                Form::new(FormKind::Symbol(OOP_LET_SYM.to_string()), span),
                                Form::new(FormKind::Symbol(sym), span),
                            ]),
                            span,
                        );
                        return Ok(stage);
                    }
                    "repl" | "debug" => {
                        let placeholder =
                            format!("{}{}_oop", DOT_CHAIN_PLACEHOLDER_PREFIX, stage.span.index);
                        let (_converted, used_placeholder) =
                            self.transform_dot_form(&stage, &placeholder)?;
                        if !used_placeholder {
                            return Ok(stage);
                        }
                    }
                    _ => {}
                }
            }
        }
        self.build_oop_dot_stage(stage)
    }

    fn read_oop_chain_stages(&mut self) -> Result<Vec<Form>, CloveError> {
        let mut stages = Vec::new();
        while !self.eof() {
            match self.current_char() {
                '?' => {
                    if !self.peek_dot_after_ws() {
                        break;
                    }
                    let marker_span = self.current_span();
                    self.advance(); // consume '?'
                    self.skip_ws();
                    stages.push(self.build_oop_nil_safe_marker(marker_span));
                }
                '.' => match self.read_oop_stage()? {
                    Some(stage) => stages.push(stage),
                    None => break,
                },
                ':' => match self.read_oop_colon_stage()? {
                    Some(stage) => stages.push(stage),
                    None => break,
                },
                _ => break,
            }
        }
        Ok(stages)
    }

    fn transform_dot_form(
        &self,
        form: &Form,
        placeholder: &str,
    ) -> Result<(Form, bool), CloveError> {
        match &form.kind {
            FormKind::Symbol(sym) => {
                if sym == "?" {
                    return Ok((
                        Form::new(FormKind::Symbol(placeholder.to_string()), form.span),
                        true,
                    ));
                }
                if sym == "*?" {
                    return self.parse_err("spread placeholder '*?' is only allowed inside lists");
                }
                Ok((form.clone(), false))
            }
            FormKind::List(items) => self.transform_dot_list(items, placeholder, form.span),
            FormKind::Vector(items) => {
                let mut converted = Vec::with_capacity(items.len());
                let mut used = false;
                for item in items {
                    let (inner, inner_used) = self.transform_dot_form(item, placeholder)?;
                    used |= inner_used;
                    converted.push(inner);
                }
                Ok((Form::new(FormKind::Vector(converted), form.span), used))
            }
            FormKind::Map(entries) => {
                let mut converted = Vec::with_capacity(entries.len());
                let mut used = false;
                for entry in entries {
                    match entry {
                        MapItem::KeyValue(k, v) => {
                            let (new_k, used_k) = self.transform_dot_form(k, placeholder)?;
                            let (new_v, used_v) = self.transform_dot_form(v, placeholder)?;
                            used |= used_k || used_v;
                            converted.push(MapItem::KeyValue(new_k, new_v));
                        }
                        MapItem::Spread(expr) => {
                            let (new_expr, used_expr) =
                                self.transform_dot_form(expr, placeholder)?;
                            used |= used_expr;
                            converted.push(MapItem::Spread(new_expr));
                        }
                    }
                }
                Ok((Form::new(FormKind::Map(converted), form.span), used))
            }
            FormKind::Set(items) => {
                let mut converted = Vec::with_capacity(items.len());
                let mut used = false;
                for item in items {
                    let (inner, inner_used) = self.transform_dot_form(item, placeholder)?;
                    used |= inner_used;
                    converted.push(inner);
                }
                Ok((Form::new(FormKind::Set(converted), form.span), used))
            }
            FormKind::ShortFn(_) => {
                // Short fn uses its own '?' placeholders; don't rewrite for dot pipeline.
                Ok((form.clone(), false))
            }
            _ => Ok((form.clone(), false)),
        }
    }

    fn transform_dot_list(
        &self,
        items: &[Form],
        placeholder: &str,
        span: Span,
    ) -> Result<(Form, bool), CloveError> {
        if items.is_empty() {
            return self.parse_err("dot pipeline stage cannot be empty");
        }
        let mut converted = Vec::with_capacity(items.len());
        let mut used_placeholder = false;
        let mut spread_pos: Option<usize> = None;
        for (idx, item) in items.iter().enumerate() {
            match &item.kind {
                FormKind::Symbol(sym) if sym == "?" => {
                    used_placeholder = true;
                    converted.push(Form::new(
                        FormKind::Symbol(placeholder.to_string()),
                        item.span,
                    ));
                }
                FormKind::Symbol(sym) if sym == "*?" => {
                    used_placeholder = true;
                    if spread_pos.is_some() {
                        return self
                            .parse_err("dot pipeline stage allows only one '*?' placeholder");
                    }
                    spread_pos = Some(idx);
                    converted.push(Form::new(
                        FormKind::Symbol(placeholder.to_string()),
                        item.span,
                    ));
                }
                _ => {
                    let (inner, inner_used) = self.transform_dot_form(item, placeholder)?;
                    used_placeholder |= inner_used;
                    converted.push(inner);
                }
            }
        }
        if let Some(pos) = spread_pos {
            if pos + 1 != converted.len() {
                return self.parse_err("spread placeholder '*?' must be the final argument");
            }
            if converted.len() < 2 {
                return self.parse_err("spread placeholder '*?' requires a function target");
            }
            let mut apply_items = Vec::with_capacity(converted.len() + 1);
            apply_items.push(Form::new(FormKind::Symbol("apply".into()), span));
            apply_items.extend(converted);
            return Ok((
                Form::new(FormKind::List(apply_items), span),
                used_placeholder,
            ));
        }
        Ok((Form::new(FormKind::List(converted), span), used_placeholder))
    }

    fn lower_oop_chain(&mut self, base: Form) -> Result<Form, CloveError> {
        if self.eof() || !matches!(self.current_char(), '.' | ':' | '?') {
            return Ok(base);
        }
        let base_span = base.span;
        let base = match base.kind {
            FormKind::List(items) if items.len() == 1 => {
                if matches!(&items[0].kind, FormKind::Symbol(_)) {
                    Form::new(FormKind::List(items), base_span)
                } else {
                    items[0].clone()
                }
            }
            other => Form::new(other, base_span),
        };
        let mut stages = Vec::new();
        stages.extend(self.read_oop_chain_stages()?);
        if stages.is_empty() {
            return Ok(base);
        }
        if stages
            .last()
            .map(|stage| self.is_oop_nil_safe_marker(stage))
            .unwrap_or(false)
        {
            return self.parse_err("nil-safe chain requires a stage after '?.'");
        }
        let mut items = Vec::with_capacity(stages.len() + 2);
        items.push(Form::new(
            FormKind::Symbol(OOP_SYNTAX_SYM.to_string()),
            base_span,
        ));
        items.push(base);
        items.extend(stages);
        Ok(Form::new(FormKind::List(items), base_span))
    }

    fn build_oop_nil_safe_marker(&self, span: Span) -> Form {
        Form::new(
            FormKind::List(vec![Form::new(
                FormKind::Symbol(OOP_NIL_SAFE_SYM.to_string()),
                span,
            )]),
            span,
        )
    }

    fn is_oop_nil_safe_marker(&self, stage: &Form) -> bool {
        match &stage.kind {
            FormKind::List(items) if items.len() == 1 => {
                matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM)
            }
            _ => false,
        }
    }

    fn read_oop_stage(&mut self) -> Result<Option<Form>, CloveError> {
        if self.current_char() != '.' {
            return Ok(None);
        }
        let next = self.peek_char();
        if next == Some('(') {
            self.advance(); // consume '.'
            let stage_form = self.read_list()?;
            if let FormKind::List(items) = &stage_form.kind {
                if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                    match head.as_str() {
                        "as" => {
                            if items.len() != 2 {
                                return self.parse_err("oop as stage expects single symbol");
                            }
                            let sym = match &items[1].kind {
                                FormKind::Symbol(sym) => sym.clone(),
                                _ => {
                                    return self.parse_err("oop as stage expects single symbol");
                                }
                            };
                            let span = stage_form.span;
                            let stage = Form::new(
                                FormKind::List(vec![
                                    Form::new(FormKind::Symbol(OOP_AS_SYM.to_string()), span),
                                    Form::new(FormKind::Symbol(sym), span),
                                ]),
                                span,
                            );
                            return Ok(Some(stage));
                        }
                        "let" => {
                            if items.len() != 2 {
                                return self.parse_err("oop let stage expects single symbol");
                            }
                            let sym = match &items[1].kind {
                                FormKind::Symbol(sym) => sym.clone(),
                                _ => {
                                    return self.parse_err("oop let stage expects single symbol");
                                }
                            };
                            if sym.starts_with('*') {
                                return self.parse_err("oop let stage expects non-star symbol");
                            }
                            let span = stage_form.span;
                            let stage = Form::new(
                                FormKind::List(vec![
                                    Form::new(FormKind::Symbol(OOP_LET_SYM.to_string()), span),
                                    Form::new(FormKind::Symbol(sym), span),
                                ]),
                                span,
                            );
                            return Ok(Some(stage));
                        }
                        "repl" | "debug" => {
                            let placeholder = format!(
                                "{}{}_oop",
                                DOT_CHAIN_PLACEHOLDER_PREFIX, stage_form.span.index
                            );
                            let (_converted, used_placeholder) =
                                self.transform_dot_form(&stage_form, &placeholder)?;
                            if !used_placeholder {
                                return Ok(Some(stage_form));
                            }
                        }
                        _ => {}
                    }
                }
            }
            let stage = self.build_oop_dot_stage(stage_form)?;
            return Ok(Some(stage));
        }
        if next == Some(':') {
            self.advance(); // consume '.'
            let key_span = self.current_span();
            let key_form = self.read_oop_keyword_segment(key_span)?;
            return Ok(Some(self.build_oop_index_stage(key_form)));
        }
        if next == Some('"') {
            self.advance(); // consume '.'
            let key_span = self.current_span();
            let key_form = self.read_string(key_span)?;
            return Ok(Some(self.build_oop_index_stage(key_form)));
        }
        if next == Some('\'') {
            self.advance(); // consume '.'
            let key_span = self.current_span();
            let key_form = self.read_oop_quoted_symbol_segment(key_span)?;
            return Ok(Some(self.build_oop_index_stage(key_form)));
        }
        if next == Some('#') {
            self.advance(); // consume '.'
            let hash_span = self.current_span();
            let dispatch_form = self.read_dispatch(hash_span)?;
            let regex_span = dispatch_form.span;
            match &dispatch_form.kind {
                FormKind::Regex { .. } | FormKind::InterpolatedRegex { .. } => {
                    let stage_form = Form::new(
                        FormKind::List(vec![
                            dispatch_form,
                            Form::new(FormKind::Symbol("?".into()), regex_span),
                        ]),
                        regex_span,
                    );
                    let stage = self.build_oop_dot_stage(stage_form)?;
                    return Ok(Some(stage));
                }
                _ => {
                    return self.parse_err("oop dot regex stage expects #/.../ after '.'");
                }
            }
        }
        if !matches!(next, Some(ch) if is_oop_ident_char(ch)) {
            return Ok(None);
        }
        self.advance(); // consume '.'
        let method_span = self.current_span();
        let name = self.read_oop_method_name()?;
        let has_args = !self.eof() && self.current_char() == '(';
        if let Some(index) = parse_oop_index_literal(&name) {
            if !has_args {
                let key_form = Form::new(FormKind::Int(index), method_span);
                return Ok(Some(self.build_oop_index_stage(key_form)));
            }
        }
        let mut stage_items = Vec::new();
        let span = if has_args {
            stage_items.push(Form::new(FormKind::Symbol(name), method_span));
            let args_form = self.read_list()?;
            let args = match args_form.kind {
                FormKind::List(items) => items,
                _ => unreachable!(),
            };
            stage_items.extend(args);
            Span {
                line: method_span.line,
                col: method_span.col,
                index: method_span.index,
            }
        } else {
            stage_items.push(Form::new(
                FormKind::Symbol(OOP_BARE_SYM.to_string()),
                method_span,
            ));
            stage_items.push(Form::new(FormKind::Symbol(name), method_span));
            method_span
        };
        Ok(Some(Form::new(FormKind::List(stage_items), span)))
    }

    fn read_oop_colon_stage(&mut self) -> Result<Option<Form>, CloveError> {
        if self.current_char() != ':' {
            return Ok(None);
        }
        if !matches!(self.peek_char(), Some(ch) if is_kw_seg_start(ch)) {
            return Ok(None);
        }
        let key_span = self.current_span();
        let key_form = self.read_oop_keyword_sugar_segment(key_span)?;
        Ok(Some(self.build_oop_index_stage(key_form)))
    }

    fn build_oop_dot_stage(&self, stage: Form) -> Result<Form, CloveError> {
        let span = stage.span;
        let placeholder = format!("{}{}_oop", DOT_CHAIN_PLACEHOLDER_PREFIX, span.index);
        let (converted, used_placeholder) = self.transform_dot_form(&stage, &placeholder)?;
        if !used_placeholder {
            return self.parse_err("dot pipeline stage requires '?' or '*?' placeholder");
        }
        let mut items = Vec::with_capacity(3);
        items.push(Form::new(
            FormKind::Symbol(OOP_DOT_STAGE_SYM.to_string()),
            span,
        ));
        items.push(Form::new(FormKind::Symbol(placeholder), span));
        items.push(converted);
        Ok(Form::new(FormKind::List(items), span))
    }

    fn build_oop_index_stage(&self, key: Form) -> Form {
        // Separate spans so stage/key don't collide in span->type for inference.
        let span = self.current_span();
        let items = vec![
            Form::new(FormKind::Symbol(OOP_INDEX_SYM.to_string()), span),
            key,
        ];
        Form::new(FormKind::List(items), span)
    }

    fn read_oop_keyword_segment(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // :
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch) || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | ';' | '.') {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        if buf.contains('/') {
            return self.parse_err(
                "namespace separator '/' has been removed; use '::' (e.g. foo::bar). If you meant a regex literal, use #/.../",
            );
        }
        Ok(Form::new(FormKind::Keyword(buf), start))
    }

    fn read_oop_keyword_sugar_segment(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // :
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | ';' | '.' | ':')
            {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        if buf.is_empty() {
            return self.parse_err("expected keyword segment after ':'");
        }
        if buf.contains('/') {
            return self.parse_err(
                "namespace separator '/' has been removed; use '::' (e.g. foo::bar). If you meant a regex literal, use #/.../",
            );
        }
        Ok(Form::new(FormKind::Keyword(buf), start))
    }

    fn read_map_ref_parent_segments(&mut self) -> Result<Vec<Form>, CloveError> {
        let mut segments = Vec::new();
        while !self.eof() && self.current_char() == '.' {
            let seg_span = self.current_span();
            self.advance(); // '.'
            if self.eof() || self.current_char() != '.' {
                return self.parse_err("map ref parent expects '../'");
            }
            self.advance(); // '.'
            if self.eof() || self.current_char() != '/' {
                return self.parse_err("map ref parent expects '../'");
            }
            self.advance(); // '/'
            segments.push(Form::new(FormKind::Symbol("..".into()), seg_span));
        }
        Ok(segments)
    }

    fn read_oop_quoted_symbol_segment(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // '
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';' | '.')
            {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        if buf.is_empty() {
            return self.parse_err("expected symbol after \"'\"");
        }
        let quote_form = Form::new(FormKind::Symbol("quote".into()), start);
        let sym_form = Form::new(FormKind::Symbol(buf), start);
        Ok(Form::new(FormKind::List(vec![quote_form, sym_form]), start))
    }

    fn read_oop_method_name(&mut self) -> Result<String, CloveError> {
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';')
                || ch == '.'
            {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        if buf.is_empty() {
            return self.parse_err("expected method name after '.'");
        }
        Ok(buf)
    }

    fn read_map(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // {
        self.container_stack.push(Some(start.index));
        self.last_form_stack.push(None);
        self.pending_blank_stack.push(false);
        self.since_newline_has_content_stack.push(true);
        let mut entries = Vec::new();
        self.skip_ws_and_comments();
        while !self.eof() && self.current_char() != '}' {
            let key_form = self.read_form()?;
            let next_non_ws = self.peek_non_ws_char_including_comma();
            if let Some(spread_expr) = self.consume_map_spread(&key_form)? {
                entries.push(MapItem::Spread(spread_expr));
                if let Some(last) = entries.last() {
                    if let MapItem::Spread(expr) = last {
                        self.mark_last_form_index(expr.span.index);
                    }
                }
                self.skip_ws_and_comments();
                continue;
            }

            let mut key_form = key_form;
            // Convert Ruby label-style keys (`foo:`) into keywords.
            if let FormKind::Symbol(sym) = &key_form.kind {
                if is_label_style_key(sym) {
                    let name = sym.trim_end_matches(':').to_string();
                    key_form.kind = FormKind::Keyword(name);
                }
            }
            self.mark_last_form_index(key_form.span.index);

            let is_special_key = matches!(
                &key_form.kind,
                FormKind::Keyword(kw) if kw == "keys" || kw == "as"
            );

            if !is_special_key && matches!(next_non_ws, Some('}') | Some(',')) {
                if let Some((key_form, value_form)) = self.map_shorthand_entry(key_form.clone())? {
                    self.mark_last_form_index(value_form.span.index);
                    // Commas are treated as whitespace; consume any leftover.
                    self.skip_ws_and_comments();
                    if !self.eof() && self.current_char() == ',' {
                        self.record_trailing_comma();
                        self.advance();
                    }
                    self.skip_ws_and_comments();
                    entries.push(MapItem::KeyValue(key_form, value_form));
                    continue;
                }
            }

            self.skip_ws_and_comments();
            if self.eof() {
                self.container_stack.pop();
                self.last_form_stack.pop();
                self.pending_blank_stack.pop();
                self.since_newline_has_content_stack.pop();
                return self.parse_err("unterminated map");
            }
            if self.current_char() == '}' {
                self.container_stack.pop();
                self.last_form_stack.pop();
                self.pending_blank_stack.pop();
                self.since_newline_has_content_stack.pop();
                return self.parse_err("map literal expects key/value pairs");
            }
            let value_form = self.read_form()?;
            self.mark_last_form_index(value_form.span.index);
            entries.push(MapItem::KeyValue(key_form, value_form));
            self.skip_ws_and_comments();
        }
        if self.eof() {
            self.container_stack.pop();
            self.last_form_stack.pop();
            self.pending_blank_stack.pop();
            self.since_newline_has_content_stack.pop();
            return self.parse_err("unterminated map");
        }
        self.advance(); // }
        self.container_stack.pop();
        self.last_form_stack.pop();
        self.pending_blank_stack.pop();
        self.since_newline_has_content_stack.pop();
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::Map(entries), start))
    }

    fn peek_non_ws_char_including_comma(&self) -> Option<char> {
        let mut idx = self.index;
        while let Some(&ch) = self.chars.get(idx) {
            if ch == ';' {
                while let Some(&next) = self.chars.get(idx) {
                    idx += 1;
                    if next == '\n' {
                        break;
                    }
                }
                continue;
            }
            if is_ws(ch) {
                idx += 1;
                continue;
            }
            return Some(ch);
        }
        None
    }

    fn map_shorthand_entry(&self, key_form: Form) -> Result<Option<(Form, Form)>, CloveError> {
        let key_span = key_form.span;
        let (raw, key_is_keyword) = match &key_form.kind {
            FormKind::Keyword(name) => (name.clone(), true),
            FormKind::Symbol(sym) => {
                if is_label_style_key(sym) {
                    (sym.trim_end_matches(':').to_string(), true)
                } else {
                    (sym.clone(), true)
                }
            }
            _ => return self.parse_err("map shorthand requires keyword or symbol key"),
        };

        if raw.is_empty() {
            return self.parse_err("map shorthand requires a key name");
        }

        let key_form = if key_is_keyword {
            Form::new(FormKind::Keyword(raw.clone()), key_span)
        } else {
            Form::new(FormKind::Symbol(raw.clone()), key_span)
        };
        let value_form = Form::new(FormKind::Symbol(raw), key_span);
        Ok(Some((key_form, value_form)))
    }

    fn consume_map_spread(&mut self, key_form: &Form) -> Result<Option<Form>, CloveError> {
        match &key_form.kind {
            FormKind::Symbol(sym) if sym == "*" => {
                if self.eof() || self.current_char() == '}' {
                    return self.parse_err("map spread '*' requires an expression");
                }
                let expr = self.read_form()?;
                Ok(Some(expr))
            }
            FormKind::Symbol(sym) => {
                if let Some(inner) = strip_spread_symbol(sym) {
                    Ok(Some(Form::new(FormKind::Symbol(inner), key_form.span)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn read_set(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // consume '{'
        let mut items = Vec::new();
        self.container_stack.push(Some(start.index));
        self.last_form_stack.push(None);
        self.pending_blank_stack.push(false);
        self.since_newline_has_content_stack.push(true);
        self.skip_ws_and_comments();

        while !self.eof() && self.current_char() != '}' {
            let form = self.read_form()?;
            self.mark_last_form_index(form.span.index);
            items.push(form);
            self.skip_ws_and_comments();
        }
        if self.eof() {
            self.container_stack.pop();
            self.last_form_stack.pop();
            self.pending_blank_stack.pop();
            self.since_newline_has_content_stack.pop();
            return self.parse_err("unterminated set");
        }
        self.advance(); // }
        self.container_stack.pop();
        self.last_form_stack.pop();
        self.pending_blank_stack.pop();
        self.since_newline_has_content_stack.pop();
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::Set(items), start))
    }

    fn read_deref_form(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // '@'
        let target = self.read_form()?;
        let deref_symbol = Form::new(FormKind::Symbol("deref".into()), start);
        Ok(Form::new(FormKind::List(vec![deref_symbol, target]), start))
    }

    fn read_quote_form(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // '\''
        let quoted = self.read_form()?;
        let quote_symbol = Form::new(FormKind::Symbol("quote".into()), start);
        Ok(Form::new(FormKind::List(vec![quote_symbol, quoted]), start))
    }

    fn has_regex_end(&self, start_idx: usize, delim: char) -> bool {
        let mut escaped = false;
        let mut idx = start_idx + 1;
        while let Some(&ch) = self.chars.get(idx) {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == delim {
                return true;
            } else if ch == '\n' && delim == '/' {
                break;
            }
            idx += 1;
        }
        false
    }

    fn should_read_regex_literal(&self) -> bool {
        let next = match self.peek_char() {
            Some(ch) => ch,
            None => return false,
        };
        if is_ws_or_comma(next) || matches!(next, ')' | ']' | '}' | ';') {
            return false;
        }
        self.has_regex_end(self.index, '/')
    }

    fn read_regex_literal(
        &mut self,
        start: Span,
        delim: char,
        delim_kind: crate::ast::RegexDelim,
    ) -> Result<Form, CloveError> {
        self.advance(); // consume opening delim
        let mut buf = String::new();
        let mut parts: Vec<InterpolatedPart> = Vec::new();
        let mut has_interpolation = false;
        let mut escaped = false;
        while !self.eof() {
            let ch = self.current_char();
            if escaped {
                buf.push(ch);
                escaped = false;
                self.advance();
                continue;
            }
            if ch == '\\' {
                escaped = true;
                buf.push(ch);
                self.advance();
                continue;
            }
            if ch == '#' && self.peek_char() == Some('{') {
                has_interpolation = true;
                if !buf.is_empty() {
                    parts.push(InterpolatedPart::Text(std::mem::take(&mut buf)));
                }
                self.advance(); // '#'
                self.advance(); // '{'
                self.skip_ws_and_comments();
                if self.eof() {
                    return self.parse_err("unterminated regex interpolation");
                }
                self.interpolation_depth += 1;
                let expr = self.read_form()?;
                self.interpolation_depth -= 1;
                self.skip_ws_and_comments();
                if self.eof() || self.current_char() != '}' {
                    return self.parse_err("unterminated regex interpolation");
                }
                self.advance(); // '}'
                parts.push(InterpolatedPart::Expr(expr));
                continue;
            }
            if ch == delim {
                self.advance();
                if has_interpolation {
                    if !buf.is_empty() {
                        parts.push(InterpolatedPart::Text(buf));
                    }
                    return Ok(Form::new(
                        FormKind::InterpolatedRegex {
                            parts,
                            delim: delim_kind,
                        },
                        start,
                    ));
                }
                return Ok(Form::new(
                    FormKind::Regex {
                        pattern: buf,
                        delim: delim_kind,
                    },
                    start,
                ));
            }
            if ch == '\n' && delim == '/' {
                return self.parse_err("unterminated regex literal");
            }
            buf.push(ch);
            self.advance();
        }
        self.parse_err("unterminated regex literal")
    }

    fn read_string(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // "
        let mut buf = String::new();
        let mut parts: Vec<InterpolatedPart> = Vec::new();
        let mut has_interpolation = false;
        let mut had_escape = false;
        while !self.eof() {
            let ch = self.current_char();
            match ch {
                '"' => {
                    self.advance();
                    if has_interpolation {
                        if !buf.is_empty() {
                            parts.push(InterpolatedPart::Text(buf));
                        }
                        if had_escape {
                            self.record_string_escape(start.index);
                        }
                        return Ok(Form::new(FormKind::InterpolatedString(parts), start));
                    }
                    if had_escape {
                        self.record_string_escape(start.index);
                    }
                    return Ok(Form::new(FormKind::String(buf), start));
                }
                '\\' => {
                    had_escape = true;
                    self.advance();
                    if self.eof() {
                        return self.parse_err("unterminated escape");
                    }
                    let esc = self.current_char();
                    let real = match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        'e' => '\u{001b}',
                        _ => esc,
                    };
                    buf.push(real);
                    self.advance();
                }
                '#' if self.peek_char() == Some('{') => {
                    has_interpolation = true;
                    if !buf.is_empty() {
                        parts.push(InterpolatedPart::Text(std::mem::take(&mut buf)));
                    }
                    self.advance(); // '#'
                    self.advance(); // '{'
                    self.skip_ws_and_comments();
                    if self.eof() {
                        return self.parse_err("unterminated interpolation");
                    }
                    self.interpolation_depth += 1;
                    let expr = self.read_form()?;
                    self.interpolation_depth -= 1;
                    self.skip_ws_and_comments();
                    if self.eof() || self.current_char() != '}' {
                        return self.parse_err("unterminated interpolation");
                    }
                    self.advance(); // '}'
                    parts.push(InterpolatedPart::Expr(expr));
                }
                _ => {
                    buf.push(ch);
                    self.advance();
                }
            }
        }
        self.parse_err("unterminated string")
    }

    fn read_keyword(&mut self, start: Span) -> Result<Form, CloveError> {
        self.advance(); // :
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch) || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | ';') {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        if buf.contains('/') {
            return self.parse_err(
                "namespace separator '/' has been removed; use '::' (e.g. foo::bar). If you meant a regex literal, use #/.../",
            );
        }
        Ok(Form::new(FormKind::Keyword(buf), start))
    }

    fn read_short_fn(&mut self, start: Span) -> Result<Form, CloveError> {
        let list = self.read_list()?;
        let items = match list.kind {
            FormKind::List(items) => items,
            _ => unreachable!(),
        };
        self.mark_form_closed(start);
        Ok(Form::new(FormKind::ShortFn(items), start))
    }

    fn read_map_ref_or_symbol(&mut self, start: Span) -> Result<Form, CloveError> {
        let next = match self.peek_char() {
            Some(ch) => ch,
            None => return self.read_atom(start),
        };
        if next == '&' {
            self.advance(); // consume '&'
            self.advance(); // consume '&'
            return Ok(Form::new(FormKind::Symbol("&&".into()), start));
        }
        if next == '.' {
            return self.parse_err("'&.' has been removed; use '&' for self and &[:k] for fields");
        }
        if next == '^' || next == ':' {
            self.advance(); // consume '&'
            let mut scope = "root";
            if self.current_char() == '^' {
                scope = "this";
                self.advance();
            }
            let mut segments = Vec::new();
            if scope == "this" {
                segments.extend(self.read_map_ref_parent_segments()?);
            }
            if self.eof() || self.current_char() != ':' {
                return self.parse_err("map ref expects ':key' after '&' or '&^'");
            }
            let mut keyword_segments = Vec::new();
            while !self.eof() && self.current_char() == ':' {
                let seg_span = self.current_span();
                keyword_segments.push(self.read_oop_keyword_sugar_segment(seg_span)?);
            }
            if keyword_segments.is_empty() {
                return self.parse_err("map ref expects a keyword segment");
            }
            segments.extend(keyword_segments);
            let mut items = Vec::with_capacity(segments.len() + 2);
            items.push(Form::new(FormKind::Symbol(MAP_REF_SYM.into()), start));
            items.push(Form::new(FormKind::Keyword(scope.into()), start));
            items.extend(segments);
            return Ok(Form::new(FormKind::List(items), start));
        }
        if is_ws_or_comma(next) || matches!(next, '(' | ')' | '[' | ']' | '{' | '}' | ';') {
            self.advance(); // consume '&'
            return Ok(Form::new(FormKind::Symbol("&".into()), start));
        }
        self.advance(); // consume '&'
        let mut name = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || matches!(
                    ch,
                    '(' | ')' | '[' | ']' | '{' | '}' | ';' | '"' | ':' | '.' | '^'
                )
            {
                break;
            }
            name.push(ch);
            self.advance();
        }
        let is_terminator = self.eof()
            || is_ws_or_comma(self.current_char())
            || matches!(self.current_char(), '(' | ')' | '[' | ']' | '{' | '}' | ';');
        if name == "self" {
            if is_terminator {
                return Ok(Form::new(FormKind::Symbol("self".into()), start));
            }
            return self.parse_err("'&self' must be standalone; use '&:key' or '&[:key]'");
        }
        if name == "ref" {
            if is_terminator {
                let mut items = Vec::with_capacity(2);
                items.push(Form::new(FormKind::Symbol(MAP_REF_SYM.into()), start));
                items.push(Form::new(FormKind::Keyword("root".into()), start));
                return Ok(Form::new(FormKind::List(items), start));
            }
            let mut scope = "root";
            if self.current_char() == '^' {
                scope = "this";
                self.advance();
            }
            let mut segments = Vec::new();
            if scope == "this" {
                segments.extend(self.read_map_ref_parent_segments()?);
            }
            if self.eof() || self.current_char() != ':' {
                return self.parse_err("'&ref' expects ':key' or '^:key'");
            }
            let mut keyword_segments = Vec::new();
            while !self.eof() && self.current_char() == ':' {
                let seg_span = self.current_span();
                keyword_segments.push(self.read_oop_keyword_sugar_segment(seg_span)?);
            }
            if keyword_segments.is_empty() {
                return self.parse_err("'&ref' expects a keyword segment");
            }
            segments.extend(keyword_segments);
            let mut items = Vec::with_capacity(segments.len() + 2);
            items.push(Form::new(FormKind::Symbol(MAP_REF_SYM.into()), start));
            items.push(Form::new(FormKind::Keyword(scope.into()), start));
            items.extend(segments);
            return Ok(Form::new(FormKind::List(items), start));
        }
        if name == "root" {
            return self.parse_err("'&root' has been removed; use '&:' for root map refs");
        }
        if name.is_empty() {
            return self.parse_err("'&' must be standalone for self or use '&^:key' / '&:key'");
        }
        self.parse_err(format!(
            "'&{}' has been removed; use '&' for self, '&[:key]' for fields, or '&^:key' / '&:key' for map refs",
            name
        ))
    }

    fn read_atom(&mut self, start: Span) -> Result<Form, CloveError> {
        let mut buf = String::new();
        let mut angle_depth = 0;
        while !self.eof() {
            let ch = self.current_char();
            let is_dispatch = ch == '#'
                && match self.peek_char() {
                    Some('{') | Some('(') | Some('_') | Some('"') | Some('/') | Some('[') => true,
                    Some(next) if next.is_ascii_digit() => true,
                    _ => false,
                };
            let is_delim = is_ws_or_comma(ch)
                || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';')
                || is_dispatch;
            if is_delim && angle_depth == 0 {
                break;
            }
            if ch == ':' && angle_depth == 0 {
                let next = self.peek_char();
                if !buf.is_empty()
                    && !buf.ends_with(':')
                    && !matches!(next, Some(':'))
                    && matches!(next, Some(next) if is_kw_seg_start(next))
                {
                    break;
                }
            }
            if ch == '?' && angle_depth == 0 && !buf.is_empty() && self.peek_dot_after_ws() {
                break;
            }
            if ch == '.' && angle_depth == 0 {
                let allow_range_token = buf.ends_with('.');
                if !allow_range_token {
                    let is_clove_syntax = buf == "clove" && self.peek_str(1, "syntax::");
                    let next = self.peek_char();
                    if !is_clove_syntax {
                        if next == Some('(') {
                            break;
                        }
                        if matches!(next, Some(next) if is_oop_method_start(next)) {
                            break;
                        }
                        if matches!(next, Some('"')) {
                            break;
                        }
                        if matches!(next, Some(next) if next.is_ascii_digit()) {
                            let numeric_prefix = buf
                                .chars()
                                .all(|ch| ch.is_ascii_digit() || ch == '_' || ch == '-');
                            if !numeric_prefix {
                                break;
                            }
                        }
                    }
                }
            }
            match ch {
                '<' if !buf.is_empty() && !buf.ends_with(':') => {
                    angle_depth += 1;
                }
                '>' => {
                    if angle_depth > 0 {
                        angle_depth -= 1;
                    }
                }
                _ => {}
            }
            buf.push(ch);
            self.advance();
        }
        if buf == "nil" {
            return Ok(Form::new(FormKind::Nil, start));
        }
        if buf == "true" {
            return Ok(Form::new(FormKind::Bool(true), start));
        }
        if buf == "false" {
            return Ok(Form::new(FormKind::Bool(false), start));
        }
        if let Some(duration) = parse_duration_literal(&buf)? {
            return Ok(Form::new(FormKind::Duration(duration), start));
        }
        if !buf.starts_with('_') {
            let normalized = strip_numeric_separators(&buf);
            let is_float_literal =
                normalized.contains('.') || normalized.contains('e') || normalized.contains('E');
            if is_float_literal {
                if let Ok(n) = normalized.parse::<f64>() {
                    return Ok(Form::new(FormKind::Float(n), start));
                }
            } else if let Ok(n) = normalized.parse::<i64>() {
                return Ok(Form::new(FormKind::Int(n), start));
            } else if let Ok(nf) = normalized.parse::<f64>() {
                // fallback: non-integer numeric
                return Ok(Form::new(FormKind::Float(nf), start));
            }
        }
        self.symbol_form(buf, start)
    }

    fn skip_ws(&mut self) {
        while !self.eof() && is_ws_or_comma(self.current_char()) {
            if self.current_char() == ',' {
                self.record_trailing_comma();
            }
            if self.current_char() == '\n' {
                if let Some(since) = self.since_newline_has_content_stack.last_mut() {
                    if !*since {
                        if let Some(pending) = self.pending_blank_stack.last_mut() {
                            *pending = true;
                        }
                    }
                    *since = false;
                }
            }
            self.advance();
        }
        if !self.eof() && !is_ws_or_comma(self.current_char()) {
            if let Some(since) = self.since_newline_has_content_stack.last_mut() {
                *since = true;
            }
        }
    }

    fn symbol_form(&self, raw: String, span: Span) -> Result<Form, CloveError> {
        if raw.contains('/') && raw != "/" {
            return self.parse_err(
                "namespace separator '/' has been removed; use '::' (e.g. foo::bar). If you meant a regex literal, use #/.../",
            );
        }
        if let Some((_name, annot)) = split_symbol_type_annotation(raw.as_str()) {
            let hint = match parse_type_hint(annot) {
                Ok(hint) => hint,
                Err(err) => {
                    if self.options.allow_invalid_type_hints {
                        return Ok(Form::new(FormKind::Symbol(raw), span));
                    }
                    return Err(err.with_span(span));
                }
            };
            return Ok(Form::new(FormKind::Symbol(raw), span).with_type_hint(hint));
        }
        Ok(Form::new(FormKind::Symbol(raw), span))
    }

    pub fn skip_ws_and_comments(&mut self) {
        loop {
            self.skip_ws();
            if self.eof() {
                break;
            }
            if self.current_char() == ';' {
                if let Some(since) = self.since_newline_has_content_stack.last_mut() {
                    *since = true;
                }
                let span = self.current_span();
                let (inline, after_opening_delim) = self.comment_context(span.index);
                let leading_blank = self.is_top_level()
                    && self.pending_blank_stack.last().copied().unwrap_or(false);
                let mut buf = String::new();
                while !self.eof() && self.current_char() != '\n' {
                    buf.push(self.current_char());
                    self.advance();
                }
                let container = self.container_stack.last().copied().flatten();
                let trailing_form = {
                    let candidate = if inline {
                        self.preceding_non_ws_char(span.index)
                    } else {
                        self.preceding_non_ws_char_any(span.index)
                    };
                    if let Some(ch) = candidate {
                        if matches!(ch, ']' | '}' | ')') {
                            self.last_closed_form
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                };
                self.comments.push(CommentTrivia {
                    span,
                    container,
                    text: buf,
                    inline,
                    after_opening_delim,
                    trailing_form,
                    leading_blank,
                });
            } else {
                break;
            }
        }
    }

    fn mark_form_closed(&mut self, span: Span) {
        self.last_closed_form = Some(span.index);
    }

    fn mark_last_form_index(&mut self, index: usize) {
        if let Some(slot) = self.last_form_stack.last_mut() {
            *slot = Some(index);
        }
    }

    fn record_trailing_comma(&mut self) {
        if let Some(Some(index)) = self.last_form_stack.last() {
            self.comma_trailing.insert(*index);
        }
    }

    fn preceding_non_ws_char(&self, mut idx: usize) -> Option<char> {
        while idx > 0 {
            idx -= 1;
            let ch = self.chars[idx];
            if ch == '\n' {
                break;
            }
            if !ch.is_whitespace() {
                return Some(ch);
            }
        }
        None
    }

    fn preceding_non_ws_char_any(&self, mut idx: usize) -> Option<char> {
        while idx > 0 {
            idx -= 1;
            let ch = self.chars[idx];
            if ch.is_whitespace() {
                continue;
            }
            return Some(ch);
        }
        None
    }

    fn comment_context(&self, index: usize) -> (bool, bool) {
        if index == 0 {
            return (false, false);
        }
        let mut idx = index;
        while idx > 0 {
            idx -= 1;
            let ch = self.chars[idx];
            if ch == '\n' {
                return (false, false);
            }
            if !ch.is_whitespace() {
                if matches!(ch, '(' | '[' | '{') {
                    return (false, true);
                }
                return (true, false);
            }
        }
        (false, false)
    }

    fn is_top_level(&self) -> bool {
        self.container_stack.len() == 1 && self.container_stack[0].is_none()
    }

    fn record_blank_line(&mut self, index: usize) {
        let is_top_level = self.is_top_level();
        if let Some(pending) = self.pending_blank_stack.last_mut() {
            if *pending {
                if is_top_level {
                    self.top_level_blank_lines.insert(index);
                } else {
                    self.inner_blank_lines.insert(index);
                }
                *pending = false;
            }
        }
        if let Some(since) = self.since_newline_has_content_stack.last_mut() {
            *since = true;
        }
    }

    fn record_string_escape(&mut self, index: usize) {
        self.string_escape_indices.insert(index);
    }

    pub fn current_char(&self) -> char {
        self.effective_char_and_len(self.index).0
    }

    fn peek_char(&self) -> Option<char> {
        let (_, len) = self.effective_char_and_len(self.index);
        let next_idx = self.index + len;
        if next_idx >= self.chars.len() {
            None
        } else {
            Some(self.effective_char_and_len(next_idx).0)
        }
    }

    fn peek_dot_after_ws(&self) -> bool {
        let mut idx = self.index;
        let (_, len) = self.effective_char_and_len(idx);
        idx += len;
        while idx < self.chars.len() {
            let (ch, ch_len) = self.effective_char_and_len(idx);
            if is_ws_or_comma(ch) {
                idx += ch_len;
                continue;
            }
            return ch == '.';
        }
        false
    }

    fn peek_str(&self, offset: usize, expected: &str) -> bool {
        let mut idx = self.index + offset;
        for ch in expected.chars() {
            if self.chars.get(idx) != Some(&ch) {
                return false;
            }
            idx += 1;
        }
        true
    }

    fn advance(&mut self) {
        if self.eof() {
            return;
        }
        let (_, len) = self.effective_char_and_len(self.index);
        for _ in 0..len {
            let ch = self.chars[self.index];
            self.index += 1;
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }

    fn effective_char_and_len(&self, idx: usize) -> (char, usize) {
        if self.interpolation_depth > 0 {
            if let Some('\\') = self.chars.get(idx) {
                if let Some(next) = self.chars.get(idx + 1).copied() {
                    let decoded = match next {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        '#' => '#',
                        '{' => '{',
                        '}' => '}',
                        other => other,
                    };
                    return (decoded, 2);
                }
            }
        }
        // Guard so the parser won't panic when called without boundary checks.
        self.chars
            .get(idx)
            .copied()
            .map(|c| (c, 1))
            .unwrap_or(('\0', 1))
    }

    fn current_span(&self) -> Span {
        Span {
            line: self.line,
            col: self.col,
            index: self.index,
        }
    }

    fn read_foreign_brace_block(&mut self, tag: &str, start: Span) -> Result<Form, CloveError> {
        if !tag.is_empty() && !(tag.contains('.') || tag.contains("::")) {
            self.ensure_foreign_tag_allowed(tag, start)?;
        }
        self.advance(); // {
        let mut depth = 1;
        let mut buf = String::new();
        let mut in_str = false;
        let mut escape = false;
        while !self.eof() {
            let ch = self.current_char();
            if escape {
                buf.push(ch);
                escape = false;
                self.advance();
                continue;
            }
            if ch == '\\' && tag == "rb" {
                // Ruby side should ignore parens in strings; honor escapes here too.
                escape = true;
                buf.push(ch);
                self.advance();
                continue;
            }
            if ch == '"' {
                in_str = !in_str;
                buf.push(ch);
                self.advance();
                continue;
            }
            if !in_str {
                match ch {
                    '{' => depth += 1,
                    '$' => return self.parse_err("nested $... is not supported"),
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            self.advance(); // }
                            self.mark_form_closed(start);
                            return Ok(Form::new(
                                FormKind::ForeignBlock {
                                    tag: tag.to_string(),
                                    code: buf,
                                },
                                start,
                            ));
                        }
                    }
                    _ => {}
                }
            }
            buf.push(ch);
            self.advance();
        }
        self.parse_err(format!("unterminated {{{} ...}} block", tag))
    }

    fn merge_ruby_suffix(
        &self,
        tag: Option<String>,
        code: String,
        suffix: Option<String>,
    ) -> (Option<String>, String) {
        if let Some(raw_tag) = tag.as_deref() {
            if raw_tag.contains('.') || raw_tag.contains("::") {
                let call = if code.trim().is_empty() {
                    format!("{}()", raw_tag)
                } else {
                    format!("{}({})", raw_tag, code)
                };
                if let Some(sfx) = suffix {
                    return (None, format!("({}){}", call, sfx));
                }
                return (None, call);
            }
        }
        if let Some(sfx) = suffix {
            return (tag, format!("({}){}", code, sfx));
        }
        (tag, code)
    }

    fn read_ruby_suffix(&mut self) -> Result<Option<String>, CloveError> {
        let mut suffix = String::new();
        while !self.eof() && self.current_char() == '.' {
            if !matches!(self.peek_char(), Some(ch) if is_oop_ident_char(ch)) {
                break;
            }
            suffix.push('.');
            self.advance(); // consume '.'
            let ident = self.read_ruby_suffix_ident()?;
            if ident.is_empty() {
                return self.parse_err("expected method name after '.'");
            }
            suffix.push_str(&ident);
            if self.eof() {
                break;
            }
            if self.current_char() == '(' {
                suffix.push_str(&self.read_ruby_suffix_args()?);
            }
        }
        if suffix.is_empty() {
            Ok(None)
        } else {
            Ok(Some(suffix))
        }
    }

    fn read_ruby_suffix_ident(&mut self) -> Result<String, CloveError> {
        let mut buf = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch)
                || matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';')
                || ch == '.'
            {
                break;
            }
            buf.push(ch);
            self.advance();
        }
        Ok(buf)
    }

    fn read_ruby_suffix_args(&mut self) -> Result<String, CloveError> {
        let mut buf = String::new();
        let mut depth = 0;
        let mut in_string = false;
        let mut escape = false;
        let mut quote_char = '"';
        while !self.eof() {
            let ch = self.current_char();
            buf.push(ch);
            self.advance();
            if in_string {
                if escape {
                    escape = false;
                    continue;
                }
                match ch {
                    '\\' => {
                        escape = true;
                    }
                    _ if ch == quote_char => {
                        in_string = false;
                    }
                    _ => {}
                }
                continue;
            }
            match ch {
                '"' | '\'' => {
                    in_string = true;
                    quote_char = ch;
                }
                '(' => {
                    depth += 1;
                }
                ')' => {
                    if depth == 0 {
                        return self.parse_err("unexpected ')'");
                    }
                    depth -= 1;
                    if depth == 0 {
                        return Ok(buf);
                    }
                }
                _ => {}
            }
        }
        self.parse_err("unterminated parentheses in foreign suffix")
    }

    pub fn eof(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn read_dollar_form(&mut self) -> Result<Form, CloveError> {
        let start = self.current_span();
        self.advance(); // $
        if self.eof() {
            return self.parse_err("unexpected end after '$'");
        }
        if self.current_char() == '{' {
            return self.read_dollar_brace(start);
        }
        if self.current_char() == '(' {
            return self.parse_err("foreign blocks with parentheses are not supported; use ${...}");
        }

        let mut token = String::new();
        while !self.eof() {
            let ch = self.current_char();
            if is_ws_or_comma(ch) || matches!(ch, ')' | '[' | ']' | '{' | '}' | '"') {
                break;
            }
            if ch == '(' {
                break;
            }
            token.push(ch);
            self.advance();
        }

        if token.is_empty() {
            return self.parse_err("expected foreign tag or symbol after '$'");
        }

        let tag_separator = token.chars().position(|ch| ch == ':').and_then(|idx| {
            if token.get(idx + 1..idx + 2) == Some(":") {
                None
            } else {
                Some(idx)
            }
        });

        let has_args = !self.eof() && self.current_char() == '(';
        let has_brace = !self.eof() && self.current_char() == '{';
        if has_args && (tag_separator.is_some() || token.contains('.') || token.contains("::")) {
            let (tag_opt, mut base) = if let Some(idx) = tag_separator {
                let (tag, rest) = token.split_at(idx);
                let rest = rest.trim_start_matches(':');
                if tag.is_empty() || rest.is_empty() {
                    (None, token.clone())
                } else {
                    self.ensure_foreign_tag_allowed(tag, start)?;
                    (Some(tag.to_string()), rest.to_string())
                }
            } else {
                (None, token.clone())
            };
            base.push_str(&self.read_ruby_suffix_args()?);
            let mut code = base;
            if let Some(suffix) = self.read_ruby_suffix()? {
                code = format!("({}){}", code, suffix);
            }
            return Ok(Form::new(
                FormKind::ForeignRaw { tag: tag_opt, code },
                start,
            ));
        } else if has_args {
            return self
                .parse_err("foreign blocks with parentheses are not supported; use $tag{...}");
        } else if has_brace
            && tag_separator.is_none()
            && !token.contains('.')
            && !token.contains("::")
        {
            self.ensure_foreign_tag_allowed(&token, start)?;
            let code_form = self.read_foreign_brace_block(token.as_str(), start)?;
            if let FormKind::ForeignBlock { code, .. } = code_form.kind {
                let suffix = self.read_ruby_suffix()?;
                let (tag, final_code) = self.merge_ruby_suffix(Some(token.clone()), code, suffix);
                return Ok(Form::new(
                    FormKind::ForeignRaw {
                        tag,
                        code: final_code,
                    },
                    start,
                ));
            }
            return self.parse_err("invalid dollar form");
        }

        let (tag_opt, mut path) = if let Some(idx) = tag_separator {
            let (tag, rest) = token.split_at(idx);
            let rest = rest.trim_start_matches(':');
            if tag.is_empty() || rest.is_empty() {
                (None, token)
            } else {
                self.ensure_foreign_tag_allowed(tag, start)?;
                (Some(tag.to_string()), rest.to_string())
            }
        } else {
            (None, token)
        };
        if !self.eof() && self.current_char() == '(' {
            path.push_str(&self.read_ruby_suffix_args()?);
        }
        if let Some(suffix) = self.read_ruby_suffix()? {
            path.push_str(&suffix);
        }
        Ok(Form::new(
            FormKind::ForeignSymbol { tag: tag_opt, path },
            start,
        ))
    }

    fn read_dollar_brace(&mut self, start: Span) -> Result<Form, CloveError> {
        let code_form = self.read_foreign_brace_block("rb", start)?;
        if let FormKind::ForeignBlock { code, .. } = code_form.kind {
            let suffix = self.read_ruby_suffix()?;
            let (tag, final_code) = self.merge_ruby_suffix(None, code, suffix);
            return Ok(Form::new(
                FormKind::ForeignRaw {
                    tag,
                    code: final_code,
                },
                start,
            ));
        }
        self.parse_err("invalid dollar form")
    }

    fn ensure_foreign_tag_allowed(&self, tag: &str, span: Span) -> Result<(), CloveError> {
        if tag.is_empty() {
            return Ok(());
        }
        let Some(allowed) = &self.options.allowed_foreign_tags else {
            return Ok(());
        };
        if allowed.contains(tag) {
            return Ok(());
        }
        let mut allowed_list: Vec<_> = allowed.iter().cloned().collect();
        allowed_list.sort();
        let mut err = CloveError::parse(format!(
            "unknown foreign tag: {} (allowed: {})",
            tag,
            allowed_list.join(", ")
        ))
        .with_span(span);
        if let Some(name) = &self.options.source_name {
            err = err.with_file(Some(name.clone()));
        }
        Err(err)
    }

    pub fn read_raw_form_string(&mut self) -> Result<String, CloveError> {
        self.skip_ws_and_comments();
        let start_index = self.index;

        self.read_form()?;

        let end_index = self.index;
        let raw = self.chars[start_index..end_index].iter().collect();
        Ok(raw)
    }

    pub fn read_braced_block_content(
        &mut self,
        tag_name: &str,
        support_single_quote: bool,
    ) -> Result<String, CloveError> {
        if self.eof() || self.current_char() != '{' {
            return self.parse_err(format!("expected '{{' after {}", tag_name));
        }
        self.advance(); // consume '{'

        let mut buf = String::new();
        let mut depth = 1;
        let mut in_string = false;
        let mut escape = false;
        let mut quote_char = '"';

        while !self.eof() {
            let ch = self.current_char();
            self.advance();

            if in_string {
                buf.push(ch);
                if escape {
                    escape = false;
                } else if ch == '\\' {
                    escape = true;
                } else if ch == quote_char {
                    in_string = false;
                }
                continue;
            }

            match ch {
                '"' => {
                    in_string = true;
                    quote_char = '"';
                    buf.push(ch);
                }
                '\'' if support_single_quote => {
                    in_string = true;
                    quote_char = '\'';
                    buf.push(ch);
                }
                '{' => {
                    depth += 1;
                    buf.push(ch);
                }
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(buf);
                    } else {
                        buf.push(ch);
                    }
                }
                _ => buf.push(ch),
            }
        }

        self.parse_err(format!("unterminated {} block", tag_name))
    }

    pub fn parse_err<T>(&self, msg: impl Into<String>) -> Result<T, CloveError> {
        let span = Span {
            line: self.line,
            col: self.col,
            index: self.index,
        };
        let raw_message = msg.into();
        let file_label = self.options.source_name.as_deref().unwrap_or("unknown");
        let formatted = format!("{}:{}:{} {}", file_label, span.line, span.col, raw_message);
        let err = CloveError::parse(formatted).with_span(span);
        let err = if let Some(name) = &self.options.source_name {
            err.with_file(Some(name.clone()))
        } else {
            err
        };
        Err(err)
    }
}

fn strip_numeric_separators(token: &str) -> String {
    token.chars().filter(|c| *c != '_').collect()
}

fn parse_oop_index_literal(token: &str) -> Option<i64> {
    if token.is_empty() {
        return None;
    }
    let normalized = strip_numeric_separators(token);
    if normalized.contains('.') || normalized.contains('e') || normalized.contains('E') {
        return None;
    }
    normalized.parse::<i64>().ok()
}

fn is_ws(ch: char) -> bool {
    ch.is_whitespace()
}

fn is_ws_or_comma(ch: char) -> bool {
    ch.is_whitespace() || ch == ','
}

fn is_kw_seg_start(ch: char) -> bool {
    if ch.is_whitespace() {
        return false;
    }
    if ch.is_ascii() {
        ch.is_ascii_lowercase() || ch == '_'
    } else {
        true
    }
}

fn is_oop_ident_char(ch: char) -> bool {
    if is_ws_or_comma(ch) {
        return false;
    }
    !matches!(ch, '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';' | '.')
}

fn is_oop_method_start(ch: char) -> bool {
    is_oop_ident_char(ch) && !ch.is_ascii_digit()
}

fn is_oop_stage_start(ch: char) -> bool {
    matches!(ch, '(' | ':' | '"' | '\'') || is_oop_ident_char(ch)
}

fn is_match_or_separator(form: &Form) -> bool {
    matches!(&form.kind, FormKind::Symbol(sym) if sym == "|")
}

fn split_range_symbol(sym: &str) -> Option<(Option<&str>, Option<&str>, bool)> {
    let (sep, exclusive, sep_len) = if let Some(idx) = sym.find("...") {
        (idx, true, 3)
    } else if let Some(idx) = sym.find("..") {
        (idx, false, 2)
    } else {
        return None;
    };
    let start = &sym[..sep];
    let end = &sym[sep + sep_len..];
    let start = if start.is_empty() { None } else { Some(start) };
    let end = if end.is_empty() { None } else { Some(end) };
    Some((start, end, exclusive))
}

fn is_duration_number(part: &str) -> bool {
    if part.is_empty() {
        return false;
    }
    let mut has_digit = false;
    for (idx, ch) in part.chars().enumerate() {
        if ch == '_' {
            continue;
        }
        if idx == 0 && (ch == '+' || ch == '-') {
            continue;
        }
        if ch.is_ascii_digit() {
            has_digit = true;
            continue;
        }
        return false;
    }
    has_digit
}

fn parse_duration_literal(token: &str) -> Result<Option<DurationValue>, CloveError> {
    const UNITS: [(&str, DurationUnit); 7] = [
        ("ms", DurationUnit::Millisecond),
        ("s", DurationUnit::Second),
        ("m", DurationUnit::Minute),
        ("h", DurationUnit::Hour),
        ("d", DurationUnit::Day),
        ("w", DurationUnit::Week),
        ("y", DurationUnit::Year),
    ];
    for (suffix, unit) in UNITS {
        if token.ends_with(suffix) {
            let number_part = &token[..token.len() - suffix.len()];
            if number_part.is_empty() {
                continue;
            }
            if !is_duration_number(number_part) {
                continue;
            }
            let normalized = strip_numeric_separators(number_part);
            let magnitude = normalized.parse::<i128>().map_err(|_| {
                CloveError::runtime(format!("invalid duration literal '{}'", token))
            })?;
            return DurationValue::from_unit(magnitude, unit).map(Some);
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeKind;

    fn parse_one(src: &str) -> Form {
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let mut forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1, "expected single form for {}", src);
        forms.remove(0)
    }

    fn assert_forms_equivalent(expected: &[Form], actual: &[Form], ctx: &str) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "different number of forms in {}",
            ctx
        );
        for (idx, (lhs, rhs)) in expected.iter().zip(actual).enumerate() {
            assert_form_equivalent(lhs, rhs, &format!("{} form {}", ctx, idx));
        }
    }

    fn assert_form_equivalent(lhs: &Form, rhs: &Form, ctx: &str) {
        assert_eq!(
            lhs.type_hint, rhs.type_hint,
            "type hint mismatch at {}",
            ctx
        );
        match (&lhs.kind, &rhs.kind) {
            (FormKind::Symbol(a), FormKind::Symbol(b))
            | (FormKind::Keyword(a), FormKind::Keyword(b))
            | (FormKind::String(a), FormKind::String(b)) => assert_eq!(a, b, "{}", ctx),
            (FormKind::Int(a), FormKind::Int(b)) => assert_eq!(a, b, "{}", ctx),
            (FormKind::Float(a), FormKind::Float(b)) => assert_eq!(a, b, "{}", ctx),
            (FormKind::Bool(a), FormKind::Bool(b)) => assert_eq!(a, b, "{}", ctx),
            (FormKind::Nil, FormKind::Nil) => {}
            (FormKind::Duration(a), FormKind::Duration(b)) => assert_eq!(a, b, "{}", ctx),
            (FormKind::InterpolatedString(a), FormKind::InterpolatedString(b)) => {
                assert_eq!(a.len(), b.len(), "{}", ctx);
                for (idx, (left, right)) in a.iter().zip(b).enumerate() {
                    match (left, right) {
                        (InterpolatedPart::Text(lt), InterpolatedPart::Text(rt)) => {
                            assert_eq!(lt, rt, "{} part {}", ctx, idx);
                        }
                        (InterpolatedPart::Expr(le), InterpolatedPart::Expr(re)) => {
                            assert_form_equivalent(le, re, &format!("{} part {}", ctx, idx));
                        }
                        _ => panic!("interpolated string part mismatch at {}", ctx),
                    }
                }
            }
            (FormKind::ShortFn(a), FormKind::ShortFn(b))
            | (FormKind::List(a), FormKind::List(b))
            | (FormKind::Vector(a), FormKind::Vector(b))
            | (FormKind::Set(a), FormKind::Set(b)) => {
                assert_forms_equivalent(a, b, ctx);
            }
            (FormKind::Map(a), FormKind::Map(b)) => assert_map_equivalent(a, b, ctx),
            (
                FormKind::Regex {
                    pattern: pa,
                    delim: da,
                },
                FormKind::Regex {
                    pattern: pb,
                    delim: db,
                },
            ) => {
                assert_eq!(pa, pb, "{}", ctx);
                assert_eq!(da, db, "{}", ctx);
            }
            (
                FormKind::ForeignBlock { tag: ta, code: ca },
                FormKind::ForeignBlock { tag: tb, code: cb },
            ) => {
                assert_eq!(ta, tb, "{}", ctx);
                assert_eq!(ca, cb, "{}", ctx);
            }
            (
                FormKind::ForeignRaw { tag: ta, code: ca },
                FormKind::ForeignRaw { tag: tb, code: cb },
            ) => {
                assert_eq!(ta, tb, "{}", ctx);
                assert_eq!(ca, cb, "{}", ctx);
            }
            (
                FormKind::ForeignSymbol { tag: ta, path: pa },
                FormKind::ForeignSymbol { tag: tb, path: pb },
            ) => {
                assert_eq!(ta, tb, "{}", ctx);
                assert_eq!(pa, pb, "{}", ctx);
            }
            other => panic!("form mismatch at {}: {:?}", ctx, other),
        }
    }

    fn assert_map_equivalent(lhs: &[MapItem], rhs: &[MapItem], ctx: &str) {
        assert_eq!(lhs.len(), rhs.len(), "map entry count mismatch at {}", ctx);
        for (idx, (left, right)) in lhs.iter().zip(rhs).enumerate() {
            match (left, right) {
                (MapItem::KeyValue(lk, lv), MapItem::KeyValue(rk, rv)) => {
                    assert_form_equivalent(lk, rk, &format!("{} map key {}", ctx, idx));
                    assert_form_equivalent(lv, rv, &format!("{} map value {}", ctx, idx));
                }
                (MapItem::Spread(le), MapItem::Spread(re)) => {
                    assert_form_equivalent(le, re, &format!("{} map spread {}", ctx, idx));
                }
                _ => panic!("map item mismatch at {}", ctx),
            }
        }
    }

    #[test]
    fn parse_rb_block() {
        let mut reader = Reader::new_with_options("$rb{ 1 }", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(code.trim(), "1");
            }
            other => panic!("expected ForeignRaw, got {:?}", other),
        }
    }

    #[test]
    fn unterminated_string_reports_span() {
        let mut reader = Reader::new_with_options("\"abc", ReaderOptions::default());
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => {
                assert!(
                    data.message.contains("unknown:1:"),
                    "expected line/col, got {}",
                    data.message
                );
            }
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn unexpected_closer_reports_span() {
        let mut reader = Reader::new_with_options(")", ReaderOptions::default());
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => assert!(data.message.contains(")")),
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn unterminated_list_reports_span() {
        let mut reader = Reader::new_with_options("(1 2", ReaderOptions::default());
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => {
                assert!(data.message.contains("unterminated list"));
            }
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn unterminated_map_reports_span() {
        let mut reader = Reader::new_with_options("{:a 1", ReaderOptions::default());
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => assert!(data.message.contains("unterminated map")),
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn map_literal_supports_shorthand_keywords() {
        let mut reader = Reader::new_with_options("{:a, :b}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let entries = match &forms[0].kind {
            FormKind::Map(entries) => entries,
            other => panic!("expected map, got {:?}", other),
        };
        assert_eq!(entries.len(), 2);
        let assert_entry = |entry: &MapItem, expected: &str| match entry {
            MapItem::KeyValue(k, v) => {
                assert!(matches!(&k.kind, FormKind::Keyword(sym) if sym == expected));
                assert!(matches!(&v.kind, FormKind::Symbol(sym) if sym == expected));
            }
            other => panic!("expected key/value entry, got {:?}", other),
        };
        assert_entry(&entries[0], "a");
        assert_entry(&entries[1], "b");
    }

    #[test]
    fn map_literal_shorthand_supports_label_style_keys() {
        let mut reader = Reader::new_with_options("{a:, b:}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let entries = match &forms[0].kind {
            FormKind::Map(entries) => entries,
            other => panic!("expected map, got {:?}", other),
        };
        assert_eq!(entries.len(), 2);
        let assert_entry = |entry: &MapItem, expected: &str| match entry {
            MapItem::KeyValue(k, v) => {
                assert!(matches!(&k.kind, FormKind::Keyword(sym) if sym == expected));
                assert!(matches!(&v.kind, FormKind::Symbol(sym) if sym == expected));
            }
            other => panic!("expected key/value entry, got {:?}", other),
        };
        assert_entry(&entries[0], "a");
        assert_entry(&entries[1], "b");
    }

    #[test]
    fn map_literal_shorthand_supports_label_style_keys_with_hyphen() {
        let mut reader =
            Reader::new_with_options("{bird-x:, pipe-speed:}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let entries = match &forms[0].kind {
            FormKind::Map(entries) => entries,
            other => panic!("expected map, got {:?}", other),
        };
        assert_eq!(entries.len(), 2);
        let assert_entry = |entry: &MapItem, expected: &str| match entry {
            MapItem::KeyValue(k, v) => {
                assert!(matches!(&k.kind, FormKind::Keyword(sym) if sym == expected));
                assert!(matches!(&v.kind, FormKind::Symbol(sym) if sym == expected));
            }
            other => panic!("expected key/value entry, got {:?}", other),
        };
        assert_entry(&entries[0], "bird-x");
        assert_entry(&entries[1], "pipe-speed");
    }

    #[test]
    fn map_literal_allows_commas_and_special_keys() {
        let mut reader =
            Reader::new_with_options("{:keys [1 2], :as \"foo\", :x 1}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let entries = match &forms[0].kind {
            FormKind::Map(entries) => entries,
            other => panic!("expected map, got {:?}", other),
        };
        assert_eq!(entries.len(), 3);
        match &entries[0] {
            MapItem::KeyValue(k, v) => {
                assert!(matches!(&k.kind, FormKind::Keyword(sym) if sym == "keys"));
                let vec_items = match &v.kind {
                    FormKind::Vector(items) => items,
                    other => panic!("expected vector, got {:?}", other),
                };
                assert_eq!(vec_items.len(), 2);
            }
            other => panic!("expected key/value, got {:?}", other),
        }
        match &entries[1] {
            MapItem::KeyValue(k, v) => {
                assert!(matches!(&k.kind, FormKind::Keyword(sym) if sym == "as"));
                assert!(matches!(&v.kind, FormKind::String(s) if s == "foo"));
            }
            other => panic!("expected key/value, got {:?}", other),
        }
    }

    #[test]
    fn short_fn_placeholder_is_expanded() {
        let mut reader = Reader::new_with_options("#(inc %)", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ShortFn(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(&items[0].kind, FormKind::Symbol(s) if s == "inc"));
                assert!(matches!(&items[1].kind, FormKind::Symbol(s) if s == "%"));
            }
            other => panic!("expected short fn, got {:?}", other),
        }
    }

    #[test]
    fn deref_macro_keeps_deref_head() {
        let mut reader = Reader::new_with_options(
            "@(promise-catch f (fn [_] :recovered))",
            ReaderOptions::default(),
        );
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::List(items) => {
                assert_eq!(items.len(), 2);
                match &items[0].kind {
                    FormKind::Symbol(sym) => assert_eq!(sym, "deref"),
                    other => panic!("expected deref symbol, got {:?}", other),
                }
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn short_fn_supports_percent_and_question() {
        let mut reader = Reader::new_with_options("#(+ %1 ?2)", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ShortFn(items) => {
                assert_eq!(items.len(), 3);
                assert!(matches!(&items[0].kind, FormKind::Symbol(s) if s == "+"));
                assert!(matches!(&items[1].kind, FormKind::Symbol(s) if s == "%1"));
                assert!(matches!(&items[2].kind, FormKind::Symbol(s) if s == "?2"));
            }
            other => panic!("expected short fn, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_symbol_without_tag() {
        let mut reader = Reader::new_with_options("$Foo.bar", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignSymbol { tag, path } => {
                assert!(tag.is_none());
                assert_eq!(path, "Foo.bar");
            }
            other => panic!("expected foreign symbol, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_symbol_with_tag_prefix() {
        let mut reader = Reader::new_with_options("$rb:Foo.bar", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignSymbol { tag, path } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(path, "Foo.bar");
            }
            other => panic!("expected foreign symbol, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_symbol_with_tag_and_ruby_suffix() {
        let mut reader = Reader::new_with_options(
            r#"$rb:Nokogiri::HTML.parse(html).search("h1").text"#,
            ReaderOptions::default(),
        );
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(code, r#"(Nokogiri::HTML.parse(html)).search("h1").text"#);
            }
            other => panic!("expected foreign symbol, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_symbol_with_double_colon_without_tag() {
        let mut reader =
            Reader::new_with_options("$Nokogiri::HTML.parse(html)", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignRaw { tag, code } => {
                assert!(tag.is_none());
                assert_eq!(code, "Nokogiri::HTML.parse(html)");
            }
            other => panic!("expected foreign symbol, got {:?}", other),
        }
    }

    #[test]
    fn disallowed_foreign_tag_is_rejected() {
        let mut reader = Reader::new_with_options(
            "$py:Foo.bar",
            ReaderOptions::language_defaults(vec!["rb".to_string()]),
        );
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => {
                assert!(
                    data.message.contains("unknown foreign tag"),
                    "unexpected message {}",
                    data.message
                );
                assert!(data.message.contains("rb"));
            }
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn allowed_foreign_tag_passes_validation() {
        let mut reader = Reader::new_with_options(
            "$rb:Foo.bar",
            ReaderOptions::language_defaults(vec!["rb".to_string(), "py".to_string()]),
        );
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignSymbol { tag, path } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(path, "Foo.bar");
            }
            other => panic!("expected foreign symbol, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_block_with_braces() {
        let mut reader = Reader::new_with_options("$rb{1+2}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(code, "1+2");
            }
            other => panic!("expected foreign raw, got {:?}", other),
        }
    }

    #[test]
    fn parse_foreign_block_with_braces_py() {
        let mut reader = Reader::new_with_options("$py{print('ok')}", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("py"));
                assert_eq!(code, "print('ok')");
            }
            other => panic!("expected foreign raw, got {:?}", other),
        }
    }

    #[test]
    fn dot_chain_lowers_to_as_thread() {
        let src = "(inc 123).(+ 1 ?).(repeat 3 ?).(map inc ?)";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 6);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "as->"));
        let placeholder = match &items[2].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        assert!(placeholder.starts_with("__dot_chain"));
        for stage_idx in 3..items.len() {
            match &items[stage_idx].kind {
                FormKind::List(parts) => {
                    let tail = parts.last().expect("stage arg");
                    assert!(matches!(&tail.kind, FormKind::Symbol(sym) if sym == &placeholder));
                }
                other => panic!("expected list stage, got {:?}", other),
            }
        }
    }

    #[test]
    fn dot_chain_does_not_rewrite_short_fn_placeholders() {
        let src = "(range 3).(filter #(+ ? 1) ?)";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        let placeholder = match &items[2].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        let stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected list stage, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == "filter"));
        assert!(matches!(&stage[2].kind, FormKind::Symbol(sym) if sym == &placeholder));
        let short_fn = match &stage[1].kind {
            FormKind::ShortFn(items) => items,
            other => panic!("expected short fn, got {:?}", other),
        };
        assert_eq!(short_fn.len(), 3);
        assert!(matches!(&short_fn[0].kind, FormKind::Symbol(sym) if sym == "+"));
        assert!(matches!(&short_fn[1].kind, FormKind::Symbol(sym) if sym == "?"));
    }

    #[test]
    fn dot_chain_supports_spread_placeholder() {
        let src = "[inc (range 10)].(map *?)";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        let placeholder = match &items[2].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        let stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == "apply"));
        assert!(matches!(&stage[1].kind, FormKind::Symbol(sym) if sym == "map"));
        assert!(matches!(&stage[2].kind, FormKind::Symbol(sym) if sym == &placeholder));
    }

    #[test]
    fn dot_chain_allows_line_start_stage() {
        let src = "(range 3)\n.(map inc ?)\n.(take 2 ?)";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 5);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "as->"));
        let placeholder = match &items[2].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        for stage_idx in 3..items.len() {
            let stage = match &items[stage_idx].kind {
                FormKind::List(parts) => parts,
                other => panic!("expected list stage, got {:?}", other),
            };
            let tail = stage.last().expect("stage arg");
            assert!(matches!(&tail.kind, FormKind::Symbol(sym) if sym == &placeholder));
        }
    }

    #[test]
    fn compose_sugar_head_dot() {
        let form = parse_one("(. f g h)");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "comp"));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "h"));
        assert!(matches!(&items[2].kind, FormKind::Symbol(sym) if sym == "g"));
        assert!(matches!(&items[3].kind, FormKind::Symbol(sym) if sym == "f"));
    }

    #[test]
    fn compose_sugar_tail_dot() {
        let form = parse_one("(f g h .)");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "comp"));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "f"));
        assert!(matches!(&items[2].kind, FormKind::Symbol(sym) if sym == "g"));
        assert!(matches!(&items[3].kind, FormKind::Symbol(sym) if sym == "h"));
    }

    #[test]
    fn dot_chain_accepts_as_directive() {
        let form = parse_one("x.(as xs)");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "as->"));
        let stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == "as"));
        assert!(matches!(&stage[1].kind, FormKind::Symbol(sym) if sym == "xs"));
    }

    #[test]
    fn dot_chain_accepts_let_directive() {
        let form = parse_one("x.(let xs)");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "as->"));
        let stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == "let"));
        assert!(matches!(&stage[1].kind, FormKind::Symbol(sym) if sym == "xs"));
    }

    #[test]
    fn dot_chain_merges_with_oop_chain_for_as() {
        let form = parse_one("10.(as x).inc");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        let as_stage = match &items[2].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&as_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_AS_SYM));
        assert!(matches!(&as_stage[1].kind, FormKind::Symbol(sym) if sym == "x"));
        let inc_stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&inc_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_BARE_SYM));
        assert!(matches!(&inc_stage[1].kind, FormKind::Symbol(sym) if sym == "inc"));
    }

    #[test]
    fn call_sugar_parses_symbol_paren() {
        let form = parse_one("path::resolve(\".\")");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "path::resolve"));
        assert!(matches!(&items[1].kind, FormKind::String(s) if s == "."));
    }

    #[test]
    fn call_sugar_allows_commas() {
        let sugar = parse_one("foo(1,2)");
        let explicit = parse_one("(foo 1 2)");
        assert_form_equivalent(&sugar, &explicit, "foo(1,2) vs (foo 1 2)");
    }

    #[test]
    fn call_sugar_rejects_chained_call() {
        let mut reader = Reader::new_with_options("a(b)(c)", ReaderOptions::default());
        let err = reader.read_all().unwrap_err();
        match err {
            CloveError::Parse(data) => {
                assert!(
                    data.message.contains("chained call sugar"),
                    "unexpected message {}",
                    data.message
                );
            }
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn oop_chain_accepts_dot_stage() {
        let src = "100.rand.(str *?).shuffle";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 5);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        let first_stage = match &items[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&first_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_BARE_SYM));
        assert!(matches!(&first_stage[1].kind, FormKind::Symbol(sym) if sym == "rand"));
        let dot_stage = match &items[3].kind {
            FormKind::List(items) => items,
            other => panic!("expected dot stage list, got {:?}", other),
        };
        assert!(matches!(&dot_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_DOT_STAGE_SYM));
        let placeholder = match &dot_stage[1].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        assert!(placeholder.starts_with(DOT_CHAIN_PLACEHOLDER_PREFIX));
        let dot_body = match &dot_stage[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected dot body list, got {:?}", other),
        };
        assert!(matches!(&dot_body[0].kind, FormKind::Symbol(sym) if sym == "apply"));
        assert!(matches!(&dot_body[1].kind, FormKind::Symbol(sym) if sym == "str"));
        assert!(matches!(&dot_body[2].kind, FormKind::Symbol(sym) if sym == &placeholder));
        let last_stage = match &items[4].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&last_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_BARE_SYM));
        assert!(matches!(&last_stage[1].kind, FormKind::Symbol(sym) if sym == "shuffle"));
    }

    #[test]
    fn oop_chain_accepts_repl_stage_without_placeholder() {
        let form = parse_one("x.a.(repl).b");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 5);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        let repl_stage = match &items[3].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&repl_stage[0].kind, FormKind::Symbol(sym) if sym == "repl"));
    }

    #[test]
    fn oop_chain_accepts_repl_stage_with_placeholder() {
        let form = parse_one("x.a.(repl ?).b");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 5);
        let repl_stage = match &items[3].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&repl_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_DOT_STAGE_SYM));
    }

    #[test]
    fn oop_chain_accepts_nil_safe_marker() {
        let form = parse_one("x?.a.b");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 5);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "x"));
        let marker = match &items[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected marker list, got {:?}", other),
        };
        assert_eq!(marker.len(), 1);
        assert!(matches!(&marker[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM));
        let first_stage = match &items[3].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&first_stage[0].kind, FormKind::Symbol(sym) if sym == OOP_BARE_SYM));
        assert!(matches!(&first_stage[1].kind, FormKind::Symbol(sym) if sym == "a"));
    }

    #[test]
    fn oop_chain_accepts_nil_safe_marker_with_line_break() {
        let form = parse_one("x?\n.a");
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "x"));
        let marker = match &items[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected marker list, got {:?}", other),
        };
        assert_eq!(marker.len(), 1);
        assert!(matches!(&marker[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM));
        let stage = match &items[3].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == OOP_BARE_SYM));
        assert!(matches!(&stage[1].kind, FormKind::Symbol(sym) if sym == "a"));
    }

    #[test]
    fn oop_chain_accepts_regex_stage() {
        let form = parse_one(r#""21".#/\d\d/"#);
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 3);
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM));
        assert!(matches!(&items[1].kind, FormKind::String(s) if s == "21"));
        let stage = match &items[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == OOP_DOT_STAGE_SYM));
        let placeholder = match &stage[1].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        let body = match &stage[2].kind {
            FormKind::List(items) => items,
            other => panic!("expected dot body list, got {:?}", other),
        };
        assert!(matches!(&body[0].kind, FormKind::Regex { .. }));
        assert!(matches!(&body[1].kind, FormKind::Symbol(sym) if sym == &placeholder));
    }

    #[test]
    fn oop_chain_keyword_indexer_sugar_matches_dot_indexer() {
        let sugar = parse_one("m:a");
        let explicit = parse_one("m.:a");
        assert_form_equivalent(&sugar, &explicit, "m:a vs m.:a");
    }

    #[test]
    fn oop_chain_keyword_indexer_sugar_allows_chaining() {
        let sugar = parse_one("m:c:d");
        let explicit = parse_one("m.:c.:d");
        assert_form_equivalent(&sugar, &explicit, "m:c:d vs m.:c.:d");
    }

    #[test]
    fn keyword_literal_with_colons_is_single_token() {
        let form = parse_one(":m:a");
        assert!(matches!(&form.kind, FormKind::Keyword(name) if name == "m:a"));
    }

    #[test]
    fn namespace_separator_is_not_split() {
        let form = parse_one("foo::bar");
        assert!(matches!(&form.kind, FormKind::Symbol(name) if name == "foo::bar"));
    }

    #[test]
    fn map_label_keys_with_values_remain_valid() {
        let shorthand = parse_one("{a: 1 b: 2}");
        let explicit = parse_one("{:a 1 :b 2}");
        assert_form_equivalent(&shorthand, &explicit, "map label keys");
    }

    #[test]
    fn symbol_with_colon_type_annotation_is_not_split() {
        let form = parse_one("x:Int");
        assert!(matches!(&form.kind, FormKind::Symbol(name) if name == "x:Int"));
        assert!(form.type_hint.is_none());
    }

    #[test]
    fn dot_chain_handles_map_updates() {
        let src = "{:a [10 20] :b 2}.(update-in ? [:a 0] inc)";
        let mut reader = Reader::new_with_options(src, ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert_eq!(items.len(), 4);
        let placeholder = match &items[2].kind {
            FormKind::Symbol(sym) => sym.clone(),
            other => panic!("expected placeholder symbol, got {:?}", other),
        };
        let stage = match &items[3].kind {
            FormKind::List(parts) => parts,
            other => panic!("expected stage list, got {:?}", other),
        };
        assert!(matches!(&stage[0].kind, FormKind::Symbol(sym) if sym == "update-in"));
        assert!(matches!(&stage[1].kind, FormKind::Symbol(sym) if sym == &placeholder));
        match &stage[2].kind {
            FormKind::Vector(entries) => {
                assert_eq!(entries.len(), 2);
            }
            other => panic!("expected vector path, got {:?}", other),
        }
        assert!(matches!(&stage[3].kind, FormKind::Symbol(sym) if sym == "inc"));
    }

    #[test]
    fn read_use_form_symbol() {
        let mut reader =
            Reader::new_with_options("(use dotchain-syntax false)", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let head = match &forms[0].kind {
            FormKind::List(items) => items.first().expect("list must have head").clone(),
            other => panic!("expected list, got {:?}", other),
        };
        match head.kind {
            FormKind::Symbol(ref sym) => assert_eq!(sym, "use"),
            other => panic!("expected symbol, got {:?}", other),
        }
    }

    #[test]
    fn indexer_forms_marked_with_placeholder_symbols() {
        let mut reader = Reader::new_with_options("m[:a]", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        let head_sym = match &items[0].kind {
            FormKind::Symbol(sym) => sym,
            other => panic!("expected symbol head, got {:?}", other),
        };
        assert!(
            head_sym == INDEX_GET_SYM,
            "expected {}, got {}",
            INDEX_GET_SYM,
            head_sym
        );
        let mut reader = Reader::new_with_options("m[:a :b || 0]", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        let head_sym = match &items[0].kind {
            FormKind::Symbol(sym) => sym,
            other => panic!("expected symbol head, got {:?}", other),
        };
        assert!(
            head_sym == INDEX_GET_IN_SYM,
            "expected {}, got {}",
            INDEX_GET_IN_SYM,
            head_sym
        );
        let mut reader = Reader::new_with_options("xs[1,5,7]", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        let head_sym = match &items[0].kind {
            FormKind::Symbol(sym) => sym,
            other => panic!("expected symbol head, got {:?}", other),
        };
        assert!(
            head_sym == INDEX_GET_MANY_SYM,
            "expected {}, got {}",
            INDEX_GET_MANY_SYM,
            head_sym
        );
    }

    #[test]
    fn indexer_accepts_negative_indexes() {
        let mut reader = Reader::new_with_options("xs[-1]", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == INDEX_GET_SYM));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "xs"));
        assert!(matches!(&items[2].kind, FormKind::Int(n) if *n == -1));
    }

    #[test]
    fn indexer_accepts_negative_range_bounds() {
        let mut reader = Reader::new_with_options("letters[-3..-1]", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        let items = match &forms[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == INDEX_GET_SYM));
        assert!(matches!(&items[1].kind, FormKind::Symbol(sym) if sym == "letters"));
        match &items[2].kind {
            FormKind::List(range_items) => {
                assert!(
                    matches!(&range_items[0].kind, FormKind::Symbol(sym) if sym == RANGE_LITERAL_SYM)
                );
                assert!(matches!(&range_items[1].kind, FormKind::Int(n) if *n == -3));
                assert!(matches!(&range_items[2].kind, FormKind::Int(n) if *n == -1));
                assert!(matches!(&range_items[3].kind, FormKind::Bool(exclusive) if !exclusive));
            }
            other => panic!("expected range literal, got {:?}", other),
        }
    }

    #[test]
    fn indexer_after_oop_chain_is_lowered() {
        let form = parse_one(r#""path".glob[0]"#);
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == INDEX_GET_SYM));
        match &items[1].kind {
            FormKind::List(oop_items) => {
                assert!(
                    matches!(&oop_items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM)
                );
            }
            other => panic!("expected oop form, got {:?}", other),
        }
        assert!(matches!(&items[2].kind, FormKind::Int(n) if *n == 0));
    }

    #[test]
    fn indexer_after_oop_chain_supports_many() {
        let form = parse_one(r#""path".glob[0,3,7]"#);
        let items = match &form.kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == INDEX_GET_MANY_SYM));
        match &items[2].kind {
            FormKind::Vector(indices) => {
                assert!(matches!(&indices[0].kind, FormKind::Int(n) if *n == 0));
                assert!(matches!(&indices[1].kind, FormKind::Int(n) if *n == 3));
                assert!(matches!(&indices[2].kind, FormKind::Int(n) if *n == 7));
            }
            other => panic!("expected vector indices, got {:?}", other),
        }
    }

    #[test]
    fn parses_dollar_brace_as_rb_block() {
        let form = parse_one("${1 + 2 * 3}");
        match &form.kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(code.trim(), "1 + 2 * 3");
            }
            other => panic!("expected ForeignRaw, got {:?}", other),
        }
    }

    #[test]
    fn parses_dollar_brace_with_suffix() {
        let form = parse_one("${foo}.bar");
        match &form.kind {
            FormKind::ForeignRaw { tag, code } => {
                assert_eq!(tag.as_deref(), Some("rb"));
                assert_eq!(code, "(foo).bar");
            }
            other => panic!("expected ForeignRaw, got {:?}", other),
        }
    }

    #[test]
    fn discard_reader_macro_skips_next_form() {
        let mut reader = Reader::new_with_options("1 #_2 3", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 2);
        assert!(matches!(forms[0].kind, FormKind::Int(1)));
        assert!(matches!(forms[1].kind, FormKind::Int(3)));
    }

    #[test]
    fn reads_regex_literals() {
        let mut reader = Reader::new_with_options("#/a+b/ /foo \\/ bar/", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 2);
        assert!(matches!(
            &forms[0].kind,
            FormKind::Regex { pattern, delim } if pattern == "a+b" && matches!(delim, crate::ast::RegexDelim::HashSlash)
        ));
        assert!(matches!(
            &forms[1].kind,
            FormKind::Regex { pattern, delim } if pattern == "foo \\/ bar" && matches!(delim, crate::ast::RegexDelim::Slash)
        ));
    }

    #[test]
    fn splits_symbol_type_annotation() {
        let result = split_symbol_type_annotation("xs<Vector<Map<Int, Str>>>").expect("annotation");
        assert_eq!(result.0, "xs");
        assert_eq!(result.1, "Vector<Map<Int, Str>>");
    }

    #[test]
    fn reader_attaches_type_hint_to_symbol() {
        let mut reader = Reader::new_with_options("foo<Int[]>", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        match &forms[0].kind {
            FormKind::Symbol(name) => assert_eq!(name, "foo<Int[]>"),
            other => panic!("expected symbol, got {:?}", other),
        }
        let hint = forms[0].type_hint.as_ref().expect("type hint");
        assert_eq!(hint.kind, TypeKind::vector(TypeKind::Int));
        assert!(hint.from_syntax);
    }

    #[test]
    fn reader_attaches_postfix_type_hint_to_list() {
        let mut reader = Reader::new_with_options("(inc 1)<Int>", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(&forms[0].kind, FormKind::List(_)));
        let hint = forms[0].type_hint.as_ref().expect("type hint");
        assert_eq!(hint.kind, TypeKind::Int);
        assert!(hint.from_syntax);
    }

    #[test]
    fn reader_attaches_postfix_type_hint_to_list_colon() {
        let mut reader = Reader::new_with_options("(inc 1): Int", ReaderOptions::default());
        let forms = reader.read_all().unwrap();
        assert_eq!(forms.len(), 1);
        assert!(matches!(&forms[0].kind, FormKind::List(_)));
        let hint = forms[0].type_hint.as_ref().expect("type hint");
        assert_eq!(hint.kind, TypeKind::Int);
        assert!(hint.from_syntax);
    }
}

fn is_label_style_key(sym: &str) -> bool {
    let bytes = sym.as_bytes();
    if bytes.len() < 2 {
        return false;
    }
    if bytes[bytes.len() - 1] != b':' {
        return false;
    }
    let body = &bytes[..bytes.len() - 1];
    if body.is_empty() {
        return false;
    }
    let first = body[0] as char;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    for b in &body[1..] {
        let c = *b as char;
        if !(c.is_ascii_alphanumeric() || c == '_' || c == '-') {
            return false;
        }
    }
    true
}

fn split_symbol_type_annotation(symbol: &str) -> Option<(&str, &str)> {
    let start_idx = symbol.find('<')?;
    if start_idx == 0 {
        return None;
    }
    let mut depth = 1usize;
    let mut end_idx = None;
    let type_start = start_idx + 1;
    for (offset, ch) in symbol[type_start..].char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                depth -= 1;
                if depth == 0 {
                    end_idx = Some(type_start + offset);
                    break;
                }
            }
            _ => {}
        }
    }
    let end_idx = end_idx?;
    if end_idx + 1 != symbol.len() {
        return None;
    }
    let name = &symbol[..start_idx];
    let annotation = &symbol[start_idx + 1..end_idx];
    if name.is_empty() || annotation.is_empty() {
        return None;
    }
    Some((name, annotation))
}
