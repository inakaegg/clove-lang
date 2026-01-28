use crate::ast::{
    desugar_interpolated_regex, desugar_interpolated_string, Form, FormKind, InterpolatedPart,
    MapItem, Span,
};
use crate::error::CloveError;
use crate::reader::{
    INDEX_GET_IN_SYM, INDEX_GET_MANY_SYM, INDEX_GET_SYM, INFIX_SYNTAX_SYM, OOP_INDEX_SYM,
    OOP_SEG_SYM,
};

pub const APPLY_SYM: &str = "__apply";
pub const RANGE_SYM: &str = "__range_literal";

pub struct Compiler {
    engine_tags: Vec<String>,
}

impl Compiler {
    pub fn new(engine_tags: Vec<String>) -> Self {
        Self { engine_tags }
    }

    pub fn compile(&self, forms: Vec<Form>) -> Result<Vec<Form>, CloveError> {
        forms
            .into_iter()
            .map(|form| self.lower_form(form, false))
            .collect()
    }

    // Helper for backward compatibility
    pub fn compile_from_source(
        &self,
        _source: &str,
        forms: Vec<Form>,
    ) -> Result<Vec<Form>, CloveError> {
        self.compile(forms)
    }

    fn lower_form(&self, form: Form, in_quote: bool) -> Result<Form, CloveError> {
        if let Some(lowered) = desugar_interpolated_string(&form) {
            return self.lower_form(lowered, in_quote);
        }
        if let Some(lowered) = desugar_interpolated_regex(&form) {
            return self.lower_form(lowered, in_quote);
        }
        if in_quote {
            return Ok(form);
        }
        if let FormKind::Symbol(ref name) = form.kind {
            if let Some(range_form) = self.lower_range_literal(&form, name)? {
                return Ok(range_form);
            }
        }
        if let FormKind::List(items) = &form.kind {
            if items.len() == 1 {
                if let FormKind::Symbol(name) = &items[0].kind {
                    if let Some(range_form) = self.lower_range_literal(&items[0], name)? {
                        return Ok(range_form);
                    }
                }
            }
        }
        match form.kind {
            FormKind::List(items) => self.lower_list(form.span, items),
            FormKind::Vector(items) => {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_form(item, false)?);
                }
                Ok(Form::new(FormKind::Vector(lowered), form.span))
            }
            FormKind::Map(entries) => {
                let mut lowered_entries = Vec::with_capacity(entries.len());
                for entry in entries {
                    match entry {
                        MapItem::KeyValue(k, v) => {
                            lowered_entries.push(MapItem::KeyValue(k, self.lower_form(v, false)?));
                        }
                        MapItem::Spread(expr) => {
                            lowered_entries.push(MapItem::Spread(self.lower_form(expr, false)?));
                        }
                    }
                }
                Ok(Form::new(FormKind::Map(lowered_entries), form.span))
            }
            FormKind::Set(items) => {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_form(item, false)?);
                }
                Ok(Form::new(FormKind::Set(lowered), form.span))
            }
            _ => Ok(form),
        }
    }

    fn lower_list(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.is_empty() {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let items = items;
        if matches!(
            items
                .first()
                .map(|it| &it.kind),
            Some(FormKind::Symbol(name)) if name == "."
        ) {
            let rewritten = self.rewrite_pipe_chain(span, items)?;
            return self.lower_list(span, rewritten);
        }
        if matches!(
            items
                .last()
                .map(|it| &it.kind),
            Some(FormKind::Symbol(name)) if name == "."
        ) {
            let rewritten = self.rewrite_backward_composition(span, items)?;
            return self.lower_list(span, rewritten);
        }
        let head_symbol = match &items[0].kind {
            FormKind::Symbol(name) => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = head_symbol {
            if name == INFIX_SYNTAX_SYM {
                if items.len() != 2 {
                    return Err(CloveError::parse(
                        "clove.syntax::infix expects exactly one expression",
                    )
                    .with_span(span));
                }
                let lowered = lower_infix_expr(&items[1]);
                return self.lower_form(lowered, false);
            }
            if name == "quote" {
                return Ok(Form::new(FormKind::List(items), span));
            }
            if is_special_form(name) {
                return self.lower_special_form(span, items);
            }
            if name == INDEX_GET_SYM
                || name == INDEX_GET_IN_SYM
                || name == INDEX_GET_MANY_SYM
                || name == OOP_INDEX_SYM
                || name == OOP_SEG_SYM
            {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_form(item, false)?);
                }
                return Ok(Form::new(FormKind::List(lowered), span));
            }
            if self.engine_tags.iter().any(|t| t == name) || name == APPLY_SYM || name == RANGE_SYM
            {
                let mut lowered = Vec::with_capacity(items.len());
                for item in items {
                    lowered.push(self.lower_form(item, false)?);
                }
                return Ok(Form::new(FormKind::List(lowered), span));
            }
        }

        let mut lowered = Vec::with_capacity(items.len());
        for item in items {
            lowered.push(self.lower_form(item, false)?);
        }
        let mut wrapped = Vec::with_capacity(lowered.len() + 1);
        wrapped.push(Form::new(FormKind::Symbol(APPLY_SYM.to_string()), span));
        wrapped.extend(lowered);
        Ok(Form::new(FormKind::List(wrapped), span))
    }

    fn rewrite_pipe_chain(
        &self,
        span: Span,
        mut items: Vec<Form>,
    ) -> Result<Vec<Form>, CloveError> {
        items.remove(0);
        if items.is_empty() {
            return Err(CloveError::parse(format!(
                "unknown:{}:{}: (. f g ...) requires at least one function",
                span.line, span.col
            )));
        }
        let mut rewritten = Vec::with_capacity(items.len() + 1);
        rewritten.push(Form::new(FormKind::Symbol("pipe".to_string()), span));
        rewritten.extend(items);
        Ok(rewritten)
    }

    fn rewrite_backward_composition(
        &self,
        span: Span,
        mut items: Vec<Form>,
    ) -> Result<Vec<Form>, CloveError> {
        items.pop();
        if items.is_empty() {
            return Err(CloveError::parse(format!(
                "unknown:{}:{}: (f g .) requires at least one function",
                span.line, span.col
            )));
        }
        let mut rewritten = Vec::with_capacity(items.len() + 1);
        rewritten.push(Form::new(FormKind::Symbol("comp".to_string()), span));
        rewritten.extend(items);
        Ok(rewritten)
    }

    fn lower_special_form(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        let head_name = match &items[0].kind {
            FormKind::Symbol(name) => name.as_str(),
            _ => return Ok(Form::new(FormKind::List(items), span)),
        };
        match head_name {
            "def" => self.lower_def(span, items),
            "def-" => self.lower_def(span, items),
            "defn" => self.lower_defn(span, items),
            "defn-" => self.lower_defn(span, items),
            "method" => self.lower_fn(span, items),
            "-def" => self.lower_def(span, items),
            "-defn" => self.lower_defn(span, items),
            "let" => self.lower_let(span, items),
            "if" => self.lower_if(span, items),
            "do" => self.lower_plain_list(span, items),
            "where" => self.lower_plain_list(span, items),
            "fn" => self.lower_fn(span, items),
            "set!" => self.lower_set(span, items),
            "redef" => self.lower_set(span, items),
            "__set-in-chain" => self.lower_plain_list(span, items),
            "and" | "or" | "when" | "when-not" | "cond" | "cond->" | "cond->>" | "->" | "->>"
            | "as->" | "some->" | "some->>" | "throw" | "recur" | "try" | "catch" | "finally"
            | "comment" | "while" | "condp" | "for" | "with-redefs" => {
                self.lower_plain_list(span, items)
            }
            "when-let" | "if-let" | "if-some" | "doseq" | "each" | "dotimes" | "with-open"
            | "map" | "filter" | "pmap" | "pfilter" | "dag::pmap" | "dag::pfilter"
            | "std::pmap" | "std::pfilter" => self.lower_binding_form(span, items),
            "break" => Ok(Form::new(FormKind::List(items), span)),
            _ => Ok(Form::new(FormKind::List(items), span)),
        }
    }

    fn lower_def(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() < 3 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::with_capacity(items.len());
        // def name <doc/meta?> value: keep name, lower only value
        lowered.push(items[0].clone());
        lowered.push(items[1].clone());
        let mut idx = 2;
        while idx + 1 < items.len() {
            if matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_)) {
                lowered.push(items[idx].clone());
                idx += 1;
            } else {
                break;
            }
        }
        if idx < items.len() {
            lowered.push(self.lower_form(items[idx].clone(), false)?);
            for item in items.into_iter().skip(idx + 1) {
                lowered.push(self.lower_form(item, false)?);
            }
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_defn(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() < 3 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::with_capacity(items.len());
        lowered.push(items[0].clone()); // defn
        lowered.push(items[1].clone()); // name
        let mut idx = 2;
        while idx < items.len() && matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_))
        {
            lowered.push(items[idx].clone()); // docstring or attr-map
            idx += 1;
        }
        if let Some(first) = items.get(idx) {
            match &first.kind {
                FormKind::Vector(_) => {
                    lowered.push(first.clone());
                    idx += 1;
                    for item in items.into_iter().skip(idx) {
                        lowered.push(self.lower_form(item, false)?);
                    }
                }
                FormKind::List(_) => {
                    for clause in items.into_iter().skip(idx) {
                        lowered.push(self.lower_fn_clause(clause)?);
                    }
                }
                _ => {
                    for item in items.into_iter().skip(idx) {
                        lowered.push(item);
                    }
                }
            }
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_let(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() < 3 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::with_capacity(items.len());
        lowered.push(items[0].clone());
        lowered.push(self.lower_let_bindings(items[1].clone())?);
        for item in items.iter().skip(2) {
            lowered.push(self.lower_form(item.clone(), false)?);
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_let_bindings(&self, form: Form) -> Result<Form, CloveError> {
        match form.kind {
            FormKind::Vector(binds) => {
                let mut lowered = Vec::with_capacity(binds.len());
                for (idx, bind) in binds.into_iter().enumerate() {
                    if idx % 2 == 0 {
                        lowered.push(bind);
                    } else {
                        lowered.push(self.lower_form(bind, false)?);
                    }
                }
                Ok(Form::new(FormKind::Vector(lowered), form.span))
            }
            _ => Ok(form),
        }
    }

    fn lower_if(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        let mut lowered = Vec::with_capacity(items.len());
        for (idx, item) in items.into_iter().enumerate() {
            if idx == 0 {
                lowered.push(item);
            } else {
                lowered.push(self.lower_form(item, false)?);
            }
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_plain_list(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        let mut lowered = Vec::with_capacity(items.len());
        for (idx, item) in items.into_iter().enumerate() {
            if idx == 0 {
                lowered.push(item);
            } else {
                lowered.push(self.lower_form(item, false)?);
            }
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_fn(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() < 2 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::with_capacity(items.len());
        lowered.push(items[0].clone());
        let mut idx = 1;
        if matches!(items.get(1).map(|f| &f.kind), Some(FormKind::Symbol(_))) {
            lowered.push(items[1].clone());
            idx += 1;
        }
        if idx >= items.len() {
            return Ok(Form::new(FormKind::List(items), span));
        }
        match &items[idx].kind {
            FormKind::Vector(_) => {
                lowered.push(items[idx].clone());
                idx += 1;
                for item in items.iter().skip(idx) {
                    lowered.push(self.lower_form(item.clone(), false)?);
                }
            }
            FormKind::List(_) => {
                for clause in items.iter().skip(idx) {
                    lowered.push(self.lower_fn_clause(clause.clone())?);
                }
            }
            _ => {
                for item in items.iter().skip(idx) {
                    lowered.push(item.clone());
                }
            }
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_set(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() != 3 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::new();
        lowered.push(items[0].clone());
        lowered.push(items[1].clone()); // target symbol
        lowered.push(self.lower_form(items[2].clone(), false)?);
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_range_literal(&self, form: &Form, symbol: &str) -> Result<Option<Form>, CloveError> {
        let (start, end, exclusive) = match parse_range_parts(symbol, form.span)? {
            Some(parts) => parts,
            None => return Ok(None),
        };
        let mut items = Vec::with_capacity(4);
        items.push(Form::new(
            FormKind::Symbol(RANGE_SYM.to_string()),
            form.span,
        ));
        items.push(range_bound_form(start, form.span));
        items.push(range_bound_form(end, form.span));
        items.push(Form::new(FormKind::Bool(exclusive), form.span));
        Ok(Some(Form::new(FormKind::List(items), form.span)))
    }

    fn lower_binding_form(&self, span: Span, items: Vec<Form>) -> Result<Form, CloveError> {
        if items.len() < 2 {
            return Ok(Form::new(FormKind::List(items), span));
        }
        let mut lowered = Vec::with_capacity(items.len());
        lowered.push(items[0].clone());
        let mut idx = 1;
        if matches!(items.get(idx).map(|f| &f.kind), Some(FormKind::Map(_)))
            && matches!(
                items.get(idx + 1).map(|f| &f.kind),
                Some(FormKind::Vector(_))
            )
        {
            lowered.push(self.lower_form(items[idx].clone(), false)?);
            idx += 1;
        }
        lowered.push(self.lower_let_bindings(items[idx].clone())?);
        for item in items.into_iter().skip(idx + 1) {
            lowered.push(self.lower_form(item, false)?);
        }
        Ok(Form::new(FormKind::List(lowered), span))
    }

    fn lower_fn_clause(&self, form: Form) -> Result<Form, CloveError> {
        match form.kind {
            FormKind::List(items) if !items.is_empty() => {
                let mut lowered = Vec::with_capacity(items.len());
                lowered.push(items[0].clone());
                for item in items.into_iter().skip(1) {
                    lowered.push(self.lower_form(item, false)?);
                }
                Ok(Form::new(FormKind::List(lowered), form.span))
            }
            _ => Ok(form),
        }
    }
}

fn lower_infix_expr(form: &Form) -> Form {
    let with_kind = |kind| {
        let mut out = Form::new(kind, form.span);
        out.type_hint = form.type_hint.clone();
        out
    };
    match &form.kind {
        FormKind::List(items) => {
            if items.is_empty() {
                return form.clone();
            }
            let head = match &items[0].kind {
                FormKind::Symbol(sym) => sym.as_str(),
                _ => "",
            };
            if matches!(head, "==" | "=") && items.len() == 3 {
                let lhs = lower_infix_expr(&items[1]);
                let rhs = lower_infix_expr(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("=".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if matches!(head, "!=" | "not=") && items.len() == 3 {
                let lhs = lower_infix_expr(&items[1]);
                let rhs = lower_infix_expr(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("not=".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "%" && items.len() == 3 {
                let lhs = lower_infix_expr(&items[1]);
                let rhs = lower_infix_expr(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("mod".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "&&" && items.len() == 3 {
                let lhs = lower_infix_expr(&items[1]);
                let rhs = lower_infix_expr(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("and".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "||" && items.len() == 3 {
                let lhs = lower_infix_expr(&items[1]);
                let rhs = lower_infix_expr(&items[2]);
                let mut new_items = Vec::with_capacity(3);
                new_items.push(Form::new(FormKind::Symbol("or".into()), items[0].span));
                new_items.push(lhs);
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            if head == "!" && items.len() == 2 {
                let rhs = lower_infix_expr(&items[1]);
                let mut new_items = Vec::with_capacity(2);
                new_items.push(Form::new(FormKind::Symbol("not".into()), items[0].span));
                new_items.push(rhs);
                return with_kind(FormKind::List(new_items));
            }
            let mut lowered_items = Vec::with_capacity(items.len());
            for item in items {
                lowered_items.push(lower_infix_expr(item));
            }
            with_kind(FormKind::List(lowered_items))
        }
        FormKind::Vector(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr(item));
            }
            with_kind(FormKind::Vector(lowered))
        }
        FormKind::Set(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr(item));
            }
            with_kind(FormKind::Set(lowered))
        }
        FormKind::Map(entries) => {
            let mut lowered = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let key = lower_infix_expr(k);
                        let value = lower_infix_expr(v);
                        lowered.push(MapItem::KeyValue(key, value));
                    }
                    MapItem::Spread(expr) => {
                        let value = lower_infix_expr(expr);
                        lowered.push(MapItem::Spread(value));
                    }
                }
            }
            with_kind(FormKind::Map(lowered))
        }
        FormKind::InterpolatedString(parts) => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        lowered.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        lowered.push(InterpolatedPart::Expr(lower_infix_expr(expr)));
                    }
                }
            }
            with_kind(FormKind::InterpolatedString(lowered))
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut lowered = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => {
                        lowered.push(InterpolatedPart::Text(text.clone()));
                    }
                    InterpolatedPart::Expr(expr) => {
                        lowered.push(InterpolatedPart::Expr(lower_infix_expr(expr)));
                    }
                }
            }
            with_kind(FormKind::InterpolatedRegex {
                parts: lowered,
                delim: *delim,
            })
        }
        FormKind::ShortFn(items) => {
            let mut lowered = Vec::with_capacity(items.len());
            for item in items {
                lowered.push(lower_infix_expr(item));
            }
            with_kind(FormKind::ShortFn(lowered))
        }
        _ => form.clone(),
    }
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "def"
            | "def-"
            | "defn"
            | "defn-"
            | "-def"
            | "-defn"
            | "method"
            | "deftype"
            | "defenum"
            | "for"
            | "condp"
            | "with-redefs"
            | "with-open"
            | "doto"
            | "describe"
            | "describe-type"
            | "infer-type"
            | "enum-members"
            | "let"
            | "if"
            | "do"
            | "where"
            | "fn"
            | "loop"
            | "set!"
            | "redef"
            | "__set-in-chain"
            | "and"
            | "or"
            | "when"
            | "when-not"
            | "if-not"
            | "when-let"
            | "if-let"
            | "if-some"
            | "cond"
            | "cond->"
            | "cond->>"
            | "->"
            | "->>"
            | "as->"
            | "some->"
            | "some->>"
            | "throw"
            | "recur"
            | "try"
            | "catch"
            | "finally"
            | "err"
            | "fin"
            | "p"
            | "comment"
            | "while"
            | "doseq"
            | "each"
            | "dotimes"
            | "map"
            | "filter"
            | "pmap"
            | "pfilter"
            | "dag::pmap"
            | "dag::pfilter"
            | "std::pmap"
            | "std::pfilter"
            | "match"
            | "break"
            | "current-ns"
            | "repl"
            | "debug"
            | "use"
            | "use-syntax"
            | "ns-map"
            | "nav"
            | "lookup"
            | "create-ns"
            | "refer"
            | "resolve"
            | "load-file"
            | "load-string"
            | "delay"
            | "eval"
            | "with-dyn"
            | "go-loop"
            | "async-scope"
            | "async::scope"
            | "scope-loop"
            | "async::scope-loop"
    )
}

fn range_bound_form(bound: Option<i64>, span: Span) -> Form {
    match bound {
        Some(n) => Form::new(FormKind::Int(n), span),
        None => Form::new(FormKind::Nil, span),
    }
}

fn parse_range_parts(
    symbol: &str,
    span: Span,
) -> Result<Option<(Option<i64>, Option<i64>, bool)>, CloveError> {
    let (sep, exclusive) = if let Some(idx) = symbol.find("...") {
        (idx, true)
    } else if let Some(idx) = symbol.find("..") {
        (idx, false)
    } else {
        return Ok(None);
    };
    let start_part = &symbol[..sep];
    let end_part = &symbol[sep + if exclusive { 3 } else { 2 }..];
    let start = if start_part.is_empty() {
        None
    } else {
        Some(start_part.parse::<i64>().map_err(|_| {
            CloveError::parse(format!(
                "unknown:{}:{}: invalid range start: {}",
                span.line, span.col, symbol
            ))
        })?)
    };
    let end = if end_part.is_empty() {
        None
    } else {
        Some(end_part.parse::<i64>().map_err(|_| {
            CloveError::parse(format!(
                "unknown:{}:{}: invalid range end: {}",
                span.line, span.col, symbol
            ))
        })?)
    };
    Ok(Some((start, end, exclusive)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn range_literal_lowering() {
        let compiler = Compiler::new(vec![]);
        let form = Form::new(
            FormKind::Symbol("1..3".to_string()),
            Span {
                line: 1,
                col: 1,
                index: 0,
            },
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        assert_eq!(lowered.len(), 1);
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert_eq!(items.len(), 4);
                assert!(matches!(items[0].kind, FormKind::Symbol(ref s) if s == RANGE_SYM));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn range_literal_in_indexer_includes_negative_bounds() {
        let compiler = Compiler::new(vec![]);
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol(INDEX_GET_SYM.to_string()), span),
                Form::new(FormKind::Symbol("letters".to_string()), span),
                Form::new(FormKind::Symbol("-3..-1".to_string()), span),
            ]),
            span,
        );
        let lowered = compiler.compile(vec![form]).expect("lowering");
        assert_eq!(lowered.len(), 1);
        let call_items = match &lowered[0].kind {
            FormKind::List(items) => items,
            other => panic!("expected list, got {:?}", other),
        };
        assert!(matches!(&call_items[0].kind, FormKind::Symbol(sym) if sym == INDEX_GET_SYM));
        assert!(matches!(&call_items[1].kind, FormKind::Symbol(sym) if sym == "letters"));
        match &call_items[2].kind {
            FormKind::List(items) => {
                assert!(matches!(&items[0].kind, FormKind::Symbol(sym) if sym == RANGE_SYM));
                assert!(matches!(&items[1].kind, FormKind::Int(n) if *n == -3));
                assert!(matches!(&items[2].kind, FormKind::Int(n) if *n == -1));
                assert!(matches!(&items[3].kind, FormKind::Bool(exclusive) if !exclusive));
            }
            other => panic!("expected range literal call, got {:?}", other),
        }
    }

    #[test]
    fn prefix_dot_rewrites_to_pipe_call() {
        let compiler = Compiler::new(vec![]);
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol(".".to_string()), span),
                Form::new(FormKind::Symbol("foo".to_string()), span),
                Form::new(FormKind::Symbol("bar".to_string()), span),
            ]),
            span,
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        assert_eq!(lowered.len(), 1);
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert!(matches!(items[0].kind, FormKind::Symbol(ref s) if s == APPLY_SYM));
                assert!(matches!(items[1].kind, FormKind::Symbol(ref s) if s == "pipe"));
                assert!(matches!(items[2].kind, FormKind::Symbol(ref s) if s == "foo"));
                assert!(matches!(items[3].kind, FormKind::Symbol(ref s) if s == "bar"));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn prefix_dot_handles_multiple_functions() {
        let compiler = Compiler::new(vec![]);
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol(".".to_string()), span),
                Form::new(FormKind::Symbol("a".to_string()), span),
                Form::new(FormKind::Symbol("b".to_string()), span),
                Form::new(FormKind::Symbol("c".to_string()), span),
            ]),
            span,
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert_eq!(items.len(), 5);
                assert!(matches!(items[1].kind, FormKind::Symbol(ref s) if s == "pipe"));
                assert!(matches!(items[2].kind, FormKind::Symbol(ref s) if s == "a"));
                assert!(matches!(items[3].kind, FormKind::Symbol(ref s) if s == "b"));
                assert!(matches!(items[4].kind, FormKind::Symbol(ref s) if s == "c"));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn suffix_dot_rewrites_to_comp_call() {
        let compiler = Compiler::new(vec![]);
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("foo".to_string()), span),
                Form::new(FormKind::Symbol("bar".to_string()), span),
                Form::new(FormKind::Symbol(".".to_string()), span),
            ]),
            span,
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        assert_eq!(lowered.len(), 1);
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert!(matches!(items[0].kind, FormKind::Symbol(ref s) if s == APPLY_SYM));
                assert!(matches!(items[1].kind, FormKind::Symbol(ref s) if s == "comp"));
                assert!(matches!(items[2].kind, FormKind::Symbol(ref s) if s == "foo"));
                assert!(matches!(items[3].kind, FormKind::Symbol(ref s) if s == "bar"));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn suffix_dot_supports_multiple_functions() {
        let compiler = Compiler::new(vec![]);
        let span = Span {
            line: 1,
            col: 1,
            index: 0,
        };
        let form = Form::new(
            FormKind::List(vec![
                Form::new(FormKind::Symbol("a".to_string()), span),
                Form::new(FormKind::Symbol("b".to_string()), span),
                Form::new(FormKind::Symbol("c".to_string()), span),
                Form::new(FormKind::Symbol(".".to_string()), span),
            ]),
            span,
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert_eq!(items.len(), 5);
                assert!(matches!(items[1].kind, FormKind::Symbol(ref s) if s == "comp"));
                assert!(matches!(items[2].kind, FormKind::Symbol(ref s) if s == "a"));
                assert!(matches!(items[3].kind, FormKind::Symbol(ref s) if s == "b"));
                assert!(matches!(items[4].kind, FormKind::Symbol(ref s) if s == "c"));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn invalid_range_reports_parse_error() {
        let compiler = Compiler::new(vec![]);
        let form = Form::new(
            FormKind::Symbol("1..x".to_string()),
            Span {
                line: 10,
                col: 5,
                index: 0,
            },
        );
        let err = compiler.compile(vec![form]).unwrap_err();
        match err {
            CloveError::Parse(data) => {
                assert!(
                    data.message.contains("10:5") && data.message.contains("1..x"),
                    "unexpected message: {}",
                    data.message
                );
            }
            other => panic!("expected parse error, got {:?}", other),
        }
    }

    #[test]
    fn apply_rewrite_injects_symbol() {
        let compiler = Compiler::new(vec![]);
        let form = Form::new(
            FormKind::List(vec![
                Form::new(
                    FormKind::Symbol("foo".to_string()),
                    Span {
                        line: 1,
                        col: 1,
                        index: 0,
                    },
                ),
                Form::new(
                    FormKind::Int(1),
                    Span {
                        line: 1,
                        col: 5,
                        index: 0,
                    },
                ),
            ]),
            Span {
                line: 1,
                col: 1,
                index: 0,
            },
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert!(matches!(items[0].kind, FormKind::Symbol(ref s) if s == APPLY_SYM));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }

    #[test]
    fn special_forms_are_not_rewritten() {
        let compiler = Compiler::new(vec![]);
        let form = Form::new(
            FormKind::List(vec![
                Form::new(
                    FormKind::Symbol("if".to_string()),
                    Span {
                        line: 1,
                        col: 1,
                        index: 0,
                    },
                ),
                Form::new(
                    FormKind::Bool(true),
                    Span {
                        line: 1,
                        col: 4,
                        index: 0,
                    },
                ),
                Form::new(
                    FormKind::Int(1),
                    Span {
                        line: 1,
                        col: 9,
                        index: 0,
                    },
                ),
                Form::new(
                    FormKind::Int(2),
                    Span {
                        line: 1,
                        col: 11,
                        index: 0,
                    },
                ),
            ]),
            Span {
                line: 1,
                col: 1,
                index: 0,
            },
        );
        let lowered = compiler.compile(vec![form]).unwrap();
        match &lowered[0].kind {
            FormKind::List(items) => {
                assert!(matches!(items[0].kind, FormKind::Symbol(ref s) if s == "if"));
            }
            other => panic!("expected list, got {:?}", other),
        }
    }
}
