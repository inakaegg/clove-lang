use crate::ast::{Form, FormKind, InterpolatedPart, MapItem};
use im::HashSet;

#[derive(Default, Clone, Copy)]
pub struct PlaceholderInfo {
    pub max_index: usize,
    pub has_rest: bool,
}

impl PlaceholderInfo {
    pub fn merge(&mut self, other: &Self) {
        self.max_index = self.max_index.max(other.max_index);
        self.has_rest |= other.has_rest;
    }

    pub fn from_symbol(sym: &str) -> Self {
        match parse_placeholder(sym) {
            Some(PlaceholderRef::Index(i)) => PlaceholderInfo {
                max_index: i,
                has_rest: false,
            },
            Some(PlaceholderRef::Spread(i)) => PlaceholderInfo {
                max_index: i,
                has_rest: false,
            },
            Some(PlaceholderRef::Rest) => PlaceholderInfo {
                max_index: 0,
                has_rest: true,
            },
            None => PlaceholderInfo::default(),
        }
    }
}

pub struct PlaceholderNames {
    pub args: Vec<String>,
    pub rest: Option<String>,
}

impl PlaceholderNames {
    pub fn new(max_index: usize, has_rest: bool) -> Self {
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

    pub fn lookup(&self, sym: &str) -> Option<String> {
        match parse_placeholder(sym) {
            Some(PlaceholderRef::Index(i)) => self.args.get(i.saturating_sub(1)).cloned(),
            Some(PlaceholderRef::Spread(i)) => self
                .args
                .get(i.saturating_sub(1))
                .map(|name| format!("*{}", name)),
            Some(PlaceholderRef::Rest) => self.rest.clone(),
            None => None,
        }
    }
}

#[derive(Clone, Copy)]
pub enum PlaceholderRef {
    Index(usize),
    Rest,
    Spread(usize),
}

pub fn parse_placeholder(sym: &str) -> Option<PlaceholderRef> {
    if sym == "?" {
        return Some(PlaceholderRef::Index(1));
    }
    if sym == "*?" {
        return Some(PlaceholderRef::Spread(1));
    }
    if sym == "%" {
        return Some(PlaceholderRef::Index(1));
    }
    if sym == "%&" {
        return Some(PlaceholderRef::Rest);
    }
    if let Some(num) = sym.strip_prefix('%').and_then(|s| s.parse::<usize>().ok()) {
        if num > 0 {
            return Some(PlaceholderRef::Index(num));
        }
    }
    if let Some(num) = sym.strip_prefix('?').and_then(|s| s.parse::<usize>().ok()) {
        if num > 0 {
            return Some(PlaceholderRef::Index(num));
        }
    }
    if let Some(num) = sym.strip_prefix("*?").and_then(|s| s.parse::<usize>().ok()) {
        if num > 0 {
            return Some(PlaceholderRef::Spread(num));
        }
    }
    None
}

fn is_question_placeholder(sym: &str) -> bool {
    if sym == "?" || sym == "*?" {
        return true;
    }
    if let Some(rest) = sym.strip_prefix('?') {
        return !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit());
    }
    if let Some(rest) = sym.strip_prefix("*?") {
        return !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit());
    }
    false
}

pub fn placeholder_info_for_form(form: &Form) -> PlaceholderInfo {
    placeholder_info_for_form_scoped(form, &HashSet::new())
}

pub fn replace_placeholders(form: &Form, mapping: &PlaceholderNames) -> Form {
    replace_placeholders_scoped(form, mapping, &HashSet::new())
}

fn placeholder_info_for_form_scoped(form: &Form, shadowed: &HashSet<String>) -> PlaceholderInfo {
    match &form.kind {
        FormKind::Symbol(s) => {
            if shadowed.contains(s) {
                PlaceholderInfo::default()
            } else {
                PlaceholderInfo::from_symbol(s)
            }
        }
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "let" {
                    let mut info = PlaceholderInfo::default();
                    let mut shadowed_body = shadowed.clone();
                    if let Some(bind_form) = items.get(1) {
                        if let FormKind::Vector(binds) = &bind_form.kind {
                            let mut idx = 0;
                            while idx + 1 < binds.len() {
                                let val_form = &binds[idx + 1];
                                info.merge(&placeholder_info_for_form_scoped(val_form, shadowed));
                                if let FormKind::Symbol(name) = &binds[idx].kind {
                                    if !is_question_placeholder(name) {
                                        shadowed_body.insert(name.clone());
                                    }
                                }
                                idx += 2;
                            }
                        }
                    }
                    for body in items.iter().skip(2) {
                        info.merge(&placeholder_info_for_form_scoped(body, &shadowed_body));
                    }
                    return info;
                }
                if head == "fn" {
                    let mut shadowed_body = shadowed.clone();
                    if let Some(params_form) = items.get(1) {
                        if let FormKind::Vector(params) = &params_form.kind {
                            for p in params {
                                if let FormKind::Symbol(name) = &p.kind {
                                    if !is_question_placeholder(name) {
                                        shadowed_body.insert(name.clone());
                                    }
                                }
                            }
                        }
                    }
                    let mut info = PlaceholderInfo::default();
                    for body in items.iter().skip(2) {
                        info.merge(&placeholder_info_for_form_scoped(body, &shadowed_body));
                    }
                    return info;
                }
            }
            let mut info = PlaceholderInfo::default();
            for it in items {
                info.merge(&placeholder_info_for_form_scoped(it, shadowed));
            }
            info
        }
        FormKind::Vector(items) => {
            let mut info = PlaceholderInfo::default();
            for it in items {
                info.merge(&placeholder_info_for_form_scoped(it, shadowed));
            }
            info
        }
        FormKind::Map(entries) => {
            let mut info = PlaceholderInfo::default();
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        info.merge(&placeholder_info_for_form_scoped(k, shadowed));
                        info.merge(&placeholder_info_for_form_scoped(v, shadowed));
                    }
                    MapItem::Spread(expr) => {
                        info.merge(&placeholder_info_for_form_scoped(expr, shadowed));
                    }
                }
            }
            info
        }
        FormKind::Set(items) => {
            let mut info = PlaceholderInfo::default();
            for it in items {
                info.merge(&placeholder_info_for_form_scoped(it, shadowed));
            }
            info
        }
        FormKind::InterpolatedString(parts) => {
            let mut info = PlaceholderInfo::default();
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    info.merge(&placeholder_info_for_form_scoped(expr, shadowed));
                }
            }
            info
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            let mut info = PlaceholderInfo::default();
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    info.merge(&placeholder_info_for_form_scoped(expr, shadowed));
                }
            }
            info
        }
        _ => PlaceholderInfo::default(),
    }
}

fn replace_placeholders_scoped(
    form: &Form,
    mapping: &PlaceholderNames,
    shadowed: &HashSet<String>,
) -> Form {
    match &form.kind {
        FormKind::Symbol(s) => {
            if shadowed.contains(s) {
                form.clone()
            } else if let Some(repl) = mapping.lookup(s) {
                Form::new(FormKind::Symbol(repl), form.span)
            } else {
                form.clone()
            }
        }
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "let" {
                    let mut out = Vec::with_capacity(items.len());
                    out.push(items[0].clone());
                    let mut shadowed_body = shadowed.clone();
                    if let Some(bind_form) = items.get(1) {
                        if let FormKind::Vector(binds) = &bind_form.kind {
                            let mut replaced_binds = Vec::with_capacity(binds.len());
                            let mut idx = 0;
                            while idx < binds.len() {
                                let name_form = &binds[idx];
                                replaced_binds.push(name_form.clone());
                                if let FormKind::Symbol(name) = &name_form.kind {
                                    if !is_question_placeholder(name) {
                                        shadowed_body.insert(name.clone());
                                    }
                                }
                                if let Some(val_form) = binds.get(idx + 1) {
                                    replaced_binds.push(replace_placeholders_scoped(
                                        val_form, mapping, shadowed,
                                    ));
                                }
                                idx += 2;
                            }
                            out.push(Form::new(FormKind::Vector(replaced_binds), bind_form.span));
                        } else {
                            out.push(replace_placeholders_scoped(bind_form, mapping, shadowed));
                        }
                    }
                    for body in items.iter().skip(2) {
                        out.push(replace_placeholders_scoped(body, mapping, &shadowed_body));
                    }
                    return Form::new(FormKind::List(out), form.span);
                }
                if head == "fn" {
                    let mut out = Vec::with_capacity(items.len());
                    out.push(items[0].clone());
                    let mut shadowed_body = shadowed.clone();
                    if let Some(params_form) = items.get(1) {
                        if let FormKind::Vector(params) = &params_form.kind {
                            let mut params_cloned = Vec::with_capacity(params.len());
                            for p in params {
                                if let FormKind::Symbol(name) = &p.kind {
                                    if !is_question_placeholder(name) {
                                        shadowed_body.insert(name.clone());
                                    }
                                }
                                params_cloned.push(p.clone());
                            }
                            out.push(Form::new(FormKind::Vector(params_cloned), params_form.span));
                        } else {
                            out.push(replace_placeholders_scoped(params_form, mapping, shadowed));
                        }
                    }
                    for body in items.iter().skip(2) {
                        out.push(replace_placeholders_scoped(body, mapping, &shadowed_body));
                    }
                    return Form::new(FormKind::List(out), form.span);
                }
            }
            let replaced: Vec<Form> = items
                .iter()
                .map(|f| replace_placeholders_scoped(f, mapping, shadowed))
                .collect();
            Form::new(FormKind::List(replaced), form.span)
        }
        FormKind::Vector(items) => {
            let replaced: Vec<Form> = items
                .iter()
                .map(|f| replace_placeholders_scoped(f, mapping, shadowed))
                .collect();
            Form::new(FormKind::Vector(replaced), form.span)
        }
        FormKind::Map(entries) => {
            let replaced = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => MapItem::KeyValue(
                        replace_placeholders_scoped(k, mapping, shadowed),
                        replace_placeholders_scoped(v, mapping, shadowed),
                    ),
                    MapItem::Spread(expr) => {
                        MapItem::Spread(replace_placeholders_scoped(expr, mapping, shadowed))
                    }
                })
                .collect();
            Form::new(FormKind::Map(replaced), form.span)
        }
        FormKind::Set(items) => {
            let replaced: Vec<Form> = items
                .iter()
                .map(|f| replace_placeholders_scoped(f, mapping, shadowed))
                .collect();
            Form::new(FormKind::Set(replaced), form.span)
        }
        FormKind::InterpolatedString(parts) => {
            let mut replaced_parts = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(_) => replaced_parts.push(part.clone()),
                    InterpolatedPart::Expr(expr) => replaced_parts.push(InterpolatedPart::Expr(
                        replace_placeholders_scoped(expr, mapping, shadowed),
                    )),
                }
            }
            Form::new(FormKind::InterpolatedString(replaced_parts), form.span)
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut replaced_parts = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(_) => replaced_parts.push(part.clone()),
                    InterpolatedPart::Expr(expr) => replaced_parts.push(InterpolatedPart::Expr(
                        replace_placeholders_scoped(expr, mapping, shadowed),
                    )),
                }
            }
            Form::new(
                FormKind::InterpolatedRegex {
                    parts: replaced_parts,
                    delim: *delim,
                },
                form.span,
            )
        }
        _ => form.clone(),
    }
}
