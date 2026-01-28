use crate::ast::{Form, FormKind, InterpolatedPart, MapItem, Span};
use crate::error::CloveError;
use crate::types::{TypeHint, TypeHintStyle, TypeKind};

pub fn parse_type_expr_from_forms(forms: &[Form]) -> Result<(TypeKind, usize), CloveError> {
    if forms.is_empty() {
        return Err(CloveError::parse("expected type expression"));
    }
    let first = &forms[0];
    if let FormKind::Vector(items) = &first.kind {
        if matches!(
            forms.get(1).map(|f| &f.kind),
            Some(FormKind::Symbol(sym)) if sym == "->"
        ) {
            let (ret, consumed) = parse_type_expr_from_forms(&forms[2..])?;
            let (params, rest) = parse_type_params(items, first.span)?;
            return Ok((TypeKind::function(params, rest, ret), consumed + 2));
        }
    }
    Ok((parse_type_from_form(first)?, 1))
}

pub fn parse_type_from_form(form: &Form) -> Result<TypeKind, CloveError> {
    match &form.kind {
        FormKind::Symbol(sym) => TypeKind::parse(sym)
            .map_err(|err| CloveError::parse(format!("invalid type hint '{}': {}", sym, err)))
            .map(|kind| kind)
            .map_err(|err| err.with_span(form.span)),
        FormKind::Keyword(name) => TypeKind::from_keyword(name).ok_or_else(|| {
            CloveError::parse(format!("invalid type hint ':{}'", name)).with_span(form.span)
        }),
        FormKind::Vector(items) => parse_vector_type(items, form.span),
        FormKind::Map(entries) => parse_map_type(entries, form.span),
        _ => Err(CloveError::parse("invalid type hint").with_span(form.span)),
    }
}

pub fn normalize_type_syntax_forms(
    forms: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    forms
        .into_iter()
        .map(|form| normalize_form(form, allow_invalid_type_hints))
        .collect()
}

fn parse_type_params(
    items: &[Form],
    span: Span,
) -> Result<(Vec<TypeKind>, Option<TypeKind>), CloveError> {
    let mut params = Vec::new();
    let mut rest = None;
    let mut idx = 0;
    while idx < items.len() {
        let item = &items[idx];
        if matches!(&item.kind, FormKind::Symbol(sym) if sym == "&") {
            if rest.is_some() {
                return Err(CloveError::parse("unexpected '&' in type parameters").with_span(span));
            }
            let next = items
                .get(idx + 1)
                .ok_or_else(|| CloveError::parse("expected type after '&'").with_span(span))?;
            let rest_ty = parse_type_from_form(next)?;
            rest = Some(rest_ty);
            if idx + 2 < items.len() {
                return Err(CloveError::parse("rest type must be the final entry").with_span(span));
            }
            return Ok((params, rest));
        }
        params.push(parse_type_from_form(item)?);
        idx += 1;
    }
    Ok((params, rest))
}

fn parse_vector_type(items: &[Form], _span: Span) -> Result<TypeKind, CloveError> {
    match items.len() {
        0 => Ok(TypeKind::vector(TypeKind::Any)),
        1 => Ok(TypeKind::vector(parse_type_from_form(&items[0])?)),
        _ => {
            let mut types = Vec::new();
            for item in items {
                types.push(parse_type_from_form(item)?);
            }
            Ok(TypeKind::Tuple(types))
        }
    }
}

fn parse_map_type(entries: &[MapItem], span: Span) -> Result<TypeKind, CloveError> {
    if entries.is_empty() {
        return Ok(TypeKind::Record(im::HashMap::new()));
    }
    let mut fields = im::HashMap::new();
    let mut is_record = true;
    for entry in entries {
        let (key, value) = match entry {
            MapItem::KeyValue(k, v) => (k, v),
            MapItem::Spread(expr) => {
                return Err(
                    CloveError::parse("type hint map does not support spread").with_span(expr.span)
                );
            }
        };
        let key_name = match &key.kind {
            FormKind::Keyword(name) => Some(name.clone()),
            FormKind::Symbol(sym) if sym.starts_with(':') => {
                Some(sym.trim_start_matches(':').to_string())
            }
            _ => None,
        };
        if let Some(name) = key_name {
            fields.insert(name, parse_type_from_form(value)?);
        } else {
            is_record = false;
        }
    }
    if is_record {
        return Ok(TypeKind::Record(fields));
    }
    if entries.len() != 1 {
        return Err(CloveError::parse("map type expects one key/value pair").with_span(span));
    }
    match &entries[0] {
        MapItem::KeyValue(k, v) => Ok(TypeKind::map(
            parse_type_from_form(k)?,
            parse_type_from_form(v)?,
        )),
        MapItem::Spread(expr) => {
            Err(CloveError::parse("type hint map does not support spread").with_span(expr.span))
        }
    }
}

fn normalize_form(form: Form, allow_invalid_type_hints: bool) -> Result<Form, CloveError> {
    let mut form = normalize_inline_symbol_type(form, allow_invalid_type_hints)?;
    match form.kind {
        FormKind::List(items) => {
            form.kind = FormKind::List(normalize_list(items, allow_invalid_type_hints)?);
            Ok(form)
        }
        FormKind::Vector(items) => {
            let items = items
                .into_iter()
                .map(|f| normalize_form(f, allow_invalid_type_hints))
                .collect::<Result<Vec<_>, _>>()?;
            form.kind = FormKind::Vector(items);
            Ok(form)
        }
        FormKind::Set(items) => {
            let items = items
                .into_iter()
                .map(|f| normalize_form(f, allow_invalid_type_hints))
                .collect::<Result<Vec<_>, _>>()?;
            form.kind = FormKind::Set(items);
            Ok(form)
        }
        FormKind::Map(entries) => {
            let mut normalized = Vec::with_capacity(entries.len());
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        let key = normalize_form(k, allow_invalid_type_hints)?;
                        let value = normalize_form(v, allow_invalid_type_hints)?;
                        normalized.push(MapItem::KeyValue(key, value));
                    }
                    MapItem::Spread(expr) => {
                        let expr = normalize_form(expr, allow_invalid_type_hints)?;
                        normalized.push(MapItem::Spread(expr));
                    }
                }
            }
            form.kind = FormKind::Map(normalized);
            Ok(form)
        }
        FormKind::ShortFn(items) => {
            let items = items
                .into_iter()
                .map(|f| normalize_form(f, allow_invalid_type_hints))
                .collect::<Result<Vec<_>, _>>()?;
            form.kind = FormKind::ShortFn(items);
            Ok(form)
        }
        FormKind::InterpolatedString(parts) => {
            let mut normalized = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => normalized.push(InterpolatedPart::Text(text)),
                    InterpolatedPart::Expr(expr) => normalized.push(InterpolatedPart::Expr(
                        normalize_form(expr, allow_invalid_type_hints)?,
                    )),
                }
            }
            form.kind = FormKind::InterpolatedString(normalized);
            Ok(form)
        }
        FormKind::InterpolatedRegex { parts, delim } => {
            let mut normalized = Vec::with_capacity(parts.len());
            for part in parts {
                match part {
                    InterpolatedPart::Text(text) => normalized.push(InterpolatedPart::Text(text)),
                    InterpolatedPart::Expr(expr) => normalized.push(InterpolatedPart::Expr(
                        normalize_form(expr, allow_invalid_type_hints)?,
                    )),
                }
            }
            form.kind = FormKind::InterpolatedRegex {
                parts: normalized,
                delim,
            };
            Ok(form)
        }
        _ => Ok(form),
    }
}

fn normalize_list(
    items: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    if items.is_empty() {
        return Ok(items);
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => {
            return items
                .into_iter()
                .map(|f| normalize_form(f, allow_invalid_type_hints))
                .collect();
        }
    };
    match head {
        "def" | "def-" | "-def" => normalize_def(items, allow_invalid_type_hints),
        "defn" | "defn-" | "-defn" => normalize_defn(items, allow_invalid_type_hints),
        "fn" | "fn*" => normalize_fn(items, allow_invalid_type_hints),
        "let" | "let*" | "loop" | "loop*" | "binding" => {
            normalize_binding_vector_form(items, 1, allow_invalid_type_hints)
        }
        "when-let" | "if-let" | "if-some" => {
            normalize_binding_vector_form(items, 1, allow_invalid_type_hints)
        }
        "for" => normalize_for(items, allow_invalid_type_hints),
        "doseq" | "each" | "dotimes" | "with-redefs" | "with-dyn" => {
            normalize_binding_vector_form(items, 1, allow_invalid_type_hints)
        }
        "go-loop" | "scope-loop" | "async::scope-loop" => {
            normalize_binding_vector_form(items, 1, allow_invalid_type_hints)
        }
        "deftype" => normalize_deftype(items, allow_invalid_type_hints),
        _ => items
            .into_iter()
            .map(|f| normalize_form(f, allow_invalid_type_hints))
            .collect(),
    }
}

fn normalize_inline_symbol_type(
    form: Form,
    allow_invalid_type_hints: bool,
) -> Result<Form, CloveError> {
    let mut form = form;
    let FormKind::Symbol(sym) = &form.kind else {
        return Ok(form);
    };
    let Some((base, annot)) = split_symbol_type_annotation(sym) else {
        return Ok(form);
    };
    if form.type_hint.is_none() {
        match TypeKind::parse(annot) {
            Ok(kind) => {
                form.type_hint = Some(TypeHint::new(kind, true));
            }
            Err(err) => {
                if allow_invalid_type_hints {
                    return Ok(form);
                }
                return Err(
                    CloveError::parse(format!("invalid type hint '{}': {}", annot, err))
                        .with_span(form.span),
                );
            }
        }
    }
    if form.type_hint.is_some() {
        form.kind = FormKind::Symbol(base.to_string());
    }
    Ok(form)
}

fn normalize_binding_vector_form(
    mut items: Vec<Form>,
    index: usize,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    for (idx, item) in items.iter_mut().enumerate() {
        if idx == index {
            if let FormKind::Vector(bindings) = &item.kind {
                let normalized = normalize_binding_pairs(bindings, allow_invalid_type_hints)?;
                item.kind = FormKind::Vector(normalized);
            } else {
                *item = normalize_form(item.clone(), allow_invalid_type_hints)?;
            }
            continue;
        }
        *item = normalize_form(item.clone(), allow_invalid_type_hints)?;
    }
    Ok(items)
}

fn normalize_binding_pairs(
    items: &[Form],
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        let mut binding = normalize_form(items[idx].clone(), allow_invalid_type_hints)?;
        let mut consumed = 0;
        if let FormKind::Symbol(sym) = &binding.kind {
            if let Some(base) = strip_typed_binder(sym) {
                match parse_type_expr_from_forms(&items[idx + 1..]) {
                    Ok((ty, used)) => {
                        consumed = used;
                        binding.kind = FormKind::Symbol(base.to_string());
                        binding.type_hint = Some(TypeHint::new(ty, true));
                    }
                    Err(err) => {
                        if !allow_invalid_type_hints {
                            return Err(err);
                        }
                    }
                }
            }
        }
        idx += 1 + consumed;
        let value = items.get(idx).ok_or_else(|| {
            CloveError::parse("binding vector expects value after binding")
                .with_span(items[idx - 1].span)
        })?;
        let value = normalize_form(value.clone(), allow_invalid_type_hints)?;
        out.push(binding);
        out.push(value);
        idx += 1;
    }
    Ok(out)
}

fn normalize_fn_params(
    items: &[Form],
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        let mut param = normalize_form(items[idx].clone(), allow_invalid_type_hints)?;
        if let FormKind::Symbol(sym) = &param.kind {
            if let Some(base) = strip_typed_binder(sym) {
                match parse_type_expr_from_forms(&items[idx + 1..]) {
                    Ok((ty, used)) => {
                        idx += used;
                        param.kind = FormKind::Symbol(base.to_string());
                        param.type_hint = Some(TypeHint::new(ty, true));
                    }
                    Err(err) => {
                        if !allow_invalid_type_hints {
                            return Err(err);
                        }
                    }
                }
            }
        }
        out.push(param);
        idx += 1;
    }
    Ok(out)
}

fn normalize_defn(
    items: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::with_capacity(items.len());
    out.push(normalize_form(items[0].clone(), allow_invalid_type_hints)?);
    if items.len() < 2 {
        return Ok(out);
    }
    let mut idx = 2;
    let mut common_return = None;
    let mut name_form = normalize_form(items[1].clone(), allow_invalid_type_hints)?;
    if let FormKind::Symbol(sym) = &name_form.kind {
        if let Some(base) = strip_typed_binder(sym) {
            match parse_type_expr_from_forms(&items[idx..]) {
                Ok((ty, used)) => {
                    common_return = Some(TypeHint::return_hint(ty, true));
                    name_form.kind = FormKind::Symbol(base.to_string());
                    name_form.type_hint = None;
                    idx += used;
                }
                Err(err) => {
                    if !allow_invalid_type_hints {
                        return Err(err);
                    }
                }
            }
        }
    }
    if common_return.is_none() {
        if let Some(hint) = name_form.type_hint.take() {
            common_return = Some(hint.with_style(TypeHintStyle::Return));
        }
    }
    out.push(name_form);
    while idx < items.len() {
        match &items[idx].kind {
            FormKind::String(_) | FormKind::Map(_) => {
                out.push(normalize_form(
                    items[idx].clone(),
                    allow_invalid_type_hints,
                )?);
                idx += 1;
            }
            _ => break,
        }
    }
    if let Some(FormKind::Symbol(sym)) = items.get(idx).map(|f| &f.kind) {
        if sym == "->" {
            match parse_type_expr_from_forms(&items[idx + 1..]) {
                Ok((ty, used)) => {
                    common_return = Some(TypeHint::return_hint(ty, true));
                    idx += used + 1;
                }
                Err(err) => {
                    if !allow_invalid_type_hints {
                        return Err(err);
                    }
                }
            }
        }
    }
    if idx >= items.len() {
        return Ok(out);
    }
    match &items[idx].kind {
        FormKind::Vector(_) => {
            let (params_form, body_start) = normalize_fn_params_with_return(
                &items,
                idx,
                common_return,
                allow_invalid_type_hints,
            )?;
            out.push(params_form);
            for form in items.iter().skip(body_start) {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
            }
        }
        FormKind::List(_) => {
            for form in items.iter().skip(idx) {
                out.push(normalize_fn_clause(
                    form.clone(),
                    common_return.clone(),
                    allow_invalid_type_hints,
                )?);
            }
        }
        _ => {
            for form in items.iter().skip(idx) {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
            }
        }
    }
    Ok(out)
}

fn normalize_def(
    items: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::with_capacity(items.len());
    if items.is_empty() {
        return Ok(items);
    }
    out.push(normalize_form(items[0].clone(), allow_invalid_type_hints)?);
    if items.len() < 2 {
        return Ok(out);
    }
    let mut idx = 2;
    let mut name_form = normalize_form(items[1].clone(), allow_invalid_type_hints)?;
    if let FormKind::Symbol(sym) = &name_form.kind {
        if let Some(base) = strip_typed_binder(sym) {
            if name_form.type_hint.is_none() {
                match parse_type_expr_from_forms(&items[idx..]) {
                    Ok((ty, used)) => {
                        name_form.kind = FormKind::Symbol(base.to_string());
                        name_form.type_hint = Some(TypeHint::new(ty, true));
                        idx += used;
                    }
                    Err(err) => {
                        if !allow_invalid_type_hints {
                            return Err(err);
                        }
                    }
                }
            }
        }
    }
    out.push(name_form);
    while idx < items.len() {
        out.push(normalize_form(
            items[idx].clone(),
            allow_invalid_type_hints,
        )?);
        idx += 1;
    }
    Ok(out)
}

fn normalize_fn(items: Vec<Form>, allow_invalid_type_hints: bool) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::with_capacity(items.len());
    out.push(normalize_form(items[0].clone(), allow_invalid_type_hints)?);
    let mut idx = 1;
    let mut common_return = None;
    while idx < items.len() {
        match &items[idx].kind {
            FormKind::String(_) | FormKind::Map(_) => {
                out.push(normalize_form(
                    items[idx].clone(),
                    allow_invalid_type_hints,
                )?);
                idx += 1;
            }
            _ => break,
        }
    }
    if let Some(FormKind::Symbol(sym)) = items.get(idx).map(|f| &f.kind) {
        if sym == "->" {
            match parse_type_expr_from_forms(&items[idx + 1..]) {
                Ok((ty, used)) => {
                    common_return = Some(TypeHint::return_hint(ty, true));
                    idx += used + 1;
                }
                Err(err) => {
                    if !allow_invalid_type_hints {
                        return Err(err);
                    }
                }
            }
        }
    }
    if idx >= items.len() {
        return Ok(out);
    }
    match &items[idx].kind {
        FormKind::Vector(_) => {
            let (params_form, body_start) = normalize_fn_params_with_return(
                &items,
                idx,
                common_return,
                allow_invalid_type_hints,
            )?;
            out.push(params_form);
            for form in items.iter().skip(body_start) {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
            }
        }
        FormKind::List(_) => {
            for form in items.iter().skip(idx) {
                out.push(normalize_fn_clause(
                    form.clone(),
                    common_return.clone(),
                    allow_invalid_type_hints,
                )?);
            }
        }
        _ => {
            for form in items.iter().skip(idx) {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
            }
        }
    }
    Ok(out)
}

fn normalize_fn_clause(
    form: Form,
    common_return: Option<TypeHint>,
    allow_invalid_type_hints: bool,
) -> Result<Form, CloveError> {
    let FormKind::List(items) = &form.kind else {
        return normalize_form(form, allow_invalid_type_hints);
    };
    if items.is_empty() {
        return normalize_form(form, allow_invalid_type_hints);
    }
    let mut out = Vec::with_capacity(items.len());
    let params_form = match &items[0].kind {
        FormKind::Vector(params) => {
            let normalized = normalize_fn_params(params, allow_invalid_type_hints)?;
            let mut params_form = items[0].clone();
            params_form.kind = FormKind::Vector(normalized);
            params_form
        }
        _ => normalize_form(items[0].clone(), allow_invalid_type_hints)?,
    };
    let mut idx = 1;
    let mut return_hint = params_form.type_hint.clone();
    if let Some(hint) = return_hint.take() {
        return_hint = Some(hint.with_style(TypeHintStyle::Return));
    }
    if let Some(FormKind::Symbol(sym)) = items.get(idx).map(|f| &f.kind) {
        if sym == "->" {
            match parse_type_expr_from_forms(&items[idx + 1..]) {
                Ok((ty, used)) => {
                    return_hint = Some(TypeHint::return_hint(ty, true));
                    idx += used + 1;
                }
                Err(err) => {
                    if !allow_invalid_type_hints {
                        return Err(err);
                    }
                }
            }
        }
    }
    if return_hint.is_none() {
        return_hint = common_return;
    }
    let mut params_form = params_form;
    params_form.type_hint = return_hint;
    out.push(params_form);
    for form in items.iter().skip(idx) {
        out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
    }
    Ok(Form::new(FormKind::List(out), form.span))
}

fn normalize_fn_params_with_return(
    items: &[Form],
    params_idx: usize,
    common_return: Option<TypeHint>,
    allow_invalid_type_hints: bool,
) -> Result<(Form, usize), CloveError> {
    let params_form = match &items[params_idx].kind {
        FormKind::Vector(params) => {
            let normalized = normalize_fn_params(params, allow_invalid_type_hints)?;
            let mut params_form = items[params_idx].clone();
            params_form.kind = FormKind::Vector(normalized);
            params_form
        }
        _ => normalize_form(items[params_idx].clone(), allow_invalid_type_hints)?,
    };
    let mut idx = params_idx + 1;
    let mut return_hint = params_form.type_hint.clone();
    if let Some(hint) = return_hint.take() {
        return_hint = Some(hint.with_style(TypeHintStyle::Return));
    }
    if let Some(FormKind::Symbol(sym)) = items.get(idx).map(|f| &f.kind) {
        if sym == "->" {
            match parse_type_expr_from_forms(&items[idx + 1..]) {
                Ok((ty, used)) => {
                    return_hint = Some(TypeHint::return_hint(ty, true));
                    idx += used + 1;
                }
                Err(err) => {
                    if !allow_invalid_type_hints {
                        return Err(err);
                    }
                }
            }
        }
    }
    if return_hint.is_none() {
        return_hint = common_return;
    }
    let mut params_form = params_form;
    params_form.type_hint = return_hint;
    Ok((params_form, idx))
}

fn normalize_for(
    items: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    if items.len() < 2 {
        return items
            .into_iter()
            .map(|f| normalize_form(f, allow_invalid_type_hints))
            .collect();
    }
    let mut out = Vec::with_capacity(items.len());
    out.push(normalize_form(items[0].clone(), allow_invalid_type_hints)?);
    let bindings_form = match &items[1].kind {
        FormKind::Vector(bindings) => {
            let normalized = normalize_for_bindings(bindings, allow_invalid_type_hints)?;
            let mut form = items[1].clone();
            form.kind = FormKind::Vector(normalized);
            form
        }
        _ => normalize_form(items[1].clone(), allow_invalid_type_hints)?,
    };
    out.push(bindings_form);
    for form in items.iter().skip(2) {
        out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
    }
    Ok(out)
}

fn normalize_for_bindings(
    items: &[Form],
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < items.len() {
        if let FormKind::Keyword(kw) = &items[idx].kind {
            let kw = kw.as_str();
            if kw == "let" {
                let next = items.get(idx + 1).ok_or_else(|| {
                    CloveError::parse("for :let expects binding vector").with_span(items[idx].span)
                })?;
                out.push(items[idx].clone());
                let binds = match &next.kind {
                    FormKind::Vector(bindings) => {
                        let normalized =
                            normalize_binding_pairs(bindings, allow_invalid_type_hints)?;
                        let mut form = next.clone();
                        form.kind = FormKind::Vector(normalized);
                        form
                    }
                    _ => normalize_form(next.clone(), allow_invalid_type_hints)?,
                };
                out.push(binds);
                idx += 2;
                continue;
            }
            if kw == "when" || kw == "while" {
                let next = items.get(idx + 1).ok_or_else(|| {
                    CloveError::parse("for filter expects expression").with_span(items[idx].span)
                })?;
                out.push(items[idx].clone());
                out.push(normalize_form(next.clone(), allow_invalid_type_hints)?);
                idx += 2;
                continue;
            }
        }
        let mut binding = normalize_form(items[idx].clone(), allow_invalid_type_hints)?;
        let mut consumed = 0;
        if let FormKind::Symbol(sym) = &binding.kind {
            if let Some(base) = strip_typed_binder(sym) {
                match parse_type_expr_from_forms(&items[idx + 1..]) {
                    Ok((ty, used)) => {
                        consumed = used;
                        binding.kind = FormKind::Symbol(base.to_string());
                        binding.type_hint = Some(TypeHint::new(ty, true));
                    }
                    Err(err) => {
                        if !allow_invalid_type_hints {
                            return Err(err);
                        }
                    }
                }
            }
        }
        idx += 1 + consumed;
        let value = items.get(idx).ok_or_else(|| {
            CloveError::parse("for binding expects collection expression")
                .with_span(items[idx - 1].span)
        })?;
        let value = normalize_form(value.clone(), allow_invalid_type_hints)?;
        out.push(binding);
        out.push(value);
        idx += 1;
    }
    Ok(out)
}

fn normalize_deftype(
    items: Vec<Form>,
    allow_invalid_type_hints: bool,
) -> Result<Vec<Form>, CloveError> {
    if items.len() < 2 {
        return items
            .into_iter()
            .map(|f| normalize_form(f, allow_invalid_type_hints))
            .collect();
    }
    let mut out = Vec::with_capacity(items.len());
    out.push(normalize_form(items[0].clone(), allow_invalid_type_hints)?);
    out.push(normalize_form(items[1].clone(), allow_invalid_type_hints)?);
    let mut idx = 2;
    while idx < items.len() {
        let form = &items[idx];
        if let FormKind::Keyword(name) = &form.kind {
            if name == "from" {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
                if let Some(sym_form) = items.get(idx + 1) {
                    out.push(normalize_form(sym_form.clone(), allow_invalid_type_hints)?);
                }
                if let Some(expr_form) = items.get(idx + 2) {
                    out.push(normalize_form(expr_form.clone(), allow_invalid_type_hints)?);
                }
                idx += 3;
                continue;
            }
        }
        if let FormKind::List(list_items) = &form.kind {
            if matches!(
                list_items.first().map(|f| &f.kind),
                Some(FormKind::Symbol(sym)) if sym == "def"
            ) {
                out.push(normalize_form(form.clone(), allow_invalid_type_hints)?);
                idx += 1;
                continue;
            }
        }
        match &form.kind {
            FormKind::Vector(fields) | FormKind::List(fields) => {
                let normalized = normalize_fn_params(fields, allow_invalid_type_hints)?;
                let mut form = form.clone();
                form.kind = match form.kind {
                    FormKind::Vector(_) => FormKind::Vector(normalized),
                    FormKind::List(_) => FormKind::List(normalized),
                    _ => form.kind,
                };
                out.push(form);
            }
            _ => out.push(normalize_form(form.clone(), allow_invalid_type_hints)?),
        }
        idx += 1;
    }
    Ok(out)
}

fn strip_typed_binder(sym: &str) -> Option<&str> {
    let base = sym.strip_suffix(':')?;
    if base.is_empty() {
        return None;
    }
    Some(base)
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
