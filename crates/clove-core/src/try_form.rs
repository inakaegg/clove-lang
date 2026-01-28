use crate::ast::{Form, FormKind, Span};
use crate::short_fn::{placeholder_info_for_form, replace_placeholders, PlaceholderNames};
use crate::symbols::canonical_symbol_name;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TryTailKind {
    OnError1,
    OnFinally0,
}

#[derive(Clone, Debug)]
pub enum TryShortPlan {
    Handler {
        body: Vec<Form>,
        on_error: Form,
    },
    Finally {
        body: Vec<Form>,
        on_finally: Form,
    },
    HandlerFinally {
        body: Vec<Form>,
        on_error: Form,
        on_finally: Form,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TryShortErrorKind {
    NoTail,
    BadOrder,
    BadArity,
    BindingsBad,
}

#[derive(Clone, Debug)]
pub struct TryShortError {
    pub kind: TryShortErrorKind,
    pub detail: Option<String>,
}

impl TryShortError {
    pub fn new(kind: TryShortErrorKind, detail: Option<String>) -> Self {
        Self { kind, detail }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrFinClauseKind {
    Err,
    Fin,
}

#[derive(Clone, Debug)]
pub struct ErrFinTail {
    pub body: Vec<Form>,
    pub err: Option<Form>,
    pub fin: Option<Form>,
}

#[derive(Clone, Debug)]
pub struct ErrFinParseError {
    pub span: Span,
    pub message: String,
}

impl ErrFinParseError {
    fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

#[derive(Clone, Copy)]
enum QuestionPlaceholderKind {
    Value,
    Spread,
}

fn parse_positive_index(text: &str) -> Option<usize> {
    if text.is_empty() || !text.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    let index = text.parse::<usize>().ok()?;
    if index == 0 {
        None
    } else {
        Some(index)
    }
}

fn parse_question_placeholder(sym: &str) -> Option<(QuestionPlaceholderKind, Option<usize>)> {
    if sym == "?" {
        return Some((QuestionPlaceholderKind::Value, Some(1)));
    }
    if sym == "*?" {
        return Some((QuestionPlaceholderKind::Spread, Some(1)));
    }
    if let Some(rest) = sym.strip_prefix('?') {
        return parse_positive_index(rest).map(|idx| (QuestionPlaceholderKind::Value, Some(idx)));
    }
    if let Some(rest) = sym.strip_prefix("*?") {
        return parse_positive_index(rest).map(|idx| (QuestionPlaceholderKind::Spread, Some(idx)));
    }
    None
}

fn err_fin_clause(form: &Form) -> Option<(ErrFinClauseKind, Vec<Form>, Span)> {
    let FormKind::List(items) = &form.kind else {
        return None;
    };
    let Some(FormKind::Symbol(tag)) = items.first().map(|f| &f.kind) else {
        return None;
    };
    let canonical = canonical_symbol_name(tag);
    match canonical.as_ref() {
        "err" => Some((ErrFinClauseKind::Err, items[1..].to_vec(), items[0].span)),
        "fin" => Some((ErrFinClauseKind::Fin, items[1..].to_vec(), items[0].span)),
        _ => None,
    }
}

pub fn parse_err_fin_tail(
    args: &[Form],
    context: &str,
) -> Result<Option<ErrFinTail>, ErrFinParseError> {
    if args.is_empty() {
        return Ok(None);
    }
    let mut end = args.len();
    let mut err_clause: Option<Form> = None;
    let mut fin_clause: Option<Form> = None;

    if end > 0 {
        if let Some((kind, _body, _span)) = err_fin_clause(&args[end - 1]) {
            if kind == ErrFinClauseKind::Fin {
                fin_clause = Some(args[end - 1].clone());
                end -= 1;
            }
        }
    }
    if end > 0 {
        if let Some((kind, _body, _span)) = err_fin_clause(&args[end - 1]) {
            if kind == ErrFinClauseKind::Err {
                err_clause = Some(args[end - 1].clone());
                end -= 1;
            }
        }
    }

    for form in &args[..end] {
        if let Some((kind, _body, span)) = err_fin_clause(form) {
            let msg = match kind {
                ErrFinClauseKind::Err => {
                    if err_clause.is_some() {
                        "multiple err clauses are not supported".to_string()
                    } else {
                        format!("err must appear at end of {context}")
                    }
                }
                ErrFinClauseKind::Fin => {
                    if fin_clause.is_some() {
                        "multiple fin clauses are not supported".to_string()
                    } else {
                        format!("fin must appear at end of {context}")
                    }
                }
            };
            return Err(ErrFinParseError::new(span, msg));
        }
    }

    if err_clause.is_none() && fin_clause.is_none() {
        return Ok(None);
    }

    Ok(Some(ErrFinTail {
        body: args[..end].to_vec(),
        err: err_clause,
        fin: fin_clause,
    }))
}

fn err_fin_do_body(body: &[Form], span: Span) -> Form {
    let mut items = Vec::with_capacity(1 + body.len());
    items.push(Form::new(FormKind::Symbol("do".into()), span));
    items.extend_from_slice(body);
    Form::new(FormKind::List(items), span)
}

pub fn build_err_clause_handler(form: &Form) -> Result<Form, ErrFinParseError> {
    let (kind, body, span) = err_fin_clause(form)
        .ok_or_else(|| ErrFinParseError::new(form.span, "err clause must be a list form"))?;
    if kind != ErrFinClauseKind::Err {
        return Err(ErrFinParseError::new(span, "expected err clause"));
    }
    let body_form = err_fin_do_body(&body, span);
    let info = placeholder_info_for_form(&body_form);
    if info.has_rest || info.max_index > 1 {
        return Err(ErrFinParseError::new(
            span,
            "err clause allows at most one placeholder",
        ));
    }
    let mapping = PlaceholderNames {
        args: vec!["err__auto".to_string()],
        rest: None,
    };
    let replaced = replace_placeholders(&body_form, &mapping);
    let params = vec![Form::new(FormKind::Symbol(mapping.args[0].clone()), span)];
    let params_form = Form::new(FormKind::Vector(params), span);
    let fn_head = Form::new(FormKind::Symbol("fn".into()), span);
    Ok(Form::new(
        FormKind::List(vec![fn_head, params_form, replaced]),
        span,
    ))
}

pub fn build_fin_clause_handler(form: &Form) -> Result<Form, ErrFinParseError> {
    let (kind, body, span) = err_fin_clause(form)
        .ok_or_else(|| ErrFinParseError::new(form.span, "fin clause must be a list form"))?;
    if kind != ErrFinClauseKind::Fin {
        return Err(ErrFinParseError::new(span, "expected fin clause"));
    }
    let body_form = err_fin_do_body(&body, span);
    let info = placeholder_info_for_form(&body_form);
    if info.has_rest || info.max_index > 0 {
        return Err(ErrFinParseError::new(
            span,
            "fin clause does not accept placeholders",
        ));
    }
    let params_form = Form::new(FormKind::Vector(Vec::new()), span);
    let fn_head = Form::new(FormKind::Symbol("fn".into()), span);
    Ok(Form::new(
        FormKind::List(vec![fn_head, params_form, body_form]),
        span,
    ))
}

pub fn try_tail_kind(form: &Form) -> Result<Option<TryTailKind>, TryShortError> {
    match &form.kind {
        FormKind::ShortFn(body) => {
            let body_form = Form::new(FormKind::List(body.clone()), form.span);
            let info = placeholder_info_for_form(&body_form);
            if info.has_rest || info.max_index >= 2 {
                let detail = if info.has_rest && info.max_index >= 2 {
                    Some("%2 or %&".to_string())
                } else if info.has_rest {
                    Some("%&".to_string())
                } else {
                    Some("%2".to_string())
                };
                return Err(TryShortError::new(TryShortErrorKind::BadArity, detail));
            }
            if info.max_index == 0 {
                Ok(Some(TryTailKind::OnFinally0))
            } else {
                Ok(Some(TryTailKind::OnError1))
            }
        }
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(sym)) = items.first().map(|f| &f.kind) {
                if canonical_symbol_name(sym).as_ref() == "fn" {
                    let params_form = match items.get(1) {
                        Some(form) => form,
                        None => {
                            return Err(TryShortError::new(
                                TryShortErrorKind::BadArity,
                                Some("fn params missing".to_string()),
                            ))
                        }
                    };
                    let FormKind::Vector(params) = &params_form.kind else {
                        return Err(TryShortError::new(
                            TryShortErrorKind::BadArity,
                            Some("fn params must be vector".to_string()),
                        ));
                    };
                    if params
                        .iter()
                        .any(|p| matches!(&p.kind, FormKind::Symbol(name) if name == "&"))
                    {
                        return Err(TryShortError::new(
                            TryShortErrorKind::BadArity,
                            Some("fn has rest params".to_string()),
                        ));
                    }
                    if params.is_empty() {
                        return Ok(Some(TryTailKind::OnFinally0));
                    }
                    if params.len() == 1 {
                        return Ok(Some(TryTailKind::OnError1));
                    }
                    return Err(TryShortError::new(
                        TryShortErrorKind::BadArity,
                        Some(format!("fn has {} params", params.len())),
                    ));
                }
            }
            let mut saw_placeholder = false;
            for item in items {
                if let FormKind::Symbol(sym) = &item.kind {
                    if let Some((kind, idx)) = parse_question_placeholder(sym) {
                        match kind {
                            QuestionPlaceholderKind::Value => {
                                if matches!(idx, Some(i) if i >= 2) {
                                    return Err(TryShortError::new(
                                        TryShortErrorKind::BadArity,
                                        Some("?2/*? not allowed".to_string()),
                                    ));
                                }
                                saw_placeholder = true;
                            }
                            QuestionPlaceholderKind::Spread => {
                                return Err(TryShortError::new(
                                    TryShortErrorKind::BadArity,
                                    Some("?2/*? not allowed".to_string()),
                                ));
                            }
                        }
                    }
                }
            }
            if saw_placeholder {
                Ok(Some(TryTailKind::OnError1))
            } else {
                Ok(None)
            }
        }
        _ => Ok(None),
    }
}

pub fn parse_try_short(args: &[Form]) -> Result<TryShortPlan, TryShortError> {
    if args.len() <= 1 {
        return Err(TryShortError::new(TryShortErrorKind::NoTail, None));
    }
    let last_form = &args[args.len() - 1];
    let last_tail = try_tail_kind(last_form)?;
    let Some(last_tail) = last_tail else {
        return Err(TryShortError::new(TryShortErrorKind::NoTail, None));
    };
    if args.len() >= 3 {
        let second_last = &args[args.len() - 2];
        let second_tail = try_tail_kind(second_last)?;
        if let Some(second_tail) = second_tail {
            if second_tail == TryTailKind::OnError1 && last_tail == TryTailKind::OnFinally0 {
                let body = args[..args.len() - 2].to_vec();
                if body.is_empty() {
                    return Err(TryShortError::new(TryShortErrorKind::NoTail, None));
                }
                return Ok(TryShortPlan::HandlerFinally {
                    body,
                    on_error: second_last.clone(),
                    on_finally: last_form.clone(),
                });
            }
            return Err(TryShortError::new(TryShortErrorKind::BadOrder, None));
        }
    }
    let body = args[..args.len() - 1].to_vec();
    if body.is_empty() {
        return Err(TryShortError::new(TryShortErrorKind::NoTail, None));
    }
    match last_tail {
        TryTailKind::OnError1 => Ok(TryShortPlan::Handler {
            body,
            on_error: last_form.clone(),
        }),
        TryTailKind::OnFinally0 => Ok(TryShortPlan::Finally {
            body,
            on_finally: last_form.clone(),
        }),
    }
}

pub fn validate_try_bindings(form: &Form) -> Result<(), TryShortError> {
    match &form.kind {
        FormKind::Vector(items) => {
            if items.len() % 2 == 0 {
                Ok(())
            } else {
                Err(TryShortError::new(
                    TryShortErrorKind::BindingsBad,
                    Some("binding vector must have even number of forms".to_string()),
                ))
            }
        }
        _ => Err(TryShortError::new(
            TryShortErrorKind::BindingsBad,
            Some("bindings must be vector".to_string()),
        )),
    }
}

pub fn format_try_error_message(err: &TryShortError) -> String {
    let reason = match err.kind {
        TryShortErrorKind::NoTail => "missing (catch/finally/err/fin) or trailing callable(s)",
        TryShortErrorKind::BadOrder => "short tail order must be [on-error][on-finally]",
        TryShortErrorKind::BadArity => "short tail must be a 0-arg or 1-arg callable",
        TryShortErrorKind::BindingsBad => {
            "bindings short requires (try [bindings] body+ on-error [on-finally])"
        }
    };
    let reason_line = match &err.detail {
        Some(detail) => format!("{} ({})", reason, detail),
        None => reason.to_string(),
    };
    format!(
        "Invalid try form: {}\n  Use:\n    (try body+ on-error)\n    (try body+ on-finally)\n    (try body+ on-error on-finally)     ; order: on-error then on-finally\n    (try [bindings] body+ on-error)\n    (try [bindings] body+ on-finally)\n    (try [bindings] body+ on-error on-finally)\n    (try body* (catch ...) (finally ...))\n    (try body* (err ...) (fin ...))\n  Hint: If a body form is callable and not intended as handler/finally, wrap it: (do <value>)",
        reason_line
    )
}
