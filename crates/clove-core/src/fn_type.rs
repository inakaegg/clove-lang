use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::ast::{Form, FormKind, InterpolatedPart, LambdaClause, MapItem, Span, Value};
use crate::fn_meta;
use crate::spread::strip_spread_symbol;
use crate::types::{TypeHint, TypeKind};
use crate::typing::infer;

static FN_TYPE_CACHE: Lazy<RwLock<HashMap<usize, String>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

pub fn fn_type_string(value: &Value) -> Option<String> {
    match value {
        Value::Lambda {
            params,
            rest,
            body,
            recur_id,
            inferred_type,
            ..
        } => inferred_signature(inferred_type.as_ref())
            .or_else(|| fn_type_for_lambda(params, rest.as_ref(), body, Some(*recur_id))),
        Value::MultiLambda {
            clauses,
            inferred_type,
            ..
        } => {
            inferred_signature(inferred_type.as_ref()).or_else(|| fn_type_for_multi_lambda(clauses))
        }
        Value::Func(func) => fn_type_for_native(func),
        Value::Partial { .. } | Value::Compose { .. } | Value::ForeignCallable { .. } => None,
        _ => None,
    }
}

fn inferred_signature(inferred: Option<&TypeKind>) -> Option<String> {
    let inferred = inferred?;
    match inferred {
        TypeKind::Function { .. } => {
            if is_all_any_function(inferred) {
                None
            } else {
                Some(inferred.describe())
            }
        }
        TypeKind::Union(_) => Some(inferred.describe()),
        _ => None,
    }
}

fn is_all_any_function(kind: &TypeKind) -> bool {
    let TypeKind::Function { params, rest, ret } = kind else {
        return false;
    };
    let params_any = params.iter().all(|ty| matches!(ty, TypeKind::Any));
    let rest_any = rest
        .as_ref()
        .map(|ty| matches!(ty.as_ref(), TypeKind::Any))
        .unwrap_or(true);
    let ret_any = matches!(ret.as_ref(), TypeKind::Any);
    params_any && rest_any && ret_any
}

fn fn_type_for_lambda(
    params: &[String],
    rest: Option<&String>,
    body: &[Form],
    cache_key: Option<usize>,
) -> Option<String> {
    if let Some(key) = cache_key {
        if let Some(cached) = FN_TYPE_CACHE.read().unwrap().get(&key) {
            return Some(cached.clone());
        }
    }
    let span = body.first().map(|f| f.span).unwrap_or_else(default_span);
    let spread_params = spread_params_for_body(body);
    let form = build_fn_form(params, rest, body, span, &spread_params);
    let result = infer::infer_forms_with_diags(&[form]);
    let ty = result.forms.first().map(|(_, ty)| ty.describe_pretty())?;
    if let Some(key) = cache_key {
        FN_TYPE_CACHE.write().unwrap().insert(key, ty.clone());
    }
    Some(ty)
}

fn fn_type_for_multi_lambda(clauses: &[LambdaClause]) -> Option<String> {
    let mut sigs = Vec::new();
    for clause in clauses {
        if let Some(sig) = fn_type_for_lambda(
            &clause.params,
            clause.rest.as_ref(),
            &clause.body,
            Some(clause.recur_id),
        ) {
            sigs.push(sig);
        }
    }
    if sigs.is_empty() {
        return None;
    }
    sigs.sort();
    sigs.dedup();
    if sigs.len() == 1 {
        return Some(sigs[0].clone());
    }
    Some(sigs.join(" | "))
}

fn fn_type_for_native(func: &std::sync::Arc<crate::ast::NativeFn>) -> Option<String> {
    let name = func.debug_name()?;
    let meta = fn_meta::get(name)?;
    fn_meta_signature(&meta)
}

fn fn_meta_signature(meta: &fn_meta::FnMeta) -> Option<String> {
    if meta.overloads.is_empty() {
        return None;
    }
    let mut sigs: Vec<String> = meta
        .overloads
        .iter()
        .map(|overload| {
            TypeKind::function(
                overload.arg_types.clone(),
                overload.rest.clone(),
                overload.ret_type.clone(),
            )
            .describe()
        })
        .collect();
    sigs.sort();
    sigs.dedup();
    if sigs.len() == 1 {
        return Some(sigs[0].clone());
    }
    Some(sigs.join(" | "))
}

fn build_fn_form(
    params: &[String],
    rest: Option<&String>,
    body: &[Form],
    span: Span,
    spread_params: &HashSet<String>,
) -> Form {
    let mut param_forms = Vec::new();
    for name in params {
        param_forms.push(param_form(name, span, spread_params));
    }
    if let Some(rest_name) = rest {
        param_forms.push(Form::new(FormKind::Symbol("&".into()), span));
        param_forms.push(param_form(rest_name, span, spread_params));
    }
    let params_form = Form::new(FormKind::Vector(param_forms), span);
    let mut items = Vec::with_capacity(2 + body.len());
    items.push(Form::new(FormKind::Symbol("fn".into()), span));
    items.push(params_form);
    items.extend(body.iter().cloned());
    Form::new(FormKind::List(items), span)
}

fn param_form(name: &str, span: Span, spread_params: &HashSet<String>) -> Form {
    let mut form = Form::new(FormKind::Symbol(name.to_string()), span);
    if spread_params.contains(name) {
        let hint = TypeHint::new(TypeKind::vector(TypeKind::Any), false);
        form = form.with_type_hint(hint);
    }
    form
}

fn spread_params_for_body(body: &[Form]) -> HashSet<String> {
    let mut out = HashSet::new();
    for form in body {
        collect_spread_params(form, &mut out);
    }
    out
}

fn collect_spread_params(form: &Form, out: &mut HashSet<String>) {
    match &form.kind {
        FormKind::Symbol(sym) => {
            if let Some(base) = strip_spread_symbol(sym) {
                out.insert(base);
            }
        }
        FormKind::List(items)
        | FormKind::Vector(items)
        | FormKind::Set(items)
        | FormKind::ShortFn(items) => {
            for item in items {
                collect_spread_params(item, out);
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        collect_spread_params(k, out);
                        collect_spread_params(v, out);
                    }
                    MapItem::Spread(expr) => collect_spread_params(expr, out),
                }
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    collect_spread_params(expr, out);
                }
            }
        }
        _ => {}
    }
}

fn default_span() -> Span {
    Span {
        line: 1,
        col: 1,
        index: 0,
    }
}
