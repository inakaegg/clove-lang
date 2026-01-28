pub mod ast;
pub mod builtins;
pub mod compiler;
pub mod concurrency;
pub mod cow;
pub mod doc_examples;
pub mod docs;
pub mod dynamic_vars;
pub mod env;
pub mod error;
pub mod eval;
pub mod fmt_config;
pub mod fn_meta;
pub mod fn_type;
pub mod foreign;
pub mod form_source;
pub mod form_to_string;
pub mod formatter;
pub mod guard;
pub mod interrupt;
pub mod io_reader;
pub mod ir;
pub mod plugin_host;
pub mod plugin_meta;
pub mod pretty_print;
pub mod profiler;
pub mod seq;
pub mod string_escape;
pub mod symbol_meta;

mod memo_support;
pub mod namespaces;
pub mod native_buf;
pub mod options;
pub mod package_registry;
pub mod reader;
pub mod reader_tags;
pub mod repl;
pub mod runtime;
pub mod settings;
mod short_fn;
pub mod spread;
pub mod symbols;
pub mod try_form;
pub mod type_registry;
pub mod type_syntax;
pub mod typed_ir;
pub mod types;
pub mod typing;
pub mod value_format;
mod vm;

pub fn print_vm_profile_if_enabled() {
    vm::profiler::print_report_if_enabled();
}

use std::path::Path;
use std::sync::Arc;

use error::{CloveError, WARN_TAG};
use foreign::ForeignEngine;
use options::EvalOptions;
pub use runtime::{
    lsp_namespace_info, parse_source_for_lsp, parse_source_for_lsp_lenient, CompileOptions,
    IRProgram, LspNamespaceInfo, RequireSpec, RequireTarget, RuntimeCtx,
};
use std::collections::HashSet;

pub fn eval_source(src: &str, options: Option<EvalOptions>) -> Result<ast::Value, CloveError> {
    eval_source_with_engines(src, options.unwrap_or_default(), &[])
}

pub fn eval_source_with_engines(
    src: &str,
    opts: EvalOptions,
    engines: &[Arc<dyn ForeignEngine>],
) -> Result<ast::Value, CloveError> {
    runtime::eval_source_with_engines(src, opts, engines)
}

pub fn eval_file(path: &Path, options: Option<EvalOptions>) -> Result<ast::Value, CloveError> {
    eval_file_with_engines(path, options.unwrap_or_default(), &[])
}

pub fn eval_file_with_engines(
    path: &Path,
    opts: EvalOptions,
    engines: &[Arc<dyn ForeignEngine>],
) -> Result<ast::Value, CloveError> {
    let ctx = RuntimeCtx::new(opts, engines);
    ctx.eval_file(path)
}

pub fn compile_to_ir(
    source: &str,
    options: Option<CompileOptions>,
) -> Result<IRProgram, CloveError> {
    runtime::compile_to_ir(source, options)
}

pub fn apply_default_foreign_tags(
    forms: Vec<ast::Form>,
    default_tag: Option<&str>,
) -> Result<Vec<ast::Form>, CloveError> {
    let effective_default = default_tag
        .map(|s| s.to_string())
        .or_else(|| collect_single_explicit_tag(&forms))
        .or_else(|| Some("rb".to_string()));
    forms
        .into_iter()
        .map(|f| apply_default_tag_form(f, effective_default.as_deref()))
        .collect()
}

pub fn apply_default_foreign_tags_for_source(
    forms: Vec<ast::Form>,
    source_name: Option<&str>,
    base_default: Option<&str>,
) -> Result<(Vec<ast::Form>, Option<String>), CloveError> {
    let (forms, use_default) = extract_use_default_foreign_tag(forms)?;
    let ext_default = source_name.and_then(detect_default_tag_from_name);
    let final_default = match (ext_default.clone(), use_default) {
        (Some(ext_tag), Some(use_tag)) => {
            if ext_tag != use_tag {
                let file = source_name.unwrap_or("<unknown>");
                eprintln!(
                    "{} file {} implies :{}, but (use default-interop/foreign :{}) was found; using :{}",
                    WARN_TAG, file, ext_tag, use_tag, ext_tag
                );
            }
            Some(ext_tag)
        }
        (Some(ext_tag), None) => Some(ext_tag),
        (None, Some(use_tag)) => Some(use_tag),
        (None, None) => base_default
            .map(|tag| tag.to_string())
            .or_else(|| Some("rb".to_string())),
    };
    let tagged = apply_default_foreign_tags(forms, final_default.as_deref())?;
    Ok((tagged, final_default))
}

fn extract_use_default_foreign_tag(
    forms: Vec<ast::Form>,
) -> Result<(Vec<ast::Form>, Option<String>), CloveError> {
    let mut sanitized = Vec::with_capacity(forms.len());
    let mut use_default: Option<String> = None;
    for form in forms {
        if let Some(tag) = parse_use_default_foreign_tag(&form)? {
            if let Some(prev) = &use_default {
                if prev != &tag {
                    return Err(CloveError::runtime(format!(
                        "use default-interop/foreign expects a single value per file (found :{} and :{})",
                        prev, tag
                    ))
                    .with_span(form.span));
                }
            } else {
                use_default = Some(tag);
            }
            continue;
        }
        sanitized.push(form);
    }
    Ok((sanitized, use_default))
}

fn parse_use_default_foreign_tag(form: &ast::Form) -> Result<Option<String>, CloveError> {
    let ast::FormKind::List(items) = &form.kind else {
        return Ok(None);
    };
    let Some(ast::FormKind::Symbol(head)) = items.first().map(|f| &f.kind) else {
        return Ok(None);
    };
    if head != "use" {
        return Ok(None);
    }
    let Some(ast::FormKind::Symbol(feature)) = items.get(1).map(|f| &f.kind) else {
        return Ok(None);
    };
    if feature != "default-interop" && feature != "default-foreign" {
        return Ok(None);
    }
    if items.len() != 3 {
        return Err(CloveError::runtime(
            "use default-interop/foreign expects a single language tag (e.g. :rb)",
        )
        .with_span(form.span));
    }
    let lang_form = &items[2];
    let tag = normalize_default_foreign_tag(lang_form)?;
    Ok(Some(tag))
}

fn normalize_default_foreign_tag(form: &ast::Form) -> Result<String, CloveError> {
    let raw =
        match &form.kind {
            ast::FormKind::Keyword(sym) | ast::FormKind::String(sym) => sym.as_str(),
            _ => return Err(CloveError::runtime(
                "use default-interop/foreign expects a keyword or string tag (e.g. :rb or \"py\")",
            )
            .with_span(form.span)),
        };
    match raw {
        "rb" | "ruby" => Ok("rb".to_string()),
        "py" | "python" => Ok("py".to_string()),
        _ => Err(CloveError::runtime(format!(
            "unknown foreign language '{}'; expected :rb/:ruby/:py/:python",
            raw
        ))
        .with_span(form.span)),
    }
}

fn collect_single_explicit_tag(forms: &[ast::Form]) -> Option<String> {
    fn walk(form: &ast::Form, tags: &mut HashSet<String>) {
        match &form.kind {
            ast::FormKind::ForeignRaw { tag, .. } => {
                if let Some(t) = tag {
                    tags.insert(t.clone());
                }
            }
            ast::FormKind::ForeignSymbol { tag, .. } => {
                if let Some(t) = tag {
                    tags.insert(t.clone());
                }
            }
            ast::FormKind::List(items)
            | ast::FormKind::Vector(items)
            | ast::FormKind::Set(items) => {
                for it in items {
                    walk(it, tags);
                }
            }
            ast::FormKind::Map(entries) => {
                for entry in entries {
                    match entry {
                        ast::MapItem::KeyValue(k, v) => {
                            walk(k, tags);
                            walk(v, tags);
                        }
                        ast::MapItem::Spread(expr) => walk(expr, tags),
                    }
                }
            }
            ast::FormKind::ShortFn(items) => {
                for it in items {
                    walk(it, tags);
                }
            }
            _ => {}
        }
    }
    let mut set = std::collections::HashSet::new();
    for f in forms {
        walk(f, &mut set);
    }
    if set.len() == 1 {
        set.into_iter().next()
    } else {
        None
    }
}

fn apply_default_tag_form(
    form: ast::Form,
    default_tag: Option<&str>,
) -> Result<ast::Form, CloveError> {
    let span = form.span;
    let kind = match form.kind {
        ast::FormKind::ForeignRaw { tag, code } => {
            let resolved_tag = tag
                .clone()
                .or_else(|| default_tag.map(|t| t.to_string()))
                .ok_or_else(|| {
                    CloveError::runtime(format!(
                        "unknown:{}:{}: foreign block requires explicit tag (e.g. ${{...}} / $rb{{...}} / $py{{...}}) or default language (e.g. :lang rb)",
                        span.line, span.col
                    ))
                })?;
            // Sugar to evaluate forms like $Foo.bar(...) with a default tag.
            // If the tag is not a language tag, replace it with the default tag and complete the code.
            let (final_tag, final_code) = match (tag.as_deref(), default_tag) {
                (Some(raw_tag), Some(def_tag)) if raw_tag != def_tag && raw_tag.contains('.') => {
                    let call = if code.trim().is_empty() {
                        format!("{}()", raw_tag)
                    } else {
                        format!("{}({})", raw_tag, code)
                    };
                    (def_tag.to_string(), call)
                }
                _ => (resolved_tag, code),
            };
            ast::FormKind::ForeignRaw {
                tag: Some(final_tag),
                code: final_code,
            }
        }
        ast::FormKind::ForeignSymbol { tag, path } => {
            let resolved_tag = tag
                .or_else(|| default_tag.map(|t| t.to_string()))
                .ok_or_else(|| {
                    CloveError::runtime(format!(
                        "unknown:{}:{}: foreign callable requires explicit tag (e.g. $rb/ $py) or default language (e.g. :lang rb)",
                        span.line, span.col
                    ))
                })?;
            ast::FormKind::ForeignSymbol {
                tag: Some(resolved_tag),
                path,
            }
        }
        ast::FormKind::List(items) => ast::FormKind::List(
            items
                .into_iter()
                .map(|it| apply_default_tag_form(it, default_tag))
                .collect::<Result<Vec<_>, CloveError>>()?,
        ),
        ast::FormKind::Vector(items) => ast::FormKind::Vector(
            items
                .into_iter()
                .map(|it| apply_default_tag_form(it, default_tag))
                .collect::<Result<Vec<_>, CloveError>>()?,
        ),
        ast::FormKind::Set(items) => ast::FormKind::Set(
            items
                .into_iter()
                .map(|it| apply_default_tag_form(it, default_tag))
                .collect::<Result<Vec<_>, CloveError>>()?,
        ),
        ast::FormKind::ShortFn(items) => ast::FormKind::ShortFn(
            items
                .into_iter()
                .map(|it| apply_default_tag_form(it, default_tag))
                .collect::<Result<Vec<_>, CloveError>>()?,
        ),
        ast::FormKind::Map(entries) => ast::FormKind::Map(
            entries
                .into_iter()
                .map(|entry| match entry {
                    ast::MapItem::KeyValue(k, v) => Ok(ast::MapItem::KeyValue(
                        apply_default_tag_form(k, default_tag)?,
                        apply_default_tag_form(v, default_tag)?,
                    )),
                    ast::MapItem::Spread(expr) => Ok(ast::MapItem::Spread(apply_default_tag_form(
                        expr,
                        default_tag,
                    )?)),
                })
                .collect::<Result<Vec<_>, CloveError>>()?,
        ),
        other => other,
    };
    Ok(ast::Form::new(kind, span))
}

pub fn detect_default_tag_from_name(name: &str) -> Option<String> {
    let path = Path::new(name);
    let mut comps: Vec<&str> = path.file_name()?.to_str()?.split('.').collect();
    if comps.len() < 2 {
        return None;
    }
    let ext = comps.pop()?;
    let lang = comps.pop()?;
    if ext == "clv" {
        match lang {
            "rb" => Some("rb".into()),
            "py" => Some("py".into()),
            _ => None,
        }
    } else {
        None
    }
}
