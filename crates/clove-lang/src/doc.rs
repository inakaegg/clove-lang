use clove_core::ast::Value;
use clove_core::docs;
use clove_core::env::EnvRef;
use clove_core::eval::CURRENT_NS_KEY;
use clove_core::fn_meta;
use clove_core::type_registry::{self, TypeEntry};
use clove_core::types::TypeKind;

use crate::symbols::{canonical_symbol_name, doc_lookup_keys};

#[derive(Clone, Debug)]
pub struct DocInfo {
    pub name: String,
    pub canonical: String,
    pub signature: Option<String>,
    pub doc: Option<String>,
    pub origin: Option<String>,
}

impl DocInfo {
    pub fn summary_line(&self) -> Option<&str> {
        self.doc
            .as_deref()
            .and_then(|d| d.lines().next())
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
    }
}

pub fn describe_symbol(name: &str, scopes: &[EnvRef]) -> Option<DocInfo> {
    let keys = doc_lookup_keys(name);
    let builtin = keys.iter().find_map(|key| docs::find_doc_entry(key));
    if let Some(meta) = keys.iter().find_map(|key| fn_meta::get(key)) {
        let canonical = meta.fq_name();
        let signature = signature_from_meta_or_doc(name, &meta, builtin);
        let doc = normalize_doc(meta.doc.clone())
            .or_else(|| builtin.and_then(|info| normalize_doc(info.doc.clone())));
        let origin = builtin
            .and_then(|info| info.origin.clone())
            .or_else(|| Some(meta.ns.clone()));
        return Some(DocInfo {
            name: name.to_string(),
            canonical,
            signature,
            doc,
            origin,
        });
    }
    if !name.contains("::") {
        for ns in ["core", "std"] {
            let qualified = format!("{ns}::{name}");
            if let Some(meta) = fn_meta::get(&qualified) {
                let canonical = meta.fq_name();
                let signature = signature_from_meta_or_doc(name, &meta, builtin);
                let doc = normalize_doc(meta.doc.clone())
                    .or_else(|| builtin.and_then(|info| normalize_doc(info.doc.clone())));
                let origin = builtin
                    .and_then(|info| info.origin.clone())
                    .or_else(|| Some(meta.ns.clone()));
                return Some(DocInfo {
                    name: name.to_string(),
                    canonical,
                    signature,
                    doc,
                    origin,
                });
            }
        }
    }
    // 1) builtin table
    if let Some(entry) = builtin {
        let doc = normalize_doc(entry.doc.clone())?;
        return Some(DocInfo {
            name: name.to_string(),
            canonical: entry.canonical.clone(),
            signature: entry.signature.clone(),
            doc: Some(doc),
            origin: entry.origin.clone(),
        });
    }

    if let Some(info) = special_form_doc(name) {
        return Some(info);
    }

    // 2) env scopes (namespace -> shared)
    for env in scopes {
        for key in &keys {
            let val_opt = env.read().ok().and_then(|g| g.get(key));
            if let Some(val) = val_opt {
                match val {
                    Value::Lambda {
                        params, rest, doc, ..
                    } => {
                        let doc_text = normalize_doc(doc.clone());
                        let sig = lambda_signature(name, &params, rest.as_ref());
                        return Some(DocInfo {
                            name: name.to_string(),
                            canonical: canonical_symbol_name(name).into_owned(),
                            signature: Some(sig),
                            doc: doc_text,
                            origin: Some("lambda".into()),
                        });
                    }
                    Value::MultiLambda { clauses, doc, .. } => {
                        let doc_text = normalize_doc(doc.clone());
                        if let Some(first) = clauses.first() {
                            let sig = lambda_signature(name, &first.params, first.rest.as_ref());
                            return Some(DocInfo {
                                name: name.to_string(),
                                canonical: canonical_symbol_name(name).into_owned(),
                                signature: Some(sig),
                                doc: doc_text,
                                origin: Some("lambda".into()),
                            });
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    if let Some(info) = enum_variant_doc_info(name, scopes) {
        return Some(info);
    }

    None
}

fn signature_from_meta_or_doc(
    name: &str,
    meta: &fn_meta::FnMeta,
    builtin: Option<&docs::DocEntry>,
) -> Option<String> {
    let meta_sig = format_fnmeta_signature(name, meta);
    if meta_signature_is_generic(meta) {
        if let Some(sig) = builtin.and_then(|info| info.signature.clone()) {
            return Some(sig);
        }
    }
    meta_sig.or_else(|| builtin.and_then(|info| info.signature.clone()))
}

fn meta_signature_is_generic(meta: &fn_meta::FnMeta) -> bool {
    if meta.overloads.is_empty() {
        return false;
    }
    meta.overloads.iter().all(overload_is_generic_any)
}

fn overload_is_generic_any(overload: &fn_meta::FnOverload) -> bool {
    overload
        .arg_types
        .iter()
        .all(|ty| matches!(ty, TypeKind::Any))
        && overload
            .rest
            .as_ref()
            .map_or(true, |ty| matches!(ty, TypeKind::Any))
        && matches!(overload.ret_type, TypeKind::Any)
}

pub fn doc_examples_for(name: &str) -> Option<Vec<String>> {
    if let Some(entry) = docs::find_doc_entry(name) {
        if !entry.examples.is_empty() {
            return Some(entry.examples.clone());
        }
    }
    None
}

pub fn doc_oop_examples_for(name: &str) -> Option<Vec<String>> {
    if let Some(entry) = docs::find_doc_entry(name) {
        if !entry.oop_examples.is_empty() {
            return Some(entry.oop_examples.clone());
        }
    }
    None
}

pub fn builtin_doc_names() -> Vec<String> {
    docs::doc_entries()
        .iter()
        .map(|entry| entry.name.clone())
        .collect()
}

fn lambda_signature(name: &str, params: &[impl ToString], rest: Option<&impl ToString>) -> String {
    let mut parts: Vec<String> = params.iter().map(|p| p.to_string()).collect();
    if let Some(r) = rest {
        parts.push(format!("& {}", r.to_string()));
    }
    format!("{} [{}]", name, parts.join(" "))
}

fn format_fnmeta_signature(name: &str, meta: &clove_core::fn_meta::FnMeta) -> Option<String> {
    if meta.overloads.is_empty() {
        return meta
            .arglist
            .first()
            .map(|args| format!("{} {}", name, args));
    }
    let mut lines = Vec::new();
    for overload in &meta.overloads {
        lines.push(format_overload_signature(name, overload));
    }
    Some(lines.join("\n"))
}

fn format_overload_signature(name: &str, overload: &clove_core::fn_meta::FnOverload) -> String {
    let mut parts: Vec<String> = overload.arg_types.iter().map(format_type_kind).collect();
    if let Some(rest) = &overload.rest {
        parts.push(format!("& {}", format_type_kind(rest)));
    }
    let args = if parts.is_empty() {
        "()".to_string()
    } else {
        format!("({})", parts.join(", "))
    };
    format!(
        "{} {} -> {}",
        name,
        args,
        format_type_kind(&overload.ret_type)
    )
}

fn format_type_kind(kind: &TypeKind) -> String {
    kind.describe()
}

fn normalize_doc(doc: Option<String>) -> Option<String> {
    doc.and_then(|text| {
        if text.trim().is_empty() {
            None
        } else {
            Some(text)
        }
    })
}

fn special_form_doc(name: &str) -> Option<DocInfo> {
    if name != "where" {
        return None;
    }
    Some(DocInfo {
        name: name.to_string(),
        canonical: name.to_string(),
        signature: Some("where form*".into()),
        doc: Some("Attach local definitions to the current function body.".into()),
        origin: Some("special-form".into()),
    })
}

fn enum_variant_doc_info(name: &str, scopes: &[EnvRef]) -> Option<DocInfo> {
    let canonical = canonical_symbol_name(name);
    let (enum_raw, variant_name) = canonical.rsplit_once("::")?;
    if enum_raw.is_empty() || variant_name.is_empty() || enum_raw.contains('/') {
        return None;
    }
    let current_ns = scopes
        .iter()
        .find_map(|env| env.read().ok().and_then(|g| g.get(CURRENT_NS_KEY)))
        .and_then(|value| match value {
            Value::Symbol(ns) => Some(ns),
            Value::String(ns) => Some(ns),
            _ => None,
        });
    let enum_fqn = if enum_raw.contains("::") {
        enum_raw.to_string()
    } else {
        let ns = current_ns.as_deref().unwrap_or("user");
        format!("{ns}::{enum_raw}")
    };
    let TypeEntry::Sum(meta) = type_registry::get_type_entry(&enum_fqn)? else {
        return None;
    };
    let variant_fqn = meta
        .members
        .iter()
        .find(|member| enum_member_local_name(member.as_str()) == variant_name)
        .cloned()?;
    let TypeEntry::Product(product) = type_registry::get_type_entry(&variant_fqn)? else {
        return None;
    };
    let doc = normalize_doc(product.doc.clone())
        .or_else(|| Some(format!("{} variant {}", enum_raw, variant_name)));
    Some(DocInfo {
        name: name.to_string(),
        canonical: variant_fqn,
        signature: None,
        doc,
        origin: Some(format!("variant of {}", enum_raw)),
    })
}

fn enum_member_local_name(member: &str) -> &str {
    member
        .rsplit_once("::")
        .map(|(_, local)| local)
        .unwrap_or(member)
}
