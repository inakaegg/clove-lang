use serde::Deserialize;
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Debug, Deserialize)]
struct RawDocEntry {
    name: String,
    #[serde(default)]
    signature: Option<String>,
    #[serde(default)]
    doc: Option<String>,
    #[serde(default)]
    origin: Option<String>,
    #[serde(default)]
    examples: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct DocEntry {
    pub name: String,
    pub signature: Option<String>,
    pub doc: Option<String>,
    pub origin: Option<String>,
    pub examples: Vec<String>,
}

#[derive(Clone, Debug)]
struct DocStore {
    entries: Vec<DocEntry>,
    index: HashMap<String, usize>,
}

static DOC_STORE: OnceLock<DocStore> = OnceLock::new();

pub fn doc_entries() -> &'static [DocEntry] {
    &doc_store().entries
}

pub fn find_doc_entry(name: &str) -> Option<&'static DocEntry> {
    let store = doc_store();
    store
        .index
        .get(name)
        .and_then(|idx| store.entries.get(*idx))
}

pub fn format_doc_entry(entry: &DocEntry) -> Option<String> {
    let mut parts = Vec::new();
    if let Some(signature) = entry
        .signature
        .as_ref()
        .map(|text| text.trim())
        .filter(|text| !text.is_empty())
    {
        parts.push(signature.to_string());
    }
    if let Some(doc) = entry
        .doc
        .as_ref()
        .map(|text| text.trim())
        .filter(|text| !text.is_empty())
    {
        parts.push(doc.to_string());
    }
    if let Some(section) = format_examples_section("Examples", &entry.examples) {
        parts.push(section);
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("\n\n"))
    }
}

fn format_examples_section(title: &str, examples: &[String]) -> Option<String> {
    let lines: Vec<String> = examples
        .iter()
        .map(|ex| ex.trim())
        .filter(|ex| !ex.is_empty())
        .map(|ex| format!("  {}", ex))
        .collect();
    if lines.is_empty() {
        None
    } else {
        Some(format!("{}:\n{}", title, lines.join("\n")))
    }
}

fn doc_store() -> &'static DocStore {
    DOC_STORE.get_or_init(load_doc_store)
}

fn load_doc_store() -> DocStore {
    let json = include_str!("../../../data/clove2_docs/clove-docs.json");
    let parsed: Vec<RawDocEntry> = serde_json::from_str(json).expect("failed to parse clove2 docs");
    let entries: Vec<DocEntry> = parsed
        .into_iter()
        .map(|entry| DocEntry {
            name: entry.name,
            signature: entry.signature,
            doc: entry.doc,
            origin: entry.origin,
            examples: entry.examples,
        })
        .collect();
    let mut index = HashMap::new();
    for (idx, entry) in entries.iter().enumerate() {
        index.insert(entry.name.clone(), idx);
    }
    DocStore { entries, index }
}
