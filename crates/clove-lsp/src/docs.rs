use clove_core::docs;
pub use clove_core::docs::DocEntry;
use std::path::PathBuf;

pub fn all_docs() -> &'static [DocEntry] {
    docs::doc_entries()
}

pub fn set_extra_doc_dirs(dirs: Vec<PathBuf>) {
    docs::set_extra_doc_dirs(dirs);
}

pub fn package_doc_dirs_from_roots(roots: &[PathBuf]) -> Vec<PathBuf> {
    docs::package_doc_dirs_from_roots(roots)
}

pub fn find_doc(symbol: &str) -> Option<&'static DocEntry> {
    docs::find_doc_entry(symbol)
}

pub fn signature_params_for(entry: &DocEntry) -> Option<Vec<String>> {
    entry
        .signature
        .as_ref()
        .map(|sig| parse_signature_params(sig))
}

fn parse_signature_params(signature: &str) -> Vec<String> {
    let tokens: Vec<&str> = signature.split_whitespace().collect();
    if tokens.len() <= 1 {
        return Vec::new();
    }
    tokens[1..].iter().map(|s| s.to_string()).collect()
}
