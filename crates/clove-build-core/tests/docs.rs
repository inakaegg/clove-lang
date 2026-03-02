use std::collections::HashSet;

use clove_build_core::builtins::BUILTIN_NAMES;
use clove_build_core::docs::doc_entries;

#[test]
fn clove2_docs_cover_builtins() {
    let entries = doc_entries();
    let names: HashSet<&str> = entries.iter().map(|entry| entry.name.as_str()).collect();
    assert!(
        entries.len() >= BUILTIN_NAMES.len(),
        "doc entries must cover builtins (docs: {}, builtins: {})",
        entries.len(),
        BUILTIN_NAMES.len()
    );
    for name in BUILTIN_NAMES {
        assert!(names.contains(name), "missing docs for builtin: {name}");
    }
}
