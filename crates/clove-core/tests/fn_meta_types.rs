use clove_core::fn_meta;
use clove_core::options::EvalOptions;
use clove_core::RuntimeCtx;
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

fn load_allowlist() -> HashSet<String> {
    let path = workspace_root()
        .join("data")
        .join("fixtures")
        .join("allowlist")
        .join("fn_meta_type_missing.txt");
    let content = match fs::read_to_string(&path) {
        Ok(text) => text,
        Err(_) => return HashSet::new(),
    };
    content
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|line| line.to_string())
        .collect()
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(PathBuf::from)
        .expect("workspace root")
}

#[test]
fn fnmeta_entries_without_overloads_are_allowlisted() {
    // Ensure builtins are registered
    let _ = clove_core::builtins::default_env();
    let allowlist = load_allowlist();
    let mut missing: Vec<String> = fn_meta::all()
        .into_iter()
        .filter(|meta| meta.overloads.is_empty())
        .map(|meta| meta.fq_name())
        .collect();
    missing.sort();
    let mut unexpected: Vec<String> = missing
        .into_iter()
        .filter(|name| !allowlist.contains(name))
        .collect();
    if !unexpected.is_empty() {
        unexpected.sort();
        panic!(
            "FnMeta entries missing overloads (add to allowlist if intentional): {:?}",
            unexpected
        );
    }
}

#[test]
fn fnmeta_entries_have_subject_pos_for_std_and_builtins() {
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let metas = ctx.with_current_ctx(|_ctx| fn_meta::all());
    let builtin_namespaces: HashSet<&str> = [
        "core", "std", "string", "io", "fs", "http", "async", "walk", "path", "shell", "process",
        "json", "ini", "time", "log", "env", "pprint", "set",
    ]
    .into_iter()
    .collect();
    let mut missing: Vec<String> = metas
        .into_iter()
        .filter(|meta| builtin_namespaces.contains(meta.ns.as_str()))
        .filter(|meta| meta.subject_pos.is_none())
        .map(|meta| meta.fq_name())
        .collect();
    if !missing.is_empty() {
        missing.sort();
        panic!(
            "FnMeta entries missing subject_pos (add subject-pos metadata if intentional): {:?}",
            missing
        );
    }
}
