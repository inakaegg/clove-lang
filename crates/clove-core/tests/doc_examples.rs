use clove_core::ast::{Key, Value};
use clove_core::doc_examples;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;
use serde::Deserialize;
use std::collections::HashSet;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

const SKIP_SYMBOLS: &[&str] = &[
    "repl",
    "sleep",
    "sleep-ms",
    "chan-take!",
    "chan-put!",
    "go",
    "go-loop",
    "async-scope",
    "async::scope",
    "scope-loop",
    "async::scope-loop",
    "select",
    "select-blocking",
    "http::request",
    "http::get",
    "http::post",
    "http::put",
    "http::delete",
    "sh",
    "process::sh",
    "std::sh",
    "repeat",
    "repeatedly",
    "iterate",
    "cycle",
    "lazy-cat",
    "line-seq",
];

#[derive(Deserialize)]
struct DocEntry {
    name: String,
    #[serde(default)]
    origin: Option<String>,
    #[serde(default)]
    examples: Vec<String>,
}

#[test]
fn doc_examples_match_actual_results() {
    let _guard = EnvVarGuard::set("CLOVE_NO_USER_CONFIG", "1");
    let repo_root = workspace_root();
    let doc_path = repo_root
        .join("data")
        .join("clove_docs")
        .join("clove-docs.json");
    let data = fs::read_to_string(&doc_path)
        .unwrap_or_else(|err| panic!("failed to read {}: {}", doc_path.display(), err));
    let entries: Vec<DocEntry> =
        serde_json::from_str(&data).expect("clove-docs.json must contain valid JSON");

    let mut failures = Vec::new();
    let allowlist = load_allowlist(&repo_root);
    let start_time = Instant::now();
    let mut executed = 0usize;
    'entry_loop: for entry in entries {
        if !should_check_entry(&allowlist, &entry.name) {
            continue;
        }
        for (idx, example) in entry.examples.iter().enumerate() {
            if start_time.elapsed() > Duration::from_secs(30) {
                eprintln!(
                    "stop doc examples after {:?} (executed {})",
                    start_time.elapsed(),
                    executed
                );
                break 'entry_loop;
            }
            let trimmed = example.trim();
            if trimmed.is_empty() {
                continue;
            }
            let parts = match doc_examples::split_example(trimmed) {
                Some(parts) => parts,
                None => {
                    failures.push(format!(
                        "{} example #{} is missing '=>': {}",
                        entry.name,
                        idx + 1,
                        example
                    ));
                    continue;
                }
            };
            if let Some(sym) = should_skip_example(&parts.expr_src) {
                eprintln!(
                    "skip doc example: {} #{} (contains {})",
                    entry.name,
                    idx + 1,
                    sym
                );
                continue;
            }
            let actual_ctx = runtime_ctx(&repo_root);
            match run_example(&actual_ctx, &parts.expr_src, &parts.expected_src) {
                Ok(_) => {}
                Err(reason) => failures.push(format!(
                    "{} example #{} failed: {}\n  code: {}\n  expect: {}",
                    entry.name,
                    idx + 1,
                    reason,
                    parts.expr_src,
                    parts.expected_src
                )),
            }
            executed += 1;
        }
    }

    if !failures.is_empty() {
        panic!(
            "doc examples failed ({} cases):\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}

#[test]
fn doc_examples_oop_match_actual_results() {
    let _guard = EnvVarGuard::set("CLOVE_NO_USER_CONFIG", "1");
    let repo_root = workspace_root();
    let doc_path = repo_root
        .join("data")
        .join("clove_docs")
        .join("clove-docs.json");
    let data = fs::read_to_string(&doc_path)
        .unwrap_or_else(|err| panic!("failed to read {}: {}", doc_path.display(), err));
    let entries: Vec<DocEntry> =
        serde_json::from_str(&data).expect("clove-docs.json must contain valid JSON");

    let mut failures = Vec::new();
    let allowlist = load_allowlist(&repo_root);
    let start_time = Instant::now();
    let mut executed = 0usize;
    'entry_loop: for entry in entries {
        if !should_check_entry(&allowlist, &entry.name) {
            continue;
        }
        for (idx, example) in entry.examples.iter().enumerate() {
            if start_time.elapsed() > Duration::from_secs(30) {
                eprintln!(
                    "stop oop doc examples after {:?} (executed {})",
                    start_time.elapsed(),
                    executed
                );
                break 'entry_loop;
            }
            let trimmed = example.trim();
            if trimmed.is_empty() {
                continue;
            }
            let parts = match doc_examples::split_example(trimmed) {
                Some(parts) => parts,
                None => {
                    failures.push(format!(
                        "{} example #{} is missing '=>': {}",
                        entry.name,
                        idx + 1,
                        example
                    ));
                    continue;
                }
            };
            let Some(oop_example) =
                doc_examples::try_gen_oop_example(trimmed, entry.origin.as_deref())
            else {
                continue;
            };
            let oop_parts = match doc_examples::split_example(&oop_example) {
                Some(parts) => parts,
                None => {
                    failures.push(format!(
                        "{} oop example #{} is missing '=>': {}",
                        entry.name,
                        idx + 1,
                        oop_example
                    ));
                    continue;
                }
            };
            if let Some(sym) = should_skip_example(&parts.expr_src) {
                eprintln!(
                    "skip oop doc example: {} #{} (contains {})",
                    entry.name,
                    idx + 1,
                    sym
                );
                continue;
            }
            let expected = clean_expected(&parts.expected_src);
            if clean_expected(&oop_parts.expected_src) != expected {
                failures.push(format!(
                    "{} oop example #{} expected mismatch: {} vs {}",
                    entry.name,
                    idx + 1,
                    oop_parts.expected_src,
                    parts.expected_src
                ));
                continue;
            }
            let original_ctx = runtime_ctx(&repo_root);
            let original = match original_ctx.eval_source(&parts.expr_src) {
                Ok(value) => value,
                Err(err) => {
                    failures.push(format!(
                        "{} example #{} original error: {}",
                        entry.name,
                        idx + 1,
                        err
                    ));
                    continue;
                }
            };
            let oop_src = format!("(do (use oop-syntax true) {})", oop_parts.expr_src);
            let oop_ctx = runtime_ctx(&repo_root);
            let oop_value = match oop_ctx.eval_source(&oop_src) {
                Ok(value) => value,
                Err(err) => {
                    failures.push(format!(
                        "{} example #{} oop error: {} (oop: {})",
                        entry.name,
                        idx + 1,
                        err,
                        oop_parts.expr_src
                    ));
                    continue;
                }
            };
            let original_rendered = render_value(&original);
            let oop_rendered = render_value(&oop_value);
            if oop_rendered != expected {
                failures.push(format!(
                    "{} example #{} expected mismatch (expected {}, got {})\n  oop: {}",
                    entry.name,
                    idx + 1,
                    expected,
                    oop_rendered,
                    oop_parts.expr_src
                ));
                continue;
            }
            if oop_rendered != original_rendered {
                failures.push(format!(
                    "{} example #{} value mismatch (orig {}, oop {})\n  expr: {}\n  oop: {}",
                    entry.name,
                    idx + 1,
                    original_rendered,
                    oop_rendered,
                    parts.expr_src,
                    oop_parts.expr_src
                ));
                continue;
            }
            executed += 1;
        }
    }

    if !failures.is_empty() {
        panic!(
            "oop doc examples failed ({} cases):\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}

fn run_example(
    actual_ctx: &Arc<RuntimeCtx>,
    expr_src: &str,
    expected_src: &str,
) -> Result<(), String> {
    let actual = match actual_ctx.eval_source(expr_src) {
        Ok(value) => value,
        Err(err) => return Err(format!("expr error: {}", err)),
    };
    let actual_rendered = render_value(&actual);
    let expected = clean_expected(expected_src);
    if actual_rendered == expected {
        Ok(())
    } else {
        Err(format!(
            "mismatch (expected {}, got {})",
            expected, actual_rendered
        ))
    }
}

fn runtime_ctx(repo_root: &Path) -> Arc<RuntimeCtx> {
    let opts = EvalOptions {
        working_dir: Some(repo_root.to_path_buf()),
        ..EvalOptions::default()
    };
    RuntimeCtx::new(opts, &[])
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Map(map) => {
            let mut entries: Vec<(&Key, String, &Value)> =
                map.iter().map(|(k, v)| (k, render_key(k), v)).collect();
            entries.sort_by(|(ka, ka_str, _), (kb, kb_str, _)| {
                let prio = |key: &Key, label: &str| -> i32 {
                    if label == ":status" {
                        0
                    } else if label == ":value" || label == ":error" {
                        1
                    } else if matches!(key, Key::Bool(_)) {
                        2
                    } else {
                        3
                    }
                };
                let pa = prio(ka, ka_str);
                let pb = prio(kb, kb_str);
                pa.cmp(&pb).then_with(|| match (ka, kb) {
                    (Key::Bool(a), Key::Bool(b)) => b.cmp(a), // true first
                    _ => ka_str.cmp(kb_str),
                })
            });
            let parts: Vec<String> = entries
                .iter()
                .map(|(_, k, v)| format!("{} {}", k, render_value(v)))
                .collect();
            format!("{{{}}}", parts.join(" "))
        }
        Value::List(items) => render_seq_like(items.iter(), "(", ")"),
        Value::Vector(items) => render_seq_like(items.iter(), "[", "]"),
        Value::Set(set) => {
            let mut items: Vec<String> = set.iter().map(|v| render_value(v)).collect();
            items.sort();
            format!("#{{{}}}", items.join(" "))
        }
        Value::Seq(handle) => match handle.collect_all() {
            Ok(items) => {
                if items.is_empty() {
                    "()".to_string()
                } else {
                    render_seq_like(items.iter(), "(", ")")
                }
            }
            Err(err) => format!("<seq error: {}>", err),
        },
        _ => value.to_string(),
    }
}

fn render_seq_like<'a>(iter: impl Iterator<Item = &'a Value>, start: &str, end: &str) -> String {
    let parts: Vec<String> = iter.map(|v| render_value(v)).collect();
    format!("{}{}{}", start, parts.join(" "), end)
}

fn render_key(key: &Key) -> String {
    match key {
        Key::Keyword(s) | Key::Symbol(s) => format!(":{}", s),
        Key::String(s) => format!("\"{}\"", s),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

fn clean_expected(raw: &str) -> String {
    strip_inline_comment(raw)
}

fn strip_inline_comment(raw: &str) -> String {
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in raw.char_indices() {
        if ch == '"' && !escape {
            in_string = !in_string;
        }
        if ch == '\\' && !escape {
            escape = true;
            continue;
        }
        if ch == ';' && !in_string {
            return raw[..idx].trim().to_string();
        }
        escape = false;
    }
    raw.trim().to_string()
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(Path::to_path_buf)
        .expect("workspace root should be parent of crate directory")
}

fn load_allowlist(repo_root: &Path) -> HashSet<String> {
    let path = repo_root
        .join("data")
        .join("clove_docs")
        .join("doc-test-allowlist.txt");
    let content = match fs::read_to_string(&path) {
        Ok(text) => text,
        Err(_) => return HashSet::new(),
    };
    let mut allow = HashSet::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if trimmed == "*" {
            continue;
        }
        allow.insert(trimmed.to_string());
    }
    allow
}

fn should_check_entry(allowlist: &HashSet<String>, name: &str) -> bool {
    !allowlist.contains(name)
}

fn should_skip_example(expr_src: &str) -> Option<&'static str> {
    let tokens: Vec<&str> = expr_src
        .split(|ch: char| {
            !(ch.is_alphanumeric() || ch == ':' || ch == '-' || ch == '!' || ch == '?')
        })
        .filter(|s| !s.is_empty())
        .collect();
    for sym in SKIP_SYMBOLS {
        if tokens.iter().any(|tok| tok == sym) {
            return Some(sym);
        }
    }
    None
}

struct EnvVarGuard {
    name: String,
    prev: Option<OsString>,
}

impl EnvVarGuard {
    fn set(name: &str, value: &str) -> Self {
        let prev = std::env::var_os(name);
        std::env::set_var(name, value);
        Self {
            name: name.to_string(),
            prev,
        }
    }
}

impl Drop for EnvVarGuard {
    fn drop(&mut self) {
        if let Some(prev) = self.prev.take() {
            std::env::set_var(&self.name, prev);
        } else {
            std::env::remove_var(&self.name);
        }
    }
}
