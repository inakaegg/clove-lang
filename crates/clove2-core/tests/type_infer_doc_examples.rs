use clove2_core::reader::read_all;
use clove2_core::syntax::parse_forms;
use clove2_core::type_infer::{check_program, DiagnosticLevel};
use clove2_core::use_directive::NativeLevel;
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Deserialize)]
struct DocEntry {
    name: String,
    #[serde(default)]
    origin: Option<String>,
    #[serde(default)]
    examples: Vec<String>,
}

struct ExampleParts {
    expr_src: String,
    expected_src: String,
}

#[test]
fn clove2_doc_examples_typecheck() {
    let repo_root = workspace_root();
    let doc_path = repo_root
        .join("data")
        .join("clove2_docs")
        .join("clove-docs.json");
    let data = fs::read_to_string(&doc_path)
        .unwrap_or_else(|err| panic!("failed to read {}: {}", doc_path.display(), err));
    let entries: Vec<DocEntry> =
        serde_json::from_str(&data).expect("clove-docs.json must contain valid JSON");

    let mut failures = Vec::new();
    for entry in entries {
        if entry.origin.as_deref() == Some("special form") {
            continue;
        }
        for (idx, example) in entry.examples.iter().enumerate() {
            let trimmed = example.trim();
            if trimmed.is_empty() {
                continue;
            }
            let Some(parts) = split_example(trimmed) else {
                failures.push(format!(
                    "{} example #{} is missing '=>': {}",
                    entry.name,
                    idx + 1,
                    example
                ));
                continue;
            };
            let expr_src = unwrap_comment_expr(&parts.expr_src).unwrap_or(&parts.expr_src);
            if expr_src.trim().is_empty() {
                continue;
            }
            let forms = match read_all(expr_src) {
                Ok(forms) => forms,
                Err(err) => {
                    failures.push(format!(
                        "{} example #{} parse error: {}\n  code: {}",
                        entry.name,
                        idx + 1,
                        err,
                        expr_src
                    ));
                    continue;
                }
            };
            let ast = match parse_forms(&forms) {
                Ok(ast) => ast,
                Err(err) => {
                    failures.push(format!(
                        "{} example #{} syntax error: {}\n  code: {}",
                        entry.name,
                        idx + 1,
                        err,
                        expr_src
                    ));
                    continue;
                }
            };
            let diags = check_program(&ast, NativeLevel::Warn);
            if diags.iter().any(|d| d.level == DiagnosticLevel::Error) {
                failures.push(format!(
                    "{} example #{} type error\n  code: {}\n  expect: {}",
                    entry.name,
                    idx + 1,
                    expr_src,
                    parts.expected_src
                ));
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "doc examples typecheck failed ({} cases):\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}

fn split_example(src: &str) -> Option<ExampleParts> {
    let mut in_string = false;
    let mut escape = false;
    let mut idx = None;
    let mut iter = src.char_indices().peekable();
    while let Some((pos, ch)) = iter.next() {
        if ch == '"' && !escape {
            in_string = !in_string;
        }
        if ch == '\\' && !escape {
            escape = true;
            continue;
        }
        if !in_string && ch == '=' {
            if let Some((_, '>')) = iter.peek() {
                idx = Some(pos);
                break;
            }
        }
        escape = false;
    }
    let Some(idx) = idx else {
        return None;
    };
    let expr_src = src[..idx].trim();
    let expected_src = src[idx + 2..].trim();
    Some(ExampleParts {
        expr_src: strip_inline_comment(expr_src),
        expected_src: strip_inline_comment(expected_src),
    })
}

fn unwrap_comment_expr(expr_src: &str) -> Option<&str> {
    let trimmed = expr_src.trim();
    if !trimmed.starts_with("(comment") || !trimmed.ends_with(')') {
        return None;
    }
    let inner = &trimmed["(comment".len()..trimmed.len() - 1];
    let inner = inner.trim();
    if inner.is_empty() {
        None
    } else {
        Some(inner)
    }
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
