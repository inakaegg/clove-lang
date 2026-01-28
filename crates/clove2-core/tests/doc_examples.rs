use clove2_core::eval::run_str;
use clove2_core::value::{Key, Value};
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
fn clove2_doc_examples_match_results() {
    let repo_root = workspace_root();
    std::env::set_current_dir(&repo_root).expect("set current dir");
    let tmp_dir = repo_root.join("tmp");
    let _ = fs::create_dir_all(&tmp_dir);

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
            match run_example(&parts) {
                Ok(()) => {}
                Err(reason) => failures.push(format!(
                    "{} example #{} failed: {}\n  code: {}\n  expect: {}",
                    entry.name,
                    idx + 1,
                    reason,
                    parts.expr_src,
                    parts.expected_src
                )),
            }
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

fn run_example(parts: &ExampleParts) -> Result<(), String> {
    let expected = clean_expected(&parts.expected_src);
    let expr_src = if expected != "nil" {
        unwrap_comment_expr(&parts.expr_src).unwrap_or(&parts.expr_src)
    } else {
        &parts.expr_src
    };
    let actual = run_str(expr_src).map_err(|err| format!("expr error: {}", err))?;
    let actual_rendered = render_doc_value(&actual);
    if actual_rendered == expected {
        Ok(())
    } else {
        Err(format!(
            "mismatch (expected {}, got {})",
            expected, actual_rendered
        ))
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

fn render_doc_value(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Bool(value) => value.to_string(),
        Value::Int(value) => value.to_string(),
        Value::Float(value) => format_float(*value),
        Value::Str(value) => format!("\"{}\"", escape_string(value)),
        Value::Regex(value) => format!("/{}/", escape_string(value)),
        Value::Keyword(value) => format!(":{}", value),
        Value::Symbol(value) => value.clone(),
        Value::Vec(items) => render_seq_like(items.iter(), "[", "]"),
        Value::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else {
                render_seq_like(items.iter(), "(", ")")
            }
        }
        Value::Set(items) => {
            let mut parts: Vec<String> = items.iter().map(render_doc_value).collect();
            parts.sort();
            format!("#{{{}}}", parts.join(" "))
        }
        Value::Map(map) => {
            let mut parts = Vec::new();
            for (key, value) in map.iter() {
                parts.push(format!("{} {}", render_key(key), render_doc_value(value)));
            }
            format!("{{{}}}", parts.join(" "))
        }
        Value::Partial(partial) => partial.desc.clone(),
        Value::Function(_) | Value::NativeFunction(_) | Value::Builtin(_) => "<fn>".to_string(),
    }
}

fn render_seq_like<'a>(iter: impl Iterator<Item = &'a Value>, start: &str, end: &str) -> String {
    let parts: Vec<String> = iter.map(render_doc_value).collect();
    format!("{}{}{}", start, parts.join(" "), end)
}

fn render_key(key: &Key) -> String {
    match key {
        Key::Str(value) => format!("\"{}\"", escape_string(value.as_ref())),
        Key::Keyword(value) => format!(":{}", value.as_ref()),
        Key::Symbol(value) => value.as_ref().to_string(),
        Key::Bool(value) => value.to_string(),
        Key::Int(value) => value.to_string(),
    }
}

fn escape_string(value: &str) -> String {
    let mut out = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

fn format_float(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{:.1}", value)
    } else {
        value.to_string()
    }
}

fn clean_expected(raw: &str) -> String {
    normalize_expected(strip_inline_comment(raw))
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

fn normalize_expected(expected: String) -> String {
    if expected.starts_with("\"\\\"") && expected.ends_with("\\\"\"") && expected.len() >= 6 {
        let unescaped = expected.replace("\\\"", "\"");
        if unescaped.starts_with("\"\"") && unescaped.ends_with("\"\"") && unescaped.len() >= 4 {
            return unescaped[1..unescaped.len() - 1].to_string();
        }
        return unescaped;
    }
    expected
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(Path::to_path_buf)
        .expect("workspace root should be parent of crate directory")
}
