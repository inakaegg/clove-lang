use clove2_core::eval::Runtime;
use clove2_core::reader::read_all;
use clove2_core::syntax::{parse_forms, TopLevel};
use clove2_core::use_directive::parse_use_directives;
use clove2_core::value::{Key, Value};
use std::fs;
use std::path::{Path, PathBuf};

struct Example {
    line: usize,
    expr_src: String,
    expected_src: String,
}

#[test]
fn syntax_coverage_examples_match_results() {
    let repo_root = workspace_root();
    std::env::set_current_dir(&repo_root).expect("set current dir");
    let tmp_dir = repo_root.join("tmp");
    let _ = fs::create_dir_all(&tmp_dir);
    let fixture_path = repo_root.join("docs/phase2/fixtures/syntax_coverage.clv");
    let content = fs::read_to_string(&fixture_path).expect("read syntax_coverage");
    let examples = extract_examples(&content);

    let forms = read_all(&content).expect("read_all");
    let (use_cfg, _) = parse_use_directives(&forms).expect("parse use directives");
    let ast = parse_forms(&forms).expect("parse forms");

    let mut runtime = Runtime::new();
    runtime.set_default_mut_mode(use_cfg.default_mut_mode());
    load_definitions(&ast, &mut runtime).expect("load defs");

    let mut failures = Vec::new();
    for example in examples {
        if should_skip_example(&example) {
            continue;
        }
        match run_example(&example, &mut runtime) {
            Ok(()) => {}
            Err(reason) => failures.push(format!(
                "line {} failed: {}\n  code: {}\n  expect: {}",
                example.line, reason, example.expr_src, example.expected_src
            )),
        }
    }

    if !failures.is_empty() {
        panic!(
            "syntax_coverage examples failed ({} cases):\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}

fn load_definitions(items: &[TopLevel], runtime: &mut Runtime) -> Result<(), String> {
    for item in items {
        match item {
            TopLevel::Def { name, value, .. } => {
                let value = runtime
                    .eval_expr(value)
                    .map_err(|err| format!("def {}: {}", name, err))?;
                runtime.set_global(name, value);
            }
            TopLevel::Defn {
                name, params, body, ..
            } => {
                let value = runtime.define_function(params.clone(), body.clone());
                runtime.set_global(name, value);
            }
            TopLevel::DefForeign { decl, .. } => {
                let value = Value::Builtin(format!("__foreign::{}", decl.name));
                runtime.set_global(&decl.name, value);
            }
            TopLevel::DefType { .. } => {}
            TopLevel::Expr { .. } => {}
        }
    }
    Ok(())
}

fn run_example(example: &Example, runtime: &mut Runtime) -> Result<(), String> {
    let expected = clean_expected(&example.expected_src);
    let forms = read_all(&example.expr_src).map_err(|err| format!("read error: {}", err))?;
    let ast = parse_forms(&forms).map_err(|err| format!("parse error: {}", err))?;
    if ast.len() != 1 {
        return Err("example must be a single form".to_string());
    }
    let expr = match &ast[0] {
        TopLevel::Expr { expr, .. } => expr,
        _ => return Err("example must be an expression form".to_string()),
    };
    let actual = runtime
        .eval_expr(expr)
        .map_err(|err| format!("expr error: {}", err))?;
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

fn should_skip_example(example: &Example) -> bool {
    let expected = clean_expected(&example.expected_src);
    let expr = example.expr_src.trim();
    if expr.starts_with("(comment") && expected != "nil" {
        return true;
    }
    false
}

fn extract_examples(src: &str) -> Vec<Example> {
    let mut out = Vec::new();
    for (idx, line) in src.lines().enumerate() {
        if let Some((expr_src, expected_src)) = split_expectation_line(line) {
            out.push(Example {
                line: idx + 1,
                expr_src,
                expected_src,
            });
        }
    }
    out
}

fn split_expectation_line(line: &str) -> Option<(String, String)> {
    let mut in_string = false;
    let mut escape = false;
    let mut comment_idx = None;
    for (idx, ch) in line.char_indices() {
        if ch == '"' && !escape {
            in_string = !in_string;
        }
        if ch == '\\' && !escape {
            escape = true;
            continue;
        }
        if ch == ';' && !in_string {
            comment_idx = Some(idx);
            break;
        }
        escape = false;
    }
    let comment_idx = comment_idx?;
    let expr_src = line[..comment_idx].trim();
    if expr_src.is_empty() {
        return None;
    }
    let comment = line[comment_idx + 1..].trim();
    let arrow = comment.find("=>")?;
    let expected_src = comment[arrow + 2..].trim();
    Some((expr_src.to_string(), expected_src.to_string()))
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

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(Path::to_path_buf)
        .expect("workspace root should be parent of crate directory")
}
