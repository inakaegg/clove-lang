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
    // Observing an agent right after a send is a race: the worker thread may not have
    // applied the function yet. Under load this produced
    // `expected #<agent state=1 pending=0> got #<agent state=0 pending=1>`.
    "agent-await",
    "agent-send!",
    "agent-send-io!",
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

const OOP_SKIP_SYMBOLS: &[&str] = &[
    "->", "->>", "as->", "cond->", "cond->>", "some->", "some->>",
];

#[derive(Deserialize)]
struct DocEntry {
    name: String,
    #[serde(default)]
    origin: Option<String>,
    #[serde(default)]
    examples: Vec<String>,
}

/// Doc examples include recursive definitions, and libtest threads are small, so the
/// corpus runs on a thread with the same kind of stack budget the CLI configures.
const TEST_STACK: usize = 256 * 1024 * 1024;

/// Wall-clock budget for one pass over the corpus. Exceeding it fails instead of
/// silently reporting success on a partial run.
const EXAMPLE_BUDGET_SECS: u64 = 600;

fn with_example_stack<F>(body: F)
where
    F: FnOnce() + Send + 'static,
{
    std::thread::Builder::new()
        .stack_size(TEST_STACK)
        .spawn(move || {
            clove_core::stack::configure_thread(TEST_STACK);
            body();
        })
        .expect("spawn doc example thread")
        .join()
        .expect("doc example thread panicked");
}

#[test]
fn doc_examples_match_actual_results() {
    with_example_stack(check_doc_examples);
}

fn check_doc_examples() {
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
            if start_time.elapsed() > Duration::from_secs(EXAMPLE_BUDGET_SECS) {
                // 打ち切って成功扱いにすると「検査したふり」になる。失敗として残す。
                failures.push(format!(
                    "doc examples exceeded the {}s budget after {:?} (executed {}); the corpus was not fully checked",
                    EXAMPLE_BUDGET_SECS,
                    start_time.elapsed(),
                    executed
                ));
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
    with_example_stack(check_oop_doc_examples);
}

fn check_oop_doc_examples() {
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
            if start_time.elapsed() > Duration::from_secs(EXAMPLE_BUDGET_SECS) {
                // 打ち切って成功扱いにすると「検査したふり」になる。失敗として残す。
                failures.push(format!(
                    "oop doc examples exceeded the {}s budget after {:?} (executed {}); the corpus was not fully checked",
                    EXAMPLE_BUDGET_SECS,
                    start_time.elapsed(),
                    executed
                ));
                break 'entry_loop;
            }
            let trimmed = example.trim();
            if trimmed.is_empty() {
                continue;
            }
            let Some(oop_example) =
                doc_examples::try_gen_oop_example(trimmed, entry.origin.as_deref())
            else {
                continue;
            };
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
            if let Some(sym) = find_skip_symbol(&parts.expr_src, OOP_SKIP_SYMBOLS) {
                eprintln!(
                    "skip oop doc example: {} #{} (threading form {})",
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
                Ok(value) => realize_value(value),
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
            let oop_src = format!("(use oop-syntax true)\n{}", oop_parts.expr_src);
            let oop_ctx = runtime_ctx(&repo_root);
            let oop_value = match oop_ctx.eval_source(&oop_src) {
                Ok(value) => realize_value(value),
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
            if !matches_expected(&oop_ctx, &oop_value, &parts.expected_src) {
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
            if shape_of(&oop_value) != shape_of(&original) {
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

#[test]
fn oop_doc_examples_skip_threading_forms() {
    for form in OOP_SKIP_SYMBOLS {
        let expr = format!("({} value (map inc))", form);
        assert_eq!(find_skip_symbol(&expr, OOP_SKIP_SYMBOLS), Some(*form));
    }
}

#[test]
fn doc_examples_skip_blocking_agent_waits() {
    assert_eq!(
        should_skip_example("(let [a (agent 0)] (agent-await a))"),
        Some("agent-await")
    );
}

fn run_example(
    actual_ctx: &Arc<RuntimeCtx>,
    expr_src: &str,
    expected_src: &str,
) -> Result<(), String> {
    let actual = match actual_ctx.eval_source(expr_src) {
        Ok(value) => realize_value(value),
        Err(err) => return Err(format!("expr error: {}", err)),
    };
    if matches_expected(actual_ctx, &actual, expected_src) {
        Ok(())
    } else {
        Err(format!(
            "mismatch (expected {}, got {})",
            clean_expected(expected_src),
            render_value(&actual)
        ))
    }
}

/// lazy seq を実体化する。
///
/// `SeqHandle::collect_all` は seq を消費するので、同じ値を2回レンダリングすると
/// 2回目は空になる。比較とエラーメッセージで同じ値を何度も見るため、先に潰しておく。
/// doc では seq もリストも `(..)` 表記なので `Value::List` へ落として構わない。
fn realize_value(value: Value) -> Value {
    match value {
        Value::Seq(handle) => match handle.collect_all() {
            Ok(items) => Value::List(items.into_iter().map(realize_value).collect()),
            Err(_) => Value::Seq(handle),
        },
        Value::List(items) => Value::List(items.into_iter().map(realize_value).collect()),
        Value::Vector(items) => Value::Vector(items.into_iter().map(realize_value).collect()),
        Value::Map(map) => Value::Map(
            map.into_iter()
                .map(|(k, v)| (k, realize_value(v)))
                .collect(),
        ),
        other => other,
    }
}

/// doc の期待値と実際の値が一致するか。
///
/// 期待値を Clove の値として読み直して**値として**比べる。文字列比較だけだと、
/// 文字列中の `"` のエスケープ、複数行文字列、マップのキー順という
/// 「値としては同じもの」で落ちる。
///
/// `#<lambda>` のように読み戻せない期待値は Clove 値にできないので、
/// 従来どおりレンダリング結果の文字列で比べる。
fn matches_expected(ctx: &Arc<RuntimeCtx>, actual: &Value, expected_src: &str) -> bool {
    let expected = clean_expected(expected_src);
    if render_value(actual) == expected {
        return true;
    }
    match parse_expected_value(ctx, &expected) {
        Some(expected_value) => shape_of(actual) == shape_of(&expected_value),
        None => false,
    }
}

/// 期待値の式を評価せずに値へ変換する。`(1 2 3)` は呼び出しではなくリストなので
/// `quote` を通す。読めない表記（`#<lambda>` など）は `None`。
fn parse_expected_value(ctx: &Arc<RuntimeCtx>, expected: &str) -> Option<Value> {
    if expected.is_empty() {
        return None;
    }
    ctx.eval_source(&format!("(quote {})", expected))
        .ok()
        .map(realize_value)
}

/// 値の比較用の正規形。
///
/// - 数値は従来のレンダリングに合わせて文字列で持つ（`3` と `3.0` を区別しない）
/// - list と lazy seq は doc ではどちらも `(..)` なので同じものとして扱う
/// - set とマップはキーで並べ替えるので、要素順・キー順に依存しない
/// - 比較できない値（関数・atom・chan など）はレンダリング文字列で持つ
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Shape {
    Nil,
    Bool(bool),
    Num(String),
    Str(String),
    /// シンボルとキーワード（Cloveのキーワードは `:` 付きのシンボル）
    Sym(String),
    /// Key で直接表せない複合値のキー。文字列キーと区別する。
    CompositeKey(Box<Shape>),
    Seq(Vec<Shape>),
    Vector(Vec<Shape>),
    Set(Vec<Shape>),
    Map(Vec<(Shape, Shape)>),
    Opaque(String),
}

fn shape_of(value: &Value) -> Shape {
    match value {
        Value::Nil => Shape::Nil,
        Value::Bool(b) => Shape::Bool(*b),
        Value::Int(_) | Value::Float(_) => Shape::Num(value.to_string()),
        Value::String(s) => Shape::Str(s.clone()),
        Value::Symbol(s) => Shape::Sym(s.clone()),
        Value::List(items) => Shape::Seq(items.iter().map(shape_of).collect()),
        Value::Vector(items) => Shape::Vector(items.iter().map(shape_of).collect()),
        Value::Seq(handle) => match handle.collect_all() {
            Ok(items) => Shape::Seq(items.iter().map(shape_of).collect()),
            Err(err) => Shape::Opaque(format!("<seq error: {}>", err)),
        },
        Value::Set(items) => {
            let mut shapes: Vec<Shape> = items.iter().map(shape_of).collect();
            shapes.sort();
            Shape::Set(shapes)
        }
        Value::Map(map) => {
            let mut entries: Vec<(Shape, Shape)> = map
                .iter()
                .map(|(k, v)| (shape_of_key(k), shape_of(v)))
                .collect();
            entries.sort();
            Shape::Map(entries)
        }
        // sorted-map もキー順に正規化する。doc の期待値はマップリテラルで書くしかなく、
        // 「順序付きであること」を普通のマップと区別して書けない。順序は keys / vals /
        // seq の例で確かめる。
        Value::SortedMap(_) | Value::SortedSet(_) => match sorted_collection_shape(value) {
            Some(shape) => shape,
            None => Shape::Opaque(render_value(value)),
        },
        _ => Shape::Opaque(render_value(value)),
    }
}

fn sorted_collection_shape(value: &Value) -> Option<Shape> {
    match value {
        Value::SortedMap(map) => {
            let mut entries: Vec<(Shape, Shape)> = map
                .entries
                .iter()
                .map(|(k, v)| (shape_of_key(k), shape_of(v)))
                .collect();
            entries.sort();
            Some(Shape::Map(entries))
        }
        Value::SortedSet(set) => {
            let mut shapes: Vec<Shape> = set.entries.iter().map(shape_of).collect();
            shapes.sort();
            Some(Shape::Set(shapes))
        }
        _ => None,
    }
}

fn shape_of_key(key: &Key) -> Shape {
    match key {
        Key::Keyword(s) => Shape::Sym(format!(":{}", s)),
        Key::Symbol(s) => Shape::Sym(s.clone()),
        Key::String(s) => Shape::Str(s.clone()),
        Key::Number(n) => Shape::Num(n.to_string()),
        Key::Bool(b) => Shape::Bool(*b),
        // 複合キーは元の値の形で比べる。文字列キーとは別物。
        Key::Composite(k) => Shape::CompositeKey(Box::new(shape_of(k.value()))),
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
            let mut items: Vec<String> = set.iter().map(render_value).collect();
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
    let parts: Vec<String> = iter.map(render_value).collect();
    format!("{}{}{}", start, parts.join(" "), end)
}

fn render_key(key: &Key) -> String {
    match key {
        Key::Keyword(s) | Key::Symbol(s) => format!(":{}", s),
        Key::String(s) => format!("\"{}\"", s),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
        // 既に Clove 構文なのでそのまま出す。
        Key::Composite(k) => k.repr().to_string(),
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
    find_skip_symbol(expr_src, SKIP_SYMBOLS)
}

fn find_skip_symbol(expr_src: &str, skip_symbols: &'static [&'static str]) -> Option<&'static str> {
    let tokens: Vec<&str> = expr_src
        .split(|ch: char| {
            !(ch.is_alphanumeric() || ch == ':' || ch == '-' || ch == '>' || ch == '!' || ch == '?')
        })
        .filter(|s| !s.is_empty())
        .collect();
    skip_symbols
        .iter()
        .copied()
        .find(|sym| tokens.iter().any(|tok| tok == sym))
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
