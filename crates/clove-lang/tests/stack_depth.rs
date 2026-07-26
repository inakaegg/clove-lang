//! Deep recursion must report a Clove error, not crash the process.
//!
//! Before this guard the tree-walking evaluator ran on the 8MB main thread and used
//! ~23KB of native stack per Clove call, so a plain `(defn sum [xs] ... (sum (rest xs)))`
//! over 1000 elements died at depth ~350 with a `SIGSEGV`. Because the embedded Ruby VM
//! was booted eagerly, Ruby's own signal handler printed a
//! `ruby ... [BUG] Segmentation fault` crash dump for what was a Clove program.

use std::process::{Command, Output};

fn run_source(source: &str, extra_args: &[&str]) -> Output {
    let dir = tempfile::tempdir().expect("create temporary directory");
    let script = dir.path().join("deep.clv");
    std::fs::write(&script, source).expect("write Clove source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_clove"));
    command.args(extra_args);
    command.arg(&script);
    command.output().expect("run clove")
}

fn combined(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

const RECURSIVE_SUM: &str = r#"
(defn sum [xs]
  (if (empty? xs) 0 (+ (first xs) (sum (rest xs)))))
(println (sum (vec (range 0 1000))))
"#;

#[test]
fn idiomatic_recursion_over_1000_elements_succeeds() {
    let output = run_source(RECURSIVE_SUM, &[]);
    let text = combined(&output);
    assert!(
        output.status.success(),
        "recursion over 1000 elements must not fail:\n{text}"
    );
    assert!(
        text.contains("499500"),
        "expected the sum in the output:\n{text}"
    );
}

#[test]
fn deep_recursion_reports_a_clove_error_instead_of_crashing() {
    // 20万段は現実的な予算では返せない。明示エラーで止まることを確認する。
    let output = run_source(
        "(defn f [n] (if (= n 0) 0 (+ 1 (f (- n 1)))))\n(println (f 200000))\n",
        &[],
    );
    let text = combined(&output);
    assert!(
        !output.status.success(),
        "unbounded recursion must not report success:\n{text}"
    );
    assert!(
        text.contains("Recursion too deep"),
        "expected a Clove recursion error:\n{text}"
    );
    assert!(
        !text.contains("[BUG]"),
        "must not surface a Ruby crash dump:\n{text}"
    );
    assert!(
        !text.contains("overflowed its stack"),
        "must not surface a native stack overflow:\n{text}"
    );
}

#[test]
fn stack_option_is_recognized_after_other_options() {
    // `--stack` is taken out of the leading options before the runtime thread starts, and
    // that scan has to skip the values of options like `--mem-hard` — otherwise the flag
    // is left behind and reported as unknown.
    let output = run_source(
        "(defn f [n] (if (= n 0) 0 (+ 1 (f (- n 1)))))\n(println (f 1000))\n",
        &["--mem-hard", "4G", "--stack", "1M"],
    );
    let text = combined(&output);
    assert!(
        !text.contains("unknown option"),
        "--stack must be accepted after another option:\n{text}"
    );
    assert!(
        text.contains("Recursion too deep"),
        "the 1M budget must still apply:\n{text}"
    );
}

#[test]
fn stack_budget_is_configurable() {
    // A tiny budget must trip well before the default one does.
    let output = run_source(
        "(defn f [n] (if (= n 0) 0 (+ 1 (f (- n 1)))))\n(println (f 1000))\n",
        &["--stack", "1M"],
    );
    let text = combined(&output);
    assert!(
        text.contains("Recursion too deep"),
        "a 1M stack budget must stop 1000-deep recursion:\n{text}"
    );

    let output = run_source(
        "(defn f [n] (if (= n 0) 0 (+ 1 (f (- n 1)))))\n(println (f 1000))\n",
        &[],
    );
    let text = combined(&output);
    assert!(
        output.status.success() && text.contains("1000"),
        "the default budget must handle 1000-deep recursion:\n{text}"
    );
}
