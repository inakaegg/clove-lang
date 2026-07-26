use std::fs;
use std::process::{Command, Output};

use tempfile::tempdir;

fn build_and_run(source: &str) -> Output {
    let root = tempdir().expect("create temporary build directory");
    let input = root.path().join("input.clv");
    let binary = root.path().join("output");
    fs::write(&input, source).expect("write Clove source");

    let build = Command::new(env!("CARGO_BIN_EXE_clove"))
        .arg("build")
        .arg(&input)
        .arg("--out")
        .arg(&binary)
        .output()
        .expect("run clove build");
    assert!(
        build.status.success(),
        "clove build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    Command::new(&binary)
        .output()
        .expect("run generated binary")
}

fn assert_success_stdout(output: Output, expected: &str) {
    assert!(
        output.status.success(),
        "generated binary failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8(output.stdout).expect("generated stdout must be UTF-8"),
        expected
    );
}

#[test]
fn build_preserves_falsey_and_bool_semantics() {
    let output = build_and_run(
        "(println (not nil))\n\
         (println (if (if true false nil) 1 2))\n\
         (println (bool \"false\"))\n\
         (println (bool \"　 false  \"))",
    );

    assert_success_stdout(output, "true\n2\nfalse\nfalse\n");
}

#[test]
fn build_uses_euclidean_mod_for_direct_and_map_calls() {
    let output = build_and_run(
        "(println (mod -3 2))\n\
         (println (mod 3 -2))\n\
         (println (map #(mod % 2) [-3 -2 -1 0 1 2 3]))",
    );

    assert_success_stdout(output, "1\n1\n[1 0 1 0 1 0 1]\n");
}

#[test]
fn build_uses_character_indexes_and_unicode_whitespace() {
    let output = build_and_run(
        "(println (subs \"あいう\" 1 2))\n\
         (println (blank? \"　\"))",
    );

    assert_success_stdout(output, "い\ntrue\n");
}

#[test]
fn build_rejects_non_ascii_case_conversion_instead_of_miscompiling() {
    let output = build_and_run("(println (capitalize \"ßeta\"))");

    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("phase2 C: capitalize currently supports ASCII strings only"),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn build_shift_and_compare_evaluate_defined_values_once() {
    let output = build_and_run(
        "(println (bit-shift-left 1 63))\n\
         (println (bit-shift-left 1 64))\n\
         (println (bit-shift-left 1 65))\n\
         (println (bit-shift-right -8 65))\n\
         (println (compare (rand-int 2) 0))",
    );

    assert_success_stdout(output, "-9223372036854775808\n1\n2\n-4\n0\n");
}

#[test]
fn build_bit_index_operations_define_the_sign_bit() {
    let output = build_and_run(
        "(println (bit-set 0 63))\n\
         (println (bit-test (bit-set 0 63) 63))\n\
         (println (bit-clear -1 63))\n\
         (println (bit-flip 0 63))\n\
         (println (map #(bit-set % 63) [0 1]))",
    );

    assert_success_stdout(
        output,
        "-9223372036854775808\ntrue\n9223372036854775807\n-9223372036854775808\n[-9223372036854775808 -9223372036854775807]\n",
    );
}

#[test]
fn build_evaluates_reused_builtin_arguments_once() {
    assert_success_stdout(
        build_and_run(
            "(println (< 0 (rand-int 100) 100))\n\
             (println (rand-int 100))",
        ),
        "true\n87\n",
    );
    assert_success_stdout(
        build_and_run("(println (juxt identity identity (rand-int 100)))"),
        "[10 10]\n",
    );
    assert_success_stdout(
        build_and_run(
            "(println (max 0 (rand-int 100)))\n\
             (println (rand-int 100))",
        ),
        "10\n87\n",
    );
    assert_success_stdout(
        build_and_run(
            "(println (not= (rand-int 100) 10 200))\n\
             (println (rand-int 100))",
        ),
        "true\n87\n",
    );
    assert_success_stdout(
        build_and_run(
            "(println (abs (rand-int 100)))\n\
             (println (rand-int 100))",
        ),
        "10\n87\n",
    );
}

#[test]
fn build_as_checks_types_without_converting_values() {
    let output = build_and_run(
        "(println (as Int 1))\n\
         (println (as Str 1))\n\
         (println (as Bool 1))\n\
         (println (as Int true))",
    );

    assert_success_stdout(output, "1\nnil\nnil\nnil\n");
}

#[test]
fn build_predicates_evaluate_inputs_and_inspect_optional_values() {
    let output = build_and_run(
        "(println (number? (rand-int 100)))\n\
         (println (rand-int 100))\n\
         (let [v (if false 1 nil)]\n\
           (println (nil? v))\n\
           (println (some? v))\n\
           (println (number? v)))\n\
         (let [v (if true false nil)]\n\
           (println (boolean? v))\n\
           (println (false? v))\n\
           (println (nil? v)))",
    );

    assert_success_stdout(output, "true\n87\ntrue\nfalse\nfalse\ntrue\ntrue\nfalse\n");
}

/// Defects recorded in docs/tooling/build.md on 2026-07-25.
mod recorded_defects {
    use super::*;

    fn build_output(source: &str) -> Output {
        let root = tempdir().expect("create temporary build directory");
        let input = root.path().join("input.clv");
        let binary = root.path().join("output");
        fs::write(&input, source).expect("write Clove source");
        Command::new(env!("CARGO_BIN_EXE_clove"))
            .arg("build")
            .arg(&input)
            .arg("--out")
            .arg(&binary)
            .output()
            .expect("run clove build")
    }

    fn build_message(source: &str) -> String {
        let output = build_output(source);
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
    }

    #[test]
    fn return_type_annotation_builds() {
        // The interpreter accepts a keyword return type right after the name
        // (docs/language/type_hints.md); the native path rejected it as
        // "params must be a vector".
        assert_success_stdout(
            build_and_run("(defn add :int [x<Int> y<Int>] (+ x y))\n(println (add 1 2))"),
            "3\n",
        );
        assert_success_stdout(
            build_and_run("(defn twice :int [x] (* x 2))\n(println (twice 21))"),
            "42\n",
        );
    }

    #[test]
    fn zero_and_many_parameter_functions_build() {
        assert_success_stdout(build_and_run("(defn f [] 1)\n(println (f))"), "1\n");
        assert_success_stdout(
            build_and_run("(defn f [a b c d] (+ a (+ b (+ c d))))\n(println (f 1 2 3 4))"),
            "10\n",
        );
        assert_success_stdout(
            build_and_run(
                "(defn g [a b c d e] (+ a (+ b (+ c (+ d e)))))\n(println (g 1 2 3 4 5))",
            ),
            "15\n",
        );
    }

    #[test]
    fn self_recursion_reports_an_error_instead_of_crashing_the_build() {
        // The backend inlines a function body at each call site, so a recursive call
        // expanded forever and took the build process down with a stack overflow.
        let message = build_message("(defn f [n] (if (< n 2) 1 (* n (f (dec n)))))\n(println (f 5))");
        assert!(
            message.contains("recursive"),
            "expected a recursion error, got:\n{message}"
        );
        assert!(
            !message.contains("overflowed its stack"),
            "the build must not crash:\n{message}"
        );
    }

    #[test]
    fn mutual_recursion_reports_an_error_instead_of_crashing_the_build() {
        let message = build_message(
            "(defn even2? [n] (if (= n 0) true (odd2? (dec n))))\n\
             (defn odd2? [n] (if (= n 0) false (even2? (dec n))))\n\
             (println (even2? 4))",
        );
        assert!(
            message.contains("recursive"),
            "expected a recursion error, got:\n{message}"
        );
        assert!(
            !message.contains("overflowed its stack"),
            "the build must not crash:\n{message}"
        );
    }
}
