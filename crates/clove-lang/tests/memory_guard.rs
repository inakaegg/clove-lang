//! The memory guard has to stop runaway allocation inside a single builtin call.
//!
//! `guard::tick` used to run only between evaluation steps, so one eager builtin
//! (`(range 0 1000000000)`, `(repeat 1000000000 1)`, `(* "x" 1000000000)`) could allocate
//! until the machine started swapping: measured 5.26GB with a 4GB cap in place, and the
//! REPL could not even be interrupted because nothing checked the interrupt flag either.

use std::process::{Command, Output};

fn run(expr: &str, extra_args: &[&str]) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_clove"));
    command.args(extra_args);
    command.arg("-e").arg(expr);
    command.output().expect("run clove")
}

fn combined(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Small cap so the test trips the guard quickly instead of eating the machine.
const CAP: [&str; 2] = ["--mem-hard", "512M"];

#[test]
fn range_stops_at_the_memory_cap() {
    let output = run("(count (range 0 100000000))", &CAP);
    let text = combined(&output);
    assert!(
        text.contains("Memory limit exceeded"),
        "range must hit the memory guard:\n{text}"
    );
}

#[test]
fn repeat_stops_at_the_memory_cap() {
    let output = run("(count (repeat 100000000 1))", &CAP);
    let text = combined(&output);
    assert!(
        text.contains("Memory limit exceeded"),
        "repeat must hit the memory guard:\n{text}"
    );
}

#[test]
fn string_repeat_is_rejected_before_allocating() {
    // 100GB in one allocation: the guard must refuse it up front rather than try.
    let output = run("(count (* \"x\" 100000000000))", &CAP);
    let text = combined(&output);
    assert!(
        text.contains("Memory limit exceeded"),
        "string repeat must hit the memory guard:\n{text}"
    );
}

#[test]
fn allocations_within_the_cap_still_work() {
    let output = run("(println (count (range 0 200000)))", &CAP);
    let text = combined(&output);
    assert!(
        output.status.success() && text.contains("200000"),
        "allocations below the cap must succeed:\n{text}"
    );
}
