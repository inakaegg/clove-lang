//! Native stack budget for the tree-walking evaluator.
//!
//! One Clove call costs several kilobytes of native stack (`eval` → `eval_list` →
//! `try_eval_special_form` → … → `invoke_lambda`), so recursion that looks modest in
//! Clove can exhaust the thread stack. Running out is a `SIGSEGV`, which cannot be
//! reported as a language error, so the evaluator instead checks how much stack it has
//! already consumed and stops with [`CloveError`] while there is still room to unwind.
//!
//! The check is address-based rather than a depth counter: how much stack a single Clove
//! call needs depends on the shape of the expression it evaluates, so no fixed depth
//! limit can be both safe and generous.
//!
//! Threads that never call [`configure_thread`] are not limited, which keeps embedders
//! and existing tests on their previous behavior.

use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::Span;
use crate::error::CloveError;

thread_local! {
    /// Stack address recorded when this thread was configured (stacks grow down, so this
    /// is the high-water mark). `0` means "not configured".
    static ANCHOR: Cell<usize> = const { Cell::new(0) };
    static BUDGET: Cell<usize> = const { Cell::new(0) };
}

/// Stack reserved for the main evaluation thread.
///
/// The reservation is virtual: only pages actually touched count towards RSS, so a large
/// default costs nothing until deep recursion needs it.
pub const DEFAULT_THREAD_STACK: usize = 512 * 1024 * 1024;

/// Stack reserved for evaluation threads started by `spawn` / tasks.
///
/// Smaller than the main thread because a program can start many of them, but still large
/// enough that ordinary recursion inside a task works.
pub const DEFAULT_TASK_STACK: usize = 64 * 1024 * 1024;

/// Reserve left unbudgeted so the deepest single Clove call, error construction, and
/// unwinding all fit after the check trips.
fn margin(stack_bytes: usize) -> usize {
    (stack_bytes / 8).min(32 * 1024 * 1024)
}

static TASK_STACK: AtomicUsize = AtomicUsize::new(DEFAULT_TASK_STACK);

/// Stack size for evaluation threads started by `spawn` / tasks.
pub fn task_stack_size() -> usize {
    TASK_STACK.load(Ordering::Relaxed)
}

pub fn set_task_stack_size(stack_bytes: usize) {
    TASK_STACK.store(stack_bytes, Ordering::Relaxed);
}

/// Record this thread's stack anchor and how much of `stack_bytes` the evaluator may use.
///
/// Call this at the top of an evaluation thread — the anchor is the caller's stack
/// address, so calling it from a deep frame shrinks the effective budget.
pub fn configure_thread(stack_bytes: usize) {
    ANCHOR.set(stack_ptr());
    BUDGET.set(stack_bytes.saturating_sub(margin(stack_bytes)));
}

/// Bytes of the budget still available, or `None` on an unconfigured thread.
pub fn remaining() -> Option<usize> {
    let anchor = ANCHOR.get();
    if anchor == 0 {
        return None;
    }
    Some(BUDGET.get().saturating_sub(used(anchor)))
}

/// Fail if the evaluator has consumed its stack budget.
pub fn check(span: Option<Span>) -> Result<(), CloveError> {
    let anchor = ANCHOR.get();
    if anchor == 0 {
        return Ok(());
    }
    let budget = BUDGET.get();
    if used(anchor) < budget {
        return Ok(());
    }
    let mut err = CloveError::guard(message(budget));
    if let Some(span) = span {
        err = err.with_span(span);
    }
    Err(err)
}

fn message(budget: usize) -> String {
    format!(
        "Recursion too deep (native stack budget {} exhausted)\nHints:\n  - rewrite the recursion with (loop ... (recur ...)) or a reduce over the collection\n  - raise the budget with --stack SIZE (e.g. --stack 1G)",
        human_bytes(budget)
    )
}

fn used(anchor: usize) -> usize {
    anchor.saturating_sub(stack_ptr())
}

/// Address of a local in the caller's frame. `inline(never)` plus `black_box` so the
/// local cannot be optimized into a register.
#[inline(never)]
fn stack_ptr() -> usize {
    let anchor = 0u8;
    std::hint::black_box(&anchor) as *const u8 as usize
}

fn human_bytes(bytes: usize) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = 1024.0 * 1024.0;
    const GB: f64 = 1024.0 * 1024.0 * 1024.0;
    let b = bytes as f64;
    if b >= GB {
        format!("{:.2}GB", b / GB)
    } else if b >= MB {
        format!("{:.2}MB", b / MB)
    } else {
        format!("{:.2}KB", b / KB)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unconfigured_threads_are_not_limited() {
        // Runs on a fresh libtest thread, so nothing configured it.
        assert!(remaining().is_none());
        assert!(check(None).is_ok());
    }

    #[test]
    fn budget_shrinks_as_the_stack_grows() {
        fn recurse(depth: usize, first: usize) -> usize {
            let left = remaining().expect("configured");
            if depth == 0 {
                assert!(left < first, "deeper frames must report less headroom");
                return left;
            }
            recurse(depth - 1, first)
        }

        std::thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(|| {
                configure_thread(2 * 1024 * 1024);
                let first = remaining().expect("configured");
                recurse(64, first);
            })
            .expect("spawn")
            .join()
            .expect("join");
    }

    #[test]
    fn exhausting_the_budget_reports_an_error() {
        std::thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(|| {
                // A budget of zero means every frame is already over the line.
                configure_thread(0);
                let err = check(None).expect_err("must report exhaustion");
                assert!(err.to_string().contains("Recursion too deep"));
            })
            .expect("spawn")
            .join()
            .expect("join");
    }
}
