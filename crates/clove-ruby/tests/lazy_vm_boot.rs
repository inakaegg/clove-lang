//! The embedded Ruby VM must not boot until a foreign block actually runs.
//!
//! Booting it eagerly costs ~50ms and RSS on every `clove` start, and Ruby installs its
//! own SIGSEGV handler, so an unrelated Clove stack overflow used to be reported as a
//! `ruby ... [BUG] Segmentation fault` crash dump.
//!
//! `harness = false` so this runs on the process main thread: the VM can only be booted
//! by the thread that created the engine. The boot state is read through
//! `engine::vm_booted()` rather than `magnus::Ruby::get()`, because magnus caches
//! `NonRubyThread` per thread and an early probe would poison the main thread.

use clove_core::ast::Value;
use clove_core::foreign::ForeignEngine;
use clove_core::options::EvalOptions;
use clove_ruby::engine::{vm_booted, RubyEngine};

fn main() {
    creating_the_engine_does_not_boot_ruby();
    running_a_script_without_foreign_blocks_does_not_boot_ruby();
    evaluating_a_foreign_block_boots_ruby();
}

fn creating_the_engine_does_not_boot_ruby() {
    let engine = RubyEngine::new();
    assert_eq!(engine.tag(), "rb");
    assert!(
        !vm_booted(),
        "creating RubyEngine must not boot the Ruby VM"
    );
}

fn running_a_script_without_foreign_blocks_does_not_boot_ruby() {
    let engines = clove_ruby::engines();
    let value = clove_core::eval_source_with_engines("(+ 1 2)", EvalOptions::default(), &engines)
        .expect("plain arithmetic should evaluate");
    assert_eq!(value, Value::Int(3));
    assert!(
        !vm_booted(),
        "a script without foreign blocks must not boot the Ruby VM"
    );
}

fn evaluating_a_foreign_block_boots_ruby() {
    let engines = clove_ruby::engines();
    let value =
        clove_core::eval_source_with_engines("$rb{1 + 2}", EvalOptions::default(), &engines)
            .expect("ruby block should evaluate");
    assert_eq!(value, Value::Int(3));
    assert!(vm_booted(), "evaluating a ruby block must boot the Ruby VM");
}
