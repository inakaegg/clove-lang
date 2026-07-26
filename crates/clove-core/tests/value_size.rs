//! `Value` sits in every collection element, so its size is a memory multiplier.
//!
//! Measured on 2026-07-25: a 1M-element vector of ints cost 136MB RSS (~88B per element)
//! against 28MB for the same list in Ruby, and the project's own 2M-element benchmark
//! peaked at 979MB. The enum was as large as its largest variant, not as large as the
//! values people actually store.
//!
//! `Result<Value, CloveError>` matters for the same reason: the evaluator returns it from
//! every recursive step, so its size shows up as native stack usage per Clove call.

use clove_core::ast::Value;
use clove_core::error::CloveError;
use std::mem::size_of;

/// Reached by boxing the payloads that were wider than a couple of words. What is left at
/// the top is the 32-byte persistent-collection variants (`Map`, `SortedMap`, `SortedSet`,
/// `Compose`, `NativeBuf`) plus the discriminant; boxing those would add an allocation to
/// ordinary map and set operations, so they stay inline.
const MAX_VALUE_SIZE: usize = 40;

/// The evaluator's return type, which every recursive step moves. One cache line.
const MAX_EVAL_RESULT_SIZE: usize = 64;

#[test]
fn value_stays_small() {
    let actual = size_of::<Value>();
    assert!(
        actual <= MAX_VALUE_SIZE,
        "size_of::<Value>() is {actual} bytes (limit {MAX_VALUE_SIZE}); a fat variant is \
         inflating every collection element"
    );
}

#[test]
fn eval_result_stays_small() {
    let actual = size_of::<Result<Value, CloveError>>();
    assert!(
        actual <= MAX_EVAL_RESULT_SIZE,
        "size_of::<Result<Value, CloveError>>() is {actual} bytes (limit \
         {MAX_EVAL_RESULT_SIZE}); every evaluator frame moves this much"
    );
}
