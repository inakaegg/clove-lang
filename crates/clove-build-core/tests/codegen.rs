use std::path::PathBuf;

use clove_build_backend_c::emit_c_ir;
use clove_build_front::{parse_typed_ir_source, SourceFile};
use clove_build_runtime_c::RuntimeConfig;

fn src(text: &str) -> SourceFile {
    SourceFile {
        path: PathBuf::from("codegen_test.clv"),
        text: text.to_string(),
    }
}

#[test]
fn codegen_reduce_is_typed_c_backend() {
    let program = parse_typed_ir_source(&src(
        "(def xs: [Int] (range 0 10))\n(def total: Int (reduce + 0 xs))\n(println total)",
    ))
    .expect("parse should succeed");
    let out = emit_c_ir(&program, &RuntimeConfig::default())
        .expect("C emit should succeed")
        .source;
    assert!(!out.contains("Value::"));
    assert!(out.contains("clv_range_i64"));
    assert!(out.contains("clv_reduce_i64"));
}

#[test]
fn codegen_every_contains_are_typed_c_backend() {
    let program = parse_typed_ir_source(&src(
        "(def xs: [Int] (range 0 10))\n(def ok: Bool (every? even? xs))\n(def ok2: Bool (not-every? even? xs))\n(def ok3: Bool (contains? xs 2))\n(println ok)",
    ))
    .expect("parse should succeed");
    let out = emit_c_ir(&program, &RuntimeConfig::default())
        .expect("C emit should succeed")
        .source;
    assert!(!out.contains("Value::"));
    assert!(out.contains("clv_every_i64"));
    assert!(out.contains(".len == 0") || out.contains("clv_map_ki64_contains("));
}
