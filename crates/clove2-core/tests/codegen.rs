use clove2_core::codegen::emit_rust_program;
use clove2_core::reader::read_all;
use clove2_core::syntax::parse_forms;
use clove2_core::use_directive::MutMode;

#[test]
fn codegen_reduce_is_typed() {
    let forms =
        read_all("(def xs: [Int] (range 0 10))\n(def total: Int (reduce + 0 xs))\n(println total)")
            .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let out = emit_rust_program(&ast, MutMode::Mut).unwrap();
    assert!(!out.contains("Value::"));
    assert!(out.contains("acc_"));
    assert!(out.contains("for value_"));
}

#[test]
fn codegen_every_contains_are_typed() {
    let forms = read_all(
        "(def xs: [Int] (range 0 10))\n(def ok: Bool (every? even? xs))\n(def ok2: Bool (not-every? even? xs))\n(def ok3: Bool (contains? xs 2))\n(println ok)",
    )
    .unwrap();
    let ast = parse_forms(&forms).unwrap();
    let out = emit_rust_program(&ast, MutMode::Mut).unwrap();
    assert!(!out.contains("Value::"));
    assert!(out.contains("return false"));
    assert!(out.contains("contains_key") || out.contains("len()"));
}
