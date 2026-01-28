use std::io::Cursor;

use clove_core::{env::Env, eval::Evaluator, repl::debug_repl_with_io};

#[test]
fn debug_repl_set_and_eval() {
    let evaluator = Evaluator::new(Env::default());
    let env = evaluator.global_env();
    // initial env has builtins but no x
    let input = b":set x 10\n(+ x 1)\n:q\n";
    let mut reader = Cursor::new(&input[..]);
    let mut output = Vec::new();
    let result = debug_repl_with_io(&evaluator, env.clone(), &mut reader, &mut output).unwrap();
    assert_eq!(result.to_string(), "nil");
    assert_eq!(env.read().unwrap().get("x").unwrap().to_string(), "10");
    let out_str = String::from_utf8(output).unwrap();
    assert!(out_str.contains("=> 11"));
}
