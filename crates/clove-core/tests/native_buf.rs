use clove_core::ast::Value;
use clove_core::eval_source;

#[test]
fn native_i32buf_basic() {
    let out = eval_source(
        "(let [b (native::i32buf-new 2)]\n\
           (native::i32buf-resize! b 3 7)\n\
           (native::i32buf-set! b 1 42)\n\
           (let [x (native::i32buf-get b 1)]\n\
             (native::i32buf-fill! b 9)\n\
             [(native::i32buf-len b)\n\
              x\n\
              (native::i32buf-get b 0)\n\
              (native::i32buf-get b 1)\n\
              (native::i32buf-get b 2)]))",
        None,
    )
    .expect("eval native i32 buf");
    let expected = eval_source("[3 42 9 9 9]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn native_buf_print() {
    let out = eval_source("(pr-str (native::i32buf-new 1))", None).expect("pr-str");
    match out {
        Value::String(text) => assert!(
            text.contains("native/i32buf"),
            "unexpected native buf print: {}",
            text
        ),
        other => panic!("unexpected value: {:?}", other),
    }
}

#[test]
fn native_i32buf_add_wrap_pack() {
    let out = eval_source(
        "(let [xs (native::i32buf-new 3)\n\
               ys (native::i32buf-new 3)\n\
               out (native::i32buf-new 0)\n\
               _ (native::i32buf-resize! xs 3 0)\n\
               _ (native::i32buf-resize! ys 3 0)\n\
               _ (native::i32buf-set! xs 0 10)\n\
               _ (native::i32buf-set! xs 1 20)\n\
               _ (native::i32buf-set! xs 2 30)\n\
               _ (native::i32buf-set! ys 0 0)\n\
               _ (native::i32buf-set! ys 1 0)\n\
               _ (native::i32buf-set! ys 2 20)\n\
               _ (native::i32buf-add! xs [1 2 3] 2)\n\
               _ (native::i32buf-wrap-min! xs 15 99)\n\
               _ (native::i32buf-pack-xy-in-rect! out xs ys -10 40 -10 10)]\n\
           (native::i32buf->vec out))",
        None,
    )
    .expect("eval native buf add/wrap/pack");
    let expected = eval_source("[24 0]", None).expect("expected vector");
    assert_eq!(out, expected);
}
