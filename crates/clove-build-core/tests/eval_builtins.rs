use clove_build_core::eval::run_str;
use clove_build_core::value::Value;

#[test]
fn comp_uses_comp_call() {
    let value = run_str("((comp inc inc) 1)").expect("comp should evaluate");
    assert_eq!(value, Value::Int(3));
}

#[test]
fn juxt_uses_juxt_call() {
    let value = run_str("((juxt inc dec) 5)").expect("juxt should evaluate");
    assert_eq!(value, Value::vec(vec![Value::Int(6), Value::Int(4)]));
}

#[test]
fn run_bang_invokes_each_item_and_returns_nil() {
    let value = run_str(
        "(def total 0)
         (defn add-total [x] (set! total (+ total x)))
         (def result (run! add-total [1 2 3]))
         [result total]",
    )
    .expect("run! should evaluate");
    assert_eq!(value, Value::vec(vec![Value::Nil, Value::Int(6)]));
}

#[test]
fn puts_alias_resolves() {
    let value = run_str("(puts \"alias\")").expect("puts should evaluate");
    assert_eq!(value, Value::Nil);
}

#[test]
fn index_of_uses_char_indices() {
    // clove-core / Cバックエンド と同じ文字単位の添字であること。
    // ここは以前から文字単位だが、3実装が揃っていることを固定しておく。
    let value = run_str(
        "[(index-of \"aéb\" \"b\")
          (last-index-of \"aéb\" \"b\")
          (subs \"aéb\" (index-of \"aéb\" \"b\"))
          (index-of \"aéba\" \"a\" 1)
          (last-index-of \"banana\" \"na\" 4)]",
    )
    .expect("index-of should evaluate");
    assert_eq!(
        value,
        Value::vec(vec![
            Value::Int(2),
            Value::Int(2),
            Value::Str("b".to_string()),
            Value::Int(3),
            Value::Int(2),
        ])
    );
}
