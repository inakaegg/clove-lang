use clove_core::{
    ast::{Key, Value},
    env,
    error::CloveError,
    eval_source_with_engines,
    foreign::ForeignEngine,
    options::EvalOptions,
};

fn eval_with_python(src: &str) -> Result<Value, CloveError> {
    eval_source_with_engines(src, EvalOptions::default(), &clove_python::engines())
}

#[test]
fn python_value_roundtrip() {
    assert!(matches!(eval_with_python("$py{None}").unwrap(), Value::Nil));
    assert_eq!(eval_with_python("$py{True}").unwrap(), Value::Bool(true));
    assert_eq!(eval_with_python("$py{10}").unwrap(), Value::Int(10));
    assert_eq!(eval_with_python("$py{1.25}").unwrap(), Value::Float(1.25));
    match eval_with_python("$py{[1, 2, 3]}").expect("py vector") {
        Value::Vector(v) => assert_eq!(v, vec![Value::Int(1), Value::Int(2), Value::Int(3)].into()),
        other => panic!("expected vector, got {other:?}"),
    }

    match eval_with_python("$py{{'a': 1, 'b': 2}}").expect("py map") {
        Value::Map(m) => {
            assert_eq!(m.get(&Key::String("a".into())), Some(&Value::Int(1)));
            assert_eq!(m.get(&Key::String("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {other:?}"),
    }

    match eval_with_python("$py{{1, 2, 3}}").expect("py set") {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
        }
        other => panic!("expected set, got {other:?}"),
    }

    // keywords/symbols are stringified when passed to Python (leading ':' is preserved).
    assert_eq!(
        eval_with_python("(let [x :foo] $py{x})").expect("py symbol"),
        Value::String(":foo".into())
    );

    // Also verify strings via direct engine call.
    let engine = clove_python::engine::PythonEngine::new();
    let env = env::new_ref(env::Env::default());
    let val = engine
        .eval_block(
            "'hi'",
            env,
            Some(clove_core::ast::Span {
                line: 1,
                col: 1,
                index: 0,
            }),
        )
        .unwrap();
    assert_eq!(val, Value::String("hi".into()));
}
