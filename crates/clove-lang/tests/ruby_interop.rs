use clove_core::{
    ast::{Key, Value},
    error::CloveError,
    eval_source_with_engines,
    options::EvalOptions,
};
use clove_lang::default_engines;

fn eval_with_ruby(src: &str) -> Value {
    eval_source_with_engines(src, EvalOptions::default(), &clove_ruby::engines()).unwrap()
}

fn main() {
    ruby_interop_all();
    ruby_foreign_callable_can_be_passed_to_map();
    tagless_foreign_requires_default_language();
}

fn ruby_interop_all() {
    // Round-trip values
    assert!(matches!(eval_with_ruby("$rb{nil}"), Value::Nil));
    assert_eq!(eval_with_ruby("$rb{true}"), Value::Bool(true));
    assert_eq!(eval_with_ruby("$rb{1}"), Value::Int(1));
    assert_eq!(eval_with_ruby("$rb{1.5}"), Value::Float(1.5));
    assert_eq!(eval_with_ruby("$rb{\"hi\"}"), Value::String("hi".into()));

    match eval_with_ruby("$rb{[1, 2, 3]}") {
        Value::Vector(v) => assert_eq!(v, vec![Value::Int(1), Value::Int(2), Value::Int(3)].into()),
        other => panic!("expected vector, got {other:?}"),
    }

    match eval_with_ruby("$rb{{ a: 1, b: 2 }}") {
        Value::Map(m) => {
            assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1)));
            assert_eq!(m.get(&Key::Keyword("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {other:?}"),
    }

    match eval_with_ruby("$rb{:foo}") {
        Value::Symbol(s) => assert_eq!(s, "foo"),
        other => panic!("expected symbol, got {other:?}"),
    }

    // Basic Ruby behavior
    let eval_default = |src: &str| {
        eval_source_with_engines(src, EvalOptions::default(), &default_engines()).unwrap()
    };

    let out = eval_default("(let [x 10] $rb{x + 5})");
    assert_eq!(out.to_string(), "15");

    let out = eval_default("$rb{1 + 2}");
    assert_eq!(out.to_string(), "3");

    let opts = EvalOptions::default();
    let out = eval_source_with_engines("$rb{'abc'.upcase}", opts, &clove_ruby::engines()).unwrap();
    assert_eq!(out.to_string(), "\"ABC\"");

    let out = eval_default("$rb{1.5}");
    assert_eq!(out.to_string(), "1.5");
}

fn ruby_foreign_callable_can_be_passed_to_map() {
    let src = r#"
${class Foo
  def self.inc(x)
    x + 1
  end
end}
(map $Foo.inc [1 2 3])
"#;
    let mut opts = EvalOptions::default();
    opts.source_name = Some("sample.rb.clv".into());
    let out = eval_source_with_engines(src, opts, &clove_ruby::engines()).unwrap();
    match out {
        Value::Vector(v) => {
            assert_eq!(v, vec![Value::Int(2), Value::Int(3), Value::Int(4)].into());
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

fn tagless_foreign_requires_default_language() {
    let src = "$Foo.bar";
    let err = eval_source_with_engines(src, EvalOptions::default(), &clove_ruby::engines())
        .expect_err("tagless foreign should error without default language");
    match err {
        CloveError::Other(data) => {
            assert!(
                data.message
                    .contains("foreign callable requires explicit tag"),
                "unexpected message: {}",
                data.message
            );
        }
        other => panic!("expected Other error, got {:?}", other),
    }
}
