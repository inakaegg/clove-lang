use clove_core::ast::Vector;
use clove_core::ast::{Key, Value};
use clove_core::eval_source_with_engines;
use clove_core::options::EvalOptions;

#[test]
fn index_inside_expression() {
    let src = "(let [arr (range 10)] (+ (arr 5) 1))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(6));
}

#[test]
fn chained_index_expressions() {
    let src = "(let [arr [[0 1] [2 3]]] ((arr 1) 0))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(2));
}

#[test]
fn literal_head_callable_vector() {
    let src = "(1 (range 10))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(1));
}

#[test]
fn symmetric_indexer_calls() {
    let src = "(let [arr (range 5) s \"hello\"] [(arr -1) (-2 arr) (s -2)])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::Int(4));
            assert_eq!(items[1], Value::Int(3));
            assert_eq!(items[2], Value::String("l".into()));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn range_variants_inclusive_exclusive() {
    let src = "[( (range 8) (1..3) ) ( (range 8) (1...3) ) ((range 8) (..3)) ((range 8) (2..))]";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(
                items[0],
                Value::Vector(Vector::from(vec![
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3)
                ]))
            );
            assert_eq!(
                items[1],
                Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)]))
            );
            assert_eq!(
                items[2],
                Value::Vector(Vector::from(vec![
                    Value::Int(0),
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3)
                ]))
            );
            assert_eq!(
                items[3],
                Value::Vector(Vector::from(vec![
                    Value::Int(2),
                    Value::Int(3),
                    Value::Int(4),
                    Value::Int(5),
                    Value::Int(6),
                    Value::Int(7)
                ]))
            );
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn range_literal_is_range_spec() {
    let src = "(0..10)";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Map(map) => {
            assert_eq!(
                map.get(&Key::Keyword("__range".into())),
                Some(&Value::Bool(true))
            );
        }
        other => panic!("expected range map, got {:?}", other),
    }
}

#[test]
fn range_with_negative_bounds() {
    let src = "((range 10) (-4..-1))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(
        val,
        Value::Vector(Vector::from(vec![
            Value::Int(6),
            Value::Int(7),
            Value::Int(8),
            Value::Int(9)
        ]))
    );
}

#[test]
fn index_vector_argument() {
    let src = "((range 10) (range 3))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(
        val,
        Value::Vector(Vector::from(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(2)
        ]))
    );
}

#[test]
fn range_map_callable_from_head() {
    let src = "(..3 (range 10))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(
        val,
        Value::Vector(Vector::from(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(2),
            Value::Int(3)
        ]))
    );
}

#[test]
fn map_and_set_calls() {
    let src = "(let [m {:key 42} s #{:a :b}] [(m :key) (:key m) (s :b) (:b s)])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::Int(42));
            assert_eq!(items[1], Value::Int(42));
            assert_eq!(items[2], Value::Symbol(":b".into()));
            assert_eq!(items[3], Value::Symbol(":b".into()));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn defn_preserves_set_literals() {
    let src = "(do (defn make-set [] #{:a}) (make-set))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Set(items) => {
            assert_eq!(items.len(), 1);
            assert!(items.contains(&Value::Symbol(":a".into())));
        }
        _ => panic!("expected set"),
    }
}

#[test]
fn map_indexer_syntax() {
    let src = r#"
    (let [config {:tick-ms 16
                  :nested {:inner 99}
                  :handler (fn [] :ok)}
          vec [0 10 20]
          letters "abc"
          tags #{:x :y}]
      [config[:tick-ms]
       config[:missing || 42]
       config[:nested :inner]
       config[:nested :missing || :fallback]
       vec[1]
       vec[5 || 999]
      letters[1]
      letters[10 || :none]
      tags[:x]
      tags[:z || :absent]
       (config[:handler])
       vec[-1]
       vec[-5 || :nope]
       vec[1..2]
       vec[..1]
       letters[-1]
       letters[-3..-1]
       letters[..1]])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::Int(16));
            assert_eq!(items[1], Value::Int(42));
            assert_eq!(items[2], Value::Int(99));
            assert_eq!(items[3], Value::Symbol(":fallback".into()));
            assert_eq!(items[4], Value::Int(10));
            assert_eq!(items[5], Value::Int(999));
            assert_eq!(items[6], Value::String("b".into()));
            assert_eq!(items[7], Value::Symbol(":none".into()));
            assert_eq!(items[8], Value::Symbol(":x".into()));
            assert_eq!(items[9], Value::Symbol(":absent".into()));
            assert_eq!(items[10], Value::Symbol(":ok".into()));
            assert_eq!(items[11], Value::Int(20));
            assert_eq!(items[12], Value::Symbol(":nope".into()));
            assert_eq!(
                items[13],
                Value::Vector(Vector::from(vec![Value::Int(10), Value::Int(20)]))
            );
            assert_eq!(
                items[14],
                Value::Vector(Vector::from(vec![Value::Int(0), Value::Int(10)]))
            );
            assert_eq!(items[15], Value::String("c".into()));
            assert_eq!(items[16], Value::String("abc".into()));
            assert_eq!(items[17], Value::String("ab".into()));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn indexer_range_allows_expr_without_spaces() {
    let src = r#"
    (do
      (use oop-syntax true)
      (let [i 3
            xs (range 10)]
        xs[(- i 2)..i.inc]))"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(
        val,
        Value::Vector(Vector::from(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4)
        ]))
    );
}

#[test]
fn indexer_negative_symbol_prefers_bound_symbol() {
    let src = "(let [xs (range 10) -foo 9 foo 3] [xs[-5..-foo] xs[-5..--foo]])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(
                items[0],
                Value::Vector(Vector::from(vec![
                    Value::Int(5),
                    Value::Int(6),
                    Value::Int(7),
                    Value::Int(8),
                    Value::Int(9)
                ]))
            );
            assert_eq!(items[1], Value::Vector(Vector::new()));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn indexer_negative_symbol_falls_back_to_unary_minus() {
    let src = "(let [xs (range 10) foo 3] xs[-5..-foo])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(
        val,
        Value::Vector(Vector::from(vec![
            Value::Int(5),
            Value::Int(6),
            Value::Int(7)
        ]))
    );
}

#[test]
fn let_binding_type_hint_is_ignored() {
    let src = "(let [x<Int> 10 y<GameConfig> {:tick-ms 100}] x)";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(10));
}

#[test]
fn fn_param_type_hint_is_ignored() {
    let src = "((fn [x<Int> y<Int>] (+ x y)) 3 4)";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(7));
}

#[test]
fn string_range_access() {
    let src = "(let [s \"abcdef\"] [(s 2) (s (1..3))])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::String("c".into()));
            assert_eq!(items[1], Value::String("bcd".into()));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn regex_values_are_callable() {
    let src = r#"
    (let [digits #/^\d+$/
          pair #/(\d+)-(\d+)/]
      [(digits "123")
       (pair "42-07")
       (digits "abc" :fallback)])
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::String("123".into()));
            match &items[1] {
                Value::Vector(matches_vec) => {
                    assert_eq!(
                        matches_vec.clone(),
                        Vector::from(vec![
                            Value::String("42-07".into()),
                            Value::String("42".into()),
                            Value::String("07".into())
                        ])
                    );
                }
                other => panic!("expected regex captures vector, got {:?}", other),
            }
            assert_eq!(items[2], Value::Symbol(":fallback".into()));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn defenum_creates_member_types() {
    let src = "(do (defenum Phases PhaseAlpha PhaseBeta) [(PhaseAlpha {}) (PhaseBeta {})])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], Value::Map(_)));
            assert!(matches!(items[1], Value::Map(_)));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn deftype_allows_empty_schema() {
    let src = "(do (deftype Placeholder {}) (Placeholder {}))";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert!(matches!(val, Value::Map(_)));
}

#[test]
fn deftype_accepts_various_field_specs() {
    let src = r#"
    (do
      (deftype EmptyType)
      (deftype FlatType :x :int :y Int)
      (deftype VectorType [:label :str :count Int])
      (deftype SymbolRef {:inner FlatType})
      [(EmptyType {})
       (FlatType {:x 1 :y 2})
       (VectorType {:label "hi" :count 3})
       (SymbolRef {:inner (FlatType {:x 1 :y 2})})])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 4);
            for item in items {
                assert!(matches!(item, Value::Map(_)));
            }
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn deftype_accepts_symbol_field_names() {
    let src = r#"
    (do
      (deftype PlainFields name String age Int)
      (PlainFields {:name "Rin" :age 4}))"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert!(matches!(val, Value::Map(_)));
}

#[test]
fn use_scope_is_removed() {
    let src = "(use dot-chain :scope user-code)";
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    assert!(err
        .to_string()
        .contains("use-syntax only accepts (use <feature> true|false)"));
}

#[test]
fn zero_field_constructor_accepts_no_args() {
    let src = "(do (defenum States Idle)
                 [(Idle {}) (Idle)])";
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            for item in items {
                assert!(matches!(item, Value::Map(_)));
            }
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn string_supports_regex_lookup() {
    let src = r#"
    (let [s "99-01" digits #/^\d+$/]
      [(s #/(\d+)-(\d+)/)
       (s digits :miss)])
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            match &items[0] {
                Value::Vector(matches_vec) => {
                    assert_eq!(
                        matches_vec.clone(),
                        Vector::from(vec![
                            Value::String("99-01".into()),
                            Value::String("99".into()),
                            Value::String("01".into())
                        ])
                    );
                }
                other => panic!("expected regex capture vector, got {:?}", other),
            }
            assert_eq!(items[1], Value::Symbol(":miss".into()));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn atom_validator_and_watch_behaves() {
    let src = r#"
    (do
      (def log (atom []))
      (def counter (atom 0 {:validator (fn [n] (>= n 0))}))
      (atom-add-watch counter :log (fn [_ _ old new]
                                     (atom-update! log (fn [xs o n] (conj xs [o n])) old new)
                                     nil))
      (atom-set! counter 1)
      (atom-update! counter (fn [value delta] (+ value delta)) 2)
      (def failure (try
                     (do (atom-set! counter -1) :ok)
                     (catch err err)))
      (def removed (atom-remove-watch counter :log))
      [@counter @log failure removed])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 4);
            assert_eq!(items[0], Value::Int(3));
            match &items[1] {
                Value::Vector(entries) => {
                    let observed: Vec<Value> = entries.iter().cloned().collect();
                    let expected = vec![
                        Value::Vector(Vector::from(vec![Value::Int(0), Value::Int(1)])),
                        Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(3)])),
                    ];
                    assert_eq!(observed, expected);
                }
                other => panic!("expected watch log vector, got {:?}", other),
            }
            match &items[2] {
                Value::String(msg) => assert!(
                    msg.contains("validator"),
                    "unexpected validator message: {}",
                    msg
                ),
                other => panic!("expected validator error string, got {:?}", other),
            }
            assert_eq!(items[3], Value::Bool(true));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_constructor_and_predicate_work() {
    let src = r#"
    (ns spec::types::animals_a)

    (deftype Dog {:name :str :age :int})

    [(type (Dog {:name "Pochi" :age 3}))
     (Dog? (Dog {:name "Pochi" :age 3}))
     (Dog? {:name "Fake" :age 2})]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(
                items[0],
                Value::Symbol("spec::types::animals_a::Dog".into())
            );
            assert_eq!(items[1], Value::Bool(true));
            assert_eq!(items[2], Value::Bool(false));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_flat_fields_and_keyword_ctor() {
    let src = r#"
    (ns spec::types::flat)
    (deftype Color :r Int :g Int :b Int :a Int)
    (let [c1 (Color {r: 1, g: 2, b: 3, a: 4})
          c2 (Color :r 1, :g 2, :b 3, :a 4)]
      (= c1 c2))
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Bool(true));
}

#[test]
fn deftype_keyword_ctor_shorthand() {
    let src = r#"
    (ns spec::types::flat_shorthand)
    (deftype W {:a Int :b Int})
    (def a 10)
    (def b 20)
    (let [w1 (W a: 1, b: 2)
          w2 (W :a 1 :b 2)
          w3 (W a:, b:)
          w4 (W :a, :b)]
      [(= w1 w2) (= w3 w4) (= w3 (W {a: 10, b: 20}))])
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::Bool(true));
            assert_eq!(items[1], Value::Bool(true));
            assert_eq!(items[2], Value::Bool(true));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_keyword_ctor_unknown_field_errors() {
    let src = r#"
    (ns spec::types::flat_unknown)
    (deftype Color :r Int :g Int)
    (Color :x 1)
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("unknown field :x"),
        "unexpected error: {}",
        msg
    );
}

#[test]
fn deftype_keyword_ctor_missing_fields_errors() {
    let src = r#"
    (ns spec::types::flat_missing)
    (deftype Color :r Int :g Int)
    (Color :r 1)
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("missing required field :g"),
        "unexpected error: {}",
        msg
    );
}

#[test]
fn deftype_alias_constructor_and_infer_type() {
    let src = r#"
    (ns spec::types::alias_basic)
    (deftype Color :alias [Int Int Int Int])
    (def c (Color [1 2 3 4]))
    [c (infer-type c)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], Value::Vector(_)));
            assert_eq!(items[1], Value::String("Color".into()));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_alias_constructor_rejects_invalid_values() {
    let src = r#"
    (ns spec::types::alias_errors)
    (deftype Color :alias [Int Int Int Int])
    (Color 1)
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("alias Color expects"),
        "unexpected error: {}",
        msg
    );

    let src = r#"
    (ns spec::types::alias_errors_len)
    (deftype Color :alias [Int Int Int Int])
    (Color [1 2 3])
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("alias Color expects"),
        "unexpected error: {}",
        msg
    );
}

#[test]
fn deftype_from_defines_default_instance() {
    let src = r#"
    (ns spec::types::from_basic)
    (deftype Config :from cfg {:screen {:w 1 :h 2}})
    [(= cfg (Config {:screen {:w 1 :h 2}}))
     (infer-type cfg)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Bool(true));
            match &items[1] {
                Value::String(text) => assert!(text.contains("Config")),
                other => panic!("expected string, got {:?}", other),
            }
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_from_def_syntax_defines_default_instance() {
    let src = r#"
    (ns spec::types::from_def)
    (deftype Config (def cfg {:screen {:w 1 :h 2}}))
    [(= cfg (Config {:screen {:w 1 :h 2}}))
     (infer-type cfg)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Bool(true));
            match &items[1] {
                Value::String(text) => assert!(text.contains("Config")),
                other => panic!("expected string, got {:?}", other),
            }
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_from_def_allows_type_annotation() {
    let src = r#"
    (ns spec::types::from_def_typed)
    (use oop-syntax true)
    (deftype Config
      (def initial {:speed 6} : {:speed Int})
      (defn speed [] (get self :speed)))
    [(initial.speed) (Config? initial)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Int(6));
            assert_eq!(items[1], Value::Bool(true));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn deftype_from_infers_alias_field_types() {
    let src = r#"
    (ns spec::types::from_alias)
    (deftype Color :alias [Int Int Int Int])
    (deftype Config :from cfg {:colors {:bg (Color [1 2 3 4])}})
    (describe Config)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Map(map) => {
            let fields = map.get(&Key::Keyword("fields".into())).expect("fields");
            let Value::Map(fields_map) = fields else {
                panic!("expected fields map, got {:?}", fields);
            };
            let colors = fields_map
                .get(&Key::Keyword("colors".into()))
                .expect("colors");
            let Value::Map(colors_map) = colors else {
                panic!("expected colors map, got {:?}", colors);
            };
            let bg = colors_map.get(&Key::Keyword("bg".into())).expect("bg");
            assert_eq!(bg, &Value::Symbol("Color".into()));
        }
        other => panic!("expected map, got {:?}", other),
    }
}

#[test]
fn deftype_from_accepts_map_refs_with_alias_fields() {
    let src = r#"
    (ns spec::types::from_map_refs)
    (deftype Color :alias [Int Int Int Int])
    (deftype Config (def config
      {:pipe   {:gap 170
                :gap-half (int (/ &^:gap 2))
                :gap-margin 70
                :gap-min (+ &^:gap-margin &^:gap-half)}
       :colors {:bg (Color [12 12 16 255])
                :fg (Color [235 235 235 255])}}))
    (type config)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Symbol(name) => assert!(name.contains("Config")),
        other => panic!("expected symbol, got {:?}", other),
    }
}

#[test]
fn defenum_and_match_patterns_work() {
    let src = r#"
    (ns spec::types::animals_b)

    (deftype Dog {:name :str})
    (deftype Cat {:name :str})

    (defenum Pets
      Dog
      Cat)

(defn describe [pet]
  (match pet
    (Dog {:name n}) :as whole (if (Dog? whole) (str n " the dog") "not a dog")
    (Cat {:name n}) :when (= n "Mimi") (str n " the picky cat")
    (Cat {:name n}) (str n " the cat")
    _ "unknown"))

    [(describe (Dog {:name "Pochi"}))
     (describe (Cat {:name "Mimi"}))
     (describe (Cat {:name "Momo"}))
     (describe {:name "Ghost"})]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items[0], Value::String("Pochi the dog".into()));
            assert_eq!(items[1], Value::String("Mimi the picky cat".into()));
            assert_eq!(items[2], Value::String("Momo the cat".into()));
            assert_eq!(items[3], Value::String("unknown".into()));
        }
        other => panic!("expected descriptions vector, got {:?}", other),
    }
}

#[test]
fn enum_variant_unit_value_and_match() {
    let src = r#"
    (ns spec::enum::unit)
    (deftype Noop {})
    (defenum Action Noop)
    (def v (Action::Noop {}))
    [v
     (match v
       (Action::Noop {}) 1
       _ 0)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], Value::Map(_)));
            assert_eq!(items[1], Value::Int(1));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn enum_variant_payload_constructor() {
    let src = r#"
    (ns spec::enum::payload)
    (deftype Running {:speed Int})
    (defenum Mode Running)
    (Mode::Running {:speed 3})
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert!(matches!(val, Value::Map(_)));
}

#[test]
fn defenum_inline_payload_unit_variant() {
    let src = r#"
    (ns spec::enum::inline_unit)
    (defenum Action Noop Quit)
    (def a (Noop {}))
    (match a
      (Noop {}) 1
      _ 0)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(1));
}

#[test]
fn defenum_inline_payload_with_fields() {
    let src = r#"
    (ns spec::enum::inline_payload)
    (defenum Mode
      Running {:score Int}
      GameOver {:score Int})
    (def m (Running {score: 10}))
    (match m
      (Running {score: s}) s
      _ 0)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(10));
}

#[test]
fn defenum_grouping_qualified_only_match() {
    let src = r#"
    (ns spec::enum::qualified_only)
    (defenum Util :qualified-only true
      RandInt {:seed Int :value Int})
    (def x (Util::RandInt {seed: 1 value: 2}))
    (match x
      (Util::RandInt {seed: s value: v}) (+ s v)
      _ 0)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Int(3));
}

#[test]
fn defenum_grouping_qualified_only_unqualified_is_unbound() {
    let src = r#"
    (ns spec::enum::qualified_only_unbound)
    (defenum Util :qualified-only true
      RandInt {:seed Int :value Int})
    RandInt
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("Unbound symbol"), "unexpected error: {}", msg);
}

#[test]
fn defenum_grouping_qualified_only_predicate() {
    let src = r#"
    (ns spec::enum::qualified_only_pred)
    (defenum Util :qualified-only true
      RandInt {:seed Int :value Int})
    (def x (Util::RandInt {seed: 1 value: 2}))
    [(Util::RandInt? x) (Util::RandInt? nil)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Bool(true));
            assert_eq!(items[1], Value::Bool(false));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn defenum_grouping_qualified_and_unqualified_match() {
    let src = r#"
    (ns spec::enum::qualified_compat)
    (defenum E
      A
      B)
    (def a (A {}))
    (def b (E::B {}))
    [(match b (B {}) 1 _ 0)
     (match b (E::B {}) 1 _ 0)]
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Int(1));
            assert_eq!(items[1], Value::Int(1));
        }
        _ => panic!("expected vector"),
    }
}

#[test]
fn enum_variant_ambiguity_errors() {
    let src = r#"
    (do
      (def Action::Noop :ns)
      (deftype Noop {})
      (defenum Action Noop)
      Action::Noop)
    "#;
    let err = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("ambiguous qualified symbol"),
        "unexpected error: {}",
        msg
    );
}

#[test]
fn describe_builtin_returns_metadata() {
    let src = r#"
    (ns spec::types::animals_c)
    (deftype Dog {:name :str})
    (deftype Cat {:name :str :lives :int})
    (describe Dog)
    (describe (type-of (Cat {:name "Momo" :lives 8})))
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Map(map) => {
            assert_eq!(
                map.get(&Key::Keyword("kind".into())),
                Some(&Value::Symbol(":product".into()))
            );
            match map.get(&Key::Keyword("fields".into())) {
                Some(Value::Map(fields)) => {
                    assert_eq!(
                        fields.get(&Key::Keyword("name".into())),
                        Some(&Value::Symbol("Str".into()))
                    );
                }
                other => panic!("expected fields map, got {:?}", other),
            }
        }
        other => panic!("expected metadata map, got {:?}", other),
    }
}

#[test]
fn current_ns_returns_namespace_symbol() {
    let src = r#"
    (ns spec::types::current_ns)
    (current-ns)
    "#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    assert_eq!(val, Value::Symbol("spec::types::current_ns".into()));
}

#[test]
fn chan_select_and_timeout_flow() {
    let src = r#"
    (do
      (def c (chan 1))
      (chan-put! c :first)
      (def first (chan-take! c))
      (def default (select [c] {:default :idle}))
      (def timed (select [(timeout 5)]))
      (spawn (fn [] (chan-put! c :async-value)))
      (def taken (select [c]))
      (def ack (chan 1))
      (def send-result (select [[:put ack :ack-now]]))
      (def ack-val (chan-take! ack))
      (chan-close! c)
      (def closed? (chan-closed? c))
      (def drained (chan-take! c))
      [first default timed taken send-result ack-val closed? drained])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 8);
            assert_eq!(items[0], Value::Symbol(":first".into()));
            match &items[1] {
                Value::Vector(pair) => {
                    assert_eq!(pair[0], Value::Symbol(":idle".into()));
                    assert_eq!(pair[1], Value::Nil);
                }
                other => panic!("expected select default vector, got {:?}", other),
            }
            match &items[2] {
                Value::Vector(pair) => {
                    assert_eq!(pair[0], Value::Nil);
                }
                other => panic!("expected timeout pair, got {:?}", other),
            }
            match &items[3] {
                Value::Vector(pair) => assert_eq!(pair[0], Value::Symbol(":async-value".into())),
                other => panic!("expected select vector, got {:?}", other),
            }
            match &items[4] {
                Value::Vector(pair) => {
                    assert_eq!(pair[0], Value::Bool(true));
                    assert!(matches!(pair[1], Value::Chan(_)));
                }
                other => panic!("expected send-result vector, got {:?}", other),
            }
            assert_eq!(items[5], Value::Symbol(":ack-now".into()));
            assert_eq!(items[6], Value::Bool(true));
            assert_eq!(items[7], Value::Nil);
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn select_prefers_ready_case_even_with_default() {
    let src = r#"
    (do
      (def c (chan 1))
      (chan-put! c :ready)
      (select [c] {:default :idle}))"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Symbol(":ready".into()));
            assert!(matches!(items[1], Value::Chan(_)));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn promise_future_and_agent_primitives() {
    let src = r#"
    (do
      (def manual (promise))
      (spawn (fn [] (promise-deliver! manual {:status :ok})))
      (def fall (future-all [(future (fn [] 1))
                             (future (fn [] 2))]))
      (def failing (future (fn [] (throw "boom"))))
      (def handled (promise-catch failing (fn [err] :handled)))
      (def ledger (agent {:count 0}))
      (agent-send! ledger (fn [state delta]
                            (let [count (get state :count 0)]
                              (assoc state :count (+ count delta))))
                   2)
      (agent-send-io! ledger (fn [state]
                                (chan-take! (timeout 1))
                                (assoc state :io true)))
      (agent-await ledger)
      [(future-deref fall)
       (promise-deref manual)
       (promise-deref handled)
       (agent-deref ledger)])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 4);
            assert_eq!(
                items[0],
                Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)]))
            );
            match &items[1] {
                Value::Map(map) => {
                    assert_eq!(
                        map.get(&Key::Keyword("status".into())),
                        Some(&Value::Symbol(":ok".into()))
                    );
                }
                other => panic!("expected status map, got {:?}", other),
            }
            assert_eq!(items[2], Value::Symbol(":handled".into()));
            match &items[3] {
                Value::Map(map) => {
                    assert_eq!(map.get(&Key::Keyword("count".into())), Some(&Value::Int(2)));
                    assert_eq!(
                        map.get(&Key::Keyword("io".into())),
                        Some(&Value::Bool(true))
                    );
                }
                other => panic!("expected ledger map, got {:?}", other),
            }
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn add_watch_supports_async_values() {
    let src = r#"
    (do
      (def events (atom []))
      (def p (promise))
      (add-watch p :promise (fn [k ref old new]
                              (atom-update! events (fn [xs] (conj xs [k (= ref p) old new])))))
      (promise-deliver! p :ok)
      (def ag (agent {:n 0}))
      (add-watch ag :agent (fn [k _ old new]
                             (atom-update! events (fn [xs] (conj xs [k old new])))))
      (agent-send! ag (fn [state] (assoc state :n 1)))
      (agent-await ag)
      @events)"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 2);
            match &items[0] {
                Value::Vector(event) => {
                    assert_eq!(event.len(), 4);
                    assert_eq!(event[0], Value::Symbol(":promise".into()));
                    assert_eq!(event[1], Value::Bool(true));
                    assert_eq!(event[2], Value::Nil);
                    match &event[3] {
                        Value::Map(map) => {
                            assert_eq!(
                                map.get(&Key::Keyword("status".into())),
                                Some(&Value::Symbol(":fulfilled".into()))
                            );
                            assert_eq!(
                                map.get(&Key::Keyword("value".into())),
                                Some(&Value::Symbol(":ok".into()))
                            );
                        }
                        other => panic!("expected status map from promise watch, got {:?}", other),
                    }
                }
                other => panic!("expected first watch entry vector, got {:?}", other),
            }
            match &items[1] {
                Value::Vector(event) => {
                    assert_eq!(event.len(), 3);
                    assert_eq!(event[0], Value::Symbol(":agent".into()));
                    match (&event[1], &event[2]) {
                        (Value::Map(old_map), Value::Map(new_map)) => {
                            assert_eq!(
                                old_map.get(&Key::Keyword("n".into())),
                                Some(&Value::Int(0))
                            );
                            assert_eq!(
                                new_map.get(&Key::Keyword("n".into())),
                                Some(&Value::Int(1))
                            );
                        }
                        other => panic!("expected agent state maps, got {:?}", other),
                    }
                }
                other => panic!("expected second watch entry vector, got {:?}", other),
            }
        }
        other => panic!("expected vector of watch events, got {:?}", other),
    }
}

#[test]
fn done_and_promise_error_cover_async_variants() {
    let src = r#"
    (do
      (def pending (promise))
      (def slow-fut (future (fn [] (chan-take! (timeout 5)) :future)))
      (def slow-task (spawn (fn [] (chan-take! (timeout 5)) :task)))
      (def slow-agent (agent 0))
      (agent-send! slow-agent (fn [s] (chan-take! (timeout 5)) (+ s 1)))
      (def fail (future (fn [] (throw "fail!"))))
      (let [initial [(done? pending) (done? slow-fut) (done? slow-task) (done? slow-agent)]]
        (promise-deliver! pending :ok)
        (future-deref slow-fut)
        (task-deref slow-task)
        (agent-await slow-agent)
        (chan-take! (timeout 5))
        (let [final [(done? pending) (done? slow-fut) (done? slow-task) (done? slow-agent)]
              errors [(promise-error pending)
                      (promise-error slow-fut)
                      (promise-error slow-task)
                      (promise-error fail)]]
          [initial final errors])))"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(top) => {
            assert_eq!(top.len(), 3);
            match &top[0] {
                Value::Vector(initial) => {
                    assert_eq!(initial.len(), 4);
                    assert_eq!(initial[0], Value::Bool(false));
                }
                other => panic!("expected initial status vector, got {:?}", other),
            }
            match &top[1] {
                Value::Vector(final_status) => {
                    assert_eq!(
                        final_status,
                        &Vector::from(vec![
                            Value::Bool(true),
                            Value::Bool(true),
                            Value::Bool(true),
                            Value::Bool(true)
                        ])
                    );
                }
                other => panic!("expected final status vector, got {:?}", other),
            }
            match &top[2] {
                Value::Vector(errors) => {
                    assert_eq!(errors.len(), 4);
                    assert_eq!(errors[0], Value::Nil);
                    assert_eq!(errors[1], Value::Nil);
                    assert_eq!(errors[2], Value::Nil);
                    match &errors[3] {
                        Value::String(text) => {
                            assert!(
                                text.contains("fail"),
                                "expected failure string, got {}",
                                text
                            );
                        }
                        other => {
                            panic!("expected failure text for failing future, got {:?}", other)
                        }
                    }
                }
                other => panic!("expected error snapshot vector, got {:?}", other),
            }
        }
        other => panic!("expected grouped result vector, got {:?}", other),
    }
}

#[test]
fn core_async_namespace_reexports_async_primitives() {
    let src = r#"
    (require async)
    (do
      (def manual (async::promise))
      (async::promise-deliver! manual :manual-ok)
      (def mirrored (async::promise))
      (async::promise-deliver! mirrored :direct-ok)
      (def fut (async::future (fn [] 10)))
      (def fut2 (async::future-then fut (fn [value] (+ value 5))))
      (def ledger (async::agent {:total 0}))
      (async::agent-send! ledger (fn [state delta]
                                  (let [total (get state :total 0)]
                                    (assoc state :total (+ total delta))))
                           7)
      (async::agent-await ledger)
      (def ch (async::chan 1))
      (async::go (fn [] (async::chan-put! ch :from-core-async)))
      (def taken (async::<!! ch))
      (def mirrored-core (core::promise))
      (core::promise-deliver! mirrored-core :core-export)
      [(async::promise-deref manual)
       (async::promise-deref mirrored)
       (async::future-deref fut2)
       (async::agent-deref ledger)
       taken
       (core::promise-deref mirrored-core)])"#;
    let val = eval_source_with_engines(src, EvalOptions::default(), &[]).unwrap();
    match val {
        Value::Vector(items) => {
            assert_eq!(items.len(), 6);
            assert_eq!(items[0], Value::Symbol(":manual-ok".into()));
            assert_eq!(items[1], Value::Symbol(":direct-ok".into()));
            assert_eq!(items[2], Value::Int(15));
            match &items[3] {
                Value::Map(map) => {
                    assert_eq!(map.get(&Key::Keyword("total".into())), Some(&Value::Int(7)));
                }
                other => panic!("expected ledger map, got {:?}", other),
            }
            assert_eq!(items[4], Value::Symbol(":from-core-async".into()));
            assert_eq!(items[5], Value::Symbol(":core-export".into()));
        }
        other => panic!("expected vector, got {:?}", other),
    }
}

#[test]
fn reader_quote_macro() {
    let symbol =
        eval_source_with_engines("'sample", EvalOptions::default(), &[]).expect("quote symbol");
    assert_eq!(symbol, Value::Symbol("sample".into()));

    let list =
        eval_source_with_engines("'(1 2 3)", EvalOptions::default(), &[]).expect("quote list");
    match list {
        Value::List(items) => {
            assert_eq!(
                items,
                Vector::from(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
            );
        }
        other => panic!("expected quoted list, got {:?}", other),
    }
}
