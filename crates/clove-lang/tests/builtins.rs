use clove_core::{
    ast::{Key, Value},
    error::CloveError,
    eval_source_with_engines,
    options::EvalOptions,
};
use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

fn eval(src: &str) -> Value {
    eval_source_with_engines(src, EvalOptions::default(), &clove_python::engines()).unwrap()
}

fn eval_err(src: &str) -> CloveError {
    eval_source_with_engines(src, EvalOptions::default(), &clove_python::engines())
        .expect_err("expected error")
}

#[test]
fn arithmetic_and_equality() {
    assert_eq!(eval("(+ 1 2 3)").to_string(), "6");
    assert_eq!(eval("(- 10 3 2)").to_string(), "5");
    assert_eq!(eval("(* 2 3 4)").to_string(), "24");
    assert_eq!(eval("(/ 20 2 2)").to_string(), "5");
    assert_eq!(eval("(= 3 3)").to_string(), "true");
    assert_eq!(eval("(= \"a\" \"b\")").to_string(), "false");
    assert_eq!(eval("(inc 9)").to_string(), "10");
}

#[test]
fn map_get_assoc_reduce() {
    let out = eval("(assoc {:a 1} :b 2)");
    assert!(out.to_string().contains(":a 1"));
    assert!(out.to_string().contains(":b 2"));

    assert_eq!(eval("(get {:a 1} :a)").to_string(), "1");
    assert_eq!(eval("(get {:a 1} :missing 42)").to_string(), "42");

    // reduce without seed starts from first element
    assert_eq!(
        eval("(reduce (fn [acc x] (- acc x)) [10 1 2])").to_string(),
        "7"
    );
    // reduce with seed
    assert_eq!(
        eval("(reduce (fn [acc x] (+ acc x)) 10 [1 2])").to_string(),
        "13"
    );

    // map over vector and list
    assert_eq!(eval("(map inc [1 2 3])").to_string(), "[2 3 4]");
}

#[test]
fn nil_tolerant_and_map_set_ops() {
    match eval("(assoc nil :a 1)") {
        Value::Map(m) => assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1))),
        other => panic!("expected map, got {:?}", other),
    }
    match eval("(update nil :a (fn [x] (if (nil? x) 1 x)))") {
        Value::Map(m) => assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1))),
        other => panic!("expected map, got {:?}", other),
    }
    match eval("(conj nil [:a 1])") {
        Value::Map(m) => assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1))),
        other => panic!("expected map, got {:?}", other),
    }
    match eval("(dissoc {:a 1 :b 2} :a)") {
        Value::Map(m) => {
            assert!(m.get(&Key::Keyword("a".into())).is_none());
            assert_eq!(m.get(&Key::Keyword("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {:?}", other),
    }
    assert_eq!(eval("(dissoc nil :a)").to_string(), "nil");
    match eval("(contains? {:a 1} :a)") {
        Value::Bool(b) => assert!(b),
        other => panic!("expected bool, got {:?}", other),
    }
    match eval("(contains? #{1 2} 2)") {
        Value::Bool(b) => assert!(b),
        other => panic!("expected bool, got {:?}", other),
    }
    match eval("(into {:a 1} [[:b 2]])") {
        Value::Map(m) => {
            assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1)));
            assert_eq!(m.get(&Key::Keyword("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {:?}", other),
    }
    match eval("(merge {:a 1} nil {:b 2})") {
        Value::Map(m) => {
            assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1)));
            assert_eq!(m.get(&Key::Keyword("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {:?}", other),
    }
}

#[test]
fn sequence_helpers_and_range() {
    assert_eq!(eval("(seq [])").to_string(), "nil");
    assert_eq!(eval("(seq [1 2])").to_string(), "<seq>");
    assert_eq!(eval("(next [1 2 3])").to_string(), "(2 3)");
    assert_eq!(eval("(second [9 8 7])").to_string(), "8");
    assert_eq!(eval("(last (list 1 2 3))").to_string(), "3");
    assert_eq!(eval("(reverse [1 2])").to_string(), "[2 1]");
    assert_eq!(eval("(nth [5 6 7] 1)").to_string(), "6");
    assert_eq!(eval("(nth [5 6] 5 99)").to_string(), "99");
    assert_eq!(eval("(take 2 [1 2 3])").to_string(), "[1 2]");
    assert_eq!(eval("(drop 1 [1 2])").to_string(), "[2]");
    assert_eq!(
        eval("(take-while (fn [x] (< x 3)) [1 2 3 4])").to_string(),
        "[1 2]"
    );
    assert_eq!(
        eval("(drop-while (fn [x] (< x 3)) [1 2 3 4])").to_string(),
        "[3 4]"
    );
    assert_eq!(eval("(range 1 4)").to_string(), "[1 2 3]");
    assert_eq!(eval("(range 3 0 -1)").to_string(), "[3 2 1]");
}

#[test]
fn comparison_truthy_and_mod() {
    assert_eq!(eval("(< 1 2 3)").to_string(), "true");
    assert_eq!(eval("(> 3 2 3)").to_string(), "false");
    assert_eq!(eval("(mod -3 5)").to_string(), "2");
    match eval("(boolean nil)") {
        Value::Bool(false) => {}
        other => panic!("expected false, got {:?}", other),
    }
    match eval("(not false)") {
        Value::Bool(true) => {}
        other => panic!("expected true, got {:?}", other),
    }
}

#[test]
fn higher_order_helpers() {
    assert_eq!(eval("(apply + [1 2 3])").to_string(), "6");
    assert_eq!(eval("((partial + 1) 2 3)").to_string(), "6");
    assert_eq!(eval("((comp inc inc) 1)").to_string(), "3");
    assert_eq!(eval("((. inc (fn [x] (* x 2))) 3)").to_string(), "8");
    assert_eq!(
        eval("(((fn [x] (* x 2)) (fn [x] (+ x 1)) .) 3)").to_string(),
        "8"
    );
}

#[test]
fn string_and_regex_helpers() {
    assert_eq!(eval("(string::join [1 2])").to_string(), "\"12\"");
    assert_eq!(eval("(string::join [1 2] \",\")").to_string(), "\"1,2\"");
    assert_eq!(eval("(split \"a-b-c\" \"-\" 0)").to_string(), "[\"a-b-c\"]");
    assert_eq!(
        eval("(split \"a-b-c\" \"-\" 2)").to_string(),
        "[\"a\" \"b-c\"]"
    );
    assert_eq!(
        eval("(str-replace \"a1a\" \"1\" \"b\")").to_string(),
        "\"aba\""
    );
    assert_eq!(eval("(trim \"  hi \")").to_string(), "\"hi\"");
    assert_eq!(eval("(triml \"  hi\")").to_string(), "\"hi\"");
    assert_eq!(eval("(trimr \"hi  \")").to_string(), "\"hi\"");
    assert_eq!(eval("(blank? \" \\n\")").to_string(), "true");
    assert_eq!(eval("(includes? \"hello\" \"ell\")").to_string(), "true");
    assert_eq!(eval("(starts-with? \"hello\" \"he\")").to_string(), "true");
    assert_eq!(eval("(ends-with? \"hello\" \"lo\")").to_string(), "true");
    assert_eq!(eval("(split-lines \"a\\nb\")").to_string(), "[\"a\" \"b\"]");
}

#[test]
fn includes_handles_collections() {
    assert_eq!(eval("(includes? [:a :b :c] :b)").to_string(), "true");
    assert_eq!(eval("(includes? [:a :b :c] :z)").to_string(), "false");
    assert_eq!(eval("(includes? [] :a)").to_string(), "false");
    assert_eq!(eval("(includes? #{:a :b} :a)").to_string(), "true");
    assert_eq!(eval("(includes? #{:a :b} :z)").to_string(), "false");
    assert_eq!(eval("(includes? {:a 1 :b 2} :a)").to_string(), "true");
    assert_eq!(eval("(includes? {:a 1 :b 2} :z)").to_string(), "false");
    assert_eq!(eval("(includes? nil :a)").to_string(), "false");
}

#[test]
fn contains_vec_type_mismatch_mentions_includes() {
    let err = eval_err("(contains? [:a :b] :a)");
    let msg = err.to_string().to_lowercase();
    assert!(msg.contains("index"), "unexpected error: {}", err);
    assert!(msg.contains("includes?"), "unexpected error: {}", err);
}

#[test]
fn disj_removes_values_from_sets() {
    assert_eq!(eval("(disj #{:a :b} :a)").to_string(), "#{:b}");
    assert_eq!(eval("(disj #{:a :b} :a :b)").to_string(), "#{}");
    assert_eq!(eval("(disj nil :a)").to_string(), "nil");
}

#[test]
fn str_coerces_plain_strings_without_quotes() {
    assert_eq!(eval("(str 1 \"2\")").to_string(), "\"12\"");
    assert_eq!(eval("(str \"a\" \"b\" 3)").to_string(), "\"ab3\"");
}

#[test]
fn new_core_like_helpers() {
    assert_eq!(eval("(subs \"abcdef\" 2)").to_string(), "\"cdef\"");
    assert_eq!(eval("(subs \"abcdef\" 2 4)").to_string(), "\"cd\"");
    assert_eq!(eval("(count \"abc\")").to_string(), "3");
    assert_eq!(eval("(vec (list 1 2 3))").to_string(), "[1 2 3]");
    assert_eq!(eval("(nth \"abc\" 1)").to_string(), "\"b\"");
    assert_eq!(eval("(nth \"abc\" 5 \"z\")").to_string(), "\"z\"");
    assert_eq!(eval("(into [1] (list 2 3))").to_string(), "[1 2 3]");
    match eval("(into {:a 1} {:b 2})") {
        Value::Map(m) => {
            assert_eq!(m.get(&Key::Keyword("a".into())), Some(&Value::Int(1)));
            assert_eq!(m.get(&Key::Keyword("b".into())), Some(&Value::Int(2)));
        }
        other => panic!("expected map, got {:?}", other),
    }
    // spit / slurp roundtrip
    let mut path = PathBuf::from(std::env::temp_dir());
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    path.push(format!("clove_test_{}.txt", ts));
    let path_str = path.to_string_lossy().to_string();
    let write_form = format!("(spit \"{}\" \"hello\")", path_str);
    assert_eq!(eval(&write_form).to_string(), format!("\"{}\"", path_str));
    let read_form = format!("(slurp \"{}\")", path_str);
    assert_eq!(eval(&read_form).to_string(), "\"hello\"");
    let _ = fs::remove_file(&path);

    // slurp http(s) (run with CLOVE_RUN_NETWORK_TESTS=1)
    if std::env::var_os("CLOVE_RUN_NETWORK_TESTS").is_some() {
        match eval(r#"(> (count (slurp "https://example.com/")) 0)"#) {
            Value::Bool(true) => {}
            other => panic!("expected true, got {:?}", other),
        }
    }
}

#[test]
fn duration_constructors() {
    assert_eq!(eval("(duration-ms 10)").to_string(), "10ms");
    assert_eq!(eval("(duration-sec 0.5)").to_string(), "500ms");
    assert_eq!(eval("(duration 2 :hour)").to_string(), "2h");
    assert_eq!(eval("(duration 1 :week)").to_string(), "1w");
    assert_eq!(eval("(sec (duration 1500 :ms))").to_string(), "1.5");
    assert_eq!(eval("(duration-day 0.5)").to_string(), "12h");
    assert_eq!(eval("(duration-week 1.5)").to_string(), "252h");
    assert_eq!(eval("(duration-year 0.5)").to_string(), "4380h");
}

#[test]
fn duration_converters() {
    assert_eq!(eval("(ms 5s)").to_string(), "5000");
    assert_eq!(eval("(sec 1500ms)").to_string(), "1.5");
    assert_eq!(eval("(hour 90m)").to_string(), "1.5");
    assert_eq!(eval("(day 12h)").to_string(), "0.5");
    assert_eq!(eval("(week 3d)").to_string(), "0.42857142857142855");
    assert_eq!(eval("(year 4380h)").to_string(), "0.5");
}
