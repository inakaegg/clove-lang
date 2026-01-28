use std::collections::HashSet;
use std::fs;
use std::sync::{Arc, Mutex};
use std::thread;

use clove_core::ast::{Form, FormKind, Span, Value, Vector};
use clove_core::env::EnvRef;
use clove_core::error::CloveError;
use clove_core::fn_meta::{self, FnMeta, SubjectPos};
use clove_core::foreign::ForeignEngine;
use clove_core::options::EvalOptions;
use clove_core::reader::OOP_METHOD_SYM;
use clove_core::runtime::RuntimeCtx;
use clove_core::{eval_source as eval_source_raw, eval_source_with_engines};

#[derive(Clone)]
struct EchoEngine {
    calls: Arc<Mutex<Vec<String>>>,
}

impl ForeignEngine for EchoEngine {
    fn tag(&self) -> &str {
        "rb"
    }

    fn eval_block(
        &self,
        code: &str,
        _env: EnvRef,
        _span: Option<Span>,
    ) -> Result<Value, CloveError> {
        self.calls.lock().unwrap().push(code.to_string());
        Ok(Value::String(code.to_string()))
    }

    fn eval_fallback(&self, _form: &Form, _env: EnvRef) -> Result<Value, CloveError> {
        Err(CloveError::runtime("fallback not supported"))
    }

    fn call_symbol(
        &self,
        _path: &str,
        _args: &[Value],
        _span: Option<Span>,
    ) -> Result<Value, CloveError> {
        Err(CloveError::runtime("call_symbol not supported"))
    }
}

fn run_with_large_stack<F, T>(f: F) -> T
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    let handle = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(f)
        .expect("failed to spawn test thread with larger stack");
    handle.join().expect("test thread panicked")
}

fn eval_source(src: &str, options: Option<EvalOptions>) -> Result<Value, CloveError> {
    let src = src.to_string();
    run_with_large_stack(move || eval_source_raw(&src, options))
}

#[test]
fn oop_chain_matches_manual_form() {
    let src = "
      (do
        (use oop-syntax true)
        (let [xs (range 10)
              chained xs.map(inc).take(3)
              manual (take 3 (map inc xs))]
          (= chained manual)))
    ";
    let out = eval_source(src, None).expect("eval oop chain");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_composes_methods() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [left (str "a,b,c".string::split(",").reverse)
              right (str (reverse (string::split "a,b,c" ",")))]
          (= left right)))
    "#;
    let out = eval_source(src, None).expect("eval split chain");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_allows_dot_stage_mix() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [out (range 5).take(3).(+ *?).inc]
          (= out 4)))
    "#;
    let out = eval_source(src, None).expect("eval oop chain with dot stage");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_as_binds_value_for_following_stages() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [out [1 2 3].(as xs).take(2).concat(xs).vec]
          (= out [1 2 1 2 3])))
    "#;
    let out = eval_source(src, None).expect("eval oop chain with as stage");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_let_stashes_value_and_continues() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [out [1 2 3].(let stash).take(2).vec]
          (and (= out [1 2])
               (= *stash [1 2 3]))))
    "#;
    let out = eval_source(src, None).expect("eval oop chain with let stage");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_subject_positions_are_respected() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [xs [1 2 3]
              ys [10 20 30]
              f +]
          (and (= (inc 1) 1.inc)
               (= xs.map(f ys) (map f xs ys))
               (= xs.reduce(f) (reduce f xs))
               (= xs.reduce(f 0) (reduce f 0 xs))
               (= xs.into(#{4}) (into #{4} xs))
               (= xs.dorun() (dorun xs))
               (= xs.dorun(2) (dorun 2 xs)))))"#;
    let out = eval_source(src, None).expect("eval oop subject-pos");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_subject_positions_cover_std_examples() {
    let tmp_dir = std::env::temp_dir();
    let src_path = tmp_dir.join("clove_oop_copy_src.txt");
    let dst_path = tmp_dir.join("clove_oop_copy_dst.txt");
    fs::write(&src_path, "ok").expect("write source file");
    let _ = fs::remove_file(&dst_path);
    let src = format!(
        r#"
      (do
        (use oop-syntax true)
        (let [xs [1 2 3]
              joined xs.string::join(",")
              sum 1.+(1)
              pmap-out xs.pmap(inc {{:max-parallel 1}})
              copied ("{src}").fs::copy("{dst}")
              diff #{{1 2 3}}.set::difference(#{{2}})]
          (and (= joined "1,2,3")
               (= sum 2)
               (= (count pmap-out) 3)
               copied
               (= diff #{{1 3}}))))
    "#,
        src = src_path.to_string_lossy(),
        dst = dst_path.to_string_lossy()
    );
    let out = eval_source(&src, None).expect("eval oop std examples");
    assert_eq!(out, Value::Bool(true));
    let _ = fs::remove_file(&src_path);
    let _ = fs::remove_file(&dst_path);
}

#[test]
fn oop_method_calls_work_for_all_fnmeta() {
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let stub = ctx
        .eval_source("(fn [& args] (core::vec args))")
        .expect("build stub");
    let metas = ctx.with_current_ctx(|_ctx| fn_meta::all());
    let builtin_namespaces: HashSet<&str> = [
        "core", "std", "string", "io", "fs", "http", "async", "walk", "path", "shell", "process",
        "json", "ini", "time", "log", "env", "pprint", "set",
    ]
    .into_iter()
    .collect();
    for meta in metas {
        if !builtin_namespaces.contains(meta.ns.as_str()) {
            continue;
        }
        let subject_pos = meta
            .subject_pos
            .clone()
            .unwrap_or_else(|| panic!("missing subject_pos for {}", meta.fq_name()));
        let args_len = match subject_pos {
            SubjectPos::Fixed(pos) => pos.max(2),
            SubjectPos::Last => 2,
        };
        let use_unqualified = meta.name.contains('<');
        let method_name = if use_unqualified {
            meta.name.clone()
        } else {
            meta.fq_name()
        };
        let ns_env = ctx
            .namespace_env(&meta.ns)
            .unwrap_or_else(|| panic!("namespace env missing for {}", meta.fq_name()));
        let target_env = if use_unqualified {
            ctx.env()
        } else {
            ns_env.clone()
        };
        let previous = {
            let mut writer = target_env.write().unwrap();
            let prev = writer.get(&meta.name);
            writer.set(&meta.name, stub.clone());
            prev
        };
        let span = Span {
            line: 0,
            col: 0,
            index: 0,
        };
        let mut items = Vec::with_capacity(args_len + 3);
        items.push(Form::new(
            FormKind::Symbol(OOP_METHOD_SYM.to_string()),
            span,
        ));
        items.push(Form::new(FormKind::Keyword("self".into()), span));
        items.push(Form::new(FormKind::String(method_name.clone()), span));
        for idx in 1..=args_len {
            items.push(Form::new(FormKind::Keyword(format!("arg{}", idx)), span));
        }
        let call_form = Form::new(FormKind::List(items), span);
        let result = ctx.with_current_ctx(|ctx| {
            let env = ctx.env();
            ctx.eval_repl_forms(vec![call_form], env, None)
        });
        {
            let mut writer = target_env.write().unwrap();
            match previous {
                Some(val) => writer.set(&meta.name, val),
                None => {
                    writer.remove(&meta.name);
                }
            }
        }
        let value =
            result.unwrap_or_else(|err| panic!("oop call failed for {}: {}", meta.fq_name(), err));
        let base = Value::Symbol(":self".into());
        let arg_values: Vec<Value> = (1..=args_len)
            .map(|idx| Value::Symbol(format!(":arg{}", idx)))
            .collect();
        let subject_pos = match subject_pos {
            SubjectPos::Fixed(pos) => pos,
            SubjectPos::Last => args_len + 1,
        };
        let insert_idx = subject_pos.saturating_sub(1);
        let mut expected = Vec::with_capacity(arg_values.len() + 1);
        let mut inserted = false;
        for (idx, arg) in arg_values.iter().enumerate() {
            if idx == insert_idx {
                expected.push(base.clone());
                inserted = true;
            }
            expected.push(arg.clone());
        }
        if !inserted {
            expected.push(base.clone());
        }
        assert_eq!(
            value,
            Value::Vector(Vector::from(expected)),
            "oop args mismatch for {}",
            meta.fq_name()
        );
    }
}

#[test]
fn oop_known_meta_requires_subject_position() {
    let meta = FnMeta::new("user", "mystery");
    fn_meta::register(meta);
    let err = eval_source("(do (use oop-syntax true) 1.mystery())", None)
        .expect_err("subject position should be required");
    assert!(
        err.to_string()
            .contains("cannot determine subject position"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_disabled_errors() {
    let err = eval_source("(do (use oop-syntax false) (range 3).take(1))", None)
        .expect_err("oop chain should fail when disabled");
    assert!(
        err.to_string()
            .contains("oop-syntax is disabled; enable it via (use oop-syntax true)"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_method_allows_amp_self_alias() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [obj {:x 10
                   :add (fn [n] (+ &self[:x] n))}]
          (obj.add 5)))
    "#;
    let out = eval_source(src, None).expect("eval &self alias");
    assert_eq!(out, Value::Int(15));
}

#[test]
fn ruby_suffix_is_kept_in_single_expression() {
    let calls = Arc::new(Mutex::new(Vec::new()));
    let engine = Arc::new(EchoEngine {
        calls: calls.clone(),
    });
    let opts = EvalOptions {
        source_name: Some("test.rb.clv".into()),
        ..Default::default()
    };
    let src = r#"
      (do
        (use oop-syntax true)
        (let [html "<h1>Hi</h1>"]
          $Nokogiri::HTML.parse(html).search("h1").text))
    "#;
    let out = eval_source_with_engines(src, opts, &[engine]).expect("eval ruby suffix");
    let expected = "(Nokogiri::HTML.parse(html)).search(\"h1\").text";
    assert_eq!(out, Value::String(expected.into()));
    let recorded = calls.lock().unwrap();
    assert_eq!(recorded.as_slice(), &[expected.to_string()]);
}

#[test]
fn oop_chain_dot_indexer_explicit_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [a ({:timing {:spawn-frames 92}}).:timing.:spawn-frames
              b ({"timing" {:spawn-frames 92}})."timing".:spawn-frames
              c ({'timing {:spawn-frames 92}}).'timing.:spawn-frames]
          [a b c]))
    "#;
    let out = eval_source(src, None).expect("eval oop explicit dot-indexer");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![
            Value::Int(92),
            Value::Int(92),
            Value::Int(92),
        ]))
    );
}

#[test]
fn oop_chain_dot_indexer_numeric_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [a [10 20 30].0
              b [10 20 30].-1
              c {0 "foo"}.0]
          (= [a b c] [10 30 "foo"])))
    "#;
    let out = eval_source(src, None).expect("eval oop numeric dot-indexer");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_dot_indexer_keyword_sugar_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [a ({:timing 1}):timing
              b ({"timing" 1})."timing"
              c ({'timing 1}).'timing
              d ({:timing nil}):timing]
          (= [a b c d] [1 1 1 nil])))
    "#;
    let out = eval_source(src, None).expect("eval oop keyword sugar dot-indexer");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_dot_indexer_bare_segment_does_not_index_map_keys() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) ({:timing 1}).timing)",
        None,
    )
    .expect_err("bare segment should not index map keys");
    assert!(
        err.to_string().contains("not callable"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_keyword_sugar_chain() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [config {:pipe {:gap-max 485}}
              a config:pipe:gap-max
              b config.:pipe.:gap-max
              c config:pipe.:gap-max]
          (= [a b c] [485 485 485])))
    "#;
    let out = eval_source(src, None).expect("eval oop dot-indexer keyword sugar chain");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_propagates() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [x1 {:a {:b {:c 123}}}
              x2 {:a nil}
              x3 {:a {:b nil}}
              x4 nil]
          (and (= 123 x1?.:a.:b.:c)
               (nil? x2?.:a.:b.:c)
               (nil? x3?.:a.:b.:c)
               (nil? x4?.:a.:b.:c))))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe chain");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_mid_chain() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [y {:a nil}]
          (nil? y.:a?.:b.:c)))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe mid-chain");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_string_and_keyword_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [z {"timing" {:spawn-frames 92}}
              w {"timing" nil}]
          (and (= 92 z?."timing".:spawn-frames)
               (nil? w?."timing".:spawn-frames))))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe key segments");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_does_not_evaluate_rest() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [called (atom 0)
              bump! (fn [_] (atom-set! called (inc @called)) 1)
              n nil]
          (and (nil? n?.bump!)
               (= 0 @called))))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe side-effect");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_predicate_escape_resolves() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [num? {:str 1}]
          (= 1 num??.:str)))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe predicate escape");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_predicate_escape_falls_back() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [num?? {:str 2}]
          (= 2 num??.:str)))
    "#;
    let out = eval_source(src, None).expect("eval oop nil-safe predicate fallback");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_nil_safe_predicate_escape_ambiguous_errors() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) (let [num? {:str 1} num?? {:str 2}] num??.:str))",
        None,
    )
    .expect_err("nil-safe predicate escape should be ambiguous");
    assert!(
        err.to_string().contains("ambiguous"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_method_call_for_bare_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [method ({:a 1}).keys
              explicit ({:keys 1}).:keys
              sugar ({:keys 1}):keys]
          (and (= method [:a]) (= explicit 1) (= sugar 1))))
    "#;
    let out = eval_source(src, None).expect("eval oop dot-indexer bare method");
    assert_eq!(out, Value::Bool(true));
}

#[test]
fn oop_chain_dot_indexer_bare_segment_errors_when_key_is_non_callable() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) ({:keys 1}).keys)",
        None,
    )
    .expect_err("bare segment should error on non-callable key");
    assert!(
        err.to_string().contains("not callable"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_bare_segment_does_not_use_map_keys() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) ({:foo 1 \"foo\" 2}).foo)",
        None,
    )
    .expect_err("bare segment should not select map keys");
    assert!(
        err.to_string().contains("not callable"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_ambiguous_key_explicit_segments() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (let [m {:foo 1 "foo" 2}
              a m.:foo
              b m."foo"]
          [a b]))
    "#;
    let out = eval_source(src, None).expect("eval explicit dot-indexer segments");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![Value::Int(1), Value::Int(2)]))
    );
}

#[test]
fn oop_chain_dot_indexer_vector_methods_still_work() {
    let out = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) [1 2 3].count)",
        None,
    )
    .expect("eval vector method dot-indexer");
    assert_eq!(out, Value::Int(3));
}

#[test]
fn oop_chain_dot_indexer_vector_key_errors() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) [1 2].:foo)",
        None,
    )
    .expect_err("vector dot-indexer keyword should error");
    assert!(
        err.to_string().contains("index must be integer"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_set_numeric_errors() {
    let err = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) #{1 2}.0)",
        None,
    )
    .expect_err("set dot-indexer numeric should error");
    assert!(
        err.to_string()
            .contains("dot-indexer does not support set targets"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_chain_dot_indexer_set_methods_still_work() {
    let out = eval_source(
        "(do (use oop-syntax true) (use dot-indexer true) #{1 2}.count)",
        None,
    )
    .expect("eval set method dot-indexer");
    assert_eq!(out, Value::Int(2));
}

#[test]
fn map_symbol_keys_are_accessible() {
    let out = eval_source("(get {'timing 1} 'timing)", None).expect("eval symbol key get");
    assert_eq!(out, Value::Int(1));
}

#[test]
fn oop_object_method_uses_method_form() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [obj {:x 10 :add (method [n] (+ &:x n))}]
          (obj.add 5)))
    "#;
    let out = eval_source(src, None).expect("eval method form in map");
    assert_eq!(out, Value::Int(15));
}

#[test]
fn oop_object_method_rejects_explicit_self_param() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [obj {:x 10 :add (method [self n] (+ (get self :x) n))}]
          (obj.add 5)))
    "#;
    let err = eval_source(src, None).expect_err("explicit self should error");
    assert!(
        err.to_string().contains("self is reserved"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_method_allows_self_in_map_literal() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [obj {:x 10 :pack (method [] {:x &[:x] :self &})}
              packed (obj.pack)]
          [(get packed :x) (get (get packed :self) :x)]))
    "#;
    let out = eval_source(src, None).expect("eval & in map literal");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![Value::Int(10), Value::Int(10)]))
    );
}

#[test]
fn oop_object_method_calls_plain_callable() {
    let out = eval_source(
        "(do (use oop-syntax true) (let [obj {:foo (fn [x] x)}] (obj.foo 1)))",
        None,
    )
    .expect("plain callable in map should work");
    assert_eq!(out, Value::Int(1));
}

#[test]
fn oop_object_method_rejects_non_callable_value() {
    let err = eval_source(
        "(do (use oop-syntax true) (let [obj {:foo 123}] (obj.foo)))",
        None,
    )
    .expect_err("non-callable map entry should error");
    assert!(
        err.to_string().contains("not callable"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_deftype_where_methods_are_attached() {
    let src = r#"
      (do
        (use oop-syntax true)
        (use dot-indexer true)
        (deftype Renderer {canvas: Any}
          (where
            (defn clear [] &[:canvas])))
        (let [r (Renderer canvas: 9)]
          [(r.clear) (r.:canvas)]))
    "#;
    let out = eval_source(src, None).expect("eval deftype where methods");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![Value::Int(9), Value::Int(9)]))
    );
}

#[test]
fn oop_deftype_direct_methods_are_attached() {
    let src = r#"
      (do
        (use oop-syntax true)
        (deftype Renderer
          (def initial {:canvas 9})
          (defn clear [] (get self :canvas))
          (method tag [] (+ (get self :canvas) 1)))
        [(initial.clear) (initial.tag)])
    "#;
    let out = eval_source(src, None).expect("eval deftype direct methods");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![Value::Int(9), Value::Int(10)]))
    );
}

#[test]
fn oop_deftype_method_rejects_self_param() {
    let src = r#"
      (do
        (deftype Bad
          (defn boom [self x] x))
        0)
    "#;
    let err = eval_source(src, None).expect_err("self param should error");
    assert!(
        err.to_string().contains("self is reserved"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn oop_assoc_infers_method_and_dissoc_removes() {
    let src = r#"
      (do
        (use oop-syntax true)
        (let [r {:canvas 3}
              r2 (assoc r :set-color (fn [x] (+ &[:canvas] x)))
              r3 (dissoc r2 :set-color)]
          [(r2.set-color 4) (get r3 :canvas)]))
    "#;
    let out = eval_source(src, None).expect("eval assoc method inference");
    assert_eq!(
        out,
        Value::Vector(Vector::from(vec![Value::Int(7), Value::Int(3)]))
    );
}

#[test]
fn oop_dissoc_removed_method_errors() {
    let err = eval_source(
        "(do (use oop-syntax true) (let [r {:canvas 3} r2 (assoc r :set-color (fn [x] (+ &[:canvas] x))) r3 (dissoc r2 :set-color)] (r3.set-color 1)))",
        None,
    )
    .expect_err("removed method should error");
    assert!(
        err.to_string()
            .contains("oop-seg could not resolve segment"),
        "unexpected error: {}",
        err
    );
}
