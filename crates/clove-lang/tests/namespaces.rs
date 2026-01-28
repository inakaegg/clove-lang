use std::fs;
use std::path::Path;

use clove_core::{
    ast::{Key, Value},
    eval_source_with_engines,
    options::EvalOptions,
};
use tempfile::tempdir;

fn write_file(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, content).expect("write file");
}

#[test]
fn require_with_alias_and_refer() {
    let tmp = tempdir().expect("create temp dir");
    let app_dir = tmp.path().join("src").join("my").join("app");
    let util_path = app_dir.join("util.clv");
    write_file(
        &util_path,
        r#"(ns my::app::util)
(defn greet [] "hello")
(def- secret 42)
"#,
    );
    let helpers_path = app_dir.join("helpers.clv");
    write_file(
        &helpers_path,
        r#"(ns my::app::helpers)
(def helper-value 99)
"#,
    );
    let core_path = app_dir.join("core.clv");
    write_file(
        &core_path,
        r#"(ns my::app::core
  (:require [my::app::util :as util :refer [greet]]))
  (require "./helpers.clv")
  (def secret-visible?
    (try
      (do my::app::util::secret true)
      (catch err false)))
  (defn run []
    {:alias (util::greet)
     :refer (greet)
     :qualified my::app::helpers::helper-value
     :secret secret-visible?})
  (run)
  "#,
    );

    let mut opts = EvalOptions::default();
    opts.source_name = Some(core_path.to_string_lossy().into_owned());
    let src = fs::read_to_string(&core_path).expect("read core");
    let result = eval_source_with_engines(&src, opts, &[]).expect("eval core");
    let map = match result {
        Value::Map(map) => map,
        other => panic!("expected map, got {:?}", other),
    };
    assert_eq!(
        map.get(&Key::Keyword("alias".into())),
        Some(&Value::String("hello".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("refer".into())),
        Some(&Value::String("hello".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("qualified".into())),
        Some(&Value::Int(99))
    );
    assert_eq!(
        map.get(&Key::Keyword("secret".into())),
        Some(&Value::Bool(false))
    );
}

#[test]
fn require_flat_specs_work() {
    let tmp = tempdir().expect("create temp dir");
    let app_dir = tmp.path().join("src").join("my").join("app");
    let util_path = app_dir.join("util.clv");
    write_file(
        &util_path,
        r#"(ns my::app::util)
(defn greet [] "hi")
"#,
    );
    let helpers_path = app_dir.join("helpers.clv");
    write_file(
        &helpers_path,
        r#"(ns my::app::helpers)
(def helper_value 7)
"#,
    );
    let flat_path = app_dir.join("flat.clv");
    write_file(
        &flat_path,
        r#"(ns my::app::flat
  (:require my::app::util :as util :refer :all))
  (require my::app::helpers :as helpers :refer [helper_value])
  (defn run []
    {:ns-alias (util::greet)
     :ns-refer (greet)
     :top-alias helpers::helper_value
     :top-refer helper_value})
  (run)
  "#,
    );

    let mut opts = EvalOptions::default();
    opts.source_name = Some(flat_path.to_string_lossy().into_owned());
    let src = fs::read_to_string(&flat_path).expect("read flat");
    let result = eval_source_with_engines(&src, opts, &[]).expect("eval flat");
    let map = match result {
        Value::Map(map) => map,
        other => panic!("expected map, got {:?}", other),
    };
    assert_eq!(
        map.get(&Key::Keyword("ns-alias".into())),
        Some(&Value::String("hi".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("ns-refer".into())),
        Some(&Value::String("hi".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("top-alias".into())),
        Some(&Value::Int(7))
    );
    assert_eq!(
        map.get(&Key::Keyword("top-refer".into())),
        Some(&Value::Int(7))
    );
}

#[test]
fn require_symbol_dsl_forms_work() {
    let tmp = tempdir().expect("create temp dir");
    let app_dir = tmp.path().join("src").join("my").join("app");
    let util_path = app_dir.join("util.clv");
    write_file(
        &util_path,
        r#"(ns my::app::util)
(defn greet [] "hi-from-dsl")
"#,
    );
    let helpers_path = app_dir.join("helpers.clv");
    write_file(
        &helpers_path,
        r#"(ns my::app::helpers)
(def helper-value 13)
"#,
    );
    let core_path = app_dir.join("core.clv");
    write_file(
        &core_path,
        r#"(ns my::app::core
  (require my::app::util as util refer greet))
  (require my::app::helpers as helpers refer *)
  (defn run []
    {:dsl-alias (util::greet)
     :dsl-refer (greet)
     :dsl-inline helper-value
     :dsl-qualified helpers::helper-value})
  (run)
  "#,
    );
    let mut opts = EvalOptions::default();
    opts.source_name = Some(core_path.to_string_lossy().into_owned());
    let src = fs::read_to_string(&core_path).expect("read core");
    let result = eval_source_with_engines(&src, opts, &[]).expect("eval core");
    let map = match result {
        Value::Map(map) => map,
        other => panic!("expected map, got {:?}", other),
    };
    assert_eq!(
        map.get(&Key::Keyword("dsl-alias".into())),
        Some(&Value::String("hi-from-dsl".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("dsl-refer".into())),
        Some(&Value::String("hi-from-dsl".into()))
    );
    assert_eq!(
        map.get(&Key::Keyword("dsl-inline".into())),
        Some(&Value::Int(13))
    );
    assert_eq!(
        map.get(&Key::Keyword("dsl-qualified".into())),
        Some(&Value::Int(13))
    );
}
