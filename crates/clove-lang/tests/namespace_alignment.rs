use clove_core::ast::{Key, Value};
use clove_core::options::EvalOptions;
use clove_lang::{create_runtime, default_engines};
use std::collections::HashMap as StdHashMap;

fn eval(ctx: &mut clove_core::runtime::RuntimeCtx, src: &str) -> Value {
    ctx.eval_source(src).expect("eval failed")
}

fn nav_vars(nav: &Value) -> Vec<Value> {
    let Value::Map(map) = nav else {
        panic!("expected nav map");
    };
    let Value::Vector(vars) = map
        .get(&Key::Keyword("var".into()))
        .cloned()
        .unwrap_or(Value::Vector(Default::default()))
    else {
        panic!("expected var vector");
    };
    vars.iter().cloned().collect()
}

fn entry_sym(entry: &Value) -> Option<String> {
    let Value::Map(map) = entry else {
        return None;
    };
    map.get(&Key::Keyword("sym".into()))
        .and_then(|value| match value {
            Value::Symbol(s) | Value::String(s) => Some(s.clone()),
            _ => None,
        })
}

fn entry_name(entry: &Value) -> Option<String> {
    let Value::Map(map) = entry else {
        return None;
    };
    map.get(&Key::Keyword("name".into()))
        .and_then(|value| match value {
            Value::Symbol(s) | Value::String(s) => Some(s.clone()),
            _ => None,
        })
}

fn entry_topics(entry: &Value) -> Vec<String> {
    let Value::Map(map) = entry else {
        return Vec::new();
    };
    let Some(value) = map.get(&Key::Keyword("topics".into())) else {
        return Vec::new();
    };
    let mut topics = Vec::new();
    let items: Vec<&Value> = match value {
        Value::Set(items) => items.iter().collect(),
        Value::SortedSet(items) => items.entries.iter().collect(),
        Value::Vector(items) => items.iter().collect(),
        _ => Vec::new(),
    };
    for item in items {
        if let Value::Symbol(text) | Value::String(text) = item {
            let trimmed = text.trim_start_matches(':').to_string();
            if !trimmed.is_empty() {
                topics.push(trimmed);
            }
        }
    }
    topics.sort();
    topics.dedup();
    topics
}

fn top_names(vars: &[Value], count: usize) -> Vec<String> {
    vars.iter().take(count).filter_map(entry_name).collect()
}

fn top_syms(vars: &[Value], count: usize) -> Vec<String> {
    vars.iter().take(count).filter_map(entry_sym).collect()
}

fn list_contains_any(values: &[String], targets: &[&str]) -> bool {
    targets
        .iter()
        .any(|target| values.iter().any(|value| value == target))
}

#[test]
fn set_aliases_match_canonical() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let canonical = eval(&mut ctx, "(set::difference #{1 2 3} #{2 3})");
    let std_alias = eval(&mut ctx, "(std::difference #{1 2 3} #{2 3})");
    let short_alias = eval(&mut ctx, "(difference #{1 2 3} #{2 3})");
    assert_eq!(std_alias, canonical);
    assert_eq!(short_alias, canonical);
}

#[test]
fn walk_aliases_match_canonical() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let canonical = eval(
        &mut ctx,
        "(walk::prewalk (fn [x] (if (number? x) (* x 2) x)) [1 {:x 2}])",
    );
    let alias = eval(
        &mut ctx,
        "(prewalk (fn [x] (if (number? x) (* x 2) x)) [1 {:x 2}])",
    );
    assert_eq!(alias, canonical);
}

#[test]
fn fs_aliases_match_canonical() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    eval(&mut ctx, "(io::spit-bytes \"namespace-align-fs.bin\" [1])");
    let canonical = eval(&mut ctx, "(fs::file-exists? \"namespace-align-fs.bin\")");
    let io_alias = eval(&mut ctx, "(io::file-exists? \"namespace-align-fs.bin\")");
    let short_alias = eval(&mut ctx, "(file-exists? \"namespace-align-fs.bin\")");
    eval(&mut ctx, "(fs::delete \"namespace-align-fs.bin\")");
    assert_eq!(io_alias, canonical);
    assert_eq!(short_alias, canonical);
}

#[test]
fn string_namespace_short_alias_works() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let canonical = eval(&mut ctx, "(string::trim \"  clove  \")");
    let alias = eval(&mut ctx, "(str::trim \"  clove  \")");
    assert_eq!(alias, canonical);
    let includes = eval(&mut ctx, "(string::includes? \"clove\" \"lo\")");
    let includes_alias = eval(&mut ctx, "(str::includes? \"clove\" \"lo\")");
    assert_eq!(includes_alias, includes);
}

#[test]
fn nav_reports_set_difference_as_canonical() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"difference\" :var)");
    let vars = nav_vars(&nav);
    let Some(entry) = vars.get(0) else {
        panic!("expected var entry");
    };
    let sym = entry_sym(entry).expect("sym missing");
    assert_eq!(sym, "set::difference");
    let Value::Map(entry) = entry else {
        panic!("expected var entry map");
    };
    let aliases = entry
        .get(&Key::Keyword("aliases".into()))
        .and_then(|value| match value {
            Value::Vector(items) => Some(
                items
                    .iter()
                    .filter_map(|item| match item {
                        Value::Symbol(s) | Value::String(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        })
        .unwrap_or_default();
    assert!(aliases.iter().any(|alias| alias == "difference"));
    assert!(aliases.iter().any(|alias| alias == "std::difference"));
}

#[test]
fn nav_reports_fs_file_exists_as_canonical() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"file-exists?\" :var)");
    let vars = nav_vars(&nav);
    let Some(entry) = vars.get(0) else {
        panic!("expected var entry");
    };
    let sym = entry_sym(entry).expect("sym missing");
    assert_eq!(sym, "fs::file-exists?");
    let Value::Map(entry) = entry else {
        panic!("expected var entry map");
    };
    let aliases = entry
        .get(&Key::Keyword("aliases".into()))
        .and_then(|value| match value {
            Value::Vector(items) => Some(
                items
                    .iter()
                    .filter_map(|item| match item {
                        Value::Symbol(s) | Value::String(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        })
        .unwrap_or_default();
    assert!(aliases.iter().any(|alias| alias == "file-exists?"));
    assert!(aliases.iter().any(|alias| alias == "io::file-exists?"));
}

#[test]
fn nav_topics_include_join_variants() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"join\" :var)");
    let vars = nav_vars(&nav);
    let mut seen = StdHashMap::new();
    for entry in vars {
        if let Some(sym) = entry_sym(&entry) {
            if sym == "set::join" || sym == "string::join" || sym == "path::join" {
                seen.insert(sym.clone(), entry_topics(&entry));
            }
        }
    }
    let set_topics = seen.get("set::join").cloned().unwrap_or_default();
    assert!(set_topics.iter().any(|topic| topic == "set"));
    let string_topics = seen.get("string::join").cloned().unwrap_or_default();
    assert!(string_topics.iter().any(|topic| topic == "string"));
    let path_topics = seen.get("path::join").cloned().unwrap_or_default();
    assert!(path_topics.iter().any(|topic| topic == "path"));
}

#[test]
fn nav_prefers_set_namespace_for_set_query() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"set\" :var)");
    let vars = nav_vars(&nav);
    let Some(entry) = vars.get(0) else {
        panic!("expected var entry");
    };
    let topics = entry_topics(entry);
    assert!(topics.iter().any(|topic| topic == "set"));
}

#[test]
fn nav_context_query_prefers_set_topics() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav #{} :var)");
    let vars = nav_vars(&nav);
    let Some(entry) = vars.get(0) else {
        panic!("expected var entry");
    };
    let topics = entry_topics(entry);
    assert!(topics.iter().any(|topic| topic == "set"));
}

#[test]
fn nav_hides_internal_vars_by_default() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    eval(&mut ctx, "(def __clove_hidden 1)");
    let nav = eval(&mut ctx, "(nav \"__clove_hidden\" :var)");
    let vars = nav_vars(&nav);
    assert!(vars.is_empty());
    let nav = eval(
        &mut ctx,
        "(nav \"__clove_hidden\" :var {:include-internal? true})",
    );
    let vars = nav_vars(&nav);
    assert!(!vars.is_empty());
}

#[test]
fn nav_context_query_ranks_core_disj_above_all_types() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav #{} :var)");
    let vars = nav_vars(&nav);
    let syms = top_syms(&vars, 15);
    assert!(list_contains_any(&syms, &["core::disj"]));
}

#[test]
fn nav_core_disj_includes_set_topic() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"disj\" :var)");
    let vars = nav_vars(&nav);
    let mut topics = Vec::new();
    for entry in vars {
        if entry_sym(&entry).as_deref() == Some("core::disj") {
            topics = entry_topics(&entry);
            break;
        }
    }
    assert!(topics.iter().any(|topic| topic == "set"));
}

#[test]
fn nav_context_query_prefers_map_primitives() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav {} :var)");
    let vars = nav_vars(&nav);
    let names = top_names(&vars, 10);
    assert!(list_contains_any(
        &names,
        &["assoc", "dissoc", "get", "find", "keys", "vals", "merge"]
    ));
}

#[test]
fn nav_context_query_prefers_vec_primitives() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav [] :var)");
    let vars = nav_vars(&nav);
    let names = top_names(&vars, 10);
    assert!(list_contains_any(
        &names,
        &["nth", "pop", "peek", "conj", "count", "subvec"]
    ));
}

#[test]
fn nav_context_query_prefers_string_primitives() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav \"\" :var)");
    let vars = nav_vars(&nav);
    let syms = top_syms(&vars, 10);
    assert!(list_contains_any(&syms, &["string::split", "string::join"]));
}

#[test]
fn nav_context_query_prefers_number_primitives() {
    let mut ctx = create_runtime(EvalOptions::default(), &default_engines());
    let nav = eval(&mut ctx, "(nav 0 :var)");
    let vars = nav_vars(&nav);
    let names = top_names(&vars, 10);
    assert!(list_contains_any(&names, &["+", "inc", "bit-and"]));
}
