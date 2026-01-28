use clove_core::eval_source;

#[test]
fn indexer_commas_fetch_multiple_indexes() {
    let out =
        eval_source("(let [xs [10 11 12 13 14 15]] xs[1,3,5])", None).expect("indexer commas");
    let expected = eval_source("[11 13 15]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn indexer_commas_accept_default() {
    let out = eval_source("(let [xs [10 11]] xs[0,2 || :ng])", None).expect("indexer default");
    let expected = eval_source("[10 :ng]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn indexer_spaces_still_use_get_in() {
    let out = eval_source("(let [xs [[0 1] [2 3] [4 5]]] xs[1 0])", None).expect("indexer get-in");
    let expected = eval_source("2", None).expect("expected get-in result");
    assert_eq!(out, expected);

    let out = eval_source("(let [xs [[0 1] [2 3] [4 5]]] xs[1,0])", None).expect("indexer commas");
    let expected = eval_source("[[2 3] [0 1]]", None).expect("expected comma result");
    assert_eq!(out, expected);
}

#[test]
fn indexer_commas_support_get_in_many_paths() {
    let out = eval_source("(let [xs [0 [1 2 [3 [4 5] 6] 7 8] 9]] xs[1 3, 1,2])", None)
        .expect("indexer get-in many");
    let expected = eval_source("[7 [1 2 [3 [4 5] 6] 7 8] 9]", None).expect("expected vector");
    assert_eq!(out, expected);
}

#[test]
fn indexer_commas_support_map_paths() {
    let out = eval_source(
        "(let [m {\"simplified\" \"x\" \"meanings\" {\"pos\" [{\"pinyin\" \"pin\"}]}}]\n  m[\"simplified\", \"meanings\" \"pos\" 0 \"pinyin\"])",
        None,
    )
    .expect("indexer map paths");
    let expected = eval_source("[\"x\" \"pin\"]", None).expect("expected vector");
    assert_eq!(out, expected);
}
