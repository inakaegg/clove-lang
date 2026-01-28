use clove_core::formatter::{format_source, FormatOptions};
use clove_core::pretty_print::{pretty_print_source, PrettyOptions};
use clove_core::reader::{Reader, ReaderOptions};
use clove_core::type_syntax::normalize_type_syntax_forms;

#[path = "common/mod.rs"]
mod common;

use common::{assert_forms_equivalent, parse_forms, read_example};

fn fmt(src: &str) -> String {
    format_source(src, FormatOptions::default()).expect("failed to format source")
}

#[test]
fn normalize_preserves_inline_symbol_type_hint() {
    let mut reader = Reader::new_with_options("x<Int>", ReaderOptions::default());
    let forms = reader.read_all().expect("parse forms");
    let normalized = normalize_type_syntax_forms(forms, false).expect("normalize types");
    assert!(normalized[0].type_hint.is_some());
}

#[test]
fn formatter_preserves_snake_examples_ast() {
    for file in ["snake_state.clv", "snake_terminal.clv"] {
        assert_roundtrip_ast(file);
    }
}

#[test]
fn formatter_is_idempotent_for_snake_examples() {
    for file in ["snake_state.clv", "snake_terminal.clv"] {
        let source = read_example(file);
        let options = FormatOptions::default();
        let formatted_once =
            format_source(&source, options.clone()).expect("first formatting failed");
        let formatted_twice =
            format_source(&formatted_once, options.clone()).expect("second formatting failed");
        assert_eq!(
            formatted_once, formatted_twice,
            "formatter must be idempotent for {}",
            file
        );
    }
}

#[test]
fn snake_state_variants_parse_without_errors() {
    for file in [
        "snake_state.clv",
        "snake_state_formatted.clv",
        "snake_terminal.clv",
    ] {
        let source = read_example(file);
        parse_forms(&source);
    }
}

fn assert_roundtrip_ast(file: &str) {
    let source = read_example(file);
    let original_forms = parse_forms(&source);
    let formatted =
        format_source(&source, FormatOptions::default()).expect("failed to format source");
    let formatted_forms = parse_forms(&formatted);
    assert_forms_equivalent(&original_forms, &formatted_forms, file);
}

#[test]
fn snake_state01_comments_stay_with_bindings() {
    let source = read_example("snake_state01.clv");
    let formatted =
        format_source(&source, FormatOptions::default()).expect("failed to format snake_state01");
    let collision_line_ok = formatted.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.starts_with("collision ")
            && trimmed.contains("(detect-collision new-head tail board)")
    });
    let collision_comment_detached = formatted.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.starts_with("collision")
            && trimmed.contains(';')
            && !trimmed.contains("(detect-collision new-head tail board)")
    });
    assert!(
        collision_line_ok,
        "collision binding should keep its expression on the same block: {}",
        formatted
    );
    assert!(
        !collision_comment_detached,
        "collision binding should not split into symbol-only comment line: {}",
        formatted
    );
    let board_comment_detached = formatted.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.starts_with("board") && trimmed.contains(';')
    });
    assert!(
        !board_comment_detached,
        "board argument should not gain stray inline comment: {}",
        formatted
    );
}

#[test]
fn formatter_aligns_let_with_inline_comments() {
    let source =
        "(let [a 1  ; a-comment\n         long-name 2 ; long\n         c (+ 1 2)] ; sum\n  body)";
    let expected = "(let [a          1 ; a-comment\n      long-name  2 ; long\n      c          (+ 1 2)] ; sum\n  body)";
    let formatted = format_source(source, FormatOptions::default())
        .expect("failed to format let with comments");
    assert_eq!(
        formatted.trim_end(),
        expected,
        "let bindings should align names/values without moving inline comments"
    );
}

#[test]
fn formatter_aligns_match_with_inline_comments() {
    let source = "(match dir\n  DirectionUp    (Position 0 -1)  ; up\n  DirectionDown  (Position 0 1)   ; down\n  DirectionLeft  (Position -1 0)  ; left\n  DirectionRight (Position 1 0))  ; right";
    let expected = "(match dir\n  DirectionUp     (Position 0 -1) ; up\n  DirectionDown   (Position 0 1) ; down\n  DirectionLeft   (Position -1 0) ; left\n  DirectionRight  (Position 1 0)) ; right";
    let formatted = format_source(source, FormatOptions::default())
        .expect("failed to format match with comments");
    assert_eq!(
        formatted.trim_end(),
        expected,
        "match clauses should align and keep inline comments at line ends"
    );
}

#[test]
fn formatter_keeps_match_guards_inline() {
    let source = "(match evt\n  (TickEvent {:time,})  (agent-send! state (update ? :tick (fnil inc 0)))\n  (KeyEvent {:key,}) :when (= key \"q\") :as whole  (agent-send! state (assoc ? :gameover true))\n  (KeyEvent {:key,})    (agent-send! state (update ? :key (fnil conj []) key))\n  _                     (agent-send! state (update ? :nil (fnil inc 0))))";
    let expected = "(match evt\n  (TickEvent {:time,})  (agent-send! state\n                          (update\n                            ?\n                            :tick\n                            (fnil inc 0)))\n  (KeyEvent {:key,}) :when (= key \"q\") :as whole  (agent-send! state (assoc ? :gameover true))\n  (KeyEvent {:key,})    (agent-send! state\n                          (update\n                            ?\n                            :key\n                            (fnil conj [])\n                            key))\n  _                     (agent-send! state\n                          (update\n                            ?\n                            :nil\n                            (fnil inc 0))))\n";
    let formatted = format_source(source, FormatOptions::default())
        .expect("failed to format match with guard/as");
    assert_eq!(
        formatted, expected,
        "match clause with :when/:as should stay inline when possible"
    );
}

#[test]
fn formatter_matches_pretty_for_control_forms_without_comments() {
    let source = "(let [a 1 b 2]\n  (match x\n    0 1\n    1 2)\n  (cond true a false b))";
    let pretty =
        pretty_print_source(source, PrettyOptions::default()).expect("pretty print failed");
    let formatted =
        format_source(source, FormatOptions::default()).expect("formatter failed on control forms");
    assert_eq!(
        formatted, pretty,
        "formatter and pretty printer should agree on commentless layout"
    );
}

#[test]
fn snake_state01_comments_do_not_jump_lines() {
    let source = read_example("snake_state01.clv");
    let formatted = format_source(&source, FormatOptions::default()).expect("format snake_state01");
    parse_forms(&formatted);
    assert!(
        formatted.contains("tail ;; Otherwise length stays the same"),
        "inline comment near tail should stay on the same line: {}",
        formatted
    );
}

#[test]
fn keeps_single_blank_line_before_comment_block() {
    let src = "(defn foo [] 1)\n\n;; comment about bar\n(defn bar [] 2)";
    let expected = "(defn foo [] 1)\n\n;; comment about bar\n(defn bar [] 2)\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn formats_thread_macros_as_pipeline() {
    let src = "(-> state (assoc :input key) (assoc :key-history history))";
    let expected = "(-> state (assoc :input key) (assoc :key-history history))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn formatter_aligns_map_destructuring_with_as() {
    let src = "(let [{:cancel! cancel! :await await :cancelled? cancelled? :cancel-chan cancel-chan :task task :children children :as handle} (async-scope)] handle)";
    let expected = "(let [{:cancel!     cancel!\n       :await       await\n       :cancelled?  cancelled?\n       :cancel-chan cancel-chan\n       :task        task\n       :children    children\n       :as          handle}\n       (async-scope)]\n  handle)\n";
    let mut options = FormatOptions::default();
    options.shorthand_map = false;
    let out = format_source(src, options).expect("failed to format source");
    assert_eq!(out, expected);
}

#[test]
fn formatter_does_not_shorthand_special_keys() {
    let src = "{:keys keys :as as :x x}";
    let expected = "{:keys keys :as as :x,}\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn formats_nested_thread_pipeline_inside_join_lines() {
    let src =
        "(join-lines (-> base-lines (into per-kind-lines) (into event-lines) (into footer-lines)))";
    let expected =
        "(join-lines (-> base-lines (into per-kind-lines) (into event-lines) (into footer-lines)))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn allows_inline_small_fn_as_argument() {
    let src = "(agent-send! state-agent (fn [state] (record-key-input state key)))";
    let expected = "(agent-send! state-agent (fn [state] (record-key-input state key)))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn breaks_large_vector_into_one_item_per_line() {
    let src = "[\"Key history (oldest -> newest):\" (str \"  \" history) \"\" (str \"Latest key: \" (or latest \"<none>\"))]";
    let expected = "[\"Key history (oldest -> newest):\"\n (str \"  \" history)\n \"\"\n (str \"Latest key: \" (or latest \"<none>\"))]\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn formats_nested_config_map_in_multiline_style() {
    let src = "(let [config {:board (Board {:width 20 :height 15}) :tick-ms 120ms}] config)";
    let formatted = fmt(src);
    assert!(
        formatted.contains("{:board") && formatted.contains(":height 15"),
        "formatted map should stay multiline: {}",
        formatted
    );
    assert!(
        !formatted.contains("{:board (Board {:width 20 :height 15}) :tick-ms 120ms}"),
        "nested map should not stay inline"
    );
}

#[test]
fn formatter_is_idempotent_for_additional_examples() {
    for file in [
        "snake_state01.clv",
        "snake_state01_formatted2.clv",
        "terminal1.clv",
        "terminal1_formatted.clv",
    ] {
        let source = read_example(file);
        let options = FormatOptions::default();
        let formatted_once =
            format_source(&source, options.clone()).expect("first formatting failed");
        let formatted_twice =
            format_source(&formatted_once, options.clone()).expect("second formatting failed");
        assert_eq!(
            formatted_once, formatted_twice,
            "formatter must be idempotent for {}",
            file
        );
    }
}

#[test]
fn snake_state01_match_comments_are_kept() {
    let source = read_example("snake_state01.clv");
    let formatted = fmt(&source);
    assert!(
        formatted.contains("Wall or self collision -> transition to GameOver"),
        "match-case comment should be preserved"
    );
    assert!(
        formatted.contains("When Title/Paused/GameOver, ticking does not change World"),
        "post-match comment should be preserved"
    );
}

#[test]
fn doseq_header_stays_inline_when_short() {
    let src = "(let [last-idx (dec (count lines))]\n  (doseq [[idx line] (map-indexed vector lines)]\n    (printf \"{}{}\" line clear-line)\n    (when (< idx last-idx)\n      (printf \"\\r\\n\"))))";
    let formatted = fmt(src);
    assert!(
        formatted.contains("(doseq [[idx line] (map-indexed vector lines)]"),
        "doseq binding should stay on one line: {}",
        formatted
    );
}

#[test]
fn small_defn_stays_inline() {
    let src = "(defn enter-alt-screen [] (printf \"{}\" (str esc \"[?1049h\")))";
    let expected = "(defn enter-alt-screen [] (printf \"{}\" (str esc \"[?1049h\")))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn defn_with_block_body_keeps_param_vector_on_newline() {
    let src = "(defn assert-available\n  []\n  (when (nil? (resolve 'dummy::init))\n    (core::runtime-error \"nope\")))";
    let expected = "(defn assert-available\n  []\n  (when (nil? (resolve 'dummy::init))\n    (core::runtime-error \"nope\")))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn multi_arity_defn_clauses_are_not_inlined() {
    let src = "(defn f\n  ([x y]\n   (+ x y))\n  ([x]\n   x))";
    let expected = "(defn f\n  ([x y]\n    (+ x y))\n  ([x]\n    x))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn quote_shorthand_is_preserved() {
    let src = "(resolve 'dummy::init)";
    let expected = "(resolve 'dummy::init)\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn vector_elements_align_without_extra_indent() {
    let src = "[\"Key history (oldest -> newest):\"\n  (str \"  \" history)\n  \"\"\n  \"Latest state:\"\n  (str state)]";
    let expected = "[\"Key history (oldest -> newest):\"\n (str \"  \" history)\n \"\"\n \"Latest state:\"\n (str state)]\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn let_binding_keeps_small_vector_of_vectors_inline() {
    let src = "(let [snake [[8 8] [7 8] [6 8]]\n      {:keys [seed pos]} (spawn-food 7 snake)]\n  [snake seed pos])";
    let formatted = fmt(src);
    let snake_inline = formatted
        .lines()
        .any(|line| line.contains("[snake") && line.contains("[[8 8] [7 8] [6 8]]"));
    assert!(
        snake_inline,
        "vector-of-vectors binding should stay inline: {}",
        formatted
    );
}

#[test]
fn let_bindings_align_columns_even_with_control_forms() {
    let src = "(let [lines (vec (debug-lines state))\n      {:keys [h]} (glyph-size (get pixel-font \"0\"))\n      line-height (+ h line-spacing)\n      content-h (if (empty? lines) 0 (+ h (* (dec (count lines)) line-height)))]\n  body)";
    let expected = "(let [lines        (vec (debug-lines state))\n      {:keys [h]}  (glyph-size (get pixel-font \"0\"))\n      line-height  (+ h line-spacing)\n      content-h    (if (empty? lines) 0 (+ h (* (dec (count lines)) line-height)))]\n  body)\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn nested_map_values_align() {
    let src = "{:initial-state {:key-history [] :input nil}\n :poll-interval 33ms}";
    let expected = "{:initial-state {:key-history [] :input nil}\n :poll-interval 33ms}\n";
    assert_eq!(fmt(src), expected, "nested map should format predictably");
}

#[test]
fn complex_short_fn_in_map_is_not_inlined() {
    let src = "(defn make-stop-signal [running]\n  (let [delivered  (atom false)\n        promise    (promise)]\n    {:deliver #(when (not @delivered)\n                  (atom-set! delivered true)\n                  (request-stop! running)\n                  (promise-deliver! promise true))\n     :await   #(promise-deref promise)}))";
    let expected = "(defn make-stop-signal [running]\n  (let [delivered  (atom false)\n        promise    (promise)]\n    {:deliver #(when (not @delivered)\n                 (atom-set! delivered true)\n                 (request-stop! running)\n                 (promise-deliver! promise true))\n     :await   #(promise-deref promise)}))\n";
    assert_eq!(fmt(src), expected);
}

#[test]
fn fn_arg_vector_keeps_destructuring_inline() {
    let src = "(defn build-frame-string [lines]\n  (let [body (reduce\n               (fn [acc [idx line]]\n                 (let [prefix (if (zero? idx) \"\" \"\\r\\n\")]\n                   (str acc prefix line clear-line)))\n               \"\"\n               (map-indexed vector lines))]\n    (str esc \"[H\" body \"\\r\\n\" clear-down)))";
    assert!(
        fmt(src).contains("(fn [acc [idx line]]"),
        "fn arg vector should stay inline with destructuring"
    );
}

#[test]
fn reader_deref_macro_renders_as_at_symbol() {
    let src = "(let [f  (future #(throw (runtime-error \"boom\")))]\n  @(promise-catch f (fn [_] :recovered)))";
    let formatted = fmt(src);
    assert!(
        formatted.contains("@(promise-catch"),
        "deref reader macro should stay as @ form: {}",
        formatted
    );
}
