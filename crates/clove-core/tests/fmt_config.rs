use clove_core::fmt_config::load_fmt_config_str;
use clove_core::formatter::{
    CommentSpacing, DanglingCommentPolicy, FlowHeadPolicy, FormatOptions, LineEnding,
};

#[test]
fn fmt_config_applies_values() {
    let src = r#"
version = 1

[line]
width = 88
indent = 4
trailing_newline = false
line_ending = "crlf"
preserve_blank_lines = true

[inline]
depth_limit = 5
map_max_entries = 4
map_max_entries_relaxed = 7
vector_max_items = 5
vector_max_items_relaxed = 8
set_max_items = 6
set_max_items_relaxed = 9
allow_nested_collections = false
map_allow_complex_values = true
max_width = 70
width_ratio = 0.6
map_inline_max_width = 50
map_inline_width_ratio = 0.4
flow_head_policy = "inline_if_fit"
flow_head_max_width = 60
flow_head_width_ratio = 0.5

[align]
let_bindings = false
maps = false
cond = false
match = false
inline_budget_multiplier = 3

[shorthand]
map = false
map_exclude_keys = ["keys", "as", "id"]

[comments]
preserve_commas = false
preserve_trailing = false
dangling_policy = "attach_prev"
spacing = "preserve"
"#;
    let loaded = load_fmt_config_str(src).expect("load config");
    let mut options = FormatOptions::default();
    loaded.config.apply(&mut options).expect("apply config");
    assert_eq!(options.max_inline_chars, 88);
    assert_eq!(options.indent_width, 4);
    assert!(!options.line_trailing_newline);
    assert_eq!(options.line_ending, LineEnding::Crlf);
    assert!(options.line_preserve_blank_lines);
    assert_eq!(options.inline_depth_limit, 5);
    assert_eq!(options.inline_map_max_entries, 4);
    assert_eq!(options.inline_map_max_entries_relaxed, 7);
    assert_eq!(options.inline_vector_max_items, 5);
    assert_eq!(options.inline_vector_max_items_relaxed, 8);
    assert_eq!(options.inline_set_max_items, 6);
    assert_eq!(options.inline_set_max_items_relaxed, 9);
    assert!(!options.inline_allow_nested_collections);
    assert!(options.inline_map_allow_complex_values);
    assert_eq!(options.inline_max_width, 70);
    assert!((options.inline_width_ratio - 0.6).abs() < f32::EPSILON);
    assert_eq!(options.map_inline_max_width, 50);
    assert!((options.map_inline_width_ratio - 0.4).abs() < f32::EPSILON);
    assert_eq!(options.flow_head_policy, FlowHeadPolicy::InlineIfFit);
    assert_eq!(options.flow_head_max_width, 60);
    assert!((options.flow_head_width_ratio - 0.5).abs() < f32::EPSILON);
    assert!(!options.align_let_bindings);
    assert!(!options.align_maps);
    assert!(!options.align_cond);
    assert!(!options.align_match);
    assert_eq!(options.align_inline_budget_multiplier, 3);
    assert!(!options.shorthand_map);
    assert_eq!(
        options.shorthand_map_exclude_keys,
        vec!["keys".to_string(), "as".to_string(), "id".to_string()]
    );
    assert!(!options.preserve_commas);
    assert!(!options.comments_preserve_trailing);
    assert_eq!(
        options.comments_dangling_policy,
        DanglingCommentPolicy::AttachPrev
    );
    assert_eq!(options.comments_spacing, CommentSpacing::Preserve);
}

#[test]
fn fmt_config_warns_on_unknown_keys() {
    let src = r#"
version = 1
unknown = 1

[line]
width = 100
extra = 2
"#;
    let loaded = load_fmt_config_str(src).expect("load config");
    assert!(loaded
        .warnings
        .contains(&"unknown key: unknown".to_string()));
    assert!(loaded
        .warnings
        .contains(&"unknown key: line.extra".to_string()));
}

#[test]
fn fmt_config_rejects_unsupported_version() {
    let err = load_fmt_config_str("version = 2").expect_err("reject version");
    assert!(err.contains("unsupported fmt config version"));
}
