use crate::formatter::{
    CommentSpacing, DanglingCommentPolicy, FlowHeadPolicy, FormatOptions, LineEnding,
};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
pub struct FmtConfig {
    pub version: Option<u32>,
    pub line: Option<FmtLineConfig>,
    pub inline: Option<FmtInlineConfig>,
    pub align: Option<FmtAlignConfig>,
    pub shorthand: Option<FmtShorthandConfig>,
    pub comments: Option<FmtCommentsConfig>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FmtLineConfig {
    pub width: Option<usize>,
    pub indent: Option<usize>,
    pub trailing_newline: Option<bool>,
    pub line_ending: Option<String>,
    pub preserve_blank_lines: Option<bool>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FmtInlineConfig {
    pub depth_limit: Option<usize>,
    pub map_max_entries: Option<usize>,
    pub map_max_entries_relaxed: Option<usize>,
    pub vector_max_items: Option<usize>,
    pub vector_max_items_relaxed: Option<usize>,
    pub set_max_items: Option<usize>,
    pub set_max_items_relaxed: Option<usize>,
    pub allow_nested_collections: Option<bool>,
    pub map_allow_complex_values: Option<bool>,
    pub max_width: Option<usize>,
    pub width_ratio: Option<f64>,
    pub map_inline_max_width: Option<usize>,
    pub map_inline_width_ratio: Option<f64>,
    pub flow_head_policy: Option<String>,
    pub flow_head_max_width: Option<usize>,
    pub flow_head_width_ratio: Option<f64>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FmtAlignConfig {
    pub let_bindings: Option<bool>,
    pub maps: Option<bool>,
    pub cond: Option<bool>,
    pub r#match: Option<bool>,
    pub inline_budget_multiplier: Option<usize>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FmtShorthandConfig {
    pub map: Option<bool>,
    pub map_exclude_keys: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct FmtCommentsConfig {
    pub preserve_commas: Option<bool>,
    pub preserve_trailing: Option<bool>,
    pub dangling_policy: Option<String>,
    pub spacing: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FmtConfigLoad {
    pub config: FmtConfig,
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtConfigResolved {
    pub version: u32,
    pub line: FmtLineResolved,
    pub inline: FmtInlineResolved,
    pub align: FmtAlignResolved,
    pub shorthand: FmtShorthandResolved,
    pub comments: FmtCommentsResolved,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtLineResolved {
    pub width: usize,
    pub indent: usize,
    pub trailing_newline: bool,
    pub line_ending: String,
    pub preserve_blank_lines: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtInlineResolved {
    pub depth_limit: usize,
    pub map_max_entries: usize,
    pub map_max_entries_relaxed: usize,
    pub vector_max_items: usize,
    pub vector_max_items_relaxed: usize,
    pub set_max_items: usize,
    pub set_max_items_relaxed: usize,
    pub allow_nested_collections: bool,
    pub map_allow_complex_values: bool,
    pub max_width: usize,
    pub width_ratio: f64,
    pub map_inline_max_width: usize,
    pub map_inline_width_ratio: f64,
    pub flow_head_policy: String,
    pub flow_head_max_width: usize,
    pub flow_head_width_ratio: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtAlignResolved {
    pub let_bindings: bool,
    pub maps: bool,
    pub cond: bool,
    pub r#match: bool,
    pub inline_budget_multiplier: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtShorthandResolved {
    pub map: bool,
    pub map_exclude_keys: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FmtCommentsResolved {
    pub preserve_commas: bool,
    pub preserve_trailing: bool,
    pub dangling_policy: String,
    pub spacing: String,
}

pub fn load_fmt_config_str(src: &str) -> Result<FmtConfigLoad, String> {
    let value: toml::Value = toml::from_str(src).map_err(|e| e.to_string())?;
    let warnings = collect_unknown_keys(&value);
    let config: FmtConfig = value.try_into().map_err(|e| e.to_string())?;
    config.validate_version()?;
    Ok(FmtConfigLoad { config, warnings })
}

pub fn load_fmt_config_path(path: &Path) -> Result<FmtConfigLoad, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
    load_fmt_config_str(&content)
}

impl FmtConfig {
    pub fn apply(&self, options: &mut FormatOptions) -> Result<(), String> {
        self.validate_version()?;
        if let Some(line) = &self.line {
            if let Some(width) = line.width {
                options.max_inline_chars = width.max(40);
            }
            if let Some(indent) = line.indent {
                options.indent_width = indent.max(1);
            }
            if let Some(trailing) = line.trailing_newline {
                options.line_trailing_newline = trailing;
            }
            if let Some(ending) = &line.line_ending {
                options.line_ending = parse_line_ending(ending)?;
            }
            if let Some(preserve_blank_lines) = line.preserve_blank_lines {
                options.line_preserve_blank_lines = preserve_blank_lines;
            }
        }
        if let Some(inline) = &self.inline {
            if let Some(depth) = inline.depth_limit {
                options.inline_depth_limit = depth;
            }
            if let Some(max_entries) = inline.map_max_entries {
                options.inline_map_max_entries = max_entries;
            }
            if let Some(max_entries) = inline.map_max_entries_relaxed {
                options.inline_map_max_entries_relaxed = max_entries;
            }
            if let Some(max_items) = inline.vector_max_items {
                options.inline_vector_max_items = max_items;
            }
            if let Some(max_items) = inline.vector_max_items_relaxed {
                options.inline_vector_max_items_relaxed = max_items;
            }
            if let Some(max_items) = inline.set_max_items {
                options.inline_set_max_items = max_items;
            }
            if let Some(max_items) = inline.set_max_items_relaxed {
                options.inline_set_max_items_relaxed = max_items;
            }
            if let Some(allow) = inline.allow_nested_collections {
                options.inline_allow_nested_collections = allow;
            }
            if let Some(allow) = inline.map_allow_complex_values {
                options.inline_map_allow_complex_values = allow;
            }
            if let Some(max_width) = inline.max_width {
                options.inline_max_width = max_width;
            }
            if let Some(ratio) = inline.width_ratio {
                if ratio < 0.0 {
                    return Err("inline.width_ratio must be >= 0".to_string());
                }
                options.inline_width_ratio = ratio as f32;
            }
            if let Some(max_width) = inline.map_inline_max_width {
                options.map_inline_max_width = max_width;
            }
            if let Some(ratio) = inline.map_inline_width_ratio {
                if ratio < 0.0 {
                    return Err("inline.map_inline_width_ratio must be >= 0".to_string());
                }
                options.map_inline_width_ratio = ratio as f32;
            }
            if let Some(policy) = &inline.flow_head_policy {
                options.flow_head_policy = parse_flow_head_policy(policy)?;
            }
            if let Some(max_width) = inline.flow_head_max_width {
                options.flow_head_max_width = max_width;
            }
            if let Some(ratio) = inline.flow_head_width_ratio {
                if ratio < 0.0 {
                    return Err("flow_head_width_ratio must be >= 0".to_string());
                }
                options.flow_head_width_ratio = ratio as f32;
            }
        }
        if let Some(align) = &self.align {
            if let Some(let_bindings) = align.let_bindings {
                options.align_let_bindings = let_bindings;
            }
            if let Some(maps) = align.maps {
                options.align_maps = maps;
            }
            if let Some(cond) = align.cond {
                options.align_cond = cond;
            }
            if let Some(r#match) = align.r#match {
                options.align_match = r#match;
            }
            if let Some(multiplier) = align.inline_budget_multiplier {
                options.align_inline_budget_multiplier = multiplier.max(1);
            }
        }
        if let Some(shorthand) = &self.shorthand {
            if let Some(map) = shorthand.map {
                options.shorthand_map = map;
            }
            if let Some(keys) = &shorthand.map_exclude_keys {
                options.shorthand_map_exclude_keys = keys.clone();
            }
        }
        if let Some(comments) = &self.comments {
            if let Some(preserve_commas) = comments.preserve_commas {
                options.preserve_commas = preserve_commas;
            }
            if let Some(preserve_trailing) = comments.preserve_trailing {
                options.comments_preserve_trailing = preserve_trailing;
            }
            if let Some(policy) = &comments.dangling_policy {
                options.comments_dangling_policy = parse_dangling_policy(policy)?;
            }
            if let Some(spacing) = &comments.spacing {
                options.comments_spacing = parse_comment_spacing(spacing)?;
            }
        }
        Ok(())
    }

    fn validate_version(&self) -> Result<(), String> {
        if let Some(version) = self.version {
            if version != 1 {
                return Err(format!("unsupported fmt config version: {}", version));
            }
        }
        Ok(())
    }
}

impl FmtConfigResolved {
    pub fn from_options(options: &FormatOptions) -> Self {
        Self {
            version: 1,
            line: FmtLineResolved {
                width: options.max_inline_chars,
                indent: options.indent_width,
                trailing_newline: options.line_trailing_newline,
                line_ending: options.line_ending.as_str().to_string(),
                preserve_blank_lines: options.line_preserve_blank_lines,
            },
            inline: FmtInlineResolved {
                depth_limit: options.inline_depth_limit,
                map_max_entries: options.inline_map_max_entries,
                vector_max_items_relaxed: options.inline_vector_max_items_relaxed,
                map_max_entries_relaxed: options.inline_map_max_entries_relaxed,
                set_max_items_relaxed: options.inline_set_max_items_relaxed,
                vector_max_items: options.inline_vector_max_items,
                set_max_items: options.inline_set_max_items,
                allow_nested_collections: options.inline_allow_nested_collections,
                map_allow_complex_values: options.inline_map_allow_complex_values,
                max_width: options.inline_max_width,
                width_ratio: options.inline_width_ratio as f64,
                map_inline_max_width: options.map_inline_max_width,
                map_inline_width_ratio: options.map_inline_width_ratio as f64,
                flow_head_policy: flow_head_policy_to_str(options.flow_head_policy).to_string(),
                flow_head_max_width: options.flow_head_max_width,
                flow_head_width_ratio: options.flow_head_width_ratio as f64,
            },
            align: FmtAlignResolved {
                let_bindings: options.align_let_bindings,
                maps: options.align_maps,
                cond: options.align_cond,
                r#match: options.align_match,
                inline_budget_multiplier: options.align_inline_budget_multiplier,
            },
            shorthand: FmtShorthandResolved {
                map: options.shorthand_map,
                map_exclude_keys: options.shorthand_map_exclude_keys.clone(),
            },
            comments: FmtCommentsResolved {
                preserve_commas: options.preserve_commas,
                preserve_trailing: options.comments_preserve_trailing,
                dangling_policy: dangling_policy_to_str(options.comments_dangling_policy)
                    .to_string(),
                spacing: comment_spacing_to_str(options.comments_spacing).to_string(),
            },
        }
    }
}

pub fn resolved_config_toml(options: &FormatOptions) -> Result<String, String> {
    let resolved = FmtConfigResolved::from_options(options);
    toml::to_string(&resolved).map_err(|e| e.to_string())
}

fn collect_unknown_keys(value: &toml::Value) -> Vec<String> {
    let mut warnings = Vec::new();
    let Some(table) = value.as_table() else {
        return warnings;
    };
    let allowed_top = [
        "version",
        "line",
        "inline",
        "align",
        "shorthand",
        "comments",
    ];
    for key in table.keys() {
        if !allowed_top.contains(&key.as_str()) {
            warnings.push(format!("unknown key: {}", key));
        }
    }
    collect_unknown_keys_table(
        table,
        "line",
        &[
            "width",
            "indent",
            "trailing_newline",
            "line_ending",
            "preserve_blank_lines",
        ],
        &mut warnings,
    );
    collect_unknown_keys_table(
        table,
        "inline",
        &[
            "depth_limit",
            "map_max_entries",
            "map_max_entries_relaxed",
            "vector_max_items",
            "vector_max_items_relaxed",
            "set_max_items",
            "set_max_items_relaxed",
            "allow_nested_collections",
            "map_allow_complex_values",
            "max_width",
            "width_ratio",
            "map_inline_max_width",
            "map_inline_width_ratio",
            "flow_head_policy",
            "flow_head_max_width",
            "flow_head_width_ratio",
        ],
        &mut warnings,
    );
    collect_unknown_keys_table(
        table,
        "align",
        &[
            "let_bindings",
            "maps",
            "cond",
            "match",
            "inline_budget_multiplier",
        ],
        &mut warnings,
    );
    collect_unknown_keys_table(
        table,
        "shorthand",
        &["map", "map_exclude_keys"],
        &mut warnings,
    );
    collect_unknown_keys_table(
        table,
        "comments",
        &[
            "preserve_commas",
            "preserve_trailing",
            "dangling_policy",
            "spacing",
        ],
        &mut warnings,
    );
    warnings
}

fn parse_line_ending(value: &str) -> Result<LineEnding, String> {
    match value {
        "auto" => Ok(LineEnding::Auto),
        "lf" => Ok(LineEnding::Lf),
        "crlf" => Ok(LineEnding::Crlf),
        _ => Err(format!("invalid line_ending value: {}", value)),
    }
}

fn parse_dangling_policy(value: &str) -> Result<DanglingCommentPolicy, String> {
    match value {
        "own_line" => Ok(DanglingCommentPolicy::OwnLine),
        "attach_prev" => Ok(DanglingCommentPolicy::AttachPrev),
        "attach_next" => Ok(DanglingCommentPolicy::AttachNext),
        _ => Err(format!("invalid dangling_policy value: {}", value)),
    }
}

fn dangling_policy_to_str(value: DanglingCommentPolicy) -> &'static str {
    match value {
        DanglingCommentPolicy::OwnLine => "own_line",
        DanglingCommentPolicy::AttachPrev => "attach_prev",
        DanglingCommentPolicy::AttachNext => "attach_next",
    }
}

fn parse_comment_spacing(value: &str) -> Result<CommentSpacing, String> {
    match value {
        "single" => Ok(CommentSpacing::Single),
        "preserve" => Ok(CommentSpacing::Preserve),
        _ => Err(format!("invalid comments.spacing value: {}", value)),
    }
}

fn comment_spacing_to_str(value: CommentSpacing) -> &'static str {
    match value {
        CommentSpacing::Single => "single",
        CommentSpacing::Preserve => "preserve",
    }
}

fn parse_flow_head_policy(value: &str) -> Result<FlowHeadPolicy, String> {
    match value {
        "multiline" => Ok(FlowHeadPolicy::Multiline),
        "inline_if_fit" => Ok(FlowHeadPolicy::InlineIfFit),
        _ => Err(format!("invalid flow_head_policy value: {}", value)),
    }
}

fn flow_head_policy_to_str(value: FlowHeadPolicy) -> &'static str {
    match value {
        FlowHeadPolicy::Multiline => "multiline",
        FlowHeadPolicy::InlineIfFit => "inline_if_fit",
    }
}

fn collect_unknown_keys_table(
    table: &toml::value::Table,
    name: &str,
    allowed: &[&str],
    warnings: &mut Vec<String>,
) {
    let Some(value) = table.get(name) else {
        return;
    };
    let Some(subtable) = value.as_table() else {
        return;
    };
    for key in subtable.keys() {
        if !allowed.contains(&key.as_str()) {
            warnings.push(format!("unknown key: {}.{}", name, key));
        }
    }
}
