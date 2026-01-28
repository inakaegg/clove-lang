use crate::ast::{Form, FormKind, InterpolatedPart, MapItem};
use crate::compiler::APPLY_SYM;
use crate::form_to_string::form_to_string;
use crate::pretty_print::{preformat_foreign_blocks, PrettyOptions};
use crate::reader::{
    CommentTrivia, Reader, MAP_REF_SYM, OOP_AS_SYM, OOP_BARE_SYM, OOP_DOT_STAGE_SYM, OOP_INDEX_SYM,
    OOP_LET_SYM, OOP_NIL_SAFE_SYM, OOP_SYNTAX_SYM,
};
use crate::type_syntax::normalize_type_syntax_forms;
use crate::types::TypeHintStyle;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

pub use crate::pretty_print::ForeignFormatter;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LineEnding {
    Auto,
    Lf,
    Crlf,
}

impl LineEnding {
    fn resolve(self, source: Option<LineEnding>) -> LineEnding {
        match self {
            LineEnding::Auto => source.unwrap_or(LineEnding::Lf),
            other => other,
        }
    }

    pub(crate) fn as_str(self) -> &'static str {
        match self {
            LineEnding::Auto => "auto",
            LineEnding::Lf => "lf",
            LineEnding::Crlf => "crlf",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DanglingCommentPolicy {
    OwnLine,
    AttachPrev,
    AttachNext,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentSpacing {
    Single,
    Preserve,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlowHeadPolicy {
    Multiline,
    InlineIfFit,
}

#[derive(Clone)]
pub struct FormatOptions {
    pub indent_width: usize,
    pub max_inline_chars: usize,
    pub foreign_formatter: Option<Arc<ForeignFormatter>>,
    pub line_trailing_newline: bool,
    pub line_preserve_blank_lines: bool,
    pub line_ending: LineEnding,
    pub inline_depth_limit: usize,
    pub inline_map_max_entries: usize,
    pub inline_map_max_entries_relaxed: usize,
    pub inline_vector_max_items: usize,
    pub inline_vector_max_items_relaxed: usize,
    pub inline_set_max_items: usize,
    pub inline_set_max_items_relaxed: usize,
    pub inline_allow_nested_collections: bool,
    pub inline_map_allow_complex_values: bool,
    pub inline_max_width: usize,
    pub inline_width_ratio: f32,
    pub map_inline_max_width: usize,
    pub map_inline_width_ratio: f32,
    pub flow_head_policy: FlowHeadPolicy,
    pub flow_head_max_width: usize,
    pub flow_head_width_ratio: f32,
    pub align_let_bindings: bool,
    pub align_maps: bool,
    pub align_cond: bool,
    pub align_match: bool,
    pub align_inline_budget_multiplier: usize,
    pub shorthand_map: bool,
    pub shorthand_map_exclude_keys: Vec<String>,
    pub preserve_commas: bool,
    pub comments_preserve_trailing: bool,
    pub comments_dangling_policy: DanglingCommentPolicy,
    pub comments_spacing: CommentSpacing,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            indent_width: 2,
            max_inline_chars: 120,
            foreign_formatter: None,
            line_trailing_newline: true,
            line_preserve_blank_lines: false,
            line_ending: LineEnding::Auto,
            inline_depth_limit: 3,
            inline_map_max_entries: 3,
            inline_map_max_entries_relaxed: 4,
            inline_vector_max_items: 4,
            inline_vector_max_items_relaxed: 6,
            inline_set_max_items: 0,
            inline_set_max_items_relaxed: 6,
            inline_allow_nested_collections: true,
            inline_map_allow_complex_values: false,
            inline_max_width: 80,
            inline_width_ratio: 0.0,
            map_inline_max_width: 0,
            map_inline_width_ratio: 0.0,
            flow_head_policy: FlowHeadPolicy::InlineIfFit,
            flow_head_max_width: 80,
            flow_head_width_ratio: 0.0,
            align_let_bindings: true,
            align_maps: true,
            align_cond: true,
            align_match: true,
            align_inline_budget_multiplier: 2,
            shorthand_map: true,
            shorthand_map_exclude_keys: vec!["keys".to_string(), "as".to_string()],
            preserve_commas: true,
            comments_preserve_trailing: true,
            comments_dangling_policy: DanglingCommentPolicy::OwnLine,
            comments_spacing: CommentSpacing::Single,
        }
    }
}

impl FormatOptions {
    pub(crate) fn as_pretty_options(&self) -> PrettyOptions {
        PrettyOptions {
            indent_width: self.indent_width,
            max_inline_chars: self.max_inline_chars,
            foreign_formatter: self.foreign_formatter.clone(),
        }
    }
}

#[derive(Default)]
struct CommentTable {
    leading: BTreeMap<usize, Vec<String>>,
    dangling: BTreeMap<usize, Vec<String>>,
    inline_trailing: BTreeMap<usize, Vec<String>>,
    opening_inline: BTreeMap<usize, Vec<String>>,
    leading_blank: BTreeSet<usize>,
    force_inline: BTreeSet<usize>,
    trailing: Vec<String>,
    trailing_commas: BTreeSet<usize>,
    blank_lines: BTreeSet<usize>,
    string_escapes: Option<BTreeSet<usize>>,
}

impl CommentTable {
    fn from_source(
        forms: &[Form],
        comments: Vec<CommentTrivia>,
        string_escapes: BTreeSet<usize>,
        commas: BTreeSet<usize>,
        blank_lines: BTreeSet<usize>,
        dangling_policy: DanglingCommentPolicy,
    ) -> Self {
        let mut table = CommentTable {
            string_escapes: Some(string_escapes),
            trailing_commas: commas,
            blank_lines,
            ..CommentTable::default()
        };
        if comments.is_empty() {
            return table;
        }
        let mut form_entries = Vec::new();
        collect_form_indices(forms, None, &mut form_entries);
        form_entries.sort_by_key(|(idx, _, _)| *idx);
        let mut form_indices = Vec::with_capacity(form_entries.len());
        let mut parent_map = BTreeMap::new();
        let mut line_map = BTreeMap::new();
        for (idx, parent, line) in form_entries {
            form_indices.push(idx);
            parent_map
                .entry(idx)
                .and_modify(|existing: &mut Option<usize>| {
                    if existing.is_some() && parent.is_none() {
                        *existing = parent;
                    }
                })
                .or_insert(parent);
            line_map.entry(idx).or_insert(line);
        }
        if form_indices.is_empty() {
            table.trailing = comments.into_iter().map(|c| c.text).collect();
            return table;
        }
        let mut sorted_comments = comments;
        sorted_comments.sort_by_key(|c| c.span.index);
        for comment in sorted_comments {
            if comment.after_opening_delim {
                if let Some(container) = comment.container {
                    if is_opening_inline_comment(&comment.text) {
                        table
                            .opening_inline
                            .entry(container)
                            .or_default()
                            .push(comment.text);
                        continue;
                    }
                    if let Some(next) = find_next_form_index_in_container(
                        &form_indices,
                        &parent_map,
                        comment.span.index,
                        Some(container),
                    ) {
                        if comment.leading_blank {
                            table.leading_blank.insert(next);
                        }
                        table.leading.entry(next).or_default().push(comment.text);
                    } else {
                        if comment.leading_blank {
                            table.leading_blank.insert(container);
                        }
                        table
                            .dangling
                            .entry(container)
                            .or_default()
                            .push(comment.text);
                    }
                    continue;
                }
            }
            if comment.inline {
                if let Some(trailing) = comment.trailing_form {
                    table.add_inline_trailing_comment(trailing, &comment);
                    continue;
                }
                if let Some(prev) = find_prev_form_index_same_line(
                    &form_indices,
                    &parent_map,
                    &line_map,
                    comment.span.index,
                    comment.span.line,
                    comment.container,
                ) {
                    table.add_inline_trailing_comment(prev, &comment);
                    continue;
                }
                if let Some(prev) = find_prev_form_index_same_line_any_parent(
                    &form_indices,
                    &line_map,
                    comment.span.index,
                    comment.span.line,
                ) {
                    table.add_inline_trailing_comment(prev, &comment);
                    continue;
                }
                if let Some(next) = find_next_form_index_same_line(
                    &form_indices,
                    &parent_map,
                    &line_map,
                    comment.span.index,
                    comment.span.line,
                    comment.container,
                ) {
                    table.add_inline_trailing_comment(next, &comment);
                    continue;
                }
                if let Some(next) = find_next_form_index_same_line_any_parent(
                    &form_indices,
                    &line_map,
                    comment.span.index,
                    comment.span.line,
                ) {
                    table.add_inline_trailing_comment(next, &comment);
                    continue;
                }
                if let Some(nearest) = find_nearest_form_index(
                    &form_indices,
                    &parent_map,
                    comment.span.index,
                    comment.container,
                ) {
                    table.add_inline_trailing_comment(nearest, &comment);
                    continue;
                }
                if let Some(prev) = find_prev_form_index(
                    &form_indices,
                    &parent_map,
                    comment.span.index,
                    comment.container,
                ) {
                    table.add_inline_trailing_comment(prev, &comment);
                    continue;
                }
            }
            if let Some(target) = find_next_form_index(&form_indices, comment.span.index) {
                if comment.leading_blank {
                    table.leading_blank.insert(target);
                }
                table.leading.entry(target).or_default().push(comment.text);
            } else if let Some(container) = comment.container {
                match dangling_policy {
                    DanglingCommentPolicy::AttachPrev => {
                        if let Some(prev) = find_prev_form_index(
                            &form_indices,
                            &parent_map,
                            comment.span.index,
                            Some(container),
                        ) {
                            table
                                .inline_trailing
                                .entry(prev)
                                .or_default()
                                .push(comment.text);
                            continue;
                        }
                    }
                    DanglingCommentPolicy::AttachNext => {
                        if let Some(next) = find_next_form_index_in_container(
                            &form_indices,
                            &parent_map,
                            comment.span.index,
                            Some(container),
                        ) {
                            if comment.leading_blank {
                                table.leading_blank.insert(next);
                            }
                            table.leading.entry(next).or_default().push(comment.text);
                            continue;
                        }
                    }
                    DanglingCommentPolicy::OwnLine => {}
                }
                if comment.leading_blank {
                    table.leading_blank.insert(container);
                }
                table
                    .dangling
                    .entry(container)
                    .or_default()
                    .push(comment.text);
            } else {
                table.trailing.push(comment.text);
            }
        }
        table
    }

    fn has_comments(&self, index: usize) -> bool {
        self.leading.contains_key(&index)
            || self.dangling.contains_key(&index)
            || self.opening_inline.contains_key(&index)
    }

    fn take_leading(&mut self, index: usize) -> Option<Vec<String>> {
        self.leading.remove(&index)
    }

    fn take_leading_blank(&mut self, index: usize) -> bool {
        self.leading_blank.remove(&index)
    }

    fn take_dangling(&mut self, index: usize) -> Option<Vec<String>> {
        self.dangling.remove(&index)
    }

    fn take_inline_trailing(&mut self, index: usize) -> Option<Vec<String>> {
        self.inline_trailing.remove(&index)
    }

    fn take_opening_inline(&mut self, index: usize) -> Option<Vec<String>> {
        self.opening_inline.remove(&index)
    }

    fn has_inline_trailing(&self, index: usize) -> bool {
        self.inline_trailing.contains_key(&index)
    }

    fn has_dangling(&self, index: usize) -> bool {
        self.dangling.contains_key(&index)
    }

    fn has_opening_inline(&self, index: usize) -> bool {
        self.opening_inline.contains_key(&index)
    }

    fn force_inline(&self, index: usize) -> bool {
        self.force_inline.contains(&index)
    }

    fn add_inline_trailing_comment(&mut self, index: usize, comment: &CommentTrivia) {
        self.inline_trailing
            .entry(index)
            .or_default()
            .push(comment.text.clone());
        if is_force_inline_comment(&comment.text) {
            self.force_inline.insert(index);
        }
    }

    fn move_inline_trailing(&mut self, from: usize, to: usize) {
        let Some(lines) = self.inline_trailing.remove(&from) else {
            return;
        };
        let entry = self.inline_trailing.entry(to).or_default();
        for line in lines {
            if is_force_inline_comment(&line) {
                self.force_inline.insert(to);
            }
            entry.push(line);
        }
        self.force_inline.remove(&from);
    }

    fn has_trailing_comma(&self, index: usize) -> bool {
        self.trailing_commas.contains(&index)
    }

    fn take_trailing_comma(&mut self, index: usize) -> bool {
        self.trailing_commas.remove(&index)
    }

    fn take_blank_line(&mut self, index: usize) -> bool {
        self.blank_lines.remove(&index)
    }

    fn drain_trailing(&mut self) -> Vec<String> {
        std::mem::take(&mut self.trailing)
    }

    fn string_had_escape(&self, idx: usize) -> bool {
        match &self.string_escapes {
            Some(set) => set.contains(&idx),
            None => true,
        }
    }
}

fn collect_form_indices(
    forms: &[Form],
    parent: Option<usize>,
    out: &mut Vec<(usize, Option<usize>, usize)>,
) {
    for form in forms {
        collect_form_indices_from_form(form, parent, out);
    }
}

fn collect_form_indices_from_form(
    form: &Form,
    parent: Option<usize>,
    out: &mut Vec<(usize, Option<usize>, usize)>,
) {
    let idx = form.span.index;
    out.push((idx, parent, form.span.line));
    match &form.kind {
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            for child in items {
                collect_form_indices_from_form(child, Some(idx), out);
            }
        }
        FormKind::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    collect_form_indices_from_form(expr, Some(idx), out);
                }
            }
        }
        FormKind::InterpolatedRegex { parts, .. } => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    collect_form_indices_from_form(expr, Some(idx), out);
                }
            }
        }
        FormKind::Map(entries) => {
            for entry in entries {
                match entry {
                    MapItem::KeyValue(k, v) => {
                        collect_form_indices_from_form(k, Some(idx), out);
                        collect_form_indices_from_form(v, Some(idx), out);
                    }
                    MapItem::Spread(expr) => {
                        collect_form_indices_from_form(expr, Some(idx), out);
                    }
                }
            }
        }
        _ => {}
    }
}

fn find_next_form_index(form_indices: &[usize], comment_index: usize) -> Option<usize> {
    for &idx in form_indices {
        if idx > comment_index {
            return Some(idx);
        }
    }
    None
}

fn find_next_form_index_in_container(
    form_indices: &[usize],
    parent_map: &BTreeMap<usize, Option<usize>>,
    comment_index: usize,
    container: Option<usize>,
) -> Option<usize> {
    for &idx in form_indices {
        if idx <= comment_index {
            continue;
        }
        if parent_map.get(&idx).copied().flatten() == container {
            return Some(idx);
        }
    }
    None
}

fn find_next_form_index_same_line(
    form_indices: &[usize],
    parent_map: &BTreeMap<usize, Option<usize>>,
    line_map: &BTreeMap<usize, usize>,
    comment_index: usize,
    comment_line: usize,
    container: Option<usize>,
) -> Option<usize> {
    for &idx in form_indices {
        if idx <= comment_index {
            continue;
        }
        if parent_map.get(&idx).copied().flatten() != container {
            continue;
        }
        if let Some(line) = line_map.get(&idx) {
            if *line == comment_line {
                return Some(idx);
            }
        }
    }
    None
}

fn find_prev_form_index(
    form_indices: &[usize],
    parent_map: &BTreeMap<usize, Option<usize>>,
    comment_index: usize,
    container: Option<usize>,
) -> Option<usize> {
    for &idx in form_indices.iter().rev() {
        if idx >= comment_index {
            continue;
        }
        if parent_map.get(&idx).copied().flatten() == container {
            return Some(idx);
        }
    }
    None
}

fn find_nearest_form_index(
    form_indices: &[usize],
    parent_map: &BTreeMap<usize, Option<usize>>,
    comment_index: usize,
    container: Option<usize>,
) -> Option<usize> {
    let mut best: Option<(usize, usize)> = None; // (distance, idx)
    for &idx in form_indices {
        if parent_map.get(&idx).copied().flatten() != container {
            continue;
        }
        let dist = if idx > comment_index {
            idx - comment_index
        } else {
            comment_index - idx
        };
        if best.map(|(d, _)| dist < d).unwrap_or(true) {
            best = Some((dist, idx));
        }
    }
    best.map(|(_, idx)| idx)
}

fn find_prev_form_index_same_line(
    form_indices: &[usize],
    parent_map: &BTreeMap<usize, Option<usize>>,
    line_map: &BTreeMap<usize, usize>,
    comment_index: usize,
    comment_line: usize,
    container: Option<usize>,
) -> Option<usize> {
    for &idx in form_indices.iter().rev() {
        if idx >= comment_index {
            continue;
        }
        if parent_map.get(&idx).copied().flatten() != container {
            continue;
        }
        if let Some(line) = line_map.get(&idx) {
            if *line == comment_line {
                return Some(idx);
            }
        }
    }
    None
}

fn find_prev_form_index_same_line_any_parent(
    form_indices: &[usize],
    line_map: &BTreeMap<usize, usize>,
    comment_index: usize,
    comment_line: usize,
) -> Option<usize> {
    for &idx in form_indices.iter().rev() {
        if idx >= comment_index {
            continue;
        }
        if let Some(line) = line_map.get(&idx) {
            if *line == comment_line {
                return Some(idx);
            }
        }
    }
    None
}

fn find_next_form_index_same_line_any_parent(
    form_indices: &[usize],
    line_map: &BTreeMap<usize, usize>,
    comment_index: usize,
    comment_line: usize,
) -> Option<usize> {
    for &idx in form_indices {
        if idx <= comment_index {
            continue;
        }
        if let Some(line) = line_map.get(&idx) {
            if *line == comment_line {
                return Some(idx);
            }
        }
    }
    None
}

fn emit_leading_comments(
    index: usize,
    indent: usize,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    let needs_blank = comments.take_leading_blank(index);
    if let Some(lines) = comments.take_leading(index) {
        if needs_blank {
            ensure_blank_line(out);
        } else if !out.is_empty() {
            trim_trailing_whitespace(out);
            if !out.ends_with('\n') {
                out.push('\n');
            }
        }
        for line in lines {
            write_indent(indent, out);
            out.push_str(&line);
            out.push('\n');
        }
        true
    } else {
        false
    }
}

fn emit_opening_inline_comments(
    index: usize,
    indent: usize,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    if let Some(lines) = comments.take_opening_inline(index) {
        for (idx, line) in lines.into_iter().enumerate() {
            if idx == 0 {
                out.push_str(&line);
                out.push('\n');
                continue;
            }
            write_indent(indent, out);
            out.push_str(&line);
            out.push('\n');
        }
        true
    } else {
        false
    }
}

fn emit_dangling_comments(
    index: usize,
    indent: usize,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    let mut inserted = false;
    let needs_blank = comments.take_leading_blank(index);
    if let Some(lines) = comments.take_dangling(index) {
        if needs_blank {
            ensure_blank_line(out);
        } else if !out.ends_with('\n') {
            out.push('\n');
        }
        for line in lines {
            write_indent(indent, out);
            out.push_str(&line);
            out.push('\n');
        }
        inserted = true;
    }
    inserted
}

fn emit_trailing_comments(comments: &mut CommentTable, out: &mut String) {
    for line in comments.drain_trailing() {
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str(&line);
        out.push('\n');
    }
}

fn emit_inline_trailing_comments(
    index: usize,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    if comments.take_trailing_comma(index) {
        out.push(',');
    }
    if let Some(lines) = comments.take_inline_trailing(index) {
        if !options.comments_preserve_trailing {
            if !out.is_empty() && !out.ends_with('\n') {
                out.push('\n');
            }
            for line in lines {
                write_indent(indent, out);
                out.push_str(&line);
                out.push('\n');
            }
            return;
        }
        for (idx, line) in lines.into_iter().enumerate() {
            if idx == 0 {
                ensure_inline_comment_spacing(out, options.comments_spacing);
                out.push_str(&line);
                out.push('\n');
                continue;
            }
            write_indent(indent, out);
            out.push_str(&line);
            out.push('\n');
        }
    }
}

fn is_force_inline_comment(text: &str) -> bool {
    let trimmed = text.trim_start();
    !trimmed.is_empty() && trimmed.starts_with(';')
}

fn is_opening_inline_comment(text: &str) -> bool {
    let trimmed = text.trim_start();
    trimmed.starts_with(";;")
}

fn ensure_inline_comment_spacing(out: &mut String, spacing: CommentSpacing) {
    if out.ends_with('\n') || out.is_empty() {
        return;
    }
    match spacing {
        CommentSpacing::Single => {
            while out.ends_with(' ') || out.ends_with('\t') {
                out.pop();
            }
            out.push(' ');
        }
        CommentSpacing::Preserve => {
            if out.ends_with(' ') || out.ends_with('\t') {
                return;
            }
            out.push(' ');
        }
    }
}

fn trim_trailing_whitespace(out: &mut String) {
    while out.ends_with(' ') || out.ends_with('\t') {
        out.pop();
    }
}

fn push_space_or_indent(out: &mut String, indent: usize) {
    if out.is_empty() {
        return;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    } else {
        out.push(' ');
    }
}

fn ensure_newline(out: &mut String) {
    if !out.ends_with('\n') {
        out.push('\n');
    }
}

fn ensure_newline_for_form(form: &Form, comments: &mut CommentTable, out: &mut String) {
    ensure_newline(out);
    if comments.take_blank_line(form.span.index) {
        out.push('\n');
    }
}

fn ensure_blank_line(out: &mut String) {
    trim_trailing_whitespace(out);
    if out.is_empty() {
        out.push('\n');
        return;
    }
    if out.ends_with("\n\n") {
        return;
    }
    if out.ends_with('\n') {
        out.push('\n');
    } else {
        out.push('\n');
        out.push('\n');
    }
}

fn format_source_once(source: &str, options: &FormatOptions) -> Result<String, String> {
    let mut reader = Reader::new(source);
    let forms = reader.read_all().map_err(|e| e.to_string())?;
    let forms = normalize_type_syntax_forms(forms, false).map_err(|e| e.to_string())?;
    let string_escapes = reader.take_string_escapes();
    let commas = if options.preserve_commas {
        reader.take_commas()
    } else {
        BTreeSet::new()
    };
    let comments = reader.take_comments();
    let blank_lines = reader.take_top_level_blank_lines();
    let inner_blank_lines = if options.line_preserve_blank_lines {
        reader.take_inner_blank_lines()
    } else {
        BTreeSet::new()
    };
    let owned_forms;
    let pretty = options.as_pretty_options();
    let formatted_forms = if pretty.foreign_formatter.is_some() {
        owned_forms = preformat_foreign_blocks(&forms, &pretty);
        &owned_forms
    } else {
        &forms
    };
    let mut table = CommentTable::from_source(
        formatted_forms,
        comments,
        string_escapes,
        commas,
        inner_blank_lines,
        options.comments_dangling_policy,
    );
    Ok(format_forms_with_comments(
        formatted_forms,
        &options,
        &mut table,
        &blank_lines,
    ))
}

pub fn format_source(source: &str, options: FormatOptions) -> Result<String, String> {
    let source_line_ending = detect_line_ending(source);
    let mut current = format_source_once(source, &options)?;
    for _ in 0..2 {
        let next = format_source_once(&current, &options)?;
        if next == current {
            return Ok(finalize_output(current, &options, Some(source_line_ending)));
        }
        current = next;
    }
    Ok(finalize_output(current, &options, Some(source_line_ending)))
}

pub fn format_forms(forms: &[Form], options: &FormatOptions) -> String {
    let owned_forms;
    let pretty = options.as_pretty_options();
    let formatted_forms = if pretty.foreign_formatter.is_some() {
        owned_forms = preformat_foreign_blocks(forms, &pretty);
        &owned_forms
    } else {
        forms
    };
    let mut table = CommentTable::default();
    let blank_lines = BTreeSet::new();
    let out = format_forms_with_comments(formatted_forms, options, &mut table, &blank_lines);
    finalize_output(out, options, None)
}

fn format_forms_with_comments(
    forms: &[Form],
    options: &FormatOptions,
    comments: &mut CommentTable,
    blank_lines: &BTreeSet<usize>,
) -> String {
    let mut out = String::new();
    for (idx, form) in forms.iter().enumerate() {
        if blank_lines.contains(&form.span.index) {
            out.push('\n');
        }
        if idx > 0 && !out.ends_with('\n') {
            out.push('\n');
        }
        write_form(form, 0, options, comments, &mut out);
        if !out.ends_with('\n') {
            out.push('\n');
        }
    }
    emit_trailing_comments(comments, &mut out);
    out
}

fn finalize_output(
    mut out: String,
    options: &FormatOptions,
    source_line_ending: Option<LineEnding>,
) -> String {
    if !options.line_trailing_newline {
        trim_final_newline(&mut out);
    }
    let line_ending = options.line_ending.resolve(source_line_ending);
    apply_line_ending(out, line_ending)
}

fn trim_final_newline(out: &mut String) {
    if out.ends_with('\n') {
        out.pop();
    }
}

fn apply_line_ending(out: String, line_ending: LineEnding) -> String {
    match line_ending {
        LineEnding::Lf => out,
        LineEnding::Crlf => out.replace('\n', "\r\n"),
        LineEnding::Auto => out,
    }
}

fn detect_line_ending(source: &str) -> LineEnding {
    if source.contains("\r\n") {
        LineEnding::Crlf
    } else {
        LineEnding::Lf
    }
}

fn write_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let inserted_comment = emit_leading_comments(form.span.index, indent, comments, out);
    if inserted_comment {
        write_indent(indent, out);
    }
    if let FormKind::List(items) = &form.kind {
        if matches!(items.first().map(|f| &f.kind), Some(FormKind::Symbol(sym)) if sym == OOP_SYNTAX_SYM)
        {
            comments.move_inline_trailing(items[0].span.index, form.span.index);
        }
    }
    if comments.force_inline(form.span.index)
        && !form_has_internal_comments(form, comments, Some(form.span.index), Some(form.span.index))
    {
        if let Some(inline) = render_force_inline(form, options, &*comments) {
            out.push_str(&inline);
            append_type_hint(form, out);
            emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
            return;
        }
    }
    if let Some(accessor) = render_sharp_accessor(form, options, comments) {
        out.push_str(&accessor);
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    if write_dot_chain(form, indent, options, comments, out) {
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    if let Some(deref) = render_deref_expr(form, options, 0, &*comments) {
        out.push_str(&deref);
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    if write_oop_chain(form, indent, options, comments, out) {
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    if let Some(map_ref) = render_map_ref(form, options, 0, &*comments) {
        out.push_str(&map_ref);
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    if let Some(indexed) = render_index_expr(form, options, 0, &*comments) {
        out.push_str(&indexed);
        append_type_hint(form, out);
        emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        return;
    }
    match &form.kind {
        FormKind::Symbol(s) => out.push_str(s),
        FormKind::Keyword(k) => {
            out.push(':');
            out.push_str(k);
        }
        FormKind::Int(n) => out.push_str(&n.to_string()),
        FormKind::Float(n) => out.push_str(&format_float(*n)),
        FormKind::String(s) => {
            push_string_literal(out, s, comments.string_had_escape(form.span.index))
        }
        FormKind::InterpolatedString(parts) => {
            let escape_newlines = comments.string_had_escape(form.span.index);
            write_interpolated_string(parts, escape_newlines, indent, options, comments, out);
        }
        FormKind::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        FormKind::Nil => out.push_str("nil"),
        FormKind::Duration(d) => out.push_str(&d.to_string()),
        FormKind::ShortFn(body) => write_short_fn(body, indent, options, comments, out),
        FormKind::Regex { pattern, delim } => match delim {
            crate::ast::RegexDelim::Slash => {
                out.push('/');
                push_regex_content(out, pattern);
                out.push('/');
            }
            crate::ast::RegexDelim::Hash => {
                out.push_str("#\"");
                push_regex_content(out, pattern);
                out.push('"');
            }
            crate::ast::RegexDelim::HashSlash => {
                out.push_str("#/");
                push_regex_content(out, pattern);
                out.push('/');
            }
        },
        FormKind::InterpolatedRegex { parts, delim } => {
            write_interpolated_regex(parts, delim, indent, options, comments, out);
        }
        FormKind::List(_) => write_list(form, indent, options, comments, out),
        FormKind::Vector(_) => write_vector(form, indent, options, comments, out),
        FormKind::Set(_) => write_set(form, indent, options, comments, out),
        FormKind::Map(_) => write_map(form, indent, options, comments, out),
        FormKind::ForeignRaw { tag, code } => {
            write_foreign_with_prefix(tag.as_deref(), code, true, indent, options, out);
        }
        FormKind::ForeignBlock { tag, code } => {
            write_foreign_with_prefix(Some(tag.as_str()), code, false, indent, options, out);
        }
        FormKind::ForeignSymbol { tag, path } => {
            out.push('$');
            if let Some(t) = tag {
                out.push_str(t);
                out.push(':');
            }
            out.push_str(path);
        }
    }
    append_type_hint(form, out);
    emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
}

fn write_list(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() == 2 {
        if let Some(head) = list_head_symbol(items) {
            if head == "quote" {
                out.push('\'');
                write_form(&items[1], indent, options, comments, out);
                return;
            }
        }
    }
    if let Some(head) = list_head_symbol(items) {
        if is_defn_head(head) || is_named_method_head(head, items) {
            if let Some(inline) = inline_small_defn(head, items, options, 0, comments) {
                out.push_str(&inline);
                emit_dangling_comments(form.span.index, indent, comments, out);
                return;
            }
        }
        if head == "quote" && items.len() == 2 {
            out.push('\'');
            write_form(&items[1], indent, options, comments, out);
            emit_dangling_comments(form.span.index, indent, comments, out);
            return;
        }
    }
    let head_symbol = list_head_symbol(items);
    if matches!(head_symbol, Some("deref")) && items.len() == 2 {
        write_deref_form(form, indent, options, comments, out);
        return;
    }
    if let Some(head) = head_symbol {
        if write_map_sugar_form(head, form, indent, options, comments, out) {
            return;
        }
    }
    let mut force_multiline = head_symbol
        .map(|head| {
            is_let_like(head)
                || matches!(
                    head,
                    "if" | "when"
                        | "match"
                        | "cond"
                        | "try"
                        | "catch"
                        | "finally"
                        | "err"
                        | "fin"
                        | "deftype"
                        | "defenum"
                )
        })
        .unwrap_or(false);
    if let Some(head) = head_symbol {
        if is_flow_head(head) && options.flow_head_policy == FlowHeadPolicy::Multiline {
            force_multiline = true;
        }
    }
    if let Some(head) = head_symbol {
        if is_defn_head(head) || is_named_method_head(head, items) {
            force_multiline = true;
        }
    }
    if matches!(items.first().map(|f| &f.kind), Some(FormKind::Vector(_))) {
        force_multiline = true;
    }
    if write_keyword_arg_call(form, indent, options, comments, out) {
        return;
    }
    if !force_multiline && !comments.has_comments(form.span.index) {
        if let Some(inline) = inline_list(form, options, 0, comments) {
            out.push_str(&inline);
            return;
        }
    }
    if let Some(head) = head_symbol {
        if is_let_like(head) {
            write_let_like(form, indent, options, comments, out);
            return;
        }
        if is_thread_macro_head(head) {
            write_thread_form(head, form, indent, options, comments, out);
            return;
        }
        match head {
            "if" if items.len() >= 3 => {
                write_if_form(form, indent, options, comments, out);
                return;
            }
            "deref" if items.len() == 2 => {
                write_deref_form(form, indent, options, comments, out);
                return;
            }
            "when" if items.len() >= 2 => {
                write_when_form(form, indent, options, comments, out);
                return;
            }
            "fn" if items.len() >= 2 => {
                write_fn_form(form, indent, options, comments, out);
                return;
            }
            "match" if items.len() >= 4 && (items.len() - 2) % 2 == 0 => {
                write_match_form(form, indent, options, comments, out);
                return;
            }
            "cond" if items.len() >= 3 && (items.len() - 1) % 2 == 0 => {
                write_cond_form(form, indent, options, comments, out);
                return;
            }
            head if (is_defn_head(head) || is_named_method_head(head, items))
                && items.len() >= 3 =>
            {
                write_defn_form(head, form, indent, options, comments, out);
                return;
            }
            "def-foreign" if items.len() >= 2 => {
                write_def_foreign_form(form, indent, options, comments, out);
                return;
            }
            "doseq" | "each" if items.len() >= 2 => {
                write_doseq_form(head, form, indent, options, comments, out);
                return;
            }
            "for" if items.len() >= 2 => {
                write_doseq_form(head, form, indent, options, comments, out);
                return;
            }
            "deftype" if items.len() >= 3 => {
                write_deftype_form(form, indent, options, comments, out);
                return;
            }
            "defenum" if items.len() >= 3 => {
                write_defenum_form(form, indent, options, comments, out);
                return;
            }
            "try" if items.len() >= 2 => {
                write_try_form(form, indent, options, comments, out);
                return;
            }
            "assoc" if items.len() >= 4 => {
                if write_assoc_form(form, indent, options, comments, out) {
                    return;
                }
            }
            "finally" if items.len() >= 2 => {
                write_finally_form("finally", form, indent, options, comments, out);
                return;
            }
            "catch" if items.len() >= 2 => {
                write_finally_form("catch", form, indent, options, comments, out);
                return;
            }
            "fin" if items.len() >= 2 => {
                write_finally_form("fin", form, indent, options, comments, out);
                return;
            }
            "err" if items.len() >= 2 => {
                write_finally_form("err", form, indent, options, comments, out);
                return;
            }
            _ => {}
        }
    }
    if write_keyword_arg_call(form, indent, options, comments, out) {
        return;
    }
    write_head_list(form, indent, options, comments, out);
}

fn write_keyword_arg_call(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    if items.len() < 3 {
        return false;
    }
    if (items.len() - 1) % 2 != 0 {
        return false;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return false,
    };
    let mut key_texts: Vec<String> = Vec::new();
    for pair in items[1..].chunks(2) {
        let key = &pair[0];
        if comments.has_comments(key.span.index) || comments.has_inline_trailing(key.span.index) {
            return false;
        }
        match &key.kind {
            FormKind::Symbol(sym) if sym.ends_with(':') => key_texts.push(sym.clone()),
            _ => return false,
        }
    }
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    let max_key_len = key_texts.iter().map(|s| s.len()).max().unwrap_or(0);
    for (idx, pair) in items[1..].chunks(2).enumerate() {
        let key = &pair[0];
        let value = &pair[1];
        ensure_newline_for_form(key, comments, out);
        write_indent(child_indent, out);
        let inserted_comment = emit_leading_comments(key.span.index, child_indent, comments, out);
        if inserted_comment {
            write_indent(child_indent, out);
        }
        let key_text = &key_texts[idx];
        out.push_str(key_text);
        emit_inline_trailing_comments(key.span.index, child_indent, options, comments, out);
        let pad = max_key_len.saturating_sub(key_text.len()) + 1;
        for _ in 0..pad {
            out.push(' ');
        }
        let value_indent = current_column(out);
        write_form(value, value_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
    true
}

fn write_assoc_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    if !options.align_maps {
        return false;
    }
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    if items.len() < 4 {
        return false;
    }
    if (items.len() - 2) % 2 != 0 {
        return false;
    }
    let mut key_strings: Vec<String> = Vec::new();
    for pair in items[2..].chunks(2) {
        if let Some(text) = inline_form_ignore_comments(&pair[0], options, 0) {
            key_strings.push(text);
        } else {
            return false;
        }
    }
    let max_key_len = key_strings.iter().map(|s| s.len()).max().unwrap_or(0);
    out.push('(');
    out.push_str("assoc");
    let child_indent = block_child_indent(indent, "assoc".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        push_space_or_indent(out, child_indent);
        out.push_str(&inline);
    } else {
        ensure_newline(out);
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, comments, out);
    }
    for (idx, pair) in items[2..].chunks(2).enumerate() {
        let key = &pair[0];
        let value = &pair[1];
        ensure_newline_for_form(key, comments, out);
        write_indent(child_indent, out);
        let key_inline_comment = comments.has_inline_trailing(key.span.index);
        let inserted_comment = emit_leading_comments(key.span.index, child_indent, comments, out);
        if inserted_comment {
            write_indent(child_indent, out);
        }
        let key_text = &key_strings[idx];
        out.push_str(key_text);
        emit_inline_trailing_comments(key.span.index, child_indent, options, comments, out);
        if key_inline_comment {
            let value_indent = child_indent + options.indent_width;
            write_indent(value_indent, out);
            write_form(value, value_indent, options, comments, out);
        } else {
            let pad = max_key_len.saturating_sub(key_text.len()) + 1;
            for _ in 0..pad {
                out.push(' ');
            }
            let value_indent = current_column(out);
            write_form(value, value_indent, options, comments, out);
        }
    }
    let had_dangling = emit_dangling_comments(form.span.index, child_indent, comments, out);
    if had_dangling || out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    true
}

fn write_head_list(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        out.push_str("()");
        return;
    }
    out.push('(');
    let child_indent = indent + options.indent_width;
    let first_inline_comment = comments.has_inline_trailing(items[0].span.index);
    write_form(&items[0], child_indent, options, comments, out);
    if items.len() == 1 {
        out.push(')');
        return;
    }
    if items.len() == 2 {
        if let FormKind::Map(_) = items[1].kind {
            push_space_or_indent(out, child_indent);
            let map_indent = current_column(out);
            write_form(&items[1], map_indent, options, comments, out);
            out.push(')');
            return;
        }
    }

    let head_sym = list_head_symbol(items);
    let is_def_head = matches!(head_sym, Some("def") | Some("def-") | Some("-def"));
    let mut inline_mode = !first_inline_comment;
    if matches!(items.first().map(|f| &f.kind), Some(FormKind::Vector(_))) {
        inline_mode = false;
    }
    if matches!(head_sym, Some("and") | Some("or")) {
        inline_mode = false;
    }
    if items.len() > 3
        && items.iter().skip(1).any(|item| {
            matches!(
                item.kind,
                FormKind::List(_) | FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
            )
        })
    {
        inline_mode = false;
    }
    for (idx, item) in items.iter().enumerate().skip(1) {
        let item_inline_comment = comments.has_inline_trailing(item.span.index);
        if inline_mode {
            let inline = if is_def_head && idx == 1 {
                inline_binding_name(item, options, comments)
            } else {
                inline_form(item, options, 0, comments).or_else(|| {
                    if matches!(item.kind, FormKind::Map(_)) && !form_has_comments(item, comments) {
                        inline_map_relaxed(item, options, 0, comments)
                    } else {
                        None
                    }
                })
            };
            if let Some(inline) = inline {
                push_space_or_indent(out, child_indent);
                out.push_str(&inline);
                if item_inline_comment {
                    inline_mode = false;
                }
                continue;
            }
            if allow_inline_multiline_head_arg(head_sym, items, idx, item, comments) {
                push_space_or_indent(out, child_indent);
                let arg_indent = current_column(out);
                write_form(item, arg_indent, options, comments, out);
                if item_inline_comment {
                    inline_mode = false;
                }
                continue;
            }
            inline_mode = false;
        }
        ensure_newline_for_form(item, comments, out);
        write_indent(child_indent, out);
        if is_def_head && idx == 1 {
            write_binding_name(item, child_indent, options, comments, out);
        } else {
            write_form(item, child_indent, options, comments, out);
        }
        if item_inline_comment {
            inline_mode = false;
        }
    }
    let had_dangling = emit_dangling_comments(form.span.index, child_indent, comments, out);
    if had_dangling || out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn allow_inline_multiline_head_arg(
    head_sym: Option<&str>,
    items: &[Form],
    idx: usize,
    item: &Form,
    comments: &CommentTable,
) -> bool {
    if head_sym != Some("into") {
        return false;
    }
    if items.len() != 3 || idx != 2 {
        return false;
    }
    if !matches!(item.kind, FormKind::List(_)) {
        return false;
    }
    !form_has_comments(item, comments)
}

fn write_short_fn(
    body: &[Form],
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    if body.is_empty() {
        out.push_str("#()");
        return;
    }
    let complex = short_fn_has_complex_body(body);
    if complex {
        out.push('#');
        let list_form = Form {
            kind: FormKind::List(body.to_vec()),
            span: body[0].span,
            type_hint: None,
        };
        write_form(&list_form, indent, options, comments, out);
        return;
    }
    out.push_str("#(");
    let child_indent = indent + options.indent_width;
    let first_inline_comment = comments.has_inline_trailing(body[0].span.index);
    write_form(&body[0], child_indent, options, comments, out);
    if body.len() == 1 {
        if out.ends_with('\n') {
            write_indent(indent, out);
        }
        out.push(')');
        return;
    }
    let mut inline_mode = !first_inline_comment && !complex;
    for item in body.iter().skip(1) {
        let item_inline_comment = comments.has_inline_trailing(item.span.index);
        if inline_mode {
            if let Some(inline) = inline_form(item, options, 0, comments) {
                push_space_or_indent(out, child_indent);
                out.push_str(&inline);
                if item_inline_comment {
                    inline_mode = false;
                }
                continue;
            }
            inline_mode = false;
        }
        ensure_newline_for_form(item, comments, out);
        write_indent(child_indent, out);
        write_form(item, child_indent, options, comments, out);
        if item_inline_comment {
            inline_mode = false;
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
}

fn write_let_like(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.is_empty() {
        out.push_str("()");
        return;
    }
    out.push('(');
    let child_indent = indent + options.indent_width;
    write_form(&items[0], child_indent, options, comments, out);
    if items.len() == 1 {
        if out.ends_with('\n') {
            write_indent(indent, out);
        }
        out.push(')');
        return;
    }
    push_space_or_indent(out, child_indent);
    write_binding_vector(&items[1], indent, options, comments, out);
    for form in items.iter().skip(2) {
        ensure_newline_for_form(form, comments, out);
        write_indent(child_indent, out);
        write_form(form, child_indent, options, comments, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_if_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str("if");
    let child_indent = block_child_indent(indent, "if".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    if let Some(inline) = inline_form_relaxed(&items[1], options, 0, comments) {
        push_space_or_indent(out, child_indent);
        out.push_str(&inline);
    } else {
        ensure_newline(out);
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, comments, out);
    }
    for branch in items.iter().skip(2) {
        ensure_newline_for_form(branch, comments, out);
        write_indent(child_indent, out);
        if is_control_form(branch) {
            write_form(branch, child_indent, options, comments, out);
            continue;
        }
        if let Some(inline) = inline_form(branch, options, 0, comments) {
            out.push_str(&inline);
        } else {
            write_form(branch, child_indent, options, comments, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_finally_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if matches!(head, "err" | "fin") && items.len() == 2 && !form_has_comments(form, comments) {
        if let Some(body_inline) = inline_form(&items[1], options, 0, comments) {
            let mut buf = String::new();
            buf.push('(');
            buf.push_str(head);
            buf.push(' ');
            buf.push_str(&body_inline);
            buf.push(')');
            if buf.len() <= inline_length_limit(options) {
                out.push_str(&buf);
                emit_dangling_comments(form.span.index, indent, comments, out);
                return;
            }
        }
    }
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    for body in items.iter().skip(1) {
        ensure_newline_for_form(body, comments, out);
        write_indent(child_indent, out);
        write_form(body, child_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_when_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str("when");
    let child_indent = block_child_indent(indent, "when".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    let inline_cond = inline_form_relaxed(&items[1], options, 0, comments).or_else(|| {
        if comments.has_comments(items[1].span.index)
            || comments.has_inline_trailing(items[1].span.index)
        {
            return None;
        }
        inline_form_ignore_comments(&items[1], options, 0).or_else(|| {
            let flat = render_flat(&items[1], options, Some(comments));
            if flat.len() <= inline_length_limit(options) {
                Some(flat)
            } else {
                None
            }
        })
    });
    if let Some(inline) = inline_cond {
        push_space_or_indent(out, child_indent);
        out.push_str(&inline);
    } else {
        ensure_newline(out);
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, comments, out);
    }
    for body in items.iter().skip(2) {
        ensure_newline_for_form(body, comments, out);
        write_indent(child_indent, out);
        if let Some(inline) = inline_form(body, options, 0, comments) {
            out.push_str(&inline);
        } else {
            write_form(body, child_indent, options, comments, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_doseq_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    if items.len() > 1 {
        if let Some(inline) = inline_form_relaxed(&items[1], options, 0, comments) {
            if out.ends_with('\n') {
                write_indent(child_indent, out);
                out.push_str(&inline);
            } else if current_column(out) + 1 + inline.len() <= inline_length_limit(options) {
                out.push(' ');
                out.push_str(&inline);
            } else {
                ensure_newline(out);
                write_indent(child_indent, out);
                write_form(&items[1], child_indent, options, comments, out);
            }
        } else {
            ensure_newline(out);
            write_indent(child_indent, out);
            write_form(&items[1], child_indent, options, comments, out);
        }
    }
    for body in items.iter().skip(2) {
        ensure_newline_for_form(body, comments, out);
        write_indent(child_indent, out);
        write_form(body, child_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_try_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str("try");
    let child_indent = block_child_indent(indent, "try".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    for body in items.iter().skip(1) {
        ensure_newline_for_form(body, comments, out);
        write_indent(child_indent, out);
        write_form(body, child_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_thread_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str(head);
    let child_indent = indent + options.indent_width * 2;
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    push_space_or_indent(out, child_indent);
    if let Some(inline) = inline_fn_args_vector_text(&items[1], options, comments) {
        out.push_str(&inline);
    } else {
        write_fn_args_vector(&items[1], child_indent, options, comments, out);
    }
    for item in items.iter().skip(2) {
        ensure_newline_for_form(item, comments, out);
        write_indent(child_indent, out);
        write_form(item, child_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_map_sugar_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    let Some(binding_idx) = map_sugar_binding_index(head, items) else {
        return false;
    };
    if binding_idx + 1 >= items.len() {
        return false;
    }
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    let mut idx = 1;
    while idx < binding_idx {
        let arg = &items[idx];
        if let Some(inline) = inline_form(arg, options, 0, comments) {
            push_space_or_indent(out, child_indent);
            out.push_str(&inline);
        } else {
            ensure_newline_for_form(arg, comments, out);
            write_indent(child_indent, out);
            write_form(arg, child_indent, options, comments, out);
        }
        idx += 1;
    }
    push_space_or_indent(out, child_indent);
    let binding_indent = current_column(out);
    write_form(&items[binding_idx], binding_indent, options, comments, out);
    for body in items.iter().skip(binding_idx + 1) {
        ensure_newline_for_form(body, comments, out);
        write_indent(child_indent, out);
        write_form(body, child_indent, options, comments, out);
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
    true
}

struct MatchClause<'a> {
    pattern: &'a Form,
    guard: Option<&'a Form>,
    binding: Option<&'a Form>,
    expr: &'a Form,
}

fn write_match_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 4 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str("match");
    let clause_indent = block_child_indent(indent, "match".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, clause_indent, options, comments, out);
    if let Some(_) = inline_form_ignore_comments(&items[1], options, 0) {
        push_space_or_indent(out, clause_indent);
        write_form(&items[1], clause_indent, options, comments, out);
    } else {
        ensure_newline(out);
        write_indent(clause_indent, out);
        write_form(&items[1], clause_indent, options, comments, out);
    }
    let mut clauses: Vec<MatchClause> = Vec::new();
    let mut idx = 2;
    while idx < items.len() {
        if idx >= items.len() {
            break;
        }
        let pattern = &items[idx];
        idx += 1;
        let mut guard = None;
        let mut binding = None;
        if idx + 1 < items.len() {
            if matches!(&items[idx].kind, FormKind::Keyword(k) if k == "when") {
                guard = Some(&items[idx + 1]);
                idx += 2;
            }
        }
        if idx + 1 < items.len() {
            if matches!(&items[idx].kind, FormKind::Keyword(k) if k == "as") {
                binding = Some(&items[idx + 1]);
                idx += 2;
            }
        }
        if idx >= items.len() {
            break;
        }
        let expr = &items[idx];
        idx += 1;
        clauses.push(MatchClause {
            pattern,
            guard,
            binding,
            expr,
        });
    }
    let align_match = options.align_match;
    let pattern_has_comments = clauses.iter().any(|clause| {
        comments.has_comments(clause.pattern.span.index)
            || comments.has_inline_trailing(clause.pattern.span.index)
    });
    let pattern_strings = if align_match && !pattern_has_comments {
        collect_inline_strings_ignore_comments(clauses.iter().map(|clause| clause.pattern), options)
    } else {
        None
    };
    let (inline_left, all_left_inline) = if align_match {
        let mut left = Vec::new();
        let mut all_inline = true;
        for clause in &clauses {
            if let Some(text) = inline_match_clause_left(clause, options, comments) {
                left.push(text);
            } else {
                all_inline = false;
                break;
            }
        }
        (left, all_inline)
    } else {
        (Vec::new(), false)
    };
    let value_column = if align_match {
        pattern_strings
            .as_ref()
            .map(|strings| clause_indent + strings.iter().map(|s| s.len()).max().unwrap_or(0) + 2)
    } else {
        None
    };
    for (idx, clause) in clauses.iter().enumerate() {
        ensure_newline_for_form(clause.pattern, comments, out);
        write_indent(clause_indent, out);
        if let (Some(_), Some(col)) = (&pattern_strings, value_column) {
            if !all_left_inline {
                write_form(clause.pattern, clause_indent, options, comments, out);
                if let Some(guard) = clause.guard {
                    if out.ends_with('\n') {
                        write_indent(clause_indent, out);
                    } else {
                        out.push(' ');
                    }
                    out.push_str(":when ");
                    write_form(guard, clause_indent + 6, options, comments, out);
                }
                if let Some(binding) = clause.binding {
                    if out.ends_with('\n') {
                        write_indent(clause_indent, out);
                    } else {
                        out.push(' ');
                    }
                    out.push_str(":as ");
                    write_form(binding, clause_indent + 4, options, comments, out);
                }
                if out.ends_with('\n') {
                    write_indent(clause_indent + options.indent_width, out);
                } else {
                    out.push(' ');
                }
                write_form(clause.expr, clause_indent, options, comments, out);
                continue;
            }
            let left_text = &inline_left[idx];
            out.push_str(left_text);
            let current_col = clause_indent + left_text.len();
            let mut pad = if col > current_col {
                col - current_col
            } else {
                2
            };
            if pad < 2 {
                pad = 2;
            }
            for _ in 0..pad {
                out.push(' ');
            }
            write_form(clause.expr, col, options, comments, out);
        } else {
            if let Some(pat_text) = inline_match_clause_left(clause, options, comments) {
                out.push_str(&pat_text);
            } else {
                write_form(clause.pattern, clause_indent, options, comments, out);
                if let Some(guard) = clause.guard {
                    if out.ends_with('\n') {
                        write_indent(clause_indent, out);
                    } else {
                        out.push(' ');
                    }
                    out.push_str(":when ");
                    write_form(guard, clause_indent + 6, options, comments, out);
                }
                if let Some(binding) = clause.binding {
                    if out.ends_with('\n') {
                        write_indent(clause_indent, out);
                    } else {
                        out.push(' ');
                    }
                    out.push_str(":as ");
                    write_form(binding, clause_indent + 4, options, comments, out);
                }
            }
            if out.ends_with('\n') {
                write_indent(clause_indent + options.indent_width, out);
            } else {
                out.push(' ');
            }
            write_form(clause.expr, clause_indent, options, comments, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn inline_match_clause_left(
    clause: &MatchClause,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    let mut buf = String::new();
    buf.push_str(&inline_form(clause.pattern, options, 0, comments)?);
    if let Some(guard) = clause.guard {
        buf.push_str(" :when ");
        buf.push_str(&inline_form(guard, options, 0, comments)?);
    }
    if let Some(binding) = clause.binding {
        buf.push_str(" :as ");
        buf.push_str(&inline_form(binding, options, 0, comments)?);
    }
    Some(buf)
}

fn write_cond_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    out.push('(');
    out.push_str("cond");
    let clause_indent = block_child_indent(indent, "cond".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, clause_indent, options, comments, out);
    let mut clauses: Vec<(&Form, &Form)> = Vec::new();
    for idx in (1..items.len()).step_by(2) {
        if idx + 1 >= items.len() {
            break;
        }
        clauses.push((&items[idx], &items[idx + 1]));
    }
    let value_column = if options.align_cond {
        let mut max_test_len = 0usize;
        for (test, _) in clauses.iter() {
            let len = formatted_last_line_len(test, options);
            if len > max_test_len {
                max_test_len = len;
            }
        }
        if max_test_len > 0 {
            Some(clause_indent + max_test_len + 2)
        } else {
            None
        }
    } else {
        None
    };
    for (test, expr) in clauses.iter() {
        ensure_newline_for_form(test, comments, out);
        write_indent(clause_indent, out);
        let test_has_inline_comment = comments.has_inline_trailing(test.span.index);
        write_form(test, clause_indent, options, comments, out);
        if let Some(col) = value_column {
            let mut needs_newline = out.ends_with('\n') || test_has_inline_comment;
            let current_col = current_column(out);
            if !needs_newline && current_col > col {
                needs_newline = true;
            }
            if needs_newline {
                ensure_newline(out);
                write_indent(col, out);
            } else {
                let mut pad = if col > current_col {
                    col - current_col
                } else {
                    2
                };
                if pad < 2 {
                    pad = 2;
                }
                for _ in 0..pad {
                    out.push(' ');
                }
            }
            write_form(expr, col, options, comments, out);
        } else {
            if out.ends_with('\n') {
                write_indent(clause_indent + options.indent_width, out);
            } else {
                out.push(' ');
            }
            write_form(expr, clause_indent, options, comments, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn formatted_last_line_len(form: &Form, options: &FormatOptions) -> usize {
    let mut out = String::new();
    let mut comments = CommentTable::default();
    write_form(form, 0, options, &mut comments, &mut out);
    let trimmed = out.trim_end_matches('\n');
    trimmed.rsplit('\n').next().unwrap_or("").len()
}

fn write_defn_form(
    head: &str,
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    push_space_or_indent(out, child_indent);
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, comments, out);
    }
    let mut had_doc_or_meta = false;
    while idx < items.len() && matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_)) {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
        had_doc_or_meta = true;
    }
    if idx >= items.len() {
        if out.ends_with('\n') {
            write_indent(indent, out);
        }
        out.push(')');
        return;
    }
    if matches!(items[idx].kind, FormKind::List(_)) {
        while idx < items.len() {
            ensure_newline_for_form(&items[idx], comments, out);
            write_indent(child_indent, out);
            write_form(&items[idx], child_indent, options, comments, out);
            idx += 1;
        }
    } else {
        let mut inlined_params = false;
        if !had_doc_or_meta {
            let inline_body = items
                .get(idx + 1)
                .and_then(|f| inline_form(f, options, 0, comments));
            let params_text = inline_fn_args_vector_text(&items[idx], options, comments);
            let params_is_empty = matches!(&items[idx].kind, FormKind::Vector(v) if v.is_empty());
            let body_is_list =
                matches!(items.get(idx + 1).map(|f| &f.kind), Some(FormKind::List(_)));
            if let Some(inline) = params_text.as_ref() {
                if params_is_empty {
                    let current_col = current_column(out);
                    let allow_empty_inline = if body_is_list {
                        inline_body
                            .as_ref()
                            .map(|body| {
                                current_col + 1 + inline.len() + 1 + body.len()
                                    <= inline_length_limit(options)
                            })
                            .unwrap_or(false)
                    } else {
                        current_col + 1 + inline.len() <= inline_length_limit(options)
                    };
                    if allow_empty_inline {
                        push_space_or_indent(out, child_indent);
                        out.push_str(inline);
                        idx += 1;
                        inlined_params = true;
                    }
                }
            }
            if !inlined_params {
                let current_col = current_column(out);
                if let (Some(inline), Some(body_text)) =
                    (params_text.as_ref(), inline_body.as_ref())
                {
                    if current_col + 1 + inline.len() + 1 + body_text.len()
                        <= inline_length_limit(options)
                    {
                        push_space_or_indent(out, child_indent);
                        out.push_str(inline);
                        idx += 1;
                        inlined_params = true;
                    }
                }
                if !inlined_params {
                    if let Some(inline) = params_text.as_ref() {
                        if !params_is_empty
                            && current_col + 1 + inline.len() <= inline_length_limit(options)
                        {
                            push_space_or_indent(out, child_indent);
                            out.push_str(inline);
                            idx += 1;
                            inlined_params = true;
                        }
                    }
                }
            }
        }
        if !inlined_params {
            ensure_newline_for_form(&items[idx], comments, out);
            write_indent(child_indent, out);
            write_fn_args_vector(&items[idx], child_indent, options, comments, out);
            idx += 1;
        }
        while idx < items.len() {
            ensure_newline_for_form(&items[idx], comments, out);
            write_indent(child_indent, out);
            write_form(&items[idx], child_indent, options, comments, out);
            idx += 1;
        }
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_def_foreign_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 2 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str("def-foreign");
    let child_indent = block_child_indent(indent, "def-foreign".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    push_space_or_indent(out, child_indent);
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, comments, out);
    }
    while idx + 1 < items.len() && matches!(items[idx].kind, FormKind::Keyword(_)) {
        let key = &items[idx];
        let value = &items[idx + 1];
        ensure_newline_for_form(key, comments, out);
        write_indent(child_indent, out);
        write_form(key, child_indent, options, comments, out);
        if comments.has_inline_trailing(key.span.index) {
            ensure_newline(out);
            let value_indent = child_indent + options.indent_width;
            write_indent(value_indent, out);
            write_form(value, value_indent, options, comments, out);
        } else {
            out.push(' ');
            let value_indent = current_column(out);
            write_form(value, value_indent, options, comments, out);
        }
        idx += 2;
    }
    if idx < items.len() {
        let params = &items[idx];
        ensure_newline_for_form(params, comments, out);
        write_indent(child_indent, out);
        if let Some(text) = render_force_inline(params, options, comments) {
            out.push_str(&text);
        } else {
            write_form(params, child_indent, options, comments, out);
        }
        idx += 1;
    }
    if idx + 1 < items.len() {
        if matches!(&items[idx].kind, FormKind::Symbol(sym) if sym == "->") {
            if !out.ends_with('\n') {
                out.push(' ');
            } else {
                write_indent(child_indent, out);
            }
            out.push_str("-> ");
            if let Some(text) = render_force_inline(&items[idx + 1], options, comments) {
                out.push_str(&text);
            } else {
                let value_indent = current_column(out);
                write_form(&items[idx + 1], value_indent, options, comments, out);
            }
            idx += 2;
        }
    }
    while idx < items.len() {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_fn_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    out.push('(');
    out.push_str("fn");
    let child_indent = block_child_indent(indent, "fn".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    let mut idx = 1;
    if idx < items.len() {
        if let Some(inline) = inline_fn_args_vector_text(&items[idx], options, comments) {
            push_space_or_indent(out, child_indent);
            out.push_str(&inline);
            idx += 1;
        } else {
            ensure_newline_for_form(&items[idx], comments, out);
            write_indent(child_indent, out);
            write_fn_args_vector(&items[idx], child_indent, options, comments, out);
            idx += 1;
        }
    }
    while idx < items.len() {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_deref_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return write_form(form, indent, options, comments, out),
    };
    out.push('@');
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        out.push_str(&inline);
    } else {
        let child_indent = indent + options.indent_width;
        ensure_newline(out);
        write_indent(child_indent, out);
        write_form(&items[1], child_indent, options, comments, out);
    }
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_deftype_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    write_block_definition(form, indent, options, comments, out, "deftype");
}

fn write_defenum_form(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str("defenum");
    let child_indent = block_child_indent(indent, "defenum".len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    push_space_or_indent(out, child_indent);
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, comments, out);
    }
    if idx < items.len() && matches!(items[idx].kind, FormKind::String(_)) {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    if idx < items.len() && matches!(items[idx].kind, FormKind::Map(_)) {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    while idx + 1 < items.len() && matches!(items[idx].kind, FormKind::Keyword(_)) {
        let option_form = &items[idx];
        let value_form = &items[idx + 1];
        ensure_newline_for_form(option_form, comments, out);
        write_indent(child_indent, out);
        write_form(option_form, child_indent, options, comments, out);
        let value_indent = if out.ends_with('\n') {
            let indent = child_indent + options.indent_width;
            write_indent(indent, out);
            indent
        } else {
            out.push(' ');
            current_column(out)
        };
        write_form(value_form, value_indent, options, comments, out);
        idx += 2;
    }
    while idx < items.len() {
        let member = &items[idx];
        if let FormKind::Symbol(sym) = &member.kind {
            if !sym.starts_with('*') {
                if let Some(payload) = items.get(idx + 1) {
                    if matches!(payload.kind, FormKind::Map(_)) {
                        let member_has_inline = comments.has_inline_trailing(member.span.index);
                        ensure_newline_for_form(member, comments, out);
                        write_indent(child_indent, out);
                        write_form(member, child_indent, options, comments, out);
                        if member_has_inline || out.ends_with('\n') {
                            ensure_newline(out);
                            write_indent(child_indent, out);
                            write_form(payload, child_indent, options, comments, out);
                        } else {
                            out.push(' ');
                            let payload_indent = current_column(out);
                            write_form(payload, payload_indent, options, comments, out);
                        }
                        idx += 2;
                        continue;
                    }
                }
            }
        }
        ensure_newline_for_form(member, comments, out);
        write_indent(child_indent, out);
        write_form(member, child_indent, options, comments, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_block_definition(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
    head: &str,
) {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => unreachable!(),
    };
    if items.len() < 3 {
        write_head_list(form, indent, options, comments, out);
        return;
    }
    let mut idx = 2;
    out.push('(');
    out.push_str(head);
    let child_indent = block_child_indent(indent, head.len(), options, out);
    emit_inline_trailing_comments(items[0].span.index, child_indent, options, comments, out);
    push_space_or_indent(out, child_indent);
    if let Some(inline) = inline_form(&items[1], options, 0, comments) {
        out.push_str(&inline);
    } else {
        write_form(&items[1], child_indent, options, comments, out);
    }
    while idx < items.len() && matches!(items[idx].kind, FormKind::String(_) | FormKind::Map(_)) {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    while idx < items.len() {
        ensure_newline_for_form(&items[idx], comments, out);
        write_indent(child_indent, out);
        write_form(&items[idx], child_indent, options, comments, out);
        idx += 1;
    }
    if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push(')');
    emit_dangling_comments(form.span.index, indent, comments, out);
}

fn write_binding_vector(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    match &form.kind {
        FormKind::Vector(items) => {
            if items.is_empty() {
                out.push('[');
                let pair_indent = current_column(out);
                let opening_inserted =
                    emit_opening_inline_comments(form.span.index, pair_indent, comments, out);
                let had_dangling =
                    emit_dangling_comments(form.span.index, pair_indent, comments, out);
                if opening_inserted || had_dangling {
                    if out.ends_with('\n') {
                        write_indent(indent, out);
                    }
                    out.push(']');
                } else {
                    out.push(']');
                }
                return;
            }
            let name_texts: Vec<Option<String>> = items
                .chunks(2)
                .map(|pair| binding_name_alignment_text(&pair[0], comments))
                .collect();
            if options.align_let_bindings
                && items.len() % 2 == 0
                && write_aligned_bindings(form, items, indent, options, comments, out)
            {
                return;
            }
            out.push('[');
            let pair_indent = current_column(out);
            let opening_inserted =
                emit_opening_inline_comments(form.span.index, pair_indent, comments, out);
            if opening_inserted {
                write_indent(pair_indent, out);
            }
            let max_name_len = if options.align_let_bindings {
                name_texts
                    .iter()
                    .filter_map(|s| s.as_ref().map(|s| s.len()))
                    .max()
            } else {
                None
            };
            let mut idx = 0;
            let mut first = true;
            while idx + 1 < items.len() {
                let name = &items[idx];
                if !first {
                    ensure_newline_for_form(name, comments, out);
                    write_indent(pair_indent, out);
                }
                let deferred_inline = comments.take_inline_trailing(name.span.index);
                let name_inline = inline_binding_name(name, options, comments).is_some();
                write_binding_name(name, pair_indent, options, comments, out);
                let name_text = name_texts.get(idx / 2).and_then(|s| s.as_ref());
                let pad = if let (Some(max_len), Some(text)) = (max_name_len, name_text) {
                    max_len.saturating_sub(text.len()) + 2
                } else {
                    1
                };
                let value = &items[idx + 1];
                let mut rendered_inline = false;
                if name_inline && !form_has_comments(value, comments) {
                    let inline_preview = inline_form_ignore_comments(value, options, 0)
                        .or_else(|| inline_form_relaxed(value, options, 0, comments))
                        .or_else(|| inline_if_force(value, options, comments));
                    if let Some(inline) = inline_preview {
                        let budget = options.align_inline_budget_multiplier.max(1);
                        let inline_limit = inline_length_limit(options).saturating_mul(budget);
                        if current_column(out) + pad + inline.len() <= inline_limit {
                            for _ in 0..pad {
                                out.push(' ');
                            }
                            out.push_str(&inline);
                            rendered_inline = true;
                        }
                    }
                }
                if !rendered_inline {
                    let force_newline = !name_inline && !matches!(name.kind, FormKind::Symbol(_));
                    if force_newline {
                        ensure_newline(out);
                        let value_indent = pair_indent + 1;
                        write_indent(value_indent, out);
                        write_form(value, value_indent, options, comments, out);
                    } else {
                        for _ in 0..pad {
                            out.push(' ');
                        }
                        let value_indent = current_column(out);
                        write_form(value, value_indent, options, comments, out);
                    }
                }
                if let Some(lines) = deferred_inline {
                    write_inline_after_value(lines, pair_indent, options, out);
                }
                idx += 2;
                first = false;
            }
            if idx < items.len() {
                ensure_newline_for_form(&items[idx], comments, out);
                write_indent(pair_indent, out);
                write_form(&items[idx], pair_indent, options, comments, out);
            }
            let had_dangling = emit_dangling_comments(form.span.index, pair_indent, comments, out);
            if had_dangling {
                write_indent(indent, out);
            }
            out.push(']');
            emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
        }
        _ => write_form(form, indent, options, comments, out),
    }
}

fn write_inline_after_value(
    lines: Vec<String>,
    indent: usize,
    options: &FormatOptions,
    out: &mut String,
) {
    for (idx, line) in lines.into_iter().enumerate() {
        if idx == 0 {
            ensure_inline_comment_spacing(out, options.comments_spacing);
            out.push_str(&line);
            out.push('\n');
            continue;
        }
        write_indent(indent, out);
        out.push_str(&line);
        out.push('\n');
    }
}

fn write_aligned_bindings(
    form: &Form,
    items: &[Form],
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    if !options.align_let_bindings {
        return false;
    }
    if items.len() % 2 != 0 {
        return false;
    }
    let mut name_strings: Vec<String> = Vec::new();
    let mut max_len = 0;
    let mut idx = 0;
    while idx < items.len() {
        if let Some(name) = inline_binding_name_ignore_comments(&items[idx], options, comments) {
            max_len = max_len.max(name.len());
            name_strings.push(name);
        } else {
            return false;
        }
        idx += 2;
    }
    if name_strings.is_empty() {
        return false;
    }
    out.push('[');
    let pair_indent = current_column(out);
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, pair_indent, comments, out);
    if opening_inserted {
        write_indent(pair_indent, out);
    }
    for (pair_idx, name_text) in name_strings.iter().enumerate() {
        let name_form = &items[pair_idx * 2];
        let value = &items[pair_idx * 2 + 1];
        if pair_idx > 0 {
            ensure_newline_for_form(name_form, comments, out);
        }
        let deferred_inline = comments.take_inline_trailing(name_form.span.index);
        let inserted_comment =
            emit_leading_comments(name_form.span.index, pair_indent, comments, out);
        if pair_idx > 0 || inserted_comment {
            write_indent(pair_indent, out);
        }
        out.push_str(name_text);
        let pad = max_len.saturating_sub(name_text.len()) + 2;
        let mut rendered_inline = false;
        if !form_has_comments(value, comments) {
            let inline_preview = inline_form_ignore_comments(value, options, 0)
                .or_else(|| inline_form_relaxed(value, options, 0, comments))
                .or_else(|| inline_if_force(value, options, comments));
            if let Some(inline) = inline_preview {
                let budget = options.align_inline_budget_multiplier.max(1);
                let inline_limit = inline_length_limit(options).saturating_mul(budget);
                if current_column(out) + pad + inline.len() <= inline_limit {
                    for _ in 0..pad {
                        out.push(' ');
                    }
                    out.push_str(&inline);
                    rendered_inline = true;
                }
            }
        }
        if !rendered_inline {
            for _ in 0..pad {
                out.push(' ');
            }
            let value_indent = current_column(out);
            write_form(value, value_indent, options, comments, out);
        }
        if let Some(lines) = deferred_inline {
            write_inline_after_value(lines, pair_indent, options, out);
        }
    }
    if out.ends_with('\n') {
        write_indent(pair_indent, out);
    }
    out.push(']');
    emit_inline_trailing_comments(form.span.index, indent, options, comments, out);
    true
}

fn write_vector(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => unreachable!(),
    };
    if !comments.has_comments(form.span.index) {
        if let Some(inline) = inline_vector(form, options, 0, comments) {
            out.push_str(&inline);
            return;
        }
    }
    out.push('[');
    if items.is_empty() {
        let item_indent = indent + 1;
        let opening_inserted =
            emit_opening_inline_comments(form.span.index, item_indent, comments, out);
        let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
        if opening_inserted || had_dangling {
            if out.ends_with('\n') {
                write_indent(indent, out);
            }
            out.push(']');
        } else {
            out.push(']');
        }
        return;
    }
    let item_indent = indent + 1;
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, item_indent, comments, out);
    if opening_inserted {
        write_indent(item_indent, out);
    }
    write_form(&items[0], item_indent, options, comments, out);
    for item in items.iter().skip(1) {
        ensure_newline_for_form(item, comments, out);
        write_indent(item_indent, out);
        write_form(item, item_indent, options, comments, out);
    }
    let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
    if had_dangling {
        write_indent(indent, out);
    }
    out.push(']');
    append_type_hint(form, out);
}

fn write_fn_args_vector(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    if let Some(inline) = inline_fn_args_vector_text(form, options, comments) {
        out.push_str(&inline);
        return;
    }
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => {
            write_form(form, indent, options, comments, out);
            return;
        }
    };
    out.push('[');
    if items.is_empty() {
        let item_indent = indent + options.indent_width;
        let opening_inserted =
            emit_opening_inline_comments(form.span.index, item_indent, comments, out);
        let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
        if opening_inserted || had_dangling {
            if out.ends_with('\n') {
                write_indent(indent, out);
            }
            out.push(']');
        } else {
            out.push(']');
        }
        return;
    }
    let item_indent = indent + options.indent_width;
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, item_indent, comments, out);
    if opening_inserted {
        write_indent(item_indent, out);
    }
    write_form(&items[0], item_indent, options, comments, out);
    for item in items.iter().skip(1) {
        ensure_newline_for_form(item, comments, out);
        write_indent(item_indent, out);
        write_form(item, item_indent, options, comments, out);
    }
    let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
    if had_dangling {
        write_indent(indent, out);
    }
    out.push(']');
    append_type_hint(form, out);
}

fn write_set(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let items = match &form.kind {
        FormKind::Set(items) => items,
        _ => unreachable!(),
    };
    if !comments.has_comments(form.span.index) {
        if let Some(inline) = inline_set(form, options, 0, comments) {
            out.push_str(&inline);
            return;
        }
    }
    out.push_str("#{");
    if items.is_empty() {
        let item_indent = indent + options.indent_width;
        let opening_inserted =
            emit_opening_inline_comments(form.span.index, item_indent, comments, out);
        let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
        if opening_inserted || had_dangling {
            if out.ends_with('\n') {
                write_indent(indent, out);
            }
            out.push('}');
        } else {
            out.push('}');
        }
        return;
    }
    let item_indent = indent + options.indent_width;
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, item_indent, comments, out);
    if opening_inserted {
        write_indent(item_indent, out);
    }
    write_form(&items[0], item_indent, options, comments, out);
    for item in items.iter().skip(1) {
        ensure_newline_for_form(item, comments, out);
        write_indent(item_indent, out);
        write_form(item, item_indent, options, comments, out);
    }
    let had_dangling = emit_dangling_comments(form.span.index, item_indent, comments, out);
    if had_dangling {
        write_indent(indent, out);
    }
    out.push('}');
}

fn write_map(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => unreachable!(),
    };
    if !comments.has_comments(form.span.index) {
        if let Some(inline) = inline_map(form, options, 0, comments) {
            out.push_str(&inline);
            return;
        }
    }
    if entries.is_empty() {
        out.push('{');
        let entry_indent = indent + 1;
        let opening_inserted =
            emit_opening_inline_comments(form.span.index, entry_indent, comments, out);
        let had_dangling = emit_dangling_comments(form.span.index, entry_indent, comments, out);
        if opening_inserted || had_dangling {
            if out.ends_with('\n') {
                write_indent(indent, out);
            }
            out.push('}');
        } else {
            out.push('}');
        }
        return;
    }
    if !write_aligned_map(form, indent, options, comments, out) {
        write_map_block(form, indent, options, comments, out);
    }
}

fn write_aligned_map(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    if !options.align_maps {
        return false;
    }
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => return false,
    };
    let mut key_strings: Vec<String> = Vec::new();
    let mut shorthand_flags: Vec<bool> = Vec::new();
    for entry in entries {
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v, options, comments) {
                    if comments.has_inline_trailing(k.span.index) {
                        return false;
                    }
                    key_strings.push(base);
                    shorthand_flags.push(true);
                    continue;
                }
                if let Some(text) = inline_form_ignore_comments(k, options, 0) {
                    key_strings.push(text);
                    shorthand_flags.push(false);
                } else {
                    return false;
                }
            }
            MapItem::Spread(_) => return false,
        }
    }
    let max_key_len = key_strings.iter().map(|s| s.len()).max().unwrap_or(0);
    out.push('{');
    let entry_indent = indent + 1;
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, entry_indent, comments, out);
    if opening_inserted {
        write_indent(entry_indent, out);
    }
    for (idx, entry) in entries.iter().enumerate() {
        if idx > 0 {
            ensure_newline(out);
            let entry_index = match entry {
                MapItem::KeyValue(k, _) => k.span.index,
                MapItem::Spread(expr) => expr.span.index,
            };
            if comments.take_blank_line(entry_index) {
                out.push('\n');
            }
            write_indent(entry_indent, out);
        }
        match entry {
            MapItem::KeyValue(k, v) => {
                if !options.shorthand_map && k.span.index == v.span.index {
                    comments.take_trailing_comma(k.span.index);
                }
                let is_shorthand = shorthand_flags.get(idx).copied().unwrap_or(false);
                let key_inline_comment = comments.has_inline_trailing(k.span.index);
                let inserted_comment =
                    emit_leading_comments(k.span.index, entry_indent, comments, out);
                if inserted_comment {
                    write_indent(entry_indent, out);
                }
                let key_text = &key_strings[idx];
                out.push_str(key_text);
                if is_shorthand {
                    comments.take_trailing_comma(k.span.index);
                    out.push(',');
                    continue;
                }
                emit_inline_trailing_comments(k.span.index, entry_indent, options, comments, out);
                if key_inline_comment {
                    let value_indent = entry_indent + options.indent_width;
                    write_indent(value_indent, out);
                    write_form(v, value_indent, options, comments, out);
                } else {
                    let pad = max_key_len.saturating_sub(key_text.len()) + 1;
                    for _ in 0..pad {
                        out.push(' ');
                    }
                    let value_indent = current_column(out);
                    write_form(v, value_indent, options, comments, out);
                }
            }
            MapItem::Spread(expr) => {
                let inserted_comment =
                    emit_leading_comments(expr.span.index, entry_indent, comments, out);
                if inserted_comment {
                    write_indent(entry_indent, out);
                }
                out.push('*');
                out.push(' ');
                write_form(expr, entry_indent, options, comments, out);
            }
        }
    }
    let had_dangling = emit_dangling_comments(form.span.index, entry_indent, comments, out);
    if had_dangling {
        write_indent(indent, out);
    } else if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push('}');
    true
}

fn write_map_block(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => unreachable!(),
    };
    out.push('{');
    let entry_indent = indent + 1;
    let opening_inserted =
        emit_opening_inline_comments(form.span.index, entry_indent, comments, out);
    if opening_inserted {
        write_indent(entry_indent, out);
    }
    for (idx, entry) in entries.iter().enumerate() {
        if idx == 0 {
            // nothing
        } else {
            ensure_newline(out);
            let entry_index = match entry {
                MapItem::KeyValue(k, _) => k.span.index,
                MapItem::Spread(expr) => expr.span.index,
            };
            if comments.take_blank_line(entry_index) {
                out.push('\n');
            }
            write_indent(entry_indent, out);
        }
        match entry {
            MapItem::KeyValue(k, v) => {
                if !options.shorthand_map && k.span.index == v.span.index {
                    comments.take_trailing_comma(k.span.index);
                }
                let key_inline_comment = comments.has_inline_trailing(k.span.index);
                if !key_inline_comment {
                    if let Some(base) = render_map_shorthand_key(k, v, options, comments) {
                        out.push_str(&format!("{},", base));
                        continue;
                    }
                }
                write_form(k, entry_indent, options, comments, out);
                if key_inline_comment {
                    ensure_newline(out);
                    let value_indent = entry_indent + options.indent_width;
                    write_indent(value_indent, out);
                    write_form(v, value_indent, options, comments, out);
                } else {
                    out.push(' ');
                    write_form(v, entry_indent, options, comments, out);
                }
            }
            MapItem::Spread(expr) => {
                out.push('*');
                out.push(' ');
                write_form(expr, entry_indent, options, comments, out);
            }
        }
    }
    let had_dangling = emit_dangling_comments(form.span.index, entry_indent, comments, out);
    if had_dangling {
        write_indent(indent, out);
    } else if out.ends_with('\n') {
        write_indent(indent, out);
    }
    out.push('}');
}

fn write_foreign_with_prefix(
    tag: Option<&str>,
    code: &str,
    is_dollar: bool,
    indent: usize,
    options: &FormatOptions,
    out: &mut String,
) {
    if is_dollar {
        out.push('$');
    }
    if let Some(t) = tag {
        if !(is_dollar && t == "rb") {
            out.push_str(t);
        }
    }
    out.push('{');
    if !code.contains('\n') {
        out.push_str(code);
        out.push('}');
        return;
    }
    out.push('\n');
    let inner_indent = indent + options.indent_width;
    let normalized = normalize_foreign_code(code, tag, options.indent_width);
    for line in normalized.split('\n') {
        if line.trim().is_empty() {
            out.push('\n');
            continue;
        }
        write_indent(inner_indent, out);
        out.push_str(line);
        out.push('\n');
    }
    write_indent(indent, out);
    out.push('}');
}

fn normalize_foreign_code<'a>(
    code: &'a str,
    tag: Option<&str>,
    indent_width: usize,
) -> Cow<'a, str> {
    if !code.contains('\n') {
        return Cow::Borrowed(code);
    }
    let dedented = dedent_foreign_block(code);
    if is_ruby_tag(tag) {
        Cow::Owned(fallback_ruby_indent(&dedented, indent_width))
    } else {
        Cow::Owned(dedented)
    }
}

fn dedent_foreign_block(code: &str) -> String {
    let mut lines: Vec<String> = code
        .split('\n')
        .map(|line| line.trim_end_matches('\r').to_string())
        .collect();
    trim_edge_blank_lines(&mut lines);
    let mut base_indent: Option<usize> = None;
    for line in &lines {
        if line.trim().is_empty() {
            continue;
        }
        if base_indent.is_some() {
            break;
        }
        base_indent = Some(leading_indent_width(line));
    }
    let base_indent = base_indent.unwrap_or(0);
    if base_indent == 0 {
        return lines.join("\n");
    }
    for line in &mut lines {
        if line.trim().is_empty() {
            line.clear();
            continue;
        }
        let indent = leading_indent_width(line);
        if indent >= base_indent {
            line.drain(0..base_indent);
        }
    }
    lines.join("\n")
}

fn leading_indent_width(line: &str) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|b| matches!(**b, b' ' | b'\t'))
        .count()
}

fn trim_edge_blank_lines(lines: &mut Vec<String>) {
    while matches!(lines.first(), Some(line) if line.trim().is_empty()) {
        lines.remove(0);
    }
    while matches!(lines.last(), Some(line) if line.trim().is_empty()) {
        lines.pop();
    }
}

fn is_ruby_tag(tag: Option<&str>) -> bool {
    match tag {
        Some(t) => t == "rb" || t == "ruby",
        None => true,
    }
}

fn fallback_ruby_indent(code: &str, indent_width: usize) -> String {
    let mut output = Vec::new();
    let mut level: usize = 0;
    let unit = indent_width.max(1);
    let mut last_blank = false;
    for raw_line in code.split('\n') {
        if raw_line.trim().is_empty() {
            if !last_blank {
                output.push(String::new());
                last_blank = true;
            }
            continue;
        }
        last_blank = false;
        let trimmed_leading = raw_line.trim_start();
        let line_no_cr = trimmed_leading.trim_end_matches('\r');
        let stripped = strip_ruby_comment(line_no_cr);
        let before = ruby_indent_adjust_before(stripped);
        level = level.saturating_sub(before);
        let mut buf = String::new();
        for _ in 0..level * unit {
            buf.push(' ');
        }
        buf.push_str(line_no_cr.trim_start());
        output.push(buf);
        let after = ruby_indent_adjust_after(stripped);
        level += after;
    }
    output.join("\n")
}

fn strip_ruby_comment(line: &str) -> &str {
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for (idx, ch) in line.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        match ch {
            '\\' => escape = true,
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '#' if !in_single && !in_double => return &line[..idx],
            _ => {}
        }
    }
    line
}

fn ruby_indent_adjust_before(line: &str) -> usize {
    let mut trimmed = line.trim_start();
    if trimmed.is_empty() {
        return 0;
    }
    let mut count = 0;
    while trimmed.starts_with('}') {
        count += 1;
        trimmed = &trimmed[1..];
        trimmed = trimmed.trim_start();
    }
    if let Some(keyword) = ruby_first_keyword(trimmed) {
        if matches!(
            keyword,
            "end" | "else" | "elsif" | "when" | "rescue" | "ensure"
        ) {
            count += 1;
        }
    }
    count
}

fn ruby_indent_adjust_after(line: &str) -> usize {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return 0;
    }
    let mut count = 0;
    if let Some(keyword) = ruby_first_keyword(trimmed) {
        if matches!(
            keyword,
            "class"
                | "module"
                | "def"
                | "case"
                | "begin"
                | "if"
                | "unless"
                | "while"
                | "until"
                | "for"
                | "loop"
        ) {
            count += 1;
        }
        if matches!(keyword, "else" | "elsif" | "when" | "rescue" | "ensure") {
            count += 1;
        }
    }
    if ruby_line_has_do_block(trimmed) {
        count += 1;
    }
    let brace_delta = ruby_brace_delta(trimmed);
    if brace_delta > 0 {
        count += brace_delta as usize;
    }
    count
}

fn ruby_first_keyword(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    if trimmed.is_empty() {
        return None;
    }
    let mut end = trimmed.len();
    for (idx, ch) in trimmed.char_indices() {
        if ch.is_alphanumeric() || ch == '_' {
            continue;
        }
        end = idx;
        break;
    }
    if end == 0 {
        return None;
    }
    Some(&trimmed[..end])
}

fn ruby_line_has_do_block(line: &str) -> bool {
    let stripped = strip_ruby_comment(line).trim_end();
    if stripped.ends_with(" do") {
        return true;
    }
    if stripped.contains(" do |") {
        return true;
    }
    if let Some(idx) = stripped.find(" do") {
        let before = stripped[..idx].chars().last();
        if matches!(before, Some(ch) if ch.is_whitespace() || ch == ')' || ch == ']') {
            return true;
        }
    }
    false
}

fn ruby_brace_delta(line: &str) -> i32 {
    let mut delta = 0;
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for ch in line.chars() {
        if escape {
            escape = false;
            continue;
        }
        match ch {
            '\\' => escape = true,
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '{' if !in_single && !in_double => delta += 1,
            '}' if !in_single && !in_double => delta -= 1,
            _ => {}
        }
    }
    delta
}

fn write_indent(width: usize, out: &mut String) {
    for _ in 0..width {
        out.push(' ');
    }
}

fn push_string_content(buf: &mut String, s: &str, escape_newlines: bool) {
    let mut chars = s.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '"' => {
                buf.push_str("\\\"");
            }
            '\\' => {
                buf.push_str("\\\\");
            }
            '\n' if escape_newlines => {
                buf.push_str("\\n");
            }
            '\n' => {
                buf.push('\n');
            }
            '\r' => {
                buf.push_str("\\r");
            }
            '\t' => {
                buf.push_str("\\t");
            }
            '#' if matches!(chars.peek(), Some('{')) => {
                buf.push_str("\\#");
            }
            other => buf.push(other),
        }
    }
}

fn push_string_literal(buf: &mut String, s: &str, escape_newlines: bool) {
    buf.push('"');
    push_string_content(buf, s, escape_newlines);
    buf.push('"');
}

fn push_regex_content(buf: &mut String, s: &str) {
    let mut chars = s.chars().peekable();
    let mut escaped = false;
    while let Some(ch) = chars.next() {
        if escaped {
            buf.push(ch);
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            buf.push(ch);
            continue;
        }
        if ch == '#' && matches!(chars.peek(), Some('{')) {
            buf.push_str("\\#");
            continue;
        }
        buf.push(ch);
    }
}

fn write_interpolated_string(
    parts: &[InterpolatedPart],
    escape_newlines: bool,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    out.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_string_content(out, text, escape_newlines),
            InterpolatedPart::Expr(expr) => {
                out.push_str("#{");
                let rendered = render_interpolation_expr(expr, indent, options, comments);
                out.push_str(&rendered);
                out.push('}');
            }
        }
    }
    out.push('"');
}

fn write_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &crate::ast::RegexDelim,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    let (prefix, suffix) = match delim {
        crate::ast::RegexDelim::Slash => ("/", "/"),
        crate::ast::RegexDelim::Hash => ("#\"", "\""),
        crate::ast::RegexDelim::HashSlash => ("#/", "/"),
    };
    out.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_regex_content(out, text),
            InterpolatedPart::Expr(expr) => {
                out.push_str("#{");
                let rendered = render_interpolation_expr(expr, indent, options, comments);
                out.push_str(&rendered);
                out.push('}');
            }
        }
    }
    out.push_str(suffix);
}

fn render_interpolation_expr(
    expr: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
) -> String {
    if let Some(inline) = inline_form(expr, options, 0, &*comments) {
        return inline;
    }
    let mut buf = String::new();
    write_form(expr, indent, options, comments, &mut buf);
    buf
}

fn inline_interpolated_string(
    parts: &[InterpolatedPart],
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
    escape_newlines: bool,
) -> Option<String> {
    let mut buf = String::new();
    buf.push('"');
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_string_content(&mut buf, text, escape_newlines),
            InterpolatedPart::Expr(expr) => {
                let inner = inline_form(expr, options, depth + 1, comments)?;
                buf.push_str("#{");
                buf.push_str(&inner);
                buf.push('}');
            }
        }
    }
    buf.push('"');
    Some(buf)
}

fn inline_interpolated_regex(
    parts: &[InterpolatedPart],
    delim: &crate::ast::RegexDelim,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let (prefix, suffix) = match delim {
        crate::ast::RegexDelim::Slash => ("/", "/"),
        crate::ast::RegexDelim::Hash => ("#\"", "\""),
        crate::ast::RegexDelim::HashSlash => ("#/", "/"),
    };
    let mut buf = String::new();
    buf.push_str(prefix);
    for part in parts {
        match part {
            InterpolatedPart::Text(text) => push_regex_content(&mut buf, text),
            InterpolatedPart::Expr(expr) => {
                let inner = inline_form(expr, options, depth + 1, comments)?;
                buf.push_str("#{");
                buf.push_str(&inner);
                buf.push('}');
            }
        }
    }
    buf.push_str(suffix);
    Some(buf)
}

fn current_column(out: &str) -> usize {
    out.rfind('\n')
        .map(|idx| out.len() - idx - 1)
        .unwrap_or(out.len())
}

fn append_type_hint(form: &Form, out: &mut String) {
    let Some(hint) = form.type_hint.as_ref() else {
        return;
    };
    if matches!(&form.kind, FormKind::Symbol(sym) if symbol_has_inline_type_hint(sym)) {
        return;
    }
    if matches!(hint.style, TypeHintStyle::Postfix) {
        if matches!(&form.kind, FormKind::Symbol(_)) {
            out.push('<');
            out.push_str(&hint.kind.describe());
            out.push('>');
            return;
        }
    }
    match hint.style {
        TypeHintStyle::Postfix => {
            out.push(':');
            out.push(' ');
        }
        TypeHintStyle::Return => {
            out.push(' ');
            out.push_str("-> ");
        }
    }
    out.push_str(&hint.kind.describe());
}

fn inline_binding_name(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    if comments.has_comments(form.span.index) || comments.has_inline_trailing(form.span.index) {
        return None;
    }
    inline_binding_name_ignore_comments(form, options, comments)
}

fn inline_binding_name_ignore_comments(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    if let FormKind::Symbol(sym) = &form.kind {
        let mut buf = sym.clone();
        if let Some(hint) = form.type_hint.as_ref() {
            if matches!(hint.style, TypeHintStyle::Postfix) && !symbol_has_inline_type_hint(sym) {
                buf.push(':');
                buf.push(' ');
                buf.push_str(&hint.kind.describe());
            }
        }
        return Some(buf);
    }
    if matches!(form.kind, FormKind::Map(_)) {
        return inline_map_relaxed(form, options, 0, comments);
    }
    inline_form_ignore_comments(form, options, 0)
        .or_else(|| inline_form_relaxed(form, options, 0, comments))
}

fn binding_name_alignment_text(form: &Form, comments: &CommentTable) -> Option<String> {
    if comments.has_comments(form.span.index) || comments.has_inline_trailing(form.span.index) {
        return None;
    }
    if let FormKind::Symbol(sym) = &form.kind {
        let mut buf = sym.clone();
        if let Some(hint) = form.type_hint.as_ref() {
            if matches!(hint.style, TypeHintStyle::Postfix) && !symbol_has_inline_type_hint(sym) {
                buf.push(':');
                buf.push(' ');
                buf.push_str(&hint.kind.describe());
            }
        }
        return Some(buf);
    }
    None
}

fn form_has_comments(form: &Form, comments: &CommentTable) -> bool {
    if comments.has_comments(form.span.index) || comments.has_inline_trailing(form.span.index) {
        return true;
    }
    match &form.kind {
        FormKind::List(items)
        | FormKind::Vector(items)
        | FormKind::Set(items)
        | FormKind::ShortFn(items) => items.iter().any(|item| form_has_comments(item, comments)),
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => {
                form_has_comments(k, comments) || form_has_comments(v, comments)
            }
            MapItem::Spread(expr) => form_has_comments(expr, comments),
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_has_comments(expr, comments),
            InterpolatedPart::Text(_) => false,
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_has_comments(expr, comments),
            InterpolatedPart::Text(_) => false,
        }),
        _ => false,
    }
}

fn form_has_internal_comments(
    form: &Form,
    comments: &CommentTable,
    ignore_inline_for: Option<usize>,
    ignore_leading_for: Option<usize>,
) -> bool {
    let index = form.span.index;
    if comments.has_dangling(index) || comments.has_opening_inline(index) {
        return true;
    }
    if comments.has_inline_trailing(index) && Some(index) != ignore_inline_for {
        return true;
    }
    if comments.leading.contains_key(&index) && Some(index) != ignore_leading_for {
        return true;
    }
    match &form.kind {
        FormKind::List(items)
        | FormKind::Vector(items)
        | FormKind::Set(items)
        | FormKind::ShortFn(items) => items
            .iter()
            .any(|item| form_has_internal_comments(item, comments, None, None)),
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => {
                form_has_internal_comments(k, comments, None, None)
                    || form_has_internal_comments(v, comments, None, None)
            }
            MapItem::Spread(expr) => form_has_internal_comments(expr, comments, None, None),
        }),
        FormKind::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_has_internal_comments(expr, comments, None, None),
            InterpolatedPart::Text(_) => false,
        }),
        FormKind::InterpolatedRegex { parts, .. } => parts.iter().any(|part| match part {
            InterpolatedPart::Expr(expr) => form_has_internal_comments(expr, comments, None, None),
            InterpolatedPart::Text(_) => false,
        }),
        _ => false,
    }
}

fn write_binding_name(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) {
    if let Some(inline) = inline_binding_name(form, options, comments) {
        out.push_str(&inline);
        return;
    }
    write_form(form, indent, options, comments, out);
}

fn append_type_hint_inline(form: &Form, mut base: String) -> String {
    append_type_hint(form, &mut base);
    base
}

fn symbol_has_inline_type_hint(sym: &str) -> bool {
    let start = match sym.find('<') {
        Some(idx) if idx > 0 => idx,
        _ => return false,
    };
    let mut depth = 0usize;
    for (offset, ch) in sym[start..].char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return start + offset + 1 == sym.len();
                }
            }
            _ => {}
        }
    }
    false
}

fn block_child_indent(indent: usize, head_len: usize, options: &FormatOptions, out: &str) -> usize {
    let head_start = current_column(out).saturating_sub(head_len + 1);
    let inline_indent = head_start + options.indent_width;
    let base_indent = indent + options.indent_width;
    inline_indent.max(base_indent)
}

fn append_trailing_comma_inline(form: &Form, comments: &CommentTable, mut text: String) -> String {
    if comments.has_trailing_comma(form.span.index) {
        text.push(',');
    }
    text
}

fn render_sharp_accessor(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() != 3 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "fn" {
        return None;
    }
    let params = match &items[1].kind {
        FormKind::Vector(params) if params.len() == 1 => params,
        _ => return None,
    };
    let param_sym = match &params[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if param_sym != "__it" {
        return None;
    }
    if form_has_internal_comments(form, comments, Some(form.span.index), Some(form.span.index)) {
        return None;
    }
    let body = &items[2];
    if let Some(indices) = sharp_accessor_indices_from_oop(body) {
        if indices.is_empty() {
            return None;
        }
        let mut buf = String::new();
        buf.push('#');
        buf.push_str(&indices[0].to_string());
        for idx in indices.iter().skip(1) {
            buf.push('.');
            buf.push_str(&idx.to_string());
        }
        return Some(buf);
    }
    let indexed = detect_index_expr(body, comments)?;
    if indexed.default.is_some() {
        return None;
    }
    if !matches!(&indexed.target.kind, FormKind::Symbol(sym) if sym == "__it") {
        return None;
    }
    let mut buf = String::new();
    buf.push('#');
    buf.push('[');
    for (idx, part) in indexed.path.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        let mut text = inline_form_ignore_comments(part.form, options, 0)
            .or_else(|| render_force_inline(part.form, options, comments))?;
        if part.comma_after && !text.ends_with(',') {
            text.push(',');
        }
        buf.push_str(&text);
    }
    buf.push(']');
    Some(buf)
}

fn sharp_accessor_indices_from_oop(form: &Form) -> Option<Vec<i64>> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 3 {
        return None;
    }
    if !matches!(
        &items[0].kind,
        FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM
    ) {
        return None;
    }
    if !matches!(
        &items[1].kind,
        FormKind::Symbol(sym) if sym == "__it"
    ) {
        return None;
    }
    let mut indices = Vec::new();
    for stage in items.iter().skip(2) {
        let stage_items = match &stage.kind {
            FormKind::List(items) => items,
            _ => return None,
        };
        if stage_items.len() != 2 {
            return None;
        }
        if !matches!(
            &stage_items[0].kind,
            FormKind::Symbol(sym) if sym == OOP_INDEX_SYM
        ) {
            return None;
        }
        let index = match &stage_items[1].kind {
            FormKind::Int(value) => *value,
            _ => return None,
        };
        indices.push(index);
    }
    Some(indices)
}

fn inline_form(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if depth > options.inline_depth_limit {
        match &form.kind {
            FormKind::List(items) => {
                if let Some(head) = list_head_symbol(items) {
                    if !is_inline_depth_relaxed_head(head) {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_) | FormKind::ShortFn(_) => {
                return None;
            }
            _ => {}
        }
    }
    if form_has_comments(form, comments)
        || comments.has_comments(form.span.index)
        || comments.has_inline_trailing(form.span.index)
    {
        return None;
    }
    if let Some(accessor) = render_sharp_accessor(form, options, comments) {
        return Some(append_trailing_comma_inline(form, comments, accessor));
    }
    if let Some(dot_chain) = render_dot_chain(form, options, depth, comments) {
        let text = append_type_hint_inline(form, dot_chain);
        return Some(append_trailing_comma_inline(form, comments, text));
    }
    if let Some(deref) = render_deref_expr(form, options, depth, comments) {
        let text = append_type_hint_inline(form, deref);
        return Some(append_trailing_comma_inline(form, comments, text));
    }
    if let Some(oop_chain) = render_oop_chain(form, options, depth, comments) {
        let text = append_type_hint_inline(form, oop_chain);
        if text.len() <= inline_length_limit(options) {
            return Some(append_trailing_comma_inline(form, comments, text));
        }
    }
    if let Some(map_ref) = render_map_ref(form, options, depth, comments) {
        let text = append_type_hint_inline(form, map_ref);
        return Some(append_trailing_comma_inline(form, comments, text));
    }
    if let Some(indexed) = render_index_expr(form, options, depth, comments) {
        let text = append_type_hint_inline(form, indexed);
        return Some(append_trailing_comma_inline(form, comments, text));
    }
    let base = match &form.kind {
        FormKind::Symbol(s) => Some(s.clone()),
        FormKind::Keyword(k) => Some(format!(":{}", k)),
        FormKind::Int(n) => Some(n.to_string()),
        FormKind::Float(n) => Some(format_float(*n)),
        FormKind::String(s) => {
            let mut buf = String::new();
            push_string_literal(&mut buf, s, comments.string_had_escape(form.span.index));
            Some(buf)
        }
        FormKind::InterpolatedString(parts) => inline_interpolated_string(
            parts,
            options,
            depth,
            comments,
            comments.string_had_escape(form.span.index),
        ),
        FormKind::InterpolatedRegex { parts, delim } => {
            inline_interpolated_regex(parts, delim, options, depth, comments)
        }
        FormKind::Bool(b) => Some(b.to_string()),
        FormKind::Nil => Some("nil".into()),
        FormKind::Duration(d) => Some(d.to_string()),
        FormKind::ShortFn(body) => {
            let mut parts = Vec::new();
            for f in body {
                if let Some(inline) = inline_form(f, options, depth + 1, comments) {
                    parts.push(inline);
                } else {
                    return None;
                }
            }
            Some(format!("#({})", parts.join(" ")))
        }
        FormKind::Regex { pattern, delim } => Some(match delim {
            crate::ast::RegexDelim::Slash => format!("/{}/", pattern),
            crate::ast::RegexDelim::Hash => format!("#\"{}\"", pattern),
            crate::ast::RegexDelim::HashSlash => format!("#/{}/", pattern),
        }),
        FormKind::List(_) => inline_list(form, options, depth + 1, comments),
        FormKind::Vector(_) => inline_vector(form, options, depth + 1, comments),
        FormKind::Set(_) => inline_set(form, options, depth + 1, comments),
        FormKind::Map(_) => inline_map(form, options, depth + 1, comments),
        FormKind::ForeignRaw { tag, code } => {
            inline_foreign_with_prefix(tag.as_deref(), code, true)
        }
        FormKind::ForeignBlock { tag, code } => {
            inline_foreign_with_prefix(Some(tag.as_str()), code, false)
        }
        FormKind::ForeignSymbol { tag, path } => {
            let mut buf = String::new();
            buf.push('$');
            if let Some(t) = tag {
                buf.push_str(t);
                buf.push(':');
            }
            buf.push_str(path);
            Some(buf)
        }
    };
    base.map(|value| {
        let mut value = append_type_hint_inline(form, value);
        if comments.has_trailing_comma(form.span.index) {
            value.push(',');
        }
        value
    })
}

fn format_float(value: f64) -> String {
    let mut text = value.to_string();
    if text == "NaN" || text == "inf" || text == "-inf" {
        return text;
    }
    if text.contains('.') || text.contains('e') || text.contains('E') {
        return text;
    }
    text.push_str(".0");
    text
}

fn inline_form_relaxed(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    match &form.kind {
        FormKind::Vector(_) => inline_vector_relaxed(form, options, depth, comments),
        FormKind::Set(_) => inline_set_relaxed(form, options, depth, comments),
        _ => inline_form(form, options, depth, comments),
    }
}

fn inline_form_ignore_comments(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
) -> Option<String> {
    inline_form(form, options, depth, &CommentTable::default())
}

fn inline_foreign_with_prefix(tag: Option<&str>, code: &str, is_dollar: bool) -> Option<String> {
    if code.contains('\n') {
        return None;
    }
    let mut buf = String::new();
    if is_dollar {
        buf.push('$');
    }
    if let Some(t) = tag {
        if !(is_dollar && t == "rb") {
            buf.push_str(t);
        }
    }
    buf.push('{');
    buf.push_str(code);
    buf.push('}');
    Some(buf)
}

fn render_deref_expr(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    _comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() != 2 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "deref" {
        return None;
    }
    let target = inline_form_ignore_comments(&items[1], options, depth + 1)?;
    Some(format!("@{}", target))
}

fn render_map_ref(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    let items = map_ref_items(items)?;
    if items.len() < 2 {
        return None;
    }
    let scope = match &items[1].kind {
        FormKind::Keyword(sym) if sym == "this" => "this",
        FormKind::Keyword(sym) if sym == "root" => "root",
        _ => return None,
    };
    let mut segments = Vec::new();
    for seg in items.iter().skip(2) {
        let rendered = render_path_segment(seg, options, depth + 1, comments)?;
        segments.push(rendered);
    }
    let mut buf = String::new();
    buf.push('&');
    if scope == "this" {
        buf.push('^');
    }
    if segments.is_empty() {
        if scope == "root" {
            return Some("&ref".to_string());
        }
        return None;
    }
    buf.push_str(&segments.join(""));
    Some(buf)
}

fn map_ref_items(items: &[Form]) -> Option<&[Form]> {
    if matches!(items.first().map(|item| &item.kind), Some(FormKind::Symbol(sym)) if sym == MAP_REF_SYM)
    {
        return Some(items);
    }
    if items.len() >= 2
        && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == APPLY_SYM)
        && matches!(&items[1].kind, FormKind::Symbol(sym) if sym == MAP_REF_SYM)
    {
        return Some(&items[1..]);
    }
    None
}

fn render_oop_chain(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 3 {
        return None;
    }
    if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM) {
        return None;
    }
    let base_str = inline_form_ignore_comments(&items[1], options, depth)
        .or_else(|| render_force_inline(&items[1], options, comments))?;
    let mut buf = String::new();
    buf.push_str(&base_str);
    let mut nil_safe_next = false;
    let mut force_keyword_dot = false;
    for stage in items.iter().skip(2) {
        if is_oop_nil_safe_marker(stage) {
            nil_safe_next = true;
            continue;
        }
        let stage_str = render_oop_stage(stage, options, depth, comments)?;
        let keyword_stage = is_oop_keyword_index_stage(stage);
        let mut used_dot = false;
        if nil_safe_next {
            buf.push_str("?.");
            used_dot = true;
            nil_safe_next = false;
        } else if force_keyword_dot && keyword_stage {
            buf.push('.');
            used_dot = true;
        } else if !keyword_stage {
            buf.push('.');
            used_dot = true;
        }
        buf.push_str(&stage_str);
        force_keyword_dot = keyword_stage && used_dot;
    }
    Some(buf)
}

fn oop_chain_has_internal_comments(form: &Form, comments: &CommentTable) -> bool {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return true,
    };
    if items.len() < 2 {
        return false;
    }
    let index = form.span.index;
    if comments.has_dangling(index) || comments.has_opening_inline(index) {
        return true;
    }
    if comments.leading.contains_key(&index) {
        return true;
    }
    let head_index = items[0].span.index;
    for item in items.iter() {
        let ignore_inline = if item.span.index == head_index {
            Some(item.span.index)
        } else {
            None
        };
        if form_has_internal_comments(item, comments, ignore_inline, None) {
            return true;
        }
    }
    false
}

fn write_oop_chain(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    if items.len() < 3 {
        return false;
    }
    if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_SYNTAX_SYM) {
        return false;
    }
    let inline = render_oop_chain(form, options, 0, comments);
    let has_internal = oop_chain_has_internal_comments(form, comments);
    if let Some(inline) = inline.as_ref() {
        if !has_internal && inline.len() <= inline_length_limit(options) {
            out.push_str(inline);
            return true;
        }
    }

    let mut base_buf = String::new();
    write_form(&items[1], indent, options, comments, &mut base_buf);
    if base_buf.ends_with('\n') {
        out.push_str(&base_buf);
        let mut nil_safe_next = false;
        for stage in items.iter().skip(2) {
            if is_oop_nil_safe_marker(stage) {
                nil_safe_next = true;
                continue;
            }
            let keyword_stage = is_oop_keyword_index_stage(stage);
            let force_dot = keyword_stage;
            if !out.ends_with('\n') {
                out.push('\n');
            }
            write_indent(indent, out);
            if !write_oop_stage(
                stage,
                indent,
                options,
                comments,
                out,
                nil_safe_next,
                force_dot,
            ) {
                if let Some(inline) = inline {
                    out.push_str(&inline);
                    return true;
                }
                return false;
            }
            nil_safe_next = false;
        }
        return true;
    }
    out.push_str(&base_buf);
    let mut nil_safe_next = false;
    let mut force_keyword_dot = false;
    for stage in items.iter().skip(2) {
        if is_oop_nil_safe_marker(stage) {
            nil_safe_next = true;
            continue;
        }
        let keyword_stage = is_oop_keyword_index_stage(stage);
        let force_dot = force_keyword_dot && keyword_stage;
        if !write_oop_stage(
            stage,
            indent,
            options,
            comments,
            out,
            nil_safe_next,
            force_dot,
        ) {
            if let Some(inline) = inline {
                out.push_str(&inline);
                return true;
            }
            return false;
        }
        force_keyword_dot = keyword_stage && (nil_safe_next || force_dot);
        nil_safe_next = false;
    }
    true
}

fn write_oop_stage(
    stage: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
    nil_safe_prefix: bool,
    force_keyword_dot: bool,
) -> bool {
    let items = match &stage.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return false,
    };
    match head {
        OOP_BARE_SYM => {
            if items.len() != 2 {
                return false;
            }
            let name = match inline_form_ignore_comments(&items[1], options, 0)
                .or_else(|| render_force_inline(&items[1], options, comments))
            {
                Some(name) => name,
                None => return false,
            };
            if nil_safe_prefix {
                out.push_str("?.");
            } else {
                out.push('.');
            }
            out.push_str(&name);
            true
        }
        OOP_DOT_STAGE_SYM => {
            if let Some(stage_str) = render_oop_stage(stage, options, 0, comments) {
                if nil_safe_prefix {
                    out.push_str("?.");
                } else {
                    out.push('.');
                }
                out.push_str(&stage_str);
                return true;
            }
            let items = match &stage.kind {
                FormKind::List(items) => items,
                _ => return false,
            };
            if items.len() != 3 {
                return false;
            }
            let placeholder = match &items[1].kind {
                FormKind::Symbol(sym) => sym.as_str(),
                _ => return false,
            };
            let stage_form = dot_chain_stage_form(&items[2], placeholder);
            if current_column(out) != indent {
                if let Some(stage_str) = render_force_inline(&stage_form, options, comments) {
                    let stage_str = wrap_stage_if_needed(stage_str);
                    if nil_safe_prefix {
                        out.push_str("?.");
                    } else {
                        out.push('.');
                    }
                    out.push_str(&stage_str);
                    return true;
                }
                return false;
            }
            let mut stage_buf = String::new();
            write_form(&stage_form, 0, options, comments, &mut stage_buf);
            let prefix = if nil_safe_prefix { "?." } else { "." };
            push_dot_chain_stage_with_prefix(&stage_buf, indent, prefix, out);
            true
        }
        OOP_AS_SYM | OOP_LET_SYM => {
            let stage_str = match render_oop_stage(stage, options, 0, comments) {
                Some(stage_str) => stage_str,
                None => return false,
            };
            if nil_safe_prefix {
                out.push_str("?.");
            } else {
                out.push('.');
            }
            out.push_str(&stage_str);
            true
        }
        OOP_INDEX_SYM => {
            if items.len() != 2 {
                return false;
            }
            let stage_str = match render_path_segment(&items[1], options, 0, comments) {
                Some(stage_str) => stage_str,
                None => return false,
            };
            if nil_safe_prefix {
                out.push_str("?.");
            } else if force_keyword_dot || !is_oop_keyword_index_stage(stage) {
                out.push('.');
            }
            out.push_str(&stage_str);
            true
        }
        _ => {
            let arg_count = items.len().saturating_sub(1);
            let stage_start = current_column(out);
            let mut inline_args = Vec::new();
            let mut can_inline = true;
            for arg in items.iter().skip(1) {
                if let Some(text) = inline_form(arg, options, 0, comments) {
                    inline_args.push(text);
                } else {
                    can_inline = false;
                    break;
                }
            }
            if can_inline {
                let joined = inline_args.join(" ");
                let stage_inline = if arg_count == 0 {
                    if nil_safe_prefix {
                        format!("?.{}()", head)
                    } else {
                        format!(".{}()", head)
                    }
                } else {
                    if nil_safe_prefix {
                        format!("?.{}({})", head, joined)
                    } else {
                        format!(".{}({})", head, joined)
                    }
                };
                let inline_limit = inline_length_limit(options);
                if stage_start + stage_inline.len() <= inline_limit {
                    out.push_str(&stage_inline);
                    return true;
                }
                if arg_count == 1 {
                    let line_limit = options.max_inline_chars;
                    if stage_start + stage_inline.len() <= line_limit {
                        out.push_str(&stage_inline);
                        return true;
                    }
                    let base_indent = indent + options.indent_width;
                    if base_indent + inline_args[0].len() > line_limit {
                        out.push_str(&stage_inline);
                        return true;
                    }
                }
            }
            if nil_safe_prefix {
                out.push_str("?.");
            } else {
                out.push('.');
            }
            out.push_str(head);
            out.push('(');
            if arg_count == 0 {
                out.push(')');
                return true;
            }
            let inline_arg_indent = current_column(out);
            if !can_inline && arg_count == 1 {
                let arg = &items[1];
                if !form_has_comments(arg, comments) {
                    write_form(arg, inline_arg_indent, options, comments, out);
                    if out.ends_with('\n') {
                        write_indent(indent, out);
                    }
                    out.push(')');
                    return true;
                }
            }
            let arg_indent = indent + options.indent_width;
            for arg in items.iter().skip(1) {
                ensure_newline_for_form(arg, comments, out);
                write_indent(arg_indent, out);
                write_form(arg, arg_indent, options, comments, out);
            }
            if out.ends_with('\n') {
                write_indent(indent, out);
            }
            out.push(')');
            true
        }
    }
}

fn is_oop_keyword_index_stage(stage: &Form) -> bool {
    let items = match &stage.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    if items.len() != 2 {
        return false;
    }
    if !matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_INDEX_SYM) {
        return false;
    }
    matches!(&items[1].kind, FormKind::Keyword(_))
}

fn is_oop_nil_safe_marker(stage: &Form) -> bool {
    let items = match &stage.kind {
        FormKind::List(items) => items,
        _ => return false,
    };
    if items.len() != 1 {
        return false;
    }
    matches!(&items[0].kind, FormKind::Symbol(sym) if sym == OOP_NIL_SAFE_SYM)
}

fn render_oop_stage(
    stage: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &stage.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    match head {
        OOP_BARE_SYM => {
            if items.len() != 2 {
                return None;
            }
            match &items[1].kind {
                FormKind::Symbol(sym) => Some(sym.clone()),
                _ => None,
            }
        }
        OOP_DOT_STAGE_SYM => {
            if items.len() != 3 {
                return None;
            }
            let placeholder = match &items[1].kind {
                FormKind::Symbol(sym) => sym.as_str(),
                _ => return None,
            };
            render_dot_stage(&items[2], placeholder, options, depth + 1, comments)
        }
        OOP_AS_SYM | OOP_LET_SYM => {
            if items.len() != 2 {
                return None;
            }
            let arg = inline_form_ignore_comments(&items[1], options, depth + 1)
                .or_else(|| render_force_inline(&items[1], options, comments))?;
            let name = if head == OOP_AS_SYM { "as" } else { "let" };
            Some(format!("({} {})", name, arg))
        }
        OOP_INDEX_SYM => {
            if items.len() != 2 {
                return None;
            }
            render_path_segment(&items[1], options, depth + 1, comments)
        }
        _ => {
            let mut args = Vec::new();
            for arg in items.iter().skip(1) {
                let rendered = inline_form_ignore_comments(arg, options, depth + 1)
                    .or_else(|| render_force_inline(arg, options, comments))?;
                args.push(rendered);
            }
            if args.is_empty() {
                return Some(format!("{}()", head));
            }
            Some(format!("{}({})", head, args.join(" ")))
        }
    }
}

fn render_path_segment(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    match &form.kind {
        FormKind::Keyword(sym) => Some(format!(":{}", sym)),
        FormKind::String(sym) => {
            let mut buf = String::new();
            push_string_literal(&mut buf, sym, comments.string_had_escape(form.span.index));
            Some(buf)
        }
        FormKind::Int(n) => Some(n.to_string()),
        FormKind::Symbol(sym) if sym == ".." => Some("../".to_string()),
        FormKind::Symbol(sym) => Some(sym.clone()),
        FormKind::List(items)
            if items.len() == 4
                && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "__range_literal")
                && matches!(&items[1].kind, FormKind::Nil)
                && matches!(&items[2].kind, FormKind::Nil)
                && matches!(&items[3].kind, FormKind::Bool(false)) =>
        {
            Some("../".to_string())
        }
        FormKind::List(items)
            if items.len() == 2
                && matches!(&items[0].kind, FormKind::Symbol(sym) if sym == "quote") =>
        {
            match &items[1].kind {
                FormKind::Symbol(sym) => Some(format!("'{}", sym)),
                _ => inline_form_ignore_comments(form, options, depth + 1),
            }
        }
        _ => None,
    }
}

fn render_index_expr(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let data = detect_index_expr(form, comments)?;
    let mut buf = String::new();
    let target_text = inline_form_ignore_comments(data.target, options, depth + 1)
        .or_else(|| render_force_inline(data.target, options, comments))?;
    buf.push_str(&target_text);
    buf.push('[');
    for (idx, part) in data.path.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        let mut text = inline_form_ignore_comments(part.form, options, depth + 1)
            .or_else(|| render_force_inline(part.form, options, comments))?;
        if part.comma_after && !text.ends_with(',') {
            text.push(',');
        }
        buf.push_str(&text);
    }
    if let Some(default_form) = data.default {
        buf.push_str(" || ");
        let default_text = inline_form_ignore_comments(default_form, options, depth + 1)
            .or_else(|| render_force_inline(default_form, options, comments))?;
        buf.push_str(&default_text);
    }
    buf.push(']');
    Some(buf)
}

fn dot_chain_parts(form: &Form) -> Option<(&Form, &str, &[Form])> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 4 {
        return None;
    }
    let head_sym = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head_sym != "as->" {
        return None;
    }
    let placeholder = match &items[2].kind {
        FormKind::Symbol(sym) if sym.starts_with(crate::reader::DOT_CHAIN_PLACEHOLDER_PREFIX) => {
            sym.as_str()
        }
        _ => return None,
    };
    Some((&items[1], placeholder, &items[3..]))
}

fn write_dot_chain(
    form: &Form,
    indent: usize,
    options: &FormatOptions,
    comments: &mut CommentTable,
    out: &mut String,
) -> bool {
    if let Some(dot_chain) = render_dot_chain(form, options, 0, &*comments) {
        out.push_str(&dot_chain);
        return true;
    }
    let Some((base, placeholder, stages)) = dot_chain_parts(form) else {
        return false;
    };
    if dot_chain_has_rewrite_sensitive_comments(form, placeholder, comments) {
        return false;
    }
    let mut base_buf = String::new();
    write_form(base, indent, options, comments, &mut base_buf);
    out.push_str(&base_buf);
    for stage in stages {
        out.push('\n');
        let stage_form = dot_chain_stage_form(stage, placeholder);
        let mut stage_buf = String::new();
        write_form(&stage_form, 0, options, comments, &mut stage_buf);
        push_dot_chain_stage(&stage_buf, indent, out);
    }
    true
}

fn render_dot_chain(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    _comments: &CommentTable,
) -> Option<String> {
    let (base, placeholder, stages) = dot_chain_parts(form)?;
    let base_str = inline_form_ignore_comments(base, options, depth + 1)?;
    let mut buf = String::new();
    buf.push_str(&base_str);
    for stage in stages {
        let stage_str = render_dot_stage(stage, placeholder, options, depth + 1, _comments)?;
        buf.push('.');
        buf.push_str(&stage_str);
    }
    Some(buf)
}

fn render_dot_stage(
    stage: &Form,
    placeholder: &str,
    options: &FormatOptions,
    depth: usize,
    _comments: &CommentTable,
) -> Option<String> {
    let replaced = dot_chain_stage_form(stage, placeholder);
    let text = inline_form_ignore_comments(&replaced, options, depth + 1)?;
    Some(wrap_stage_if_needed(text))
}

fn dot_chain_stage_form(stage: &Form, placeholder: &str) -> Form {
    if let FormKind::List(items) = &stage.kind {
        if items.len() >= 3 {
            if let FormKind::Symbol(head) = &items[0].kind {
                if head == "apply" {
                    if matches!(&items.last().unwrap().kind, FormKind::Symbol(sym) if sym == placeholder)
                    {
                        let replaced: Vec<Form> = items[1..]
                            .iter()
                            .map(|f| replace_placeholder(f, placeholder, "*?"))
                            .collect();
                        return Form::new(FormKind::List(replaced), stage.span);
                    }
                }
            }
        }
    }
    replace_placeholder(stage, placeholder, "?")
}

fn dot_chain_has_rewrite_sensitive_comments(
    form: &Form,
    placeholder: &str,
    comments: &CommentTable,
) -> bool {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return true,
    };
    if items.len() < 3 {
        return true;
    }
    let head = &items[0];
    let placeholder_form = &items[2];
    if !matches!(
        &placeholder_form.kind,
        FormKind::Symbol(sym) if sym == placeholder
    ) {
        return true;
    }
    let sensitive_indices = [
        form.span.index,
        head.span.index,
        placeholder_form.span.index,
    ];
    for index in sensitive_indices {
        if comments.has_dangling(index)
            || comments.has_opening_inline(index)
            || comments.has_inline_trailing(index)
            || comments.leading.contains_key(&index)
        {
            return true;
        }
    }
    false
}

fn push_dot_chain_stage(stage: &str, indent: usize, out: &mut String) {
    push_dot_chain_stage_with_prefix(stage, indent, ".", out);
}

fn push_dot_chain_stage_with_prefix(stage: &str, indent: usize, prefix: &str, out: &mut String) {
    for (idx, line) in stage.lines().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        write_indent(indent, out);
        if idx == 0 {
            out.push_str(prefix);
        }
        out.push_str(line);
    }
}

fn wrap_stage_if_needed(stage: String) -> String {
    if stage.starts_with('(') {
        stage
    } else {
        format!("({})", stage)
    }
}

fn replace_placeholder(form: &Form, placeholder: &str, replacement: &str) -> Form {
    let kind = match &form.kind {
        FormKind::Symbol(sym) if sym == placeholder => FormKind::Symbol(replacement.to_string()),
        FormKind::List(items) => FormKind::List(
            items
                .iter()
                .map(|f| replace_placeholder(f, placeholder, replacement))
                .collect(),
        ),
        FormKind::Vector(items) => FormKind::Vector(
            items
                .iter()
                .map(|f| replace_placeholder(f, placeholder, replacement))
                .collect(),
        ),
        FormKind::Set(items) => FormKind::Set(
            items
                .iter()
                .map(|f| replace_placeholder(f, placeholder, replacement))
                .collect(),
        ),
        FormKind::Map(entries) => FormKind::Map(
            entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => MapItem::KeyValue(
                        replace_placeholder(k, placeholder, replacement),
                        replace_placeholder(v, placeholder, replacement),
                    ),
                    MapItem::Spread(expr) => {
                        MapItem::Spread(replace_placeholder(expr, placeholder, replacement))
                    }
                })
                .collect(),
        ),
        FormKind::ShortFn(items) => FormKind::ShortFn(
            items
                .iter()
                .map(|f| replace_placeholder(f, placeholder, replacement))
                .collect(),
        ),
        FormKind::ForeignBlock { tag, code } => FormKind::ForeignBlock {
            tag: tag.clone(),
            code: code.clone(),
        },
        FormKind::ForeignRaw { tag, code } => FormKind::ForeignRaw {
            tag: tag.clone(),
            code: code.clone(),
        },
        FormKind::ForeignSymbol { tag, path } => FormKind::ForeignSymbol {
            tag: tag.clone(),
            path: path.clone(),
        },
        other => other.clone(),
    };
    Form::new(kind, form.span)
}

fn detect_index_expr<'a>(form: &'a Form, comments: &CommentTable) -> Option<IndexExpr<'a>> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.len() < 3 || items.len() > 4 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    let target = &items[1];
    let default = items.get(3);
    match head {
        crate::reader::INDEX_GET_SYM => Some(IndexExpr {
            target,
            path: vec![IndexPart {
                form: &items[2],
                comma_after: comments.has_trailing_comma(items[2].span.index),
            }],
            default,
        }),
        crate::reader::INDEX_GET_IN_SYM => match &items[2].kind {
            FormKind::Vector(path_items) if !path_items.is_empty() => Some(IndexExpr {
                target,
                path: path_items
                    .iter()
                    .map(|item| IndexPart {
                        form: item,
                        comma_after: comments.has_trailing_comma(item.span.index),
                    })
                    .collect(),
                default,
            }),
            _ => None,
        },
        crate::reader::INDEX_GET_MANY_SYM => match &items[2].kind {
            FormKind::Vector(path_items) if !path_items.is_empty() => Some(IndexExpr {
                target,
                path: collect_index_many_parts(path_items, comments),
                default,
            }),
            _ => None,
        },
        _ => None,
    }
}

fn collect_index_many_parts<'a>(
    path_items: &'a [Form],
    comments: &CommentTable,
) -> Vec<IndexPart<'a>> {
    let mut parts = Vec::new();
    if path_items.is_empty() {
        return parts;
    }
    let last_group = path_items.len().saturating_sub(1);
    for (group_idx, item) in path_items.iter().enumerate() {
        match &item.kind {
            FormKind::Vector(inner) | FormKind::List(inner) => {
                if inner.is_empty() {
                    continue;
                }
                let last_inner = inner.len().saturating_sub(1);
                for (idx, inner_item) in inner.iter().enumerate() {
                    let mut comma_after = comments.has_trailing_comma(inner_item.span.index);
                    if idx == last_inner && group_idx < last_group {
                        comma_after = true;
                    }
                    parts.push(IndexPart {
                        form: inner_item,
                        comma_after,
                    });
                }
            }
            _ => {
                let mut comma_after = comments.has_trailing_comma(item.span.index);
                if group_idx < last_group {
                    comma_after = true;
                }
                parts.push(IndexPart {
                    form: item,
                    comma_after,
                });
            }
        }
    }
    parts
}

struct IndexExpr<'a> {
    target: &'a Form,
    path: Vec<IndexPart<'a>>,
    default: Option<&'a Form>,
}

struct IndexPart<'a> {
    form: &'a Form,
    comma_after: bool,
}

fn collect_inline_strings_ignore_comments<'a, I>(
    forms: I,
    options: &FormatOptions,
) -> Option<Vec<String>>
where
    I: IntoIterator<Item = &'a Form>,
{
    let mut strings = Vec::new();
    for form in forms {
        if let Some(text) = inline_form_ignore_comments(form, options, 0) {
            strings.push(text);
        } else {
            return None;
        }
    }
    Some(strings)
}

fn is_simple_inline_list_arg(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> bool {
    if form_has_comments(form, comments) {
        return false;
    }
    render_oop_chain(form, options, 0, comments).is_some()
        || render_dot_chain(form, options, 0, comments).is_some()
        || render_deref_expr(form, options, 0, comments).is_some()
        || render_index_expr(form, options, 0, comments).is_some()
        || render_map_ref(form, options, 0, comments).is_some()
}

fn inline_list(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return Some("()".into());
    }
    let head_symbol = list_head_symbol(items);
    if let Some(range) = inline_range_literal(items, options, depth, comments) {
        return Some(range);
    }
    if matches!(items.first().map(|f| &f.kind), Some(FormKind::Vector(_))) {
        return None;
    }
    if let Some(head) = head_symbol {
        if is_thread_macro_head(head) && options.flow_head_policy == FlowHeadPolicy::Multiline {
            return None;
        }
        if is_flow_head(head) && options.flow_head_policy == FlowHeadPolicy::Multiline {
            return None;
        }
        if map_sugar_binding_index(head, items).is_some() {
            return None;
        }
        if is_defn_head(head) || is_named_method_head(head, items) {
            if let Some(inline) = inline_small_defn(head, items, options, depth, comments) {
                return Some(inline);
            }
            return None;
        }
        if head == "fn" {
            if let Some(inline) = inline_small_fn(items, options, depth, comments) {
                return Some(inline);
            }
            return None;
        }
        if head == "quote" && items.len() == 2 {
            let quoted = inline_form(&items[1], options, depth + 1, comments)?;
            let buf = format!("'{}", quoted);
            return Some(buf);
        }
        if head == "if" {
            if let Some(inline) = inline_small_if(items, options, depth, comments) {
                return Some(inline);
            }
            return None;
        }
        if is_block_head(head) {
            return None;
        }
    }
    let relax_require = matches!(head_symbol, Some("require"));
    let inline_item = |item: &Form| {
        if relax_require {
            inline_form_relaxed(item, options, depth, comments)
        } else {
            inline_form(item, options, depth, comments)
        }
    };
    let has_collection_arg = items.iter().skip(1).any(|item| {
        matches!(
            item.kind,
            FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
        )
    });
    let has_nested_arg = items.iter().skip(1).any(|item| match &item.kind {
        FormKind::List(_) => !is_simple_inline_list_arg(item, options, comments),
        FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_) => true,
        _ => false,
    });
    if items.len() > 3 && has_nested_arg && !relax_require {
        if let Some(head) = head_symbol {
            if !is_flow_head(head) {
                return None;
            }
        } else {
            return None;
        }
    }
    let mut buf = String::new();
    buf.push('(');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_item(item)?);
    }
    buf.push(')');
    let mut inline_limit = inline_length_limit(options);
    if has_collection_arg && !options.inline_allow_nested_collections {
        inline_limit = inline_limit.saturating_div(2);
    }
    if let Some(head) = head_symbol {
        if is_flow_head(head) {
            if let Some(flow_limit) = flow_head_inline_limit(options) {
                inline_limit = inline_limit.min(flow_limit);
            } else {
                return None;
            }
        }
    }
    if buf.len() <= inline_limit {
        Some(buf)
    } else {
        None
    }
}

fn inline_range_literal(
    items: &[Form],
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if items.len() != 4 {
        return None;
    }
    let head = match &items[0].kind {
        FormKind::Symbol(sym) => sym.as_str(),
        _ => return None,
    };
    if head != "__range_literal" {
        return None;
    }
    let exclusive = match &items[3].kind {
        FormKind::Bool(flag) => *flag,
        _ => return None,
    };
    let start = if matches!(items[1].kind, FormKind::Nil) {
        String::new()
    } else {
        inline_form(&items[1], options, depth + 1, comments)?
    };
    let end = if matches!(items[2].kind, FormKind::Nil) {
        String::new()
    } else {
        inline_form(&items[2], options, depth + 1, comments)?
    };
    let op = if exclusive { "..." } else { ".." };
    Some(format!("{}{}{}", start, op, end))
}

fn inline_small_fn(
    items: &[Form],
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if items.len() != 3 {
        return None;
    }
    if !matches!(items[1].kind, FormKind::Vector(_)) {
        return None;
    }
    let params = inline_fn_args_vector_text(&items[1], options, comments)?;
    let body = inline_form(&items[2], options, depth + 1, comments)?;
    let buf = format!("(fn {} {})", params, body);
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_small_if(
    items: &[Form],
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if items.len() != 4 {
        return None;
    }
    let cond = inline_form_relaxed(&items[1], options, depth, comments)?;
    let then = inline_form_relaxed(&items[2], options, depth, comments)?;
    let else_expr = inline_form_relaxed(&items[3], options, depth, comments)?;
    let buf = format!("(if {} {} {})", cond, then, else_expr);
    if buf.len() <= inline_length_limit(options).saturating_mul(2) {
        Some(buf)
    } else {
        None
    }
}

fn inline_if_force(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::List(items) => items,
        _ => return None,
    };
    if list_head_symbol(items) != Some("if") || items.len() != 4 {
        return None;
    }
    let cond = render_flat(&items[1], options, Some(comments));
    let then = render_flat(&items[2], options, Some(comments));
    let else_expr = render_flat(&items[3], options, Some(comments));
    let buf = format!("(if {} {} {})", cond, then, else_expr);
    if buf.len() <= inline_length_limit(options).saturating_mul(2) {
        Some(buf)
    } else {
        None
    }
}

fn render_flat(form: &Form, options: &FormatOptions, comments: Option<&CommentTable>) -> String {
    if let Some(table) = comments {
        if let Some(text) = render_force_inline(form, options, table) {
            return text;
        }
    }
    let mut buf = match &form.kind {
        FormKind::Symbol(s) => s.clone(),
        FormKind::Keyword(k) => format!(":{}", k),
        FormKind::Int(n) => n.to_string(),
        FormKind::Float(n) => format_float(*n),
        FormKind::String(s) => {
            let mut buf = String::new();
            let escape_newlines = comments
                .map(|c| c.string_had_escape(form.span.index))
                .unwrap_or(true);
            push_string_literal(&mut buf, s, escape_newlines);
            buf
        }
        FormKind::Bool(b) => {
            if *b {
                "true".into()
            } else {
                "false".into()
            }
        }
        FormKind::Nil => "nil".into(),
        FormKind::Duration(d) => d.to_string(),
        FormKind::List(items) => {
            if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "quote" && items.len() == 2 {
                    return format!("'{}", render_flat(&items[1], options, comments));
                }
            }
            let parts: Vec<String> = items
                .iter()
                .map(|f| render_flat(f, options, comments))
                .collect();
            format!("({})", parts.join(" "))
        }
        FormKind::Vector(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|f| render_flat(f, options, comments))
                .collect();
            format!("[{}]", parts.join(" "))
        }
        FormKind::Set(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|f| render_flat(f, options, comments))
                .collect();
            format!("#{{{}}}", parts.join(" "))
        }
        FormKind::Map(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        format!(
                            "{} {}",
                            render_flat(k, options, comments),
                            render_flat(v, options, comments)
                        )
                    }
                    MapItem::Spread(expr) => format!("*{}", render_flat(expr, options, comments)),
                })
                .collect();
            format!("{{{}}}", parts.join(" "))
        }
        FormKind::ShortFn(body) => {
            let parts: Vec<String> = body
                .iter()
                .map(|f| render_flat(f, options, comments))
                .collect();
            format!("#({})", parts.join(" "))
        }
        FormKind::Regex { pattern, delim } => match delim {
            crate::ast::RegexDelim::Slash => format!("/{}/", pattern),
            crate::ast::RegexDelim::Hash => format!("#\"{}\"", pattern),
            crate::ast::RegexDelim::HashSlash => format!("#/{}/", pattern),
        },
        FormKind::ForeignSymbol { tag, path } => {
            let mut buf = String::new();
            if let Some(t) = tag {
                buf.push('$');
                buf.push_str(t);
                buf.push(':');
            }
            buf.push_str(path);
            buf
        }
        FormKind::ForeignRaw { tag, code } => {
            let mut buf = String::new();
            if let Some(t) = tag {
                buf.push('$');
                buf.push_str(t);
            }
            buf.push('(');
            buf.push_str(code);
            buf.push(')');
            buf
        }
        FormKind::ForeignBlock { tag, code } => {
            let mut buf = String::new();
            buf.push_str(tag);
            buf.push('(');
            buf.push_str(code);
            buf.push(')');
            buf
        }
        _ => form_to_string(form, ""),
    };
    if let Some(table) = comments {
        if table.has_trailing_comma(form.span.index) {
            buf.push(',');
        }
    }
    buf
}

fn render_force_inline(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    if let Some(dot_chain) = render_dot_chain(form, options, 0, comments) {
        return Some(dot_chain);
    }
    if let Some(deref) = render_deref_expr(form, options, 0, comments) {
        return Some(deref);
    }
    if let Some(oop_chain) = render_oop_chain(form, options, 0, comments) {
        return Some(oop_chain);
    }
    if let Some(map_ref) = render_map_ref(form, options, 0, comments) {
        return Some(map_ref);
    }
    if let Some(indexed) = render_index_expr(form, options, 0, comments) {
        return Some(indexed);
    }
    let mut buf = match &form.kind {
        FormKind::Symbol(s) => s.clone(),
        FormKind::Keyword(k) => format!(":{}", k),
        FormKind::Int(n) => n.to_string(),
        FormKind::Float(n) => format_float(*n),
        FormKind::String(s) => {
            let mut buf = String::new();
            let escape_newlines = comments.string_had_escape(form.span.index);
            push_string_literal(&mut buf, s, escape_newlines);
            buf
        }
        FormKind::Bool(b) => {
            if *b {
                "true".into()
            } else {
                "false".into()
            }
        }
        FormKind::Nil => "nil".into(),
        FormKind::Duration(d) => d.to_string(),
        FormKind::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else if let Some(FormKind::Symbol(head)) = items.first().map(|f| &f.kind) {
                if head == "quote" && items.len() == 2 {
                    let quoted = render_force_inline(&items[1], options, comments)?;
                    format!("'{}", quoted)
                } else {
                    let parts: Vec<String> = items
                        .iter()
                        .map(|item| render_force_inline(item, options, comments))
                        .collect::<Option<Vec<_>>>()?;
                    format!("({})", parts.join(" "))
                }
            } else {
                let parts: Vec<String> = items
                    .iter()
                    .map(|item| render_force_inline(item, options, comments))
                    .collect::<Option<Vec<_>>>()?;
                format!("({})", parts.join(" "))
            }
        }
        FormKind::Vector(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|item| render_force_inline(item, options, comments))
                .collect::<Option<Vec<_>>>()?;
            format!("[{}]", parts.join(" "))
        }
        FormKind::Set(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|item| render_force_inline(item, options, comments))
                .collect::<Option<Vec<_>>>()?;
            format!("#{{{}}}", parts.join(" "))
        }
        FormKind::Map(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| match entry {
                    MapItem::KeyValue(k, v) => {
                        if let Some(base) = render_map_shorthand_key(k, v, options, comments) {
                            Some(format!("{},", base))
                        } else {
                            let key = render_force_inline(k, options, comments)?;
                            let value = render_force_inline(v, options, comments)?;
                            Some(format!("{} {}", key, value))
                        }
                    }
                    MapItem::Spread(expr) => {
                        let spread = render_force_inline(expr, options, comments)?;
                        Some(format!("* {}", spread))
                    }
                })
                .collect::<Option<Vec<_>>>()?;
            format!("{{{}}}", parts.join(" "))
        }
        FormKind::ShortFn(body) => {
            let parts: Vec<String> = body
                .iter()
                .map(|item| render_force_inline(item, options, comments))
                .collect::<Option<Vec<_>>>()?;
            format!("#({})", parts.join(" "))
        }
        FormKind::Regex { pattern, delim } => match delim {
            crate::ast::RegexDelim::Slash => format!("/{}/", pattern),
            crate::ast::RegexDelim::Hash => format!("#\"{}\"", pattern),
            crate::ast::RegexDelim::HashSlash => format!("#/{}/", pattern),
        },
        FormKind::ForeignSymbol { tag, path } => {
            let mut buf = String::new();
            if let Some(t) = tag {
                buf.push('$');
                buf.push_str(t);
                buf.push(':');
            }
            buf.push_str(path);
            buf
        }
        FormKind::ForeignRaw { tag, code } => {
            inline_foreign_with_prefix(tag.as_deref(), code, true)?
        }
        FormKind::ForeignBlock { tag, code } => {
            inline_foreign_with_prefix(Some(tag.as_str()), code, false)?
        }
        _ => form_to_string(form, ""),
    };
    if comments.has_trailing_comma(form.span.index) {
        buf.push(',');
    }
    Some(buf)
}

fn inline_small_defn(
    head: &str,
    items: &[Form],
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if items.len() != 4 {
        return None;
    }
    if comments.has_comments(items[0].span.index)
        || comments.has_comments(items[1].span.index)
        || comments.has_comments(items[2].span.index)
        || comments.has_comments(items[3].span.index)
    {
        return None;
    }
    let name = inline_form(&items[1], options, depth + 1, comments)?;
    let params = inline_fn_args_vector_text(&items[2], options, comments)?;
    let body = inline_form(&items[3], options, depth + 1, comments)?;
    let buf = format!("({} {} {} {})", head, name, params, body);
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_vector(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return Some("[]".into());
    }
    if options.inline_vector_max_items > 0 && items.len() > options.inline_vector_max_items {
        return None;
    }
    if items.len() >= 4
        && items
            .iter()
            .any(|item| matches!(item.kind, FormKind::List(_)))
    {
        return None;
    }
    if !options.inline_allow_nested_collections
        && items.len() > 3
        && items.iter().any(|item| {
            matches!(
                item.kind,
                FormKind::List(_) | FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
            )
        })
    {
        return None;
    }
    let mut buf = String::new();
    buf.push('[');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_form(item, options, depth + 1, comments)?);
    }
    buf.push(']');
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_vector_relaxed(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return Some("[]".into());
    }
    if items.len() > options.inline_vector_max_items_relaxed {
        return None;
    }
    if !options.inline_allow_nested_collections
        && items.len() > 3
        && items.iter().any(|item| {
            matches!(
                item.kind,
                FormKind::List(_) | FormKind::Vector(_) | FormKind::Set(_) | FormKind::Map(_)
            )
        })
    {
        return None;
    }
    let mut buf = String::new();
    buf.push('[');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_form(item, options, depth + 1, comments)?);
    }
    buf.push(']');
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_set(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::Set(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return Some("#{}".into());
    }
    if options.inline_set_max_items > 0 && items.len() > options.inline_set_max_items {
        return None;
    }
    let mut buf = String::new();
    buf.push_str("#{");
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_form(item, options, depth, comments)?);
    }
    buf.push('}');
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_set_relaxed(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::Set(items) => items,
        _ => return None,
    };
    if items.is_empty() {
        return Some("#{}".into());
    }
    if items.len() > options.inline_set_max_items_relaxed {
        return None;
    }
    let mut buf = String::new();
    buf.push_str("#{");
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_form(item, options, depth, comments)?);
    }
    buf.push('}');
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn render_map_shorthand_key(
    key: &Form,
    value: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    if !options.shorthand_map {
        return None;
    }
    if comments.has_comments(key.span.index)
        || comments.has_comments(value.span.index)
        || comments.has_inline_trailing(key.span.index)
        || comments.has_inline_trailing(value.span.index)
    {
        return None;
    }
    if value.type_hint.is_some() {
        return None;
    }
    match (&key.kind, &value.kind) {
        (FormKind::Keyword(k), FormKind::Symbol(v))
            if k == v && !is_map_shorthand_excluded(k, options) =>
        {
            Some(format!(":{}", k))
        }
        _ => None,
    }
}

fn is_map_shorthand_excluded(key: &str, options: &FormatOptions) -> bool {
    options
        .shorthand_map_exclude_keys
        .iter()
        .any(|exclude| exclude == key)
}

fn inline_map(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => return None,
    };
    if entries.is_empty() {
        return Some("{}".into());
    }
    let has_shorthand = entries.iter().any(|entry| match entry {
        MapItem::KeyValue(k, v) => render_map_shorthand_key(k, v, options, comments).is_some(),
        MapItem::Spread(_) => false,
    });
    if !has_shorthand && entries.len() > options.inline_map_max_entries {
        return None;
    }
    if entries
        .iter()
        .any(|entry| matches!(entry, MapItem::Spread(_)))
    {
        return None;
    }
    if !options.inline_map_allow_complex_values {
        let simple_value = |v: &Form| is_simple_map_value(v, options, depth, comments);
        if entries.iter().any(|entry| match entry {
            MapItem::KeyValue(_, v) => !simple_value(v),
            MapItem::Spread(_) => false,
        }) {
            return None;
        }
    }
    let mut buf = String::new();
    buf.push('{');
    for (idx, entry) in entries.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v, options, comments) {
                    buf.push_str(&format!("{},", base));
                } else {
                    buf.push_str(&inline_form(k, options, depth, comments)?);
                    buf.push(' ');
                    buf.push_str(&inline_form(v, options, depth, comments)?);
                }
            }
            MapItem::Spread(expr) => {
                buf.push('*');
                buf.push(' ');
                buf.push_str(&inline_form(expr, options, depth, comments)?);
            }
        }
    }
    buf.push('}');
    if buf.len() <= inline_map_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_fn_arg_form(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    if depth > options.inline_depth_limit {
        return None;
    }
    if comments.has_comments(form.span.index) || comments.has_inline_trailing(form.span.index) {
        return None;
    }
    match &form.kind {
        FormKind::Symbol(_) => inline_binding_name(form, options, comments),
        FormKind::Vector(_) => inline_vector_args(form, options, depth, comments),
        FormKind::Map(_) => inline_map_relaxed(form, options, depth, comments),
        _ => inline_form(form, options, depth, comments),
    }
}

fn inline_fn_args_vector_text(
    form: &Form,
    options: &FormatOptions,
    comments: &CommentTable,
) -> Option<String> {
    let inline = inline_vector_args(form, options, 0, comments)?;
    Some(append_type_hint_inline(form, inline))
}

fn inline_vector_args(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let items = match &form.kind {
        FormKind::Vector(items) => items,
        _ => return None,
    };
    if items.len() > 8 {
        return None;
    }
    let mut buf = String::new();
    buf.push('[');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        buf.push_str(&inline_fn_arg_form(item, options, depth + 1, comments)?);
    }
    buf.push(']');
    if buf.len() <= inline_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn inline_map_relaxed(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> Option<String> {
    let entries = match &form.kind {
        FormKind::Map(entries) => entries,
        _ => return None,
    };
    if entries.is_empty() {
        return Some("{}".into());
    }
    let has_shorthand = entries.iter().any(|entry| match entry {
        MapItem::KeyValue(k, v) => render_map_shorthand_key(k, v, options, comments).is_some(),
        MapItem::Spread(_) => false,
    });
    if !has_shorthand && entries.len() > options.inline_map_max_entries_relaxed {
        return None;
    }
    let mut buf = String::new();
    buf.push('{');
    for (idx, entry) in entries.iter().enumerate() {
        if idx > 0 {
            buf.push(' ');
        }
        match entry {
            MapItem::KeyValue(k, v) => {
                if let Some(base) = render_map_shorthand_key(k, v, options, comments) {
                    buf.push_str(&format!("{},", base));
                } else {
                    buf.push_str(&inline_fn_arg_form(k, options, depth + 1, comments)?);
                    buf.push(' ');
                    buf.push_str(&inline_fn_arg_form(v, options, depth + 1, comments)?);
                }
            }
            MapItem::Spread(_) => return None,
        }
    }
    buf.push('}');
    if buf.len() <= inline_map_length_limit(options) {
        Some(buf)
    } else {
        None
    }
}

fn list_head_symbol(items: &[Form]) -> Option<&str> {
    items.first().and_then(|form| match &form.kind {
        FormKind::Symbol(s) => Some(s.as_str()),
        _ => None,
    })
}

fn short_fn_has_complex_body(body: &[Form]) -> bool {
    if let Some(head) = short_fn_head_symbol(body) {
        if is_complex_head(head) {
            return true;
        }
    }
    body.iter().any(form_has_complex_control)
}

fn fn_body_is_complex(items: &[Form]) -> bool {
    if items.len() != 3 {
        return true;
    }
    if !matches!(items[1].kind, FormKind::Vector(_)) {
        return true;
    }
    form_has_complex_control(&items[2])
}

fn form_has_complex_control(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) => list_has_complex_head(items),
        FormKind::ShortFn(body) => short_fn_has_complex_body(body),
        _ => false,
    }
}

fn short_fn_head_symbol(body: &[Form]) -> Option<&str> {
    body.first().and_then(|form| match &form.kind {
        FormKind::Symbol(s) => Some(s.as_str()),
        _ => None,
    })
}

fn list_has_complex_head(items: &[Form]) -> bool {
    list_head_symbol(items)
        .map(is_complex_head)
        .unwrap_or(false)
}

fn is_complex_head(head: &str) -> bool {
    matches!(
        head,
        "when"
            | "let"
            | "let*"
            | "loop"
            | "binding"
            | "do"
            | "doseq"
            | "each"
            | "for"
            | "cond"
            | "case"
            | "match"
            | "if"
            | "try"
            | "catch"
            | "finally"
            | "err"
            | "fin"
    )
}

fn is_inline_depth_relaxed_head(head: &str) -> bool {
    matches!(
        head,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | "<="
            | ">"
            | ">="
            | "="
            | "=="
            | "!="
            | "not="
            | "int"
            | "float"
            | OOP_SYNTAX_SYM
    )
}

fn is_simple_map_value(
    form: &Form,
    options: &FormatOptions,
    depth: usize,
    comments: &CommentTable,
) -> bool {
    if comments.has_comments(form.span.index) || comments.has_inline_trailing(form.span.index) {
        return false;
    }
    match &form.kind {
        FormKind::ShortFn(body) => {
            if short_fn_has_complex_body(body) {
                return false;
            }
            inline_form(form, options, depth + 1, comments)
                .map(|s| s.len() <= inline_map_length_limit(options))
                .unwrap_or(false)
        }
        FormKind::List(items) => {
            if let Some(head) = list_head_symbol(items) {
                if head == "fn" {
                    if fn_body_is_complex(items) {
                        return false;
                    }
                    if let Some(text) = inline_small_fn(items, options, depth + 1, comments) {
                        return text.len() <= inline_map_length_limit(options);
                    }
                }
            }
            is_inline_scalar(form)
        }
        FormKind::Vector(_) => inline_form(form, options, depth + 1, comments)
            .map(|s| s.len() <= inline_map_length_limit(options))
            .unwrap_or(false),
        _ => is_inline_scalar(form),
    }
}

fn is_defn_head(head: &str) -> bool {
    matches!(head, "defn" | "defn-" | "-defn")
}

fn is_named_method_head(head: &str, items: &[Form]) -> bool {
    if head != "method" || items.len() < 3 {
        return false;
    }
    matches!(items[1].kind, FormKind::Symbol(_))
}

fn is_block_head(head: &str) -> bool {
    if is_defn_head(head) {
        return true;
    }
    matches!(
        head,
        "let"
            | "let*"
            | "loop"
            | "binding"
            | "go"
            | "go-loop"
            | "async-scope"
            | "async::scope"
            | "scope-loop"
            | "async::scope-loop"
            | "when"
            | "fn"
            | "defmacro"
            | "def"
            | "macro"
            | "do"
            | "where"
            | "cond"
            | "case"
            | "match"
            | "try"
            | "catch"
            | "err"
            | "fin"
    )
}

fn is_thread_macro_head(head: &str) -> bool {
    matches!(head, "->" | "->>" | "as->" | "cond->" | "cond->>")
}

fn is_flow_head(head: &str) -> bool {
    matches!(
        head,
        "and" | "or" | "->" | "->>" | "as->" | "cond->" | "cond->>"
    )
}

fn map_sugar_binding_index(head: &str, items: &[Form]) -> Option<usize> {
    match head {
        "map" | "filter" | "remove" | "keep" | "some" | "every?" | "not-any?" | "not-every?"
        | "take-while" | "drop-while" | "split-with" | "partition-by" | "group-by" | "run!" => {
            if items.len() >= 3 && matches!(items[1].kind, FormKind::Vector(_)) {
                Some(1)
            } else {
                None
            }
        }
        "sort-by" => {
            if items.len() == 3 && matches!(items[1].kind, FormKind::Vector(_)) {
                Some(1)
            } else {
                None
            }
        }
        "pmap" | "pfilter" | "dag::pmap" | "dag::pfilter" | "std::pmap" | "std::pfilter" => {
            if items.len() >= 4
                && matches!(items[1].kind, FormKind::Map(_))
                && matches!(items[2].kind, FormKind::Vector(_))
            {
                Some(2)
            } else if items.len() >= 3 && matches!(items[1].kind, FormKind::Vector(_)) {
                Some(1)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn inline_length_limit_with(line_width: usize, max_width: usize, ratio: f32) -> usize {
    let mut limit = line_width.max(1);
    if max_width > 0 {
        limit = limit.min(max_width);
    }
    if ratio > 0.0 {
        let ratio_limit = (line_width as f32 * ratio).floor() as usize;
        if ratio_limit > 0 {
            limit = limit.min(ratio_limit);
        } else {
            limit = 1;
        }
    }
    limit.max(1)
}

fn inline_length_limit(options: &FormatOptions) -> usize {
    inline_length_limit_with(
        options.max_inline_chars,
        options.inline_max_width,
        options.inline_width_ratio,
    )
}

fn inline_map_length_limit(options: &FormatOptions) -> usize {
    let map_limit = inline_length_limit_with(
        options.max_inline_chars,
        options.map_inline_max_width,
        options.map_inline_width_ratio,
    );
    inline_length_limit(options).min(map_limit)
}

fn flow_head_inline_limit(options: &FormatOptions) -> Option<usize> {
    if options.flow_head_policy != FlowHeadPolicy::InlineIfFit {
        return None;
    }
    let flow_limit = inline_length_limit_with(
        options.max_inline_chars,
        options.flow_head_max_width,
        options.flow_head_width_ratio,
    );
    Some(flow_limit.min(inline_length_limit(options)))
}

fn is_inline_scalar(form: &Form) -> bool {
    match form.kind {
        FormKind::Symbol(_)
        | FormKind::Keyword(_)
        | FormKind::Int(_)
        | FormKind::Float(_)
        | FormKind::String(_)
        | FormKind::Bool(_)
        | FormKind::Nil
        | FormKind::Duration(_)
        | FormKind::Regex { .. }
        | FormKind::ForeignSymbol { .. } => true,
        _ => false,
    }
}

fn is_let_like(head: &str) -> bool {
    matches!(head, "let" | "let*" | "loop" | "binding")
}

fn is_control_form(form: &Form) -> bool {
    match &form.kind {
        FormKind::List(items) if !items.is_empty() => {
            if let Some(head) = list_head_symbol(items) {
                matches!(head, "if" | "when" | "cond" | "match" | "case")
            } else {
                false
            }
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(input: &str) -> String {
        format_source(
            input,
            FormatOptions {
                indent_width: 2,
                ..FormatOptions::default()
            },
        )
        .unwrap()
    }

    #[test]
    fn formats_inline_call() {
        let src = "(inc 1)";
        assert_eq!(fmt(src), "(inc 1)\n");
    }

    #[test]
    fn formats_inline_call_with_oop_chain_args() {
        let src = "puts(entry.1,entry.2,entry.3,entry.4,entry.5.0)";
        let expected = "(puts entry.1, entry.2, entry.3, entry.4, entry.5.0)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_sharp_accessor_function() {
        let src = "(map #0 xs)";
        let expected = "(map #0 xs)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_sharp_accessor_indexer() {
        let src = "(sqlite::query db \"SELECT * FROM terms\").map(#[0,13])";
        let expected = "(sqlite::query db \"SELECT * FROM terms\").map(#[0, 13])\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_dot_chain_with_comments_inside_base() {
        let src = "(map [x xs]\n  ; note\n  (inc x)).(concat *?)";
        let expected = "(map [x xs]\n  ; note\n  (inc x))\n.(concat *?)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_float_literal_suffix() {
        let src = "(def speed 1.0)\n(def slow -0.0)";
        let expected = "(def speed 1.0)\n(def slow -0.0)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_let_bindings() {
        let src = "(let [x 1 y (+ 2 3)] (+ x y))";
        let expected = "(let [x  1\n      y  (+ 2 3)]\n  (+ x y))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_map_literal() {
        let src = "(println {:a 1 :b 2})";
        let expected = "(println {:a 1 :b 2})\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_map_sugar_binding_on_newline() {
        let src = "(map [x xs] (+ x 1))";
        let expected = "(map [x xs]\n  (+ x 1))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_pmap_sugar_binding_on_newline() {
        let src = "(pmap {:max-parallel 2} [x xs] (+ x 1))";
        let expected = "(pmap {:max-parallel 2} [x xs]\n  (+ x 1))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_def_with_relaxed_inline_map() {
        let src = "(def mesh-draw-opts {:flip-y? true :shade? true})";
        let mut options = FormatOptions::default();
        options.inline_map_max_entries = 1;
        options.inline_map_max_entries_relaxed = 2;
        options.inline_width_ratio = 0.0;
        let expected = "(def mesh-draw-opts {:flip-y? true :shade? true})\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_def_with_typed_binder_inline() {
        let src = "(def xs: Vec<Int> (range 0 2000000))";
        let expected = "(def xs: [Int] (range 0 2000000))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_vector_literal() {
        let src = "(vec [1 2 3])";
        let expected = "(vec [1 2 3])\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_dollar_brace_as_rb_block() {
        let src = "${1 + 2 * 3}";
        let expected = "${1 + 2 * 3}\n";
        assert_eq!(fmt(src), expected);
        let src = "${(1 + 2) * 3}";
        let expected = "${(1 + 2) * 3}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_dollar_brace_keeps_code() {
        let src = "${f(1,2)}";
        let expected = "${f(1,2)}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_dollar_brace_keeps_equals() {
        let src = "${1 = 1}";
        let expected = "${1 = 1}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_match_form() {
        let src = "(match dir DirectionUp (Position 0 -1) DirectionDown (Position 0 1))";
        let expected =
            "(match dir\n  DirectionUp    (Position 0 -1)\n  DirectionDown  (Position 0 1))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_match_with_vector_pattern() {
        let src = "(match [current requested] [DirectionUp DirectionDown] current [_ _] requested)";
        let expected = "(match [current requested]\n  [DirectionUp DirectionDown]  current\n  [_ _]                        requested)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_cond_form() {
        let src = "(cond hit-wall? (CollisionHitWall) hit-self? (CollisionHitSelf) :else (CollisionNone))";
        let expected =
            "(cond\n  hit-wall?  (CollisionHitWall)\n  hit-self?  (CollisionHitSelf)\n  :else      (CollisionNone))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_cond_with_multiline_test() {
        let src = "(cond quit? world (match action (Quit {}) true _ false) result :else nope)";
        let expected =
            "(cond\n  quit?                world\n  (match action\n    (Quit {})  true\n    _          false)  result\n  :else                nope)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_when_block_body_on_newlines() {
        let src = "(when (< i 10) (println \"tick\" i) (recur (inc i)))";
        let expected = "(when (< i 10)\n  (println \"tick\" i)\n  (recur (inc i)))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_go_loop_with_when_and_comments() {
        let src = "(go-loop [i 0] (when (< i 10) (println \"tick\" i) (<! (timeout 1000)) ; wait 1s\n    (recur (inc i))))";
        let expected = "(go-loop [i 0]\n  (when (< i 10)\n    (println \"tick\" i)\n    (<! (timeout 1000)) ; wait 1s\n    (recur (inc i))))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_defn_multiline() {
        let src = "(defn snake-head [snake] (first snake))";
        let expected = "(defn snake-head [snake] (first snake))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_named_method_block() {
        let src =
            "(method draw [r] (reduce (fn [_ prim] (r.draw-prim self.:pos prim)) nil flyer-prims))";
        let expected = "(method draw [r]\n  (reduce\n    (fn [_ prim] (r.draw-prim self:pos prim))\n    nil\n    flyer-prims))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_method_params_inline_with_multiline_body() {
        let src =
            "(method draw-strip [color pts-world] (self.draw-segs color (strip->segs pts-world)))";
        let expected = "(method draw-strip [color pts-world]\n  (self.draw-segs color (strip->segs pts-world)))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_deftype_block() {
        let src = "(deftype World config<GameConfig> snake<Snake>)";
        let expected = "(deftype World\n  config<GameConfig>\n  snake<Snake>)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_defenum_block() {
        let src = "(defenum Phase A B)";
        let expected = "(defenum Phase\n  A\n  B)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_defenum_with_payload_map_on_same_line() {
        let src = "(defenum U RandInt {:seed Int :value Int} SpawnedPipe {:seed Int :pipe Pipe} ScoreResult {:pipes [Pipe] :score Int} StepResult {:mode Mode :best Int})";
        let expected = "(defenum U\n  RandInt {:seed Int :value Int}\n  SpawnedPipe {:seed Int :pipe Pipe}\n  ScoreResult {:pipes [Pipe] :score Int}\n  StepResult {:mode Mode :best Int})\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_def_foreign_block() {
        let src =
            "(def-foreign sha1 :file \"ruby/crypto.rb\" :entry \"Crypto.sha1\" [s: Str] -> Str)";
        let expected = "(def-foreign sha1\n  :file \"ruby/crypto.rb\"\n  :entry \"Crypto.sha1\"\n  [s: Str] -> Str)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_flow_head_inline_if_fit() {
        let src = "(or (Restart? action) (Flap? action))";
        let mut options = FormatOptions::default();
        options.flow_head_policy = FlowHeadPolicy::InlineIfFit;
        options.flow_head_max_width = 60;
        options.flow_head_width_ratio = 0.0;
        let expected = "(or (Restart? action) (Flap? action))\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn inlines_list_with_collection_args_when_allowed() {
        let src = "(into [] (map (fn [o] (o.step input)) self:gameObjects))";
        let mut options = FormatOptions::default();
        options.inline_allow_nested_collections = true;
        options.inline_max_width = 80;
        options.inline_width_ratio = 0.0;
        let expected = "(into [] (map (fn [o] (o.step input)) self:gameObjects))\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_require_with_refer_vector_inline() {
        let src = "(require appkit::helpers :refer [load-font init-timing step-timing render-frame update-window-title apply-frame-cap])";
        let mut options = FormatOptions::default();
        options.inline_max_width = 140;
        options.inline_width_ratio = 0.0;
        let expected = "(require appkit::helpers :refer [load-font init-timing step-timing render-frame update-window-title apply-frame-cap])\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn keeps_defn_empty_params_inline() {
        let src = "(defn initialClodius [] {:variables initialGameVariables :objects [initialVicViper]})";
        let expected =
            "(defn initialClodius []\n  {:variables initialGameVariables :objects [initialVicViper]})\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_map_with_inline_max_width_cap() {
        let src = "{:variables initialGameVariables :objects [initialVicViper]}";
        let mut options = FormatOptions::default();
        options.inline_max_width = 40;
        options.inline_width_ratio = 0.0;
        options.align_maps = false;
        let expected = "{:variables initialGameVariables\n :objects [initialVicViper]}\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_index_sugar_expression() {
        let src = "(foo world[:config])";
        let expected = "(foo world[:config])\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_index_sugar_with_path_and_default() {
        let src = "(foo config[:a :b || 10])";
        let expected = "(foo config[:a :b || 10])\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_index_sugar_in_bindings() {
        let src = "(let [config<GameConfig> world[:config]\n          board<Board> config[:board]]\n  [config board])";
        let expected = "(let [config: GameConfig  world[:config]\n      board: Board        config[:board]]\n  [config board])\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_index_sugar_after_oop_chain() {
        let src = "foo.bar[0][5][0]";
        let expected = "foo.bar[0][5][0]\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_index_sugar_with_comma_groups() {
        let src = "xs[1 3, 1,2]";
        let expected = "xs[1 3, 1, 2]\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_map_ref_syntax() {
        let src = "&ref\n&^:gap\n&^../:gap\n&^../../:gap\n&:screen:h\n&:pipe:gap-half";
        let expected = "&ref\n&^:gap\n&^../:gap\n&^../../:gap\n&:screen:h\n&:pipe:gap-half\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_with_dot_indexer() {
        let src =
            "config.pipe.gap-max\nconfig.:pipe.\"gap\".'sym.0\nxs.map(inc).reduce(+ 0).dorun()";
        let expected =
            "config.pipe.gap-max\nconfig:pipe.\"gap\".'sym.0\nxs.map(inc).reduce(+ 0).dorun()\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_with_nil_safe() {
        let src = "cfg?.:a.:b\nobj.a?.b.c\nnum??.:str";
        let expected = "cfg?.:a.:b\nobj.a?.b.c\nnum??.:str\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_keyword_sugar() {
        let src = "self.:canvas\nself:canvas";
        let expected = "self:canvas\nself:canvas\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_with_trailing_comment() {
        let src = "input:dt ; note";
        let expected = "input:dt ; note\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_with_complex_args() {
        let src = "world.assoc({:a 1 :b 2})";
        let mut options = FormatOptions::default();
        options.inline_map_max_entries = 1;
        options.inline_width_ratio = 0.0;
        let expected = "world.assoc({:a 1 :b 2})\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_oop_chain_with_dot_stage_after_index() {
        let src = "(let [db (sqlite::open sqlite-path)]\n  (sqlite::query db \"SELECT term, gloss FROM terms WHERE term LIKE '%surprising%'\").0.(map-indexed (fn [i v] [i v]) ?).vec\n  (sqlite::query db \"SELECT term, gloss FROM terms WHERE term LIKE '%surprising%'\").0.13.puts)";
        let expected = "(let [db  (sqlite::open sqlite-path)]\n  (sqlite::query db \"SELECT term, gloss FROM terms WHERE term LIKE '%surprising%'\").0.(map-indexed (fn [i v] [i v]) ?).vec\n  (sqlite::query db \"SELECT term, gloss FROM terms WHERE term LIKE '%surprising%'\").0.13.puts)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_err_fin_inline() {
        let src = "(err \"[ERR] #{?} (text: #{text})\")\n(fin \"bye\")";
        let expected = "(err \"[ERR] #{?} (text: #{text})\")\n(fin \"bye\")\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_oop_chain_in_nested_inline_form() {
        let src = "(if (< nx (- (- (int (/ cfg:w 2))) 60)) ok ng)";
        let expected = "(if (< nx (- (- (int (/ cfg:w 2))) 60))\n  ok\n  ng)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_forced_inline_if_with_oop_chain() {
        let src =
            "(let [rolled (if behavior:doesScroll (assoc obj :position [(- x scroll-speed) y]) obj)] rolled)";
        let expected =
            "(let [rolled  (if behavior:doesScroll (assoc obj :position [(- x scroll-speed) y]) obj)]\n  rolled)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_force_inline_with_empty_comment() {
        let src = "(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;";
        let mut options = FormatOptions::default();
        options.max_inline_chars = 10;
        let expected = "(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_force_inline_with_trailing_comment_text() {
        let src = "(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ; TODO";
        let mut options = FormatOptions::default();
        options.max_inline_chars = 10;
        let expected = "(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ; TODO\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn formats_force_inline_ignores_internal_comments() {
        let src = "(let [x 1 ; note\n      y 2]) ;";
        let expected = "(let [x  1 ; note\n      y  2]) ;\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_constructor_keyword_args() {
        let src = "(Renderer canvas: canvas w: cfg:w h: cfg:h font: font)";
        let expected =
            "(Renderer\n  canvas: canvas\n  w:      cfg:w\n  h:      cfg:h\n  font:   font)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_commas_in_list_and_vector() {
        let src = "(GameOver :bird, :pipes, :score)\n[r: Rect, color: Color]";
        let expected = "(GameOver :bird, :pipes, :score)\n[r: Rect, color: Color]\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_set_literal_multiline() {
        let src = "#{1 (+ 2 3)}";
        let expected = "#{1 (+ 2 3)}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_top_level_comments() {
        let src = "; hello\n(inc 1)";
        let expected = "; hello\n(inc 1)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_inner_leading_comments() {
        let src = "(foo ; note\n  bar)";
        let expected = "(foo ; note\n  bar)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_dangling_comments_before_closing() {
        let src = "(foo\n  bar\n  ; end\n  )";
        let expected = "(foo bar\n  ; end\n)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_map_value_comments() {
        let src = "{:a 1 :b ; note\n 2}";
        let expected = "{:a 1\n :b ; note\n   2}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_leading_comment_after_opening_map() {
        let src = "{; note\n :a 1\n :bbb 2}";
        let expected = "{\n ; note\n :a   1\n :bbb 2}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_inline_comment_after_opening_map() {
        let src = "{;; note\n :a 1}";
        let expected = "{;; note\n :a 1}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_leading_comment_after_opening_let_bindings() {
        let src = "(let [; note\n      a 1\n      bb 2]\n  (+ a bb))";
        let expected = "(let [\n      ; note\n      a   1\n      bb  2]\n  (+ a bb))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn aligns_map_entries_with_inline_comments() {
        let src = "{:a 1 ; note\n :bbb 2 ; note2\n}";
        let expected = "{:a   1 ; note\n :bbb 2 ; note2\n}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn aligns_map_entries_with_shorthand() {
        let src = "{:kind :strip :pts viper-body-strip1 :fg C-v-body :shade-off shade-off}";
        let mut options = FormatOptions::default();
        options.inline_max_width = 20;
        options.inline_width_ratio = 0.0;
        let expected =
            "{:kind      :strip\n :pts       viper-body-strip1\n :fg        C-v-body\n :shade-off,}\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn aligns_assoc_pairs() {
        let src = "(assoc flyer :mesh fly-mesh :pos [220 80] :vx -120)";
        let mut options = FormatOptions::default();
        options.inline_max_width = 40;
        options.inline_width_ratio = 0.0;
        let expected = "(assoc flyer\n  :mesh fly-mesh\n  :pos  [220 80]\n  :vx   -120)\n";
        assert_eq!(format_source(src, options).unwrap(), expected);
    }

    #[test]
    fn aligns_let_bindings_with_inline_comments() {
        let src = "(let [a 1 ; note\n      bb 2 ; note2\n      ]\n  (+ a bb))";
        let expected = "(let [a   1 ; note\n      bb  2 ; note2\n      ]\n  (+ a bb))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_inline_comment_after_head_without_space() {
        let src = "(when;; note\n  (ok))";
        let expected = "(when ;; note\n  (ok))\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn comment_table_marks_dangling_comments() {
        let src = "(foo\n  bar\n  ; end\n  )";
        let mut reader = Reader::new(src);
        let forms = reader.read_all().unwrap();
        let string_escapes = reader.take_string_escapes();
        let comments = reader.take_comments();
        let commas = reader.take_commas();
        let table = CommentTable::from_source(
            &forms,
            comments,
            string_escapes,
            commas,
            BTreeSet::new(),
            DanglingCommentPolicy::OwnLine,
        );
        assert!(table.has_comments(forms[0].span.index));
    }

    #[test]
    fn keeps_inline_trailing_comments_with_preceding_form() {
        let src = "(foo) ; note\n(bar)";
        let expected = "(foo) ; note\n(bar)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn formats_top_level_forms_without_blank_lines() {
        let src = "(println 1)\n(println 2)";
        let expected = "(println 1)\n(println 2)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn keeps_top_level_blank_lines() {
        let src = "(println 1)\n\n\n(println 2)";
        let expected = "(println 1)\n\n(println 2)\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn preserves_single_blank_line_inside_forms_when_enabled() {
        let src = "(defn foo []\n  (a)\n\n  (b))";
        let mut options = FormatOptions::default();
        options.line_preserve_blank_lines = true;
        let out = format_source(src, options).unwrap();
        let expected = "(defn foo []\n  (a)\n\n  (b))\n";
        assert_eq!(out, expected);
    }

    #[test]
    fn formats_multiline_ruby_block_with_indentation() {
        let src = "(println ${puts(1)\nputs(2)})";
        let expected = "(println\n  ${\n    puts(1)\n    puts(2)\n  })\n";
        let out = format_source(src, FormatOptions::default()).unwrap();
        assert_eq!(out, expected);
    }

    #[test]
    fn multiline_foreign_block_indentation_is_idempotent() {
        let src = "${\n        puts 1\n        puts 2\n}";
        let formatted = fmt(src);
        assert!(
            formatted.contains("\n  puts 1\n  puts 2\n"),
            "formatted output should be dedented: {}",
            formatted
        );
        let reformatted = fmt(&formatted);
        assert_eq!(formatted, reformatted);
    }

    #[test]
    fn multiline_foreign_block_resets_leading_indent() {
        let src = "${\n        module Timeline\n    module_function\n  \n    def window(city, days)\n      base = Date.today\n      Array.new(days) do |offset|\n        {\n          \n     city: city,\n          day: (base + offset).iso8601,\n      index: offset\n        }\n      end\n    end\n  end\n}";
        let formatted = fmt(src);
        let mut lines = formatted.lines();
        assert_eq!(lines.next(), Some("${"));
        assert_eq!(lines.next(), Some("  module Timeline"));
        assert!(
            formatted.contains("city: city"),
            "formatted block should keep inner lines intact: {}",
            formatted
        );
    }

    #[test]
    fn ruby_block_fallback_reindents_nested_content() {
        let src = "${\nmodule Timeline\nmodule_function\n\ndef window(city, days)\nbase = Date.today\nArray.new(days) do |offset|\n{\ncity: city,\nday: (base + offset).iso8601,\nindex: offset\n}\nend\nend\nend\n}";
        let expected = "${\n  module Timeline\n    module_function\n\n    def window(city, days)\n      base = Date.today\n      Array.new(days) do |offset|\n        {\n          city: city,\n          day: (base + offset).iso8601,\n          index: offset\n        }\n      end\n    end\n  end\n}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn ruby_block_without_tag_defaults_to_ruby() {
        let src = "${\nmodule Foo\n\ndef bar(x)\nif x\nx + 1\nend\nend\nend\n}";
        let expected = "${\n  module Foo\n\n    def bar(x)\n      if x\n        x + 1\n      end\n    end\n  end\n}\n";
        assert_eq!(fmt(src), expected);
    }

    #[test]
    fn inline_fn_args_vector_supports_map_destructuring() {
        let src = "[{:keys [dir] :as state} events]";
        let mut reader = Reader::new(src);
        let forms = reader.read_all().expect("parse args vector");
        let mut comments = CommentTable::from_source(
            &forms,
            reader.take_comments(),
            reader.take_string_escapes(),
            reader.take_commas(),
            BTreeSet::new(),
            DanglingCommentPolicy::OwnLine,
        );
        let inline =
            inline_fn_args_vector_text(&forms[0], &FormatOptions::default(), &mut comments)
                .expect("inline fn args vector");
        assert_eq!(inline, "[{:keys [dir] :as state} events]");
    }
}
