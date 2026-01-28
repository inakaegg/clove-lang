use std::borrow::Cow;
use std::env;
use std::fs;
use std::io::IsTerminal;
use std::io::Write as IoWrite;
use std::mem;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;

use nu_ansi_term::{Color, Style};
use reedline::{
    default_emacs_keybindings, Completer as ReedlineCompleter, DescriptionMode, Emacs,
    FileBackedHistory, Highlighter, History, HistoryItem, HistoryItemId, HistorySessionId, IdeMenu,
    KeyCode, KeyModifiers, Keybindings, MenuBuilder, Prompt, PromptEditMode, PromptHistorySearch,
    Reedline, ReedlineError, ReedlineEvent, ReedlineMenu, SearchQuery, Signal, Span, StyledText,
    Suggestion, ValidationResult, Validator,
};

use clove_core::apply_default_foreign_tags_for_source;
use clove_core::ast::{FormKind, Key, RegexValue, Span as SourceSpan, Value};
use clove_core::env::EnvRef;
use clove_core::error::{format_error, CloveError, ERROR_TAG};
use clove_core::eval::{set_current_file, Evaluator, CURRENT_NS_KEY};
use clove_core::foreign::ForeignEngine;
use clove_core::interrupt;
use clove_core::options::EvalOptions;
use clove_core::reader::ReaderOptions;

use clove_core::reader::Reader;
use clove_core::repl::{self as seed_repl, history_path};
use clove_core::settings::REPL_ON_ERROR_VAR;
use clove_core::type_registry::{self, TypeEntry};
use clove_core::value_format;

use crate::create_runtime;
use crate::doc::{self, DocInfo};
use crate::symbols::{
    builtin_alias_target, builtin_aliases, canonical_symbol_name,
    is_namespaced_symbol as canonical_is_namespaced_symbol,
};
use clove_core::compiler::Compiler;
use clove_core::runtime::RuntimeCtx;
use once_cell::sync::OnceCell;

const META_COMMANDS: &[&str] = &[
    ":env",
    ":lang",
    ":load",
    ":set",
    ":source",
    ":doc",
    ":q",
    ":quit",
    ":h",
    ":help",
    ":c",
    ":cont",
    ":continue",
    ":n",
    ":next",
    ":whereami",
    ":where",
    ":w",
    ":backtrace",
    ":bt",
    ":vars",
    ":local_vars",
    ":local_variables",
];

struct FlushHistory<H: History> {
    inner: H,
}

impl<H: History> FlushHistory<H> {
    fn new(inner: H) -> Self {
        Self { inner }
    }
}

impl<H: History> History for FlushHistory<H> {
    fn save(&mut self, h: HistoryItem) -> reedline::Result<HistoryItem> {
        let item = self.inner.save(h)?;
        if item.id.is_some() {
            self.inner.sync().map_err(ReedlineError::from)?;
        }
        Ok(item)
    }

    fn load(&self, id: HistoryItemId) -> reedline::Result<HistoryItem> {
        self.inner.load(id)
    }

    fn count(&self, query: SearchQuery) -> reedline::Result<i64> {
        self.inner.count(query)
    }

    fn search(&self, query: SearchQuery) -> reedline::Result<Vec<HistoryItem>> {
        self.inner.search(query)
    }

    fn update(
        &mut self,
        id: HistoryItemId,
        updater: &dyn Fn(HistoryItem) -> HistoryItem,
    ) -> reedline::Result<()> {
        self.inner.update(id, updater)
    }

    fn clear(&mut self) -> reedline::Result<()> {
        self.inner.clear()
    }

    fn delete(&mut self, h: HistoryItemId) -> reedline::Result<()> {
        self.inner.delete(h)
    }

    fn sync(&mut self) -> std::io::Result<()> {
        self.inner.sync()
    }

    fn session(&self) -> Option<HistorySessionId> {
        self.inner.session()
    }
}

const SPECIAL_FORMS: &[&str] = &[
    "ns",
    "require",
    "def",
    "def-",
    "defn",
    "defn-",
    "deftype",
    "defenum",
    "describe",
    "describe-type",
    "infer-type",
    "enum-members",
    "loop",
    "recur",
    "let",
    "if",
    "if-not",
    "do",
    "p",
    "where",
    "fn",
    "quote",
    "set!",
    "redef",
    "-def",
    "-defn",
    "and",
    "or",
    "when",
    "when-not",
    "when-let",
    "with-open",
    "with-redefs",
    "with-redefs-fn",
    "if-let",
    "if-some",
    "cond",
    "condp",
    "for",
    "->",
    "->>",
    "as->",
    "cond->",
    "cond->>",
    "some->",
    "some->>",
    "while",
    "doseq",
    "each",
    "dotimes",
    "doto",
    "try",
    "throw",
    "match",
    "comment",
    "break",
    "current-ns",
    "repl",
    "debug",
    "use",
    "use-syntax",
    "load-file",
    "load-string",
    "delay",
    "ns-map",
    "nav",
    "lookup",
    "create-ns",
    "refer",
    "resolve",
    "eval",
    "with-dyn",
    "go-loop",
    "async-scope",
    "async::scope",
    "scope-loop",
    "async::scope-loop",
];
const LITERAL_CANDIDATES: &[&str] = &["nil", "true", "false", "__DATA__"];
const COMPLETION_MENU_NAME: &str = "completion_menu";
const REPL_LAST_VALUE_SYM: &str = "*repl-last*";
const INTERACTIVE_HELP_TEXT: &str = "Available commands:
  :help / :h         Show this help
  :env               Show global environment (paged)
  :vars              Show current namespace bindings
  :doc SYMBOL        Show documentation
  (nav QUERY ...)    Unified search across namespaces, vars, and docs
  :load FILE         Load and evaluate FILE
  :set NAME EXPR     Evaluate EXPR and bind it to NAME
  :source PATH [LINE COL]
                     Set source name and optional starting position for error reporting
  :lang TAG          Set default foreign language tag
  :whereami / :where Show snippet around the last error
  :backtrace / :bt   Show stacktrace for the last error
  :quit / :q         Exit the REPL";
const DEBUG_HELP_TEXT: &str = "debug REPL commands:
  :help / :h         Show this help
  :vars              Show local variables
  :env               Show full environment (paged)
  :doc SYMBOL        Show documentation
  (nav QUERY ...)    Unified search across namespaces, vars, and docs
  :load FILE         Load and evaluate FILE
  :set NAME EXPR     Evaluate EXPR and bind it
  :source PATH [LINE COL]
                     Set source name and optional starting position for error reporting
  :lang TAG          Set default foreign language tag
  :whereami / :where Show snippet around the last error
  :backtrace / :bt   Show stacktrace for the last error
  :continue / :c     Leave the debug REPL and resume
  :next / :n         Alias for :continue (resume execution)
  :quit / :q         Exit the debug REPL";

fn split_last_token(input: &str) -> Option<(&str, &str)> {
    let trimmed = input.trim_end();
    let idx = trimmed.rfind(|ch: char| ch.is_whitespace())?;
    let (head, tail) = trimmed.split_at(idx);
    let tail = tail.trim_start();
    if tail.is_empty() {
        None
    } else {
        Some((head, tail))
    }
}

fn parse_source_spec(input: &str) -> (String, Option<(usize, usize)>) {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return (String::new(), None);
    }
    if let Some((head, last)) = split_last_token(trimmed) {
        if let Ok(col) = last.parse::<usize>() {
            if let Some((head2, prev)) = split_last_token(head) {
                if let Ok(line) = prev.parse::<usize>() {
                    let path = head2.trim_end();
                    if !path.is_empty() {
                        return (path.to_string(), Some((line, col)));
                    }
                }
            }
        }
    }
    (trimmed.to_string(), None)
}

fn ensure_repl_specials(env: &EnvRef) {
    let mut writer = env.write().unwrap();
    for name in ["*1", "*2", "*3", "*e"] {
        if writer.get(name).is_none() {
            writer.set(name, Value::Nil);
        }
    }
}

fn update_repl_success(env: &EnvRef, value: Value) {
    let (prev_1, prev_2) = {
        let reader = env.read().unwrap();
        (reader.get("*1"), reader.get("*2"))
    };
    let mut writer = env.write().unwrap();
    writer.set("*3", prev_2.unwrap_or(Value::Nil));
    writer.set("*2", prev_1.unwrap_or(Value::Nil));
    writer.set("*1", value);
}

fn update_repl_error(env: &EnvRef, err: &CloveError) {
    let mut writer = env.write().unwrap();
    writer.set("*e", Value::String(err.to_string()));
}

fn meta_description(cmd: &str) -> &'static str {
    match normalize_command(cmd).as_deref() {
        Some("env") => "show current environment bindings",
        Some("lang") => "set the default host tag (:lang TAG)",
        Some("load") => "load and evaluate a file (:load PATH)",
        Some("set") => "evaluate expression and bind (:set name EXPR)",
        Some("source") => "set source name used in errors",
        Some("doc") => "show documentation for a symbol",
        Some("q") | Some("quit") => "exit the REPL",
        Some("help") | Some("h") => "show REPL help",
        Some("continue") | Some("cont") | Some("c") => "leave post-mortem session (debug REPL)",
        Some("next") | Some("n") => "resume execution (alias of :continue in debug REPL)",
        Some("whereami") | Some("where") | Some("w") => "show snippet around last error",
        Some("backtrace") | Some("bt") => "show stacktrace for last error",
        Some("vars") | Some("local_vars") | Some("local_variables") => "show local variables",
        _ => "",
    }
}

struct ReplCompleter {
    namespace_env: EnvRef,
    shared_env: EnvRef,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum SymbolSource {
    NamespacePlain,
    NamespaceNamespaced,
    SharedPlain,
    SharedNamespaced,
    SpecialForm,
    Docs,
    Literal,
}

#[derive(Clone, Debug)]
struct SymbolEntry {
    name: String,
    value: Option<Value>,
    source: SymbolSource,
}

#[derive(Clone, Debug)]
struct SymbolChoice {
    display: String,
    aliases: Vec<String>,
    alias_of: Option<String>,
    source: SymbolSource,
    match_kind: MatchKind,
    match_indices: Option<Vec<usize>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum MatchKind {
    Prefix,
    Contains,
    Subsequence,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum IdentityKey {
    Function(Value),
    Name(String),
}

impl IdentityKey {
    fn from_entry(entry: &SymbolEntry) -> Self {
        match &entry.value {
            Some(v) if is_function_like(v) => IdentityKey::Function(v.clone()),
            _ => IdentityKey::Name(entry.name.clone()),
        }
    }
}

fn is_function_like(value: &Value) -> bool {
    matches!(
        value,
        Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. }
            | Value::Future(_)
            | Value::Task(_)
    )
}

fn namespace_part(name: &str) -> Option<&str> {
    name.rsplit_once("::").map(|(ns, _)| ns)
}

impl ReplCompleter {
    fn new(namespace_env: EnvRef, shared_env: EnvRef) -> Self {
        Self {
            namespace_env,
            shared_env,
        }
    }

    fn describe_symbol(&self, name: &str) -> Option<DocInfo> {
        let scopes = [self.namespace_env.clone(), self.shared_env.clone()];
        doc::describe_symbol(name, &scopes)
    }

    fn source_rank(source: SymbolSource) -> usize {
        match source {
            SymbolSource::NamespacePlain => 0,
            SymbolSource::SharedPlain => 1,
            SymbolSource::NamespaceNamespaced => 2,
            SymbolSource::SharedNamespaced => 3,
            SymbolSource::SpecialForm => 4,
            SymbolSource::Docs => 5,
            SymbolSource::Literal => 6,
        }
    }

    fn collect_symbol_entries(&self, include_docs: bool) -> Vec<SymbolEntry> {
        use std::collections::HashSet;
        let mut entries = Vec::new();
        let mut seen = HashSet::new();
        let mut push_entry = |name: String, value: Option<Value>, source: SymbolSource| {
            if seen.insert(name.clone()) {
                entries.push(SymbolEntry {
                    name,
                    value,
                    source,
                });
            }
        };

        for (name, value, source) in Self::env_symbols(
            &self.namespace_env,
            SymbolSource::NamespacePlain,
            SymbolSource::NamespaceNamespaced,
        )
        .into_iter()
        .chain(Self::env_symbols(
            &self.shared_env,
            SymbolSource::SharedPlain,
            SymbolSource::SharedNamespaced,
        )) {
            push_entry(name, value, source);
        }

        for name in SPECIAL_FORMS.iter().copied() {
            push_entry(name.to_string(), None, SymbolSource::SpecialForm);
        }
        if include_docs {
            for name in doc::builtin_doc_names() {
                push_entry(name, None, SymbolSource::Docs);
            }
        }
        for lit in LITERAL_CANDIDATES.iter().copied() {
            push_entry(lit.to_string(), None, SymbolSource::Literal);
        }
        entries
    }

    fn symbol_choices(&self, fragment: &str, include_docs: bool) -> Vec<SymbolChoice> {
        let entries = self.collect_symbol_entries(include_docs);
        let fragment_has_ns = fragment.contains("::");
        let mut grouped: std::collections::HashMap<IdentityKey, Vec<SymbolEntry>> =
            std::collections::HashMap::new();
        for entry in entries {
            grouped
                .entry(IdentityKey::from_entry(&entry))
                .or_default()
                .push(entry);
        }
        let mut choices = Vec::new();
        for group_entries in grouped.values() {
            let mut group_names: Vec<String> = group_entries
                .iter()
                .map(|entry| canonical_symbol_name(&entry.name).into_owned())
                .collect();
            group_names.sort();
            group_names.dedup();
            for entry in group_entries {
                let display_name = canonical_symbol_name(&entry.name).into_owned();
                let match_kind = if Self::symbol_starts_with(&display_name, fragment) {
                    MatchKind::Prefix
                } else if fragment.is_empty() {
                    MatchKind::Prefix
                } else if Self::symbol_contains(&display_name, fragment) {
                    MatchKind::Contains
                } else if Self::symbol_subsequence(&display_name, fragment) {
                    MatchKind::Subsequence
                } else {
                    continue;
                };
                let aliases: Vec<String> = group_names
                    .iter()
                    .filter(|name| *name != &display_name)
                    .cloned()
                    .collect();
                let alias_of = builtin_alias_target(&display_name).map(|name| name.to_string());
                let match_indices =
                    Self::compute_match_indices(&display_name, fragment, match_kind);
                choices.push(SymbolChoice {
                    display: display_name,
                    aliases,
                    alias_of,
                    source: entry.source,
                    match_kind,
                    match_indices,
                });
            }
        }
        choices.sort_by(|a, b| {
            let match_rank = |k: MatchKind| match k {
                MatchKind::Prefix => 0,
                MatchKind::Contains => 1,
                MatchKind::Subsequence => 2,
            };
            let string_ns_rank = |name: &str| {
                if Self::is_namespaced_symbol(name) {
                    match namespace_part(name) {
                        Some("string") => 0,
                        _ => 1,
                    }
                } else {
                    0
                }
            };
            let alias_rank = |alias: &Option<String>| if alias.is_some() { 1 } else { 0 };
            let ns_rank = |name: &str| {
                let qualified = Self::is_namespaced_symbol(name);
                if fragment_has_ns {
                    if qualified {
                        0
                    } else {
                        1
                    }
                } else if qualified {
                    2
                } else {
                    0
                }
            };
            let specificity_rank =
                |name: &str| Self::fragment_specificity_rank(name, fragment, fragment_has_ns);
            (
                specificity_rank(&a.display),
                match_rank(a.match_kind),
                string_ns_rank(&a.display),
                alias_rank(&a.alias_of),
                ns_rank(&a.display),
                Self::source_rank(a.source),
                a.display.len(),
                &a.display,
            )
                .cmp(&(
                    specificity_rank(&b.display),
                    match_rank(b.match_kind),
                    string_ns_rank(&b.display),
                    alias_rank(&b.alias_of),
                    ns_rank(&b.display),
                    Self::source_rank(b.source),
                    b.display.len(),
                    &b.display,
                ))
        });
        Self::disambiguate_duplicate_displays(&mut choices, fragment);
        choices
    }

    fn fragment_specificity_rank(name: &str, fragment: &str, fragment_has_ns: bool) -> usize {
        if fragment.is_empty() {
            return 2;
        }
        if fragment_has_ns {
            if name == fragment {
                0
            } else if name.starts_with(fragment) {
                1
            } else {
                2
            }
        } else {
            let base = match name.rsplit_once("::") {
                Some((_ns, base)) => base,
                None => name,
            };
            if base == fragment {
                0
            } else if base.starts_with(fragment) {
                1
            } else {
                2
            }
        }
    }

    fn disambiguate_duplicate_displays(choices: &mut [SymbolChoice], fragment: &str) {
        let mut indices =
            std::collections::HashMap::<String, Vec<usize>>::with_capacity(choices.len());
        for (idx, choice) in choices.iter().enumerate() {
            indices.entry(choice.display.clone()).or_default().push(idx);
        }
        for group in indices.values() {
            if group.len() <= 1 {
                continue;
            }
            for (pos, idx) in group.iter().enumerate() {
                if pos == 0 {
                    continue;
                }
                if let Some(alias) = Self::preferred_duplicate_alias(&choices[*idx]) {
                    choices[*idx].display = alias;
                    choices[*idx].match_indices = Self::compute_match_indices(
                        &choices[*idx].display,
                        fragment,
                        choices[*idx].match_kind,
                    );
                }
            }
        }
    }

    fn preferred_duplicate_alias(choice: &SymbolChoice) -> Option<String> {
        let mut std_alias = None;
        for alias in &choice.aliases {
            if let Some(ns) = namespace_part(alias) {
                if ns != "std" {
                    return Some(alias.clone());
                }
                if std_alias.is_none() {
                    std_alias = Some(alias.clone());
                }
            }
        }
        std_alias.or_else(|| choice.aliases.first().cloned())
    }

    fn enum_variant_suggestions(
        &self,
        fragment: &str,
        span: Span,
        include_docs: bool,
    ) -> Option<Vec<Suggestion>> {
        let (enum_raw, variant_prefix) = fragment.rsplit_once("::")?;
        if enum_raw.is_empty() {
            return None;
        }
        let current_ns = namespace_name_from_env(&self.namespace_env)
            .or_else(|| namespace_name_from_env(&self.shared_env));
        let enum_fqn = if enum_raw.contains("::") {
            enum_raw.to_string()
        } else {
            let ns = current_ns.as_deref().unwrap_or("user");
            format!("{ns}::{enum_raw}")
        };
        let TypeEntry::Sum(meta) = type_registry::get_type_entry(&enum_fqn)? else {
            return None;
        };
        let prefix_lower = variant_prefix.to_lowercase();
        let mut out = Vec::new();
        for member in &meta.members {
            let variant = enum_member_local_name(member.as_str());
            if !prefix_lower.is_empty() && !variant.to_lowercase().starts_with(&prefix_lower) {
                continue;
            }
            let display = format!("{enum_raw}::{variant}");
            let description = if include_docs {
                self.describe_symbol(&display)
                    .map(|info| format_description(&info, &[], None))
            } else {
                None
            };
            out.push(Suggestion {
                value: display,
                description,
                style: None,
                extra: None,
                span,
                append_whitespace: false,
                match_indices: None,
            });
        }
        if out.is_empty() {
            None
        } else {
            Some(out)
        }
    }

    fn complete_symbols(&self, fragment: &str, span: Span, include_docs: bool) -> Vec<Suggestion> {
        let normalized_fragment = if fragment.ends_with('/') && fragment != "/" {
            Cow::Owned(format!("{}::", fragment.trim_end_matches('/'),))
        } else {
            canonical_symbol_name(fragment)
        };
        if let Some(suggestions) =
            self.enum_variant_suggestions(normalized_fragment.as_ref(), span, include_docs)
        {
            return suggestions;
        }
        self.symbol_choices(normalized_fragment.as_ref(), include_docs)
            .into_iter()
            .map(|choice| self.build_suggestion(&choice, span))
            .collect()
    }

    fn build_suggestion(&self, choice: &SymbolChoice, span: Span) -> Suggestion {
        let display = choice.display.clone();
        let description = self.describe_symbol(&display).and_then(|info| {
            Some(format_description(
                &info,
                &choice.aliases,
                choice.alias_of.as_deref(),
            ))
        });
        Suggestion {
            value: display,
            description,
            style: style_for_source(choice.source),
            extra: None,
            span,
            append_whitespace: false,
            match_indices: choice.match_indices.clone(),
        }
    }

    fn complete_meta(&self, fragment: &str, span: Span) -> Vec<Suggestion> {
        META_COMMANDS
            .iter()
            .filter(|cmd| cmd.starts_with(fragment))
            .map(|cmd| Suggestion {
                value: (*cmd).to_string(),
                description: Some(meta_description(cmd).to_string()),
                style: None,
                extra: None,
                span,
                append_whitespace: false,
                match_indices: None,
            })
            .collect()
    }

    fn complete_doc_target(&self, fragment: &str, span: Span) -> Vec<Suggestion> {
        self.complete_symbols(fragment, span, true)
    }

    fn complete_load_path(&self, line: &str, pos: usize, start: usize) -> Vec<Suggestion> {
        if pos < start || pos > line.len() {
            return Vec::new();
        }
        let fragment = &line[start..pos];
        let (dir_prefix, name_prefix, sep_char) = split_path_fragment(fragment);
        let separator = sep_char.unwrap_or('/');
        let dir_path = resolve_dir(dir_prefix).unwrap_or_else(|| PathBuf::from("."));
        let mut entries = Vec::new();
        if let Ok(read_dir) = fs::read_dir(&dir_path) {
            for entry in read_dir.flatten() {
                let file_name = entry.file_name();
                let name = file_name.to_string_lossy();
                if !name.starts_with(name_prefix) {
                    continue;
                }
                let is_dir = entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false);
                let mut replacement = String::new();
                replacement.push_str(dir_prefix);
                replacement.push_str(&name);
                if is_dir {
                    replacement.push(separator);
                }
                entries.push(Suggestion {
                    value: replacement.clone(),
                    description: Some(if is_dir { "directory" } else { "file" }.to_string()),
                    style: None,
                    extra: None,
                    span: Span::new(start, pos),
                    append_whitespace: false,
                    match_indices: None,
                });
            }
        }
        entries.sort_by(|a, b| a.value.cmp(&b.value));
        entries
    }

    fn env_symbols(
        env: &EnvRef,
        plain_source: SymbolSource,
        ns_source: SymbolSource,
    ) -> Vec<(String, Option<Value>, SymbolSource)> {
        let entries = env.read().unwrap().flatten();
        let mut out = Vec::new();
        for (name, value) in entries {
            let canonical = canonical_symbol_name(&name).into_owned();
            if canonical == REPL_ON_ERROR_VAR {
                continue;
            }
            if canonical.starts_with("__") || canonical.starts_with("core::") {
                continue;
            }
            if let Some((_, local)) = canonical.rsplit_once("::") {
                if local.starts_with("__") {
                    continue;
                }
            }
            let source = if Self::is_namespaced_symbol(&canonical) {
                ns_source
            } else {
                plain_source
            };
            out.push((canonical, Some(value), source));
        }
        out
    }

    fn symbol_starts_with(name: &str, normalized_fragment: &str) -> bool {
        if normalized_fragment.is_empty() {
            true
        } else {
            canonical_symbol_name(name).starts_with(normalized_fragment)
        }
    }

    fn symbol_contains(name: &str, normalized_fragment: &str) -> bool {
        if normalized_fragment.is_empty() {
            true
        } else {
            canonical_symbol_name(name).contains(normalized_fragment)
        }
    }

    fn symbol_subsequence(name: &str, normalized_fragment: &str) -> bool {
        if normalized_fragment.is_empty() {
            return true;
        }
        let mut fragment_chars = normalized_fragment.chars();
        let mut expected = match fragment_chars.next() {
            Some(ch) => ch,
            None => return true,
        };
        for ch in canonical_symbol_name(name).chars() {
            if ch == expected {
                if let Some(next) = fragment_chars.next() {
                    expected = next;
                } else {
                    return true;
                }
            }
        }
        false
    }

    fn compute_match_indices(name: &str, fragment: &str, kind: MatchKind) -> Option<Vec<usize>> {
        if fragment.is_empty() {
            return None;
        }
        match kind {
            MatchKind::Prefix => {
                if !name.starts_with(fragment) {
                    return None;
                }
                Some((0..fragment.chars().count()).collect())
            }
            MatchKind::Contains => {
                let start = name.find(fragment)?;
                let offset = name[..start].chars().count();
                let len = fragment.chars().count();
                Some((0..len).map(|i| offset + i).collect())
            }
            MatchKind::Subsequence => {
                let mut indices = Vec::new();
                let mut frag_chars = fragment.chars();
                let mut current = match frag_chars.next() {
                    Some(ch) => ch,
                    None => return None,
                };
                for (idx, ch) in name.chars().enumerate() {
                    if ch == current {
                        indices.push(idx);
                        match frag_chars.next() {
                            Some(next) => current = next,
                            None => return Some(indices),
                        }
                    }
                }
                None
            }
        }
    }

    fn is_namespaced_symbol(name: &str) -> bool {
        canonical_is_namespaced_symbol(name)
    }

    #[cfg(test)]
    fn complete_symbol_names_for_test(&self, fragment: &str) -> Vec<String> {
        self.complete_symbols(fragment, Span::new(0, fragment.len()), false)
            .into_iter()
            .map(|s| s.value)
            .collect()
    }
}

impl ReedlineCompleter for ReplCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        if let Some((token_start, token_end)) = first_token_range(line) {
            if line[token_start..].starts_with(':') {
                if pos >= token_start && pos <= token_end {
                    let prefix = &line[token_start..pos];
                    return self.complete_meta(prefix, Span::new(token_start, pos));
                }
                let command = &line[token_start..token_end];
                if command == ":load" {
                    let path_start = skip_spaces(line, token_end);
                    if path_start > token_end && pos >= path_start {
                        return self.complete_load_path(line, pos, path_start);
                    }
                }
                if command == ":doc" {
                    let target_start = skip_spaces(line, token_end);
                    if target_start > token_end && pos >= target_start {
                        let fragment = &line[target_start..pos];
                        return self.complete_doc_target(fragment, Span::new(target_start, pos));
                    }
                }
            }
        }
        if let Some((start, fragment)) = extract_symbol_fragment(line, pos) {
            return self.complete_symbols(&fragment, Span::new(start, pos), false);
        }
        self.complete_symbols("", Span::new(pos, pos), false)
    }
}

fn format_description(info: &DocInfo, aliases: &[String], alias_of: Option<&str>) -> String {
    let mut lines = Vec::new();

    // Aggregate origin/alias info at the top
    let mut meta_line = info.origin.clone();
    if let Some(target) = alias_of {
        let alias_info = format!("alias of {}", target);
        match &mut meta_line {
            Some(line) => {
                line.push(' ');
                line.push_str(&alias_info);
            }
            None => {
                meta_line = Some(alias_info);
            }
        }
    }
    let alias_list = doc_aliases(info, aliases);
    if !alias_list.is_empty() {
        let alias_block = format!("[{}]", alias_list.join(", "));
        match &mut meta_line {
            Some(line) => {
                line.push(' ');
                line.push_str(&alias_block);
            }
            None => {
                meta_line = Some(alias_block);
            }
        }
    }
    if let Some(line) = meta_line {
        lines.push(line);
    }

    // --- edit here ---
    let sig_lines: Vec<String> = match info.signature.as_deref() {
        Some(sig) => split_signature_lines(&info.name, sig)
            .into_iter()
            .map(|l| Style::new().fg(Color::Green).paint(l).to_string())
            .collect(),
        None => vec![Style::new().fg(Color::Green).paint(&info.name).to_string()],
    };
    lines.extend(sig_lines);
    // --- end ---

    if let Some(doc) = info.doc.as_deref() {
        let doc_full = doc
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .collect::<Vec<_>>()
            .join(" ");
        if !doc_full.is_empty() {
            lines.push(doc_full);
        }
    }

    if let Some(examples) = doc::doc_examples_for(&info.name) {
        if !examples.is_empty() {
            lines.push("Examples:".into());
            for example in examples.iter().take(3) {
                lines.push(format!("  {}", example));
            }
        }
    }
    if let Some(examples) =
        doc::doc_oop_examples_for(&info.canonical).or_else(|| doc::doc_oop_examples_for(&info.name))
    {
        if !examples.is_empty() {
            lines.push("OOP Examples:".into());
            for example in examples.iter().take(3) {
                lines.push(format!("  {}", example));
            }
        }
    }

    lines.join("\n")
}

fn doc_aliases(info: &DocInfo, extras: &[String]) -> Vec<String> {
    // Normalize legacy "/" separators to "::" and then deduplicate
    let mut aliases: Vec<String> = builtin_aliases(&info.canonical)
        .into_iter()
        .chain(std::iter::once(info.canonical.clone()))
        .chain(std::iter::once(info.name.clone()))
        .chain(extras.iter().cloned())
        .map(|alias| canonical_symbol_name(&alias).into_owned())
        .collect();
    aliases.sort();
    aliases.dedup();
    if info.name == info.canonical {
        aliases.retain(|alias| alias != &info.canonical);
    }
    aliases
}

/// Split the signature correctly based on repeated function names.
fn split_signature_lines(name: &str, signature: &str) -> Vec<String> {
    let sig = signature.trim();
    if sig.is_empty() {
        return Vec::new();
    }

    // If it already contains newlines, keep them as-is.
    if sig.contains('\n') {
        return sig
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .map(|l| l.to_string())
            .collect();
    }

    // If it's a single line, split on repeated "<name><space>" markers.
    let marker = format!("{} ", name);

    let mut starts = Vec::new();
    let mut search_from = 0usize;
    while let Some(rel) = sig[search_from..].find(&marker) {
        starts.push(search_from + rel);
        search_from += rel + marker.len();
    }

    // If the marker appears only once, keep it as a single line.
    if starts.len() <= 1 {
        return vec![sig.to_string()];
    }

    let mut lines = Vec::new();
    for (idx, &start) in starts.iter().enumerate() {
        let end = if idx + 1 < starts.len() {
            starts[idx + 1]
        } else {
            sig.len()
        };
        let slice = sig[start..end].trim();
        if !slice.is_empty() {
            lines.push(slice.to_string());
        }
    }

    if lines.is_empty() {
        vec![sig.to_string()]
    } else {
        lines
    }
}

fn style_for_source(_source: SymbolSource) -> Option<Style> {
    None
}

fn completion_match_style() -> Style {
    Style::new()
        .fg(Color::Rgb(255, 219, 148))
        .bold()
        .on(Color::Rgb(63, 40, 0))
}

fn completion_selected_match_style() -> Style {
    Style::new()
        .fg(Color::Rgb(20, 12, 0))
        .bold()
        .on(Color::Rgb(255, 240, 180))
}

fn namespace_name_from_env(env: &EnvRef) -> Option<String> {
    env.read()
        .ok()
        .and_then(|guard| guard.get(CURRENT_NS_KEY))
        .and_then(|value| match value {
            Value::Symbol(ns) => Some(ns),
            Value::String(ns) => Some(ns),
            _ => None,
        })
}

fn enum_member_local_name(member: &str) -> &str {
    member
        .rsplit_once("::")
        .map(|(_, local)| local)
        .unwrap_or(member)
}

fn configure_completion_keybindings(keybindings: &mut Keybindings) {
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );
    keybindings.add_binding(
        KeyModifiers::SHIFT,
        KeyCode::BackTab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
            ReedlineEvent::MenuPrevious,
        ]),
    );
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Down,
        ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuDown, ReedlineEvent::Down]),
    );
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Up,
        ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuUp, ReedlineEvent::Up]),
    );
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Left,
        ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuLeft, ReedlineEvent::Left]),
    );
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Right,
        ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuRight, ReedlineEvent::Right]),
    );
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('g'),
        ReedlineEvent::Esc,
    );
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('d'),
        ReedlineEvent::Multiple(vec![ReedlineEvent::Esc, ReedlineEvent::CtrlD]),
    );
}

struct ReplPrompt {
    namespace: String,
    seq: usize,
}

impl ReplPrompt {
    fn new(namespace: &str, seq: usize) -> Self {
        Self {
            namespace: namespace.to_string(),
            seq,
        }
    }

    fn label(&self) -> String {
        format!("clove({}):{:03}", self.namespace, self.seq)
    }
}

impl Prompt for ReplPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        Cow::Owned(self.label())
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: PromptEditMode) -> Cow<'_, str> {
        Cow::Borrowed("> ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Owned(format!("{}* ", self.label()))
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        Cow::Borrowed("(search) ")
    }
}

struct ReplValidator;

impl Validator for ReplValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        match analyze_buffer(line) {
            BufferStatus::Incomplete => ValidationResult::Incomplete,
            BufferStatus::Error(err) => {
                let _ = err;
                ValidationResult::Complete
            }
            BufferStatus::Empty | BufferStatus::Ready => ValidationResult::Complete,
        }
    }
}

#[derive(Default)]
struct ReplHighlighter;

impl Highlighter for ReplHighlighter {
    fn highlight(&self, line: &str, cursor: usize) -> StyledText {
        let mut styled = StyledText::new();
        styled.push((Style::new(), line.to_string()));
        if let Some((open_idx, close_idx)) = matching_bracket_indices(line, cursor) {
            // Emphasize with RGB colors that look calm in 24-bit environments (including VSCode)
            let emph = bracket_highlight_style();
            styled.style_range(open_idx, open_idx + 1, emph);
            styled.style_range(close_idx, close_idx + 1, emph);
        }
        styled
    }
}

pub fn interactive_repl(opts: EvalOptions, engines: &[Arc<dyn ForeignEngine>]) {
    ensure_debug_repl_handler();
    let ctx = create_runtime(opts.clone(), engines);
    start_repl_loop(ctx);
}

pub fn interactive_repl_with_context(ctx: Arc<RuntimeCtx>) {
    ensure_debug_repl_handler();
    start_repl_loop(ctx);
}

fn start_repl_loop(ctx: Arc<RuntimeCtx>) {
    ctx.with_current_ctx(|ctx| start_repl_loop_inner(ctx));
}

fn start_repl_loop_inner(ctx: Arc<RuntimeCtx>) {
    let default_tag = ctx.default_tag();
    ctx.set_source_name(Some("<repl>".to_string()));
    ctx.set_source_position(1, 1);
    ctx.set_default_tag(default_tag);
    let shared_env = ctx.env();
    let default_ns = ctx.default_namespace_name();
    let namespace_env = ctx
        .namespace_env(&default_ns)
        .unwrap_or_else(|| shared_env.clone());
    let env = shared_env.clone();
    ensure_repl_specials(&env);
    let history = history_path()
        .and_then(|path| FileBackedHistory::with_file(1_000, path).ok())
        .map(FlushHistory::new);

    let mut keybindings = default_emacs_keybindings();
    configure_completion_keybindings(&mut keybindings);
    let edit_mode = Box::new(Emacs::new(keybindings));

    let completer = Box::new(ReplCompleter::new(
        namespace_env.clone(),
        shared_env.clone(),
    ));
    fn create_completion_menu() -> Box<IdeMenu> {
        Box::new(
            IdeMenu::default()
                .with_name(COMPLETION_MENU_NAME)
                .with_description_mode(DescriptionMode::PreferRight)
                .with_padding(1)
                // Completion list width
                .with_min_completion_width(0)
                .with_max_completion_width(48)
                .with_max_completion_height(u16::MAX) // No height limit for the list
                // Description width settings
                .with_min_description_width(40)
                .with_max_description_width(100)
                .with_max_description_height(30)
                .with_description_offset(2)
                .with_match_text_style(completion_match_style())
                .with_selected_match_text_style(completion_selected_match_style()),
        )
    }

    let mut rl = Reedline::create()
        .with_quick_completions(false)
        .with_validator(Box::new(ReplValidator))
        .with_highlighter(Box::new(ReplHighlighter::default()))
        .with_completer(completer)
        .with_menu(ReedlineMenu::EngineCompleter(create_completion_menu()))
        .with_edit_mode(edit_mode)
        .with_ansi_colors(true)
        .use_bracketed_paste(true);

    if let Some(history) = history {
        rl = rl.with_history(Box::new(history));
    }

    println!("clove REPL. :q to quit. Tab opens completions; arrow keys move inside the menu.");
    println!(":env to show vars, :load FILE to load code, :doc SYMBOL for docs.");
    println!(":source PATH [LINE COL] sets source name and position, :lang TAG sets the default host language.");
    let mut prompt_seq = 1usize;
    let mut last_error: Option<CloveError> = None;
    let mut last_value: Option<Value> = None;
    loop {
        let prompt_ns = ctx.default_namespace_name();
        let prompt = ReplPrompt::new(&prompt_ns, prompt_seq);
        match rl.read_line(&prompt) {
            Ok(Signal::Success(line)) => {
                interrupt::clear_interrupt();
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                match handle_meta(
                    trimmed,
                    ctx.clone(),
                    namespace_env.clone(),
                    env.clone(),
                    &mut last_error,
                ) {
                    MetaResult::Continue => {
                        prompt_seq += 1;
                        continue;
                    }
                    MetaResult::Exit => break,
                    MetaResult::NotHandled => {}
                }
                let mut source = normalize_repl_dot_chain(&line);
                match rewrite_repl_dot_chain_with_last(&source, last_value.is_some()) {
                    Ok(Some(rewritten)) => source = rewritten,
                    Ok(None) => {}
                    Err(e) => {
                        print_formatted_error(&e, false);
                        last_error = Some(e);
                        prompt_seq += 1;
                        continue;
                    }
                }
                match ctx.eval_source(&source) {
                    Ok(val) => {
                        update_repl_success(&env, val.clone());
                        let print_result = if is_nav_result(&val) {
                            let lines = render_nav_result(&val);
                            print_lines_with_optional_pager(lines);
                            Ok(())
                        } else {
                            print_value_with_optional_pager(&val)
                        };
                        match print_result {
                            Ok(()) => {
                                env.write().unwrap().set(REPL_LAST_VALUE_SYM, val.clone());
                                last_value = Some(val);
                                last_error = None;
                            }
                            Err(e) => {
                                if interrupt::is_interrupted() {
                                    println!("; execution interrupted");
                                    interrupt::clear_interrupt();
                                    last_error = None;
                                    prompt_seq += 1;
                                    continue;
                                }
                                print_formatted_error(&e, false);
                                last_error = Some(e);
                            }
                        }
                    }
                    Err(e) => {
                        update_repl_error(&env, &e);
                        if interrupt::is_interrupted() {
                            println!("; execution interrupted");
                            interrupt::clear_interrupt();
                            last_error = None;
                            prompt_seq += 1;
                            continue;
                        }
                        print_formatted_error(&e, false);
                        last_error = Some(e);
                    }
                }
                prompt_seq += 1;
            }
            Ok(Signal::CtrlC) => {
                interrupt::request_interrupt();
                println!("; interrupted");
            }
            Ok(Signal::CtrlD) => break,
            Err(err) => {
                eprintln!("{} repl: {}", ERROR_TAG, err);
                break;
            }
        }
    }
}

fn handle_meta(
    line: &str,
    ctx: Arc<RuntimeCtx>,
    namespace_env: EnvRef,
    shared_env: EnvRef,
    last_error: &mut Option<CloveError>,
) -> MetaResult {
    if command_matches(line, &["q", "quit"]) {
        return MetaResult::Exit;
    }
    if command_matches(line, &["continue", "cont", "c", "next", "n"]) {
        println!("nothing to continue; already at top level");
        return MetaResult::Continue;
    }
    if command_matches(line, &["help", "h"]) {
        print_content_with_optional_pager(INTERACTIVE_HELP_TEXT);
        return MetaResult::Continue;
    }
    if command_matches(line, &["whereami", "where", "w"]) {
        if let Some(err) = last_error.as_ref() {
            display_whereami(err, ctx.source_name().as_deref());
        } else {
            println!("no recent error");
        }
        return MetaResult::Continue;
    }
    if command_matches(line, &["backtrace", "bt"]) {
        if let Some(err) = last_error.as_ref() {
            display_backtrace(err);
        } else {
            println!("no recent error");
        }
        return MetaResult::Continue;
    }
    if command_matches(line, &["vars", "local_vars", "local_variables"]) {
        let entries = collect_env_entries(&namespace_env, true);
        print_lines_with_optional_pager(entries_to_lines(entries));
        return MetaResult::Continue;
    }
    if line == ":env" {
        let entries = collect_env_entries(&shared_env, true);
        print_lines_with_optional_pager(entries_to_lines(entries));
        return MetaResult::Continue;
    }

    if let Some(rest) = line.strip_prefix(":doc ") {
        let target = rest.trim();
        if target.is_empty() {
            println!("usage: :doc symbol");
            return MetaResult::Continue;
        }
        let scopes = [namespace_env.clone(), shared_env.clone()];
        match doc::describe_symbol(target, &scopes) {
            Some(info) => print_doc_full(&info),
            None => println!("no documentation for '{}'", target),
        }
        return MetaResult::Continue;
    }

    if let Some(rest) = line.strip_prefix(":load ") {
        let path = rest.trim();
        let prev_source = ctx.source_name();
        let prev_tag = ctx.default_tag();
        match std::fs::read_to_string(path) {
            Ok(content) => {
                ctx.set_source_name(Some(path.to_string()));
                match ctx.eval_source(&content) {
                    Ok(_) => {
                        *last_error = None;
                    }
                    Err(e) => {
                        print_formatted_error(&e, false);
                        *last_error = Some(e);
                    }
                }
                ctx.set_source_name(prev_source);
                ctx.set_default_tag(prev_tag);
            }
            Err(e) => println!("{} load: {}", ERROR_TAG, e),
        }
        return MetaResult::Continue;
    }
    if let Some(rest) = line.strip_prefix(":set ") {
        let mut parts = rest.splitn(2, ' ');
        if let (Some(name), Some(expr)) = (parts.next(), parts.next()) {
            match ctx.eval_source(expr) {
                Ok(val) => {
                    shared_env.write().unwrap().set_in_chain(name, val);
                    *last_error = None;
                }
                Err(e) => {
                    print_formatted_error(&e, false);
                    *last_error = Some(e);
                }
            }
        }
        return MetaResult::Continue;
    }
    if let Some(rest) = line.strip_prefix(":source") {
        let (path, pos) = parse_source_spec(rest);
        if path.is_empty() {
            ctx.set_source_name(None);
            ctx.set_source_position(1, 1);
            ctx.set_repl_data_source(None);
            println!("source cleared");
        } else {
            ctx.set_source_name(Some(path.clone()));
            if let Some((line, col)) = pos {
                ctx.set_source_position(line, col);
            } else {
                ctx.set_source_position(1, 1);
            }
            ctx.set_repl_data_source(Some(PathBuf::from(&path)));
            println!("source set to {}", path);
        }
        return MetaResult::Continue;
    }
    if let Some(rest) = line.strip_prefix(":lang ") {
        let tag = rest.trim();
        if tag.is_empty() {
            ctx.set_default_tag(None);
            println!("default language cleared");
        } else {
            ctx.set_default_tag(Some(tag.to_string()));
            println!("default language set to {}", tag);
        }
        return MetaResult::Continue;
    }
    MetaResult::NotHandled
}

static DEBUG_REPL_HANDLER: OnceCell<()> = OnceCell::new();

pub fn ensure_debug_repl_handler() {
    DEBUG_REPL_HANDLER.get_or_init(|| {
        seed_repl::set_debug_repl_handler(Some(run_debug_repl_session));
    });
}

fn run_debug_repl_session(evaluator: &Evaluator, env: EnvRef) -> Result<Value, CloveError> {
    let mut session = InlineReplSession::new(evaluator, env);
    session.run()
}

struct DebugPrompt {
    ns_label: String,
    seq: usize,
}

impl DebugPrompt {
    fn new(ns_label: String, seq: usize) -> Self {
        Self { ns_label, seq }
    }

    fn label(&self) -> String {
        format!("{}:{:03}", self.ns_label, self.seq)
    }
}

impl Prompt for DebugPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        Cow::Owned(self.label())
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _: PromptEditMode) -> Cow<'_, str> {
        Cow::Borrowed("> ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Owned(format!("{}* ", self.label()))
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        Cow::Borrowed("(search) ")
    }
}

struct InlineReplSession<'a> {
    evaluator: &'a Evaluator,
    env: EnvRef,
    reader_opts: ReaderOptions,
    compiler: Compiler,
    default_tag: Option<String>,
    source_name: Option<String>,
    source_line: usize,
    source_col: usize,
    last_error: Option<CloveError>,
    last_value: Option<Value>,
}

impl<'a> InlineReplSession<'a> {
    fn new(evaluator: &'a Evaluator, env: EnvRef) -> Self {
        let tags = evaluator.engine_tags();
        Self {
            evaluator,
            env,
            reader_opts: ReaderOptions::language_defaults(tags.clone()),
            compiler: Compiler::new(tags),
            default_tag: None,
            source_name: None,
            source_line: 1,
            source_col: 1,
            last_error: seed_repl::take_last_debug_error(),
            last_value: None,
        }
    }

    fn prompt_namespace_label(&self) -> String {
        let ns = namespace_name_from_env(&self.env).unwrap_or_else(|| "user".into());
        format!("debug({})", ns)
    }

    fn run(&mut self) -> Result<Value, CloveError> {
        let shared_env = self.evaluator.global_env();
        let history = history_path()
            .and_then(|path| FileBackedHistory::with_file(1_000, path).ok())
            .map(FlushHistory::new);

        let mut keybindings = default_emacs_keybindings();
        configure_completion_keybindings(&mut keybindings);
        let edit_mode = Box::new(Emacs::new(keybindings));

        let completer = Box::new(ReplCompleter::new(self.env.clone(), shared_env.clone()));
        let completion_menu = Box::new(
            IdeMenu::default()
                .with_name(COMPLETION_MENU_NAME)
                .with_description_mode(DescriptionMode::PreferRight)
                .with_padding(1)
                .with_min_completion_width(0)
                .with_max_completion_width(48)
                .with_max_completion_height(u16::MAX)
                .with_min_description_width(32)
                .with_max_description_width(64)
                .with_max_description_height(10)
                .with_description_offset(2)
                .with_match_text_style(completion_match_style())
                .with_selected_match_text_style(completion_selected_match_style()),
        );

        let mut rl = Reedline::create()
            .with_quick_completions(false)
            .with_validator(Box::new(ReplValidator))
            .with_highlighter(Box::new(ReplHighlighter::default()))
            .with_completer(completer)
            .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
            .with_edit_mode(edit_mode)
            .with_ansi_colors(true)
            .use_bracketed_paste(true);

        if let Some(history) = history {
            rl = rl.with_history(Box::new(history));
        }

        let mut seq = 1usize;
        loop {
            let prompt = DebugPrompt::new(self.prompt_namespace_label(), seq);
            match rl.read_line(&prompt) {
                Ok(Signal::Success(line)) => {
                    interrupt::clear_interrupt();
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    match self.handle_meta(trimmed) {
                        MetaResult::Continue => {
                            seq += 1;
                            continue;
                        }
                        MetaResult::Exit => break,
                        MetaResult::NotHandled => {}
                    }
                    let mut source = normalize_repl_dot_chain(&line);
                    match rewrite_repl_dot_chain_with_last(&source, self.last_value.is_some()) {
                        Ok(Some(rewritten)) => source = rewritten,
                        Ok(None) => {}
                        Err(e) => {
                            print_formatted_error(&e, false);
                            self.last_error = Some(e);
                            seq += 1;
                            continue;
                        }
                    }
                    match self.eval_source(&source) {
                        Ok(val) => match value_format::format_value(&val) {
                            Ok(text) => {
                                println!("=> {}", text);
                                self.env
                                    .write()
                                    .unwrap()
                                    .set(REPL_LAST_VALUE_SYM, val.clone());
                                self.last_value = Some(val);
                                self.last_error = None;
                            }
                            Err(e) => {
                                if interrupt::is_interrupted() {
                                    println!("; execution interrupted");
                                    interrupt::clear_interrupt();
                                    self.last_error = None;
                                    seq += 1;
                                    continue;
                                }
                                print_formatted_error(&e, false);
                                self.last_error = Some(e);
                            }
                        },
                        Err(e) => {
                            if interrupt::is_interrupted() {
                                println!("; execution interrupted");
                                interrupt::clear_interrupt();
                                self.last_error = None;
                                seq += 1;
                                continue;
                            }
                            print_formatted_error(&e, false);
                            self.last_error = Some(e);
                        }
                    }
                    seq += 1;
                }
                Ok(Signal::CtrlC) => {
                    interrupt::request_interrupt();
                    println!("; interrupted");
                }
                Ok(Signal::CtrlD) => break,
                Err(err) => {
                    eprintln!("{} repl: {}", ERROR_TAG, err);
                    break;
                }
            }
        }
        Ok(Value::Nil)
    }

    fn eval_source(&mut self, src: &str) -> Result<Value, CloveError> {
        if let Some(result) = RuntimeCtx::try_with_current(|ctx| {
            ctx.set_source_position(self.source_line, self.source_col);
            ctx.eval_repl_source(
                src,
                self.env.clone(),
                self.source_name.as_deref(),
                self.default_tag.as_deref(),
            )
        }) {
            return result;
        }
        self.eval_source_fallback(src)
    }

    #[allow(dead_code)]
    fn eval_file_source(&mut self, src: &str) -> Result<Value, CloveError> {
        if let Some(result) = RuntimeCtx::try_with_current(|ctx| ctx.eval_source(src)) {
            return result;
        }
        Err(CloveError::runtime(
            ":load requires runtime context; unable to evaluate file",
        ))
    }

    fn eval_source_fallback(&mut self, src: &str) -> Result<Value, CloveError> {
        set_current_file(self.source_name.clone());
        ensure_repl_specials(&self.env);
        let mut reader_opts = self.reader_opts.clone();
        reader_opts.source_name = self.source_name.clone();
        reader_opts.start_line = self.source_line;
        reader_opts.start_col = self.source_col;
        let mut reader = Reader::new_with_options(src, reader_opts);
        let forms = reader.read_all()?;
        let (tagged, _) = apply_default_foreign_tags_for_source(
            forms,
            self.source_name.as_deref(),
            self.default_tag.as_deref(),
        )?;
        let lowered = self.compiler.compile(tagged)?;
        let mut last = Value::Nil;
        for form in lowered {
            match self.evaluator.eval(&form, self.env.clone()) {
                Ok(val) => {
                    update_repl_success(&self.env, val.clone());
                    last = val;
                }
                Err(err) => {
                    update_repl_error(&self.env, &err);
                    return Err(err);
                }
            }
        }
        Ok(last)
    }

    fn handle_meta(&mut self, line: &str) -> MetaResult {
        if command_matches(line, &["q", "quit"]) {
            return MetaResult::Exit;
        }
        if command_matches(line, &["continue", "cont", "c", "next", "n"]) {
            return MetaResult::Exit;
        }
        if command_matches(line, &["help", "h"]) {
            println!("{}", DEBUG_HELP_TEXT);
            return MetaResult::Continue;
        }
        if command_matches(line, &["vars", "local_vars", "local_variables"]) {
            let entries = collect_env_entries(&self.env, false);
            print_lines_with_optional_pager(entries_to_lines(entries));
            return MetaResult::Continue;
        }
        if line == ":env" {
            let entries = collect_env_entries(&self.env, true);
            print_lines_with_optional_pager(entries_to_lines(entries));
            return MetaResult::Continue;
        }
        if command_matches(line, &["whereami", "where", "w"]) {
            if let Some(err) = self.last_error.as_ref() {
                display_whereami(err, self.source_name.as_deref());
            } else {
                println!("no recent error");
            }
            return MetaResult::Continue;
        }
        if command_matches(line, &["backtrace", "bt"]) {
            if let Some(err) = self.last_error.as_ref() {
                display_backtrace(err);
            } else {
                println!("no recent error");
            }
            return MetaResult::Continue;
        }
        if let Some(rest) = line.strip_prefix(":doc ") {
            let target = rest.trim();
            if target.is_empty() {
                println!("usage: :doc symbol");
                return MetaResult::Continue;
            }
            let scopes = [self.env.clone(), self.evaluator.global_env()];
            match doc::describe_symbol(target, &scopes) {
                Some(info) => print_doc_full(&info),
                None => println!("no documentation for '{}'", target),
            }
            return MetaResult::Continue;
        }
        if let Some(rest) = line.strip_prefix(":load ") {
            let path = rest.trim();
            if path.is_empty() {
                println!("usage: :load path");
                return MetaResult::Continue;
            }
            match self.load_file(path) {
                Ok(_) => self.last_error = None,
                Err(e) => {
                    print_formatted_error(&e, false);
                    self.last_error = Some(e);
                }
            }
            return MetaResult::Continue;
        }
        if let Some(rest) = line.strip_prefix(":set ") {
            let mut parts = rest.splitn(2, ' ');
            if let (Some(name), Some(expr)) = (parts.next(), parts.next()) {
                match self.eval_source(expr) {
                    Ok(val) => {
                        self.env.write().unwrap().set_in_chain(name, val);
                        self.last_error = None;
                    }
                    Err(e) => {
                        print_formatted_error(&e, false);
                        self.last_error = Some(e);
                    }
                }
            } else {
                println!("usage: :set name expr");
            }
            return MetaResult::Continue;
        }
        if let Some(rest) = line.strip_prefix(":source") {
            let (path, pos) = parse_source_spec(rest);
            if path.is_empty() {
                self.source_name = None;
                self.source_line = 1;
                self.source_col = 1;
                set_current_file(None);
                let _ = RuntimeCtx::try_with_current(|ctx| {
                    ctx.set_repl_data_source(None);
                    Ok(())
                });
                println!("source cleared");
            } else {
                self.source_name = Some(path.clone());
                if let Some((line, col)) = pos {
                    self.source_line = line.max(1);
                    self.source_col = col.max(1);
                } else {
                    self.source_line = 1;
                    self.source_col = 1;
                }
                set_current_file(self.source_name.clone());
                let _ = RuntimeCtx::try_with_current(|ctx| {
                    ctx.set_repl_data_source(Some(PathBuf::from(&path)));
                    Ok(())
                });
                println!("source set to {}", path);
            }
            return MetaResult::Continue;
        }
        if let Some(rest) = line.strip_prefix(":lang ") {
            let tag = rest.trim();
            if tag.is_empty() {
                self.default_tag = None;
                println!("default language cleared");
            } else {
                self.default_tag = Some(tag.to_string());
                println!("default language set to {}", tag);
            }
            return MetaResult::Continue;
        }
        MetaResult::NotHandled
    }

    fn load_file(&mut self, path: &str) -> Result<(), CloveError> {
        let path = path.trim();
        if path.is_empty() {
            println!("usage: :load path");
            return Ok(());
        }
        let prev_source = self.source_name.clone();
        let prev_tag = self.default_tag.clone();
        let content = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                println!("{} load: {}", ERROR_TAG, e);
                return Ok(());
            }
        };
        match RuntimeCtx::try_with_current(|ctx| {
            ctx.set_source_name(Some(path.to_string()));
            ctx.eval_source(&content)
        }) {
            Some(result) => {
                let outcome = result;
                let _ = RuntimeCtx::try_with_current(|ctx| {
                    ctx.set_source_name(prev_source.clone());
                    ctx.set_default_tag(prev_tag.clone());
                    Ok(())
                });
                outcome.map(|_| ())
            }
            None => {
                self.source_name = prev_source;
                self.default_tag = prev_tag;
                Err(CloveError::runtime(
                    ":load requires runtime context; unable to evaluate file",
                ))
            }
        }
    }
}

enum MetaResult {
    Continue,
    Exit,
    NotHandled,
}

fn normalize_command(line: &str) -> Option<String> {
    let mut parts = line.trim().split_whitespace();
    let cmd = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    Some(cmd.trim_start_matches(':').to_ascii_lowercase())
}

fn command_matches(line: &str, names: &[&str]) -> bool {
    let Some(cmd) = normalize_command(line) else {
        return false;
    };
    names
        .iter()
        .any(|name| cmd == name.trim_start_matches(':').to_ascii_lowercase())
}

fn collect_env_entries(env: &EnvRef, include_outer: bool) -> Vec<(String, Value)> {
    let guard = env.read().unwrap();
    let mut entries = if include_outer {
        guard.flatten()
    } else {
        guard.clone_data()
    };
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    entries
}

fn entries_to_lines(entries: Vec<(String, Value)>) -> Vec<String> {
    entries
        .into_iter()
        .map(|(k, v)| format!("{} = {}", k, v))
        .collect()
}

fn is_nav_result(value: &Value) -> bool {
    let Value::Map(map) = value else {
        return false;
    };
    matches!(
        map.get(&Key::Keyword("kind".into())),
        Some(Value::Symbol(kind)) if kind == ":nav"
    )
}

#[derive(Clone)]
enum NavQuery {
    Text(String),
    Regex(RegexValue),
    Other(Value),
}

fn render_nav_result(value: &Value) -> Vec<String> {
    let Value::Map(map) = value else {
        return vec![format!("{}", value)];
    };
    let query = map
        .get(&Key::Keyword("query".into()))
        .and_then(nav_query_from_value);
    let ns_items = nav_collect_entries(map.get(&Key::Keyword("ns".into())));
    let var_items = nav_collect_entries(map.get(&Key::Keyword("var".into())));
    let doc_items = nav_collect_entries(map.get(&Key::Keyword("doc".into())));

    let mut lines = Vec::new();
    lines.push(format!(
        "nav query={} ns={} var={} doc={}",
        nav_query_display(query.as_ref()),
        ns_items.len(),
        var_items.len(),
        doc_items.len()
    ));

    for item in ns_items {
        let Value::Map(entry) = item else { continue };
        let name = entry
            .get(&Key::Keyword("ns".into()))
            .and_then(nav_value_to_string)
            .unwrap_or_default();
        let match_kinds = nav_match_kinds(entry.get(&Key::Keyword("match".into())));
        let (match_name, _) = nav_match_flags(&match_kinds);
        let name = if let (Some(query), true) = (query.as_ref(), match_name) {
            nav_highlight_text(&name, query)
        } else {
            name
        };
        let badge = nav_match_badge(&match_kinds);
        lines.push(format!("NS   {}  {}", name, badge));
    }

    for item in var_items {
        let Value::Map(entry) = item else { continue };
        let sym = entry
            .get(&Key::Keyword("sym".into()))
            .and_then(nav_value_to_string)
            .or_else(|| {
                entry
                    .get(&Key::Keyword("name".into()))
                    .and_then(nav_value_to_string)
            })
            .unwrap_or_default();
        let doc = entry
            .get(&Key::Keyword("doc".into()))
            .and_then(nav_value_to_string)
            .unwrap_or_default();
        let aliases = entry
            .get(&Key::Keyword("aliases".into()))
            .and_then(nav_value_to_strings)
            .unwrap_or_default();
        let match_kinds = nav_match_kinds(entry.get(&Key::Keyword("match".into())));
        let (match_name, match_doc) = nav_match_flags(&match_kinds);
        let sym = if let (Some(query), true) = (query.as_ref(), match_name) {
            nav_highlight_text(&sym, query)
        } else {
            sym
        };
        let doc = if let (Some(query), true) = (query.as_ref(), match_doc) {
            nav_highlight_text(&doc, query)
        } else {
            doc
        };
        let badge = nav_match_badge(&match_kinds);
        let topics = entry
            .get(&Key::Keyword("topics".into()))
            .and_then(nav_value_to_topics)
            .unwrap_or_default();
        let topics_suffix = if topics.is_empty() {
            String::new()
        } else {
            format!("(topics: {})", topics.join(","))
        };
        let alias_suffix = if aliases.is_empty() {
            String::new()
        } else {
            format!("alias: {}", aliases.join(", "))
        };
        let mut parts = vec![format!("VAR  {}  {}", sym, badge)];
        if !topics_suffix.is_empty() {
            parts.push(topics_suffix);
        }
        if !doc.is_empty() {
            parts.push(doc);
        }
        if !alias_suffix.is_empty() {
            parts.push(alias_suffix);
        }
        lines.push(parts.join("  "));
    }

    for item in doc_items {
        let Value::Map(entry) = item else { continue };
        let name = entry
            .get(&Key::Keyword("name".into()))
            .and_then(nav_value_to_string)
            .unwrap_or_default();
        let doc = entry
            .get(&Key::Keyword("doc".into()))
            .and_then(nav_value_to_string)
            .unwrap_or_default();
        let match_kinds = nav_match_kinds(entry.get(&Key::Keyword("match".into())));
        let (match_name, match_doc) = nav_match_flags(&match_kinds);
        let name = if let (Some(query), true) = (query.as_ref(), match_name) {
            nav_highlight_text(&name, query)
        } else {
            name
        };
        let doc = if let (Some(query), true) = (query.as_ref(), match_doc) {
            nav_highlight_text(&doc, query)
        } else {
            doc
        };
        let badge = nav_match_badge(&match_kinds);
        if doc.is_empty() {
            lines.push(format!("DOC  {}  {}", name, badge));
        } else {
            lines.push(format!("DOC  {}  {}  {}", name, badge, doc));
        }
    }

    lines
}

fn nav_collect_entries<'a>(value: Option<&'a Value>) -> Vec<&'a Value> {
    let Some(Value::Vector(items)) = value else {
        return Vec::new();
    };
    items.iter().collect()
}

fn nav_query_from_value(value: &Value) -> Option<NavQuery> {
    match value {
        Value::String(text) | Value::Symbol(text) => Some(NavQuery::Text(text.clone())),
        Value::Regex(regex) => Some(NavQuery::Regex(regex.clone())),
        other => Some(NavQuery::Other(other.clone())),
    }
}

fn nav_query_display(query: Option<&NavQuery>) -> String {
    match query {
        Some(NavQuery::Text(text)) => text.clone(),
        Some(NavQuery::Regex(regex)) => {
            let pattern = escape_regex_slash(&regex.pattern);
            format!("#/{}/", pattern)
        }
        Some(NavQuery::Other(value)) => format!("{}", value),
        None => String::new(),
    }
}

fn nav_value_to_string(value: &Value) -> Option<String> {
    match value {
        Value::Symbol(text) | Value::String(text) => Some(text.clone()),
        _ => None,
    }
}

fn escape_regex_slash(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len());
    let mut backslash_run = 0usize;
    for ch in pattern.chars() {
        if ch == '\\' {
            backslash_run += 1;
            out.push(ch);
            continue;
        }
        if ch == '/' && backslash_run % 2 == 0 {
            out.push('\\');
        }
        out.push(ch);
        backslash_run = 0;
    }
    out
}

fn nav_value_to_strings(value: &Value) -> Option<Vec<String>> {
    let Value::Vector(items) = value else {
        return None;
    };
    let mut out = Vec::new();
    for item in items {
        if let Some(text) = nav_value_to_string(item) {
            out.push(text);
        }
    }
    Some(out)
}

fn nav_value_to_topics(value: &Value) -> Option<Vec<String>> {
    let mut topics = Vec::new();
    let items: Vec<&Value> = match value {
        Value::Set(items) => items.iter().collect(),
        Value::SortedSet(items) => items.entries.iter().collect(),
        Value::Vector(items) => items.iter().collect(),
        _ => return None,
    };
    for item in items {
        if let Some(text) = nav_value_to_string(item) {
            let trimmed = text.trim_start_matches(':').to_string();
            if !trimmed.is_empty() {
                topics.push(trimmed);
            }
        }
    }
    topics.sort();
    topics.dedup();
    Some(topics)
}

fn nav_match_kinds(value: Option<&Value>) -> Vec<String> {
    let Some(Value::Vector(items)) = value else {
        return Vec::new();
    };
    let mut kinds = Vec::new();
    for item in items {
        if let Some(text) = nav_value_to_string(item) {
            let trimmed = text.trim_start_matches(':').to_string();
            if !trimmed.is_empty() {
                kinds.push(trimmed);
            }
        }
    }
    kinds
}

fn nav_match_flags(kinds: &[String]) -> (bool, bool) {
    let mut match_name = false;
    let mut match_doc = false;
    for kind in kinds {
        match kind.as_str() {
            "name" => match_name = true,
            "doc" => match_doc = true,
            _ => {}
        }
    }
    (match_name, match_doc)
}

fn nav_match_badge(kinds: &[String]) -> String {
    let label = format!("[{}]", kinds.join(","));
    nav_badge_style().paint(label).to_string()
}

fn nav_highlight_text(text: &str, query: &NavQuery) -> String {
    let ranges = match query {
        NavQuery::Text(pattern) => nav_substring_ranges(text, pattern),
        NavQuery::Regex(regex) => nav_regex_ranges(text, regex),
        NavQuery::Other(_) => Vec::new(),
    };
    nav_apply_highlight(text, &ranges)
}

fn nav_substring_ranges(text: &str, pattern: &str) -> Vec<(usize, usize)> {
    if pattern.is_empty() {
        return Vec::new();
    }
    let mut ranges = Vec::new();
    let mut offset = 0usize;
    while let Some(pos) = text[offset..].find(pattern) {
        let start = offset + pos;
        let end = start + pattern.len();
        ranges.push((start, end));
        offset = end;
    }
    ranges
}

fn nav_regex_ranges(text: &str, regex: &RegexValue) -> Vec<(usize, usize)> {
    if regex.pattern.is_empty() {
        return Vec::new();
    }
    regex
        .regex
        .find_iter(text)
        .map(|m| (m.start(), m.end()))
        .collect()
}

fn nav_apply_highlight(text: &str, ranges: &[(usize, usize)]) -> String {
    if ranges.is_empty() {
        return text.to_string();
    }
    let style = nav_highlight_style();
    let mut out = String::new();
    let mut last = 0usize;
    for (start, end) in ranges {
        if *start > last {
            out.push_str(&text[last..*start]);
        }
        out.push_str(&style.paint(&text[*start..*end]).to_string());
        last = *end;
    }
    if last < text.len() {
        out.push_str(&text[last..]);
    }
    out
}

fn nav_highlight_style() -> Style {
    Style::new()
        .bold()
        .fg(Color::Rgb(32, 20, 8))
        .on(Color::Rgb(255, 214, 123))
}

fn nav_badge_style() -> Style {
    Style::new().fg(Color::Rgb(96, 156, 188))
}

fn print_lines_with_optional_pager(lines: Vec<String>) {
    if lines.is_empty() {
        println!("(no entries)");
        return;
    }
    let content = lines.join("\n");
    print_content_with_optional_pager(&content);
}

fn print_value_with_optional_pager(val: &Value) -> Result<(), CloveError> {
    let content = value_format::format_value(val)?;
    print_content_with_optional_pager(&content);
    Ok(())
}

fn print_content_with_optional_pager(content: &str) {
    if should_page(content) && page_content(content) {
        return;
    }
    if content.ends_with('\n') {
        print!("{}", content);
    } else {
        println!("{}", content);
    }
}

fn should_page(content: &str) -> bool {
    if content.is_empty() {
        return false;
    }
    let columns = env_usize("COLUMNS", 80);
    let max_lines = env_usize("LINES", 40).saturating_sub(1);
    let mut total_lines = 0usize;
    for line in content.split('\n') {
        let visible = visible_len(line);
        let wrapped = ((visible + columns - 1) / columns).max(1);
        total_lines = total_lines.saturating_add(wrapped);
        if total_lines > max_lines {
            return true;
        }
    }
    false
}

fn visible_len(line: &str) -> usize {
    let mut len = 0usize;
    let mut chars = line.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            if let Some('[') = chars.peek().copied() {
                chars.next();
                while let Some(next) = chars.next() {
                    if next == 'm' {
                        break;
                    }
                }
                continue;
            }
        }
        len += 1;
    }
    len
}

fn env_usize(name: &str, default: usize) -> usize {
    env::var(name)
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(default)
}

fn pager_disabled() -> bool {
    if env::var_os("NO_PAGER").is_some() {
        return true;
    }
    if let Ok(value) = env::var("CLOVE_PAGER") {
        if value == "0" {
            return true;
        }
    }
    false
}

fn page_content(content: &str) -> bool {
    if content.trim().is_empty() {
        return true;
    }
    if pager_disabled() {
        return false;
    }
    if !std::io::stdout().is_terminal() {
        return false;
    }
    if let Ok(custom) = env::var("PAGER") {
        if spawn_pager_command(&custom, content) {
            return true;
        }
        return false;
    }
    spawn_pager_command("less -R", content)
}

fn spawn_pager_command(cmdline: &str, content: &str) -> bool {
    let mut parts = split_command_line(cmdline);
    if parts.is_empty() {
        return false;
    }
    let cmd = parts.remove(0);
    let mut child = match Command::new(cmd).args(parts).stdin(Stdio::piped()).spawn() {
        Ok(child) => child,
        Err(_) => return false,
    };
    if let Some(stdin) = child.stdin.as_mut() {
        if stdin.write_all(content.as_bytes()).is_err() {
            return false;
        }
    }
    child.wait().is_ok()
}

fn split_command_line(cmdline: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quote = false;
    let mut quote_char = '\0';
    let mut escape = false;
    for ch in cmdline.chars() {
        if escape {
            current.push(ch);
            escape = false;
            continue;
        }
        if in_quote && ch == '\\' {
            escape = true;
            continue;
        }
        if ch == '"' || ch == '\'' {
            if in_quote {
                if ch == quote_char {
                    in_quote = false;
                } else {
                    current.push(ch);
                }
            } else {
                in_quote = true;
                quote_char = ch;
            }
            continue;
        }
        if ch.is_whitespace() && !in_quote {
            if !current.is_empty() {
                parts.push(mem::take(&mut current));
            }
            continue;
        }
        current.push(ch);
    }
    if !current.is_empty() {
        parts.push(current);
    }
    parts
}

fn display_backtrace(err: &CloveError) {
    let mut lines = format_error(err);
    if lines.is_empty() {
        println!("(no stacktrace)");
        return;
    }
    lines.remove(0);
    if lines.is_empty() {
        println!("(no stacktrace)");
        return;
    }
    print_lines_with_optional_pager(lines);
}

fn display_whereami(err: &CloveError, fallback_file: Option<&str>) {
    match error_location(err, fallback_file) {
        Some((file, span)) => match fs::read_to_string(&file) {
            Ok(source) => {
                let mut out = Vec::new();
                out.push(format!("-- {}:{}:{}", file, span.line, span.col));
                let lines: Vec<&str> = source.lines().collect();
                let start_line = if span.line > 2 { span.line - 2 } else { 1 };
                let end_line = span.line + 2;
                for line_no in start_line..=end_line {
                    if line_no == 0 {
                        continue;
                    }
                    if let Some(text) = lines.get(line_no.saturating_sub(1)) {
                        let marker = if line_no == span.line { '>' } else { ' ' };
                        out.push(format!("{} {:4} | {}", marker, line_no, text));
                        if line_no == span.line {
                            let caret = span.col.saturating_sub(1);
                            let padding = " ".repeat(caret);
                            out.push(format!("  {:4} | {}^", "", padding));
                        }
                    }
                }
                print_lines_with_optional_pager(out);
            }
            Err(err) => println!("failed to read {}: {}", file, err),
        },
        None => println!("location unknown"),
    }
}

fn error_location(err: &CloveError, fallback_file: Option<&str>) -> Option<(String, SourceSpan)> {
    for frame in err.stack().iter().rev() {
        if let Some(span) = frame.span {
            if let Some(file) = frame.file.as_deref() {
                if is_readable_source(file) {
                    return Some((file.to_string(), span));
                }
            }
        }
    }
    if let Some(span) = err.span() {
        if let Some(file) = err.file().or(fallback_file) {
            if is_readable_source(file) {
                return Some((file.to_string(), span));
            }
        }
    }
    None
}

fn is_readable_source(file: &str) -> bool {
    if file.starts_with('<') {
        return false;
    }
    Path::new(file).exists()
}

enum BufferStatus {
    Empty,
    Incomplete,
    Ready,
    Error(CloveError),
}

fn normalize_repl_dot_chain(src: &str) -> String {
    if !src.contains('\n') {
        return src.to_string();
    }
    let lines: Vec<&str> = src.lines().collect();
    let mut out = Vec::with_capacity(lines.len());
    let mut idx = 0usize;
    while idx < lines.len() {
        let line = lines[idx];
        let trimmed = line.trim_end();
        if trimmed.ends_with('.') {
            if let Some(next) = lines.get(idx + 1) {
                let next_trimmed = next.trim_start();
                if !next_trimmed.is_empty() {
                    let left = if next_trimmed.starts_with('.') {
                        let new_len = trimmed.len().saturating_sub(1);
                        &trimmed[..new_len]
                    } else {
                        trimmed
                    };
                    out.push(format!("{}{}", left, next_trimmed));
                    idx += 2;
                    continue;
                }
            }
        }
        out.push(line.to_string());
        idx += 1;
    }
    out.join("\n")
}

fn rewrite_repl_dot_chain_with_last(
    src: &str,
    has_last: bool,
) -> Result<Option<String>, CloveError> {
    let trimmed = src.trim_start();
    if !trimmed.starts_with(".(") {
        return Ok(None);
    }
    if !has_last {
        return Err(CloveError::runtime(
            "dot-chain in REPL requires a previous value",
        ));
    }
    let prefix_len = src.len().saturating_sub(trimmed.len());
    let mut out = String::with_capacity(src.len() + REPL_LAST_VALUE_SYM.len());
    out.push_str(&src[..prefix_len]);
    out.push_str(REPL_LAST_VALUE_SYM);
    out.push_str(trimmed);
    Ok(Some(out))
}

fn ends_with_dot_symbol(forms: &[clove_core::ast::Form]) -> bool {
    matches!(
        forms.last().map(|form| &form.kind),
        Some(FormKind::Symbol(sym)) if sym == "."
    )
}

fn ends_with_dangling_nil_safe_marker(src: &str, forms: &[clove_core::ast::Form]) -> bool {
    let Some(last) = forms.last() else {
        return false;
    };
    if !matches!(&last.kind, FormKind::Symbol(sym) if sym == "?") {
        return false;
    }
    let mut prev: Option<(usize, usize)> = None;
    let mut last_non_ws: Option<(usize, usize, char)> = None;
    for (idx, ch) in src.char_indices() {
        if ch.is_whitespace() {
            continue;
        }
        let len = ch.len_utf8();
        prev = last_non_ws.map(|(last_idx, last_len, _)| (last_idx, last_len));
        last_non_ws = Some((idx, len, ch));
    }
    let Some((last_idx, last_len, last_ch)) = last_non_ws else {
        return false;
    };
    if last_ch != '?' {
        return false;
    }
    let Some((prev_idx, prev_len)) = prev else {
        return false;
    };
    prev_idx + prev_len == last_idx
}

fn analyze_buffer(src: &str) -> BufferStatus {
    if src.trim().is_empty() {
        return BufferStatus::Empty;
    }
    let mut reader = Reader::new_with_options(src, ReaderOptions::language_defaults(vec![]));
    match reader.read_all() {
        Ok(forms) => match forms.is_empty() {
            true => BufferStatus::Empty,
            false => {
                if ends_with_dot_symbol(&forms) || ends_with_dangling_nil_safe_marker(src, &forms) {
                    BufferStatus::Incomplete
                } else {
                    BufferStatus::Ready
                }
            }
        },
        Err(CloveError::Parse(data)) if message_indicates_incomplete(&data.message) => {
            BufferStatus::Incomplete
        }
        Err(e) => BufferStatus::Error(e),
    }
}

fn message_indicates_incomplete(msg: &str) -> bool {
    let lower = msg.to_lowercase();
    lower.contains("unexpected end")
        || lower.contains("unterminated list")
        || lower.contains("unterminated vector")
        || lower.contains("unterminated map")
        || lower.contains("unterminated set")
        || lower.contains("unterminated string")
        || lower.contains("unterminated (")
        || lower.contains("unterminated #json")
        || lower.contains("unterminated #yaml")
}

fn print_formatted_error(err: &CloveError, use_stderr: bool) {
    for line in format_error(err) {
        if use_stderr {
            eprintln!("{}", line);
        } else {
            println!("{}", line);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BracketKind {
    Paren,
    Bracket,
    Brace,
}

#[derive(Clone, Copy)]
struct Bracket {
    idx: usize,
    ch: char,
    kind: BracketKind,
    is_open: bool,
}

fn matching_bracket_indices(line: &str, cursor: usize) -> Option<(usize, usize)> {
    let brackets = collect_brackets(line);
    let target_idx = bracket_index_at_cursor(&brackets, cursor)?;
    let target = brackets[target_idx];
    if target.is_open {
        let mut depth = 0;
        for candidate in brackets.iter().skip(target_idx + 1) {
            if candidate.kind != target.kind {
                continue;
            }
            if candidate.is_open {
                depth += 1;
            } else if depth == 0 {
                return Some((target.idx, candidate.idx));
            } else {
                depth -= 1;
            }
        }
    } else {
        let mut depth = 0;
        for candidate in brackets[..target_idx].iter().rev() {
            if candidate.kind != target.kind {
                continue;
            }
            if candidate.is_open {
                if depth == 0 {
                    return Some((candidate.idx, target.idx));
                }
                depth -= 1;
            } else {
                depth += 1;
            }
        }
    }
    None
}

fn bracket_index_at_cursor(brackets: &[Bracket], cursor: usize) -> Option<usize> {
    for (idx, bracket) in brackets.iter().enumerate() {
        if cursor == bracket.idx + bracket.ch.len_utf8() {
            return Some(idx);
        }
    }
    for (idx, bracket) in brackets.iter().enumerate() {
        if cursor == bracket.idx {
            return Some(idx);
        }
    }
    None
}

fn collect_brackets(line: &str) -> Vec<Bracket> {
    let mut brackets = Vec::new();
    let mut in_string = false;
    let mut escaped = false;
    let mut in_comment = false;
    for (idx, ch) in line.char_indices() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            ';' => in_comment = true,
            '(' | ')' | '[' | ']' | '{' | '}' => {
                let (kind, is_open) = bracket_kind(ch);
                brackets.push(Bracket {
                    idx,
                    ch,
                    kind,
                    is_open,
                });
            }
            _ => {}
        }
    }
    brackets
}

fn bracket_kind(ch: char) -> (BracketKind, bool) {
    match ch {
        '(' => (BracketKind::Paren, true),
        ')' => (BracketKind::Paren, false),
        '[' => (BracketKind::Bracket, true),
        ']' => (BracketKind::Bracket, false),
        '{' => (BracketKind::Brace, true),
        '}' => (BracketKind::Brace, false),
        _ => unreachable!("bracket_kind called with non-bracket character"),
    }
}

fn bracket_highlight_style() -> Style {
    if truecolor_enabled() {
        // Emphasize with RGB colors that look calm in 24-bit environments (including VSCode)
        let fg = Color::Rgb(30, 24, 18);
        let bg = Color::Rgb(201, 170, 108);
        Style::new().bold().fg(fg).on(bg)
    } else {
        // Fall back to a 16/256-color-safe scheme when truecolor is unavailable
        Style::new().bold().fg(Color::Black).on(Color::Yellow)
    }
}

fn truecolor_enabled() -> bool {
    match env::var("COLORTERM") {
        Ok(val) => {
            let lower = val.to_ascii_lowercase();
            lower.contains("truecolor") || lower.contains("24bit") || lower.contains("24-bit")
        }
        Err(_) => false,
    }
}

fn first_token_range(line: &str) -> Option<(usize, usize)> {
    let trimmed = line.trim_start();
    if trimmed.is_empty() {
        return None;
    }
    let start = line.len() - trimmed.len();
    let mut end = line.len();
    for (offset, ch) in trimmed.char_indices() {
        if ch.is_whitespace() {
            end = start + offset;
            break;
        }
    }
    Some((start, end))
}

fn skip_spaces(line: &str, mut idx: usize) -> usize {
    let bytes = line.as_bytes();
    while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
        idx += 1;
    }
    idx
}

fn extract_symbol_fragment(line: &str, pos: usize) -> Option<(usize, String)> {
    if pos > line.len() {
        return None;
    }
    let mut start = pos;
    let bytes = line.as_bytes();
    while start > 0 {
        let ch = bytes[start - 1] as char;
        if is_symbol_delimiter(ch) {
            break;
        }
        start -= 1;
    }
    if start == pos {
        return None;
    }
    let fragment = &line[start..pos];
    if let Some((offset, tail)) = split_oop_fragment(fragment) {
        return Some((start + offset, tail.to_string()));
    }
    Some((start, fragment.to_string()))
}

fn is_symbol_delimiter(ch: char) -> bool {
    ch.is_whitespace()
        || matches!(
            ch,
            '(' | ')' | '[' | ']' | '{' | '}' | '"' | '\'' | '`' | ',' | ';'
        )
}

fn split_oop_fragment(fragment: &str) -> Option<(usize, &str)> {
    let bytes = fragment.as_bytes();
    let mut last_dot = None;
    let mut in_string = false;
    let mut escape = false;
    for (idx, &b) in bytes.iter().enumerate() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if b == b'\\' {
                escape = true;
                continue;
            }
            if b == b'"' {
                in_string = false;
                continue;
            }
        } else {
            if b == b'"' {
                in_string = true;
                continue;
            }
            if b == b'.' {
                last_dot = Some(idx);
            }
        }
    }
    let dot = last_dot?;
    let next = dot + 1;
    if next > bytes.len() {
        return None;
    }
    Some((next, &fragment[next..]))
}

fn split_path_fragment(fragment: &str) -> (&str, &str, Option<char>) {
    let mut last_sep = None;
    let mut sep_char = None;
    for (idx, ch) in fragment.char_indices() {
        if is_path_separator(ch) {
            last_sep = Some(idx);
            sep_char = Some(ch);
        }
    }
    match last_sep {
        Some(idx) => {
            let next = idx + sep_char.unwrap().len_utf8();
            (&fragment[..next], &fragment[next..], sep_char)
        }
        None => ("", fragment, None),
    }
}

fn resolve_dir(prefix: &str) -> Option<PathBuf> {
    if prefix.starts_with("~/") {
        home_dir().map(|home| home.join(&prefix[2..]))
    } else if prefix == "~" {
        home_dir()
    } else if prefix.is_empty() {
        Some(PathBuf::from("."))
    } else {
        Some(PathBuf::from(prefix))
    }
}

fn home_dir() -> Option<PathBuf> {
    env::var_os("HOME")
        .map(PathBuf::from)
        .or_else(|| env::var_os("USERPROFILE").map(PathBuf::from))
}

fn is_path_separator(ch: char) -> bool {
    ch == '/' || ch == '\\'
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::env;

    const COMPLETION_DOC_ALLOWLIST: &[&str] = &[
        "*err*",
        "*out*",
        "<!",
        "<!!",
        ">!",
        ">!!",
        "add-watch",
        "associative-container?",
        "async::>!!",
        "async::go",
        "copy",
        "day",
        "delete",
        "dir?",
        "env-parse-file",
        "file-exists?",
        "file?",
        "flatten-into",
        "format-time",
        "current-ns",
        "defenum",
        "deftype",
        "get-validator",
        "go",
        "hour",
        "http-delete",
        "http-get",
        "http-get-json",
        "http-post",
        "http-post-json",
        "http-put",
        "http-request",
        "http::delete",
        "http::post",
        "http::put",
        "ini-generate",
        "ini-parse",
        "insert-sorted",
        "fs::copy",
        "fs::delete",
        "fs::dir?",
        "fs::file-exists?",
        "fs::file?",
        "io::line-seq",
        "fs::list-dir",
        "fs::mkdir",
        "fs::mkdirs",
        "fs::move",
        "io::read-all",
        "io::read-line",
        "io::slurp-bytes",
        "io::spit-bytes",
        "json-generate",
        "json-generate-pretty",
        "json-parse",
        "json-read-file",
        "json-write-file",
        "line-seq",
        "list-dir",
        "log-debug",
        "log-error",
        "log-info",
        "log-trace",
        "log-warn",
        "log::error",
        "log::info",
        "log::trace",
        "log::warn",
        "mkdir",
        "mkdirs",
        "move",
        "ms",
        "name",
        "neg?",
        "normalize-order",
        "parse-time",
        "partition-all-core",
        "partition-core",
        "range-core",
        "read-all",
        "read-line",
        "remove-watch",
        "sec",
        "seqs-have-values?",
        "set-validator!",
        "shell-escape",
        "shell-split",
        "slurp-bytes",
        "sort-by*",
        "spit-bytes",
        "use",
        "use-syntax",
        "with-open",
        "std::__clove_current_ns",
        "std::associative-container?",
        "std::copy",
        "std::delete",
        "std::dir?",
        "std::env-parse-file",
        "std::file-exists?",
        "std::file?",
        "std::flatten-into",
        "std::format-time",
        "std::use",
        "std::use-syntax",
        "std::http-delete",
        "std::http-get",
        "std::http-get-json",
        "std::http-post",
        "std::http-post-json",
        "std::http-put",
        "std::http-request",
        "std::ini-generate",
        "std::ini-parse",
        "std::insert-sorted",
        "std::json-generate",
        "std::json-generate-pretty",
        "std::json-parse",
        "std::json-read-file",
        "std::json-write-file",
        "std::list-dir",
        "std::log-debug",
        "std::log-error",
        "std::log-info",
        "std::log-trace",
        "std::log-warn",
        "std::mkdir",
        "std::mkdirs",
        "std::move",
        "std::normalize-order",
        "std::parse-time",
        "std::partition-all-core",
        "std::partition-core",
        "std::range-core",
        "std::read-all",
        "std::read-line",
        "std::seqs-have-values?",
        "std::shell-escape",
        "std::shell-split",
        "std::slurp-bytes",
        "std::sort-by*",
        "std::spit-bytes",
        "std::time-between",
        "std::time-minus",
        "std::time-plus",
        "std::toml-generate",
        "std::toml-parse",
        "std::toml-read-file",
        "std::toml-write-file",
        "std::use",
        "std::use-syntax",
        "std::yaml-generate",
        "std::yaml-parse",
        "std::yaml-read-file",
        "std::yaml-write-file",
        "time-between",
        "time-minus",
        "time-plus",
        "toml-generate",
        "toml-parse",
        "toml-read-file",
        "toml-write-file",
        "validator",
        "week",
        "yaml-generate",
        "yaml-parse",
        "yaml-read-file",
        "yaml-write-file",
        "year",
    ];
    use super::*;
    use crate::doc;
    use clove_core::ast::FnArity;
    use clove_core::ast::HashMap;
    use clove_core::ast::Key;
    use clove_core::ast::Value;
    use clove_core::ast::Vector;
    use clove_core::env::{new_ref, Env};
    use clove_core::foreign::ForeignEngine;
    use clove_core::options::EvalOptions;
    use std::sync::Arc;

    fn env_with(names: &[&str]) -> EnvRef {
        let env = new_ref(Env::default());
        {
            let mut data = env.write().unwrap();
            for name in names {
                data.set(name, Value::Nil);
            }
        }
        env
    }

    #[test]
    fn symbol_completion_prioritizes_namespace() {
        let helper = ReplCompleter::new(env_with(&["apple"]), env_with(&["aardvark"]));
        let replacements = helper.complete_symbol_names_for_test("a");
        assert!(replacements.len() >= 2);
        assert_eq!(replacements[0], "apple");
        assert_eq!(replacements[1], "aardvark");
    }

    #[test]
    fn empty_prefix_returns_all_symbols() {
        let helper = ReplCompleter::new(env_with(&["foo"]), env_with(&["bar"]));
        let replacements = helper.complete_symbol_names_for_test("");
        assert!(replacements.contains(&"foo".to_string()));
        assert!(replacements.contains(&"bar".to_string()));
    }

    #[test]
    fn clove_core_symbols_are_suppressed() {
        let helper = ReplCompleter::new(env_with(&["foo"]), env_with(&["core::foo"]));
        let replacements = helper.complete_symbol_names_for_test("");
        assert!(replacements.contains(&"foo".to_string()));
        assert!(!replacements.iter().any(|name| name.starts_with("core::")));
    }

    #[test]
    fn string_symbols_follow_plain_symbols() {
        let helper = ReplCompleter::new(
            env_with(&[]),
            env_with(&["trim", "string::trim", "string::upper-case"]),
        );
        let replacements = helper.complete_symbol_names_for_test("");
        let trim_idx = replacements
            .iter()
            .position(|name| name == "trim")
            .expect("trim must exist");
        let namespaced_idx = replacements
            .iter()
            .position(|name| name == "string::trim")
            .expect("string::trim must exist");
        assert!(trim_idx < namespaced_idx);
    }

    #[test]
    fn other_namespaces_follow_string_group() {
        let helper = ReplCompleter::new(
            env_with(&[]),
            env_with(&[
                "trim",
                "string::trim",
                "foo.core::trim",
                "string::lower-case",
            ]),
        );
        let replacements = helper.complete_symbol_names_for_test("");
        let str_idx = replacements
            .iter()
            .position(|name| name == "string::trim")
            .expect("string::trim must exist");
        let other_idx = replacements
            .iter()
            .position(|name| name == "foo.core::trim")
            .expect("foo.core::trim must exist");
        assert!(str_idx < other_idx);
    }

    #[test]
    fn slash_symbol_stays_in_plain_group() {
        let helper = ReplCompleter::new(env_with(&["/"]), env_with(&["foo::bar"]));
        let replacements = helper.complete_symbol_names_for_test("");
        let slash_idx = replacements.iter().position(|name| name == "/").unwrap();
        let namespaced_idx = replacements
            .iter()
            .position(|name| name == "foo::bar")
            .unwrap();
        assert!(slash_idx < namespaced_idx);
    }

    #[test]
    fn substring_matches_follow_prefix_matches() {
        let helper = ReplCompleter::new(
            env_with(&["keys", "keyword?", "stringify-keys"]),
            env_with(&[]),
        );
        let replacements = helper.complete_symbol_names_for_test("key");
        assert_eq!(
            replacements,
            vec![
                "keys".to_string(),
                "keyword?".to_string(),
                "stringify-keys".to_string()
            ]
        );
    }

    #[test]
    fn subsequence_matches_allow_abbreviations() {
        let helper = ReplCompleter::new(env_with(&["future-deref", "future-done?"]), env_with(&[]));
        let replacements = helper.complete_symbol_names_for_test("fud");
        assert_eq!(
            replacements,
            vec!["future-deref".to_string(), "future-done?".to_string()]
        );
    }

    #[test]
    fn base_name_exact_matches_are_prioritized() {
        let helper = ReplCompleter::new(
            env_with(&["shell::split", "process::sh", "process::sh!", "hash-map"]),
            env_with(&[]),
        );
        let replacements = helper.complete_symbol_names_for_test("sh");
        assert!(replacements.len() >= 2);
        assert_eq!(replacements[0], "process::sh");
        assert_eq!(replacements[1], "process::sh!");
    }

    #[test]
    fn path_fragment_split_detects_separator() {
        let (dir, prefix, sep) = split_path_fragment("src/lib");
        assert_eq!(dir, "src/");
        assert_eq!(prefix, "lib");
        assert_eq!(sep, Some('/'));
        let (root_dir, root_prefix, root_sep) = split_path_fragment("lib");
        assert_eq!(root_dir, "");
        assert_eq!(root_prefix, "lib");
        assert_eq!(root_sep, None);
    }

    #[test]
    fn double_colon_fragment_prefers_colon_display() {
        let helper = ReplCompleter::new(env_with(&[]), env_with(&["async::future"]));
        let replacements = helper.complete_symbol_names_for_test("async::");
        assert!(replacements.contains(&"async::future".to_string()));
    }

    #[test]
    fn enum_variant_completion_after_double_colon() {
        let engines: Vec<Arc<dyn ForeignEngine>> = Vec::new();
        let mut ctx = create_runtime(EvalOptions::default(), &engines);
        ctx.eval_source(
            "(ns repl::enum_completion)\n\
             (deftype Noop {})\n\
             (deftype Restart {:x Int})\n\
             (defenum Action Noop Restart)",
        )
        .expect("enum definition");
        let namespace_env = ctx
            .namespace_env("repl::enum_completion")
            .expect("namespace env");
        let shared_env = ctx.env();
        let helper = ReplCompleter::new(namespace_env, shared_env);
        let replacements = helper.complete_symbol_names_for_test("Action::");
        assert_eq!(
            replacements,
            vec!["Action::Noop".to_string(), "Action::Restart".to_string()]
        );
    }

    #[test]
    fn slash_fragment_matches_colon_symbol() {
        let helper = ReplCompleter::new(env_with(&["examples::types::demo::Dog"]), env_with(&[]));
        let replacements = helper.complete_symbol_names_for_test("examples/");
        assert!(replacements.contains(&"examples::types::demo::Dog".to_string()));
    }

    #[test]
    fn alias_functions_are_grouped() {
        let env = new_ref(Env::default());
        {
            let mut data = env.write().unwrap();
            let func = Value::native_fn(FnArity::exact(0), |_args| Ok(Value::Nil));
            data.set("repeat", func.clone());
            data.set("std::repeat", func);
        }
        let helper = ReplCompleter::new(env.clone(), env_with(&[]));
        let unqualified = helper.complete_symbol_names_for_test("rep");
        assert!(!unqualified.is_empty());
        assert_eq!(unqualified[0], "repeat".to_string());
        let qualified = helper.complete_symbol_names_for_test("std::rep");
        assert_eq!(qualified, vec!["std::repeat".to_string()]);
    }

    #[test]
    fn duplicate_plain_names_show_namespaced_aliases() {
        let namespace_env = new_ref(Env::default());
        let shared_env = new_ref(Env::default());
        {
            let mut ns = namespace_env.write().unwrap();
            let std_join = Value::native_fn(FnArity::exact(0), |_args| Ok(Value::Nil));
            ns.set("join", std_join.clone());
            ns.set("std::join", std_join);
        }
        {
            let mut shared = shared_env.write().unwrap();
            let string_join = Value::native_fn(FnArity::exact(0), |_args| Ok(Value::Nil));
            shared.set("join", string_join.clone());
            shared.set("string::join", string_join);
        }
        let helper = ReplCompleter::new(namespace_env, shared_env);
        let replacements = helper.complete_symbol_names_for_test("join");
        assert!(replacements.contains(&"join".to_string()));
        assert!(replacements.contains(&"string::join".to_string()));
    }

    #[test]
    fn split_signature_lines_splits_multi_arity_signature() {
        let sig = "repeat (Any) -> Any repeat (Int, Any) -> Any";
        let lines = super::split_signature_lines("repeat", sig);
        assert_eq!(
            lines,
            vec![
                "repeat (Any) -> Any".to_string(),
                "repeat (Int, Any) -> Any".to_string()
            ]
        );
    }

    #[test]
    fn test_format_description() {
        use crate::doc::DocInfo;

        let info = DocInfo {
            name: "repeat".to_string(),
            canonical: "core::repeat".to_string(),
            signature: Some(
                "repeat (Any) -> Any repeat (Int, Any) -> Any".to_string()
            ),
            doc: Some(
                "Produces copies of a value: one-arg returns an infinite lazy sequence, two-arg returns a vector of n repetitions.".to_string()
            ),
            origin: Some("core".to_string()),
        };

        let aliases = vec!["std::repeat".to_string()];
        let result = format_description(&info, &aliases, None);

        println!("=== Format Description Result ===");
        println!("{}", result);
        println!("=== End ===");

        // Ensure newlines are present
        assert!(result.contains("\n"));
        assert!(result.contains("repeat (Any) -> Any"));
        assert!(result.contains("repeat (Int, Any) -> Any"));
        assert!(result.contains("[core::repeat, repeat, std::repeat]"));
    }

    #[test]
    fn repl_completions_have_docs() {
        let engines: Vec<Arc<dyn ForeignEngine>> = Vec::new();
        let ctx = create_runtime(EvalOptions::default(), &engines);
        ctx.with_current_ctx(|ctx| {
            let shared_env = ctx.env();
            let default_ns = ctx.default_namespace_name();
            let namespace_env = ctx
                .namespace_env(&default_ns)
                .unwrap_or_else(|| shared_env.clone());
            let completer = ReplCompleter::new(namespace_env.clone(), shared_env.clone());
            let scopes = [namespace_env.clone(), shared_env.clone()];
            let allow: HashSet<&str> = COMPLETION_DOC_ALLOWLIST.iter().copied().collect();
            let mut missing = Vec::new();
            for entry in completer.collect_symbol_entries(true) {
                if entry.name.ends_with("::") || entry.name.ends_with('/') {
                    continue;
                }
                if matches!(entry.source, SymbolSource::Literal) {
                    continue;
                }
                let has_doc = doc::describe_symbol(&entry.name, &scopes)
                    .and_then(|info| info.doc)
                    .map(|doc| !doc.trim().is_empty())
                    .unwrap_or(false);
                if !has_doc {
                    missing.push(entry.name.clone());
                }
            }
            missing.sort();
            missing.dedup();
            missing.retain(|name| !allow.contains(name.as_str()));
            assert!(
                missing.is_empty(),
                "documentation missing for symbols: {:?}",
                missing
            );
        });
    }

    fn nav_result(ns_count: usize, var_count: usize, doc_count: usize) -> Value {
        let mut map = HashMap::new();
        map.insert(Key::Keyword("kind".into()), Value::Symbol(":nav".into()));
        map.insert(Key::Keyword("query".into()), Value::String("disj".into()));

        let ns_vec = (0..ns_count)
            .map(|idx| {
                let mut entry = HashMap::new();
                entry.insert(
                    Key::Keyword("ns".into()),
                    Value::Symbol(format!("std::ns{}", idx)),
                );
                entry.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(vec![Value::Symbol(":name".into())])),
                );
                Value::Map(entry)
            })
            .collect::<Vec<_>>();
        map.insert(
            Key::Keyword("ns".into()),
            Value::Vector(Vector::from(ns_vec)),
        );

        let var_vec = (0..var_count)
            .map(|idx| {
                let mut entry = HashMap::new();
                entry.insert(
                    Key::Keyword("sym".into()),
                    Value::Symbol(format!("std::set::disj{}", idx)),
                );
                entry.insert(Key::Keyword("ns".into()), Value::Symbol("std::set".into()));
                entry.insert(Key::Keyword("name".into()), Value::Symbol("disj".into()));
                entry.insert(
                    Key::Keyword("doc".into()),
                    Value::String("Remove items from a set.".into()),
                );
                entry.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(vec![
                        Value::Symbol(":name".into()),
                        Value::Symbol(":doc".into()),
                    ])),
                );
                Value::Map(entry)
            })
            .collect::<Vec<_>>();
        map.insert(
            Key::Keyword("var".into()),
            Value::Vector(Vector::from(var_vec)),
        );

        let doc_vec = (0..doc_count)
            .map(|idx| {
                let mut entry = HashMap::new();
                entry.insert(
                    Key::Keyword("name".into()),
                    Value::Symbol(format!("when-let{}", idx)),
                );
                entry.insert(
                    Key::Keyword("doc".into()),
                    Value::String("Conditional binding and evaluation.".into()),
                );
                entry.insert(
                    Key::Keyword("match".into()),
                    Value::Vector(Vector::from(vec![Value::Symbol(":doc".into())])),
                );
                Value::Map(entry)
            })
            .collect::<Vec<_>>();
        map.insert(
            Key::Keyword("doc".into()),
            Value::Vector(Vector::from(doc_vec)),
        );

        Value::Map(map)
    }

    #[test]
    fn render_nav_result_handles_empty() {
        let value = nav_result(0, 0, 0);
        let lines = super::render_nav_result(&value);
        assert_eq!(lines.len(), 1);
    }

    #[test]
    fn render_nav_result_handles_small() {
        let value = nav_result(1, 1, 1);
        let lines = super::render_nav_result(&value);
        assert!(lines.len() >= 4);
    }

    #[test]
    fn render_nav_result_handles_large() {
        let value = nav_result(0, 50, 0);
        let lines = super::render_nav_result(&value);
        assert_eq!(lines.len(), 51);
    }

    #[test]
    fn should_page_respects_terminal_size() {
        let prev_lines = env::var("LINES").ok();
        let prev_cols = env::var("COLUMNS").ok();

        env::set_var("LINES", "10");
        env::set_var("COLUMNS", "10");

        let short = "1234567890".repeat(2);
        assert!(!super::should_page(&short));

        let long = "1234567890".repeat(10);
        assert!(super::should_page(&long));

        match prev_lines {
            Some(value) => env::set_var("LINES", value),
            None => env::remove_var("LINES"),
        }
        match prev_cols {
            Some(value) => env::set_var("COLUMNS", value),
            None => env::remove_var("COLUMNS"),
        }
    }

    #[test]
    fn print_lines_with_optional_pager_does_not_panic() {
        super::print_lines_with_optional_pager(vec!["a".to_string(), "b".to_string()]);
    }
}
fn print_doc_full(info: &DocInfo) {
    let mut lines = Vec::new();
    if let Some(sig) = &info.signature {
        lines.push(sig.to_string());
    } else {
        lines.push(info.name.clone());
    }
    let alias_line = doc_aliases(info, &[]);
    if !alias_line.is_empty() {
        lines.push(format!("alias: {}", alias_line.join(", ")));
    }
    if let Some(origin) = &info.origin {
        lines.push(format!("source: {}", origin));
    }
    match &info.doc {
        Some(text) => lines.push(text.to_string()),
        None => lines.push("(no documentation available)".to_string()),
    }
    if let Some(examples) =
        doc::doc_examples_for(&info.canonical).or_else(|| doc::doc_examples_for(&info.name))
    {
        if !examples.is_empty() {
            lines.push("examples:".to_string());
            for ex in examples {
                lines.push(format!("  {}", ex));
            }
        }
    }
    if let Some(examples) =
        doc::doc_oop_examples_for(&info.canonical).or_else(|| doc::doc_oop_examples_for(&info.name))
    {
        if !examples.is_empty() {
            lines.push("oop examples:".to_string());
            for ex in examples {
                lines.push(format!("  {}", ex));
            }
        }
    }
    print_lines_with_optional_pager(lines);
}
