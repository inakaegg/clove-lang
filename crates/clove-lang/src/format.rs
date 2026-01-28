use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;

use clove_core::fmt_config::{load_fmt_config_path, resolved_config_toml};
use clove_core::formatter::{format_source, ForeignFormatter, FormatOptions};

pub fn run_fmt(args: Vec<String>) -> Result<(), String> {
    let mut use_stdin = false;
    let mut indent_width: Option<usize> = None;
    let mut max_inline_chars: Option<usize> = None;
    let mut config_path: Option<PathBuf> = None;
    let mut use_config_file = true;
    let mut print_config = false;
    let mut files: Vec<String> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--stdin" => {
                use_stdin = true;
                i += 1;
            }
            arg if arg.starts_with("--config=") => {
                let value = &arg["--config=".len()..];
                if value.is_empty() {
                    return Err("--config requires a value".into());
                }
                config_path = Some(PathBuf::from(value));
                i += 1;
            }
            "--config" => {
                if i + 1 >= args.len() {
                    return Err("--config requires a value".into());
                }
                config_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            "--no-config" => {
                use_config_file = false;
                i += 1;
            }
            "--print-config" => {
                print_config = true;
                i += 1;
            }
            arg if arg.starts_with("--width=") => {
                let value = &arg["--width=".len()..];
                max_inline_chars = Some(parse_width(value)?);
                i += 1;
            }
            "--width" => {
                if i + 1 >= args.len() {
                    return Err("--width requires a value".into());
                }
                max_inline_chars = Some(parse_width(&args[i + 1])?);
                i += 2;
            }
            arg if arg.starts_with("--indent=") => {
                let value = &arg["--indent=".len()..];
                indent_width = Some(parse_width(value)?);
                i += 1;
            }
            "--indent" => {
                if i + 1 >= args.len() {
                    return Err("--indent requires a value".into());
                }
                indent_width = Some(parse_width(&args[i + 1])?);
                i += 2;
            }
            "--help" | "-h" => return Err(fmt_usage()),
            other => {
                files.push(other.to_string());
                i += 1;
            }
        }
    }

    if use_stdin && !files.is_empty() {
        return Err("cannot use both --stdin and file paths".into());
    }
    if files.len() > 1 {
        return Err("fmt accepts exactly one file or --stdin".into());
    }

    let start_dir = resolve_start_dir(use_stdin, &files)?;
    let resolved_config_path = if use_config_file {
        if let Some(path) = config_path {
            Some(path)
        } else {
            find_fmt_config(&start_dir)
        }
    } else {
        None
    };

    let mut options = FormatOptions::default();
    if let Some(path) = resolved_config_path {
        let loaded = load_fmt_config_path(&path)?;
        for warning in loaded.warnings {
            eprintln!("warning: {}", warning);
        }
        loaded.config.apply(&mut options)?;
    }
    if let Some(indent) = indent_width {
        options.indent_width = indent.max(1);
    }
    if let Some(width) = max_inline_chars {
        options.max_inline_chars = width.max(40);
    }
    apply_foreign_formatter(&mut options);

    if print_config {
        let resolved = resolved_config_toml(&options)?;
        print!("{}", resolved);
        return Ok(());
    }

    let stdin_mode = use_stdin || files.is_empty() || (files.len() == 1 && files[0] == "-");
    let source = if stdin_mode {
        read_stdin()?
    } else if files.len() == 1 {
        fs::read_to_string(&files[0]).map_err(|e| format!("failed to read {}: {}", files[0], e))?
    } else {
        return Err("fmt accepts exactly one file or --stdin".into());
    };

    let (source, base_indent) = if stdin_mode {
        prepare_stdin_indent(&source)
    } else {
        (source, None)
    };

    let mut formatted = format_source(&source, options)?;
    if let Some(prefix) = base_indent {
        formatted = apply_base_indent(&formatted, &prefix);
    }
    print!("{}", formatted);
    Ok(())
}

fn parse_width(value: &str) -> Result<usize, String> {
    value
        .parse::<usize>()
        .map_err(|_| format!("invalid width value: {}", value))
}

fn read_stdin() -> Result<String, String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|e| format!("failed to read stdin: {}", e))?;
    Ok(buf)
}

fn prepare_stdin_indent(source: &str) -> (String, Option<String>) {
    let Some(prefix) = detect_base_indent_prefix(source) else {
        return (source.to_string(), None);
    };
    if prefix.is_empty() {
        return (source.to_string(), None);
    }
    let dedented = strip_base_indent(source, prefix.len());
    (dedented, Some(prefix))
}

fn detect_base_indent_prefix(source: &str) -> Option<String> {
    let mut min_indent: Option<usize> = None;
    let mut first_prefix: Option<String> = None;
    for chunk in source.split_inclusive('\n') {
        let (line, _) = split_line_ending(chunk);
        if line.trim().is_empty() {
            continue;
        }
        let indent = leading_indent_width(line);
        if first_prefix.is_none() {
            first_prefix = Some(line[..indent].to_string());
        }
        min_indent = Some(match min_indent {
            Some(min) => min.min(indent),
            None => indent,
        });
        if min_indent == Some(0) {
            break;
        }
    }
    let min_indent = min_indent?;
    if min_indent == 0 {
        return None;
    }
    let mut prefix = first_prefix.unwrap_or_default();
    if prefix.len() > min_indent {
        prefix.truncate(min_indent);
    }
    Some(prefix)
}

fn strip_base_indent(source: &str, indent: usize) -> String {
    if indent == 0 {
        return source.to_string();
    }
    let mut out = String::new();
    for chunk in source.split_inclusive('\n') {
        let (line, ending) = split_line_ending(chunk);
        if line.trim().is_empty() {
            out.push_str(ending);
            continue;
        }
        let trimmed = trim_leading_indent(line, indent);
        out.push_str(trimmed);
        out.push_str(ending);
    }
    out
}

fn apply_base_indent(source: &str, prefix: &str) -> String {
    if prefix.is_empty() {
        return source.to_string();
    }
    let mut out = String::new();
    for chunk in source.split_inclusive('\n') {
        let (line, ending) = split_line_ending(chunk);
        if line.trim().is_empty() {
            out.push_str(ending);
            continue;
        }
        out.push_str(prefix);
        out.push_str(line);
        out.push_str(ending);
    }
    out
}

fn split_line_ending(line: &str) -> (&str, &str) {
    if line.ends_with("\r\n") {
        let len = line.len();
        return (&line[..len - 2], "\r\n");
    }
    if line.ends_with('\n') {
        let len = line.len();
        return (&line[..len - 1], "\n");
    }
    if line.ends_with('\r') {
        let len = line.len();
        return (&line[..len - 1], "\r");
    }
    (line, "")
}

fn trim_leading_indent(line: &str, indent: usize) -> &str {
    if indent == 0 {
        return line;
    }
    let mut count = 0usize;
    for (idx, b) in line.as_bytes().iter().enumerate() {
        if count >= indent {
            return &line[idx..];
        }
        if matches!(b, b' ' | b'\t') {
            count += 1;
        } else {
            return &line[idx..];
        }
    }
    ""
}

fn leading_indent_width(line: &str) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|b| matches!(**b, b' ' | b'\t'))
        .count()
}

fn fmt_usage() -> String {
    "usage: clove fmt [--stdin] [--indent N] [--width N] [--config PATH] [--no-config] [--print-config] [file]".into()
}

fn apply_foreign_formatter(opts: &mut FormatOptions) {
    #[cfg(feature = "ruby")]
    {
        opts.foreign_formatter = Some(ruby_formatter());
    }
}

fn resolve_start_dir(use_stdin: bool, files: &[String]) -> Result<PathBuf, String> {
    let cwd = std::env::current_dir().map_err(|e| format!("failed to get cwd: {}", e))?;
    if use_stdin || files.is_empty() {
        return Ok(cwd);
    }
    if files.len() == 1 && files[0] == "-" {
        return Ok(cwd);
    }
    if files.len() != 1 {
        return Err("fmt accepts exactly one file or --stdin".into());
    }
    let input_path = PathBuf::from(&files[0]);
    let resolved_path = if input_path.is_absolute() {
        input_path
    } else {
        cwd.join(input_path)
    };
    Ok(resolved_path.parent().unwrap_or(&cwd).to_path_buf())
}

fn find_fmt_config(start_dir: &Path) -> Option<PathBuf> {
    for dir in start_dir.ancestors() {
        let primary = dir.join("clovefmt.toml");
        if primary.is_file() {
            return Some(primary);
        }
        let hidden = dir.join(".clovefmt.toml");
        if hidden.is_file() {
            return Some(hidden);
        }
    }
    None
}

#[cfg(feature = "ruby")]
fn ruby_formatter() -> Arc<ForeignFormatter> {
    let assume_tagless_is_ruby = true;
    Arc::new(move |tag, code| {
        let targets_ruby = match tag {
            Some(t) => t == "rb",
            None => assume_tagless_is_ruby,
        };
        if !targets_ruby {
            return None;
        }
        format_ruby_code(code)
    })
}

#[cfg(feature = "ruby")]
fn format_ruby_code(code: &str) -> Option<String> {
    for cmd in RUBY_COMMANDS {
        if let Some(out) = run_ruby_formatter(cmd, code) {
            return Some(out);
        }
    }
    None
}

#[cfg(feature = "ruby")]
enum RubyOutputMode {
    Plain,
    RubocopLike,
}

#[cfg(feature = "ruby")]
struct RubyFormatterCmd {
    program: &'static str,
    args: &'static [&'static str],
    mode: RubyOutputMode,
    needs_bundle: bool,
}

#[cfg(feature = "ruby")]
const RUBY_COMMANDS: &[RubyFormatterCmd] = &[
    RubyFormatterCmd {
        program: "rubocop",
        args: &[
            "-A",
            "--except",
            "Style/FrozenStringLiteralComment,Layout/EmptyLineAfterMagicComment,Layout/TrailingEmptyLines",
            "--stdin",
            "clove.rb",
        ],
        mode: RubyOutputMode::RubocopLike,
        needs_bundle: false,
    },
    RubyFormatterCmd {
        program: "bundle",
        args: &[
            "exec",
            "rubocop",
            "-A",
            "--except",
            "Style/FrozenStringLiteralComment,Layout/EmptyLineAfterMagicComment,Layout/TrailingEmptyLines",
            "--stdin",
            "clove.rb",
        ],
        mode: RubyOutputMode::RubocopLike,
        needs_bundle: true,
    },
    RubyFormatterCmd {
        program: "ruby",
        args: &[
            "-rsyntax_tree",
            "-e",
            "STDOUT.write(SyntaxTree.format(ARGF.read))",
        ],
        mode: RubyOutputMode::Plain,
        needs_bundle: false,
    },
];

#[cfg(feature = "ruby")]
fn run_ruby_formatter(cmd: &RubyFormatterCmd, code: &str) -> Option<String> {
    if cmd.needs_bundle && !has_gemfile() {
        return None;
    }
    let mut child = Command::new(cmd.program);
    child.args(cmd.args);
    child.stdin(Stdio::piped());
    child.stdout(Stdio::piped());
    child.stderr(Stdio::piped());
    let mut child = child.spawn().ok()?;
    {
        let stdin = child.stdin.as_mut()?;
        stdin.write_all(code.as_bytes()).ok()?;
    }
    let output = child.wait_with_output().ok()?;
    if !output.status.success() {
        return None;
    }
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    if stdout.is_empty() {
        return None;
    }
    let cleaned = match cmd.mode {
        RubyOutputMode::Plain => stdout,
        RubyOutputMode::RubocopLike => extract_rubocop_output(&stdout)?,
    };
    let cleaned = strip_frozen_comment(&cleaned);
    let cleaned = trim_trailing_newlines(&cleaned);
    if cleaned.is_empty() {
        return None;
    }
    Some(cleaned)
}

#[cfg(feature = "ruby")]
fn extract_rubocop_output(output: &str) -> Option<String> {
    if let Some(idx) = output.rfind("====================") {
        let tail = &output[idx + "====================".len()..];
        let trimmed = tail.trim_start_matches(|c| c == '\n' || c == '\r' || c == ' ');
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }
    let trimmed = output.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

#[cfg(feature = "ruby")]
fn strip_frozen_comment(s: &str) -> String {
    let mut lines = s.lines();
    let mut dropped = false;
    let mut out = Vec::new();
    while let Some(line) = lines.next() {
        if !dropped && line.trim() == "# frozen_string_literal: true" {
            dropped = true;
            continue;
        }
        if dropped && out.is_empty() && line.trim().is_empty() {
            continue;
        }
        out.push(line);
    }
    out.join("\n")
}

fn trim_trailing_newlines(s: &str) -> String {
    s.trim_end_matches(|c| c == '\n' || c == '\r').to_string()
}

#[cfg(feature = "ruby")]
fn has_gemfile() -> bool {
    Path::new("Gemfile").exists() || Path::new("gems.rb").exists()
}
