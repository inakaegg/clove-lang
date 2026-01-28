use std::cell::{Cell, RefCell};
use std::io::{BufRead, Write};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use rustyline::{error::ReadlineError, DefaultEditor};

use crate::ast::Value;
use crate::env::EnvRef;
use crate::error::{format_error, CloveError, ERROR_TAG};
use crate::eval::Evaluator;
use crate::options::EvalOptions;
use crate::runtime::RuntimeCtx;

use crate::interrupt;
use once_cell::sync::Lazy;

pub type DebugReplHandler = fn(&Evaluator, EnvRef) -> Result<Value, CloveError>;

static DEBUG_REPL_HANDLER: Lazy<RwLock<Option<DebugReplHandler>>> = Lazy::new(|| RwLock::new(None));

thread_local! {
    static LAST_DEBUG_ERROR: RefCell<Option<CloveError>> = RefCell::new(None);
    static DEBUG_REPL_DEPTH: Cell<usize> = Cell::new(0);
}

pub fn set_debug_repl_handler(handler: Option<DebugReplHandler>) {
    *DEBUG_REPL_HANDLER.write().unwrap() = handler;
    let _ = RuntimeCtx::try_with_current(|ctx| {
        ctx.refresh_debug_stash_enabled();
        Ok(())
    });
}

pub fn debug_repl_handler_is_set() -> bool {
    DEBUG_REPL_HANDLER.read().unwrap().is_some()
}

pub fn in_debug_repl() -> bool {
    DEBUG_REPL_DEPTH.with(|depth| depth.get() > 0)
}

struct DebugReplGuard {
    prev_depth: usize,
}

impl DebugReplGuard {
    fn enter() -> Self {
        DEBUG_REPL_DEPTH.with(|depth| {
            let prev = depth.get();
            depth.set(prev + 1);
            Self { prev_depth: prev }
        })
    }
}

impl Drop for DebugReplGuard {
    fn drop(&mut self) {
        DEBUG_REPL_DEPTH.with(|depth| depth.set(self.prev_depth));
    }
}

pub fn set_last_debug_error(err: CloveError) {
    LAST_DEBUG_ERROR.with(|slot| {
        *slot.borrow_mut() = Some(err);
    });
}

pub fn take_last_debug_error() -> Option<CloveError> {
    LAST_DEBUG_ERROR.with(|slot| slot.borrow_mut().take())
}

pub fn run_debug_repl(evaluator: &Evaluator, env: EnvRef) -> Result<Value, CloveError> {
    let _guard = DebugReplGuard::enter();
    if let Some(handler) = *DEBUG_REPL_HANDLER.read().unwrap() {
        return handler(evaluator, env);
    }
    debug_repl_with_editor(evaluator, env)
}

fn print_formatted_error(err: &CloveError) {
    for line in format_error(err) {
        println!("{}", line);
    }
}

fn write_formatted_error<W: Write>(writer: &mut W, err: &CloveError) {
    for line in format_error(err) {
        writeln!(writer, "{}", line).ok();
    }
}

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

pub fn interactive_repl_with_engines(
    opts: EvalOptions,
    engines: &[std::sync::Arc<dyn crate::foreign::ForeignEngine>],
) {
    let ctx = RuntimeCtx::new(opts, engines);
    let shared_env = ctx.env();

    let mut rl = DefaultEditor::new().expect("create line editor");
    let hist_path = history_path();
    if let Some(ref path) = hist_path {
        let _ = rl.load_history(path);
    }
    println!("clove REPL. :q to quit, :env to show vars, :load FILE to load.");
    loop {
        let current_ns = ctx.default_namespace_name();
        let namespace_env = ctx
            .namespace_env(&current_ns)
            .unwrap_or_else(|| shared_env.clone());
        match rl.readline("repl> ") {
            Ok(line) => {
                interrupt::clear_interrupt();
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                match handle_interactive_meta(
                    trimmed,
                    ctx.clone(),
                    namespace_env.clone(),
                    shared_env.clone(),
                ) {
                    MetaResult::Continue => continue,
                    MetaResult::Exit => break,
                    MetaResult::NotHandled => {}
                }
                match eval_line_in_env(ctx.clone(), namespace_env.clone(), &line) {
                    Ok(val) => println!("{}", val),
                    Err(e) => {
                        if interrupt::is_interrupted() {
                            println!("; execution interrupted");
                            interrupt::clear_interrupt();
                        } else {
                            print_formatted_error(&e);
                        }
                    }
                }
                if let Some(ref path) = hist_path {
                    let _ = rl.add_history_entry(trimmed);
                    let _ = rl.append_history(path);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("{} repl: {}", ERROR_TAG, e);
                break;
            }
        }
    }
}

fn handle_interactive_meta(
    line: &str,
    ctx: Arc<RuntimeCtx>,
    namespace_env: EnvRef,
    shared_env: EnvRef,
) -> MetaResult {
    if command_matches(line, &["q", "quit"]) {
        return MetaResult::Exit;
    }
    if command_matches(line, &["continue", "cont", "c", "next", "n"]) {
        println!("nothing to continue; already at top level");
        return MetaResult::Continue;
    }
    if command_matches(line, &["help", "h"]) {
        println!(":env to show vars, :load FILE to load, :set NAME EXPR to bind");
        return MetaResult::Continue;
    }
    match line {
        ":env" => {
            print_env_bindings(&shared_env);
            return MetaResult::Continue;
        }
        ":vars" | ":local_vars" | ":local_variables" => {
            print_env_bindings(&namespace_env);
            return MetaResult::Continue;
        }
        _ => {}
    }

    if let Some(rest) = line.strip_prefix(":load ") {
        let path = rest.trim();
        if path.is_empty() {
            println!("usage: :load path");
            return MetaResult::Continue;
        }
        let prev_source = ctx.source_name();
        let prev_tag = ctx.default_tag();
        match std::fs::read_to_string(path) {
            Ok(content) => {
                ctx.set_source_name(Some(path.to_string()));
                match ctx.eval_source(&content) {
                    Ok(_) => {}
                    Err(e) => print_formatted_error(&e),
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
            match eval_line_in_env(ctx, namespace_env.clone(), expr) {
                Ok(val) => {
                    shared_env.write().unwrap().set_in_chain(name, val);
                }
                Err(e) => print_formatted_error(&e),
            }
        } else {
            println!("usage: :set name expr");
        }
        return MetaResult::Continue;
    }

    if let Some(rest) = line.strip_prefix(":source") {
        let (path, pos) = parse_source_spec(rest);
        if path.is_empty() {
            ctx.set_source_name(None);
            ctx.set_source_position(1, 1);
            println!("source cleared");
        } else {
            ctx.set_source_name(Some(path.clone()));
            if let Some((line, col)) = pos {
                ctx.set_source_position(line, col);
            } else {
                ctx.set_source_position(1, 1);
            }
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

fn eval_line_in_env(ctx: Arc<RuntimeCtx>, env: EnvRef, src: &str) -> Result<Value, CloveError> {
    let source_name = ctx.source_name();
    let default_tag = ctx.default_tag();
    ctx.eval_repl_source(src, env, source_name.as_deref(), default_tag.as_deref())
}

fn eval_debug_line(env: EnvRef, src: &str) -> Result<Value, CloveError> {
    RuntimeCtx::with_current(|ctx| {
        let source_name = ctx.source_name();
        let default_tag = ctx.default_tag();
        ctx.eval_repl_source(src, env, source_name.as_deref(), default_tag.as_deref())
    })
}

pub fn debug_repl_with_io<R: BufRead, W: Write>(
    _evaluator: &Evaluator,
    env: EnvRef,
    reader: &mut R,
    writer: &mut W,
) -> Result<Value, CloveError> {
    if RuntimeCtx::try_with_current(|_| Ok(())).is_some() {
        return debug_repl_loop(env, reader, writer);
    }
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    ctx.with_current_ctx(|_| debug_repl_loop(env, reader, writer))
}

fn debug_repl_loop<R: BufRead, W: Write>(
    env: EnvRef,
    reader: &mut R,
    writer: &mut W,
) -> Result<Value, CloveError> {
    loop {
        print_prompt(writer, "dbg> ");
        let mut line = String::new();
        if reader
            .read_line(&mut line)
            .map_err(|e| CloveError::runtime(e.to_string()))?
            == 0
        {
            break;
        }
        let trimmed = line.trim();
        match handle_meta(trimmed, env.clone(), writer) {
            MetaResult::Continue => continue,
            MetaResult::Exit => break,
            MetaResult::NotHandled => {}
        }
        if let Err(e) = match eval_debug_line(env.clone(), trimmed) {
            Ok(val) => writeln!(writer, "=> {}", val),
            Err(e) => {
                write_formatted_error(writer, &e);
                Ok(())
            }
        } {
            let _ = writeln!(writer, "{} repl: {}", ERROR_TAG, e);
        }
    }
    Ok(Value::Nil)
}

// Interactive mode for break. Uses rustyline, so line editing works without rlwrap.
pub fn debug_repl_with_editor(_evaluator: &Evaluator, env: EnvRef) -> Result<Value, CloveError> {
    let mut rl = DefaultEditor::new().map_err(|e| CloveError::runtime(e.to_string()))?;
    let hist_path = history_path();
    if let Some(ref path) = hist_path {
        let _ = rl.load_history(path);
    }
    println!("(break) debug REPL. :q/:quit/:continue/:next to resume, :env to show vars.");
    loop {
        match rl.readline("dbg> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                match handle_meta(trimmed, env.clone(), &mut std::io::stdout()) {
                    MetaResult::Continue => continue,
                    MetaResult::Exit => break,
                    MetaResult::NotHandled => {}
                }
                match eval_debug_line(env.clone(), trimmed) {
                    Ok(last) => println!("=> {}", last),
                    Err(e) => print_formatted_error(&e),
                }
                if let Some(ref path) = hist_path {
                    let _ = rl.add_history_entry(trimmed);
                    let _ = rl.append_history(path);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("{} repl: {}", ERROR_TAG, e);
                break;
            }
        }
    }
    Ok(Value::Nil)
}

fn handle_meta<W: Write>(line: &str, env: EnvRef, writer: &mut W) -> MetaResult {
    if command_matches(line, &["q", "quit"]) {
        return MetaResult::Exit;
    }
    if command_matches(line, &["continue", "cont", "c", "next", "n"]) {
        return MetaResult::Exit;
    }
    if command_matches(line, &["help", "h"]) {
        writeln!(
            writer,
            ":env to show vars, :load FILE to load, :set NAME EXPR to bind"
        )
        .ok();
        return MetaResult::Continue;
    }
    if line == ":env" {
        let mut keys: Vec<_> = env.read().unwrap().clone_data().into_iter().collect();
        keys.sort_by_key(|(k, _)| k.clone());
        for (k, v) in keys {
            writeln!(writer, "{} = {}", k, v).ok();
        }
        return MetaResult::Continue;
    }
    if let Some(rest) = line.strip_prefix(":load ") {
        let path = rest.trim();
        if path.is_empty() {
            writeln!(writer, "usage: :load path").ok();
            return MetaResult::Continue;
        }
        match std::fs::read_to_string(path) {
            Ok(content) => {
                if let Err(e) = RuntimeCtx::with_current(|ctx| {
                    let prev_source = ctx.source_name();
                    let prev_tag = ctx.default_tag();
                    ctx.set_source_name(Some(path.to_string()));
                    let result = ctx.eval_source(&content);
                    ctx.set_source_name(prev_source);
                    ctx.set_default_tag(prev_tag);
                    result.map(|_| ())
                }) {
                    write_formatted_error(writer, &e);
                }
            }
            Err(e) => {
                let _ = writeln!(writer, "{} load: {}", ERROR_TAG, e);
            }
        }
        return MetaResult::Continue;
    }
    if let Some(rest) = line.strip_prefix(":set ") {
        let mut parts = rest.splitn(2, ' ');
        if let (Some(name), Some(expr)) = (parts.next(), parts.next()) {
            match RuntimeCtx::with_current(|ctx| {
                let source_name = ctx.source_name();
                let default_tag = ctx.default_tag();
                ctx.eval_repl_source(
                    expr,
                    env.clone(),
                    source_name.as_deref(),
                    default_tag.as_deref(),
                )
            }) {
                Ok(v) => {
                    env.write().unwrap().set_in_chain(name, v);
                }
                Err(e) => write_formatted_error(writer, &e),
            }
        } else {
            writeln!(writer, "usage: :set name expr").ok();
        }
        return MetaResult::Continue;
    }

    if let Some(rest) = line.strip_prefix(":source") {
        let (path, pos) = parse_source_spec(rest);
        let result = RuntimeCtx::with_current(|ctx| {
            if path.is_empty() {
                ctx.set_source_name(None);
                ctx.set_source_position(1, 1);
            } else {
                ctx.set_source_name(Some(path.clone()));
                if let Some((line, col)) = pos {
                    ctx.set_source_position(line, col);
                } else {
                    ctx.set_source_position(1, 1);
                }
            }
            Ok(())
        });
        match result {
            Ok(()) => {
                if path.is_empty() {
                    writeln!(writer, "source cleared").ok();
                } else {
                    writeln!(writer, "source set to {}", path).ok();
                }
            }
            Err(e) => write_formatted_error(writer, &e),
        }
        return MetaResult::Continue;
    }

    if let Some(rest) = line.strip_prefix(":lang ") {
        let tag = rest.trim();
        let result = RuntimeCtx::with_current(|ctx| {
            if tag.is_empty() {
                ctx.set_default_tag(None);
            } else {
                ctx.set_default_tag(Some(tag.to_string()));
            }
            Ok(())
        });
        match result {
            Ok(()) => {
                if tag.is_empty() {
                    writeln!(writer, "default language cleared").ok();
                } else {
                    writeln!(writer, "default language set to {}", tag).ok();
                }
            }
            Err(e) => write_formatted_error(writer, &e),
        }
        return MetaResult::Continue;
    }

    MetaResult::NotHandled
}

fn print_prompt<W: Write>(writer: &mut W, prompt: &str) {
    write!(writer, "{}", prompt).ok();
    let _ = writer.flush();
}

pub fn history_path() -> Option<PathBuf> {
    std::env::var_os("HOME").map(|home| {
        let mut p = PathBuf::from(home);
        p.push(".clove_history");
        p
    })
}

fn print_env_bindings(env: &EnvRef) {
    let mut entries: Vec<_> = env.read().unwrap().clone_data().into_iter().collect();
    entries.sort_by_key(|(k, _)| k.clone());
    for (k, v) in entries {
        println!("{} = {}", k, v);
    }
}

#[derive(PartialEq)]
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
