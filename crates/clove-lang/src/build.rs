use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use clove_core::ast::{Form, FormKind, MapItem};
use clove_core::error::CloveError;
use clove_core::plugin_meta;
use clove_core::reader::ReaderOptions;
use clove_core::typed_ir::{build_typed_exprs, format_typed_exprs, TypedExpr, TypedExprKind};
use clove_core::typing::infer::infer_forms;
#[path = "hm_codegen.rs"]
mod hm_codegen;

use crate::deps;
use clove_core::reader::Reader;

const MUSL_TARGET: &str = "x86_64-unknown-linux-musl";

#[derive(Debug, Clone, Copy)]
enum BuildBackend {
    /// Interpreter bundle (legacy behavior).
    Interp,
    /// Typed build based on HM type inference (not fully implemented yet).
    Hm { strict_types: bool },
}

#[derive(Debug)]
struct BuildOptions {
    input: PathBuf,
    output: PathBuf,
    static_link: bool,
    embed_ruby: bool,
    embed_python: bool,
    allow_native_plugins: bool,
    plugin_dirs: Vec<PathBuf>,
    backend: BuildBackend,
    emit_typed_ir: Option<PathBuf>,
}

pub fn run_build(args: Vec<String>) -> Result<(), String> {
    let opts = parse_args(args)?;
    match opts.backend {
        BuildBackend::Hm { strict_types } => run_hm_build(opts, strict_types),
        BuildBackend::Interp => run_interp_build(opts),
    }
}

fn run_interp_build(opts: BuildOptions) -> Result<(), String> {
    if opts.static_link && std::env::consts::OS != "linux" {
        return Err("--static is currently supported only on Linux hosts".into());
    }
    if (opts.embed_ruby || opts.embed_python) && std::env::consts::OS != "linux" {
        return Err("--embed-ruby/--embed-python is supported only on Linux (musl)".into());
    }
    if (opts.embed_ruby || opts.embed_python) && !opts.static_link {
        return Err(
            "--embed-ruby/--embed-python requires --static (x86_64-unknown-linux-musl)".into(),
        );
    }

    let source = fs::read_to_string(&opts.input)
        .map_err(|e| format!("failed to read {}: {}", opts.input.display(), e))?;
    if opts.static_link && !(opts.embed_ruby || opts.embed_python) && contains_foreign(&source)? {
        return Err("--static is supported only for pure clove code (no ${...} / $rb{...} / $py{...} blocks)".into());
    }

    let crate_name = crate_name_from_path(&opts.input);
    let build_dir = Path::new("target")
        .join("clove-build")
        .join(crate_name.clone());
    fs::create_dir_all(build_dir.join("src"))
        .map_err(|e| format!("failed to create build directory: {}", e))?;
    // keep paths absolute so e.g. PYO3_CONFIG_FILE is accepted
    let build_dir = fs::canonicalize(build_dir)
        .map_err(|e| format!("failed to canonicalize build dir: {}", e))?;

    write_cargo_toml(
        &build_dir,
        &crate_name,
        opts.static_link,
        opts.embed_ruby,
        opts.embed_python,
    )?;
    write_main_rs(
        &build_dir,
        &opts.input,
        opts.allow_native_plugins,
        &opts.plugin_dirs,
    )?;

    let mut cmd = Command::new("cargo");
    cmd.arg("build").arg("--release");
    if opts.static_link {
        cmd.args(["--target", MUSL_TARGET]);
    }
    cmd.current_dir(&build_dir);
    cmd.stderr(Stdio::inherit());
    cmd.stdout(Stdio::inherit());

    if opts.embed_ruby {
        cmd.env("RB_SYS_CARGO_BUILD_RUBY", "true");
    }
    let python_cfg = if opts.embed_python {
        Some(generate_pyo3_config(&build_dir)?)
    } else {
        None
    };
    if let Some(cfg) = python_cfg {
        cmd.env("PYO3_CONFIG_FILE", cfg);
    }

    let status = cmd
        .status()
        .map_err(|e| format!("failed to spawn cargo build: {}", e))?;
    if !status.success() {
        return Err("cargo build failed".into());
    }

    let target_dir = if opts.static_link {
        build_dir.join("target").join(MUSL_TARGET).join("release")
    } else {
        build_dir.join("target").join("release")
    };
    let built_bin = target_dir.join(exe_name(&crate_name));
    if !built_bin.exists() {
        return Err(format!("built binary not found at {}", built_bin.display()));
    }
    if let Some(parent) = opts.output.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("failed to create {}: {}", parent.display(), e))?;
        }
    }
    fs::copy(&built_bin, &opts.output).map_err(|e| format!("failed to copy binary: {}", e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&opts.output)
            .map_err(|e| format!("failed to read permissions: {}", e))?
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&opts.output, perms)
            .map_err(|e| format!("failed to set permissions: {}", e))?;
    }
    println!(
        "built {} -> {}",
        opts.input.display(),
        opts.output.display()
    );
    Ok(())
}

fn parse_args(args: Vec<String>) -> Result<BuildOptions, String> {
    let mut static_link = false;
    let mut embed_ruby = false;
    let mut embed_python = false;
    let mut allow_native_plugins = false;
    let mut plugin_dirs: Vec<PathBuf> = Vec::new();
    let mut output: Option<PathBuf> = None;
    let mut backend_name: Option<String> = None;
    let mut strict_types = false;
    let mut emit_typed_ir: Option<PathBuf> = None;
    let mut tail: Vec<String> = Vec::new();
    let mut iter = args.into_iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--static" => static_link = true,
            "--embed-ruby" => embed_ruby = true,
            "--embed-python" => embed_python = true,
            "--allow-native-plugins" => allow_native_plugins = true,
            "--plugin-dir" => {
                if let Some(path) = iter.next() {
                    plugin_dirs.push(PathBuf::from(path));
                } else {
                    return Err("missing value for --plugin-dir".into());
                }
            }
            "--strict-types" => strict_types = true,
            "--emit-typed-ir" => {
                if let Some(path) = iter.next() {
                    emit_typed_ir = Some(PathBuf::from(path));
                } else {
                    return Err("missing value for --emit-typed-ir".into());
                }
            }
            opt if opt.starts_with("--opt=") => {
                let val = opt.trim_start_matches("--opt=");
                backend_name = Some(val.to_string());
            }
            "--opt" => {
                if let Some(val) = iter.next() {
                    backend_name = Some(val);
                } else {
                    return Err("missing value for --opt".into());
                }
            }
            "-o" | "--output" => {
                if let Some(path) = iter.next() {
                    output = Some(PathBuf::from(path));
                } else {
                    return Err("missing value for -o/--output".into());
                }
            }
            "--help" | "-h" => {
                print_build_help();
                std::process::exit(0);
            }
            other if other.starts_with('-') => return Err(format!("unknown option: {}", other)),
            other => tail.push(other.to_string()),
        }
    }

    if tail.is_empty() {
        return Err("no input file specified".into());
    }
    if tail.len() > 1 {
        return Err("multiple input files are not supported".into());
    }

    let input = PathBuf::from(&tail[0]);
    if !input.exists() {
        return Err(format!("input file not found: {}", input.display()));
    }
    let output = output.unwrap_or_else(|| default_output_path(&input));
    let backend = parse_backend(backend_name.as_deref().unwrap_or("interp"), strict_types)?;
    if !matches!(backend, BuildBackend::Hm { .. }) && strict_types {
        return Err("--strict-types requires --opt hm".into());
    }

    Ok(BuildOptions {
        input,
        output,
        static_link,
        embed_ruby,
        embed_python,
        allow_native_plugins,
        plugin_dirs,
        backend,
        emit_typed_ir,
    })
}

fn print_build_help() {
    println!("Usage: clove build [OPTIONS] file");
    println!();
    println!("Options:");
    println!("  --static            Linux musl (x86_64-unknown-linux-musl) static build (pure clove only)");
    println!("  --embed-ruby        Embed Ruby runtime (Linux musl, requires --static)");
    println!("  --embed-python      Embed Python runtime (Linux musl, requires --static)");
    println!("  --allow-native-plugins  enable native plugins by default in the output binary");
    println!(
        "  --plugin-dir PATH       add plugin search path (repeatable, baked into output binary)"
    );
    println!("  --opt {{interp|hm}}   Select build backend (default: interp)");
    println!("  --strict-types      Only with --opt hm. Fail on missing types instead of fallback (planned).");
    println!(
        "  --emit-typed-ir F   Only with --opt hm. Dump inferred typed IR to a file (debug use)."
    );
    println!(
        "  -o, --output PATH   Output binary name (default: <file> without extension)"
    );
}

fn parse_backend(name: &str, strict_types: bool) -> Result<BuildBackend, String> {
    match name {
        "interp" => Ok(BuildBackend::Interp),
        "hm" | "typed" => Ok(BuildBackend::Hm { strict_types }),
        other => Err(format!(
            "unknown --opt value '{}'; supported: interp, hm",
            other
        )),
    }
}

fn run_hm_build(opts: BuildOptions, strict_types: bool) -> Result<(), String> {
    if opts.static_link || opts.embed_ruby || opts.embed_python {
        return Err(
            "HM backend (--opt hm) currently does not support --static/--embed-ruby/--embed-python"
                .into(),
        );
    }
    if !matches!(opts.backend, BuildBackend::Hm { .. }) {
        return Err("internal error: run_hm_build called with non-HM backend".into());
    }
    let mut meta_dirs = opts.plugin_dirs.clone();
    if opts.allow_native_plugins {
        if let Some(dir) = opts.input.parent() {
            match deps::load_project_plugin_dirs(dir) {
                Ok(extra) => meta_dirs.extend(extra),
                Err(err) => eprintln!("[hm] meta: {}", err),
            }
        }
    }
    if !meta_dirs.is_empty() {
        if let Err(err) = plugin_meta::load_meta_from_dirs(&meta_dirs) {
            eprintln!("[hm] meta: {}", err);
        }
    }
    let source = fs::read_to_string(&opts.input)
        .map_err(|e| format!("failed to read {}: {}", opts.input.display(), e))?;
    let mut reader = Reader::new_with_options(&source, ReaderOptions::default());
    let forms = reader
        .read_all()
        .map_err(|e| format!("parse error: {}", e))?;
    let infer = infer_forms(&forms).map_err(|e| format_infer_error(e, &opts.input))?;
    let typed_ir = build_typed_exprs(&infer);
    println!(
        "[hm] type inference ok ({} forms, typed_ir len = {})",
        infer.forms.len(),
        typed_ir.len()
    );
    check_hm_compatibility(&typed_ir, strict_types)?;
    if let Some(path) = &opts.emit_typed_ir {
        let dump = format_typed_exprs(&typed_ir);
        fs::write(path, dump)
            .map_err(|e| format!("failed to write typed IR to {}: {}", path.display(), e))?;
        println!("[hm] wrote typed IR dump to {}", path.display());
    }
    let crate_name = crate_name_from_path(&opts.input);
    let build_dir = std::env::temp_dir()
        .join("clove-typed-build")
        .join(format!("typed-{}", crate_name));
    let build_dir = fs::canonicalize(&build_dir).unwrap_or(build_dir);
    let project = if hm_codegen::requires_dynamic_fallback(&typed_ir) {
        println!("[hm] fallback: generate runtime build (unsupported forms detected)");
        fs::create_dir_all(build_dir.join("src"))
            .map_err(|e| format!("failed to create build dir: {}", e))?;
        write_cargo_toml(&build_dir, &crate_name, false, false, false)?;
        write_main_rs(
            &build_dir,
            &opts.input,
            opts.allow_native_plugins,
            &opts.plugin_dirs,
        )?;
        hm_codegen::GeneratedProject {
            build_dir: build_dir.clone(),
            crate_name: crate_name.clone(),
        }
    } else {
        hm_codegen::generate_rust_project(
            &build_dir,
            &crate_name,
            &opts.output,
            &opts.input,
            &typed_ir,
        )?
    };
    let mut cmd = Command::new("cargo");
    cmd.arg("build").arg("--release");
    cmd.current_dir(&project.build_dir);
    cmd.stderr(Stdio::inherit());
    cmd.stdout(Stdio::inherit());
    let status = cmd
        .status()
        .map_err(|e| format!("failed to spawn cargo build: {}", e))?;
    if !status.success() {
        return Err("cargo build failed (hm backend)".into());
    }
    let built_bin = project
        .build_dir
        .join("target")
        .join("release")
        .join(exe_name(&project.crate_name));
    fs::copy(&built_bin, &opts.output)
        .map_err(|e| format!("failed to copy typed binary: {}", e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&opts.output)
            .map_err(|e| format!("failed to read permissions: {}", e))?
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&opts.output, perms)
            .map_err(|e| format!("failed to set permissions: {}", e))?;
    }
    println!(
        "[hm] built typed binary -> {} (strict_types={})",
        opts.output.display(),
        strict_types
    );
    Ok(())
}

fn default_output_path(input: &Path) -> PathBuf {
    let stem = input.file_stem().and_then(OsStr::to_str).unwrap_or("app");
    PathBuf::from(stem)
}

fn crate_name_from_path(path: &Path) -> String {
    let stem = path
        .file_stem()
        .and_then(OsStr::to_str)
        .unwrap_or("clove_app");
    let mut out = String::new();
    for (idx, ch) in stem.chars().enumerate() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else if ch == '_' || ch == '-' {
            out.push('_');
        } else if idx == 0 {
            out.push('c');
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        "clove_app".to_string()
    } else {
        out
    }
}

fn check_hm_compatibility(exprs: &[TypedExpr], strict: bool) -> Result<(), String> {
    let mut issues = Vec::new();
    for expr in exprs {
        collect_issues(expr, &mut issues);
    }
    if issues.is_empty() {
        return Ok(());
    }
    println!(
        "[hm] WARNING: found {} potential incompatibilities (strict_types={})",
        issues.len(),
        strict
    );
    for msg in &issues {
        println!("  - {}", msg);
    }
    if strict {
        return Err("hm backend: strict_types enabled and incompatibilities detected".into());
    }
    Ok(())
}

fn format_infer_error(err: CloveError, input: &Path) -> String {
    let mut out = format!("type inference failed: {}", err);
    if let Some(span) = err.span() {
        out.push_str(&format!(
            "\n  at {}:{}:{}",
            input.display(),
            span.line,
            span.col
        ));
    }
    out
}

fn collect_issues(expr: &TypedExpr, issues: &mut Vec<String>) {
    use clove_core::typing::hm::Type;
    // Top-level forms to ignore (definitions are not part of HM codegen)
    if let TypedExprKind::Call { callee, .. } = &expr.kind {
        if let TypedExprKind::Symbol(sym) = &callee.kind {
            if sym == "defn"
                || sym == "defn-"
                || sym == "def-"
                || sym == "def"
                || sym == "ns"
                || sym == "deftype"
                || sym == "defenum"
                || sym == "use"
                || sym == "use-syntax"
                || sym == "require"
                || sym == "require-native"
            {
                return;
            }
        }
    }
    match &expr.ty {
        Type::Any => issues.push(format!("type Any at {}:{}", expr.span.line, expr.span.col)),
        Type::Var(tv) => issues.push(format!(
            "unresolved type var {:?} at {}:{}",
            tv, expr.span.line, expr.span.col
        )),
        _ => {}
    }
    match &expr.kind {
        TypedExprKind::Unknown => issues.push(format!(
            "unknown expr at {}:{}",
            expr.span.line, expr.span.col
        )),
        TypedExprKind::Call { callee, args } => {
            collect_issues(callee, issues);
            for a in args {
                collect_issues(a, issues);
            }
        }
        TypedExprKind::Vector(items) => {
            for it in items {
                collect_issues(it, issues);
            }
        }
        TypedExprKind::Map(map) => {
            for (_k, v) in map {
                collect_issues(v, issues);
            }
        }
        TypedExprKind::Fn { body, .. } => {
            for b in body {
                collect_issues(b, issues);
            }
        }
        TypedExprKind::Let { bindings, body } => {
            for (_n, v) in bindings {
                collect_issues(v, issues);
            }
            for b in body {
                collect_issues(b, issues);
            }
        }
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_issues(cond, issues);
            for t in then_branch {
                collect_issues(t, issues);
            }
            for e in else_branch {
                collect_issues(e, issues);
            }
        }
        TypedExprKind::Do(items) => {
            for it in items {
                collect_issues(it, issues);
            }
        }
        TypedExprKind::Literal(_) | TypedExprKind::Symbol(_) => {}
    }
}

fn write_cargo_toml(
    build_dir: &Path,
    crate_name: &str,
    static_link: bool,
    embed_ruby: bool,
    embed_python: bool,
) -> Result<(), String> {
    let clove_path = fs::canonicalize(Path::new("crates").join("clove-lang"))
        .map_err(|e| format!("failed to locate clove-lang crate: {}", e))?;
    let clove_core_path = fs::canonicalize(Path::new("crates").join("clove-core"))
        .map_err(|e| format!("failed to locate clove-core crate: {}", e))?;
    let mut toml = format!(
        "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\n",
        crate_name
    );
    if static_link {
        let mut dep = format!(
            "clove-lang = {{ path = \"{}\", default-features = false",
            clove_path.display()
        );
        let mut features = Vec::new();
        if embed_ruby {
            features.push("\"ruby\"");
        }
        if embed_python {
            features.push("\"python\"");
        }
        if features.is_empty() {
            dep.push_str(" }}\n");
        } else {
            dep.push_str(&format!(", features = [{}] }}\n", features.join(", ")));
        }
        toml.push_str(&dep);
    } else {
        let mut dep = format!(
            "clove-lang = {{ path = \"{}\", default-features = false",
            clove_path.display()
        );
        let features = vec!["\"ruby\"", "\"python\""];
        if features.is_empty() {
            dep.push_str(" }}\n");
        } else {
            dep.push_str(&format!(", features = [{}] }}\n", features.join(", ")));
        }
        toml.push_str(&dep);
    }
    toml.push_str(&format!(
        "clove-core = {{ path = \"{}\" }}\n",
        clove_core_path.display()
    ));
    toml.push_str("\n[workspace]\nmembers = []\n");
    fs::write(build_dir.join("Cargo.toml"), toml)
        .map_err(|e| format!("failed to write Cargo.toml: {}", e))
}

fn write_main_rs(
    build_dir: &Path,
    input: &Path,
    allow_native_plugins: bool,
    plugin_dirs: &[PathBuf],
) -> Result<(), String> {
    let src_path = fs::canonicalize(input)
        .map_err(|e| format!("failed to canonicalize {}: {}", input.display(), e))?;
    let src_literal = format!("{:?}", src_path.to_string_lossy());
    let allow_literal = if allow_native_plugins {
        "true"
    } else {
        "false"
    };
    let plugin_dirs_literal = if plugin_dirs.is_empty() {
        "&[]".to_string()
    } else {
        let parts = plugin_dirs
            .iter()
            .map(|p| format!("{:?}", p.to_string_lossy()))
            .collect::<Vec<_>>()
            .join(", ");
        format!("&[{}]", parts)
    };
    let main_path = build_dir.join("src").join("main.rs");
    let mut f = fs::File::create(&main_path)
        .map_err(|e| format!("failed to write {}: {}", main_path.display(), e))?;
    writeln!(
        f,
        r#"use std::sync::Arc;
use clove_core::error::format_error;

const SRC_PATH: &str = {src};
const SOURCE: &str = include_str!({src});
const DEFAULT_ALLOW_NATIVE_PLUGINS: bool = {allow};
const DEFAULT_PLUGIN_DIRS: &[&str] = {plugin_dirs};

fn print_usage(app: &str) {{
    println!("Usage: {{}} [--main|--no-main] [--allow-native-plugins] [--plugin-dir PATH] [--help] [args...]", app);
    println!("  --main       run -main after evaluating the script (default)");
    println!("  --no-main    evaluate only and print the last value");
    println!("  --allow-native-plugins  enable native plugin loading");
    println!("  --plugin-dir PATH       add plugin search path (repeatable)");
    println!("  --help, -h   show this help");
    println!("  args...      passed to -main when --main is used");
}}

fn parse_args() -> Result<(bool, bool, Vec<std::path::PathBuf>, Vec<String>), i32> {{
    let mut run_main = true;
    let mut allow_native_plugins = DEFAULT_ALLOW_NATIVE_PLUGINS;
    let mut plugin_dirs: Vec<std::path::PathBuf> = DEFAULT_PLUGIN_DIRS
        .iter()
        .map(|s| std::path::PathBuf::from(s))
        .collect();
    let mut script_args = Vec::new();
    let mut main_flag = false;
    let mut no_main_flag = false;
    let mut iter = std::env::args().skip(1).peekable();
    while let Some(arg) = iter.next() {{
        match arg.as_str() {{
            "--help" | "-h" => {{
                let app = std::env::args().next().unwrap_or_else(|| "app".to_string());
                print_usage(&app);
                std::process::exit(0);
            }}
            "--main" => {{
                run_main = true;
                main_flag = true;
            }}
            "--no-main" => {{
                run_main = false;
                no_main_flag = true;
            }}
            "--allow-native-plugins" => {{
                allow_native_plugins = true;
            }}
            "--plugin-dir" => {{
                if let Some(path) = iter.next() {{
                    plugin_dirs.push(std::path::PathBuf::from(path));
                }} else {{
                    eprintln!("--plugin-dir requires a value");
                    return Err(2);
                }}
            }}
            "--" => {{
                script_args.extend(iter);
                break;
            }}
            _ if arg.starts_with("--") => {{
                script_args.push(arg);
                script_args.extend(iter);
                break;
            }}
            _ => {{
                script_args.push(arg);
                script_args.extend(iter);
                break;
            }}
        }}
    }}
    if main_flag && no_main_flag {{
        eprintln!("cannot specify both --main and --no-main");
        return Err(2);
    }}
    Ok((run_main, allow_native_plugins, plugin_dirs, script_args))
}}

fn main() {{
    let (run_main, allow_native_plugins, plugin_dirs, script_args) = match parse_args() {{
        Ok(v) => v,
        Err(code) => std::process::exit(code),
    }};
    let mut opts = clove_core::options::EvalOptions::default();
    opts.source_name = Some(SRC_PATH.to_string());
    if let Some(dir) = std::path::Path::new(SRC_PATH).parent() {{
        opts.working_dir = Some(dir.to_path_buf());
    }}
    let lock_dir = opts
        .working_dir
        .clone()
        .or_else(|| std::env::current_dir().ok());
    if let Some(dir) = lock_dir.as_ref() {{
        match clove_lang::deps::load_project_lock_overrides(&dir) {{
            Ok(overrides) => opts.package_overrides = overrides,
            Err(err) => {{
                eprintln!("{{}}", err);
                std::process::exit(1);
            }}
        }}
    }}
    let auto_plugin_dirs = match lock_dir {{
        Some(dir) => match clove_lang::deps::load_project_plugin_dirs(&dir) {{
            Ok(dirs) => dirs,
            Err(err) => {{
                eprintln!("{{}}", err);
                std::process::exit(1);
            }}
        }},
        None => Vec::new(),
    }};
    let base_dir = std::env::current_dir().ok();
    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|path| path.parent().map(|dir| dir.to_path_buf()));
    clove_lang::set_native_plugin_config(clove_lang::NativePluginConfig {{
        allow: allow_native_plugins,
        plugin_dirs,
        auto_plugin_dirs,
        base_dir,
        exe_dir,
    }});
    let engines: Vec<Arc<dyn clove_core::foreign::ForeignEngine>> = clove_lang::default_engines();
    match clove_lang::run_source_with_lang_features(SOURCE, opts, &engines, run_main, &script_args) {{
        Ok(val) => println!("{{}}", val),
        Err(err) => {{
            for line in format_error(&err) {{
                eprintln!("{{}}", line);
            }}
            std::process::exit(1);
        }}
    }}
}}
"#,
        src = src_literal,
        allow = allow_literal,
        plugin_dirs = plugin_dirs_literal
    )
    .map_err(|e| format!("failed to write {}: {}", main_path.display(), e))
}

fn exe_name(base: &str) -> String {
    if cfg!(windows) {
        format!("{}.exe", base)
    } else {
        base.to_string()
    }
}

fn contains_foreign(source: &str) -> Result<bool, String> {
    let opts = ReaderOptions::language_defaults(vec![]);
    let mut reader = Reader::new_with_options(source, opts);

    let forms = reader.read_all().map_err(|e| e.to_string())?;
    Ok(forms.iter().any(form_has_foreign))
}

fn form_has_foreign(form: &Form) -> bool {
    match &form.kind {
        FormKind::ForeignBlock { .. }
        | FormKind::ForeignRaw { .. }
        | FormKind::ForeignSymbol { .. } => true,
        FormKind::List(items) | FormKind::Vector(items) | FormKind::Set(items) => {
            items.iter().any(form_has_foreign)
        }
        FormKind::Map(entries) => entries.iter().any(|entry| match entry {
            MapItem::KeyValue(k, v) => form_has_foreign(k) || form_has_foreign(v),
            MapItem::Spread(expr) => form_has_foreign(expr),
        }),
        _ => false,
    }
}

fn generate_pyo3_config(dir: &Path) -> Result<PathBuf, String> {
    let script = r#"
import struct
import sys
import sysconfig

lib = sysconfig.get_config_var("LDLIBRARY") or ""
libname = lib
for suffix in (".a", ".so", ".dylib", ".dll"):
    if libname.endswith(suffix):
        libname = libname[: -len(suffix)]
        break
if libname.startswith("lib"):
    libname = libname[3:]
if not libname:
    libname = f"python{sys.version_info[0]}.{sys.version_info[1]}"
libdir = sysconfig.get_config_var("LIBDIR") or sysconfig.get_config_var("LIBPL") or ""

cfg = {
    "implementation": "CPython",
    "version": f"{sys.version_info[0]}.{sys.version_info[1]}",
    "shared": "false",
    "abi3": "false",
    "lib_name": libname,
    "lib_dir": libdir,
    "executable": sys.executable,
    "pointer_width": str(struct.calcsize('P') * 8),
    "build_flags": "",
    "suppress_build_script_link_lines": "false",
}
for k, v in cfg.items():
    print(f"{k}={v}")
"#;
    let output = Command::new("python3")
        .arg("-c")
        .arg(script)
        .output()
        .map_err(|e| format!("failed to invoke python3: {}", e))?;
    if !output.status.success() {
        return Err("failed to gather Python config for embedding".into());
    }
    let content = String::from_utf8(output.stdout)
        .map_err(|e| format!("python output was not valid UTF-8: {}", e))?;
    let path = dir.join("pyo3-config.txt");
    fs::write(&path, content).map_err(|e| format!("failed to write {}: {}", path.display(), e))?;
    fs::canonicalize(&path).map_err(|e| format!("failed to canonicalize {}: {}", path.display(), e))
}
