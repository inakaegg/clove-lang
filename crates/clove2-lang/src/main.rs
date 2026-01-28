use std::env;
use std::fs;
use std::hash::{Hash, Hasher};
use std::process;

use std::path::{Path, PathBuf};

use clove2_core::codegen::emit_rust_program;
use clove2_core::dynamic_detect::contains_dynamic;
use clove2_core::eval::run_program_with_mut_mode;
use clove2_core::json_infer::{
    infer_json_schema, read_snapshot_type, validate_json_value, write_snapshot,
};
use clove2_core::reader::read_all;
use clove2_core::syntax::parse_forms;
use clove2_core::type_infer::{check_program, Diagnostic, DiagnosticLevel};
use clove2_core::use_directive::parse_use_directives;
use clove2_core::use_directive::Mode;
use clove2_core::use_directive::NativeLevel;
use clove2_core::vm::{profiler as vm_profiler, run_program_vm};

fn print_usage() {
    eprintln!("Usage:");
    eprintln!("  clove2 infer-json <path>");
    eprintln!("  clove2 validate-json <snapshot> <path>");
    eprintln!("  clove2 read <path>");
    eprintln!("  clove2 parse <path>");
    eprintln!("  clove2 analyze <path>");
    eprintln!("  clove2 check <path> [--native=strict|warn|allow] [--mode=native|dynamic] [--mut=hard|soft]");
    eprintln!(
        "  clove2 run <path> [--emit-rust] [--vm-prof] [--native=strict|warn] [--mut=hard|soft]"
    );
    eprintln!(
        "  clove2 build <path> [--out <path>] [--emit-rust] [--native=strict] [--mut=hard|soft]"
    );
}

fn main() {
    let mut args = env::args();
    let _bin = args.next();
    let Some(cmd) = args.next() else {
        print_usage();
        process::exit(1);
    };

    match cmd.as_str() {
        "infer-json" => {
            let Some(path) = args.next() else {
                eprintln!("infer-json requires a file path");
                process::exit(1);
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let value: serde_json::Value = match serde_json::from_str(&content) {
                Ok(value) => value,
                Err(err) => {
                    eprintln!("failed to parse json {}: {}", path, err);
                    process::exit(1);
                }
            };
            let inferred = infer_json_schema(&value);
            let ty = inferred.ty.clone();
            let snapshot_path = match write_snapshot(Path::new(&path), &content, &inferred) {
                Ok(path) => path,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            println!("{}", ty);
            println!("{}", snapshot_path.display());
        }
        "validate-json" => {
            let Some(snapshot) = args.next() else {
                eprintln!("validate-json requires a snapshot path");
                process::exit(1);
            };
            let Some(path) = args.next() else {
                eprintln!("validate-json requires a json file path");
                process::exit(1);
            };
            let snapshot_path = PathBuf::from(&snapshot);
            let ty = match read_snapshot_type(&snapshot_path) {
                Ok(ty) => ty,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let value: serde_json::Value = match serde_json::from_str(&content) {
                Ok(value) => value,
                Err(err) => {
                    eprintln!("failed to parse json {}: {}", path, err);
                    process::exit(1);
                }
            };
            if validate_json_value(&value, &ty) {
                println!("ok");
            } else {
                eprintln!("validation failed: {}", ty);
                process::exit(2);
            }
        }
        "read" => {
            let Some(path) = args.next() else {
                eprintln!("read requires a file path");
                process::exit(1);
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let forms = match read_all(&content) {
                Ok(forms) => forms,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            println!("{:#?}", forms);
        }
        "parse" => {
            let Some(path) = args.next() else {
                eprintln!("parse requires a file path");
                process::exit(1);
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let forms = match read_all(&content) {
                Ok(forms) => forms,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let parsed = match parse_forms(&forms) {
                Ok(parsed) => parsed,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            println!("{:#?}", parsed);
        }
        "analyze" => {
            let Some(path) = args.next() else {
                eprintln!("analyze requires a file path");
                process::exit(1);
            };
            let overrides = match parse_cli_overrides(&mut args, false) {
                Ok((overrides, _)) => overrides,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let forms = match read_all(&content) {
                Ok(forms) => forms,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let (use_cfg, consumed) = match parse_use_directives(&forms) {
                Ok(result) => result,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let mut use_cfg = use_cfg;
            apply_overrides(&mut use_cfg, overrides);
            let dynamic = contains_dynamic(&forms);
            println!("use: {:?}", use_cfg);
            println!("use_forms: {}", consumed);
            println!("dynamic: {}", dynamic);
        }
        "check" => {
            let Some(path) = args.next() else {
                eprintln!("check requires a file path");
                process::exit(1);
            };
            let overrides = match parse_cli_overrides(&mut args, false) {
                Ok((overrides, _)) => overrides,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let forms = match read_all(&content) {
                Ok(forms) => forms,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let (use_cfg, _) = match parse_use_directives(&forms) {
                Ok(result) => result,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let mut use_cfg = use_cfg;
            apply_overrides(&mut use_cfg, overrides);
            let dynamic = contains_dynamic(&forms);
            let level = use_cfg.native_level.unwrap_or(NativeLevel::Strict);
            let parsed = match parse_forms(&forms) {
                Ok(parsed) => parsed,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let diags = check_program(&parsed, level);
            let mut has_error = false;
            let path_buf = PathBuf::from(&path);
            if matches!(use_cfg.mode, Some(clove2_core::use_directive::Mode::Native)) && dynamic {
                println!(
                    "{}",
                    format_message(
                        &path_buf,
                        "Error",
                        "dynamic features detected in native mode"
                    )
                );
                has_error = true;
            }
            if matches!(
                use_cfg.mode,
                Some(clove2_core::use_directive::Mode::Dynamic)
            ) && !dynamic
            {
                println!(
                    "{}",
                    format_message(
                        &path_buf,
                        "Warning",
                        "dynamic mode is set but no dynamic features detected"
                    )
                );
            }
            for diag in &diags {
                println!("{}", format_diagnostic(&path_buf, &content, diag));
                if diag.level == DiagnosticLevel::Error {
                    has_error = true;
                }
            }
            if has_error {
                process::exit(1);
            }
        }
        "run" => {
            let Some(path) = args.next() else {
                eprintln!("run requires a file path");
                process::exit(1);
            };
            let content = match fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("failed to read {}: {}", path, err);
                    process::exit(1);
                }
            };
            let (overrides, _) = match parse_cli_overrides(&mut args, false) {
                Ok((overrides, out)) => (overrides, out),
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let forms = match read_all(&content) {
                Ok(forms) => forms,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let (use_cfg, _) = match parse_use_directives(&forms) {
                Ok((use_cfg, consumed)) => (use_cfg, consumed),
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            let mut use_cfg = use_cfg;
            apply_overrides(&mut use_cfg, overrides);
            if use_cfg.mode.is_none() {
                use_cfg.mode = Some(Mode::Native);
            }
            if overrides.vm_prof {
                vm_profiler::set_enabled(true);
                vm_profiler::reset();
            }
            if let Err(err) = validate_run_config(Path::new(&path), &use_cfg) {
                eprintln!("{}", err);
                process::exit(1);
            }
            if matches!(use_cfg.mode, Some(Mode::Native)) && contains_dynamic(&forms) {
                eprintln!(
                    "{}",
                    format_message(
                        Path::new(&path),
                        "Error",
                        "dynamic features detected in native mode"
                    )
                );
                process::exit(1);
            }
            let parsed = match parse_forms(&forms) {
                Ok(parsed) => parsed,
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            if overrides.emit_rust {
                let rust_path = default_output_path(&path).with_extension("rs");
                if let Err(err) = write_rust_output(&rust_path, &parsed, use_cfg.default_mut_mode())
                {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            }
            match run_program_vm(&parsed, use_cfg.default_mut_mode()) {
                Ok(_) => {}
                Err(err) => {
                    let should_fallback = err
                        .message()
                        .map(|msg| msg.starts_with("vm:"))
                        .unwrap_or(false);
                    if should_fallback {
                        if let Err(err) =
                            run_program_with_mut_mode(&parsed, use_cfg.default_mut_mode())
                        {
                            eprintln!("{}", err);
                            process::exit(1);
                        }
                    } else {
                        eprintln!("{}", err);
                        process::exit(1);
                    }
                }
            }
            if overrides.vm_prof {
                if let Some(report) = vm_profiler::report() {
                    eprintln!("{}", report);
                }
            }
        }
        "build" => {
            let Some(path) = args.next() else {
                eprintln!("build requires a file path");
                process::exit(1);
            };
            let (overrides, out) = match parse_cli_overrides(&mut args, true) {
                Ok((overrides, out)) => (overrides, out),
                Err(err) => {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            };
            if let Err(err) = build_native(
                &path,
                out.as_deref(),
                overrides.emit_rust,
                overrides.mut_hard,
                overrides.native_level,
                overrides.mode,
            ) {
                eprintln!("{}", err);
                process::exit(1);
            }
        }
        "-h" | "--help" => {
            print_usage();
        }
        _ => {
            eprintln!("unknown command: {}", cmd);
            print_usage();
            process::exit(1);
        }
    }
}

fn build_native(
    path: &str,
    out: Option<&str>,
    emit_rust: bool,
    mut_hard: Option<bool>,
    native_level: Option<NativeLevel>,
    mode_override: Option<Mode>,
) -> Result<(), String> {
    let input_path = fs::canonicalize(path).map_err(|err| err.to_string())?;
    let content = fs::read_to_string(&input_path).map_err(|err| err.to_string())?;
    let forms = read_all(&content).map_err(|err| err.to_string())?;
    let (use_cfg, _) = parse_use_directives(&forms).map_err(|err| err.to_string())?;
    let mut use_cfg = use_cfg;
    if let Some(value) = mut_hard {
        use_cfg.mut_hard = Some(value);
    }
    if let Some(value) = native_level {
        use_cfg.native_level = Some(value);
    }
    if let Some(value) = mode_override {
        use_cfg.mode = Some(value);
    }
    if use_cfg.mode.is_none() {
        use_cfg.mode = Some(Mode::Native);
    }
    if matches!(use_cfg.mode, Some(Mode::Dynamic)) {
        return Err(format_message(
            &input_path,
            "Error",
            "build does not support dynamic mode",
        ));
    }
    let dynamic = contains_dynamic(&forms);
    if matches!(use_cfg.mode, Some(clove2_core::use_directive::Mode::Native)) && dynamic {
        return Err(format_message(
            &input_path,
            "Error",
            "dynamic features detected in native mode",
        ));
    }
    let parsed = parse_forms(&forms).map_err(|err| err.to_string())?;
    let level = use_cfg.native_level.unwrap_or(NativeLevel::Strict);
    if level != NativeLevel::Strict {
        return Err(format_message(
            &input_path,
            "Error",
            "build only supports native=strict",
        ));
    }
    let diags = check_program(&parsed, level);
    let mut errors = Vec::new();
    for diag in &diags {
        if diag.level == DiagnosticLevel::Error {
            errors.push(format_diagnostic(&input_path, &content, diag));
        }
    }
    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    input_path.hash(&mut hasher);
    content.hash(&mut hasher);
    let hash = hasher.finish();
    let build_dir = PathBuf::from("target")
        .join("clove2")
        .join("build")
        .join(format!("{:x}", hash));
    let src_dir = build_dir.join("src");
    fs::create_dir_all(&src_dir).map_err(|err| err.to_string())?;

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let core_path = manifest_dir
        .join("..")
        .join("..")
        .join("crates")
        .join("clove2-core");
    let package_name = format!("clove2_build_{:x}", hash);
    let built_bin = build_dir.join("target").join("release").join(&package_name);
    let main_rs =
        emit_rust_program(&parsed, use_cfg.default_mut_mode()).map_err(|err| err.to_string())?;
    let core_hash = hash_core_sources(&core_path)?;
    let main_hash = hash_text(&main_rs);
    let meta_path = build_dir.join("clove2-build-meta");
    let mut needs_build = true;
    if built_bin.exists() {
        if let Ok(prev) = fs::read_to_string(&meta_path) {
            let expected = format!("{:x}:{:x}", core_hash, main_hash);
            if prev.trim() == expected {
                needs_build = false;
            }
        }
    }

    if needs_build {
        let cargo_toml = format!(
            "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nclove2-core = {{ path = \"{}\" }}\n\n[workspace]\n",
            package_name,
            core_path.display()
        );
        fs::write(build_dir.join("Cargo.toml"), cargo_toml).map_err(|err| err.to_string())?;

        fs::write(src_dir.join("main.rs"), main_rs).map_err(|err| err.to_string())?;

        let status = process::Command::new("cargo")
            .arg("build")
            .arg("--release")
            .current_dir(&build_dir)
            .status()
            .map_err(|err| err.to_string())?;
        if !status.success() {
            return Err("build failed".to_string());
        }

        fs::write(meta_path, format!("{:x}:{:x}", core_hash, main_hash))
            .map_err(|err| err.to_string())?;
    }
    let output_path = match out {
        Some(out) => PathBuf::from(out),
        None => default_output_path(path),
    };
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).map_err(|err| err.to_string())?;
    }
    fs::copy(&built_bin, &output_path).map_err(|err| err.to_string())?;
    if emit_rust {
        let rust_path = output_path.with_extension("rs");
        write_rust_output(&rust_path, &parsed, use_cfg.default_mut_mode())?;
    }
    println!("{}", output_path.display());
    Ok(())
}

fn validate_run_config(
    path: &Path,
    use_cfg: &clove2_core::use_directive::UseConfig,
) -> Result<(), String> {
    let mode = use_cfg.mode.unwrap_or(Mode::Native);
    let level = use_cfg.native_level.unwrap_or(NativeLevel::Strict);
    if mode != Mode::Native {
        return Err(format_message(
            path,
            "Error",
            "run does not support dynamic mode (use repl)",
        ));
    }
    if level == NativeLevel::Allow {
        return Err(format_message(
            path,
            "Error",
            "run does not support native=allow",
        ));
    }
    Ok(())
}

fn parse_mut_hard_flag(arg: &str) -> Option<bool> {
    let value = arg.strip_prefix("--mut=")?;
    match value {
        "hard" => Some(true),
        "soft" => Some(false),
        _ => None,
    }
}

fn parse_native_level_flag(arg: &str) -> Option<NativeLevel> {
    let value = arg.strip_prefix("--native=")?;
    match value {
        "strict" => Some(NativeLevel::Strict),
        "warn" => Some(NativeLevel::Warn),
        "allow" => Some(NativeLevel::Allow),
        _ => None,
    }
}

fn parse_mode_flag(arg: &str) -> Option<Mode> {
    let value = arg.strip_prefix("--mode=")?;
    match value {
        "native" => Some(Mode::Native),
        "dynamic" => Some(Mode::Dynamic),
        _ => None,
    }
}

#[derive(Default, Clone, Copy)]
struct CliOverrides {
    mut_hard: Option<bool>,
    native_level: Option<NativeLevel>,
    mode: Option<Mode>,
    emit_rust: bool,
    vm_prof: bool,
}

fn apply_overrides(cfg: &mut clove2_core::use_directive::UseConfig, overrides: CliOverrides) {
    if let Some(value) = overrides.mut_hard {
        cfg.mut_hard = Some(value);
    }
    if let Some(value) = overrides.native_level {
        cfg.native_level = Some(value);
    }
    if let Some(value) = overrides.mode {
        cfg.mode = Some(value);
    }
}

fn parse_cli_overrides(
    args: &mut env::Args,
    allow_out: bool,
) -> Result<(CliOverrides, Option<String>), String> {
    let mut overrides = CliOverrides::default();
    let mut out = None;
    while let Some(arg) = args.next() {
        if allow_out && arg == "--out" {
            out = args.next();
            if out.is_none() {
                return Err("missing value for --out".to_string());
            }
            continue;
        }
        if let Some(value) = parse_mut_hard_flag(&arg) {
            overrides.mut_hard = Some(value);
            continue;
        }
        if let Some(value) = parse_native_level_flag(&arg) {
            overrides.native_level = Some(value);
            continue;
        }
        if arg.starts_with("--native=") {
            return Err(format!("unknown native level: {}", arg));
        }
        if let Some(value) = parse_mode_flag(&arg) {
            overrides.mode = Some(value);
            continue;
        }
        if arg.starts_with("--mode=") {
            return Err(format!("unknown mode: {}", arg));
        }
        if arg == "--emit-rust" {
            overrides.emit_rust = true;
            continue;
        }
        if arg == "--vm-prof" {
            overrides.vm_prof = true;
            continue;
        }
        return Err(format!("unknown flag: {}", arg));
    }
    Ok((overrides, out))
}

fn default_output_path(path: &str) -> PathBuf {
    let stem = Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("clove2_out");
    PathBuf::from("target")
        .join("clove2")
        .join("bin")
        .join(stem)
}

fn write_rust_output(
    output_path: &Path,
    parsed: &[clove2_core::syntax::TopLevel],
    default_mut_mode: clove2_core::use_directive::MutMode,
) -> Result<(), String> {
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).map_err(|err| err.to_string())?;
    }
    let main_rs = emit_rust_program(parsed, default_mut_mode).map_err(|err| err.to_string())?;
    fs::write(output_path, main_rs).map_err(|err| err.to_string())
}

fn format_diagnostic(path: &Path, content: &str, diag: &Diagnostic) -> String {
    let level = match diag.level {
        DiagnosticLevel::Error => "Error",
        DiagnosticLevel::Warning => "Warning",
    };
    if let Some(span) = diag.span.as_ref() {
        let (line, col) = position_at(content, span.start);
        format!(
            "{}:{}:{}: {}: {}",
            path.display(),
            line,
            col,
            level,
            diag.message
        )
    } else {
        format!("{}: {}: {}", path.display(), level, diag.message)
    }
}

fn format_message(path: &Path, level: &str, message: &str) -> String {
    format!("{}: {}: {}", path.display(), level, message)
}

fn position_at(text: &str, idx: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    let mut count = 0;
    for ch in text.chars() {
        if count >= idx {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        count += 1;
    }
    (line, col)
}

fn hash_core_sources(core_path: &Path) -> Result<u64, String> {
    let mut files = Vec::new();
    collect_rs_files(&core_path.join("src"), &mut files)?;
    files.push(core_path.join("Cargo.toml"));
    files.sort_by(|a, b| a.to_string_lossy().cmp(&b.to_string_lossy()));

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for path in files {
        path.to_string_lossy().hash(&mut hasher);
        let data = fs::read(&path).map_err(|err| err.to_string())?;
        data.hash(&mut hasher);
    }
    Ok(hasher.finish())
}

fn hash_text(text: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = fs::read_dir(dir).map_err(|err| err.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            out.push(path);
        }
    }
    Ok(())
}
