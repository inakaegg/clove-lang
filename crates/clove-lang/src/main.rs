use std::env;
use std::fs;
use std::io;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

mod build;
mod deps;
mod format;
mod pkg;

use clove_core::error::{format_error, ERROR_TAG};
use clove_core::guard;
use clove_core::options::{use_vm_from_env, vm_profiler_from_env, EvalOptions};
use clove_core::plugin_meta;
use clove_core::profiler;

use clove_lang::{create_runtime, default_engines, plugin, repl};
use once_cell::sync::Lazy;
use signal_hook::consts::signal::{SIGHUP, SIGINT, SIGTERM};
use signal_hook::flag;

static ALWAYS_TRUE_FLAG: Lazy<Arc<AtomicBool>> = Lazy::new(|| Arc::new(AtomicBool::new(true)));

fn help() -> ! {
    println!("Usage: clove [--repl] [--main] [--vm] [--vm-prof] [--no-std] [--auto-fallback] [--version] [-e CODE] [file]");
    println!("       clove build [OPTIONS] file");
    println!("       clove fmt [--stdin] [--indent N] [--width N] [--config PATH] [--no-config] [--print-config] [file]");
    println!("       clove pkg <subcommand>");
    println!("       clove plugin <subcommand>");
    println!();
    println!("Options:");
    println!("  --repl                Start the REPL, or enter it after running a script in the same context");
    println!("  --main                Call `-main` after eval if present (for script execution)");
    println!("  --vm                  Enable VM execution (fallback to eval for unsupported forms)");
    println!("  --vm-prof             Enable lightweight VM profiling (opcode/chunk stats)");
    println!("  --no-std              Disable auto-loading std (for perf measurement)");
    println!("  -e CODE               Evaluate CODE and exit");
    println!("  -a, --auto-fallback   Enable automatic fallback (default: off)");
    println!("  --allow-native-plugins Allow native plugin loading");
    println!("  --plugin-dir PATH     Add native plugin search directory (repeatable)");
    println!("  --mem-soft SIZE       Soft memory cap (e.g. 3G, 3072M)");
    println!("  --mem-hard SIZE       Hard memory cap (e.g. 4G, 4096M)");
    println!("  --mem-guard=off       Disable memory guard");
    println!("  build                 Build a standalone binary from a script");
    println!("  pkg                   Package management (install/list/update/uninstall)");
    println!("  plugin                Plugin helper commands (write-meta, etc.)");
    println!("  --version             Show version");
    println!("  -h, --help            Show this help");
    std::process::exit(0);
}

fn unknown_option(opt: &str) -> ! {
    eprintln!("unknown option: {}", opt);
    help();
}

fn parse_bytes_arg(raw: &str) -> Result<u64, String> {
    let raw = raw.trim();
    if raw.is_empty() {
        return Err("size must not be empty".to_string());
    }
    let (number_part, suffix) = match raw.chars().last() {
        Some(ch) if ch.is_ascii_alphabetic() => (&raw[..raw.len() - 1], Some(ch)),
        _ => (raw, None),
    };
    let number = number_part.trim().replace('_', "");
    let base = number
        .parse::<u64>()
        .map_err(|_| format!("invalid size: {}", raw))?;
    let multiplier = match suffix.map(|c| c.to_ascii_lowercase()) {
        None => 1,
        Some('k') => 1024,
        Some('m') => 1024 * 1024,
        Some('g') => 1024 * 1024 * 1024,
        Some(_) => return Err(format!("unknown size suffix in {}", raw)),
    };
    Ok(base.saturating_mul(multiplier))
}

fn configure_guard(mem_soft: Option<u64>, mem_hard: Option<u64>, enabled: Option<bool>) {
    let mut config = guard::default_config();
    if let Some(soft) = mem_soft {
        config.soft_bytes = soft;
    }
    if let Some(hard) = mem_hard {
        config.hard_bytes = hard;
    }
    if let Some(on) = enabled {
        config.enabled = on;
    }
    guard::configure(config);
}

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();

    if args.first().map(|s| s.as_str()) == Some("build") {
        args.remove(0);
        if let Err(e) = build::run_build(args) {
            eprintln!("{} {}", ERROR_TAG, e);
            std::process::exit(1);
        }
        return;
    }

    if args.first().map(|s| s.as_str()) == Some("fmt") {
        args.remove(0);
        if let Err(e) = format::run_fmt(args) {
            eprintln!("{}", e);
            std::process::exit(1);
        }
        return;
    }

    if args.first().map(|s| s.as_str()) == Some("pkg") {
        args.remove(0);
        if let Err(e) = pkg::run_pkg(args) {
            eprintln!("{} {}", ERROR_TAG, e);
            std::process::exit(1);
        }
        return;
    }

    if args.first().map(|s| s.as_str()) == Some("plugin") {
        args.remove(0);
        if let Err(e) = plugin::run_plugin(args) {
            eprintln!("{} {}", ERROR_TAG, e);
            std::process::exit(1);
        }
        return;
    }

    let mut source = None;
    let mut repl_after_run = false;
    let mut run_main = false;
    let mut use_vm = use_vm_from_env();
    let mut vm_profiler = vm_profiler_from_env();
    let mut no_std = false;
    let mut auto_fallback = false;
    let mut allow_native_plugins = false;
    let mut plugin_dirs: Vec<std::path::PathBuf> = Vec::new();
    let mut mem_soft: Option<u64> = None;
    let mut mem_hard: Option<u64> = None;
    let mut mem_guard_enabled: Option<bool> = None;

    loop {
        if args.first().map(|s| s.as_str()) == Some("-e") && args.len() >= 2 && source.is_none() {
            source = Some(args[1].clone());
            args.drain(0..2);
            continue;
        }
        match args.first().map(|s| s.as_str()) {
            Some("--repl") => {
                repl_after_run = true;
                args.remove(0);
            }
            Some("--main") => {
                run_main = true;
                args.remove(0);
            }
            Some("--vm") => {
                use_vm = true;
                args.remove(0);
            }
            Some("--vm-prof") => {
                vm_profiler = true;
                args.remove(0);
            }
            Some("--no-std") => {
                no_std = true;
                args.remove(0);
            }
            Some("-a") | Some("--auto-fallback") => {
            // Disabled by default
                auto_fallback = true;
                args.remove(0);
            }
            Some("--allow-native-plugins") => {
                allow_native_plugins = true;
                args.remove(0);
            }
            Some("--plugin-dir") => {
                if args.len() < 2 {
                    eprintln!("--plugin-dir requires a value");
                    help();
                }
                plugin_dirs.push(std::path::PathBuf::from(args[1].clone()));
                args.drain(0..2);
            }
            Some("--mem-soft") => {
                if args.len() < 2 {
                    eprintln!("--mem-soft requires a value");
                    help();
                }
                mem_soft = Some(parse_bytes_arg(&args[1]).unwrap_or_else(|err| {
                    eprintln!("{} --mem-soft: {}", ERROR_TAG, err);
                    std::process::exit(1);
                }));
                args.drain(0..2);
            }
            Some("--mem-hard") => {
                if args.len() < 2 {
                    eprintln!("--mem-hard requires a value");
                    help();
                }
                mem_hard = Some(parse_bytes_arg(&args[1]).unwrap_or_else(|err| {
                    eprintln!("{} --mem-hard: {}", ERROR_TAG, err);
                    std::process::exit(1);
                }));
                args.drain(0..2);
            }
            Some(s) if s.starts_with("--mem-guard=") => {
                let value = s.trim_start_matches("--mem-guard=");
                match value {
                    "off" => mem_guard_enabled = Some(false),
                    "on" => mem_guard_enabled = Some(true),
                    _ => {
                        eprintln!("{} --mem-guard must be on/off", ERROR_TAG);
                        std::process::exit(1);
                    }
                }
                args.remove(0);
            }
            Some("--version") => {
                let git_hash = option_env!("CLOVE_GIT_HASH").unwrap_or("unknown");
                println!("{} ({})", env!("CARGO_PKG_VERSION"), git_hash);
                return;
            }
            Some("-h") | Some("--help") => help(),
            Some(s) if s.starts_with('-') => unknown_option(s),
            _ => break,
        }
    }

    let repl_mode = source.is_none() && args.is_empty();
    configure_guard(mem_soft, mem_hard, mem_guard_enabled);
    install_signal_handlers(repl_mode || repl_after_run);
    repl::ensure_debug_repl_handler();
    let base_dir = std::env::current_dir().ok();
    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|path| path.parent().map(|p| p.to_path_buf()));

    if repl_mode {
        let mut opts = EvalOptions::default();
        opts.auto_fallback = auto_fallback;
        opts.use_vm = use_vm;
        opts.vm_profiler = vm_profiler;
        opts.no_std = no_std;
        if let Err(err) = apply_project_lock(&mut opts, None) {
            eprintln!("{} {}", ERROR_TAG, err);
            std::process::exit(1);
        }
        if let Err(err) = configure_native_plugins(
            allow_native_plugins,
            &plugin_dirs,
            None,
            base_dir.clone(),
            exe_dir.clone(),
        ) {
            eprintln!("{} {}", ERROR_TAG, err);
            std::process::exit(1);
        }
        let engines = default_engines();
        repl::interactive_repl(opts, &engines);
        return;
    }

    let mut opts = EvalOptions::default();
    opts.auto_fallback = auto_fallback;
    opts.use_vm = use_vm;
    opts.vm_profiler = vm_profiler;
    opts.no_std = no_std;

    if let Some(src) = source {
        opts.source_name = Some("<cmdline>".into());
        if let Err(err) = apply_project_lock(&mut opts, None) {
            eprintln!("{} {}", ERROR_TAG, err);
            std::process::exit(1);
        }
        if let Err(err) = configure_native_plugins(
            allow_native_plugins,
            &plugin_dirs,
            None,
            base_dir.clone(),
            exe_dir.clone(),
        ) {
            eprintln!("{} {}", ERROR_TAG, err);
            std::process::exit(1);
        }
        run_script(None, &src, opts, run_main, repl_after_run, Vec::new());
        return;
    }

    let file = args.first().unwrap_or_else(|| help());
    let script_path = std::path::Path::new(file);
    let absolute = script_path
        .canonicalize()
        .unwrap_or_else(|_| script_path.to_path_buf());
    opts.source_name = Some(absolute.to_string_lossy().into_owned());
    let script_args: Vec<String> = args.iter().skip(1).cloned().collect();
    let lock_dir = opts.working_dir.clone();
    if let Err(err) = apply_project_lock(&mut opts, lock_dir.as_deref()) {
        eprintln!("{} {}", ERROR_TAG, err);
        std::process::exit(1);
    }
    if let Err(err) = configure_native_plugins(
        allow_native_plugins,
        &plugin_dirs,
        lock_dir.as_deref(),
        base_dir.clone(),
        exe_dir.clone(),
    ) {
        eprintln!("{} {}", ERROR_TAG, err);
        std::process::exit(1);
    }

    let code = match fs::read_to_string(&absolute) {
        Ok(code) => code,
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                eprintln!("{} File not found: {}", ERROR_TAG, absolute.display());
                eprintln!("  Hint: check the path and file extension");
            } else {
                eprintln!("{} Failed to read the file", ERROR_TAG);
                eprintln!("  Target: {}", absolute.display());
                eprintln!("  Reason: {}", e);
            }
            std::process::exit(1);
        }
    };

    run_script(
        Some(absolute),
        &code,
        opts,
        run_main,
        repl_after_run,
        script_args,
    );
}

fn apply_project_lock(
    opts: &mut EvalOptions,
    start_dir: Option<&std::path::Path>,
) -> Result<(), String> {
    let base = match start_dir {
        Some(dir) => dir.to_path_buf(),
        None => std::env::current_dir().map_err(|err| err.to_string())?,
    };
    let overrides = deps::load_project_lock_overrides(&base)?;
    opts.package_overrides = overrides;
    Ok(())
}

fn configure_native_plugins(
    allow: bool,
    plugin_dirs: &[std::path::PathBuf],
    lock_dir: Option<&std::path::Path>,
    base_dir: Option<std::path::PathBuf>,
    exe_dir: Option<std::path::PathBuf>,
) -> Result<(), String> {
    let doc_roots = match lock_dir {
        Some(dir) => vec![dir.to_path_buf()],
        None => {
            let base = std::env::current_dir().map_err(|err| err.to_string())?;
            vec![base]
        }
    };
    let doc_dirs = clove_core::docs::package_doc_dirs_from_roots(&doc_roots);
    clove_core::docs::set_extra_doc_dirs(doc_dirs);
    let auto_plugin_dirs = match lock_dir {
        Some(dir) => deps::load_project_plugin_dirs(dir)?,
        None => {
            let base = std::env::current_dir().map_err(|err| err.to_string())?;
            deps::load_project_plugin_dirs(&base)?
        }
    };
    clove_lang::set_native_plugin_config(clove_lang::NativePluginConfig {
        allow,
        plugin_dirs: plugin_dirs.to_vec(),
        auto_plugin_dirs: auto_plugin_dirs.clone(),
        base_dir,
        exe_dir,
    });
    let mut meta_dirs = plugin_dirs.to_vec();
    meta_dirs.extend(auto_plugin_dirs);
    if !meta_dirs.is_empty() {
        if let Err(err) = plugin_meta::load_meta_from_dirs(&meta_dirs) {
            eprintln!("{} {}", ERROR_TAG, err);
        }
    }
    Ok(())
}

fn install_signal_handlers(repl_mode: bool) {
    // Script execution: exit immediately on SIGINT/SIGTERM/SIGHUP.
    // REPL: only connect SIGINT to the interrupt flag when repl_guard is enabled.
    if repl_mode {
        #[cfg(feature = "repl_guard")]
        {
            if let Err(err) = flag::register(SIGINT, clove_core::interrupt::interrupt_flag()) {
                eprintln!(
                    "{} failed to register signal handler ({}): {}",
                    ERROR_TAG, SIGINT, err
                );
            }
        }
        for sig in [SIGTERM, SIGHUP] {
            if let Err(err) = flag::register_conditional_shutdown(sig, 1, ALWAYS_TRUE_FLAG.clone())
            {
                eprintln!(
                    "{} failed to register signal handler ({}): {}",
                    ERROR_TAG, sig, err
                );
            }
        }
    } else {
        for sig in [SIGINT, SIGTERM, SIGHUP] {
            if let Err(err) = flag::register_conditional_shutdown(sig, 1, ALWAYS_TRUE_FLAG.clone())
            {
                eprintln!(
                    "{} failed to register signal handler ({}): {}",
                    ERROR_TAG, sig, err
                );
            }
        }
    }
}

fn run_script(
    file_path: Option<std::path::PathBuf>,
    code: &str,
    opts: EvalOptions,
    run_main: bool,
    repl_after_run: bool,
    script_args: Vec<String>,
) {
    let engines = default_engines();
    let opts_source_name = opts.source_name.clone();
    let ctx = create_runtime(opts, &engines);
    let capture_name = file_path
        .as_ref()
        .map(|path| path.to_string_lossy().into_owned())
        .or(opts_source_name);
    ctx.set_top_level_err_fin_capture(capture_name);
    // create_runtime already applies lang extras idempotently
    let before_names = ctx.namespace_names();

    let eval_result = if let Some(path) = file_path.as_ref() {
        ctx.eval_file(path)
    } else {
        ctx.eval_source(code)
    };

    let value = match eval_result {
        Ok(v) => v,
        Err(err) => {
            for line in format_error(&err) {
                eprintln!("{}", line);
            }
            std::process::exit(1);
        }
    };

    let after_names = ctx.namespace_names();
    let mut default_ns = file_path
        .as_ref()
        .and_then(|path| ctx.namespace_for_path(path));
    if default_ns.is_none() {
        default_ns = clove_lang::pick_new_namespace(&before_names, &after_names);
    }
    if let Some(ns) = default_ns {
        ctx.set_default_namespace_name(ns);
    }

    let main_result = if run_main {
        match clove_lang::call_main(&ctx, &script_args) {
            Ok(v) => Some(v),
            Err(err) => {
                for line in format_error(&err) {
                    eprintln!("{}", line);
                }
                std::process::exit(1);
            }
        }
    } else {
        None
    };

    if repl_after_run {
        if let Some(path) = file_path.as_ref() {
            ctx.set_repl_data_source(Some(path.clone()));
        }
        repl::interactive_repl_with_context(ctx);
        return;
    }

    if let Some(result) = main_result {
        println!("{}", result);
    } else {
        println!("{}", value);
    }
    profiler::print_report_if_enabled();
    clove_core::print_vm_profile_if_enabled();
}
