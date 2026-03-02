use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use clove_build_backend_c::emit_c;
use clove_build_front::{parse_source, SourceFile};
use clove_build_runtime_c::RuntimeConfig;

#[derive(Debug)]
struct BuildOptions {
    input: PathBuf,
    output: PathBuf,
    emit_c: bool,
}

pub fn run_build(args: Vec<String>) -> Result<(), String> {
    let opts = parse_args(args)?;
    let src = read_source(&opts.input)?;
    let parsed = parse_source(&src).map_err(|err| err.to_string())?;
    let config = RuntimeConfig::default();
    let artifact = emit_c(&parsed, &config).map_err(|err| err.to_string())?;

    let out_bin = opts.output.clone();
    let out_c = out_bin.with_extension("c");
    if let Some(parent) = out_c.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {}", parent.display(), err))?;
    }
    fs::write(&out_c, artifact.source)
        .map_err(|err| format!("failed to write {}: {}", out_c.display(), err))?;
    compile_c_to_bin(&out_c, &out_bin)?;

    if opts.emit_c {
        println!("{}", out_c.display());
    } else {
        println!("{}", out_bin.display());
    }
    Ok(())
}

fn parse_args(args: Vec<String>) -> Result<BuildOptions, String> {
    let mut emit_c = false;
    let mut output: Option<PathBuf> = None;
    let mut input: Option<PathBuf> = None;
    let mut iter = args.into_iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--emit-c" => emit_c = true,
            "--out" | "--output" | "-o" => {
                let Some(path) = iter.next() else {
                    return Err(format!("{} requires a value", arg));
                };
                output = Some(PathBuf::from(path));
            }
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }
            "--static"
            | "--embed-ruby"
            | "--embed-python"
            | "--strict-types"
            | "--emit-typed-ir"
            | "--opt"
            | "--allow-native-plugins"
            | "--plugin-dir" => {
                return Err(format!(
                    "{} is no longer supported in `clove build` (C backend only)",
                    arg
                ));
            }
            opt if opt.starts_with("--opt=")
                || opt.starts_with("--emit-typed-ir=")
                || opt.starts_with("--plugin-dir=") =>
            {
                return Err(format!(
                    "{} is no longer supported in `clove build` (C backend only)",
                    opt
                ));
            }
            other if other.starts_with('-') => return Err(format!("unknown option: {}", other)),
            other => {
                if input.is_some() {
                    return Err("multiple input files are not supported".to_string());
                }
                input = Some(PathBuf::from(other));
            }
        }
    }

    let input = input.ok_or_else(|| "no input file specified".to_string())?;
    if !input.exists() {
        return Err(format!("input file not found: {}", input.display()));
    }
    let output = output.unwrap_or_else(|| default_output_path(&input));
    Ok(BuildOptions {
        input,
        output,
        emit_c,
    })
}

fn print_help() {
    println!("Usage: clove build [OPTIONS] file");
    println!();
    println!("Options:");
    println!("  --emit-c            Print generated C file path instead of binary path");
    println!("  --out PATH          Output binary path (default: target/clove/bin/<file-stem>)");
    println!("  -o, --output PATH   Alias of --out");
}

fn default_output_path(input: &Path) -> PathBuf {
    let stem = input.file_stem().and_then(OsStr::to_str).unwrap_or("a");
    PathBuf::from("target").join("clove").join("bin").join(stem)
}

fn read_source(path: &Path) -> Result<SourceFile, String> {
    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    Ok(SourceFile {
        path: path.to_path_buf(),
        text,
    })
}

fn compile_c_to_bin(out_c: &Path, out_bin: &Path) -> Result<(), String> {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    if let Some(parent) = out_bin.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {}", parent.display(), err))?;
    }
    let status = Command::new(&cc)
        .arg("-O3")
        .arg(out_c)
        .arg("-o")
        .arg(out_bin)
        .status()
        .map_err(|err| format!("failed to run {}: {}", cc, err))?;
    if !status.success() {
        return Err(format!("{} failed with status {}", cc, status));
    }
    Ok(())
}
