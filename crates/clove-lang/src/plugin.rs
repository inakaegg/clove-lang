use std::ffi::CStr;
use std::path::{Path, PathBuf};

use libloading::{Library, Symbol};

pub fn run_plugin(args: Vec<String>) -> Result<(), String> {
    let mut args = args;
    let sub = args.first().map(|s| s.as_str()).unwrap_or("");
    match sub {
        "write-meta" => {
            args.remove(0);
            run_write_meta(args)
        }
        "-h" | "--help" | "" => {
            print_plugin_help();
            Ok(())
        }
        other => Err(format!("unknown plugin subcommand: {}", other)),
    }
}

fn print_plugin_help() {
    println!("Usage:");
    println!("  clove plugin write-meta --plugin <path> --out-dir <dir> [--name <id>]");
}

fn run_write_meta(args: Vec<String>) -> Result<(), String> {
    let mut plugin_path: Option<PathBuf> = None;
    let mut out_dir: Option<PathBuf> = None;
    let mut name: Option<String> = None;
    let mut idx = 0;
    while idx < args.len() {
        match args[idx].as_str() {
            "--plugin" => {
                if idx + 1 >= args.len() {
                    return Err("--plugin requires a value".into());
                }
                plugin_path = Some(PathBuf::from(&args[idx + 1]));
                idx += 2;
            }
            "--out-dir" | "--out" => {
                if idx + 1 >= args.len() {
                    return Err("--out-dir requires a value".into());
                }
                out_dir = Some(PathBuf::from(&args[idx + 1]));
                idx += 2;
            }
            "--name" => {
                if idx + 1 >= args.len() {
                    return Err("--name requires a value".into());
                }
                name = Some(args[idx + 1].clone());
                idx += 2;
            }
            "-h" | "--help" => {
                print_plugin_help();
                return Ok(());
            }
            other => return Err(format!("unknown argument: {}", other)),
        }
    }
    let plugin_path = plugin_path.ok_or_else(|| "--plugin is required".to_string())?;
    let out_dir = out_dir.ok_or_else(|| "--out-dir is required".to_string())?;
    let output = write_meta_from_plugin(&plugin_path, &out_dir, name.as_deref())?;
    println!("wrote {}", output.display());
    Ok(())
}

pub fn maybe_generate_plugin_meta(install_dir: &Path) -> Result<(), String> {
    let plugin_root = install_dir.join("plugins");
    if !plugin_root.is_dir() {
        return Ok(());
    }
    let platform_dir = plugin_root.join(plugin_platform_tag());
    let target_dir = if platform_dir.is_dir() {
        platform_dir
    } else {
        plugin_root
    };
    generate_meta_in_dir(&target_dir)
}

fn generate_meta_in_dir(dir: &Path) -> Result<(), String> {
    if !dir.is_dir() {
        return Ok(());
    }
    let entries = std::fs::read_dir(dir).map_err(|err| err.to_string())?;
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        if !is_dynamic_lib(&path) {
            continue;
        }
        let plugin_name = infer_plugin_name(&path)
            .ok_or_else(|| format!("failed to infer plugin name from {}", path.display()))?;
        let meta_path = dir.join(format!("{}.meta.json", plugin_name));
        if meta_path.exists() {
            continue;
        }
        if let Err(err) = write_meta_from_plugin(&path, dir, Some(&plugin_name)) {
            eprintln!("meta generation failed for {}: {}", path.display(), err);
        }
    }
    Ok(())
}

pub(crate) fn write_meta_from_plugin(
    plugin_path: &Path,
    out_dir: &Path,
    name: Option<&str>,
) -> Result<PathBuf, String> {
    let plugin_name = match name {
        Some(name) => name.to_string(),
        None => infer_plugin_name(plugin_path)
            .ok_or_else(|| "failed to infer plugin name; pass --name".to_string())?,
    };
    let meta_json = load_meta_json(plugin_path)?;
    std::fs::create_dir_all(out_dir).map_err(|err| err.to_string())?;
    let out_path = out_dir.join(format!("{}.meta.json", plugin_name));
    std::fs::write(&out_path, format!("{}\n", meta_json)).map_err(|err| err.to_string())?;
    Ok(out_path)
}

fn load_meta_json(plugin_path: &Path) -> Result<String, String> {
    unsafe {
        let lib = Library::new(plugin_path).map_err(|err| err.to_string())?;
        let symbol: Symbol<unsafe extern "C" fn() -> *const std::os::raw::c_char> = lib
            .get(b"clove_plugin_meta_json_v1")
            .map_err(|err| err.to_string())?;
        let ptr = symbol();
        if ptr.is_null() {
            return Err("clove_plugin_meta_json_v1 returned null".into());
        }
        let c_str = CStr::from_ptr(ptr);
        let json = c_str.to_string_lossy().into_owned();
        Ok(json)
    }
}

fn infer_plugin_name(path: &Path) -> Option<String> {
    let file_stem = path.file_stem()?.to_string_lossy();
    let mut stem = file_stem.to_string();
    if stem.starts_with("lib") {
        stem = stem.trim_start_matches("lib").to_string();
    }
    if let Some(stripped) = stem.strip_suffix("_plugin") {
        stem = stripped.to_string();
    } else if let Some(stripped) = stem.strip_suffix("-plugin") {
        stem = stripped.to_string();
    }
    let normalized = stem.replace('_', "-");
    if normalized.is_empty() {
        None
    } else {
        Some(normalized)
    }
}

fn is_dynamic_lib(path: &Path) -> bool {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("dylib") | Some("so") | Some("dll") => true,
        _ => false,
    }
}

fn plugin_platform_tag() -> String {
    format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}
