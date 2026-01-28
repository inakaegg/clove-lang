use crate::ast::Vector;
use crate::ast::{FnArity, Key, Value};
use crate::builtins::{def_builtin, err, expect_path_buf, map_like_to_hashmap, type_mismatch_arg};
use crate::env::Env;
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::runtime::{runtime_context_required, RuntimeCtx};
use crate::seq::{SeqEngine, SeqHandle};
use crate::types::TypeKind;
use std::fs;
use std::path::{Component, Path, PathBuf};

pub(crate) fn install(env: &mut Env) {
    def_builtin!(
        env,
        "fs::file-exists?",
        FnArity::exact(1),
        |args| match args {
            [path] => path_op("file-exists?", 1, path, |p| Ok(Value::Bool(p.exists()))),
            _ => err("file-exists? expects path"),
        }
    );
    define_alias(env, "file-exists?", "fs::file-exists?");
    define_alias(env, "io::file-exists?", "fs::file-exists?");

    def_builtin!(env, "fs::file?", FnArity::exact(1), |args| match args {
        [path] => path_op("file?", 1, path, |p| Ok(Value::Bool(p.is_file()))),
        _ => err("file? expects path"),
    });
    define_alias(env, "file?", "fs::file?");
    define_alias(env, "io::file?", "fs::file?");

    def_builtin!(env, "fs::dir?", FnArity::exact(1), |args| match args {
        [path] => path_op("dir?", 1, path, |p| Ok(Value::Bool(p.is_dir()))),
        _ => err("dir? expects path"),
    });
    define_alias(env, "dir?", "fs::dir?");
    define_alias(env, "io::dir?", "fs::dir?");

    def_builtin!(env, "fs::list-dir", FnArity::exact(1), |args| match args {
        [path] => list_dir(path),
        _ => err("list-dir expects path"),
    });
    define_alias(env, "list-dir", "fs::list-dir");
    define_alias(env, "io::list-dir", "fs::list-dir");

    def_builtin!(env, "fs::mkdir", FnArity::exact(1), |args| match args {
        [path] => {
            let path_buf = expect_path_buf(path, "mkdir", 1)?;
            fs::create_dir(&path_buf).map_err(|e| CloveError::runtime(e.to_string()))?;
            Ok(path.clone())
        }
        _ => err("mkdir expects path"),
    });
    define_alias(env, "mkdir", "fs::mkdir");
    define_alias(env, "io::mkdir", "fs::mkdir");

    def_builtin!(env, "fs::mkdirs", FnArity::exact(1), |args| match args {
        [path] => {
            let path_buf = expect_path_buf(path, "mkdirs", 1)?;
            fs::create_dir_all(&path_buf).map_err(|e| CloveError::runtime(e.to_string()))?;
            Ok(path.clone())
        }
        _ => err("mkdirs expects path"),
    });
    define_alias(env, "mkdirs", "fs::mkdirs");
    define_alias(env, "io::mkdirs", "fs::mkdirs");

    def_builtin!(env, "fs::delete", FnArity::exact(1), |args| match args {
        [path] => {
            let path_buf = expect_path_buf(path, "delete", 1)?;
            if path_buf.is_dir() {
                fs::remove_dir_all(&path_buf).map_err(|e| CloveError::runtime(e.to_string()))?;
            } else if path_buf.exists() {
                fs::remove_file(&path_buf).map_err(|e| CloveError::runtime(e.to_string()))?;
            }
            Ok(path.clone())
        }
        _ => err("delete expects path"),
    });
    define_alias(env, "delete", "fs::delete");
    define_alias(env, "io::delete", "fs::delete");

    def_builtin!(env, "fs::copy", FnArity::exact(2), |args| match args {
        [from, to] => copy_file(from, to),
        _ => err("copy expects from and to"),
    });
    define_alias(env, "copy", "fs::copy");
    define_alias(env, "io::copy", "fs::copy");

    def_builtin!(env, "fs::move", FnArity::exact(2), |args| match args {
        [from, to] => move_file(from, to),
        _ => err("move expects from and to"),
    });
    define_alias(env, "move", "fs::move");
    define_alias(env, "io::move", "fs::move");

    def_builtin!(env, "fs::glob", FnArity::range(1, 2), |args| match args {
        [pattern] => glob_eager(pattern, None),
        [pattern, opts] => glob_eager(pattern, Some(opts)),
        _ => err("glob expects pattern and optional opts map"),
    });
    define_alias(env, "glob", "fs::glob");
    define_alias(env, "io::glob", "fs::glob");

    def_builtin!(env, "fs::glob*", FnArity::range(1, 2), |args| match args {
        [pattern] => glob_lazy(pattern, None),
        [pattern, opts] => glob_lazy(pattern, Some(opts)),
        _ => err("glob* expects pattern and optional opts map"),
    });
    define_alias(env, "glob*", "fs::glob*");
    define_alias(env, "io::glob*", "fs::glob*");

    install_fn_meta();
}

pub(crate) fn glob_paths(pattern: &str) -> Result<Vec<String>, CloveError> {
    let value = Value::String(pattern.to_string());
    let (glob, opts) = parse_glob_args("glob*", &value, None, false)?;
    collect_glob(&glob, &opts)
}

pub(crate) fn glob_paths_eager(pattern: &str) -> Result<Vec<String>, CloveError> {
    let value = Value::String(pattern.to_string());
    let (glob, opts) = parse_glob_args("glob", &value, None, true)?;
    let mut items = collect_glob(&glob, &opts)?;
    if opts.sorted {
        items.sort();
    }
    Ok(items)
}

fn define_alias(env: &mut Env, alias: &str, target: &str) {
    if let Some(value) = env.get(target) {
        env.define_builtin(alias, value);
    }
}

fn install_fn_meta() {
    fn named(name: &str) -> TypeKind {
        TypeKind::named(name.to_string())
    }
    fn path_ty() -> TypeKind {
        TypeKind::union(vec![TypeKind::Str, named("core::Symbol")])
    }
    fn overload(args: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> FnOverload {
        FnOverload {
            arg_types: args,
            rest,
            ret_type: ret,
            special_op: None,
        }
    }
    fn register(name: &str, arglist: &[&str], doc: &str, overloads: Vec<FnOverload>) {
        let (ns, local) = match name.split_once("::") {
            Some((ns, local)) => (ns, local),
            None => ("core", name),
        };
        let mut meta = FnMeta::new(ns, local);
        for arg in arglist {
            meta.arglist.push((*arg).into());
        }
        meta.doc = Some(doc.to_string());
        meta.overloads = overloads;
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }
    fn register_aliases(
        canonical: &str,
        aliases: &[&str],
        arglist: &[&str],
        doc: &str,
        overloads: Vec<FnOverload>,
    ) {
        register(canonical, arglist, doc, overloads.clone());
        for alias in aliases {
            register(alias, arglist, doc, overloads.clone());
        }
    }

    register_aliases(
        "fs::file-exists?",
        &["file-exists?", "io::file-exists?", "std::file-exists?"],
        &["[path]"],
        "Return true if the path exists.",
        vec![overload(vec![path_ty()], None, TypeKind::Bool)],
    );
    register_aliases(
        "fs::file?",
        &["file?", "io::file?", "std::file?"],
        &["[path]"],
        "Return true if the path exists and is a regular file.",
        vec![overload(vec![path_ty()], None, TypeKind::Bool)],
    );
    register_aliases(
        "fs::dir?",
        &["dir?", "io::dir?", "std::dir?"],
        &["[path]"],
        "Return true if the path exists and is a directory.",
        vec![overload(vec![path_ty()], None, TypeKind::Bool)],
    );
    register_aliases(
        "fs::list-dir",
        &["list-dir", "io::list-dir", "std::list-dir"],
        &["[path]"],
        "Return a vector of entry names in the directory at path.",
        vec![overload(
            vec![path_ty()],
            None,
            TypeKind::vector(TypeKind::Str),
        )],
    );
    register_aliases(
        "fs::mkdir",
        &["mkdir", "io::mkdir", "std::mkdir"],
        &["[path]"],
        "Create the directory at path (non-recursive). Returns path.",
        vec![overload(vec![path_ty()], None, path_ty())],
    );
    register_aliases(
        "fs::mkdirs",
        &["mkdirs", "io::mkdirs", "std::mkdirs"],
        &["[path]"],
        "Create the directory path and parents. Returns path.",
        vec![overload(vec![path_ty()], None, path_ty())],
    );
    register_aliases(
        "fs::delete",
        &["delete", "io::delete", "std::delete"],
        &["[path]"],
        "Delete the file or directory at path (directories removed recursively). Returns path.",
        vec![overload(vec![path_ty()], None, path_ty())],
    );
    register_aliases(
        "fs::copy",
        &["copy", "io::copy", "std::copy"],
        &["[from to]"],
        "Copy the file from from to to. Returns destination path.",
        vec![overload(vec![path_ty(), path_ty()], None, path_ty())],
    );
    register_aliases(
        "fs::move",
        &["move", "io::move", "std::move"],
        &["[from to]"],
        "Move or rename the file from from to to. Returns destination path.",
        vec![overload(vec![path_ty(), path_ty()], None, path_ty())],
    );
    register_aliases(
        "fs::glob",
        &["glob", "io::glob", "std::glob"],
        &["[pattern]", "[pattern opts]"],
        "Return a vector of paths matching pattern. opts: {:cwd PATH :recursive? bool :dotfiles? bool :sorted? bool :only :file|:dir|:any :follow-symlinks? bool :on-error :skip|:throw}.",
        vec![
            overload(vec![path_ty()], None, TypeKind::vector(TypeKind::Str)),
            overload(
                vec![path_ty(), TypeKind::map(TypeKind::Any, TypeKind::Any)],
                None,
                TypeKind::vector(TypeKind::Str),
            ),
        ],
    );
    register_aliases(
        "fs::glob*",
        &["glob*", "io::glob*", "std::glob*"],
        &["[pattern]", "[pattern opts]"],
        "Return a lazy seq of paths matching pattern. opts: {:cwd PATH :recursive? bool :dotfiles? bool :sorted? bool :only :file|:dir|:any :follow-symlinks? bool :on-error :skip|:throw}.",
        vec![
            overload(vec![path_ty()], None, named("core::Seq")),
            overload(
                vec![path_ty(), TypeKind::map(TypeKind::Any, TypeKind::Any)],
                None,
                named("core::Seq"),
            ),
        ],
    );
}

fn path_op<F>(op: &str, arg_index: usize, value: &Value, f: F) -> Result<Value, CloveError>
where
    F: Fn(&Path) -> Result<Value, CloveError>,
{
    let path = expect_path_buf(value, op, arg_index)?;
    f(&path)
}

fn list_dir(path: &Value) -> Result<Value, CloveError> {
    path_op("list-dir", 1, path, |p| {
        let entries = fs::read_dir(p).map_err(|e| CloveError::runtime(e.to_string()))?;
        let mut out = Vector::new();
        for entry in entries {
            let entry = entry.map_err(|e| CloveError::runtime(e.to_string()))?;
            let name = entry.file_name().to_string_lossy().to_string();
            out.push_back(Value::String(name));
        }
        Ok(Value::Vector(out))
    })
}

fn copy_file(from: &Value, to: &Value) -> Result<Value, CloveError> {
    let from_p = expect_path_buf(from, "copy", 1)?;
    let to_p = expect_path_buf(to, "copy", 2)?;
    fs::copy(&from_p, &to_p).map_err(|e| CloveError::runtime(e.to_string()))?;
    Ok(to.clone())
}

fn move_file(from: &Value, to: &Value) -> Result<Value, CloveError> {
    let from_p = expect_path_buf(from, "move", 1)?;
    let to_p = expect_path_buf(to, "move", 2)?;
    fs::rename(&from_p, &to_p).map_err(|e| CloveError::runtime(e.to_string()))?;
    Ok(to.clone())
}

#[derive(Clone, Copy)]
enum GlobOnError {
    Skip,
    Throw,
}

#[derive(Clone, Copy)]
enum GlobOnly {
    Any,
    File,
    Dir,
}

#[derive(Clone)]
struct GlobOptions {
    cwd: PathBuf,
    recursive: bool,
    dotfiles: bool,
    sorted: bool,
    only: GlobOnly,
    follow_symlinks: bool,
    on_error: GlobOnError,
}

#[derive(Clone)]
struct GlobPattern {
    root: PathBuf,
    segments: Vec<String>,
    output_relative: bool,
}

fn glob_eager(pattern: &Value, opts: Option<&Value>) -> Result<Value, CloveError> {
    let (glob, opts) = parse_glob_args("glob", pattern, opts, true)?;
    let mut items = collect_glob(&glob, &opts)?;
    if opts.sorted {
        items.sort();
    }
    let mut out = Vector::new();
    for item in items {
        out.push_back(Value::String(item));
    }
    Ok(Value::Vector(out))
}

fn glob_lazy(pattern: &Value, opts: Option<&Value>) -> Result<Value, CloveError> {
    let (glob, opts) = parse_glob_args("glob*", pattern, opts, false)?;
    if opts.sorted {
        let mut items = collect_glob(&glob, &opts)?;
        items.sort();
        let values = items.into_iter().map(Value::String).collect::<Vec<_>>();
        return Ok(Value::Seq(SeqHandle::from_iter(values)));
    }
    Ok(Value::Seq(SeqHandle::new(Box::new(GlobSeqEngine::new(
        glob, opts,
    )))))
}

fn parse_glob_args(
    op: &str,
    pattern: &Value,
    opts: Option<&Value>,
    sorted_default: bool,
) -> Result<(GlobPattern, GlobOptions), CloveError> {
    let pattern = match pattern {
        Value::String(s) | Value::Symbol(s) => s.clone(),
        other => return Err(type_mismatch_arg("path string or symbol", op, 1, other)),
    };
    let opts = parse_glob_opts(op, 2, opts, sorted_default)?;
    let (root, segments, is_absolute) = split_pattern(&pattern);
    let (root, output_relative) = if is_absolute {
        (root, false)
    } else {
        (opts.cwd.clone(), true)
    };
    Ok((
        GlobPattern {
            root,
            segments,
            output_relative,
        },
        opts,
    ))
}

fn parse_glob_opts(
    op: &str,
    arg_index: usize,
    opts_val: Option<&Value>,
    sorted_default: bool,
) -> Result<GlobOptions, CloveError> {
    let base_cwd = resolve_cwd()?;
    let mut opts = GlobOptions {
        cwd: base_cwd.clone(),
        recursive: true,
        dotfiles: false,
        sorted: sorted_default,
        only: GlobOnly::Any,
        follow_symlinks: false,
        on_error: GlobOnError::Skip,
    };
    let Some(opts_val) = opts_val else {
        return Ok(opts);
    };
    let map = map_like_to_hashmap(opts_val, op, arg_index)?;
    if let Some(v) = map.get(&Key::Keyword("cwd".into())) {
        let mut cwd = expect_path_buf(v, op, arg_index)?;
        if !cwd.is_absolute() {
            cwd = base_cwd.join(cwd);
        }
        opts.cwd = cwd;
    }
    if let Some(v) = map.get(&Key::Keyword("recursive?".into())) {
        opts.recursive = match v {
            Value::Bool(b) => *b,
            other => {
                return Err(type_mismatch_arg("bool", op, arg_index, other));
            }
        };
    }
    if let Some(v) = map.get(&Key::Keyword("dotfiles?".into())) {
        opts.dotfiles = match v {
            Value::Bool(b) => *b,
            other => {
                return Err(type_mismatch_arg("bool", op, arg_index, other));
            }
        };
    }
    if let Some(v) = map.get(&Key::Keyword("sorted?".into())) {
        opts.sorted = match v {
            Value::Bool(b) => *b,
            other => {
                return Err(type_mismatch_arg("bool", op, arg_index, other));
            }
        };
    }
    if let Some(v) = map.get(&Key::Keyword("only".into())) {
        opts.only = parse_glob_only(op, arg_index, v)?;
    }
    if let Some(v) = map.get(&Key::Keyword("follow-symlinks?".into())) {
        opts.follow_symlinks = match v {
            Value::Bool(b) => *b,
            other => {
                return Err(type_mismatch_arg("bool", op, arg_index, other));
            }
        };
    }
    if let Some(v) = map.get(&Key::Keyword("on-error".into())) {
        opts.on_error = parse_glob_on_error(op, arg_index, v)?;
    }
    Ok(opts)
}

fn parse_glob_only(op: &str, arg_index: usize, value: &Value) -> Result<GlobOnly, CloveError> {
    let raw = match value {
        Value::Symbol(s) | Value::String(s) => s.trim_start_matches(':'),
        other => return Err(type_mismatch_arg("keyword", op, arg_index, other)),
    };
    match raw {
        "file" => Ok(GlobOnly::File),
        "dir" => Ok(GlobOnly::Dir),
        "any" => Ok(GlobOnly::Any),
        _ => Err(CloveError::runtime(format!(
            "glob :only must be :file, :dir, or :any"
        ))),
    }
}

fn parse_glob_on_error(
    op: &str,
    arg_index: usize,
    value: &Value,
) -> Result<GlobOnError, CloveError> {
    let raw = match value {
        Value::Symbol(s) | Value::String(s) => s.trim_start_matches(':'),
        other => return Err(type_mismatch_arg("keyword", op, arg_index, other)),
    };
    match raw {
        "skip" => Ok(GlobOnError::Skip),
        "throw" => Ok(GlobOnError::Throw),
        _ => Err(CloveError::runtime(format!(
            "glob :on-error must be :skip or :throw"
        ))),
    }
}

fn resolve_cwd() -> Result<PathBuf, CloveError> {
    if let Some(result) = RuntimeCtx::try_with_current(|ctx| Ok(ctx.working_dir().to_path_buf())) {
        return result;
    }
    if runtime_context_required() {
        return Err(CloveError::runtime("runtime context is not available"));
    }
    std::env::current_dir().map_err(|e| CloveError::runtime(e.to_string()))
}

fn split_pattern(pattern: &str) -> (PathBuf, Vec<String>, bool) {
    let path = Path::new(pattern);
    let mut root = PathBuf::new();
    let mut segments = Vec::new();
    for comp in path.components() {
        match comp {
            Component::Prefix(prefix) => root.push(prefix.as_os_str()),
            Component::RootDir => {
                let mut root_str = root.to_string_lossy().to_string();
                root_str.push(std::path::MAIN_SEPARATOR);
                root = PathBuf::from(root_str);
            }
            Component::CurDir => segments.push(".".to_string()),
            Component::ParentDir => segments.push("..".to_string()),
            Component::Normal(seg) => segments.push(seg.to_string_lossy().to_string()),
        }
    }
    (root, segments, path.is_absolute())
}

fn collect_glob(glob: &GlobPattern, opts: &GlobOptions) -> Result<Vec<String>, CloveError> {
    let mut out = Vec::new();
    let rel = PathBuf::new();
    collect_glob_inner(
        &glob.root,
        &rel,
        0,
        &glob.segments,
        glob.output_relative,
        opts,
        &mut out,
    )?;
    Ok(out)
}

fn collect_glob_inner(
    path: &Path,
    rel: &Path,
    idx: usize,
    segments: &[String],
    output_relative: bool,
    opts: &GlobOptions,
    out: &mut Vec<String>,
) -> Result<(), CloveError> {
    if idx >= segments.len() {
        if glob_only_match(path, opts)? {
            let value = if output_relative {
                if rel.as_os_str().is_empty() {
                    ".".to_string()
                } else {
                    rel.to_string_lossy().to_string()
                }
            } else {
                path.to_string_lossy().to_string()
            };
            out.push(value);
        }
        return Ok(());
    }
    let seg = segments[idx].as_str();
    if seg.is_empty() || seg == "." {
        return collect_glob_inner(path, rel, idx + 1, segments, output_relative, opts, out);
    }
    if seg == ".." {
        if let Some(parent) = path.parent() {
            let mut next_rel = rel.to_path_buf();
            next_rel.push("..");
            collect_glob_inner(
                parent,
                &next_rel,
                idx + 1,
                segments,
                output_relative,
                opts,
                out,
            )?;
        }
        return Ok(());
    }
    if seg == "**" && opts.recursive {
        collect_glob_inner(path, rel, idx + 1, segments, output_relative, opts, out)?;
        let is_last = idx + 1 == segments.len();
        let entries = match fs::read_dir(path) {
            Ok(entries) => entries,
            Err(e) => {
                handle_glob_error(opts, e)?;
                return Ok(());
            }
        };
        for entry in entries {
            let entry = match entry {
                Ok(entry) => entry,
                Err(e) => {
                    handle_glob_error(opts, e)?;
                    continue;
                }
            };
            let name = entry.file_name().to_string_lossy().to_string();
            if should_skip_dot(&name, opts.dotfiles, seg) {
                continue;
            }
            let entry_path = entry.path();
            let mut next_rel = rel.to_path_buf();
            next_rel.push(&name);
            if is_last || dir_entry_is_dir(&entry, opts)? {
                collect_glob_inner(
                    &entry_path,
                    &next_rel,
                    idx,
                    segments,
                    output_relative,
                    opts,
                    out,
                )?;
            }
        }
        return Ok(());
    }
    if !path_is_dir(path, opts)? {
        return Ok(());
    }
    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(e) => {
            handle_glob_error(opts, e)?;
            return Ok(());
        }
    };
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                handle_glob_error(opts, e)?;
                continue;
            }
        };
        let name = entry.file_name().to_string_lossy().to_string();
        if !segment_matches(seg, &name, opts.dotfiles) {
            continue;
        }
        let entry_path = entry.path();
        let mut next_rel = rel.to_path_buf();
        next_rel.push(&name);
        collect_glob_inner(
            &entry_path,
            &next_rel,
            idx + 1,
            segments,
            output_relative,
            opts,
            out,
        )?;
    }
    Ok(())
}

fn segment_matches(segment: &str, name: &str, dotfiles: bool) -> bool {
    if should_skip_dot(name, dotfiles, segment) {
        return false;
    }
    if segment.contains('*') || segment.contains('?') {
        wildcard_match(segment, name)
    } else {
        segment == name
    }
}

fn should_skip_dot(name: &str, dotfiles: bool, segment: &str) -> bool {
    !dotfiles && name.starts_with('.') && !segment.starts_with('.')
}

fn wildcard_match(pattern: &str, text: &str) -> bool {
    let p: Vec<char> = pattern.chars().collect();
    let t: Vec<char> = text.chars().collect();
    let mut p_idx = 0;
    let mut t_idx = 0;
    let mut star_idx: Option<usize> = None;
    let mut match_idx = 0;
    while t_idx < t.len() {
        if p_idx < p.len() && (p[p_idx] == '?' || p[p_idx] == t[t_idx]) {
            p_idx += 1;
            t_idx += 1;
        } else if p_idx < p.len() && p[p_idx] == '*' {
            star_idx = Some(p_idx);
            p_idx += 1;
            match_idx = t_idx;
        } else if let Some(star) = star_idx {
            p_idx = star + 1;
            match_idx += 1;
            t_idx = match_idx;
        } else {
            return false;
        }
    }
    while p_idx < p.len() && p[p_idx] == '*' {
        p_idx += 1;
    }
    p_idx == p.len()
}

fn glob_only_match(path: &Path, opts: &GlobOptions) -> Result<bool, CloveError> {
    match opts.only {
        GlobOnly::Any => Ok(true),
        GlobOnly::File => {
            let meta = match path_metadata(path, opts) {
                Ok(meta) => meta,
                Err(err) => {
                    handle_glob_error(opts, err)?;
                    return Ok(false);
                }
            };
            Ok(meta.is_file())
        }
        GlobOnly::Dir => {
            let meta = match path_metadata(path, opts) {
                Ok(meta) => meta,
                Err(err) => {
                    handle_glob_error(opts, err)?;
                    return Ok(false);
                }
            };
            Ok(meta.is_dir())
        }
    }
}

fn path_metadata(path: &Path, opts: &GlobOptions) -> Result<fs::Metadata, CloveError> {
    if opts.follow_symlinks {
        fs::metadata(path).map_err(|e| CloveError::runtime(e.to_string()))
    } else {
        fs::symlink_metadata(path).map_err(|e| CloveError::runtime(e.to_string()))
    }
}

fn path_is_dir(path: &Path, opts: &GlobOptions) -> Result<bool, CloveError> {
    let meta = match path_metadata(path, opts) {
        Ok(meta) => meta,
        Err(err) => {
            handle_glob_error(opts, err)?;
            return Ok(false);
        }
    };
    Ok(meta.is_dir())
}

fn dir_entry_is_dir(entry: &fs::DirEntry, opts: &GlobOptions) -> Result<bool, CloveError> {
    let file_type = match entry.file_type() {
        Ok(ft) => ft,
        Err(err) => {
            handle_glob_error(opts, err)?;
            return Ok(false);
        }
    };
    if file_type.is_dir() {
        return Ok(true);
    }
    if file_type.is_symlink() && opts.follow_symlinks {
        let meta = match fs::metadata(entry.path()) {
            Ok(meta) => meta,
            Err(err) => {
                handle_glob_error(opts, err)?;
                return Ok(false);
            }
        };
        return Ok(meta.is_dir());
    }
    Ok(false)
}

fn handle_glob_error(opts: &GlobOptions, err: impl std::fmt::Display) -> Result<(), CloveError> {
    match opts.on_error {
        GlobOnError::Skip => Ok(()),
        GlobOnError::Throw => Err(CloveError::runtime(format!("glob failed: {}", err))),
    }
}

struct GlobSeqEngine {
    stack: Vec<GlobState>,
    opts: GlobOptions,
    segments: Vec<String>,
    output_relative: bool,
}

impl GlobSeqEngine {
    fn new(pattern: GlobPattern, opts: GlobOptions) -> Self {
        Self {
            stack: vec![GlobState::Check {
                path: pattern.root,
                rel: PathBuf::new(),
                idx: 0,
            }],
            segments: pattern.segments,
            output_relative: pattern.output_relative,
            opts,
        }
    }
}

enum GlobState {
    Check {
        path: PathBuf,
        rel: PathBuf,
        idx: usize,
    },
    IterDir {
        iter: fs::ReadDir,
        rel: PathBuf,
        idx: usize,
        segment: String,
    },
    IterRecursive {
        iter: fs::ReadDir,
        rel: PathBuf,
        idx: usize,
        last: bool,
    },
}

impl SeqEngine for GlobSeqEngine {
    fn next(&mut self) -> Result<Option<Value>, CloveError> {
        'outer: loop {
            let state = match self.stack.pop() {
                Some(state) => state,
                None => return Ok(None),
            };
            match state {
                GlobState::Check { path, rel, idx } => {
                    if idx >= self.segments.len() {
                        if glob_only_match(&path, &self.opts)? {
                            let value = if self.output_relative {
                                if rel.as_os_str().is_empty() {
                                    ".".to_string()
                                } else {
                                    rel.to_string_lossy().to_string()
                                }
                            } else {
                                path.to_string_lossy().to_string()
                            };
                            return Ok(Some(Value::String(value)));
                        }
                        continue;
                    }
                    let seg = self.segments[idx].clone();
                    if seg.is_empty() || seg == "." {
                        self.stack.push(GlobState::Check {
                            path,
                            rel,
                            idx: idx + 1,
                        });
                        continue;
                    }
                    if seg == ".." {
                        if let Some(parent) = path.parent() {
                            let mut next_rel = rel.clone();
                            next_rel.push("..");
                            self.stack.push(GlobState::Check {
                                path: parent.to_path_buf(),
                                rel: next_rel,
                                idx: idx + 1,
                            });
                        }
                        continue;
                    }
                    if seg == "**" && self.opts.recursive {
                        self.stack.push(GlobState::Check {
                            path: path.clone(),
                            rel: rel.clone(),
                            idx: idx + 1,
                        });
                        let last = idx + 1 == self.segments.len();
                        let entries = match fs::read_dir(&path) {
                            Ok(entries) => entries,
                            Err(err) => {
                                handle_glob_error(&self.opts, err)?;
                                continue;
                            }
                        };
                        self.stack.push(GlobState::IterRecursive {
                            iter: entries,
                            rel,
                            idx,
                            last,
                        });
                        continue;
                    }
                    if !path_is_dir(&path, &self.opts)? {
                        continue;
                    }
                    let entries = match fs::read_dir(&path) {
                        Ok(entries) => entries,
                        Err(err) => {
                            handle_glob_error(&self.opts, err)?;
                            continue;
                        }
                    };
                    self.stack.push(GlobState::IterDir {
                        iter: entries,
                        rel,
                        idx,
                        segment: seg,
                    });
                }
                GlobState::IterDir {
                    mut iter,
                    rel,
                    idx,
                    segment,
                } => {
                    while let Some(entry) = iter.next() {
                        let entry = match entry {
                            Ok(entry) => entry,
                            Err(err) => {
                                handle_glob_error(&self.opts, err)?;
                                continue;
                            }
                        };
                        let name = entry.file_name().to_string_lossy().to_string();
                        if !segment_matches(&segment, &name, self.opts.dotfiles) {
                            continue;
                        }
                        let mut next_rel = rel.clone();
                        next_rel.push(&name);
                        self.stack.push(GlobState::IterDir {
                            iter,
                            rel,
                            idx,
                            segment,
                        });
                        self.stack.push(GlobState::Check {
                            path: entry.path(),
                            rel: next_rel,
                            idx: idx + 1,
                        });
                        continue 'outer;
                    }
                }
                GlobState::IterRecursive {
                    mut iter,
                    rel,
                    idx,
                    last,
                } => {
                    while let Some(entry) = iter.next() {
                        let entry = match entry {
                            Ok(entry) => entry,
                            Err(err) => {
                                handle_glob_error(&self.opts, err)?;
                                continue;
                            }
                        };
                        let name = entry.file_name().to_string_lossy().to_string();
                        if should_skip_dot(&name, self.opts.dotfiles, "**") {
                            continue;
                        }
                        if !last && !dir_entry_is_dir(&entry, &self.opts)? {
                            continue;
                        }
                        let mut next_rel = rel.clone();
                        next_rel.push(&name);
                        self.stack.push(GlobState::IterRecursive {
                            iter,
                            rel,
                            idx,
                            last,
                        });
                        self.stack.push(GlobState::Check {
                            path: entry.path(),
                            rel: next_rel,
                            idx,
                        });
                        continue 'outer;
                    }
                }
            }
        }
    }
}
