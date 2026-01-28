use clove_core::fmt_config::load_fmt_config_path;
use clove_core::formatter::{format_source, FormatOptions};
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn fmt_snapshots() {
    let update = std::env::var("CLOVE_UPDATE_SNAPSHOTS").ok().as_deref() == Some("1");
    let base_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fmt_snapshots");
    let cases_dir = base_dir.join("cases");
    let expected_default_dir = base_dir.join("expected/default");
    let expected_alt_dir = base_dir.join("expected/alt");
    let alt_config_path = base_dir.join("alt_config.toml");

    let alt_options = load_alt_options(&alt_config_path);
    let mut entries = read_case_entries(&cases_dir);

    entries.sort_by_key(|path| path.file_name().map(|n| n.to_string_lossy().to_string()));

    for case_path in entries {
        let case_name = case_path.file_name().unwrap().to_string_lossy().to_string();
        let input = read_to_string(&case_path);
        let default_out = normalize_newlines(
            &format_source(&input, FormatOptions::default())
                .unwrap_or_else(|e| panic!("failed to format {}: {}", case_name, e)),
        );
        let alt_out = normalize_newlines(
            &format_source(&input, alt_options.clone())
                .unwrap_or_else(|e| panic!("failed to format {} (alt): {}", case_name, e)),
        );

        let expected_default = expected_default_dir.join(&case_name);
        let expected_alt = expected_alt_dir.join(&case_name);

        assert_snapshot(&case_name, &default_out, &expected_default, update);
        assert_snapshot(&case_name, &alt_out, &expected_alt, update);

        if is_invariant_case(&case_name) {
            assert_eq!(
                default_out, alt_out,
                "case {} must not change between default and alt",
                case_name
            );
        }
    }
}

fn load_alt_options(path: &Path) -> FormatOptions {
    let loaded = load_fmt_config_path(path).expect("load alt_config.toml");
    let mut options = FormatOptions::default();
    loaded.config.apply(&mut options).expect("apply alt config");
    options
}

fn read_case_entries(dir: &Path) -> Vec<PathBuf> {
    let mut entries = Vec::new();
    let read_dir = fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("failed to read cases dir {}: {}", dir.display(), e));
    for entry in read_dir {
        let entry = entry.unwrap_or_else(|e| panic!("failed to read case: {}", e));
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("clv") {
            entries.push(path);
        }
    }
    entries
}

fn read_to_string(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e))
}

fn normalize_newlines(input: &str) -> String {
    input.replace("\r\n", "\n")
}

fn write_atomic(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .unwrap_or_else(|e| panic!("failed to create {}: {}", parent.display(), e));
    }
    let tmp_path = path.with_extension("tmp");
    fs::write(&tmp_path, content)
        .unwrap_or_else(|e| panic!("failed to write {}: {}", tmp_path.display(), e));
    fs::rename(&tmp_path, path)
        .unwrap_or_else(|e| panic!("failed to rename {}: {}", tmp_path.display(), e));
}

fn assert_snapshot(case_name: &str, actual: &str, expected_path: &Path, update: bool) {
    if update {
        write_atomic(expected_path, actual);
        return;
    }
    if !expected_path.exists() {
        panic!(
            "missing snapshot for {}: {} (run: CLOVE_UPDATE_SNAPSHOTS=1 cargo test -p clove-core --test fmt_snapshots)",
            case_name,
            expected_path.display()
        );
    }
    let expected = normalize_newlines(&read_to_string(expected_path));
    if expected != actual {
        let window = first_mismatch_window(&expected, actual);
        panic!(
            "snapshot mismatch for {}: {}\n{}\n(run: CLOVE_UPDATE_SNAPSHOTS=1 cargo test -p clove-core --test fmt_snapshots)",
            case_name,
            expected_path.display(),
            window
        );
    }
}

fn first_mismatch_window(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();
    let max_len = expected_lines.len().max(actual_lines.len());
    for idx in 0..max_len {
        let left = expected_lines.get(idx).copied().unwrap_or("");
        let right = actual_lines.get(idx).copied().unwrap_or("");
        if left != right {
            let start = idx.saturating_sub(2);
            let end = (idx + 3).min(max_len);
            let mut out = String::new();
            out.push_str("expected:\n");
            for line_idx in start..end {
                let line = expected_lines.get(line_idx).copied().unwrap_or("");
                out.push_str(&format!("{:>4} | {}\n", line_idx + 1, line));
            }
            out.push_str("actual:\n");
            for line_idx in start..end {
                let line = actual_lines.get(line_idx).copied().unwrap_or("");
                out.push_str(&format!("{:>4} | {}\n", line_idx + 1, line));
            }
            return out;
        }
    }
    "no diff".to_string()
}

fn is_invariant_case(case_name: &str) -> bool {
    matches!(
        case_name,
        "fmt_016_empty_collections.clv" | "fmt_017_strings_and_regex.clv"
    )
}
