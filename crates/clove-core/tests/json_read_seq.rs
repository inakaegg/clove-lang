use clove_core::eval_source;
use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

fn unique_temp_path(label: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    std::env::temp_dir().join(format!("clove_json_read_seq_{label}_{nanos}.json"))
}

fn clove_string_literal(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[test]
fn json_read_seq_accepts_path_and_reader() {
    let path = unique_temp_path("basic");
    fs::write(&path, "[1, 2, 3]").expect("write json temp");
    let path_literal = clove_string_literal(&path.to_string_lossy());
    let src = format!(
        r#"(let [p "{path}"
                 v1 (vec (json::read-seq p))
                 r (io::open-reader p)
                 v2 (vec (json::read-seq r))
                 _ (io::close r)]
             [v1 v2])"#,
        path = path_literal
    );
    let out = eval_source(&src, None).expect("eval json::read-seq");
    let expected = eval_source("[[1 2 3] [1 2 3]]", None).expect("expected result");
    assert_eq!(out, expected);
}
