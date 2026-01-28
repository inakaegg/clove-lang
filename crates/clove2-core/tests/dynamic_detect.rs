use clove2_core::dynamic_detect::contains_dynamic;
use clove2_core::reader::read_all;

#[test]
fn detect_dynamic_calls() {
    let forms = read_all("(eval 1)").unwrap();
    assert!(contains_dynamic(&forms));
}

#[test]
fn detect_dynamic_loads() {
    let forms = read_all("(read-file \"x\")\n(load-native \"x\")").unwrap();
    assert!(contains_dynamic(&forms));
}

#[test]
fn detect_dynamic_nested() {
    let forms = read_all("(if true (load-string \"x\") 1)").unwrap();
    assert!(contains_dynamic(&forms));
}

#[test]
fn no_dynamic() {
    let forms = read_all("(let [x 1] (inc x))").unwrap();
    assert!(!contains_dynamic(&forms));
}

#[test]
fn detect_foreign_block() {
    let forms = read_all("$rb{puts 1}").unwrap();
    assert!(contains_dynamic(&forms));
}

#[test]
fn ignore_foreign_block_in_def_foreign() {
    let forms = read_all("(def-foreign sha1 :code $rb{puts 1} :entry Crypto.sha1 [s: Str] -> Str)")
        .unwrap();
    assert!(!contains_dynamic(&forms));
}
