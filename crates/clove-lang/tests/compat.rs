use clove_core::{
    ast::{FormKind, Value},
    eval_source_with_engines,
    options::EvalOptions,
    reader::{Reader, ReaderOptions},
};

fn eval_py(src: &str) -> Value {
    eval_source_with_engines(src, EvalOptions::default(), &clove_python::engines()).unwrap()
}

#[test]
fn python_brace_block_reads_and_evals() {
    let out = eval_py("$py{1 + 2}");
    assert_eq!(out.to_string(), "3");
}

#[test]
fn reader_consumes_py_brace_block() {
    let opts = ReaderOptions::language_defaults(vec![]);
    let mut reader = Reader::new_with_options("$py{1 + 2}", opts);

    let forms = reader.read_all().expect("parse");
    assert_eq!(forms.len(), 1);
    match &forms[0].kind {
        FormKind::ForeignRaw { tag, code } => {
            assert_eq!(tag.as_deref(), Some("py"));
            assert_eq!(code.trim(), "1 + 2");
        }
        other => panic!("expected ForeignRaw, got {:?}", other),
    }
}

#[test]
fn lexical_scope_and_set_in_chain() {
    let val = eval_py("(let [x 10] ((fn [] x)))");
    assert_eq!(val.to_string(), "10");

    // set! updates outer binding
    let opts = EvalOptions::default();
    let out = eval_source_with_engines(
        "(do (def x 1) (let [y 2] (set! x 3) x) x)",
        opts,
        &clove_python::engines(),
    )
    .unwrap();
    assert_eq!(out.to_string(), "3");
}

#[test]
fn nil_tolerant_builtins() {
    assert_eq!(eval_py("(assoc nil :a 1)").to_string(), "{:a 1}");
    assert_eq!(eval_py("(conj nil [:a 1])").to_string(), "{:a 1}");
    assert_eq!(eval_py("(get nil :a 42)").to_string(), "42");
    assert_eq!(eval_py("(dissoc nil :a)").to_string(), "nil");
}
