//! Guards the phase1 and phase2 readers against silently drifting apart.
//!
//! phase2 (`clove-build-core`) is a deliberate *subset* of phase1
//! (`clove-core`): it may reject syntax phase1 accepts. The reverse must never
//! happen — if phase2 accepts something phase1 rejects, phase2 is implementing
//! syntax that is not part of the language.
//!
//! That is exactly how the `/` namespace separator survived in phase2 after
//! `TASK/DONE/名前空間.md` abolished it: no test compared the two readers.

use clove_build_core::reader::read_all as p2_read;
use clove_core::reader::{Reader, ReaderOptions};

fn phase1_accepts(src: &str) -> bool {
    Reader::new_with_options(src, ReaderOptions::language_defaults(Vec::new()))
        .read_all()
        .is_ok()
}

fn phase2_accepts(src: &str) -> bool {
    p2_read(src).is_ok()
}

/// Constructs both readers must accept.
const SHARED: &[(&str, &str)] = &[
    ("namespace ::", "(println 'foo::bar)"),
    ("keyword ::", "(println :foo::bar)"),
    ("division /", "(println (/ 6 2))"),
    ("set literal", "(println #{1 2})"),
    ("short fn", "(println (map #(inc %) [1]))"),
    ("foreign $rb", "(println $rb{1 + 2})"),
    ("foreign $py", "(println $py{1 + 2})"),
    ("hint <Int>", "(def x<Int> 1)"),
    ("hint : Int", "(def x: Int 1)"),
    ("hint Map<Str, Int>", "(def m: Map<Str, Int> {\"a\" 1})"),
    ("hint fn type", "(def f: [Int Int] -> Str (fn [a b] \"x\"))"),
    (
        "hint defn return",
        "(defn add :int [x<Int> y<Int>] (+ x y))",
    ),
    ("indexer", "(def xs [1 2 3])\n(println xs[0])"),
    ("indexer default", "(def xs [1])\n(println xs[9 || :ng])"),
    ("indexer range", "(def xs [1 2 3])\n(println xs[0...2])"),
    ("dot-chain", "(println (range 3).(map inc ?))"),
    ("oop chain", "(println 1.inc)"),
    ("map shorthand", "(println {name: 1})"),
    ("regex literal", "(println (re-find /a+/ \"aa\"))"),
    ("duration", "(println 10ms)"),
    ("numeric underscore", "(println 1_000)"),
    ("deref", "(def a (atom 1))\n(println @a)"),
    ("quote", "(println '(1 2))"),
    ("interpolation", "(let [n 1] (println \"v=#{n}\"))"),
];

/// Constructs both readers must reject.
const REJECTED_BY_BOTH: &[(&str, &str)] = &[
    ("symbol foo/bar", "(println 'foo/bar)"),
    ("keyword :foo/bar", "(println :foo/bar)"),
    ("foreign paren form", "(println $rb(1 + 2))"),
    ("unknown reader tag", "(println #nosuchtag{1})"),
];

/// Syntax phase1 supports and phase2 does not implement yet.
///
/// These are allowed to be one-sided, but only in this direction. When phase2
/// gains support, move the entry to `SHARED` so the list stays truthful.
const PHASE1_ONLY: &[(&str, &str)] = &[
    ("#json tag", "(def c #json{\"h\":1})"),
    ("#yaml tag", "(def c #yaml{\n  h: 1\n})"),
    ("#/re/ literal", "(println (re-find #/a+/ \"aa\"))"),
    ("tag-less ${}", "(println ${1 + 2})"),
];

#[test]
fn phase2_is_never_more_permissive_than_phase1() {
    let mut violations = Vec::new();
    for (label, src) in SHARED.iter().chain(REJECTED_BY_BOTH).chain(PHASE1_ONLY) {
        if !phase1_accepts(src) && phase2_accepts(src) {
            violations.push(format!(
                "  {label}: phase1 rejects but phase2 accepts\n    {src:?}"
            ));
        }
    }
    assert!(
        violations.is_empty(),
        "phase2 accepts syntax that is not part of the language:\n{}",
        violations.join("\n")
    );
}

#[test]
fn shared_syntax_is_accepted_by_both_readers() {
    for (label, src) in SHARED {
        assert!(phase1_accepts(src), "phase1 should accept {label}: {src:?}");
        assert!(phase2_accepts(src), "phase2 should accept {label}: {src:?}");
    }
}

#[test]
fn invalid_syntax_is_rejected_by_both_readers() {
    for (label, src) in REJECTED_BY_BOTH {
        assert!(
            !phase1_accepts(src),
            "phase1 should reject {label}: {src:?}"
        );
        assert!(
            !phase2_accepts(src),
            "phase2 should reject {label}: {src:?}"
        );
    }
}

#[test]
fn phase1_only_list_matches_reality() {
    for (label, src) in PHASE1_ONLY {
        assert!(phase1_accepts(src), "phase1 should accept {label}: {src:?}");
        assert!(
            !phase2_accepts(src),
            "phase2 now accepts {label} — move it to SHARED: {src:?}"
        );
    }
}
