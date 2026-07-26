//! Startup used to do the same expensive work several times over.
//!
//! Measured on 2026-07-25: `clove examples/hello.clv` took ~500ms, of which roughly half
//! was one runtime creation and another fifth was a *second* runtime that the doc store
//! built for itself. Environments created and dropped are invisible to `live_env_count`,
//! so this test watches `total_env_creations` instead.

use clove_core::env::total_env_creations;
use clove_core::options::EvalOptions;
use clove_core::runtime::RuntimeCtx;

/// One runtime installs the builtin environment plus a handful of namespace
/// environments. `set_imported_symbol` used to build a full builtin environment per
/// imported symbol and drop it, which put this in the hundreds.
const MAX_ENVS_PER_RUNTIME: usize = 80;

#[test]
fn startup_does_not_repeat_expensive_work() {
    // One test function so the two measurements cannot interleave with each other:
    // both read process-wide counters.
    let before = total_env_creations();
    let ctx = RuntimeCtx::new(EvalOptions::default(), &[]);
    let per_runtime = total_env_creations() - before;
    assert!(
        per_runtime <= MAX_ENVS_PER_RUNTIME,
        "creating a runtime built {per_runtime} environments (limit {MAX_ENVS_PER_RUNTIME}); \
         something is building throwaway environments per symbol"
    );
    drop(ctx);

    // Configuring doc directories must not spin up a runtime of its own. The doc store is
    // built on first lookup, by which time a real runtime exists.
    let before = total_env_creations();
    clove_core::docs::set_extra_doc_dirs(Vec::new());
    let for_docs = total_env_creations() - before;
    assert!(
        for_docs <= 2,
        "configuring doc dirs built {for_docs} environments; the doc store must not create \
         a second runtime"
    );

    // Docs still resolve.
    let entry = clove_core::docs::find_doc_entry("map").expect("map must have a doc entry");
    assert_eq!(entry.canonical, "map");
}

/// The doc store may be built before any runtime exists (an embedder looking up a doc
/// first). The fallback runtime it creates for itself has to evaluate the standard
/// library, because the subject positions that `gen_oop_examples` needs are registered
/// there: without them, std entries silently lose their OOP examples.
#[test]
fn doc_entries_keep_oop_examples_for_std_functions() {
    let entry = clove_core::docs::find_doc_entry("tap").expect("tap must have a doc entry");
    assert!(
        !entry.examples.is_empty(),
        "tap should document plain examples"
    );
    assert!(
        !entry.oop_examples.is_empty(),
        "tap declares {{:subject-pos :last}} in the standard library, so it must have OOP \
         examples; an empty list means the doc store was built without std"
    );
}
