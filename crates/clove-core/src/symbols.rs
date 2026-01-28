use std::borrow::Cow;
use std::collections::HashSet;

pub fn canonical_symbol_name(name: &str) -> Cow<'_, str> {
    Cow::Borrowed(strip_type_hint_suffix(name))
}

static BUILTIN_ALIASES: &[(&str, &str)] = &[
    ("def-", "def"),
    ("-def", "def"),
    ("defn-", "defn"),
    ("-defn", "defn"),
    ("type-of", "type"),
    ("describe-type", "describe"),
    ("puts", "println"),
    ("pprint", "pp"),
    ("pprint-str", "pp-str"),
    ("swap!", "atom-update!"),
    ("reset!", "atom-set!"),
    ("redef", "set!"),
    ("set-validator!", "atom-set-validator!"),
    ("validator", "atom-validator"),
    ("get-validator", "atom-validator"),
    ("<!!", "chan-take!"),
    (">!!", "chan-put!"),
    ("put!", "chan-put!"),
    ("take!", "chan-take!"),
    ("go", "spawn"),
    ("<!", "chan-take!"),
    (">!", "chan-put!"),
    ("async::chan", "chan"),
    ("async::chan?", "chan?"),
    ("async::chan-put!", "chan-put!"),
    ("async::chan-take!", "chan-take!"),
    ("async::chan-close!", "chan-close!"),
    ("async::chan-closed?", "chan-closed?"),
    ("async::timeout", "timeout"),
    ("async::cancelled?", "cancelled?"),
    ("async::cancel-chan", "cancel-chan"),
    ("async::scope-cancelled?", "cancelled?"),
    ("async::scope-cancel-chan", "cancel-chan"),
    ("async::scope-select", "scope-select"),
    ("async::select", "select"),
    ("async::go", "spawn"),
    ("async::<!!", "chan-take!"),
    ("async::>!!", "chan-put!"),
    ("async::add-watch", "add-watch"),
    ("async::remove-watch", "remove-watch"),
    ("async::done?", "done?"),
    ("async::promise", "promise"),
    ("async::promise?", "promise?"),
    ("async::promise-deref", "promise-deref"),
    ("async::promise-deliver!", "promise-deliver!"),
    ("async::promise-done?", "promise-done?"),
    ("async::promise-error", "promise-error"),
    ("async::promise-then", "promise-then"),
    ("async::promise-catch", "promise-catch"),
    ("async::promise-finally", "promise-finally"),
    ("async::promise-all", "promise-all"),
    ("async::promise-all-settled", "promise-all-settled"),
    ("async::promise-race", "promise-race"),
    ("async::promise-any", "promise-any"),
    ("async::future", "future"),
    ("async::future?", "future?"),
    ("async::future-deref", "future-deref"),
    ("async::future-done?", "future-done?"),
    ("async::future-cancel!", "future-cancel!"),
    ("async::future-then", "future-then"),
    ("async::future-catch", "future-catch"),
    ("async::future-finally", "future-finally"),
    ("async::future-all", "future-all"),
    ("async::future-any", "future-any"),
    ("async::future-race", "future-race"),
    ("async::future-all-settled", "future-all-settled"),
    ("async::agent", "agent"),
    ("async::agent?", "agent?"),
    ("async::agent-deref", "agent-deref"),
    ("async::agent-send!", "agent-send!"),
    ("async::agent-send-io!", "agent-send-io!"),
    ("async::agent-await", "agent-await"),
    ("async::agent-done?", "agent-done?"),
    ("async::agent-error", "agent-error"),
    ("async::agent-restart!", "agent-restart!"),
    ("string::split", "split"),
    ("string::replace", "str-replace"),
    ("string::replace-first", "replace-first"),
    ("string::upper-case", "upper-case"),
    ("string::lower-case", "lower-case"),
    ("string::trim", "trim"),
    ("string::triml", "triml"),
    ("string::trimr", "trimr"),
    ("string::blank?", "blank?"),
    ("string::starts-with?", "starts-with?"),
    ("string::ends-with?", "ends-with?"),
    ("string::split-lines", "split-lines"),
    ("string::lines", "lines"),
    ("string::reverse", "reverse-str"),
    ("string::capitalize", "capitalize"),
    ("string::trim-newline", "trim-newline"),
    ("string::escape", "escape"),
    ("string::index-of", "index-of"),
    ("string::last-index-of", "last-index-of"),
    ("string::re-pattern", "re-pattern"),
    ("string::regex", "regex"),
    ("string::re-seq", "re-seq"),
    ("string::re-matcher", "re-matcher"),
    ("walk", "walk::walk"),
    ("core::walk", "walk::walk"),
    ("prewalk", "walk::prewalk"),
    ("core::prewalk", "walk::prewalk"),
    ("postwalk", "walk::postwalk"),
    ("core::postwalk", "walk::postwalk"),
    ("keywordize-keys", "walk::keywordize-keys"),
    ("core::keywordize-keys", "walk::keywordize-keys"),
    ("stringify-keys", "walk::stringify-keys"),
    ("core::stringify-keys", "walk::stringify-keys"),
    ("difference", "set::difference"),
    ("std::difference", "set::difference"),
    ("intersection", "set::intersection"),
    ("std::intersection", "set::intersection"),
    ("union", "set::union"),
    ("std::union", "set::union"),
    ("set-select", "set::select"),
    ("std::set-select", "set::select"),
    ("project", "set::project"),
    ("std::project", "set::project"),
    ("join", "string::join"),
    ("std::join", "string::join"),
    ("rename", "set::rename"),
    ("std::rename", "set::rename"),
    ("rename-keys", "set::rename-keys"),
    ("std::rename-keys", "set::rename-keys"),
    ("index", "set::index"),
    ("std::index", "set::index"),
    ("map-invert", "set::map-invert"),
    ("std::map-invert", "set::map-invert"),
    ("subset?", "set::subset?"),
    ("std::subset?", "set::subset?"),
    ("superset?", "set::superset?"),
    ("std::superset?", "set::superset?"),
    ("slurp-bytes", "io::slurp-bytes"),
    ("core::slurp-bytes", "io::slurp-bytes"),
    ("std::slurp-bytes", "io::slurp-bytes"),
    ("spit-bytes", "io::spit-bytes"),
    ("core::spit-bytes", "io::spit-bytes"),
    ("std::spit-bytes", "io::spit-bytes"),
    ("line-seq", "io::line-seq"),
    ("core::line-seq", "io::line-seq"),
    ("std::line-seq", "io::line-seq"),
    ("read-line", "io::read-line"),
    ("core::read-line", "io::read-line"),
    ("std::read-line", "io::read-line"),
    ("read-all", "io::read-all"),
    ("core::read-all", "io::read-all"),
    ("std::read-all", "io::read-all"),
    ("resource-url", "io::resource-url"),
    ("core::resource-url", "io::resource-url"),
    ("std::resource-url", "io::resource-url"),
    ("resource-bytes", "io::resource-bytes"),
    ("core::resource-bytes", "io::resource-bytes"),
    ("std::resource-bytes", "io::resource-bytes"),
    ("resource->tempfile", "io::resource->tempfile"),
    ("core::resource->tempfile", "io::resource->tempfile"),
    ("std::resource->tempfile", "io::resource->tempfile"),
    ("file-exists?", "fs::file-exists?"),
    ("core::file-exists?", "fs::file-exists?"),
    ("std::file-exists?", "fs::file-exists?"),
    ("io::file-exists?", "fs::file-exists?"),
    ("file?", "fs::file?"),
    ("core::file?", "fs::file?"),
    ("std::file?", "fs::file?"),
    ("io::file?", "fs::file?"),
    ("dir?", "fs::dir?"),
    ("core::dir?", "fs::dir?"),
    ("std::dir?", "fs::dir?"),
    ("io::dir?", "fs::dir?"),
    ("list-dir", "fs::list-dir"),
    ("core::list-dir", "fs::list-dir"),
    ("std::list-dir", "fs::list-dir"),
    ("io::list-dir", "fs::list-dir"),
    ("mkdir", "fs::mkdir"),
    ("core::mkdir", "fs::mkdir"),
    ("std::mkdir", "fs::mkdir"),
    ("io::mkdir", "fs::mkdir"),
    ("mkdirs", "fs::mkdirs"),
    ("core::mkdirs", "fs::mkdirs"),
    ("std::mkdirs", "fs::mkdirs"),
    ("io::mkdirs", "fs::mkdirs"),
    ("delete", "fs::delete"),
    ("core::delete", "fs::delete"),
    ("std::delete", "fs::delete"),
    ("io::delete", "fs::delete"),
    ("copy", "fs::copy"),
    ("core::copy", "fs::copy"),
    ("std::copy", "fs::copy"),
    ("io::copy", "fs::copy"),
    ("move", "fs::move"),
    ("core::move", "fs::move"),
    ("std::move", "fs::move"),
    ("io::move", "fs::move"),
    ("glob", "fs::glob"),
    ("core::glob", "fs::glob"),
    ("std::glob", "fs::glob"),
    ("io::glob", "fs::glob"),
    ("glob*", "fs::glob*"),
    ("core::glob*", "fs::glob*"),
    ("std::glob*", "fs::glob*"),
    ("io::glob*", "fs::glob*"),
    ("path-cwd", "path::cwd"),
    ("std::path-cwd", "path::cwd"),
    ("path-home-dir", "path::home-dir"),
    ("std::path-home-dir", "path::home-dir"),
    ("path-temp-dir", "path::temp-dir"),
    ("std::path-temp-dir", "path::temp-dir"),
    ("path-join", "path::join"),
    ("std::path-join", "path::join"),
    ("path-basename", "path::basename"),
    ("std::path-basename", "path::basename"),
    ("path-dirname", "path::dirname"),
    ("std::path-dirname", "path::dirname"),
    ("path-extname", "path::extname"),
    ("std::path-extname", "path::extname"),
    ("path-normalize", "path::normalize"),
    ("std::path-normalize", "path::normalize"),
    ("path-canonicalize", "path::canonicalize"),
    ("std::path-canonicalize", "path::canonicalize"),
    ("path-absolute?", "path::absolute?"),
    ("std::path-absolute?", "path::absolute?"),
    ("path-relative?", "path::relative?"),
    ("std::path-relative?", "path::relative?"),
    ("path-resolve", "path::resolve"),
    ("std::path-resolve", "path::resolve"),
    ("path-source-dir", "path::source-dir"),
    ("std::path-source-dir", "path::source-dir"),
    ("sanitize", "path::sanitize"),
    ("std::sanitize", "path::sanitize"),
    ("path::sanitize-filename", "path::sanitize"),
    ("sh", "process::sh"),
    ("core::sh", "process::sh"),
    ("std::sh", "process::sh"),
    ("sh!", "process::sh!"),
    ("core::sh!", "process::sh!"),
    ("std::sh!", "process::sh!"),
    ("sh-line", "process::sh-line"),
    ("core::sh-line", "process::sh-line"),
    ("std::sh-line", "process::sh-line"),
];

pub fn symbol_aliases(symbol: &str) -> Vec<String> {
    let canonical = canonical_symbol_name(symbol);
    let canonical_ref = canonical.as_ref();
    let mut out = vec![canonical_ref.to_string()];
    if let Some(local) = canonical_ref.strip_prefix("string::") {
        out.push(format!("str::{local}"));
    } else if let Some(local) = canonical_ref.strip_prefix("str::") {
        out.push(format!("string::{local}"));
    }
    out
}

pub fn builtin_alias_target(symbol: &str) -> Option<&'static str> {
    let canonical = canonical_symbol_name(symbol);
    BUILTIN_ALIASES
        .iter()
        .find_map(|(alias, target)| (*alias == canonical.as_ref()).then_some(*target))
}

pub fn builtin_aliases(symbol: &str) -> Vec<String> {
    let canonical = canonical_symbol_name(symbol);
    let canonical_ref = canonical.as_ref();
    let core_stripped = canonical_ref
        .strip_prefix("core::")
        .unwrap_or(canonical_ref);
    let string_stripped = canonical_ref
        .strip_prefix("string::")
        .unwrap_or(canonical_ref);
    let mut out = Vec::new();
    for (alias, target) in BUILTIN_ALIASES {
        if *target == canonical_ref || *target == core_stripped || *target == string_stripped {
            out.push((*alias).to_string());
            if let Some(local) = alias.strip_prefix("string::") {
                out.push(format!("str::{local}"));
            }
        }
    }
    if let Some(local) = canonical_ref.strip_prefix("string::") {
        out.push(format!("str::{local}"));
    } else if let Some(local) = canonical_ref.strip_prefix("str::") {
        out.push(format!("string::{local}"));
    }
    out
}

pub fn namespace_aliases(ns: &str, local: &str) -> Vec<String> {
    let mut out = vec![format!("{}::{}", ns, local)];
    if ns == "string" {
        out.push(format!("str::{local}"));
    }
    out
}

pub fn is_namespaced_symbol(name: &str) -> bool {
    name.contains("::")
}

pub fn doc_lookup_keys(symbol: &str) -> Vec<String> {
    fn insert(value: &str, allow_alias: bool, keys: &mut Vec<String>, seen: &mut HashSet<String>) {
        if value.is_empty() {
            return;
        }
        if allow_alias {
            if let Some(target) = builtin_alias_target(value) {
                if seen.insert(target.to_string()) {
                    keys.push(target.to_string());
                }
            }
        }
        if seen.insert(value.to_string()) {
            keys.push(value.to_string());
        }
    }

    let mut keys = Vec::new();
    let mut seen = HashSet::new();

    let canonical = canonical_symbol_name(symbol);
    let canonical_ref = canonical.as_ref();

    insert(canonical_ref, true, &mut keys, &mut seen);
    if let Some(local) = canonical_ref.strip_prefix("string::") {
        let alias = format!("str::{local}");
        insert(&alias, false, &mut keys, &mut seen);
    } else if let Some(local) = canonical_ref.strip_prefix("str::") {
        let alias = format!("string::{local}");
        insert(&alias, false, &mut keys, &mut seen);
    }
    if let Some(stripped) = canonical_ref.strip_prefix("core::") {
        insert(stripped, true, &mut keys, &mut seen);
    }
    if let Some((_, tail)) = canonical_ref.rsplit_once("::") {
        insert(tail, false, &mut keys, &mut seen);
    }
    if symbol != canonical_ref {
        insert(symbol, false, &mut keys, &mut seen);
    }

    keys
}

fn strip_type_hint_suffix(name: &str) -> &str {
    match name.find('<') {
        Some(idx) if idx > 0 => &name[..idx],
        _ => name,
    }
}
