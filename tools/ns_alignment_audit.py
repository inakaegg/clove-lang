#!/usr/bin/env python3
import json
import re
from datetime import date
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DOCS_PATH = ROOT / "data/clove_docs/clove-docs.json"
STD_PATH = ROOT / "crates/clove-core/assets/clove_std.clv"
ALIASES_PATH = ROOT / "crates/clove-core/src/symbols.rs"
# Written under tmp/ (git-ignored): the report is a snapshot of the current
# tree, not a document worth versioning.
REPORT_PATH = ROOT / "tmp/ns_alignment_report.md"

ORIGIN_NS = {
    "string": "string",
    "walk": "walk",
    "edn": "edn",
    "pprint": "pprint",
    "test": "test",
    "shell": "shell",
    "io": "io",
    "zip": "zip",
    "cli": "cli",
    "config": "config",
    "data": "data",
    "duration": "duration",
    "http": "http",
    "json": "json",
    "path": "path",
    "process": "process",
    "sdl2": "sdl2",
    "term": "term",
    "time": "time",
    "yaml": "yaml",
}

EXPECTED = {
    "set": [
        "union",
        "intersection",
        "difference",
        "select",
        "set-select",
        "project",
        "join",
        "rename",
        "rename-keys",
        "index",
        "map-invert",
        "subset?",
        "superset?",
    ],
    "string": [
        "blank?",
        "capitalize",
        "ends-with?",
        "escape",
        "join",
        "lower-case",
        "upper-case",
        "replace",
        "replace-first",
        "reverse",
        "split",
        "split-lines",
        "starts-with?",
        "trim",
        "triml",
        "trimr",
    ],
    "walk": [
        "walk",
        "prewalk",
        "postwalk",
        "prewalk-replace",
        "postwalk-replace",
        "keywordize-keys",
        "stringify-keys",
    ],
    "edn": ["read", "read-string"],
    "test": ["deftest", "is", "testing", "run-tests"],
}


class SymbolInfo:
    def __init__(self, canonical: str) -> None:
        self.canonical = canonical
        self.names = {canonical}
        self.doc = ""
        self.doc_priority = -1
        self.origin = ""


def normalize_symbol(name: str) -> str:
    return name.strip()


def load_docs():
    with DOCS_PATH.open() as f:
        return json.load(f)


def parse_std_defs(text: str):
    pattern = re.compile(r"\(defn\s+([^\s\)]+)\s*(?:\"([^\"]*)\")?", re.S)
    out = []
    for match in pattern.finditer(text):
        name = match.group(1)
        doc = match.group(2) or ""
        out.append((name, doc))
    return out


def parse_builtin_aliases(text: str):
    pattern = re.compile(r"\(\"([^\"]+)\",\s*\"([^\"]+)\"\)")
    return pattern.findall(text)


def resolve_canonical(name: str, origin: str, std_names, name_origins):
    normalized = normalize_symbol(name)
    if "::" in normalized:
        return normalized
    ns = origin or name_origins.get(normalized, "")
    if ns in ORIGIN_NS:
        return f"{ns}::{normalized}"
    if normalized in std_names and ns in ("", "core"):
        return f"std::{normalized}"
    if ns in ("", "core"):
        return f"core::{normalized}"
    if ns == "special form":
        return normalized
    return normalized


def doc_summary(doc: str) -> str:
    if not doc:
        return ""
    text = " ".join(doc.strip().split())
    if len(text) > 90:
        return text[:87] + "..."
    return text


def pick_current(canonicals, expected):
    if expected in canonicals:
        return expected
    for prefix in ("std::", "core::"):
        for name in canonicals:
            if name.startswith(prefix):
                return name
    return canonicals[0]


def main() -> None:
    docs = load_docs()
    name_origins = {}
    for entry in docs:
        name = entry.get("name", "")
        if "::" in name:
            continue
        origin = entry.get("origin") or ""
        if origin and name not in name_origins:
            name_origins[name] = origin

    std_text = STD_PATH.read_text()
    std_defs = parse_std_defs(std_text)
    std_names = {name for name, _ in std_defs}

    symbols = {}

    def ensure_symbol(canonical: str) -> SymbolInfo:
        if canonical not in symbols:
            symbols[canonical] = SymbolInfo(canonical)
        return symbols[canonical]

    def add_entry(name: str, canonical: str, doc: str, priority: int, origin: str) -> None:
        info = ensure_symbol(canonical)
        info.names.add(name)
        if doc and priority >= info.doc_priority:
            info.doc = doc
            info.doc_priority = priority
        if origin and not info.origin:
            info.origin = origin

    for entry in docs:
        name = entry.get("name", "")
        origin = entry.get("origin") or ""
        doc = entry.get("doc") or ""
        canonical = resolve_canonical(name, origin, std_names, name_origins)
        add_entry(name, canonical, doc, 2, origin)

    for name, doc in std_defs:
        canonical = f"std::{name}"
        add_entry(name, canonical, doc, 1, "std")

    alias_text = ALIASES_PATH.read_text()
    for alias, target in parse_builtin_aliases(alias_text):
        origin = name_origins.get(target, "")
        canonical = resolve_canonical(target, origin, std_names, name_origins)
        info = ensure_symbol(canonical)
        info.names.add(alias)

    name_to_canonicals = {}
    for canonical in symbols:
        tail = canonical.rsplit("::", 1)[-1]
        name_to_canonicals.setdefault(tail, []).append(canonical)

    mismatches = []
    matched = 0

    for ns, names in EXPECTED.items():
        expected_ns = f"{ns}::"
        for name in names:
            if ns == "set" and name == "select" and "set-select" in name_to_canonicals:
                continue
            canonicals = name_to_canonicals.get(name, [])
            if not canonicals:
                continue
            matched += 1
            expected = f"{expected_ns}{name}"
            current = pick_current(canonicals, expected)
            if current != expected:
                info = symbols[current]
                aliases = sorted(n for n in info.names if n != current)
                mismatches.append(
                    {
                        "ns": ns,
                        "name": name,
                        "current": current,
                        "expected": expected,
                        "aliases": aliases,
                        "doc": doc_summary(info.doc),
                    }
                )

    sections = {ns: [] for ns in EXPECTED.keys()}
    for row in mismatches:
        sections[row["ns"]].append(row)

    lines = []
    lines.append("# Namespace alignment report (Clojure-compatible)")
    lines.append("")
    lines.append(f"Generated at: {date.today().isoformat()}")
    lines.append("")
    lines.append("## Summary")
    lines.append(f"- scanned vars: {len(symbols)}")
    lines.append(f"- matched clojure-mapped names: {matched}")
    lines.append(f"- mismatches: {len(mismatches)}")

    for ns in EXPECTED.keys():
        lines.append("")
        lines.append(f"## {ns}::")
        lines.append("| name | current canonical | expected canonical | existing aliases | doc summary |")
        lines.append("|---|---|---|---|---|")
        rows = sections[ns]
        if not rows:
            lines.append("| (none) | | | | |")
            continue
        for row in rows:
            aliases = f"[{', '.join(row['aliases'])}]" if row["aliases"] else "[]"
            doc = row["doc"]
            lines.append(
                f"| {row['name']} | {row['current']} | {row['expected']} | {aliases} | {doc} |"
            )

    lines.append("")
    lines.append("## Actions")
    if not mismatches:
        lines.append("- (none)")
    else:
        for row in mismatches:
            lines.append("")
            lines.append(f"### {row['expected']}")
            lines.append(f"- Move canonical to {row['expected']}")
            lines.append(f"- Keep alias: {row['current']} -> {row['expected']}")
            lines.append(f"- Keep core alias: {row['name']} -> {row['expected']}")
            lines.append(
                f"- Docs: ensure canonical is {row['expected']}, alias list updated"
            )

    REPORT_PATH.write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
