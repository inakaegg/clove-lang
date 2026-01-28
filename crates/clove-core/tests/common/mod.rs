use clove_core::ast::{Form, FormKind, InterpolatedPart, MapItem};
use clove_core::reader::Reader;
use std::fs;
use std::path::{Path, PathBuf};

pub fn read_example(name: &str) -> String {
    fs::read_to_string(example_path(name))
        .unwrap_or_else(|e| panic!("failed to read {}: {}", example_path(name).display(), e))
}

pub fn parse_forms(source: &str) -> Vec<Form> {
    let mut reader = Reader::new(source);
    reader
        .read_all()
        .unwrap_or_else(|e| panic!("failed to parse source: {}", e))
}

pub fn assert_forms_equivalent(expected: &[Form], actual: &[Form], ctx: &str) {
    assert_eq!(
        expected.len(),
        actual.len(),
        "different number of forms in {}",
        ctx
    );
    for (idx, (lhs, rhs)) in expected.iter().zip(actual).enumerate() {
        assert_form_equivalent(lhs, rhs, &format!("{} form {}", ctx, idx));
    }
}

fn assert_form_equivalent(lhs: &Form, rhs: &Form, ctx: &str) {
    assert_eq!(
        lhs.type_hint, rhs.type_hint,
        "type hint mismatch at {}",
        ctx
    );
    match (&lhs.kind, &rhs.kind) {
        (FormKind::Symbol(a), FormKind::Symbol(b))
        | (FormKind::Keyword(a), FormKind::Keyword(b))
        | (FormKind::String(a), FormKind::String(b)) => assert_eq!(a, b, "{}", ctx),
        (FormKind::Int(a), FormKind::Int(b)) => assert_eq!(a, b, "{}", ctx),
        (FormKind::Float(a), FormKind::Float(b)) => assert_eq!(a, b, "{}", ctx),
        (FormKind::Bool(a), FormKind::Bool(b)) => assert_eq!(a, b, "{}", ctx),
        (FormKind::Nil, FormKind::Nil) => {}
        (FormKind::Duration(a), FormKind::Duration(b)) => assert_eq!(a, b, "{}", ctx),
        (FormKind::InterpolatedString(a), FormKind::InterpolatedString(b)) => {
            assert_eq!(a.len(), b.len(), "{}", ctx);
            for (idx, (left, right)) in a.iter().zip(b).enumerate() {
                match (left, right) {
                    (InterpolatedPart::Text(lt), InterpolatedPart::Text(rt)) => {
                        assert_eq!(lt, rt, "{} part {}", ctx, idx);
                    }
                    (InterpolatedPart::Expr(le), InterpolatedPart::Expr(re)) => {
                        assert_form_equivalent(le, re, &format!("{} part {}", ctx, idx));
                    }
                    _ => panic!("interpolated string part mismatch at {}", ctx),
                }
            }
        }
        (
            FormKind::InterpolatedRegex {
                parts: a,
                delim: da,
            },
            FormKind::InterpolatedRegex {
                parts: b,
                delim: db,
            },
        ) => {
            assert_eq!(da, db, "{}", ctx);
            assert_eq!(a.len(), b.len(), "{}", ctx);
            for (idx, (left, right)) in a.iter().zip(b).enumerate() {
                match (left, right) {
                    (InterpolatedPart::Text(lt), InterpolatedPart::Text(rt)) => {
                        assert_eq!(lt, rt, "{} part {}", ctx, idx);
                    }
                    (InterpolatedPart::Expr(le), InterpolatedPart::Expr(re)) => {
                        assert_form_equivalent(le, re, &format!("{} part {}", ctx, idx));
                    }
                    _ => panic!("interpolated regex part mismatch at {}", ctx),
                }
            }
        }
        (FormKind::ShortFn(a), FormKind::ShortFn(b))
        | (FormKind::List(a), FormKind::List(b))
        | (FormKind::Vector(a), FormKind::Vector(b))
        | (FormKind::Set(a), FormKind::Set(b)) => {
            assert_forms_equivalent(a, b, ctx);
        }
        (FormKind::Map(a), FormKind::Map(b)) => assert_map_equivalent(a, b, ctx),
        (
            FormKind::Regex {
                pattern: pa,
                delim: da,
            },
            FormKind::Regex {
                pattern: pb,
                delim: db,
            },
        ) => {
            assert_eq!(pa, pb, "{}", ctx);
            assert_eq!(da, db, "{}", ctx);
        }
        (
            FormKind::ForeignBlock { tag: ta, code: ca },
            FormKind::ForeignBlock { tag: tb, code: cb },
        ) => {
            assert_eq!(ta, tb, "{}", ctx);
            assert_eq!(ca, cb, "{}", ctx);
        }
        (
            FormKind::ForeignRaw { tag: ta, code: ca },
            FormKind::ForeignRaw { tag: tb, code: cb },
        ) => {
            assert_eq!(ta, tb, "{}", ctx);
            assert_eq!(ca, cb, "{}", ctx);
        }
        (
            FormKind::ForeignSymbol { tag: ta, path: pa },
            FormKind::ForeignSymbol { tag: tb, path: pb },
        ) => {
            assert_eq!(ta, tb, "{}", ctx);
            assert_eq!(pa, pb, "{}", ctx);
        }
        other => panic!("form mismatch at {}: {:?}", ctx, other),
    }
}

fn assert_map_equivalent(lhs: &[MapItem], rhs: &[MapItem], ctx: &str) {
    assert_eq!(lhs.len(), rhs.len(), "map entry count mismatch at {}", ctx);
    for (idx, (left, right)) in lhs.iter().zip(rhs).enumerate() {
        match (left, right) {
            (MapItem::KeyValue(lk, lv), MapItem::KeyValue(rk, rv)) => {
                assert_form_equivalent(lk, rk, &format!("{} map key {}", ctx, idx));
                assert_form_equivalent(lv, rv, &format!("{} map value {}", ctx, idx));
            }
            (MapItem::Spread(le), MapItem::Spread(re)) => {
                assert_form_equivalent(le, re, &format!("{} map spread {}", ctx, idx));
            }
            _ => panic!("map item mismatch at {}", ctx),
        }
    }
}

fn example_path(name: &str) -> PathBuf {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir must have parent")
        .parent()
        .expect("workspace root must exist");
    let fixture = workspace_root
        .join("crates")
        .join("clove-core")
        .join("tests")
        .join("fixtures")
        .join(name);
    if fixture.exists() {
        return fixture;
    }
    workspace_root.join("examples").join(name)
}
