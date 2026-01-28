use crate::ast::{Expr, ExprKind};

pub fn contains_dynamic(forms: &[Expr]) -> bool {
    forms.iter().any(expr_contains_dynamic)
}

fn expr_contains_dynamic(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::List(items) => {
            if let Some(head) = items.first() {
                if let ExprKind::Symbol(sym) = &head.kind {
                    if matches!(sym.as_str(), "comment" | "quote" | "quasiquote") {
                        return false;
                    }
                    if is_def_foreign(sym) {
                        return false;
                    }
                    if is_dynamic_head(sym) {
                        return true;
                    }
                }
            }
            items.iter().any(expr_contains_dynamic)
        }
        ExprKind::Vector(items) => items.iter().any(expr_contains_dynamic),
        ExprKind::Map(entries) => entries
            .iter()
            .any(|(k, v)| expr_contains_dynamic(k) || expr_contains_dynamic(v)),
        ExprKind::ForeignBlock { .. } => true,
        _ => false,
    }
}

fn is_dynamic_head(sym: &str) -> bool {
    matches!(
        sym,
        "eval"
            | "load-string"
            | "load-file"
            | "load-native"
            | "read-string"
            | "read-file"
            | "with-redefs"
            | "with-dyn"
    )
}

fn is_def_foreign(sym: &str) -> bool {
    matches!(sym, "def-foreign" | "def-interop" | "interop")
}
