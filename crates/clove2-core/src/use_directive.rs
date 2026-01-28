use crate::ast::{Expr, ExprKind};
use crate::error::Clove2Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Native,
    Dynamic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeLevel {
    Strict,
    Warn,
    Allow,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MutMode {
    Mut,
    Imut,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct UseConfig {
    pub mode: Option<Mode>,
    pub native_level: Option<NativeLevel>,
    pub imut_default: Option<bool>,
    pub mut_hard: Option<bool>,
}

impl UseConfig {
    pub fn default_mut_mode(&self) -> MutMode {
        if self.imut_default.unwrap_or(false) {
            MutMode::Imut
        } else {
            MutMode::Mut
        }
    }
}

pub fn parse_use_directives(forms: &[Expr]) -> Result<(UseConfig, usize), Clove2Error> {
    let mut cfg = UseConfig::default();
    let mut consumed = 0;
    for form in forms {
        if parse_use_form(form, &mut cfg)? {
            consumed += 1;
        } else {
            break;
        }
    }
    Ok((cfg, consumed))
}

fn parse_use_form(form: &Expr, cfg: &mut UseConfig) -> Result<bool, Clove2Error> {
    let ExprKind::List(items) = &form.kind else {
        return Ok(false);
    };
    let Some(head) = items.first() else {
        return Ok(false);
    };
    if !matches!(&head.kind, ExprKind::Symbol(sym) if sym == "use") {
        return Ok(false);
    }
    if items.len() != 3 {
        return Err(Clove2Error::new("use expects 2 arguments"));
    }
    let key = extract_symbol_or_keyword(&items[1])?;
    match key.as_str() {
        "mode" => {
            let value = extract_symbol_or_keyword(&items[2])?;
            cfg.mode = Some(match value.as_str() {
                "native" => Mode::Native,
                "dynamic" => Mode::Dynamic,
                _ => return Err(Clove2Error::new("use mode expects :native or :dynamic")),
            });
        }
        "native" => {
            let value = extract_symbol_or_keyword(&items[2])?;
            cfg.native_level = Some(match value.as_str() {
                "strict" => NativeLevel::Strict,
                "warn" => NativeLevel::Warn,
                "allow" => NativeLevel::Allow,
                _ => {
                    return Err(Clove2Error::new(
                        "use native expects :strict, :warn, or :allow",
                    ))
                }
            });
        }
        "imut-default" => {
            let value = extract_bool(&items[2])?;
            cfg.imut_default = Some(value);
        }
        "mut-default" => {
            let value = extract_bool(&items[2])?;
            cfg.imut_default = Some(!value);
        }
        "mut-hard" => {
            let value = extract_bool(&items[2])?;
            cfg.mut_hard = Some(value);
        }
        _ => {
            return Err(Clove2Error::new("unknown use directive"));
        }
    }
    Ok(true)
}

fn extract_symbol_or_keyword(expr: &Expr) -> Result<String, Clove2Error> {
    match &expr.kind {
        ExprKind::Symbol(sym) => Ok(sym.clone()),
        ExprKind::Keyword(sym) => Ok(sym.clone()),
        _ => Err(Clove2Error::new("expected symbol or keyword")),
    }
}

fn extract_bool(expr: &Expr) -> Result<bool, Clove2Error> {
    match &expr.kind {
        ExprKind::Literal(crate::ast::Literal::Bool(value)) => Ok(*value),
        _ => Err(Clove2Error::new("expected bool")),
    }
}
