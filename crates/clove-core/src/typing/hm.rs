//! Minimal HM type inference core (not yet wired to constraint generation).
//! - Type representation (`Type` / `PrimType`)
//! - Type schemes (`Scheme`)
//! - Substitution (`Subst`) and unification
//! - Generalization / instantiation

use std::collections::HashMap;

use crate::ast::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrimType {
    Int,
    Float,
    Bool,
    Str,
    Nil,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(TypeVar),
    Prim(PrimType),
    Func(Vec<Type>, Option<Box<Type>>, Box<Type>),
    Vector(Box<Type>),
    Tuple(Vec<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Option(Box<Type>),
    Mut(Box<Type>),
    Record(HashMap<String, Type>),
    Opaque(String),
    Overloaded(Vec<Type>),
    Any,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeEnv {
    vars: HashMap<String, Scheme>,
    next_var: u32,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            next_var: 0,
        }
    }

    pub fn with_bindings(bindings: impl IntoIterator<Item = (String, Scheme)>) -> Self {
        let mut env = Self::new();
        for (k, v) in bindings {
            env.vars.insert(k, v);
        }
        env
    }

    pub fn lookup(&self, name: &str) -> Option<Scheme> {
        self.vars.get(name).cloned()
    }

    pub fn insert(&mut self, name: impl Into<String>, scheme: Scheme) {
        self.vars.insert(name.into(), scheme);
    }

    pub fn snapshot_bindings(&self) -> HashMap<String, Scheme> {
        self.vars.clone()
    }

    pub fn restore_bindings(&mut self, vars: HashMap<String, Scheme>) {
        self.vars = vars;
    }

    pub fn fresh_var(&mut self) -> TypeVar {
        let id = self.next_var;
        self.next_var += 1;
        TypeVar(id)
    }

    pub fn generalize(&self, ty: &Type) -> Scheme {
        let free_env = free_type_vars_scheme_map(&self.vars);
        let free_ty = free_type_vars(ty);
        let vars: Vec<TypeVar> = free_ty
            .into_iter()
            .filter(|v| !free_env.contains(v))
            .collect();
        Scheme {
            vars,
            ty: ty.clone(),
        }
    }

    pub fn generalize_with_subst(&self, ty: &Type, subst: &Subst) -> Scheme {
        let applied = subst.apply(ty);
        let free_env = free_type_vars_scheme_map_with_subst(&self.vars, subst);
        let free_ty = free_type_vars(&applied);
        let vars: Vec<TypeVar> = free_ty
            .into_iter()
            .filter(|v| !free_env.contains(v))
            .collect();
        Scheme { vars, ty: applied }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Subst {
    pub map: HashMap<TypeVar, Type>,
}

impl Subst {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn singleton(var: TypeVar, ty: Type) -> Self {
        let mut map = HashMap::new();
        map.insert(var, ty);
        Self { map }
    }

    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(v) => self.map.get(v).cloned().unwrap_or(Type::Var(*v)),
            Type::Prim(_) => ty.clone(),
            Type::Any => Type::Any,
            Type::Vector(inner) => Type::Vector(Box::new(self.apply(inner))),
            Type::Tuple(items) => {
                let items = items.iter().map(|t| self.apply(t)).collect();
                Type::Tuple(items)
            }
            Type::Map(k, v) => Type::Map(Box::new(self.apply(k)), Box::new(self.apply(v))),
            Type::Set(inner) => Type::Set(Box::new(self.apply(inner))),
            Type::Option(inner) => Type::Option(Box::new(self.apply(inner))),
            Type::Mut(inner) => Type::Mut(Box::new(self.apply(inner))),
            Type::Record(fields) => {
                let mut mapped = HashMap::new();
                for (k, v) in fields {
                    mapped.insert(k.clone(), self.apply(v));
                }
                Type::Record(mapped)
            }
            Type::Opaque(name) => Type::Opaque(name.clone()),
            Type::Overloaded(candidates) => {
                let candidates = candidates.iter().map(|t| self.apply(t)).collect();
                Type::Overloaded(candidates)
            }
            Type::Func(args, rest, ret) => {
                let args = args.iter().map(|a| self.apply(a)).collect();
                let rest = rest.as_ref().map(|t| Box::new(self.apply(t)));
                let ret = Box::new(self.apply(ret));
                Type::Func(args, rest, ret)
            }
        }
    }

    pub fn compose(&self, other: &Subst) -> Subst {
        let mut composed = HashMap::new();
        // s1 ∘ s2: apply s1 over values of s2, then add s1 entries
        for (k, v) in &other.map {
            composed.insert(*k, self.apply(v));
        }
        for (k, v) in &self.map {
            composed.insert(*k, v.clone());
        }
        Subst { map: composed }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError {
    pub message: String,
    pub span: Option<Span>,
}

impl TypeError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Option<Span>) -> Self {
        if self.span.is_none() {
            self.span = span;
        }
        self
    }
}

pub fn instantiate(scheme: &Scheme, env: &mut TypeEnv) -> Type {
    let mut subst = Subst::new();
    for var in &scheme.vars {
        subst.map.insert(*var, Type::Var(env.fresh_var())); // fresh vars
    }
    subst.apply(&scheme.ty)
}

pub fn unify(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Any, _other) | (_other, Type::Any) => Ok(Subst::new()),
        (Type::Overloaded(candidates), other) => unify_overloaded(candidates, other),
        (other, Type::Overloaded(candidates)) => unify_overloaded(candidates, other),
        (Type::Var(v), t) | (t, Type::Var(v)) => bind_var(*v, t),
        (Type::Prim(p1), Type::Prim(p2)) if p1 == p2 => Ok(Subst::new()),
        (Type::Vector(x), Type::Vector(y)) => {
            let s = unify(x, y)?;
            Ok(s)
        }
        (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
            let mut subst = Subst::new();
            for (x, y) in xs.iter().zip(ys) {
                let s = unify(&subst.apply(x), &subst.apply(y))?;
                subst = s.compose(&subst);
            }
            Ok(subst)
        }
        (Type::Map(k1, v1), Type::Map(k2, v2)) => {
            let s1 = unify(k1, k2)?;
            let s2 = unify(&s1.apply(v1), &s1.apply(v2))?;
            Ok(s2.compose(&s1))
        }
        (Type::Set(inner1), Type::Set(inner2)) => {
            let s = unify(inner1, inner2)?;
            Ok(s)
        }
        (Type::Option(inner1), Type::Option(inner2)) => {
            let s = unify(inner1, inner2)?;
            Ok(s)
        }
        (Type::Mut(inner1), Type::Mut(inner2)) => {
            let s = unify(inner1, inner2)?;
            Ok(s)
        }
        (Type::Record(r1), Type::Record(r2)) => unify_records(r1, r2),
        (Type::Opaque(name1), Type::Opaque(name2)) if name1 == name2 => Ok(Subst::new()),
        (Type::Func(args1, rest1, ret1), Type::Func(args2, rest2, ret2)) => {
            unify_func(args1, rest1.as_deref(), ret1, args2, rest2.as_deref(), ret2)
        }
        _ => Err(TypeError::new(format!("cannot unify {:?} with {:?}", a, b))),
    }
}

fn unify_overloaded(candidates: &[Type], other: &Type) -> Result<Subst, TypeError> {
    let mut last_err = None;
    match other {
        Type::Overloaded(other_candidates) => {
            for cand in candidates {
                for other_cand in other_candidates {
                    match unify(cand, other_cand) {
                        Ok(subst) => return Ok(subst),
                        Err(err) => last_err = Some(err),
                    }
                }
            }
        }
        _ => {
            for cand in candidates {
                match unify(cand, other) {
                    Ok(subst) => return Ok(subst),
                    Err(err) => last_err = Some(err),
                }
            }
        }
    }
    Err(last_err.unwrap_or_else(|| {
        TypeError::new(format!(
            "cannot unify {:?} with {:?}",
            Type::Overloaded(candidates.to_vec()),
            other
        ))
    }))
}

fn unify_func(
    args1: &[Type],
    rest1: Option<&Type>,
    ret1: &Type,
    args2: &[Type],
    rest2: Option<&Type>,
    ret2: &Type,
) -> Result<Subst, TypeError> {
    let mut subst = Subst::new();
    let min_len = args1.len().min(args2.len());
    for (x, y) in args1.iter().zip(args2).take(min_len) {
        let s = unify(&subst.apply(x), &subst.apply(y))?;
        subst = s.compose(&subst);
    }
    if args1.len() > args2.len() {
        match rest2 {
            Some(rest) => {
                for arg in args1.iter().skip(args2.len()) {
                    let s = unify(&subst.apply(arg), &subst.apply(rest))?;
                    subst = s.compose(&subst);
                }
            }
            None => {
                return Err(TypeError::new(format!(
                    "function arity mismatch: {} vs {}",
                    args1.len(),
                    args2.len()
                )))
            }
        }
    } else if args2.len() > args1.len() {
        match rest1 {
            Some(rest) => {
                for arg in args2.iter().skip(args1.len()) {
                    let s = unify(&subst.apply(rest), &subst.apply(arg))?;
                    subst = s.compose(&subst);
                }
            }
            None => {
                return Err(TypeError::new(format!(
                    "function arity mismatch: {} vs {}",
                    args1.len(),
                    args2.len()
                )))
            }
        }
    }
    if let (Some(r1), Some(r2)) = (rest1, rest2) {
        let s = unify(&subst.apply(r1), &subst.apply(r2))?;
        subst = s.compose(&subst);
    }
    let s_ret = unify(&subst.apply(ret1), &subst.apply(ret2))?;
    Ok(s_ret.compose(&subst))
}

fn bind_var(var: TypeVar, ty: &Type) -> Result<Subst, TypeError> {
    if let Type::Var(v) = ty {
        if *v == var {
            return Ok(Subst::new());
        }
    }
    if occurs_in(var, ty) {
        return Err(TypeError::new(format!(
            "occurs check failed: {:?} in {:?}",
            var, ty
        )));
    }
    Ok(Subst::singleton(var, ty.clone()))
}

fn occurs_in(var: TypeVar, ty: &Type) -> bool {
    match ty {
        Type::Var(v) => *v == var,
        Type::Prim(_) | Type::Any => false,
        Type::Vector(inner) => occurs_in(var, inner),
        Type::Tuple(items) => items.iter().any(|t| occurs_in(var, t)),
        Type::Map(k, v) => occurs_in(var, k) || occurs_in(var, v),
        Type::Set(inner) => occurs_in(var, inner),
        Type::Option(inner) => occurs_in(var, inner),
        Type::Mut(inner) => occurs_in(var, inner),
        Type::Record(fields) => fields.values().any(|t| occurs_in(var, t)),
        Type::Opaque(_) => false,
        Type::Overloaded(candidates) => candidates.iter().any(|t| occurs_in(var, t)),
        Type::Func(args, rest, ret) => {
            args.iter().any(|t| occurs_in(var, t))
                || rest.as_ref().map(|t| occurs_in(var, t)).unwrap_or(false)
                || occurs_in(var, ret)
        }
    }
}

fn unify_records(
    r1: &HashMap<String, Type>,
    r2: &HashMap<String, Type>,
) -> Result<Subst, TypeError> {
    if r1.len() != r2.len() {
        return Err(TypeError::new(format!(
            "record arity mismatch: {:?} vs {:?}",
            r1, r2
        )));
    }
    let mut subst = Subst::new();
    for (k, v1) in r1 {
        let v2 = r2
            .get(k)
            .ok_or_else(|| TypeError::new(format!("record missing field {}", k)))?;
        let s = unify(&subst.apply(v1), &subst.apply(v2))?;
        subst = s.compose(&subst);
    }
    Ok(subst)
}

fn free_type_vars(ty: &Type) -> Vec<TypeVar> {
    match ty {
        Type::Var(v) => vec![*v],
        Type::Prim(_) | Type::Any => vec![],
        Type::Vector(inner) => free_type_vars(inner),
        Type::Tuple(items) => items.iter().flat_map(|t| free_type_vars(t)).collect(),
        Type::Map(k, v) => {
            let mut out = free_type_vars(k);
            out.extend(free_type_vars(v));
            out
        }
        Type::Set(inner) => free_type_vars(inner),
        Type::Option(inner) => free_type_vars(inner),
        Type::Mut(inner) => free_type_vars(inner),
        Type::Record(fields) => {
            let mut out = Vec::new();
            for t in fields.values() {
                out.extend(free_type_vars(t));
            }
            out
        }
        Type::Opaque(_) => vec![],
        Type::Overloaded(candidates) => candidates.iter().flat_map(|t| free_type_vars(t)).collect(),
        Type::Func(args, rest, ret) => {
            let mut out = Vec::new();
            for a in args {
                out.extend(free_type_vars(a));
            }
            if let Some(rest) = rest {
                out.extend(free_type_vars(rest));
            }
            out.extend(free_type_vars(ret));
            out
        }
    }
}

impl Type {
    pub fn describe(&self) -> String {
        match self {
            Type::Var(v) => type_var_name(*v),
            Type::Prim(PrimType::Int) => "Int".into(),
            Type::Prim(PrimType::Float) => "Float".into(),
            Type::Prim(PrimType::Bool) => "Bool".into(),
            Type::Prim(PrimType::Str) => "Str".into(),
            Type::Prim(PrimType::Nil) => "Nil".into(),
            Type::Func(args, rest, ret) => {
                let mut params: Vec<String> = args.iter().map(|t| t.describe()).collect();
                if let Some(rest) = rest {
                    params.push(format!("& {}", rest.describe()));
                }
                format!("[{}] -> {}", params.join(" "), ret.describe())
            }
            Type::Vector(inner) => format!("[{}]", inner.describe()),
            Type::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|t| t.describe()).collect();
                format!("[{}]", parts.join(" "))
            }
            Type::Map(k, v) => format!("{{{} {}}}", k.describe(), v.describe()),
            Type::Set(inner) => format!("#{{{}}}", inner.describe()),
            Type::Option(inner) => format!("{}?", inner.describe()),
            Type::Mut(inner) => format!("Mut<{}>", inner.describe()),
            Type::Record(fields) => {
                let mut parts: Vec<_> = fields
                    .iter()
                    .map(|(k, v)| format!(":{} {}", k, v.describe()))
                    .collect();
                parts.sort();
                format!("{{{}}}", parts.join(", "))
            }
            Type::Opaque(name) => format!("Opaque({})", name),
            Type::Overloaded(candidates) => {
                let parts: Vec<_> = candidates.iter().map(|t| t.describe()).collect();
                format!("overload({})", parts.join(" | "))
            }
            Type::Any => "Any".into(),
        }
    }

    pub fn describe_pretty(&self) -> String {
        let mut namer = PrettyTypeNamer::new();
        self.describe_pretty_with(&mut namer)
    }

    fn describe_pretty_with(&self, namer: &mut PrettyTypeNamer) -> String {
        match self {
            Type::Var(v) => namer.name_for(*v),
            Type::Prim(PrimType::Int) => "Int".into(),
            Type::Prim(PrimType::Float) => "Float".into(),
            Type::Prim(PrimType::Bool) => "Bool".into(),
            Type::Prim(PrimType::Str) => "Str".into(),
            Type::Prim(PrimType::Nil) => "Nil".into(),
            Type::Func(args, rest, ret) => {
                let mut params: Vec<String> =
                    args.iter().map(|t| t.describe_pretty_with(namer)).collect();
                if let Some(rest) = rest {
                    params.push(format!("& {}", rest.describe_pretty_with(namer)));
                }
                format!(
                    "[{}] -> {}",
                    params.join(" "),
                    ret.describe_pretty_with(namer)
                )
            }
            Type::Vector(inner) => format!("[{}]", inner.describe_pretty_with(namer)),
            Type::Tuple(items) => {
                let parts: Vec<String> = items
                    .iter()
                    .map(|t| t.describe_pretty_with(namer))
                    .collect();
                format!("[{}]", parts.join(" "))
            }
            Type::Map(k, v) => format!(
                "{{{} {}}}",
                k.describe_pretty_with(namer),
                v.describe_pretty_with(namer)
            ),
            Type::Set(inner) => format!("#{{{}}}", inner.describe_pretty_with(namer)),
            Type::Option(inner) => format!("{}?", inner.describe_pretty_with(namer)),
            Type::Mut(inner) => format!("Mut<{}>", inner.describe_pretty_with(namer)),
            Type::Record(fields) => {
                let mut parts: Vec<_> = fields
                    .iter()
                    .map(|(k, v)| format!(":{} {}", k, v.describe_pretty_with(namer)))
                    .collect();
                parts.sort();
                format!("{{{}}}", parts.join(", "))
            }
            Type::Opaque(name) => format!("Opaque({})", name),
            Type::Overloaded(candidates) => {
                let parts: Vec<_> = candidates
                    .iter()
                    .map(|t| t.describe_pretty_with(namer))
                    .collect();
                format!("overload({})", parts.join(" | "))
            }
            Type::Any => "Any".into(),
        }
    }
}

fn type_var_name(var: TypeVar) -> String {
    let id = var.0 as usize;
    if id < 26 {
        let ch = (b'a' + id as u8) as char;
        ch.to_string()
    } else {
        format!("t{}", var.0)
    }
}

struct PrettyTypeNamer {
    names: HashMap<TypeVar, String>,
    next_id: usize,
}

impl PrettyTypeNamer {
    fn new() -> Self {
        Self {
            names: HashMap::new(),
            next_id: 0,
        }
    }

    fn name_for(&mut self, var: TypeVar) -> String {
        if let Some(name) = self.names.get(&var) {
            return name.clone();
        }
        let name = format_pretty_type_var(self.next_id);
        self.next_id += 1;
        self.names.insert(var, name.clone());
        name
    }
}

fn format_pretty_type_var(id: usize) -> String {
    if id < 26 {
        let ch = (b'a' + id as u8) as char;
        format!("'{}", ch)
    } else {
        format!("'t{}", id)
    }
}

fn free_type_vars_scheme_map(map: &HashMap<String, Scheme>) -> Vec<TypeVar> {
    let mut out = Vec::new();
    for scheme in map.values() {
        out.extend(free_type_vars_scheme(scheme));
    }
    out
}

fn free_type_vars_scheme_map_with_subst(
    map: &HashMap<String, Scheme>,
    subst: &Subst,
) -> Vec<TypeVar> {
    let mut out = Vec::new();
    for scheme in map.values() {
        out.extend(free_type_vars_scheme_with_subst(scheme, subst));
    }
    out
}

fn free_type_vars_scheme(scheme: &Scheme) -> Vec<TypeVar> {
    let mut vars = free_type_vars(&scheme.ty);
    vars.retain(|v| !scheme.vars.contains(v));
    vars
}

fn free_type_vars_scheme_with_subst(scheme: &Scheme, subst: &Subst) -> Vec<TypeVar> {
    let mut filtered = subst.clone();
    for var in &scheme.vars {
        filtered.map.remove(var);
    }
    let applied = filtered.apply(&scheme.ty);
    let mut vars = free_type_vars(&applied);
    vars.retain(|v| !scheme.vars.contains(v));
    vars
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_primitive_ok() {
        assert!(unify(&Type::Prim(PrimType::Int), &Type::Prim(PrimType::Int)).is_ok());
        assert!(unify(&Type::Prim(PrimType::Int), &Type::Prim(PrimType::Float)).is_err());
    }

    #[test]
    fn unify_mut_requires_mut() {
        let mut_int = Type::Mut(Box::new(Type::Prim(PrimType::Int)));
        assert!(unify(&mut_int, &mut_int).is_ok());
        assert!(unify(&mut_int, &Type::Prim(PrimType::Int)).is_err());
    }

    #[test]
    fn occurs_check_fails() {
        let t = Type::Func(
            vec![Type::Var(TypeVar(0))],
            None,
            Box::new(Type::Prim(PrimType::Int)),
        );
        let res = unify(&Type::Var(TypeVar(0)), &t);
        assert!(res.is_err());
    }

    #[test]
    fn compose_subst_applies_in_order() {
        let s1 = Subst::singleton(TypeVar(0), Type::Prim(PrimType::Int));
        let s2 = Subst::singleton(TypeVar(1), Type::Var(TypeVar(0)));
        let composed = s1.compose(&s2);
        let applied = composed.apply(&Type::Var(TypeVar(1)));
        assert_eq!(applied, Type::Prim(PrimType::Int));
    }

    #[test]
    fn describe_pretty_renames_type_vars_in_order() {
        let ty = Type::Func(
            vec![
                Type::Var(TypeVar(3)),
                Type::Var(TypeVar(1)),
                Type::Var(TypeVar(3)),
            ],
            None,
            Box::new(Type::Var(TypeVar(1))),
        );
        assert_eq!(ty.describe_pretty(), "['a 'b 'a] -> 'b");
    }
}
