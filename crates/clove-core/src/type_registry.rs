use std::collections::{HashMap as StdHashMap, HashSet};
use std::sync::RwLock;

use once_cell::sync::Lazy;

use crate::ast::{HashMap, Key, Value, Vector};
use crate::builtins::core::meta_lookup;
use crate::error::CloveError;
use crate::runtime::RuntimeCtx;
use crate::types::TypeKind as TypeExprKind;

static REGISTRY: Lazy<RwLock<RegistryStore>> = Lazy::new(|| {
    let mut base = RegistryState::new();
    base.install_primitives();
    let global = base.clone();
    RwLock::new(RegistryStore {
        base,
        global,
        per_runtime: StdHashMap::new(),
    })
});

const CORE_NS: &str = "core";
const TYPE_KEY: &str = "type";

#[derive(Clone, Debug)]
pub enum TypeKind {
    Primitive,
    Product,
    Sum,
    Alias,
}

#[derive(Clone, Debug)]
pub struct PrimitiveMeta {
    pub namespace: String,
    pub name: String,
    pub doc: Option<String>,
}

#[derive(Clone, Debug)]
pub struct ProductMeta {
    pub namespace: String,
    pub name: String,
    pub doc: Option<String>,
    pub fields: Vec<FieldMeta>,
    pub belongs_to: HashSet<String>,
    pub required_methods: Vec<RequiredMethod>,
}

#[derive(Clone, Debug)]
pub struct FnSigSpec {
    pub params: Vec<TypeExprKind>,
    pub rest: Option<TypeExprKind>,
    pub ret: TypeExprKind,
}

#[derive(Clone, Debug)]
pub struct RequiredMethod {
    pub name: String,
    pub sig: Option<FnSigSpec>,
}

#[derive(Clone, Debug)]
pub struct FieldMeta {
    pub name: String,
    pub schema: FieldSchema,
}

#[derive(Clone, Debug)]
pub enum FieldSchema {
    Primitive(PrimitiveType),
    TypeRef(String),
    TypeExpr(TypeExprKind),
}

impl FieldSchema {
    pub fn to_type_kind(&self) -> TypeExprKind {
        match self {
            FieldSchema::Primitive(prim) => TypeExprKind::from_primitive_type(*prim),
            FieldSchema::TypeRef(name) => TypeExprKind::named(name.clone()),
            FieldSchema::TypeExpr(kind) => kind.clone(),
        }
    }

    pub fn describe(&self, current_ns: Option<&str>) -> String {
        describe_type_kind(&self.to_type_kind(), current_ns)
    }

    fn to_value(&self, current_ns: Option<&str>) -> Value {
        type_kind_to_value(&self.to_type_kind(), current_ns)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PrimitiveType {
    Int,
    Float,
    Str,
    Bool,
    Nil,
    List,
    Vector,
    Seq,
    Map,
    Set,
    Symbol,
    Regex,
    Duration,
    Function,
    Atom,
    Chan,
    Promise,
    Task,
    Future,
    Agent,
    Foreign,
}

#[derive(Clone, Debug)]
pub struct SumMeta {
    pub namespace: String,
    pub name: String,
    pub doc: Option<String>,
    pub members: Vec<String>,
    pub qualified_only: bool,
}

#[derive(Clone, Debug)]
pub struct AliasMeta {
    pub namespace: String,
    pub name: String,
    pub doc: Option<String>,
    pub target: TypeExprKind,
}

#[derive(Clone, Debug)]
pub enum TypeEntry {
    Primitive(PrimitiveMeta),
    Product(ProductMeta),
    Sum(SumMeta),
    Alias(AliasMeta),
}

impl TypeEntry {
    pub fn kind(&self) -> TypeKind {
        match self {
            TypeEntry::Primitive(_) => TypeKind::Primitive,
            TypeEntry::Product(_) => TypeKind::Product,
            TypeEntry::Sum(_) => TypeKind::Sum,
            TypeEntry::Alias(_) => TypeKind::Alias,
        }
    }
    pub fn namespace(&self) -> &str {
        match self {
            TypeEntry::Primitive(meta) => &meta.namespace,
            TypeEntry::Product(meta) => &meta.namespace,
            TypeEntry::Sum(meta) => &meta.namespace,
            TypeEntry::Alias(meta) => &meta.namespace,
        }
    }
    pub fn name(&self) -> &str {
        match self {
            TypeEntry::Primitive(meta) => &meta.name,
            TypeEntry::Product(meta) => &meta.name,
            TypeEntry::Sum(meta) => &meta.name,
            TypeEntry::Alias(meta) => &meta.name,
        }
    }
}

#[derive(Clone)]
struct RegistryState {
    entries: StdHashMap<String, TypeEntry>,
}

struct RegistryStore {
    base: RegistryState,
    global: RegistryState,
    per_runtime: StdHashMap<usize, RegistryState>,
}

fn current_runtime_id() -> Option<usize> {
    RuntimeCtx::try_with_current(|ctx| Ok(ctx.runtime_id())).and_then(|res| res.ok())
}

fn with_registry<F, R>(f: F) -> R
where
    F: FnOnce(&RegistryState) -> R,
{
    let runtime_id = current_runtime_id();
    let guard = REGISTRY.read().unwrap();
    if let Some(id) = runtime_id {
        if let Some(state) = guard.per_runtime.get(&id) {
            f(state)
        } else {
            f(&guard.base)
        }
    } else {
        f(&guard.global)
    }
}

fn with_registry_mut<F, R>(f: F) -> R
where
    F: FnOnce(&mut RegistryState) -> R,
{
    let runtime_id = current_runtime_id();
    let mut guard = REGISTRY.write().unwrap();
    if let Some(id) = runtime_id {
        if !guard.per_runtime.contains_key(&id) {
            let base = guard.base.clone();
            guard.per_runtime.insert(id, base);
        }
        let state = guard
            .per_runtime
            .get_mut(&id)
            .expect("runtime registry missing");
        f(state)
    } else {
        f(&mut guard.global)
    }
}

impl RegistryState {
    fn new() -> Self {
        Self {
            entries: StdHashMap::new(),
        }
    }

    fn install_primitives(&mut self) {
        for (name, doc) in primitive_definitions() {
            let entry = TypeEntry::Primitive(PrimitiveMeta {
                namespace: CORE_NS.into(),
                name: name.into(),
                doc: doc.map(|s| s.to_string()),
            });
            self.entries.insert(format!("{}::{}", CORE_NS, name), entry);
        }
        self.install_builtin_type_overrides();
    }

    fn install_builtin_type_overrides(&mut self) {
        // Treat Bool as a sum type so enum-members can return true/false
        let bool_sum = SumMeta {
            namespace: CORE_NS.into(),
            name: "Bool".into(),
            doc: Some("Boolean value.".into()),
            members: vec![
                format!("{}::Bool::false", CORE_NS),
                format!("{}::Bool::true", CORE_NS),
            ],
            qualified_only: false,
        };
        self.entries
            .insert(format!("{}::Bool", CORE_NS), TypeEntry::Sum(bool_sum));

        // Treat Map as a product (use product kind without fields)
        let map_product = ProductMeta {
            namespace: CORE_NS.into(),
            name: "Map".into(),
            doc: Some("Persistent map.".into()),
            fields: Vec::new(),
            belongs_to: HashSet::new(),
            required_methods: Vec::new(),
        };
        self.entries
            .insert(format!("{}::Map", CORE_NS), TypeEntry::Product(map_product));
    }
}

fn primitive_definitions() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        ("Int", Some("Built-in integer type.")),
        ("Float", Some("Built-in floating point type.")),
        ("Str", Some("UTF-8 string.")),
        ("Bool", Some("Boolean value.")),
        ("Nil", Some("nil value.")),
        ("List", Some("Persistent list.")),
        ("Vector", Some("Persistent vector.")),
        ("Seq", Some("Lazy single-pass sequence.")),
        ("Map", Some("Persistent map.")),
        ("Set", Some("Persistent set.")),
        ("Symbol", Some("Symbol or keyword.")),
        ("Regex", Some("Regular expression literal.")),
        ("Duration", Some("Duration literal.")),
        ("Function", Some("Callable function or lambda.")),
        ("Atom", Some("Atom reference type.")),
        ("Chan", Some("Channel.")),
        ("Promise", Some("Promise.")),
        ("Task", Some("Async task.")),
        ("Future", Some("Future value.")),
        ("Agent", Some("Agent reference.")),
        ("Foreign", Some("Foreign value from host runtime.")),
    ]
}

// Helper to normalize legacy "String" to new "Str".
fn canonicalize_type_name(name: &str) -> String {
    let name = name.strip_prefix(':').unwrap_or(name);
    if name == "String" {
        return "Str".into();
    }
    if let Some(prefix) = name.strip_suffix("::String") {
        return format!("{}::Str", prefix);
    }
    if let Some(stripped) = name.strip_prefix("clove::") {
        return stripped.to_string();
    }
    name.to_string()
}

pub fn register_product(meta: ProductMeta) -> Result<(), CloveError> {
    let fqn = format!("{}::{}", meta.namespace, meta.name);
    with_registry_mut(move |guard| {
        if guard.entries.contains_key(&fqn) {
            return Err(CloveError::runtime(format!(
                "type '{}' is already defined",
                fqn
            )));
        }
        guard.entries.insert(fqn.clone(), TypeEntry::Product(meta));
        Ok(())
    })
}

pub fn register_sum(meta: SumMeta) -> Result<(), CloveError> {
    let fqn = format!("{}::{}", meta.namespace, meta.name);
    with_registry_mut(move |guard| {
        if guard.entries.contains_key(&fqn) {
            return Err(CloveError::runtime(format!(
                "type '{}' is already defined",
                fqn
            )));
        }
        guard.entries.insert(fqn.clone(), TypeEntry::Sum(meta));
        Ok(())
    })
}

pub fn register_alias(meta: AliasMeta) -> Result<(), CloveError> {
    let fqn = format!("{}::{}", meta.namespace, meta.name);
    with_registry_mut(move |guard| {
        if guard.entries.contains_key(&fqn) {
            return Err(CloveError::runtime(format!(
                "type '{}' is already defined",
                fqn
            )));
        }
        guard.entries.insert(fqn.clone(), TypeEntry::Alias(meta));
        Ok(())
    })
}

pub fn add_product_membership(product: &str, enum_name: &str) {
    with_registry_mut(|guard| {
        if let Some(TypeEntry::Product(meta)) = guard.entries.get_mut(product) {
            meta.belongs_to.insert(enum_name.to_string());
        }
    });
}

pub fn get_type_entry(name: &str) -> Option<TypeEntry> {
    let canonical = canonicalize_type_name(name);
    with_registry(|guard| guard.entries.get(&canonical).cloned())
}

pub fn has_type(name: &str) -> bool {
    let canonical = canonicalize_type_name(name);
    with_registry(|guard| guard.entries.contains_key(&canonical))
}

pub fn list_all_types() -> Vec<String> {
    with_registry(|guard| guard.entries.keys().cloned().collect())
}

pub fn list_types_in_namespace(ns: &str) -> Vec<String> {
    with_registry(|guard| {
        let mut names: Vec<String> = guard
            .entries
            .values()
            .filter(|entry| entry.namespace() == ns)
            .map(|entry| entry.name().to_string())
            .collect();
        names.sort();
        names
    })
}

pub fn list_aliases() -> Vec<(String, AliasMeta)> {
    with_registry(|guard| {
        guard
            .entries
            .iter()
            .filter_map(|(name, entry)| match entry {
                TypeEntry::Alias(meta) => Some((name.clone(), meta.clone())),
                _ => None,
            })
            .collect()
    })
}

pub fn clear_runtime(runtime_id: usize) {
    let mut guard = REGISTRY.write().unwrap();
    guard.per_runtime.remove(&runtime_id);
}

pub fn resolve_alias_name(name: &str, current_ns: Option<&str>) -> Option<String> {
    let canonical = canonicalize_type_name(name);
    if canonical.contains("::") {
        let entry = get_type_entry(&canonical)?;
        return matches!(entry, TypeEntry::Alias(_)).then_some(canonical);
    }
    if let Some(ns) = current_ns {
        let scoped = format!("{}::{}", ns, canonical);
        if let Some(TypeEntry::Alias(_)) = get_type_entry(&scoped) {
            return Some(scoped);
        }
    }
    let mut found = None;
    for (name, _meta) in list_aliases() {
        if let Some(local) = name.rsplit_once("::").map(|(_, local)| local) {
            if local == canonical {
                if found.is_some() {
                    return None;
                }
                found = Some(name);
            }
        }
    }
    found
}

pub fn describe(name: &str) -> Option<Value> {
    let name = canonicalize_type_name(name);
    with_registry(|guard| {
        let entry = guard.entries.get(&name)?;
        Some(match entry {
            TypeEntry::Primitive(meta) => describe_primitive(meta, &name),
            TypeEntry::Product(meta) => describe_product(meta, &name),
            TypeEntry::Sum(meta) => describe_sum(meta, &name),
            TypeEntry::Alias(meta) => describe_alias(meta, &name),
        })
    })
}

pub fn describe_value_arg(arg: &Value, current_ns: Option<&str>) -> Result<Value, CloveError> {
    let (mut type_name, wants_fn_type) = match arg {
        Value::Symbol(sym) => (sym.clone(), false),
        other => (type_of_value(other), is_function_value(other)),
    };
    if !type_name.contains("::") {
        if let Some(ns) = current_ns {
            type_name = format!("{}::{}", ns, type_name);
        }
    }
    let mut described = describe(&type_name)
        .ok_or_else(|| CloveError::runtime(format!("unknown type '{}'", type_name)))?;
    if wants_fn_type {
        if let Some(sig) = crate::fn_type::fn_type_string(arg) {
            if let Value::Map(map) = &mut described {
                map.insert(Key::Keyword("fn-type".into()), Value::String(sig));
            }
        }
    }
    Ok(described)
}

fn is_function_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. }
    )
}

fn describe_common(
    namespace: &str,
    name: &str,
    kind: &str,
    doc: Option<&String>,
) -> HashMap<Key, Value> {
    let mut map = HashMap::new();
    map.insert(
        Key::Keyword("kind".into()),
        Value::Symbol(format!(":{}", kind)),
    );
    map.insert(
        Key::Keyword("ns".into()),
        Value::Symbol(format!(":{}", namespace)),
    );
    map.insert(
        Key::Keyword("name".into()),
        Value::Symbol(format!(":{}", name)),
    );
    if let Some(doc) = doc {
        map.insert(Key::Keyword("doc".into()), Value::String(doc.clone()));
    }
    map
}

fn format_type_name(name: &str, current_ns: Option<&str>) -> String {
    if let Some(stripped) = name.strip_prefix("clove::core::") {
        return stripped.to_string();
    }
    if let Some(ns) = current_ns {
        let prefix = format!("{}::", ns);
        if let Some(stripped) = name.strip_prefix(&prefix) {
            return stripped.to_string();
        }
    }
    name.to_string()
}

fn describe_type_kind(kind: &TypeExprKind, current_ns: Option<&str>) -> String {
    match kind {
        TypeExprKind::Int => "Int".into(),
        TypeExprKind::Float => "Float".into(),
        TypeExprKind::Bool => "Bool".into(),
        TypeExprKind::Str => "Str".into(),
        TypeExprKind::Nil => "Nil".into(),
        TypeExprKind::Any => "Any".into(),
        TypeExprKind::Vector(inner) => format!("[{}]", describe_type_kind(inner, current_ns)),
        TypeExprKind::Tuple(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|ty| describe_type_kind(ty, current_ns))
                .collect();
            format!("[{}]", parts.join(" "))
        }
        TypeExprKind::Map(key, value) => format!(
            "{{{} {}}}",
            describe_type_kind(key, current_ns),
            describe_type_kind(value, current_ns)
        ),
        TypeExprKind::Set(inner) => format!("#{{{}}}", describe_type_kind(inner, current_ns)),
        TypeExprKind::Option(inner) => format!("{}?", describe_type_kind(inner, current_ns)),
        TypeExprKind::Mut(inner) => format!("Mut<{}>", describe_type_kind(inner, current_ns)),
        TypeExprKind::Record(fields) => {
            let mut parts: Vec<_> = fields
                .iter()
                .map(|(name, ty)| format!(":{} {}", name, describe_type_kind(ty, current_ns)))
                .collect();
            parts.sort();
            format!("{{{}}}", parts.join(", "))
        }
        TypeExprKind::Union(types) => types
            .iter()
            .map(|ty| describe_type_kind(ty, current_ns))
            .collect::<Vec<_>>()
            .join("|"),
        TypeExprKind::Function { params, rest, ret } => {
            let mut parts: Vec<String> = params
                .iter()
                .map(|ty| describe_type_kind(ty, current_ns))
                .collect();
            if let Some(rest_ty) = rest.as_ref() {
                parts.push("&".into());
                let rest_desc = match rest_ty.as_ref() {
                    TypeExprKind::Vector(inner) => describe_type_kind(inner, current_ns),
                    other => describe_type_kind(other, current_ns),
                };
                parts.push(rest_desc);
            }
            format!(
                "[{}] -> {}",
                parts.join(" "),
                describe_type_kind(ret, current_ns)
            )
        }
        TypeExprKind::Named(name) => format_type_name(name, current_ns),
    }
}

fn type_kind_to_value(kind: &TypeExprKind, current_ns: Option<&str>) -> Value {
    match kind {
        TypeExprKind::Int => Value::Symbol("Int".into()),
        TypeExprKind::Float => Value::Symbol("Float".into()),
        TypeExprKind::Bool => Value::Symbol("Bool".into()),
        TypeExprKind::Str => Value::Symbol("Str".into()),
        TypeExprKind::Nil => Value::Symbol("Nil".into()),
        TypeExprKind::Any => Value::Symbol("Any".into()),
        TypeExprKind::Vector(inner) => {
            Value::Vector(Vector::from(vec![type_kind_to_value(inner, current_ns)]))
        }
        TypeExprKind::Tuple(items) => Value::Vector(Vector::from(
            items
                .iter()
                .map(|ty| type_kind_to_value(ty, current_ns))
                .collect::<Vec<_>>(),
        )),
        TypeExprKind::Record(fields) => {
            let mut map = HashMap::new();
            for (name, ty) in fields {
                map.insert(
                    Key::Keyword(name.clone()),
                    type_kind_to_value(ty, current_ns),
                );
            }
            Value::Map(map)
        }
        TypeExprKind::Named(name) => Value::Symbol(format_type_name(name, current_ns)),
        _ => Value::String(describe_type_kind(kind, current_ns)),
    }
}

fn describe_primitive(meta: &PrimitiveMeta, fqn: &str) -> Value {
    let mut map = describe_common(&meta.namespace, &meta.name, "primitive", meta.doc.as_ref());
    map.insert(
        Key::Keyword("fqn".into()),
        Value::Symbol(format!(":{}", fqn)),
    );
    Value::Map(map)
}

fn describe_product(meta: &ProductMeta, fqn: &str) -> Value {
    let mut map = describe_common(&meta.namespace, &meta.name, "product", meta.doc.as_ref());
    map.insert(
        Key::Keyword("fqn".into()),
        Value::Symbol(format!(":{}", fqn)),
    );
    let mut fields = HashMap::new();
    for field in &meta.fields {
        let key = Key::Keyword(field.name.clone());
        let value = field.schema.to_value(Some(&meta.namespace));
        fields.insert(key, value);
    }
    map.insert(Key::Keyword("fields".into()), Value::Map(fields));
    if !meta.belongs_to.is_empty() {
        let mut belongs: Vec<String> = meta.belongs_to.iter().cloned().collect();
        belongs.sort();
        let values: Vec<Value> = belongs
            .into_iter()
            .map(|name| Value::Symbol(format!(":{}", name)))
            .collect();
        map.insert(
            Key::Keyword("belongs-to".into()),
            Value::Vector(Vector::from(values)),
        );
    }
    Value::Map(map)
}

fn describe_sum(meta: &SumMeta, fqn: &str) -> Value {
    let mut map = describe_common(&meta.namespace, &meta.name, "sum", meta.doc.as_ref());
    map.insert(
        Key::Keyword("fqn".into()),
        Value::Symbol(format!(":{}", fqn)),
    );
    let members: Vec<Value> = meta
        .members
        .iter()
        .cloned()
        .map(|m| Value::Symbol(format!(":{}", m)))
        .collect();
    map.insert(
        Key::Keyword("members".into()),
        Value::Vector(Vector::from(members)),
    );
    Value::Map(map)
}

fn describe_alias(meta: &AliasMeta, fqn: &str) -> Value {
    let mut map = describe_common(&meta.namespace, &meta.name, "alias", meta.doc.as_ref());
    map.insert(
        Key::Keyword("fqn".into()),
        Value::Symbol(format!(":{}", fqn)),
    );
    map.insert(
        Key::Keyword("target".into()),
        type_kind_to_value(&meta.target, Some(&meta.namespace)),
    );
    Value::Map(map)
}

fn matches_type_kind(kind: &TypeExprKind, value: &Value) -> bool {
    let mut seen = HashSet::new();
    matches_type_kind_inner(kind, value, &mut seen)
}

fn matches_type_kind_inner(
    kind: &TypeExprKind,
    value: &Value,
    seen_aliases: &mut HashSet<String>,
) -> bool {
    match kind {
        TypeExprKind::Any => true,
        TypeExprKind::Int => matches!(value, Value::Int(_)),
        TypeExprKind::Float => matches!(value, Value::Float(_)),
        TypeExprKind::Bool => matches!(value, Value::Bool(_)),
        TypeExprKind::Str => matches!(value, Value::String(_)),
        TypeExprKind::Nil => matches!(value, Value::Nil),
        TypeExprKind::Vector(inner) => match value {
            Value::Vector(items) => items
                .iter()
                .all(|item| matches_type_kind_inner(inner, item, seen_aliases)),
            _ => false,
        },
        TypeExprKind::Set(inner) => match value {
            Value::Set(items) => items
                .iter()
                .all(|item| matches_type_kind_inner(inner, item, seen_aliases)),
            Value::SortedSet(items) => items
                .entries
                .iter()
                .all(|item| matches_type_kind_inner(inner, item, seen_aliases)),
            _ => false,
        },
        TypeExprKind::Tuple(items) => match value {
            Value::Vector(values) => {
                if values.len() != items.len() {
                    return false;
                }
                values
                    .iter()
                    .zip(items.iter())
                    .all(|(value, item)| matches_type_kind_inner(item, value, seen_aliases))
            }
            _ => false,
        },
        TypeExprKind::Map(_, _) => matches!(value, Value::Map(_) | Value::SortedMap(_)),
        TypeExprKind::Record(fields) => match value {
            Value::Map(map) => fields.iter().all(|(name, ty)| {
                let key = Key::Keyword(name.clone());
                map.get(&key)
                    .is_some_and(|value| matches_type_kind_inner(ty, value, seen_aliases))
            }),
            _ => false,
        },
        TypeExprKind::Mut(_inner) => matches!(
            value,
            Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)
        ),
        TypeExprKind::Option(inner) => {
            matches!(value, Value::Nil) || matches_type_kind_inner(inner, value, seen_aliases)
        }
        TypeExprKind::Union(types) => types
            .iter()
            .any(|ty| matches_type_kind_inner(ty, value, seen_aliases)),
        TypeExprKind::Function { .. } => is_function_value(value),
        TypeExprKind::Named(name) => {
            let lookup = match name.split('<').next() {
                Some(base) if base.is_empty() => name.as_str(),
                Some(base) => base,
                None => name.as_str(),
            };
            match get_type_entry(lookup) {
                Some(TypeEntry::Primitive(meta)) => {
                    if let Some(primitive) = PrimitiveType::from_symbol(&meta.name) {
                        primitive.matches_value(value)
                    } else {
                        false
                    }
                }
                Some(TypeEntry::Product(_)) | Some(TypeEntry::Sum(_)) => {
                    is_instance_of(value, lookup)
                }
                Some(TypeEntry::Alias(meta)) => {
                    if !seen_aliases.insert(lookup.to_string()) {
                        return false;
                    }
                    let matches = matches_type_kind_inner(&meta.target, value, seen_aliases);
                    seen_aliases.remove(lookup);
                    matches
                }
                None => false,
            }
        }
    }
}

pub fn validate_product_fields(
    type_name: &str,
    data: &HashMap<Key, Value>,
) -> Result<(), CloveError> {
    let type_name = canonicalize_type_name(type_name);
    let meta = with_registry(|guard| match guard.entries.get(&type_name) {
        Some(TypeEntry::Product(prod)) => Some(prod.clone()),
        _ => None,
    })
    .ok_or_else(|| CloveError::runtime(format!("unknown product type '{}'", type_name)))?;
    for field in &meta.fields {
        let key = Key::Keyword(field.name.clone());
        let value = data.get(&key).ok_or_else(|| {
            CloveError::runtime(format!(
                "missing required field :{} for type {}",
                field.name, type_name
            ))
        })?;
        match &field.schema {
            FieldSchema::Primitive(kind) => {
                if !kind.matches_value(value) {
                    return Err(CloveError::runtime(format!(
                        "field :{} expected {} in type {}",
                        field.name,
                        kind.keyword_name(),
                        type_name
                    )));
                }
            }
            FieldSchema::TypeRef(expected) => {
                if !is_instance_of(value, expected) {
                    return Err(CloveError::runtime(format!(
                        "field :{} expected value of type {}",
                        field.name, expected
                    )));
                }
            }
            FieldSchema::TypeExpr(kind) => {
                if !matches_type_kind(kind, value) {
                    let expected = describe_type_kind(kind, None);
                    return Err(CloveError::runtime(format!(
                        "field :{} expected {} in type {}",
                        field.name, expected, type_name
                    )));
                }
            }
        }
    }
    Ok(())
}

pub fn is_instance(type_name: &str, value: &Value) -> bool {
    is_instance_of(value, type_name)
}

fn is_instance_of(value: &Value, type_name: &str) -> bool {
    let expected = canonicalize_type_name(type_name);
    let entry = get_type_entry(&expected);
    if let Some(TypeEntry::Alias(meta)) = entry.as_ref() {
        return matches_type_kind(&meta.target, value);
    }
    match value {
        Value::NativeBuf { ty, .. } => {
            let name = crate::native_buf::native_buf_type_name(*ty);
            expected == format!("native::{}", name)
        }
        Value::Map(map) => {
            let Some(Value::Symbol(tag)) = map.get(&Key::Keyword(TYPE_KEY.to_string())) else {
                return false;
            };
            match entry {
                Some(TypeEntry::Sum(meta)) => meta.members.iter().any(|m| m == tag),
                _ => tag == &expected,
            }
        }
        _ => false,
    }
}

pub fn matches_type_expr(kind: &TypeExprKind, value: &Value) -> bool {
    matches_type_kind(kind, value)
}

pub fn conforms_named(type_name: &str, value: &Value) -> bool {
    let mut seen_aliases = HashSet::new();
    conforms_named_inner(type_name, value, &mut seen_aliases)
}

fn conforms_named_inner(
    type_name: &str,
    value: &Value,
    seen_aliases: &mut HashSet<String>,
) -> bool {
    let expected = canonicalize_type_name(type_name);
    let lookup = match expected.split('<').next() {
        Some(base) if base.is_empty() => expected.as_str(),
        Some(base) => base,
        None => expected.as_str(),
    };
    let entry = get_type_entry(lookup);
    match entry {
        Some(TypeEntry::Primitive(meta)) => PrimitiveType::from_symbol(&meta.name)
            .is_some_and(|primitive| primitive.matches_value(value)),
        Some(TypeEntry::Product(_)) => conforms_product_value(lookup, value),
        Some(TypeEntry::Sum(meta)) => conforms_sum_value(&meta, value),
        Some(TypeEntry::Alias(meta)) => {
            if !seen_aliases.insert(lookup.to_string()) {
                return false;
            }
            let matches = conforms_type_kind_inner(&meta.target, value, seen_aliases);
            seen_aliases.remove(lookup);
            matches
        }
        None => false,
    }
}

fn conforms_product_value(type_name: &str, value: &Value) -> bool {
    if !is_instance_of(value, type_name) {
        return false;
    }
    match value {
        Value::Map(map) => {
            if validate_product_fields(type_name, map).is_err() {
                return false;
            }
            validate_required_methods(type_name, map)
        }
        _ => false,
    }
}

fn conforms_sum_value(meta: &SumMeta, value: &Value) -> bool {
    let map = match value {
        Value::Map(map) => map,
        _ => return false,
    };
    let Some(Value::Symbol(tag)) = map.get(&Key::Keyword(TYPE_KEY.to_string())) else {
        return false;
    };
    if !meta.members.iter().any(|member| member == tag) {
        return false;
    }
    match get_type_entry(tag) {
        Some(TypeEntry::Product(_)) => {
            if validate_product_fields(tag, map).is_err() {
                return false;
            }
            validate_required_methods(tag, map)
        }
        _ => true,
    }
}

fn validate_required_methods(type_name: &str, data: &HashMap<Key, Value>) -> bool {
    let type_name = canonicalize_type_name(type_name);
    let meta = match get_type_entry(&type_name) {
        Some(TypeEntry::Product(meta)) => meta,
        _ => return false,
    };
    if meta.required_methods.is_empty() {
        return true;
    }
    for required in &meta.required_methods {
        let key = Key::Keyword(required.name.clone());
        let value = match data.get(&key) {
            Some(value) => value,
            None => return false,
        };
        if !is_callable_value(value) {
            return false;
        }
        if !is_method_value(value) {
            return false;
        }
        if let Some(expected) = &required.sig {
            let actual = match signature_from_value(value) {
                Some(actual) => actual,
                None => return false,
            };
            if !method_signature_matches(expected, &actual) {
                return false;
            }
        }
    }
    true
}

fn is_callable_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Func(_)
            | Value::Partial { .. }
            | Value::Compose { .. }
            | Value::Lambda { .. }
            | Value::MultiLambda { .. }
            | Value::ForeignCallable { .. }
    ) || matches!(value, Value::Symbol(sym) if sym.starts_with(':'))
}

fn is_method_value(value: &Value) -> bool {
    match meta_lookup(value) {
        Value::Map(map) => matches!(
            map.get(&Key::Keyword("is-method".into())),
            Some(Value::Bool(true))
        ),
        _ => false,
    }
}

fn signature_from_value(value: &Value) -> Option<FnSigSpec> {
    match value {
        Value::Lambda {
            inferred_type: Some(kind),
            ..
        }
        | Value::MultiLambda {
            inferred_type: Some(kind),
            ..
        } => signature_from_kind(kind),
        _ => None,
    }
}

fn signature_from_kind(kind: &TypeExprKind) -> Option<FnSigSpec> {
    match kind {
        TypeExprKind::Function { params, rest, ret } => Some(FnSigSpec {
            params: params.clone(),
            rest: rest.as_ref().map(|ty| (**ty).clone()),
            ret: (**ret).clone(),
        }),
        _ => None,
    }
}

fn method_signature_matches(expected: &FnSigSpec, actual: &FnSigSpec) -> bool {
    let mut actual_params = actual.params.clone();
    if actual_params.is_empty() {
        return false;
    }
    actual_params.remove(0); // drop self
    if expected.params.len() != actual_params.len() {
        return false;
    }
    if expected.rest.is_some() != actual.rest.is_some() {
        return false;
    }
    for (expected_ty, actual_ty) in expected.params.iter().zip(actual_params.iter()) {
        if !expected_is_any(expected_ty) && expected_ty != actual_ty {
            return false;
        }
    }
    if let (Some(expected_rest), Some(actual_rest)) = (&expected.rest, &actual.rest) {
        if !expected_is_any(expected_rest) && expected_rest != actual_rest {
            return false;
        }
    }
    if !expected_is_any(&expected.ret) && expected.ret != actual.ret {
        return false;
    }
    true
}

fn expected_is_any(kind: &TypeExprKind) -> bool {
    matches!(kind, TypeExprKind::Any)
}

fn conforms_type_kind_inner(
    kind: &TypeExprKind,
    value: &Value,
    seen_aliases: &mut HashSet<String>,
) -> bool {
    match kind {
        TypeExprKind::Any => true,
        TypeExprKind::Int => matches!(value, Value::Int(_)),
        TypeExprKind::Float => matches!(value, Value::Float(_)),
        TypeExprKind::Bool => matches!(value, Value::Bool(_)),
        TypeExprKind::Str => matches!(value, Value::String(_)),
        TypeExprKind::Nil => matches!(value, Value::Nil),
        TypeExprKind::Vector(inner) => match value {
            Value::Vector(items) => items
                .iter()
                .all(|item| conforms_type_kind_inner(inner, item, seen_aliases)),
            _ => false,
        },
        TypeExprKind::Set(inner) => match value {
            Value::Set(items) => items
                .iter()
                .all(|item| conforms_type_kind_inner(inner, item, seen_aliases)),
            Value::SortedSet(items) => items
                .entries
                .iter()
                .all(|item| conforms_type_kind_inner(inner, item, seen_aliases)),
            _ => false,
        },
        TypeExprKind::Tuple(items) => match value {
            Value::Vector(values) => {
                if values.len() != items.len() {
                    return false;
                }
                values
                    .iter()
                    .zip(items.iter())
                    .all(|(value, item)| conforms_type_kind_inner(item, value, seen_aliases))
            }
            _ => false,
        },
        TypeExprKind::Map(_, _) => matches!(value, Value::Map(_) | Value::SortedMap(_)),
        TypeExprKind::Record(fields) => match value {
            Value::Map(map) => fields.iter().all(|(name, ty)| {
                let key = Key::Keyword(name.clone());
                map.get(&key)
                    .is_some_and(|value| conforms_type_kind_inner(ty, value, seen_aliases))
            }),
            _ => false,
        },
        TypeExprKind::Mut(_inner) => matches!(
            value,
            Value::MutVector(_) | Value::MutMap(_) | Value::MutSet(_)
        ),
        TypeExprKind::Option(inner) => {
            matches!(value, Value::Nil) || conforms_type_kind_inner(inner, value, seen_aliases)
        }
        TypeExprKind::Union(types) => types
            .iter()
            .any(|ty| conforms_type_kind_inner(ty, value, seen_aliases)),
        TypeExprKind::Function { .. } => is_function_value(value),
        TypeExprKind::Named(name) => {
            let lookup = match name.split('<').next() {
                Some(base) if base.is_empty() => name.as_str(),
                Some(base) => base,
                None => name.as_str(),
            };
            conforms_named_inner(lookup, value, seen_aliases)
        }
    }
}

pub fn add_type_tag(mut data: HashMap<Key, Value>, type_name: &str) -> HashMap<Key, Value> {
    let type_name = canonicalize_type_name(type_name);
    data.insert(
        Key::Keyword(TYPE_KEY.to_string()),
        Value::Symbol(type_name.to_string()),
    );
    data
}

pub fn primitive_type_name(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "Int",
        Value::Float(_) => "Float",
        Value::String(_) => "Str",
        Value::Bool(_) => "Bool",
        Value::Nil => "Nil",
        Value::List(_) => "List",
        Value::Vector(_) => "Vector",
        Value::Seq(_) => "Seq",
        Value::Map(_) => "Map",
        Value::SortedMap(_) => "SortedMap",
        Value::Set(_) => "Set",
        Value::SortedSet(_) => "SortedSet",
        Value::MutVector(_) => "Vector",
        Value::MutMap(_) => "Map",
        Value::MutSet(_) => "Set",
        Value::TransientVector(_) => "TransientVector",
        Value::TransientMap(_) => "TransientMap",
        Value::TransientSet(_) => "TransientSet",
        Value::Regex(_) => "Regex",
        Value::Duration(_) => "Duration",
        Value::Func(_)
        | Value::Partial { .. }
        | Value::Compose { .. }
        | Value::Lambda { .. }
        | Value::MultiLambda { .. }
        | Value::ForeignCallable { .. } => "Function",
        Value::Atom(_) => "Atom",
        Value::Chan(_) => "Chan",
        Value::Promise(_) => "Promise",
        Value::Task(_) => "Task",
        Value::Future(_) => "Future",
        Value::Agent(_) => "Agent",
        Value::Delay(_) => "Delay",
        Value::Symbol(_) => "Symbol",
        Value::Foreign(_) => "Foreign",
        Value::NativeBuf { ty, .. } => crate::native_buf::native_buf_type_name(*ty),
    }
}

pub fn type_of_value(value: &Value) -> String {
    match value {
        Value::NativeBuf { ty, .. } => {
            return format!(":native::{}", crate::native_buf::native_buf_type_name(*ty));
        }
        Value::Map(map) => {
            if let Some(Value::Symbol(tag)) = map.get(&Key::Keyword(TYPE_KEY.to_string())) {
                return canonicalize_type_name(tag);
            }
        }
        Value::MutMap(handle) => {
            let map = handle.lock().unwrap_or_else(|e| e.into_inner());
            if let Some(Value::Symbol(tag)) = map.get(&Key::Keyword(TYPE_KEY.to_string())) {
                return canonicalize_type_name(tag);
            }
        }
        Value::SortedMap(map) => {
            if let Some(Value::Symbol(tag)) = map.entries.iter().find_map(|(k, v)| match k {
                Key::Keyword(name) if name == TYPE_KEY => Some(v),
                _ => None,
            }) {
                return canonicalize_type_name(tag);
            }
        }
        _ => {}
    }
    format!(":{}::{}", CORE_NS, primitive_type_name(value))
}

impl PrimitiveType {
    pub fn from_keyword(name: &str) -> Option<Self> {
        match name {
            "int" => Some(Self::Int),
            "float" => Some(Self::Float),
            "str" | "string" => Some(Self::Str),
            "bool" => Some(Self::Bool),
            "nil" => Some(Self::Nil),
            "list" => Some(Self::List),
            "vector" => Some(Self::Vector),
            "seq" => Some(Self::Seq),
            "map" => Some(Self::Map),
            "set" => Some(Self::Set),
            "symbol" => Some(Self::Symbol),
            "regex" => Some(Self::Regex),
            "duration" => Some(Self::Duration),
            "function" => Some(Self::Function),
            "atom" => Some(Self::Atom),
            "chan" => Some(Self::Chan),
            "promise" => Some(Self::Promise),
            "task" => Some(Self::Task),
            "future" => Some(Self::Future),
            "agent" => Some(Self::Agent),
            "foreign" => Some(Self::Foreign),
            _ => None,
        }
    }

    pub fn from_symbol(name: &str) -> Option<Self> {
        let base = name.rsplit("::").next().unwrap_or(name);
        match base {
            "Int" => Some(Self::Int),
            "Float" => Some(Self::Float),
            "Str" | "String" => Some(Self::Str),
            "Bool" => Some(Self::Bool),
            "Nil" => Some(Self::Nil),
            "List" => Some(Self::List),
            "Vector" => Some(Self::Vector),
            "Seq" => Some(Self::Seq),
            "Map" => Some(Self::Map),
            "Set" => Some(Self::Set),
            "Symbol" => Some(Self::Symbol),
            "Regex" => Some(Self::Regex),
            "Duration" => Some(Self::Duration),
            "Function" => Some(Self::Function),
            "Atom" => Some(Self::Atom),
            "Chan" => Some(Self::Chan),
            "Promise" => Some(Self::Promise),
            "Task" => Some(Self::Task),
            "Future" => Some(Self::Future),
            "Agent" => Some(Self::Agent),
            "Foreign" => Some(Self::Foreign),
            _ => None,
        }
    }

    fn keyword_name(&self) -> &'static str {
        match self {
            PrimitiveType::Int => ":int",
            PrimitiveType::Float => ":float",
            PrimitiveType::Str => ":str",
            PrimitiveType::Bool => ":bool",
            PrimitiveType::Nil => ":nil",
            PrimitiveType::List => ":list",
            PrimitiveType::Vector => ":vector",
            PrimitiveType::Seq => ":seq",
            PrimitiveType::Map => ":map",
            PrimitiveType::Set => ":set",
            PrimitiveType::Symbol => ":symbol",
            PrimitiveType::Regex => ":regex",
            PrimitiveType::Duration => ":duration",
            PrimitiveType::Function => ":function",
            PrimitiveType::Atom => ":atom",
            PrimitiveType::Chan => ":chan",
            PrimitiveType::Promise => ":promise",
            PrimitiveType::Task => ":task",
            PrimitiveType::Future => ":future",
            PrimitiveType::Agent => ":agent",
            PrimitiveType::Foreign => ":foreign",
        }
    }

    fn matches_value(&self, value: &Value) -> bool {
        matches!(
            (self, value),
            (PrimitiveType::Int, Value::Int(_))
                | (PrimitiveType::Float, Value::Float(_))
                | (PrimitiveType::Str, Value::String(_))
                | (PrimitiveType::Bool, Value::Bool(_))
                | (PrimitiveType::Nil, Value::Nil)
                | (PrimitiveType::List, Value::List(_))
                | (PrimitiveType::Vector, Value::Vector(_))
                | (PrimitiveType::Vector, Value::MutVector(_))
                | (PrimitiveType::Seq, Value::Seq(_))
                | (PrimitiveType::Map, Value::Map(_))
                | (PrimitiveType::Map, Value::MutMap(_))
                | (PrimitiveType::Set, Value::Set(_))
                | (PrimitiveType::Set, Value::MutSet(_))
                | (PrimitiveType::Symbol, Value::Symbol(_))
                | (PrimitiveType::Regex, Value::Regex(_))
                | (PrimitiveType::Duration, Value::Duration(_))
                | (PrimitiveType::Function, Value::Func(_))
                | (PrimitiveType::Function, Value::Partial { .. })
                | (PrimitiveType::Function, Value::Compose { .. })
                | (PrimitiveType::Function, Value::Lambda { .. })
                | (PrimitiveType::Function, Value::MultiLambda { .. })
                | (PrimitiveType::Function, Value::ForeignCallable { .. })
                | (PrimitiveType::Atom, Value::Atom(_))
                | (PrimitiveType::Chan, Value::Chan(_))
                | (PrimitiveType::Promise, Value::Promise(_))
                | (PrimitiveType::Task, Value::Task(_))
                | (PrimitiveType::Future, Value::Future(_))
                | (PrimitiveType::Agent, Value::Agent(_))
                | (PrimitiveType::Foreign, Value::Foreign(_))
        )
    }
}
