use std::ffi::CString;

use once_cell::sync::OnceCell;
use serde::Serialize;

pub use clove_plugin_api::*;
pub use clove_plugin_sdk_macros::clove_fn;
pub use inventory;

#[derive(Clone, Copy, Debug)]
pub enum TypeKind {
    Opaque,
}

#[derive(Clone, Copy, Debug)]
pub struct TypeSpec {
    pub name: &'static str,
    pub kind: TypeKind,
    pub doc: Option<&'static str>,
}

impl TypeSpec {
    pub const fn opaque(name: &'static str) -> Self {
        Self {
            name,
            kind: TypeKind::Opaque,
            doc: None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct FnSpec {
    pub name: &'static str,
    pub ty: &'static str,
    pub overloads: &'static [&'static str],
    pub doc: Option<&'static str>,
    pub arity_min: u8,
    pub arity_max: u8,
    pub register: fn(&HostApiV1, EnvHandle) -> bool,
}

inventory::collect!(FnSpec);
inventory::collect!(TypeSpec);

pub fn register_all(host: &HostApiV1, env: EnvHandle) -> bool {
    for spec in inventory::iter::<FnSpec> {
        if !(spec.register)(host, env) {
            return false;
        }
    }
    true
}

pub fn register_fn(
    host: &HostApiV1,
    env: EnvHandle,
    name: &str,
    arity_min: u8,
    arity_max: u8,
    func: BuiltinFn,
) -> bool {
    let ok = unsafe {
        (host.define_fn_utf8)(
            env,
            name.as_ptr(),
            name.len(),
            arity_min as usize,
            arity_max as usize,
            func,
            std::ptr::null_mut(),
        )
    };
    ok != 0
}

#[derive(Debug, Serialize)]
struct MetaFile {
    schema: u32,
    #[serde(default)]
    types: Vec<MetaType>,
    #[serde(default)]
    fns: Vec<MetaFn>,
}

#[derive(Debug, Serialize)]
struct MetaType {
    name: String,
    kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
}

#[derive(Debug, Serialize)]
struct MetaFn {
    name: String,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    type_expr: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    overloads: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
}

fn build_meta_file() -> MetaFile {
    let mut types: Vec<MetaType> = inventory::iter::<TypeSpec>
        .into_iter()
        .map(|spec| MetaType {
            name: spec.name.to_string(),
            kind: match spec.kind {
                TypeKind::Opaque => "opaque".to_string(),
            },
            doc: spec.doc.map(|doc| doc.to_string()),
        })
        .collect();
    types.sort_by(|a, b| a.name.cmp(&b.name));

    let mut fns: Vec<MetaFn> = inventory::iter::<FnSpec>
        .into_iter()
        .map(|spec| {
            let mut overloads = spec
                .overloads
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>();
            if overloads.len() > 1 {
                overloads.sort();
            }
            let type_expr = if overloads.is_empty() {
                Some(spec.ty.to_string())
            } else {
                None
            };
            MetaFn {
                name: spec.name.to_string(),
                type_expr,
                overloads,
                doc: spec.doc.map(|doc| doc.to_string()),
            }
        })
        .collect();
    fns.sort_by(|a, b| a.name.cmp(&b.name));

    MetaFile {
        schema: 1,
        types,
        fns,
    }
}

pub fn meta_json() -> String {
    let meta = build_meta_file();
    serde_json::to_string_pretty(&meta).unwrap_or_else(|_| "{}".to_string())
}

pub fn meta_json_cstr() -> &'static CString {
    static META: OnceCell<CString> = OnceCell::new();
    META.get_or_init(|| {
        let json = meta_json();
        CString::new(json).unwrap_or_else(|_| CString::new("{}").expect("cstr"))
    })
}

#[macro_export]
macro_rules! clove_opaque_type {
    ($name:expr) => {
        $crate::inventory::submit! {
            $crate::TypeSpec::opaque($name)
        }
    };
}

#[macro_export]
macro_rules! clove_plugin_exports_v1 {
    () => {
        #[no_mangle]
        pub extern "C" fn clove_plugin_init_v1(
            host: *const ::clove_plugin_sdk::HostApiV1,
            env: ::clove_plugin_sdk::EnvHandle,
        ) -> i32 {
            let host = match unsafe { host.as_ref() } {
                Some(host) => host,
                None => return 1,
            };
            if host.api_version != ::clove_plugin_sdk::CLOVE_PLUGIN_API_VERSION
                || host.size < std::mem::size_of::<::clove_plugin_sdk::HostApiV1>()
            {
                return 2;
            }
            if !$crate::register_all(host, env) {
                return 3;
            }
            0
        }

        #[no_mangle]
        pub extern "C" fn clove_plugin_meta_json_v1() -> *const ::std::os::raw::c_char {
            $crate::meta_json_cstr().as_ptr()
        }
    };
}
