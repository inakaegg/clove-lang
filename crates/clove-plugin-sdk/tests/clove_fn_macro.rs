//! `#[clove_fn]` の生成コードが実際にコンパイルされ、inventory とメタJSONへ載ることを確認する。
//!
//! リポジトリ内にプラグインの実装がないため、このテストがないとマクロの展開結果は
//! どのビルドでもコンパイルされない。

use std::ffi::c_void;

use clove_plugin_sdk::{
    clove_fn, clove_opaque_type, meta_json, EnvHandle, FnSpec, HostApiV1, TypeSpec, ValueHandle,
};

#[clove_fn(
    name = "test/echo",
    ty = "Any -> Any",
    doc = "引数をそのまま返す",
    arity = "1..1"
)]
unsafe extern "C" fn test_echo(
    _host: *const HostApiV1,
    _env: EnvHandle,
    argc: usize,
    argv: *const ValueHandle,
    _user_data: *mut c_void,
) -> ValueHandle {
    if argc == 0 {
        return std::ptr::null_mut();
    }
    unsafe { *argv }
}

#[clove_fn(
    name = "test/add",
    ty = "Int -> Int -> Int",
    overloads("Int -> Int -> Int", "Float -> Float -> Float"),
    arity = "2..2"
)]
unsafe extern "C" fn test_add(
    _host: *const HostApiV1,
    _env: EnvHandle,
    _argc: usize,
    _argv: *const ValueHandle,
    _user_data: *mut c_void,
) -> ValueHandle {
    std::ptr::null_mut()
}

clove_opaque_type!("test/Handle");

fn spec(name: &str) -> &'static FnSpec {
    clove_plugin_sdk::inventory::iter::<FnSpec>
        .into_iter()
        .find(|spec| spec.name == name)
        .unwrap_or_else(|| panic!("{name} が inventory に登録されていない"))
}

#[test]
fn clove_fn_registers_spec_in_inventory() {
    let echo = spec("test/echo");
    assert_eq!(echo.ty, "Any -> Any");
    assert_eq!(echo.doc, Some("引数をそのまま返す"));
    assert_eq!((echo.arity_min, echo.arity_max), (1, 1));
    assert!(echo.overloads.is_empty());

    let add = spec("test/add");
    assert_eq!((add.arity_min, add.arity_max), (2, 2));
    assert_eq!(
        add.overloads,
        &["Int -> Int -> Int", "Float -> Float -> Float"]
    );
}

#[test]
fn register_is_an_unsafe_fn_pointer() {
    // FnSpec::register の safety契約が型に出ていること。safe fn ポインタへは代入できない。
    let _: unsafe fn(&HostApiV1, EnvHandle) -> bool = spec("test/echo").register;
}

#[test]
fn clove_opaque_type_registers_type_spec() {
    let found = clove_plugin_sdk::inventory::iter::<TypeSpec>
        .into_iter()
        .any(|spec| spec.name == "test/Handle");
    assert!(found, "test/Handle が inventory に登録されていない");
}

#[test]
fn meta_json_lists_registered_fns() {
    let json = meta_json();
    let meta: serde_json::Value = serde_json::from_str(&json).expect("メタJSONがパースできる");

    assert_eq!(meta["schema"], 1);

    let fns = meta["fns"].as_array().expect("fns は配列");
    let echo = fns
        .iter()
        .find(|f| f["name"] == "test/echo")
        .expect("test/echo がメタJSONにある");
    assert_eq!(echo["type"], "Any -> Any");
    assert_eq!(echo["doc"], "引数をそのまま返す");

    let add = fns
        .iter()
        .find(|f| f["name"] == "test/add")
        .expect("test/add がメタJSONにある");
    // overloads があるときは type を出さず、overloads をソートして出す。
    assert!(add.get("type").is_none());
    assert_eq!(
        add["overloads"],
        serde_json::json!(["Float -> Float -> Float", "Int -> Int -> Int"])
    );

    // fns は名前順。
    let names: Vec<&str> = fns.iter().filter_map(|f| f["name"].as_str()).collect();
    let mut sorted = names.clone();
    sorted.sort_unstable();
    assert_eq!(names, sorted);
}
