use std::ffi::c_void;

pub const CLOVE_PLUGIN_API_VERSION: u32 = 1;
pub const CLOVE_ARITY_UNBOUNDED: usize = usize::MAX;

pub type EnvHandle = *mut c_void;
pub type ValueHandle = *mut c_void;

// 0 = false, 1 = true
pub type Bool = i32;

pub type BuiltinFn = unsafe extern "C" fn(
    host: *const HostApiV1,
    env: EnvHandle,
    argc: usize,
    argv: *const ValueHandle,
    user_data: *mut c_void,
) -> ValueHandle;

pub type PluginInitV1 = unsafe extern "C" fn(host: *const HostApiV1, env: EnvHandle) -> i32;

pub type DefineFnUtf8 = unsafe extern "C" fn(
    env: EnvHandle,
    name_ptr: *const u8,
    name_len: usize,
    arity_min: usize,
    arity_max: usize,
    func: BuiltinFn,
    user_data: *mut c_void,
) -> Bool;

pub type MakeNil = unsafe extern "C" fn(env: EnvHandle) -> ValueHandle;
pub type MakeBool = unsafe extern "C" fn(env: EnvHandle, value: Bool) -> ValueHandle;
pub type MakeInt = unsafe extern "C" fn(env: EnvHandle, value: i64) -> ValueHandle;
pub type MakeFloat = unsafe extern "C" fn(env: EnvHandle, value: f64) -> ValueHandle;
pub type MakeStringUtf8 =
    unsafe extern "C" fn(env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle;
pub type MakeBytes =
    unsafe extern "C" fn(env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle;
pub type MakeKeywordUtf8 =
    unsafe extern "C" fn(env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle;

pub type MakeVec = unsafe extern "C" fn(env: EnvHandle) -> ValueHandle;
pub type VecPush = unsafe extern "C" fn(env: EnvHandle, vec: ValueHandle, val: ValueHandle) -> Bool;
pub type VecLen = unsafe extern "C" fn(env: EnvHandle, vec: ValueHandle, out: *mut usize) -> Bool;
pub type VecGet = unsafe extern "C" fn(env: EnvHandle, vec: ValueHandle, idx: usize) -> ValueHandle;
pub type MakeMap = unsafe extern "C" fn(env: EnvHandle) -> ValueHandle;
pub type MapPut = unsafe extern "C" fn(
    env: EnvHandle,
    map: ValueHandle,
    key: ValueHandle,
    val: ValueHandle,
) -> Bool;
pub type MapGet =
    unsafe extern "C" fn(env: EnvHandle, map: ValueHandle, key: ValueHandle) -> ValueHandle;

pub type AsInt = unsafe extern "C" fn(env: EnvHandle, v: ValueHandle, out: *mut i64) -> Bool;
pub type AsFloat = unsafe extern "C" fn(env: EnvHandle, v: ValueHandle, out: *mut f64) -> Bool;
pub type AsBool = unsafe extern "C" fn(env: EnvHandle, v: ValueHandle, out: *mut Bool) -> Bool;
pub type AsStringUtf8 = unsafe extern "C" fn(
    env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) -> Bool;
pub type AsSymbolUtf8 = unsafe extern "C" fn(
    env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) -> Bool;
pub type IsNil = unsafe extern "C" fn(env: EnvHandle, v: ValueHandle) -> Bool;
pub type NativeI32BufSlice = unsafe extern "C" fn(
    env: EnvHandle,
    v: ValueHandle,
    out_ptr: *mut *const i32,
    out_len: *mut usize,
) -> Bool;

pub type RaiseRuntimeErrorUtf8 =
    unsafe extern "C" fn(env: EnvHandle, ptr: *const u8, len: usize) -> ValueHandle;

pub type ReaderReadFn =
    unsafe extern "C" fn(ctx: *mut c_void, buf_ptr: *mut u8, buf_len: usize) -> isize;
pub type ReaderCloseFn = Option<unsafe extern "C" fn(ctx: *mut c_void)>;
pub type ReaderErrorFn =
    Option<unsafe extern "C" fn(ctx: *mut c_void, out_ptr: *mut *const u8, out_len: *mut usize)>;
pub type MakeReaderHandle = unsafe extern "C" fn(
    env: EnvHandle,
    read_fn: ReaderReadFn,
    close_fn: ReaderCloseFn,
    error_fn: ReaderErrorFn,
    ctx: *mut c_void,
) -> ValueHandle;

pub type CallValue = unsafe extern "C" fn(
    env: EnvHandle,
    func: ValueHandle,
    argc: usize,
    argv: *const ValueHandle,
    out_ok: *mut Bool,
) -> ValueHandle;

// SAFETY: String pointers are only valid during the call; plugins must not store them.
// SAFETY: v1 does not provide retain/release; plugins must not keep handles beyond returns.
#[repr(C)]
pub struct HostApiV1 {
    pub api_version: u32,
    pub size: usize,
    pub define_fn_utf8: DefineFnUtf8,
    pub make_nil: MakeNil,
    pub make_bool: MakeBool,
    pub make_int: MakeInt,
    pub make_float: MakeFloat,
    pub make_string_utf8: MakeStringUtf8,
    pub make_bytes: MakeBytes,
    pub make_keyword_utf8: MakeKeywordUtf8,
    pub make_vec: MakeVec,
    pub vec_push: VecPush,
    pub vec_len: VecLen,
    pub vec_get: VecGet,
    pub make_map: MakeMap,
    pub map_put: MapPut,
    pub map_get: MapGet,
    pub as_int: AsInt,
    pub as_float: AsFloat,
    pub as_bool: AsBool,
    pub as_string_utf8: AsStringUtf8,
    pub as_symbol_utf8: AsSymbolUtf8,
    pub is_nil: IsNil,
    pub raise_runtime_error_utf8: RaiseRuntimeErrorUtf8,
    pub native_i32buf_slice: NativeI32BufSlice,
    pub make_reader_handle: MakeReaderHandle,
    pub call_value: CallValue,
}
