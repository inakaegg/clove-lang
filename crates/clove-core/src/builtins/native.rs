use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::ast::Vector;

use crate::ast::{FnArity, MainThreadBuf, NativeBufHandle, NativeBufTy, Value};
use crate::builtins::shared::type_mismatch_arg;
use crate::builtins::{def_builtin, err};
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::native_buf::{
    begin_f32buf_guard, begin_i32buf_guard, begin_i64buf_guard, end_f32buf_guard, end_i32buf_guard,
    end_i64buf_guard, expect_f32buf_handle, expect_i32buf_handle, expect_i64buf_handle,
    with_f32buf_mut, with_f32buf_ref, with_i32buf_mut, with_i32buf_ref, with_i64buf_mut,
    with_i64buf_ref, NativeBuf,
};
use crate::profiler;
use crate::runtime::RuntimeCtx;
use crate::type_registry::{self, ProductMeta};
use crate::types::TypeKind;

const NATIVE_BUF_NOTE: &str =
    "Note: Mutable, reference type; unlike Vector it is not persistent, and is intended for FFI/high-performance use.";

pub fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "native::i32buf-new", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_i32buf_new(capacity),
            _ => err("native::i32buf-new expects capacity"),
        }
    });
    def_builtin!(env, "native::i32buf-new-main", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_i32buf_new_main(capacity),
            _ => err("native::i32buf-new-main expects capacity"),
        }
    });
    def_builtin!(env, "native::i32buf-begin", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i32buf_begin(buf),
            _ => err("native::i32buf-begin expects buf"),
        }
    });
    def_builtin!(env, "native::i32buf-end", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i32buf_end(buf),
            _ => err("native::i32buf-end expects buf"),
        }
    });
    def_builtin!(env, "native::i32buf-len", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i32buf_len(buf),
            _ => err("native::i32buf-len expects buf"),
        }
    });
    def_builtin!(
        env,
        "native::i32buf-capacity",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_i32buf_capacity(buf),
            _ => err("native::i32buf-capacity expects buf"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-resize!",
        FnArity::exact(3),
        |args| match args {
            [buf, new_len, fill] => native_i32buf_resize(buf, new_len, fill),
            _ => err("native::i32buf-resize! expects (buf new-len fill)"),
        }
    );
    def_builtin!(env, "native::i32buf-get", FnArity::exact(2), |args| {
        match args {
            [buf, idx] => native_i32buf_get(buf, idx),
            _ => err("native::i32buf-get expects (buf idx)"),
        }
    });
    def_builtin!(
        env,
        "native::i32buf-set!",
        FnArity::exact(3),
        |args| match args {
            [buf, idx, value] => native_i32buf_set(buf, idx, value),
            _ => err("native::i32buf-set! expects (buf idx value)"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-set-range!",
        FnArity::exact(3),
        |args| match args {
            [buf, start, values] => native_i32buf_set_range(buf, start, values),
            _ => err("native::i32buf-set-range! expects (buf start values)"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-fill!",
        FnArity::exact(2),
        |args| match args {
            [buf, value] => native_i32buf_fill(buf, value),
            _ => err("native::i32buf-fill! expects (buf value)"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-fill-xy-step!",
        FnArity::exact(6),
        |args| match args {
            [buf, start_x, start_y, count, step_x, step_y] => {
                native_i32buf_fill_xy_step(buf, start_x, start_y, count, step_x, step_y)
            }
            _ => err(
                "native::i32buf-fill-xy-step! expects (buf start-x start-y count step-x step-y)",
            ),
        }
    );
    def_builtin!(
        env,
        "native::i32buf->vec",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_i32buf_to_vec(buf),
            _ => err("native::i32buf->vec expects buf"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-add!",
        FnArity::range(2, 3),
        |args| match args {
            [buf, src] => native_i32buf_add(buf, src, None),
            [buf, src, scale] => native_i32buf_add(buf, src, Some(scale)),
            _ => err("native::i32buf-add! expects (buf src [scale])"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-wrap-min!",
        FnArity::exact(3),
        |args| match args {
            [buf, min, reset] => native_i32buf_wrap_min(buf, min, reset),
            _ => err("native::i32buf-wrap-min! expects (buf min reset)"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-pack-xy-in-rect!",
        FnArity::exact(7),
        |args| match args {
            [out, xs, ys, min_x, max_x, min_y, max_y] => {
                native_i32buf_pack_xy_in_rect(out, xs, ys, min_x, max_x, min_y, max_y)
            }
            _ => err("native::i32buf-pack-xy-in-rect! expects (out xs ys min-x max-x min-y max-y)"),
        }
    );
    def_builtin!(
        env,
        "native::i32buf-step-pack-xy-in-rect!",
        FnArity::exact(11),
        |args| {
            match args {
            [out, xs, ys, vxs, dt, wrap_min, wrap_reset, min_x, max_x, min_y, max_y] => {
                native_i32buf_step_pack_xy_in_rect(
                    out, xs, ys, vxs, dt, wrap_min, wrap_reset, min_x, max_x, min_y, max_y,
                )
            }
            _ => err(
                "native::i32buf-step-pack-xy-in-rect! expects (out xs ys vxs dt wrap-min wrap-reset min-x max-x min-y max-y)",
            ),
        }
        }
    );

    def_builtin!(env, "native::f32buf-new", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_f32buf_new(capacity),
            _ => err("native::f32buf-new expects capacity"),
        }
    });
    def_builtin!(env, "native::f32buf-new-main", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_f32buf_new_main(capacity),
            _ => err("native::f32buf-new-main expects capacity"),
        }
    });
    def_builtin!(env, "native::f32buf-begin", FnArity::exact(1), |args| {
        match args {
            [buf] => native_f32buf_begin(buf),
            _ => err("native::f32buf-begin expects buf"),
        }
    });
    def_builtin!(env, "native::f32buf-end", FnArity::exact(1), |args| {
        match args {
            [buf] => native_f32buf_end(buf),
            _ => err("native::f32buf-end expects buf"),
        }
    });
    def_builtin!(env, "native::f32buf-len", FnArity::exact(1), |args| {
        match args {
            [buf] => native_f32buf_len(buf),
            _ => err("native::f32buf-len expects buf"),
        }
    });
    def_builtin!(
        env,
        "native::f32buf-capacity",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_f32buf_capacity(buf),
            _ => err("native::f32buf-capacity expects buf"),
        }
    );
    def_builtin!(
        env,
        "native::f32buf-resize!",
        FnArity::exact(3),
        |args| match args {
            [buf, new_len, fill] => native_f32buf_resize(buf, new_len, fill),
            _ => err("native::f32buf-resize! expects (buf new-len fill)"),
        }
    );
    def_builtin!(env, "native::f32buf-get", FnArity::exact(2), |args| {
        match args {
            [buf, idx] => native_f32buf_get(buf, idx),
            _ => err("native::f32buf-get expects (buf idx)"),
        }
    });
    def_builtin!(
        env,
        "native::f32buf-set!",
        FnArity::exact(3),
        |args| match args {
            [buf, idx, value] => native_f32buf_set(buf, idx, value),
            _ => err("native::f32buf-set! expects (buf idx value)"),
        }
    );
    def_builtin!(
        env,
        "native::f32buf-set-range!",
        FnArity::exact(3),
        |args| match args {
            [buf, start, values] => native_f32buf_set_range(buf, start, values),
            _ => err("native::f32buf-set-range! expects (buf start values)"),
        }
    );
    def_builtin!(
        env,
        "native::f32buf-fill!",
        FnArity::exact(2),
        |args| match args {
            [buf, value] => native_f32buf_fill(buf, value),
            _ => err("native::f32buf-fill! expects (buf value)"),
        }
    );
    def_builtin!(
        env,
        "native::f32buf->vec",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_f32buf_to_vec(buf),
            _ => err("native::f32buf->vec expects buf"),
        }
    );

    def_builtin!(env, "native::i64buf-new", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_i64buf_new(capacity),
            _ => err("native::i64buf-new expects capacity"),
        }
    });
    def_builtin!(env, "native::i64buf-new-main", FnArity::exact(1), |args| {
        match args {
            [capacity] => native_i64buf_new_main(capacity),
            _ => err("native::i64buf-new-main expects capacity"),
        }
    });
    def_builtin!(env, "native::i64buf-begin", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i64buf_begin(buf),
            _ => err("native::i64buf-begin expects buf"),
        }
    });
    def_builtin!(env, "native::i64buf-end", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i64buf_end(buf),
            _ => err("native::i64buf-end expects buf"),
        }
    });
    def_builtin!(env, "native::i64buf-len", FnArity::exact(1), |args| {
        match args {
            [buf] => native_i64buf_len(buf),
            _ => err("native::i64buf-len expects buf"),
        }
    });
    def_builtin!(
        env,
        "native::i64buf-capacity",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_i64buf_capacity(buf),
            _ => err("native::i64buf-capacity expects buf"),
        }
    );
    def_builtin!(
        env,
        "native::i64buf-resize!",
        FnArity::exact(3),
        |args| match args {
            [buf, new_len, fill] => native_i64buf_resize(buf, new_len, fill),
            _ => err("native::i64buf-resize! expects (buf new-len fill)"),
        }
    );
    def_builtin!(env, "native::i64buf-get", FnArity::exact(2), |args| {
        match args {
            [buf, idx] => native_i64buf_get(buf, idx),
            _ => err("native::i64buf-get expects (buf idx)"),
        }
    });
    def_builtin!(
        env,
        "native::i64buf-set!",
        FnArity::exact(3),
        |args| match args {
            [buf, idx, value] => native_i64buf_set(buf, idx, value),
            _ => err("native::i64buf-set! expects (buf idx value)"),
        }
    );
    def_builtin!(
        env,
        "native::i64buf-set-range!",
        FnArity::exact(3),
        |args| match args {
            [buf, start, values] => native_i64buf_set_range(buf, start, values),
            _ => err("native::i64buf-set-range! expects (buf start values)"),
        }
    );
    def_builtin!(
        env,
        "native::i64buf->vec",
        FnArity::exact(1),
        |args| match args {
            [buf] => native_i64buf_to_vec(buf),
            _ => err("native::i64buf->vec expects buf"),
        }
    );

    register_native_buf_types();
    register_native_buf_metas();
}

fn register_native_buf_types() {
    let belongs_to = HashSet::new();
    let required_methods = Vec::new();
    let types = [
        ("NativeI32Buf", "Contiguous native i32 buffer."),
        ("NativeF32Buf", "Contiguous native f32 buffer."),
        ("NativeI64Buf", "Contiguous native i64 buffer."),
    ];
    for (name, doc) in types {
        let meta = ProductMeta {
            namespace: "native".into(),
            name: name.into(),
            doc: Some(format!("{}\n{}", doc, NATIVE_BUF_NOTE)),
            fields: Vec::new(),
            belongs_to: belongs_to.clone(),
            required_methods: required_methods.clone(),
        };
        let _ = type_registry::register_product(meta);
    }
}

fn register_native_buf_metas() {
    fn native_i32buf() -> TypeKind {
        TypeKind::Named("clove::native::NativeI32Buf".into())
    }
    fn native_f32buf() -> TypeKind {
        TypeKind::Named("clove::native::NativeF32Buf".into())
    }
    fn native_i64buf() -> TypeKind {
        TypeKind::Named("clove::native::NativeI64Buf".into())
    }
    fn int_type() -> TypeKind {
        TypeKind::Int
    }
    fn float_type() -> TypeKind {
        TypeKind::Float
    }
    fn vec_int() -> TypeKind {
        TypeKind::vector(TypeKind::Int)
    }
    fn vec_float() -> TypeKind {
        TypeKind::vector(TypeKind::Float)
    }

    let mut meta = FnMeta::new("native", "i32buf-new");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a NativeI32Buf with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-new-main");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a thread-affine NativeI32Buf (only the creating thread may access it) with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-begin");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Lock the buffer for repeated operations on this thread; call native::i32buf-end afterwards.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-end");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Release the lock acquired by native::i32buf-begin.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-len");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Return the number of elements in the buffer.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-capacity");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!("Return the buffer capacity.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-resize!");
    meta.arglist.push("[buf new-len fill]".into());
    meta.doc = Some(format!(
        "Resize the buffer length; fill with the provided value if needed.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type(), int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-get");
    meta.arglist.push("[buf idx]".into());
    meta.doc = Some(format!("Get the value at the index.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-set!");
    meta.arglist.push("[buf idx value]".into());
    meta.doc = Some(format!(
        "Update the value at the index.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type(), int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-set-range!");
    meta.arglist.push("[buf start values]".into());
    meta.doc = Some(format!(
        "Copy values (vector/list or native i32 buffer) into the buffer starting at start.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type(), TypeKind::Any],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-fill!");
    meta.arglist.push("[buf value]".into());
    meta.doc = Some(format!(
        "Fill the entire buffer with the value.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-fill-xy-step!");
    meta.arglist
        .push("[buf start-x start-y count step-x step-y]".into());
    meta.doc = Some(format!(
        "Fill the buffer with count (x,y) pairs starting at (start-x,start-y) and stepping by (step-x,step-y). Resizes to count*2.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![
            native_i32buf(),
            int_type(),
            int_type(),
            int_type(),
            int_type(),
            int_type(),
        ],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf->vec");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Copy the buffer into a Vector and return it (for debugging).\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf()],
        rest: None,
        ret_type: vec_int(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-add!");
    meta.arglist.push("[buf src]".into());
    meta.arglist.push("[buf src scale]".into());
    meta.doc = Some(format!(
        "Add src values into the buffer in place. When scale is provided, adds src[i] * scale.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), TypeKind::Any],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), TypeKind::Any, TypeKind::Any],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-wrap-min!");
    meta.arglist.push("[buf min reset]".into());
    meta.doc = Some(format!(
        "Replace values smaller than min with reset.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i32buf(), int_type(), int_type()],
        rest: None,
        ret_type: native_i32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-pack-xy-in-rect!");
    meta.arglist
        .push("[out xs ys min-x max-x min-y max-y]".into());
    meta.doc = Some(format!(
        "Pack (x,y) pairs from xs/ys into out when inside the rectangle; returns the number of values written.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![
            native_i32buf(),
            TypeKind::Any,
            TypeKind::Any,
            int_type(),
            int_type(),
            int_type(),
            int_type(),
        ],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i32buf-step-pack-xy-in-rect!");
    meta.arglist
        .push("[out xs ys vxs dt wrap-min wrap-reset min-x max-x min-y max-y]".into());
    meta.doc = Some(format!(
        "Update xs by vxs*dt (seconds), wrap values smaller than wrap-min to wrap-reset, then pack (x,y) pairs into out when inside the rectangle; returns the number of values written.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![
            native_i32buf(),
            native_i32buf(),
            native_i32buf(),
            native_f32buf(),
            TypeKind::Any,
            int_type(),
            int_type(),
            int_type(),
            int_type(),
            int_type(),
            int_type(),
        ],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-new");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a NativeF32Buf with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-new-main");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a thread-affine NativeF32Buf (only the creating thread may access it) with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-begin");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Lock the buffer for repeated operations on this thread; call native::f32buf-end afterwards.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-end");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Release the lock acquired by native::f32buf-begin.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-len");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Return the number of elements in the buffer.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-capacity");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!("Return the buffer capacity.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-resize!");
    meta.arglist.push("[buf new-len fill]".into());
    meta.doc = Some(format!(
        "Resize the buffer length; fill with the provided value if needed.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf(), int_type(), float_type()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-get");
    meta.arglist.push("[buf idx]".into());
    meta.doc = Some(format!("Get the value at the index.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf(), int_type()],
        rest: None,
        ret_type: float_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-set!");
    meta.arglist.push("[buf idx value]".into());
    meta.doc = Some(format!(
        "Update the value at the index.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf(), int_type(), float_type()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-set-range!");
    meta.arglist.push("[buf start values]".into());
    meta.doc = Some(format!(
        "Copy values (vector/list or native f32 buffer) into the buffer starting at start.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf(), int_type(), TypeKind::Any],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf-fill!");
    meta.arglist.push("[buf value]".into());
    meta.doc = Some(format!(
        "Fill the entire buffer with the value.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf(), float_type()],
        rest: None,
        ret_type: native_f32buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "f32buf->vec");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Copy the buffer into a Vector and return it (for debugging).\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_f32buf()],
        rest: None,
        ret_type: vec_float(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-new");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a NativeI64Buf with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-new-main");
    meta.arglist.push("[capacity]".into());
    meta.doc = Some(format!(
        "Create a thread-affine NativeI64Buf (only the creating thread may access it) with the given capacity.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![int_type()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-begin");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Lock the buffer for repeated operations on this thread; call native::i64buf-end afterwards.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-end");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Release the lock acquired by native::i64buf-begin.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-len");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Return the number of elements in the buffer.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-capacity");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!("Return the buffer capacity.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-resize!");
    meta.arglist.push("[buf new-len fill]".into());
    meta.doc = Some(format!(
        "Resize the buffer length; fill with the provided value if needed.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf(), int_type(), int_type()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-get");
    meta.arglist.push("[buf idx]".into());
    meta.doc = Some(format!("Get the value at the index.\n{}", NATIVE_BUF_NOTE));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf(), int_type()],
        rest: None,
        ret_type: int_type(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-set!");
    meta.arglist.push("[buf idx value]".into());
    meta.doc = Some(format!(
        "Update the value at the index.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf(), int_type(), int_type()],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf-set-range!");
    meta.arglist.push("[buf start values]".into());
    meta.doc = Some(format!(
        "Copy values (vector/list or native i64 buffer) into the buffer starting at start.\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf(), int_type(), TypeKind::Any],
        rest: None,
        ret_type: native_i64buf(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);

    let mut meta = FnMeta::new("native", "i64buf->vec");
    meta.arglist.push("[buf]".into());
    meta.doc = Some(format!(
        "Copy the buffer into a Vector and return it (for debugging).\n{}",
        NATIVE_BUF_NOTE
    ));
    meta.overloads.push(FnOverload {
        arg_types: vec![native_i64buf()],
        rest: None,
        ret_type: vec_int(),
        special_op: None,
    });
    meta.subject_pos = Some(SubjectPos::Fixed(1));
    fn_meta::register(meta);
}

fn native_i32buf_new(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::i32buf-new", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(Mutex::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::I32(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32(buf),
        })
    })
}

fn native_i32buf_new_main(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::i32buf-new-main", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(MainThreadBuf::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::I32Main(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::I32,
            handle: NativeBufHandle::I32Main(buf),
        })
    })
}

fn native_i32buf_begin(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::i32buf-begin";
    let (id, handle) = expect_i32buf_handle(buf)?;
    begin_i32buf_guard(id, handle, op)?;
    Ok(buf.clone())
}

fn native_i32buf_end(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::i32buf-end";
    let (id, _handle) = expect_i32buf_handle(buf)?;
    end_i32buf_guard(id, op)?;
    Ok(buf.clone())
}

fn native_i32buf_len(buf: &Value) -> Result<Value, CloveError> {
    with_native_i32buf(buf, "native::i32buf-len", |guard| {
        Ok(Value::Int(guard.len() as i64))
    })
}

fn native_i32buf_capacity(buf: &Value) -> Result<Value, CloveError> {
    with_native_i32buf(buf, "native::i32buf-capacity", |guard| {
        Ok(Value::Int(guard.capacity() as i64))
    })
}

fn native_i32buf_resize(buf: &Value, new_len: &Value, fill: &Value) -> Result<Value, CloveError> {
    let new_len = expect_non_negative_usize(new_len, "native::i32buf-resize!", 2)?;
    let fill = expect_i32(fill, "native::i32buf-resize!", 3)?;
    with_native_i32buf(buf, "native::i32buf-resize!", |guard| {
        guard.resize(new_len, fill);
        Ok(buf.clone())
    })
}

fn native_i32buf_get(buf: &Value, idx: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::i32buf-get", 2)?;
    with_native_i32buf(buf, "native::i32buf-get", |guard| {
        guard
            .get(idx)
            .copied()
            .map(|v| Value::Int(v as i64))
            .ok_or_else(|| CloveError::runtime("native::i32buf-get index out of bounds"))
    })
}

fn native_i32buf_set(buf: &Value, idx: &Value, value: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::i32buf-set!", 2)?;
    let value = expect_i32(value, "native::i32buf-set!", 3)?;
    with_native_i32buf(buf, "native::i32buf-set!", |guard| {
        if idx >= guard.len() {
            return Err(CloveError::runtime(
                "native::i32buf-set! index out of bounds",
            ));
        }
        guard[idx] = value;
        Ok(buf.clone())
    })
}

fn native_i32buf_set_range(
    buf: &Value,
    start: &Value,
    values: &Value,
) -> Result<Value, CloveError> {
    let op = "native::i32buf-set-range!";
    let start = expect_non_negative_usize(start, op, 2)?;
    let (id, handle) = expect_i32buf_handle(buf)?;
    let buf_value = buf.clone();
    with_i32buf_mut(id, handle, op, |guard| match values {
        Value::NativeBuf {
            ty: NativeBufTy::I32,
            ..
        } => {
            let (src_id, src_handle) = expect_i32buf_handle(values)?;
            if src_id == id {
                return Err(CloveError::runtime(
                    "native::i32buf-set-range! values must be distinct from buf",
                ));
            }
            with_i32buf_ref(src_id, src_handle, op, |src_guard| {
                let end = start.saturating_add(src_guard.len());
                if end > guard.len() {
                    return Err(CloveError::runtime(
                        "native::i32buf-set-range! index out of bounds",
                    ));
                }
                guard[start..end].copy_from_slice(src_guard);
                Ok(buf_value.clone())
            })
        }
        Value::NativeBuf { .. } => Err(type_mismatch_arg(
            "native i32 buffer or vector/list",
            op,
            3,
            values,
        )),
        Value::Vector(items) | Value::List(items) => {
            let end = start.saturating_add(items.len());
            if end > guard.len() {
                return Err(CloveError::runtime(
                    "native::i32buf-set-range! index out of bounds",
                ));
            }
            for (offset, item) in items.iter().enumerate() {
                let value = expect_i32(item, op, 3)?;
                guard[start + offset] = value;
            }
            Ok(buf_value.clone())
        }
        other => Err(type_mismatch_arg(
            "native i32 buffer or vector/list",
            op,
            3,
            other,
        )),
    })
}

fn native_i32buf_fill(buf: &Value, value: &Value) -> Result<Value, CloveError> {
    let value = expect_i32(value, "native::i32buf-fill!", 2)?;
    with_native_i32buf(buf, "native::i32buf-fill!", |guard| {
        for v in guard.iter_mut() {
            *v = value;
        }
        Ok(buf.clone())
    })
}

fn native_i32buf_fill_xy_step(
    buf: &Value,
    start_x: &Value,
    start_y: &Value,
    count: &Value,
    step_x: &Value,
    step_y: &Value,
) -> Result<Value, CloveError> {
    let op = "native::i32buf-fill-xy-step!";
    let start_x = expect_i32(start_x, op, 2)?;
    let start_y = expect_i32(start_y, op, 3)?;
    let count = expect_non_negative_usize(count, op, 4)?;
    let step_x = expect_i32(step_x, op, 5)?;
    let step_y = expect_i32(step_y, op, 6)?;
    with_native_i32buf(buf, op, |guard| {
        let total = count.saturating_mul(2);
        if guard.len() != total {
            guard.resize(total, 0);
        }
        let mut x = start_x;
        let mut y = start_y;
        for i in 0..count {
            let idx = i * 2;
            guard[idx] = x;
            guard[idx + 1] = y;
            x = x.wrapping_add(step_x);
            y = y.wrapping_add(step_y);
        }
        Ok(buf.clone())
    })
}

fn native_i32buf_to_vec(buf: &Value) -> Result<Value, CloveError> {
    with_native_i32buf(buf, "native::i32buf->vec", |guard| {
        let items: Vec<Value> = guard.iter().map(|v| Value::Int(*v as i64)).collect();
        Ok(Value::Vector(Vector::from(items)))
    })
}

fn native_i32buf_add(buf: &Value, src: &Value, scale: Option<&Value>) -> Result<Value, CloveError> {
    let op = "native::i32buf-add!";
    let scale = match scale {
        Some(scale) => expect_finite_number(scale, op, 3)?,
        None => 1.0,
    };
    let (buf_id, buf_handle) = expect_i32buf_handle(buf)?;
    let buf_value = buf.clone();
    with_i32buf_mut(buf_id, buf_handle, op, |dst_guard| match src {
        Value::NativeBuf {
            ty: NativeBufTy::I32,
            ..
        } => {
            let (src_id, src_handle) = expect_i32buf_handle(src)?;
            if src_id == buf_id {
                let factor = 1.0 + scale;
                for v in dst_guard.iter_mut() {
                    let next = (*v as f64) * factor;
                    *v = checked_i32_from_f64(next, op)?;
                }
                return Ok(buf_value.clone());
            }
            with_i32buf_ref(src_id, src_handle, op, |src_guard| {
                if src_guard.len() != dst_guard.len() {
                    return Err(CloveError::runtime("native::i32buf-add! length mismatch"));
                }
                for (dst, src) in dst_guard.iter_mut().zip(src_guard.iter()) {
                    let next = (*dst as f64) + (*src as f64) * scale;
                    *dst = checked_i32_from_f64(next, op)?;
                }
                Ok(buf_value.clone())
            })
        }
        Value::NativeBuf {
            ty: NativeBufTy::F32,
            ..
        } => {
            let (src_id, src_handle) = expect_f32buf_handle(src)?;
            with_f32buf_ref(src_id, src_handle, op, |src_guard| {
                if src_guard.len() != dst_guard.len() {
                    return Err(CloveError::runtime("native::i32buf-add! length mismatch"));
                }
                for (dst, src) in dst_guard.iter_mut().zip(src_guard.iter()) {
                    let next = (*dst as f64) + (*src as f64) * scale;
                    *dst = checked_i32_from_f64(next, op)?;
                }
                Ok(buf_value.clone())
            })
        }
        Value::NativeBuf { .. } => Err(type_mismatch_arg(
            "native i32/f32 buffer or vector/list",
            op,
            2,
            src,
        )),
        Value::Vector(items) | Value::List(items) => {
            if items.len() != dst_guard.len() {
                return Err(CloveError::runtime("native::i32buf-add! length mismatch"));
            }
            for (dst, item) in dst_guard.iter_mut().zip(items.iter()) {
                let src = expect_finite_number(item, op, 2)?;
                let next = (*dst as f64) + src * scale;
                *dst = checked_i32_from_f64(next, op)?;
            }
            Ok(buf_value.clone())
        }
        other => Err(type_mismatch_arg(
            "native i32 buffer or vector/list",
            op,
            2,
            other,
        )),
    })
}

fn native_i32buf_wrap_min(buf: &Value, min: &Value, reset: &Value) -> Result<Value, CloveError> {
    let min = expect_i32(min, "native::i32buf-wrap-min!", 2)?;
    let reset = expect_i32(reset, "native::i32buf-wrap-min!", 3)?;
    with_native_i32buf(buf, "native::i32buf-wrap-min!", |guard| {
        for v in guard.iter_mut() {
            if *v < min {
                *v = reset;
            }
        }
        Ok(buf.clone())
    })
}

fn native_i32buf_pack_xy_in_rect(
    out: &Value,
    xs: &Value,
    ys: &Value,
    min_x: &Value,
    max_x: &Value,
    min_y: &Value,
    max_y: &Value,
) -> Result<Value, CloveError> {
    let _guard = profiler::enter("native.i32buf_pack_xy_in_rect");
    let op = "native::i32buf-pack-xy-in-rect!";
    let (out_id, out_handle) = expect_i32buf_handle(out)?;
    let min_x = expect_i32(min_x, op, 4)?;
    let max_x = expect_i32(max_x, op, 5)?;
    let min_y = expect_i32(min_y, op, 6)?;
    let max_y = expect_i32(max_y, op, 7)?;
    with_i32buf_mut(out_id, out_handle, op, |out_guard| {
        out_guard.clear();
        let (xs_native, xs_items) = match xs {
            Value::NativeBuf {
                ty: NativeBufTy::I32,
                ..
            } => (Some(expect_i32buf_handle(xs)?), None),
            Value::NativeBuf { .. } => {
                return Err(type_mismatch_arg(
                    "native i32 buffer or vector/list",
                    op,
                    2,
                    xs,
                ))
            }
            Value::Vector(items) | Value::List(items) => (None, Some(items)),
            other => {
                return Err(type_mismatch_arg(
                    "native i32 buffer or vector/list",
                    op,
                    2,
                    other,
                ))
            }
        };
        let (ys_native, ys_items) = match ys {
            Value::NativeBuf {
                ty: NativeBufTy::I32,
                ..
            } => (Some(expect_i32buf_handle(ys)?), None),
            Value::NativeBuf { .. } => {
                return Err(type_mismatch_arg(
                    "native i32 buffer or vector/list",
                    op,
                    3,
                    ys,
                ))
            }
            Value::Vector(items) | Value::List(items) => (None, Some(items)),
            other => {
                return Err(type_mismatch_arg(
                    "native i32 buffer or vector/list",
                    op,
                    3,
                    other,
                ))
            }
        };
        match (xs_native, ys_native, xs_items, ys_items) {
            (Some((xs_id, xs_handle)), Some((ys_id, ys_handle)), None, None) => {
                if xs_id == out_id || ys_id == out_id {
                    return Err(CloveError::runtime(
                        "native::i32buf-pack-xy-in-rect! out must be distinct from xs/ys",
                    ));
                }
                with_i32buf_ref(xs_id, xs_handle, op, |xs_guard| {
                    with_i32buf_ref(ys_id, ys_handle, op, |ys_guard| {
                        if xs_guard.len() != ys_guard.len() {
                            return Err(CloveError::runtime(
                                "native::i32buf-pack-xy-in-rect! length mismatch",
                            ));
                        }
                        out_guard.reserve(xs_guard.len() * 2);
                        for (x, y) in xs_guard.iter().zip(ys_guard.iter()) {
                            if *x >= min_x && *x <= max_x && *y >= min_y && *y <= max_y {
                                out_guard.push(*x);
                                out_guard.push(*y);
                            }
                        }
                        Ok(())
                    })
                })?;
            }
            (Some((xs_id, xs_handle)), None, None, Some(ys_items)) => {
                if xs_id == out_id {
                    return Err(CloveError::runtime(
                        "native::i32buf-pack-xy-in-rect! out must be distinct from xs",
                    ));
                }
                with_i32buf_ref(xs_id, xs_handle, op, |xs_guard| {
                    if xs_guard.len() != ys_items.len() {
                        return Err(CloveError::runtime(
                            "native::i32buf-pack-xy-in-rect! length mismatch",
                        ));
                    }
                    out_guard.reserve(xs_guard.len() * 2);
                    for (x, y_val) in xs_guard.iter().zip(ys_items.iter()) {
                        let y = expect_number_i32(y_val, op, 3)?;
                        if *x >= min_x && *x <= max_x && y >= min_y && y <= max_y {
                            out_guard.push(*x);
                            out_guard.push(y);
                        }
                    }
                    Ok(())
                })?;
            }
            (None, Some((ys_id, ys_handle)), Some(xs_items), None) => {
                if ys_id == out_id {
                    return Err(CloveError::runtime(
                        "native::i32buf-pack-xy-in-rect! out must be distinct from ys",
                    ));
                }
                with_i32buf_ref(ys_id, ys_handle, op, |ys_guard| {
                    if xs_items.len() != ys_guard.len() {
                        return Err(CloveError::runtime(
                            "native::i32buf-pack-xy-in-rect! length mismatch",
                        ));
                    }
                    out_guard.reserve(ys_guard.len() * 2);
                    for (x_val, y) in xs_items.iter().zip(ys_guard.iter()) {
                        let x = expect_number_i32(x_val, op, 2)?;
                        if x >= min_x && x <= max_x && *y >= min_y && *y <= max_y {
                            out_guard.push(x);
                            out_guard.push(*y);
                        }
                    }
                    Ok(())
                })?;
            }
            (None, None, Some(xs_items), Some(ys_items)) => {
                if xs_items.len() != ys_items.len() {
                    return Err(CloveError::runtime(
                        "native::i32buf-pack-xy-in-rect! length mismatch",
                    ));
                }
                out_guard.reserve(xs_items.len() * 2);
                for (x_val, y_val) in xs_items.iter().zip(ys_items.iter()) {
                    let x = expect_number_i32(x_val, op, 2)?;
                    let y = expect_number_i32(y_val, op, 3)?;
                    if x >= min_x && x <= max_x && y >= min_y && y <= max_y {
                        out_guard.push(x);
                        out_guard.push(y);
                    }
                }
            }
            _ => {
                return Err(CloveError::runtime(
                    "native::i32buf-pack-xy-in-rect! invalid arguments",
                ))
            }
        }
        Ok(Value::Int(out_guard.len() as i64))
    })
}

fn native_i32buf_step_pack_xy_in_rect(
    out: &Value,
    xs: &Value,
    ys: &Value,
    vxs: &Value,
    dt: &Value,
    wrap_min: &Value,
    wrap_reset: &Value,
    min_x: &Value,
    max_x: &Value,
    min_y: &Value,
    max_y: &Value,
) -> Result<Value, CloveError> {
    let _guard = profiler::enter("native.i32buf_step_pack_xy_in_rect");
    let op = "native::i32buf-step-pack-xy-in-rect!";
    let (out_id, out_handle) = expect_i32buf_handle(out)?;
    let (xs_id, xs_handle) = expect_i32buf_handle(xs)?;
    let (ys_id, ys_handle) = expect_i32buf_handle(ys)?;
    let (vxs_id, vxs_handle) = expect_f32buf_handle(vxs)?;
    if out_id == xs_id || out_id == ys_id {
        return Err(CloveError::runtime(
            "native::i32buf-step-pack-xy-in-rect! out must be distinct from xs/ys",
        ));
    }
    let dt = expect_f32(dt, op, 5)?;
    let wrap_min = expect_i32(wrap_min, op, 6)?;
    let wrap_reset = expect_i32(wrap_reset, op, 7)?;
    let min_x = expect_i32(min_x, op, 8)?;
    let max_x = expect_i32(max_x, op, 9)?;
    let min_y = expect_i32(min_y, op, 10)?;
    let max_y = expect_i32(max_y, op, 11)?;
    with_i32buf_mut(out_id, out_handle, op, |out_guard| {
        out_guard.clear();
        with_i32buf_mut(xs_id, xs_handle, op, |xs_guard| {
            with_i32buf_ref(ys_id, ys_handle, op, |ys_guard| {
                with_f32buf_ref(vxs_id, vxs_handle, op, |vxs_guard| {
                    if xs_guard.len() != ys_guard.len() || xs_guard.len() != vxs_guard.len() {
                        return Err(CloveError::runtime(
                            "native::i32buf-step-pack-xy-in-rect! length mismatch",
                        ));
                    }
                    out_guard.reserve(xs_guard.len() * 2);
                    let wrap_min_f = wrap_min as f32;
                    let wrap_reset_f = wrap_reset as f32;
                    let min_x_f = min_x as f32;
                    let max_x_f = max_x as f32;
                    for ((x, y), vx) in xs_guard
                        .iter_mut()
                        .zip(ys_guard.iter())
                        .zip(vxs_guard.iter())
                    {
                        let mut next = (*x as f32) + (*vx) * dt;
                        if next < wrap_min_f {
                            next = wrap_reset_f;
                        }
                        let next_i32 = next as i32;
                        *x = next_i32;
                        if next >= min_x_f && next <= max_x_f && *y >= min_y && *y <= max_y {
                            out_guard.push(next_i32);
                            out_guard.push(*y);
                        }
                    }
                    Ok(Value::Int(out_guard.len() as i64))
                })
            })
        })
    })
}

fn native_f32buf_new(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::f32buf-new", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(Mutex::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::F32(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::F32,
            handle: NativeBufHandle::F32(buf),
        })
    })
}

fn native_f32buf_new_main(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::f32buf-new-main", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(MainThreadBuf::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::F32Main(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::F32,
            handle: NativeBufHandle::F32Main(buf),
        })
    })
}

fn native_f32buf_begin(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::f32buf-begin";
    let (id, handle) = expect_f32buf_handle(buf)?;
    begin_f32buf_guard(id, handle, op)?;
    Ok(buf.clone())
}

fn native_f32buf_end(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::f32buf-end";
    let (id, _handle) = expect_f32buf_handle(buf)?;
    end_f32buf_guard(id, op)?;
    Ok(buf.clone())
}

fn native_f32buf_len(buf: &Value) -> Result<Value, CloveError> {
    with_native_f32buf(buf, "native::f32buf-len", |guard| {
        Ok(Value::Int(guard.len() as i64))
    })
}

fn native_f32buf_capacity(buf: &Value) -> Result<Value, CloveError> {
    with_native_f32buf(buf, "native::f32buf-capacity", |guard| {
        Ok(Value::Int(guard.capacity() as i64))
    })
}

fn native_f32buf_resize(buf: &Value, new_len: &Value, fill: &Value) -> Result<Value, CloveError> {
    let new_len = expect_non_negative_usize(new_len, "native::f32buf-resize!", 2)?;
    let fill = expect_f32(fill, "native::f32buf-resize!", 3)?;
    with_native_f32buf(buf, "native::f32buf-resize!", |guard| {
        guard.resize(new_len, fill);
        Ok(buf.clone())
    })
}

fn native_f32buf_get(buf: &Value, idx: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::f32buf-get", 2)?;
    with_native_f32buf(buf, "native::f32buf-get", |guard| {
        guard
            .get(idx)
            .copied()
            .map(|v| Value::Float(v as f64))
            .ok_or_else(|| CloveError::runtime("native::f32buf-get index out of bounds"))
    })
}

fn native_f32buf_set(buf: &Value, idx: &Value, value: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::f32buf-set!", 2)?;
    let value = expect_f32(value, "native::f32buf-set!", 3)?;
    with_native_f32buf(buf, "native::f32buf-set!", |guard| {
        if idx >= guard.len() {
            return Err(CloveError::runtime(
                "native::f32buf-set! index out of bounds",
            ));
        }
        guard[idx] = value;
        Ok(buf.clone())
    })
}

fn native_f32buf_set_range(
    buf: &Value,
    start: &Value,
    values: &Value,
) -> Result<Value, CloveError> {
    let op = "native::f32buf-set-range!";
    let start = expect_non_negative_usize(start, op, 2)?;
    let (id, handle) = expect_f32buf_handle(buf)?;
    let buf_value = buf.clone();
    with_f32buf_mut(id, handle, op, |guard| match values {
        Value::NativeBuf {
            ty: NativeBufTy::F32,
            ..
        } => {
            let (src_id, src_handle) = expect_f32buf_handle(values)?;
            if src_id == id {
                return Err(CloveError::runtime(
                    "native::f32buf-set-range! values must be distinct from buf",
                ));
            }
            with_f32buf_ref(src_id, src_handle, op, |src_guard| {
                let end = start.saturating_add(src_guard.len());
                if end > guard.len() {
                    return Err(CloveError::runtime(
                        "native::f32buf-set-range! index out of bounds",
                    ));
                }
                guard[start..end].copy_from_slice(src_guard);
                Ok(buf_value.clone())
            })
        }
        Value::NativeBuf { .. } => Err(type_mismatch_arg(
            "native f32 buffer or vector/list",
            op,
            3,
            values,
        )),
        Value::Vector(items) | Value::List(items) => {
            let end = start.saturating_add(items.len());
            if end > guard.len() {
                return Err(CloveError::runtime(
                    "native::f32buf-set-range! index out of bounds",
                ));
            }
            for (offset, item) in items.iter().enumerate() {
                let value = expect_f32(item, op, 3)?;
                guard[start + offset] = value;
            }
            Ok(buf_value.clone())
        }
        other => Err(type_mismatch_arg(
            "native f32 buffer or vector/list",
            op,
            3,
            other,
        )),
    })
}

fn native_f32buf_fill(buf: &Value, value: &Value) -> Result<Value, CloveError> {
    let value = expect_f32(value, "native::f32buf-fill!", 2)?;
    with_native_f32buf(buf, "native::f32buf-fill!", |guard| {
        for v in guard.iter_mut() {
            *v = value;
        }
        Ok(buf.clone())
    })
}

fn native_f32buf_to_vec(buf: &Value) -> Result<Value, CloveError> {
    with_native_f32buf(buf, "native::f32buf->vec", |guard| {
        let items: Vec<Value> = guard.iter().map(|v| Value::Float(*v as f64)).collect();
        Ok(Value::Vector(Vector::from(items)))
    })
}

fn native_i64buf_new(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::i64buf-new", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(Mutex::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::I64(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::I64,
            handle: NativeBufHandle::I64(buf),
        })
    })
}

fn native_i64buf_new_main(capacity: &Value) -> Result<Value, CloveError> {
    let capacity = expect_non_negative_usize(capacity, "native::i64buf-new-main", 1)?;
    RuntimeCtx::with_current(|ctx| {
        let buf = Arc::new(MainThreadBuf::new(Vec::with_capacity(capacity)));
        let id = ctx.with_native_bufs_mut(|bufs| bufs.insert(NativeBuf::I64Main(buf.clone())));
        Ok(Value::NativeBuf {
            id,
            ty: NativeBufTy::I64,
            handle: NativeBufHandle::I64Main(buf),
        })
    })
}

fn native_i64buf_begin(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::i64buf-begin";
    let (id, handle) = expect_i64buf_handle(buf)?;
    begin_i64buf_guard(id, handle, op)?;
    Ok(buf.clone())
}

fn native_i64buf_end(buf: &Value) -> Result<Value, CloveError> {
    let op = "native::i64buf-end";
    let (id, _handle) = expect_i64buf_handle(buf)?;
    end_i64buf_guard(id, op)?;
    Ok(buf.clone())
}

fn native_i64buf_len(buf: &Value) -> Result<Value, CloveError> {
    with_native_i64buf(buf, "native::i64buf-len", |guard| {
        Ok(Value::Int(guard.len() as i64))
    })
}

fn native_i64buf_capacity(buf: &Value) -> Result<Value, CloveError> {
    with_native_i64buf(buf, "native::i64buf-capacity", |guard| {
        Ok(Value::Int(guard.capacity() as i64))
    })
}

fn native_i64buf_resize(buf: &Value, new_len: &Value, fill: &Value) -> Result<Value, CloveError> {
    let new_len = expect_non_negative_usize(new_len, "native::i64buf-resize!", 2)?;
    let fill = expect_i64(fill, "native::i64buf-resize!", 3)?;
    with_native_i64buf(buf, "native::i64buf-resize!", |guard| {
        guard.resize(new_len, fill);
        Ok(buf.clone())
    })
}

fn native_i64buf_get(buf: &Value, idx: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::i64buf-get", 2)?;
    with_native_i64buf(buf, "native::i64buf-get", |guard| {
        guard
            .get(idx)
            .copied()
            .map(Value::Int)
            .ok_or_else(|| CloveError::runtime("native::i64buf-get index out of bounds"))
    })
}

fn native_i64buf_set(buf: &Value, idx: &Value, value: &Value) -> Result<Value, CloveError> {
    let idx = expect_non_negative_usize(idx, "native::i64buf-set!", 2)?;
    let value = expect_i64(value, "native::i64buf-set!", 3)?;
    with_native_i64buf(buf, "native::i64buf-set!", |guard| {
        if idx >= guard.len() {
            return Err(CloveError::runtime(
                "native::i64buf-set! index out of bounds",
            ));
        }
        guard[idx] = value;
        Ok(buf.clone())
    })
}

fn native_i64buf_set_range(
    buf: &Value,
    start: &Value,
    values: &Value,
) -> Result<Value, CloveError> {
    let op = "native::i64buf-set-range!";
    let start = expect_non_negative_usize(start, op, 2)?;
    let (id, handle) = expect_i64buf_handle(buf)?;
    let buf_value = buf.clone();
    with_i64buf_mut(id, handle, op, |guard| match values {
        Value::NativeBuf {
            ty: NativeBufTy::I64,
            ..
        } => {
            let (src_id, src_handle) = expect_i64buf_handle(values)?;
            if src_id == id {
                return Err(CloveError::runtime(
                    "native::i64buf-set-range! values must be distinct from buf",
                ));
            }
            with_i64buf_ref(src_id, src_handle, op, |src_guard| {
                let end = start.saturating_add(src_guard.len());
                if end > guard.len() {
                    return Err(CloveError::runtime(
                        "native::i64buf-set-range! index out of bounds",
                    ));
                }
                guard[start..end].copy_from_slice(src_guard);
                Ok(buf_value.clone())
            })
        }
        Value::NativeBuf { .. } => Err(type_mismatch_arg(
            "native i64 buffer or vector/list",
            op,
            3,
            values,
        )),
        Value::Vector(items) | Value::List(items) => {
            let end = start.saturating_add(items.len());
            if end > guard.len() {
                return Err(CloveError::runtime(
                    "native::i64buf-set-range! index out of bounds",
                ));
            }
            for (offset, item) in items.iter().enumerate() {
                let value = expect_i64(item, op, 3)?;
                guard[start + offset] = value;
            }
            Ok(buf_value.clone())
        }
        other => Err(type_mismatch_arg(
            "native i64 buffer or vector/list",
            op,
            3,
            other,
        )),
    })
}

fn native_i64buf_to_vec(buf: &Value) -> Result<Value, CloveError> {
    with_native_i64buf(buf, "native::i64buf->vec", |guard| {
        let items: Vec<Value> = guard.iter().map(|v| Value::Int(*v)).collect();
        Ok(Value::Vector(Vector::from(items)))
    })
}

fn with_native_i32buf<T>(
    buf: &Value,
    op: &str,
    f: impl FnOnce(&mut Vec<i32>) -> Result<T, CloveError>,
) -> Result<T, CloveError> {
    let (id, handle) = expect_i32buf_handle(buf)?;
    with_i32buf_mut(id, handle, op, f)
}

fn with_native_f32buf<T>(
    buf: &Value,
    op: &str,
    f: impl FnOnce(&mut Vec<f32>) -> Result<T, CloveError>,
) -> Result<T, CloveError> {
    let (id, handle) = expect_f32buf_handle(buf)?;
    with_f32buf_mut(id, handle, op, f)
}

fn with_native_i64buf<T>(
    buf: &Value,
    op: &str,
    f: impl FnOnce(&mut Vec<i64>) -> Result<T, CloveError>,
) -> Result<T, CloveError> {
    let (id, handle) = expect_i64buf_handle(buf)?;
    with_i64buf_mut(id, handle, op, f)
}

fn expect_i32(value: &Value, op: &str, arg_index: usize) -> Result<i32, CloveError> {
    match value {
        Value::Int(n) if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 => Ok(*n as i32),
        Value::Int(_) => Err(CloveError::runtime(format!(
            "{} expects i32 range (arg {})",
            op, arg_index
        ))),
        other => Err(type_mismatch_arg("int", op, arg_index, other)),
    }
}

fn expect_finite_number(value: &Value, op: &str, arg_index: usize) -> Result<f64, CloveError> {
    let (n, _) = crate::builtins::as_number(value)
        .map_err(|_| type_mismatch_arg("number", op, arg_index, value))?;
    if n.is_finite() {
        Ok(n)
    } else {
        Err(CloveError::runtime(format!(
            "{} expects finite number (arg {})",
            op, arg_index
        )))
    }
}

fn expect_number_i32(value: &Value, op: &str, arg_index: usize) -> Result<i32, CloveError> {
    let n = expect_finite_number(value, op, arg_index)?;
    if n < i32::MIN as f64 || n > i32::MAX as f64 {
        return Err(CloveError::runtime(format!(
            "{} expects i32 range (arg {})",
            op, arg_index
        )));
    }
    Ok(n as i32)
}

fn expect_i64(value: &Value, op: &str, arg_index: usize) -> Result<i64, CloveError> {
    match value {
        Value::Int(n) => Ok(*n),
        other => Err(type_mismatch_arg("int", op, arg_index, other)),
    }
}

fn expect_f32(value: &Value, op: &str, arg_index: usize) -> Result<f32, CloveError> {
    let (n, _) = crate::builtins::as_number(value)
        .map_err(|_| type_mismatch_arg("number", op, arg_index, value))?;
    let n = n as f32;
    if n.is_finite() {
        Ok(n)
    } else {
        Err(CloveError::runtime(format!(
            "{} expects finite float (arg {})",
            op, arg_index
        )))
    }
}

fn checked_i32_from_f64(value: f64, op: &str) -> Result<i32, CloveError> {
    if !value.is_finite() {
        return Err(CloveError::runtime(format!("{op} result is not finite")));
    }
    if value < i32::MIN as f64 || value > i32::MAX as f64 {
        return Err(CloveError::runtime(format!("{op} result out of i32 range")));
    }
    Ok(value as i32)
}

fn expect_non_negative_usize(
    value: &Value,
    op: &str,
    arg_index: usize,
) -> Result<usize, CloveError> {
    match value {
        Value::Int(n) if *n >= 0 => Ok(*n as usize),
        Value::Int(_) => Err(CloveError::runtime(format!(
            "{} expects non-negative int (arg {})",
            op, arg_index
        ))),
        other => Err(type_mismatch_arg("int", op, arg_index, other)),
    }
}
