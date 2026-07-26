use std::collections::BTreeMap;

use clove_build_core::ast::Literal;
use clove_build_core::typed_ir::{
    Expr as IrExpr, ExprKind as IrExprKind, Program as IrProgram, TopLevel as IrTopLevel,
};
use clove_build_runtime_c::RuntimeConfig;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CArtifact {
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BackendError {
    pub message: String,
}

impl std::fmt::Display for BackendError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for BackendError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontProgram {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopLevel {
    Def { name: String, value: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Nil,
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    Keyword(String),
    Map(Vec<(Expr, Expr)>),
    Vector(Vec<Expr>),
    Do(Vec<Expr>),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Let {
        bindings: Vec<(String, Expr)>,
        body: Box<Expr>,
    },
    Lambda {
        params: Vec<String>,
        body: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CType {
    Nil,
    I64,
    OptI64,
    VecI64,
    VecVecI64,
    VecStr,
    MapKI64,
    MapI64VecI64,
    Bool,
    OptBool,
    Str,
    OptStr,
}

#[derive(Debug, Clone)]
struct CValue {
    ctype: CType,
    repr: CRepr,
}

#[derive(Debug, Clone)]
enum CRepr {
    Expr(String),
    Var(String),
}

#[derive(Debug, Clone)]
enum Binding {
    Value(CValue),
    Lambda {
        /// Identifies this particular lambda binding. Recursion detection compares ids,
        /// not names: a local lambda may shadow an outer function of the same name, and
        /// calling it is not recursion.
        id: usize,
        params: Vec<String>,
        body: Expr,
    },
}

impl Binding {
    /// The parameters and body of a lambda binding with exactly `arity` parameters.
    ///
    /// The sequence builtins lower callables of a fixed shape: `map` takes a unary
    /// function, `reduce` a binary one, and so on.
    fn as_lambda(&self, arity: usize) -> Option<(&[String], &Expr)> {
        match self {
            Binding::Lambda { params, body, .. } if params.len() == arity => {
                Some((params.as_slice(), body))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct MapOp {
    code: i32,
    k: i64,
}

#[derive(Debug, Clone, Copy)]
struct PredOp {
    code: i32,
    k: i64,
}

#[derive(Debug, Clone, Copy)]
struct ReduceOp {
    code: i32,
}

#[derive(Debug, Clone, Copy)]
struct UpdateOp {
    code: i32,
    k: i64,
}

#[derive(Debug, Clone, Copy)]
struct MapIndexedOp {
    code: i32,
    k: i64,
}

#[derive(Debug, Clone, Copy)]
struct PredIndexedOp {
    code: i32,
    k: i64,
}

#[derive(Debug, Clone, Copy)]
struct ZipOp {
    code: i32,
}

#[derive(Debug, Clone)]
struct VecInput {
    name: String,
    releasable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VecKind {
    I64,
    VecI64,
    Str,
}

#[derive(Debug, Clone)]
struct VecVar {
    name: String,
    kind: VecKind,
}

#[derive(Debug, Clone)]
enum MapKind {
    KI64,
    I64VecI64,
}

#[derive(Debug, Clone)]
struct MapVar {
    name: String,
    kind: MapKind,
}

#[derive(Debug, Clone)]
struct ResourceSnapshot {
    vec_names: Vec<String>,
    map_names: Vec<String>,
    str_names: Vec<String>,
}

pub fn emit_c(program: &FrontProgram, config: &RuntimeConfig) -> Result<CArtifact, BackendError> {
    let mut compiler = Compiler {
        config,
        lines: Vec::new(),
        bindings: BTreeMap::new(),
        vec_vars: Vec::new(),
        map_vars: Vec::new(),
        str_vars: Vec::new(),
        temp_id: 0,
        inlining: Vec::new(),
        lambda_ids: 0,
    };
    compiler.emit_program(program)?;
    Ok(CArtifact {
        source: compiler.finish(),
    })
}

pub fn emit_c_ir(program: &IrProgram, config: &RuntimeConfig) -> Result<CArtifact, BackendError> {
    let front_program = lower_ir_program(program)?;
    emit_c(&front_program, config)
}

fn lower_ir_program(program: &IrProgram) -> Result<FrontProgram, BackendError> {
    Ok(FrontProgram {
        top_levels: program
            .top_levels
            .iter()
            .map(lower_ir_top_level)
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn lower_ir_top_level(top: &IrTopLevel) -> Result<TopLevel, BackendError> {
    match top {
        IrTopLevel::Def { name, value, .. } => Ok(TopLevel::Def {
            name: name.clone(),
            value: lower_ir_expr(value)?,
        }),
        IrTopLevel::FnDef {
            name, params, body, ..
        } => Ok(TopLevel::Def {
            name: name.clone(),
            value: lower_ir_lambda(
                &params
                    .iter()
                    .map(|param| param.name.clone())
                    .collect::<Vec<_>>(),
                lower_ir_expr(body)?,
            )?,
        }),
        IrTopLevel::Expr { expr, .. } => Ok(TopLevel::Expr(lower_ir_expr(expr)?)),
        IrTopLevel::DefType { .. } => Err(BackendError {
            message: "deftype is not supported in phase2 C backend yet".to_string(),
        }),
        IrTopLevel::DefForeign { .. } => Err(BackendError {
            message: "def-foreign is not supported in phase2 C backend yet".to_string(),
        }),
    }
}

fn lower_ir_expr(expr: &IrExpr) -> Result<Expr, BackendError> {
    match &expr.kind {
        IrExprKind::Const(Literal::Nil) => Ok(Expr::Nil),
        IrExprKind::Const(Literal::Int(v)) => Ok(Expr::Int(*v)),
        IrExprKind::Const(Literal::Bool(v)) => Ok(Expr::Bool(*v)),
        IrExprKind::Const(Literal::Str(v)) => Ok(Expr::Str(v.clone())),
        IrExprKind::Const(other) => Err(BackendError {
            message: format!("unsupported literal in phase2 C backend: {:?}", other),
        }),
        IrExprKind::Var(name) => Ok(Expr::Symbol(name.clone())),
        IrExprKind::Keyword(name) => Ok(Expr::Keyword(name.clone())),
        IrExprKind::VectorLit(items) => Ok(Expr::Vector(
            items
                .iter()
                .map(lower_ir_expr)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        IrExprKind::MapLit(entries) => Ok(Expr::Map(
            entries
                .iter()
                .map(|(k, v)| Ok((lower_ir_expr(k)?, lower_ir_expr(v)?)))
                .collect::<Result<Vec<_>, BackendError>>()?,
        )),
        IrExprKind::If {
            cond,
            then_expr,
            else_expr,
        } => Ok(Expr::If {
            cond: Box::new(lower_ir_expr(cond)?),
            then_expr: Box::new(lower_ir_expr(then_expr)?),
            else_expr: Box::new(lower_ir_expr(else_expr)?),
        }),
        IrExprKind::Let { bindings, body } => Ok(Expr::Let {
            bindings: bindings
                .iter()
                .map(|binding| Ok((binding.name.clone(), lower_ir_expr(&binding.value)?)))
                .collect::<Result<Vec<_>, BackendError>>()?,
            body: Box::new(lower_ir_expr(body)?),
        }),
        IrExprKind::Do(items) => Ok(Expr::Do(
            items
                .iter()
                .map(lower_ir_expr)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        IrExprKind::BuiltinCall { name, args } => Ok(Expr::Call {
            callee: name.clone(),
            args: args
                .iter()
                .map(lower_ir_expr)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        IrExprKind::Call { callee, args } => Ok(Expr::Call {
            callee: lower_ir_symbol_callee(callee)?,
            args: args
                .iter()
                .map(lower_ir_expr)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        IrExprKind::Lambda { params, body, .. } => lower_ir_lambda(
            &params
                .iter()
                .map(|param| param.name.clone())
                .collect::<Vec<_>>(),
            lower_ir_expr(body)?,
        ),
    }
}

fn lower_ir_symbol_callee(expr: &IrExpr) -> Result<String, BackendError> {
    match &expr.kind {
        IrExprKind::Var(name) => Ok(name.clone()),
        _ => Err(BackendError {
            message: "non-symbol callee is not supported in phase2 C backend yet".to_string(),
        }),
    }
}

fn lower_ir_lambda(params: &[String], body: Expr) -> Result<Expr, BackendError> {
    Ok(Expr::Lambda {
        params: params.to_vec(),
        body: Box::new(body),
    })
}

struct Compiler<'a> {
    config: &'a RuntimeConfig,
    lines: Vec<String>,
    bindings: BTreeMap<String, Binding>,
    vec_vars: Vec<VecVar>,
    map_vars: Vec<MapVar>,
    str_vars: Vec<String>,
    temp_id: usize,
    /// Lambda bindings currently being inlined, innermost last. See
    /// [`Compiler::inline_call`].
    inlining: Vec<usize>,
    lambda_ids: usize,
}

impl<'a> Compiler<'a> {
    fn probe_expr_ctype(&mut self, expr: &Expr) -> Result<CType, BackendError> {
        let saved_lines = std::mem::take(&mut self.lines);
        let saved_vec_vars = self.vec_vars.clone();
        let saved_map_vars = self.map_vars.clone();
        let saved_str_vars = self.str_vars.clone();
        let saved_temp_id = self.temp_id;
        let value = self.compile_expr(expr);
        self.lines = saved_lines;
        self.vec_vars = saved_vec_vars;
        self.map_vars = saved_map_vars;
        self.str_vars = saved_str_vars;
        self.temp_id = saved_temp_id;
        value.map(|v| v.ctype)
    }

    fn merge_loop_result_type(
        &self,
        left: Option<CType>,
        right: Option<CType>,
    ) -> Result<Option<CType>, BackendError> {
        match (left, right) {
            (None, None) => Ok(None),
            (Some(ty), None) | (None, Some(ty)) => Ok(Some(ty)),
            (Some(left), Some(right)) if left == right => Ok(Some(left)),
            (Some(CType::I64), Some(CType::Nil)) | (Some(CType::Nil), Some(CType::I64)) => {
                Ok(Some(CType::OptI64))
            }
            (Some(CType::Bool), Some(CType::Nil)) | (Some(CType::Nil), Some(CType::Bool)) => {
                Ok(Some(CType::OptBool))
            }
            (Some(CType::Str), Some(CType::Nil)) | (Some(CType::Nil), Some(CType::Str)) => {
                Ok(Some(CType::OptStr))
            }
            _ => Err(BackendError {
                message: "loop branches must currently have the same type".to_string(),
            }),
        }
    }

    fn infer_loop_result_type(
        &mut self,
        loop_name: &str,
        expr: &Expr,
    ) -> Result<Option<CType>, BackendError> {
        match expr {
            Expr::Do(items) => {
                if let Some(last) = items.last() {
                    self.infer_loop_result_type(loop_name, last)
                } else {
                    Ok(Some(CType::Nil))
                }
            }
            Expr::If {
                then_expr,
                else_expr,
                ..
            } => {
                let then_ty = self.infer_loop_result_type(loop_name, then_expr)?;
                let else_ty = self.infer_loop_result_type(loop_name, else_expr)?;
                self.merge_loop_result_type(then_ty, else_ty)
            }
            Expr::Call { callee, .. } if callee == loop_name => Ok(None),
            _ => self.probe_expr_ctype(expr).map(Some),
        }
    }

    fn compile_loop_tail_expr(
        &mut self,
        loop_name: &str,
        loop_bindings: &[(String, String, CType)],
        result_name: &str,
        result_type: &CType,
        expr: &Expr,
    ) -> Result<(), BackendError> {
        match expr {
            Expr::Do(items) => {
                if items.is_empty() {
                    self.assign_result_var(
                        result_name,
                        result_type,
                        CValue {
                            ctype: CType::Nil,
                            repr: CRepr::Expr("NULL".to_string()),
                        },
                    )?;
                    self.lines.push("break;".to_string());
                    return Ok(());
                }
                for item in &items[..items.len() - 1] {
                    let _ = self.compile_expr(item)?;
                }
                self.compile_loop_tail_expr(
                    loop_name,
                    loop_bindings,
                    result_name,
                    result_type,
                    &items[items.len() - 1],
                )
            }
            Expr::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_raw = self.compile_expr(cond)?;
                let cond_value = self.materialize_value("loop_cond", cond_raw)?;
                let cond_expr = self.truthy_expr_from_value(&cond_value);
                self.lines.push(format!("if ({}) {{", cond_expr));
                self.compile_loop_tail_expr(
                    loop_name,
                    loop_bindings,
                    result_name,
                    result_type,
                    then_expr,
                )?;
                self.lines.push("} else {".to_string());
                self.compile_loop_tail_expr(
                    loop_name,
                    loop_bindings,
                    result_name,
                    result_type,
                    else_expr,
                )?;
                self.lines.push("}".to_string());
                Ok(())
            }
            Expr::Call { callee, args } if callee == loop_name => {
                if args.len() != loop_bindings.len() {
                    return Err(BackendError {
                        message: format!("{} expects {} args", loop_name, loop_bindings.len()),
                    });
                }
                let mut next_values = Vec::with_capacity(args.len());
                for arg in args {
                    let raw = self.compile_expr(arg)?;
                    next_values.push(self.materialize_value("recur_arg", raw)?);
                }
                for ((_, loop_var, loop_type), value) in loop_bindings.iter().zip(next_values) {
                    self.assign_result_var(loop_var, loop_type, value)?;
                }
                self.lines.push("continue;".to_string());
                Ok(())
            }
            _ => {
                let value = self.compile_expr(expr)?;
                self.assign_result_var(result_name, result_type, value)?;
                self.lines.push("break;".to_string());
                Ok(())
            }
        }
    }

    fn try_compile_loop_let(
        &mut self,
        bindings: &[(String, Expr)],
        body: &Expr,
    ) -> Option<Result<CValue, BackendError>> {
        if bindings.len() != 1 {
            return None;
        }
        let (loop_name, lambda_expr) = &bindings[0];
        if !loop_name.starts_with("__loop__") {
            return None;
        }
        let Expr::Call {
            callee,
            args: init_args,
        } = body
        else {
            return None;
        };
        if callee != loop_name {
            return None;
        }
        let (params, loop_body) = match lambda_expr {
            Expr::Lambda { params, body } => (params.clone(), body.as_ref().clone()),
            _ => return None,
        };
        if params.len() != init_args.len() {
            return Some(Err(BackendError {
                message: "loop init arity mismatch".to_string(),
            }));
        }

        let mut run = || -> Result<CValue, BackendError> {
            let mut loop_bindings = Vec::with_capacity(params.len());
            let mut saved = Vec::with_capacity(params.len());
            for (param, init_arg) in params.iter().zip(init_args.iter()) {
                let raw = self.compile_expr(init_arg)?;
                let value = self.materialize_value("loop_init", raw)?;
                let loop_var = self.next_tmp("loop_var");
                self.lines.push(format!(
                    "{} {} = {};",
                    Self::c_type_name(&value.ctype),
                    loop_var,
                    Self::c_type_zero(&value.ctype).unwrap_or("0")
                ));
                self.assign_result_var(&loop_var, &value.ctype, value.clone())?;
                let prev = self.bindings.insert(
                    param.clone(),
                    Binding::Value(CValue {
                        ctype: value.ctype.clone(),
                        repr: CRepr::Var(loop_var.clone()),
                    }),
                );
                saved.push((param.clone(), prev));
                loop_bindings.push((param.clone(), loop_var, value.ctype));
            }

            let result_type = self
                .infer_loop_result_type(loop_name, &loop_body)?
                .unwrap_or(CType::Nil);
            let result_name = self.next_tmp("loop_result");
            match Self::c_type_zero(&result_type) {
                Some(init) => self.lines.push(format!(
                    "{} {} = {};",
                    Self::c_type_name(&result_type),
                    result_name,
                    init
                )),
                None => self.lines.push(format!(
                    "{} {};",
                    Self::c_type_name(&result_type),
                    result_name
                )),
            }
            if matches!(result_type, CType::Str) {
                self.str_vars.push(result_name.clone());
            }

            self.lines.push("while (true) {".to_string());
            self.compile_loop_tail_expr(
                loop_name,
                &loop_bindings,
                &result_name,
                &result_type,
                &loop_body,
            )?;
            self.lines.push("}".to_string());

            for (name, prev) in saved.into_iter().rev() {
                if let Some(prev) = prev {
                    self.bindings.insert(name, prev);
                } else {
                    self.bindings.remove(&name);
                }
            }

            Ok(CValue {
                ctype: result_type,
                repr: CRepr::Var(result_name),
            })
        };

        Some(run())
    }

    fn emit_program(&mut self, program: &FrontProgram) -> Result<(), BackendError> {
        // Pass 1: collect lambda defs to allow symbol references from later forms.
        for tl in &program.top_levels {
            if let TopLevel::Def { name, value } = tl {
                if let Expr::Lambda { params, body } = value {
                    let binding = Binding::Lambda {
                        id: self.next_lambda_id(),
                        params: params.clone(),
                        body: (**body).clone(),
                    };
                    self.bindings.insert(name.clone(), binding);
                }
            }
        }

        // Pass 2: emit value defs and top-level expressions.
        for tl in &program.top_levels {
            match tl {
                TopLevel::Def { name, value } => {
                    if matches!(value, Expr::Lambda { .. }) {
                        continue;
                    }
                    let cval = self.compile_expr(value)?;
                    let binding = self.materialize_def(name, cval)?;
                    self.bindings.insert(name.clone(), Binding::Value(binding));
                }
                TopLevel::Expr(expr) => self.emit_top_expr(expr)?,
            }
        }
        Ok(())
    }

    fn finish(self) -> String {
        let mut src = String::new();
        src.push_str("/* generated by phase2 C backend-c */\n");
        src.push_str("#include <stdbool.h>\n");
        src.push_str("#include <stdint.h>\n");
        src.push_str("#include <stdio.h>\n");
        src.push_str("#include <ctype.h>\n");
        src.push_str("#include <string.h>\n");
        src.push_str("#include <stdlib.h>\n\n");
        src.push_str("#include <sys/types.h>\n\n");
        src.push_str("#include <time.h>\n\n");
        src.push_str("#include <regex.h>\n\n");
        src.push_str("typedef struct { bool has; int64_t value; } clv_opt_i64;\n");
        src.push_str("typedef struct { bool has; bool value; } clv_opt_bool;\n");
        src.push_str("typedef struct { bool has; char* value; } clv_opt_str;\n\n");
        src.push_str(&runtime_prelude(self.config.allow_external_c_libs));
        src.push_str("int main(void) {\n");
        for line in &self.lines {
            src.push_str("  ");
            src.push_str(line);
            src.push('\n');
        }
        for v in &self.vec_vars {
            match v.kind {
                VecKind::I64 => {
                    src.push_str("  clv_vec_free(&");
                    src.push_str(&v.name);
                    src.push_str(");\n");
                }
                VecKind::VecI64 => {
                    src.push_str("  clv_vec_vec_i64_free(&");
                    src.push_str(&v.name);
                    src.push_str(");\n");
                }
                VecKind::Str => {
                    src.push_str("  clv_vec_str_free(&");
                    src.push_str(&v.name);
                    src.push_str(");\n");
                }
            }
        }
        for m in &self.map_vars {
            match m.kind {
                MapKind::KI64 => {
                    src.push_str("  clv_map_ki64_free(&");
                    src.push_str(&m.name);
                    src.push_str(");\n");
                }
                MapKind::I64VecI64 => {
                    src.push_str("  clv_map_i64_vec_i64_free(&");
                    src.push_str(&m.name);
                    src.push_str(");\n");
                }
            }
        }
        for name in &self.str_vars {
            src.push_str("  clv_str_free(");
            src.push_str(name);
            src.push_str(");\n");
        }
        src.push_str("  clv_arena_dispose();\n");
        src.push_str("  return 0;\n");
        src.push_str("}\n");
        src
    }

    fn next_tmp(&mut self, prefix: &str) -> String {
        let name = format!("{}_{}", prefix, self.temp_id);
        self.temp_id += 1;
        name
    }

    fn as_i64_expr(&mut self, expr: &Expr) -> Result<String, BackendError> {
        let v = self.compile_expr(expr)?;
        match v.ctype {
            CType::I64 => Ok(match v.repr {
                CRepr::Expr(e) => e,
                CRepr::Var(v) => v,
            }),
            _ => Err(BackendError {
                message: "expected Int expression".to_string(),
            }),
        }
    }

    fn as_str_expr(&mut self, expr: &Expr) -> Result<String, BackendError> {
        let v = self.compile_expr(expr)?;
        match v.ctype {
            CType::Str => Ok(match v.repr {
                CRepr::Expr(e) => e,
                CRepr::Var(v) => v,
            }),
            _ => Err(BackendError {
                message: "expected Str expression".to_string(),
            }),
        }
    }

    fn stringify_value_expr(
        &mut self,
        value: CValue,
        pr_mode: bool,
    ) -> Result<String, BackendError> {
        match value.ctype {
            CType::Nil => Ok("clv_str_clone(\"nil\")".to_string()),
            CType::I64 => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => format!("clv_i64_to_str({})", e),
            }),
            CType::OptI64 => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => {
                    format!(
                        "(({}).has ? clv_i64_to_str(({}).value) : clv_str_clone(\"nil\"))",
                        e, e
                    )
                }
            }),
            CType::Bool => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => format!("clv_bool_to_str({})", e),
            }),
            CType::OptBool => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => {
                    format!(
                        "(({}).has ? clv_bool_to_str(({}).value) : clv_str_clone(\"nil\"))",
                        e, e
                    )
                }
            }),
            CType::Str => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => {
                    if pr_mode {
                        format!("clv_pr_str_str({})", e)
                    } else {
                        format!("clv_str_clone({})", e)
                    }
                }
            }),
            CType::OptStr => Ok(match value.repr {
                CRepr::Expr(e) | CRepr::Var(e) => {
                    if pr_mode {
                        format!(
                            "(({}).has ? clv_pr_str_str(({}).value) : clv_str_clone(\"nil\"))",
                            e, e
                        )
                    } else {
                        format!(
                            "(({}).has ? clv_str_clone(({}).value) : clv_str_clone(\"nil\"))",
                            e, e
                        )
                    }
                }
            }),
            _ => Err(BackendError {
                message: "stringification supports Int/Bool/Str only".to_string(),
            }),
        }
    }

    fn materialize_value(&mut self, prefix: &str, value: CValue) -> Result<CValue, BackendError> {
        match value {
            CValue {
                ctype: CType::Nil,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("void* {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::Nil,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::I64,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("int64_t {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::OptI64,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("clv_opt_i64 {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::OptI64,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::Bool,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("bool {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::OptBool,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("clv_opt_bool {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::OptBool,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::Str,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines
                    .push(format!("char* {} = clv_str_clone({});", var, e));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::OptStr,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("clv_opt_str {} = {};", var, e));
                Ok(CValue {
                    ctype: CType::OptStr,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::MapKI64,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines.push(format!("clv_map_ki64 {} = {};", var, e));
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            CValue {
                ctype: CType::MapI64VecI64,
                repr: CRepr::Expr(e),
            } => {
                let var = self.next_tmp(prefix);
                self.lines
                    .push(format!("clv_map_i64_vec_i64 {} = {};", var, e));
                self.track_map_var(var.clone(), MapKind::I64VecI64);
                Ok(CValue {
                    ctype: CType::MapI64VecI64,
                    repr: CRepr::Var(var),
                })
            }
            v => Ok(v),
        }
    }

    fn as_vec_input(&mut self, expr: &Expr) -> Result<VecInput, BackendError> {
        let v = self.compile_expr(expr)?;
        match v.ctype {
            CType::VecI64 => match v.repr {
                CRepr::Var(name) => Ok(VecInput {
                    releasable: self.is_releasable_var(&name),
                    name,
                }),
                CRepr::Expr(_) => Err(BackendError {
                    message: "internal error: Vec must be materialized variable".to_string(),
                }),
            },
            _ => Err(BackendError {
                message: "expected Vec<Int> expression".to_string(),
            }),
        }
    }

    fn as_vec_str_input(&mut self, expr: &Expr) -> Result<VecInput, BackendError> {
        let v = self.compile_expr(expr)?;
        match v.ctype {
            CType::VecStr => match v.repr {
                CRepr::Var(name) => Ok(VecInput {
                    releasable: self.is_releasable_var(&name),
                    name,
                }),
                CRepr::Expr(_) => Err(BackendError {
                    message: "internal error: Vec<Str> must be materialized variable".to_string(),
                }),
            },
            _ => Err(BackendError {
                message: "expected Vec<Str> expression".to_string(),
            }),
        }
    }

    fn as_map_input(&mut self, expr: &Expr) -> Result<VecInput, BackendError> {
        let v = self.compile_expr(expr)?;
        match v.ctype {
            CType::MapKI64 => match v.repr {
                CRepr::Var(name) => Ok(VecInput {
                    releasable: self.is_releasable_var(&name),
                    name,
                }),
                CRepr::Expr(_) => Err(BackendError {
                    message: "internal error: Map must be materialized variable".to_string(),
                }),
            },
            _ => Err(BackendError {
                message: "expected Map<Keyword,Int> expression".to_string(),
            }),
        }
    }

    fn is_releasable_var(&self, name: &str) -> bool {
        !self.bindings.values().any(|binding| {
            matches!(
                binding,
                Binding::Value(CValue {
                    repr: CRepr::Var(bound),
                    ..
                }) if bound == name
            )
        })
    }

    fn resource_snapshot(&self) -> ResourceSnapshot {
        ResourceSnapshot {
            vec_names: self.vec_vars.iter().map(|var| var.name.clone()).collect(),
            map_names: self.map_vars.iter().map(|var| var.name.clone()).collect(),
            str_names: self.str_vars.clone(),
        }
    }

    fn take_scoped_cleanup(&mut self, before: &ResourceSnapshot) -> Vec<String> {
        let mut cleanup = Vec::new();
        self.vec_vars.retain(|var| {
            if before.vec_names.contains(&var.name) {
                true
            } else {
                cleanup.push(match var.kind {
                    VecKind::I64 => format!("clv_vec_free(&{});", var.name),
                    VecKind::VecI64 => format!("clv_vec_vec_i64_free(&{});", var.name),
                    VecKind::Str => format!("clv_vec_str_free(&{});", var.name),
                });
                false
            }
        });
        self.map_vars.retain(|var| {
            if before.map_names.contains(&var.name) {
                true
            } else {
                cleanup.push(match var.kind {
                    MapKind::KI64 => format!("clv_map_ki64_free(&{});", var.name),
                    MapKind::I64VecI64 => {
                        format!("clv_map_i64_vec_i64_free(&{});", var.name)
                    }
                });
                false
            }
        });
        self.str_vars.retain(|name| before.str_names.contains(name));
        cleanup
    }

    fn track_vec_var(&mut self, name: String, kind: VecKind) {
        self.vec_vars.push(VecVar { name, kind });
    }

    fn release_vec_var(&mut self, name: &str) {
        if let Some(pos) = self.vec_vars.iter().position(|v| v.name == name) {
            let v = self.vec_vars.remove(pos);
            match v.kind {
                VecKind::I64 => self.lines.push(format!("clv_vec_free(&{});", name)),
                VecKind::VecI64 => self.lines.push(format!("clv_vec_vec_i64_free(&{});", name)),
                VecKind::Str => self.lines.push(format!("clv_vec_str_free(&{});", name)),
            }
        }
    }

    fn track_map_var(&mut self, name: String, kind: MapKind) {
        self.map_vars.push(MapVar { name, kind });
    }

    fn release_map_var(&mut self, name: &str) {
        if let Some(pos) = self.map_vars.iter().position(|m| m.name == name) {
            let m = self.map_vars.remove(pos);
            match m.kind {
                MapKind::KI64 => self.lines.push(format!("clv_map_ki64_free(&{});", name)),
                MapKind::I64VecI64 => self
                    .lines
                    .push(format!("clv_map_i64_vec_i64_free(&{});", name)),
            }
        }
    }

    fn materialize_def(&mut self, name: &str, value: CValue) -> Result<CValue, BackendError> {
        match (&value.ctype, &value.repr) {
            (CType::I64, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("int64_t {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            (CType::OptI64, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("clv_opt_i64 {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::OptI64,
                    repr: CRepr::Var(var),
                })
            }
            (CType::Bool, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("bool {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            (CType::OptBool, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("clv_opt_bool {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::OptBool,
                    repr: CRepr::Var(var),
                })
            }
            (CType::Nil, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("void* {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::Nil,
                    repr: CRepr::Var(var),
                })
            }
            (CType::Str, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines
                    .push(format!("char* {} = clv_str_clone({});", var, expr));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            (CType::OptStr, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("clv_opt_str {} = {};", var, expr));
                Ok(CValue {
                    ctype: CType::OptStr,
                    repr: CRepr::Var(var),
                })
            }
            (CType::MapKI64, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines.push(format!("clv_map_ki64 {} = {};", var, expr));
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            (CType::MapI64VecI64, CRepr::Expr(expr)) => {
                let var = format!("v_{}", sanitize(name));
                self.lines
                    .push(format!("clv_map_i64_vec_i64 {} = {};", var, expr));
                self.track_map_var(var.clone(), MapKind::I64VecI64);
                Ok(CValue {
                    ctype: CType::MapI64VecI64,
                    repr: CRepr::Var(var),
                })
            }
            (_, CRepr::Var(var)) => Ok(CValue {
                ctype: value.ctype,
                repr: CRepr::Var(var.clone()),
            }),
            _ => Err(BackendError {
                message: "unsupported def value".to_string(),
            }),
        }
    }

    fn truthy_expr_from_value(&self, value: &CValue) -> String {
        let repr = match &value.repr {
            CRepr::Expr(e) | CRepr::Var(e) => e.clone(),
        };
        match value.ctype {
            CType::Nil => "false".to_string(),
            CType::Bool => repr,
            CType::OptBool => format!("(({}).has && ({}).value)", repr, repr),
            CType::OptI64 | CType::OptStr => format!("({}).has", repr),
            CType::I64
            | CType::Str
            | CType::VecI64
            | CType::VecVecI64
            | CType::VecStr
            | CType::MapKI64
            | CType::MapI64VecI64 => "true".to_string(),
        }
    }

    fn c_type_name(ctype: &CType) -> &'static str {
        match ctype {
            CType::Nil => "void*",
            CType::I64 => "int64_t",
            CType::OptI64 => "clv_opt_i64",
            CType::VecI64 => "clv_vec_i64",
            CType::VecVecI64 => "clv_vec_vec_i64",
            CType::VecStr => "clv_vec_str",
            CType::MapKI64 => "clv_map_ki64",
            CType::MapI64VecI64 => "clv_map_i64_vec_i64",
            CType::Bool => "bool",
            CType::OptBool => "clv_opt_bool",
            CType::Str => "char*",
            CType::OptStr => "clv_opt_str",
        }
    }

    fn c_type_zero(ctype: &CType) -> Option<&'static str> {
        match ctype {
            CType::Nil => Some("NULL"),
            CType::I64 => Some("0"),
            CType::OptI64 => Some("(clv_opt_i64){ .has = false, .value = 0 }"),
            CType::Bool => Some("false"),
            CType::OptBool => Some("(clv_opt_bool){ .has = false, .value = false }"),
            CType::Str => Some("NULL"),
            CType::OptStr => Some("(clv_opt_str){ .has = false, .value = NULL }"),
            CType::VecI64
            | CType::VecVecI64
            | CType::VecStr
            | CType::MapKI64
            | CType::MapI64VecI64 => None,
        }
    }

    fn assign_result_var(
        &mut self,
        target: &str,
        ctype: &CType,
        value: CValue,
    ) -> Result<(), BackendError> {
        let repr = match value.repr {
            CRepr::Expr(e) | CRepr::Var(e) => e,
        };
        match ctype {
            CType::Str => {
                self.lines
                    .push(format!("{} = clv_str_clone({});", target, repr));
            }
            CType::OptI64 => match value.ctype {
                CType::I64 => self.lines.push(format!(
                    "{} = (clv_opt_i64){{ .has = true, .value = {} }};",
                    target, repr
                )),
                CType::Nil => self.lines.push(format!(
                    "{} = (clv_opt_i64){{ .has = false, .value = 0 }};",
                    target
                )),
                _ => self.lines.push(format!("{} = {};", target, repr)),
            },
            CType::OptBool => match value.ctype {
                CType::Bool => self.lines.push(format!(
                    "{} = (clv_opt_bool){{ .has = true, .value = {} }};",
                    target, repr
                )),
                CType::Nil => self.lines.push(format!(
                    "{} = (clv_opt_bool){{ .has = false, .value = false }};",
                    target
                )),
                _ => self.lines.push(format!("{} = {};", target, repr)),
            },
            CType::OptStr => {
                match value.ctype {
                    CType::Str => self.lines.push(format!(
                        "{} = (clv_opt_str){{ .has = true, .value = clv_str_clone({}) }};",
                        target, repr
                    )),
                    CType::Nil => self.lines.push(format!(
                        "{} = (clv_opt_str){{ .has = false, .value = NULL }};",
                        target
                    )),
                    _ => self.lines.push(format!(
                        "{} = ({}).has ? (clv_opt_str){{ .has = true, .value = clv_str_clone(({}).value) }} : (clv_opt_str){{ .has = false, .value = NULL }};",
                        target, repr, repr
                    )),
                }
            }
            _ => {
                self.lines.push(format!("{} = {};", target, repr));
            }
        }
        Ok(())
    }

    fn compile_expr_in_branch(
        &mut self,
        expr: &Expr,
    ) -> Result<(Vec<String>, CValue), BackendError> {
        let saved_lines = std::mem::take(&mut self.lines);
        let value = self.compile_expr(expr)?;
        let branch_lines = std::mem::take(&mut self.lines);
        self.lines = saved_lines;
        Ok((branch_lines, value))
    }

    fn emit_print_arg_stmt(&mut self, value: CValue, pr_mode: bool) -> Result<(), BackendError> {
        match (&value.ctype, &value.repr) {
            (CType::Nil, _) => {
                self.lines.push("printf(\"nil\");".to_string());
            }
            (CType::I64, CRepr::Var(e)) | (CType::I64, CRepr::Expr(e)) => {
                self.lines
                    .push(format!("printf(\"%lld\", (long long)({}));", e));
            }
            (CType::OptI64, CRepr::Var(e)) | (CType::OptI64, CRepr::Expr(e)) => {
                self.lines.push(format!(
                    "if (({}).has) {{ printf(\"%lld\", (long long)(({}).value)); }} else {{ printf(\"nil\"); }}",
                    e, e
                ));
            }
            (CType::Bool, CRepr::Var(e)) | (CType::Bool, CRepr::Expr(e)) => {
                self.lines
                    .push(format!("printf(\"%s\", ({}) ? \"true\" : \"false\");", e));
            }
            (CType::OptBool, CRepr::Var(e)) | (CType::OptBool, CRepr::Expr(e)) => {
                self.lines.push(format!(
                    "if (({}).has) {{ printf(\"%s\", (({}).value) ? \"true\" : \"false\"); }} else {{ printf(\"nil\"); }}",
                    e, e
                ));
            }
            (CType::Str, CRepr::Var(e)) | (CType::Str, CRepr::Expr(e)) => {
                if pr_mode {
                    let q = self.next_tmp("prn_q");
                    self.lines
                        .push(format!("char* {} = clv_pr_str_str({});", q, e));
                    self.str_vars.push(q.clone());
                    self.lines.push(format!("printf(\"%s\", {});", q));
                } else {
                    self.lines.push(format!("printf(\"%s\", {});", e));
                }
            }
            (CType::OptStr, CRepr::Var(e)) | (CType::OptStr, CRepr::Expr(e)) => {
                if pr_mode {
                    self.lines.push(format!(
                        "if (({}).has) {{ char* prn_q = clv_pr_str_str(({}).value); printf(\"%s\", prn_q); free(prn_q); }} else {{ printf(\"nil\"); }}",
                        e, e
                    ));
                } else {
                    self.lines.push(format!(
                        "if (({}).has) {{ printf(\"%s\", ({}).value); }} else {{ printf(\"nil\"); }}",
                        e, e
                    ));
                }
            }
            (CType::VecI64, CRepr::Var(e)) => {
                self.lines
                    .push(format!("clv_vec_i64_fprint(stdout, &{});", e));
            }
            (CType::VecStr, CRepr::Var(e)) => {
                self.lines
                    .push(format!("clv_vec_str_fprint(stdout, &{});", e));
            }
            (CType::VecVecI64, CRepr::Var(e)) => {
                self.lines
                    .push(format!("clv_vec_vec_i64_fprint(stdout, &{}, false);", e));
            }
            (CType::MapKI64, CRepr::Var(e)) => {
                self.lines
                    .push(format!("clv_map_ki64_print(&{}, false);", e));
            }
            (CType::MapI64VecI64, CRepr::Var(e)) => {
                self.lines.push(format!(
                    "clv_map_i64_vec_i64_fprint(stdout, &{}, false);",
                    e
                ));
            }
            _ => {
                return Err(BackendError {
                    message: "print currently supports Int/Bool/Str/Vec/Map values".to_string(),
                });
            }
        }
        Ok(())
    }

    fn emit_print_call(&mut self, args: &[Expr], callee: &str) -> Result<CValue, BackendError> {
        let pr_mode = callee == "prn" || callee == "pp";
        let newline = callee == "println" || callee == "prn" || callee == "pp";
        for (idx, arg) in args.iter().enumerate() {
            if idx > 0 {
                self.lines.push("printf(\" \");".to_string());
            }
            let arg_v = self.compile_expr(arg)?;
            let value = self.materialize_value("print_arg", arg_v)?;
            self.emit_print_arg_stmt(value, pr_mode)?;
        }
        if newline {
            self.lines.push("printf(\"\\n\");".to_string());
        }
        Ok(CValue {
            ctype: CType::Nil,
            repr: CRepr::Expr("NULL".to_string()),
        })
    }

    fn emit_top_expr(&mut self, expr: &Expr) -> Result<(), BackendError> {
        if let Expr::Call { callee, args } = expr {
            if callee == "println" && args.len() == 1 {
                let v = self.compile_expr(&args[0])?;
                match (v.ctype, v.repr) {
                    (CType::Nil, _) => {
                        self.lines.push("printf(\"nil\\n\");".to_string());
                        return Ok(());
                    }
                    (CType::I64, CRepr::Expr(e)) | (CType::I64, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("printf(\"%lld\\n\", (long long)({}));", e));
                        return Ok(());
                    }
                    (CType::OptI64, CRepr::Expr(e)) | (CType::OptI64, CRepr::Var(e)) => {
                        self.lines.push(format!(
                            "if (({}).has) {{ printf(\"%lld\\n\", (long long)(({}).value)); }} else {{ printf(\"nil\\n\"); }}",
                            e, e
                        ));
                        return Ok(());
                    }
                    (CType::Bool, CRepr::Expr(e)) | (CType::Bool, CRepr::Var(e)) => {
                        self.lines.push(format!(
                            "printf(\"%s\\n\", ({}) ? \"true\" : \"false\");",
                            e
                        ));
                        return Ok(());
                    }
                    (CType::OptBool, CRepr::Expr(e)) | (CType::OptBool, CRepr::Var(e)) => {
                        self.lines.push(format!(
                            "if (({}).has) {{ printf(\"%s\\n\", (({}).value) ? \"true\" : \"false\"); }} else {{ printf(\"nil\\n\"); }}",
                            e, e
                        ));
                        return Ok(());
                    }
                    (CType::Str, CRepr::Expr(e)) | (CType::Str, CRepr::Var(e)) => {
                        self.lines.push(format!("printf(\"%s\\n\", {});", e));
                        return Ok(());
                    }
                    (CType::OptStr, CRepr::Expr(e)) | (CType::OptStr, CRepr::Var(e)) => {
                        self.lines.push(format!(
                            "if (({}).has) {{ printf(\"%s\\n\", ({}).value); }} else {{ printf(\"nil\\n\"); }}",
                            e, e
                        ));
                        return Ok(());
                    }
                    (CType::MapKI64, CRepr::Var(e)) => {
                        self.lines.push(format!("clv_map_ki64_println(&{});", e));
                        return Ok(());
                    }
                    (CType::VecI64, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("clv_vec_i64_fprint(stdout, &{});", e));
                        self.lines.push("printf(\"\\n\");".to_string());
                        return Ok(());
                    }
                    (CType::VecStr, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("clv_vec_str_fprint(stdout, &{});", e));
                        self.lines.push("printf(\"\\n\");".to_string());
                        return Ok(());
                    }
                    (CType::VecVecI64, CRepr::Var(e)) => {
                        self.lines.push(format!("clv_vec_vec_i64_println(&{});", e));
                        return Ok(());
                    }
                    (CType::MapI64VecI64, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("clv_map_i64_vec_i64_println(&{});", e));
                        return Ok(());
                    }
                    _ => {}
                }
            }
            if matches!(callee.as_str(), "print" | "println" | "prn" | "pp") {
                self.emit_print_call(args, callee)?;
                return Ok(());
            }
        }
        let _ = self.compile_expr(expr)?;
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<CValue, BackendError> {
        match expr {
            Expr::Nil => Ok(CValue {
                ctype: CType::Nil,
                repr: CRepr::Expr("NULL".to_string()),
            }),
            Expr::Int(v) => Ok(CValue {
                ctype: CType::I64,
                repr: CRepr::Expr(format!("{}LL", v)),
            }),
            Expr::Bool(v) => Ok(CValue {
                ctype: CType::Bool,
                repr: CRepr::Expr(if *v { "true" } else { "false" }.to_string()),
            }),
            Expr::Str(v) => Ok(CValue {
                ctype: CType::Str,
                repr: CRepr::Expr(format!("\"{}\"", escape_c_string(v))),
            }),
            Expr::Keyword(v) => Ok(CValue {
                ctype: CType::Str,
                repr: CRepr::Expr(format!("\":{}\"", escape_c_string(v))),
            }),
            Expr::Symbol(name) => match self.bindings.get(name) {
                Some(Binding::Value(v)) => Ok(v.clone()),
                Some(Binding::Lambda { .. }) => Err(BackendError {
                    message: format!("symbol '{}' is lambda and cannot be used as value", name),
                }),
                None => Err(BackendError {
                    message: format!("unknown symbol: {}", name),
                }),
            },
            Expr::Map(entries) => {
                let var = self.next_tmp("map");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_ki64_new({});",
                    var,
                    entries.len().max(1)
                ));
                for (key, value) in entries {
                    let k = self.as_str_expr(key)?;
                    let v = self.as_i64_expr(value)?;
                    self.lines
                        .push(format!("clv_map_ki64_put(&{}, {}, {});", var, k, v));
                }
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            Expr::Vector(items) => {
                if items
                    .iter()
                    .all(|it| matches!(it, Expr::Str(_) | Expr::Keyword(_)))
                {
                    let var = self.next_tmp("vecs");
                    self.lines.push(format!(
                        "clv_vec_str {} = clv_vec_str_new({});",
                        var,
                        items.len()
                    ));
                    for item in items {
                        let e = self.as_str_expr(item)?;
                        self.lines
                            .push(format!("clv_vec_str_push(&{}, clv_str_clone({}));", var, e));
                    }
                    self.track_vec_var(var.clone(), VecKind::Str);
                    return Ok(CValue {
                        ctype: CType::VecStr,
                        repr: CRepr::Var(var),
                    });
                }
                let mut compiled_items = Vec::with_capacity(items.len());
                let mut all_vec_i64 = true;
                for item in items {
                    let value = self.compile_expr(item)?;
                    all_vec_i64 &= matches!(value.ctype, CType::VecI64);
                    compiled_items.push(value);
                }
                if all_vec_i64 {
                    let var = self.next_tmp("vecvv");
                    self.lines.push(format!(
                        "clv_vec_vec_i64 {} = clv_vec_vec_i64_new({});",
                        var,
                        items.len()
                    ));
                    for value in compiled_items {
                        let CRepr::Var(name) = value.repr else {
                            return Err(BackendError {
                                message: "internal error: nested Vec must be materialized variable"
                                    .to_string(),
                            });
                        };
                        self.lines.push(format!(
                            "clv_vec_vec_i64_push(&{}, clv_vec_copy_i64(&{}));",
                            var, name
                        ));
                    }
                    self.track_vec_var(var.clone(), VecKind::VecI64);
                    return Ok(CValue {
                        ctype: CType::VecVecI64,
                        repr: CRepr::Var(var),
                    });
                }
                let var = self.next_tmp("vec");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_new({});",
                    var,
                    items.len()
                ));
                for item in items {
                    let e = self.as_i64_expr(item)?;
                    self.lines.push(format!("clv_vec_push(&{}, {});", var, e));
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            Expr::Do(items) => {
                let mut last = CValue {
                    ctype: CType::Nil,
                    repr: CRepr::Expr("NULL".to_string()),
                };
                for item in items {
                    last = self.compile_expr(item)?;
                }
                Ok(last)
            }
            Expr::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_raw = self.compile_expr(cond)?;
                let cond_value = self.materialize_value("if_cond", cond_raw)?;
                let cond_expr = self.truthy_expr_from_value(&cond_value);
                let (then_lines, then_value) = self.compile_expr_in_branch(then_expr)?;
                let (else_lines, else_value) = self.compile_expr_in_branch(else_expr)?;
                let c_type = match (&then_value.ctype, &else_value.ctype) {
                    (left, right) if left == right => left.clone(),
                    (CType::I64, CType::Nil) | (CType::Nil, CType::I64) => CType::OptI64,
                    (CType::Bool, CType::Nil) | (CType::Nil, CType::Bool) => CType::OptBool,
                    (CType::Str, CType::Nil) | (CType::Nil, CType::Str) => CType::OptStr,
                    _ => {
                        return Err(BackendError {
                            message: "if branches must currently have the same type".to_string(),
                        });
                    }
                };
                let result_name = self.next_tmp("if_result");
                if !matches!(
                    c_type,
                    CType::Nil
                        | CType::I64
                        | CType::OptI64
                        | CType::Bool
                        | CType::OptBool
                        | CType::Str
                        | CType::OptStr
                ) {
                    return Err(BackendError {
                        message: "if currently supports Nil/Int/Bool/Str branches only".to_string(),
                    });
                }
                match Self::c_type_zero(&c_type) {
                    Some(init) => self.lines.push(format!(
                        "{} {} = {};",
                        Self::c_type_name(&c_type),
                        result_name,
                        init
                    )),
                    None => {
                        self.lines
                            .push(format!("{} {};", Self::c_type_name(&c_type), result_name))
                    }
                }
                match c_type {
                    CType::Str => self.str_vars.push(result_name.clone()),
                    CType::Nil | CType::I64 | CType::OptI64 | CType::Bool | CType::OptBool => {}
                    CType::OptStr => {}
                    CType::VecI64
                    | CType::VecVecI64
                    | CType::VecStr
                    | CType::MapKI64
                    | CType::MapI64VecI64 => unreachable!("guarded above"),
                }
                self.lines.push(format!("if ({}) {{", cond_expr));
                for line in then_lines {
                    self.lines.push(format!("  {}", line));
                }
                self.assign_result_var(&result_name, &c_type, then_value)?;
                self.lines.push("} else {".to_string());
                for line in else_lines {
                    self.lines.push(format!("  {}", line));
                }
                self.assign_result_var(&result_name, &c_type, else_value)?;
                self.lines.push("}".to_string());
                Ok(CValue {
                    ctype: c_type,
                    repr: CRepr::Var(result_name),
                })
            }
            Expr::Let { bindings, body } => {
                if let Some(result) = self.try_compile_loop_let(bindings, body) {
                    return result;
                }
                let mut saved = Vec::with_capacity(bindings.len());
                for (name, value_expr) in bindings {
                    let binding = match value_expr {
                        Expr::Lambda { params, body } => Binding::Lambda {
                            id: self.next_lambda_id(),
                            params: params.clone(),
                            body: body.as_ref().clone(),
                        },
                        _ => {
                            let raw_value = self.compile_expr(value_expr)?;
                            let value = self.materialize_value("let", raw_value)?;
                            Binding::Value(value)
                        }
                    };
                    let prev = self.bindings.insert(name.clone(), binding);
                    saved.push((name.clone(), prev));
                }
                let result = self.compile_expr(body);
                for (name, prev) in saved.into_iter().rev() {
                    if let Some(prev) = prev {
                        self.bindings.insert(name, prev);
                    } else {
                        self.bindings.remove(&name);
                    }
                }
                result
            }
            Expr::Lambda { .. } => Err(BackendError {
                message: "lambda value is only allowed as builtin argument".to_string(),
            }),
            Expr::Call { callee, args } => self.compile_call(callee, args),
        }
    }

    fn compile_call(&mut self, callee: &str, args: &[Expr]) -> Result<CValue, BackendError> {
        match callee {
            "+" | "-" | "*" | "/" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: format!("{} expects 2 args", callee),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) {} ({}))", lhs, callee, rhs)),
                })
            }
            "<" | "<=" | ">" | ">=" => {
                if args.len() <= 1 {
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr("true".to_string()),
                    });
                }
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    let expr = self.as_i64_expr(arg)?;
                    let var = self.next_tmp("ordered_arg");
                    self.lines.push(format!("int64_t {} = {};", var, expr));
                    values.push(var);
                }
                let mut parts = Vec::with_capacity(args.len() - 1);
                for i in 0..(args.len() - 1) {
                    parts.push(format!(
                        "(({}) {} ({}))",
                        values[i],
                        callee,
                        values[i + 1]
                    ));
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("({})", parts.join(" && "))),
                })
            }
            "=" => {
                if args.len() <= 1 {
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr("true".to_string()),
                    });
                }
                let mut vals = Vec::with_capacity(args.len());
                for arg in args {
                    let raw = self.compile_expr(arg)?;
                    vals.push(self.materialize_value("equal_arg", raw)?);
                }
                let all_i64 = vals.iter().all(|v| matches!(v.ctype, CType::I64));
                let all_bool = vals.iter().all(|v| matches!(v.ctype, CType::Bool));
                let all_str = vals.iter().all(|v| matches!(v.ctype, CType::Str));

                let repr_of = |v: &CValue| match &v.repr {
                    CRepr::Expr(e) | CRepr::Var(e) => e.clone(),
                };

                if all_i64 || all_bool {
                    let mut parts = Vec::with_capacity(vals.len() - 1);
                    for i in 0..(vals.len() - 1) {
                        parts.push(format!(
                            "(({}) == ({}))",
                            repr_of(&vals[i]),
                            repr_of(&vals[i + 1])
                        ));
                    }
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr(format!("({})", parts.join(" && "))),
                    });
                }
                if all_str {
                    let mut parts = Vec::with_capacity(vals.len() - 1);
                    for i in 0..(vals.len() - 1) {
                        parts.push(format!(
                            "(strcmp({}, {}) == 0)",
                            repr_of(&vals[i]),
                            repr_of(&vals[i + 1])
                        ));
                    }
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr(format!("({})", parts.join(" && "))),
                    });
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr("false".to_string()),
                })
            }
            "and" => {
                if args.is_empty() {
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr("true".to_string()),
                    });
                }
                let result = self.next_tmp("and");
                self.lines.push(format!("bool {} = true;", result));
                for arg in args {
                    let resources = self.resource_snapshot();
                    let body_start = self.lines.len();
                    let v = self.compile_expr(arg)?;
                    if !matches!(v.ctype, CType::Bool) {
                        return Err(BackendError {
                            message: "and expects Bool args in phase2 C subset".to_string(),
                        });
                    }
                    let e = match v.repr {
                        CRepr::Expr(e) | CRepr::Var(e) => e,
                    };
                    let body = self.lines.split_off(body_start);
                    let cleanup = self.take_scoped_cleanup(&resources);
                    self.lines.push(format!("if ({}) {{", result));
                    self.lines
                        .extend(body.into_iter().map(|line| format!("  {}", line)));
                    self.lines.push(format!("  {} = ({});", result, e));
                    self.lines
                        .extend(cleanup.into_iter().map(|line| format!("  {}", line)));
                    self.lines.push("}".to_string());
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(result),
                })
            }
            "or" => {
                if args.is_empty() {
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr("false".to_string()),
                    });
                }
                let result = self.next_tmp("or");
                self.lines.push(format!("bool {} = false;", result));
                for arg in args {
                    let resources = self.resource_snapshot();
                    let body_start = self.lines.len();
                    let v = self.compile_expr(arg)?;
                    if !matches!(v.ctype, CType::Bool) {
                        return Err(BackendError {
                            message: "or expects Bool args in phase2 C subset".to_string(),
                        });
                    }
                    let e = match v.repr {
                        CRepr::Expr(e) | CRepr::Var(e) => e,
                    };
                    let body = self.lines.split_off(body_start);
                    let cleanup = self.take_scoped_cleanup(&resources);
                    self.lines.push(format!("if (!{}) {{", result));
                    self.lines
                        .extend(body.into_iter().map(|line| format!("  {}", line)));
                    self.lines.push(format!("  {} = ({});", result, e));
                    self.lines
                        .extend(cleanup.into_iter().map(|line| format!("  {}", line)));
                    self.lines.push("}".to_string());
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(result),
                })
            }
            "identity" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "identity expects 1 arg".to_string(),
                    });
                }
                self.compile_expr(&args[0])
            }
            "not" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "not expects 1 arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[0])?;
                let pred = format!("!({})", self.truthy_expr_from_value(&v));
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(pred),
                })
            }
            "bool" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "bool expects 1 arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[0])?;
                match v.ctype {
                    CType::Bool => Ok(v),
                    CType::Str => {
                        let value = match v.repr {
                            CRepr::Expr(e) | CRepr::Var(e) => e,
                        };
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Expr(format!("clv_parse_bool_str({})", value)),
                        })
                    }
                    _ => Err(BackendError {
                        message: "bool expects Bool or Str in phase2 C subset".to_string(),
                    }),
                }
            }
            "boolean" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "boolean expects 1 arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(self.truthy_expr_from_value(&v)),
                })
            }
            "bit-shift-left" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-shift-left expects 2 args".to_string(),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_wrapping_shl_i64({}, {})", lhs, rhs)),
                })
            }
            "bit-shift-right" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-shift-right expects 2 args".to_string(),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_wrapping_shr_i64({}, {})", lhs, rhs)),
                })
            }
            "int" | "long" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "int expects 1 arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[0])?;
                match v.ctype {
                    CType::I64 => Ok(CValue {
                        ctype: CType::I64,
                        repr: v.repr,
                    }),
                    CType::Bool => {
                        let e = match v.repr {
                            CRepr::Expr(e) | CRepr::Var(e) => e,
                        };
                        Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Expr(format!("(({}) ? 1LL : 0LL)", e)),
                        })
                    }
                    _ => Err(BackendError {
                        message: "int expects Int/Bool in phase2 C subset".to_string(),
                    }),
                }
            }
            "as" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "as expects 2 args (Type value)".to_string(),
                    });
                }
                let tag = extract_type_tag(&args[0])?;
                let value = self.compile_expr(&args[1])?;
                let normalized = tag.to_ascii_lowercase();
                let optional_match = matches!(
                    (normalized.as_str(), &value.ctype),
                    ("int" | "integer" | "long" | "number", CType::OptI64)
                        | ("bool" | "boolean", CType::OptBool)
                        | ("str" | "string", CType::OptStr)
                );
                if type_matches_tag(&tag, &value.ctype) || optional_match {
                    Ok(value)
                } else if matches!(
                    normalized.as_str(),
                    "int"
                        | "integer"
                        | "long"
                        | "number"
                        | "bool"
                        | "boolean"
                        | "str"
                        | "string"
                        | "vec"
                        | "vector"
                        | "map"
                ) {
                    Ok(CValue {
                        ctype: CType::Nil,
                        repr: CRepr::Expr("NULL".to_string()),
                    })
                } else {
                    Err(BackendError {
                        message: format!("as unsupported type tag in phase2 C subset: {}", tag),
                    })
                }
            }
            "expect" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "expect expects 2 args (Type value)".to_string(),
                    });
                }
                let tag = extract_type_tag(&args[0])?;
                let value = self.compile_expr(&args[1])?;
                if type_matches_tag(&tag, &value.ctype) {
                    Ok(value)
                } else {
                    Err(BackendError {
                        message: format!("expect {} mismatch in phase2 C subset", tag),
                    })
                }
            }
            "bit-not" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "bit-not expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(~({}))", v)),
                })
            }
            "bit-and" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: "bit-and expects at least 1 arg".to_string(),
                    });
                }
                let mut acc = self.as_i64_expr(&args[0])?;
                for arg in &args[1..] {
                    let rhs = self.as_i64_expr(arg)?;
                    acc = format!("(({}) & ({}))", acc, rhs);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(acc),
                })
            }
            "bit-or" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: "bit-or expects at least 1 arg".to_string(),
                    });
                }
                let mut acc = self.as_i64_expr(&args[0])?;
                for arg in &args[1..] {
                    let rhs = self.as_i64_expr(arg)?;
                    acc = format!("(({}) | ({}))", acc, rhs);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(acc),
                })
            }
            "bit-xor" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: "bit-xor expects at least 1 arg".to_string(),
                    });
                }
                let mut acc = self.as_i64_expr(&args[0])?;
                for arg in &args[1..] {
                    let rhs = self.as_i64_expr(arg)?;
                    acc = format!("(({}) ^ ({}))", acc, rhs);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(acc),
                })
            }
            "bit-and-not" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-and-not expects 2 args".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let y = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) & ~({}))", x, y)),
                })
            }
            "bit-clear" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-clear expects 2 args".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let n = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_bit_clear_i64({}, {})", x, n)),
                })
            }
            "bit-flip" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-flip expects 2 args".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let n = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_bit_flip_i64({}, {})", x, n)),
                })
            }
            "bit-set" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-set expects 2 args".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let n = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_bit_set_i64({}, {})", x, n)),
                })
            }
            "bit-test" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bit-test expects 2 args".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let n = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("clv_bit_test_i64({}, {})", x, n)),
                })
            }
            "mod" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "mod expects 2 args".to_string(),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_mod_i64({}, {})", lhs, rhs)),
                })
            }
            "rem" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "rem expects 2 args".to_string(),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_rem_i64({}, {})", lhs, rhs)),
                })
            }
            "rand-int" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "rand-int expects 1 arg".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_rand_int_i64({})", n)),
                })
            }
            "rand" => {
                Err(BackendError {
                    message:
                        "rand is not supported in the phase2 C subset because Float is unsupported; use rand-int"
                            .to_string(),
                })
            }
            "compare" | "compare-desc" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "compare/compare-desc expects 2 args".to_string(),
                    });
                }
                let (lhs_expr, rhs_expr) = if callee == "compare-desc" {
                    (&args[1], &args[0])
                } else {
                    (&args[0], &args[1])
                };
                let lhs = self.as_i64_expr(lhs_expr)?;
                let rhs = self.as_i64_expr(rhs_expr)?;
                let lhs_var = self.next_tmp("compare_lhs");
                let rhs_var = self.next_tmp("compare_rhs");
                self.lines.push(format!("int64_t {} = {};", lhs_var, lhs));
                self.lines.push(format!("int64_t {} = {};", rhs_var, rhs));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!(
                        "((({}) < ({})) ? -1LL : ((({}) > ({})) ? 1LL : 0LL))",
                        lhs_var, rhs_var, lhs_var, rhs_var
                    )),
                })
            }
            "max" | "min" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: format!("{} expects at least 1 arg", callee),
                    });
                }
                let acc_var = self.next_tmp(callee);
                let first = self.as_i64_expr(&args[0])?;
                self.lines.push(format!("int64_t {} = {};", acc_var, first));
                for arg in &args[1..] {
                    let v = self.as_i64_expr(arg)?;
                    let value_var = self.next_tmp(callee);
                    self.lines
                        .push(format!("int64_t {} = {};", value_var, v));
                    if callee == "max" {
                        self.lines.push(format!(
                            "if (({}) > ({})) {} = ({});",
                            value_var, acc_var, acc_var, value_var
                        ));
                    } else {
                        self.lines.push(format!(
                            "if (({}) < ({})) {} = ({});",
                            value_var, acc_var, acc_var, value_var
                        ));
                    }
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(acc_var),
                })
            }
            "inc" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "inc expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) + 1LL)", v)),
                })
            }
            "dec" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "dec expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) - 1LL)", v)),
                })
            }
            "constantly" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "constantly expects 2 args in phase2 C subset".to_string(),
                    });
                }
                self.compile_expr(&args[0])
            }
            "comp" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "comp expects 3 args (f g x) in phase2 C subset".to_string(),
                    });
                }
                let outer = self.lower_map_op(&args[0])?;
                let inner = self.lower_map_op(&args[1])?;
                let x = self.as_i64_expr(&args[2])?;
                let tmp1 = self.next_tmp("comp_inner");
                self.lines.push(format!(
                    "int64_t {} = clv_apply_map_i64({}, {}LL, {});",
                    tmp1, inner.code, inner.k, x
                ));
                let tmp2 = self.next_tmp("comp_outer");
                self.lines.push(format!(
                    "int64_t {} = clv_apply_map_i64({}, {}LL, {});",
                    tmp2, outer.code, outer.k, tmp1
                ));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(tmp2),
                })
            }
            "__comp-call" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "__comp-call expects 2 args (fn-vector x)".to_string(),
                    });
                }
                let funcs = extract_symbol_vector(&args[0])?;
                if funcs.is_empty() {
                    return Err(BackendError {
                        message: "__comp-call requires at least one function".to_string(),
                    });
                }
                let Expr::Vector(call_args) = &args[1] else {
                    return Err(BackendError {
                        message: "__comp-call expects an argument vector".to_string(),
                    });
                };
                if call_args.len() != 1 {
                    return Err(BackendError {
                        message:
                            "__comp-call currently supports exactly one argument in phase2 C subset"
                                .to_string(),
                    });
                }
                let mut current = self.as_i64_expr(&call_args[0])?;
                for (i, name) in funcs.iter().rev().enumerate() {
                    let op = self.lower_map_op(&Expr::Symbol(name.clone()))?;
                    let next = self.next_tmp(&format!("comp_call_{}", i));
                    self.lines.push(format!(
                        "int64_t {} = clv_apply_map_i64({}, {}LL, {});",
                        next, op.code, op.k, current
                    ));
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(current),
                })
            }
            "pipe" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "pipe expects at least 2 args (...fns x) in phase2 C subset"
                            .to_string(),
                    });
                }
                let x = self.as_i64_expr(args.last().expect("pipe has at least 2 args"))?;
                let mut current = x;
                for func in &args[..args.len() - 1] {
                    let op = self.lower_map_op(func)?;
                    let next = self.next_tmp("pipe");
                    self.lines.push(format!(
                        "int64_t {} = clv_apply_map_i64({}, {}LL, {});",
                        next, op.code, op.k, current
                    ));
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(current),
                })
            }
            "juxt" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "juxt expects (...fns x) in phase2 C subset".to_string(),
                    });
                }
                let x_expr = self.as_i64_expr(args.last().expect("juxt has at least 2 args"))?;
                let x = self.next_tmp("juxt_input");
                self.lines.push(format!("int64_t {} = {};", x, x_expr));
                let funcs = &args[..args.len() - 1];
                let out = self.next_tmp("juxt");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_new({});",
                    out,
                    funcs.len().max(1)
                ));
                for (i, f) in funcs.iter().enumerate() {
                    let op = self.lower_map_op(f)?;
                    self.lines.push(format!(
                        "{}.data[{}] = clv_apply_map_i64({}, {}LL, {});",
                        out, i, op.code, op.k, x
                    ));
                }
                self.lines.push(format!("{}.len = {};", out, funcs.len()));
                self.track_vec_var(out.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(out),
                })
            }
            "__juxt-call" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "__juxt-call expects 2 args (fn-vector x)".to_string(),
                    });
                }
                let funcs = extract_symbol_vector(&args[0])?;
                let Expr::Vector(call_args) = &args[1] else {
                    return Err(BackendError {
                        message: "__juxt-call expects an argument vector".to_string(),
                    });
                };
                if call_args.len() != 1 {
                    return Err(BackendError {
                        message:
                            "__juxt-call currently supports exactly one argument in phase2 C subset"
                                .to_string(),
                    });
                }
                let x_expr = self.as_i64_expr(&call_args[0])?;
                let x = self.next_tmp("juxt_input");
                self.lines.push(format!("int64_t {} = {};", x, x_expr));
                let out = self.next_tmp("juxt");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_new({});",
                    out,
                    funcs.len().max(1)
                ));
                for (i, name) in funcs.iter().enumerate() {
                    let op = self.lower_map_op(&Expr::Symbol(name.clone()))?;
                    self.lines.push(format!(
                        "{}.data[{}] = clv_apply_map_i64({}, {}LL, {});",
                        out, i, op.code, op.k, x
                    ));
                }
                self.lines.push(format!("{}.len = {};", out, funcs.len()));
                self.track_vec_var(out.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(out),
                })
            }
            "complement" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "complement expects 2 args in phase2 C subset".to_string(),
                    });
                }
                let pred = self.lower_pred_op(&args[0])?;
                let x = self.as_i64_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!(
                        "(!clv_apply_pred_i64({}, {}LL, {}))",
                        pred.code, pred.k, x
                    )),
                })
            }
            "partial" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "partial expects 3 args (op k x) in phase2 C subset".to_string(),
                    });
                }
                let Expr::Symbol(op) = &args[0] else {
                    return Err(BackendError {
                        message: "partial first arg must be symbol op".to_string(),
                    });
                };
                let k = self.as_i64_expr(&args[1])?;
                let x = self.as_i64_expr(&args[2])?;
                let expr = match op.as_str() {
                    "+" => format!("(({}) + ({}))", x, k),
                    "-" => format!("(({}) - ({}))", x, k),
                    "*" => format!("(({}) * ({}))", x, k),
                    "mod" => format!("clv_mod_i64({}, {})", x, k),
                    "rem" => format!("clv_rem_i64({}, {})", x, k),
                    "quot" => format!("(({}) / ({}))", x, k),
                    _ => {
                        return Err(BackendError {
                            message: format!("partial unsupported op: {}", op),
                        });
                    }
                };
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(expr),
                })
            }
            "range" => {
                // `(range end)` starts at 0, like the interpreter. The public benchmark
                // in docs/phase2/bench uses that form.
                let (start, end) = match args {
                    [end] => ("0LL".to_string(), self.as_i64_expr(end)?),
                    [start, end] => (self.as_i64_expr(start)?, self.as_i64_expr(end)?),
                    _ => {
                        return Err(BackendError {
                            message: "range expects (end) or (start end)".to_string(),
                        })
                    }
                };
                let var = self.next_tmp("range");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_range_i64({}, {});",
                    var, start, end
                ));
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "iterate" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "iterate expects 3 args (f seed n)".to_string(),
                    });
                }
                let op = self.lower_map_op(&args[0])?;
                let seed = self.as_i64_expr(&args[1])?;
                let n = self.as_i64_expr(&args[2])?;
                let var = self.next_tmp("iterate");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_iterate_i64({}, {}LL, {}, {});",
                    var, op.code, op.k, seed, n
                ));
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "repeatedly" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "repeatedly expects 2 args (n f) in phase2 C subset".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let op = self.lower_map_op(&args[1])?;
                let var = self.next_tmp("repeatedly");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_repeatedly_i64({}, {}LL, {});",
                    var, op.code, op.k, n
                ));
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "partition" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "partition expects 2 or 3 args (n [step] coll)".to_string(),
                    });
                }
                let n_expr = self.as_i64_expr(&args[0])?;
                let n = self.next_tmp("partition_n");
                self.lines.push(format!("int64_t {} = {};", n, n_expr));
                let (step, coll_arg) = if args.len() == 3 {
                    (self.as_i64_expr(&args[1])?, &args[2])
                } else {
                    (n.clone(), &args[1])
                };
                let src = self.as_vec_input(coll_arg)?;
                let var = self.next_tmp("partition");
                self.lines.push(format!(
                    "clv_vec_vec_i64 {} = clv_partition_i64(&{}, {}, {}, false);",
                    var, src.name, n, step
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::VecI64);
                Ok(CValue {
                    ctype: CType::VecVecI64,
                    repr: CRepr::Var(var),
                })
            }
            "partition-all" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "partition-all expects 2 or 3 args (n [step] coll)".to_string(),
                    });
                }
                let n_expr = self.as_i64_expr(&args[0])?;
                let n = self.next_tmp("partition_all_n");
                self.lines.push(format!("int64_t {} = {};", n, n_expr));
                let (step, coll_arg) = if args.len() == 3 {
                    (self.as_i64_expr(&args[1])?, &args[2])
                } else {
                    (n.clone(), &args[1])
                };
                let src = self.as_vec_input(coll_arg)?;
                let var = self.next_tmp("partition_all");
                self.lines.push(format!(
                    "clv_vec_vec_i64 {} = clv_partition_i64(&{}, {}, {}, true);",
                    var, src.name, n, step
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::VecI64);
                Ok(CValue {
                    ctype: CType::VecVecI64,
                    repr: CRepr::Var(var),
                })
            }
            "partition-by" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "partition-by expects function and collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("partition_by");
                if let Ok(op) = self.lower_map_op(&args[0]) {
                    self.lines.push(format!(
                        "clv_vec_vec_i64 {} = clv_partition_by_map_i64(&{}, {}, {}LL);",
                        var, src.name, op.code, op.k
                    ));
                } else {
                    let pred = self.lower_pred_op(&args[0])?;
                    self.lines.push(format!(
                        "clv_vec_vec_i64 {} = clv_partition_by_pred_i64(&{}, {}, {}LL);",
                        var, src.name, pred.code, pred.k
                    ));
                }
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::VecI64);
                Ok(CValue {
                    ctype: CType::VecVecI64,
                    repr: CRepr::Var(var),
                })
            }
            "group-by" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "group-by expects function and collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("group_by");
                if let Ok(op) = self.lower_map_op(&args[0]) {
                    self.lines.push(format!(
                        "clv_map_i64_vec_i64 {} = clv_group_by_map_i64(&{}, {}, {}LL);",
                        var, src.name, op.code, op.k
                    ));
                } else {
                    let pred = self.lower_pred_op(&args[0])?;
                    self.lines.push(format!(
                        "clv_map_i64_vec_i64 {} = clv_group_by_pred_i64(&{}, {}, {}LL);",
                        var, src.name, pred.code, pred.k
                    ));
                }
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_map_var(var.clone(), MapKind::I64VecI64);
                Ok(CValue {
                    ctype: CType::MapI64VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "map" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "map expects function and collection".to_string(),
                    });
                }
                if let Expr::Call {
                    callee,
                    args: fargs,
                } = &args[0]
                {
                    if callee == "comp" && fargs.len() == 2 {
                        let outer = self.lower_map_op(&fargs[0])?;
                        let inner = self.lower_map_op(&fargs[1])?;
                        let src = self.as_vec_input(&args[1])?;
                        let var = self.next_tmp("map_comp");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_map_comp_i64(&{}, {}, {}LL, {}, {}LL);",
                            var, src.name, outer.code, outer.k, inner.code, inner.k
                        ));
                        if src.releasable {
                            self.release_vec_var(&src.name);
                        }
                        self.track_vec_var(var.clone(), VecKind::I64);
                        return Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        });
                    }
                }
                let op = self.lower_map_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("map");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_map_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "pvalues" | "mapcat" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: format!("{} expects function and collection", callee),
                    });
                }
                let op = self.lower_map_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp(if callee == "pvalues" {
                    "pvalues"
                } else {
                    "mapcat"
                });
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_map_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "map-indexed" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "map-indexed expects function and collection".to_string(),
                    });
                }
                let op = self.lower_map_indexed_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("map_indexed");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_map_indexed_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "filter" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "filter expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("filter");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_filter_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "keep-indexed" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "keep-indexed expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_indexed_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("keep_indexed");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_keep_indexed_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "keep" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "keep expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("keep");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_keep_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "remove" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "remove expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("remove");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_remove_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "every?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "every? expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("every");
                self.lines.push(format!(
                    "bool {} = clv_every_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            "not-every?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "not-every? expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("not_every");
                self.lines.push(format!(
                    "bool {} = !clv_every_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            "not-any?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "not-any? expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("not_any");
                self.lines.push(format!(
                    "bool {} = !clv_any_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            "some" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "some expects predicate and collection".to_string(),
                    });
                }
                let op = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("some");
                self.lines.push(format!(
                    "bool {} = clv_any_i64(&{}, {}, {}LL);",
                    var, src.name, op.code, op.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Var(var),
                })
            }
            "reduce" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "reduce expects op, init, and collection".to_string(),
                    });
                }
                let op = self.lower_reduce_op(&args[0])?;
                let init = self.as_i64_expr(&args[1])?;
                let src = self.as_vec_input(&args[2])?;
                let var = self.next_tmp("reduce");
                self.lines.push(format!(
                    "int64_t {} = clv_reduce_i64(&{}, {}, {});",
                    var, src.name, op.code, init
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "apply" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "apply expects function and collection in phase2 C subset"
                            .to_string(),
                    });
                }
                let op = match &args[0] {
                    Expr::Symbol(sym) => match sym.as_str() {
                        "+" => 1,
                        "*" => 2,
                        "max" => 3,
                        "min" => 4,
                        "bit-and" => 5,
                        "bit-or" => 6,
                        "bit-xor" => 7,
                        _ => {
                            return Err(BackendError {
                                message: format!("apply unsupported function: {}", sym),
                            });
                        }
                    },
                    _ => {
                        return Err(BackendError {
                            message: "apply function must be symbol in phase2 C subset".to_string(),
                        });
                    }
                };
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("apply");
                self.lines.push(format!(
                    "int64_t {} = clv_apply_builtin_i64({}, &{});",
                    var, op, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "hash-map" => {
                if args.len() % 2 != 0 {
                    return Err(BackendError {
                        message: "hash-map expects even number of args".to_string(),
                    });
                }
                let var = self.next_tmp("hash_map");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_ki64_new({});",
                    var,
                    (args.len() / 2).max(1)
                ));
                for pair in args.chunks_exact(2) {
                    let k = self.as_str_expr(&pair[0])?;
                    let v = self.as_i64_expr(&pair[1])?;
                    self.lines
                        .push(format!("clv_map_ki64_put(&{}, {}, {});", var, k, v));
                }
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            "zipmap" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "zipmap expects 2 args".to_string(),
                    });
                }
                let keys = self.as_vec_str_input(&args[0])?;
                let vals = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("zipmap");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_zipmap_ki64(&{}, &{});",
                    var, keys.name, vals.name
                ));
                if keys.releasable {
                    self.release_vec_var(&keys.name);
                }
                if vals.releasable {
                    self.release_vec_var(&vals.name);
                }
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            "keys" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "keys expects 1 arg".to_string(),
                    });
                }
                let src = self.as_map_input(&args[0])?;
                let var = self.next_tmp("keys");
                self.lines.push(format!(
                    "clv_vec_str {} = clv_map_keys_ki64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_map_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::Str);
                Ok(CValue {
                    ctype: CType::VecStr,
                    repr: CRepr::Var(var),
                })
            }
            "vals" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "vals expects 1 arg".to_string(),
                    });
                }
                let src = self.as_map_input(&args[0])?;
                let var = self.next_tmp("vals");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_map_vals_ki64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_map_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "dissoc" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "dissoc expects map and keys".to_string(),
                    });
                }
                let src = self.as_map_input(&args[0])?;
                let current = self.next_tmp("dissoc");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_copy_ki64(&{});",
                    current, src.name
                ));
                self.track_map_var(current.clone(), MapKind::KI64);
                if src.releasable {
                    self.release_map_var(&src.name);
                }
                for key_expr in &args[1..] {
                    let key = self.as_str_expr(key_expr)?;
                    self.lines
                        .push(format!("clv_map_ki64_dissoc(&{}, {});", current, key));
                }
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(current),
                })
            }
            "select-keys" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "select-keys expects map and keys".to_string(),
                    });
                }
                let src = self.as_map_input(&args[0])?;
                let keys = self.as_vec_str_input(&args[1])?;
                let var = self.next_tmp("select_keys");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_select_keys_ki64(&{}, &{});",
                    var, src.name, keys.name
                ));
                if src.releasable {
                    self.release_map_var(&src.name);
                }
                if keys.releasable {
                    self.release_vec_var(&keys.name);
                }
                self.track_map_var(var.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(var),
                })
            }
            "merge" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: "merge expects at least one map".to_string(),
                    });
                }
                let first = self.as_map_input(&args[0])?;
                let mut current = self.next_tmp("merge");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_copy_ki64(&{});",
                    current, first.name
                ));
                self.track_map_var(current.clone(), MapKind::KI64);
                if first.releasable {
                    self.release_map_var(&first.name);
                }
                for map_expr in &args[1..] {
                    let src = self.as_map_input(map_expr)?;
                    let next = self.next_tmp("merge");
                    self.lines.push(format!(
                        "clv_map_ki64 {} = clv_map_merge_ki64(&{}, &{});",
                        next, current, src.name
                    ));
                    self.release_map_var(&current);
                    if src.releasable {
                        self.release_map_var(&src.name);
                    }
                    self.track_map_var(next.clone(), MapKind::KI64);
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(current),
                })
            }
            "merge-with" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "merge-with expects f and maps".to_string(),
                    });
                }
                let merge_op = lower_merge_op(&args[0])?;
                let first = self.as_map_input(&args[1])?;
                let mut current = self.next_tmp("merge_with");
                self.lines.push(format!(
                    "clv_map_ki64 {} = clv_map_copy_ki64(&{});",
                    current, first.name
                ));
                self.track_map_var(current.clone(), MapKind::KI64);
                if first.releasable {
                    self.release_map_var(&first.name);
                }
                for map_expr in &args[2..] {
                    let src = self.as_map_input(map_expr)?;
                    let next = self.next_tmp("merge_with");
                    self.lines.push(format!(
                        "clv_map_ki64 {} = clv_map_merge_with_ki64({}, &{}, &{});",
                        next, merge_op, current, src.name
                    ));
                    self.release_map_var(&current);
                    if src.releasable {
                        self.release_map_var(&src.name);
                    }
                    self.track_map_var(next.clone(), MapKind::KI64);
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(current),
                })
            }
            "reduce-kv" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "reduce-kv expects f, init, map".to_string(),
                    });
                }
                let op = self.lower_reduce_kv_op(&args[0])?;
                let init = self.as_i64_expr(&args[1])?;
                let src = self.as_map_input(&args[2])?;
                let var = self.next_tmp("reduce_kv");
                self.lines.push(format!(
                    "int64_t {} = clv_reduce_kv_ki64({}, {}, &{});",
                    var, op, init, src.name
                ));
                if src.releasable {
                    self.release_map_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "nth" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "nth expects 2 or 3 args".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let idx = self.as_i64_expr(&args[1])?;
                let has_default = args.len() == 3;
                let default_value = if has_default {
                    self.as_i64_expr(&args[2])?
                } else {
                    "0LL".to_string()
                };
                let var = self.next_tmp("nth");
                self.lines.push(format!(
                    "int64_t {} = clv_nth_i64(&{}, {}, {}, {});",
                    var,
                    src.name,
                    idx,
                    if has_default { "true" } else { "false" },
                    default_value
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "get" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "get expects 2 or 3 args".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: get vec source must be var".to_string(),
                            });
                        };
                        let idx = self.as_i64_expr(&args[1])?;
                        let has_default = args.len() == 3;
                        let default_value = if has_default {
                            self.as_i64_expr(&args[2])?
                        } else {
                            "0LL".to_string()
                        };
                        let var = self.next_tmp("get");
                        self.lines.push(format!(
                            "int64_t {} = clv_nth_i64(&{}, {}, {}, {});",
                            var,
                            name,
                            idx,
                            if has_default { "true" } else { "false" },
                            default_value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: get map source must be var".to_string(),
                            });
                        };
                        let key = self.as_str_expr(&args[1])?;
                        let has_default = args.len() == 3;
                        let default_value = if has_default {
                            self.as_i64_expr(&args[2])?
                        } else {
                            "0LL".to_string()
                        };
                        let var = self.next_tmp("get");
                        self.lines.push(format!(
                            "int64_t {} = clv_map_ki64_get(&{}, {}, {}, {});",
                            var,
                            name,
                            key,
                            if has_default { "true" } else { "false" },
                            default_value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "get expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "get-in" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "get-in expects 2 or 3 args".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: get-in vec source must be var"
                                    .to_string(),
                            });
                        };
                        let idx = extract_single_index_path(&args[1])?;
                        let has_default = args.len() == 3;
                        let default_value = if has_default {
                            self.as_i64_expr(&args[2])?
                        } else {
                            "0LL".to_string()
                        };
                        let var = self.next_tmp("get_in");
                        self.lines.push(format!(
                            "int64_t {} = clv_nth_i64(&{}, {}LL, {}, {});",
                            var,
                            name,
                            idx,
                            if has_default { "true" } else { "false" },
                            default_value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: get-in map source must be var"
                                    .to_string(),
                            });
                        };
                        let key = extract_single_key_path(&args[1])?;
                        let has_default = args.len() == 3;
                        let default_value = if has_default {
                            self.as_i64_expr(&args[2])?
                        } else {
                            "0LL".to_string()
                        };
                        let var = self.next_tmp("get_in");
                        self.lines.push(format!(
                            "int64_t {} = clv_map_ki64_get(&{}, \"{}\", {}, {});",
                            var,
                            name,
                            escape_c_string(&key),
                            if has_default { "true" } else { "false" },
                            default_value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "get-in expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "update-in" => {
                if args.len() != 3 && args.len() != 4 {
                    return Err(BackendError {
                        message: "update-in expects 3 or 4 args in phase2 C subset".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                let op = self.lower_update_op(&args[2], args.get(3))?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: update-in vec source must be var"
                                    .to_string(),
                            });
                        };
                        let idx = extract_single_index_path(&args[1])?;
                        let old_var = self.next_tmp("update_old");
                        self.lines.push(format!(
                            "int64_t {} = clv_nth_i64(&{}, {}LL, false, 0LL);",
                            old_var, name, idx
                        ));
                        let new_var = self.next_tmp("update_new");
                        self.lines.push(format!(
                            "int64_t {} = clv_apply_update_i64({}, {}LL, {});",
                            new_var, op.code, op.k, old_var
                        ));
                        let out_var = self.next_tmp("update_in");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_assoc_i64(&{}, {}LL, {});",
                            out_var, name, idx, new_var
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(out_var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(out_var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: update-in map source must be var"
                                    .to_string(),
                            });
                        };
                        let key = extract_single_key_path(&args[1])?;
                        let old_var = self.next_tmp("update_old");
                        self.lines.push(format!(
                            "int64_t {} = clv_map_ki64_get(&{}, \"{}\", true, 0LL);",
                            old_var,
                            name,
                            escape_c_string(&key)
                        ));
                        let new_var = self.next_tmp("update_new");
                        self.lines.push(format!(
                            "int64_t {} = clv_apply_update_i64({}, {}LL, {});",
                            new_var, op.code, op.k, old_var
                        ));
                        let out_var = self.next_tmp("update_in");
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_map_assoc_ki64(&{}, \"{}\", {});",
                            out_var,
                            name,
                            escape_c_string(&key),
                            new_var
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        self.track_map_var(out_var.clone(), MapKind::KI64);
                        Ok(CValue {
                            ctype: CType::MapKI64,
                            repr: CRepr::Var(out_var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "update-in expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "update" => {
                if args.len() != 3 && args.len() != 4 {
                    return Err(BackendError {
                        message: "update expects 3 or 4 args in phase2 C subset".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                let op = self.lower_update_op(&args[2], args.get(3))?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: update vec source must be var"
                                    .to_string(),
                            });
                        };
                        let idx = self.as_i64_expr(&args[1])?;
                        let old_var = self.next_tmp("update_old");
                        self.lines.push(format!(
                            "int64_t {} = clv_nth_i64(&{}, {}, false, 0LL);",
                            old_var, name, idx
                        ));
                        let new_var = self.next_tmp("update_new");
                        self.lines.push(format!(
                            "int64_t {} = clv_apply_update_i64({}, {}LL, {});",
                            new_var, op.code, op.k, old_var
                        ));
                        let out_var = self.next_tmp("update");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_assoc_i64(&{}, {}, {});",
                            out_var, name, idx, new_var
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(out_var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(out_var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: update map source must be var"
                                    .to_string(),
                            });
                        };
                        let key = self.as_str_expr(&args[1])?;
                        let old_var = self.next_tmp("update_old");
                        self.lines.push(format!(
                            "int64_t {} = clv_map_ki64_get(&{}, {}, true, 0LL);",
                            old_var, name, key
                        ));
                        let new_var = self.next_tmp("update_new");
                        self.lines.push(format!(
                            "int64_t {} = clv_apply_update_i64({}, {}LL, {});",
                            new_var, op.code, op.k, old_var
                        ));
                        let out_var = self.next_tmp("update");
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_map_assoc_ki64(&{}, {}, {});",
                            out_var, name, key, new_var
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        self.track_map_var(out_var.clone(), MapKind::KI64);
                        Ok(CValue {
                            ctype: CType::MapKI64,
                            repr: CRepr::Var(out_var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "update expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "assoc" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "assoc expects 3 args in phase2 C subset".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: assoc vec source must be var".to_string(),
                            });
                        };
                        let idx = self.as_i64_expr(&args[1])?;
                        let value = self.as_i64_expr(&args[2])?;
                        let var = self.next_tmp("assoc");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_assoc_i64(&{}, {}, {});",
                            var, name, idx, value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: assoc map source must be var".to_string(),
                            });
                        };
                        let key = self.as_str_expr(&args[1])?;
                        let value = self.as_i64_expr(&args[2])?;
                        let var = self.next_tmp("assoc");
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_map_assoc_ki64(&{}, {}, {});",
                            var, name, key, value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        self.track_map_var(var.clone(), MapKind::KI64);
                        Ok(CValue {
                            ctype: CType::MapKI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "assoc expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "assoc-in" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "assoc-in expects 3 args in phase2 C subset".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: assoc-in vec source must be var"
                                    .to_string(),
                            });
                        };
                        let idx = extract_single_index_path(&args[1])?;
                        let value = self.as_i64_expr(&args[2])?;
                        let var = self.next_tmp("assoc_in");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_assoc_i64(&{}, {}LL, {});",
                            var, name, idx, value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: assoc-in map source must be var"
                                    .to_string(),
                            });
                        };
                        let key = extract_single_key_path(&args[1])?;
                        let value = self.as_i64_expr(&args[2])?;
                        let var = self.next_tmp("assoc_in");
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_map_assoc_ki64(&{}, \"{}\", {});",
                            var,
                            name,
                            escape_c_string(&key),
                            value
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        self.track_map_var(var.clone(), MapKind::KI64);
                        Ok(CValue {
                            ctype: CType::MapKI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "assoc-in expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "seq" | "not-empty" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: format!("{} expects 1 arg", callee),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp(if callee == "seq" { "seq" } else { "not_empty" });
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_not_empty_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "list" => {
                let var = self.next_tmp("list");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_new({});",
                    var,
                    args.len().max(1)
                ));
                for arg in args {
                    let e = self.as_i64_expr(arg)?;
                    self.lines.push(format!("clv_vec_push(&{}, {});", var, e));
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "str" | "pr-str" | "pp-str" => {
                let pr_mode = callee == "pr-str" || callee == "pp-str";
                if args.is_empty() {
                    return Ok(CValue {
                        ctype: CType::Str,
                        repr: CRepr::Expr("clv_str_clone(\"\")".to_string()),
                    });
                }

                let mut parts = Vec::with_capacity(args.len());
                for arg in args {
                    let v = self.compile_expr(arg)?;
                    let expr = self
                        .stringify_value_expr(v, pr_mode)
                        .map_err(|_| BackendError {
                            message: format!(
                                "{} currently supports Int/Bool/Str arguments only",
                                callee
                            ),
                        })?;
                    parts.push(expr);
                }
                if parts.len() == 1 {
                    return Ok(CValue {
                        ctype: CType::Str,
                        repr: CRepr::Expr(parts[0].clone()),
                    });
                }
                let mut acc_var = self.next_tmp(if pr_mode { "pr_str" } else { "str" });
                self.lines
                    .push(format!("char* {} = {};", acc_var, parts[0]));
                for part in &parts[1..] {
                    let next = self.next_tmp("str_cat");
                    self.lines.push(format!(
                        "char* {} = clv_str_concat2({}, {});",
                        next, acc_var, part
                    ));
                    acc_var = next;
                }
                self.str_vars.push(acc_var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(acc_var),
                })
            }
            "runtime-error" => {
                if args.is_empty() {
                    return Ok(CValue {
                        ctype: CType::Str,
                        repr: CRepr::Expr("clv_str_clone(\"runtime error\")".to_string()),
                    });
                }
                let mut parts = Vec::with_capacity(args.len());
                for arg in args {
                    let v = self.compile_expr(arg)?;
                    let expr = self
                        .stringify_value_expr(v, false)
                        .map_err(|_| BackendError {
                            message: "runtime-error supports Int/Bool/Str arguments only"
                                .to_string(),
                        })?;
                    parts.push(expr);
                }
                let mut acc = self.next_tmp("runtime_error");
                self.lines.push(format!("char* {} = {};", acc, parts[0]));
                for part in &parts[1..] {
                    let with_sep = self.next_tmp("runtime_error_sep");
                    self.lines.push(format!(
                        "char* {} = clv_str_concat2({}, \" \");",
                        with_sep, acc
                    ));
                    let next = self.next_tmp("runtime_error_cat");
                    self.lines.push(format!(
                        "char* {} = clv_str_concat2({}, {});",
                        next, with_sep, part
                    ));
                    acc = next;
                }
                self.str_vars.push(acc.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(acc),
                })
            }
            "throw" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "throw expects 1 arg".to_string(),
                    });
                }
                let throw_v = self.compile_expr(&args[0])?;
                let msg = self
                    .stringify_value_expr(throw_v, false)
                    .map_err(|_| BackendError {
                        message: "throw supports Int/Bool/Str values only in phase2 C subset"
                            .to_string(),
                    })?;
                self.lines
                    .push(format!("fprintf(stderr, \"%s\\n\", {});", msg));
                self.lines.push("abort();".to_string());
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr("0LL".to_string()),
                })
            }
            "time" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "time expects 1 arg in phase2 C subset".to_string(),
                    });
                }
                let start = self.next_tmp("time_start");
                let elapsed = self.next_tmp("time_elapsed");
                let result_var = self.next_tmp("time_result");
                let map = self.next_tmp("time_map");
                self.lines
                    .push(format!("int64_t {} = clv_now_ns();", start));
                let result = self.as_i64_expr(&args[0])?;
                self.lines
                    .push(format!("int64_t {} = {};", result_var, result));
                self.lines
                    .push(format!("int64_t {} = clv_now_ns() - {};", elapsed, start));
                self.lines
                    .push(format!("clv_map_ki64 {} = clv_map_ki64_new(3);", map));
                self.lines.push(format!(
                    "clv_map_ki64_put(&{}, \":result\", {});",
                    map, result_var
                ));
                self.lines
                    .push(format!("clv_map_ki64_put(&{}, \":runs\", 1LL);", map));
                self.lines.push(format!(
                    "clv_map_ki64_put(&{}, \":elapsed-ns\", {});",
                    map, elapsed
                ));
                self.track_map_var(map.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(map),
                })
            }
            "bench" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "bench expects 2 args (iterations expr) in phase2 C subset"
                            .to_string(),
                    });
                }
                let runs = self.as_i64_expr(&args[0])?;
                let runs_var = self.next_tmp("bench_runs");
                let start = self.next_tmp("bench_start");
                let elapsed = self.next_tmp("bench_elapsed");
                let i = self.next_tmp("bench_i");
                let result_var = self.next_tmp("bench_result");
                let map = self.next_tmp("bench_map");
                self.lines.push(format!(
                    "int64_t {} = (({}) < 0LL) ? 0LL : ({});",
                    runs_var, runs, runs
                ));
                self.lines
                    .push(format!("int64_t {} = clv_now_ns();", start));
                self.lines.push(format!("int64_t {} = 0LL;", result_var));
                let resources = self.resource_snapshot();
                let body_start = self.lines.len();
                let expr = self.as_i64_expr(&args[1])?;
                let body = self.lines.split_off(body_start);
                let cleanup = self.take_scoped_cleanup(&resources);
                self.lines.push(format!(
                    "for (int64_t {} = 0LL; {} < {}; ++{}) {{",
                    i, i, runs_var, i
                ));
                self.lines
                    .extend(body.into_iter().map(|line| format!("  {}", line)));
                self.lines.push(format!("  {} = {};", result_var, expr));
                self.lines
                    .extend(cleanup.into_iter().map(|line| format!("  {}", line)));
                self.lines.push("}".to_string());
                self.lines
                    .push(format!("int64_t {} = clv_now_ns() - {};", elapsed, start));
                self.lines
                    .push(format!("clv_map_ki64 {} = clv_map_ki64_new(3);", map));
                self.lines.push(format!(
                    "clv_map_ki64_put(&{}, \":result\", {});",
                    map, result_var
                ));
                self.lines.push(format!(
                    "clv_map_ki64_put(&{}, \":runs\", {});",
                    map, runs_var
                ));
                self.lines.push(format!(
                    "clv_map_ki64_put(&{}, \":elapsed-ns\", {});",
                    map, elapsed
                ));
                self.track_map_var(map.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(map),
                })
            }
            "run!" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "run! expects 2 args".to_string(),
                    });
                }
                let (param, body) = self.resolve_unary_callable(&args[0], "run!")?;
                if let Ok(src) = self.as_vec_input(&args[1]) {
                    let idx = self.next_tmp("run_i");
                    let item = self.next_tmp("run_item");
                    self.lines.push(format!(
                        "for (size_t {} = 0; {} < {}.len; ++{}) {{",
                        idx, idx, src.name, idx
                    ));
                    self.lines
                        .push(format!("int64_t {} = {}.data[{}];", item, src.name, idx));
                    self.with_temp_binding(
                        &param,
                        Binding::Value(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Var(item.clone()),
                        }),
                        |this| {
                            let _ = this.compile_expr(&body)?;
                            Ok(())
                        },
                    )?;
                    self.lines.push("}".to_string());
                    if src.releasable {
                        self.release_vec_var(&src.name);
                    }
                    return Ok(CValue {
                        ctype: CType::Nil,
                        repr: CRepr::Expr("NULL".to_string()),
                    });
                }
                let src = self.as_vec_str_input(&args[1])?;
                let idx = self.next_tmp("run_i");
                let item = self.next_tmp("run_item");
                self.lines.push(format!(
                    "for (size_t {} = 0; {} < {}.len; ++{}) {{",
                    idx, idx, src.name, idx
                ));
                self.lines
                    .push(format!("char* {} = {}.data[{}];", item, src.name, idx));
                self.with_temp_binding(
                    &param,
                    Binding::Value(CValue {
                        ctype: CType::Str,
                        repr: CRepr::Var(item.clone()),
                    }),
                    |this| {
                        let _ = this.compile_expr(&body)?;
                        Ok(())
                    },
                )?;
                self.lines.push("}".to_string());
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::Nil,
                    repr: CRepr::Expr("NULL".to_string()),
                })
            }
            "print" | "println" | "prn" | "pp" => self.emit_print_call(args, callee),
            "p" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "p expects 1 arg in phase2 C subset".to_string(),
                    });
                }
                let arg_v = self.compile_expr(&args[0])?;
                let v = self.materialize_value("p_arg", arg_v)?;
                match (&v.ctype, &v.repr) {
                    (CType::I64, CRepr::Var(e)) | (CType::I64, CRepr::Expr(e)) => {
                        self.lines
                            .push(format!("printf(\"%lld\\n\", (long long)({}));", e));
                    }
                    (CType::Bool, CRepr::Var(e)) | (CType::Bool, CRepr::Expr(e)) => {
                        self.lines.push(format!(
                            "printf(\"%s\\n\", ({}) ? \"true\" : \"false\");",
                            e
                        ));
                    }
                    (CType::Str, CRepr::Var(e)) | (CType::Str, CRepr::Expr(e)) => {
                        self.lines.push(format!("printf(\"%s\\n\", {});", e));
                    }
                    (CType::MapKI64, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("clv_map_ki64_print(&{}, true);", e));
                    }
                    _ => {
                        return Err(BackendError {
                            message: "p currently supports Int/Bool/Str/Map only".to_string(),
                        });
                    }
                }
                Ok(v)
            }
            "err" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "err expects 1 arg in phase2 C subset".to_string(),
                    });
                }
                let arg_v = self.compile_expr(&args[0])?;
                let v = self.materialize_value("err_arg", arg_v)?;
                match (&v.ctype, &v.repr) {
                    (CType::I64, CRepr::Var(e)) | (CType::I64, CRepr::Expr(e)) => {
                        self.lines
                            .push(format!("fprintf(stderr, \"%lld\\n\", (long long)({}));", e));
                    }
                    (CType::Bool, CRepr::Var(e)) | (CType::Bool, CRepr::Expr(e)) => {
                        self.lines.push(format!(
                            "fprintf(stderr, \"%s\\n\", ({}) ? \"true\" : \"false\");",
                            e
                        ));
                    }
                    (CType::Str, CRepr::Var(e)) | (CType::Str, CRepr::Expr(e)) => {
                        self.lines
                            .push(format!("fprintf(stderr, \"%s\\n\", {});", e));
                    }
                    (CType::MapKI64, CRepr::Var(e)) => {
                        self.lines
                            .push(format!("clv_map_ki64_fprint(stderr, &{}, true);", e));
                    }
                    _ => {
                        return Err(BackendError {
                            message: "err currently supports Int/Bool/Str/Map only".to_string(),
                        });
                    }
                }
                Ok(v)
            }
            "format" => {
                if args.is_empty() {
                    return Err(BackendError {
                        message: "format expects at least 1 arg".to_string(),
                    });
                }
                let fmt = self.as_str_expr(&args[0])?;
                if args.len() == 1 {
                    return Ok(CValue {
                        ctype: CType::Str,
                        repr: CRepr::Expr(format!("clv_str_clone({})", fmt)),
                    });
                }
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "format currently supports 1 format arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[1])?;
                let expr = match (v.ctype, v.repr) {
                    (CType::I64, CRepr::Expr(e)) | (CType::I64, CRepr::Var(e)) => {
                        format!("clv_format1_i64({}, {})", fmt, e)
                    }
                    (CType::Bool, CRepr::Expr(e)) | (CType::Bool, CRepr::Var(e)) => {
                        format!("clv_format1_bool({}, {})", fmt, e)
                    }
                    (CType::Str, CRepr::Expr(e)) | (CType::Str, CRepr::Var(e)) => {
                        format!("clv_format1_str({}, {})", fmt, e)
                    }
                    _ => {
                        return Err(BackendError {
                            message: "format currently supports Int/Bool/Str arg".to_string(),
                        });
                    }
                };
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(expr),
                })
            }
            "keyword" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "keyword expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_keyword_from_str({})", s)),
                })
            }
            "symbol" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "symbol expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_symbol_from_str({})", s)),
                })
            }
            "gensym" => {
                if args.len() > 1 {
                    return Err(BackendError {
                        message: "gensym expects 0 or 1 args".to_string(),
                    });
                }
                let prefix = if args.is_empty() {
                    "\"G__\"".to_string()
                } else {
                    self.as_str_expr(&args[0])?
                };
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_gensym({})", prefix)),
                })
            }
            "name" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "name expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_name_str({})", s)),
                })
            }
            "slurp" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "slurp expects 1 arg".to_string(),
                    });
                }
                let path = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_slurp_file({})", path)),
                })
            }
            "spit" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "spit expects 2 args".to_string(),
                    });
                }
                let path = self.as_str_expr(&args[0])?;
                let content = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_spit_file({}, {})", path, content)),
                })
            }
            "json::write-string" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "json::write-string expects 1 arg".to_string(),
                    });
                }
                let map = self.as_map_input(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_json_write_ki64(&{})", map.name)),
                })
            }
            "json::read-string" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "json::read-string expects 1 arg".to_string(),
                    });
                }
                let src = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Expr(format!("clv_json_read_ki64({})", src)),
                })
            }
            "regex" | "re-pattern" | "re-matcher" => {
                if callee == "re-matcher" {
                    if args.is_empty() || args.len() > 2 {
                        return Err(BackendError {
                            message: "re-matcher expects 1 or 2 args".to_string(),
                        });
                    }
                } else if args.len() != 1 {
                    return Err(BackendError {
                        message: "regex/re-pattern expects 1 arg".to_string(),
                    });
                }
                let pat = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_str_clone({})", pat)),
                })
            }
            "re-find" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "re-find expects 2 args".to_string(),
                    });
                }
                let pat = self.as_str_expr(&args[0])?;
                let src = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_re_find_str({}, {})", pat, src)),
                })
            }
            "re-matches" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "re-matches expects 2 args".to_string(),
                    });
                }
                let pat = self.as_str_expr(&args[0])?;
                let src = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Expr(format!("clv_re_matches_str({}, {})", pat, src)),
                })
            }
            "re-seq" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "re-seq expects 2 args".to_string(),
                    });
                }
                let pat = self.as_str_expr(&args[0])?;
                let src = self.as_str_expr(&args[1])?;
                let var = self.next_tmp("re_seq");
                self.lines.push(format!(
                    "clv_vec_str {} = clv_re_seq_str({}, {});",
                    var, pat, src
                ));
                self.track_vec_var(var.clone(), VecKind::Str);
                Ok(CValue {
                    ctype: CType::VecStr,
                    repr: CRepr::Var(var),
                })
            }
            "float" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "float expects 1 arg".to_string(),
                    });
                }
                // phase2 C subset currently does not expose Float value type.
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(v),
                })
            }
            "float?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "float? expects 1 arg".to_string(),
                    });
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr("false".to_string()),
                })
            }
            "instance?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "instance? expects 2 args".to_string(),
                    });
                }
                let tag = extract_type_tag(&args[0])?;
                let v = self.compile_expr(&args[1])?;
                let is_true = matches!(
                    (tag.as_str(), v.ctype),
                    ("Int", CType::I64)
                        | ("I64", CType::I64)
                        | ("Bool", CType::Bool)
                        | ("Str", CType::Str)
                        | ("Vec", CType::VecI64)
                        | ("VecStr", CType::VecStr)
                        | ("Map", CType::MapKI64)
                );
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(if is_true { "true" } else { "false" }.to_string()),
                })
            }
            "split" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "split expects string and separator".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let sep = self.as_str_expr(&args[1])?;
                let var = self.next_tmp("split");
                self.lines.push(format!(
                    "clv_vec_str {} = clv_split_str({}, {});",
                    var, s, sep
                ));
                self.track_vec_var(var.clone(), VecKind::Str);
                Ok(CValue {
                    ctype: CType::VecStr,
                    repr: CRepr::Var(var),
                })
            }
            "split-lines" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "split-lines expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("split_lines");
                self.lines
                    .push(format!("clv_vec_str {} = clv_split_lines_str({});", var, s));
                self.track_vec_var(var.clone(), VecKind::Str);
                Ok(CValue {
                    ctype: CType::VecStr,
                    repr: CRepr::Var(var),
                })
            }
            "lines" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "lines expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("lines");
                self.lines
                    .push(format!("clv_vec_str {} = clv_lines_str({});", var, s));
                self.track_vec_var(var.clone(), VecKind::Str);
                Ok(CValue {
                    ctype: CType::VecStr,
                    repr: CRepr::Var(var),
                })
            }
            "replace" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "replace expects 3 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let from = self.as_str_expr(&args[1])?;
                let to = self.as_str_expr(&args[2])?;
                let var = self.next_tmp("replace");
                self.lines.push(format!(
                    "char* {} = clv_replace_str({}, {}, {});",
                    var, s, from, to
                ));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "replace-first" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "replace-first expects 3 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let from = self.as_str_expr(&args[1])?;
                let to = self.as_str_expr(&args[2])?;
                let var = self.next_tmp("replace_first");
                self.lines.push(format!(
                    "char* {} = clv_replace_first_str({}, {}, {});",
                    var, s, from, to
                ));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "subs" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "subs expects 2 or 3 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let start = self.as_i64_expr(&args[1])?;
                let (has_end, end) = if args.len() == 3 {
                    (true, self.as_i64_expr(&args[2])?)
                } else {
                    (false, "0LL".to_string())
                };
                let var = self.next_tmp("subs");
                self.lines.push(format!(
                    "char* {} = clv_subs_str({}, {}, {}, {});",
                    var,
                    s,
                    start,
                    if has_end { "true" } else { "false" },
                    end
                ));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "upper-case" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "upper-case expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("upper");
                self.lines
                    .push(format!("char* {} = clv_upper_case_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "lower-case" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "lower-case expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("lower");
                self.lines
                    .push(format!("char* {} = clv_lower_case_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "capitalize" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "capitalize expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("capitalize");
                self.lines
                    .push(format!("char* {} = clv_capitalize_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "trim" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "trim expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("trim");
                self.lines
                    .push(format!("char* {} = clv_trim_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "triml" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "triml expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("triml");
                self.lines
                    .push(format!("char* {} = clv_triml_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "trimr" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "trimr expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("trimr");
                self.lines
                    .push(format!("char* {} = clv_trimr_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "trim-newline" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "trim-newline expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("trim_newline");
                self.lines
                    .push(format!("char* {} = clv_trim_newline_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "escape" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "escape expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("escape");
                self.lines
                    .push(format!("char* {} = clv_escape_runtime({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "reverse-str" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "reverse-str expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let var = self.next_tmp("reverse_str");
                self.lines
                    .push(format!("char* {} = clv_reverse_str({});", var, s));
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "blank?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "blank? expects 1 arg".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("clv_blank_str({})", s)),
                })
            }
            "starts-with?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "starts-with? expects 2 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let p = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("clv_starts_with_str({}, {})", s, p)),
                })
            }
            "ends-with?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "ends-with? expects 2 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let p = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("clv_ends_with_str({}, {})", s, p)),
                })
            }
            "includes?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "includes? expects 2 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let p = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("clv_includes_str({}, {})", s, p)),
                })
            }
            "index-of" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "index-of expects 2 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let needle = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_index_of_str({}, {})", s, needle)),
                })
            }
            "last-index-of" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "last-index-of expects 2 args".to_string(),
                    });
                }
                let s = self.as_str_expr(&args[0])?;
                let needle = self.as_str_expr(&args[1])?;
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("clv_last_index_of_str({}, {})", s, needle)),
                })
            }
            "join" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "join expects separator and collection".to_string(),
                    });
                }
                let sep = self.as_str_expr(&args[0])?;
                let src = self.as_vec_str_input(&args[1])?;
                let var = self.next_tmp("join");
                self.lines.push(format!(
                    "char* {} = clv_join_str(&{}, {});",
                    var, src.name, sep
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.str_vars.push(var.clone());
                Ok(CValue {
                    ctype: CType::Str,
                    repr: CRepr::Var(var),
                })
            }
            "sort" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "sort expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("sort");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_sort_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "sort-by" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "sort-by expects 2 or 3 args in phase2 C subset".to_string(),
                    });
                }
                let (keyfn, coll, desc) = if args.len() == 2 {
                    (&args[0], &args[1], false)
                } else {
                    let is_desc = match &args[1] {
                        Expr::Symbol(sym) if sym == "compare-desc" || sym == ">" => true,
                        Expr::Symbol(sym) if sym == "compare" || sym == "<" => false,
                        _ => {
                            return Err(BackendError {
                                message: "sort-by comparator supports compare/compare-desc/< /> only in phase2 C subset".to_string(),
                            });
                        }
                    };
                    (&args[0], &args[2], is_desc)
                };
                let op = self.lower_map_op(keyfn)?;
                let src = self.as_vec_input(coll)?;
                let var = self.next_tmp("sort_by");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_sort_by_i64(&{}, {}, {}LL, {});",
                    var,
                    src.name,
                    op.code,
                    op.k,
                    if desc { "true" } else { "false" }
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "frequencies" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "frequencies expects 1 arg".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                let (ctype, name) = match (src.ctype, src.repr) {
                    (ctype, CRepr::Var(name)) => (ctype, name),
                    (_, CRepr::Expr(_)) => {
                        return Err(BackendError {
                            message: "internal error: frequencies source must be materialized"
                                .to_string(),
                        });
                    }
                };
                let out = self.next_tmp("frequencies");
                match ctype {
                    CType::VecI64 => {
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_frequencies_i64(&{});",
                            out, name
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                    }
                    CType::VecStr => {
                        self.lines.push(format!(
                            "clv_map_ki64 {} = clv_frequencies_str(&{});",
                            out, name
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                    }
                    _ => {
                        return Err(BackendError {
                            message: "frequencies expects Vec<Int> or Vec<Str> in phase2 C subset"
                                .to_string(),
                        });
                    }
                }
                self.track_map_var(out.clone(), MapKind::KI64);
                Ok(CValue {
                    ctype: CType::MapKI64,
                    repr: CRepr::Var(out),
                })
            }
            "reverse" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "reverse expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("reverse");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_reverse_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "flatten" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "flatten expects 1 arg in phase2 C subset".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                let var = self.next_tmp("flatten");
                match src {
                    CValue {
                        ctype: CType::VecI64,
                        repr: CRepr::Var(name),
                    } => {
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_take_i64(&{}, {}.len);",
                            var, name, name
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CValue {
                        ctype: CType::VecVecI64,
                        repr: CRepr::Var(name),
                    } => {
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_flatten_vec_vec_i64(&{});",
                            var, name
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        self.track_vec_var(var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "flatten expects Vec<Int> or Vec<Vec<Int>> in phase2 C subset"
                            .to_string(),
                    }),
                }
            }
            "dorun" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "dorun expects 1 arg in phase2 C subset".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("dorun");
                self.lines
                    .push(format!("int64_t {} = clv_dorun_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "rseq" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "rseq expects 1 arg".to_string(),
                    });
                }
                let v = self.compile_expr(&args[0])?;
                match v.ctype {
                    CType::VecI64 => {
                        let src = self.materialize_value("rseq_src", v)?;
                        let src_expr = match src.repr {
                            CRepr::Expr(e) | CRepr::Var(e) => e,
                        };
                        let var = self.next_tmp("rseq");
                        self.lines.push(format!(
                            "clv_vec_i64 {} = clv_reverse_i64(&{});",
                            var, src_expr
                        ));
                        self.track_vec_var(var.clone(), VecKind::I64);
                        Ok(CValue {
                            ctype: CType::VecI64,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::Str => {
                        let s = match v.repr {
                            CRepr::Expr(e) | CRepr::Var(e) => e,
                        };
                        let var = self.next_tmp("rseq_str");
                        self.lines
                            .push(format!("char* {} = clv_reverse_str({});", var, s));
                        self.str_vars.push(var.clone());
                        Ok(CValue {
                            ctype: CType::Str,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "rseq currently supports Vec<Int>/Str only".to_string(),
                    }),
                }
            }
            "take" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "take expects n and collection".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("take");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_take_i64(&{}, {});",
                    var, src.name, n
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "take-while" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "take-while expects predicate and collection".to_string(),
                    });
                }
                let pred = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("take_while");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_take_while_i64(&{}, {}, {}LL);",
                    var, src.name, pred.code, pred.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "drop-while" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "drop-while expects predicate and collection".to_string(),
                    });
                }
                let pred = self.lower_pred_op(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("drop_while");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_drop_while_i64(&{}, {}, {}LL);",
                    var, src.name, pred.code, pred.k
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "drop-last" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "drop-last expects n and collection".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("drop_last");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_drop_last_i64(&{}, {});",
                    var, src.name, n
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "take-last" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "take-last expects n and collection".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("take_last");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_take_last_i64(&{}, {});",
                    var, src.name, n
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "butlast" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "butlast expects collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("butlast");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_butlast_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "pop" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "pop expects collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("pop");
                self.lines
                    .push(format!("clv_vec_i64 {} = clv_pop_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "empty" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "empty expects collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("empty");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_empty_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "cons" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "cons expects value and collection".to_string(),
                    });
                }
                let x = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("cons");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_cons_i64({}, &{});",
                    var, x, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "conj" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "conj expects collection and values".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let current = self.next_tmp("conj");
                if args.len() == 2 {
                    let x = self.as_i64_expr(&args[1])?;
                    self.lines.push(format!(
                        "clv_vec_i64 {} = clv_conj_i64(&{}, {});",
                        current, src.name, x
                    ));
                } else {
                    let mut values = Vec::with_capacity(args.len() - 1);
                    for arg in &args[1..] {
                        values.push(self.as_i64_expr(arg)?);
                    }
                    let values_arr = self.next_tmp("conj_vals");
                    self.lines.push(format!(
                        "int64_t {}[{}] = {{{}}};",
                        values_arr,
                        values.len(),
                        values.join(", ")
                    ));
                    self.lines.push(format!(
                        "clv_vec_i64 {} = clv_conj_many_i64(&{}, {}, {});",
                        current,
                        src.name,
                        values_arr,
                        values.len()
                    ));
                }
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(current.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(current),
                })
            }
            "repeat" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "repeat expects n and value".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let x = self.as_i64_expr(&args[1])?;
                let var = self.next_tmp("repeat");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_repeat_i64({}, {});",
                    var, n, x
                ));
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "interpose" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "interpose expects separator and collection".to_string(),
                    });
                }
                let sep = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("interpose");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_interpose_i64(&{}, {});",
                    var, src.name, sep
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "interleave" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "interleave expects at least 2 collections".to_string(),
                    });
                }
                let src0 = self.as_vec_input(&args[0])?;
                let src1 = self.as_vec_input(&args[1])?;
                let mut current = self.next_tmp("interleave");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_interleave_i64(&{}, &{});",
                    current, src0.name, src1.name
                ));
                if src0.releasable {
                    self.release_vec_var(&src0.name);
                }
                if src1.releasable {
                    self.release_vec_var(&src1.name);
                }
                self.track_vec_var(current.clone(), VecKind::I64);
                for expr in &args[2..] {
                    let src = self.as_vec_input(expr)?;
                    let next = self.next_tmp("interleave");
                    self.lines.push(format!(
                        "clv_vec_i64 {} = clv_interleave_i64(&{}, &{});",
                        next, current, src.name
                    ));
                    self.release_vec_var(&current);
                    if src.releasable {
                        self.release_vec_var(&src.name);
                    }
                    self.track_vec_var(next.clone(), VecKind::I64);
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(current),
                })
            }
            "zip" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "zip expects 2 collections in phase2 C subset".to_string(),
                    });
                }
                let a = self.as_vec_input(&args[0])?;
                let b = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("zip");
                self.lines.push(format!(
                    "clv_vec_vec_i64 {} = clv_zip_i64(&{}, &{});",
                    var, a.name, b.name
                ));
                if a.releasable {
                    self.release_vec_var(&a.name);
                }
                if b.releasable {
                    self.release_vec_var(&b.name);
                }
                self.track_vec_var(var.clone(), VecKind::VecI64);
                Ok(CValue {
                    ctype: CType::VecVecI64,
                    repr: CRepr::Var(var),
                })
            }
            "zip-with" => {
                if args.len() != 3 {
                    return Err(BackendError {
                        message: "zip-with expects op and 2 collections".to_string(),
                    });
                }
                let op = self.lower_zip_op(&args[0])?;
                let a = self.as_vec_input(&args[1])?;
                let b = self.as_vec_input(&args[2])?;
                let var = self.next_tmp("zip_with");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_zip_with_i64({}, &{}, &{});",
                    var, op.code, a.name, b.name
                ));
                if a.releasable {
                    self.release_vec_var(&a.name);
                }
                if b.releasable {
                    self.release_vec_var(&b.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "into" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "into expects destination and source".to_string(),
                    });
                }
                let dst = self.as_vec_input(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("into");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_into_i64(&{}, &{});",
                    var, dst.name, src.name
                ));
                if dst.releasable {
                    self.release_vec_var(&dst.name);
                }
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "dedupe" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "dedupe expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("dedupe");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_dedupe_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "distinct" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "distinct expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("distinct");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_distinct_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "shuffle" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "shuffle expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("shuffle");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_shuffle_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "rand-nth" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "rand-nth expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("rand_nth");
                self.lines.push(format!(
                    "int64_t {} = clv_rand_nth_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "vec" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "vec expects collection".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("vec_copy");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_copy_i64(&{});",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "vector" => {
                let var = self.next_tmp("vector");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_vec_new({});",
                    var,
                    args.len()
                ));
                for arg in args {
                    let e = self.as_i64_expr(arg)?;
                    self.lines.push(format!("clv_vec_push(&{}, {});", var, e));
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "first" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "first expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("first");
                self.lines
                    .push(format!("int64_t {} = clv_first_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "second" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "second expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("second");
                self.lines
                    .push(format!("int64_t {} = clv_second_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "last" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "last expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("last");
                self.lines
                    .push(format!("int64_t {} = clv_last_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "peek" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "peek expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("peek");
                self.lines
                    .push(format!("int64_t {} = clv_peek_i64(&{});", var, src.name));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "drop" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "drop expects n and collection".to_string(),
                    });
                }
                let n = self.as_i64_expr(&args[0])?;
                let src = self.as_vec_input(&args[1])?;
                let var = self.next_tmp("drop");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_drop_i64(&{}, {});",
                    var, src.name, n
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "rest" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "rest expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("rest");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_drop_i64(&{}, 1LL);",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "next" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "next expects 1 arg".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let var = self.next_tmp("next");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_drop_i64(&{}, 1LL);",
                    var, src.name
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "subvec" => {
                if args.len() != 2 && args.len() != 3 {
                    return Err(BackendError {
                        message: "subvec expects 2 or 3 args".to_string(),
                    });
                }
                let src = self.as_vec_input(&args[0])?;
                let start = self.as_i64_expr(&args[1])?;
                let has_end = args.len() == 3;
                let end = if has_end {
                    self.as_i64_expr(&args[2])?
                } else {
                    "0LL".to_string()
                };
                let var = self.next_tmp("subvec");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_subvec_i64(&{}, {}, {}, {});",
                    var,
                    src.name,
                    start,
                    if has_end { "true" } else { "false" },
                    end
                ));
                if src.releasable {
                    self.release_vec_var(&src.name);
                }
                self.track_vec_var(var.clone(), VecKind::I64);
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(var),
                })
            }
            "concat" => {
                if args.len() < 2 {
                    return Err(BackendError {
                        message: "concat expects at least 2 collections".to_string(),
                    });
                }
                let src0 = self.as_vec_input(&args[0])?;
                let src1 = self.as_vec_input(&args[1])?;
                let mut current = self.next_tmp("concat");
                self.lines.push(format!(
                    "clv_vec_i64 {} = clv_concat_i64(&{}, &{});",
                    current, src0.name, src1.name
                ));
                if src0.releasable {
                    self.release_vec_var(&src0.name);
                }
                if src1.releasable {
                    self.release_vec_var(&src1.name);
                }
                self.track_vec_var(current.clone(), VecKind::I64);

                for expr in &args[2..] {
                    let src = self.as_vec_input(expr)?;
                    let next = self.next_tmp("concat");
                    self.lines.push(format!(
                        "clv_vec_i64 {} = clv_concat_i64(&{}, &{});",
                        next, current, src.name
                    ));
                    self.release_vec_var(&current);
                    if src.releasable {
                        self.release_vec_var(&src.name);
                    }
                    self.track_vec_var(next.clone(), VecKind::I64);
                    current = next;
                }
                Ok(CValue {
                    ctype: CType::VecI64,
                    repr: CRepr::Var(current),
                })
            }
            "count" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "count expects 1 arg".to_string(),
                    });
                }
                if let Expr::Call {
                    callee: inner,
                    args: range_args,
                } = &args[0]
                {
                    if inner == "range" && range_args.len() == 2 {
                        let start = self.as_i64_expr(&range_args[0])?;
                        let end = self.as_i64_expr(&range_args[1])?;
                        return Ok(CValue {
                            ctype: CType::I64,
                            repr: CRepr::Expr(format!(
                                "(({} > {}) ? ({} - {}) : 0LL)",
                                end, start, end, start
                            )),
                        });
                    }
                }
                let src = self.compile_expr(&args[0])?;
                let (src_ctype, src_name) = match (src.ctype, src.repr) {
                    (ctype, CRepr::Var(name)) => (ctype, name),
                    (_, CRepr::Expr(_)) => {
                        return Err(BackendError {
                            message: "internal error: count source must be materialized"
                                .to_string(),
                        });
                    }
                };
                match src_ctype {
                    CType::VecI64
                    | CType::VecVecI64
                    | CType::VecStr
                    | CType::MapKI64
                    | CType::MapI64VecI64 => {}
                    _ => {
                        return Err(BackendError {
                            message: "count expects vector/map expression".to_string(),
                        });
                    }
                }
                let var = self.next_tmp("count");
                self.lines
                    .push(format!("int64_t {} = ((int64_t)({}.len));", var, src_name));
                if self.is_releasable_var(&src_name) {
                    match src_ctype {
                        CType::MapKI64 | CType::MapI64VecI64 => self.release_map_var(&src_name),
                        _ => self.release_vec_var(&src_name),
                    }
                }
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Var(var),
                })
            }
            "contains?" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "contains? expects collection and key/index".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: contains? vec source must be var"
                                    .to_string(),
                            });
                        };
                        let idx = self.as_i64_expr(&args[1])?;
                        let idx_var = self.next_tmp("contains_idx");
                        self.lines.push(format!("int64_t {} = {};", idx_var, idx));
                        let var = self.next_tmp("contains_q");
                        self.lines.push(format!(
                            "bool {} = ({} >= 0LL) && (((size_t)({})) < {}.len);",
                            var, idx_var, idx_var, name
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: contains? map source must be var"
                                    .to_string(),
                            });
                        };
                        let key = self.as_str_expr(&args[1])?;
                        let var = self.next_tmp("contains_q");
                        self.lines.push(format!(
                            "bool {} = clv_map_ki64_contains(&{}, {});",
                            var, name, key
                        ));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "contains? expects Vec<Int> or Map<Keyword,Int>".to_string(),
                    }),
                }
            }
            "empty?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "empty? expects 1 arg".to_string(),
                    });
                }
                let src = self.compile_expr(&args[0])?;
                match src.ctype {
                    CType::VecI64 | CType::VecVecI64 | CType::VecStr => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: empty? vec source must be var"
                                    .to_string(),
                            });
                        };
                        let var = self.next_tmp("empty_q");
                        self.lines
                            .push(format!("bool {} = ({}.len == 0);", var, name));
                        if self.is_releasable_var(&name) {
                            self.release_vec_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapKI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: empty? map source must be var"
                                    .to_string(),
                            });
                        };
                        let var = self.next_tmp("empty_q");
                        self.lines
                            .push(format!("bool {} = ({}.len == 0);", var, name));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Var(var),
                        })
                    }
                    CType::MapI64VecI64 => {
                        let CRepr::Var(name) = src.repr else {
                            return Err(BackendError {
                                message: "internal error: empty? map source must be var"
                                    .to_string(),
                            });
                        };
                        let var = self.next_tmp("empty_q");
                        self.lines
                            .push(format!("bool {} = ({}.len == 0);", var, name));
                        if self.is_releasable_var(&name) {
                            self.release_map_var(&name);
                        }
                        Ok(CValue {
                            ctype: CType::Bool,
                            repr: CRepr::Var(var),
                        })
                    }
                    _ => Err(BackendError {
                        message: "empty? expects collection".to_string(),
                    }),
                }
            }
            "not=" | "!=" => {
                if args.len() <= 1 {
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr("false".to_string()),
                    });
                }
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    let expr = self.as_i64_expr(arg)?;
                    let var = self.next_tmp("not_equal_arg");
                    self.lines.push(format!("int64_t {} = {};", var, expr));
                    values.push(var);
                }
                let first = &values[0];
                let mut parts = Vec::with_capacity(args.len() - 1);
                for value in &values[1..] {
                    parts.push(format!("(({}) != ({}))", first, value));
                }
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("({})", parts.join(" || "))),
                })
            }
            "abs" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "abs expects 1 arg".to_string(),
                    });
                }
                let expr = self.as_i64_expr(&args[0])?;
                let v = self.next_tmp("abs_arg");
                self.lines.push(format!("int64_t {} = {};", v, expr));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) < 0LL ? -({}) : ({}))", v, v, v)),
                })
            }
            "quot" => {
                if args.len() != 2 {
                    return Err(BackendError {
                        message: "quot expects 2 args".to_string(),
                    });
                }
                let lhs = self.as_i64_expr(&args[0])?;
                let rhs = self.as_i64_expr(&args[1])?;
                let rhs_var = self.next_tmp("quot_den");
                self.lines.push(format!("int64_t {} = {};", rhs_var, rhs));
                self.lines.push(format!(
                    "if ({} == 0LL) {{ fprintf(stderr, \"phase2 C: quot by zero\\n\"); abort(); }}",
                    rhs_var
                ));
                Ok(CValue {
                    ctype: CType::I64,
                    repr: CRepr::Expr(format!("(({}) / {})", lhs, rhs_var)),
                })
            }
            "true?" | "false?" | "number?" | "int?" | "integer?" | "string?" | "str?"
            | "vector?" | "vec?" | "coll?" | "sequential?" | "map?" | "keyword?" | "symbol?"
            | "nil?" | "some?" | "fn?" | "boolean?" | "bool?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: format!("{} expects 1 arg", callee),
                    });
                }

                if callee == "fn?" {
                    let is_fn = match &args[0] {
                        Expr::Lambda { .. } => true,
                        Expr::Symbol(name) => {
                            matches!(self.bindings.get(name), Some(Binding::Lambda { .. }))
                        }
                        _ => false,
                    };
                    return Ok(CValue {
                        ctype: CType::Bool,
                        repr: CRepr::Expr(if is_fn { "true" } else { "false" }.to_string()),
                    });
                }

                let arg_expr = &args[0];
                let raw = self.compile_expr(arg_expr)?;
                let v = self.materialize_value("predicate_arg", raw)?;
                let ctype = v.ctype.clone();
                let repr = match v.repr {
                    CRepr::Expr(e) => e,
                    CRepr::Var(e) => e,
                };
                let pred = match callee {
                    "true?" => match ctype {
                        CType::Bool => format!("({})", repr),
                        CType::OptBool => format!("(({}).has && ({}).value)", repr, repr),
                        _ => "false".to_string(),
                    },
                    "false?" => match ctype {
                        CType::Bool => format!("(!({}))", repr),
                        CType::OptBool => format!("(({}).has && !({}).value)", repr, repr),
                        _ => "false".to_string(),
                    },
                    "number?" | "int?" | "integer?" => match ctype {
                        CType::I64 => "true".to_string(),
                        CType::OptI64 => format!("({}).has", repr),
                        _ => "false".to_string(),
                    },
                    "string?" | "str?" => match ctype {
                        CType::Str => "true".to_string(),
                        CType::OptStr => format!("({}).has", repr),
                        _ => "false".to_string(),
                    },
                    "vector?" | "vec?" | "coll?" | "sequential?" => match ctype {
                        CType::VecI64 | CType::VecVecI64 | CType::VecStr => "true".to_string(),
                        _ => "false".to_string(),
                    },
                    "map?" => match ctype {
                        CType::MapKI64 | CType::MapI64VecI64 => "true".to_string(),
                        _ => "false".to_string(),
                    },
                    "keyword?" | "symbol?" => "false".to_string(),
                    "nil?" => match ctype {
                        CType::Nil => "true".to_string(),
                        CType::OptI64 | CType::OptBool | CType::OptStr => {
                            format!("(!({}).has)", repr)
                        }
                        _ => "false".to_string(),
                    },
                    "some?" => match ctype {
                        CType::Nil => "false".to_string(),
                        CType::OptI64 | CType::OptBool | CType::OptStr => {
                            format!("({}).has", repr)
                        }
                        _ => "true".to_string(),
                    },
                    "boolean?" | "bool?" => match ctype {
                        CType::Bool => "true".to_string(),
                        CType::OptBool => format!("({}).has", repr),
                        _ => "false".to_string(),
                    },
                    _ => {
                        return Err(BackendError {
                            message: format!("internal error: unknown predicate {}", callee),
                        });
                    }
                };
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(pred),
                })
            }
            "even?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "even? expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("(({}) % 2LL == 0LL)", v)),
                })
            }
            "zero?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "zero? expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("(({}) == 0LL)", v)),
                })
            }
            "pos?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "pos? expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("(({}) > 0LL)", v)),
                })
            }
            "neg?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "neg? expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("(({}) < 0LL)", v)),
                })
            }
            "odd?" => {
                if args.len() != 1 {
                    return Err(BackendError {
                        message: "odd? expects 1 arg".to_string(),
                    });
                }
                let v = self.as_i64_expr(&args[0])?;
                Ok(CValue {
                    ctype: CType::Bool,
                    repr: CRepr::Expr(format!("(({}) % 2LL != 0LL)", v)),
                })
            }
            _ => {
                if let Some(Binding::Lambda { id, params, body }) =
                    self.bindings.get(callee).cloned()
                {
                    return self.inline_call(callee, id, &params, &body, args);
                }
                Err(BackendError {
                    message: format!("unsupported call in phase2 C build: {}", callee),
                })
            }
        }
    }

    fn lower_map_op(&self, expr: &Expr) -> Result<MapOp, BackendError> {
        match expr {
            Expr::Symbol(sym) => match sym.as_str() {
                "identity" => Ok(MapOp { code: 0, k: 0 }),
                "int" => Ok(MapOp { code: 0, k: 0 }),
                "long" => Ok(MapOp { code: 0, k: 0 }),
                "inc" => Ok(MapOp { code: 1, k: 0 }),
                "dec" => Ok(MapOp { code: 2, k: 0 }),
                "abs" => Ok(MapOp { code: 9, k: 0 }),
                "bit-not" => Ok(MapOp { code: 18, k: 0 }),
                "rand" => Ok(MapOp { code: 20, k: 0 }),
                _ => self.lookup_lambda_map(sym),
            },
            Expr::Call { callee, args } => match callee.as_str() {
                "constantly" => {
                    if args.len() != 1 {
                        return Err(BackendError {
                            message: "constantly in map expects 1 arg".to_string(),
                        });
                    }
                    parse_const_i64(&args[0]).map(|k| MapOp { code: 26, k })
                }
                "partial" => {
                    if args.len() != 2 {
                        return Err(BackendError {
                            message: "partial in map expects op and const".to_string(),
                        });
                    }
                    let Expr::Symbol(op) = &args[0] else {
                        return Err(BackendError {
                            message: "partial in map expects symbol op".to_string(),
                        });
                    };
                    let k = parse_const_i64(&args[1])?;
                    match op.as_str() {
                        "+" => Ok(MapOp { code: 3, k }),
                        "-" => Ok(MapOp { code: 4, k }),
                        "*" => Ok(MapOp { code: 5, k }),
                        "max" => Ok(MapOp { code: 6, k }),
                        "min" => Ok(MapOp { code: 7, k }),
                        "mod" => Ok(MapOp { code: 8, k }),
                        "quot" => Ok(MapOp { code: 10, k }),
                        "rem" => Ok(MapOp { code: 11, k }),
                        _ => Err(BackendError {
                            message: format!("partial in map unsupported op: {}", op),
                        }),
                    }
                }
                _ => Err(BackendError {
                    message: "map expects unary function".to_string(),
                }),
            },
            Expr::Lambda { params, body } if params.len() == 1 => {
                lower_lambda_map(&params[0], body)
            }
            _ => Err(BackendError {
                message: "map expects unary function".to_string(),
            }),
        }
    }

    fn lookup_lambda_map(&self, symbol: &str) -> Result<MapOp, BackendError> {
        let Some((params, body)) = self.bindings.get(symbol).and_then(|b| b.as_lambda(1)) else {
            return Err(BackendError {
                message: format!("map unsupported function: {}", symbol),
            });
        };
        lower_lambda_map(&params[0], body)
    }

    fn resolve_unary_callable(
        &self,
        expr: &Expr,
        builtin_name: &str,
    ) -> Result<(String, Expr), BackendError> {
        match expr {
            Expr::Lambda { params, body } if params.len() == 1 => {
                Ok((params[0].clone(), body.as_ref().clone()))
            }
            Expr::Symbol(sym) => {
                if let Some((params, body)) = self.bindings.get(sym).and_then(|b| b.as_lambda(1)) {
                    Ok((params[0].clone(), body.clone()))
                } else {
                    let param = "__run_item".to_string();
                    Ok((
                        param.clone(),
                        Expr::Call {
                            callee: sym.clone(),
                            args: vec![Expr::Symbol(param)],
                        },
                    ))
                }
            }
            _ => Err(BackendError {
                message: format!("{} expects unary function", builtin_name),
            }),
        }
    }

    /// Compile a call to a user-defined function by expanding its body at the call site.
    ///
    /// The backend has no notion of a C function yet, so a call is an inline expansion.
    /// That makes recursion impossible to compile — expanding it never terminates — so
    /// recursive calls are reported instead of taking the build process down with a stack
    /// overflow. `inlining` holds the functions currently being expanded, which catches
    /// mutual recursion too.
    fn inline_call(
        &mut self,
        callee: &str,
        lambda_id: usize,
        params: &[String],
        body: &Expr,
        args: &[Expr],
    ) -> Result<CValue, BackendError> {
        if self.inlining.contains(&lambda_id) {
            return Err(BackendError {
                message: format!(
                    "recursive function '{}' is not supported by the C backend yet (calls are inlined); rewrite it with (loop ... (recur ...)) or run it with `clove {}`",
                    callee, "app.clv"
                ),
            });
        }
        if args.len() != params.len() {
            return Err(BackendError {
                message: format!(
                    "{} expects {} arg(s), got {}",
                    callee,
                    params.len(),
                    args.len()
                ),
            });
        }

        // Arguments are evaluated in the caller's scope, so compile them all before any
        // parameter shadows a name.
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            let raw = self.compile_expr(arg)?;
            values.push(self.materialize_value("call_arg", raw)?);
        }

        let mut saved = Vec::with_capacity(params.len());
        for (param, value) in params.iter().zip(values) {
            let previous = self.bindings.insert(param.clone(), Binding::Value(value));
            saved.push((param.clone(), previous));
        }
        self.inlining.push(lambda_id);
        let result = self.compile_expr(body);
        self.inlining.pop();
        for (name, previous) in saved.into_iter().rev() {
            match previous {
                Some(previous) => {
                    self.bindings.insert(name, previous);
                }
                None => {
                    self.bindings.remove(&name);
                }
            }
        }
        result
    }

    fn next_lambda_id(&mut self) -> usize {
        self.lambda_ids += 1;
        self.lambda_ids
    }

    fn with_temp_binding<T, F>(
        &mut self,
        name: &str,
        binding: Binding,
        f: F,
    ) -> Result<T, BackendError>
    where
        F: FnOnce(&mut Self) -> Result<T, BackendError>,
    {
        let previous = self.bindings.insert(name.to_string(), binding);
        let result = f(self);
        if let Some(previous) = previous {
            self.bindings.insert(name.to_string(), previous);
        } else {
            self.bindings.remove(name);
        }
        result
    }

    fn lower_pred_op(&self, expr: &Expr) -> Result<PredOp, BackendError> {
        match expr {
            Expr::Symbol(sym) => match sym.as_str() {
                "even?" => Ok(PredOp { code: 1, k: 0 }),
                "odd?" => Ok(PredOp { code: 2, k: 0 }),
                "zero?" => Ok(PredOp { code: 8, k: 0 }),
                "pos?" => Ok(PredOp { code: 9, k: 0 }),
                "neg?" => Ok(PredOp { code: 10, k: 0 }),
                "number?" | "int?" | "integer?" | "some?" => Ok(PredOp { code: 12, k: 0 }),
                "true?" | "false?" | "string?" | "str?" | "vector?" | "vec?" | "coll?"
                | "sequential?" | "map?" | "keyword?" | "symbol?" | "nil?" | "fn?" | "boolean?"
                | "bool?" => Ok(PredOp { code: 13, k: 0 }),
                _ => self.lookup_lambda_pred(sym),
            },
            Expr::Call { callee, args } => {
                if callee == "complement" {
                    if args.len() != 1 {
                        return Err(BackendError {
                            message: "complement expects 1 predicate arg".to_string(),
                        });
                    }
                    let inner = self.lower_pred_op(&args[0])?;
                    return Ok(PredOp {
                        code: inner.code + 100,
                        k: inner.k,
                    });
                }
                if callee == "partial" {
                    if args.len() != 2 {
                        return Err(BackendError {
                            message: "partial in predicate expects op and const".to_string(),
                        });
                    }
                    let Expr::Symbol(op) = &args[0] else {
                        return Err(BackendError {
                            message: "partial in predicate expects symbol op".to_string(),
                        });
                    };
                    let k = parse_const_i64(&args[1])?;
                    let code = match op.as_str() {
                        "<" => 3,
                        "<=" => 4,
                        ">" => 5,
                        ">=" => 6,
                        "=" => 7,
                        "not=" | "!=" => 11,
                        _ => {
                            return Err(BackendError {
                                message: format!("partial in predicate unsupported op: {}", op),
                            });
                        }
                    };
                    return Ok(PredOp { code, k });
                }
                Err(BackendError {
                    message: "filter expects unary predicate".to_string(),
                })
            }
            Expr::Lambda { params, body } if params.len() == 1 => {
                lower_lambda_pred(&params[0], body)
            }
            _ => Err(BackendError {
                message: "filter expects unary predicate".to_string(),
            }),
        }
    }

    fn lookup_lambda_pred(&self, symbol: &str) -> Result<PredOp, BackendError> {
        let Some((params, body)) = self.bindings.get(symbol).and_then(|b| b.as_lambda(1)) else {
            return Err(BackendError {
                message: format!("filter unsupported predicate: {}", symbol),
            });
        };
        lower_lambda_pred(&params[0], body)
    }

    fn lower_map_indexed_op(&self, expr: &Expr) -> Result<MapIndexedOp, BackendError> {
        match expr {
            Expr::Lambda { params, body } if params.len() == 2 => {
                lower_lambda_map_indexed(&params[0], &params[1], body)
            }
            Expr::Symbol(sym) => {
                if let Some((params, body)) = self.bindings.get(sym).and_then(|b| b.as_lambda(2)) {
                    return lower_lambda_map_indexed(&params[0], &params[1], body);
                }
                let base = self.lower_map_op(expr)?;
                Ok(MapIndexedOp {
                    code: 100 + base.code,
                    k: base.k,
                })
            }
            _ => {
                let base = self.lower_map_op(expr)?;
                Ok(MapIndexedOp {
                    code: 100 + base.code,
                    k: base.k,
                })
            }
        }
    }

    fn lower_pred_indexed_op(&self, expr: &Expr) -> Result<PredIndexedOp, BackendError> {
        match expr {
            Expr::Lambda { params, body } if params.len() == 2 => {
                lower_lambda_pred_indexed(&params[0], &params[1], body)
            }
            Expr::Symbol(sym) => {
                if let Some((params, body)) = self.bindings.get(sym).and_then(|b| b.as_lambda(2)) {
                    return lower_lambda_pred_indexed(&params[0], &params[1], body);
                }
                let base = self.lower_pred_op(expr)?;
                Ok(PredIndexedOp {
                    code: 100 + base.code,
                    k: base.k,
                })
            }
            _ => {
                let base = self.lower_pred_op(expr)?;
                Ok(PredIndexedOp {
                    code: 100 + base.code,
                    k: base.k,
                })
            }
        }
    }

    fn lower_zip_op(&self, expr: &Expr) -> Result<ZipOp, BackendError> {
        match expr {
            Expr::Symbol(sym) => {
                let code = match sym.as_str() {
                    "+" => 1,
                    "-" => 2,
                    "*" => 3,
                    "max" => 4,
                    "min" => 5,
                    "bit-and" => 6,
                    "bit-or" => 7,
                    "bit-xor" => 8,
                    _ => {
                        return Err(BackendError {
                            message: format!("zip-with unsupported op: {}", sym),
                        });
                    }
                };
                Ok(ZipOp { code })
            }
            _ => Err(BackendError {
                message: "zip-with expects symbol op in phase2 C subset".to_string(),
            }),
        }
    }

    fn lower_reduce_op(&self, expr: &Expr) -> Result<ReduceOp, BackendError> {
        match expr {
            Expr::Symbol(sym) => match sym.as_str() {
                "+" => Ok(ReduceOp { code: 1 }),
                "max" => Ok(ReduceOp { code: 2 }),
                "min" => Ok(ReduceOp { code: 3 }),
                _ => match self.bindings.get(sym) {
                    Some(binding) if binding.as_lambda(2).is_some() => {
                        let (params, body) = binding.as_lambda(2).expect("checked above");
                        lower_lambda_reduce(&params[0], &params[1], body)
                    }
                    _ => Err(BackendError {
                        message: format!("reduce unsupported op: {}", sym),
                    }),
                },
            },
            Expr::Lambda { params, body } if params.len() == 2 => {
                lower_lambda_reduce(&params[0], &params[1], body)
            }
            _ => Err(BackendError {
                message: "reduce expects symbol op".to_string(),
            }),
        }
    }

    fn lower_reduce_kv_op(&self, expr: &Expr) -> Result<i32, BackendError> {
        match expr {
            Expr::Symbol(sym) if sym == "+" => Ok(1),
            Expr::Symbol(sym) => match self.bindings.get(sym) {
                Some(binding) if binding.as_lambda(3).is_some() => {
                    let (params, body) = binding.as_lambda(3).expect("checked above");
                    lower_lambda_reduce_kv(&params[0], &params[2], body)
                }
                _ => Err(BackendError {
                    message: format!("reduce-kv unsupported reducer: {}", sym),
                }),
            },
            Expr::Lambda { params, body } if params.len() == 3 => {
                lower_lambda_reduce_kv(&params[0], &params[2], body)
            }
            _ => Err(BackendError {
                message: "reduce-kv expects reducer symbol or lambda3".to_string(),
            }),
        }
    }

    fn lower_update_op(
        &self,
        updater: &Expr,
        arg: Option<&Expr>,
    ) -> Result<UpdateOp, BackendError> {
        match (updater, arg) {
            (Expr::Symbol(sym), None) => match sym.as_str() {
                "identity" => Ok(UpdateOp { code: 0, k: 0 }),
                "inc" => Ok(UpdateOp { code: 1, k: 0 }),
                "dec" => Ok(UpdateOp { code: 2, k: 0 }),
                _ => Err(BackendError {
                    message: format!("update-in unsupported updater: {}", sym),
                }),
            },
            (Expr::Symbol(sym), Some(Expr::Int(k))) => match sym.as_str() {
                "+" => Ok(UpdateOp { code: 3, k: *k }),
                "-" => Ok(UpdateOp { code: 4, k: *k }),
                "*" => Ok(UpdateOp { code: 5, k: *k }),
                _ => Err(BackendError {
                    message: format!("update-in unsupported updater with arg: {}", sym),
                }),
            },
            _ => Err(BackendError {
                message: "update-in updater must be symbol (optional Int arg)".to_string(),
            }),
        }
    }
}

fn lower_lambda_map(param: &str, body: &Expr) -> Result<MapOp, BackendError> {
    if let Expr::Symbol(sym) = body {
        if sym == param {
            return Ok(MapOp { code: 0, k: 0 });
        }
    }
    let Expr::Call { callee, args } = body else {
        return Err(BackendError {
            message: "map lambda must be simple arithmetic".to_string(),
        });
    };
    match callee.as_str() {
        "bit-not" => {
            if args.len() != 1 {
                return Err(BackendError {
                    message: "map lambda bit-not must have 1 arg".to_string(),
                });
            }
            parse_param_only(param, &args[0]).map(|_| MapOp { code: 18, k: 0 })
        }
        "rand-int" => {
            if args.len() != 1 {
                return Err(BackendError {
                    message: "map lambda rand-int must have 1 arg".to_string(),
                });
            }
            parse_const_i64(&args[0]).map(|k| MapOp { code: 19, k })
        }
        "rand" => {
            if args.is_empty() {
                Ok(MapOp { code: 20, k: 0 })
            } else if args.len() == 1 {
                parse_const_i64(&args[0]).map(|k| MapOp { code: 20, k })
            } else {
                Err(BackendError {
                    message: "map lambda rand must have 0 or 1 arg".to_string(),
                })
            }
        }
        _ => {
            if args.len() != 2 {
                return Err(BackendError {
                    message: "map lambda arithmetic must have 2 args".to_string(),
                });
            }
            let same_param = is_sym(&args[0], param) && is_sym(&args[1], param);
            match callee.as_str() {
                "+" if same_param => Ok(MapOp { code: 28, k: 0 }),
                "+" => parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 3, k }),
                "-" => {
                    parse_param_const_left(param, &args[0], &args[1]).map(|k| MapOp { code: 4, k })
                }
                "*" if same_param => Ok(MapOp { code: 27, k: 0 }),
                "*" => parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 5, k }),
                "max" => parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 6, k }),
                "min" => parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 7, k }),
                "mod" => {
                    parse_param_const_right(param, &args[0], &args[1]).map(|k| MapOp { code: 8, k })
                }
                "quot" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 10, k }),
                "rem" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 11, k }),
                "compare" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 12, k })
                }
                "compare-desc" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 21, k })
                }
                "bit-and" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 13, k })
                }
                "bit-or" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 14, k })
                }
                "bit-xor" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 15, k })
                }
                "bit-shift-left" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 16, k }),
                "bit-shift-right" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 17, k }),
                "bit-and-not" => {
                    parse_param_const(param, &args[0], &args[1]).map(|k| MapOp { code: 22, k })
                }
                "bit-clear" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 23, k }),
                "bit-flip" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 24, k }),
                "bit-set" => parse_param_const_right(param, &args[0], &args[1])
                    .map(|k| MapOp { code: 25, k }),
                _ => Err(BackendError {
                    message:
                        "map lambda supports +,-,*,max,min,mod,quot,rem,compare/compare-desc,bit-*,rand and x+x/x*x only"
                            .to_string(),
                }),
            }
        }
    }
}

fn lower_lambda_pred(param: &str, body: &Expr) -> Result<PredOp, BackendError> {
    let Expr::Call { callee, args } = body else {
        return Err(BackendError {
            message: "predicate lambda must be comparison".to_string(),
        });
    };
    if args.len() != 2 {
        return Err(BackendError {
            message: "predicate comparison must have 2 args".to_string(),
        });
    }

    let mut out = None;
    if let (Expr::Symbol(sym), Expr::Int(k)) = (&args[0], &args[1]) {
        if sym == param {
            out = Some(match callee.as_str() {
                "<" => PredOp { code: 3, k: *k },
                "<=" => PredOp { code: 4, k: *k },
                ">" => PredOp { code: 5, k: *k },
                ">=" => PredOp { code: 6, k: *k },
                "=" => PredOp { code: 7, k: *k },
                "not=" | "!=" => PredOp { code: 11, k: *k },
                "bit-test" => PredOp { code: 14, k: *k },
                _ => {
                    return Err(BackendError {
                        message: "predicate supports < <= > >= = not= != bit-test".to_string(),
                    });
                }
            });
        }
    } else if let (Expr::Int(k), Expr::Symbol(sym)) = (&args[0], &args[1]) {
        if sym == param {
            out = Some(match callee.as_str() {
                "<" => PredOp { code: 5, k: *k },
                "<=" => PredOp { code: 6, k: *k },
                ">" => PredOp { code: 3, k: *k },
                ">=" => PredOp { code: 4, k: *k },
                "=" => PredOp { code: 7, k: *k },
                "not=" | "!=" => PredOp { code: 11, k: *k },
                _ => {
                    return Err(BackendError {
                        message: "predicate supports < <= > >= = not= !=".to_string(),
                    });
                }
            });
        }
    }
    out.ok_or_else(|| BackendError {
        message: "predicate lambda must compare parameter and constant".to_string(),
    })
}

fn lower_lambda_map_indexed(
    param_i: &str,
    param_x: &str,
    body: &Expr,
) -> Result<MapIndexedOp, BackendError> {
    match body {
        Expr::Symbol(sym) if sym == param_x => {
            return Ok(MapIndexedOp { code: 100, k: 0 });
        }
        Expr::Symbol(sym) if sym == param_i => {
            return Ok(MapIndexedOp { code: 8, k: 0 });
        }
        _ => {}
    }
    let Expr::Call { callee, args } = body else {
        return Err(BackendError {
            message: "map-indexed lambda must be simple arithmetic".to_string(),
        });
    };
    if args.len() != 2 {
        return Err(BackendError {
            message: "map-indexed lambda must have 2-arg arithmetic".to_string(),
        });
    }
    match callee.as_str() {
        "+" => {
            if is_sym(&args[0], param_i) && is_sym(&args[1], param_x)
                || is_sym(&args[0], param_x) && is_sym(&args[1], param_i)
            {
                return Ok(MapIndexedOp { code: 6, k: 0 });
            }
            if is_sym(&args[0], param_x) {
                return parse_const_i64(&args[1]).map(|k| MapIndexedOp { code: 3, k });
            }
            if is_sym(&args[1], param_x) {
                return parse_const_i64(&args[0]).map(|k| MapIndexedOp { code: 3, k });
            }
        }
        "-" => {
            if is_sym(&args[0], param_x) && is_sym(&args[1], param_i) {
                return Ok(MapIndexedOp { code: 7, k: 0 });
            }
            if is_sym(&args[0], param_x) {
                return parse_const_i64(&args[1]).map(|k| MapIndexedOp { code: 4, k });
            }
        }
        "*" => {
            if is_sym(&args[0], param_x) {
                return parse_const_i64(&args[1]).map(|k| MapIndexedOp { code: 5, k });
            }
            if is_sym(&args[1], param_x) {
                return parse_const_i64(&args[0]).map(|k| MapIndexedOp { code: 5, k });
            }
        }
        _ => {}
    }
    Err(BackendError {
        message: "map-indexed lambda unsupported (use i/x with +,-,*)".to_string(),
    })
}

fn lower_lambda_pred_indexed(
    param_i: &str,
    param_x: &str,
    body: &Expr,
) -> Result<PredIndexedOp, BackendError> {
    if let Expr::Call { callee, args } = body {
        if (callee == "even?" || callee == "odd?") && args.len() == 1 {
            if is_sym(&args[0], param_i) {
                return Ok(PredIndexedOp {
                    code: if callee == "even?" { 7 } else { 8 },
                    k: 0,
                });
            }
            if is_sym(&args[0], param_x) {
                let base = if callee == "even?" { 1 } else { 2 };
                return Ok(PredIndexedOp {
                    code: 100 + base,
                    k: 0,
                });
            }
        }
        if args.len() == 2 {
            if let (Expr::Symbol(sym), Expr::Int(k)) = (&args[0], &args[1]) {
                if sym == param_i {
                    let code = match callee.as_str() {
                        "<" => 1,
                        "<=" => 2,
                        ">" => 3,
                        ">=" => 4,
                        "=" => 5,
                        "not=" | "!=" => 6,
                        _ => {
                            return Err(BackendError {
                                message: "keep-indexed predicate on i supports < <= > >= = !="
                                    .to_string(),
                            });
                        }
                    };
                    return Ok(PredIndexedOp { code, k: *k });
                }
                if sym == param_x {
                    let base = match callee.as_str() {
                        "<" => 3,
                        "<=" => 4,
                        ">" => 5,
                        ">=" => 6,
                        "=" => 7,
                        "not=" | "!=" => 11,
                        _ => {
                            return Err(BackendError {
                                message: "keep-indexed predicate on x supports < <= > >= = !="
                                    .to_string(),
                            });
                        }
                    };
                    return Ok(PredIndexedOp {
                        code: 100 + base,
                        k: *k,
                    });
                }
            }
        }
    }
    Err(BackendError {
        message: "keep-indexed lambda unsupported (use i/x simple predicates)".to_string(),
    })
}

fn lower_lambda_reduce(
    param_acc: &str,
    param_val: &str,
    body: &Expr,
) -> Result<ReduceOp, BackendError> {
    let Expr::Call { callee, args } = body else {
        return Err(BackendError {
            message: "reduce lambda must be simple arithmetic".to_string(),
        });
    };
    if args.len() != 2 {
        return Err(BackendError {
            message: "reduce lambda must have 2 args".to_string(),
        });
    }
    let lhs_acc = matches!(&args[0], Expr::Symbol(s) if s == param_acc);
    let rhs_acc = matches!(&args[1], Expr::Symbol(s) if s == param_acc);
    let lhs_val = matches!(&args[0], Expr::Symbol(s) if s == param_val);
    let rhs_val = matches!(&args[1], Expr::Symbol(s) if s == param_val);
    let uses_pair = (lhs_acc && rhs_val) || (lhs_val && rhs_acc);
    if !uses_pair {
        return Err(BackendError {
            message: "reduce lambda must use accumulator and value directly".to_string(),
        });
    }
    match callee.as_str() {
        "+" => Ok(ReduceOp { code: 1 }),
        "max" => Ok(ReduceOp { code: 2 }),
        "min" => Ok(ReduceOp { code: 3 }),
        _ => Err(BackendError {
            message: "reduce lambda supports +, max, min only".to_string(),
        }),
    }
}

fn lower_lambda_reduce_kv(
    param_acc: &str,
    param_val: &str,
    body: &Expr,
) -> Result<i32, BackendError> {
    let Expr::Call { callee, args } = body else {
        return Err(BackendError {
            message: "reduce-kv lambda must be (+ acc v) in phase2 C subset".to_string(),
        });
    };
    if callee != "+" || args.len() != 2 {
        return Err(BackendError {
            message: "reduce-kv lambda must be (+ acc v) in phase2 C subset".to_string(),
        });
    }
    let left = matches!(&args[0], Expr::Symbol(s) if s == param_acc)
        && matches!(&args[1], Expr::Symbol(s) if s == param_val);
    let right = matches!(&args[0], Expr::Symbol(s) if s == param_val)
        && matches!(&args[1], Expr::Symbol(s) if s == param_acc);
    if left || right {
        Ok(1)
    } else {
        Err(BackendError {
            message: "reduce-kv lambda must use (+ acc v)".to_string(),
        })
    }
}

fn is_sym(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Symbol(sym) if sym == name)
}

fn parse_param_const(param: &str, lhs: &Expr, rhs: &Expr) -> Result<i64, BackendError> {
    if let (Expr::Symbol(sym), Expr::Int(k)) = (lhs, rhs) {
        if sym == param {
            return Ok(*k);
        }
    }
    if let (Expr::Int(k), Expr::Symbol(sym)) = (lhs, rhs) {
        if sym == param {
            return Ok(*k);
        }
    }
    Err(BackendError {
        message: "lambda must be param +/-/* const".to_string(),
    })
}

fn parse_param_only(param: &str, expr: &Expr) -> Result<(), BackendError> {
    if let Expr::Symbol(sym) = expr {
        if sym == param {
            return Ok(());
        }
    }
    Err(BackendError {
        message: "lambda must use parameter directly".to_string(),
    })
}

fn parse_const_i64(expr: &Expr) -> Result<i64, BackendError> {
    if let Expr::Int(k) = expr {
        return Ok(*k);
    }
    Err(BackendError {
        message: "lambda argument must be Int literal".to_string(),
    })
}

fn parse_param_const_left(param: &str, lhs: &Expr, rhs: &Expr) -> Result<i64, BackendError> {
    if let (Expr::Symbol(sym), Expr::Int(k)) = (lhs, rhs) {
        if sym == param {
            return Ok(*k);
        }
    }
    Err(BackendError {
        message: "subtraction lambda must be (param - const)".to_string(),
    })
}

fn parse_param_const_right(param: &str, lhs: &Expr, rhs: &Expr) -> Result<i64, BackendError> {
    if let (Expr::Symbol(sym), Expr::Int(k)) = (lhs, rhs) {
        if sym == param {
            return Ok(*k);
        }
    }
    Err(BackendError {
        message: "lambda must be (param op const) form".to_string(),
    })
}

fn extract_single_index_path(path: &Expr) -> Result<i64, BackendError> {
    let Expr::Vector(items) = path else {
        return Err(BackendError {
            message: "path must be vector literal in phase2 C subset".to_string(),
        });
    };
    if items.len() != 1 {
        return Err(BackendError {
            message: "phase2 C subset supports single-index path only".to_string(),
        });
    }
    let Expr::Int(idx) = items[0] else {
        return Err(BackendError {
            message: "path index must be Int literal".to_string(),
        });
    };
    Ok(idx)
}

fn extract_single_key_path(path: &Expr) -> Result<String, BackendError> {
    let Expr::Vector(items) = path else {
        return Err(BackendError {
            message: "path must be vector literal in phase2 C subset".to_string(),
        });
    };
    if items.len() != 1 {
        return Err(BackendError {
            message: "phase2 C subset supports single-key path only".to_string(),
        });
    }
    match &items[0] {
        Expr::Keyword(k) => Ok(format!(":{}", k)),
        Expr::Str(s) => Ok(s.clone()),
        _ => Err(BackendError {
            message: "path key must be keyword/string literal".to_string(),
        }),
    }
}

fn lower_merge_op(expr: &Expr) -> Result<i32, BackendError> {
    match expr {
        Expr::Symbol(sym) => match sym.as_str() {
            "+" => Ok(1),
            "max" => Ok(2),
            "min" => Ok(3),
            _ => Err(BackendError {
                message: format!("merge-with unsupported function: {}", sym),
            }),
        },
        _ => Err(BackendError {
            message: "merge-with expects symbol reducer".to_string(),
        }),
    }
}

fn extract_type_tag(expr: &Expr) -> Result<String, BackendError> {
    match expr {
        Expr::Symbol(s) | Expr::Str(s) => Ok(s.clone()),
        _ => Err(BackendError {
            message: "instance? first arg must be type tag symbol/string".to_string(),
        }),
    }
}

fn type_matches_tag(tag: &str, ctype: &CType) -> bool {
    let normalized = tag.to_ascii_lowercase();
    matches!(
        (normalized.as_str(), ctype),
        ("int", CType::I64)
            | ("integer", CType::I64)
            | ("long", CType::I64)
            | ("number", CType::I64)
            | ("bool", CType::Bool)
            | ("boolean", CType::Bool)
            | ("str", CType::Str)
            | ("string", CType::Str)
            | ("vec", CType::VecI64)
            | ("vec", CType::VecVecI64)
            | ("vec", CType::VecStr)
            | ("vector", CType::VecI64)
            | ("vector", CType::VecVecI64)
            | ("vector", CType::VecStr)
            | ("map", CType::MapKI64)
            | ("map", CType::MapI64VecI64)
    )
}

fn extract_symbol_vector(expr: &Expr) -> Result<Vec<String>, BackendError> {
    let Expr::Vector(items) = expr else {
        return Err(BackendError {
            message: "expected vector literal of symbols".to_string(),
        });
    };
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        if let Expr::Symbol(sym) = item {
            out.push(sym.clone());
        } else {
            return Err(BackendError {
                message: "vector must contain symbols only in phase2 C subset".to_string(),
            });
        }
    }
    Ok(out)
}

fn sanitize(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
        } else {
            out.push('_');
            out.push_str(&format!("{:x}", ch as u32));
            out.push('_');
        }
    }
    if out.is_empty() {
        "v".to_string()
    } else {
        out
    }
}

fn escape_c_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn runtime_prelude(_allow_external_c_libs: bool) -> String {
    r#"
typedef struct {
  int64_t* data;
  size_t len;
  size_t cap;
} clv_vec_i64;

typedef struct {
  char** data;
  size_t len;
  size_t cap;
} clv_vec_str;

typedef struct {
  clv_vec_i64* data;
  size_t len;
  size_t cap;
} clv_vec_vec_i64;

typedef struct clv_arena_chunk {
  char* data;
  size_t len;
  size_t cap;
  struct clv_arena_chunk* next;
} clv_arena_chunk;

typedef struct {
  clv_arena_chunk* head;
  clv_arena_chunk* tail;
} clv_arena;

static clv_arena CLV_STR_ARENA = {NULL, NULL};

static void* clv_arena_alloc(size_t n) {
  if (n == 0) {
    n = 1;
  }
  clv_arena_chunk* chunk = CLV_STR_ARENA.tail;
  if (!chunk || chunk->cap - chunk->len < n) {
    size_t cap = 4096;
    while (cap < n) {
      cap *= 2;
    }
    clv_arena_chunk* next = (clv_arena_chunk*)malloc(sizeof(clv_arena_chunk));
    if (!next) {
      fprintf(stderr, "phase2 C: arena alloc failed\n");
      abort();
    }
    next->data = (char*)malloc(cap);
    if (!next->data) {
      free(next);
      fprintf(stderr, "phase2 C: arena alloc failed\n");
      abort();
    }
    next->len = 0;
    next->cap = cap;
    next->next = NULL;
    if (CLV_STR_ARENA.tail) {
      CLV_STR_ARENA.tail->next = next;
    } else {
      CLV_STR_ARENA.head = next;
    }
    CLV_STR_ARENA.tail = next;
    chunk = next;
  }
  void* ptr = chunk->data + chunk->len;
  chunk->len += n;
  return ptr;
}

static void clv_arena_dispose(void) {
  clv_arena_chunk* chunk = CLV_STR_ARENA.head;
  while (chunk) {
    clv_arena_chunk* next = chunk->next;
    free(chunk->data);
    free(chunk);
    chunk = next;
  }
  CLV_STR_ARENA.head = NULL;
  CLV_STR_ARENA.tail = NULL;
}

static uint64_t clv_i64_bits(int64_t value) {
  uint64_t bits;
  memcpy(&bits, &value, sizeof(bits));
  return bits;
}

static int64_t clv_i64_from_bits(uint64_t bits) {
  int64_t value;
  memcpy(&value, &bits, sizeof(value));
  return value;
}

static int64_t clv_wrapping_shl_i64(int64_t value, int64_t shift) {
  uint32_t count = (uint32_t)((uint64_t)shift & 63ULL);
  return clv_i64_from_bits(clv_i64_bits(value) << count);
}

static int64_t clv_wrapping_shr_i64(int64_t value, int64_t shift) {
  uint32_t count = (uint32_t)((uint64_t)shift & 63ULL);
  if (count == 0) {
    return value;
  }
  uint64_t bits = clv_i64_bits(value);
  uint64_t shifted = bits >> count;
  if ((bits & (1ULL << 63)) != 0) {
    shifted |= UINT64_MAX << (64U - count);
  }
  return clv_i64_from_bits(shifted);
}

static uint64_t clv_bit_mask_i64(int64_t index) {
  uint32_t count = (uint32_t)((uint64_t)index & 63ULL);
  return 1ULL << count;
}

static bool clv_bit_test_i64(int64_t value, int64_t index) {
  return (clv_i64_bits(value) & clv_bit_mask_i64(index)) != 0;
}

static int64_t clv_bit_set_i64(int64_t value, int64_t index) {
  return clv_i64_from_bits(clv_i64_bits(value) | clv_bit_mask_i64(index));
}

static int64_t clv_bit_clear_i64(int64_t value, int64_t index) {
  return clv_i64_from_bits(clv_i64_bits(value) & ~clv_bit_mask_i64(index));
}

static int64_t clv_bit_flip_i64(int64_t value, int64_t index) {
  return clv_i64_from_bits(clv_i64_bits(value) ^ clv_bit_mask_i64(index));
}

static int64_t clv_rem_i64(int64_t value, int64_t divisor) {
  if (divisor == 0) {
    fprintf(stderr, "phase2 C: rem by zero\n");
    abort();
  }
  if (value == INT64_MIN && divisor == -1) {
    return 0;
  }
  return value % divisor;
}

static int64_t clv_mod_i64(int64_t value, int64_t divisor) {
  if (divisor == 0) {
    fprintf(stderr, "phase2 C: mod by zero\n");
    abort();
  }
  int64_t remainder = clv_rem_i64(value, divisor);
  if (remainder >= 0) {
    return remainder;
  }
  uint64_t magnitude = divisor < 0
    ? (uint64_t)(-(divisor + 1)) + 1ULL
    : (uint64_t)divisor;
  uint64_t remainder_magnitude = (uint64_t)(-(remainder + 1)) + 1ULL;
  return (int64_t)(magnitude - remainder_magnitude);
}

static char* clv_str_clone_n(const char* s, size_t n) {
  char* out = (char*)clv_arena_alloc(n + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  if (n > 0) {
    memcpy(out, s, n);
  }
  out[n] = '\0';
  return out;
}

static char* clv_str_clone(const char* s) {
  return clv_str_clone_n(s, strlen(s));
}

static char* clv_i64_to_str(int64_t x) {
  char buf[32];
  int n = snprintf(buf, sizeof(buf), "%lld", (long long)x);
  if (n < 0) {
    fprintf(stderr, "phase2 C: snprintf failed\n");
    abort();
  }
  return clv_str_clone_n(buf, (size_t)n);
}

static char* clv_bool_to_str(bool x) {
  return clv_str_clone(x ? "true" : "false");
}

static char* clv_pr_str_str(const char* s) {
  size_t len = strlen(s);
  char* out = (char*)clv_arena_alloc(len + 3);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  out[0] = '"';
  if (len > 0) {
    memcpy(out + 1, s, len);
  }
  out[len + 1] = '"';
  out[len + 2] = '\0';
  return out;
}

static char* clv_str_concat2(const char* a, const char* b) {
  size_t na = strlen(a);
  size_t nb = strlen(b);
  char* out = (char*)clv_arena_alloc(na + nb + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  if (na > 0) {
    memcpy(out, a, na);
  }
  if (nb > 0) {
    memcpy(out + na, b, nb);
  }
  out[na + nb] = '\0';
  return out;
}

static char* clv_format1_i64(const char* fmt, int64_t v) {
  int n = snprintf(NULL, 0, fmt, (long long)v);
  if (n < 0) {
    return clv_str_clone("");
  }
  char* out = (char*)clv_arena_alloc((size_t)n + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  snprintf(out, (size_t)n + 1, fmt, (long long)v);
  return out;
}

static char* clv_format1_bool(const char* fmt, bool v) {
  const char* s = v ? "true" : "false";
  int n = snprintf(NULL, 0, fmt, s);
  if (n < 0) {
    return clv_str_clone("");
  }
  char* out = (char*)clv_arena_alloc((size_t)n + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  snprintf(out, (size_t)n + 1, fmt, s);
  return out;
}

static char* clv_format1_str(const char* fmt, const char* v) {
  int n = snprintf(NULL, 0, fmt, v);
  if (n < 0) {
    return clv_str_clone("");
  }
  char* out = (char*)clv_arena_alloc((size_t)n + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  snprintf(out, (size_t)n + 1, fmt, v);
  return out;
}

static char* clv_keyword_from_str(const char* s) {
  if (s[0] == ':') {
    return clv_str_clone(s);
  }
  size_t len = strlen(s);
  char* out = (char*)clv_arena_alloc(len + 2);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  out[0] = ':';
  if (len > 0) {
    memcpy(out + 1, s, len);
  }
  out[len + 1] = '\0';
  return out;
}

static char* clv_symbol_from_str(const char* s) {
  if (s[0] == ':') {
    return clv_str_clone(s + 1);
  }
  return clv_str_clone(s);
}

static int64_t clv_gensym_counter = 0;

static char* clv_gensym(const char* prefix) {
  if (!prefix || prefix[0] == '\0') {
    prefix = "G__";
  }
  int64_t id = clv_gensym_counter++;
  int n = snprintf(NULL, 0, "%s%lld", prefix, (long long)id);
  if (n < 0) {
    return clv_str_clone("G__0");
  }
  char* out = (char*)clv_arena_alloc((size_t)n + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  snprintf(out, (size_t)n + 1, "%s%lld", prefix, (long long)id);
  return out;
}

/* `/` is not a namespace separator (see TASK/DONE/名前空間.md) and the reader
   rejects it, so the whole token after any leading ':' is the name. */
static char* clv_name_str(const char* s) {
  const char* base = s;
  if (*base == ':') {
    base++;
  }
  return clv_str_clone(base);
}

static int64_t clv_now_ns(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    return 0LL;
  }
  return ((int64_t)ts.tv_sec * 1000000000LL) + (int64_t)ts.tv_nsec;
}

static char* clv_slurp_file(const char* path) {
  FILE* fp = fopen(path, "rb");
  if (!fp) {
    fprintf(stderr, "phase2 C: slurp open failed: %s\n", path);
    abort();
  }
  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    fprintf(stderr, "phase2 C: slurp seek failed: %s\n", path);
    abort();
  }
  long sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    fprintf(stderr, "phase2 C: slurp tell failed: %s\n", path);
    abort();
  }
  rewind(fp);
  char* out = (char*)clv_arena_alloc((size_t)sz + 1);
  if (!out) {
    fclose(fp);
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  size_t nread = fread(out, 1, (size_t)sz, fp);
  fclose(fp);
  out[nread] = '\0';
  return out;
}

static char* clv_spit_file(const char* path, const char* data) {
  FILE* fp = fopen(path, "wb");
  if (!fp) {
    fprintf(stderr, "phase2 C: spit open failed: %s\n", path);
    abort();
  }
  size_t len = strlen(data);
  size_t nw = fwrite(data, 1, len, fp);
  fclose(fp);
  if (nw != len) {
    fprintf(stderr, "phase2 C: spit write failed: %s\n", path);
    abort();
  }
  return clv_str_clone(data);
}

static char* clv_re_find_str(const char* pattern, const char* s) {
  regex_t re;
  if (regcomp(&re, pattern, REG_EXTENDED) != 0) {
    return clv_str_clone("");
  }
  regmatch_t m[1];
  int rc = regexec(&re, s, 1, m, 0);
  regfree(&re);
  if (rc != 0 || m[0].rm_so < 0 || m[0].rm_eo < 0) {
    return clv_str_clone("");
  }
  size_t st = (size_t)m[0].rm_so;
  size_t ed = (size_t)m[0].rm_eo;
  return clv_str_clone_n(s + st, ed - st);
}

static char* clv_re_matches_str(const char* pattern, const char* s) {
  regex_t re;
  if (regcomp(&re, pattern, REG_EXTENDED) != 0) {
    return clv_str_clone("");
  }
  regmatch_t m[1];
  int rc = regexec(&re, s, 1, m, 0);
  regfree(&re);
  size_t len = strlen(s);
  if (rc == 0 && m[0].rm_so == 0 && (size_t)m[0].rm_eo == len) {
    return clv_str_clone(s);
  }
  return clv_str_clone("");
}

static clv_vec_str clv_vec_str_new(size_t cap);
static void clv_vec_str_push(clv_vec_str* v, char* x);

static clv_vec_str clv_re_seq_str(const char* pattern, const char* s) {
  clv_vec_str out = clv_vec_str_new(8);
  regex_t re;
  if (regcomp(&re, pattern, REG_EXTENDED) != 0) {
    return out;
  }
  const char* cur = s;
  while (*cur) {
    regmatch_t m[1];
    int rc = regexec(&re, cur, 1, m, 0);
    if (rc != 0 || m[0].rm_so < 0 || m[0].rm_eo < 0) {
      break;
    }
    size_t st = (size_t)m[0].rm_so;
    size_t ed = (size_t)m[0].rm_eo;
    clv_vec_str_push(&out, clv_str_clone_n(cur + st, ed - st));
    if (ed == 0) {
      cur += 1;
    } else {
      cur += ed;
    }
  }
  regfree(&re);
  return out;
}

static clv_vec_i64 clv_vec_new(size_t cap) {
  clv_vec_i64 out;
  out.len = 0;
  out.cap = cap > 0 ? cap : 4;
  out.data = (int64_t*)malloc(sizeof(int64_t) * out.cap);
  if (!out.data) {
    fprintf(stderr, "phase2 C: alloc failed\n");
    abort();
  }
  return out;
}

static void clv_vec_push(clv_vec_i64* v, int64_t x) {
  if (v->len == v->cap) {
    size_t next = v->cap * 2;
    int64_t* ptr = (int64_t*)realloc(v->data, sizeof(int64_t) * next);
    if (!ptr) {
      fprintf(stderr, "phase2 C: realloc failed\n");
      abort();
    }
    v->data = ptr;
    v->cap = next;
  }
  v->data[v->len++] = x;
}

static void clv_vec_free(clv_vec_i64* v) {
  if (v->data) {
    free(v->data);
  }
  v->data = NULL;
  v->len = 0;
  v->cap = 0;
}

static clv_vec_vec_i64 clv_vec_vec_i64_new(size_t cap) {
  clv_vec_vec_i64 out;
  out.len = 0;
  out.cap = cap > 0 ? cap : 4;
  out.data = (clv_vec_i64*)malloc(sizeof(clv_vec_i64) * out.cap);
  if (!out.data) {
    fprintf(stderr, "phase2 C: alloc failed\n");
    abort();
  }
  return out;
}

static void clv_vec_vec_i64_push(clv_vec_vec_i64* v, clv_vec_i64 x) {
  if (v->len == v->cap) {
    size_t next = v->cap * 2;
    clv_vec_i64* ptr = (clv_vec_i64*)realloc(v->data, sizeof(clv_vec_i64) * next);
    if (!ptr) {
      fprintf(stderr, "phase2 C: realloc failed\n");
      abort();
    }
    v->data = ptr;
    v->cap = next;
  }
  v->data[v->len++] = x;
}

static void clv_vec_vec_i64_free(clv_vec_vec_i64* v) {
  if (v->data) {
    for (size_t i = 0; i < v->len; ++i) {
      clv_vec_free(&v->data[i]);
    }
    free(v->data);
  }
  v->data = NULL;
  v->len = 0;
  v->cap = 0;
}

static void clv_vec_i64_fprint(FILE* fp, const clv_vec_i64* v) {
  fprintf(fp, "[");
  for (size_t i = 0; i < v->len; ++i) {
    if (i > 0) {
      fprintf(fp, " ");
    }
    fprintf(fp, "%lld", (long long)v->data[i]);
  }
  fprintf(fp, "]");
}

static void clv_vec_str_fprint(FILE* fp, const clv_vec_str* v) {
  fprintf(fp, "[");
  for (size_t i = 0; i < v->len; ++i) {
    if (i > 0) {
      fprintf(fp, " ");
    }
    fprintf(fp, "%s", v->data[i]);
  }
  fprintf(fp, "]");
}

static void clv_vec_vec_i64_fprint(FILE* fp, const clv_vec_vec_i64* v, bool newline) {
  fprintf(fp, "[");
  for (size_t i = 0; i < v->len; ++i) {
    if (i > 0) {
      fprintf(fp, " ");
    }
    clv_vec_i64_fprint(fp, &v->data[i]);
  }
  fprintf(fp, "]");
  if (newline) {
    fprintf(fp, "\n");
  }
}

static void clv_vec_vec_i64_println(const clv_vec_vec_i64* v) {
  clv_vec_vec_i64_fprint(stdout, v, true);
}

static clv_vec_i64 clv_flatten_vec_vec_i64(const clv_vec_vec_i64* src) {
  size_t total = 0;
  for (size_t i = 0; i < src->len; ++i) {
    total += src->data[i].len;
  }
  clv_vec_i64 out = clv_vec_new(total > 0 ? total : 1);
  for (size_t i = 0; i < src->len; ++i) {
    const clv_vec_i64* inner = &src->data[i];
    for (size_t j = 0; j < inner->len; ++j) {
      clv_vec_push(&out, inner->data[j]);
    }
  }
  return out;
}

static clv_vec_str clv_vec_str_new(size_t cap) {
  clv_vec_str out;
  out.len = 0;
  out.cap = cap > 0 ? cap : 4;
  out.data = (char**)malloc(sizeof(char*) * out.cap);
  if (!out.data) {
    fprintf(stderr, "phase2 C: alloc failed\n");
    abort();
  }
  return out;
}

static void clv_vec_str_push(clv_vec_str* v, char* x) {
  if (v->len == v->cap) {
    size_t next = v->cap * 2;
    char** ptr = (char**)realloc(v->data, sizeof(char*) * next);
    if (!ptr) {
      fprintf(stderr, "phase2 C: realloc failed\n");
      abort();
    }
    v->data = ptr;
    v->cap = next;
  }
  v->data[v->len++] = x;
}

static void clv_vec_str_free(clv_vec_str* v) {
  if (v->data) {
    /* string payloads are arena-managed; only free pointer array here */
    free(v->data);
  }
  v->data = NULL;
  v->len = 0;
  v->cap = 0;
}

static clv_vec_i64 clv_vec_new(size_t cap);

typedef struct {
  char* key;
  int64_t value;
} clv_map_entry_ki64;

typedef struct {
  clv_map_entry_ki64* data;
  size_t len;
  size_t cap;
} clv_map_ki64;

typedef struct {
  int64_t key;
  clv_vec_i64 value;
} clv_map_entry_i64_vec_i64;

typedef struct {
  clv_map_entry_i64_vec_i64* data;
  size_t len;
  size_t cap;
} clv_map_i64_vec_i64;

static clv_map_ki64 clv_map_ki64_new(size_t cap) {
  clv_map_ki64 out;
  out.len = 0;
  out.cap = cap > 0 ? cap : 4;
  out.data = (clv_map_entry_ki64*)malloc(sizeof(clv_map_entry_ki64) * out.cap);
  if (!out.data) {
    fprintf(stderr, "phase2 C: map alloc failed\n");
    abort();
  }
  return out;
}

static void clv_map_ki64_free(clv_map_ki64* m) {
  if (m->data) {
    free(m->data);
  }
  m->data = NULL;
  m->len = 0;
  m->cap = 0;
}

static ssize_t clv_map_ki64_find(const clv_map_ki64* m, const char* key) {
  for (size_t i = 0; i < m->len; ++i) {
    if (strcmp(m->data[i].key, key) == 0) {
      return (ssize_t)i;
    }
  }
  return -1;
}

static void clv_map_ki64_reserve(clv_map_ki64* m, size_t need) {
  if (m->cap >= need) {
    return;
  }
  size_t cap = m->cap > 0 ? m->cap : 4;
  while (cap < need) {
    cap *= 2;
  }
  clv_map_entry_ki64* ptr = (clv_map_entry_ki64*)realloc(m->data, sizeof(clv_map_entry_ki64) * cap);
  if (!ptr) {
    fprintf(stderr, "phase2 C: map realloc failed\n");
    abort();
  }
  m->data = ptr;
  m->cap = cap;
}

static void clv_map_ki64_put(clv_map_ki64* m, const char* key, int64_t value) {
  ssize_t idx = clv_map_ki64_find(m, key);
  if (idx >= 0) {
    m->data[(size_t)idx].value = value;
    return;
  }
  clv_map_ki64_reserve(m, m->len + 1);
  m->data[m->len].key = clv_str_clone(key);
  m->data[m->len].value = value;
  m->len += 1;
}

static size_t clv_json_escaped_len(const char* s) {
  size_t n = 0;
  for (; *s; ++s) {
    switch (*s) {
      case '\\':
      case '"':
      case '\n':
      case '\r':
      case '\t':
        n += 2;
        break;
      default:
        n += 1;
        break;
    }
  }
  return n;
}

static char* clv_json_escape_copy(char* out, const char* s) {
  for (; *s; ++s) {
    switch (*s) {
      case '\\': *out++ = '\\'; *out++ = '\\'; break;
      case '"': *out++ = '\\'; *out++ = '"'; break;
      case '\n': *out++ = '\\'; *out++ = 'n'; break;
      case '\r': *out++ = '\\'; *out++ = 'r'; break;
      case '\t': *out++ = '\\'; *out++ = 't'; break;
      default: *out++ = *s; break;
    }
  }
  return out;
}

static char* clv_json_write_ki64(const clv_map_ki64* m) {
  size_t total = 3;
  for (size_t i = 0; i < m->len; ++i) {
    int digits = snprintf(NULL, 0, "%lld", (long long)m->data[i].value);
    if (digits < 0) {
      fprintf(stderr, "phase2 C: snprintf failed\n");
      abort();
    }
    if (i > 0) {
      total += 1;
    }
    total += 3 + clv_json_escaped_len(m->data[i].key) + (size_t)digits;
  }
  char* out = (char*)clv_arena_alloc(total);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  char* p = out;
  *p++ = '{';
  for (size_t i = 0; i < m->len; ++i) {
    if (i > 0) {
      *p++ = ',';
    }
    *p++ = '"';
    p = clv_json_escape_copy(p, m->data[i].key);
    *p++ = '"';
    *p++ = ':';
    int n = snprintf(p, total - (size_t)(p - out), "%lld", (long long)m->data[i].value);
    if (n < 0) {
      fprintf(stderr, "phase2 C: snprintf failed\n");
      abort();
    }
    p += (size_t)n;
  }
  *p++ = '}';
  *p = '\0';
  return out;
}

static const char* clv_json_skip_ws(const char* s) {
  while (*s == ' ' || *s == '\n' || *s == '\r' || *s == '\t') {
    ++s;
  }
  return s;
}

static char* clv_json_parse_string(const char** sp) {
  const char* s = *sp;
  if (*s != '"') {
    fprintf(stderr, "phase2 C: json key must start with quote\n");
    abort();
  }
  ++s;
  char* out = (char*)clv_arena_alloc(strlen(s) + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  size_t w = 0;
  while (*s && *s != '"') {
    if (*s == '\\') {
      ++s;
      switch (*s) {
        case '"': out[w++] = '"'; break;
        case '\\': out[w++] = '\\'; break;
        case 'n': out[w++] = '\n'; break;
        case 'r': out[w++] = '\r'; break;
        case 't': out[w++] = '\t'; break;
        default:
          fprintf(stderr, "phase2 C: unsupported json escape\n");
          abort();
      }
      ++s;
      continue;
    }
    out[w++] = *s++;
  }
  if (*s != '"') {
    fprintf(stderr, "phase2 C: unterminated json string\n");
    abort();
  }
  out[w] = '\0';
  *sp = s + 1;
  return out;
}

static clv_map_ki64 clv_json_read_ki64(const char* src) {
  const char* s = clv_json_skip_ws(src);
  if (*s != '{') {
    fprintf(stderr, "phase2 C: json object must start with '{'\n");
    abort();
  }
  ++s;
  clv_map_ki64 out = clv_map_ki64_new(8);
  s = clv_json_skip_ws(s);
  if (*s == '}') {
    return out;
  }
  while (true) {
    s = clv_json_skip_ws(s);
    char* key = clv_json_parse_string(&s);
    s = clv_json_skip_ws(s);
    if (*s != ':') {
      fprintf(stderr, "phase2 C: expected ':' in json object\n");
      abort();
    }
    ++s;
    s = clv_json_skip_ws(s);
    char* end = NULL;
    long long value = strtoll(s, &end, 10);
    if (end == s) {
      fprintf(stderr, "phase2 C: expected integer json value\n");
      abort();
    }
    clv_map_ki64_put(&out, key, (int64_t)value);
    s = clv_json_skip_ws(end);
    if (*s == '}') {
      break;
    }
    if (*s != ',') {
      fprintf(stderr, "phase2 C: expected ',' in json object\n");
      abort();
    }
    ++s;
  }
  return out;
}

static bool clv_map_ki64_contains(const clv_map_ki64* m, const char* key) {
  return clv_map_ki64_find(m, key) >= 0;
}

static int64_t clv_map_ki64_get(const clv_map_ki64* m, const char* key, bool has_default, int64_t default_value) {
  ssize_t idx = clv_map_ki64_find(m, key);
  if (idx >= 0) {
    return m->data[(size_t)idx].value;
  }
  if (has_default) {
    return default_value;
  }
  fprintf(stderr, "phase2 C: key not found: %s\n", key);
  abort();
}

static clv_map_ki64 clv_map_copy_ki64(const clv_map_ki64* src) {
  clv_map_ki64 out = clv_map_ki64_new(src->len > 0 ? src->len : 1);
  for (size_t i = 0; i < src->len; ++i) {
    clv_map_ki64_put(&out, src->data[i].key, src->data[i].value);
  }
  return out;
}

static clv_map_ki64 clv_map_assoc_ki64(const clv_map_ki64* src, const char* key, int64_t value) {
  clv_map_ki64 out = clv_map_copy_ki64(src);
  clv_map_ki64_put(&out, key, value);
  return out;
}

static void clv_map_ki64_dissoc(clv_map_ki64* m, const char* key) {
  ssize_t idx = clv_map_ki64_find(m, key);
  if (idx < 0) {
    return;
  }
  size_t i = (size_t)idx;
  for (size_t j = i + 1; j < m->len; ++j) {
    m->data[j - 1] = m->data[j];
  }
  m->len -= 1;
}

static clv_map_ki64 clv_map_merge_ki64(const clv_map_ki64* a, const clv_map_ki64* b) {
  clv_map_ki64 out = clv_map_copy_ki64(a);
  for (size_t i = 0; i < b->len; ++i) {
    clv_map_ki64_put(&out, b->data[i].key, b->data[i].value);
  }
  return out;
}

static inline int64_t clv_merge_with_apply_i64(int op, int64_t lhs, int64_t rhs) {
  switch (op) {
    case 1: return lhs + rhs;
    case 2: return lhs > rhs ? lhs : rhs;
    case 3: return lhs < rhs ? lhs : rhs;
    default:
      fprintf(stderr, "phase2 C: unknown merge-with op %d\n", op);
      abort();
  }
}

static clv_map_ki64 clv_map_merge_with_ki64(int op, const clv_map_ki64* a, const clv_map_ki64* b) {
  clv_map_ki64 out = clv_map_copy_ki64(a);
  for (size_t i = 0; i < b->len; ++i) {
    const char* key = b->data[i].key;
    int64_t rhs = b->data[i].value;
    ssize_t idx = clv_map_ki64_find(&out, key);
    if (idx >= 0) {
      int64_t lhs = out.data[(size_t)idx].value;
      out.data[(size_t)idx].value = clv_merge_with_apply_i64(op, lhs, rhs);
    } else {
      clv_map_ki64_put(&out, key, rhs);
    }
  }
  return out;
}

static clv_map_ki64 clv_zipmap_ki64(const clv_vec_str* keys, const clv_vec_i64* vals) {
  size_t n = keys->len < vals->len ? keys->len : vals->len;
  clv_map_ki64 out = clv_map_ki64_new(n > 0 ? n : 1);
  for (size_t i = 0; i < n; ++i) {
    clv_map_ki64_put(&out, keys->data[i], vals->data[i]);
  }
  return out;
}

static clv_vec_str clv_map_keys_ki64(const clv_map_ki64* m) {
  clv_vec_str out = clv_vec_str_new(m->len > 0 ? m->len : 1);
  for (size_t i = 0; i < m->len; ++i) {
    clv_vec_str_push(&out, clv_str_clone(m->data[i].key));
  }
  return out;
}

static clv_vec_i64 clv_map_vals_ki64(const clv_map_ki64* m) {
  clv_vec_i64 out = clv_vec_new(m->len > 0 ? m->len : 1);
  for (size_t i = 0; i < m->len; ++i) {
    out.data[i] = m->data[i].value;
  }
  out.len = m->len;
  return out;
}

static clv_map_ki64 clv_map_select_keys_ki64(const clv_map_ki64* m, const clv_vec_str* keys) {
  clv_map_ki64 out = clv_map_ki64_new(keys->len > 0 ? keys->len : 1);
  for (size_t i = 0; i < keys->len; ++i) {
    const char* key = keys->data[i];
    ssize_t idx = clv_map_ki64_find(m, key);
    if (idx >= 0) {
      clv_map_ki64_put(&out, key, m->data[(size_t)idx].value);
    }
  }
  return out;
}

static clv_map_ki64 clv_frequencies_i64(const clv_vec_i64* src) {
  typedef struct {
    int64_t key;
    int64_t count;
    bool used;
  } clv_i64_freq_entry;
  size_t cap = 256;
  clv_i64_freq_entry* entries = (clv_i64_freq_entry*)calloc(cap, sizeof(clv_i64_freq_entry));
  if (!entries) {
    fprintf(stderr, "phase2 C: frequencies alloc failed\n");
    abort();
  }
  size_t len = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t key = src->data[i];
    if ((len + 1) * 10 >= cap * 7) {
      size_t next_cap = cap * 2;
      clv_i64_freq_entry* next = (clv_i64_freq_entry*)calloc(next_cap, sizeof(clv_i64_freq_entry));
      if (!next) {
        free(entries);
        fprintf(stderr, "phase2 C: frequencies realloc failed\n");
        abort();
      }
      for (size_t j = 0; j < cap; ++j) {
        if (!entries[j].used) {
          continue;
        }
        uint64_t h = (uint64_t)entries[j].key * 11400714819323198485ull;
        size_t idx = (size_t)h & (next_cap - 1);
        while (next[idx].used) {
          idx = (idx + 1) & (next_cap - 1);
        }
        next[idx] = entries[j];
      }
      free(entries);
      entries = next;
      cap = next_cap;
    }
    uint64_t h = (uint64_t)key * 11400714819323198485ull;
    size_t idx = (size_t)h & (cap - 1);
    while (entries[idx].used && entries[idx].key != key) {
      idx = (idx + 1) & (cap - 1);
    }
    if (entries[idx].used) {
      entries[idx].count += 1LL;
    } else {
      entries[idx].used = true;
      entries[idx].key = key;
      entries[idx].count = 1LL;
      ++len;
    }
  }
  clv_map_ki64 out = clv_map_ki64_new(len > 0 ? len : 1);
  for (size_t i = 0; i < cap; ++i) {
    if (!entries[i].used) {
      continue;
    }
    char* key = clv_i64_to_str(entries[i].key);
    clv_map_ki64_put(&out, key, entries[i].count);
  }
  free(entries);
  return out;
}

static clv_map_ki64 clv_frequencies_str(const clv_vec_str* src) {
  clv_map_ki64 out = clv_map_ki64_new(src->len > 0 ? src->len : 1);
  for (size_t i = 0; i < src->len; ++i) {
    const char* key = src->data[i];
    int64_t count = clv_map_ki64_get(&out, key, true, 0LL);
    clv_map_ki64_put(&out, key, count + 1LL);
  }
  return out;
}

static int64_t clv_reduce_kv_ki64(int op, int64_t init, const clv_map_ki64* m) {
  int64_t acc = init;
  switch (op) {
    case 1:
      for (size_t i = 0; i < m->len; ++i) {
        acc += m->data[i].value;
      }
      return acc;
    default:
      fprintf(stderr, "phase2 C: unknown reduce-kv op %d\n", op);
      abort();
  }
}

static void clv_map_ki64_fprint(FILE* fp, const clv_map_ki64* m, bool newline) {
  fprintf(fp, "{");
  for (size_t i = 0; i < m->len; ++i) {
    if (i > 0) {
      fprintf(fp, " ");
    }
    fprintf(fp, "%s %lld", m->data[i].key, (long long)m->data[i].value);
  }
  fprintf(fp, "}");
  if (newline) {
    fprintf(fp, "\n");
  }
}

static void clv_map_ki64_print(const clv_map_ki64* m, bool newline) {
  clv_map_ki64_fprint(stdout, m, newline);
}

static void clv_map_ki64_println(const clv_map_ki64* m) {
  clv_map_ki64_print(m, true);
}

static clv_map_i64_vec_i64 clv_map_i64_vec_i64_new(size_t cap) {
  clv_map_i64_vec_i64 out;
  out.len = 0;
  out.cap = cap > 0 ? cap : 4;
  out.data = (clv_map_entry_i64_vec_i64*)malloc(sizeof(clv_map_entry_i64_vec_i64) * out.cap);
  if (!out.data) {
    fprintf(stderr, "phase2 C: map alloc failed\n");
    abort();
  }
  return out;
}

static void clv_map_i64_vec_i64_free(clv_map_i64_vec_i64* m) {
  if (m->data) {
    for (size_t i = 0; i < m->len; ++i) {
      clv_vec_free(&m->data[i].value);
    }
    free(m->data);
  }
  m->data = NULL;
  m->len = 0;
  m->cap = 0;
}

static ssize_t clv_map_i64_vec_i64_find(const clv_map_i64_vec_i64* m, int64_t key) {
  for (size_t i = 0; i < m->len; ++i) {
    if (m->data[i].key == key) {
      return (ssize_t)i;
    }
  }
  return -1;
}

static void clv_map_i64_vec_i64_reserve(clv_map_i64_vec_i64* m, size_t need) {
  if (m->cap >= need) {
    return;
  }
  size_t cap = m->cap > 0 ? m->cap : 4;
  while (cap < need) {
    cap *= 2;
  }
  clv_map_entry_i64_vec_i64* ptr =
      (clv_map_entry_i64_vec_i64*)realloc(m->data, sizeof(clv_map_entry_i64_vec_i64) * cap);
  if (!ptr) {
    fprintf(stderr, "phase2 C: map realloc failed\n");
    abort();
  }
  m->data = ptr;
  m->cap = cap;
}

static clv_vec_i64* clv_map_i64_vec_i64_get_or_insert(clv_map_i64_vec_i64* m, int64_t key) {
  ssize_t idx = clv_map_i64_vec_i64_find(m, key);
  if (idx >= 0) {
    return &m->data[(size_t)idx].value;
  }
  clv_map_i64_vec_i64_reserve(m, m->len + 1);
  m->data[m->len].key = key;
  m->data[m->len].value = clv_vec_new(4);
  m->len += 1;
  return &m->data[m->len - 1].value;
}

static void clv_map_i64_vec_i64_fprint(FILE* fp, const clv_map_i64_vec_i64* m, bool newline) {
  fprintf(fp, "{");
  for (size_t i = 0; i < m->len; ++i) {
    if (i > 0) {
      fprintf(fp, " ");
    }
    fprintf(fp, "%lld ", (long long)m->data[i].key);
    clv_vec_i64_fprint(fp, &m->data[i].value);
  }
  fprintf(fp, "}");
  if (newline) {
    fprintf(fp, "\n");
  }
}

static void clv_map_i64_vec_i64_println(const clv_map_i64_vec_i64* m) {
  clv_map_i64_vec_i64_fprint(stdout, m, true);
}

static uint64_t clv_rng_state = 0x9E3779B97F4A7C15ULL;

static inline uint64_t clv_rng_next(void) {
  uint64_t x = clv_rng_state;
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  clv_rng_state = x;
  return x * 2685821657736338717ULL;
}

static inline int64_t clv_rand_int_i64(int64_t upper) {
  if (upper <= 0) {
    fprintf(stderr, "phase2 C: rand-int upper must be positive\n");
    abort();
  }
  return (int64_t)(clv_rng_next() % (uint64_t)upper);
}

static clv_vec_i64 clv_range_i64(int64_t start, int64_t end) {
  if (end <= start) {
    return clv_vec_new(1);
  }
  size_t cap = (size_t)(end - start);
  clv_vec_i64 out = clv_vec_new(cap);
  for (size_t i = 0; i < cap; ++i) {
    out.data[i] = start + (int64_t)i;
  }
  out.len = cap;
  return out;
}

static inline int64_t clv_apply_map_i64(int op, int64_t k, int64_t x) {
  switch (op) {
    case 0: return x;
    case 1: return x + 1;
    case 2: return x - 1;
    case 3: return x + k;
    case 4: return x - k;
    case 5: return x * k;
    case 6: return x > k ? x : k;
    case 7: return x < k ? x : k;
    case 8:
      return clv_mod_i64(x, k);
    case 9: return x < 0 ? -x : x;
    case 10:
      if (k == 0) {
        fprintf(stderr, "phase2 C: quot by zero\n");
        abort();
      }
      return x / k;
    case 11:
      return clv_rem_i64(x, k);
    case 12: return x < k ? -1LL : (x > k ? 1LL : 0LL);
    case 13: return x & k;
    case 14: return x | k;
    case 15: return x ^ k;
    case 16: return clv_wrapping_shl_i64(x, k);
    case 17: return clv_wrapping_shr_i64(x, k);
    case 18: return ~x;
    case 19:
      if (k <= 0) {
        fprintf(stderr, "phase2 C: rand-int upper must be positive\n");
        abort();
      }
      return (int64_t)(clv_rng_next() % (uint64_t)k);
    case 20:
      if (k <= 0) {
        return (int64_t)(clv_rng_next() & 0x7FFFFFFFFFFFFFFFULL);
      }
      return (int64_t)(clv_rng_next() % (uint64_t)k);
    case 21: return x < k ? 1LL : (x > k ? -1LL : 0LL);
    case 22: return x & (~k);
    case 23: return clv_bit_clear_i64(x, k);
    case 24: return clv_bit_flip_i64(x, k);
    case 25: return clv_bit_set_i64(x, k);
    case 26: return k;
    case 27: return x * x;
    case 28: return x + x;
    default:
      fprintf(stderr, "phase2 C: unknown map op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_map_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  for (size_t i = 0; i < src->len; ++i) {
    out.data[i] = clv_apply_map_i64(op, k, src->data[i]);
  }
  out.len = src->len;
  return out;
}

static clv_vec_i64 clv_map_comp_i64(
    const clv_vec_i64* src,
    int outer_op,
    int64_t outer_k,
    int inner_op,
    int64_t inner_k
) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  for (size_t i = 0; i < src->len; ++i) {
    int64_t t = clv_apply_map_i64(inner_op, inner_k, src->data[i]);
    out.data[i] = clv_apply_map_i64(outer_op, outer_k, t);
  }
  out.len = src->len;
  return out;
}

static inline int64_t clv_apply_map_indexed_i64(int op, int64_t k, int64_t i, int64_t x) {
  if (op >= 100) {
    return clv_apply_map_i64(op - 100, k, x);
  }
  switch (op) {
    case 6: return x + i;
    case 7: return x - i;
    case 8: return i;
    default:
      fprintf(stderr, "phase2 C: unknown map-indexed op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_map_indexed_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  for (size_t i = 0; i < src->len; ++i) {
    out.data[i] = clv_apply_map_indexed_i64(op, k, (int64_t)i, src->data[i]);
  }
  out.len = src->len;
  return out;
}

static clv_vec_i64 clv_iterate_i64(int op, int64_t k, int64_t seed, int64_t n) {
  if (n <= 0) {
    return clv_vec_new(1);
  }
  clv_vec_i64 out = clv_vec_new((size_t)n);
  int64_t cur = seed;
  for (int64_t i = 0; i < n; ++i) {
    out.data[(size_t)i] = cur;
    cur = clv_apply_map_i64(op, k, cur);
  }
  out.len = (size_t)n;
  return out;
}

static clv_vec_i64 clv_repeatedly_i64(int op, int64_t k, int64_t n) {
  if (n <= 0) {
    return clv_vec_new(1);
  }
  clv_vec_i64 out = clv_vec_new((size_t)n);
  for (int64_t i = 0; i < n; ++i) {
    out.data[(size_t)i] = clv_apply_map_i64(op, k, i);
  }
  out.len = (size_t)n;
  return out;
}

static inline bool clv_apply_pred_i64(int op, int64_t k, int64_t x) {
  if (op >= 100) {
    return !clv_apply_pred_i64(op - 100, k, x);
  }
  switch (op) {
    case 1: return (x % 2) == 0;
    case 2: return (x % 2) != 0;
    case 3: return x < k;
    case 4: return x <= k;
    case 5: return x > k;
    case 6: return x >= k;
    case 7: return x == k;
    case 8: return x == 0;
    case 9: return x > 0;
    case 10: return x < 0;
    case 11: return x != k;
    case 12: return true;
    case 13: return false;
    case 14: return clv_bit_test_i64(x, k);
    default:
      fprintf(stderr, "phase2 C: unknown pred op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_slice_i64(const clv_vec_i64* src, size_t st, size_t ed) {
  if (ed < st) {
    ed = st;
  }
  size_t n = ed - st;
  clv_vec_i64 out = clv_vec_new(n > 0 ? n : 1);
  if (n > 0) {
    memcpy(out.data, src->data + st, sizeof(int64_t) * n);
    out.len = n;
  }
  return out;
}

static clv_vec_vec_i64 clv_partition_i64(const clv_vec_i64* src, int64_t n, int64_t step, bool keep_partial) {
  if (n <= 0 || step <= 0) {
    fprintf(stderr, "phase2 C: partition n/step must be positive\n");
    abort();
  }
  clv_vec_vec_i64 out = clv_vec_vec_i64_new(8);
  size_t i = 0;
  while (i < src->len) {
    size_t ed = i + (size_t)n;
    if (ed > src->len) {
      if (keep_partial) {
        clv_vec_vec_i64_push(&out, clv_slice_i64(src, i, src->len));
      }
      break;
    }
    clv_vec_vec_i64_push(&out, clv_slice_i64(src, i, ed));
    i += (size_t)step;
  }
  return out;
}

static clv_vec_vec_i64 clv_partition_by_map_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_vec_i64 out = clv_vec_vec_i64_new(8);
  if (src->len == 0) {
    return out;
  }
  size_t st = 0;
  int64_t cur_key = clv_apply_map_i64(op, k, src->data[0]);
  for (size_t i = 1; i < src->len; ++i) {
    int64_t key = clv_apply_map_i64(op, k, src->data[i]);
    if (key != cur_key) {
      clv_vec_vec_i64_push(&out, clv_slice_i64(src, st, i));
      st = i;
      cur_key = key;
    }
  }
  clv_vec_vec_i64_push(&out, clv_slice_i64(src, st, src->len));
  return out;
}

static clv_vec_vec_i64 clv_partition_by_pred_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_vec_i64 out = clv_vec_vec_i64_new(8);
  if (src->len == 0) {
    return out;
  }
  size_t st = 0;
  bool cur_key = clv_apply_pred_i64(op, k, src->data[0]);
  for (size_t i = 1; i < src->len; ++i) {
    bool key = clv_apply_pred_i64(op, k, src->data[i]);
    if (key != cur_key) {
      clv_vec_vec_i64_push(&out, clv_slice_i64(src, st, i));
      st = i;
      cur_key = key;
    }
  }
  clv_vec_vec_i64_push(&out, clv_slice_i64(src, st, src->len));
  return out;
}

static clv_map_i64_vec_i64 clv_group_by_map_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_map_i64_vec_i64 out = clv_map_i64_vec_i64_new(8);
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    int64_t key = clv_apply_map_i64(op, k, x);
    clv_vec_i64* bucket = clv_map_i64_vec_i64_get_or_insert(&out, key);
    clv_vec_push(bucket, x);
  }
  return out;
}

static clv_map_i64_vec_i64 clv_group_by_pred_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_map_i64_vec_i64 out = clv_map_i64_vec_i64_new(4);
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    int64_t key = clv_apply_pred_i64(op, k, x) ? 1LL : 0LL;
    clv_vec_i64* bucket = clv_map_i64_vec_i64_get_or_insert(&out, key);
    clv_vec_push(bucket, x);
  }
  return out;
}

static clv_vec_i64 clv_filter_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  size_t write_idx = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    if (clv_apply_pred_i64(op, k, x)) {
      out.data[write_idx++] = x;
    }
  }
  out.len = write_idx;
  return out;
}

static clv_vec_i64 clv_keep_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  size_t write_idx = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    if (clv_apply_pred_i64(op, k, x)) {
      out.data[write_idx++] = x;
    }
  }
  out.len = write_idx;
  return out;
}

static inline bool clv_apply_pred_indexed_i64(int op, int64_t k, int64_t i, int64_t x) {
  if (op >= 200) {
    return !clv_apply_pred_i64(op - 200, k, x);
  }
  if (op >= 100) {
    return clv_apply_pred_i64(op - 100, k, x);
  }
  switch (op) {
    case 1: return i < k;
    case 2: return i <= k;
    case 3: return i > k;
    case 4: return i >= k;
    case 5: return i == k;
    case 6: return i != k;
    case 7: return (i % 2LL) == 0LL;
    case 8: return (i % 2LL) != 0LL;
    default:
      fprintf(stderr, "phase2 C: unknown keep-indexed op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_keep_indexed_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  size_t write_idx = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    if (clv_apply_pred_indexed_i64(op, k, (int64_t)i, x)) {
      out.data[write_idx++] = x;
    }
  }
  out.len = write_idx;
  return out;
}

static clv_vec_i64 clv_remove_i64(const clv_vec_i64* src, int op, int64_t k) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  size_t write_idx = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    if (!clv_apply_pred_i64(op, k, x)) {
      out.data[write_idx++] = x;
    }
  }
  out.len = write_idx;
  return out;
}

static bool clv_every_i64(const clv_vec_i64* src, int op, int64_t k) {
  for (size_t i = 0; i < src->len; ++i) {
    if (!clv_apply_pred_i64(op, k, src->data[i])) {
      return false;
    }
  }
  return true;
}

static bool clv_any_i64(const clv_vec_i64* src, int op, int64_t k) {
  for (size_t i = 0; i < src->len; ++i) {
    if (clv_apply_pred_i64(op, k, src->data[i])) {
      return true;
    }
  }
  return false;
}

static int64_t clv_dorun_i64(const clv_vec_i64* src) {
  volatile int64_t sink = 0;
  for (size_t i = 0; i < src->len; ++i) {
    sink ^= src->data[i];
  }
  (void)sink;
  return 0LL;
}

static int64_t clv_reduce_i64(const clv_vec_i64* src, int op, int64_t init) {
  int64_t acc = init;
  switch (op) {
    case 1:
      for (size_t i = 0; i < src->len; ++i) acc += src->data[i];
      return acc;
    case 2:
      for (size_t i = 0; i < src->len; ++i) if (src->data[i] > acc) acc = src->data[i];
      return acc;
    case 3:
      for (size_t i = 0; i < src->len; ++i) if (src->data[i] < acc) acc = src->data[i];
      return acc;
    default:
      fprintf(stderr, "phase2 C: unknown reduce op %d\n", op);
      abort();
  }
}

static int64_t clv_apply_builtin_i64(int op, const clv_vec_i64* src) {
  switch (op) {
    case 1: {
      int64_t acc = 0;
      for (size_t i = 0; i < src->len; ++i) acc += src->data[i];
      return acc;
    }
    case 2: {
      int64_t acc = 1;
      for (size_t i = 0; i < src->len; ++i) acc *= src->data[i];
      return acc;
    }
    case 3: {
      if (src->len == 0) return 0;
      int64_t acc = src->data[0];
      for (size_t i = 1; i < src->len; ++i) if (src->data[i] > acc) acc = src->data[i];
      return acc;
    }
    case 4: {
      if (src->len == 0) return 0;
      int64_t acc = src->data[0];
      for (size_t i = 1; i < src->len; ++i) if (src->data[i] < acc) acc = src->data[i];
      return acc;
    }
    case 5: {
      int64_t acc = -1LL;
      for (size_t i = 0; i < src->len; ++i) acc &= src->data[i];
      return acc;
    }
    case 6: {
      int64_t acc = 0LL;
      for (size_t i = 0; i < src->len; ++i) acc |= src->data[i];
      return acc;
    }
    case 7: {
      int64_t acc = 0LL;
      for (size_t i = 0; i < src->len; ++i) acc ^= src->data[i];
      return acc;
    }
    default:
      fprintf(stderr, "phase2 C: unknown apply builtin op %d\n", op);
      abort();
  }
}

static int64_t clv_nth_i64(const clv_vec_i64* src, int64_t idx, bool has_default, int64_t default_value) {
  if (idx < 0 || (size_t)idx >= src->len) {
    if (has_default) {
      return default_value;
    }
    fprintf(stderr, "phase2 C: nth index out of bounds: %lld (len=%zu)\n", (long long)idx, src->len);
    abort();
  }
  return src->data[(size_t)idx];
}

static int64_t clv_first_i64(const clv_vec_i64* src) {
  return clv_nth_i64(src, 0LL, false, 0LL);
}

static int64_t clv_second_i64(const clv_vec_i64* src) {
  return clv_nth_i64(src, 1LL, false, 0LL);
}

static int64_t clv_last_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    fprintf(stderr, "phase2 C: last on empty vector\n");
    abort();
  }
  return src->data[src->len - 1];
}

static int64_t clv_peek_i64(const clv_vec_i64* src) {
  return clv_last_i64(src);
}

static inline int64_t clv_apply_update_i64(int op, int64_t k, int64_t x) {
  switch (op) {
    case 0: return x;
    case 1: return x + 1;
    case 2: return x - 1;
    case 3: return x + k;
    case 4: return x - k;
    case 5: return x * k;
    default:
      fprintf(stderr, "phase2 C: unknown update op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_assoc_i64(const clv_vec_i64* src, int64_t idx, int64_t value) {
  if (idx < 0 || (size_t)idx > src->len) {
    fprintf(stderr, "phase2 C: assoc index out of bounds: %lld (len=%zu)\n", (long long)idx, src->len);
    abort();
  }
  size_t idx_u = (size_t)idx;
  size_t out_len = src->len + (idx_u == src->len ? 1 : 0);
  clv_vec_i64 out = clv_vec_new(out_len > 0 ? out_len : 1);
  if (src->len > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  }
  if (idx_u == src->len) {
    out.data[src->len] = value;
  } else {
    out.data[idx_u] = value;
  }
  out.len = out_len;
  return out;
}

typedef struct {
  int64_t key;
  int64_t value;
} clv_pair_i64;

static bool clv_sort_by_desc = false;

static int clv_qsort_cmp_sort_by_i64(const void* a, const void* b) {
  const clv_pair_i64* pa = (const clv_pair_i64*)a;
  const clv_pair_i64* pb = (const clv_pair_i64*)b;
  int cmp = 0;
  if (pa->key < pb->key) cmp = -1;
  else if (pa->key > pb->key) cmp = 1;
  else if (pa->value < pb->value) cmp = -1;
  else if (pa->value > pb->value) cmp = 1;
  if (clv_sort_by_desc) {
    return -cmp;
  }
  return cmp;
}

static clv_vec_i64 clv_sort_by_i64(const clv_vec_i64* src, int op, int64_t k, bool desc) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  if (src->len == 0) {
    return out;
  }
  clv_pair_i64* pairs = (clv_pair_i64*)malloc(sizeof(clv_pair_i64) * src->len);
  if (!pairs) {
    fprintf(stderr, "phase2 C: sort-by alloc failed\n");
    abort();
  }
  int64_t min_key = 0;
  int64_t max_key = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t value = src->data[i];
    pairs[i].value = value;
    int64_t key = clv_apply_map_i64(op, k, value);
    pairs[i].key = key;
    if (i == 0) {
      min_key = key;
      max_key = key;
    } else {
      if (key < min_key) min_key = key;
      if (key > max_key) max_key = key;
    }
  }

  if (max_key >= min_key) {
    uint64_t span_u64 = (uint64_t)(max_key - min_key) + 1ULL;
    if (span_u64 > 0ULL && span_u64 <= 65536ULL && span_u64 * 4ULL <= (uint64_t)src->len + 1024ULL) {
      size_t key_span = (size_t)span_u64;
      size_t* counts = (size_t*)calloc(key_span, sizeof(size_t));
      size_t* offsets = (size_t*)malloc(sizeof(size_t) * key_span);
      if (!counts || !offsets) {
        free(counts);
        free(offsets);
        free(pairs);
        fprintf(stderr, "phase2 C: sort-by bucket alloc failed\n");
        abort();
      }
      for (size_t i = 0; i < src->len; ++i) {
        size_t idx = (size_t)(pairs[i].key - min_key);
        counts[idx] += 1;
      }
      size_t acc = 0;
      if (!desc) {
        for (size_t i = 0; i < key_span; ++i) {
          offsets[i] = acc;
          acc += counts[i];
        }
      } else {
        for (size_t i = key_span; i-- > 0;) {
          offsets[i] = acc;
          acc += counts[i];
        }
      }
      for (size_t i = 0; i < src->len; ++i) {
        size_t idx = (size_t)(pairs[i].key - min_key);
        size_t pos = offsets[idx]++;
        out.data[pos] = pairs[i].value;
      }
      out.len = src->len;
      free(offsets);
      free(counts);
      free(pairs);
      return out;
    }
  }
  clv_sort_by_desc = desc;
  qsort(pairs, src->len, sizeof(clv_pair_i64), clv_qsort_cmp_sort_by_i64);
  out.len = src->len;
  for (size_t i = 0; i < src->len; ++i) {
    out.data[i] = pairs[i].value;
  }
  free(pairs);
  return out;
}

static int clv_qsort_cmp_i64(const void* a, const void* b) {
  const int64_t va = *((const int64_t*)a);
  const int64_t vb = *((const int64_t*)b);
  if (va < vb) return -1;
  if (va > vb) return 1;
  return 0;
}

static clv_vec_i64 clv_sort_i64(const clv_vec_i64* src) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  if (src->len == 0) {
    return out;
  }
  memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  out.len = src->len;
  qsort(out.data, out.len, sizeof(int64_t), clv_qsort_cmp_i64);
  return out;
}

static clv_vec_i64 clv_reverse_i64(const clv_vec_i64* src) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  if (src->len == 0) {
    return out;
  }
  out.len = src->len;
  for (size_t i = 0; i < src->len; ++i) {
    out.data[i] = src->data[src->len - 1 - i];
  }
  return out;
}

static clv_vec_i64 clv_take_i64(const clv_vec_i64* src, int64_t n) {
  if (n <= 0) {
    return clv_vec_new(1);
  }
  size_t limit = (size_t)n;
  if (limit > src->len) {
    limit = src->len;
  }
  clv_vec_i64 out = clv_vec_new(limit > 0 ? limit : 1);
  if (limit > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * limit);
    out.len = limit;
  }
  return out;
}

static clv_vec_i64 clv_take_while_i64(const clv_vec_i64* src, int op, int64_t k) {
  size_t end = 0;
  while (end < src->len) {
    if (!clv_apply_pred_i64(op, k, src->data[end])) {
      break;
    }
    end++;
  }
  clv_vec_i64 out = clv_vec_new(end > 0 ? end : 1);
  if (end > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * end);
    out.len = end;
  }
  return out;
}

static clv_vec_i64 clv_drop_while_i64(const clv_vec_i64* src, int op, int64_t k) {
  size_t start = 0;
  while (start < src->len) {
    if (!clv_apply_pred_i64(op, k, src->data[start])) {
      break;
    }
    start++;
  }
  size_t remain = src->len - start;
  clv_vec_i64 out = clv_vec_new(remain > 0 ? remain : 1);
  if (remain > 0) {
    memcpy(out.data, src->data + start, sizeof(int64_t) * remain);
    out.len = remain;
  }
  return out;
}

static clv_vec_i64 clv_drop_last_i64(const clv_vec_i64* src, int64_t n) {
  size_t drop_n = 0;
  if (n > 0) {
    drop_n = (size_t)n;
    if (drop_n > src->len) {
      drop_n = src->len;
    }
  }
  size_t out_len = src->len - drop_n;
  clv_vec_i64 out = clv_vec_new(out_len > 0 ? out_len : 1);
  if (out_len > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * out_len);
    out.len = out_len;
  }
  return out;
}

static clv_vec_i64 clv_take_last_i64(const clv_vec_i64* src, int64_t n) {
  if (n <= 0) {
    return clv_vec_new(1);
  }
  size_t take_n = (size_t)n;
  if (take_n > src->len) {
    take_n = src->len;
  }
  clv_vec_i64 out = clv_vec_new(take_n > 0 ? take_n : 1);
  if (take_n > 0) {
    size_t start = src->len - take_n;
    memcpy(out.data, src->data + start, sizeof(int64_t) * take_n);
    out.len = take_n;
  }
  return out;
}

static clv_vec_i64 clv_butlast_i64(const clv_vec_i64* src) {
  return clv_drop_last_i64(src, 1);
}

static clv_vec_i64 clv_pop_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    fprintf(stderr, "phase2 C: pop on empty vector\n");
    abort();
  }
  return clv_drop_last_i64(src, 1);
}

static clv_vec_i64 clv_empty_i64(const clv_vec_i64* _src) {
  return clv_vec_new(1);
}

static clv_vec_i64 clv_not_empty_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    return clv_vec_new(1);
  }
  clv_vec_i64 out = clv_vec_new(src->len);
  memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  out.len = src->len;
  return out;
}

static clv_vec_i64 clv_conj_many_i64(const clv_vec_i64* src, const int64_t* extra, size_t extra_len) {
  clv_vec_i64 out = clv_vec_new(src->len + extra_len);
  if (src->len > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  }
  if (extra_len > 0) {
    memcpy(out.data + src->len, extra, sizeof(int64_t) * extra_len);
  }
  out.len = src->len + extra_len;
  return out;
}

static inline clv_vec_i64 clv_conj_i64(const clv_vec_i64* src, int64_t x) {
  clv_vec_i64 out = clv_vec_new(src->len + 1);
  if (src->len > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  }
  out.data[src->len] = x;
  out.len = src->len + 1;
  return out;
}

static clv_vec_i64 clv_cons_i64(int64_t x, const clv_vec_i64* src) {
  clv_vec_i64 out = clv_vec_new(src->len + 1);
  out.data[0] = x;
  if (src->len > 0) {
    memcpy(out.data + 1, src->data, sizeof(int64_t) * src->len);
  }
  out.len = src->len + 1;
  return out;
}

static clv_vec_i64 clv_repeat_i64(int64_t n, int64_t x) {
  if (n <= 0) {
    return clv_vec_new(1);
  }
  size_t len = (size_t)n;
  clv_vec_i64 out = clv_vec_new(len);
  for (size_t i = 0; i < len; ++i) {
    out.data[i] = x;
  }
  out.len = len;
  return out;
}

static clv_vec_i64 clv_interpose_i64(const clv_vec_i64* src, int64_t sep) {
  if (src->len == 0) {
    return clv_vec_new(1);
  }
  size_t out_len = src->len * 2 - 1;
  clv_vec_i64 out = clv_vec_new(out_len);
  size_t w = 0;
  for (size_t i = 0; i < src->len; ++i) {
    out.data[w++] = src->data[i];
    if (i + 1 < src->len) {
      out.data[w++] = sep;
    }
  }
  out.len = out_len;
  return out;
}

static clv_vec_i64 clv_interleave_i64(const clv_vec_i64* a, const clv_vec_i64* b) {
  size_t min_len = a->len < b->len ? a->len : b->len;
  size_t out_len = min_len * 2;
  clv_vec_i64 out = clv_vec_new(out_len > 0 ? out_len : 1);
  size_t w = 0;
  for (size_t i = 0; i < min_len; ++i) {
    out.data[w++] = a->data[i];
    out.data[w++] = b->data[i];
  }
  out.len = out_len;
  return out;
}

static clv_vec_vec_i64 clv_zip_i64(const clv_vec_i64* a, const clv_vec_i64* b) {
  size_t min_len = a->len < b->len ? a->len : b->len;
  clv_vec_vec_i64 out = clv_vec_vec_i64_new(min_len > 0 ? min_len : 1);
  for (size_t i = 0; i < min_len; ++i) {
    clv_vec_i64 pair = clv_vec_new(2);
    clv_vec_push(&pair, a->data[i]);
    clv_vec_push(&pair, b->data[i]);
    clv_vec_vec_i64_push(&out, pair);
  }
  return out;
}

static inline int64_t clv_apply_zip_i64(int op, int64_t x, int64_t y) {
  switch (op) {
    case 1: return x + y;
    case 2: return x - y;
    case 3: return x * y;
    case 4: return x > y ? x : y;
    case 5: return x < y ? x : y;
    case 6: return x & y;
    case 7: return x | y;
    case 8: return x ^ y;
    default:
      fprintf(stderr, "phase2 C: unknown zip op %d\n", op);
      abort();
  }
}

static clv_vec_i64 clv_zip_with_i64(int op, const clv_vec_i64* a, const clv_vec_i64* b) {
  size_t min_len = a->len < b->len ? a->len : b->len;
  clv_vec_i64 out = clv_vec_new(min_len > 0 ? min_len : 1);
  for (size_t i = 0; i < min_len; ++i) {
    out.data[i] = clv_apply_zip_i64(op, a->data[i], b->data[i]);
  }
  out.len = min_len;
  return out;
}

static clv_vec_i64 clv_dedupe_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    return clv_vec_new(1);
  }
  clv_vec_i64 out = clv_vec_new(src->len);
  size_t w = 0;
  int64_t prev = src->data[0];
  out.data[w++] = prev;
  for (size_t i = 1; i < src->len; ++i) {
    int64_t x = src->data[i];
    if (x != prev) {
      out.data[w++] = x;
      prev = x;
    }
  }
  out.len = w;
  return out;
}

static clv_vec_i64 clv_distinct_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    return clv_vec_new(1);
  }
  clv_vec_i64 out = clv_vec_new(src->len);
  typedef struct {
    int64_t key;
    bool used;
  } clv_i64_set_entry;
  size_t cap = 256;
  clv_i64_set_entry* entries = (clv_i64_set_entry*)calloc(cap, sizeof(clv_i64_set_entry));
  if (!entries) {
    fprintf(stderr, "phase2 C: distinct alloc failed\n");
    abort();
  }
  size_t used = 0;
  size_t w = 0;
  for (size_t i = 0; i < src->len; ++i) {
    int64_t x = src->data[i];
    if ((used + 1) * 10 >= cap * 7) {
      size_t next_cap = cap * 2;
      clv_i64_set_entry* next =
          (clv_i64_set_entry*)calloc(next_cap, sizeof(clv_i64_set_entry));
      if (!next) {
        free(entries);
        fprintf(stderr, "phase2 C: distinct realloc failed\n");
        abort();
      }
      for (size_t j = 0; j < cap; ++j) {
        if (!entries[j].used) {
          continue;
        }
        uint64_t h = (uint64_t)entries[j].key * 11400714819323198485ull;
        size_t idx = (size_t)h & (next_cap - 1);
        while (next[idx].used) {
          idx = (idx + 1) & (next_cap - 1);
        }
        next[idx] = entries[j];
      }
      free(entries);
      entries = next;
      cap = next_cap;
    }
    uint64_t h = (uint64_t)x * 11400714819323198485ull;
    size_t idx = (size_t)h & (cap - 1);
    while (entries[idx].used && entries[idx].key != x) {
      idx = (idx + 1) & (cap - 1);
    }
    if (!entries[idx].used) {
      entries[idx].used = true;
      entries[idx].key = x;
      ++used;
      out.data[w++] = x;
    }
  }
  free(entries);
  out.len = w;
  return out;
}

static int64_t clv_rand_nth_i64(const clv_vec_i64* src) {
  if (src->len == 0) {
    fprintf(stderr, "phase2 C: rand-nth on empty vector\n");
    abort();
  }
  size_t idx = (size_t)(clv_rng_next() % src->len);
  return src->data[idx];
}

static clv_vec_i64 clv_shuffle_i64(const clv_vec_i64* src) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  if (src->len == 0) {
    return out;
  }
  memcpy(out.data, src->data, sizeof(int64_t) * src->len);
  out.len = src->len;
  for (size_t i = out.len - 1; i > 0; --i) {
    size_t j = (size_t)(clv_rng_next() % (i + 1));
    int64_t t = out.data[i];
    out.data[i] = out.data[j];
    out.data[j] = t;
  }
  return out;
}

static clv_vec_i64 clv_into_i64(const clv_vec_i64* dst, const clv_vec_i64* src) {
  size_t n = dst->len + src->len;
  clv_vec_i64 out = clv_vec_new(n > 0 ? n : 1);
  if (dst->len > 0) {
    memcpy(out.data, dst->data, sizeof(int64_t) * dst->len);
  }
  if (src->len > 0) {
    memcpy(out.data + dst->len, src->data, sizeof(int64_t) * src->len);
  }
  out.len = n;
  return out;
}

static clv_vec_i64 clv_vec_copy_i64(const clv_vec_i64* src) {
  clv_vec_i64 out = clv_vec_new(src->len > 0 ? src->len : 1);
  if (src->len > 0) {
    memcpy(out.data, src->data, sizeof(int64_t) * src->len);
    out.len = src->len;
  }
  return out;
}

static clv_vec_i64 clv_drop_i64(const clv_vec_i64* src, int64_t n) {
  size_t start = 0;
  if (n > 0) {
    start = (size_t)n;
    if (start > src->len) {
      start = src->len;
    }
  }
  size_t remain = src->len - start;
  clv_vec_i64 out = clv_vec_new(remain > 0 ? remain : 1);
  if (remain > 0) {
    memcpy(out.data, src->data + start, sizeof(int64_t) * remain);
    out.len = remain;
  }
  return out;
}

static clv_vec_i64 clv_subvec_i64(const clv_vec_i64* src, int64_t start, bool has_end, int64_t end) {
  if (start < 0 || (size_t)start > src->len) {
    fprintf(stderr, "phase2 C: subvec start out of bounds: %lld (len=%zu)\n", (long long)start, src->len);
    abort();
  }
  size_t s = (size_t)start;
  size_t e = src->len;
  if (has_end) {
    if (end < start || (size_t)end > src->len) {
      fprintf(stderr, "phase2 C: subvec end out of bounds: %lld (start=%lld, len=%zu)\n", (long long)end, (long long)start, src->len);
      abort();
    }
    e = (size_t)end;
  }
  size_t n = e - s;
  clv_vec_i64 out = clv_vec_new(n > 0 ? n : 1);
  if (n > 0) {
    memcpy(out.data, src->data + s, sizeof(int64_t) * n);
    out.len = n;
  }
  return out;
}

static clv_vec_i64 clv_concat_i64(const clv_vec_i64* a, const clv_vec_i64* b) {
  size_t n = a->len + b->len;
  clv_vec_i64 out = clv_vec_new(n > 0 ? n : 1);
  if (a->len > 0) {
    memcpy(out.data, a->data, sizeof(int64_t) * a->len);
  }
  if (b->len > 0) {
    memcpy(out.data + a->len, b->data, sizeof(int64_t) * b->len);
  }
  out.len = n;
  return out;
}

static clv_vec_str clv_split_str(const char* s, const char* sep) {
  size_t sep_len = strlen(sep);
  if (sep_len == 0) {
    fprintf(stderr, "phase2 C: split separator must not be empty\n");
    abort();
  }
  clv_vec_str out = clv_vec_str_new(8);
  const char* cur = s;
  while (1) {
    const char* pos = strstr(cur, sep);
    if (!pos) {
      clv_vec_str_push(&out, clv_str_clone(cur));
      break;
    }
    clv_vec_str_push(&out, clv_str_clone_n(cur, (size_t)(pos - cur)));
    cur = pos + sep_len;
  }
  return out;
}

static clv_vec_str clv_split_lines_str(const char* s) {
  clv_vec_str out = clv_vec_str_new(8);
  const char* cur = s;
  while (1) {
    const char* pos = strchr(cur, '\n');
    if (!pos) {
      size_t len = strlen(cur);
      if (len > 0 && cur[len - 1] == '\r') {
        --len;
      }
      clv_vec_str_push(&out, clv_str_clone_n(cur, len));
      break;
    }
    size_t len = (size_t)(pos - cur);
    if (len > 0 && cur[len - 1] == '\r') {
      --len;
    }
    clv_vec_str_push(&out, clv_str_clone_n(cur, len));
    cur = pos + 1;
  }
  return out;
}

static clv_vec_str clv_lines_str(const char* s) {
  clv_vec_str out = clv_vec_str_new(8);
  const char* cur = s;
  while (*cur != '\0') {
    const char* nl = strchr(cur, '\n');
    if (!nl) {
      clv_vec_str_push(&out, clv_str_clone(cur));
      break;
    }
    size_t len = (size_t)(nl - cur) + 1;
    clv_vec_str_push(&out, clv_str_clone_n(cur, len));
    cur = nl + 1;
  }
  return out;
}

static char* clv_reverse_str(const char* s) {
  const unsigned char* bytes = (const unsigned char*)s;
  size_t len = strlen(s);
  char* out = (char*)clv_arena_alloc(len + 1);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  size_t w = 0;
  size_t i = len;
  while (i > 0) {
    size_t start = i - 1;
    while (start > 0 && (bytes[start] & 0xC0) == 0x80) {
      start--;
    }
    size_t cp_len = i - start;
    memcpy(out + w, bytes + start, cp_len);
    w += cp_len;
    i = start;
  }
  out[w] = '\0';
  return out;
}

static char* clv_escape_runtime(const char* s) {
  size_t in_len = strlen(s);
  size_t out_cap = in_len * 2 + 1;
  char* out = (char*)clv_arena_alloc(out_cap);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  size_t w = 0;
  for (size_t i = 0; i < in_len; ++i) {
    unsigned char c = (unsigned char)s[i];
    switch (c) {
      case '\\':
        out[w++] = '\\';
        out[w++] = '\\';
        break;
      case '\"':
        out[w++] = '\\';
        out[w++] = '\"';
        break;
      case '\n':
        out[w++] = '\\';
        out[w++] = 'n';
        break;
      case '\r':
        out[w++] = '\\';
        out[w++] = 'r';
        break;
      case '\t':
        out[w++] = '\\';
        out[w++] = 't';
        break;
      default:
        out[w++] = (char)c;
        break;
    }
  }
  out[w] = '\0';
  return out;
}

static char* clv_join_str(const clv_vec_str* src, const char* sep) {
  if (src->len == 0) {
    return clv_str_clone("");
  }
  size_t sep_len = strlen(sep);
  size_t total = 1;
  for (size_t i = 0; i < src->len; ++i) {
    total += strlen(src->data[i]);
  }
  total += sep_len * (src->len - 1);
  char* out = (char*)clv_arena_alloc(total);
  if (!out) {
    fprintf(stderr, "phase2 C: arena alloc failed\n");
    abort();
  }
  char* p = out;
  for (size_t i = 0; i < src->len; ++i) {
    size_t len = strlen(src->data[i]);
    if (len > 0) {
      memcpy(p, src->data[i], len);
      p += len;
    }
    if (i + 1 < src->len && sep_len > 0) {
      memcpy(p, sep, sep_len);
      p += sep_len;
    }
  }
  *p = '\0';
  return out;
}

static char* clv_replace_first_str(const char* s, const char* from, const char* to) {
  size_t from_len = strlen(from);
  if (from_len == 0) {
    return clv_str_clone(s);
  }
  const char* pos = strstr(s, from);
  if (!pos) {
    return clv_str_clone(s);
  }
  size_t to_len = strlen(to);
  size_t prefix_len = (size_t)(pos - s);
  size_t s_len = strlen(s);
  size_t suffix_off = prefix_len + from_len;
  size_t suffix_len = s_len - suffix_off;
  size_t out_len = prefix_len + to_len + suffix_len;
  char* out = (char*)clv_arena_alloc(out_len + 1);
  if (prefix_len > 0) {
    memcpy(out, s, prefix_len);
  }
  if (to_len > 0) {
    memcpy(out + prefix_len, to, to_len);
  }
  if (suffix_len > 0) {
    memcpy(out + prefix_len + to_len, s + suffix_off, suffix_len);
  }
  out[out_len] = '\0';
  return out;
}

static char* clv_replace_str(const char* s, const char* from, const char* to) {
  size_t from_len = strlen(from);
  if (from_len == 0) {
    return clv_str_clone(s);
  }
  size_t to_len = strlen(to);
  size_t count = 0;
  const char* scan = s;
  while (1) {
    const char* pos = strstr(scan, from);
    if (!pos) {
      break;
    }
    ++count;
    scan = pos + from_len;
  }
  if (count == 0) {
    return clv_str_clone(s);
  }
  size_t s_len = strlen(s);
  size_t out_len = s_len + count * (to_len - from_len);
  char* out = (char*)clv_arena_alloc(out_len + 1);
  char* w = out;
  const char* cur = s;
  while (1) {
    const char* pos = strstr(cur, from);
    if (!pos) {
      size_t tail = strlen(cur);
      if (tail > 0) {
        memcpy(w, cur, tail);
        w += tail;
      }
      break;
    }
    size_t chunk = (size_t)(pos - cur);
    if (chunk > 0) {
      memcpy(w, cur, chunk);
      w += chunk;
    }
    if (to_len > 0) {
      memcpy(w, to, to_len);
      w += to_len;
    }
    cur = pos + from_len;
  }
  *w = '\0';
  return out;
}

static void clv_str_free(char* _s) {
  (void)_s;
}

static uint32_t clv_utf8_next(const char** cursor) {
  const unsigned char* p = (const unsigned char*)*cursor;
  uint32_t codepoint;
  size_t width;
  if (p[0] < 0x80) {
    codepoint = p[0];
    width = 1;
  } else if ((p[0] & 0xE0) == 0xC0 && p[1] != '\0' && (p[1] & 0xC0) == 0x80) {
    codepoint = ((uint32_t)(p[0] & 0x1F) << 6) | (uint32_t)(p[1] & 0x3F);
    width = 2;
    if (codepoint < 0x80) {
      goto invalid_utf8;
    }
  } else if ((p[0] & 0xF0) == 0xE0
      && p[1] != '\0'
      && p[2] != '\0'
      && (p[1] & 0xC0) == 0x80
      && (p[2] & 0xC0) == 0x80) {
    codepoint = ((uint32_t)(p[0] & 0x0F) << 12)
      | ((uint32_t)(p[1] & 0x3F) << 6)
      | (uint32_t)(p[2] & 0x3F);
    width = 3;
    if (codepoint < 0x800 || (codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
      goto invalid_utf8;
    }
  } else if ((p[0] & 0xF8) == 0xF0
      && p[1] != '\0'
      && p[2] != '\0'
      && p[3] != '\0'
      && (p[1] & 0xC0) == 0x80
      && (p[2] & 0xC0) == 0x80
      && (p[3] & 0xC0) == 0x80) {
    codepoint = ((uint32_t)(p[0] & 0x07) << 18)
      | ((uint32_t)(p[1] & 0x3F) << 12)
      | ((uint32_t)(p[2] & 0x3F) << 6)
      | (uint32_t)(p[3] & 0x3F);
    width = 4;
    if (codepoint < 0x10000 || codepoint > 0x10FFFF) {
      goto invalid_utf8;
    }
  } else {
    goto invalid_utf8;
  }
  *cursor += width;
  return codepoint;

invalid_utf8:
  fprintf(stderr, "phase2 C: invalid UTF-8 string\n");
  abort();
}

static bool clv_unicode_whitespace(uint32_t codepoint) {
  return (codepoint >= 0x0009 && codepoint <= 0x000D)
    || codepoint == 0x0020
    || codepoint == 0x0085
    || codepoint == 0x00A0
    || codepoint == 0x1680
    || (codepoint >= 0x2000 && codepoint <= 0x200A)
    || codepoint == 0x2028
    || codepoint == 0x2029
    || codepoint == 0x202F
    || codepoint == 0x205F
    || codepoint == 0x3000;
}

static bool clv_parse_bool_str(const char* value) {
  const char* start = value;
  while (*start != '\0') {
    const char* next = start;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      break;
    }
    start = next;
  }
  const char* cursor = start;
  const char* content_end = start;
  while (*cursor != '\0') {
    const char* next = cursor;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      content_end = next;
    }
    cursor = next;
  }
  size_t len = (size_t)(content_end - start);
  if (len == 4 && memcmp(start, "true", 4) == 0) {
    return true;
  }
  if (len == 5 && memcmp(start, "false", 5) == 0) {
    return false;
  }
  fprintf(stderr, "phase2 C: bool expects \"true\" or \"false\"\n");
  abort();
}

static size_t clv_utf8_byte_offset(const char* s, int64_t index, const char* label) {
  if (index < 0) {
    fprintf(stderr, "phase2 C: %s out of bounds: %lld\n", label, (long long)index);
    abort();
  }
  const char* cursor = s;
  for (int64_t i = 0; i < index; ++i) {
    if (*cursor == '\0') {
      fprintf(stderr, "phase2 C: %s out of bounds: %lld\n", label, (long long)index);
      abort();
    }
    clv_utf8_next(&cursor);
  }
  return (size_t)(cursor - s);
}

static void clv_require_ascii(const char* s, const char* operation) {
  for (const unsigned char* p = (const unsigned char*)s; *p; ++p) {
    if (*p >= 0x80) {
      fprintf(stderr, "phase2 C: %s currently supports ASCII strings only\n", operation);
      abort();
    }
  }
}

static char* clv_subs_str(const char* s, int64_t start, bool has_end, int64_t end) {
  size_t st = clv_utf8_byte_offset(s, start, "subs start");
  size_t ed = strlen(s);
  if (has_end) {
    if (end < start) {
      fprintf(stderr, "phase2 C: subs end out of bounds: %lld (start=%lld)\n", (long long)end, (long long)start);
      abort();
    }
    ed = clv_utf8_byte_offset(s, end, "subs end");
  }
  return clv_str_clone_n(s + st, ed - st);
}

static char* clv_upper_case_str(const char* s) {
  clv_require_ascii(s, "upper-case");
  size_t len = strlen(s);
  char* out = clv_str_clone_n(s, len);
  for (size_t i = 0; i < len; ++i) {
    out[i] = (char)toupper((unsigned char)out[i]);
  }
  return out;
}

static char* clv_lower_case_str(const char* s) {
  clv_require_ascii(s, "lower-case");
  size_t len = strlen(s);
  char* out = clv_str_clone_n(s, len);
  for (size_t i = 0; i < len; ++i) {
    out[i] = (char)tolower((unsigned char)out[i]);
  }
  return out;
}

static char* clv_capitalize_str(const char* s) {
  clv_require_ascii(s, "capitalize");
  size_t len = strlen(s);
  if (len == 0) {
    return clv_str_clone("");
  }
  char* out = clv_str_clone_n(s, len);
  out[0] = (char)toupper((unsigned char)out[0]);
  for (size_t i = 1; i < len; ++i) {
    out[i] = (char)tolower((unsigned char)out[i]);
  }
  return out;
}

static char* clv_triml_str(const char* s) {
  const char* cursor = s;
  while (*cursor != '\0') {
    const char* next = cursor;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      break;
    }
    cursor = next;
  }
  return clv_str_clone(cursor);
}

static char* clv_trimr_str(const char* s) {
  const char* cursor = s;
  const char* keep_end = s;
  while (*cursor != '\0') {
    const char* next = cursor;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      keep_end = next;
    }
    cursor = next;
  }
  return clv_str_clone_n(s, (size_t)(keep_end - s));
}

static char* clv_trim_str(const char* s) {
  const char* start = s;
  while (*start != '\0') {
    const char* next = start;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      break;
    }
    start = next;
  }
  const char* cursor = start;
  const char* keep_end = start;
  while (*cursor != '\0') {
    const char* next = cursor;
    uint32_t codepoint = clv_utf8_next(&next);
    if (!clv_unicode_whitespace(codepoint)) {
      keep_end = next;
    }
    cursor = next;
  }
  return clv_str_clone_n(start, (size_t)(keep_end - start));
}

static char* clv_trim_newline_str(const char* s) {
  size_t len = strlen(s);
  while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r')) {
    --len;
  }
  return clv_str_clone_n(s, len);
}

static bool clv_blank_str(const char* s) {
  const char* cursor = s;
  while (*cursor != '\0') {
    if (!clv_unicode_whitespace(clv_utf8_next(&cursor))) {
      return false;
    }
  }
  return true;
}

static bool clv_starts_with_str(const char* s, const char* prefix) {
  size_t plen = strlen(prefix);
  if (plen == 0) {
    return true;
  }
  size_t slen = strlen(s);
  return slen >= plen && memcmp(s, prefix, plen) == 0;
}

static bool clv_ends_with_str(const char* s, const char* suffix) {
  size_t ulen = strlen(suffix);
  if (ulen == 0) {
    return true;
  }
  size_t slen = strlen(s);
  return slen >= ulen && memcmp(s + (slen - ulen), suffix, ulen) == 0;
}

static bool clv_includes_str(const char* s, const char* needle) {
  return strstr(s, needle) != NULL;
}

/* 先頭 bytes バイトに含まれる文字数。index-of の戻り値は subs と同じ文字単位。 */
static int64_t clv_utf8_char_count_n(const char* s, size_t bytes) {
  int64_t count = 0;
  const char* cursor = s;
  const char* limit = s + bytes;
  while (cursor < limit && *cursor != '\0') {
    clv_utf8_next(&cursor);
    ++count;
  }
  return count;
}

static int64_t clv_index_of_str(const char* s, const char* needle) {
  if (needle[0] == '\0') {
    return 0;
  }
  const char* pos = strstr(s, needle);
  if (!pos) {
    return -1;
  }
  return clv_utf8_char_count_n(s, (size_t)(pos - s));
}

static int64_t clv_last_index_of_str(const char* s, const char* needle) {
  size_t needle_len = strlen(needle);
  size_t s_len = strlen(s);
  if (needle_len == 0) {
    return clv_utf8_char_count_n(s, s_len);
  }
  if (needle_len > s_len) {
    return -1;
  }
  for (size_t i = s_len - needle_len + 1; i-- > 0;) {
    if (memcmp(s + i, needle, needle_len) == 0) {
      return clv_utf8_char_count_n(s, i);
    }
  }
  return -1;
}

"#
    .to_string()
}

#[cfg(test)]
mod tests {
    use clove_build_runtime_c::RuntimeConfig;
    use std::fs;
    use std::path::PathBuf;
    use std::process::Command;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use crate::{emit_c, Expr, FrontProgram, TopLevel};

    static TEST_ID: AtomicUsize = AtomicUsize::new(0);

    fn compile_and_run(program: &FrontProgram) -> String {
        let artifact = emit_c(program, &RuntimeConfig::default()).expect("emit should succeed");
        let id = TEST_ID.fetch_add(1, Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "clove-build-backend-c-{}-{}",
            std::process::id(),
            id
        ));
        fs::create_dir_all(&root).expect("create temp directory");
        let source: PathBuf = root.join("test.c");
        let binary = root.join("test-bin");
        fs::write(&source, artifact.source).expect("write generated C");
        let compile = Command::new("cc")
            .args(["-O1"])
            .arg(&source)
            .arg("-o")
            .arg(&binary)
            .output()
            .expect("run C compiler");
        assert!(
            compile.status.success(),
            "C compilation failed: {}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let output = Command::new(&binary)
            .output()
            .expect("run generated binary");
        assert!(
            output.status.success(),
            "generated binary failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        fs::remove_dir_all(&root).expect("remove temp directory");
        String::from_utf8(output.stdout).expect("generated output must be UTF-8")
    }

    #[test]
    fn generated_strings_remain_valid_after_arena_growth() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "before".to_string(),
                    value: Expr::Str("sentinel".to_string()),
                },
                TopLevel::Def {
                    name: "large".to_string(),
                    value: Expr::Str("x".repeat(5000)),
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("before".to_string())],
                }),
            ],
        };
        assert_eq!(compile_and_run(&program), "sentinel\n");
    }

    #[test]
    fn index_of_returns_char_index_like_subs() {
        // subs / count は文字単位。index-of が byte offset を返すと
        // (subs s (index-of s x)) が壊れる。
        fn print(expr: Expr) -> TopLevel {
            TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![expr],
            })
        }
        fn call(callee: &str, args: Vec<Expr>) -> Expr {
            Expr::Call {
                callee: callee.to_string(),
                args,
            }
        }
        let text = || Expr::Str("aéb".to_string());
        let program = FrontProgram {
            top_levels: vec![
                print(call("index-of", vec![text(), Expr::Str("b".to_string())])),
                print(call(
                    "last-index-of",
                    vec![text(), Expr::Str("b".to_string())],
                )),
                print(call(
                    "subs",
                    vec![
                        text(),
                        call("index-of", vec![text(), Expr::Str("b".to_string())]),
                    ],
                )),
                print(call("index-of", vec![text(), Expr::Str("z".to_string())])),
            ],
        };
        assert_eq!(compile_and_run(&program), "2\n2\nb\n-1\n");
    }

    #[test]
    fn identity_does_not_transfer_collection_ownership() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Vector(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]),
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Call {
                                callee: "identity".to_string(),
                                args: vec![Expr::Symbol("xs".to_string())],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    }],
                }),
            ],
        };
        assert_eq!(compile_and_run(&program), "3\n");
    }

    #[test]
    fn and_or_preserve_short_circuit_side_effects_and_cleanup_scope() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "and".to_string(),
                        args: vec![
                            Expr::Bool(false),
                            Expr::Do(vec![
                                Expr::Call {
                                    callee: "println".to_string(),
                                    args: vec![Expr::Bool(true)],
                                },
                                Expr::Bool(true),
                            ]),
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "and".to_string(),
                        args: vec![
                            Expr::Bool(true),
                            Expr::Call {
                                callee: "=".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "upper-case".to_string(),
                                        args: vec![Expr::Str("a".to_string())],
                                    },
                                    Expr::Str("A".to_string()),
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        assert_eq!(compile_and_run(&program), "false\ntrue\n");
    }

    #[test]
    fn time_and_bench_keep_measured_work_inside_the_timed_region() {
        let measured = Expr::Call {
            callee: "reduce".to_string(),
            args: vec![
                Expr::Symbol("+".to_string()),
                Expr::Int(0),
                Expr::Call {
                    callee: "range".to_string(),
                    args: vec![Expr::Int(0), Expr::Int(100)],
                },
            ],
        };
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "timed".to_string(),
                    value: Expr::Call {
                        callee: "time".to_string(),
                        args: vec![measured.clone()],
                    },
                },
                TopLevel::Def {
                    name: "benched".to_string(),
                    value: Expr::Call {
                        callee: "bench".to_string(),
                        args: vec![Expr::Int(2), measured],
                    },
                },
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        let main = c
            .source
            .split("int main(void) {")
            .nth(1)
            .expect("main body");
        let time_start = main.find("time_start").expect("time start");
        let timed_range = main.find("clv_range_i64").expect("timed range");
        assert!(time_start < timed_range);
        let loop_start = main.find("for (int64_t bench_i").expect("bench loop");
        let benched_range = main.rfind("clv_range_i64").expect("benched range");
        assert!(loop_start < benched_range);
    }

    #[test]
    fn bench_cleans_temporary_resources_inside_the_loop() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Call {
                    callee: "get".to_string(),
                    args: vec![
                        Expr::Call {
                            callee: "bench".to_string(),
                            args: vec![
                                Expr::Int(2),
                                Expr::Call {
                                    callee: "index-of".to_string(),
                                    args: vec![
                                        Expr::Call {
                                            callee: "upper-case".to_string(),
                                            args: vec![Expr::Str("a".to_string())],
                                        },
                                        Expr::Str("A".to_string()),
                                    ],
                                },
                            ],
                        },
                        Expr::Keyword("result".to_string()),
                    ],
                }],
            })],
        };
        assert_eq!(compile_and_run(&program), "0\n");
    }

    #[test]
    fn rand_is_rejected_until_float_semantics_are_supported() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "rand".to_string(),
                args: vec![Expr::Int(1)],
            })],
        };
        let err = emit_c(&program, &RuntimeConfig::default())
            .expect_err("integer rand lowering silently changes Float semantics");
        assert!(err
            .to_string()
            .contains("rand is not supported in the phase2 C subset"));
    }

    #[test]
    fn distinct_clove_names_do_not_collide_as_c_identifiers() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "foo-bar".to_string(),
                    value: Expr::Int(1),
                },
                TopLevel::Def {
                    name: "foo_bar".to_string(),
                    value: Expr::Int(2),
                },
                TopLevel::Def {
                    name: "foo_2d_bar".to_string(),
                    value: Expr::Int(3),
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("foo-bar".to_string())],
                }),
            ],
        };
        assert_eq!(compile_and_run(&program), "1\n");
    }

    #[test]
    fn emit_reduce_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "total".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Symbol("inc".to_string()),
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("total".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_range_i64"));
        assert!(c.source.contains("clv_map_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("clv_vec_free(&range_"));
        assert!(c.source.contains("clv_vec_free(&map_"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_keep_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "total".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "keep".to_string(),
                                args: vec![
                                    Expr::Symbol("even?".to_string()),
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("total".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_keep_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_sort_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "sort".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("ys".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_sort_i64"));
        assert!(c.source.contains("qsort("));
        assert!(c.source.contains("memcpy("));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_reverse_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "reverse".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_reverse_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_take_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "take".to_string(),
                        args: vec![Expr::Int(4), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_take_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_take_while_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "take-while".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "<".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(4)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_take_while_i64"));
        assert!(c.source.contains(", 3, 4LL);"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_drop_while_remove_every_not_any_not_every_empty_q_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(-5), Expr::Int(6)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "drop-while".to_string(),
                        args: vec![
                            Expr::Symbol("neg?".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "zs".to_string(),
                    value: Expr::Call {
                        callee: "remove".to_string(),
                        args: vec![
                            Expr::Symbol("odd?".to_string()),
                            Expr::Symbol("ys".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "every?".to_string(),
                        args: vec![
                            Expr::Symbol("even?".to_string()),
                            Expr::Symbol("zs".to_string()),
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "not-any?".to_string(),
                        args: vec![
                            Expr::Symbol("neg?".to_string()),
                            Expr::Symbol("zs".to_string()),
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "not-every?".to_string(),
                        args: vec![
                            Expr::Symbol("even?".to_string()),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "empty?".to_string(),
                        args: vec![Expr::Call {
                            callee: "drop".to_string(),
                            args: vec![Expr::Int(100), Expr::Symbol("xs".to_string())],
                        }],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_drop_while_i64"));
        assert!(c.source.contains("clv_remove_i64"));
        assert!(c.source.contains("clv_every_i64"));
        assert!(c.source.contains("clv_any_i64"));
        assert!(c.source.contains(".len == 0"));
        assert!(c.source.contains("printf(\"%s\\n\""));
    }

    #[test]
    fn emit_drop_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "drop".to_string(),
                        args: vec![Expr::Int(4), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_drop_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_vec_ops_batch_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "pop".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "butlast".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "c".to_string(),
                    value: Expr::Call {
                        callee: "take-last".to_string(),
                        args: vec![Expr::Int(3), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "drop-last".to_string(),
                        args: vec![Expr::Int(3), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d2".to_string(),
                    value: Expr::Call {
                        callee: "rest".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d3".to_string(),
                    value: Expr::Call {
                        callee: "next".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "e".to_string(),
                    value: Expr::Call {
                        callee: "cons".to_string(),
                        args: vec![Expr::Int(-1), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "e2".to_string(),
                    value: Expr::Call {
                        callee: "conj".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Int(100),
                            Expr::Int(101),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "f".to_string(),
                    value: Expr::Call {
                        callee: "repeat".to_string(),
                        args: vec![Expr::Int(3), Expr::Int(7)],
                    },
                },
                TopLevel::Def {
                    name: "g".to_string(),
                    value: Expr::Call {
                        callee: "interpose".to_string(),
                        args: vec![Expr::Int(0), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "h".to_string(),
                    value: Expr::Call {
                        callee: "into".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Symbol("a".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "i".to_string(),
                    value: Expr::Call {
                        callee: "vec".to_string(),
                        args: vec![Expr::Symbol("h".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("b".to_string())],
                            },
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("i".to_string())],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_pop_i64"));
        assert!(c.source.contains("clv_butlast_i64"));
        assert!(c.source.contains("clv_take_last_i64"));
        assert!(c.source.contains("clv_drop_last_i64"));
        assert!(c.source.contains("clv_cons_i64"));
        assert!(c.source.contains("clv_conj_many_i64"));
        assert!(c.source.contains("clv_repeat_i64"));
        assert!(c.source.contains("clv_interpose_i64"));
        assert!(c.source.contains("clv_into_i64"));
        assert!(c.source.contains("clv_vec_copy_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_vector_first_second_last_peek_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "vector".to_string(),
                        args: vec![Expr::Int(10), Expr::Int(20), Expr::Int(30), Expr::Int(40)],
                    },
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "first".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "second".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "c".to_string(),
                    value: Expr::Call {
                        callee: "last".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "peek".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Symbol("a".to_string()),
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Symbol("b".to_string()),
                                    Expr::Call {
                                        callee: "+".to_string(),
                                        args: vec![
                                            Expr::Symbol("c".to_string()),
                                            Expr::Symbol("d".to_string()),
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_first_i64"));
        assert!(c.source.contains("clv_second_i64"));
        assert!(c.source.contains("clv_last_i64"));
        assert!(c.source.contains("clv_peek_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_nth_get_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "v1".to_string(),
                    value: Expr::Call {
                        callee: "nth".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(0), Expr::Int(8)],
                            },
                            Expr::Int(3),
                            Expr::Int(0),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "v2".to_string(),
                    value: Expr::Call {
                        callee: "get".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(0), Expr::Int(8)],
                            },
                            Expr::Int(100),
                            Expr::Int(7),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Symbol("v1".to_string()),
                            Expr::Symbol("v2".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_nth_i64"));
        assert!(c.source.contains("clv_vec_free(&range_"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_assoc_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(8)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "assoc".to_string(),
                        args: vec![Expr::Symbol("xs".to_string()), Expr::Int(3), Expr::Int(99)],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "nth".to_string(),
                        args: vec![Expr::Symbol("ys".to_string()), Expr::Int(3), Expr::Int(0)],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_assoc_i64"));
        assert!(c.source.contains("clv_nth_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_get_in_update_in_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(8)],
                    },
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "get-in".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Vector(vec![Expr::Int(3)]),
                            Expr::Int(0),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "update-in".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Vector(vec![Expr::Int(3)]),
                            Expr::Symbol("inc".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Symbol("a".to_string()),
                            Expr::Call {
                                callee: "nth".to_string(),
                                args: vec![
                                    Expr::Symbol("ys".to_string()),
                                    Expr::Int(3),
                                    Expr::Int(0),
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_apply_update_i64"));
        assert!(c.source.contains("clv_assoc_i64"));
        assert!(c.source.contains("clv_nth_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_split_join_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "parts".to_string(),
                    value: Expr::Call {
                        callee: "split".to_string(),
                        args: vec![Expr::Str("a,b,c".to_string()), Expr::Str(",".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "s".to_string(),
                    value: Expr::Call {
                        callee: "join".to_string(),
                        args: vec![
                            Expr::Str("-".to_string()),
                            Expr::Symbol("parts".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("s".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_split_str"));
        assert!(c.source.contains("clv_join_str"));
        assert!(c.source.contains("clv_vec_str_free(&"));
        assert!(c.source.contains("free(join_"));
        assert!(c.source.contains("printf(\"%s\\n\""));
    }

    #[test]
    fn emit_string_ops_batch_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "subs".to_string(),
                        args: vec![Expr::Str("clove".to_string()), Expr::Int(2)],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "upper-case".to_string(),
                        args: vec![Expr::Symbol("a".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "c".to_string(),
                    value: Expr::Call {
                        callee: "lower-case".to_string(),
                        args: vec![Expr::Symbol("b".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "capitalize".to_string(),
                        args: vec![Expr::Symbol("c".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "e".to_string(),
                    value: Expr::Call {
                        callee: "trim".to_string(),
                        args: vec![Expr::Str("  x  ".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "f".to_string(),
                    value: Expr::Call {
                        callee: "trim-newline".to_string(),
                        args: vec![Expr::Str("x\n".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("d".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_subs_str"));
        assert!(c.source.contains("clv_upper_case_str"));
        assert!(c.source.contains("clv_lower_case_str"));
        assert!(c.source.contains("clv_capitalize_str"));
        assert!(c.source.contains("clv_trim_str"));
        assert!(c.source.contains("clv_trim_newline_str"));
    }

    #[test]
    fn emit_string_predicates_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "s".to_string(),
                    value: Expr::Str("  clove bench  ".to_string()),
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "blank?".to_string(),
                        args: vec![Expr::Str("   ".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "starts-with?".to_string(),
                        args: vec![Expr::Symbol("s".to_string()), Expr::Str("  cl".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "c".to_string(),
                    value: Expr::Call {
                        callee: "ends-with?".to_string(),
                        args: vec![Expr::Symbol("s".to_string()), Expr::Str("  ".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "includes?".to_string(),
                        args: vec![
                            Expr::Symbol("s".to_string()),
                            Expr::Str("bench".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("d".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_blank_str"));
        assert!(c.source.contains("clv_starts_with_str"));
        assert!(c.source.contains("clv_ends_with_str"));
        assert!(c.source.contains("clv_includes_str"));
        assert!(c.source.contains("\"true\" : \"false\""));
    }

    #[test]
    fn emit_string_search_replace_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "s".to_string(),
                    value: Expr::Str("a,b,c,b".to_string()),
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "replace".to_string(),
                        args: vec![
                            Expr::Symbol("s".to_string()),
                            Expr::Str("b".to_string()),
                            Expr::Str("x".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "replace-first".to_string(),
                        args: vec![
                            Expr::Symbol("s".to_string()),
                            Expr::Str("b".to_string()),
                            Expr::Str("x".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "lines".to_string(),
                    value: Expr::Call {
                        callee: "split-lines".to_string(),
                        args: vec![Expr::Str("a\nb\r\nc".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "index-of".to_string(),
                                args: vec![
                                    Expr::Symbol("a".to_string()),
                                    Expr::Str("x".to_string()),
                                ],
                            },
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "last-index-of".to_string(),
                                        args: vec![
                                            Expr::Symbol("b".to_string()),
                                            Expr::Str("x".to_string()),
                                        ],
                                    },
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("lines".to_string())],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_replace_str"));
        assert!(c.source.contains("clv_replace_first_str"));
        assert!(c.source.contains("clv_split_lines_str"));
        assert!(c.source.contains("clv_index_of_str"));
        assert!(c.source.contains("clv_last_index_of_str"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_rem_compare_contains_lines_reverse_str_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "rem".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(3)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "zs".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "compare".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(5)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ok".to_string(),
                    value: Expr::Call {
                        callee: "contains?".to_string(),
                        args: vec![Expr::Symbol("xs".to_string()), Expr::Int(3)],
                    },
                },
                TopLevel::Def {
                    name: "lns".to_string(),
                    value: Expr::Call {
                        callee: "lines".to_string(),
                        args: vec![Expr::Str("a\nb\n".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "rev".to_string(),
                    value: Expr::Call {
                        callee: "reverse-str".to_string(),
                        args: vec![Expr::Str("abc".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "reduce".to_string(),
                                args: vec![
                                    Expr::Symbol("+".to_string()),
                                    Expr::Int(0),
                                    Expr::Symbol("ys".to_string()),
                                ],
                            },
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "reduce".to_string(),
                                        args: vec![
                                            Expr::Symbol("+".to_string()),
                                            Expr::Int(0),
                                            Expr::Symbol("zs".to_string()),
                                        ],
                                    },
                                    Expr::Call {
                                        callee: "+".to_string(),
                                        args: vec![
                                            Expr::Call {
                                                callee: "count".to_string(),
                                                args: vec![Expr::Symbol("lns".to_string())],
                                            },
                                            Expr::Call {
                                                callee: "count".to_string(),
                                                args: vec![Expr::Call {
                                                    callee: "split".to_string(),
                                                    args: vec![
                                                        Expr::Symbol("rev".to_string()),
                                                        Expr::Str("b".to_string()),
                                                    ],
                                                }],
                                            },
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("ok".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 11, 3LL);"));
        assert!(c.source.contains(", 12, 5LL);"));
        assert!(c.source.contains(".len);"));
        assert!(c.source.contains("clv_lines_str"));
        assert!(c.source.contains("clv_reverse_str"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
        assert!(c.source.contains("printf(\"%s\\n\""));
    }

    #[test]
    fn emit_core_batch_bit_rand_seq_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(100)],
                    },
                },
                TopLevel::Def {
                    name: "id".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Symbol("identity".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "bit-and".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(63)],
                                }),
                            },
                            Expr::Symbol("id".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Symbol("bit-not".to_string()),
                            Expr::Symbol("a".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "c".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "bit-shift-right".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(1)],
                                }),
                            },
                            Expr::Symbol("b".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "interleave".to_string(),
                        args: vec![Expr::Symbol("a".to_string()), Expr::Symbol("c".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "e".to_string(),
                    value: Expr::Call {
                        callee: "dedupe".to_string(),
                        args: vec![Expr::Symbol("d".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "f".to_string(),
                    value: Expr::Call {
                        callee: "distinct".to_string(),
                        args: vec![Expr::Symbol("e".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "g".to_string(),
                    value: Expr::Call {
                        callee: "shuffle".to_string(),
                        args: vec![Expr::Symbol("f".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "r".to_string(),
                    value: Expr::Call {
                        callee: "rand-int".to_string(),
                        args: vec![Expr::Int(100)],
                    },
                },
                TopLevel::Def {
                    name: "rn".to_string(),
                    value: Expr::Call {
                        callee: "rand-nth".to_string(),
                        args: vec![Expr::Symbol("g".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "n1".to_string(),
                    value: Expr::Call {
                        callee: "not".to_string(),
                        args: vec![Expr::Bool(true)],
                    },
                },
                TopLevel::Def {
                    name: "b1".to_string(),
                    value: Expr::Call {
                        callee: "bool".to_string(),
                        args: vec![Expr::Symbol("n1".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "i1".to_string(),
                    value: Expr::Call {
                        callee: "int".to_string(),
                        args: vec![Expr::Symbol("b1".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "reduce".to_string(),
                                args: vec![
                                    Expr::Symbol("+".to_string()),
                                    Expr::Int(0),
                                    Expr::Symbol("g".to_string()),
                                ],
                            },
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Symbol("r".to_string()),
                                    Expr::Call {
                                        callee: "+".to_string(),
                                        args: vec![
                                            Expr::Symbol("rn".to_string()),
                                            Expr::Symbol("i1".to_string()),
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 13, 63LL);"));
        assert!(c.source.contains(", 18, 0LL);"));
        assert!(c.source.contains(", 17, 1LL);"));
        assert!(c.source.contains("clv_interleave_i64"));
        assert!(c.source.contains("clv_dedupe_i64"));
        assert!(c.source.contains("clv_distinct_i64"));
        assert!(c.source.contains("clv_shuffle_i64"));
        assert!(c.source.contains("clv_rand_int_i64"));
        assert!(c.source.contains("clv_rand_nth_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_max_min_calls_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "max".to_string(),
                        args: vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "min".to_string(),
                        args: vec![Expr::Int(9), Expr::Int(3), Expr::Int(7)],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![Expr::Symbol("a".to_string()), Expr::Symbol("b".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("int64_t max_"));
        assert!(c.source.contains("int64_t min_"));
        assert!(c.source.contains("> (max_"));
        assert!(c.source.contains("< (min_"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_map_lambda_max_min_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Lambda {
                                        params: vec!["i".to_string()],
                                        body: Box::new(Expr::Call {
                                            callee: "max".to_string(),
                                            args: vec![Expr::Symbol("i".to_string()), Expr::Int(7)],
                                        }),
                                    },
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Lambda {
                                        params: vec!["i".to_string()],
                                        body: Box::new(Expr::Call {
                                            callee: "min".to_string(),
                                            args: vec![Expr::Symbol("i".to_string()), Expr::Int(7)],
                                        }),
                                    },
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![Expr::Symbol("a".to_string()), Expr::Symbol("b".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 6, 7LL);"));
        assert!(c.source.contains(", 7, 7LL);"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_abs_map_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Symbol("abs".to_string()),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Lambda {
                                        params: vec!["x".to_string()],
                                        body: Box::new(Expr::Call {
                                            callee: "-".to_string(),
                                            args: vec![Expr::Symbol("x".to_string()), Expr::Int(5)],
                                        }),
                                    },
                                    Expr::Symbol("xs".to_string()),
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 9, 0LL);"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_mod_map_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "total".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Lambda {
                                        params: vec!["i".to_string()],
                                        body: Box::new(Expr::Call {
                                            callee: "mod".to_string(),
                                            args: vec![
                                                Expr::Symbol("i".to_string()),
                                                Expr::Int(97),
                                            ],
                                        }),
                                    },
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("total".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_map_i64"));
        assert!(c.source.contains(", 8, 97LL);"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_quot_map_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "total".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "map".to_string(),
                                args: vec![
                                    Expr::Lambda {
                                        params: vec!["i".to_string()],
                                        body: Box::new(Expr::Call {
                                            callee: "quot".to_string(),
                                            args: vec![Expr::Symbol("i".to_string()), Expr::Int(3)],
                                        }),
                                    },
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("total".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_map_i64"));
        assert!(c.source.contains(", 10, 3LL);"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_subvec_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "subvec".to_string(),
                        args: vec![Expr::Symbol("xs".to_string()), Expr::Int(2), Expr::Int(6)],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_subvec_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_concat_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(5)],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(5), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "concat".to_string(),
                        args: vec![Expr::Symbol("a".to_string()), Expr::Symbol("b".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Symbol("ys".to_string()),
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_concat_i64"));
        assert!(c.source.contains("clv_reduce_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_filter_odd_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "total".to_string(),
                    value: Expr::Call {
                        callee: "reduce".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Int(0),
                            Expr::Call {
                                callee: "filter".to_string(),
                                args: vec![
                                    Expr::Symbol("odd?".to_string()),
                                    Expr::Call {
                                        callee: "range".to_string(),
                                        args: vec![Expr::Int(0), Expr::Int(10)],
                                    },
                                ],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("total".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_filter_i64"));
        assert!(c.source.contains(", 2, 0LL);"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_filter_zero_pos_neg_pipeline_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "z".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Symbol("zero?".to_string()),
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(-4), Expr::Int(5)],
                            },
                        ],
                    },
                },
                TopLevel::Def {
                    name: "p".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Symbol("pos?".to_string()),
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(-4), Expr::Int(5)],
                            },
                        ],
                    },
                },
                TopLevel::Def {
                    name: "n".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Symbol("neg?".to_string()),
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(-4), Expr::Int(5)],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("z".to_string())],
                            },
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("p".to_string())],
                                    },
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("n".to_string())],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 8, 0LL);"));
        assert!(c.source.contains(", 9, 0LL);"));
        assert!(c.source.contains(", 10, 0LL);"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_filter_not_eq_lambda_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "ys".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "not=".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(0)],
                                }),
                            },
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(0), Expr::Int(10)],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("ys".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 11, 0LL);"));
        assert!(c.source.contains("clv_filter_i64"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_not_eq_direct_call_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Call {
                    callee: "not=".to_string(),
                    args: vec![Expr::Int(1), Expr::Int(1), Expr::Int(2)],
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("not_equal_arg"));
        assert!(c.source.contains("printf(\"%s\\n\""));
    }

    #[test]
    fn emit_filter_type_predicates_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Symbol("number?".to_string()),
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(0), Expr::Int(10)],
                            },
                        ],
                    },
                },
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Symbol("string?".to_string()),
                            Expr::Call {
                                callee: "range".to_string(),
                                args: vec![Expr::Int(0), Expr::Int(10)],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("a".to_string())],
                            },
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("b".to_string())],
                            },
                        ],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 12, 0LL);"));
        assert!(c.source.contains(", 13, 0LL);"));
        assert!(c.source.contains("clv_filter_i64"));
    }

    #[test]
    fn emit_fn_predicate_direct_call_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "id".to_string(),
                    value: Expr::Lambda {
                        params: vec!["x".to_string()],
                        body: Box::new(Expr::Symbol("x".to_string())),
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "fn?".to_string(),
                        args: vec![Expr::Symbol("id".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("printf(\"%s\\n\""));
        assert!(c.source.contains("? \"true\" : \"false\""));
    }

    #[test]
    fn emit_batch_compare_seq_apply_associn_update_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(10)],
                    },
                },
                TopLevel::Def {
                    name: "lt".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "<".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(5)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ge".to_string(),
                    value: Expr::Call {
                        callee: "filter".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: ">=".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(5)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ok".to_string(),
                    value: Expr::Call {
                        callee: "some".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: ">".to_string(),
                                    args: vec![Expr::Symbol("x".to_string()), Expr::Int(7)],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ne".to_string(),
                    value: Expr::Call {
                        callee: "not-empty".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "sq".to_string(),
                    value: Expr::Call {
                        callee: "seq".to_string(),
                        args: vec![Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "ai".to_string(),
                    value: Expr::Call {
                        callee: "assoc-in".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Vector(vec![Expr::Int(2)]),
                            Expr::Int(99),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "up".to_string(),
                    value: Expr::Call {
                        callee: "update".to_string(),
                        args: vec![
                            Expr::Symbol("xs".to_string()),
                            Expr::Int(3),
                            Expr::Symbol("inc".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ap".to_string(),
                    value: Expr::Call {
                        callee: "apply".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Symbol("ap".to_string()),
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("ai".to_string())],
                                    },
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("up".to_string())],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "+".to_string(),
                        args: vec![
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("lt".to_string())],
                            },
                            Expr::Call {
                                callee: "+".to_string(),
                                args: vec![
                                    Expr::Call {
                                        callee: "count".to_string(),
                                        args: vec![Expr::Symbol("ge".to_string())],
                                    },
                                    Expr::Call {
                                        callee: "+".to_string(),
                                        args: vec![
                                            Expr::Call {
                                                callee: "count".to_string(),
                                                args: vec![Expr::Symbol("ne".to_string())],
                                            },
                                            Expr::Call {
                                                callee: "count".to_string(),
                                                args: vec![Expr::Symbol("sq".to_string())],
                                            },
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("ok".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_any_i64"));
        assert!(c.source.contains("clv_not_empty_i64"));
        assert!(c.source.contains("clv_apply_builtin_i64"));
        assert!(c.source.contains("clv_assoc_i64"));
        assert!(c.source.contains("clv_apply_update_i64"));
    }

    #[test]
    fn emit_batch_str_print_list_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "list".to_string(),
                        args: vec![Expr::Int(1), Expr::Int(2), Expr::Int(3), Expr::Int(4)],
                    },
                },
                TopLevel::Def {
                    name: "s".to_string(),
                    value: Expr::Call {
                        callee: "str".to_string(),
                        args: vec![Expr::Call {
                            callee: "count".to_string(),
                            args: vec![Expr::Symbol("xs".to_string())],
                        }],
                    },
                },
                TopLevel::Def {
                    name: "p".to_string(),
                    value: Expr::Call {
                        callee: "pr-str".to_string(),
                        args: vec![Expr::Str("ok".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "print".to_string(),
                    args: vec![Expr::Symbol("s".to_string())],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "prn".to_string(),
                    args: vec![Expr::Symbol("p".to_string())],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("s".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_i64_to_str"));
        assert!(c.source.contains("clv_pr_str_str"));
        assert!(c.source.contains("printf(\"%s\""));
        assert!(c.source.contains("printf(\"%s\""));
        assert!(c.source.contains("printf(\"\\n\")"));
    }

    #[test]
    fn emit_and_or_batch_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "a".to_string(),
                    value: Expr::Call {
                        callee: "and".to_string(),
                        args: vec![Expr::Bool(true), Expr::Bool(false), Expr::Bool(true)],
                    },
                },
                TopLevel::Def {
                    name: "o".to_string(),
                    value: Expr::Call {
                        callee: "or".to_string(),
                        args: vec![Expr::Bool(false), Expr::Bool(false), Expr::Bool(true)],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("a".to_string())],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("o".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("&&"));
        assert!(c.source.contains("||"));
        assert!(c.source.contains("printf(\"%s\\n\","));
    }

    #[test]
    fn emit_misc_clojure_basic_batch_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "k".to_string(),
                    value: Expr::Call {
                        callee: "keyword".to_string(),
                        args: vec![Expr::Str("user/name".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "s".to_string(),
                    value: Expr::Call {
                        callee: "symbol".to_string(),
                        args: vec![Expr::Str("user/name".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "n".to_string(),
                    value: Expr::Call {
                        callee: "name".to_string(),
                        args: vec![Expr::Symbol("k".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "fmti".to_string(),
                    value: Expr::Call {
                        callee: "format".to_string(),
                        args: vec![Expr::Str("%lld".to_string()), Expr::Int(42)],
                    },
                },
                TopLevel::Def {
                    name: "fmts".to_string(),
                    value: Expr::Call {
                        callee: "format".to_string(),
                        args: vec![Expr::Str("%s".to_string()), Expr::Symbol("n".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "fmtb".to_string(),
                    value: Expr::Call {
                        callee: "format".to_string(),
                        args: vec![Expr::Str("%s".to_string()), Expr::Bool(true)],
                    },
                },
                TopLevel::Def {
                    name: "m1".to_string(),
                    value: Expr::Call {
                        callee: "re-find".to_string(),
                        args: vec![
                            Expr::Str("[0-9]+".to_string()),
                            Expr::Str("ab12cd".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "m2".to_string(),
                    value: Expr::Call {
                        callee: "re-matches".to_string(),
                        args: vec![
                            Expr::Str("ab[0-9]+cd".to_string()),
                            Expr::Str("ab12cd".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ms".to_string(),
                    value: Expr::Call {
                        callee: "re-seq".to_string(),
                        args: vec![
                            Expr::Str("[0-9]+".to_string()),
                            Expr::Str("x1y22z333".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "okf".to_string(),
                    value: Expr::Call {
                        callee: "float?".to_string(),
                        args: vec![Expr::Int(1)],
                    },
                },
                TopLevel::Def {
                    name: "okinst".to_string(),
                    value: Expr::Call {
                        callee: "instance?".to_string(),
                        args: vec![Expr::Symbol("Int".to_string()), Expr::Int(1)],
                    },
                },
                TopLevel::Def {
                    name: "tmp".to_string(),
                    value: Expr::Call {
                        callee: "spit".to_string(),
                        args: vec![
                            Expr::Str("/tmp/clove_build_misc_test.txt".to_string()),
                            Expr::Str("hello".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "tmp2".to_string(),
                    value: Expr::Call {
                        callee: "slurp".to_string(),
                        args: vec![Expr::Str("/tmp/clove_build_misc_test.txt".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "p".to_string(),
                    args: vec![Expr::Symbol("fmts".to_string())],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "err".to_string(),
                    args: vec![Expr::Symbol("n".to_string())],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("ms".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_keyword_from_str"));
        assert!(c.source.contains("clv_symbol_from_str"));
        assert!(c.source.contains("clv_name_str"));
        assert!(c.source.contains("clv_format1_i64"));
        assert!(c.source.contains("clv_format1_str"));
        assert!(c.source.contains("clv_format1_bool"));
        assert!(c.source.contains("clv_re_find_str"));
        assert!(c.source.contains("clv_re_matches_str"));
        assert!(c.source.contains("clv_re_seq_str"));
        assert!(c.source.contains("clv_slurp_file"));
        assert!(c.source.contains("clv_spit_file"));
        assert!(c.source.contains("fprintf(stderr,"));
        assert!(c.source.contains("printf(\"%s\\n\","));
    }

    #[test]
    fn emit_misc_core_batch2_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "b".to_string(),
                    value: Expr::Call {
                        callee: "boolean".to_string(),
                        args: vec![Expr::Bool(false)],
                    },
                },
                TopLevel::Def {
                    name: "l".to_string(),
                    value: Expr::Call {
                        callee: "long".to_string(),
                        args: vec![Expr::Bool(true)],
                    },
                },
                TopLevel::Def {
                    name: "ban".to_string(),
                    value: Expr::Call {
                        callee: "bit-and-not".to_string(),
                        args: vec![Expr::Int(7), Expr::Int(3)],
                    },
                },
                TopLevel::Def {
                    name: "bclr".to_string(),
                    value: Expr::Call {
                        callee: "bit-clear".to_string(),
                        args: vec![Expr::Int(15), Expr::Int(1)],
                    },
                },
                TopLevel::Def {
                    name: "bfl".to_string(),
                    value: Expr::Call {
                        callee: "bit-flip".to_string(),
                        args: vec![Expr::Int(2), Expr::Int(1)],
                    },
                },
                TopLevel::Def {
                    name: "bst".to_string(),
                    value: Expr::Call {
                        callee: "bit-set".to_string(),
                        args: vec![Expr::Int(2), Expr::Int(3)],
                    },
                },
                TopLevel::Def {
                    name: "bt".to_string(),
                    value: Expr::Call {
                        callee: "bit-test".to_string(),
                        args: vec![Expr::Int(8), Expr::Int(3)],
                    },
                },
                TopLevel::Def {
                    name: "cd".to_string(),
                    value: Expr::Call {
                        callee: "compare-desc".to_string(),
                        args: vec![Expr::Int(1), Expr::Int(2)],
                    },
                },
                TopLevel::Def {
                    name: "rg".to_string(),
                    value: Expr::Call {
                        callee: "regex".to_string(),
                        args: vec![Expr::Str("[0-9]+".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "rp".to_string(),
                    value: Expr::Call {
                        callee: "re-pattern".to_string(),
                        args: vec![Expr::Str("[a-z]+".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "rm".to_string(),
                    value: Expr::Call {
                        callee: "re-matcher".to_string(),
                        args: vec![Expr::Symbol("rg".to_string()), Expr::Str("x1".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "es".to_string(),
                    value: Expr::Call {
                        callee: "escape".to_string(),
                        args: vec![Expr::Str("a\nb".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "rv".to_string(),
                    value: Expr::Call {
                        callee: "rseq".to_string(),
                        args: vec![Expr::Vector(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)])],
                    },
                },
                TopLevel::Def {
                    name: "pps".to_string(),
                    value: Expr::Call {
                        callee: "pp-str".to_string(),
                        args: vec![Expr::Str("ok".to_string())],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "pp".to_string(),
                    args: vec![Expr::Symbol("pps".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("& ~("));
        assert!(c.source.contains("clv_bit_mask_i64"));
        assert!(c.source.contains("clv_str_clone("));
        assert!(c.source.contains("clv_escape_runtime"));
        assert!(c.source.contains("clv_reverse_i64"));
        assert!(c.source.contains("printf(\"%s\""));
        assert!(c.source.contains("printf(\"\\n\")"));
    }

    #[test]
    fn emit_misc_core_batch3_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(16)],
                    },
                },
                TopLevel::Def {
                    name: "idxs".to_string(),
                    value: Expr::Call {
                        callee: "map-indexed".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["i".to_string(), "x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "+".to_string(),
                                    args: vec![
                                        Expr::Symbol("i".to_string()),
                                        Expr::Symbol("x".to_string()),
                                    ],
                                }),
                            },
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "kept".to_string(),
                    value: Expr::Call {
                        callee: "keep-indexed".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["i".to_string(), "x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "<".to_string(),
                                    args: vec![Expr::Symbol("i".to_string()), Expr::Int(8)],
                                }),
                            },
                            Expr::Symbol("idxs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "iter".to_string(),
                    value: Expr::Call {
                        callee: "iterate".to_string(),
                        args: vec![Expr::Symbol("inc".to_string()), Expr::Int(0), Expr::Int(8)],
                    },
                },
                TopLevel::Def {
                    name: "reps".to_string(),
                    value: Expr::Call {
                        callee: "repeatedly".to_string(),
                        args: vec![Expr::Int(8), Expr::Symbol("inc".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "zipped".to_string(),
                    value: Expr::Call {
                        callee: "zip-with".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Symbol("iter".to_string()),
                            Expr::Symbol("reps".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "z2".to_string(),
                    value: Expr::Call {
                        callee: "zip".to_string(),
                        args: vec![
                            Expr::Symbol("iter".to_string()),
                            Expr::Symbol("reps".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "flat".to_string(),
                    value: Expr::Call {
                        callee: "flatten".to_string(),
                        args: vec![Expr::Symbol("zipped".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "pv".to_string(),
                    value: Expr::Call {
                        callee: "pvalues".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Symbol("flat".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "mc".to_string(),
                    value: Expr::Call {
                        callee: "mapcat".to_string(),
                        args: vec![
                            Expr::Symbol("dec".to_string()),
                            Expr::Symbol("pv".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "d".to_string(),
                    value: Expr::Call {
                        callee: "dorun".to_string(),
                        args: vec![Expr::Symbol("mc".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "g".to_string(),
                    value: Expr::Call {
                        callee: "gensym".to_string(),
                        args: vec![Expr::Str("tmp__".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "cv".to_string(),
                    value: Expr::Call {
                        callee: "constantly".to_string(),
                        args: vec![Expr::Int(7), Expr::Int(99)],
                    },
                },
                TopLevel::Def {
                    name: "pv2".to_string(),
                    value: Expr::Call {
                        callee: "partial".to_string(),
                        args: vec![Expr::Symbol("+".to_string()), Expr::Int(3), Expr::Int(5)],
                    },
                },
                TopLevel::Def {
                    name: "cmpv".to_string(),
                    value: Expr::Call {
                        callee: "comp".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Symbol("dec".to_string()),
                            Expr::Int(10),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "cmplv".to_string(),
                    value: Expr::Call {
                        callee: "complement".to_string(),
                        args: vec![Expr::Symbol("even?".to_string()), Expr::Int(3)],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("z2".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_map_indexed_i64"));
        assert!(c.source.contains("clv_keep_indexed_i64"));
        assert!(c.source.contains("clv_iterate_i64"));
        assert!(c.source.contains("clv_repeatedly_i64"));
        assert!(c.source.contains("clv_zip_with_i64"));
        assert!(c.source.contains("clv_zip_i64"));
        assert!(c.source.contains("clv_dorun_i64"));
        assert!(c.source.contains("clv_gensym"));
        assert!(c.source.contains("clv_apply_map_i64"));
        assert!(c.source.contains("clv_apply_pred_i64"));
    }

    #[test]
    fn emit_map_core_batch_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "m".to_string(),
                    value: Expr::Map(vec![
                        (Expr::Keyword("a".to_string()), Expr::Int(1)),
                        (Expr::Keyword("b".to_string()), Expr::Int(2)),
                    ]),
                },
                TopLevel::Def {
                    name: "m2".to_string(),
                    value: Expr::Call {
                        callee: "hash-map".to_string(),
                        args: vec![
                            Expr::Keyword("a".to_string()),
                            Expr::Int(3),
                            Expr::Keyword("c".to_string()),
                            Expr::Int(4),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "z".to_string(),
                    value: Expr::Call {
                        callee: "zipmap".to_string(),
                        args: vec![
                            Expr::Vector(vec![
                                Expr::Keyword("x".to_string()),
                                Expr::Keyword("y".to_string()),
                            ]),
                            Expr::Vector(vec![Expr::Int(10), Expr::Int(20)]),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "m3".to_string(),
                    value: Expr::Call {
                        callee: "merge-with".to_string(),
                        args: vec![
                            Expr::Symbol("+".to_string()),
                            Expr::Symbol("m".to_string()),
                            Expr::Symbol("m2".to_string()),
                            Expr::Symbol("z".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "ks".to_string(),
                    value: Expr::Call {
                        callee: "keys".to_string(),
                        args: vec![Expr::Symbol("m3".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "vs".to_string(),
                    value: Expr::Call {
                        callee: "vals".to_string(),
                        args: vec![Expr::Symbol("m3".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "pick".to_string(),
                    value: Expr::Call {
                        callee: "select-keys".to_string(),
                        args: vec![
                            Expr::Symbol("m3".to_string()),
                            Expr::Vector(vec![Expr::Keyword("a".to_string())]),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "m4".to_string(),
                    value: Expr::Call {
                        callee: "assoc".to_string(),
                        args: vec![
                            Expr::Symbol("pick".to_string()),
                            Expr::Keyword("d".to_string()),
                            Expr::Int(9),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "m5".to_string(),
                    value: Expr::Call {
                        callee: "update-in".to_string(),
                        args: vec![
                            Expr::Symbol("m4".to_string()),
                            Expr::Vector(vec![Expr::Keyword("d".to_string())]),
                            Expr::Symbol("inc".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "sum".to_string(),
                    value: Expr::Call {
                        callee: "reduce-kv".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["acc".to_string(), "k".to_string(), "v".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "+".to_string(),
                                    args: vec![
                                        Expr::Symbol("acc".to_string()),
                                        Expr::Symbol("v".to_string()),
                                    ],
                                }),
                            },
                            Expr::Int(0),
                            Expr::Symbol("m5".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("sum".to_string())],
                }),
            ],
        };

        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_map_ki64_new"));
        assert!(c.source.contains("clv_zipmap_ki64"));
        assert!(c.source.contains("clv_map_keys_ki64"));
        assert!(c.source.contains("clv_map_vals_ki64"));
        assert!(c.source.contains("clv_map_select_keys_ki64"));
        assert!(c.source.contains("clv_map_merge_with_ki64"));
        assert!(c.source.contains("clv_map_assoc_ki64"));
        assert!(c.source.contains("clv_reduce_kv_ki64"));
    }

    #[test]
    fn emit_misc_core_batch4_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(32)],
                    },
                },
                TopLevel::Def {
                    name: "sb".to_string(),
                    value: Expr::Call {
                        callee: "sort-by".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "fr".to_string(),
                    value: Expr::Call {
                        callee: "frequencies".to_string(),
                        args: vec![Expr::Call {
                            callee: "split-lines".to_string(),
                            args: vec![Expr::Str("a\na\nb\n".to_string())],
                        }],
                    },
                },
                TopLevel::Def {
                    name: "piped".to_string(),
                    value: Expr::Call {
                        callee: "pipe".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Symbol("dec".to_string()),
                            Expr::Int(10),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "jx".to_string(),
                    value: Expr::Call {
                        callee: "juxt".to_string(),
                        args: vec![
                            Expr::Symbol("inc".to_string()),
                            Expr::Symbol("dec".to_string()),
                            Expr::Int(7),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "jx2".to_string(),
                    value: Expr::Call {
                        callee: "__juxt-call".to_string(),
                        args: vec![
                            Expr::Vector(vec![
                                Expr::Symbol("inc".to_string()),
                                Expr::Symbol("dec".to_string()),
                            ]),
                            Expr::Vector(vec![Expr::Int(9)]),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "casted".to_string(),
                    value: Expr::Call {
                        callee: "as".to_string(),
                        args: vec![Expr::Symbol("Int".to_string()), Expr::Int(1)],
                    },
                },
                TopLevel::Def {
                    name: "checked".to_string(),
                    value: Expr::Call {
                        callee: "expect".to_string(),
                        args: vec![
                            Expr::Symbol("Int".to_string()),
                            Expr::Symbol("casted".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "tm".to_string(),
                    value: Expr::Call {
                        callee: "time".to_string(),
                        args: vec![Expr::Call {
                            callee: "reduce".to_string(),
                            args: vec![
                                Expr::Symbol("+".to_string()),
                                Expr::Int(0),
                                Expr::Symbol("xs".to_string()),
                            ],
                        }],
                    },
                },
                TopLevel::Def {
                    name: "bm".to_string(),
                    value: Expr::Call {
                        callee: "bench".to_string(),
                        args: vec![
                            Expr::Int(5),
                            Expr::Call {
                                callee: "count".to_string(),
                                args: vec![Expr::Symbol("sb".to_string())],
                            },
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "throw".to_string(),
                    args: vec![Expr::Call {
                        callee: "runtime-error".to_string(),
                        args: vec![Expr::Str("boom".to_string()), Expr::Int(1)],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_sort_by_i64"));
        assert!(c.source.contains("clv_frequencies_str"));
        assert!(c.source.contains("clv_now_ns"));
        assert!(c.source.contains("clv_apply_map_i64"));
        assert!(c.source.contains("clv_map_ki64_put(&"));
        assert!(c.source.contains(":elapsed-ns"));
        assert!(c.source.contains("fprintf(stderr, \"%s\\n\","));
    }

    #[test]
    fn emit_partition_group_by_and_comp_call_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "xs".to_string(),
                    value: Expr::Call {
                        callee: "range".to_string(),
                        args: vec![Expr::Int(0), Expr::Int(64)],
                    },
                },
                TopLevel::Def {
                    name: "compv".to_string(),
                    value: Expr::Call {
                        callee: "__comp-call".to_string(),
                        args: vec![
                            Expr::Vector(vec![
                                Expr::Symbol("inc".to_string()),
                                Expr::Symbol("dec".to_string()),
                            ]),
                            Expr::Vector(vec![Expr::Int(10)]),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "p1".to_string(),
                    value: Expr::Call {
                        callee: "partition".to_string(),
                        args: vec![Expr::Int(4), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "p2".to_string(),
                    value: Expr::Call {
                        callee: "partition-all".to_string(),
                        args: vec![Expr::Int(7), Expr::Int(3), Expr::Symbol("xs".to_string())],
                    },
                },
                TopLevel::Def {
                    name: "p3".to_string(),
                    value: Expr::Call {
                        callee: "partition-by".to_string(),
                        args: vec![
                            Expr::Symbol("odd?".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "g1".to_string(),
                    value: Expr::Call {
                        callee: "group-by".to_string(),
                        args: vec![
                            Expr::Symbol("odd?".to_string()),
                            Expr::Symbol("xs".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("p1".to_string())],
                    }],
                }),
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "count".to_string(),
                        args: vec![Expr::Symbol("g1".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_partition_i64"));
        assert!(c.source.contains("clv_partition_by_pred_i64"));
        assert!(c.source.contains("clv_group_by_pred_i64"));
        assert!(c.source.contains("clv_map_i64_vec_i64"));
        assert!(c.source.contains("clv_apply_map_i64"));
    }

    #[test]
    fn emit_let_expr_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Let {
                    bindings: vec![
                        ("x".to_string(), Expr::Int(10)),
                        ("y".to_string(), Expr::Int(20)),
                    ],
                    body: Box::new(Expr::Call {
                        callee: "+".to_string(),
                        args: vec![Expr::Symbol("x".to_string()), Expr::Symbol("y".to_string())],
                    }),
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("int64_t let_"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_if_expr_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::If {
                    cond: Box::new(Expr::Bool(true)),
                    then_expr: Box::new(Expr::Int(1)),
                    else_expr: Box::new(Expr::Int(2)),
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("if ("));
        assert!(c.source.contains("int64_t if_result_"));
        assert!(c.source.contains("printf(\"%lld\\n\""));
    }

    #[test]
    fn emit_when_let_rewrite_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Let {
                    bindings: vec![("__whenlet".to_string(), Expr::Int(2))],
                    body: Box::new(Expr::If {
                        cond: Box::new(Expr::Symbol("__whenlet".to_string())),
                        then_expr: Box::new(Expr::Let {
                            bindings: vec![(
                                "x".to_string(),
                                Expr::Symbol("__whenlet".to_string()),
                            )],
                            body: Box::new(Expr::Do(vec![
                                Expr::Call {
                                    callee: "println".to_string(),
                                    args: vec![Expr::Symbol("x".to_string())],
                                },
                                Expr::Symbol("x".to_string()),
                            ])),
                        }),
                        else_expr: Box::new(Expr::Nil),
                    }),
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("printf(\"%lld\\n\""));
        assert!(c.source.contains("if ("));
    }

    #[test]
    fn emit_optional_if_expr_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::If {
                    cond: Box::new(Expr::Bool(true)),
                    then_expr: Box::new(Expr::Int(1)),
                    else_expr: Box::new(Expr::Nil),
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_opt_i64"));
        assert!(c.source.contains(".has"));
    }

    #[test]
    fn emit_loop_recur_as_while_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Let {
                    bindings: vec![(
                        "__loop__1".to_string(),
                        Expr::Lambda {
                            params: vec!["i".to_string()],
                            body: Box::new(Expr::If {
                                cond: Box::new(Expr::Call {
                                    callee: "<".to_string(),
                                    args: vec![Expr::Symbol("i".to_string()), Expr::Int(3)],
                                }),
                                then_expr: Box::new(Expr::Call {
                                    callee: "__loop__1".to_string(),
                                    args: vec![Expr::Call {
                                        callee: "+".to_string(),
                                        args: vec![Expr::Symbol("i".to_string()), Expr::Int(1)],
                                    }],
                                }),
                                else_expr: Box::new(Expr::Symbol("i".to_string())),
                            }),
                        },
                    )],
                    body: Box::new(Expr::Call {
                        callee: "__loop__1".to_string(),
                        args: vec![Expr::Int(0)],
                    }),
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("while (true)"));
        assert!(c.source.contains("continue;"));
        assert!(c.source.contains("break;"));
    }

    #[test]
    fn emit_variadic_println_with_vector() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![
                    Expr::Str("xs".to_string()),
                    Expr::Vector(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]),
                ],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("printf(\" \")"));
        assert!(c.source.contains("clv_vec_i64_fprint"));
    }

    #[test]
    fn emit_map_lambda_square_and_double_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "sq".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "*".to_string(),
                                    args: vec![
                                        Expr::Symbol("x".to_string()),
                                        Expr::Symbol("x".to_string()),
                                    ],
                                }),
                            },
                            Expr::Vector(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]),
                        ],
                    },
                },
                TopLevel::Def {
                    name: "dbl".to_string(),
                    value: Expr::Call {
                        callee: "map".to_string(),
                        args: vec![
                            Expr::Lambda {
                                params: vec!["x".to_string()],
                                body: Box::new(Expr::Call {
                                    callee: "+".to_string(),
                                    args: vec![
                                        Expr::Symbol("x".to_string()),
                                        Expr::Symbol("x".to_string()),
                                    ],
                                }),
                            },
                            Expr::Symbol("sq".to_string()),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Symbol("dbl".to_string())],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains(", 27, 0LL);"));
        assert!(c.source.contains(", 28, 0LL);"));
        assert!(c.source.contains("clv_vec_i64_fprint"));
    }

    #[test]
    fn emit_flatten_vecvec_c() {
        let program = FrontProgram {
            top_levels: vec![
                TopLevel::Def {
                    name: "parts".to_string(),
                    value: Expr::Call {
                        callee: "partition".to_string(),
                        args: vec![
                            Expr::Int(2),
                            Expr::Vector(vec![
                                Expr::Int(1),
                                Expr::Int(2),
                                Expr::Int(3),
                                Expr::Int(4),
                            ]),
                        ],
                    },
                },
                TopLevel::Expr(Expr::Call {
                    callee: "println".to_string(),
                    args: vec![Expr::Call {
                        callee: "flatten".to_string(),
                        args: vec![Expr::Symbol("parts".to_string())],
                    }],
                }),
            ],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_partition_i64"));
        assert!(c.source.contains("clv_flatten_vec_vec_i64"));
        assert!(c.source.contains("clv_vec_i64_fprint"));
    }

    #[test]
    fn emit_json_roundtrip_c() {
        let program = FrontProgram {
            top_levels: vec![TopLevel::Expr(Expr::Call {
                callee: "println".to_string(),
                args: vec![Expr::Call {
                    callee: "json::read-string".to_string(),
                    args: vec![Expr::Call {
                        callee: "json::write-string".to_string(),
                        args: vec![Expr::Map(vec![
                            (Expr::Str("a".to_string()), Expr::Int(1)),
                            (Expr::Str("b".to_string()), Expr::Int(2)),
                        ])],
                    }],
                }],
            })],
        };
        let c = emit_c(&program, &RuntimeConfig::default()).expect("emit should succeed");
        assert!(c.source.contains("clv_json_write_ki64"));
        assert!(c.source.contains("clv_json_read_ki64"));
        assert!(c.source.contains("clv_map_ki64_println"));
    }
}
