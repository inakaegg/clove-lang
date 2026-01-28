use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::time::Instant;

use crate::vm::bytecode::{BuiltinId, Instruction};

#[derive(Default, Clone, Copy)]
struct ChunkStat {
    calls: u64,
    total_ns: u128,
}

#[derive(Default)]
struct VmProfilerState {
    enabled: bool,
    op_counts: HashMap<String, u64>,
    chunk_stats: HashMap<String, ChunkStat>,
    vm_attempts: u64,
    fallback_compile_unsupported: u64,
    fallback_runtime_unsupported: u64,
    fallback_compile_reasons: HashMap<String, FallbackReasonStat>,
    fallback_runtime_reasons: HashMap<String, FallbackReasonStat>,
}

#[derive(Default, Clone)]
struct FallbackReasonStat {
    count: u64,
    sample: Option<String>,
}

thread_local! {
    static VM_PROFILER: RefCell<VmProfilerState> = RefCell::new(VmProfilerState::default());
}

pub struct VmProfileGuard {
    label: String,
    start: Instant,
}

pub fn enable_from_env() {
    if env_flag("CLOVE_VM_PROF") {
        set_enabled(true);
    }
}

pub fn set_enabled(enabled: bool) {
    VM_PROFILER.with(|cell| {
        cell.borrow_mut().enabled = enabled;
    });
}

pub fn is_enabled() -> bool {
    VM_PROFILER.with(|cell| cell.borrow().enabled)
}

pub fn enter_chunk(label: &str) -> Option<VmProfileGuard> {
    if !is_enabled() {
        return None;
    }
    Some(VmProfileGuard {
        label: label.to_string(),
        start: Instant::now(),
    })
}

impl Drop for VmProfileGuard {
    fn drop(&mut self) {
        let elapsed = self.start.elapsed().as_nanos();
        let label = self.label.clone();
        VM_PROFILER.with(|cell| {
            let mut state = cell.borrow_mut();
            if !state.enabled {
                return;
            }
            let entry = state.chunk_stats.entry(label).or_default();
            entry.calls = entry.calls.saturating_add(1);
            entry.total_ns = entry.total_ns.saturating_add(elapsed);
        });
    }
}

pub fn count_op(instr: &Instruction) {
    if !is_enabled() {
        return;
    }
    let name = op_label(instr);
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        if !state.enabled {
            return;
        }
        let entry = state.op_counts.entry(name).or_insert(0);
        *entry = entry.saturating_add(1);
    });
}

pub enum VmFallbackReason {
    CompileUnsupported,
    RuntimeUnsupported,
}

pub fn count_vm_attempt() {
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        state.vm_attempts = state.vm_attempts.saturating_add(1);
    });
}

pub fn count_vm_fallback(reason: VmFallbackReason) {
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        match reason {
            VmFallbackReason::CompileUnsupported => {
                state.fallback_compile_unsupported =
                    state.fallback_compile_unsupported.saturating_add(1);
            }
            VmFallbackReason::RuntimeUnsupported => {
                state.fallback_runtime_unsupported =
                    state.fallback_runtime_unsupported.saturating_add(1);
            }
        }
    });
}

pub fn count_vm_fallback_detail(kind: VmFallbackReason, reason: &str, sample: Option<String>) {
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        let map = match kind {
            VmFallbackReason::CompileUnsupported => &mut state.fallback_compile_reasons,
            VmFallbackReason::RuntimeUnsupported => &mut state.fallback_runtime_reasons,
        };
        let entry = map.entry(reason.to_string()).or_default();
        entry.count = entry.count.saturating_add(1);
        if entry.sample.is_none() {
            entry.sample = sample;
        }
    });
}

pub fn reset() {
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        state.op_counts.clear();
        state.chunk_stats.clear();
        state.vm_attempts = 0;
        state.fallback_compile_unsupported = 0;
        state.fallback_runtime_unsupported = 0;
        state.fallback_compile_reasons.clear();
        state.fallback_runtime_reasons.clear();
    });
}

pub fn report() -> Option<String> {
    VM_PROFILER.with(|cell| {
        let state = cell.borrow();
        if !state.enabled {
            return None;
        }
        let mut out = String::new();
        let no_entries = state.op_counts.is_empty() && state.chunk_stats.is_empty();
        if no_entries {
            out.push_str("[vm-prof] no entries\n");
        }
        if !state.op_counts.is_empty() {
            let mut ops: Vec<(&String, u64)> =
                state.op_counts.iter().map(|(k, v)| (k, *v)).collect();
            ops.sort_by(|a, b| b.1.cmp(&a.1));
            out.push_str("[vm-prof] opcode counts (top 10)\n");
            for (name, count) in ops.into_iter().take(10) {
                out.push_str(&format!("[vm-prof] {:<24} {:>10}\n", name, count));
            }
        }
        if !state.chunk_stats.is_empty() {
            let mut chunks: Vec<(&String, ChunkStat)> =
                state.chunk_stats.iter().map(|(k, v)| (k, *v)).collect();
            chunks.sort_by(|a, b| b.1.total_ns.cmp(&a.1.total_ns));
            out.push_str("[vm-prof] chunk time (top 10)\n");
            for (name, stat) in chunks.iter().take(10) {
                let total_ms = stat.total_ns as f64 / 1_000_000.0;
                let avg_ms = if stat.calls > 0 {
                    total_ms / stat.calls as f64
                } else {
                    0.0
                };
                out.push_str(&format!(
                    "[vm-prof] {:<24} {:>10.3}ms {:>8} {:>10.3}ms\n",
                    name, total_ms, stat.calls, avg_ms
                ));
            }
            let mut by_calls = chunks;
            by_calls.sort_by(|a, b| b.1.calls.cmp(&a.1.calls));
            out.push_str("[vm-prof] chunk calls (top 10)\n");
            for (name, stat) in by_calls.into_iter().take(10) {
                out.push_str(&format!("[vm-prof] {:<24} {:>10}\n", name, stat.calls));
            }
        }
        if state.vm_attempts > 0 {
            let fallback_total =
                state.fallback_compile_unsupported + state.fallback_runtime_unsupported;
            let rate = fallback_total as f64 / state.vm_attempts as f64 * 100.0;
            out.push_str("[vm-prof] fallback summary\n");
            out.push_str(&format!(
                "[vm-prof] vm attempts           {:>10}\n",
                state.vm_attempts
            ));
            out.push_str(&format!(
                "[vm-prof] fallback compile      {:>10}\n",
                state.fallback_compile_unsupported
            ));
            out.push_str(&format!(
                "[vm-prof] fallback runtime      {:>10}\n",
                state.fallback_runtime_unsupported
            ));
            out.push_str(&format!("[vm-prof] fallback rate         {:>9.2}%\n", rate));
            if no_entries && fallback_total == state.vm_attempts {
                out.push_str("[vm-prof] note: fallback 100% (no vm ops executed)\n");
            }
        }
        if !state.fallback_compile_reasons.is_empty() {
            let mut reasons: Vec<(&String, &FallbackReasonStat)> =
                state.fallback_compile_reasons.iter().collect();
            reasons.sort_by(|a, b| b.1.count.cmp(&a.1.count));
            out.push_str("[vm-prof] fallback compile reasons (top 20)\n");
            for (name, stat) in reasons.into_iter().take(20) {
                if let Some(sample) = &stat.sample {
                    out.push_str(&format!(
                        "[vm-prof] {:<24} {:>10} {}\n",
                        name, stat.count, sample
                    ));
                } else {
                    out.push_str(&format!("[vm-prof] {:<24} {:>10}\n", name, stat.count));
                }
            }
        }
        if !state.fallback_runtime_reasons.is_empty() {
            let mut reasons: Vec<(&String, &FallbackReasonStat)> =
                state.fallback_runtime_reasons.iter().collect();
            reasons.sort_by(|a, b| b.1.count.cmp(&a.1.count));
            out.push_str("[vm-prof] fallback runtime reasons (top 20)\n");
            for (name, stat) in reasons.into_iter().take(20) {
                if let Some(sample) = &stat.sample {
                    out.push_str(&format!(
                        "[vm-prof] {:<24} {:>10} {}\n",
                        name, stat.count, sample
                    ));
                } else {
                    out.push_str(&format!("[vm-prof] {:<24} {:>10}\n", name, stat.count));
                }
            }
        }
        Some(out)
    })
}

pub fn print_report_if_enabled() {
    if let Some(report) = report() {
        eprint!("{}", report);
    }
}

fn env_flag(name: &str) -> bool {
    let Ok(value) = env::var(name) else {
        return false;
    };
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn op_label(instr: &Instruction) -> String {
    match instr {
        Instruction::Const(_, _) => "Const".to_string(),
        Instruction::ConstI64(_, _) => "ConstI64".to_string(),
        Instruction::ConstF64(_, _) => "ConstF64".to_string(),
        Instruction::ConstBool(_, _) => "ConstBool".to_string(),
        Instruction::ConstNil(_) => "ConstNil".to_string(),
        Instruction::LoadLocal(_, _) => "LoadLocal".to_string(),
        Instruction::StoreLocal(_, _) => "StoreLocal".to_string(),
        Instruction::LoadGlobalId(_, _) => "LoadGlobalId".to_string(),
        Instruction::DefGlobalId(_, _, _) => "DefGlobal".to_string(),
        Instruction::DefnGlobalId(_, _, _, _) => "DefnGlobal".to_string(),
        Instruction::DefLocalForm(_, _, _) => "DefLocal".to_string(),
        Instruction::DefLocalId(_, _) => "DefLocalId".to_string(),
        Instruction::AddI64(_, _) => "AddI64".to_string(),
        Instruction::AddF64(_, _) => "AddF64".to_string(),
        Instruction::SubI64(_, _) => "SubI64".to_string(),
        Instruction::SubF64(_, _) => "SubF64".to_string(),
        Instruction::MulI64(_, _) => "MulI64".to_string(),
        Instruction::MulF64(_, _) => "MulF64".to_string(),
        Instruction::DivF64(_, _) => "DivF64".to_string(),
        Instruction::EqI64(_, _) => "EqI64".to_string(),
        Instruction::EqF64(_, _) => "EqF64".to_string(),
        Instruction::LtI64(_, _) => "LtI64".to_string(),
        Instruction::LtF64(_, _) => "LtF64".to_string(),
        Instruction::LeI64(_, _) => "LeI64".to_string(),
        Instruction::LeF64(_, _) => "LeF64".to_string(),
        Instruction::GtI64(_, _) => "GtI64".to_string(),
        Instruction::GtF64(_, _) => "GtF64".to_string(),
        Instruction::GeI64(_, _) => "GeI64".to_string(),
        Instruction::GeF64(_, _) => "GeF64".to_string(),
        Instruction::IncI64(_, _) => "IncI64".to_string(),
        Instruction::IncF64(_, _) => "IncF64".to_string(),
        Instruction::DecI64(_, _) => "DecI64".to_string(),
        Instruction::DecF64(_, _) => "DecF64".to_string(),
        Instruction::MakeClosure(_, _, _) => "MakeClosure".to_string(),
        Instruction::MakeMultiClosure(_, _, _, _) => "MakeMultiClosure".to_string(),
        Instruction::AttachMeta(_, _) => "AttachMeta".to_string(),
        Instruction::AttachDoc(_, _) => "AttachDoc".to_string(),
        Instruction::Dup(_) => "Dup".to_string(),
        Instruction::MakeVector(_, _) => "MakeVector".to_string(),
        Instruction::MakeSet(_, _) => "MakeSet".to_string(),
        Instruction::MakeMap(_, _) => "MakeMap".to_string(),
        Instruction::TruncateLocals(_, _) => "TruncateLocals".to_string(),
        Instruction::Jump(_, _) => "Jump".to_string(),
        Instruction::JumpIfFalse(_, _) => "JumpIfFalse".to_string(),
        Instruction::JumpIfFalseBool(_, _) => "JumpIfFalseBool".to_string(),
        Instruction::Call0(_) => "Call0".to_string(),
        Instruction::Call1(_) => "Call1".to_string(),
        Instruction::Call2(_) => "Call2".to_string(),
        Instruction::Call3(_) => "Call3".to_string(),
        Instruction::Call4(_) => "Call4".to_string(),
        Instruction::Call(_, _) => "Call".to_string(),
        Instruction::Apply(_, _) => "Apply".to_string(),
        Instruction::OopIndex(_) => "OopIndex".to_string(),
        Instruction::OopMethod(_, _) => "OopMethod".to_string(),
        Instruction::CallBuiltin(id, _, _, _) => format!("CallBuiltin({})", builtin_label(*id)),
        Instruction::Pop(_) => "Pop".to_string(),
        Instruction::Return(_) => "Return".to_string(),
    }
}

fn builtin_label(id: BuiltinId) -> &'static str {
    id.name()
}
