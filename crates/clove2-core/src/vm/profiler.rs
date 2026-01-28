use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Default, Clone, Copy)]
struct OpStat {
    count: u64,
    total_ns: u128,
}

#[derive(Default)]
struct VmProfilerState {
    op_stats: HashMap<&'static str, OpStat>,
    total_ns: u128,
}

static VM_PROF_ENABLED: AtomicBool = AtomicBool::new(false);

thread_local! {
    static VM_PROFILER: RefCell<VmProfilerState> = RefCell::new(VmProfilerState::default());
}

pub fn set_enabled(enabled: bool) {
    VM_PROF_ENABLED.store(enabled, Ordering::Relaxed);
}

pub fn is_enabled() -> bool {
    VM_PROF_ENABLED.load(Ordering::Relaxed)
}

pub fn reset() {
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        state.op_stats.clear();
        state.total_ns = 0;
    });
}

pub(crate) fn record_op_label(label: &'static str, elapsed_ns: u128) {
    if !is_enabled() {
        return;
    }
    VM_PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        let entry = state.op_stats.entry(label).or_default();
        entry.count = entry.count.saturating_add(1);
        entry.total_ns = entry.total_ns.saturating_add(elapsed_ns);
        state.total_ns = state.total_ns.saturating_add(elapsed_ns);
    });
}

pub fn report() -> Option<String> {
    if !is_enabled() {
        return None;
    }
    VM_PROFILER.with(|cell| {
        let state = cell.borrow();
        let mut out = String::new();
        if state.op_stats.is_empty() {
            out.push_str("[vm-prof] no entries\n");
            return Some(out);
        }
        let mut ops: Vec<(&'static str, OpStat)> =
            state.op_stats.iter().map(|(k, v)| (*k, *v)).collect();
        ops.sort_by(|a, b| b.1.total_ns.cmp(&a.1.total_ns));
        out.push_str("[vm-prof] opcode time (top 12)\n");
        for (name, stat) in ops.iter().take(12) {
            let total_ms = stat.total_ns as f64 / 1_000_000.0;
            let pct = if state.total_ns > 0 {
                (stat.total_ns as f64 / state.total_ns as f64) * 100.0
            } else {
                0.0
            };
            out.push_str(&format!(
                "[vm-prof] {:<20} {:>10.3}ms {:>6.2}% {:>10}\n",
                name, total_ms, pct, stat.count
            ));
        }
        ops.sort_by(|a, b| b.1.count.cmp(&a.1.count));
        out.push_str("[vm-prof] opcode counts (top 12)\n");
        for (name, stat) in ops.iter().take(12) {
            out.push_str(&format!("[vm-prof] {:<20} {:>10}\n", name, stat.count));
        }
        Some(out)
    })
}
