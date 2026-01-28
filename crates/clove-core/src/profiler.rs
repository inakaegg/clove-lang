use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::time::Instant;

#[derive(Default, Clone, Copy)]
struct Stat {
    calls: u64,
    total_ns: u128,
}

#[derive(Default)]
struct ProfilerState {
    enabled: bool,
    stats: HashMap<String, Stat>,
}

thread_local! {
    static PROFILER: RefCell<ProfilerState> = RefCell::new(ProfilerState::default());
}

pub struct ProfileGuard {
    label: String,
    start: Instant,
}

pub fn enable_from_env() {
    if env_flag("CLOVE_PROFILE") {
        set_enabled(true);
    }
}

pub fn set_enabled(enabled: bool) {
    PROFILER.with(|cell| {
        cell.borrow_mut().enabled = enabled;
    });
}

pub fn is_enabled() -> bool {
    PROFILER.with(|cell| cell.borrow().enabled)
}

pub fn enter(label: &str) -> Option<ProfileGuard> {
    if !is_enabled() {
        return None;
    }
    Some(ProfileGuard {
        label: label.to_string(),
        start: Instant::now(),
    })
}

impl Drop for ProfileGuard {
    fn drop(&mut self) {
        let elapsed = self.start.elapsed().as_nanos();
        let label = self.label.clone();
        PROFILER.with(|cell| {
            let mut state = cell.borrow_mut();
            if !state.enabled {
                return;
            }
            let entry = state.stats.entry(label).or_default();
            entry.calls = entry.calls.saturating_add(1);
            entry.total_ns = entry.total_ns.saturating_add(elapsed);
        });
    }
}

pub fn reset() {
    PROFILER.with(|cell| {
        let mut state = cell.borrow_mut();
        state.stats.clear();
    });
}

pub fn report() -> Option<String> {
    PROFILER.with(|cell| {
        let state = cell.borrow();
        if !state.enabled {
            return None;
        }
        if state.stats.is_empty() {
            return Some("[profile] no entries".to_string());
        }
        let mut entries: Vec<(&String, Stat)> = state.stats.iter().map(|(k, v)| (k, *v)).collect();
        entries.sort_by(|a, b| b.1.total_ns.cmp(&a.1.total_ns));
        let total_ns: u128 = entries.iter().map(|(_, s)| s.total_ns).sum();
        let total_ms = total_ns as f64 / 1_000_000.0;
        let name_width = entries
            .iter()
            .map(|(k, _)| k.len())
            .max()
            .unwrap_or(4)
            .max(4);
        let mut out = String::new();
        out.push_str(&format!(
            "[profile] total {:.3}ms, entries {}\n",
            total_ms,
            entries.len()
        ));
        out.push_str(&format!(
            "[profile] {:<width$} {:>10} {:>7} {:>8} {:>10}\n",
            "name",
            "total_ms",
            "pct",
            "calls",
            "avg_ms",
            width = name_width
        ));
        for (name, stat) in entries {
            let stat_total_ms = stat.total_ns as f64 / 1_000_000.0;
            let pct = if total_ns > 0 {
                (stat.total_ns as f64 / total_ns as f64) * 100.0
            } else {
                0.0
            };
            let avg_ms = if stat.calls > 0 {
                stat_total_ms / stat.calls as f64
            } else {
                0.0
            };
            out.push_str(&format!(
                "[profile] {:<width$} {:>10.3} {:>6.2}% {:>8} {:>10.3}\n",
                name,
                stat_total_ms,
                pct,
                stat.calls,
                avg_ms,
                width = name_width
            ));
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
