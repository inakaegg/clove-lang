use crate::ast::Span;
use crate::error::CloveError;
use crate::interrupt;

use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug)]
pub struct GuardConfig {
    pub enabled: bool,
    pub soft_bytes: u64,
    pub hard_bytes: u64,
    pub check_every: Duration,
    pub warn_every: Duration,
    pub iter_mask: u64,
}

impl GuardConfig {
    pub fn normalized(mut self) -> Self {
        if self.hard_bytes == 0 {
            self.enabled = false;
            self.soft_bytes = 0;
            return self;
        }
        if self.soft_bytes >= self.hard_bytes {
            self.soft_bytes = self.hard_bytes.saturating_mul(3) / 4;
        }
        self
    }
}

pub fn default_config() -> GuardConfig {
    let mut hard = 4_u64 * 1024 * 1024 * 1024;
    if let Some(total) = total_memory_bytes() {
        let scaled = (total / 10).saturating_mul(6);
        hard = hard.min(scaled);
    }
    let soft = (3_u64 * 1024 * 1024 * 1024).min(hard.saturating_mul(3) / 4);
    GuardConfig {
        enabled: true,
        soft_bytes: soft,
        hard_bytes: hard,
        check_every: Duration::from_millis(50),
        warn_every: Duration::from_millis(1000),
        iter_mask: 0x3fff,
    }
}

#[cfg(feature = "repl_guard")]
mod guard_impl {
    use super::*;
    use crate::error::WARN_TAG;
    use std::cell::RefCell;
    use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

    static ENABLED: AtomicBool = AtomicBool::new(false);
    static SOFT_BYTES: AtomicU64 = AtomicU64::new(0);
    static HARD_BYTES: AtomicU64 = AtomicU64::new(0);
    static CHECK_EVERY_MS: AtomicU64 = AtomicU64::new(50);
    static WARN_EVERY_MS: AtomicU64 = AtomicU64::new(1000);
    static ITER_MASK: AtomicU64 = AtomicU64::new(0x3fff);

    struct GuardState {
        iters: u64,
        last_check: Instant,
        last_warn: Instant,
    }

    thread_local! {
        static STATE: RefCell<GuardState> = RefCell::new(GuardState {
            iters: 0,
            last_check: Instant::now(),
            last_warn: Instant::now().checked_sub(Duration::from_secs(3600)).unwrap_or_else(Instant::now),
        });
    }

    pub(super) fn configure_inner(config: GuardConfig) {
        let config = config.normalized();
        ENABLED.store(config.enabled, Ordering::SeqCst);
        SOFT_BYTES.store(config.soft_bytes, Ordering::SeqCst);
        HARD_BYTES.store(config.hard_bytes, Ordering::SeqCst);
        CHECK_EVERY_MS.store(config.check_every.as_millis() as u64, Ordering::SeqCst);
        WARN_EVERY_MS.store(config.warn_every.as_millis() as u64, Ordering::SeqCst);
        ITER_MASK.store(config.iter_mask, Ordering::SeqCst);
    }

    pub(super) fn tick_inner(span: Option<Span>) -> Result<(), CloveError> {
        if interrupt::is_interrupted() {
            return Err(CloveError::guard("Interrupted"));
        }
        if !ENABLED.load(Ordering::Relaxed) {
            return Ok(());
        }
        let hard = HARD_BYTES.load(Ordering::Relaxed);
        if hard == 0 {
            return Ok(());
        }
        let soft = SOFT_BYTES.load(Ordering::Relaxed);
        let check_every = Duration::from_millis(CHECK_EVERY_MS.load(Ordering::Relaxed));
        let warn_every = Duration::from_millis(WARN_EVERY_MS.load(Ordering::Relaxed));
        let iter_mask = ITER_MASK.load(Ordering::Relaxed);

        let mut should_check = false;
        let mut should_warn = false;
        let mut rss = None;

        STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            state.iters = state.iters.wrapping_add(1);
            if (state.iters & iter_mask) == 0 || state.last_check.elapsed() >= check_every {
                state.last_check = Instant::now();
                if let Some(current) = current_rss_bytes() {
                    rss = Some(current);
                    if soft > 0 && current > soft && state.last_warn.elapsed() >= warn_every {
                        state.last_warn = Instant::now();
                        should_warn = true;
                    }
                }
                should_check = true;
            }
        });

        if !should_check {
            return Ok(());
        }

        let Some(current) = rss else {
            return Ok(());
        };
        if current > hard {
            return Err(memory_error(current, soft, hard, span));
        }
        if should_warn && soft > 0 && current > soft {
            eprintln!(
                "{} memory guard: rss={} > soft={}",
                WARN_TAG,
                human_bytes(current),
                human_bytes(soft)
            );
        }
        Ok(())
    }

    fn memory_error(rss: u64, soft: u64, hard: u64, span: Option<Span>) -> CloveError {
        let mut msg = format!(
            "Memory limit exceeded (rss={}, hard={}; soft={})",
            human_bytes(rss),
            human_bytes(hard),
            human_bytes(soft)
        );
        msg.push_str(
            "\nHints:\n  - eager interleave/map may be materializing an unbounded sequence\n  - add (take N ...) or use a lazy variant to avoid full materialization\n  - for JSON, prefer streaming/foreign representation for huge data",
        );
        let mut err = CloveError::guard(msg);
        if let Some(span) = span {
            err = err.with_span(span);
        }
        err
    }

    #[cfg(target_os = "linux")]
    fn current_rss_bytes() -> Option<u64> {
        let text = std::fs::read_to_string("/proc/self/status").ok()?;
        for line in text.lines() {
            if let Some(rest) = line.strip_prefix("VmRSS:") {
                let parts = rest.trim().split_whitespace().collect::<Vec<_>>();
                if parts.is_empty() {
                    continue;
                }
                if let Ok(kb) = parts[0].parse::<u64>() {
                    return Some(kb.saturating_mul(1024));
                }
            }
        }
        None
    }

    #[cfg(target_os = "macos")]
    fn current_rss_bytes() -> Option<u64> {
        use libc::{
            mach_msg_type_number_t, mach_task_basic_info, mach_task_basic_info_data_t,
            mach_task_self, task_info, KERN_SUCCESS, MACH_TASK_BASIC_INFO,
        };
        let mut info: mach_task_basic_info_data_t = unsafe { std::mem::zeroed() };
        let mut count = libc::MACH_TASK_BASIC_INFO_COUNT as mach_msg_type_number_t;
        let result = unsafe {
            task_info(
                mach_task_self(),
                MACH_TASK_BASIC_INFO,
                &mut info as *mut mach_task_basic_info as *mut _,
                &mut count as *mut _,
            )
        };
        if result != KERN_SUCCESS {
            return None;
        }
        Some(info.resident_size as u64)
    }

    #[cfg(target_os = "macos")]
    fn total_memory_bytes_impl() -> Option<u64> {
        use libc::{c_void, size_t, sysctlbyname};
        use std::ffi::CString;
        let name = CString::new("hw.memsize").ok()?;
        let mut value: u64 = 0;
        let mut size = std::mem::size_of::<u64>() as size_t;
        let ret = unsafe {
            sysctlbyname(
                name.as_ptr(),
                &mut value as *mut _ as *mut c_void,
                &mut size as *mut _,
                std::ptr::null_mut(),
                0,
            )
        };
        if ret != 0 {
            None
        } else {
            Some(value)
        }
    }

    #[cfg(target_os = "linux")]
    fn total_memory_bytes_impl() -> Option<u64> {
        let text = std::fs::read_to_string("/proc/meminfo").ok()?;
        for line in text.lines() {
            if let Some(rest) = line.strip_prefix("MemTotal:") {
                let parts = rest.trim().split_whitespace().collect::<Vec<_>>();
                if parts.is_empty() {
                    continue;
                }
                if let Ok(kb) = parts[0].parse::<u64>() {
                    return Some(kb.saturating_mul(1024));
                }
            }
        }
        None
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    fn current_rss_bytes() -> Option<u64> {
        None
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    fn total_memory_bytes_impl() -> Option<u64> {
        None
    }

    pub(super) fn human_bytes(bytes: u64) -> String {
        const KB: f64 = 1024.0;
        const MB: f64 = 1024.0 * 1024.0;
        const GB: f64 = 1024.0 * 1024.0 * 1024.0;
        let b = bytes as f64;
        if b >= GB {
            format!("{:.2}GB", b / GB)
        } else if b >= MB {
            format!("{:.2}MB", b / MB)
        } else if b >= KB {
            format!("{:.2}KB", b / KB)
        } else {
            format!("{}B", bytes)
        }
    }

    pub(super) fn total_memory_bytes() -> Option<u64> {
        total_memory_bytes_impl()
    }
}

#[cfg(feature = "repl_guard")]
pub fn configure(config: GuardConfig) {
    guard_impl::configure_inner(config);
}

#[cfg(not(feature = "repl_guard"))]
pub fn configure(_config: GuardConfig) {}

#[cfg(feature = "repl_guard")]
pub fn tick(span: Option<Span>) -> Result<(), CloveError> {
    guard_impl::tick_inner(span)
}

#[cfg(not(feature = "repl_guard"))]
pub fn tick(_span: Option<Span>) -> Result<(), CloveError> {
    interrupt::check_for_interrupt()
}

#[cfg(feature = "repl_guard")]
fn total_memory_bytes() -> Option<u64> {
    guard_impl::total_memory_bytes()
}

#[cfg(not(feature = "repl_guard"))]
fn total_memory_bytes() -> Option<u64> {
    None
}
