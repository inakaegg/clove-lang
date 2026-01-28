use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::error::CloveError;
use once_cell::sync::Lazy;

static INTERRUPTED: Lazy<Arc<AtomicBool>> = Lazy::new(|| Arc::new(AtomicBool::new(false)));

/// Request interruption on Ctrl+C/SIGTERM, etc.
pub fn request_interrupt() {
    INTERRUPTED.store(true, Ordering::SeqCst);
}

/// Shared flag referenced by signal-hook's flag API.
pub fn interrupt_flag() -> Arc<AtomicBool> {
    Arc::clone(&INTERRUPTED)
}

/// Whether an interrupt has been requested.
pub fn is_interrupted() -> bool {
    INTERRUPTED.load(Ordering::SeqCst)
}

/// Clear the flag when starting a new evaluation, etc.
pub fn clear_interrupt() {
    INTERRUPTED.store(false, Ordering::SeqCst);
}

/// Called in the eval loop; abort immediately on interrupt.
pub fn check_for_interrupt() -> Result<(), CloveError> {
    if is_interrupted() {
        Err(CloveError::runtime("execution interrupted"))
    } else {
        Ok(())
    }
}
