use anyhow::Result;
use duct::Handle;
use once_cell::sync::Lazy;
use std::sync::{Mutex, Weak};

static CHILD_HANDLES: Lazy<Mutex<Vec<Weak<Handle>>>> = Lazy::new(|| Mutex::new(Vec::new()));

pub fn init_ctrlc_handler() -> Result<()> {
    ctrlc::set_handler(|| {
        let mut handles = CHILD_HANDLES.lock().expect("handle list is poisoned");

        // call valid handlers
        for handle in &mut **handles {
            if let Some(handle) = handle.upgrade() {
                let pid = handle.pids()[0];
                let _ = handle.kill();
                eprintln!("rsh: killed process {}", pid);
            }
        }

        // remove invalid handlers
        handles.retain(|handle| handle.upgrade().is_some());
    })?;

    Ok(())
}

pub fn register_child_handle(handle: Weak<Handle>) {
    let mut handles = CHILD_HANDLES.lock().expect("children list is poisoned");
    handles.push(handle);

    // remove invalid handlers
    handles.retain(|handle| handle.upgrade().is_some());
}
