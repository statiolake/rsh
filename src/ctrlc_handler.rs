use anyhow::Result;
use once_cell::sync::Lazy;
use shared_child::SharedChild;
use std::sync::{Mutex, Weak};

static CHILD_PROCESSES: Lazy<Mutex<Vec<Weak<SharedChild>>>> = Lazy::new(|| Mutex::new(Vec::new()));

pub fn init_ctrlc_handler() -> Result<()> {
    ctrlc::set_handler(|| {
        let mut children = CHILD_PROCESSES.lock().expect("children list is poisoned");

        // call valid handlers
        for child in &mut **children {
            if let Some(child) = child.upgrade() {
                let pid = child.id();
                let _ = child.kill();
                eprintln!("rsh: killed process {}", pid);
            }
        }

        // remove invalid handlers
        children.retain(|child| child.upgrade().is_some());
    })?;

    Ok(())
}

pub fn register_child_process(child: Weak<SharedChild>) {
    let mut children = CHILD_PROCESSES.lock().expect("children list is poisoned");
    children.push(child);

    // remove invalid handlers
    children.retain(|child| child.upgrade().is_some());
}
