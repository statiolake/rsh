use anyhow::Result;
use once_cell::sync::Lazy;
use shared_child::SharedChild;
use std::sync::{Mutex, Weak};

static CHILDLEN: Lazy<Mutex<Vec<Weak<SharedChild>>>> = Lazy::new(|| Mutex::new(Vec::new()));

pub fn init_ctrlc_handler() -> Result<()> {
    ctrlc::set_handler(|| {
        let mut children = CHILDLEN.lock().expect("child list is poisoned");

        // kill children
        for child in &mut **children {
            if let Some(child) = child.upgrade() {
                let pid = child.id();
                let _ = child.kill();
                eprintln!("rsh: killed process {}", pid);
            }
        }

        // remove invalid children
        children.retain(|child| child.upgrade().is_some());
    })?;

    Ok(())
}

pub fn register_child(child: Weak<SharedChild>) {
    let mut children = CHILDLEN.lock().expect("child list is poisoned");
    children.push(child);

    // remove invalid children
    children.retain(|child| child.upgrade().is_some());
}
