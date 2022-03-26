use anyhow::Result;
use crossterm_winapi::{ConsoleMode, Handle};

pub struct ConsoleModeKeeper {
    mode: u32,
}

impl ConsoleModeKeeper {
    pub fn new() -> Result<Self> {
        Ok(Self {
            mode: get_console_mode()?,
        })
    }
}

impl Drop for ConsoleModeKeeper {
    fn drop(&mut self) {
        let _ = set_console_mode(self.mode);
    }
}

fn get_console_mode() -> Result<u32> {
    let handle = ConsoleMode::from(Handle::current_out_handle()?);
    Ok(handle.mode()?)
}

fn set_console_mode(mode: u32) -> Result<()> {
    let handle = ConsoleMode::from(Handle::current_out_handle()?);
    handle.set_mode(mode)?;

    Ok(())
}
