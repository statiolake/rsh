#[cfg(windows)]
#[path = "conmode_windows.rs"]
mod conmode;
#[cfg(not(windows))]
#[path = "conmode_other.rs"]
mod conmode;
mod ctrlc_handler;
mod executable;
mod shell;
mod view;

use anyhow::Result;
use ctrlc_handler::init_ctrlc_handler;
use shell::Shell;

fn main() -> Result<()> {
    init_ctrlc_handler()?;

    let mut shell = Shell::new()?;
    shell.mainloop();

    Ok(())
}
