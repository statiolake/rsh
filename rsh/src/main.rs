mod cmdline;
mod ctrlc_handler;
mod line_parser;
mod shell;

use anyhow::Result;
use ctrlc_handler::init_ctrlc_handler;
use shell::Shell;

fn main() -> Result<()> {
    init_ctrlc_handler()?;

    let mut shell = Shell::new()?;
    shell.mainloop();

    Ok(())
}
