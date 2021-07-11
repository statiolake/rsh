mod line_parser;
mod shell;

use anyhow::Result;
use shell::Shell;

fn main() -> Result<()> {
    let mut shell = Shell::new()?;
    shell.mainloop();

    Ok(())
}
