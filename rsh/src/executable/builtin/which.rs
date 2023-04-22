use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
};
use anyhow::Result;
use which::which;

use super::find;

#[derive(Debug)]
pub struct CmdWhich(Vec<String>);

impl CmdWhich {
    pub fn new(args: Vec<String>) -> Self {
        CmdWhich(args)
    }
}

impl Executable for CmdWhich {
    fn executable(&self) -> Option<&std::path::Path> {
        None
    }

    fn execute(
        &mut self,
        _state: &mut ShellState,
        _stdin: Box<dyn ReadIntoStdio>,
        mut stdout: Box<dyn WriteIntoStdio>,
        mut stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        if self.0.len() != 1 {
            writeln!(stderr, "which: requires exact one argument")?;
            return Ok(Exit::Failure);
        }

        let name = &self.0[0];

        match find(name, vec![]) {
            Some(_) => writeln!(stdout, "(built-in command)")?,
            None => match which(name) {
                Ok(path) => writeln!(stdout, "{}", path.display())?,
                Err(_) => {
                    writeln!(stderr, "{}: not found", name)?;
                    return Ok(Exit::Failure);
                }
            },
        }

        Ok(Exit::Success)
    }
}
