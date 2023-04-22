use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
};
use anyhow::Result;

#[derive(Debug)]
pub struct CmdExit(Vec<String>);

impl CmdExit {
    pub fn new(args: Vec<String>) -> Self {
        CmdExit(args)
    }
}

impl Executable for CmdExit {
    fn executable(&self) -> Option<&std::path::Path> {
        None
    }

    fn execute(
        &mut self,
        state: &mut ShellState,
        _stdin: Box<dyn ReadIntoStdio>,
        _stdout: Box<dyn WriteIntoStdio>,
        mut stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        if !self.0.is_empty() {
            writeln!(stderr, "exit: does not take additional argument")?;
            return Ok(Exit::Failure);
        }

        state.loop_running = false;
        Ok(Exit::Success)
    }
}
