use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
};
use anyhow::Result;
use std::env::set_var;

#[derive(Debug)]
pub struct CmdSet(Vec<String>);

impl CmdSet {
    pub fn new(args: Vec<String>) -> Self {
        CmdSet(args)
    }
}

impl Executable for CmdSet {
    fn executable(&self) -> Option<&std::path::Path> {
        None
    }

    fn execute(
        &mut self,
        _state: &mut ShellState,
        _stdin: Box<dyn ReadIntoStdio>,
        _stdout: Box<dyn WriteIntoStdio>,
        mut stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        if self.0.len() != 2 {
            writeln!(stderr, "set: requires exact two arguments")?;
            return Ok(Exit::Failure);
        }

        let key = &self.0[0];
        let value = &self.0[1];
        set_var(key, value);
        Ok(Exit::Success)
    }
}
