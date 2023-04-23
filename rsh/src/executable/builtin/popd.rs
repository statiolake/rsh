use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
    view::beautify_path,
};
use anyhow::{anyhow, Result};
use itertools::Itertools;

#[derive(Debug)]
pub struct CmdPopd(Vec<String>);

impl CmdPopd {
    pub fn new(args: Vec<String>) -> Self {
        CmdPopd(args)
    }
}

impl Executable for CmdPopd {
    fn executable(&self) -> Option<&std::path::Path> {
        None
    }

    fn execute(
        &mut self,
        state: &mut ShellState,
        _stdin: Box<dyn ReadIntoStdio>,
        mut stdout: Box<dyn WriteIntoStdio>,
        _stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        let back_to = state
            .pushd_stack
            .pop()
            .ok_or_else(|| anyhow!("empty stack"))?;
        state.chdir(&back_to)?;

        let beautified_stack = state
            .pushd_stack
            .iter()
            .map(beautify_path)
            .collect::<Result<Vec<_>>>()?;
        writeln!(stdout, "{}", beautified_stack.into_iter().format(" "))?;

        Ok(Exit::Success)
    }
}
