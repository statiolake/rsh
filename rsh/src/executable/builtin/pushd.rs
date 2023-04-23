use std::{env::current_dir, path::PathBuf};

use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
    view::beautify_path,
};
use anyhow::{anyhow, Result};
use itertools::Itertools;

#[derive(Debug)]
pub struct CmdPushd(Vec<String>);

impl CmdPushd {
    pub fn new(args: Vec<String>) -> Self {
        CmdPushd(args)
    }
}

impl Executable for CmdPushd {
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
        let target = match self.0.get(0).map(|s| &**s) {
            Some("--") => Some(PathBuf::from(
                self.0.get(1).ok_or_else(|| anyhow!("empty argument"))?,
            )),
            Some(target) => Some(PathBuf::from(target)),
            None => None,
        };

        let current = dunce::canonicalize(current_dir()?)?;
        if state.pushd_stack.last() != Some(&current) {
            state.pushd_stack.push(current);
        }

        if let Some(target) = target {
            state.chdir(target)?;
        }

        let beautified_stack = state
            .pushd_stack
            .iter()
            .map(beautify_path)
            .collect::<Result<Vec<_>>>()?;
        writeln!(stdout, "{}", beautified_stack.into_iter().format(" "))?;

        Ok(Exit::Success)
    }
}
