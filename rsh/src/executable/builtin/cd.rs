use std::path::PathBuf;

use crate::{
    executable::{Executable, Exit, ReadIntoStdio, WriteIntoStdio},
    shell::ShellState,
};
use anyhow::{anyhow, Result};

#[derive(Debug)]
pub struct CmdCd(Vec<String>);

impl CmdCd {
    pub fn new(args: Vec<String>) -> Self {
        CmdCd(args)
    }
}

impl Executable for CmdCd {
    fn executable(&self) -> Option<&std::path::Path> {
        None
    }

    fn execute(
        &mut self,
        state: &mut ShellState,
        _stdin: Box<dyn ReadIntoStdio>,
        _stdout: Box<dyn WriteIntoStdio>,
        _stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        let target = match self.0.get(0).map(|s| &**s) {
            Some("--") => PathBuf::from(self.0.get(1).ok_or_else(|| anyhow!("empty argument"))?),
            Some("-") => state
                .last_working_dir
                .take()
                .ok_or_else(|| anyhow!("no previous working directory"))?,
            Some(target) => PathBuf::from(target),
            None => dirs::home_dir().ok_or_else(|| anyhow!("failed to get home directory"))?,
        };

        state.chdir(target)?;

        Ok(Exit::Success)
    }
}
