use super::{Executable, Exit, ReadIntoStdio, WriteIntoStdio};
use crate::shell::ShellState;
use anyhow::{anyhow, Result};
use std::{
    env::{current_dir, set_current_dir, set_var},
    path::PathBuf,
};
use which::which;

pub fn find(cmd: &str, args: Vec<String>) -> Option<Box<dyn Executable>> {
    match cmd {
        "exit" => Some(Box::new(CmdExit(args))),
        "cd" => Some(Box::new(CmdCd(args))),
        "which" => Some(Box::new(CmdWhich(args))),
        "set" => Some(Box::new(CmdSet(args))),
        _ => None,
    }
}

#[derive(Debug)]
pub struct CmdExit(Vec<String>);

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

#[derive(Debug)]
pub struct CmdCd(Vec<String>);

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

        state.last_working_dir = Some(current_dir()?);
        set_current_dir(target)?;

        Ok(Exit::Success)
    }
}

#[derive(Debug)]
pub struct CmdWhich(Vec<String>);

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

#[derive(Debug)]
pub struct CmdSet(Vec<String>);

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
