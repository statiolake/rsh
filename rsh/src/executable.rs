pub mod builtin;

use crate::{ctrlc_handler::register_child, shell::ShellState};
use anyhow::anyhow;
use anyhow::Result;
use os_pipe::PipeWriter;
use shared_child::SharedChild;
use std::{
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
};
use which::which;

pub trait ReadIntoStdio: Read + Send + Sync + 'static {
    fn box_into_stdio(self: Box<Self>) -> Stdio;
}

pub trait WriteIntoStdio: Write + Send + Sync + 'static {
    fn try_clone(&self) -> Result<Box<dyn WriteIntoStdio>>;
    fn box_into_stdio(self: Box<Self>) -> Stdio;
}

impl<T: Read + Into<Stdio> + Send + Sync + 'static> ReadIntoStdio for T {
    fn box_into_stdio(self: Box<Self>) -> Stdio {
        (*self).into()
    }
}

impl<T: Write + TryClone + Into<Stdio> + Send + Sync + 'static> WriteIntoStdio for T {
    fn try_clone(&self) -> Result<Box<dyn WriteIntoStdio>> {
        (*self).try_clone().map(|cloned| cloned as _)
    }

    fn box_into_stdio(self: Box<Self>) -> Stdio {
        (*self).into()
    }
}

impl From<Box<dyn ReadIntoStdio>> for Stdio {
    fn from(boxed: Box<dyn ReadIntoStdio>) -> Self {
        boxed.box_into_stdio()
    }
}

impl From<Box<dyn WriteIntoStdio>> for Stdio {
    fn from(boxed: Box<dyn WriteIntoStdio>) -> Self {
        boxed.box_into_stdio()
    }
}

pub trait TryClone {
    fn try_clone(&self) -> Result<Box<Self>>;
}

impl TryClone for File {
    fn try_clone(&self) -> Result<Box<Self>> {
        self.try_clone()
            .map_err(Into::into)
            .map(|cloned| Box::new(cloned as _))
    }
}

impl TryClone for PipeWriter {
    fn try_clone(&self) -> Result<Box<Self>> {
        self.try_clone()
            .map_err(Into::into)
            .map(|cloned| Box::new(cloned as _))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Exit {
    Success,
    Failure,
}

pub trait Executable: Send + Sync + 'static {
    fn executable(&self) -> Option<&Path>;

    fn execute(
        &mut self,
        state: &mut ShellState,
        stdin: Box<dyn ReadIntoStdio>,
        stdout: Box<dyn WriteIntoStdio>,
        stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit>;
}

#[derive(Debug, Clone)]
pub struct ExternalCommand {
    cmd: PathBuf,
    args: Vec<String>,
}

impl ExternalCommand {
    pub fn new(cmd: PathBuf, args: Vec<String>) -> Self {
        Self { cmd, args }
    }
}

impl Executable for ExternalCommand {
    fn executable(&self) -> Option<&Path> {
        Some(&self.cmd)
    }

    fn execute(
        &mut self,
        _state: &mut ShellState,
        stdin: Box<dyn ReadIntoStdio>,
        stdout: Box<dyn WriteIntoStdio>,
        stderr: Box<dyn WriteIntoStdio>,
    ) -> Result<Exit> {
        let mut cmd = Command::new(&self.cmd);
        cmd.args(&self.args)
            .stdin(Stdio::from(stdin))
            .stdout(Stdio::from(stdout))
            .stderr(Stdio::from(stderr));

        let child = Arc::new(SharedChild::spawn(&mut cmd)?);

        // register Ctrl+C handler to kill the child.
        register_child(Arc::downgrade(&child));

        // TODO: retrieve exit status
        child.wait()?;

        Ok(Exit::Success)
    }
}

pub fn resolve_executable(cmd: String, args: Vec<String>) -> Result<Box<dyn Executable>> {
    if let Some(builtin) = self::builtin::find(&cmd, args.clone()) {
        return Ok(builtin);
    }

    let cmd = which(&cmd).map_err(|err| anyhow!("{}: {}", cmd, err))?;
    Ok(Box::new(ExternalCommand::new(cmd, args)))
}
