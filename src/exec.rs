use std::process::{Command, Stdio};
use std::{error, fmt, result};

use crate::ast::FnCall;
use crate::ShellState;

pub type Result<T> = result::Result<T, ExecError>;

pub trait ChainableToExecError<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T>;
}

#[derive(Debug)]
pub struct ExecError {
    kind: ErrorKind,
    cause: Option<Box<dyn error::Error>>,
}

#[derive(Debug)]
pub enum ErrorKind {
    CmdInvokationError,
}

#[derive(Debug)]
pub struct Exec(FnCall);

impl Exec {
    pub fn run_toplevel(self, state: &mut ShellState) -> Result<bool> {
        let (cmd, args) = flatten(self, state)?;
        let mut cmd = make_raw_command(
            cmd,
            args,
            Stdio::inherit(),
            Stdio::inherit(),
            Stdio::inherit(),
        );

        cmd.spawn()
            .and_then(|mut c| c.wait())
            .map(|s| s.success())
            .chain_err(ErrorKind::CmdInvokationError)
    }

    pub fn run(self, state: &mut ShellState) -> Result<String> {
        let (cmd, args) = flatten(self, state)?;
        let mut cmd = make_raw_command(
            cmd,
            args,
            Stdio::inherit(),
            Stdio::piped(),
            Stdio::inherit(),
        );

        let res = cmd.output().chain_err(ErrorKind::CmdInvokationError)?;
        let res_str = String::from_utf8_lossy(&res.stdout);
        Ok(res_str.into_owned())
    }
}

impl ExecError {
    #[allow(dead_code)]
    pub fn new(kind: ErrorKind) -> ExecError {
        ExecError { kind, cause: None }
    }

    pub fn with_cause(kind: ErrorKind, cause: Box<dyn error::Error>) -> ExecError {
        ExecError {
            kind,
            cause: Some(cause),
        }
    }
}

impl From<FnCall> for Exec {
    fn from(fncall: FnCall) -> Exec {
        Exec(fncall)
    }
}

impl fmt::Display for ExecError {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "command execution failed.")?;
        if let Some(ref cause) = self.cause {
            write!(b, "\n  caused by: {}", cause)?;
        }
        Ok(())
    }
}

impl error::Error for ExecError {}

impl<T, E: error::Error + 'static> ChainableToExecError<T> for result::Result<T, E> {
    fn chain_err(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|e| ExecError::with_cause(kind, Box::new(e)))
    }
}

fn make_raw_command(
    cmd: String,
    args: Vec<String>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> Command {
    let mut cmd = Command::new(cmd);
    cmd.args(args).stdin(stdin).stdout(stdout).stderr(stderr);
    cmd
}

fn flatten(Exec(FnCall(cmd, args)): Exec, state: &mut ShellState) -> Result<(String, Vec<String>)> {
    let cmd = cmd.run(state).chain_err(ErrorKind::CmdInvokationError)?;
    let args: Vec<_> = args
        .into_iter()
        .map(|arg| arg.run(state).chain_err(ErrorKind::CmdInvokationError))
        .collect::<Result<_>>()?;
    Ok((cmd, args))
}
