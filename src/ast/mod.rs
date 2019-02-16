use std::error;
use std::fmt;
use std::io;
use std::process::{Command, Stdio};
use std::result;

use log::debug;

pub mod builtin;

use super::ShellState;

pub use self::builtin::Builtin;

pub type Result<T> = result::Result<T, AstError>;

pub trait ChainableToAstError<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T>;
}

#[derive(Debug)]
pub struct AstError {
    kind: ErrorKind,
    cause: Option<Box<dyn error::Error>>,
}

impl fmt::Display for AstError {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::EmptyFnCall => write!(b, "attempted to call empty function.")?,
            ErrorKind::CmdInvokationError => write!(b, "failed to invoke command.")?,
        }
        if let Some(ref cause) = self.cause {
            write!(b, "\n  caused by: {}", cause)?;
        }
        Ok(())
    }
}

impl error::Error for AstError {}

impl AstError {
    pub fn new(kind: ErrorKind) -> AstError {
        AstError { kind, cause: None }
    }

    pub fn with_cause(kind: ErrorKind, cause: Box<dyn error::Error>) -> AstError {
        AstError {
            kind,
            cause: Some(cause),
        }
    }
}

impl<T> ChainableToAstError<T> for io::Result<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|e| AstError::with_cause(kind, Box::new(e)))
    }
}

impl ChainableToAstError<bool> for builtin::builtin_impl::Result {
    fn chain_err(self, kind: ErrorKind) -> Result<bool> {
        self.map_err(|e| AstError::with_cause(kind, Box::new(e)))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    EmptyFnCall,
    CmdInvokationError,
}

#[derive(Debug)]
pub enum Ast {
    FnCall(Vec<Ast>),
    Literal(String),
}

#[derive(Debug)]
pub struct IR(Vec<String>);

#[derive(Debug)]
pub enum Exec {
    Command(Command),
    Builtin(Builtin),
}

impl Ast {
    pub fn flattened(self) -> Result<String> {
        match self {
            Ast::Literal(lit) => Ok(lit),
            Ast::FnCall(args) => {
                let flattened_args = child_flattened(args)?;
                let mut cmd = make_raw_command(
                    flattened_args,
                    Stdio::inherit(),
                    Stdio::piped(),
                    Stdio::inherit(),
                )?;
                let res = cmd.output().chain_err(ErrorKind::CmdInvokationError)?;
                let res_stdout = String::from_utf8_lossy(&res.stdout).into();
                Ok(res_stdout)
            }
        }
    }

    pub fn make_toplevel_command(self) -> Result<Exec> {
        let args = self.unwrap_fncall();
        match builtin::check_builtin(args)? {
            Ok(builtin) => Ok(Exec::Builtin(builtin)),
            Err(args) => {
                let args = child_flattened(args)?;
                make_raw_command(args, Stdio::inherit(), Stdio::inherit(), Stdio::inherit())
                    .map(Exec::Command)
            }
        }
    }

    #[allow(dead_code)]
    fn unwrap_literal(self) -> String {
        match self {
            Ast::Literal(s) => s,
            Ast::FnCall(_) => panic!("unwrap_literal called on FnCall value."),
        }
    }

    #[allow(dead_code)]
    fn unwrap_fncall(self) -> Vec<Ast> {
        match self {
            Ast::Literal(_) => panic!("unwrap_fncall called on Literal value."),
            Ast::FnCall(args) => args,
        }
    }
}

fn child_flattened(args: Vec<Ast>) -> Result<IR> {
    debug!("about to flatten: {:?}", args);
    args.into_iter().map(|x| x.flattened()).collect()
}

fn make_raw_command(args: IR, stdin: Stdio, stdout: Stdio, stderr: Stdio) -> Result<Command> {
    let mut args = args.into_inner().into_iter();
    let mut cmd = Command::new(
        args.next()
            .ok_or_else(|| AstError::new(ErrorKind::EmptyFnCall))?,
    );
    cmd.args(args).stdin(stdin).stdout(stdout).stderr(stderr);
    Ok(cmd)
}

impl IR {
    pub fn into_inner(self) -> Vec<String> {
        self.0
    }
}

use std::iter::FromIterator;
impl FromIterator<String> for IR {
    fn from_iter<I: IntoIterator<Item = String>>(x: I) -> IR {
        IR(x.into_iter().collect())
    }
}

impl Exec {
    pub fn run(self, state: &mut ShellState) -> Result<bool> {
        match self {
            Exec::Command(mut cmd) => cmd
                .spawn()
                .and_then(|mut c| c.wait())
                .map(|s| s.success())
                .chain_err(ErrorKind::CmdInvokationError),
            Exec::Builtin(builtin) => builtin.run(state).chain_err(ErrorKind::CmdInvokationError),
        }
    }
}
