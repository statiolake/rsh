use std::error;
use std::fmt;
use std::io;
use std::process::{Command, Stdio};
use std::result;

use log::debug;

pub type Result<T> = result::Result<T, ExprError>;

pub trait ErrorChainToExprError<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T>;
}

#[derive(Debug)]
pub struct ExprError {
    kind: ErrorKind,
    cause: Option<Box<dyn error::Error>>,
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self)
    }
}

impl error::Error for ExprError {
    fn cause(&self) -> Option<&dyn error::Error> {
        self.cause.as_ref().map(|x| &**x as &dyn error::Error)
    }
}

impl ExprError {
    pub fn new(kind: ErrorKind) -> ExprError {
        ExprError {
            kind: kind,
            cause: None,
        }
    }

    pub fn with_cause(kind: ErrorKind, cause: Box<dyn error::Error>) -> ExprError {
        ExprError {
            kind: kind,
            cause: Some(cause),
        }
    }
}

impl<T> ErrorChainToExprError<T> for result::Result<T, io::Error> {
    fn chain_err(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|e| ExprError::with_cause(kind, Box::new(e)))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    EmptyFnCall,
    CmdInvokationError,
}

#[derive(Debug)]
pub enum Expr {
    FnCall(Vec<Expr>),
    Literal(String),
}

impl Expr {
    pub fn child_flattened(self) -> Result<Expr> {
        debug!("about to flatten: {:?}", self);
        match self {
            Expr::Literal(_) => Ok(self),
            Expr::FnCall(args) => {
                let flattened = Expr::FnCall(
                    args.into_iter()
                        .map(|x| x.child_flattened())
                        .collect::<Result<_>>()?,
                );
                let mut cmd =
                    flattened.make_command(Stdio::inherit(), Stdio::piped(), Stdio::inherit())?;
                let res = cmd.output().chain_err(ErrorKind::CmdInvokationError)?;
                let res_stdout = String::from_utf8_lossy(&res.stdout).into();
                Ok(Expr::Literal(res_stdout))
            }
        }
    }

    pub fn make_toplevel_command(self) -> Result<Command> {
        self.make_command(Stdio::inherit(), Stdio::inherit(), Stdio::inherit())
    }

    fn unwrap_literal(self) -> String {
        match self {
            Expr::Literal(s) => s,
            Expr::FnCall(_) => panic!("unwrap_literal called on FnCall value."),
        }
    }

    fn unwrap_fncall(self) -> Vec<Expr> {
        match self {
            Expr::Literal(_) => panic!("unwrap_fncall called on Literal value."),
            Expr::FnCall(args) => args,
        }
    }

    fn make_command(self, stdin: Stdio, stdout: Stdio, stderr: Stdio) -> Result<Command> {
        let mut args = self
            .unwrap_fncall()
            .into_iter()
            .map(|x| x.child_flattened().map(|x| x.unwrap_literal()))
            .collect::<Result<Vec<_>>>()?
            .into_iter();
        let mut cmd = Command::new(args.next().ok_or(ExprError::new(ErrorKind::EmptyFnCall))?);
        cmd.args(args).stdin(stdin).stdout(stdout).stderr(stderr);
        Ok(cmd)
    }
}
