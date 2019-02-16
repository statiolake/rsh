use std::{error, fmt, result};

use crate::builtin::Builtin;
use crate::exec::Exec;
use crate::ShellState;

pub type Result<T> = result::Result<T, AstError>;

pub trait ChainableToAstError<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T>;
}

#[derive(Debug)]
pub struct AstError {
    kind: ErrorKind,
    cause: Option<Box<dyn error::Error>>,
}

#[derive(Debug)]
pub enum ErrorKind {
    RunLiteralError,
    BuiltinDeterminationError,
    BuiltinExecutionError,
    ExternalExecutionError,
}

#[derive(Debug)]
pub struct FnCall(pub Box<Ast>, pub Vec<Ast>);

#[derive(Debug)]
pub enum Ast {
    FnCall(FnCall),
    Literal(String),
}

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

impl fmt::Display for AstError {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::RunLiteralError => {
                write!(b, "attempted to execute an literal, not fncall.")?
            }
            ErrorKind::BuiltinDeterminationError => write!(
                b,
                "failed to determine whether the command is builtin or external."
            )?,
            ErrorKind::BuiltinExecutionError => write!(b, "failed to run builtin command.")?,
            ErrorKind::ExternalExecutionError => write!(b, "failed to execute external command.")?,
        }
        if let Some(ref cause) = self.cause {
            write!(b, "\n  caused by: {}", cause)?;
        }
        Ok(())
    }
}

impl error::Error for AstError {}

impl<T, E: error::Error + 'static> ChainableToAstError<T> for result::Result<T, E> {
    fn chain_err(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|e| AstError::with_cause(kind, Box::new(e)))
    }
}

impl Ast {
    pub fn run_toplevel(self, state: &mut ShellState) -> Result<bool> {
        match self {
            Ast::Literal(_) => Err(AstError::new(ErrorKind::RunLiteralError)),
            Ast::FnCall(fncall) => fncall.run_toplevel(state),
        }
    }

    pub fn run(self, state: &mut ShellState) -> Result<String> {
        match self {
            Ast::Literal(lit) => Ok(lit),
            Ast::FnCall(fncall) => fncall.run(state),
        }
    }
}

impl FnCall {
    pub fn run_toplevel(self, state: &mut ShellState) -> Result<bool> {
        match dispatch(self, state)? {
            CallKind::Builtin(builtin) => builtin
                .run_toplevel(state)
                .chain_err(ErrorKind::BuiltinExecutionError),
            CallKind::Exec(exec) => exec
                .run_toplevel(state)
                .chain_err(ErrorKind::ExternalExecutionError),
        }
    }

    pub fn run(self, state: &mut ShellState) -> Result<String> {
        match dispatch(self, state)? {
            CallKind::Builtin(builtin) => builtin
                .run(state)
                .chain_err(ErrorKind::BuiltinExecutionError),
            CallKind::Exec(exec) => exec.run(state).chain_err(ErrorKind::ExternalExecutionError),
        }
    }
}

#[derive(Debug)]
pub enum CallKind {
    Builtin(Builtin),
    Exec(Exec),
}

fn dispatch(fncall: FnCall, state: &mut ShellState) -> Result<CallKind> {
    match Builtin::try_from_fncall(fncall, state).chain_err(ErrorKind::BuiltinDeterminationError)? {
        Ok(builtin) => Ok(CallKind::Builtin(builtin)),
        Err(fncall) => Ok(CallKind::Exec(Exec::from(fncall))),
    }
}
