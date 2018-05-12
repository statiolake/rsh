use std::error;
use std::fmt;
use std::io;
use std::result;

/// Result type representing expression related result.
pub type Result<T> = result::Result<T, ExprError>;

/// represents errors in expressions
#[derive(Debug)]
pub struct ExprError {
    kind: ErrorKind,
    cause: Option<Box<dyn error::Error>>,
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::CmdInvokationError => {
                writeln!(f, "in invoking command")?;
                if let Some(ref e) = self.cause {
                    writeln!(f, "    caused by: {}", e)?;
                }
            }
            ErrorKind::EmptyFnCall => {
                writeln!(f, "nothing inside parenthesis, empty function call!")?;
                if let Some(ref e) = self.cause {
                    writeln!(f, "    caused by: {}", e)?;
                }
            }
        }
        Ok(())
    }
}

impl error::Error for ExprError {
    fn cause(&self) -> Option<&dyn error::Error> {
        self.cause.as_ref().map(|x| &**x as &dyn error::Error)
    }
}

impl ExprError {
    /// creates new instance of ExprError
    pub fn new(kind: ErrorKind) -> ExprError {
        ExprError {
            kind: kind,
            cause: None,
        }
    }

    /// creates new instance of ExprError, with the cause
    pub fn with_cause(kind: ErrorKind, cause: Box<dyn error::Error>) -> ExprError {
        ExprError {
            kind: kind,
            cause: Some(cause),
        }
    }
}

/// a kind of error that may appear in ExprError
#[derive(Debug)]
pub enum ErrorKind {
    EmptyFnCall,
    CmdInvokationError,
}

/// type implementing this trait can be chained to ExprError
pub trait ChainableToExprError<T> {
    fn chain_err(self, kind: ErrorKind) -> Result<T>;
}

impl<T> ChainableToExprError<T> for result::Result<T, io::Error> {
    fn chain_err(self, kind: ErrorKind) -> Result<T> {
        self.map_err(|e| ExprError::with_cause(kind, box e))
    }
}
