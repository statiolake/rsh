use std::error;
use std::fmt;
use std::result;

use crate::ast::Ast;
use crate::ShellState;

pub type Result = result::Result<bool, BuiltinError>;

#[derive(Debug)]
pub enum BuiltinError {
    UnexpectedNumberOfArgument { expected: u32, found: u32 },
}

impl fmt::Display for BuiltinError {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BuiltinError::UnexpectedNumberOfArgument { expected, found } => write!(
                b,
                "unexpected number of argument, expected {} but found {}",
                expected, found
            ),
        }
    }
}

impl error::Error for BuiltinError {}

pub fn exit(state: &mut ShellState, args: Vec<Ast>) -> Result {
    if !args.is_empty() {
        return Err(BuiltinError::UnexpectedNumberOfArgument {
            expected: 0,
            found: args.len() as u32,
        });
    }
    state.running = false;
    Ok(true)
}
