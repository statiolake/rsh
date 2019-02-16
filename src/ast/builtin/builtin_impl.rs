use std::env;
use std::error;
use std::fmt;
use std::result;

use crate::ast::{Ast, AstError};
use crate::ShellState;

pub type Result = result::Result<bool, BuiltinError>;

#[derive(Debug)]
pub enum BuiltinError {
    UnexpectedNumberOfArgument { expected: u32, found: u32 },
    AstError(AstError),
}

impl From<AstError> for BuiltinError {
    fn from(e: AstError) -> BuiltinError {
        BuiltinError::AstError(e)
    }
}

impl fmt::Display for BuiltinError {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BuiltinError::UnexpectedNumberOfArgument { expected, found } => write!(
                b,
                "unexpected number of argument, expected {} but found {}",
                expected, found
            ),
            BuiltinError::AstError(ref e) => write!(b, "error while flattening: {}", e),
        }
    }
}

impl error::Error for BuiltinError {}

fn print_err(msg: impl fmt::Display) {
    use crate::COLOR_ERROR;
    use colored_print::color::ConsoleColor;
    use colored_print::colored_println;
    colored_println! {
        true;
        COLOR_ERROR, "error:";
        ConsoleColor::Reset, " {}", msg;
    }
}

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

pub fn cd(_: &mut ShellState, args: Vec<Ast>) -> Result {
    if args.len() != 1 {
        return Err(BuiltinError::UnexpectedNumberOfArgument {
            expected: 1,
            found: args.len() as u32,
        });
    }
    let path = args.into_iter().next().unwrap().flattened()?;
    match env::set_current_dir(path) {
        Ok(_) => Ok(true),
        Err(e) => {
            print_err(e);
            Ok(false)
        }
    }
}
