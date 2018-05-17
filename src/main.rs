#![feature(box_syntax)]

#[macro_use]
extern crate colored_print;
#[macro_use]
extern crate log;
extern crate env_logger;

mod builtin;
mod consts;
mod expr;
mod parser;

use std::error;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::result;

use colored_print::color::ConsoleColor;

use parser::Parser;

pub const COLOR_ERROR: ConsoleColor = ConsoleColor::Red;

pub type Result<T> = result::Result<T, RshError>;

pub trait ErrorChainToRshError<T> {
    fn chain_err(self) -> Result<T>;
}

#[derive(Debug)]
pub enum RshError {
    ExprError(expr::ExprError),
    ParseError(parser::ParseError),
}

impl fmt::Display for RshError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RshError::ExprError(ref e) => writeln!(f, "in expression: {}", e),
            RshError::ParseError(ref e) => writeln!(f, "parsing expression: {}", e),
        }
    }
}

impl<T> ErrorChainToRshError<T> for expr::Result<T> {
    fn chain_err(self) -> Result<T> {
        self.map_err(|e| RshError::ExprError(e))
    }
}

impl<T> ErrorChainToRshError<T> for parser::Result<T> {
    fn chain_err(self) -> Result<T> {
        self.map_err(|e| RshError::ParseError(e))
    }
}

impl error::Error for RshError {
    fn cause(&self) -> Option<&dyn error::Error> {
        match *self {
            RshError::ExprError(ref e) => Some(e),
            RshError::ParseError(ref e) => Some(e),
        }
    }
}

fn main() {
    env_logger::init();
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    loop {
        if let Err(e) = run_once(&mut stdin) {
            colored_println! {
                true;
                COLOR_ERROR, "error: ";
                ConsoleColor::Reset, "{}", e;
            }
        }
    }
}

fn run_once(stdin: &mut io::StdinLock) -> Result<()> {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    let expr = Parser::from(line.trim()).parse().chain_err()?;
    debug!("parser result: {:?}", expr);
    let cmd = expr.make_toplevel_runnable().chain_err()?;
    debug!("invoke cmd: {:?}", cmd);
    println!("{:?}", cmd.run());
    Ok(())
}
