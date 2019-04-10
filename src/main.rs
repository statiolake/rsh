mod ast;
mod builtin;
mod exec;
mod fncall;
mod parser;

use std::env;
use std::error;
use std::io;
use std::io::prelude::*;
use std::result;

use colored_print::color::ConsoleColor;
use colored_print::colored_println;
use log::debug;

use crate::parser::Parser;

pub const COLOR_ERROR: ConsoleColor = ConsoleColor::Red;
pub const COLOR_INFO: ConsoleColor = ConsoleColor::Cyan;

pub type Result<T> = result::Result<T, Box<dyn error::Error>>;

#[derive(Default)]
pub struct ShellState {
    running: bool,
}

impl ShellState {
    pub fn new() -> ShellState {
        ShellState { running: true }
    }
}

fn main() {
    env_logger::init();
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut state = ShellState::new();
    while state.running {
        if let Err(e) = run_once(&mut state, &mut stdin) {
            colored_println! {
                true;
                COLOR_ERROR, "error: ";
                ConsoleColor::Reset, "{}", e;
            }
        }
    }
}

fn run_once(state: &mut ShellState, stdin: &mut io::StdinLock) -> Result<()> {
    print!("{} $ ", env::current_dir()?.display());
    io::stdout().flush().unwrap();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    let ast = Parser::from(line.trim()).parse()?;
    debug!("parser result: {:?}", ast);
    let res = ast.run_toplevel(state)?;
    colored_println! {
        true;
        COLOR_INFO, "result:";
        ConsoleColor::Reset, " {}", res;
    }
    Ok(())
}
