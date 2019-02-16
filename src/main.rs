mod ast;
mod consts;
mod parser;

use std::error;
use std::io;
use std::io::prelude::*;
use std::result;

use colored_print::color::ConsoleColor;
use colored_print::colored_println;
use log::debug;

use crate::parser::Parser;

pub const COLOR_ERROR: ConsoleColor = ConsoleColor::Red;

pub type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
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
    let ast = Parser::from(line.trim()).parse()?;
    debug!("parser result: {:?}", ast);
    let cmd = ast.make_toplevel_command()?;
    debug!("invoke cmd: {:?}", cmd);
    println!("{:?}", cmd.run());
    Ok(())
}
