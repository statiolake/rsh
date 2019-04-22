mod ast;
mod builtin;
mod exec;
mod fncall;
mod parser;
mod rustyline_helper;

use std::env;
use std::error;
use std::result;

use colored_print::color::ConsoleColor;
use colored_print::colored_println;
use log::debug;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, EditMode, Editor, OutputStreamType};

use crate::parser::Parser;
use crate::rustyline_helper::Helper;

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

    let mut state = ShellState::new();

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();

    let mut rle = Editor::with_config(config);

    rle.set_helper(Some(Helper::new()));

    while state.running {
        if let Err(e) = run_once(&mut state, &mut rle) {
            colored_println! {
                true;
                COLOR_ERROR, "error: ";
                ConsoleColor::Reset, "{}", e;
            }
        }
    }
}

fn run_once(state: &mut ShellState, rle: &mut Editor<Helper>) -> Result<()> {
    let line = prompt(rle)?;
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

fn prompt(rle: &mut Editor<Helper>) -> Result<String> {
    let readline = rle.readline(&format!("{} $ ", env::current_dir()?.display()));

    match readline {
        Ok(line) => {
            rle.add_history_entry(&*line);
            Ok(line)
        }

        Err(ReadlineError::Interrupted) => Ok("".into()),
        Err(ReadlineError::Eof) => Ok("exit".into()),
        Err(e) => Err(Box::new(e)),
    }
}
