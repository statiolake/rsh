mod ast;
mod builtin;
mod exec;
mod fncall;
mod parser;
mod print_macros;
mod rustyline_helper;

use crate::parser::Parser;
use crate::print_macros::{COLOR_ERROR, COLOR_INFO, COLOR_RESET};
use crate::rustyline_helper::Helper;
use log::debug;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, EditMode, Editor, OutputStreamType};
use std::env;
use std::error;
use std::result;

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
            println! {
                COLOR_ERROR => "error: ";
                COLOR_RESET => "{}", e;
            }
        }
    }
}

fn run_once(state: &mut ShellState, rle: &mut Editor<Helper>) -> Result<()> {
    let line = prompt(rle)?;
    let ast = Parser::from(line.trim()).parse()?;
    debug!("parser result: {:?}", ast);
    let res = ast.run_toplevel(state)?;
    println! {
        COLOR_INFO => "result:";
        COLOR_RESET => " {}", res;
    };

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
