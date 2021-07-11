use crate::line_parser::LineParser;
use anyhow::Result;
use anyhow::{anyhow, ensure};
use std::env;
use std::fmt::Display;
use std::path::PathBuf;
use std::process::Command;
use which::which;

pub struct Shell {
    loop_running: bool,
}

impl Shell {
    pub fn new() -> Shell {
        Shell {
            loop_running: false,
        }
    }

    pub fn mainloop(&mut self) {
        self.loop_running = true;
        while self.loop_running {
            if let Err(e) = self.read_and_run() {
                print_error(e);
            }
        }
    }

    pub fn read_and_run(&mut self) -> Result<()> {
        let cmdline = read_line()?;
        let args = parse_cmdline(&cmdline)?;
        self.run_args(args)?;
        Ok(())
    }

    fn run_args(&mut self, mut args: Vec<String>) -> Result<()> {
        // if empty, do nothing.
        if args.is_empty() {
            return Ok(());
        }

        let cmd = args.remove(0);

        // handle builtins
        if let Some(res) = self.handle_builtins(&cmd, &args) {
            return res;
        }

        let cmd = resolve_cmd(&cmd).map_err(|e| anyhow!("{}: {}", cmd, e))?;
        let mut child = Command::new(cmd).args(&args).spawn()?;
        child.wait()?;
        Ok(())
    }
}

fn print_error<D: Display>(err: D) {
    eprintln!("rsh: {}", err);
}

fn read_line() -> Result<String> {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;

    let mut rl = Editor::<()>::new();
    match rl.readline(&format!("{} $ ", env::current_dir()?.display())) {
        Err(ReadlineError::Eof) => Ok("exit".to_string()),
        res => res.map_err(Into::into),
    }
}

fn parse_cmdline(cmdline: &str) -> Result<Vec<String>> {
    LineParser::new(cmdline).parse()
}

fn resolve_cmd(cmd: &str) -> Result<PathBuf> {
    which(cmd).map_err(|err| anyhow!("{}", err))
}

// builtin functions
impl Shell {
    pub fn handle_builtins(&mut self, cmd: &str, args: &[String]) -> Option<Result<()>> {
        match cmd {
            "exit" => Some(self.builtin_exit(args)),
            "cd" => Some(self.builtin_cd(args)),
            _ => None,
        }
    }

    pub fn builtin_exit(&mut self, args: &[String]) -> Result<()> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(())
    }

    pub fn builtin_cd(&mut self, args: &[String]) -> Result<()> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0])?;
        Ok(())
    }
}
