use crate::line_parser::LineParser;
use anyhow::Result;
use anyhow::{anyhow, ensure};
use rustyline::Editor;
use std::env;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::process::Command;
use which::which;

const HISTORY_FILE: &str = ".rsh_history";

pub struct Shell {
    loop_running: bool,
    rl: Editor<()>,
}

impl Shell {
    pub fn new() -> Result<Shell> {
        let mut rl = Editor::new();
        let _ = rl.load_history(&history_path()?);

        Ok(Shell {
            loop_running: false,
            rl,
        })
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
        let cmdline = self.read_line()?;
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

    fn read_line(&mut self) -> Result<String> {
        use rustyline::error::ReadlineError;
        let prompt = format!("{} $ ", env::current_dir()?.display());
        let line = match self.rl.readline(&prompt) {
            Err(ReadlineError::Eof) => "exit".to_string(),
            res => res?,
        };
        self.rl.add_history_entry(&line);
        let _ = self
            .rl
            .save_history(&history_path().expect("must be checked before"));
        Ok(line)
    }
}

fn path_under_home(path: &Path) -> Result<PathBuf> {
    let home_dir =
        dirs::home_dir().ok_or_else(|| anyhow!("failed to determine user home directory"))?;
    Ok(home_dir.join(path))
}

fn history_path() -> Result<PathBuf> {
    path_under_home(Path::new(HISTORY_FILE))
}

fn print_error<D: Display>(err: D) {
    eprintln!("rsh: {}", err);
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
