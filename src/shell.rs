use crate::cmdline::*;
use crate::ctrlc_handler::register_child_process;
use crate::line_parser::{ArgsParser, Cursor};
use anyhow::Result;
use anyhow::{anyhow, ensure};
use rustyline::Editor;
use shared_child::SharedChild;
use std::env;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
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
        let mut args = parse_cmdline(&cmdline)?;

        // if arguments are empty, do nothing.
        if args.is_empty() {
            return Ok(());
        }

        let cmd = resolve_cmd(&args.remove(0))?;
        let cmdline = CommandLine::new(cmd, args);
        self.run_cmdline(&cmdline)?;
        Ok(())
    }

    fn run_cmdline(&mut self, cmdline: &CommandLine) -> Result<()> {
        match &cmdline.cmd {
            CommandKind::Builtin(cmd) => self.execute_builtin(*cmd, &cmdline.args),
            CommandKind::External(cmd) => self.execute_external(cmd, &cmdline.args),
        }
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

    fn execute_external(&mut self, cmd: &Path, args: &[Arg]) -> Result<()> {
        let args = args.iter().map(|arg| arg.to_string());
        let child = SharedChild::spawn(Command::new(cmd).args(args))?;
        let child = Arc::new(child);

        // register Ctrl+C handler to kill the child.
        register_child_process(Arc::downgrade(&child));
        child.wait()?;

        Ok(())
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

fn parse_cmdline(cmdline: &str) -> Result<Vec<Arg>> {
    let mut cursor = Cursor::new(cmdline);
    let args = ArgsParser::new(&mut cursor).parse_args()?;
    ensure!(cursor.is_finished(), "input was not entirely read");
    Ok(args)
}

fn resolve_cmd(cmd: &Arg) -> Result<CommandKind> {
    match &*cmd.to_string() {
        "exit" => Ok(CommandKind::Builtin(BuiltinCommand::Exit)),
        "cd" => Ok(CommandKind::Builtin(BuiltinCommand::Cd)),
        cmd => which(cmd)
            .map(CommandKind::External)
            .map_err(|err| anyhow!("{}", err)),
    }
}

// builtin functions
impl Shell {
    pub fn execute_builtin(&mut self, cmd: BuiltinCommand, args: &[Arg]) -> Result<()> {
        match cmd {
            BuiltinCommand::Exit => self.builtin_exit(args),
            BuiltinCommand::Cd => self.builtin_cd(args),
        }
    }

    pub fn builtin_exit(&mut self, args: &[Arg]) -> Result<()> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(())
    }

    pub fn builtin_cd(&mut self, args: &[Arg]) -> Result<()> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0].to_string())?;
        Ok(())
    }
}
