use crate::cmdline::*;
use crate::ctrlc_handler::register_child_handle;
use crate::line_parser::{ArgsParser, Cursor};
use anyhow::bail;
use anyhow::Result;
use anyhow::{anyhow, ensure};
use rustyline::Editor;
use std::env;
use std::fmt::Display;
use std::path::{Path, PathBuf};
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
        let args = parse_cmdline(&cmdline)?;
        self.run_args(&args)
    }

    pub fn run_args(&mut self, args: &Args) -> Result<()> {
        let mut args = args.flatten(self)?.to_vec(self);
        if args.is_empty() {
            return Ok(());
        }

        let cmd = resolve_cmd(&args.remove(0))?;
        self.execute(&cmd, &args, false).map(|_| ())
    }

    pub fn run_args_captured(&mut self, args: &Args) -> Result<String> {
        let mut args = args.flatten(self)?.to_vec(self);
        if args.is_empty() {
            bail!("the command is empty")
        }

        let cmd = resolve_cmd(&args.remove(0))?;
        self.execute(&cmd, &args, true)
            .map(|out| out.expect("captured output not found"))
    }

    pub fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
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

    fn execute(
        &mut self,
        cmd: &CommandKind,
        args: &[String],
        capture: bool,
    ) -> Result<Option<String>> {
        match cmd {
            CommandKind::Builtin(cmd) => self.execute_builtin(*cmd, args, capture),
            CommandKind::External(cmd) => self.execute_external(cmd, args, capture),
        }
    }

    fn execute_external(
        &mut self,
        cmd: &Path,
        args: &[String],
        capture: bool,
    ) -> Result<Option<String>> {
        let args = args.iter().map(|arg| arg.to_string());

        let mut cmd = duct::cmd(cmd, args).unchecked();
        if capture {
            cmd = cmd.stdout_capture();
        }

        let handle = Arc::new(cmd.start()?);

        // register Ctrl+C handler to kill the child.
        register_child_handle(Arc::downgrade(&handle));

        let output = handle.wait()?;

        if capture {
            let out = String::from_utf8_lossy(&output.stdout);
            Ok(Some(out.to_string()))
        } else {
            Ok(None)
        }
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

fn parse_cmdline(cmdline: &str) -> Result<Args> {
    let mut cursor = Cursor::new(cmdline);
    let args = ArgsParser::new(&mut cursor).parse_args()?;
    ensure!(cursor.is_finished(), "input was not entirely read");
    Ok(args)
}

fn resolve_cmd(cmd: &str) -> Result<CommandKind> {
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
    pub fn execute_builtin(
        &mut self,
        cmd: BuiltinCommand,
        args: &[String],
        capture: bool,
    ) -> Result<Option<String>> {
        match cmd {
            BuiltinCommand::Exit => self.builtin_exit(args, capture),
            BuiltinCommand::Cd => self.builtin_cd(args, capture),
        }
    }

    pub fn builtin_exit(&mut self, args: &[String], capture: bool) -> Result<Option<String>> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(empty_output(capture))
    }

    pub fn builtin_cd(&mut self, args: &[String], capture: bool) -> Result<Option<String>> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0].to_string())?;
        Ok(empty_output(capture))
    }
}

fn empty_output(capture: bool) -> Option<String> {
    if capture {
        Some(String::new())
    } else {
        None
    }
}
