use crate::cmdline::*;
use crate::ctrlc_handler::register_child;
use crate::line_parser::{ArgsParser, Cursor};
use anyhow::Result;
use anyhow::{anyhow, ensure};
use os_pipe::{PipeReader, PipeWriter};
use rustyline::Editor;
use shared_child::SharedChild;
use std::env;
use std::fmt::Display;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::thread;
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
        self.run_args_inherit(&args)
    }

    pub fn run_args_inherit(&mut self, args: &Args) -> Result<()> {
        let stdin = os_pipe::dup_stdin()?;
        let stdout = os_pipe::dup_stdout()?;
        self.run_args_pipe(stdin, stdout, args)
    }

    pub fn run_args_capture(&mut self, args: &Args) -> Result<String> {
        let stdin = os_pipe::dup_stdin()?;
        let (mut reader, stdout) = os_pipe::pipe()?;
        let th = thread::spawn(move || {
            let mut out = String::new();
            let _ = reader.read_to_string(&mut out);
            out
        });

        self.run_args_pipe(stdin, stdout, &args)?;
        let out = th
            .join()
            .map_err(|_| anyhow!("failed to read child stdout"))?;

        Ok(out)
    }

    pub fn run_args_pipe(
        &mut self,
        stdin: PipeReader,
        stdout: PipeWriter,
        args: &Args,
    ) -> Result<()> {
        let mut args = args.flatten(self)?.to_vec(self);
        if args.is_empty() {
            return Ok(());
        }

        let cmd = resolve_cmd(&args.remove(0))?;
        self.execute(stdin, stdout, &cmd, &args)
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
        stdin: PipeReader,
        stdout: PipeWriter,
        cmd: &CommandKind,
        args: &[String],
    ) -> Result<()> {
        match cmd {
            CommandKind::Builtin(cmd) => self.execute_builtin(stdin, stdout, *cmd, args),
            CommandKind::External(cmd) => self.execute_external(stdin, stdout, cmd, args),
        }
    }

    fn execute_external(
        &mut self,
        stdin: PipeReader,
        stdout: PipeWriter,
        cmd: &Path,
        args: &[String],
    ) -> Result<()> {
        let args = args.iter().map(|arg| arg.to_string());
        let mut cmd = Command::new(cmd);
        cmd.args(args).stdin(stdin).stdout(stdout);
        let child = Arc::new(SharedChild::spawn(&mut cmd)?);

        // register Ctrl+C handler to kill the child.
        register_child(Arc::downgrade(&child));

        // TODO: retrieve exit status
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
            .map_err(|err| anyhow!("{}: {}", cmd, err)),
    }
}

// builtin functions
impl Shell {
    pub fn execute_builtin(
        &mut self,
        stdin: PipeReader,
        stdout: PipeWriter,
        cmd: BuiltinCommand,
        args: &[String],
    ) -> Result<()> {
        match cmd {
            BuiltinCommand::Exit => self.builtin_exit(stdin, stdout, args),
            BuiltinCommand::Cd => self.builtin_cd(stdin, stdout, args),
        }
    }

    pub fn builtin_exit(
        &mut self,
        _stdin: PipeReader,
        _stdout: PipeWriter,
        args: &[String],
    ) -> Result<()> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(())
    }

    pub fn builtin_cd(
        &mut self,
        _stdin: PipeReader,
        _stdout: PipeWriter,
        args: &[String],
    ) -> Result<()> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0].to_string())?;
        Ok(())
    }
}
