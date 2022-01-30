use crate::cmdline::*;
use crate::ctrlc_handler::register_child;
use crate::line_parser::{ArgsCompositionParser, Cursor};
use anyhow::Result;
use anyhow::{anyhow, ensure};
use os_pipe::PipeWriter;
use rsh_line_editor::{LineEditor, PromptWriter, UserInput};
use shared_child::SharedChild;
use std::env;
use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;
use std::thread;
use which::which;

const HISTORY_FILE: &str = ".rsh_history";

#[derive(Debug)]
pub struct Shell {
    rle: LineEditor,
    state: ShellState,
}

#[derive(Debug, Clone)]
pub struct ShellState {
    loop_running: bool,
}

impl Shell {
    pub fn new() -> Result<Self> {
        let rle = LineEditor::new();
        let state = ShellState::new()?;
        Ok(Self { rle, state })
    }

    pub fn mainloop(&mut self) {
        self.state.loop_running = true;
        while self.state.loop_running {
            if let Err(e) = self.read_and_run() {
                print_error(e);
            }
        }
    }

    pub fn read_and_run(&mut self) -> Result<()> {
        let cmdline = self.read_line()?;
        let composition = parse_cmdline(&cmdline)?;
        self.state.run_compsition_inherit(composition)
    }

    fn read_line(&mut self) -> Result<String> {
        let input = self.rle.read_line(Prompt);

        let line = match input? {
            UserInput::String(line) => line,
            UserInput::EOF => "exit".to_string(),
        };

        Ok(line)
    }
}

pub trait TryClone {
    fn try_clone(&self) -> Result<Box<Self>>;
}

impl TryClone for File {
    fn try_clone(&self) -> Result<Box<Self>> {
        self.try_clone()
            .map_err(Into::into)
            .map(|cloned| Box::new(cloned as _))
    }
}

impl TryClone for PipeWriter {
    fn try_clone(&self) -> Result<Box<Self>> {
        self.try_clone()
            .map_err(Into::into)
            .map(|cloned| Box::new(cloned as _))
    }
}

impl ShellState {
    pub fn new() -> Result<ShellState> {
        Ok(Self {
            loop_running: false,
        })
    }

    pub fn run_compsition_inherit(&mut self, composition: ArgsComposition) -> Result<()> {
        let stdin = os_pipe::dup_stdin()?;
        let stdout = os_pipe::dup_stdout()?;
        let stderr = os_pipe::dup_stderr()?;
        self.run_composition_pipe(stdin, stdout, stderr, composition)
    }

    pub fn run_composition_capture(&mut self, composition: ArgsComposition) -> Result<String> {
        let stdin = os_pipe::dup_stdin()?;
        let (mut reader, stdout) = os_pipe::pipe()?;
        let stderr = os_pipe::dup_stderr()?;
        let th = thread::spawn(move || -> Result<String> {
            let mut out = String::new();
            reader.read_to_string(&mut out)?;
            Ok(out)
        });

        self.run_composition_pipe(stdin, stdout, stderr, composition)?;
        let out = th
            .join()
            .map_err(|_| anyhow!("failed to join the thread reading child stdout"))??;

        Ok(out)
    }

    pub fn run_composition_pipe<R, WO, WE>(
        &mut self,
        stdin: R,
        stdout: WO,
        stderr: WE,
        composition: ArgsComposition,
    ) -> Result<()>
    where
        R: Read + Into<Stdio> + Send + Sync + 'static,
        WO: Write + TryClone + Into<Stdio> + Send + Sync + 'static,
        WE: Write + TryClone + Into<Stdio> + Send + Sync + 'static,
    {
        trait ReadIntoStdio: Read + Send + Sync + 'static {
            fn box_into_stdio(self: Box<Self>) -> Stdio;
        }

        trait WriteIntoStdio: Write + Send + Sync + 'static {
            fn try_clone(&self) -> Result<Box<dyn WriteIntoStdio>>;
            fn box_into_stdio(self: Box<Self>) -> Stdio;
        }

        impl<T: Read + Into<Stdio> + Send + Sync + 'static> ReadIntoStdio for T {
            fn box_into_stdio(self: Box<Self>) -> Stdio {
                (*self).into()
            }
        }

        impl<T: Write + TryClone + Into<Stdio> + Send + Sync + 'static> WriteIntoStdio for T {
            fn try_clone(&self) -> Result<Box<dyn WriteIntoStdio>> {
                (*self).try_clone().map(|cloned| cloned as _)
            }

            fn box_into_stdio(self: Box<Self>) -> Stdio {
                (*self).into()
            }
        }

        impl From<Box<dyn ReadIntoStdio>> for Stdio {
            fn from(boxed: Box<dyn ReadIntoStdio>) -> Self {
                boxed.box_into_stdio()
            }
        }

        impl From<Box<dyn WriteIntoStdio>> for Stdio {
            fn from(boxed: Box<dyn WriteIntoStdio>) -> Self {
                boxed.box_into_stdio()
            }
        }

        let mut handles = vec![];

        let mut stdin = Some(Box::new(stdin) as Box<dyn ReadIntoStdio>);
        for (args, dest_stdout, dest_stderr) in composition.composition {
            let curr_stdin = stdin.take().expect("stdin is already taken");
            let (is_inherit, next_stdin, curr_stdout): (
                bool,
                Option<Box<dyn ReadIntoStdio>>,
                Box<dyn WriteIntoStdio>,
            ) = match dest_stdout {
                StdoutDestination::Inherit => (true, None, stdout.try_clone()? as _),
                StdoutDestination::PipeToNext => {
                    let (next_stdin, stdout) = os_pipe::pipe()?;
                    (false, Some(Box::new(next_stdin)), Box::new(stdout) as _)
                }
                StdoutDestination::File(path) => (false, None, Box::new(File::create(path)?) as _),
            };
            let curr_stderr: Box<dyn WriteIntoStdio> = match dest_stderr {
                StderrDestination::Inherit => stderr.try_clone()? as _,
                StderrDestination::Stdout => curr_stdout.try_clone()? as _,
                StderrDestination::File(path) => Box::new(File::create(path)?) as _,
            };

            let mut cloned = self.clone();
            let handle = thread::spawn(move || -> Result<Option<ShellState>> {
                cloned.run_args_pipe(curr_stdin, curr_stdout, curr_stderr, args)?;
                Ok(Some(cloned).filter(|_| is_inherit))
            });
            handles.push(handle);

            stdin = next_stdin.map(|i| i as _);
        }

        let mut replaced = false;
        for handle in handles {
            let state = handle
                .join()
                .map_err(|_| anyhow!("failed to join threads for running piped commands"))??;

            // Update the state to match the last command's result. This must occur only once.
            if let Some(state) = state {
                if replaced {
                    panic!("replacing the state more then once");
                }
                replaced = true;
                let _ = std::mem::replace(self, state);
            }
        }

        Ok(())
    }

    pub fn run_args_pipe<R, WO, WE>(
        &mut self,
        stdin: R,
        stdout: WO,
        stderr: WE,
        args: Args,
    ) -> Result<()>
    where
        R: Read + Into<Stdio>,
        WO: Write + Into<Stdio>,
        WE: Write + Into<Stdio>,
    {
        let mut args = args.flatten(self)?.to_vec(self);
        if args.is_empty() {
            return Ok(());
        }

        let cmd = resolve_cmd(&args.remove(0))?;
        self.execute(stdin, stdout, stderr, &cmd, &args)
    }

    pub fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
    }

    fn execute<R, WO, WE>(
        &mut self,
        stdin: R,
        stdout: WO,
        stderr: WE,
        cmd: &CommandKind,
        args: &[String],
    ) -> Result<()>
    where
        R: Read + Into<Stdio>,
        WO: Write + Into<Stdio>,
        WE: Write + Into<Stdio>,
    {
        match cmd {
            CommandKind::Builtin(cmd) => self.execute_builtin(stdin, stdout, stderr, *cmd, args),
            CommandKind::External(cmd) => self.execute_external(stdin, stdout, stderr, cmd, args),
        }
    }

    fn execute_external<R, WO, WE>(
        &mut self,
        stdin: R,
        stdout: WO,
        stderr: WE,
        cmd: &Path,
        args: &[String],
    ) -> Result<()>
    where
        R: Into<Stdio>,
        WO: Into<Stdio>,
        WE: Into<Stdio>,
    {
        let args = args.iter().map(|arg| arg.to_string());
        let mut cmd = Command::new(cmd);
        cmd.args(args).stdin(stdin).stdout(stdout).stderr(stderr);
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

fn parse_cmdline(cmdline: &str) -> Result<ArgsComposition> {
    let mut cursor = Cursor::new(cmdline);
    let composition = ArgsCompositionParser::new(&mut cursor).parse()?;
    assert!(
        cursor.is_finished(),
        "input was not entirely read: left `{}`",
        cursor.left()
    );
    Ok(composition)
}

fn resolve_cmd(cmd: &str) -> Result<CommandKind> {
    match &*cmd.to_string() {
        "exit" => Ok(CommandKind::Builtin(BuiltinCommand::Exit)),
        "cd" => Ok(CommandKind::Builtin(BuiltinCommand::Cd)),
        "which" => Ok(CommandKind::Builtin(BuiltinCommand::Which)),
        cmd => which(cmd)
            .map(CommandKind::External)
            .map_err(|err| anyhow!("{}: {}", cmd, err)),
    }
}

struct Prompt;
impl PromptWriter for Prompt {
    fn write<W: Write>(&mut self, out: &mut W) -> anyhow::Result<()> {
        use crossterm::style::Stylize;
        if cfg!(debug_assertions) {
            write!(out, "{} ", "(debug)".dark_grey())?;
        }

        let now = chrono::Local::now();
        let time = now.format("%H:%M:%S");
        let username = whoami::username();
        let computername = whoami::hostname();
        let path = env::current_dir()?;
        // FIXME: change face according to the previous exit status
        let face = if true { "('-')/" } else { "(-_-)/" };

        write!(
            out,
            "{time} {whoami}:{path}\n{face} > ",
            time = time,
            whoami = format!("{}@{}", username, computername).green(),
            path = path.display().to_string().blue(),
            face = face
        )?;
        out.flush()?;

        Ok(())
    }
}

// builtin functions
impl ShellState {
    pub fn execute_builtin<R, WO, WE>(
        &mut self,
        stdin: R,
        stdout: WO,
        stderr: WE,
        cmd: BuiltinCommand,
        args: &[String],
    ) -> Result<()>
    where
        R: Read,
        WO: Write,
        WE: Write,
    {
        match cmd {
            BuiltinCommand::Exit => self.builtin_exit(stdin, stdout, stderr, args),
            BuiltinCommand::Cd => self.builtin_cd(stdin, stdout, stderr, args),
            BuiltinCommand::Which => self.builtin_which(stdin, stdout, stderr, args),
        }
    }

    pub fn builtin_exit<R, WO, WE>(
        &mut self,
        _stdin: R,
        _stdout: WO,
        _stderr: WE,
        args: &[String],
    ) -> Result<()> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(())
    }

    pub fn builtin_cd<R, WO, WE>(
        &mut self,
        _stdin: R,
        _stdout: WO,
        _stderr: WE,
        args: &[String],
    ) -> Result<()> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0].to_string())?;
        Ok(())
    }

    pub fn builtin_which<R, WO, WE>(
        &mut self,
        _stdin: R,
        mut stdout: WO,
        mut stderr: WE,
        args: &[String],
    ) -> Result<()>
    where
        WO: Write,
        WE: Write,
    {
        ensure!(args.len() == 1, "which: requires exact one argument");
        let name = &args[0];
        let cmd = match resolve_cmd(name) {
            Ok(cmd) => match cmd {
                CommandKind::Builtin(_) => "(builtin command)".to_string(),
                CommandKind::External(path) => format!("{}", path.display()),
            },
            Err(_) => {
                writeln!(stderr, "{}: not found", name)?;
                return Ok(());
            }
        };

        writeln!(stdout, "{}", cmd)?;
        Ok(())
    }
}
