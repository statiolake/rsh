use crate::cmdline::*;
use crate::ctrlc_handler::register_child;
use crate::line_parser::{ArgsCompositionParser, Cursor};
use anyhow::Result;
use anyhow::{anyhow, ensure};
use rustyline::Editor;
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
    rl: Editor<()>,
    state: ShellState,
}

#[derive(Debug, Clone)]
pub struct ShellState {
    loop_running: bool,
}

impl Shell {
    pub fn new() -> Result<Self> {
        let mut rl = Editor::new();
        let _ = rl.load_history(&history_path()?);
        let state = ShellState::new()?;
        Ok(Self { rl, state })
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

impl ShellState {
    pub fn new() -> Result<ShellState> {
        Ok(Self {
            loop_running: false,
        })
    }

    pub fn run_compsition_inherit(&mut self, composition: ArgsComposition) -> Result<()> {
        let stdin = os_pipe::dup_stdin()?;
        let stdout = os_pipe::dup_stdout()?;
        self.run_composition_pipe(stdin, stdout, composition)
    }

    pub fn run_composition_capture(&mut self, composition: ArgsComposition) -> Result<String> {
        let stdin = os_pipe::dup_stdin()?;
        let (mut reader, stdout) = os_pipe::pipe()?;
        let th = thread::spawn(move || -> Result<String> {
            let mut out = String::new();
            reader.read_to_string(&mut out)?;
            Ok(out)
        });

        self.run_composition_pipe(stdin, stdout, composition)?;
        let out = th
            .join()
            .map_err(|_| anyhow!("failed to join the thread reading child stdout"))??;

        Ok(out)
    }

    pub fn run_composition_pipe<R, W>(
        &mut self,
        stdin: R,
        stdout: W,
        composition: ArgsComposition,
    ) -> Result<()>
    where
        R: Read + Into<Stdio> + Send + Sync + 'static,
        W: Write + Into<Stdio> + Send + Sync + 'static,
    {
        trait ReadIntoStdio: Read + Send + Sync + 'static {
            fn box_into_stdio(self: Box<Self>) -> Stdio;
        }

        trait WriteIntoStdio: Write + Send + Sync + 'static {
            fn box_into_stdio(self: Box<Self>) -> Stdio;
        }

        impl<T: Read + Into<Stdio> + Send + Sync + 'static> ReadIntoStdio for T {
            fn box_into_stdio(self: Box<Self>) -> Stdio {
                (*self).into()
            }
        }

        impl<T: Write + Into<Stdio> + Send + Sync + 'static> WriteIntoStdio for T {
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
        let mut stdout = Some(Box::new(stdout) as Box<dyn WriteIntoStdio>);

        for (args, dest) in composition.composition {
            let mut cloned = self.clone();
            let curr_stdin = stdin.take().expect("stdin is already taken");
            match dest {
                StdoutDestination::Inherit => {
                    let curr_stdout = stdout.take().expect("stdout is already taken");
                    let handle = thread::spawn(move || -> Result<Option<ShellState>> {
                        cloned.run_args_pipe(curr_stdin, curr_stdout, args)?;
                        Ok(Some(cloned))
                    });
                    handles.push(handle);
                }
                StdoutDestination::File(path) => {
                    let curr_stdout = File::create(path)?;
                    let handle = thread::spawn(move || -> Result<Option<ShellState>> {
                        cloned.run_args_pipe(curr_stdin, curr_stdout, args)?;
                        Ok(Some(cloned))
                    });
                    handles.push(handle);
                }
                StdoutDestination::PipeToNext => {
                    let (next_stdin, stdout) = os_pipe::pipe()?;
                    let handle = thread::spawn(move || -> Result<Option<ShellState>> {
                        cloned.run_args_pipe(curr_stdin, stdout, args)?;
                        Ok(None)
                    });
                    handles.push(handle);
                    stdin = Some(Box::new(next_stdin));
                }
            };
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

    pub fn run_args_pipe<R, W>(&mut self, stdin: R, stdout: W, args: Args) -> Result<()>
    where
        R: Read + Into<Stdio>,
        W: Write + Into<Stdio>,
    {
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

    fn execute<R, W>(
        &mut self,
        stdin: R,
        stdout: W,
        cmd: &CommandKind,
        args: &[String],
    ) -> Result<()>
    where
        R: Read + Into<Stdio>,
        W: Write + Into<Stdio>,
    {
        match cmd {
            CommandKind::Builtin(cmd) => self.execute_builtin(stdin, stdout, *cmd, args),
            CommandKind::External(cmd) => self.execute_external(stdin, stdout, cmd, args),
        }
    }

    fn execute_external<R, W>(
        &mut self,
        stdin: R,
        stdout: W,
        cmd: &Path,
        args: &[String],
    ) -> Result<()>
    where
        R: Into<Stdio>,
        W: Into<Stdio>,
    {
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

fn parse_cmdline(cmdline: &str) -> Result<ArgsComposition> {
    let mut cursor = Cursor::new(cmdline);
    let composition = ArgsCompositionParser::new(&mut cursor).parse()?;
    ensure!(cursor.is_finished(), "input was not entirely read");
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

// builtin functions
impl ShellState {
    pub fn execute_builtin<R, W>(
        &mut self,
        stdin: R,
        stdout: W,
        cmd: BuiltinCommand,
        args: &[String],
    ) -> Result<()>
    where
        R: Read,
        W: Write,
    {
        match cmd {
            BuiltinCommand::Exit => self.builtin_exit(stdin, stdout, args),
            BuiltinCommand::Cd => self.builtin_cd(stdin, stdout, args),
            BuiltinCommand::Which => self.builtin_which(stdin, stdout, args),
        }
    }

    pub fn builtin_exit<R, W>(&mut self, _stdin: R, _stdout: W, args: &[String]) -> Result<()> {
        ensure!(args.is_empty(), "exit: does not take additional argument");
        self.loop_running = false;
        Ok(())
    }

    pub fn builtin_cd<R, W>(&mut self, _stdin: R, _stdout: W, args: &[String]) -> Result<()> {
        ensure!(args.len() == 1, "cd: requires exact one argument");
        env::set_current_dir(&args[0].to_string())?;
        Ok(())
    }

    pub fn builtin_which<R, W>(&mut self, _stdin: R, mut stdout: W, args: &[String]) -> Result<()>
    where
        W: Write,
    {
        ensure!(args.len() == 1, "which: requires exact one argument");
        let name = &args[0];
        let cmd = match resolve_cmd(name) {
            Ok(cmd) => match cmd {
                CommandKind::Builtin(_) => "(builtin command)".to_string(),
                CommandKind::External(path) => format!("{}", path.display()),
            },
            Err(_) => {
                // TODO: give PipeReader for stderr
                eprintln!("{}: not found", name);
                return Ok(());
            }
        };

        writeln!(stdout, "{}", cmd)?;
        Ok(())
    }
}
