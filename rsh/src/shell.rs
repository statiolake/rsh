use crate::{conmode::ConsoleModeKeeper, executable::resolve_executable};
use crate::{
    executable::{Exit, ReadIntoStdio, WriteIntoStdio},
    view::beautify_path,
};
use anyhow::anyhow;
use anyhow::Result;
use itertools::Itertools;
use rsh_line_editor::{LineEditor, PromptWriter, UserInput};
use rsh_line_parser::{
    lexer::{normalize_flattened_tokens, Lexer},
    parser::{
        parse_command_line, CommandLine, IOSpec, PipeCommand, StderrDestination, StdinSource,
        StdoutDestination,
    },
    token::{Atom, FlattenedToken, Token, TokenKind},
};
use same_file::is_same_file;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::thread;
use std::{env, thread::JoinHandle};
use std::{env::current_dir, fmt::Display};
use std::{env::set_current_dir, fs::File};

const HISTORY_FILE: &str = ".rsh_history";

#[derive(Debug)]
pub struct Shell {
    rle: LineEditor,
    state: ShellState,
}

#[derive(Debug, Clone)]
pub struct ShellState {
    pub loop_running: bool,
    pub last_working_dir: Option<PathBuf>,
    pub pushd_stack: Vec<PathBuf>,
}

impl Shell {
    pub fn new() -> Result<Self> {
        let rle = LineEditor::new();
        let state = ShellState::new()?;
        Ok(Self { rle, state })
    }

    pub fn mainloop(&mut self) {
        self.state.loop_running = true;
        #[allow(clippy::while_immutable_condition)]
        while self.state.loop_running {
            let res = crossbeam::scope(|scope| {
                scope.spawn(|_| {
                    if let Err(e) = self.read_and_run() {
                        print_error(e);
                    }
                });
            });

            if res.is_err() {
                eprintln!("!INTERNAL ERROR! panicked.");
            }
        }
    }

    pub fn read_and_run(&mut self) -> Result<()> {
        let cmdline = self.read_line()?;
        let source = cmdline.chars().collect_vec();
        let tokens = Lexer::new(&source).tokenize()?;

        if tokens.data.is_empty() {
            return Ok(());
        }

        let _keeper = ConsoleModeKeeper::new()?;
        self.state.run(tokens.data)
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

impl ShellState {
    pub fn new() -> Result<ShellState> {
        Ok(Self {
            loop_running: false,
            last_working_dir: None,
            pushd_stack: vec![],
        })
    }

    pub fn run(&mut self, tokens: Vec<Token>) -> Result<()> {
        run_tokens(
            self,
            tokens,
            IOSpec::new(
                StdinSource::InheritStdin,
                StdoutDestination::InheritStdout,
                StderrDestination::InheritStderr,
            ),
        )
        .map(drop)
    }

    pub fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
    }

    pub fn chdir<P: AsRef<Path>>(&mut self, target: P) -> Result<()> {
        let target = target.as_ref();
        let cwd = env::current_dir()?;
        self.last_working_dir = Some(cwd);
        set_current_dir(target)?;
        Ok(())
    }
}

pub struct CapturedOutput {
    pub stdout: Option<String>,
    pub stderr: Option<String>,
}

impl CapturedOutput {
    pub fn new() -> Self {
        Self {
            stdout: None,
            stderr: None,
        }
    }
}

fn run_tokens(
    state: &mut ShellState,
    tokens: Vec<Token>,
    iospec: IOSpec,
) -> Result<CapturedOutput> {
    let res = flatten(state, tokens, iospec.clone())?;
    let command_line = parse_command_line(&res, iospec)?;
    run_command_line(state, command_line)
}

fn flatten(
    state: &mut ShellState,
    tokens: Vec<Token>,
    iospec: IOSpec,
) -> Result<Vec<FlattenedToken>> {
    let mut res = vec![];
    for token in tokens {
        res.extend(flatten_token(state, token, Some(iospec.clone()))?);
    }
    normalize_flattened_tokens(&mut res);

    Ok(res)
}

fn flatten_token(
    state: &mut ShellState,
    token: Token,
    iospec: Option<IOSpec>,
) -> Result<Vec<FlattenedToken>> {
    let mut res = vec![];

    match token.data {
        TokenKind::Atom(atom) => res.extend(flatten_atom(state, atom, false, iospec)?),
        TokenKind::SingleQuoted(q) => {
            q.0.data
                .into_iter()
                .for_each(|ch| res.push(FlattenedToken::Atom(ch.data)))
        }
        TokenKind::DoubleQuoted(q) => q.0.data.into_iter().try_for_each(|atom| {
            flatten_atom(state, atom, true, iospec.clone()).map(|t| res.extend(t))
        })?,
        TokenKind::ArgDelim => res.push(FlattenedToken::ArgDelim),
        TokenKind::Redirect(redir) => res.push(FlattenedToken::Redirect(redir)),
        TokenKind::Pipe => res.push(FlattenedToken::Pipe),
        TokenKind::Delim => res.push(FlattenedToken::Delim),
    }

    Ok(res)
}

fn flatten_atom(
    state: &mut ShellState,
    atom: Atom,
    in_double_quote: bool,
    iospec: Option<IOSpec>,
) -> Result<Vec<FlattenedToken>> {
    let mut res = vec![];

    match atom {
        Atom::Char(ch) => res.push(FlattenedToken::Atom(ch.data)),
        Atom::EnvVar(env_var) => {
            let var_name: String = env_var.0.data.iter().map(|ch| ch.data).collect();
            state
                .var(&var_name)
                .unwrap_or_default()
                .chars()
                .for_each(|ch| res.push(FlattenedToken::Atom(ch)))
        }
        Atom::Substitution(st) => {
            // When iospec is None means we know substitution never occurs. However it occured ---
            // that's a bug.
            let iospec = IOSpec {
                stdout: StdoutDestination::Capture,
                ..iospec.expect("internal error: disabled substitution occured")
            };

            let output = run_tokens(state, st.0.data, iospec)?
                .stdout
                .expect("internal error: cannot access captured stdout");

            Lexer::new(&output.chars().collect_vec())
                .tokenize_substitution(!in_double_quote)?
                .into_iter()
                .for_each(|token| res.extend(flatten_token(state, token, None).unwrap()));
        }
    }

    Ok(res)
}

fn run_command_line(state: &mut ShellState, command_line: CommandLine) -> Result<CapturedOutput> {
    let mut last_result = Ok(CapturedOutput::new());
    for pipe_command in command_line.delimited_pipe_command {
        last_result = run_pipe_command(state, pipe_command)
    }

    last_result
}

fn run_pipe_command(state: &mut ShellState, pipe_command: PipeCommand) -> Result<CapturedOutput> {
    let components = pipe_command.pipe_components;
    if components.is_empty() {
        return Ok(CapturedOutput {
            stdout: None,
            stderr: None,
        });
    }

    let mut handles: Vec<JoinHandle<_>> = vec![];

    let mut pipe_input = None;
    let mut captured_stdout = None;
    let mut captured_stderr = None;
    for command in components {
        let stdin: Box<dyn ReadIntoStdio> = match command.iospec.stdin {
            StdinSource::InheritStdin => Box::new(os_pipe::dup_stdin()?),
            StdinSource::PipeFromPrevious => pipe_input
                .take()
                .expect("internal error: failed to take previous pipe stdin"),
            StdinSource::File(path) => Box::new(File::open(path)?),
        };

        let stdout: Box<dyn WriteIntoStdio> = match &command.iospec.stdout {
            StdoutDestination::InheritStdout => Box::new(os_pipe::dup_stdout()?),
            StdoutDestination::InheritStderr => Box::new(os_pipe::dup_stderr()?),
            StdoutDestination::PipeToNext => {
                let (reader, writer) = os_pipe::pipe()?;
                pipe_input = Some(Box::new(reader));
                Box::new(writer)
            }
            StdoutDestination::File { path, append } => Box::new(
                File::options()
                    .write(true)
                    .append(*append)
                    .truncate(!*append)
                    .create(true)
                    .open(path)?,
            ),
            StdoutDestination::Capture => {
                let (reader, writer) = os_pipe::pipe()?;
                captured_stdout = Some(Box::new(reader));
                Box::new(writer)
            }
        };

        let stderr: Box<dyn WriteIntoStdio> = match &command.iospec.stderr {
            StderrDestination::InheritStdout => Box::new(os_pipe::dup_stdout()?),
            StderrDestination::InheritStderr => Box::new(os_pipe::dup_stderr()?),
            StderrDestination::PipeToNext => {
                let (reader, writer) = os_pipe::pipe()?;
                pipe_input = Some(Box::new(reader));
                Box::new(writer)
            }
            StderrDestination::File { path, append } => {
                // stdout と同じファイルだったら特別扱いしないと上書きしてしまう
                let stdout_iospec = &command.iospec.stdout;
                if matches!(
                    stdout_iospec,
                    StdoutDestination::File {
                        path: stdout_path,
                        ..
                    } if is_same_file(stdout_path, path)?
                ) {
                    stdout.try_clone()?
                } else {
                    Box::new(
                        File::options()
                            .write(true)
                            .append(*append)
                            .truncate(!*append)
                            .create(true)
                            .open(path)?,
                    )
                }
            }
            StderrDestination::Capture => {
                let (reader, writer) = os_pipe::pipe()?;
                captured_stderr = Some(Box::new(reader));
                Box::new(writer)
            }
        };

        let mut cloned = state.clone();
        let mut args = command.args;
        let mut cmd = resolve_executable(args.remove(0), args)?;
        let handle = thread::spawn(move || {
            let exit = cmd.execute(&mut cloned, stdin, stdout, stderr)?;
            Ok((exit, cloned))
        });
        handles.push(handle);
    }

    // captured stdout
    let stdout_capture_handle = thread::spawn(move || {
        captured_stdout
            .map(|mut captured_stdout| {
                let mut buf = String::new();
                captured_stdout.read_to_string(&mut buf).map(move |_| buf)
            })
            .transpose()
    });

    // captured stderr
    let stderr_capture_handle = thread::spawn(move || {
        captured_stderr
            .map(|mut captured_stderr| {
                let mut buf = String::new();
                captured_stderr.read_to_string(&mut buf).map(move |_| buf)
            })
            .transpose()
    });

    // Wait for all piped processes to finish
    //
    // TODO: use this exit status somewhere (prompt?)
    let (exit, next_state) = handles
        .into_iter()
        .map(|h| h.join())
        .collect::<Result<Result<Vec<_>>, _>>()
        .map_err(|_| anyhow!("failed to wait for child jobs"))??
        .into_iter()
        .fold((Exit::Success, None), |(entire_exit, _), (exit, state)| {
            if entire_exit == Exit::Success && exit == Exit::Success {
                (Exit::Success, Some(state))
            } else {
                (Exit::Failure, None)
            }
        });

    // reflect built-in command result here
    if let Some(next_state) = next_state {
        assert_eq!(exit, Exit::Success);
        *state = next_state;
    }

    let stdout = stdout_capture_handle
        .join()
        .map_err(|_| anyhow!("failed to wait for stdout capture"))??;

    let stderr = stderr_capture_handle
        .join()
        .map_err(|_| anyhow!("failed to wait for stderr capture"))??;

    Ok(CapturedOutput { stdout, stderr })
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
        let path = beautify_path(current_dir()?)?;
        // FIXME: change face according to the previous exit status
        let face = if true { "('-')/" } else { "(-_-)/" };

        let esc_cwd = format!("\x1b]9;9;{}\x1b\\", env::current_dir()?.display());

        write!(
            out,
            "{esc_cwd}{time} {whoami}:{path}\n{face} > ",
            whoami = format!("{}@{}", username, computername).green(),
            path = path.blue(),
        )?;
        out.flush()?;

        Ok(())
    }
}
