use crate::executable::{Exit, ReadIntoStdio, WriteIntoStdio};
use crate::{conmode::ConsoleModeKeeper, executable::resolve_executable};
use anyhow::anyhow;
use anyhow::Result;
use itertools::Itertools;
use rsh_line_editor::{LineEditor, PromptWriter, UserInput};
use rsh_line_parser::{
    lexer::{Lexer, ESCAPE_CHAR},
    parser::{
        parse_command_line, CommandLine, IOSpec, PipeCommand, StderrDestination, StdinSource,
        StdoutDestination,
    },
    span::{Span, Spanned},
    token::{AtomKind, FlattenedToken, FlattenedTokenKind, Token, TokenKind},
};
use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf, MAIN_SEPARATOR};
use std::thread;
use std::{env, thread::JoinHandle};

const HISTORY_FILE: &str = ".rsh_history";

#[derive(Debug)]
pub struct Shell {
    rle: LineEditor,
    state: ShellState,
}

#[derive(Debug, Clone)]
pub struct ShellState {
    pub loop_running: bool,
}

impl Shell {
    pub fn new() -> Result<Self> {
        let rle = LineEditor::with_escape_char(Some(ESCAPE_CHAR));
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
        let tokens = Lexer::new(&cmdline.chars().collect_vec()).tokenize()?;

        let _keeper = ConsoleModeKeeper::new()?;
        self.state.run(tokens)
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
}

struct CapturedOutput {
    stdout: Option<String>,
    stderr: Option<String>,
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
    default_iospec: IOSpec,
) -> Result<CapturedOutput> {
    fn flatten_atom(
        state: &mut ShellState,
        default_iospec: IOSpec,
        flattened_tokens: &mut Vec<FlattenedToken>,
        span: Span,
        atom: AtomKind,
    ) -> Result<()> {
        match atom {
            AtomKind::Char(ch) => flattened_tokens.push(Spanned {
                span,
                data: FlattenedTokenKind::Atom(ch),
            }),
            AtomKind::EnvVar(env_var) => state
                .var(&env_var.0)
                .unwrap_or("".to_string())
                .chars()
                .for_each(|ch| {
                    flattened_tokens.push(Spanned {
                        span,
                        data: FlattenedTokenKind::Atom(ch),
                    })
                }),
            AtomKind::Substitution(st) => {
                let iospec = IOSpec {
                    stdout: StdoutDestination::Capture,
                    ..default_iospec.clone()
                };

                let output = run_tokens(state, st.0, iospec)?;
                output
                    .stdout
                    .expect("internal error: cannot access captured stdout")
                    .chars()
                    .for_each(|ch| {
                        flattened_tokens.push(Spanned {
                            span,
                            data: FlattenedTokenKind::Atom(ch),
                        })
                    });
            }
        }

        Ok(())
    }

    let mut flattened_tokens = vec![];

    for token in tokens {
        match token.data {
            TokenKind::Atom(atom) => flatten_atom(
                state,
                default_iospec.clone(),
                &mut flattened_tokens,
                token.span,
                atom,
            )?,
            TokenKind::SingleQuoted(q) => {
                let span = token.span;
                flattened_tokens.extend(
                    q.0.into_iter()
                        .map(|ch| FlattenedToken::new(span, FlattenedTokenKind::Atom(ch))),
                )
            }
            TokenKind::DoubleQuoted(q) => {
                let span = token.span;
                q.0.into_iter()
                    .map(|atom| {
                        flatten_atom(
                            state,
                            default_iospec.clone(),
                            &mut flattened_tokens,
                            span,
                            atom,
                        )
                    })
                    .collect::<Result<_>>()?
            }
            TokenKind::ArgDelim => flattened_tokens.push(Spanned {
                span: token.span,
                data: FlattenedTokenKind::ArgDelim,
            }),
            TokenKind::Redirect(redir) => flattened_tokens.push(Spanned {
                span: token.span,
                data: FlattenedTokenKind::Redirect(redir),
            }),
            TokenKind::Pipe => flattened_tokens.push(Spanned {
                span: token.span,
                data: FlattenedTokenKind::Pipe,
            }),
            TokenKind::Delim => flattened_tokens.push(Spanned {
                span: token.span,
                data: FlattenedTokenKind::Delim,
            }),
        }
    }

    let command_line = parse_command_line(&flattened_tokens, default_iospec)?;

    run_command_line(state, command_line)
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
    if components.len() == 0 {
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

        let stdout: Box<dyn WriteIntoStdio> = match command.iospec.stdout {
            StdoutDestination::InheritStdout => Box::new(os_pipe::dup_stdout()?),
            StdoutDestination::InheritStderr => Box::new(os_pipe::dup_stderr()?),
            StdoutDestination::PipeToNext => {
                let (reader, writer) = os_pipe::pipe()?;
                pipe_input = Some(Box::new(reader));
                Box::new(writer)
            }
            StdoutDestination::File(path) => Box::new(File::create(path)?),
            StdoutDestination::Capture => {
                let (reader, writer) = os_pipe::pipe()?;
                captured_stdout = Some(Box::new(reader));
                Box::new(writer)
            }
        };

        let stderr: Box<dyn WriteIntoStdio> = match command.iospec.stderr {
            StderrDestination::InheritStdout => Box::new(os_pipe::dup_stdout()?),
            StderrDestination::InheritStderr => Box::new(os_pipe::dup_stderr()?),
            StderrDestination::PipeToNext => {
                let (reader, writer) = os_pipe::pipe()?;
                pipe_input = Some(Box::new(reader));
                Box::new(writer)
            }
            StderrDestination::File(path) => Box::new(File::create(path)?),
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
        let path = current_dir()?;
        // FIXME: change face according to the previous exit status
        let face = if true { "('-')/" } else { "(-_-)/" };

        write!(
            out,
            "{time} {whoami}:{path}\n{face} > ",
            time = time,
            whoami = format!("{}@{}", username, computername).green(),
            path = path.blue(),
            face = face
        )?;
        out.flush()?;

        Ok(())
    }
}

fn current_dir() -> Result<String> {
    let path = env::current_dir()?;
    let home = match dirs::home_dir() {
        Some(home) => home,
        None => return Ok(path.to_string_lossy().into_owned()),
    };
    Ok(match path.strip_prefix(home) {
        Ok(under_home) if under_home.as_os_str().is_empty() => "~".to_string(),
        Ok(under_home) => format!("~{}{}", MAIN_SEPARATOR, under_home.display()),
        Err(_) => path.to_string_lossy().into_owned(),
    })
}
