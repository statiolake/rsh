use crate::token::{
    DoubleQuoted, FlattenedQuoted, FlattenedToken, FlattenedTokenKind, SingleQuoted,
};
use itertools::Itertools;
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("empty command")]
    EmptyCommand,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct CommandLine {
    pub delimited_pipe_command: Vec<PipeCommand>,
}

pub struct PipeCommand {
    pub pipe_components: Vec<Command>,
}

pub struct Command {
    pub args: Vec<String>,
    pub iospec: IOSpec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IOSpec {
    pub stdin: StdinSource,
    pub stdout: StdoutDestination,
    pub stderr: StderrDestination,
}

impl IOSpec {
    pub fn new(stdin: StdinSource, stdout: StdoutDestination, stderr: StderrDestination) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StdinSource {
    InheritStdin,
    PipeFromPrevious,
    File(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StdoutDestination {
    InheritStdout,
    InheritStderr,
    PipeToNext,
    File { path: PathBuf, append: bool },
    Capture,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StderrDestination {
    InheritStdout,
    InheritStderr,
    PipeToNext,
    File { path: PathBuf, append: bool },
    Capture,
}

pub fn parse_command_line(
    tokens: &[FlattenedToken],
    default_iospec: IOSpec,
) -> Result<CommandLine> {
    let delimited = tokens
        .split(|tok| *tok == FlattenedTokenKind::Delim)
        .map(|tokens| parse_piped_command(tokens, default_iospec.clone()))
        .collect::<Result<Vec<_>>>()?;
    Ok(CommandLine {
        delimited_pipe_command: delimited,
    })
}

pub fn parse_piped_command(
    tokens: &[FlattenedToken],
    default_iospec: IOSpec,
) -> Result<PipeCommand> {
    let mut components = vec![];
    let cmds_toks = tokens
        .split(|tok| *tok == FlattenedTokenKind::Pipe)
        .collect_vec();
    let len = cmds_toks.len();
    for (idx, cmd_toks) in cmds_toks.into_iter().enumerate() {
        let mut iospec = default_iospec.clone();

        // Change IOSpec for intermediate stdin / stdout since it's piped
        if idx != 0 {
            iospec.stdin = StdinSource::PipeFromPrevious;
        }
        if idx != len - 1 {
            iospec.stdout = StdoutDestination::PipeToNext;
        }

        components.push(parse_command(cmd_toks, iospec)?);
    }

    Ok(PipeCommand {
        pipe_components: components,
    })
}

pub fn parse_command(tokens: &[FlattenedToken], default_iospec: IOSpec) -> Result<Command> {
    use crate::token::{RedirectKind as RK, RedirectReferenceKind as RRK};

    let mut iospec = default_iospec;
    let mut args = vec![];

    for arg_toks in tokens.split(|tok| *tok == FlattenedTokenKind::ArgDelim) {
        if arg_toks.is_empty() {
            continue;
        }

        if let FlattenedTokenKind::Redirect(redir) = &arg_toks[0] {
            // Process redirect
            if let Some(rrk) = redir.reference {
                match (redir.kind, rrk) {
                    (RK::Stdin, _) => {
                        panic!("internal error: stdin redirect can't reference other")
                    }
                    (RK::Stdout, RRK::Stderr) => {
                        iospec.stdout = match &iospec.stderr {
                            StderrDestination::InheritStdout => StdoutDestination::InheritStdout,
                            StderrDestination::InheritStderr => StdoutDestination::InheritStderr,
                            StderrDestination::PipeToNext => StdoutDestination::PipeToNext,
                            StderrDestination::File { path, append } => StdoutDestination::File {
                                path: path.clone(),
                                append: *append,
                            },
                            StderrDestination::Capture => StdoutDestination::Capture,
                        }
                    }
                    (RK::Stderr, RRK::Stdout) => {
                        iospec.stderr = match &iospec.stdout {
                            StdoutDestination::InheritStderr => StderrDestination::InheritStderr,
                            StdoutDestination::InheritStdout => StderrDestination::InheritStdout,
                            StdoutDestination::PipeToNext => StderrDestination::PipeToNext,
                            StdoutDestination::File { path, append } => StderrDestination::File {
                                path: path.clone(),
                                append: *append,
                            },
                            StdoutDestination::Capture => StderrDestination::Capture,
                        }
                    }
                    _ => {}
                }
            } else {
                let path = toks_to_string(&arg_toks[1..]);
                match redir.kind {
                    RK::Stdin => iospec.stdin = StdinSource::File(PathBuf::from(path)),
                    RK::Stdout => {
                        iospec.stdout = StdoutDestination::File {
                            path: PathBuf::from(path),
                            append: redir.append,
                        }
                    }
                    RK::Stderr => {
                        iospec.stderr = StderrDestination::File {
                            path: PathBuf::from(path),
                            append: redir.append,
                        }
                    }
                }
            }
        } else {
            args.push(toks_to_string(arg_toks));
        }
    }

    if args.is_empty() {
        return Err(Error::EmptyCommand);
    }

    Ok(Command { args, iospec })
}

fn toks_to_string(toks: &[FlattenedToken]) -> String {
    let mut arg = String::new();
    for tok in toks {
        match &tok {
            FlattenedTokenKind::Atom(a) => arg.push(*a),
            FlattenedTokenKind::Quoted(FlattenedQuoted(q)) => arg.extend(q.iter().map(|ch| ch)),
            e => panic!("internal error: invalid token kind `{:?}` is in args", e),
        }
    }
    arg
}
