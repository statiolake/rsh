use itertools::Itertools;

use crate::token::{FlattenedToken, FlattenedTokenKind};
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct CommandLine {
    pub delimited_piped_commands: Vec<PipedCommand>,
}

pub struct PipedCommand {
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
    File(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StderrDestination {
    InheritStdout,
    InheritStderr,
    PipeToNext,
    File(PathBuf),
}

pub fn parse_command_line(
    tokens: &[FlattenedToken],
    default_iospec: IOSpec,
) -> Result<CommandLine> {
    let delimited: Result<Vec<_>> = tokens
        .split(|tok| tok.data == FlattenedTokenKind::Delim)
        .map(|tokens| parse_piped_command(tokens, default_iospec.clone()))
        .collect();
    delimited.map(|piped| CommandLine {
        delimited_piped_commands: piped,
    })
}

pub fn parse_piped_command(
    tokens: &[FlattenedToken],
    default_iospec: IOSpec,
) -> Result<PipedCommand> {
    let mut components = vec![];
    let cmds_toks = tokens
        .split(|tok| tok.data == FlattenedTokenKind::Delim)
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

    Ok(PipedCommand {
        pipe_components: components,
    })
}

/// ## Note
///
/// Tokens must be flattened.
pub fn parse_command(tokens: &[FlattenedToken], default_iospec: IOSpec) -> Result<Command> {
    use crate::token::{RedirectKind as RK, RedirectReferenceKind as RRK};

    let mut iospec = default_iospec;
    let mut args = vec![];

    for arg_toks in tokens.split(|tok| tok.data == FlattenedTokenKind::ArgDelim) {
        if arg_toks.is_empty() {
            continue;
        }

        if let FlattenedTokenKind::Redirect(redir) = &arg_toks[0].data {
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
                            StderrDestination::File(path) => StdoutDestination::File(path.clone()),
                        }
                    }
                    (RK::Stderr, RRK::Stdout) => {
                        iospec.stderr = match &iospec.stdout {
                            StdoutDestination::InheritStderr => StderrDestination::InheritStderr,
                            StdoutDestination::InheritStdout => StderrDestination::InheritStdout,
                            StdoutDestination::PipeToNext => StderrDestination::PipeToNext,
                            StdoutDestination::File(path) => StderrDestination::File(path.clone()),
                        }
                    }
                    _ => {}
                }
            } else {
                let path = toks_to_string(&arg_toks[1..]);
                match redir.kind {
                    RK::Stdin => iospec.stdin = StdinSource::File(PathBuf::from(path)),
                    RK::Stdout => iospec.stdout = StdoutDestination::File(PathBuf::from(path)),
                    RK::Stderr => iospec.stderr = StderrDestination::File(PathBuf::from(path)),
                }
            }
        } else {
            args.push(toks_to_string(arg_toks));
        }
    }

    Ok(Command { args, iospec })
}

pub fn toks_to_string(toks: &[FlattenedToken]) -> String {
    use FlattenedTokenKind::*;

    let mut arg = String::new();
    for tok in toks {
        match &tok.data {
            Atom(a) => arg.push(*a),
            Quoted(q) => arg.extend(q.iter().copied()),
            e => panic!("internal error: invalid token kind `{:?}` is in args", e),
        }
    }
    arg
}

// pub struct Parser<'a> {
//     tokens: &'a [FlattenedToken],
//     current: usize,
// }
//
// impl<'a> Parser<'a> {
//     pub fn new(tokens: &'a [FlattenedToken]) -> Self {
//         Self { tokens, current: 0 }
//     }
//
//     pub fn parse(&mut self) -> Result<CommandLine<'a>> {
//         let mut commands = Vec::new();
//         while self.peek().is_some() {
//             commands.push(self.next_piped_command()?);
//         }
//
//         Ok(CommandLine {
//             piped_commands: commands,
//         })
//     }
//
//     fn next_piped_command(&mut self) -> Result<PipedCommand<'a>> {
//         // TODO
//     }
//
//     fn next_command(&mut self, default_iospec: IOSpec<'a>) -> Result<Command<'a>> {
//         use crate::lexer::{RedirectKind as RK, RedirectReferenceKind as RRK};
//
//         // Skip leading ArgDelims.
//         while let Some(tok) = self.peek() {
//             if !matches!(tok.data, FlattenedTokenKind::ArgDelim) {
//                 break;
//             }
//         }
//
//         let mut args = vec![];
//         let mut iospec = default_iospec;
//         while let Some(tok) = self.peek() {
//             match &tok.data {
//                 FlattenedTokenKind::Delim => break,
//                 FlattenedTokenKind::Redirect(redir) => todo!(),
//                 FlattenedTokenKind::Pipe => todo!(),
//                 FlattenedTokenKind::Atom(_) => todo!(),
//                 FlattenedTokenKind::SingleQuoted(_) => todo!(),
//                 FlattenedTokenKind::DoubleQuoted(_) => todo!(),
//                 FlattenedTokenKind::ArgDelim => todo!(),
//             }
//         }
//
//         Ok(Command { args, iospec })
//     }
//
//     fn peek(&self) -> Option<&'a FlattenedToken> {
//         self.lookahead(0)
//     }
//
//     fn lookahead(&self, n: usize) -> Option<&'a FlattenedToken> {
//         self.peek_rest().get(n)
//     }
//
//     fn peek_rest(&self) -> &'a [FlattenedToken] {
//         &self.tokens[self.current..]
//     }
// }
