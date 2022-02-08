use anyhow::Result;
use std::path::PathBuf;

use crate::shell::ShellState;

#[derive(Debug, Clone)]
pub enum CommandKind {
    Builtin(BuiltinCommand),
    External(PathBuf),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinCommand {
    Exit,
    Cd,
    Which,
    Set,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StdinSource {
    Inherit,
    PipeFromPrevious,
    File(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StdoutDestination {
    Inherit,
    PipeToNext,
    File(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StderrDestination {
    Inherit,
    Stdout,
    File(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IOSpec {
    pub is: StdinSource,
    pub od: StdoutDestination,
    pub ed: StderrDestination,
}

impl IOSpec {
    pub fn new(is: StdinSource, od: StdoutDestination, ed: StderrDestination) -> Self {
        Self { is, od, ed }
    }
}

#[derive(Debug, Clone)]
pub struct ArgsComposition {
    pub composition: Vec<(Args, IOSpec)>,
}

#[derive(Debug, Clone)]
pub struct Args {
    atoms: Vec<ArgAtom>,
}

#[derive(Debug, Clone)]
pub enum ArgAtom {
    Delim,
    Char(char),
    Var(String),
    Cmd(ArgsComposition),
}

impl Args {
    pub fn from_atoms(atoms: Vec<ArgAtom>) -> Self {
        Self { atoms }
    }

    pub fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    pub fn flatten(self, state: &mut ShellState) -> Result<Args> {
        let mut atoms = vec![];
        for atom in self.atoms {
            match atom {
                ArgAtom::Cmd(composition) => {
                    let output = str_to_atoms(&state.run_composition_capture(composition)?);
                    atoms.extend(output);
                }
                other => {
                    atoms.push(other.clone());
                }
            }
        }

        Ok(Self::from_atoms(atoms))
    }

    pub fn to_vec(&self, state: &ShellState) -> Vec<String> {
        self.atoms
            .split(ArgAtom::is_delim)
            .map(|arg| {
                arg.iter()
                    .map(|atom| match atom {
                        ArgAtom::Delim => unreachable!("found delimiter after splitting"),
                        ArgAtom::Char(ch) => ch.to_string(),
                        ArgAtom::Var(name) => state.var(name).unwrap_or_else(String::new),
                        ArgAtom::Cmd(_) => panic!("to_vec() called on unflattened args"),
                    })
                    .collect()
            })
            .collect()
    }
}

impl ArgAtom {
    fn is_delim(&self) -> bool {
        matches!(self, ArgAtom::Delim)
    }
}

/// Break output to lines and parse them as an atom
fn str_to_atoms(out: &str) -> Vec<ArgAtom> {
    let mut atoms = vec![];
    for line in out.trim().lines() {
        if !atoms.is_empty() {
            atoms.push(ArgAtom::Delim);
        }
        atoms.extend(line.chars().map(ArgAtom::Char));
    }

    atoms
}
