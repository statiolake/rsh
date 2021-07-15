use anyhow::Result;
use std::path::PathBuf;

use crate::shell::Shell;

#[derive(Debug, Clone)]
pub enum CommandKind {
    Builtin(BuiltinCommand),
    External(PathBuf),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinCommand {
    Exit,
    Cd,
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
    Cmd(Args),
}

impl Args {
    pub fn from_atoms(atoms: Vec<ArgAtom>) -> Self {
        Self { atoms }
    }

    pub fn flatten(&self, shell: &mut Shell) -> Result<Args> {
        let mut atoms = vec![];
        for atom in &self.atoms {
            match atom {
                ArgAtom::Cmd(args) => {
                    let output = str_to_atoms(&shell.run_args_captured(&args)?);
                    atoms.extend(output);
                }
                other => {
                    atoms.push(other.clone());
                }
            }
        }

        Ok(Self::from_atoms(atoms))
    }

    pub fn to_vec(&self, shell: &Shell) -> Vec<String> {
        self.atoms
            .split(ArgAtom::is_delim)
            .map(|arg| {
                arg.iter()
                    .map(|atom| match atom {
                        ArgAtom::Delim => unreachable!("found delimiter after splitting"),
                        ArgAtom::Char(ch) => ch.to_string(),
                        ArgAtom::Var(name) => shell.var(name).unwrap_or_else(String::new),
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
