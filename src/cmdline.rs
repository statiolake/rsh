use itertools::Itertools as _;
use std::env;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CommandLine {
    pub cmd: CommandKind,
    pub args: Vec<Arg>,
}

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
pub struct Arg {
    atoms: Vec<ArgAtom>,
}

#[derive(Debug, Clone)]
pub enum ArgAtom {
    Char(char),
    Var(String),
}

impl CommandLine {
    pub fn new(cmd: CommandKind, args: Vec<Arg>) -> Self {
        Self { cmd, args }
    }
}

impl Arg {
    pub fn new(atoms: Vec<ArgAtom>) -> Arg {
        Arg { atoms }
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "{}", self.atoms.iter().format(""))
    }
}

impl fmt::Display for ArgAtom {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArgAtom::Char(ch) => write!(b, "{}", ch),
            ArgAtom::Var(name) => {
                if let Ok(value) = env::var(name) {
                    write!(b, "{}", value)?;
                }
                Ok(())
            }
        }
    }
}
