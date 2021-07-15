use crate::cmdline::{ArgAtom, Args};
use anyhow::Result;
use anyhow::{bail, ensure};
use itertools::Itertools;

pub struct Cursor {
    reversed: Vec<char>,
}

impl Cursor {
    pub fn new(s: &str) -> Self {
        Self {
            reversed: s.chars().rev().collect_vec(),
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.reversed.last().copied()
    }

    pub fn next(&mut self) -> Option<char> {
        self.reversed.pop()
    }

    pub fn skip_whitespace(&mut self) {
        while self.peek().map(char::is_whitespace).unwrap_or(false) {
            self.next();
        }
    }

    pub fn is_finished(&self) -> bool {
        self.reversed.is_empty()
    }
}

pub struct ArgsParser<'a> {
    cursor: &'a mut Cursor,
}

impl<'a> ArgsParser<'a> {
    pub fn new(cursor: &'a mut Cursor) -> Self {
        Self { cursor }
    }

    pub fn parse_args(&mut self) -> Result<Args> {
        let mut atoms = Vec::new();
        loop {
            self.cursor.skip_whitespace();
            match self.cursor.peek() {
                // All of the input is read.
                None => break,

                // The endmarker of (possible) parent invocation.
                Some(')') => break,

                // Otherwise continue parsing in this level.
                _ => {}
            }

            if !atoms.is_empty() {
                atoms.push(ArgAtom::Delim);
            }
            atoms.extend(self.parse_arg()?);
        }

        Ok(Args::from_atoms(atoms))
    }

    fn parse_arg(&mut self) -> Result<Vec<ArgAtom>> {
        ArgParser::new(&mut self.cursor).parse_arg()
    }
}

pub struct ArgParser<'a> {
    cursor: &'a mut Cursor,
    in_single: bool,
    in_double: bool,
    atoms: Vec<ArgAtom>,
}

impl<'a> ArgParser<'a> {
    fn new(cursor: &'a mut Cursor) -> Self {
        Self {
            cursor,
            in_single: false,
            in_double: false,
            atoms: vec![],
        }
    }

    fn parse_arg(mut self) -> Result<Vec<ArgAtom>> {
        loop {
            let ch = match self.cursor.peek() {
                None => {
                    ensure!(!self.in_single, "single quote is not closed");
                    ensure!(!self.in_double, "double quote is not closed");
                    return Ok(self.atoms);
                }
                Some(')') if !self.in_single => {
                    ensure!(!self.in_double, "double quote is not closed");
                    return Ok(self.atoms);
                }
                Some(ch) if ch.is_whitespace() && (!self.in_single && !self.in_double) => {
                    return Ok(self.atoms);
                }
                Some(ch) => ch,
            };

            match ch {
                '"' if !self.in_single => {
                    assert_eq!(self.cursor.next(), Some('"'));
                    self.in_double = !self.in_double;
                }
                '\'' if !self.in_double => {
                    assert_eq!(self.cursor.next(), Some('\''));
                    self.in_single = !self.in_single;
                }
                _ => {
                    let atom = self.parse_arg_atom()?;
                    self.atoms.push(atom);
                }
            }
        }
    }

    fn parse_arg_atom(&mut self) -> Result<ArgAtom> {
        let ch = self.cursor.next().expect("there must be at least one char");
        match ch {
            '^' if !self.in_single => {
                let next = match self.cursor.next() {
                    Some(ch) => ch,
                    None => bail!("unexpected eol after escape `^`"),
                };
                match next {
                    '"' => Ok(ArgAtom::Char('"')),
                    'n' => Ok(ArgAtom::Char('\n')),
                    't' => Ok(ArgAtom::Char('\t')),
                    ' ' => Ok(ArgAtom::Char(' ')),
                    _ => bail!("unknown escape sequence: ^{}", next),
                }
            }
            '$' if !self.in_single => match self.cursor.peek() {
                Some('(') => self.parse_arg_atom_cmd(),
                _ => self.parse_arg_atom_var(),
            },
            _ => Ok(ArgAtom::Char(ch)),
        }
    }

    fn parse_arg_atom_cmd(&mut self) -> Result<ArgAtom> {
        assert_eq!(self.cursor.next(), Some('('));
        let args = ArgsParser::new(&mut self.cursor).parse_args()?;
        ensure!(self.cursor.next() == Some(')'), "no matching ')' found");
        Ok(ArgAtom::Cmd(args))
    }

    fn parse_arg_atom_var(&mut self) -> Result<ArgAtom> {
        let brace_end = self.cursor.peek() == Some('{');
        if brace_end {
            assert_eq!(self.cursor.next(), Some('{'));
        }

        let mut name = String::new();
        loop {
            match self.cursor.next() {
                None if brace_end => bail!("unexpected end during parsing variable name"),
                Some('}') if brace_end => break,
                None => break,
                Some(ch) if !brace_end && ch.is_whitespace() => break,
                Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => name.push(ch),
                _ => bail!("variable name may only contain alphabet or underscore"),
            }
        }

        Ok(ArgAtom::Var(name))
    }
}
