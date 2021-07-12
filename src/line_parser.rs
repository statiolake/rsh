use crate::cmdline::{Arg, ArgAtom};
use anyhow::Result;
use anyhow::{bail, ensure};
use itertools::Itertools;

pub struct Cursor {
    reversed: Vec<char>,
}

impl Cursor {
    fn new(s: &str) -> Self {
        Self {
            reversed: s.chars().rev().collect_vec(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.reversed.last().copied()
    }

    fn next(&mut self) -> Option<char> {
        self.reversed.pop()
    }

    fn skip_whitespace(&mut self) {
        while self.peek().map(char::is_whitespace).unwrap_or(false) {
            self.next();
        }
    }

    fn is_finished(&self) -> bool {
        self.reversed.is_empty()
    }
}

pub struct ArgsParser {
    cursor: Cursor,
}

impl ArgsParser {
    pub fn new(s: &str) -> Self {
        Self {
            cursor: Cursor::new(s),
        }
    }

    pub fn parse_args(&mut self) -> Result<Vec<Arg>> {
        let mut args = vec![];
        while !self.cursor.is_finished() {
            self.cursor.skip_whitespace();
            let arg = self.parse_arg()?;
            args.push(arg);
            self.cursor.skip_whitespace();
        }

        Ok(args)
    }

    fn parse_arg(&mut self) -> Result<Arg> {
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

    fn parse_arg(mut self) -> Result<Arg> {
        loop {
            let ch = match self.cursor.peek() {
                None => {
                    ensure!(!self.in_single, "single quote is not closed");
                    ensure!(!self.in_double, "double quote is not closed");
                    return Ok(Arg::new(self.atoms));
                }
                Some(ch) if ch.is_whitespace() && (!self.in_single && !self.in_double) => {
                    return Ok(Arg::new(self.atoms));
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
            _ => Ok(ArgAtom::Char(ch)),
        }
    }
}
