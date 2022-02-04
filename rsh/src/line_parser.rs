use crate::cmdline::{
    ArgAtom, Args, ArgsComposition, IOSpec, StderrDestination, StdinSource, StdoutDestination,
};
use anyhow::Result;
use anyhow::{bail, ensure};
use itertools::Itertools;
use std::path::PathBuf;

#[derive(Debug)]
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
        self.lookahead(0)
    }

    pub fn lookahead(&self, n: usize) -> Option<char> {
        self.reversed
            .len()
            .checked_sub(n)
            .and_then(|idx| idx.checked_sub(1))
            .and_then(|idx| self.reversed.get(idx))
            .copied()
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

    pub fn left(&self) -> String {
        self.reversed.iter().rev().copied().collect()
    }
}

#[derive(Debug)]
/// Parse command line.
///
/// <command-line> ::= <args> ("|" <args>)*
pub struct ArgsCompositionParser<'a> {
    cursor: &'a mut Cursor,
}

impl<'a> ArgsCompositionParser<'a> {
    pub fn new(cursor: &'a mut Cursor) -> Self {
        Self { cursor }
    }

    pub fn parse(mut self) -> Result<ArgsComposition> {
        let mut composition = vec![];
        self.cursor.skip_whitespace();
        let mut is_piped = false;
        while !self.cursor.is_finished() {
            let args = ArgsParser::new(self.cursor).parse()?;
            self.cursor.skip_whitespace();
            let mut spec = self.parse_redirect()?;
            if is_piped {
                ensure!(
                    spec.is == StdinSource::Inherit,
                    "pipe and redirect conflicts for stdin"
                );
                spec.is = StdinSource::PipeFromPrevious;
            }

            self.cursor.skip_whitespace();
            match self.cursor.peek() {
                // Pipe.
                Some('|') => {
                    // Consume this pipe.
                    self.cursor.next();
                    self.cursor.skip_whitespace();
                    ensure!(!args.is_empty(), "pipe source is empty command");
                    ensure!(
                        spec.od == StdoutDestination::Inherit,
                        "pipe and redirect conflicts for stdout"
                    );
                    spec.od = StdoutDestination::PipeToNext;
                    composition.push((args, spec));
                    is_piped = true;
                }
                // End of the command line
                Some(';') | None => {
                    // Consume this endmarker.
                    self.cursor.next();
                    self.cursor.skip_whitespace();
                    composition.push((args, spec));
                    break;
                }
                // End of nested command.
                Some(')') => {
                    // DO NOT consume this; the parent parse_arg_atom_cmd() expects ')' to be there.
                    composition.push((args, spec));
                    break;
                }

                Some(ch) => unreachable!("args parser stopped at non-terminal char: {}", ch),
            }
        }

        Ok(ArgsComposition { composition })
    }

    fn parse_redirect(&mut self) -> Result<IOSpec> {
        let mut is = StdinSource::Inherit;
        let mut od = StdoutDestination::Inherit;
        let mut ed = StderrDestination::Inherit;

        loop {
            self.cursor.skip_whitespace();
            match (self.cursor.lookahead(0), self.cursor.lookahead(1)) {
                // Stdin redirect
                (Some('<'), _) => {
                    // Consume this redirect.
                    self.cursor.next();
                    self.cursor.skip_whitespace();
                    let path = self.parse_path()?;
                    is = StdinSource::File(path);
                }

                // Stdout redirect
                (Some('>'), _) | (Some('1'), Some('>')) => {
                    // Consume this redirect.
                    if self.cursor.next() == Some('1') {
                        self.cursor.next();
                    }

                    self.cursor.skip_whitespace();
                    let path = self.parse_path()?;
                    od = StdoutDestination::File(path);
                }

                // Stderr redirect
                (Some('2'), Some('>')) => {
                    // Consume this redirect.
                    self.cursor.next();
                    self.cursor.next();

                    self.cursor.skip_whitespace();
                    if self.cursor.peek() == Some('&') {
                        // Consume this specifier.
                        self.cursor.next();
                        match self.cursor.next() {
                            Some('1') => ed = StderrDestination::Stdout,
                            Some(ch) => bail!("unknown stream number: &{}", ch),
                            None => bail!("stream number expected after &"),
                        }
                    } else {
                        let path = self.parse_path()?;
                        ed = StderrDestination::File(path);
                    }
                }

                _ => break,
            }
        }

        Ok(IOSpec::new(is, od, ed))
    }

    fn parse_path(&mut self) -> Result<PathBuf> {
        self.cursor.skip_whitespace();
        let path: String = ArgParser::new(self.cursor, false)
            .parse()?
            .into_iter()
            .map(|atom| match atom {
                ArgAtom::Char(ch) => ch,
                _ => unreachable!("found non-char even though interpolation is disabled"),
            })
            .collect();
        Ok(PathBuf::from(path))
    }
}

#[derive(Debug)]
pub struct ArgsParser<'a> {
    cursor: &'a mut Cursor,
}

impl<'a> ArgsParser<'a> {
    pub fn new(cursor: &'a mut Cursor) -> Self {
        Self { cursor }
    }

    pub fn parse(mut self) -> Result<Args> {
        let mut atoms = Vec::new();
        loop {
            self.cursor.skip_whitespace();
            match self.cursor.peek() {
                // All of the input is read.
                None => break,

                // The endmarker of (possible) parent invocation.
                Some(')') => break,

                // Pipe.
                Some('|') => break,

                // Redirect.
                Some('<') => break,
                Some('>') => break,
                Some('1') | Some('2') if self.cursor.lookahead(1) == Some('>') => break,

                // Otherwise continue parsing in this level.
                _ => {}
            }

            if !atoms.is_empty() {
                atoms.push(ArgAtom::Delim);
            }
            atoms.extend(ArgParser::new(&mut self.cursor, true).parse()?);
        }

        Ok(Args::from_atoms(atoms))
    }
}

#[derive(Debug)]
pub struct ArgParser<'a> {
    cursor: &'a mut Cursor,
    interpolate: bool,
    in_single: bool,
    in_double: bool,
    atoms: Vec<ArgAtom>,
}

impl<'a> ArgParser<'a> {
    fn new(cursor: &'a mut Cursor, interpolate: bool) -> Self {
        Self {
            cursor,
            interpolate,
            in_single: false,
            in_double: false,
            atoms: vec![],
        }
    }

    fn parse(mut self) -> Result<Vec<ArgAtom>> {
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
            '$' if !self.in_single && self.interpolate => match self.cursor.peek() {
                Some('(') => self.parse_arg_atom_cmd(),
                _ => self.parse_arg_atom_var(),
            },
            _ => Ok(ArgAtom::Char(ch)),
        }
    }

    fn parse_arg_atom_cmd(&mut self) -> Result<ArgAtom> {
        assert_eq!(self.cursor.next(), Some('('));
        let args = ArgsCompositionParser::new(&mut self.cursor).parse()?;
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
