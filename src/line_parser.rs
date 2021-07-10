use anyhow::Result;
use anyhow::{bail, ensure};
use itertools::Itertools;

pub struct LineParser {
    rest_reversed: Vec<char>,
}

impl LineParser {
    pub fn new(s: &str) -> Self {
        Self {
            rest_reversed: s.chars().rev().collect_vec(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<String>> {
        let mut args = vec![];
        while !self.rest_reversed.is_empty() {
            self.skip_whitespace();
            let arg = self.parse_arg()?;
            args.push(arg);
            self.skip_whitespace();
        }

        Ok(args)
    }

    fn peek(&self) -> Option<char> {
        self.rest_reversed.last().copied()
    }

    fn next(&mut self) -> Option<char> {
        self.rest_reversed.pop()
    }

    fn skip_whitespace(&mut self) {
        while self.peek().map(char::is_whitespace).unwrap_or(false) {
            self.next();
        }
    }

    fn parse_arg(&mut self) -> Result<String> {
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut arg = String::new();

        loop {
            let ch = match self.next() {
                None => {
                    ensure!(!in_single_quote, "single quote is not closed");
                    ensure!(!in_double_quote, "double quote is not closed");
                    return Ok(arg);
                }
                Some(ch) => ch,
            };

            match ch {
                '"' if !in_single_quote => in_double_quote = !in_double_quote,
                '\'' if !in_double_quote => in_single_quote = !in_single_quote,
                '^' if !in_single_quote => {
                    let next = match self.next() {
                        Some(ch) => ch,
                        None => bail!("unexpected eol after escape `^`"),
                    };
                    match next {
                        '"' => arg.push('"'),
                        'n' => arg.push('\n'),
                        't' => arg.push('\t'),
                        ' ' => arg.push(' '),
                        _ => bail!("unknown escape sequence: ^{}", next),
                    }
                }
                _ if (!in_double_quote && !in_single_quote) && ch.is_whitespace() => {
                    return Ok(arg);
                }
                _ => {
                    arg.push(ch);
                }
            }
        }
    }
}
