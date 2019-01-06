use std::error;
use std::fmt;
use std::iter::Peekable;
use std::result;
use std::str::Chars;

use itertools::Itertools;

use crate::expr::Expr;

pub const ESCAPE_CHAR: char = '`';

struct State {
    double_quote: bool,
    single_quote: bool,
    escaped: bool,
}

impl State {
    fn new() -> State {
        State {
            double_quote: false,
            single_quote: false,
            escaped: false,
        }
    }

    fn next(&self) -> State {
        State {
            double_quote: self.double_quote,
            single_quote: self.single_quote,
            escaped: false,
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self)
    }
}

impl error::Error for ParseError {}

impl ParseError {
    #[allow(dead_code)]
    pub fn new(kind: ErrorKind) -> ParseError {
        ParseError { kind }
    }
}

#[derive(Debug)]
pub enum ErrorKind {}

pub type Result<T> = result::Result<T, ParseError>;

pub struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &str) -> Parser {
        Parser::new(s.chars().peekable())
    }
}

impl<'a> Parser<'a> {
    pub fn new(chars: Peekable<Chars<'a>>) -> Parser {
        Parser { chars }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.parse_fncall()
    }

    fn parse_inner(&mut self) -> Result<Expr> {
        if let Some('(') = self.chars.peek() {
            assert_eq!(self.chars.next(), Some('('), "logic error");
            let expr = self.parse_fncall();
            assert_eq!(self.chars.next(), Some(')'), "incorrect close delimiter");
            expr
        } else {
            self.parse_literal()
        }
    }

    fn parse_fncall(&mut self) -> Result<Expr> {
        // drop delimiter whitespaces
        fn drop_whitespace_chars(chars: &mut Peekable<Chars>) {
            chars
                .peeking_take_while(|&ch| Parser::is_whitespace(ch))
                .for_each(drop);
        }

        let mut expr = Vec::new();
        while let Some(ch) = self.chars.peek() {
            match ch {
                ')' => break,
                _ => {
                    expr.push(self.parse_inner()?);
                    drop_whitespace_chars(&mut self.chars);
                }
            }
        }

        Ok(Expr::FnCall(expr))
    }

    fn parse_literal(&mut self) -> Result<Expr> {
        let mut state = State::new();
        let mut literal = String::new();
        while let Some(&ch) = self.chars.peek() {
            let whitespace = Parser::is_whitespace(ch);

            let mut next_state = state.next();
            match ch {
                // when escaping current char
                ch if state.escaped => literal.push(ch),

                // when escape character
                ESCAPE_CHAR => next_state.escaped = true,

                // when double-quote character in single-quoted string --- treat it as ordinal
                // character
                '\"' if state.single_quote => literal.push('\"'),

                // when double-quote character elsewhere --- beginning or ending of double-quoted
                // string
                '\"' => next_state.double_quote = !state.double_quote,

                // when single-quote character in double-quoted string --- treat it as ordinal
                // character
                '\'' if state.double_quote => literal.push('\''),

                // when single-quote character elsewhere --- beginning or ending of single-quoted
                // string
                '\'' => next_state.single_quote = !state.single_quote,

                // other ordinal characters in quotation
                ch if state.double_quote || state.single_quote => literal.push(ch),

                // other whitespace : delimiter
                _ if whitespace => break,

                // close parenthesis : delimiter
                ')' => break,

                // other ordinal characters
                ch => literal.push(ch),
            }

            // drop current char
            let _ = self.chars.next();
            state = next_state;
        }

        assert!(!state.double_quote, "double quote not ended.");
        assert!(!state.single_quote, "single quote not ended.");
        Ok(Expr::Literal(literal))
    }

    fn is_whitespace(ch: char) -> bool {
        " \t\r\n".chars().find(|&c| c == ch).is_some()
    }
}
