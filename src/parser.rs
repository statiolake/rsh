use std::mem;
use std::result;
use std::str::Chars;
use std::fmt;
use std::error;

use expr::Expr;

pub const ESCAPE_CHAR: char = '`';

#[derive(Clone)]
struct State {
    double_quote: bool,
    single_quote: bool,
    escaped: bool,
    just_after_whitespace: bool,
}

impl State {
    fn new() -> State {
        State {
            double_quote: false,
            single_quote: false,
            escaped: false,
            just_after_whitespace: false,
        }
    }

    fn next(&self) -> State {
        State {
            double_quote: self.double_quote,
            single_quote: self.single_quote,
            escaped: false,
            just_after_whitespace: false,
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
    pub fn new(kind: ErrorKind) -> ParseError {
        ParseError { kind }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    OpenParenthesisIsNotJustAfterWhitespace,

    DoubleQuoteNotEnded,
    SingleQuoteNotEnded,
}

pub type Result<T> = result::Result<T, ParseError>;

pub struct Parser<'a> {
    chars: Chars<'a>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &str) -> Parser {
        Parser { chars: s.chars() }
    }
}

impl<'a> Parser<'a> {
    pub fn new(chars: Chars<'a>) -> Parser {
        Parser { chars }
    }

    pub fn parse<'b>(&'b mut self) -> Result<Expr> {
        self.parse_inner().map(|x| x.0)
    }

    fn parse_inner<'b>(&'b mut self) -> Result<(Expr, Chars<'a>)> {
        let mut state_mut = State::new();

        let mut expr = Vec::new();
        let mut arg = String::new();
        while let Some(ch) = self.chars.next() {
            let state = state_mut;
            let mut next_state = state.next();

            let ch_is_whitespace = Parser::is_whitespace(ch);
            match ch {
                // when escaping current char
                ch if state.escaped => {
                    arg.push(ch);
                }

                // when whitespace in double/single-quoted string
                ch if ch_is_whitespace && state.double_quote || state.single_quote => {
                    arg.push(ch);
                }

                // other whitespace : delimiter
                _ if ch_is_whitespace => {
                    // if there's something in arg push it
                    if !arg.is_empty() {
                        expr.push(Expr::Literal(mem::replace(&mut arg, String::new())));
                    }
                    next_state.just_after_whitespace = true;
                }

                // when escape character
                ESCAPE_CHAR => {
                    next_state.escaped = true;
                }

                // when double-quote character in single-quoted string
                '\"' if state.single_quote => {
                    // treat it as ordinal character
                    arg.push('\"');
                }

                // when double-quote character elsewhere
                '\"' => {
                    // beginning or ending of double-quoted string
                    next_state.double_quote = !state.double_quote;
                }

                // when single-quote character in double-quoted string
                '\'' if state.double_quote => {
                    // treat it as ordinal character
                    arg.push('\'');
                }

                // when single-quote character elsewhere
                '\'' => {
                    // beginning or ending of single-quoted string
                    next_state.single_quote = !state.single_quote;
                }

                // when parenthesis in single/double-quoted string
                ch @ '(' | ch @ ')' if state.single_quote || state.double_quote => arg.push(ch),

                // when open-parenthesis elsewhere
                '(' => {
                    if !arg.is_empty() {
                        return Err(ParseError::new(
                            ErrorKind::OpenParenthesisIsNotJustAfterWhitespace,
                        ));
                    }
                    let (child_expr, new_chars) = Parser::new(self.chars.clone()).parse_inner()?;
                    self.chars = new_chars;
                    expr.push(child_expr);
                }

                // when close-parenthesis
                ')' => {
                    // push last argument AFTER breaking; nothing do here
                    state_mut = next_state;
                    break;
                }

                // other characters
                ch => {
                    arg.push(ch);
                }
            }
            state_mut = next_state;
        }

        if state_mut.double_quote {
            return Err(ParseError::new(ErrorKind::DoubleQuoteNotEnded));
        }

        if state_mut.single_quote {
            return Err(ParseError::new(ErrorKind::SingleQuoteNotEnded));
        }

        if !arg.is_empty() {
            expr.push(Expr::Literal(arg));
        }

        Ok((Expr::FnCall(expr), self.chars.clone()))
    }

    fn is_whitespace(ch: char) -> bool {
        " \t\r\n".chars().find(|&c| c == ch).is_some()
    }
}
