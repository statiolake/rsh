mod error;

pub use self::error::{ErrorKind, ParseError, Result};

use std::mem;
use std::str::Chars;

use expr::Expr;

pub const ESCAPE_CHAR: char = '`';

/// represents current parser state
#[derive(Clone)]
struct State {
    double_quote: bool,
    single_quote: bool,
    escaped: bool,
    just_after_whitespace: bool,
}

impl State {
    /// creates new instance of State
    fn new() -> State {
        State {
            double_quote: false,
            single_quote: false,
            escaped: false,
            just_after_whitespace: false,
        }
    }

    /// cretes next base state, according to current state
    /// for example, info about quotations will be inherited to next,
    /// but info about escapeness will reset to false every time.
    fn next(&self) -> State {
        State {
            double_quote: self.double_quote,
            single_quote: self.single_quote,
            escaped: false,
            just_after_whitespace: false,
        }
    }
}

struct ParserStates {
    curr_state: State,
    next_state: State,
    expr: Vec<Expr>,
    arg: String,
}

impl ParserStates {
    pub fn new() -> ParserStates {
        ParserStates {
            curr_state: State::new(),
            next_state: State::new(),
            expr: Vec::new(),
            arg: String::new(),
        }
    }
}

/// parser for command line input
pub struct Parser<'a> {
    chars: Chars<'a>,
}

/// parser can be built from &str because it has .chars() method
impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &str) -> Parser {
        Parser { chars: s.chars() }
    }
}

impl<'a> Parser<'a> {
    /// creates new instance
    pub fn new(chars: Chars<'a>) -> Parser {
        Parser { chars }
    }

    /// parses command line input; it is easy-to-use interface
    pub fn parse<'b>(&'b mut self) -> Result<Expr> {
        self.parse_inner().map(|x| x.0)
    }

    /// parses command line input in practice
    fn parse_inner<'b>(&'b mut self) -> Result<(Expr, Chars<'a>)> {
        let mut states = ParserStates::new();

        // main parse loop
        while let Some(ch) = self.chars.next() {
            if !self.handle_char(ch, &mut states)? {
                break;
            }
        }

        // error check : double quote not closed
        if states.curr_state.double_quote {
            return Err(ParseError::new(ErrorKind::DoubleQuoteNotEnded));
        }

        // error check : single quote not closed
        if states.curr_state.single_quote {
            return Err(ParseError::new(ErrorKind::SingleQuoteNotEnded));
        }

        // push last argument if needed
        if !states.arg.is_empty() {
            states.expr.push(Expr::Literal(states.arg));
        }

        Ok((Expr::FnCall(states.expr), self.chars.clone()))
    }

    /// handle ch, returns (whether continues loop, next state)
    fn handle_char(&mut self, ch: char, states: &mut ParserStates) -> Result<bool> {
        // states.next_state is already prepared for next_state in previous iteration of loop
        // in the last ofthis function.
        // at first states.next_state is same with State::new(), but this is desired work.

        let ch_is_whitespace = " \t\r\n".chars().find(|&c| c == ch).is_some();

        let mut cont = true;
        match ch {
            // when current char is escaped
            ch if states.curr_state.escaped => states.arg.push(ch),

            // when whitespace
            ch if ch_is_whitespace => self.handle_whitespace(ch, states),

            // when escape character
            ESCAPE_CHAR => states.next_state.escaped = true,

            // when double-quote character
            '\"' => self.handle_double_quote(states),

            // when single-quote character
            '\'' => self.handle_single_quote(states),

            // when parenthesis in single/double-quoted string
            ch @ '(' | ch @ ')'
                if states.curr_state.single_quote || states.curr_state.double_quote =>
            {
                states.arg.push(ch)
            }

            // when open-parenthesis elsewhere
            '(' => self.handle_fncall_open_parenthesis(states)?,

            // when close-parenthesis
            ')' => cont = false,

            // other characters
            ch => states.arg.push(ch),
        }
        let next_next_state = states.next_state.next();
        states.curr_state = mem::replace(&mut states.next_state, next_next_state);

        Ok(cont)
    }

    fn handle_whitespace(&self, ch: char, states: &mut ParserStates) {
        if states.curr_state.double_quote || states.curr_state.single_quote {
            // if in double_quote or single_quote
            // treat it as ordinal character
            states.arg.push(ch);
        } else {
            // push arg to expr if needed
            if !states.arg.is_empty() {
                let lit = Expr::Literal(mem::replace(&mut states.arg, String::new()));
                states.expr.push(lit);
            }
            states.next_state.just_after_whitespace = true;
        }
    }

    fn handle_double_quote(&self, states: &mut ParserStates) {
        if states.curr_state.single_quote {
            // when double-quote character in single-quoted string
            // treat it as ordinal character
            states.arg.push('\"');
        } else {
            // beginning or ending of double-quoted string
            states.next_state.double_quote = !states.curr_state.double_quote;
        }
    }

    fn handle_single_quote(&self, states: &mut ParserStates) {
        if states.curr_state.double_quote {
            // when double-quote character in single-quoted string
            // treat it as ordinal character
            states.arg.push('\'');
        } else {
            // beginning or ending of double-quoted string
            states.next_state.single_quote = !states.curr_state.single_quote;
        }
    }

    fn handle_fncall_open_parenthesis(&mut self, states: &mut ParserStates) -> Result<()> {
        if !states.arg.is_empty() {
            return Err(ParseError::new(
                ErrorKind::OpenParenthesisIsNotJustAfterWhitespace,
            ));
        }
        let (child_expr, new_chars) = Parser::new(self.chars.clone()).parse_inner()?;
        self.chars = new_chars;
        states.expr.push(child_expr);
        Ok(())
    }
}
