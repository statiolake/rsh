use std::error;
use std::fmt;
use std::result;

/// Result type representing parse related result
pub type Result<T> = result::Result<T, ParseError>;

/// represents errors in parsing
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
    /// creates new instance of ParseError
    pub fn new(kind: ErrorKind) -> ParseError {
        ParseError { kind }
    }
}

/// a kind of Error that may appear in ParseError
#[derive(Debug)]
pub enum ErrorKind {
    OpenParenthesisIsNotJustAfterWhitespace,

    DoubleQuoteNotEnded,
    SingleQuoteNotEnded,
}
