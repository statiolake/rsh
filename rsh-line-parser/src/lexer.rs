use crate::span::{Span, Spanned};

pub const ESCAPE_CHAR: char = '^';
pub const SHOULD_ESCAPE_CHAR: [char; 11] = [' ', '$', '(', ')', '{', '}', '|', '<', '>', '"', '\''];

pub enum TokenKind {
    /// Normal character.
    Char(char),

    // Quotes.
    SingleQuoteStart,
    SingleQuoteEnd,
    DoubleQuoteStart,
    DoubleQuoteEnd,

    // Environment variables.
    EnvVarStart,
    EnvVarEnd,

    // Child command invocations.
    ChildStart,
    ChildEnd,

    /// Argument delimiter. Usually an whitespace outside of quotes.
    Delim,

    // Redirects.
    RedirectStdin,
    RedirectStdout,
    RedirectStderr,
    RedirectReferenceStdout,
    RedirectReferenceStderr,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("found '{}' but no following character found", ESCAPE_CHAR)]
    NoEscapedChar,

    #[error("invalid escape sequence '{}{}'", ESCAPE_CHAR, ch)]
    InvalidEscapeSequence { ch: char },

    #[error("child invocation is not ended")]
    UnbalancedChildInvocation,

    #[error("environment variable is not ended")]
    UnbalancedEnvVarBrace,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type Token = Spanned<TokenKind>;

pub struct Lexer<'a> {
    source: &'a [char],
    current: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self { source, current: 0 }
    }

    pub fn tokenize(&mut self, child_level: usize) -> Result<Vec<Token>> {
        let mut res = Vec::new();

        // Skip initial whitespace.
        self.skip_whitespace();

        let mut in_double = false;
        let mut in_single = false;
        while let Some(ch) = self.peek() {
            // Break if nested child seems to be ended
            if ch == ')' && child_level > 0 {
                break;
            }

            match ch {
                ESCAPE_CHAR => res.extend(self.tokenize_escape_sequence()?),
                '"' if !in_single => {
                    let span = self.eat(Some('"'));
                    in_double = !in_double;
                    if in_double {
                        res.push(Token {
                            data: TokenKind::DoubleQuoteStart,
                            span: Some(span),
                        });
                    } else {
                        res.push(Token {
                            data: TokenKind::DoubleQuoteEnd,
                            span: Some(span),
                        });
                    }
                }
                '\'' if !in_double => {
                    let span = self.eat(Some('\''));
                    in_single = !in_single;
                    if in_single {
                        res.push(Token {
                            data: TokenKind::SingleQuoteStart,
                            span: Some(span),
                        });
                    } else {
                        res.push(Token {
                            data: TokenKind::SingleQuoteEnd,
                            span: Some(span),
                        });
                    }
                }
                ch if in_single => {
                    let span = self.eat(Some(ch));
                    res.push(Token {
                        data: TokenKind::Char(ch),
                        span: Some(span),
                    });
                }
                '$' => match self.lookahead(1) {
                    Some('(') => {
                        let span = self.eat("$(".chars());
                        res.push(Token {
                            data: TokenKind::ChildStart,
                            span: Some(span),
                        });
                        res.extend(self.tokenize(child_level + 1)?);
                        if self.peek() != Some(')') {
                            return Err(Error::UnbalancedChildInvocation);
                        }
                        res.push(Token {
                            data: TokenKind::ChildEnd,
                            span: Some(self.eat(Some(')'))),
                        });
                    }
                    _ => res.extend(self.tokenize_envvar()?),
                },
            }
        }

        Ok(res)
    }

    fn tokenize_escape_sequence(&mut self) -> Result<impl IntoIterator<Item = Token>> {
        match self.lookahead(1) {
            None => Err(Error::NoEscapedChar),
            Some(ESCAPE_CHAR) => {
                let span = self.eat([ESCAPE_CHAR, ESCAPE_CHAR]);
                Ok(Some(Token {
                    data: TokenKind::Char(ESCAPE_CHAR),
                    span: Some(span),
                }))
            }
            Some(ch) if SHOULD_ESCAPE_CHAR.contains(&ch) => {
                let span = self.eat([ESCAPE_CHAR, ch]);
                Ok(Some(Token {
                    data: TokenKind::Char(ch),
                    span: Some(span),
                }))
            }
            Some('n') => {
                let span = self.eat([ESCAPE_CHAR, 'n']);
                Ok(Some(Token {
                    data: TokenKind::Char('\n'),
                    span: Some(span),
                }))
            }
            Some('t') => {
                let span = self.eat([ESCAPE_CHAR, 't']);
                Ok(Some(Token {
                    data: TokenKind::Char('\t'),
                    span: Some(span),
                }))
            }
            Some(ch) => Err(Error::InvalidEscapeSequence { ch }),
        }
    }

    fn tokenize_envvar(&mut self) -> Result<impl IntoIterator<Item = Token>> {
        let mut res = Vec::new();

        let (span, brace) = match self.lookahead(1) {
            Some('{') => (self.eat("${".chars()), true),
            _ => (self.eat(Some('$')), false),
        };
        res.push(Token {
            data: TokenKind::EnvVarStart,
            span: Some(span),
        });

        while let Some(ch) = self.peek() {
            match ch {
                '}' if brace => break,
                ch if brace || ch.is_ascii_alphabetic() || ch == '_' => res.push(Token {
                    data: TokenKind::Char(ch),
                    span: Some(self.eat(Some(ch))),
                }),
                _ => break,
            }
        }

        let span = if brace {
            if self.peek() != Some('}') {
                return Err(Error::UnbalancedEnvVarBrace);
            }
            Some(self.eat(Some('}')))
        } else {
            None
        };

        res.push(Token {
            data: TokenKind::EnvVarEnd,
            span,
        });

        Ok(res)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.eat(Some(ch));
            } else {
                return;
            }
        }
    }

    fn eat(&mut self, expected: impl IntoIterator<Item = char>) -> Span {
        let start = self.current;
        for ch in expected {
            assert_eq!(
                self.peek(),
                Some(ch),
                "internal error: unexpected character: {}",
                ch
            );
            self.current += 1;
        }
        let end = self.current;
        Span { start, end }
    }

    fn peek(&self) -> Option<char> {
        self.lookahead(0)
    }

    fn lookahead(&self, n: usize) -> Option<char> {
        self.source.get(self.current + n).copied()
    }

    // fn span(&self, lookahead: usize) -> Span {
    //     Span {
    //         start: self.current,
    //         end: self.current + lookahead + 1,
    //     }
    // }
}
