use crate::span::{Span, Spanned};

pub const ESCAPE_CHAR: char = '^';
pub const SHOULD_ESCAPE_CHAR: [char; 12] = [
    ESCAPE_CHAR,
    ' ',
    '$',
    '(',
    ')',
    '{',
    '}',
    '|',
    '<',
    '>',
    '"',
    '\'',
];

#[derive(Debug, Clone)]
pub enum TokenKind {
    /// Normal atom.
    Atom(AtomKind),

    /// Single-quoted argument.
    SingleQuoted(SingleQuoted),

    /// Double-quoted argument.
    DoubleQuoted(DoubleQuoted),

    /// Argument delimiter. Usually an whitespace (outside of quotes).
    Delim,

    /// Redirect (like <).
    Redirect(RedirectKind),

    /// Redirect reference (like &1).
    RedirectReference(RedirectReferenceKind),
}

#[derive(Debug, Clone)]
pub struct SingleQuoted(Vec<char>);
impl From<SingleQuoted> for TokenKind {
    fn from(v: SingleQuoted) -> Self {
        TokenKind::SingleQuoted(v)
    }
}

#[derive(Debug, Clone)]
pub struct DoubleQuoted(Vec<AtomKind>);
impl From<DoubleQuoted> for TokenKind {
    fn from(v: DoubleQuoted) -> Self {
        TokenKind::DoubleQuoted(v)
    }
}

#[derive(Debug, Clone)]
pub enum AtomKind {
    /// Normal character.
    Char(char),

    /// Environmental variable.
    EnvVar(EnvVar),

    /// Sub shell invocation.
    SubShell(SubShell),
}
impl From<AtomKind> for TokenKind {
    fn from(v: AtomKind) -> Self {
        Self::Atom(v)
    }
}

impl From<char> for TokenKind {
    fn from(v: char) -> Self {
        Self::Atom(AtomKind::Char(v))
    }
}

impl From<char> for AtomKind {
    fn from(v: char) -> Self {
        Self::Char(v)
    }
}

#[derive(Debug, Clone)]
pub struct EnvVar(String);
impl From<EnvVar> for TokenKind {
    fn from(v: EnvVar) -> Self {
        Self::Atom(AtomKind::EnvVar(v))
    }
}
impl From<EnvVar> for AtomKind {
    fn from(v: EnvVar) -> Self {
        Self::EnvVar(v)
    }
}

#[derive(Debug, Clone)]
pub struct SubShell(Vec<Token>);
impl From<SubShell> for TokenKind {
    fn from(v: SubShell) -> Self {
        Self::Atom(AtomKind::SubShell(v))
    }
}
impl From<SubShell> for AtomKind {
    fn from(v: SubShell) -> Self {
        Self::SubShell(v)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RedirectKind {
    Stdin,
    Stdout,
    Stderr,
}
impl From<RedirectKind> for TokenKind {
    fn from(v: RedirectKind) -> Self {
        Self::Redirect(v)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RedirectReferenceKind {
    Stdout,
    Stderr,
}
impl From<RedirectReferenceKind> for TokenKind {
    fn from(v: RedirectReferenceKind) -> Self {
        Self::RedirectReference(v)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("found '{}' but no following character found", ESCAPE_CHAR)]
    NoEscapedChar(Span),

    #[error("invalid escape sequence '{}{}'", ESCAPE_CHAR, .0.data)]
    InvalidEscapeSequence(Spanned<char>),

    #[error("no environment variable or subshell invocation")]
    NoEnvVarOrSubShell(Span),

    #[error("subshell invocation is not ended")]
    UnbalancedSubShell(Span),

    #[error("environment variable is not ended")]
    UnbalancedEnvVarBrace(Span),

    #[error("quoted string is not ended")]
    UnbalancedQuotedString(Span),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub type Token = Spanned<TokenKind>;

pub struct Lexer<'a> {
    source: &'a [char],
    current: usize,

    subshell_level: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self {
            source,
            current: 0,
            subshell_level: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        // Get tokens while there is another token. "There is another token" if:
        // - there is another char when it is not subshell (subshell_level == 0), or
        // - there is another char other than ')' if it is subshell (subshell_level > 0).
        while matches!(self.peek(), Some(ch) if self.subshell_level == 0 || ch != ')') {
            tokens.push(self.next_token()?);
        }

        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if let Some(span) = self.skip_whitespace() {
            return Ok(Token {
                data: TokenKind::Delim,
                span,
            });
        };

        match self.peek().expect("internal error: no token found") {
            ESCAPE_CHAR => self.next_escaped_sequence().map(|v| v.map(Into::into)),
            '\'' => self.next_single_quoted().map(|v| v.map(Into::into)),
            '"' => self.next_double_quoted().map(|v| v.map(Into::into)),
            // TODO: redirects
            _ => self.next_atom().map(|v| v.map(Into::into)),
        }
    }

    fn next_single_quoted(&mut self) -> Result<Spanned<SingleQuoted>> {
        let mut span = self.eat(Some('\''));
        let mut chars = Vec::new();
        while let Some(ch) = self.peek() {
            match ch {
                '\'' => {
                    span = span.merged(self.eat(Some(ch)));
                    return Ok(Spanned {
                        data: SingleQuoted(chars),
                        span,
                    });
                }
                _ => {
                    let ch = self.next_char()?;
                    span = span.merged(ch.span);
                    chars.push(ch.data);
                }
            }
        }

        Err(Error::UnbalancedQuotedString(span))
    }

    fn next_double_quoted(&mut self) -> Result<Spanned<DoubleQuoted>> {
        let mut span = self.eat(Some('"'));
        let mut atoms = Vec::new();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    span = span.merged(self.eat(Some(ch)));
                    return Ok(Spanned {
                        data: DoubleQuoted(atoms),
                        span,
                    });
                }
                ESCAPE_CHAR => {
                    let esc = self.next_escaped_sequence()?;
                    span = span.merged(esc.span);
                    atoms.push(AtomKind::Char(esc.data));
                }
                ch => {
                    span = span.merged(self.eat(Some(ch)));
                    atoms.push(AtomKind::Char(ch));
                }
            }
        }

        Err(Error::UnbalancedQuotedString(span))
    }

    // fn next_envvar(&mut self) -> Result<impl IntoIterator<Item = Token>> {
    //     let mut res = Vec::new();
    //
    //     let (span, brace) = match self.lookahead(1) {
    //         Some('{') => (self.eat("${".chars()), true),
    //         _ => (self.eat(Some('$')), false),
    //     };
    //     res.push(Token {
    //         data: TokenKind::EnvVarStart,
    //         span,
    //     });
    //
    //     while let Some(ch) = self.peek() {
    //         match ch {
    //             '}' if brace => break,
    //             ch if brace || ch.is_ascii_alphabetic() || ch == '_' => res.push(Token {
    //                 data: TokenKind::Atom(ch),
    //                 span: Some(self.eat(Some(ch))),
    //             }),
    //             _ => break,
    //         }
    //     }
    //
    //     let span = if brace {
    //         if self.peek() != Some('}') {
    //             return Err(Error::UnbalancedEnvVarBrace);
    //         }
    //         Some(self.eat(Some('}')))
    //     } else {
    //         None
    //     };
    //
    //     res.push(Token {
    //         data: TokenKind::EnvVarEnd,
    //         span,
    //     });
    //
    //     Ok(res)
    // }

    fn next_escaped_sequence(&mut self) -> Result<Spanned<char>> {
        match self.lookahead(1) {
            None => Err(Error::NoEscapedChar(self.eat(Some(ESCAPE_CHAR)))),
            Some(ch) if SHOULD_ESCAPE_CHAR.contains(&ch) => {
                let span = self.eat([ESCAPE_CHAR, ch]);
                Ok(Spanned { data: ch, span })
            }
            Some('n') => {
                let span = self.eat([ESCAPE_CHAR, 'n']);
                Ok(Spanned { data: '\n', span })
            }
            Some('t') => {
                let span = self.eat([ESCAPE_CHAR, 't']);
                Ok(Spanned { data: '\t', span })
            }
            Some(ch) => {
                let span = self.eat([ESCAPE_CHAR, ch]);
                Err(Error::InvalidEscapeSequence(Spanned { data: ch, span }))
            }
        }
    }

    fn next_char(&mut self) -> Result<Spanned<char>> {
        match self.peek() {
            Some(ESCAPE_CHAR) => self.next_escaped_sequence(),
            Some(ch) => {
                let span = self.eat(Some(ch));
                Ok(Spanned { data: ch, span })
            }
            None => panic!("internal error: no next character"),
        }
    }

    fn next_word(&mut self) -> Result<Spanned<String>> {
        let mut span = self.span_for_current_point();
        let mut word = String::new();
        while let Some(ch) = self.peek() {
            if !ch.is_alphabetic() {
                break;
            }

            let ch_span = self.eat(Some(ch));
            span = span.merged(ch_span);
            word.push(ch);
        }

        Ok(Spanned { data: word, span })
    }

    fn next_subshell(&mut self) -> Result<Spanned<SubShell>> {
        let mut span = self.eat("$(".chars());
        self.subshell_level += 1;
        let tokens = self.tokenize()?;
        self.subshell_level -= 1;
        for token in &tokens {
            span = span.merged(token.span);
        }
        if self.peek() != Some(')') {
            return Err(Error::UnbalancedSubShell(span));
        }
        span = span.merged(self.eat(Some(')')));

        Ok(Spanned {
            data: SubShell(tokens),
            span,
        })
    }

    fn next_envvar(&mut self) -> Result<Spanned<EnvVar>> {
        let mut span = self.eat(Some('$'));
        let varname = match self.peek() {
            Some('{') => {
                span = span.merged(self.eat(Some('{')));
                let varname = self.next_word()?;
                span = span.merged(varname.span);
                if self.peek() != Some('}') {
                    return Err(Error::UnbalancedEnvVarBrace(span));
                }
                span = span.merged(self.eat(Some('}')));
                varname.data
            }
            _ => {
                let varname = self.next_word()?;
                span = span.merged(varname.span);
                varname.data
            }
        };

        Ok(Spanned {
            data: EnvVar(varname),
            span,
        })
    }

    fn next_atom(&mut self) -> Result<Spanned<AtomKind>> {
        match self.peek() {
            Some('$') => match self.lookahead(1) {
                Some('(') => self.next_subshell().map(|v| v.map(|v| v.into())),
                Some(_) => self.next_envvar().map(|v| v.map(|v| v.into())),
                None => {
                    let span = self.eat(Some('$'));
                    Err(Error::NoEnvVarOrSubShell(span))
                }
            },
            Some(ESCAPE_CHAR) => self.next_escaped_sequence().map(|v| v.map(|v| v.into())),
            None => todo!(),
        }
    }

    fn skip_whitespace(&mut self) -> Option<Span> {
        let mut span: Option<Span> = None;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                let ch_span = self.eat(Some(ch));
                span = match span {
                    Some(span) => Some(span.merged(ch_span)),
                    None => Some(ch_span),
                }
            } else {
                break;
            }
        }

        span
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

    fn span_for_current_point(&self) -> Span {
        Span {
            start: self.current,
            end: self.current,
        }
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
