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
    ArgDelim,

    /// Redirect (like <). RedirectReferenceKind is Some() when there's redirect reference (like
    /// &1), otherwise None. When None, succeeding Atoms should be treated as file name.
    Redirect(Redirect),

    /// Pipe operator (`|`).
    Pipe,
}

#[derive(Debug, Clone)]
pub struct SingleQuoted(pub Vec<char>);
impl From<SingleQuoted> for TokenKind {
    fn from(v: SingleQuoted) -> Self {
        TokenKind::SingleQuoted(v)
    }
}

#[derive(Debug, Clone)]
pub struct DoubleQuoted(pub Vec<AtomKind>);
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
pub struct EnvVar(pub String);
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
pub struct SubShell(pub Vec<Token>);
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

#[derive(Debug, Clone, Copy)]
pub enum RedirectReferenceKind {
    Stdout,
    Stderr,
}

impl RedirectKind {
    pub fn is_output(self) -> bool {
        matches!(self, RedirectKind::Stdout | RedirectKind::Stderr)
    }
}

#[derive(Debug, Clone)]
pub struct Redirect {
    pub kind: RedirectKind,
    pub reference: Option<RedirectReferenceKind>,
}
impl From<Redirect> for TokenKind {
    fn from(v: Redirect) -> Self {
        Self::Redirect(v)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("found '{}' but no following character found", ESCAPE_CHAR)]
    NoEscapedChar { span: Span },

    #[error("invalid escape sequence '{}{}'", ESCAPE_CHAR, ch)]
    InvalidEscapeSequence { span: Span, ch: char },

    #[error("no environment variable or subshell invocation")]
    NoEnvVarOrSubShell { span: Span },

    #[error("subshell invocation is not ended")]
    UnbalancedSubShell { span: Span },

    #[error("environment variable is not ended")]
    UnbalancedEnvVarBrace { span: Span },

    #[error("quoted string is not ended")]
    UnbalancedQuotedString { span: Span },
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Error::NoEscapedChar { span } => *span,
            Error::InvalidEscapeSequence { span, .. } => *span,
            Error::NoEnvVarOrSubShell { span } => *span,
            Error::UnbalancedSubShell { span } => *span,
            Error::UnbalancedEnvVarBrace { span } => *span,
            Error::UnbalancedQuotedString { span } => *span,
        }
    }
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
                data: TokenKind::ArgDelim,
                span,
            });
        };

        macro_rules! into {
            ($e:expr) => {
                $e.map(|v| v.map(Into::into))
            };
        }

        match self.peek_rest() {
            [] => panic!("internal error: no token found"),
            [ESCAPE_CHAR, ..] => into!(self.next_escaped_sequence()),
            ['\'', ..] => into!(self.next_single_quoted()),
            ['"', ..] => into!(self.next_double_quoted()),
            ['>' | '<', ..] | ['1' | '2', '>', ..] => into!(self.next_redirect()),
            ['|', ..] => into!(self.next_delim()),
            _ => into!(self.next_atom()),
        }
    }

    fn next_single_quoted(&mut self) -> Result<Spanned<SingleQuoted>> {
        let mut span = self.eat(['\'']);
        let mut chars = Vec::new();
        while let Some(ch) = self.peek() {
            match ch {
                '\'' => {
                    span = span.merged(self.eat([ch]));
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

        Err(Error::UnbalancedQuotedString { span })
    }

    fn next_double_quoted(&mut self) -> Result<Spanned<DoubleQuoted>> {
        let mut span = self.eat(['"']);
        let mut atoms = Vec::new();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    span = span.merged(self.eat([ch]));
                    return Ok(Spanned {
                        data: DoubleQuoted(atoms),
                        span,
                    });
                }
                _ => {
                    let atom = self.next_atom()?;
                    span = span.merged(atom.span);
                    atoms.push(atom.data);
                }
            }
        }

        Err(Error::UnbalancedQuotedString { span })
    }

    fn next_redirect(&mut self) -> Result<Spanned<Redirect>> {
        let mut span = self.span_for_current_point();
        let kind = match self.peek_rest() {
            ['>', ..] => {
                span = span.merged(self.eat(['>']));
                RedirectKind::Stdout
            }
            ['<', ..] => {
                span = span.merged(self.eat(['<']));
                RedirectKind::Stdin
            }
            ['1', '>', ..] => {
                span = span.merged(self.eat(['1', '>']));
                RedirectKind::Stdout
            }
            ['2', '>', ..] => {
                span = span.merged(self.eat(['2', '>']));
                RedirectKind::Stderr
            }
            _ => panic!("internal error: tried to parse non-redirect"),
        };

        let reference = match self.peek_rest() {
            ['&', '1'] if kind.is_output() => {
                span = span.merged(self.eat(['&', '1']));
                Some(RedirectReferenceKind::Stdout)
            }
            ['&', '2'] if kind.is_output() => {
                span = span.merged(self.eat(['&', '2']));
                Some(RedirectReferenceKind::Stderr)
            }
            _ => None,
        };

        Ok(Spanned {
            data: Redirect { kind, reference },
            span,
        })
    }

    fn next_delim(&mut self) -> Result<Token> {
        match self.peek() {
            Some('|') => {
                let span = self.eat(['|']);
                Ok(Token {
                    data: TokenKind::Pipe,
                    span,
                })
            }
            Some(ch) => panic!("internal error: unknown delimiter `{}`", ch),
            None => panic!("internal error: no delimiter"),
        }
    }

    fn next_escaped_sequence(&mut self) -> Result<Spanned<char>> {
        assert!(
            self.peek() == Some(ESCAPE_CHAR),
            "internal error: escaped sequence not starting with `{}`",
            ESCAPE_CHAR
        );

        match self.lookahead(1) {
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
                Err(Error::InvalidEscapeSequence { span, ch })
            }
            None => Err(Error::NoEscapedChar {
                span: self.eat([ESCAPE_CHAR]),
            }),
        }
    }

    fn next_char(&mut self) -> Result<Spanned<char>> {
        match self.peek() {
            Some(ESCAPE_CHAR) => self.next_escaped_sequence(),
            Some(ch) => {
                let span = self.eat([ch]);
                Ok(Spanned { data: ch, span })
            }
            None => panic!("internal error: no next character"),
        }
    }

    fn next_ascii_word(&mut self) -> Result<Spanned<String>> {
        let mut span = self.span_for_current_point();
        let mut word = String::new();
        while let Some(ch) = self.peek() {
            if !ch.is_ascii_alphanumeric() {
                break;
            }
            span = span.merged(self.eat([ch]));
            word.push(ch);
        }

        Ok(Spanned { data: word, span })
    }

    fn next_subshell(&mut self) -> Result<Spanned<SubShell>> {
        let mut span = self.eat(['$', '(']);
        self.subshell_level += 1;

        let tokens = self.tokenize()?;
        for token in &tokens {
            span = span.merged(token.span);
        }
        if self.peek() != Some(')') {
            return Err(Error::UnbalancedSubShell { span });
        }
        span = span.merged(self.eat([')']));

        self.subshell_level -= 1;

        Ok(Spanned {
            data: SubShell(tokens),
            span,
        })
    }

    fn next_envvar(&mut self) -> Result<Spanned<EnvVar>> {
        let mut span = self.span_for_current_point();
        let varname = match self.peek_rest() {
            ['$', '{', ..] => {
                span = span.merged(self.eat(['$', '{']));
                let varname = self.next_ascii_word()?;
                span = span.merged(varname.span);
                if self.peek() != Some('}') {
                    return Err(Error::UnbalancedEnvVarBrace { span });
                }
                span = span.merged(self.eat(['}']));
                varname.data
            }
            ['$', ..] => {
                span = span.merged(self.eat(['$']));
                let varname = self.next_ascii_word()?;
                span = span.merged(varname.span);
                varname.data
            }
            _ => panic!("internal error: envvar not starting with `$`"),
        };

        Ok(Spanned {
            data: EnvVar(varname),
            span,
        })
    }

    fn next_atom(&mut self) -> Result<Spanned<AtomKind>> {
        macro_rules! into {
            ($e:expr) => {
                $e.map(|v| v.map(|v| v.into()))
            };
        }
        match self.peek_rest() {
            ['$', '(', ..] => into!(self.next_subshell()),
            ['$', ..] => into!(self.next_envvar()),
            [ESCAPE_CHAR, ..] => into!(self.next_escaped_sequence()),
            _ => into!(self.next_char()),
        }
    }

    fn skip_whitespace(&mut self) -> Option<Span> {
        let mut span: Option<Span> = None;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                let ch_span = self.eat([ch]);
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
                "internal error: unexpected character `{}`",
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

    fn peek_rest(&self) -> &[char] {
        &self.source[self.current..]
    }

    // fn span(&self, lookahead: usize) -> Span {
    //     Span {
    //         start: self.current,
    //         end: self.current + lookahead + 1,
    //     }
    // }
}
