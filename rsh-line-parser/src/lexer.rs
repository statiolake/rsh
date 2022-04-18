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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    /// Command delimiter (`;`)
    Delim,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SingleQuoted(pub Vec<char>);
impl From<SingleQuoted> for TokenKind {
    fn from(v: SingleQuoted) -> Self {
        TokenKind::SingleQuoted(v)
    }
}
impl From<Vec<char>> for SingleQuoted {
    fn from(v: Vec<char>) -> Self {
        Self(v)
    }
}
impl From<&[char]> for SingleQuoted {
    fn from(v: &[char]) -> Self {
        Self(v.to_vec())
    }
}
impl From<&str> for SingleQuoted {
    fn from(v: &str) -> Self {
        Self(v.chars().collect())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DoubleQuoted(pub Vec<AtomKind>);
impl From<DoubleQuoted> for TokenKind {
    fn from(v: DoubleQuoted) -> Self {
        TokenKind::DoubleQuoted(v)
    }
}
impl From<Vec<AtomKind>> for DoubleQuoted {
    fn from(v: Vec<AtomKind>) -> Self {
        DoubleQuoted(v)
    }
}
impl From<&[AtomKind]> for DoubleQuoted {
    fn from(v: &[AtomKind]) -> Self {
        DoubleQuoted(v.to_vec())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
impl From<&str> for EnvVar {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
impl From<&[Token]> for SubShell {
    fn from(v: &[Token]) -> Self {
        Self(v.to_vec())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum RedirectKind {
    Stdin,
    Stdout,
    Stderr,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum RedirectReferenceKind {
    Stdout,
    Stderr,
}

impl RedirectKind {
    pub fn is_output(self) -> bool {
        matches!(self, RedirectKind::Stdout | RedirectKind::Stderr)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Redirect {
    pub kind: RedirectKind,
    pub reference: Option<RedirectReferenceKind>,
}
impl From<Redirect> for TokenKind {
    fn from(v: Redirect) -> Self {
        Self::Redirect(v)
    }
}
impl From<RedirectKind> for Redirect {
    fn from(kind: RedirectKind) -> Self {
        Self {
            kind,
            reference: None,
        }
    }
}
impl From<(RedirectKind, RedirectReferenceKind)> for Redirect {
    fn from((kind, reference): (RedirectKind, RedirectReferenceKind)) -> Self {
        Self {
            kind,
            reference: Some(reference),
        }
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

        while let Some(ch) = self.peek() {
            if self.subshell_level > 0 && ch == ')' {
                // If tokenize in subshell, stop tokenizing at ')'.
                break;
            }

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
            ['|' | ';', ..] => into!(self.next_delim()),
            _ => into!(self.next_atom()),
        }
    }

    /// Tokenize as if it is quoted. Usually called when tokenizing subshell invocation inside the
    /// double quote.
    ///
    /// ## Note
    ///
    /// In this mode, everything is treated as simple string (except for whitespace; when `delim` is
    /// `true`, whitespaces are treated as ArgDelim, otherwise just a whitespace character.) For
    /// example, environmental variables substitution or subshell invocation won't be parsed.
    ///
    /// ## Examples
    ///
    /// ```console
    /// > echo $(echo '$(ls)')
    /// $(ls)
    /// ```
    ///
    /// At first lowering the command line text will be `echo $(ls)`, but it's not subshell
    /// invocation here. Much like `echo '$(ls)'`.
    pub fn partial_tokenize(&mut self, delim: bool) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.peek().is_some() {
            tokens.push(self.partial_next_token(delim)?);
        }

        Ok(tokens)
    }

    pub fn partial_next_token(&mut self, delim: bool) -> Result<Token> {
        if delim {
            if let Some(span) = self.skip_whitespace() {
                return Ok(Token {
                    data: TokenKind::ArgDelim,
                    span,
                });
            }
        }

        self.next_char().map(|tok| tok.map(Into::into))
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
            Some(';') => {
                let span = self.eat([';']);
                Ok(Token {
                    data: TokenKind::Delim,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    macro_rules! atom {
        (ss $ss:expr) => {
            AtomKind::from(SubShell::from(&$ss[..]))
        };
        (env $env:expr) => {
            AtomKind::from(EnvVar::from(&*$env))
        };
        ($atom:expr) => {
            AtomKind::from($atom)
        };
    }

    macro_rules! tok {
        ($range:expr, atom $($atom:tt)*) => {
            Token {
                data: atom!($($atom)*).into(),
                span: $range.into(),
            }
        };
        ($range:expr, squote $squote:expr) => {
            Token {
                data: SingleQuoted::from(&*$squote).into(),
                span: $range.into(),
            }
        };
        ($range:expr, dquote $dquote:expr) => {
            Token {
                data: DoubleQuoted::from(&$dquote[..]).into(),
                span: $range.into(),
            }
        };
        ($range:expr, argd) => {
            Token {
                data: TokenKind::ArgDelim,
                span: $range.into(),
            }
        };
        ($range:expr, redir $redir:expr) => {
            Token {
                data: Redirect::from($redir).into(),
                span: $range.into(),
            }
        };
        ($range:expr, pipe) => {
            Token {
                data: TokenKind::Pipe,
                span: $range.into(),
            }
        };
    }

    fn tokenize_str(s: &str) -> Vec<Token> {
        Lexer::new(&s.chars().collect_vec())
            .tokenize()
            .expect("should parse")
    }

    #[test]
    fn single() {
        let tokens = tokenize_str("ls");
        let expected = [tok!(0..1, atom 'l'), tok!(1..2, atom 's')];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn single_arg() {
        let tokens = tokenize_str("ls -al");
        let expected = [
            tok!(0..1, atom 'l'),
            tok!(1..2, atom 's'),
            tok!(2..3, argd),
            tok!(3..4, atom '-'),
            tok!(4..5, atom 'a'),
            tok!(5..6, atom 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn duplicate_whitespace() {
        let tokens = tokenize_str("ls   -al");
        let expected = [
            tok!(0..1, atom 'l'),
            tok!(1..2, atom 's'),
            tok!(2..5, argd),
            tok!(5..6, atom '-'),
            tok!(6..7, atom 'a'),
            tok!(7..8, atom 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn multiple_args() {
        let tokens = tokenize_str("ls a b c");
        let expected = [
            tok!(0..1, atom 'l'),
            tok!(1..2, atom 's'),
            tok!(2..3, argd),
            tok!(3..4, atom 'a'),
            tok!(4..5, argd),
            tok!(5..6, atom 'b'),
            tok!(6..7, argd),
            tok!(7..8, atom 'c'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn envvar() {
        let tokens = tokenize_str("echo $ABC def");
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..9, atom env "ABC"),
            tok!(9..10, argd),
            tok!(10..11, atom 'd'),
            tok!(11..12, atom 'e'),
            tok!(12..13, atom 'f'),
        ];
        assert_eq!(tokens, expected);

        let tokens = tokenize_str("echo ${ABC} def");
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..11, atom env "ABC"),
            tok!(11..12, argd),
            tok!(12..13, atom 'd'),
            tok!(13..14, atom 'e'),
            tok!(14..15, atom 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn subshell() {
        let tokens = tokenize_str("echo $(ls) def");
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..10, atom ss [
                tok!(7..8, atom 'l'),
                tok!(8..9, atom 's'),
            ]),
            tok!(10..11, argd),
            tok!(11..12, atom 'd'),
            tok!(12..13, atom 'e'),
            tok!(13..14, atom 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn subshell_args() {
        let tokens = tokenize_str("echo $(ls -al) def");
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..14, atom ss [
                tok!(7..8, atom 'l'),
                tok!(8..9, atom 's'),
                tok!(9..10, argd),
                tok!(10..11, atom '-'),
                tok!(11..12, atom 'a'),
                tok!(12..13, atom 'l'),
            ]),
            tok!(14..15, argd),
            tok!(15..16, atom 'd'),
            tok!(16..17, atom 'e'),
            tok!(17..18, atom 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn subsubshell() {
        let tokens = tokenize_str("echo $(ls $(ls)) def");
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..16, atom ss [
                tok!(7..8, atom 'l'),
                tok!(8..9, atom 's'),
                tok!(9..10, argd),
                tok!(10..15, atom ss [
                    tok!(12..13, atom 'l'),
                    tok!(13..14, atom 's'),
                ]),
            ]),
            tok!(16..17, argd),
            tok!(17..18, atom 'd'),
            tok!(18..19, atom 'e'),
            tok!(19..20, atom 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn dquote() {
        let tokens = tokenize_str(r#"echo "a b""#);
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..10, dquote [
                atom!('a'),
                atom!(' '),
                atom!('b'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn dquote_envvar() {
        let tokens = tokenize_str(r#"echo "a $var c""#);
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..15, dquote [
                atom!('a'),
                atom!(' '),
                atom!(env "var"),
                atom!(' '),
                atom!('c'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn squote() {
        let tokens = tokenize_str(r#"echo 'a b'"#);
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..10, squote "a b"),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn squote_envvar() {
        let tokens = tokenize_str(r#"echo 'a $var c'"#);
        let expected = [
            tok!(0..1, atom 'e'),
            tok!(1..2, atom 'c'),
            tok!(2..3, atom 'h'),
            tok!(3..4, atom 'o'),
            tok!(4..5, argd),
            tok!(5..15, squote "a $var c"),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn redirect() {
        use RedirectKind as RK;
        let tokens = tokenize_str("ls > file.txt");
        let expected = [
            tok!(0..1, atom 'l'),
            tok!(1..2, atom 's'),
            tok!(2..3, argd),
            tok!(3..4, redir RK::Stdout),
            tok!(4..5, argd),
            tok!(5..6, atom 'f'),
            tok!(6..7, atom 'i'),
            tok!(7..8, atom 'l'),
            tok!(8..9, atom 'e'),
            tok!(9..10, atom '.'),
            tok!(10..11, atom 't'),
            tok!(11..12, atom 'x'),
            tok!(12..13, atom 't'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn redirect_reference() {
        use RedirectKind as RK;
        use RedirectReferenceKind as RRK;
        let tokens = tokenize_str("ls > file.txt 2>&1");
        let expected = [
            tok!(0..1, atom 'l'),
            tok!(1..2, atom 's'),
            tok!(2..3, argd),
            tok!(3..4, redir RK::Stdout),
            tok!(4..5, argd),
            tok!(5..6, atom 'f'),
            tok!(6..7, atom 'i'),
            tok!(7..8, atom 'l'),
            tok!(8..9, atom 'e'),
            tok!(9..10, atom '.'),
            tok!(10..11, atom 't'),
            tok!(11..12, atom 'x'),
            tok!(12..13, atom 't'),
            tok!(13..14, argd),
            tok!(14..18, redir(RK::Stderr, RRK::Stdout)),
        ];
        assert_eq!(tokens, expected);
    }
}
