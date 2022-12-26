use crate::span::Spanned;

pub type Token = Spanned<TokenKind>;
pub type FlattenedToken = Spanned<FlattenedTokenKind>;

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
    Substitution(Substitution),
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
pub struct Substitution(pub Vec<Token>);
impl From<Substitution> for TokenKind {
    fn from(v: Substitution) -> Self {
        Self::Atom(AtomKind::Substitution(v))
    }
}
impl From<Substitution> for AtomKind {
    fn from(v: Substitution) -> Self {
        Self::Substitution(v)
    }
}
impl From<&[Token]> for Substitution {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlattenedTokenKind {
    Atom(char),
    Quoted(Vec<char>),
    ArgDelim,
    Redirect(Redirect),
    Pipe,
    Delim,
}
