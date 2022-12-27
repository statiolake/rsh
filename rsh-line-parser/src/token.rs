use crate::span::Spanned;

pub type TokenBase<A> = Spanned<TokenKindBase<A>>;

pub type TokenKind = TokenKindBase<AtomKind>;
pub type Token = TokenBase<AtomKind>;
pub type FlattenedTokenKind = TokenKindBase<char>;
pub type FlattenedToken = TokenBase<char>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKindBase<A> {
    /// Normal atom.
    Atom(A),

    /// Single-quoted argument.
    SingleQuoted(SingleQuoted),

    /// Double-quoted argument.
    DoubleQuoted(DoubleQuoted<A>),

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

impl<A> From<SingleQuoted> for TokenKindBase<A> {
    fn from(v: SingleQuoted) -> Self {
        Self::SingleQuoted(v)
    }
}

impl From<Vec<char>> for SingleQuoted {
    fn from(v: Vec<char>) -> Self {
        Self(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DoubleQuoted<A>(pub Vec<A>);

impl<A> From<DoubleQuoted<A>> for TokenKindBase<A> {
    fn from(v: DoubleQuoted<A>) -> Self {
        Self::DoubleQuoted(v)
    }
}

impl<A> From<Vec<A>> for DoubleQuoted<A> {
    fn from(v: Vec<A>) -> Self {
        DoubleQuoted(v)
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

impl From<char> for AtomKind {
    fn from(v: char) -> Self {
        Self::Char(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnvVar(pub String);

impl From<EnvVar> for AtomKind {
    fn from(v: EnvVar) -> Self {
        Self::EnvVar(v)
    }
}

impl From<String> for EnvVar {
    fn from(s: String) -> Self {
        Self(s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Substitution(pub Vec<Token>);

impl From<Substitution> for AtomKind {
    fn from(v: Substitution) -> Self {
        Self::Substitution(v)
    }
}

impl From<Vec<Token>> for Substitution {
    fn from(v: Vec<Token>) -> Self {
        Self(v)
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
    pub append: bool,
}

impl<A> From<Redirect> for TokenKindBase<A> {
    fn from(v: Redirect) -> Self {
        Self::Redirect(v)
    }
}

impl From<RedirectKind> for Redirect {
    fn from(kind: RedirectKind) -> Self {
        Self {
            kind,
            reference: None,
            append: false,
        }
    }
}

impl From<(RedirectKind, RedirectReferenceKind)> for Redirect {
    fn from((kind, reference): (RedirectKind, RedirectReferenceKind)) -> Self {
        Self {
            kind,
            reference: Some(reference),
            append: false,
        }
    }
}
