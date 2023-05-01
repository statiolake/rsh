use crate::span::Spanned;

use super::HasTokenKind;

pub type Token = Spanned<TokenKind>;

pub type TokenList = Spanned<Vec<Token>>;

impl HasTokenKind for Token {
    fn is_arg_delim(&self) -> bool {
        matches!(self.data, TokenKind::ArgDelim)
    }

    fn is_redirect(&self) -> bool {
        matches!(self.data, TokenKind::Redirect(_))
    }

    fn is_pipe(&self) -> bool {
        matches!(self.data, TokenKind::Pipe)
    }

    fn is_delim(&self) -> bool {
        matches!(self.data, TokenKind::Delim)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Normal atom.
    Atom(Atom),

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

impl TokenKind {
    pub fn atom(&self) -> Option<&Atom> {
        match self {
            Self::Atom(a) => Some(a),
            _ => None,
        }
    }

    pub fn single_quoted(&self) -> Option<&SingleQuoted> {
        match self {
            Self::SingleQuoted(s) => Some(s),
            _ => None,
        }
    }

    pub fn double_quoted(&self) -> Option<&DoubleQuoted> {
        match self {
            Self::DoubleQuoted(s) => Some(s),
            _ => None,
        }
    }

    pub fn arg_delim(&self) -> Option<()> {
        match self {
            Self::ArgDelim => Some(()),
            _ => None,
        }
    }

    pub fn redirect(&self) -> Option<&Redirect> {
        match self {
            Self::Redirect(s) => Some(s),
            _ => None,
        }
    }

    pub fn pipe(&self) -> Option<()> {
        match self {
            Self::Pipe => Some(()),
            _ => None,
        }
    }

    pub fn delim(&self) -> Option<()> {
        match self {
            Self::Delim => Some(()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SingleQuoted(pub Spanned<Vec<Spanned<char>>>);

impl From<SingleQuoted> for TokenKind {
    fn from(v: SingleQuoted) -> Self {
        Self::SingleQuoted(v)
    }
}

impl From<Spanned<Vec<Spanned<char>>>> for SingleQuoted {
    fn from(v: Spanned<Vec<Spanned<char>>>) -> Self {
        Self(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DoubleQuoted(pub Spanned<Vec<Atom>>);

impl From<DoubleQuoted> for TokenKind {
    fn from(v: DoubleQuoted) -> Self {
        Self::DoubleQuoted(v)
    }
}

impl From<Spanned<Vec<Atom>>> for DoubleQuoted {
    fn from(v: Spanned<Vec<Atom>>) -> Self {
        DoubleQuoted(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Atom {
    /// Normal character.
    Char(Spanned<char>),

    /// Environmental variable.
    EnvVar(EnvVar),

    /// Sub shell invocation.
    Substitution(Substitution),
}

impl Atom {
    pub fn char(&self) -> Option<Spanned<char>> {
        match self {
            Self::Char(ch) => Some(*ch),
            _ => None,
        }
    }

    pub fn env_var(&self) -> Option<&EnvVar> {
        match self {
            Self::EnvVar(env_var) => Some(env_var),
            _ => None,
        }
    }

    pub fn substitution(&self) -> Option<&Substitution> {
        match self {
            Self::Substitution(subst) => Some(subst),
            _ => None,
        }
    }
}

impl From<Atom> for TokenKind {
    fn from(v: Atom) -> Self {
        Self::Atom(v)
    }
}

impl From<Spanned<char>> for Atom {
    fn from(v: Spanned<char>) -> Self {
        Self::Char(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnvVar(pub Spanned<Vec<Spanned<char>>>);

impl From<EnvVar> for Atom {
    fn from(v: EnvVar) -> Self {
        Self::EnvVar(v)
    }
}

impl From<Spanned<Vec<Spanned<char>>>> for EnvVar {
    fn from(s: Spanned<Vec<Spanned<char>>>) -> Self {
        Self(s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Substitution(pub Spanned<Vec<Token>>);

impl From<Substitution> for Atom {
    fn from(v: Substitution) -> Self {
        Self::Substitution(v)
    }
}

impl From<Spanned<Vec<Token>>> for Substitution {
    fn from(v: Spanned<Vec<Token>>) -> Self {
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
