use crate::span::Spanned;

use super::HasTokenKind;

pub type Token<'b> = Spanned<'b, TokenKind<'b>>;

pub type TokenList<'b> = Spanned<'b, Vec<Token<'b>>>;

impl HasTokenKind for Token<'_> {
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
pub enum TokenKind<'b> {
    /// Normal atom.
    Atom(Atom<'b>),

    /// Single-quoted argument.
    SingleQuoted(SingleQuoted<'b>),

    /// Double-quoted argument.
    DoubleQuoted(DoubleQuoted<'b>),

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

impl<'b> TokenKind<'b> {
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
pub struct SingleQuoted<'b>(pub Spanned<'b, Vec<Spanned<'b, char>>>);

impl<'b> From<SingleQuoted<'b>> for TokenKind<'b> {
    fn from(v: SingleQuoted<'b>) -> Self {
        Self::SingleQuoted(v)
    }
}

impl<'b> From<Spanned<'b, Vec<Spanned<'b, char>>>> for SingleQuoted<'b> {
    fn from(v: Spanned<'b, Vec<Spanned<'b, char>>>) -> Self {
        Self(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DoubleQuoted<'b>(pub Spanned<'b, Vec<Atom<'b>>>);

impl<'b> From<DoubleQuoted<'b>> for TokenKind<'b> {
    fn from(v: DoubleQuoted<'b>) -> Self {
        Self::DoubleQuoted(v)
    }
}

impl<'b> From<Spanned<'b, Vec<Atom<'b>>>> for DoubleQuoted<'b> {
    fn from(v: Spanned<'b, Vec<Atom<'b>>>) -> Self {
        DoubleQuoted(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Atom<'b> {
    /// Normal character.
    Char(Spanned<'b, char>),

    /// Environmental variable.
    EnvVar(EnvVar<'b>),

    /// Sub shell invocation.
    Substitution(Substitution<'b>),
}

impl<'b> Atom<'b> {
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

impl<'b> From<Atom<'b>> for TokenKind<'b> {
    fn from(v: Atom<'b>) -> Self {
        Self::Atom(v)
    }
}

impl<'b> From<Spanned<'b, char>> for Atom<'b> {
    fn from(v: Spanned<'b, char>) -> Self {
        Self::Char(v)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnvVar<'b>(pub Spanned<'b, Vec<Spanned<'b, char>>>);

impl<'b> From<EnvVar<'b>> for Atom<'b> {
    fn from(v: EnvVar<'b>) -> Self {
        Self::EnvVar(v)
    }
}

impl<'b> From<Spanned<'b, Vec<Spanned<'b, char>>>> for EnvVar<'b> {
    fn from(s: Spanned<'b, Vec<Spanned<'b, char>>>) -> Self {
        Self(s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Substitution<'b>(pub Spanned<'b, Vec<Token<'b>>>);

impl<'b> From<Substitution<'b>> for Atom<'b> {
    fn from(v: Substitution<'b>) -> Self {
        Self::Substitution(v)
    }
}

impl<'b> From<Spanned<'b, Vec<Token<'b>>>> for Substitution<'b> {
    fn from(v: Spanned<'b, Vec<Token<'b>>>) -> Self {
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

impl<'b> From<Redirect> for TokenKind<'b> {
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
