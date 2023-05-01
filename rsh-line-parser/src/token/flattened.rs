use super::{HasTokenKind, Redirect};

pub type FlattenedToken = FlattenedTokenKind;

pub type FlattenedTokenList = Vec<FlattenedToken>;

impl HasTokenKind for FlattenedToken {
    fn is_arg_delim(&self) -> bool {
        matches!(self, FlattenedTokenKind::ArgDelim)
    }

    fn is_redirect(&self) -> bool {
        matches!(self, FlattenedTokenKind::Redirect(_))
    }

    fn is_pipe(&self) -> bool {
        matches!(self, FlattenedTokenKind::Pipe)
    }

    fn is_delim(&self) -> bool {
        matches!(self, FlattenedTokenKind::Delim)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlattenedTokenKind {
    /// Normal atom.
    Atom(char),

    /// Single or double-quoted argument.
    Quoted(FlattenedQuoted),

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

impl FlattenedTokenKind {
    pub fn atom(&self) -> Option<char> {
        match self {
            Self::Atom(a) => Some(*a),
            _ => None,
        }
    }

    pub fn quoted(&self) -> Option<&FlattenedQuoted> {
        match self {
            Self::Quoted(s) => Some(s),
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
pub struct FlattenedQuoted(pub Vec<char>);

impl From<FlattenedQuoted> for FlattenedTokenKind {
    fn from(v: FlattenedQuoted) -> Self {
        Self::Quoted(v)
    }
}

impl From<Vec<char>> for FlattenedQuoted {
    fn from(v: Vec<char>) -> Self {
        FlattenedQuoted(v)
    }
}
