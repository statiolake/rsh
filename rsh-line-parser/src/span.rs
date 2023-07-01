use std::{fmt, hash, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn is_followed_by(self, other: Self) -> bool {
        self.end == other.start
    }

    pub fn merged(self, other: Self) -> Self {
        debug_assert!(self.is_followed_by(other));
        Self {
            start: self.start,
            end: other.end,
        }
    }

    pub fn start_point(self) -> Span {
        Self {
            start: self.start,
            end: self.start,
        }
    }

    pub fn end_point(self) -> Span {
        Self {
            start: self.end,
            end: self.end,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Clone, Copy, PartialOrd, Ord)]
pub struct Spanned<'b, T> {
    pub source: &'b [char],
    pub data: T,
    pub span: Span,
}

impl<T: PartialEq> PartialEq for Spanned<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.span == other.span
    }
}

impl<T: Eq> Eq for Spanned<'_, T> {}

impl<T: hash::Hash> hash::Hash for Spanned<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
        self.span.hash(state);
    }
}

impl<'b, T> Spanned<'b, T> {
    pub fn new(source: &'b [char], span: Span, data: T) -> Self {
        Self { source, span, data }
    }

    pub fn map<F, U>(self, f: F) -> Spanned<'b, U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            source: self.source,
            span: self.span,
            data: f(self.data),
        }
    }
}

impl<'b, T: fmt::Debug> fmt::Debug for Spanned<'b, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Spanned")
            .field("data", &self.data)
            .field("span", &self.span)
            .finish()
    }
}
