use std::ops::Range;

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
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, data: T) -> Self {
        Self { span, data }
    }

    pub fn map<F, U>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            data: f(self.data),
            span: self.span,
        }
    }
}
