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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Spanned<'b, T> {
    pub source: &'b [char],
    pub data: T,
    pub span: Span,
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
