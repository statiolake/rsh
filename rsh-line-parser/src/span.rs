#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn is_followed_by(self, other: Self) -> bool {
        self.end == other.start
    }

    pub fn merged(&mut self, other: Self) -> Self {
        debug_assert!(self.is_followed_by(other));
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
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
