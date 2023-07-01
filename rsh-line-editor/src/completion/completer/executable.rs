use super::{Completer, CompletionResult};
use crate::LineBuffer;
use rsh_line_parser::{span::Spanned, token::Atom};

pub struct ExecutableCompleter<'b> {
    pub buf: &'b LineBuffer,
    pub target: Spanned<'b, Vec<Atom<'b>>>,
    pub in_double: bool,
    pub in_single: bool,
}

impl Completer for ExecutableCompleter<'_> {
    fn complete(&self) -> CompletionResult {
        CompletionResult::Partial {
            partial_span: self.target.span,
            partially_replace_to: "exec".to_string(),
            candidates: vec!["executable: not implemented yet!".to_string()],
        }
    }
}
