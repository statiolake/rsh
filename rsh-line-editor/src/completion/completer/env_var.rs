use super::{Completer, CompletionResult};
use crate::LineBuffer;
use rsh_line_parser::{span::Spanned, token::Atom};

pub struct EnvVarCompleter<'b> {
    pub buf: &'b LineBuffer,
    pub target: Spanned<Vec<Atom>>,
}

impl Completer for EnvVarCompleter<'_> {
    fn complete(&self) -> CompletionResult {
        CompletionResult::Partial {
            partial_span: self.target.span,
            partially_replace_to: "env_var".to_string(),
            candidates: vec!["env_var: not yet implemented!".to_string()],
        }
    }
}
