use rsh_line_parser::span::Span;

mod env_var;
mod executable;
mod path;

pub use env_var::EnvVarCompleter;
pub use executable::ExecutableCompleter;
pub use path::PathCompleter;

pub enum CompletionResult {
    Sole {
        span: Span,
        replace_to: String,
    },
    Partial {
        partial_span: Span,
        partially_replace_to: String,
        candidates: Vec<String>,
    },
}

pub trait Completer {
    fn complete(&self) -> CompletionResult;
}
