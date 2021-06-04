use std::borrow::Cow;

use rustyline::completion::{Completer, FilenameCompleter};
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::Validator;
use rustyline::{Context, Result};

pub struct Helper {
    filename_completer: FilenameCompleter,
    history_hinter: HistoryHinter,
    matching_bracket_highlighter: MatchingBracketHighlighter,
}

impl Helper {
    pub fn new() -> Helper {
        Helper {
            filename_completer: FilenameCompleter::new(),
            history_hinter: HistoryHinter {},
            matching_bracket_highlighter: MatchingBracketHighlighter::new(),
        }
    }
}

impl Completer for Helper {
    type Candidate = <FilenameCompleter as Completer>::Candidate;
    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context,
    ) -> Result<(usize, Vec<Self::Candidate>)> {
        self.filename_completer.complete(line, pos, ctx)
    }
}

impl Hinter for Helper {
    type Hint = String;
    fn hint(&self, line: &str, pos: usize, ctx: &Context) -> Option<String> {
        self.history_hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for Helper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.matching_bracket_highlighter.highlight(line, pos)
    }
}

impl Validator for Helper {}

impl rustyline::Helper for Helper {}
