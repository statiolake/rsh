use crate::{
    span::{Span, Spanned},
    token::{
        Atom, DoubleQuoted, EnvVar, FlattenedToken, FlattenedTokenKind, HasTokenKind, Redirect,
        RedirectKind, RedirectReferenceKind, SingleQuoted, Substitution, Token, TokenKind,
    },
};

pub const ESCAPE_CHAR: char = '^';
pub const SHOULD_ESCAPE_CHAR: [char; 12] = [
    ESCAPE_CHAR,
    ' ',
    '$',
    '(',
    ')',
    '{',
    '}',
    '|',
    '<',
    '>',
    '"',
    '\'',
];

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("found '{}' but no following character found", ESCAPE_CHAR)]
    NoEscapedChar { span: Span },

    #[error("invalid escape sequence '{}{}'", ESCAPE_CHAR, ch)]
    InvalidEscapeSequence { span: Span, ch: char },

    #[error("no environment variable or substitution invocation")]
    NoEnvVarOrSubstitution { span: Span },

    #[error("substitution invocation is not ended")]
    UnbalancedSubstitution { span: Span },

    #[error("environment variable is not ended")]
    UnbalancedEnvVarBrace { span: Span },

    #[error("quoted string is not ended")]
    UnbalancedQuotedString { span: Span },
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Error::NoEscapedChar { span } => *span,
            Error::InvalidEscapeSequence { span, .. } => *span,
            Error::NoEnvVarOrSubstitution { span } => *span,
            Error::UnbalancedSubstitution { span } => *span,
            Error::UnbalancedEnvVarBrace { span } => *span,
            Error::UnbalancedQuotedString { span } => *span,
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Lexer<'a> {
    source: &'a [char],
    current: usize,

    substitution_level: usize,

    /// Keep lexer running even when there's error
    recover_error: bool,

    /// Trim whitespaces at the both end.
    trim_delims: bool,
}

impl<'a> Lexer<'a> {
    pub fn recover_error(&mut self, value: bool) -> &mut Self {
        self.recover_error = value;
        self
    }

    pub fn trim_delims(&mut self, value: bool) -> &mut Self {
        self.trim_delims = value;
        self
    }

    pub fn new(source: &'a [char]) -> Self {
        Self {
            source,
            current: 0,
            substitution_level: 0,
            recover_error: false,
            trim_delims: true,
        }
    }

    pub fn tokenize(&mut self) -> Result<Spanned<'a, Vec<Token<'a>>>> {
        let mut tokens: Vec<Token> = Vec::new();

        let whole_span = self.span_for_current_point();
        while let Some(ch) = self.peek() {
            if self.substitution_level > 0 && ch == ')' {
                // If tokenize in substitution, stop tokenizing at ')'.
                break;
            }
            tokens.push(self.next_token()?);
        }
        let whole_span = tokens
            .iter()
            .fold(whole_span, |merged, tok| merged.merged(tok.span));

        // Normalize tokens
        normalize_tokens(self.trim_delims, &mut tokens);

        Ok(Spanned::new(self.source, whole_span, tokens))
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        if let Some(span) = self.skip_whitespace() {
            return Ok(Token::new(self.source, span, TokenKind::ArgDelim));
        };

        match self.peek_rest() {
            [] => panic!("internal error: no token found"),
            [ESCAPE_CHAR, ..] => self
                .next_escaped_sequence()
                .map(|ch| Spanned::new(self.source, ch.span, TokenKind::Atom(Atom::from(ch)))),
            ['\'', ..] => self
                .next_single_quoted()
                .map(|v| v.map(TokenKind::SingleQuoted)),
            ['"', ..] => self
                .next_double_quoted()
                .map(|v| v.map(TokenKind::DoubleQuoted)),
            ['>' | '<', ..] | ['1' | '2', '>', ..] => {
                self.next_redirect().map(|v| v.map(TokenKind::Redirect))
            }
            ['|' | ';', ..] => self.next_delim(),
            _ => self.next_atom().map(|v| v.map(TokenKind::Atom)),
        }
    }

    /// Tokenize as if it is quoted. Usually called when tokenizing substitution invocation inside the
    /// double quote.
    ///
    /// ## Note
    ///
    /// In this mode, everything is treated as simple string (except for whitespace; when `delim` is
    /// `true`, whitespaces are treated as ArgDelim, otherwise just a whitespace character.) For
    /// example, environmental variables substitution or substitution invocation won't be parsed.
    ///
    /// ## Examples
    ///
    /// ```console
    /// > echo $(echo '$(ls)')
    /// $(ls)
    /// ```
    ///
    /// At first lowering the command line text will be `echo $(ls)`, but it's not substitution
    /// invocation here. Much like `echo '$(ls)'`.
    ///
    /// The `delim` option is to handle substitution $() differently according to its position, in
    /// plain command line or in double quote. Normally the substituted output is broke into
    /// multiple arguments at the whitespace characters. However, in double quotes, the substituted
    /// output should not delimited by whitespace characters.
    ///
    /// For example, if show_args is a program just showing its arguments:
    /// ```console
    /// > show_args $(echo 'hello world')
    /// 0: show_args
    /// 1: hello
    /// 2: world
    /// > show_args "$(echo 'hello world')"
    /// 0: show_args
    /// 1: hello world
    /// ```
    pub fn tokenize_substitution(&mut self, delim: bool) -> Result<Vec<Token<'a>>> {
        let mut tokens = Vec::new();

        while self.peek().is_some() {
            tokens.push(self.next_token_substitution(delim)?);
        }

        Ok(tokens)
    }

    pub fn next_token_substitution(&mut self, delim: bool) -> Result<Token<'a>> {
        if delim {
            if let Some(span) = self.skip_whitespace() {
                return Ok(Token::new(self.source, span, TokenKind::ArgDelim));
            }
        }

        self.next_char()
            .map(|ch| Spanned::new(self.source, ch.span, TokenKind::Atom(Atom::from(ch))))
    }

    fn next_atom(&mut self) -> Result<Spanned<'a, Atom<'a>>> {
        match self.peek_rest() {
            ['$', '(', ..] => self.next_substitution().map(|v| v.map(Atom::Substitution)),
            ['$', ..] => self.next_env_var().map(|v| v.map(Atom::EnvVar)),
            _ => self
                .next_char()
                .map(|ch| Spanned::new(self.source, ch.span, Atom::Char(ch))),
        }
    }

    fn next_single_quoted(&mut self) -> Result<Spanned<'a, SingleQuoted<'a>>> {
        let mut whole_span = self.eat(['\'']);
        let mut inner_span = self.span_for_current_point();
        let mut chars = vec![];
        while let Some(ch) = self.peek() {
            match ch {
                '\'' => {
                    whole_span = whole_span.merged(self.eat([ch]));
                    return Ok(Spanned::new(
                        self.source,
                        whole_span,
                        SingleQuoted(Spanned::new(self.source, inner_span, chars)),
                    ));
                }
                _ => {
                    let ch = self.next_char()?;
                    whole_span = whole_span.merged(ch.span);
                    inner_span = inner_span.merged(ch.span);
                    chars.push(ch);
                }
            }
        }

        if self.recover_error {
            Ok(Spanned::new(
                self.source,
                whole_span,
                SingleQuoted(Spanned::new(self.source, inner_span, chars)),
            ))
        } else {
            Err(Error::UnbalancedQuotedString { span: whole_span })
        }
    }

    fn next_double_quoted(&mut self) -> Result<Spanned<'a, DoubleQuoted<'a>>> {
        let mut whole_span = self.eat(['"']);
        let mut inner_span = self.span_for_current_point();
        let mut atoms = vec![];
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    whole_span = whole_span.merged(self.eat([ch]));
                    return Ok(Spanned::new(
                        self.source,
                        whole_span,
                        DoubleQuoted(Spanned::new(self.source, inner_span, atoms)),
                    ));
                }
                _ => {
                    let atom = self.next_atom()?;
                    whole_span = whole_span.merged(atom.span);
                    inner_span = inner_span.merged(atom.span);
                    atoms.push(atom.data);
                }
            }
        }

        if self.recover_error {
            Ok(Spanned::new(
                self.source,
                whole_span,
                DoubleQuoted(Spanned::new(self.source, inner_span, atoms)),
            ))
        } else {
            Err(Error::UnbalancedQuotedString { span: whole_span })
        }
    }

    fn next_redirect(&mut self) -> Result<Spanned<'a, Redirect>> {
        let mut span = self.span_for_current_point();

        let (append, kind) = match self.peek_rest() {
            ['>', '>', ..] => {
                span = span.merged(self.eat(['>', '>']));
                (true, RedirectKind::Stdout)
            }
            ['>', ..] => {
                span = span.merged(self.eat(['>']));
                (false, RedirectKind::Stdout)
            }
            ['<', ..] => {
                span = span.merged(self.eat(['<']));
                (false, RedirectKind::Stdin)
            }
            ['1', '>', '>', ..] => {
                span = span.merged(self.eat(['1', '>', '>']));
                (true, RedirectKind::Stdout)
            }
            ['1', '>', ..] => {
                span = span.merged(self.eat(['1', '>']));
                (false, RedirectKind::Stdout)
            }
            ['2', '>', '>', ..] => {
                span = span.merged(self.eat(['2', '>', '>']));
                (true, RedirectKind::Stderr)
            }
            ['2', '>', ..] => {
                span = span.merged(self.eat(['2', '>']));
                (false, RedirectKind::Stderr)
            }
            _ => panic!("internal error: tried to parse non-redirect"),
        };

        let reference = match self.peek_rest() {
            ['&', '1'] if kind.is_output() => {
                span = span.merged(self.eat(['&', '1']));
                Some(RedirectReferenceKind::Stdout)
            }
            ['&', '2'] if kind.is_output() => {
                span = span.merged(self.eat(['&', '2']));
                Some(RedirectReferenceKind::Stderr)
            }
            _ => None,
        };

        Ok(Spanned::new(
            self.source,
            span,
            Redirect {
                kind,
                reference,
                append,
            },
        ))
    }

    fn next_delim(&mut self) -> Result<Token<'a>> {
        match self.peek() {
            Some('|') => {
                let span = self.eat(['|']);
                Ok(Token::new(self.source, span, TokenKind::Pipe))
            }
            Some(';') => {
                let span = self.eat([';']);
                Ok(Token::new(self.source, span, TokenKind::Delim))
            }
            Some(ch) => panic!("internal error: unknown delimiter `{}`", ch),
            None => panic!("internal error: no delimiter"),
        }
    }

    fn next_escaped_sequence(&mut self) -> Result<Spanned<'a, char>> {
        assert!(
            self.peek() == Some(ESCAPE_CHAR),
            "internal error: escaped sequence not starting with `{}`",
            ESCAPE_CHAR
        );

        match self.lookahead(1) {
            Some(ch) if SHOULD_ESCAPE_CHAR.contains(&ch) => {
                let span = self.eat([ESCAPE_CHAR, ch]);
                Ok(Spanned::new(self.source, span, ch))
            }
            Some('n') => {
                let span = self.eat([ESCAPE_CHAR, 'n']);
                Ok(Spanned::new(self.source, span, '\n'))
            }
            Some('t') => {
                let span = self.eat([ESCAPE_CHAR, 't']);
                Ok(Spanned::new(self.source, span, '\t'))
            }
            Some(ch) => {
                let span = self.eat([ESCAPE_CHAR, ch]);
                Err(Error::InvalidEscapeSequence { span, ch })
            }
            None => Err(Error::NoEscapedChar {
                span: self.eat([ESCAPE_CHAR]),
            }),
        }
    }

    fn next_char(&mut self) -> Result<Spanned<'a, char>> {
        match self.peek() {
            Some(ESCAPE_CHAR) => self.next_escaped_sequence(),
            Some(ch) => {
                let span = self.eat([ch]);
                Ok(Spanned::new(self.source, span, ch))
            }
            None => panic!("internal error: no next character"),
        }
    }

    fn next_ascii_word(&mut self) -> Result<Vec<Spanned<'a, char>>> {
        let mut word = vec![];
        while let Some(ch) = self.peek() {
            if !ch.is_ascii_alphanumeric() && ch != '_' {
                break;
            }

            let ch = self
                .next_char()
                .expect("internal error: peeked char was not produced");
            word.push(ch);
        }

        Ok(word)
    }

    fn next_substitution(&mut self) -> Result<Spanned<'a, Substitution<'a>>> {
        let whole_span = self.eat(['$', '(']);
        let inner_span = self.span_for_current_point();
        self.substitution_level += 1;

        let tokens = self.tokenize()?;

        // Compute span by merging all
        let mut whole_span = tokens
            .data
            .iter()
            .fold(whole_span, |merged, tok| merged.merged(tok.span));
        let inner_span = tokens
            .data
            .iter()
            .fold(inner_span, |merged, tok| merged.merged(tok.span));

        if self.peek() == Some(')') {
            whole_span = whole_span.merged(self.eat([')']));
        } else if !self.recover_error {
            return Err(Error::UnbalancedSubstitution { span: whole_span });
        }

        self.substitution_level -= 1;

        Ok(Spanned::new(
            self.source,
            whole_span,
            Substitution(Spanned::new(self.source, inner_span, tokens.data)),
        ))
    }

    fn next_env_var(&mut self) -> Result<Spanned<'a, EnvVar<'a>>> {
        match self.peek_rest() {
            ['$', '{', ..] => {
                let whole_span = self.eat(['$', '{']);
                let inner_span = self.span_for_current_point();

                // FIXME: should we limit to ascii word here?
                let var_name = self.next_ascii_word()?;

                // Merge spans
                let inner_span = var_name
                    .iter()
                    .fold(inner_span, |merged, tok| merged.merged(tok.span));
                let mut whole_span = var_name
                    .iter()
                    .fold(whole_span, |merged, tok| merged.merged(tok.span));

                if self.peek() == Some('}') {
                    whole_span = whole_span.merged(self.eat(['}']));
                } else if !self.recover_error {
                    return Err(Error::UnbalancedEnvVarBrace { span: whole_span });
                }

                Ok(Spanned::new(
                    self.source,
                    whole_span,
                    EnvVar(Spanned::new(self.source, inner_span, var_name)),
                ))
            }
            ['$', ..] => {
                let whole_span = self.eat(['$']);
                let inner_span = self.span_for_current_point();

                let var_name = self.next_ascii_word()?;

                // Merge spans
                let inner_span = var_name
                    .iter()
                    .fold(inner_span, |merged, tok| merged.merged(tok.span));
                let whole_span = var_name
                    .iter()
                    .fold(whole_span, |merged, tok| merged.merged(tok.span));

                Ok(Spanned::new(
                    self.source,
                    whole_span,
                    EnvVar(Spanned::new(self.source, inner_span, var_name)),
                ))
            }
            _ => panic!("internal error: EnvVar not starting with `$`"),
        }
    }

    fn skip_whitespace(&mut self) -> Option<Span> {
        let mut span: Option<Span> = None;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                let ch_span = self.eat([ch]);
                span = match span {
                    Some(span) => Some(span.merged(ch_span)),
                    None => Some(ch_span),
                }
            } else {
                break;
            }
        }

        span
    }

    fn eat(&mut self, expected: impl IntoIterator<Item = char>) -> Span {
        let start = self.current;
        for ch in expected {
            assert_eq!(
                self.peek(),
                Some(ch),
                "internal error: unexpected character `{}`",
                ch
            );
            self.current += 1;
        }
        let end = self.current;
        Span { start, end }
    }

    fn span_for_current_point(&self) -> Span {
        Span {
            start: self.current,
            end: self.current,
        }
    }

    fn peek(&self) -> Option<char> {
        self.lookahead(0)
    }

    fn lookahead(&self, n: usize) -> Option<char> {
        self.peek_rest().get(n).copied()
    }

    fn peek_rest(&self) -> &'a [char] {
        &self.source[self.current..]
    }
}

pub fn normalize_tokens<'a>(trim_delims: bool, tokens: &mut Vec<Token<'a>>) {
    let create_arg_delim_between = |tok1: &Token<'a>, tok2: &Token<'a>| {
        let span = Span::from(tok1.span.end..tok2.span.start);
        Spanned::new(tok1.source, span, TokenKind::ArgDelim)
    };

    if trim_delims {
        remove_surrounding_arg_delim_or_delim(tokens);
    }

    // Some normalization should always be executed
    remove_duplicated_arg_delim_or_delim(tokens);
    remove_arg_delim_around_delim_or_pipe(tokens);
    normalize_arg_delim_around_redirect(tokens, create_arg_delim_between);
}

pub fn normalize_flattened_tokens(tokens: &mut Vec<FlattenedToken>) {
    let create_arg_delim_between =
        |_: &FlattenedToken, _: &FlattenedToken| FlattenedTokenKind::ArgDelim;

    remove_surrounding_arg_delim_or_delim(tokens);
    remove_duplicated_arg_delim_or_delim(tokens);
    remove_arg_delim_around_delim_or_pipe(tokens);
    normalize_arg_delim_around_redirect(tokens, create_arg_delim_between);
}

fn remove_surrounding_arg_delim_or_delim<T: HasTokenKind>(tokens: &mut Vec<T>) {
    let is_non_delim = |tok: &T| !tok.is_delim() && !tok.is_arg_delim();

    // Remove leading delimiters
    let first_non_delim = tokens.iter().position(is_non_delim).unwrap_or(tokens.len());
    tokens.drain(..first_non_delim.min(tokens.len()));

    // Remove trailing delimiters
    let last_non_delim = tokens
        .iter()
        .rposition(is_non_delim)
        .unwrap_or(tokens.len());
    tokens.drain((last_non_delim + 1).min(tokens.len())..);
}

fn remove_duplicated_arg_delim_or_delim<T: HasTokenKind>(tokens: &mut Vec<T>) {
    let mut idx = 1;
    while idx < tokens.len() {
        let tok = &tokens[idx];
        let tok1 = &tokens[idx - 1];

        if (tok.is_arg_delim() && tok1.is_arg_delim()) || (tok.is_delim() && tok1.is_delim()) {
            tokens.remove(idx);
            idx -= 1;
        }

        idx += 1;
    }
}

fn remove_arg_delim_around_delim_or_pipe<T: HasTokenKind>(tokens: &mut Vec<T>) {
    let mut idx = 0;
    while idx < tokens.len() {
        let tok = &tokens[idx];
        if !tok.is_delim() && !tok.is_pipe() {
            idx += 1;
            continue;
        }

        if tokens.get(idx.wrapping_sub(1)).map(T::is_arg_delim) == Some(true) {
            // Remove ArgDelim before Delim or Pipe.
            tokens.remove(idx - 1);
            idx -= 1;
        }

        if tokens.get(idx.wrapping_add(1)).map(T::is_arg_delim) == Some(true) {
            // Remove ArgDelim after Delim or Pipe.
            tokens.remove(idx + 1);
        }

        idx += 1;
    }
}

fn normalize_arg_delim_around_redirect<T: HasTokenKind>(
    tokens: &mut Vec<T>,
    create_arg_delim_between: impl Fn(&T, &T) -> T,
) {
    let mut idx = 0;

    while idx < tokens.len() {
        if !tokens[idx].is_redirect() {
            idx += 1;
            continue;
        }

        // If current token is redirect,
        // 1. ensure token before redirect is ArgDelim,
        // 2. remove ArgDelim after redirect if exists.
        if tokens.get(idx.wrapping_sub(1)).map(T::is_arg_delim) != Some(true) {
            let arg_delim = create_arg_delim_between(&tokens[idx - 1], &tokens[idx]);
            tokens.insert(idx, arg_delim);
            idx += 1;
        }

        if tokens.get(idx.wrapping_add(1)).map(T::is_arg_delim) == Some(true) {
            tokens.remove(idx + 1);
        }

        idx += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    macro_rules! ch {
        ($range:expr, $ch:expr) => {
            Spanned::new(&[], Span::from($range), $ch)
        };
    }

    macro_rules! atom {
        ($range:expr, st $st:expr) => {
            Atom::Substitution(Substitution(Spanned::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                Span::from($range),
                $st.to_vec(),
            )))
        };
        ($range:expr, env $env:expr) => {
            Atom::EnvVar(EnvVar(Spanned::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                Span::from($range),
                $env.to_vec(),
            )))
        };
        ($range:expr, $atom:expr) => {
            Atom::Char(Spanned::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                Span::from($range),
                $atom,
            ))
        };
    }

    macro_rules! tok {
        ($range:expr, atom $($atom:tt)*) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                atom!($($atom)*).into(),
            )
        };
        ($range:expr, squote $inner_range:expr, $squote:expr) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                SingleQuoted(Spanned::new(
                    // Hack: this is just for test. This should be a original source.
                    &[],
                    Span::from($inner_range),
                    $squote.to_vec()
                )).into(),
            )
        };
        ($range:expr, dquote $inner_range:expr, $dquote:expr) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                DoubleQuoted(Spanned::new(
                    // Hack: this is just for test. This should be a original source.
                    &[],
                    Span::from($inner_range),
                    $dquote.to_vec()
                )).into(),
            )
        };
        ($range:expr, argd) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                TokenKind::ArgDelim,
            )
        };
        ($range:expr, redir $redir:expr) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                Redirect::from($redir).into(),
            )
        };
        ($range:expr, pipe) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                TokenKind::Pipe,
            )
        };
        ($range:expr, delim) => {
            Token::new(
                // Hack: this is just for test. This should be a original source.
                &[],
                $range.into(),
                TokenKind::Delim,
            )
        };
    }

    fn tokenize_str(s: &str) -> Vec<Token> {
        // Hack: leak everything to get long-lived lifetime
        Lexer::new(s.chars().collect_vec().leak())
            .tokenize()
            .expect("should parse")
            .data
    }

    fn tokenize_error_str(s: &str) -> Vec<Token> {
        Lexer::new(s.chars().collect_vec().leak())
            .recover_error(true)
            .trim_delims(false)
            .tokenize()
            .expect("should parse")
            .data
    }

    #[test]
    fn single() {
        let tokens = tokenize_str("ls");
        let expected = [tok!(0..1, atom 0..1, 'l'), tok!(1..2, atom 1..2, 's')];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn single_arg() {
        let tokens = tokenize_str("ls -al");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..4, atom 3..4, '-'),
            tok!(4..5, atom 4..5, 'a'),
            tok!(5..6, atom 5..6, 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn duplicate_whitespace() {
        let tokens = tokenize_str("ls   -al");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..5, argd),
            tok!(5..6, atom 5..6, '-'),
            tok!(6..7, atom 6..7, 'a'),
            tok!(7..8, atom 7..8, 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn multiple_args() {
        let tokens = tokenize_str("ls a b c");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..4, atom 3..4, 'a'),
            tok!(4..5, argd),
            tok!(5..6, atom 5..6, 'b'),
            tok!(6..7, argd),
            tok!(7..8, atom 7..8, 'c'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn env_var() {
        let tokens = tokenize_str("echo $A_C def");
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..9, atom 6..9, env [
                ch!(6..7, 'A'),
                ch!(7..8, '_'),
                ch!(8..9, 'C'),
            ]),
            tok!(9..10, argd),
            tok!(10..11, atom 10..11, 'd'),
            tok!(11..12, atom 11..12, 'e'),
            tok!(12..13, atom 12..13, 'f'),
        ];
        assert_eq!(tokens, expected);

        let tokens = tokenize_str("echo ${A_C} def");
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..11, atom 7..10, env [
                ch!(7..8, 'A'),
                ch!(8..9, '_'),
                ch!(9..10, 'C'),
            ]),
            tok!(11..12, argd),
            tok!(12..13, atom 12..13, 'd'),
            tok!(13..14, atom 13..14, 'e'),
            tok!(14..15, atom 14..15, 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn substitution() {
        let tokens = tokenize_str("echo $(ls) def");
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..10, atom 7..9, st [
                tok!(7..8, atom 7..8, 'l'),
                tok!(8..9, atom 8..9, 's'),
            ]),
            tok!(10..11, argd),
            tok!(11..12, atom 11..12, 'd'),
            tok!(12..13, atom 12..13, 'e'),
            tok!(13..14, atom 13..14, 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn substitution_args() {
        let tokens = tokenize_str("echo $(ls -al) def");
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..14, atom 7..13, st [
                tok!(7..8, atom 7..8, 'l'),
                tok!(8..9, atom 8..9, 's'),
                tok!(9..10, argd),
                tok!(10..11, atom 10..11, '-'),
                tok!(11..12, atom 11..12, 'a'),
                tok!(12..13, atom 12..13, 'l'),
            ]),
            tok!(14..15, argd),
            tok!(15..16, atom 15..16, 'd'),
            tok!(16..17, atom 16..17, 'e'),
            tok!(17..18, atom 17..18, 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn subsubstitution() {
        let tokens = tokenize_str("echo $(ls $(ls)) def");
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..16, atom 7..15, st [
                tok!(7..8, atom 7..8, 'l'),
                tok!(8..9, atom 8..9, 's'),
                tok!(9..10, argd),
                tok!(10..15, atom 12..14, st [
                    tok!(12..13, atom 12..13, 'l'),
                    tok!(13..14, atom 13..14, 's'),
                ]),
            ]),
            tok!(16..17, argd),
            tok!(17..18, atom 17..18, 'd'),
            tok!(18..19, atom 18..19, 'e'),
            tok!(19..20, atom 19..20, 'f'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn dquote() {
        let tokens = tokenize_str(r#"echo "a b""#);
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..10, dquote 6..9, [
                atom!(6..7, 'a'),
                atom!(7..8, ' '),
                atom!(8..9, 'b'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn dquote_env_var() {
        let tokens = tokenize_str(r#"echo "a $var c""#);
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..15, dquote 6..14, [
                atom!(6..7, 'a'),
                atom!(7..8, ' '),
                atom!(9..12, env [
                    ch!(9..10, 'v'),
                    ch!(10..11, 'a'),
                    ch!(11..12, 'r'),
                ]),
                atom!(12..13, ' '),
                atom!(13..14, 'c'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn squote() {
        let tokens = tokenize_str(r#"echo 'a b'"#);
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..10, squote 6..9, [
                ch!(6..7, 'a'),
                ch!(7..8, ' '),
                ch!(8..9, 'b'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn squote_env_var() {
        let tokens = tokenize_str(r#"echo 'a $var c'"#);
        let expected = [
            tok!(0..1, atom 0..1, 'e'),
            tok!(1..2, atom 1..2, 'c'),
            tok!(2..3, atom 2..3, 'h'),
            tok!(3..4, atom 3..4, 'o'),
            tok!(4..5, argd),
            tok!(5..15, squote 6..14, [
                 ch!(6..7, 'a'),
                 ch!(7..8, ' '),
                 ch!(8..9, '$'),
                 ch!(9..10, 'v'),
                 ch!(10..11, 'a'),
                 ch!(11..12, 'r'),
                 ch!(12..13, ' '),
                 ch!(13..14, 'c'),
            ]),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn redirect() {
        use RedirectKind as RK;
        let tokens = tokenize_str("ls > file.txt");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..4, redir RK::Stdout),
            tok!(5..6, atom 5..6, 'f'),
            tok!(6..7, atom 6..7, 'i'),
            tok!(7..8, atom 7..8, 'l'),
            tok!(8..9, atom 8..9, 'e'),
            tok!(9..10, atom 9..10, '.'),
            tok!(10..11, atom 10..11, 't'),
            tok!(11..12, atom 11..12, 'x'),
            tok!(12..13, atom 12..13, 't'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn redirect_reference() {
        use RedirectKind as RK;
        use RedirectReferenceKind as RRK;
        let tokens = tokenize_str("ls > file.txt 2>&1");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..4, redir RK::Stdout),
            tok!(5..6, atom 5..6, 'f'),
            tok!(6..7, atom 6..7, 'i'),
            tok!(7..8, atom 7..8, 'l'),
            tok!(8..9, atom 8..9, 'e'),
            tok!(9..10, atom 9..10, '.'),
            tok!(10..11, atom 10..11, 't'),
            tok!(11..12, atom 11..12, 'x'),
            tok!(12..13, atom 12..13, 't'),
            tok!(13..14, argd),
            tok!(14..18, redir(RK::Stderr, RRK::Stdout)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn redirect_normalize() {
        use RedirectKind as RK;
        let tokens = tokenize_str("ls >file.txt>     file.txt");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..4, redir RK::Stdout),
            tok!(4..5, atom 4..5, 'f'),
            tok!(5..6, atom 5..6, 'i'),
            tok!(6..7, atom 6..7, 'l'),
            tok!(7..8, atom 7..8, 'e'),
            tok!(8..9, atom 8..9, '.'),
            tok!(9..10, atom 9..10, 't'),
            tok!(10..11, atom 10..11, 'x'),
            tok!(11..12, atom 11..12, 't'),
            tok!(12..12, argd),
            tok!(12..13, redir RK::Stdout),
            tok!(18..19, atom 18..19, 'f'),
            tok!(19..20, atom 19..20, 'i'),
            tok!(20..21, atom 20..21, 'l'),
            tok!(21..22, atom 21..22, 'e'),
            tok!(22..23, atom 22..23, '.'),
            tok!(23..24, atom 23..24, 't'),
            tok!(24..25, atom 24..25, 'x'),
            tok!(25..26, atom 25..26, 't'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn delim() {
        let tokens = tokenize_str("ls; wc -l");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, delim),
            tok!(4..5, atom 4..5, 'w'),
            tok!(5..6, atom 5..6, 'c'),
            tok!(6..7, argd),
            tok!(7..8, atom 7..8, '-'),
            tok!(8..9, atom 8..9, 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn pipe() {
        let tokens = tokenize_str("ls | wc -l");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(3..4, pipe),
            tok!(5..6, atom 5..6, 'w'),
            tok!(6..7, atom 6..7, 'c'),
            tok!(7..8, argd),
            tok!(8..9, atom 8..9, '-'),
            tok!(9..10, atom 9..10, 'l'),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn recover_error() {
        let tokens = tokenize_error_str("ls $(echo ");
        let expected = [
            tok!(0..1, atom 0..1, 'l'),
            tok!(1..2, atom 1..2, 's'),
            tok!(2..3, argd),
            tok!(3..10, atom 5..10, st [
                tok!(5..6, atom 5..6, 'e'),
                tok!(6..7, atom 6..7, 'c'),
                tok!(7..8, atom 7..8, 'h'),
                tok!(8..9, atom 8..9, 'o'),
                tok!(9..10, argd),
            ]),
        ];

        assert_eq!(tokens, expected);
    }
}
