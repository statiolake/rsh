use rsh_line_parser::{
    lexer::Lexer,
    span::Spanned,
    token::{Atom, Token, TokenKind, TokenList},
};

use self::completer::{
    Completer, CompletionResult, EnvVarCompleter, ExecutableCompleter, PathCompleter,
};
use crate::Result;
use crate::{LineBuffer, LinePrinter};

mod completer;

pub fn handle_completion<P>(printer: &mut LinePrinter<P>, buf: &mut LineBuffer) -> Result<()> {
    let before_cursor = &buf.buf[..buf.cursor_at];
    let tokens = Lexer::new(before_cursor)
        .recover_error(true)
        .tokenize()
        .unwrap_or_else(|e| panic!("internal error: lexer error not recovered: {}", e));

    let Some(res) = find_completer(buf, &tokens).map(|completer| completer.complete()) else {
        return Ok(());
    };

    match res {
        CompletionResult::Sole { span, replace_to } => buf.replace_span(span, replace_to.chars()),
        CompletionResult::Partial {
            partial_span,
            partially_replace_to,
            candidates,
        } => {
            buf.replace_span(partial_span, partially_replace_to.chars());
            printer.set_hints(candidates);
        }
    }

    Ok(())
}

/// Find appropriate completer from all tokens before cursor.
///
/// For example:
///
/// ```console
/// > check_complete foo ba|r        # Case 1
/// (PathCompleter) target: ba
/// > check_complete 'foo ba|r'      # Case 2
/// (PathCompleter) target: foo ba
/// > check_complete ${ENV_|V}       # Case 3
/// (EnvVarCompleter) target: ENV_
/// > check_complete $(ls worl|d)    # Case 4
/// (PathCompleter) target: worl
/// > check_compl|
/// (ExecutableCompletor) target: check_compl
/// ```
///
fn find_completer<'b>(
    buf: &'b LineBuffer,
    tokens: &TokenList<'b>,
) -> Option<Box<dyn Completer + 'b>> {
    let tokens = extract_delim_splitted_last_tokens(tokens);

    // Basically we want to check the last token to determine the kind of completion.
    let Some(last_token) = tokens.data.last()
        else {
        // If tokens are empty, command line is empty. Just complete all executables.
        let span = tokens.span;

        return Some(Box::new(ExecutableCompleter {
            buf,
            target: Spanned::new(&buf.buf, span, vec![]),
            in_double: false,
            in_single: false,
        }))
    };

    match &last_token.data {
        TokenKind::Atom(Atom::Substitution(subst)) => {
            // If the cursor is inside a substitution (Case 4), enter to the inner substitution and
            // complete there.
            find_completer(buf, &subst.0)
        }
        TokenKind::Atom(Atom::EnvVar(env_var)) => {
            // If the cursor is inside an environment variable name (Case 3), complete a variable
            // name.
            let target: Spanned<Vec<Atom>> = Spanned::new(
                env_var.0.source,
                env_var.0.span,
                env_var.0.data.iter().map(|ch| Atom::Char(*ch)).collect(),
            );
            Some(Box::new(EnvVarCompleter { buf, target }) as _)
        }
        TokenKind::SingleQuoted(sq) => {
            // target are the string inside single quotes.
            let target = Spanned::new(
                sq.0.source,
                sq.0.span,
                sq.0.data.iter().map(|ch| Atom::Char(*ch)).collect(),
            );

            // If the cursor is inside a single quoted string, complete executable name if it is
            // first argument (it is when no ArgDelim found). Otherwise complete a file path.
            let has_arg_delim = tokens.data.iter().any(|tok| tok.data.arg_delim().is_some());
            if has_arg_delim {
                Some(Box::new(PathCompleter {
                    buf,
                    target,
                    in_single: true,
                    in_double: false,
                }) as _)
            } else {
                Some(Box::new(ExecutableCompleter {
                    buf,
                    target,
                    in_single: true,
                    in_double: false,
                }) as _)
            }
        }
        TokenKind::DoubleQuoted(dq) => {
            // target are the atoms inside double quotes.
            let target = dq.0.clone();

            // If the cursor is inside a double quoted string, complete executable name if it is
            // first argument (it is when no ArgDelim found). Otherwise complete a file path.
            let has_arg_delim = tokens.data.iter().any(|tok| tok.data.arg_delim().is_some());
            if has_arg_delim {
                Some(Box::new(PathCompleter {
                    buf,
                    target,
                    in_single: false,
                    in_double: true,
                }) as _)
            } else {
                Some(Box::new(ExecutableCompleter {
                    buf,
                    target,
                    in_single: false,
                    in_double: true,
                }) as _)
            }
        }
        TokenKind::ArgDelim | TokenKind::Redirect(_) => {
            // If the cursor is after the delimiter (or redirect), complete paths.
            // TODO: support command line argument custom completion?
            Some(Box::new(PathCompleter {
                buf,
                target: Spanned::new(&[], last_token.span.end_point(), vec![]),
                in_single: false,
                in_double: false,
            }) as _)
        }
        TokenKind::Pipe => {
            // If the cursor is after the pipe, complete executable.
            Some(Box::new(ExecutableCompleter {
                buf,
                target: Spanned::new(&[], last_token.span.end_point(), vec![]),
                in_single: false,
                in_double: false,
            }) as _)
        }
        TokenKind::Delim => {
            panic!("internal error: Delim found inside Delim-splitted element");
        }
        _ => {
            // Otherwise, We need to extract last atom sequence and determine appropriate completion
            // kind.
            let target = {
                let mut target_atoms = vec![];
                let mut spans = vec![];
                for tok in tokens.data.iter().rev() {
                    let Spanned { span, data: TokenKind::Atom(atom), .. } = tok else {
                        break;
                    };

                    target_atoms.push(atom.clone());
                    spans.push(*span);
                }

                target_atoms.reverse();
                spans.reverse();
                let target_span = spans
                    .iter()
                    .fold(spans[0].start_point(), |sum, span| sum.merged(*span));
                Spanned::new(tokens.source, target_span, target_atoms)
            };

            // If there is no ArgDelim, it should be executable completion.
            let is_executable_completion =
                tokens.data.iter().all(|tok| tok.data.arg_delim().is_none());

            if is_executable_completion {
                Some(Box::new(ExecutableCompleter {
                    buf,
                    target,
                    in_single: false,
                    in_double: false,
                }) as _)
            } else {
                Some(Box::new(PathCompleter {
                    buf,
                    target,
                    in_single: false,
                    in_double: false,
                }) as _)
            }
        }
    }
}

/// Extracts the last component splitted by Delim. This function returns the same content if there
/// is no Delim.
fn extract_delim_splitted_last_tokens<'b>(
    tokens: &Spanned<'b, Vec<Token<'b>>>,
) -> Spanned<'b, Vec<Token<'b>>> {
    let is_delim = |tok: &Token| tok.data.delim().is_some();

    // Find the last component. Note that rsplit() never returns an empty iterator; even if there is
    // no Delims in tokens, rsplit() returns a iterator producing the single original content.
    let last_tokens = tokens.data.rsplit(is_delim).next().unwrap();

    // Compute the span.
    let last_tokens_span = if !last_tokens.is_empty() {
        let init = last_tokens[0].span.start_point();
        last_tokens.iter().fold(init, |s, t| s.merged(t.span))
    } else {
        // Find the delimiting Delim span or starting point of this tokens.
        tokens
            .data
            .iter()
            .rfind(|t| is_delim(t))
            .map(|t| t.span.end_point())
            .unwrap_or(tokens.span.start_point())
    };

    Spanned::new(tokens.source, last_tokens_span, last_tokens.to_vec())
}
