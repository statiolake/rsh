use crate::Result;
use crate::{LineBuffer, LinePrinter};
use std::borrow::Cow;
use std::fs::read_dir;
use std::path::{Path, MAIN_SEPARATOR};

struct CompletePositionContext {
    start: Option<usize>,
    in_single: bool,
    in_double: bool,
    escaped: bool,
}

impl CompletePositionContext {
    pub fn new() -> Self {
        Self {
            start: Some(0),
            in_single: false,
            in_double: false,
            escaped: false,
        }
    }
}

pub fn handle_completion<P>(
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    escape_char: Option<char>,
) -> Result<()> {
    let end = buf.cursor_at;

    // check quotation first; is this argument quoted?
    let ctx =
        buf.chars()
            .enumerate()
            .take(end)
            .fold(CompletePositionContext::new(), |ctx, (pos, ch)| {
                match (ctx, ch) {
                    // Erroneous ctx
                    (
                        CompletePositionContext {
                            in_single: true,
                            in_double: true,
                            ..
                        },
                        _,
                    ) => {
                        unreachable!("internal error: both single- and double-quoted")
                    }

                    (
                        CompletePositionContext {
                            in_single: true,
                            escaped: true,
                            ..
                        },
                        _,
                    ) => {
                        unreachable!("internal error: both single-quoted and escaped")
                    }

                    // Escape: should be treated only when non-single quoted string.
                    (ctx @ CompletePositionContext { escaped: true, .. }, _) => {
                        CompletePositionContext {
                            escaped: false,
                            ..ctx
                        }
                    }
                    (
                        ctx @ CompletePositionContext {
                            in_single: false,
                            escaped: false,
                            ..
                        },
                        ch,
                    ) if Some(ch) == escape_char => CompletePositionContext {
                        escaped: true,
                        ..ctx
                    },

                    // Close quotation
                    (
                        ctx @ CompletePositionContext {
                            in_single: true,
                            in_double: false,
                            ..
                        },
                        '\'',
                    ) => CompletePositionContext {
                        start: None,
                        in_single: false,
                        ..ctx
                    },
                    (
                        ctx @ CompletePositionContext {
                            in_single: false,
                            in_double: true,
                            ..
                        },
                        '"',
                    ) => CompletePositionContext {
                        start: None,
                        in_double: false,
                        ..ctx
                    },

                    // Start quotation; this position is important. Next to the quote is start of argument.
                    (
                        ctx @ CompletePositionContext {
                            in_single: false,
                            in_double: false,
                            ..
                        },
                        '\'',
                    ) => CompletePositionContext {
                        start: Some(pos + 1),
                        in_single: true,
                        ..ctx
                    },
                    (
                        ctx @ CompletePositionContext {
                            in_single: false,
                            in_double: false,
                            ..
                        },
                        '"',
                    ) => CompletePositionContext {
                        start: Some(pos + 1),
                        in_double: true,
                        ..ctx
                    },

                    // Unescaped whitespace characters; argument changed. Update position.
                    (
                        ctx @ CompletePositionContext {
                            in_single: false,
                            in_double: false,
                            escaped: false,
                            ..
                        },
                        ' ',
                    ) => CompletePositionContext {
                        start: Some(pos + 1),
                        ..ctx
                    },

                    // Any other characters
                    (ctx, _) => ctx,
                }
            });

    let start = match ctx.start {
        Some(start) => start,
        None => return Ok(()),
    };

    assert!(start <= end);
    let entire = buf.chars().take(end).collect::<String>();
    let mut arg = buf
        .chars()
        .skip(start)
        .take(end - start)
        .collect::<String>();

    if !ctx.in_single {
        // Replace escaped chars
        let mut escaped = false;
        arg = arg
            .chars()
            .flat_map(|ch| match ch {
                ch if escaped => {
                    escaped = false;
                    Some(ch)
                }
                ch if !escaped && Some(ch) == escape_char => {
                    escaped = true;
                    None
                }
                ch => Some(ch),
            })
            .collect();
    }

    printer.set_hints(vec![
        format!(
            "start: {}, end: {}, entire: {}, arg: {}",
            start, end, entire, arg
        ),
        format!("in_single: {}", ctx.in_single),
        format!("in_double: {}", ctx.in_double),
    ]);

    run_completor(printer, buf, &ctx, &entire, &arg, escape_char)
}

fn run_completor<P>(
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    ctx: &CompletePositionContext,
    _entire: &str,
    arg: &str,
    escape_char: Option<char>,
) -> Result<()> {
    // Clear previous hint text
    printer.set_hints(Vec::new());

    // TODO: Support other completor
    file_completor(printer, buf, ctx, arg, escape_char)
}

fn file_completor<P>(
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    ctx: &CompletePositionContext,
    arg: &str,
    escape_char: Option<char>,
) -> Result<()> {
    let start = match ctx.start {
        Some(start) => start,
        None => return Ok(()),
    };

    // Parse arg as a path
    let path = Path::new(arg);
    let (file_name, parent) = if arg.ends_with(MAIN_SEPARATOR) {
        (Cow::from(""), path)
    } else {
        let name = path
            .file_name()
            .map(|name| name.to_string_lossy())
            .unwrap_or_else(|| Cow::from(""));
        (name, path.parent().unwrap_or_else(|| Path::new(".")))
    };

    let entries = match read_dir(parent) {
        Ok(entries) => entries,
        Err(_) => return Ok(()),
    };
    let entries: Vec<_> = entries
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .file_name()
                .map(|name| {
                    name.to_string_lossy()
                        .to_lowercase()
                        .starts_with(&*file_name.to_lowercase())
                })
                .unwrap_or(false)
        })
        .collect();

    if entries.is_empty() {
        printer.set_hints(vec!["no matching path found".to_string()]);
    } else if entries.len() == 1 {
        // Unique entry; complete it.
        let entry = &entries[0];
        let mut entry_str = entry.path().display().to_string();
        if !ctx.in_double && !ctx.in_single {
            if let Some(escape_char) = escape_char {
                let escape = |ch| format!("{}{}", escape_char, ch);
                entry_str = entry_str
                    .replace(escape_char, &escape(escape_char))
                    .replace(' ', &escape(' '))
                    .replace('(', &escape('('))
                    .replace(')', &escape(')'))
                    .replace('<', &escape('<'))
                    .replace('>', &escape('>'));
            }
        }

        buf.replace_range(start..buf.cursor_at, entry_str.chars());

        if entry.path().is_dir() {
            // Insert path separator after name if directory
            buf.insert_norecord(MAIN_SEPARATOR);
        } else {
            // Insert whitespace otherwise; close the quote if necessary.
            if ctx.in_single || ctx.in_double {
                let quote = if ctx.in_single { '\'' } else { '"' };
                if buf.char_at(buf.cursor_at) == Some(quote) {
                    // Go out of quote
                    buf.move_right(1);
                } else {
                    // Close this quote
                    buf.insert_norecord(quote);
                }
            }
            // Insert whitespace
            buf.insert_norecord(' ');
        }
    } else {
        // Set list of file names
        let entries: Vec<_> = entries
            .into_iter()
            .map(|entry| entry.file_name().to_string_lossy().into_owned())
            .collect();
        printer.set_hints(printer.format_hint_grid(&entries));

        // Compute common prefix
        fn common_prefix<'a>(a: &'a str, b: &'a str) -> &'a str {
            let idx = (0..=a.len().min(b.len()))
                .rev()
                .find(|&idx| a[..idx] == b[..idx])
                .expect("internal error: even empty string is not equal");
            &a[..idx]
        }

        // Insert **lower case** common prefix.
        let entries: Vec<_> = entries.iter().map(|s| s.to_lowercase()).collect();
        let mut iter = entries.iter();
        let first = iter.next().expect("internal error: entries is empty");
        let prefix = iter.fold(&**first, |prefix, next| common_prefix(prefix, next));
        let mut completion = if parent != Path::new(".") {
            parent.join(prefix).display().to_string()
        } else {
            prefix.to_string()
        };
        if !ctx.in_double && !ctx.in_single {
            if let Some(escape_char) = escape_char {
                let escape = |ch| format!("{}{}", escape_char, ch);
                completion = completion
                    .replace(escape_char, &escape(escape_char))
                    .replace(' ', &escape(' '))
                    .replace('(', &escape('('))
                    .replace(')', &escape(')'))
                    .replace('<', &escape('<'))
                    .replace('>', &escape('>'));
            }
        }
        buf.replace_range(start..buf.cursor_at, completion.chars());
    }

    Ok(())
}
