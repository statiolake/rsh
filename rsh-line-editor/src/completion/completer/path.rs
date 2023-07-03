use super::{Completer, CompletionResult};
use crate::LineBuffer;
use rsh_line_parser::{span::Spanned, token::Atom};

pub struct PathCompleter<'b> {
    pub buf: &'b LineBuffer,
    pub target: Spanned<'b, Vec<Atom<'b>>>,
    pub in_double: bool,
    pub in_single: bool,
}

impl Completer for PathCompleter<'_> {
    fn complete(&self) -> CompletionResult {
        CompletionResult::Partial {
            partial_span: self.target.span,
            partially_replace_to: "path".to_string(),
            candidates: vec!["path: not implemented yet!".to_string()],
        }
    }
}

// pub fn run<P>(
//     printer: &mut LinePrinter<P>,
//     buf: &mut LineBuffer,
//     ctx: &CompletionContext,
//     arg: &str,
//     escape_char: Option<char>,
// ) -> Result<()> {
//     let start = match ctx.start {
//         Some(start) => start,
//         None => return Ok(()),
//     };
//
//     // Parse arg as a path
//     let path = Path::new(arg);
//     let (file_name, parent) = if arg.ends_with(MAIN_SEPARATOR) {
//         (Cow::from(""), path)
//     } else {
//         let name = path
//             .file_name()
//             .map(|name| name.to_string_lossy())
//             .unwrap_or_else(|| Cow::from(""));
//         (name, path.parent().unwrap_or_else(|| Path::new(".")))
//     };
//
//     let entries = match read_dir(parent) {
//         Ok(entries) => entries,
//         Err(_) => return Ok(()),
//     };
//     let entries: Vec<_> = entries
//         .filter_map(|entry| entry.ok())
//         .filter(|entry| {
//             entry
//                 .path()
//                 .file_name()
//                 .map(|name| {
//                     name.to_string_lossy()
//                         .to_lowercase()
//                         .starts_with(&*file_name.to_lowercase())
//                 })
//                 .unwrap_or(false)
//         })
//         .collect();
//
//     if entries.is_empty() {
//         printer.set_hints(vec!["no matching path found".to_string()]);
//     } else if entries.len() == 1 {
//         // Unique entry; complete it.
//         let entry = &entries[0];
//         let mut entry_str = entry.path().display().to_string();
//         if !ctx.in_double && !ctx.in_single {
//             if let Some(escape_char) = escape_char {
//                 let escape = |ch| format!("{}{}", escape_char, ch);
//                 entry_str = entry_str
//                     .replace(escape_char, &escape(escape_char))
//                     .replace(' ', &escape(' '))
//                     .replace('(', &escape('('))
//                     .replace(')', &escape(')'))
//                     .replace('<', &escape('<'))
//                     .replace('>', &escape('>'));
//             }
//         }
//
//         buf.replace_range(start..buf.cursor_at, entry_str.chars());
//
//         if entry.path().is_dir() {
//             // Insert path separator after name if directory
//             buf.insert_norecord(MAIN_SEPARATOR);
//         } else {
//             // Insert whitespace otherwise; close the quote if necessary.
//             if ctx.in_single || ctx.in_double {
//                 let quote = if ctx.in_single { '\'' } else { '"' };
//                 if buf.char_at(buf.cursor_at) == Some(quote) {
//                     // Go out of quote
//                     buf.move_right(1);
//                 } else {
//                     // Close this quote
//                     buf.insert_norecord(quote);
//                 }
//             }
//             // Insert whitespace
//             buf.insert_norecord(' ');
//         }
//     } else {
//         // Set list of file names
//         let entries: Vec<_> = entries
//             .into_iter()
//             .map(|entry| entry.file_name().to_string_lossy().into_owned())
//             .collect();
//         printer.set_hints(printer.format_hint_grid(&entries));
//
//         // Compute common prefix
//         fn common_prefix<'a>(a: &'a str, b: &'a str) -> &'a str {
//             let idx = (0..=a.len().min(b.len()))
//                 .rev()
//                 .find(|&idx| a[..idx] == b[..idx])
//                 .expect("internal error: even empty string is not equal");
//             &a[..idx]
//         }
//
//         // Insert **lower case** common prefix.
//         let entries: Vec<_> = entries.iter().map(|s| s.to_lowercase()).collect();
//         let mut iter = entries.iter();
//         let first = iter.next().expect("internal error: entries is empty");
//         let prefix = iter.fold(&**first, |prefix, next| common_prefix(prefix, next));
//         let mut completion = if parent != Path::new(".") {
//             parent.join(prefix).display().to_string()
//         } else {
//             prefix.to_string()
//         };
//         if !ctx.in_double && !ctx.in_single {
//             if let Some(escape_char) = escape_char {
//                 let escape = |ch| format!("{}{}", escape_char, ch);
//                 completion = completion
//                     .replace(escape_char, &escape(escape_char))
//                     .replace(' ', &escape(' '))
//                     .replace('(', &escape('('))
//                     .replace(')', &escape(')'))
//                     .replace('<', &escape('<'))
//                     .replace('>', &escape('>'));
//             }
//         }
//         buf.replace_range(start..buf.cursor_at, completion.chars());
//     }
//
//     Ok(())
// }
