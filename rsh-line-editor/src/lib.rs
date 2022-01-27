use crossterm::cursor::{
    position as cursor_position, Hide, MoveTo, MoveToPreviousLine, RestorePosition, SavePosition,
    Show,
};
use crossterm::event::{read, KeyCode, KeyModifiers};
use crossterm::event::{Event, KeyEvent};
use crossterm::queue;
use crossterm::style::Print;
use crossterm::terminal::{size as term_size, Clear, ClearType};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, StdoutLock, Write};
use std::mem::take;
use unicode_width::UnicodeWidthChar;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UserInput {
    String(String),
    EOF,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("rle error: prompt failed")]
    PromptError(anyhow::Error),

    #[error("rle error: terminal operation failed")]
    CrosstermError(crossterm::ErrorKind),

    #[error("rle error: IO failed")]
    IOError(io::Error),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

impl From<crossterm::ErrorKind> for Error {
    fn from(err: crossterm::ErrorKind) -> Self {
        Error::CrosstermError(err)
    }
}

#[derive(Debug)]
pub struct LineEditor {
    history: Vec<String>,
}

impl LineEditor {
    pub fn new() -> LineEditor {
        LineEditor {
            history: Vec::new(),
        }
    }

    pub fn read_line<P: PromptWriter>(&mut self, prompt_writer: P) -> Result<UserInput> {
        let mut buf = LineBuffer::new();
        let stdout = io::stdout();
        let mut printer = LinePrinter::new(stdout.lock(), prompt_writer)?;
        let mut history = History::new(&mut self.history);

        printer.print_prompt()?;
        loop {
            if let Event::Key(key) = read()? {
                if let Some(input) = handle_key(key, &mut printer, &mut buf, &mut history)? {
                    return Ok(input);
                }
            }

            // Get current cursor position. This is the base point for readline.
            printer.update_buffer(&buf)?;
            printer.print()?;
        }
    }
}

fn handle_key<P: PromptWriter>(
    key: KeyEvent,
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    history: &mut History,
) -> Result<Option<UserInput>, Error> {
    let is_ctrl = key.modifiers == KeyModifiers::CONTROL;
    let is_alt = key.modifiers == KeyModifiers::ALT;

    match key.code {
        KeyCode::Enter => {
            history.add_accepted_entry(buf.to_string());
            printer.print_accepted()?;
            return Ok(Some(UserInput::String(buf.to_string())));
        }
        KeyCode::Backspace => buf.backspace(),
        KeyCode::Delete => buf.delete(),
        KeyCode::Char('d') if is_ctrl && buf.is_empty() => {
            printer.print_accepted()?;
            return Ok(Some(UserInput::EOF));
        }
        KeyCode::Tab => printer.set_hints(vec!["this is example hint text".to_string()]),
        KeyCode::Char('d') if is_ctrl => buf.delete(),
        KeyCode::Char('b') if is_ctrl => buf.move_left(1),
        KeyCode::Char('f') if is_ctrl => buf.move_right(1),
        KeyCode::Char('a') if is_ctrl => buf.move_begin(),
        KeyCode::Char('e') if is_ctrl => buf.move_end(),
        KeyCode::Char('b') if is_alt => buf.move_left_word(),
        KeyCode::Char('f') if is_alt => buf.move_right_word(),
        KeyCode::Char('w') if is_ctrl => buf.backspace_word(),
        KeyCode::Char('d') if is_alt => buf.delete_word(),
        KeyCode::Char('l') if is_ctrl => printer.clear()?,
        KeyCode::Char('k') if is_ctrl => buf.delete_after(),
        // FIXME: '/' cannot be mapped...
        KeyCode::Char('y') if is_ctrl => buf.redo_edit(),
        KeyCode::Char('z') if is_ctrl => buf.undo_edit(),
        KeyCode::Char('p') if is_ctrl => *buf = history.prev_history(take(buf)),
        KeyCode::Char('n') if is_ctrl => *buf = history.next_history(take(buf)),
        KeyCode::Char('r') if is_ctrl => handle_reverse_search(printer, buf, history)?,
        KeyCode::Char('s') if is_ctrl => handle_forward_search(printer, buf, history)?,
        KeyCode::Char(ch) if !is_ctrl => buf.insert(ch),
        _ => {}
    }

    Ok(None)
}

fn handle_reverse_search<P>(
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    history: &mut History,
) -> Result<()> {
    let orig_history_idx = history.current_idx();
    let mut curr_history_idx = orig_history_idx;
    let mut query = String::new();
    loop {
        if let Event::Key(key) = read()? {
            let is_ctrl = key.modifiers == KeyModifiers::CONTROL;
            let is_none = key.modifiers == KeyModifiers::NONE;
            match key.code {
                KeyCode::Enter => break,
                KeyCode::Backspace => {
                    query.pop();
                }
                KeyCode::Char('r') if is_ctrl => {
                    curr_history_idx = history.current_idx();
                }
                KeyCode::Char(ch) if is_none => {
                    query.push(ch);
                    curr_history_idx = orig_history_idx;
                }
                _ => {}
            }
        }

        *buf = history.recall(take(buf), curr_history_idx);
        let (found, next_buf) = history.reverse_search(take(buf), |s| {
            s.to_lowercase().contains(&query.to_lowercase())
        });
        *buf = next_buf;

        printer.update_buffer(buf)?;
        printer.set_hints(vec![format!(
            "({}r-search) {}",
            if found { "" } else { "failing " },
            query
        )]);
        printer.print()?;
    }

    printer.set_hints(Vec::new());

    Ok(())
}

fn handle_forward_search<P>(
    printer: &mut LinePrinter<P>,
    buf: &mut LineBuffer,
    history: &mut History,
) -> Result<()> {
    todo!()
}

#[derive(Debug)]
struct History<'a> {
    history: &'a mut Vec<String>,
    local_history: HashMap<usize, LineBuffer>,
    history_idx: usize,
}

impl<'a> History<'a> {
    pub fn new(history: &'a mut Vec<String>) -> Self {
        let history_idx = history.len();
        Self {
            history,
            local_history: HashMap::new(),
            history_idx,
        }
    }

    pub fn add_accepted_entry(&mut self, s: String) {
        self.history.push(s);
        self.history_idx = self.history.len();
        self.local_history.clear();
    }

    pub fn current_idx(&self) -> usize {
        self.history_idx
    }

    pub fn prev_history(&mut self, buf: LineBuffer) -> LineBuffer {
        self.recall(buf, self.history_idx.saturating_sub(1))
    }

    pub fn next_history(&mut self, buf: LineBuffer) -> LineBuffer {
        self.recall(buf, (self.history_idx + 1).min(self.history.len()))
    }

    pub fn reverse_search<F>(&mut self, buf: LineBuffer, pred: F) -> (bool, LineBuffer)
    where
        F: Fn(&str) -> bool,
    {
        let idx = (0..=self.history_idx)
            .rev()
            .map(|idx| (idx, self.peek(idx)))
            .find(|(_, s)| pred(s))
            .map(|(idx, _)| idx);
        match idx {
            Some(idx) => (true, self.recall(buf, idx)),
            None => (false, buf),
        }
    }

    pub fn recall(&mut self, buf: LineBuffer, idx: usize) -> LineBuffer {
        self.local_history.insert(self.history_idx, buf);
        if idx > self.history.len() {
            panic!(
                "internal error: invalid history id {} (<= {})",
                idx,
                self.history.len()
            );
        }

        self.history_idx = idx;
        self.local_history
            .remove(&idx)
            .unwrap_or_else(|| LineBuffer::from(&*self.history[idx]))
    }

    fn peek(&mut self, idx: usize) -> String {
        self.local_history
            .get(&idx)
            .map(|buf| buf.to_string())
            .or_else(|| self.history.get(idx).cloned())
            .unwrap_or_default()
    }
}

pub trait PromptWriter {
    fn write<W: Write>(&mut self, out: &mut W) -> anyhow::Result<()>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Coord {
    col: u16,
    row: u16,
}

impl Coord {
    pub fn new(col: u16, row: u16) -> Self {
        Self { col, row }
    }
}

impl From<(u16, u16)> for Coord {
    fn from((col, row): (u16, u16)) -> Self {
        Coord::new(col, row)
    }
}

impl From<Coord> for (u16, u16) {
    fn from(coord: Coord) -> Self {
        (coord.col, coord.row)
    }
}

pub struct LinePrinter<'a, P> {
    // This lock prevents stdout to be modified outside of LineLayout.
    stdout: StdoutLock<'a>,
    prompt_writer: P,
    prompt_pos: Coord,
    lines: Vec<String>,
    cursor_pos: Coord,
    end_cursor_pos: Coord,
    term_size: Coord,
    hint_lines: Vec<String>,
}

impl<'a, P> LinePrinter<'a, P> {
    pub fn new(stdout: StdoutLock<'a>, prompt_writer: P) -> Result<Self> {
        let cursor = cursor_position()?.into();
        Ok(Self {
            stdout,
            prompt_writer,
            prompt_pos: cursor,
            lines: vec![],
            cursor_pos: cursor,
            end_cursor_pos: cursor,
            term_size: term_size()?.into(),
            hint_lines: vec![],
        })
    }

    pub fn update_buffer(&mut self, buf: &LineBuffer) -> Result<()> {
        // Use consistent terminal size during layout calculation & printing.
        let term_size = Coord::from(term_size()?);
        self.term_size = term_size;

        let (lines, cursor_pos, end_cursor_pos) = self.wrap_lines(buf)?;

        self.lines = lines;
        self.cursor_pos = cursor_pos;
        self.end_cursor_pos = end_cursor_pos;

        Ok(())
    }

    pub fn set_hints(&mut self, hint_lines: Vec<String>) {
        self.hint_lines = hint_lines;
    }

    pub fn print_prompt(&mut self) -> Result<()>
    where
        P: PromptWriter,
    {
        self.prompt_writer
            .write(&mut self.stdout)
            .map_err(Error::PromptError)?;
        self.stdout.flush()?;

        // Update cursor positions
        let pos = cursor_position()?.into();
        self.prompt_pos = pos;
        self.cursor_pos = pos;
        self.end_cursor_pos = pos;

        Ok(())
    }

    pub fn clear(&mut self) -> Result<()>
    where
        P: PromptWriter,
    {
        queue!(self.stdout, Hide)?;
        // Scroll entire screen
        self.scroll_up(self.term_size.row)?;
        // Move to top-left and write normally
        queue!(self.stdout, MoveTo(0, 0))?;
        self.print_prompt()?;
        self.print()?;
        queue!(self.stdout, Show)?;
        self.stdout.flush()?;

        Ok(())
    }

    pub fn print_accepted(&mut self) -> Result<()> {
        let (col, row) = self.end_cursor_pos.into();
        queue!(
            self.stdout,
            MoveTo(col, row),
            Print('\n'),
            Clear(ClearType::UntilNewLine),
            Clear(ClearType::FromCursorDown)
        )?;
        self.stdout.flush()?;

        Ok(())
    }

    pub fn print(&mut self) -> Result<()> {
        queue!(self.stdout, Hide)?;
        self.ensure_room()?;
        self.move_cursor_to_prompt()?;
        self.print_lines()?;
        self.print_hints()?;
        self.move_cursor_to_input()?;
        queue!(self.stdout, Show)?;
        self.stdout.flush()?;

        Ok(())
    }

    /// Returns (lines, cursor_pos, end_cursor_pos). end_cursor_pos is the cursor position at the
    /// end of input.
    fn wrap_lines(&self, buf: &LineBuffer) -> Result<(Vec<String>, Coord, Coord)> {
        // Update line string and cursor position
        let mut lines = Vec::new();
        let mut line = String::new();
        let mut width = self.prompt_pos.col;
        let mut cursor_pos = self.prompt_pos;
        let mut end_cursor_pos = self.prompt_pos;

        for (idx, ch) in buf.chars().enumerate() {
            // FIXME: Investigate the size becomes None?
            let ch_width = ch.width().unwrap_or(0) as u16;

            // Wrap if current char width is too wide to fit in the current line.
            let should_wrap = width + ch_width > self.term_size.col;

            // If current position is before the cursor, increment cursor position as well.
            let move_cursor = idx < buf.cursor_at;

            if should_wrap {
                lines.push(take(&mut line));
                width = 0;
                end_cursor_pos.row += 1;
                end_cursor_pos.col = 0;
                if move_cursor {
                    cursor_pos = end_cursor_pos;
                }
            }

            line.push(ch);
            width += ch_width;
            end_cursor_pos.col += ch_width;
            if move_cursor {
                cursor_pos = end_cursor_pos;
            }
        }

        // Add last line.
        lines.push(line);

        // if cursor is on the end of the line, move it to the beginning of the next line.
        if cursor_pos.col == self.term_size.col {
            cursor_pos.row += 1;
            cursor_pos.col = 0;
        }
        if end_cursor_pos.col == self.term_size.col {
            end_cursor_pos.row += 1;
            end_cursor_pos.col = 0;
        }

        Ok((lines, cursor_pos, end_cursor_pos))
    }

    fn ensure_room(&mut self) -> Result<()> {
        let row = self.end_cursor_pos.row + self.hint_lines.len() as u16;
        if row < self.term_size.row {
            // There is no need to make room now.
            return Ok(());
        }

        // The amount needed to ensure the current cursor position < terminal size.
        let amount = row - self.term_size.row + 1;
        self.scroll_up(amount)?;

        Ok(())
    }

    fn scroll_up(&mut self, amount: u16) -> Result<()> {
        // FIXME: Find reliable way to scroll back to the previous contents in order to redraw long
        // input string at the prompt start. It seems very difficult: ScrollUp and ScrollDown on
        // Windows does not move things outside current viewport, and console API such as
        // ScrollConsoleScreenBuffer() doesn't work well with terminal emulators (Windows Terminal,
        // WezTerm and so on.)
        //
        // Take that into consideration, currently there's no meaning to scroll more than terminal
        // size.
        let amount = amount.min(self.term_size.row);

        // Move cursor to the last line and print '\n' as needed.
        let last_row = self.term_size.row - 1;
        queue!(self.stdout, SavePosition, MoveTo(0, last_row))?;
        for _ in 0..amount {
            queue!(self.stdout, Print('\n'))?;
        }

        // Update positions according to scrolling amount.
        // FIXME: if amount is equal to terminal size, it essentially clears entire screen. In that
        // case, prompt position may be above the viewport, so the calculated prompt position is
        // "one line under" the actual prompt.
        self.prompt_pos.row = self.prompt_pos.row.saturating_sub(amount);
        self.cursor_pos.row = self.cursor_pos.row.saturating_sub(amount);

        // Restore cursor position
        queue!(self.stdout, RestorePosition, MoveToPreviousLine(amount))?;

        Ok(())
    }

    fn print_lines(&mut self) -> Result<()> {
        for (idx, line) in self.lines.iter().enumerate() {
            let col = if idx == 0 { self.prompt_pos.col } else { 0 };
            let row = self.prompt_pos.row + idx as u16;
            queue!(self.stdout, MoveTo(col, row), Print(line))?;
        }

        let (col, row) = self.end_cursor_pos.into();
        queue!(
            self.stdout,
            MoveTo(col, row),
            Clear(ClearType::FromCursorDown),
            Clear(ClearType::UntilNewLine)
        )?;

        Ok(())
    }

    fn print_hints(&mut self) -> Result<()> {
        let (_, row) = self.end_cursor_pos.into();
        for (idx, line) in self.hint_lines.iter().enumerate() {
            let line: String = line
                .chars()
                .scan(0, |w, ch| {
                    *w += ch.width().unwrap_or(0) as u16;
                    Some((*w, ch))
                })
                .take_while(|(w, _)| *w <= self.term_size.col)
                .map(|(_, ch)| ch)
                .collect();
            queue!(self.stdout, MoveTo(0, row + idx as u16 + 1), Print(line))?;
        }

        Ok(())
    }

    fn move_cursor_to_prompt(&mut self) -> Result<()> {
        let (col, row) = self.prompt_pos.into();
        queue!(self.stdout, MoveTo(col, row))?;

        Ok(())
    }

    fn move_cursor_to_input(&mut self) -> Result<()> {
        let (col, row) = self.cursor_pos.into();
        queue!(self.stdout, MoveTo(col, row))?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LineBuffer {
    prev_buffer: Option<Box<LineBuffer>>,
    next_buffer: Option<Box<LineBuffer>>,
    buf: Vec<char>,
    cursor_at: usize,
}

impl From<&str> for LineBuffer {
    fn from(s: &str) -> Self {
        let buf = s.chars().collect_vec();
        LineBuffer {
            prev_buffer: None,
            next_buffer: None,
            cursor_at: buf.len(),
            buf,
        }
    }
}

impl LineBuffer {
    pub fn new() -> Self {
        Self {
            prev_buffer: None,
            next_buffer: None,
            buf: Vec::new(),
            cursor_at: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn insert(&mut self, ch: char) {
        self.record_history();
        self.buf.insert(self.cursor_at, ch);
        self.cursor_at += 1;
    }

    pub fn move_left(&mut self, n: usize) {
        self.cursor_at = self.cursor_at.saturating_sub(n);
    }

    pub fn move_right(&mut self, n: usize) {
        self.cursor_at = self.cursor_at.saturating_add(n).min(self.buf.len());
    }

    pub fn move_left_word(&mut self) {
        self.cursor_at = self.word_start_before(self.cursor_at);
    }

    pub fn move_right_word(&mut self) {
        self.cursor_at = self.word_start_after(self.cursor_at);
    }

    pub fn backspace_word(&mut self) {
        self.record_history();
        let start = self.word_start_before(self.cursor_at);
        self.buf.drain(start..self.cursor_at);
        self.cursor_at = start;
    }

    pub fn delete_word(&mut self) {
        self.record_history();
        let end = self.word_end_after(self.cursor_at);
        self.buf.drain(self.cursor_at..end);
    }

    pub fn delete_after(&mut self) {
        self.record_history();
        self.buf.drain(self.cursor_at..);
    }

    pub fn move_begin(&mut self) {
        self.cursor_at = 0;
    }

    pub fn move_end(&mut self) {
        self.cursor_at = self.buf.len();
    }

    pub fn backspace(&mut self) {
        self.record_history();
        if self.cursor_at > 0 {
            self.cursor_at -= 1;
            self.delete();
        }
    }

    pub fn delete(&mut self) {
        self.record_history();
        if self.cursor_at < self.buf.len() {
            self.buf.remove(self.cursor_at);
        }
    }

    pub fn redo_edit(&mut self) {
        let next = self.next_buffer.take();
        if let Some(mut next) = next {
            next.prev_buffer = Some(Box::new(self.clone()));
            *self = *next;
        }
    }

    pub fn undo_edit(&mut self) {
        let prev = self.prev_buffer.take();
        if let Some(mut prev) = prev {
            prev.next_buffer = Some(Box::new(self.clone()));
            *self = *prev;
        }
    }

    pub fn num_chars(&self) -> usize {
        self.buf.len()
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        self.buf.iter().copied()
    }

    fn word_start_before(&self, n: usize) -> usize {
        self.buf
            .iter()
            .enumerate()
            .take(n)
            .rev()
            .skip_while(|(_, ch)| ch.is_whitespace())
            .find(|(_, ch)| ch.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(0)
    }

    fn word_start_after(&self, n: usize) -> usize {
        self.buf
            .iter()
            .enumerate()
            .skip(n)
            .skip_while(|(_, ch)| !ch.is_whitespace())
            .find(|(_, ch)| !ch.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(self.buf.len())
    }

    fn word_end_after(&self, n: usize) -> usize {
        self.buf
            .iter()
            .enumerate()
            .skip(n)
            .skip_while(|(_, ch)| ch.is_whitespace())
            .find(|(_, ch)| ch.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(self.buf.len())
    }

    fn record_history(&mut self) {
        let cloned = self.clone();
        self.prev_buffer = Some(Box::new(cloned));
        self.next_buffer = None;
    }
}

impl Default for LineBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for LineBuffer {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "{}", self.buf.iter().format(""))
    }
}

impl Default for LineEditor {
    fn default() -> Self {
        Self::new()
    }
}
