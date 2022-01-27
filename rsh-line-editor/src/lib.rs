use crossterm::cursor::{
    position as cursor_position, Hide, MoveTo, MoveToPreviousLine, RestorePosition, SavePosition,
    Show,
};
use crossterm::event::Event;
use crossterm::event::{read, KeyCode, KeyModifiers};
use crossterm::queue;
use crossterm::style::Print;
use crossterm::terminal::size as term_size;
use itertools::Itertools;
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
pub struct LineEditor {}

impl LineEditor {
    pub fn new() -> LineEditor {
        LineEditor {}
    }

    pub fn read_line<F>(&self, prompt_writer: F) -> Result<UserInput>
    where
        F: FnOnce() -> Result<(), anyhow::Error>,
    {
        prompt_writer().map_err(Error::PromptError)?;
        read_line()
    }
}

pub trait PromptWriter {
    fn write(&mut self) -> anyhow::Result<()>;
}

fn read_line() -> Result<UserInput> {
    let mut buf = LineBuffer::new();
    let stdout = io::stdout();
    let mut printer = LinePrinter::new(stdout.lock())?;
    loop {
        if let Event::Key(key) = read()? {
            let is_ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
            match key.code {
                KeyCode::Enter => {
                    printer.print_newline()?;
                    return Ok(UserInput::String(buf.to_string()));
                }
                KeyCode::Char('d') if is_ctrl && buf.is_empty() => return Ok(UserInput::EOF),
                KeyCode::Backspace => buf.backspace(),
                KeyCode::Delete => buf.delete(),
                KeyCode::Char('b') if is_ctrl => buf.move_left(1),
                KeyCode::Char('f') if is_ctrl => buf.move_right(1),
                KeyCode::Char(ch) => buf.insert(ch),
                _ => {}
            }

            // Get current cursor position. This is the base point for readline.
            printer.update_buffer(&buf)?;
            printer.print()?;
        }
    }
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

pub struct LinePrinter<'a> {
    // This lock prevents stdout to be modified outside of LineLayout.
    stdout: StdoutLock<'a>,
    prompt_pos: Coord,
    lines: Vec<String>,
    cursor_pos: Coord,
    term_size: Coord,
}

impl<'a> LinePrinter<'a> {
    pub fn new(stdout: StdoutLock<'a>) -> Result<Self> {
        let cursor = cursor_position()?.into();
        Ok(Self {
            stdout,
            prompt_pos: cursor,
            lines: vec![],
            cursor_pos: cursor,
            term_size: term_size()?.into(),
        })
    }

    pub fn update_buffer(&mut self, buf: &LineBuffer) -> Result<()> {
        // Use consistent terminal size during layout calculation & printing.
        let term_size = Coord::from(term_size()?);
        self.term_size = term_size;

        let (lines, pos) = self.wrap_lines(buf)?;

        self.lines = lines;
        self.cursor_pos = pos;

        Ok(())
    }

    pub fn print_newline(&mut self) -> Result<()> {
        writeln!(self.stdout)?;
        Ok(())
    }

    pub fn print(&mut self) -> Result<()> {
        queue!(self.stdout, Hide)?;
        self.ensure_room()?;
        self.move_cursor_to_prompt()?;
        self.print_lines()?;
        self.move_cursor_to_input()?;
        queue!(self.stdout, Show)?;
        self.stdout.flush()?;

        Ok(())
    }

    fn wrap_lines(&self, buf: &LineBuffer) -> Result<(Vec<String>, Coord)> {
        // Update line string and cursor position
        let mut lines = Vec::new();
        let mut line = String::new();
        let mut width = self.prompt_pos.col;
        let mut pos = self.prompt_pos;

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
                if move_cursor {
                    pos.row += 1;
                    pos.col = 0;
                }
            }

            line.push(ch);
            width += ch_width;
            if move_cursor {
                pos.col += ch_width;
            }
        }

        // Add last line.
        lines.push(line);

        // if cursor is on the end of the line, move it to the beginning of the next line.
        if pos.col == self.term_size.col {
            pos.row += 1;
            pos.col = 0;
        }

        Ok((lines, pos))
    }

    fn ensure_room(&mut self) -> Result<()> {
        if self.cursor_pos.row < self.term_size.row {
            // There is no need to make room now.
            return Ok(());
        }

        // The amount needed to ensure the current cursor position < terminal size.
        let amount = self.cursor_pos.row - self.term_size.row + 1;
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
        queue!(self.stdout, SavePosition)?;
        for (idx, line) in self.lines.iter().enumerate() {
            if idx > 0 {
                queue!(self.stdout, Print('\n'))?;
            }
            queue!(self.stdout, Print(line))?;
        }
        queue!(self.stdout, RestorePosition)?;

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

pub struct LineBuffer {
    buf: Vec<char>,
    cursor_at: usize,
}

impl LineBuffer {
    pub fn new() -> Self {
        Self {
            buf: Vec::new(),
            cursor_at: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn insert(&mut self, ch: char) {
        self.buf.insert(self.cursor_at, ch);
        self.cursor_at += 1;
    }

    pub fn move_left(&mut self, n: usize) {
        self.cursor_at = self.cursor_at.saturating_sub(n);
    }

    pub fn move_right(&mut self, n: usize) {
        self.cursor_at = self.cursor_at.saturating_add(n).min(self.buf.len());
    }

    pub fn backspace(&mut self) {
        if self.cursor_at > 0 {
            self.cursor_at -= 1;
            self.delete();
        }
    }

    pub fn delete(&mut self) {
        if self.cursor_at < self.buf.len() {
            self.buf.remove(self.cursor_at);
        }
    }

    pub fn num_chars(&self) -> usize {
        self.buf.len()
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        self.buf.iter().copied()
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
