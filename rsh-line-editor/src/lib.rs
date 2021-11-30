use std::error::Error;
use std::io::stdin;

pub enum RLEError {
    PromptError(anyhow::Error),
}

pub type Result<T, E = RLEError> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct LineEditor {}

impl LineEditor {
    pub fn new() -> LineEditor {
        LineEditor {}
    }

    pub fn read_line<F>(&self, prompt_writer: F) -> Result<String>
    where
        F: FnOnce() -> Result<(), anyhow::Error>,
    {
        // TODO: measure the prompt size from the start and end of cursor position
        prompt_writer().map_err(RLEError::PromptError)?;

        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();
        Ok(buf)
    }
}

impl Default for LineEditor {
    fn default() -> Self {
        Self::new()
    }
}
