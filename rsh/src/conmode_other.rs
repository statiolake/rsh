// TODO: add appropriate impl
pub struct ConsoleModeKeeper;

impl ConsoleModeKeeper {
    pub fn new() -> Result<Self> {
        Ok(Self)
    }
}

impl Drop for ConsoleModeKeeper {
    fn drop(&mut self) {}
}
