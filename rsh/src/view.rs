use anyhow::Result;
use std::path::{Path, MAIN_SEPARATOR};

pub fn beautify_path<P: AsRef<Path>>(path: P) -> Result<String> {
    let path = dunce::canonicalize(path.as_ref())?;

    if let Some(home) = dirs::home_dir() {
        if let Ok(rest) = path.strip_prefix(home) {
            if rest.as_os_str().is_empty() {
                return Ok("~".to_string());
            }

            return Ok(format!("~{}{}", MAIN_SEPARATOR, rest.display()));
        }
    }

    return Ok(path.display().to_string());
}
