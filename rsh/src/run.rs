pub mod builtin;

use self::builtin::BuiltinCommand;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum CommandKind {
    Builtin(BuiltinCommand),
    External(PathBuf),
}
