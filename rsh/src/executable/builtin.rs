mod cd;
mod exit;
mod set;
mod which;

use self::{cd::CmdCd, exit::CmdExit, set::CmdSet, which::CmdWhich};
use super::Executable;

pub fn find(cmd: &str, args: Vec<String>) -> Option<Box<dyn Executable>> {
    match cmd {
        "exit" => Some(Box::new(CmdExit::new(args))),
        "cd" => Some(Box::new(CmdCd::new(args))),
        "which" => Some(Box::new(CmdWhich::new(args))),
        "set" => Some(Box::new(CmdSet::new(args))),
        _ => None,
    }
}
