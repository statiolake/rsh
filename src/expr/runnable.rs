use std::io;
use std::process::Command;

use builtin::Builtin;

#[derive(Debug)]
pub enum Runnable {
    Command(Command),
    Builtin(Builtin),
}

impl Runnable {
    pub fn run(self) -> io::Result<Vec<u8>> {
        let stdout = match self {
            Runnable::Command(mut cmd) => {
                let res = cmd.output()?;
                res.stdout
            }
            Runnable::Builtin(_blt) => unimplemented!(),
        };
        Ok(stdout)
    }
}

impl From<Command> for Runnable {
    fn from(cmd: Command) -> Runnable {
        Runnable::Command(cmd)
    }
}

impl From<Builtin> for Runnable {
    fn from(blt: Builtin) -> Runnable {
        Runnable::Builtin(blt)
    }
}
