use anyhow::ensure;
use anyhow::Result;
use itertools::Itertools;
use std::env::current_dir;
use std::ffi::OsStr;
use std::fmt::Display;
use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::process::Command;

pub struct Shell {
    loop_running: bool,
}

impl Shell {
    pub fn new() -> Shell {
        Shell {
            loop_running: false,
        }
    }

    pub fn mainloop(&mut self) {
        self.loop_running = true;
        while self.loop_running {
            if let Err(e) = self.read_and_run() {
                print_error(e);
            }
        }
    }

    pub fn read_and_run(&mut self) -> Result<()> {
        let command = read_line()?;
        let command = command.split_whitespace().collect_vec();
        self.run_command(command)?;
        Ok(())
    }

    fn run_command<S: AsRef<OsStr> + AsRef<str>>(&mut self, mut args: Vec<S>) -> Result<()> {
        // if empty, do nothing.
        if args.is_empty() {
            return Ok(());
        }

        let cmd = args.remove(0);
        let cmd: &str = cmd.as_ref();

        // handle exit
        if cmd == "exit" {
            ensure!(args.is_empty(), "exit does not take additional argument.");
            self.loop_running = false;
            return Ok(());
        }

        let mut child = Command::new(cmd).args(&args).spawn()?;
        child.wait()?;
        Ok(())
    }
}

fn print_error<D: Display>(err: D) {
    eprintln!("rsh: {}", err);
}

fn read_line() -> Result<String> {
    // show prompt
    print!("{} $ ", current_dir()?.display());
    stdout().flush()?;

    let mut buf = String::new();
    stdin().read_line(&mut buf)?;
    Ok(buf)
}
