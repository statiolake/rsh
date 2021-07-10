mod shell;

use shell::Shell;

fn main() {
    let mut shell = Shell::new();
    shell.mainloop();
}
