use once_cell::sync::Lazy;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::sync::Mutex;
use termcolor::WriteColor;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream};

pub static STDOUT: Lazy<Mutex<StandardStream>> =
    Lazy::new(|| Mutex::new(StandardStream::stdout(ColorChoice::Auto)));
// pub static STDERR: Lazy<Mutex<StandardStream>> =
//     Lazy::new(|| Mutex::new(StandardStream::stdout(ColorChoice::Auto)));

pub static COLOR_RESET: Lazy<ColorSpec> = Lazy::new(ColorSpec::new);

pub static COLOR_ERROR: Lazy<ColorSpec> = Lazy::new(|| {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(Color::Red));
    spec
});

pub static COLOR_INFO: Lazy<ColorSpec> = Lazy::new(|| {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(Color::Cyan));
    spec
});

pub fn print_with_color(
    stream: &mut StandardStream,
    color: &ColorSpec,
    args: fmt::Arguments,
) -> io::Result<()> {
    stream.flush()?;
    stream.set_color(color)?;
    stream.write_fmt(args)?;
    stream.flush()?;

    Ok(())
}

#[macro_export]
macro_rules! print {
    ($($color:expr => $fmt:literal $(, $args:expr)*$(,)?);*$(;)?) => {
        $(
            $crate::print_macros::print_with_color(
                &mut *$crate::print_macros::STDOUT.lock().unwrap(),
                &*$color,
                format_args!($fmt $(, $args)*),
            ).unwrap();
        )*
    };
}

#[macro_export]
macro_rules! println {
    ($($tokens:tt)*) => {
        $crate::print!($($tokens)*);
        $crate::print!($crate::print_macros::COLOR_RESET => "\n");
    };
}
