use std::result;

pub use crate::ast::Result;
use crate::ast::{Ast, FnCall};
use crate::ShellState;

#[derive(Debug)]
pub struct Builtin {
    kind: Kind,
    args: Vec<Ast>,
}

#[derive(Debug)]
pub enum Kind {
    Exit,
    Cd,
}

impl Builtin {
    pub fn try_from_fncall(
        fncall: FnCall,
        state: &mut ShellState,
    ) -> Result<result::Result<Builtin, FnCall>> {
        let FnCall(cmd, args) = fncall;

        let res = cmd.run(state)?;
        match &*res {
            "exit" => Ok(Ok(Builtin {
                kind: Kind::Exit,
                args,
            })),

            "cd" => Ok(Ok(Builtin {
                kind: Kind::Cd,
                args,
            })),

            _ => Ok(Err(FnCall(Box::new(Ast::Literal(res)), args))),
        }
    }

    pub fn run_toplevel(self, state: &mut ShellState) -> builtin_impl::Result {
        match self.kind {
            Kind::Exit => builtin_impl::exit(state, self.args),
            Kind::Cd => builtin_impl::cd(state, self.args),
        }
    }

    pub fn run(self, _: &mut ShellState) -> result::Result<String, builtin_impl::BuiltinError> {
        match self.kind {
            _ => Err(builtin_impl::BuiltinError::ToplevelOnlyCommand),
        }
    }
}

pub mod builtin_impl {
    use std::{error, fmt, result};

    use crate::ast::{Ast, AstError};
    use crate::ShellState;

    pub type Result = result::Result<bool, BuiltinError>;

    #[derive(Debug)]
    pub enum BuiltinError {
        ToplevelOnlyCommand,
        UnexpectedNumberOfArgument { expected: u32, found: u32 },
        AstError(AstError),
    }

    impl From<AstError> for BuiltinError {
        fn from(e: AstError) -> BuiltinError {
            BuiltinError::AstError(e)
        }
    }

    impl fmt::Display for BuiltinError {
        fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                BuiltinError::ToplevelOnlyCommand => write!(
                    b,
                    "this command is top-level command; result cannot be used."
                ),
                BuiltinError::UnexpectedNumberOfArgument { expected, found } => write!(
                    b,
                    "unexpected number of argument, expected {} but found {}",
                    expected, found
                ),
                BuiltinError::AstError(ref e) => write!(b, "error while flattening: {}", e),
            }
        }
    }

    impl error::Error for BuiltinError {}

    fn print_err(msg: impl fmt::Display) {
        use crate::COLOR_ERROR;
        use colored_print::color::ConsoleColor;
        use colored_print::colored_println;
        colored_println! {
            true;
            COLOR_ERROR, "error:";
            ConsoleColor::Reset, " {}", msg;
        }
    }

    pub fn exit(state: &mut ShellState, args: Vec<Ast>) -> Result {
        if !args.is_empty() {
            return Err(BuiltinError::UnexpectedNumberOfArgument {
                expected: 0,
                found: args.len() as u32,
            });
        }
        state.running = false;
        Ok(true)
    }

    pub fn cd(state: &mut ShellState, args: Vec<Ast>) -> Result {
        use std::env;
        if args.len() != 1 {
            return Err(BuiltinError::UnexpectedNumberOfArgument {
                expected: 1,
                found: args.len() as u32,
            });
        }

        let path = args.into_iter().next().unwrap().run(state)?;
        match env::set_current_dir(path) {
            Ok(_) => Ok(true),
            Err(e) => {
                print_err(e);
                Ok(false)
            }
        }
    }
}
