use std::result;

use crate::ast::Ast;
use crate::ast::Result;
use crate::fncall::FnCall;
use crate::print_macros::COLOR_RESET;
use crate::println;
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
    Split,
    Trim,
}

impl Builtin {
    pub fn try_from_fncall(
        fncall: FnCall,
        state: &mut ShellState,
    ) -> Result<result::Result<Builtin, FnCall>> {
        let FnCall(cmd, args) = fncall;

        let res = cmd.run_get_string(state)?;

        match &*res {
            "exit" => Ok(Ok(Builtin {
                kind: Kind::Exit,
                args,
            })),

            "cd" => Ok(Ok(Builtin {
                kind: Kind::Cd,
                args,
            })),

            "split" => Ok(Ok(Builtin {
                kind: Kind::Split,
                args,
            })),

            "trim" => Ok(Ok(Builtin {
                kind: Kind::Trim,
                args,
            })),

            _ => Ok(Err(FnCall(Box::new(Ast::Literal(res)), args))),
        }
    }

    pub fn run_toplevel(self, state: &mut ShellState) -> builtin_impl::TopLevelResult {
        match self.kind {
            Kind::Exit => builtin_impl::exit(state, self.args),
            Kind::Cd => builtin_impl::cd(state, self.args),
            _ => {
                let result = self.run(state)?;
                println!(COLOR_RESET => "{:?}", result);
                Ok(true)
            }
        }
    }

    pub fn run(self, state: &mut ShellState) -> builtin_impl::ChildResult {
        match self.kind {
            Kind::Split => builtin_impl::split(state, self.args),
            Kind::Trim => builtin_impl::trim(state, self.args),
            _ => Err(builtin_impl::BuiltinError::ToplevelOnlyCommand),
        }
    }
}

pub mod builtin_impl {
    use crate::ast::{Ast, AstError};
    use crate::print_macros::{COLOR_ERROR, COLOR_RESET};
    use crate::println;
    use crate::ShellState;
    use std::{error, fmt, result};

    type Result<T> = result::Result<T, BuiltinError>;
    pub type TopLevelResult = Result<bool>;
    pub type ChildResult = Result<Vec<String>>;

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
        println! {
            COLOR_ERROR => "error:";
            COLOR_RESET => " {}", msg;
        }
    }

    pub fn exit(state: &mut ShellState, args: Vec<Ast>) -> TopLevelResult {
        expect_num_args(&args, 0)?;
        state.running = false;
        Ok(true)
    }

    pub fn cd(state: &mut ShellState, args: Vec<Ast>) -> TopLevelResult {
        let path = unwrap_one_arg(args)?.run_get_string(state)?;
        match std::env::set_current_dir(path) {
            Ok(_) => Ok(true),
            Err(e) => {
                print_err(e);
                Ok(false)
            }
        }
    }

    pub fn split(state: &mut ShellState, args: Vec<Ast>) -> ChildResult {
        let value = unwrap_one_arg(args)?.run_get_string(state)?;
        Ok(value
            .split('\n')
            .filter(|x| !x.is_empty())
            .map(|x| x.trim().into())
            .collect())
    }

    pub fn trim(state: &mut ShellState, args: Vec<Ast>) -> ChildResult {
        let value = unwrap_one_arg(args)?.run_get_string(state)?;
        Ok(vec![value.trim().into()])
    }

    fn expect_num_args(args: &[Ast], num: usize) -> Result<()> {
        if args.len() != num {
            Err(BuiltinError::UnexpectedNumberOfArgument {
                expected: 1,
                found: args.len() as _,
            })
        } else {
            Ok(())
        }
    }

    fn unwrap_one_arg(args: Vec<Ast>) -> Result<Ast> {
        expect_num_args(&args, 1)?;
        Ok(args.into_iter().next().unwrap())
    }
}
