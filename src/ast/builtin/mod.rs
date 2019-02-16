use std::result;

use crate::ast::{Ast, AstError, ErrorKind, Result};
use crate::ShellState;

pub mod builtin_impl;

#[derive(Debug)]
pub struct Builtin {
    kind: BuiltinKind,
    args: Vec<Ast>,
}

#[derive(Debug)]
pub enum BuiltinKind {
    Exit,
}

pub fn check_builtin(ast: Vec<Ast>) -> Result<result::Result<Builtin, Vec<Ast>>> {
    let mut ast = ast.into_iter();
    let cmd = ast
        .next()
        .ok_or_else(|| AstError::new(ErrorKind::EmptyFnCall))?;
    match &*cmd.flattened()? {
        "exit" => Ok(Ok(Builtin {
            kind: BuiltinKind::Exit,
            args: ast.collect(),
        })),
        other => Ok(Err(Some(Ast::Literal(other.to_string()))
            .into_iter()
            .chain(ast)
            .collect())),
    }
}

impl Builtin {
    pub fn run(self, state: &mut ShellState) -> builtin_impl::Result {
        match self.kind {
            BuiltinKind::Exit => builtin_impl::exit(state, self.args),
        }
    }
}
