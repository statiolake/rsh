use std::result;

use super::Ast;
use super::{AstError, ErrorKind, Result};

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
