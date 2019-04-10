use crate::ast::Ast;

#[derive(Debug)]
pub struct FnCall(pub Box<Ast>, pub Vec<Ast>);
