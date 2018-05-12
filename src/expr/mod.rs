mod error;

use std::process::{Command, Stdio};

pub use self::error::{ChainableToExprError, ErrorKind, ExprError, Result};

#[derive(Debug)]
pub enum Expr {
    FnCall(Vec<Expr>),
    Literal(String),
}

impl Expr {
    pub fn child_flattened(self) -> Result<Expr> {
        debug!("about to flatten: {:?}", self);
        match self {
            Expr::Literal(_) => Ok(self),
            Expr::FnCall(args) => {
                let flattened = Expr::FnCall(args.into_iter()
                    .map(|x| x.child_flattened())
                    .collect::<Result<_>>()?);
                let mut cmd =
                    flattened.make_command(Stdio::inherit(), Stdio::piped(), Stdio::inherit())?;
                let res = cmd.output().chain_err(ErrorKind::CmdInvokationError)?;
                let res_stdout = String::from_utf8_lossy(&res.stdout).into();
                Ok(Expr::Literal(res_stdout))
            }
        }
    }

    pub fn make_toplevel_command(self) -> Result<Command> {
        self.make_command(Stdio::inherit(), Stdio::inherit(), Stdio::inherit())
    }

    fn unwrap_literal(self) -> String {
        match self {
            Expr::Literal(s) => s,
            Expr::FnCall(_) => panic!("unwrap_literal called on FnCall value."),
        }
    }

    fn unwrap_fncall(self) -> Vec<Expr> {
        match self {
            Expr::Literal(_) => panic!("unwrap_fncall called on Literal value."),
            Expr::FnCall(args) => args,
        }
    }

    fn make_command(self, stdin: Stdio, stdout: Stdio, stderr: Stdio) -> Result<Command> {
        let mut args = self.unwrap_fncall()
            .into_iter()
            .map(|x| x.child_flattened().map(|x| x.unwrap_literal()))
            .collect::<Result<Vec<_>>>()?
            .into_iter();
        let mut cmd = Command::new(args.next().ok_or(ExprError::new(ErrorKind::EmptyFnCall))?);
        cmd.args(args).stdin(stdin).stdout(stdout).stderr(stderr);
        Ok(cmd)
    }
}
