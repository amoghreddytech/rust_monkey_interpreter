use anyhow::{Error, anyhow};

use crate::{
    TokenType,
    parser::{IdentifierLiteral, Statement},
};

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: TokenType,
    pub body: Box<Statement>,
    pub parameters: Vec<IdentifierLiteral>,
}

impl FunctionLiteral {
    pub fn new(
        token: TokenType,
        body: Box<Statement>,
        parameters: Vec<IdentifierLiteral>,
    ) -> Result<Self, Error> {
        match *body {
            Statement::BlockStatement(_) => Ok(Self {
                token,
                body,
                parameters,
            }),
            _ => Err(anyhow!("the body was not a Block Statement")),
        }
    }
}
