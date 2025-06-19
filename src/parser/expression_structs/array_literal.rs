use anyhow::{Error, anyhow};

use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: TokenType,
    pub elements: Vec<Box<Expression>>,
}

impl ArrayLiteral {
    pub fn new(token: TokenType, elements: Vec<Box<Expression>>) -> Result<Self, Error> {
        match token {
            TokenType::LBRACKET => Ok(Self { token, elements }),
            _ => Err(anyhow!(
                "Found wrong tokentype in arrayLiteral created got {:?}",
                token
            )),
        }
    }
}
