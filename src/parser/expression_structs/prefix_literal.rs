use anyhow::{Error, anyhow};

use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct PrefixLiteral {
    pub token: TokenType,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixLiteral {
    pub fn new(
        token: TokenType,
        operator: String,
        expression: Box<Expression>,
    ) -> Result<Self, Error> {
        match token {
            TokenType::BANG | TokenType::MINUS => Ok(Self {
                token,
                operator,
                right: expression,
            }),
            _ => Err(anyhow!("found wrong tokentype in Prefix Struct creation")),
        }
    }
}
