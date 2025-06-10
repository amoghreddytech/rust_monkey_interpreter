use anyhow::{Error, anyhow};

use crate::TokenType;

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: TokenType) -> Result<Self, Error> {
        match token {
            TokenType::String(s) => Ok(Self { value: s }),
            _ => Err(anyhow!("Not a string token got {:?}", token)),
        }
    }
}
