use anyhow::{Error, anyhow};

use crate::TokenType;

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: TokenType) -> Result<Self, Error> {
        match token {
            TokenType::INT(x) => {
                let value = x.parse::<i64>()?;
                Ok(Self { value })
            }
            _ => Err(anyhow!(
                "found wrong tokentype in IntegerLiteral Struct creationg"
            )),
        }
    }
}
