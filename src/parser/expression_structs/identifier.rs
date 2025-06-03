use crate::TokenType;
use anyhow::{Error, Result, anyhow};

#[derive(Debug, Clone)]
pub struct IdentifierLiteral {
    pub value: String,
}

impl IdentifierLiteral {
    pub fn new(token: TokenType) -> Result<Self, Error> {
        match token {
            TokenType::IDENT(ref value) => Ok(Self {
                value: value.to_string(),
            }),
            _ => Err(anyhow!("Not an Ident token token recieved {:?}", token)),
        }
    }

    pub fn string_literal(&self) -> String {
        self.value.clone()
    }
}
