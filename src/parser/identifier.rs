use crate::TokenType;
use anyhow::{Error, Result, anyhow};

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: TokenType,
    pub value: String,
}

impl Identifier {
    pub fn new(token: TokenType) -> Result<Self, Error> {
        match token {
            TokenType::IDENT(ref value) => Ok(Self {
                token: token.clone(),
                value: value.to_string(),
            }),
            _ => Err(anyhow!("Not an Ident token token recieved {:?}", token)),
        }
    }

    pub fn string_literal(&self) -> String {
        self.value.clone()
    }
}
