use anyhow::{Error, anyhow};

use crate::TokenType;

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
}

impl BooleanLiteral {
    pub fn new(token: TokenType) -> Result<Self, Error> {
        match token {
            TokenType::TRUE => Ok(Self { value: true }),
            TokenType::FALSE => Ok(Self { value: false }),
            _ => Err(anyhow!("Not a TRUE OR FALSE Token recieved {:?}", token)),
        }
    }

    pub fn get_value(&self) -> bool {
        self.value
    }
}
