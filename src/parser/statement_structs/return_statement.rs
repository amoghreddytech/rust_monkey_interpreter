use crate::{TokenType, parser::Expression};

// RETURN STATEMENT
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    // This is going to be the let Token,
    pub token: TokenType,
    pub return_value: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(token: TokenType) -> Self {
        Self {
            token,
            return_value: None,
        }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.token.token_literal());

        if let Some(return_value) = &self.return_value {
            buffer.push_str(&return_value.string_literal());
        }
        buffer.push_str(&";");

        buffer
    }
}
