use crate::{TokenType, parser::Expression};

// RETURN STATEMENT
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    // This is going to be the let Token,
    pub token: TokenType,
    pub return_value: Expression,
}

impl ReturnStatement {
    pub fn new(token: TokenType, expression: Expression) -> Self {
        Self {
            token,
            return_value: expression,
        }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.token.token_literal());

        buffer.push_str(&self.return_value.string_literal());
        buffer.push_str(&";");

        buffer
    }
}
