use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: TokenType,
    pub expression: Box<Expression>,
}

impl ExpressionStatement {
    pub fn new(token: TokenType, expression: Box<Expression>) -> Self {
        Self { token, expression }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.expression.string_literal());
        return buffer;
    }
}
