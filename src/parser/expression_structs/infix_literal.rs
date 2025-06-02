use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct InfixLiteral {
    pub token: TokenType,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl InfixLiteral {
    pub fn new(
        token: TokenType,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    ) -> Self {
        Self {
            token,
            left,
            operator,
            right,
        }
    }
}
