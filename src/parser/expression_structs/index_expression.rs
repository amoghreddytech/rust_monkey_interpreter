use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct IndexLiteral {
    pub token: TokenType,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl IndexLiteral {
    pub fn new(token: TokenType, left: Box<Expression>, index: Box<Expression>) -> Self {
        Self { token, left, index }
    }
}
