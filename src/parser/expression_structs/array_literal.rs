use crate::{TokenType, parser::Expression};

pub struct ArrayLiteral {
    pub token: TokenType,
    pub elements: Vec<Box<Expression>>,
}
