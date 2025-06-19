use std::collections::HashMap;

use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: TokenType,
    pub pairs: HashMap<Expression, Expression>,
}

impl HashLiteral {
    pub fn new(token: TokenType, pairs: HashMap<Expression, Expression>) -> Self {
        Self { token, pairs }
    }
}
