use std::collections::HashMap;

use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: TokenType,
    pub pairs: Vec<(Box<Expression>, Box<Expression>)>,
}

impl HashLiteral {
    pub fn new(token: TokenType, pairs: Vec<(Box<Expression>, Box<Expression>)>) -> Self {
        Self { token, pairs }
    }
}
