use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct IdentifierExpression {
    value: String,
}

// maybe we pass the token to create the IdentifierExpression
impl IdentifierExpression {
    pub fn new(tok: TokenType) -> Self {
        match tok {
            TokenType::IDENT(x) => return Self { value: x },
            _ => panic!("Not an IDENT token"),
        }
    }
}

impl Expression for IdentifierExpression {
    fn string_representation(&self) -> String {
        self.value.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
