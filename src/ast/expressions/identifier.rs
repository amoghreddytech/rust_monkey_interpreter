use crate::{ast::traits::Expression, ast::traits::Node, token::token::TokenType};

#[derive(Debug)]
pub struct IdentifierExpression {
    pub value: String,
}

// maybe we pass the token to create the IdentifierExpression
impl IdentifierExpression {
    pub fn new(tok: TokenType) -> Self {
        match tok {
            TokenType::IDENT(x) => {
                return Self { value: x };
            }
            _ => panic!("Not an IDENT token"),
        }
    }
}

impl Node for IdentifierExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Expression for IdentifierExpression {
    fn string_representation(&self) -> String {
        self.value.clone()
    }
}
