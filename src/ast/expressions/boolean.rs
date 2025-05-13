use crate::{ast::traits::Expression, ast::traits::Node, token::token::TokenType};

#[derive(Debug)]
pub struct BooleanExpression {
    pub token: TokenType,
    pub value: bool,
}

impl Node for BooleanExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl BooleanExpression {
    pub fn new(token: TokenType) -> Self {
        match token {
            TokenType::TRUE => {
                return Self {
                    token: token.clone(),
                    value: true,
                };
            }
            TokenType::FALSE => {
                return Self {
                    token: token.clone(),
                    value: false,
                };
            }
            _ => panic!(""),
        }
    }
}

impl Expression for BooleanExpression {
    fn string_representation(&self) -> String {
        if self.value == true {
            return "true".to_string();
        } else {
            return "false".to_string();
        }
    }
}
