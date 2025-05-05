use crate::ast::traits::{Expression, Statement};
use crate::parser::parser::Parser;
use crate::token::token::TokenType;

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: TokenType, // first token of the expression
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn string_representation(&self) -> String {
        let mut buffer = String::new();
        if let Some(exp) = &self.expression {
            buffer.push_str(&exp.string_representation());
        }

        buffer
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ExpressionStatement {
    pub fn new(token: TokenType) -> Self {
        Self {
            token,
            expression: None,
        }
    }
}
