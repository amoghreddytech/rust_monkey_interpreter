use crate::ast::traits::{Expression, Node, Statement};
use crate::token::token::TokenType;

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: TokenType, // first token of the expression
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Statement for ExpressionStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }
    fn string_representation(&self) -> String {
        let mut buffer = String::new();
        if let Some(exp) = &self.expression {
            buffer.push_str(&exp.string_representation());
        }

        buffer
    }

    fn token_literal(&self) -> String {
        self.token.string_representation()
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
