use crate::{
    ast::traits::{Node, Statement},
    token::token::TokenType,
};

#[derive(Debug)]
pub struct BlockStatement {
    token: TokenType,
    pub statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: TokenType) -> Self {
        Self {
            token,
            statements: vec![],
        }
    }
}

impl Node for BlockStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Statement for BlockStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }

    fn token_literal(&self) -> String {
        "".to_string()
    }

    fn string_representation(&self) -> String {
        let mut buffer = String::new();

        for stmt in &self.statements {
            buffer.push_str(&stmt.string_representation());
        }

        buffer
    }
}
