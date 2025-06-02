use crate::{TokenType, parser::Statement};

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: TokenType,
    pub statements: Box<Vec<Statement>>,
}

impl BlockStatement {
    pub fn new(token: TokenType, statements: Box<Vec<Statement>>) -> Self {
        Self { token, statements }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        for stmt in &*self.statements {
            buffer.push_str(&stmt.string_representation());
        }

        buffer
    }
}
