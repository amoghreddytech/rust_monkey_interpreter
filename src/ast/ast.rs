use crate::ast::traits::{Node, Statement};
use crate::{parser::parser::Parser, token::token::TokenType};
use anyhow::{Result, anyhow};

// Program structure
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
    parser: Parser,
    pub errors_from_parser: Vec<anyhow::Error>,
}

impl Node for Program {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Program {
    pub fn new(parser: Parser) -> Self {
        Self {
            statements: Vec::new(),
            parser,
            errors_from_parser: vec![],
        }
    }

    // This should only provide shared ownership and hence it should be chill
    fn get_root_statment(&self) -> Result<&dyn Statement> {
        self.statements
            .first()
            .map(|stmt| &**stmt)
            .ok_or_else(|| anyhow!("No statements in the Program"))
    }

    pub fn parse_program(&mut self) {
        while self.parser.cur_token != TokenType::EOF {
            let stmt = self.parser.parse_statement();
            match stmt {
                Some(s) => {
                    self.statements.push(s);
                }
                None => {}
            }
            self.parser.next_token()
        }
    }

    pub fn string(&self) -> String {
        let mut buffer = String::new();

        for stmt in &self.statements {
            buffer.push_str(&stmt.string_representation());
        }

        return buffer;
    }
}

#[cfg(test)]
mod tests {}
