use crate::ast::traits::{Expression, Node, Statement};

#[derive(Debug)]
pub struct ReturnStatement {
    // return_value: Box<dyn Expression>,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn as_node(&self) -> &dyn Node {
        self
    }

    fn string_representation(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.token_literal());
        if let Some(value) = &self.return_value {
            buffer.push_str(&value.string_representation());
        }
        buffer
    }

    fn token_literal(&self) -> String {
        "return".to_string()
    }
}

impl ReturnStatement {
    pub fn new(return_value: Option<Box<dyn Expression>>) -> Self {
        Self { return_value }
    }
}
