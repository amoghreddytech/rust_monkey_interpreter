use crate::ast::traits::{Expression, Statement};

#[derive(Debug)]
pub struct ReturnStatement {
    // return_value: Box<dyn Expression>,
    return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn string_representation(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str(&self.token_literal());
        if let Some(value) = &self.return_value {
            buffer.push_str(&value.string_representation());
        }
        buffer
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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
