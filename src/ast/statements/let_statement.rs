use crate::ast::traits::{Expression, Statement};

#[derive(Debug)]
pub struct LetStatement {
    name: String,
    value: Option<Box<dyn Expression>>, // This could also be Vec<T> later let's see
}

impl LetStatement {
    fn token_literal(&self) -> String {
        self.name.clone()
    }
}

impl Statement for LetStatement {
    fn string_representation(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.token_literal());
        buffer.push_str(" ");
        buffer.push_str(" = ");

        if let Some(value) = &self.value {
            buffer.push_str(&value.string_representation());
        }
        buffer
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn token_literal(&self) -> String {
        self.name.clone()
    }
}

impl LetStatement {
    pub fn new(name: String, value: Option<Box<dyn Expression>>) -> Self {
        Self { name, value }
    }
}
