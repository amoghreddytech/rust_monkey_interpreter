use crate::ast::traits::{Expression, Statement};

#[derive(Debug)]
pub struct LetStatement {
    name: String,
    value: Option<Box<dyn Expression>>, // This could also be Vec<T> later let's see
}

impl Statement for LetStatement {
    fn string_representation(&self) -> String {
        // let token = TokenType::LET;
        // let mut buffer = String::new();
        // buffer.push_str(&token.string_representation());
        // buffer.push_str(" ");
        // buffer.push_str(" = ");

        // if let Some(value) = &self.value {
        // buffer.push_str(&value.string_representation());
        // }
        // buffer
        self.name.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl LetStatement {
    pub fn new(name: String, value: Option<Box<dyn Expression>>) -> Self {
        Self { name, value }
    }
}
