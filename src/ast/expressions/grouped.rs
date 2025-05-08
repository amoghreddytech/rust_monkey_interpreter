use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct GroupedExpression {
    pub token: TokenType,
    pub value: Option<Box<dyn Expression>>,
}

impl GroupedExpression {
    pub fn new(token: TokenType) -> Self {
        Self { token, value: None }
    }
}

impl Expression for GroupedExpression {
    fn string_representation(&self) -> String {
        match &self.value {
            Some(value) => value.string_representation(),
            None => "There is no Expression".to_string(),
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
