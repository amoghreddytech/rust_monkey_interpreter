use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct InfixExpression {
    pub token: TokenType,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    pub fn new(token: TokenType, left: Box<dyn Expression>) -> Self {
        Self {
            token: token.clone(),
            left: Some(left),
            operator: token.string_representation(),
            right: None,
        }
    }
}

impl Expression for InfixExpression {
    fn string_representation(&self) -> String {
        let left_str = match &self.left {
            Some(l) => l.string_representation(),
            None => "left is none".to_string(),
        };

        let right_str = match &self.right {
            Some(r) => r.string_representation(),
            None => "right is none".to_string(),
        };

        format!("({} {} {})", left_str, self.operator, right_str)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
