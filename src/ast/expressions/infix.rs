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
        if let (Some(l), Some(r)) = (&self.left, &self.right) {
            return "(".to_string()
                + &l.string_representation()
                + " "
                + &self.operator
                + " "
                + &r.string_representation();
        };

        if let Some(l) = &self.left {
            return "(".to_string()
                + &l.string_representation()
                + " "
                + &self.operator
                + " "
                + "right is none";
        };

        if let Some(r) = &self.right {
            return "(".to_string()
                + "left is none"
                + " "
                + &self.operator
                + " "
                + &r.string_representation();
        };

        "(".to_string() + "left is none" + " " + &self.operator + " " + "right is None"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
