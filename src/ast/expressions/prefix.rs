use crate::{ast::traits::Expression, token::token::TokenType};

// Opertor is a string that will either contain "!" or "-"
#[derive(Debug)]
pub struct PrefixExpression {
    token: TokenType,
    operator: String,
    right: Box<dyn Expression>,
}

impl Expression for PrefixExpression {
    fn string_representation(&self) -> String {
        "(".to_string() + &self.right.string_representation() + &self.operator.clone() + ")"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
