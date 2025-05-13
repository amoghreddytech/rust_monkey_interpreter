use crate::{ast::traits::Expression, ast::traits::Node, token::token::TokenType};

// Opertor is a string that will either contain "!" or "-"
#[derive(Debug)]
pub struct PrefixExpression {
    pub token: TokenType,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for PrefixExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl PrefixExpression {
    pub fn new(token: TokenType) -> Self {
        match token {
            TokenType::BANG => Self {
                token: token.clone(),
                operator: token.string_representation(),
                right: None,
            },

            TokenType::MINUS => Self {
                token: token.clone(),
                operator: token.string_representation(),
                right: None,
            },

            TokenType::PLUS => Self {
                token: token.clone(),
                operator: token.string_representation(),
                right: None,
            },

            _ => panic!("The token provided {:?} cannot be a prefix operator", token),
        }
    }
}

impl Expression for PrefixExpression {
    fn string_representation(&self) -> String {
        if let Some(expr) = &self.right {
            "(".to_string() + &self.operator.clone() + &expr.string_representation() + ")"
        } else {
            "(".to_string() + "None" + &self.operator.clone() + ")"
        }
    }
}
