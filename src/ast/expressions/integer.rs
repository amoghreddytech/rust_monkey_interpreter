use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct IntegerExpression {
    pub value: usize,
}

impl IntegerExpression {
    pub fn new(tok: TokenType) -> Self {
        match tok {
            TokenType::INT(x) => Self {
                value: x.parse().unwrap(),
            },
            _ => panic!("Not an INT token"),
        }
    }
}

impl Expression for IntegerExpression {
    fn string_representation(&self) -> String {
        self.value.to_string()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
