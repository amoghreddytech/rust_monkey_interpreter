use crate::{
    ast::{
        expressions::IdentifierExpression,
        traits::{Expression, Node, Statement},
    },
    token::token::TokenType,
};

#[derive(Debug)]
pub struct LetStatement {
    pub token: TokenType,
    pub name: IdentifierExpression,
    pub value: Option<Box<dyn Expression>>, // This could also be Vec<T> later let's see
}

impl Node for LetStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    fn token_literal(&self) -> String {
        "let".to_string()
    }
}

impl LetStatement {
    pub fn new(
        token: TokenType,
        name: IdentifierExpression,
        value: Option<Box<dyn Expression>>,
    ) -> Self {
        Self { token, name, value }
    }
}

// type LestStatment {
// token : Tokentype,
// name: *Identifier
// value : Expression
// }
