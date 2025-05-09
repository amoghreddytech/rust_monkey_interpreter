use crate::{
    ast::{
        statements::BlockStatement,
        traits::{Expression, Statement},
    },
    token::token::TokenType,
};

use super::IdentifierExpression;

#[derive(Debug)]
pub struct FunctionExpression {
    token: TokenType,
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl FunctionExpression {
    pub fn new(
        token: TokenType,
        parameters: Vec<IdentifierExpression>,
        body: BlockStatement,
    ) -> Self {
        Self {
            token,
            parameters,
            body,
        }
    }
}

impl Expression for FunctionExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn string_representation(&self) -> String {
        let mut buffer = String::new();
        let mut parameters: Vec<String> = vec![];

        for (_, p) in self.parameters.iter().enumerate() {
            parameters.push(p.string_representation())
        }

        buffer.push_str(&self.token.string_representation());
        buffer.push_str(&"(");
        buffer.push_str(&parameters.join(", "));
        buffer.push_str(&")");
        buffer.push_str(&self.body.string_representation());

        buffer
    }
}
