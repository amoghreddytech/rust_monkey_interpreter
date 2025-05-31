use super::identifier::Identifier;
use crate::TokenType;

#[derive(Debug, Clone)]
pub enum Expression {
    IdentiferExpression(Identifier),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Self::IdentiferExpression(identifier) => identifier.token.token_literal(),
        }
    }

    pub fn string_literal(&self) -> String {
        match self {
            Expression::IdentiferExpression(identifier) => todo!(),
        }
    }
}
