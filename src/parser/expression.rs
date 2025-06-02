use super::{Identifier, InfixLiteral, IntegerLiteral, PrefixLiteral};

#[derive(Debug, Clone)]
pub enum Expression {
    IdentiferExpression(Identifier),
    IntegerExpression(IntegerLiteral),
    PrefixExpression(PrefixLiteral),
    InfixExpression(InfixLiteral),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Self::IdentiferExpression(identifier) => identifier.value.clone(),
            Self::IntegerExpression(integer_data) => integer_data.value.to_string(),
            Self::PrefixExpression(prefix_literal) => prefix_literal.token.token_literal(),
            Self::InfixExpression(infix_literal) => infix_literal.token.token_literal(),
        }
    }

    pub fn string_literal(&self) -> String {
        match self {
            Expression::IdentiferExpression(identifier) => identifier.value.clone(),
            Self::IntegerExpression(integer_data) => integer_data.value.to_string(),
            Self::PrefixExpression(prefix_literal) => {
                format!(
                    "({}{})",
                    prefix_literal.operator,
                    prefix_literal.right.string_literal()
                )
            }
            Self::InfixExpression(infix_literal) => {
                format!(
                    "({} {} {})",
                    infix_literal.left.string_literal(),
                    infix_literal.operator,
                    infix_literal.right.string_literal()
                )
            }
        }
    }
}
