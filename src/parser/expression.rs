use super::{
    BooleanLiteral, Identifier, IfLiteral, InfixLiteral, IntegerLiteral, PrefixLiteral,
    expression_structs::if_literal,
};

#[derive(Debug, Clone)]
pub enum Expression {
    IdentiferExpression(Identifier),
    IntegerExpression(IntegerLiteral),
    PrefixExpression(PrefixLiteral),
    InfixExpression(InfixLiteral),
    BooleanExpression(BooleanLiteral),
    IfExpression(IfLiteral),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Self::IdentiferExpression(identifier) => identifier.value.clone(),
            Self::IntegerExpression(integer_data) => integer_data.value.to_string(),
            Self::PrefixExpression(prefix_literal) => prefix_literal.token.token_literal(),
            Self::InfixExpression(infix_literal) => infix_literal.token.token_literal(),
            Self::BooleanExpression(boolean_literal) => boolean_literal.value.to_string(),
            Self::IfExpression(if_literal) => if_literal.token.token_literal(),
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
            Self::BooleanExpression(boolean_literal) => boolean_literal.value.to_string(),
            Self::IfExpression(if_literal) => {
                let mut s = format!(
                    "if{} {}",
                    if_literal.condition.string_literal(),
                    if_literal.consequence.string_representation(),
                );

                if let Some(alt) = &if_literal.alternative {
                    s.push_str(&format!("else {}", alt.string_representation()));
                }

                s
            }
        }
    }
}
