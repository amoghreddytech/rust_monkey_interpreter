use super::{
    BooleanLiteral, CallLiteral, FunctionLiteral, IdentifierLiteral, IfLiteral, InfixLiteral,
    IntegerLiteral, PrefixLiteral, StringLiteral,
};

#[derive(Debug, Clone)]
pub enum Expression {
    IdentiferExpression(IdentifierLiteral),
    IntegerExpression(IntegerLiteral),
    PrefixExpression(PrefixLiteral),
    InfixExpression(InfixLiteral),
    BooleanExpression(BooleanLiteral),
    IfExpression(IfLiteral),
    FunctionExpression(FunctionLiteral),
    CallExpression(CallLiteral),
    StringExpression(StringLiteral),
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
            Self::FunctionExpression(func_literal) => func_literal.token.token_literal(),
            Self::CallExpression(call_literal) => call_literal.token.token_literal(),
            Self::StringExpression(string_literal) => string_literal.value.clone(),
        }
    }

    pub fn string_literal(&self) -> String {
        match self {
            Self::IdentiferExpression(identifier) => identifier.value.clone(),
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

            Self::FunctionExpression(func_literal) => {
                let params: String = func_literal
                    .parameters
                    .iter()
                    .map(|c| c.string_literal())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "{}({}){}",
                    func_literal.token.token_literal(),
                    params,
                    func_literal.body.string_representation()
                )
            }

            Self::CallExpression(call_literal) => {
                let arguments: String = call_literal
                    .arguments
                    .iter()
                    .map(|a| a.string_literal())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{}({})", call_literal.function.string_literal(), arguments,)
            }

            Self::StringExpression(string_literal) => {
                format!("{}", string_literal.value)
            }
        }
    }
}
