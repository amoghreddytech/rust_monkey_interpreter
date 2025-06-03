use crate::{
    TokenType,
    parser::{Expression, IdentifierLiteral},
};

// LET STATEMENT
#[derive(Debug, Clone)]
pub struct LetStatement {
    // This is going to be the let Token,
    pub token: TokenType,
    pub identifier: IdentifierLiteral,
    pub value: Option<Expression>,
}

impl LetStatement {
    pub fn new(token: TokenType, identifier: IdentifierLiteral, value: Option<Expression>) -> Self {
        Self {
            token,
            identifier,
            value: None,
        }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.token.token_literal());
        buffer.push_str(" ");
        buffer.push_str(&self.identifier.string_literal());
        buffer.push_str(&" = ");

        if let Some(val) = &self.value {
            buffer.push_str(&val.string_literal());
        }

        buffer.push_str(&";");

        buffer
    }
}
