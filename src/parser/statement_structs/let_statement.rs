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
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: TokenType, identifier: IdentifierLiteral, value: Expression) -> Self {
        Self {
            token,
            identifier,
            value,
        }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.token.token_literal());
        buffer.push_str(" ");
        buffer.push_str(&self.identifier.string_literal());
        buffer.push_str(&" = ");

        buffer.push_str(&self.value.string_literal());

        buffer.push_str(&";");

        buffer
    }
}
