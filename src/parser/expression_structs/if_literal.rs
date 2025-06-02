use crate::{
    TokenType,
    parser::{Expression, Statement},
};

#[derive(Debug, Clone)]
pub struct IfLiteral {
    pub alternative: Option<Box<Statement>>,
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub token: TokenType,
}

impl IfLiteral {
    pub fn new(
        token: TokenType,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    ) -> Self {
        if let Some(alt) = alternative {
            Self {
                token,
                condition,
                consequence,
                alternative: Some(alt),
            }
        } else {
            Self {
                token,
                condition,
                consequence,
                alternative: None,
            }
        }
    }
}
