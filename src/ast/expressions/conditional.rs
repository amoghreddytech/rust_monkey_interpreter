use crate::ast::statements::BlockStatement;
use crate::ast::traits::Statement;
use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct ConditionalExpression {
    token: TokenType,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}
impl ConditionalExpression {
    pub fn new(
        token: TokenType,
        condition: Box<dyn Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl Expression for ConditionalExpression {
    fn string_representation(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str("if");

        buffer.push_str(&self.condition.string_representation());
        buffer.push_str(" ");
        buffer.push_str(&self.consequence.string_representation());
        if let Some(alt) = &self.alternative {
            buffer.push_str("else ");
            buffer.push_str(&alt.string_representation());
        }

        buffer
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
