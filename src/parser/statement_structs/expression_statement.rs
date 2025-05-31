use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: TokenType,
    pub expression: Option<Expression>,
}

impl ExpressionStatement {
    pub fn new(token: TokenType) -> Self {
        Self {
            token,
            expression: None,
        }
    }

    pub fn string_literal(&self) -> String {
        let mut buffer = String::new();

        if let Some(expr) = &self.expression {
            buffer.push_str(&expr.string_literal());
            return buffer;
        }

        return "".to_string();
    }
}
