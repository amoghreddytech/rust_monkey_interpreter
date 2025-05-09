use crate::{ast::traits::Expression, token::token::TokenType};

#[derive(Debug)]
pub struct CallExpression {
    token: TokenType,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl CallExpression {
    pub fn new(token: TokenType, function: Box<dyn Expression>) -> Self {
        Self {
            token,
            function,
            arguments: vec![],
        }
    }
}

impl Expression for CallExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn string_representation(&self) -> String {
        let mut buffer = String::new();

        let mut args: Vec<String> = vec![];

        for arg in &self.arguments {
            args.push(arg.string_representation());
        }

        buffer.push_str(&self.function.string_representation());
        buffer.push_str(&"(");
        buffer.push_str(&args.join(", "));
        buffer.push_str(&")");

        buffer
    }
}
