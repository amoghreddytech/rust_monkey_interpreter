use anyhow::{Error, anyhow};

use crate::{TokenType, parser::Expression};

#[derive(Debug, Clone)]
pub struct CallLiteral {
    pub token: TokenType,
    pub function: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}

impl CallLiteral {
    pub fn new(
        token: TokenType,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    ) -> Result<Self, Error> {
        match &*function {
            Expression::FunctionExpression(fe) => Ok(Self {
                token,
                function,
                arguments,
            }),
            _ => Err(anyhow!(
                "Did not get a functionliteral in the function defifition."
            )),
        }
    }
}
