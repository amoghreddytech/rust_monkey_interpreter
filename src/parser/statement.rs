use super::{ExpressionStatement, LetStatement, ReturnStatement};

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(ls) => ls.token.token_literal(),
            Self::ReturnStatement(rs) => rs.token.token_literal(),
            Self::ExpressionStatement(es) => es.token.token_literal(),
        }
    }

    pub fn string_representation(&self) -> &str {
        match self {
            Statement::LetStatement(let_statement) => todo!(),
            Statement::ReturnStatement(return_statement) => todo!(),
            Statement::ExpressionStatement(expression_statement) => todo!(),
        }
    }
}
