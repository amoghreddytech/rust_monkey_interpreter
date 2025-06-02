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

    pub fn string_representation(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.string_literal(),
            Statement::ReturnStatement(return_statement) => return_statement.string_literal(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.string_literal()
            }
        }
    }
}
