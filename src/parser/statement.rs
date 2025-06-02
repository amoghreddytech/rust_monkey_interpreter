use super::{BlockStatement, ExpressionStatement, LetStatement, ReturnStatement};

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(ls) => ls.token.token_literal(),
            Self::ReturnStatement(rs) => rs.token.token_literal(),
            Self::ExpressionStatement(es) => es.token.token_literal(),
            Self::BlockStatement(bs) => bs.token.token_literal(),
        }
    }

    pub fn string_representation(&self) -> String {
        match self {
            Self::LetStatement(let_statement) => let_statement.string_literal(),
            Self::ReturnStatement(return_statement) => return_statement.string_literal(),
            Self::ExpressionStatement(expression_statement) => {
                expression_statement.string_literal()
            }
            Self::BlockStatement(block_statement) => block_statement.string_literal(),
        }
    }
}
