use crate::ast::expressions::{IdentifierExpression, IntegerExpression};
use crate::ast::statements::{ExpressionStatement, LetStatement, ReturnStatement};
use crate::ast::traits::{Expression, Statement};
use crate::lexer::lexer::Lexer;
use crate::token::token::TokenType;
use anyhow::{Result, anyhow};
use std::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PRECEDENCE {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer<'a>,
    pub cur_token: TokenType,
    pub next_token: TokenType,
    pub errors: Vec<anyhow::Error>,
}

impl std::fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cur token {}, Next Token, {}",
            self.cur_token.string_representation(),
            self.next_token.string_representation()
        )
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let cur_token: TokenType = lexer.next_token();
        let next_token: TokenType = lexer.next_token();

        if cur_token == TokenType::EOF
            || cur_token == TokenType::ILLEGAL
            || next_token == TokenType::EOF
            || next_token == TokenType::ILLEGAL
        {
            panic!("The lexer in the parser constructor does not have two tokens.")
        }

        Self {
            lexer,
            cur_token,
            next_token,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.lexer.next_token()
    }

    fn compare_cur_token(&self, tok: &TokenType) -> bool {
        mem::discriminant(tok) == mem::discriminant(&self.cur_token)
    }

    fn compare_next_token(&self, tok: &TokenType) -> bool {
        mem::discriminant(tok) == mem::discriminant(&self.next_token)
    }

    fn peek_and_move(&mut self, tok: &TokenType) -> bool {
        if self.compare_next_token(tok) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        if self.cur_token != TokenType::LET {
            return Err(anyhow!(
                "The token needs to be a let token but its a/an {:?} token",
                self.cur_token
            ));
        }

        if !self.peek_and_move(&TokenType::IDENT("".to_string())) {
            return Err(anyhow!(
                "The token needs to be an IDENT token but its a/an {:?} token",
                self.next_token
            ));
        }

        let mut name = String::new();

        if let TokenType::IDENT(ident_name) = &self.cur_token {
            name = ident_name.clone();
        }

        if !self.peek_and_move(&TokenType::ASSIGN) {
            return Err(anyhow!(
                "the token needs to an ASSIGN token but its a/an {:?} token",
                self.next_token
            ));
        }

        // We'll skip the expression until we encounter a semicolon
        // we have to write this bit later
        while !self.compare_cur_token(&TokenType::SEMICOLON) {
            self.next_token();
        }

        let let_statement = LetStatement::new(name, None);

        Ok(let_statement)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        if self.cur_token != TokenType::RETURN {
            return Err(anyhow!(
                "
                    The token need to a return token but it's a/an {:?} token
                ",
                self.cur_token
            ));
        }

        self.next_token();

        // TODO : We're skipping expressions until we encounter a semicolon;

        while !self.compare_cur_token(&TokenType::SEMICOLON) {
            if self.cur_token == TokenType::ILLEGAL || self.cur_token == TokenType::EOF {
                return Err(anyhow!("A return statemnt needs to end with a semicolon"));
            }

            self.next_token();
        }

        // we need to pass the expression value into the return statment
        let return_statement = ReturnStatement::new(None);

        Ok(return_statement)
    }

    fn parse_expression(&mut self, precedece: PRECEDENCE) -> Option<Box<dyn Expression>> {
        match self.cur_token.clone() {
            TokenType::IDENT(_) => {
                let ident_expression = IdentifierExpression::new(self.cur_token.clone());
                return Some(Box::new(ident_expression));
            }

            TokenType::INT(_) => {
                let interget_expression = IntegerExpression::new(self.cur_token.clone());
                return Some(Box::new(interget_expression));
            }
            _ => None,
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let mut expression_statement = ExpressionStatement::new(self.cur_token.clone());

        expression_statement.expression = self.parse_expression(PRECEDENCE::LOWEST);

        if self.compare_next_token(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(expression_statement)
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            TokenType::LET => match self.parse_let_statement() {
                Ok(let_statement) => return Some(Box::new(let_statement)),
                Err(e) => {
                    self.errors.push(e);
                    None
                }
            },
            TokenType::RETURN => match self.parse_return_statement() {
                Ok(return_statement) => return Some(Box::new(return_statement)),
                Err(e) => {
                    self.errors.push(e);
                    None
                }
            },
            _ => match self.parse_expression_statement() {
                Ok(expression_statement) => return Some(Box::new(expression_statement)),
                Err(e) => {
                    self.errors.push(e);
                    None
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::ast::Program;

    use super::*;

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut lexer = Lexer::new(input);
        let parser = Parser::new(&mut lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
        let statement = p.statements.get(0).unwrap();
        assert!(
            statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .is_some()
        );

        if let Some(expr_statement) = statement.as_any().downcast_ref::<ExpressionStatement>() {
            if let Some(inner_expression) = &expr_statement.expression {
                if let Some(inner_expr) = inner_expression
                    .as_any()
                    .downcast_ref::<IdentifierExpression>()
                {
                    assert_eq!(inner_expr.string_representation(), "foobar".to_string());
                }
            }
        } else {
            panic!("Omg");
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut lexer = Lexer::new(input);
        let parser = Parser::new(&mut lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
        let statement = p.statements.get(0).unwrap();
        assert!(
            statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .is_some()
        );

        if let Some(expr_statement) = statement.as_any().downcast_ref::<ExpressionStatement>() {
            if let Some(inner_expression) = &expr_statement.expression {
                if let Some(inner_expr) = inner_expression
                    .as_any()
                    .downcast_ref::<IntegerExpression>()
                {
                    assert_eq!(inner_expr.value, 5);
                    assert_eq!(inner_expr.string_representation(), "5".to_string());
                }
            } else {
                panic!("noooo");
            }
        }
    }

    #[test]
    fn test_prefix_bang_operand() {
        let input = "!5;";
        let mut lexer = Lexer::new(input);
        let parser = Parser::new(&mut lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
    }
}
