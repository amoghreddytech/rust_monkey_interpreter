use crate::ast::expressions::{
    IdentifierExpression, InfixExpression, IntegerExpression, PrefixExpression,
};
use crate::ast::statements::{ExpressionStatement, LetStatement, ReturnStatement};
use crate::ast::traits::{Expression, Statement};
use crate::lexer::lexer::Lexer;
use crate::token::token::{PRECEDENCE, TokenType};
use anyhow::{Result, anyhow};
use std::mem;

#[allow(dead_code)]
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

#[allow(dead_code)]
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

    fn get_cur_precedence(&self) -> PRECEDENCE {
        self.cur_token.get_precedence()
    }

    fn get_peek_precedence(&self) -> PRECEDENCE {
        self.next_token.get_precedence()
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

    // this function is used for bang "!" and "-"
    // the naming is pretty weird here.
    // this is actual technical debt lol
    fn parse_prefix_expression_bang_and_minus(&mut self) -> PrefixExpression {
        let mut expression = PrefixExpression::new(self.cur_token.clone());

        self.next_token();

        expression.right = self.parse_expression(PRECEDENCE::PREFIX);

        expression
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let mut expression = InfixExpression::new(self.cur_token.clone(), left);

        let cur_precedence: PRECEDENCE = self.get_cur_precedence();

        self.next_token();

        expression.right = self.parse_expression(cur_precedence);

        Some(Box::new(expression))
    }

    fn parse_prefix(&mut self) -> Option<Box<dyn Expression>> {
        match self.cur_token.clone() {
            TokenType::IDENT(_) => {
                let ident_expression = IdentifierExpression::new(self.cur_token.clone());
                Some(Box::new(ident_expression))
            }

            TokenType::INT(_) => {
                let interget_expression = IntegerExpression::new(self.cur_token.clone());
                Some(Box::new(interget_expression))
            }

            TokenType::BANG | TokenType::MINUS => {
                let prefix_expression = self.parse_prefix_expression_bang_and_minus();
                Some(Box::new(prefix_expression))
            }
            _ => None,
        }
    }

    fn parse_expression(&mut self, precedence: PRECEDENCE) -> Option<Box<dyn Expression>> {
        let mut left = self.parse_prefix()?;

        let precedence_as_usize = precedence as usize;

        while !self.compare_next_token(&TokenType::SEMICOLON)
            && precedence_as_usize < self.get_peek_precedence() as usize
        {
            left = match self.next_token {
                TokenType::PLUS
                | TokenType::MINUS
                | TokenType::SLASH
                | TokenType::ASTERISK
                | TokenType::EQ
                | TokenType::NOTEQ
                | TokenType::LT
                | TokenType::GT => {
                    self.next_token();
                    self.parse_infix_expression(left)?
                }
                _ => break,
            };
        }

        Some(left)
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
        let statement = &p.statements[0];
        let expr_statment = statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected an expression statemnt");

        let expression = expr_statment
            .expression
            .as_ref()
            .expect("Expression needs to exists in an expression statement");

        let identifier_expression = expression
            .as_any()
            .downcast_ref::<IdentifierExpression>()
            .expect("This needs to be an identifier expression");

        assert_eq!(identifier_expression.string_representation(), "foobar");
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut lexer = Lexer::new(input);
        let parser = Parser::new(&mut lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statmet = statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression = expr_statmet
            .expression
            .as_ref()
            .expect("Expression should exists");

        let integer_expression = expression
            .as_any()
            .downcast_ref::<IntegerExpression>()
            .expect("Expected Prefix Expression");

        assert_eq!(integer_expression.value, 5);
        assert_eq!(integer_expression.string_representation(), "5");
    }

    fn test_interget_literal(expr: &dyn Expression, value: usize) -> bool {
        expr.as_any()
            .downcast_ref::<IntegerExpression>()
            .map_or(false, |int_expr| int_expr.value == value)
    }

    #[test]
    fn test_prefix_bang_operand() {
        let input: Vec<String> = vec!["!5;".to_string(), "-15;".to_string()];
        let outputs: Vec<(&str, usize)> = vec![("!", 5), ("-", 15)];

        for (index, inpu) in input.iter().enumerate() {
            let mut lexer = Lexer::new(inpu);
            let parser = Parser::new(&mut lexer);
            let mut p = Program::new(parser);
            p.parse_program();
            assert_eq!(p.statements.len(), 1);
            let statement = &p.statements[0];
            let expr_statmet = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Expected ExpressionStatement");

            let expression = expr_statmet
                .expression
                .as_ref()
                .expect("Expression should exists");

            let prefix_expression = expression
                .as_any()
                .downcast_ref::<PrefixExpression>()
                .expect("Expected Prefix Expression");

            let right_expr = prefix_expression
                .right
                .as_ref()
                .expect("Right operand should exists");
            let (output_operator, output_value) = outputs[index];
            assert_eq!(prefix_expression.operator, output_operator);
            assert!(test_interget_literal(right_expr.as_ref(), output_value));
        }
    }

    #[test]
    fn test_infix_expressions() {
        let input: Vec<(String, usize, String, usize)> = vec![
            ("5 + 5".to_string(), 5, "+".to_string(), 5),
            ("5 - 5".to_string(), 5, "-".to_string(), 5),
            ("5 * 5".to_string(), 5, "*".to_string(), 5),
            ("5 / 5".to_string(), 5, "/".to_string(), 5),
            ("5 > 5".to_string(), 5, ">".to_string(), 5),
            ("5 < 5".to_string(), 5, "<".to_string(), 5),
            ("5 == 5".to_string(), 5, "==".to_string(), 5),
            ("5 != 6".to_string(), 5, "!=".to_string(), 6),
        ];

        for (index, values) in input.iter().enumerate() {
            let (inpu, left_value, operator, right_value) = values;
            let mut lexer = Lexer::new(inpu);
            let parser = Parser::new(&mut lexer);
            let mut p = Program::new(parser);
            p.parse_program();
            assert_eq!(p.statements.len(), 1);

            let statement = &p.statements[0];

            let expr_statement = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Expected an expression statemnt");

            let expression = expr_statement
                .expression
                .as_ref()
                .expect("Expression should exists");

            let infix_expression = expression
                .as_any()
                .downcast_ref::<InfixExpression>()
                .expect("Expected Infix Expression");

            let left_expression = infix_expression
                .left
                .as_ref()
                .expect("left expression should exists");

            let right_expression = infix_expression
                .right
                .as_ref()
                .expect("right expression should exist");

            assert!(test_interget_literal(left_expression.as_ref(), *left_value));
            assert!(test_interget_literal(
                right_expression.as_ref(),
                *right_value
            ));
        }
    }
}
