use super::{
    AbstractSyntaxTree, ArrayLiteral, BlockStatement, BooleanLiteral, CallLiteral, Expression,
    ExpressionStatement, FunctionLiteral, IdentifierLiteral, IfLiteral, IndexLiteral, InfixLiteral,
    IntegerLiteral, LetStatement, PrefixLiteral, ReturnStatement, Statement, StringLiteral,
};
use crate::{Lexer, PRECEDENCE, TokenType};
use anyhow::{Error, Result, anyhow};

#[allow(dead_code)]
#[derive(Debug)]
pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: TokenType,
    pub peek_token: TokenType,
    pub errors: Vec<Error>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, expected: TokenType) -> bool {
        match (&self.cur_token, expected) {
            (TokenType::IDENT(_), TokenType::IDENT(_)) => true,
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(&b),
        }
    }

    fn peek_error(&self, expected: &TokenType) -> Error {
        anyhow!(
            "exprected peek token to be {:?} got {:?} instead",
            expected,
            self.peek_token
        )
    }

    fn peek_token_is(&self, expected: TokenType) -> bool {
        let output = std::mem::discriminant(&self.peek_token) == std::mem::discriminant(&expected);
        output
    }

    fn is_peek_and_move(&mut self, expected: TokenType) -> Result<(), Error> {
        if self.peek_token_is(expected.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(self.peek_error(&expected))
        }
    }

    pub fn parse_program(&mut self) -> Result<AbstractSyntaxTree, Vec<Error>> {
        let mut tree = AbstractSyntaxTree::new();

        while self.cur_token != TokenType::EOF {
            match self.parse_statement() {
                Ok(stmt) => tree.statements.push(stmt),
                Err(e) => self.errors.push(e),
            }

            self.next_token();
        }

        Ok(tree)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        let statement: Statement = match &self.cur_token {
            TokenType::LET => self.parse_let_statement()?,
            TokenType::RETURN => self.parse_return_statement()?,
            _ => self.parse_expression_statement()?,
        };

        Ok(statement)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();

        self.is_peek_and_move(TokenType::IDENT("_".into()))?;

        let identifier: IdentifierLiteral = IdentifierLiteral::new(self.cur_token.clone())?;

        self.is_peek_and_move(TokenType::ASSIGN)?;

        self.next_token();

        let expression = self.parse_expression(PRECEDENCE::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let let_statment = LetStatement::new(token, identifier, expression);

        Ok(Statement::LetStatement(let_statment))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        let expression = self.parse_expression(PRECEDENCE::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let return_statement = ReturnStatement::new(token, expression);

        Ok(Statement::ReturnStatement(return_statement))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();

        let expression = Box::new(self.parse_expression(PRECEDENCE::LOWEST)?);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let expression_statement = ExpressionStatement::new(token, expression);

        Ok(Statement::ExpressionStatement(expression_statement))
    }

    fn parse_expression(&mut self, precedence: PRECEDENCE) -> Result<Expression, Error> {
        let mut left_exp = match &self.cur_token {
            TokenType::IDENT(_) => self.parse_identifier()?,
            TokenType::INT(_) => self.parse_integer_literal()?,
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean()?,
            TokenType::BANG | TokenType::MINUS => self.parse_prefix_expression()?,
            TokenType::LPAREN => self.parse_grouped_expressions()?,
            TokenType::IF => self.parse_if_expression()?,
            TokenType::FUNCTION => self.parse_function_expression()?,
            TokenType::LBACKET => self.parse_array_literal()?,

            TokenType::String(_) => self.parse_string_expression()?,

            _ => return Err(anyhow!("no prefix parse function for {:?}", self.cur_token)),
        };

        while !self.peek_token_is(TokenType::SEMICOLON)
            && precedence < self.peek_token.get_precedence()
        {
            if self.peek_token_is(TokenType::LPAREN) {
                self.next_token();
                left_exp = self.parse_call_expresiion(Box::new(left_exp))?;
                continue;
            }

            if self.peek_token_is(TokenType::LBACKET) {
                self.next_token();
                left_exp = self.parse_index_expression(Box::new(left_exp))?;
                continue;
            }

            if let Some(_) = self.peek_token.get_infix_token() {
                self.next_token();
                left_exp = self.parse_infix_expression(Box::new(left_exp))?;
            } else {
                break;
            }
        }

        Ok(left_exp)
    }

    fn parse_index_expression(&mut self, left: Box<Expression>) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        let index_expression = Box::new(self.parse_expression(PRECEDENCE::LOWEST)?);

        self.is_peek_and_move(TokenType::RBRAKCET);

        let index_literal = IndexLiteral::new(token, left, index_expression);

        Ok(Expression::IndexExpression(index_literal))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenType::RBRAKCET)?;

        let array_literal = ArrayLiteral::new(token, elements)?;

        return Ok(Expression::ArrayExpression(array_literal));
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Box<Expression>>, Error> {
        let mut list: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        let first_expression = Box::new(self.parse_expression(PRECEDENCE::LOWEST)?);
        list.push(first_expression);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let expression = Box::new(self.parse_expression(PRECEDENCE::LOWEST)?);
            list.push(expression)
        }

        self.is_peek_and_move(end);

        return Ok(list);
    }

    fn parse_grouped_expressions(&mut self) -> Result<Expression, Error> {
        self.next_token();

        let expression = self.parse_expression(PRECEDENCE::LOWEST)?;

        self.is_peek_and_move(TokenType::RPAREN)?;

        return Ok(expression);
    }

    fn parse_boolean(&self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let boolean_literal = BooleanLiteral::new(token)?;
        Ok(Expression::BooleanExpression(boolean_literal))
    }

    fn parse_identifier(&self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let identifier = IdentifierLiteral::new(token)?;

        Ok(Expression::IdentiferExpression(identifier))
    }

    fn parse_integer_literal(&self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let i_literal = IntegerLiteral::new(token)?;

        Ok(Expression::IntegerExpression(i_literal))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.is_peek_and_move(TokenType::LPAREN)?;

        self.next_token();

        let condition = self.parse_expression(PRECEDENCE::LOWEST)?;
        self.is_peek_and_move(TokenType::RPAREN)?;

        self.is_peek_and_move(TokenType::LBRACE)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            self.is_peek_and_move(TokenType::LBRACE)?;
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Ok(Expression::IfExpression(IfLiteral::new(
            token,
            Box::new(condition),
            Box::new(consequence),
            alternative,
        )))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();
        let mut statements: Vec<Statement> = Vec::new();

        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        let block_statement_struct = BlockStatement::new(token, Box::new(statements));

        Ok(Statement::BlockStatement(block_statement_struct))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.is_peek_and_move(TokenType::LPAREN)?;

        let parameters = self.parse_function_parameters()?;

        self.is_peek_and_move(TokenType::LBRACE)?;

        let body = self.parse_block_statement()?;

        let function_literal = FunctionLiteral::new(token, Box::new(body), parameters)?;

        Ok(Expression::FunctionExpression(function_literal))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierLiteral>, Error> {
        let mut parameters = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(parameters);
        }

        self.next_token();

        let first_identifier = IdentifierLiteral::new(self.cur_token.clone())?;
        parameters.push(first_identifier);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let identifier = IdentifierLiteral::new(self.cur_token.clone())?;
            parameters.push(identifier)
        }

        self.is_peek_and_move(TokenType::RPAREN)?;

        return Ok(parameters);
    }

    fn parse_call_expresiion(&mut self, function: Box<Expression>) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let arguments = self.parse_call_arguments()?;
        let call_literal = CallLiteral::new(token, function, arguments)?;
        Ok(Expression::CallExpression(call_literal))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Box<Expression>>, Error> {
        let mut arguments = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();
        let first_argument = self.parse_expression(PRECEDENCE::LOWEST)?;
        arguments.push(Box::new(first_argument));

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            let argument = self.parse_expression(PRECEDENCE::LOWEST)?;
            arguments.push(Box::new(argument));
        }

        self.is_peek_and_move(TokenType::RPAREN)?;

        return Ok(arguments);
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let operator = token.token_literal();

        self.next_token();

        let expression = Box::new(self.parse_expression(PRECEDENCE::PREFIX)?);

        let prefix_data = PrefixLiteral::new(token, operator, expression)?;

        Ok(Expression::PrefixExpression(prefix_data))
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let operator = token.token_literal();
        let precedence = token.get_precedence();

        self.next_token();

        let right = Box::new(self.parse_expression(precedence)?);

        let infix_literal = InfixLiteral::new(token, left, operator, right);

        Ok(Expression::InfixExpression(infix_literal))
    }

    fn parse_string_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let string_literal = StringLiteral::new(token)?;
        Ok(Expression::StringExpression(string_literal))
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        Lexer,
        parser::{Expression, Statement},
    };

    #[test]
    fn test_parsing_index_expression() {
        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse Program");
        check_parse_errors(&parser);

        assert!(
            &parser.errors.is_empty(),
            "Parser errors: {:?}",
            &parser.errors
        );

        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        match *stmt.expression {
            Expression::IndexExpression(ref il) => {
                match *il.left {
                    Expression::IdentiferExpression(ref s) => {
                        assert_eq!(s.value, "myArray".to_string());
                    }
                    _ => panic!("Not an identifier expression"),
                }

                match *il.index {
                    Expression::InfixExpression(ref ie) => {
                        assert_eq!(ie.operator, "+");
                        match *ie.left {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 1);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }

                        match *ie.right {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 1);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }
                    }
                    _ => panic!("not an index expression"),
                }
            }

            _ => panic!("Not an index expression"),
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("Failed to parse program");

        check_parse_errors(&parser);

        assert!(
            &parser.errors.is_empty(),
            "Parser errors: {:?}",
            &parser.errors
        );

        // Verify it's an expression statement
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        match *stmt.expression {
            Expression::ArrayExpression(ref al) => {
                assert_eq!(al.elements.len(), 3);

                match *al.elements[0] {
                    Expression::IntegerExpression(ref ie) => {
                        assert_eq!(ie.value, 1);
                    }
                    _ => panic!("First interger literal is wrong expected 1"),
                }

                match *al.elements[1] {
                    Expression::InfixExpression(ref ie) => {
                        assert_eq!("*", ie.operator);
                        match *ie.left {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 2);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }

                        match *ie.right {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 2);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }
                    }
                    _ => panic!("Expred infix expression but got something else"),
                }

                match *al.elements[2] {
                    Expression::InfixExpression(ref ie) => {
                        assert_eq!("+", ie.operator);
                        match *ie.left {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 3);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }

                        match *ie.right {
                            Expression::IntegerExpression(ref ie) => {
                                assert_eq!(ie.value, 3);
                            }
                            _ => {
                                panic!("left interger literal is wrong expected 2 for second index")
                            }
                        }
                    }
                    _ => panic!("Expred infix expression but got something else"),
                }
            }
            _ => panic!("not a string expression"),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        check_parse_errors(&parser);
        assert!(
            &parser.errors.is_empty(),
            "Parser errors: {:?}",
            &parser.errors
        );

        // Verify it's an expression statement
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        match *stmt.expression {
            Expression::StringExpression(ref s) => {
                assert_eq!(s.value, "hello world".to_string());
            }
            _ => panic!("not a string expression"),
        }
    }

    #[test]
    fn test_let_statement_parsing() {
        let input = "let x = 5; let y = 10; let foobar = 838383;";
        let expected_names = ["x", "y", "foobar"];

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        // // Verify parsing succeeded without errors
        check_parse_errors(&parser);

        assert!(
            &parser.errors.is_empty(),
            "Parser errors: {:?}",
            &parser.errors
        );

        // Verify statement count
        assert_eq!(
            program.statements.len(),
            expected_names.len(),
            "Incorrect number of statements parsed"
        );

        // Verify each statement
        for (stmt, &expected_name) in program.statements.iter().zip(expected_names.iter()) {
            verify_let_statement(stmt, expected_name);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");

        // Check parser errors
        assert!(
            parser.errors.is_empty(),
            "Parser had errors: {:?}",
            parser.errors
        );

        // Verify statement count
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        // Verify it's an expression statement
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        // Verify it's an if expression
        let if_expr = match &*stmt.expression {
            Expression::IfExpression(e) => e,
            _ => panic!(
                "stmt.expression is not IfExpression. got={:?}",
                stmt.expression
            ),
        };

        // Verify condition is x < y
        match &*if_expr.condition {
            Expression::InfixExpression(infix) => {
                assert_eq!(infix.operator, "<");
                // Verify left is 'x'
                match &*infix.left {
                    Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "x"),
                    _ => panic!("Left of condition is not identifier 'x'"),
                }
                // Verify right is 'y'
                match &*infix.right {
                    Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "y"),
                    _ => panic!("Right of condition is not identifier 'y'"),
                }
            }
            _ => panic!("Condition is not InfixExpression"),
        }

        // // Verify consequence has 1 statement
        match &*if_expr.consequence {
            Statement::BlockStatement(block) => {
                assert_eq!(
                    block.statements.len(),
                    1,
                    "consequence does not contain 1 statement. got={}",
                    block.statements.len()
                );

                // Verify consequence statement is 'x'
                match &block.statements[0] {
                    Statement::ExpressionStatement(expr_stmt) => match &*expr_stmt.expression {
                        Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "x"),
                        _ => panic!("Consequence expression is not identifier 'x'"),
                    },
                    _ => panic!("Consequence statement is not ExpressionStatement"),
                }
            }
            _ => panic!("Consequence is not BlockStatement"),
        }

        // Verify no alternative
        assert!(
            if_expr.alternative.is_none(),
            "if_expr.alternative was not None. got={:?}",
            if_expr.alternative
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");

        // Check parser errors
        assert!(
            parser.errors.is_empty(),
            "Parser had errors: {:?}",
            parser.errors
        );

        // Verify statement count
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        // Verify it's an expression statement
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        // Verify it's an if expression
        let if_expr = match &*stmt.expression {
            Expression::IfExpression(e) => e,
            _ => panic!(
                "stmt.expression is not IfExpression. got={:?}",
                stmt.expression
            ),
        };

        // Verify condition is x < y
        match &*if_expr.condition {
            Expression::InfixExpression(infix) => {
                assert_eq!(infix.operator, "<");
                // Verify left is 'x'
                match &*infix.left {
                    Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "x"),
                    _ => panic!("Left of condition is not identifier 'x'"),
                }
                // Verify right is 'y'
                match &*infix.right {
                    Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "y"),
                    _ => panic!("Right of condition is not identifier 'y'"),
                }
            }
            _ => panic!("Condition is not InfixExpression"),
        }

        // // Verify consequence has 1 statement
        match &*if_expr.consequence {
            Statement::BlockStatement(block) => {
                assert_eq!(
                    block.statements.len(),
                    1,
                    "consequence does not contain 1 statement. got={}",
                    block.statements.len()
                );

                // Verify consequence statement is 'x'
                match &block.statements[0] {
                    Statement::ExpressionStatement(expr_stmt) => match &*expr_stmt.expression {
                        Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "x"),
                        _ => panic!("Consequence expression is not identifier 'x'"),
                    },
                    _ => panic!("Consequence statement is not ExpressionStatement"),
                }
            }
            _ => panic!("Consequence is not BlockStatement"),
        }

        match if_expr.alternative.as_ref() {
            Some(alt_stmt) => match &**alt_stmt {
                Statement::BlockStatement(block) => {
                    assert_eq!(
                        block.statements.len(),
                        1,
                        "Alternative does not contain 1 statement, got = {}",
                        block.statements.len()
                    );

                    match &block.statements[0] {
                        Statement::ExpressionStatement(expr_stmt) => match &*expr_stmt.expression {
                            Expression::IdentiferExpression(ident) => {
                                assert_eq!(ident.value, "y");
                            }
                            _ => panic!("Alternative statement is not an identifier 'y'"),
                        },
                        _ => panic!("Alternative statement is not an ExpressionStatement"),
                    }
                }
                _ => panic!("Alternative is not BlockStatement"),
            },
            None => panic!("if_expr.alternative was None"),
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase {
            input: String,
            expected: String,
        }

        let tests = vec![
            TestCase {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            TestCase {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            TestCase {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            TestCase {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            TestCase {
                input: "a * b * c".to_string(),
                expected: "((a * b) * c)".to_string(),
            },
            TestCase {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            TestCase {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            TestCase {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            TestCase {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            TestCase {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            TestCase {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            TestCase {
                input: "true".to_string(),
                expected: "true".to_string(),
            },
            TestCase {
                input: "false".to_string(),
                expected: "false".to_string(),
            },
            TestCase {
                input: "3 > 5 == false".to_string(),
                expected: "((3 > 5) == false)".to_string(),
            },
            TestCase {
                input: "3 < 5 == true".to_string(),
                expected: "((3 < 5) == true)".to_string(),
            },
            TestCase {
                input: "1 + (2 + 3) + 4".to_string(),
                expected: "((1 + (2 + 3)) + 4)".to_string(),
            },
            TestCase {
                input: "(5 + 5) * 2".to_string(),
                expected: "((5 + 5) * 2)".to_string(),
            },
            TestCase {
                input: "2 / (5 + 5)".to_string(),
                expected: "(2 / (5 + 5))".to_string(),
            },
            TestCase {
                input: "-(5 + 5)".to_string(),
                expected: "(-(5 + 5))".to_string(),
            },
            TestCase {
                input: "!(true == true)".to_string(),
                expected: "(!(true == true))".to_string(),
            },
            TestCase {
                input: "a + add(b * c) + d".to_string(),
                expected: "((a + add((b * c))) + d)".to_string(),
            },
            TestCase {
                input: "a + add(b * c) + d".to_string(),
                expected: "((a + add((b * c))) + d)".to_string(),
            },
            TestCase {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            },
            TestCase {
                input: "add(a + b + c * d / f + g)".to_string(),
                expected: "add((((a + b) + ((c * d) / f)) + g))".to_string(),
            },
            TestCase {
                input: "a * [1, 2, 3, 4][b * c] * d".to_string(),
                expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)".to_string(),
            },
            TestCase {
                input: "add(a * b[2], b[1], 2 * [1, 2][1])".to_string(),
                expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))".to_string(),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("Failed to parse program");

            // Check for parser errors
            assert!(
                parser.errors.is_empty(),
                "Parser had errors: {:?}",
                parser.errors
            );

            // println!("{:?}", program.statements[0]);
            let actual = program.string_representation();
            assert_eq!(
                actual, test.expected,
                "Test failed for input: {}\nExpected: {}\nGot: {}",
                test.input, test.expected, actual
            );
        }
    }

    fn check_parse_errors(p: &Parser) {
        let errors = &p.errors;

        if errors.len() == 0 {
            return;
        }

        eprintln!("parser has {} errors", errors.len());

        for error in errors {
            eprintln!("{:?}", error)
        }

        panic!()
    }

    fn verify_let_statement(statement: &Statement, expected_name: &str) -> bool {
        match statement {
            Statement::LetStatement(let_statement) => {
                assert_eq!(let_statement.token.token_literal(), "let".to_string());
                assert_eq!(let_statement.identifier.value, expected_name.to_string());
                true
            }
            _ => false,
        }
    }

    fn verify_return_statement(statement: &Statement) -> bool {
        match statement {
            Statement::ReturnStatement(return_statement) => {
                assert_eq!(return_statement.token.token_literal(), "return".to_string());
                true
            }
            _ => false,
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5; return 10; return 838383;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        // // Verify parsing succeeded without errors
        check_parse_errors(&parser);

        assert!(
            &parser.errors.is_empty(),
            "Parser errors: {:?}",
            &parser.errors
        );

        // Verify statement count
        assert_eq!(
            program.statements.len(),
            3,
            "Incorrect number of statements parsed"
        );
        for stmt in program.statements {
            assert!(verify_return_statement(&stmt))
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        // // Verify parsing succeeded without errors
        check_parse_errors(&parser);
        assert_eq!(program.statements.len(), 1);
        let expression: &Statement = &program.statements[0];
        if !matches!(expression, Statement::ExpressionStatement(_)) {
            panic!("should be an expression statement");
        }
        if let Statement::ExpressionStatement(expression_statement) = expression {
            let expression = &expression_statement.expression;
            if !matches!(**expression, Expression::IdentiferExpression(_)) {
                panic!("should be an Identifier Expression");
            }

            if let Expression::IdentiferExpression(ref identifier_expression_data) = **expression {
                assert_eq!(identifier_expression_data.value, "foobar");
            }
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        // // Verify parsing succeeded without errors
        check_parse_errors(&parser);
        assert_eq!(program.statements.len(), 1);
        let expression: &Statement = &program.statements[0];
        if !matches!(expression, Statement::ExpressionStatement(_)) {
            panic!("should be an expression statement");
        }
        if let Statement::ExpressionStatement(expression_statement) = expression {
            let expression = &expression_statement.expression;
            if !matches!(**expression, Expression::IntegerExpression(_)) {
                panic!("should be an Integer Expression");
            }

            if let Expression::IntegerExpression(ref integer_expression_data) = **expression {
                assert_eq!(integer_expression_data.value, 5);
            }
        }
    }

    #[test]
    fn test_prefix_expressions() {
        #[derive(Debug, PartialEq)]
        enum TestValue {
            Int(i64),
            Bool(bool),
        }

        struct TestCase {
            input: &'static str,
            operator: &'static str,
            right_value: TestValue,
        }

        let test_cases = vec![
            TestCase {
                input: "!5;",
                operator: "!",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "-15;",
                operator: "-",
                right_value: TestValue::Int(15),
            },
            TestCase {
                input: "!true;",
                operator: "!",
                right_value: TestValue::Bool(true),
            },
            TestCase {
                input: "!false;",
                operator: "!",
                right_value: TestValue::Bool(false),
            },
        ];

        for test in test_cases {
            let lexer = Lexer::new(test.input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("Failed to parse program");

            // Verify no parsing errors
            assert!(
                parser.errors.is_empty(),
                "Parser had errors: {:?}",
                parser.errors
            );

            // Verify exactly one statement was parsed
            assert_eq!(program.statements.len(), 1, "Expected 1 statement");

            // Verify it's an expression statement
            let statement = &program.statements[0];
            let expression = match statement {
                Statement::ExpressionStatement(expr_stmt) => &expr_stmt.expression,
                _ => panic!("Expected ExpressionStatement, got {:?}", statement),
            };

            // Verify it's a prefix expression
            let prefix_expr = match &**expression {
                Expression::PrefixExpression(expr) => expr,
                _ => panic!("Expected PrefixExpression, got {:?}", expression),
            };

            // Verify operator matches
            assert_eq!(
                prefix_expr.operator, test.operator,
                "Expected operator '{}', got '{}'",
                test.operator, prefix_expr.operator
            );

            // Verify right value matches expected type and value
            match (&*prefix_expr.right, &test.right_value) {
                (Expression::IntegerExpression(value), TestValue::Int(expected)) => {
                    assert_eq!(
                        value.value, *expected,
                        "Expected integer value {}, got {}",
                        expected, value.value
                    );
                }
                (Expression::BooleanExpression(value), TestValue::Bool(expected)) => {
                    assert_eq!(
                        value.value, *expected,
                        "Expected boolean value {}, got {}",
                        expected, value.value
                    );
                }
                (expr, expected) => panic!(
                    "Type mismatch in prefix expression. Expected {:?}, got {:?}",
                    expected, expr
                ),
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        #[derive(Debug, PartialEq, Eq)]
        enum TestValue {
            Int(i64),
            Bool(bool),
        }

        #[derive(Debug, PartialEq, Eq)]
        struct TestCase {
            input: &'static str,
            left_value: TestValue,
            operator: &'static str,
            right_value: TestValue,
        }

        let tests = vec![
            TestCase {
                input: "5 + 5;",
                left_value: TestValue::Int(5),
                operator: "+",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 - 5;",
                left_value: TestValue::Int(5),
                operator: "-",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 * 5;",
                left_value: TestValue::Int(5),
                operator: "*",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 / 5;",
                left_value: TestValue::Int(5),
                operator: "/",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 > 5;",
                left_value: TestValue::Int(5),
                operator: ">",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 < 5;",
                left_value: TestValue::Int(5),
                operator: "<",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 == 5;",
                left_value: TestValue::Int(5),
                operator: "==",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "5 != 5;",
                left_value: TestValue::Int(5),
                operator: "!=",
                right_value: TestValue::Int(5),
            },
            TestCase {
                input: "true == true",
                left_value: TestValue::Bool(true),
                operator: "==",
                right_value: TestValue::Bool(true),
            },
            TestCase {
                input: "true != false",
                left_value: TestValue::Bool(true),
                operator: "!=",
                right_value: TestValue::Bool(false),
            },
            TestCase {
                input: "false == false",
                left_value: TestValue::Bool(false),
                operator: "==",
                right_value: TestValue::Bool(false),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("Failed to parse program");

            // Check for parser errors
            assert!(
                parser.errors.is_empty(),
                "Parser had errors: {:?}",
                parser.errors
            );

            // Verify exactly one statement
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement"
            );

            // Verify it's an expression statement
            let stmt = &program.statements[0];
            let expr_stmt = match stmt {
                Statement::ExpressionStatement(s) => s,
                _ => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    stmt
                ),
            };

            // // Verify it's an infix expression
            let infix_expr = match &*expr_stmt.expression {
                Expression::InfixExpression(e) => e,
                _ => panic!("exp is not InfixExpression. got={:?}", expr_stmt.expression),
            };

            // Verify left value

            match (&*infix_expr.left, &test.left_value) {
                (Expression::IntegerExpression(i), TestValue::Int(v)) => assert_eq!(i.value, *v),
                (Expression::BooleanExpression(b), TestValue::Bool(v)) => assert_eq!(b.value, *v),
                _ => panic!("Type mismatch in left value"),
            }

            // Verify operator
            assert_eq!(infix_expr.operator, test.operator);

            // Verify right value
            match (&*infix_expr.right, &test.right_value) {
                (Expression::IntegerExpression(i), TestValue::Int(v)) => assert_eq!(i.value, *v),
                (Expression::BooleanExpression(b), TestValue::Bool(v)) => assert_eq!(b.value, *v),
                _ => panic!("Type mismatch in right value"),
            }
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct TestCase {
            input: &'static str,
            expected_params: Vec<&'static str>,
        }

        let tests = vec![
            TestCase {
                input: "fn() {};",
                expected_params: vec![],
            },
            TestCase {
                input: "fn(x) {};",
                expected_params: vec!["x"],
            },
            TestCase {
                input: "fn(x, y, z) {};",
                expected_params: vec!["x", "y", "z"],
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("Failed to parse program");

            // Check parser errors
            assert!(
                parser.errors.is_empty(),
                "Parser had errors: {:?}",
                parser.errors
            );

            // Verify statement count
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );

            // Verify it's an expression statement
            let stmt = match &program.statements[0] {
                Statement::ExpressionStatement(s) => s,
                _ => panic!(
                    "program.statements[0] is not ExpressionStatement. got={:?}",
                    program.statements[0]
                ),
            };

            // Verify it's a function expression
            let function = match &*stmt.expression {
                Expression::FunctionExpression(f) => f,
                _ => panic!(
                    "stmt.expression is not FunctionExpression. got={:?}",
                    stmt.expression
                ),
            };

            // Verify parameter count
            assert_eq!(
                function.parameters.len(),
                test.expected_params.len(),
                "length parameters wrong. want {}, got={}",
                test.expected_params.len(),
                function.parameters.len()
            );

            // Verify each parameter
            for (i, &expected_param) in test.expected_params.iter().enumerate() {
                assert_eq!(
                    function.parameters[i].value, expected_param,
                    "parameter {} wrong. want {}, got={}",
                    i, expected_param, function.parameters[i].value
                );
            }
        }
    }

    #[test]
    fn test_function_expression() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");

        // Check parser errors
        assert!(
            parser.errors.is_empty(),
            "Parser had errors: {:?}",
            parser.errors
        );

        // Ensure only one statement exists
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::ExpressionStatement(s) => s,
            _ => panic!(
                "program.statements[0] is not ExpressionStatement. got={:?}",
                stmt
            ),
        };

        let function = match &*expr_stmt.expression {
            Expression::FunctionExpression(f) => f,
            _ => panic!(
                "stmt.Expression is not FunctionLiteral. got={:?}",
                expr_stmt.expression
            ),
        };

        // Check parameters
        assert_eq!(
            function.parameters.len(),
            2,
            "function literal parameters wrong. want=2, got={}",
            function.parameters.len()
        );

        assert_eq!(function.parameters[0].value, "x");
        assert_eq!(function.parameters[1].value, "y");

        match &*function.body {
            Statement::BlockStatement(block) => {
                assert_eq!(
                    block.statements.len(),
                    1,
                    "function body has not 1 statement. got={}",
                    block.statements.len()
                );

                // Verify body statement is an expression statement
                let body_stmt = match &block.statements[0] {
                    Statement::ExpressionStatement(s) => s,
                    _ => panic!(
                        "function body statement is not ExpressionStatement. got={:?}",
                        block.statements[0]
                    ),
                };

                // Verify the expression is an infix expression x + y
                match &*body_stmt.expression {
                    Expression::InfixExpression(infix) => {
                        assert_eq!(infix.operator, "+");
                        // Verify left is 'x'
                        match &*infix.left {
                            Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "x"),
                            _ => panic!("Left of infix is not identifier 'x'"),
                        }
                        // Verify right is 'y'
                        match &*infix.right {
                            Expression::IdentiferExpression(ident) => assert_eq!(ident.value, "y"),
                            _ => panic!("Right of infix is not identifier 'y'"),
                        }
                    }
                    _ => panic!("Body expression is not InfixExpression"),
                }
            }
            _ => panic!("Function body is not BlockStatement"),
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        // Check parser errors
        assert!(
            parser.errors.is_empty(),
            "Parser had errors: {:?}",
            parser.errors
        );

        // Check statement count
        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        // Check statement type
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => stmt,
            _ => panic!(
                "stmt is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        };

        // Check expression type
        let exp = match &*stmt.expression {
            Expression::CallExpression(exp) => exp,
            _ => panic!(
                "stmt.Expression is not CallExpression. got={:?}",
                stmt.expression
            ),
        };

        // Check function identifier
        match &*exp.function {
            Expression::IdentiferExpression(ident) => assert_eq!(
                ident.value, "add",
                "expected function name 'add', got '{}'",
                ident.value
            ),
            _ => panic!("function is not Identifier"),
        }

        // Check argument count
        assert_eq!(
            exp.arguments.len(),
            3,
            "wrong length of arguments. got={}",
            exp.arguments.len()
        );

        // Test first argument (1)
        match &*exp.arguments[0] {
            Expression::IntegerExpression(int) => assert_eq!(int.value, 1),
            _ => panic!("first argument is not IntegerLiteral"),
        }

        // Test second argument (2 * 3)
        match &*exp.arguments[1] {
            Expression::InfixExpression(infix) => {
                match &*infix.left {
                    Expression::IntegerExpression(int) => assert_eq!(int.value, 2),
                    _ => panic!("left operand is not 2"),
                }
                assert_eq!(infix.operator, "*");
                match &*infix.right {
                    Expression::IntegerExpression(int) => assert_eq!(int.value, 3),
                    _ => panic!("right operand is not 3"),
                }
            }
            _ => panic!("second argument is not InfixExpression"),
        }

        // // Test third argument (4 + 5)
        match &*exp.arguments[2] {
            Expression::InfixExpression(infix) => {
                match &*infix.left {
                    Expression::IntegerExpression(int) => assert_eq!(int.value, 4),
                    _ => panic!("left operand is not 4"),
                }
                assert_eq!(infix.operator, "+");
                match &*infix.right {
                    Expression::IntegerExpression(int) => assert_eq!(int.value, 5),
                    _ => panic!("right operand is not 5"),
                }
            }
            _ => panic!("third argument is not InfixExpression"),
        }
    }
}
