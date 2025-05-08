use crate::ast::expressions::{
    BooleanExpression, GroupedExpression, IdentifierExpression, InfixExpression, IntegerExpression,
    PrefixExpression,
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

    fn parse_grouped_expression(&mut self) -> Option<GroupedExpression> {
        let mut grouped_expression = GroupedExpression::new(self.cur_token.clone());

        self.next_token();

        grouped_expression.value = self.parse_expression(PRECEDENCE::LOWEST);

        if !self.peek_and_move(&TokenType::RPAREN) {
            return None;
        }

        return Some(grouped_expression);
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
                let interger_expression = IntegerExpression::new(self.cur_token.clone());
                Some(Box::new(interger_expression))
            }

            TokenType::BANG | TokenType::MINUS => {
                let prefix_expression = self.parse_prefix_expression_bang_and_minus();
                Some(Box::new(prefix_expression))
            }

            TokenType::TRUE | TokenType::FALSE => {
                let boolean_expression = BooleanExpression::new(self.cur_token.clone());
                Some(Box::new(boolean_expression))
            }

            TokenType::LPAREN => {
                let grouped_expression = self.parse_grouped_expression();
                match grouped_expression {
                    Some(exp) => Some(Box::new(exp)),
                    None => None,
                }
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

    use crate::ast::{ast::Program, expressions::boolean::BooleanExpression};

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

    fn test_identifier_literal(expr: &dyn Expression, value: String) -> bool {
        expr.as_any()
            .downcast_ref::<IdentifierExpression>()
            .map_or(false, |ident_expr| ident_expr.value == value)
    }

    fn test_boolean_literal(expr: &dyn Expression, value: bool) -> bool {
        expr.as_any()
            .downcast_ref::<BooleanExpression>()
            .map_or(false, |bool_expr| bool_expr.value == value)
    }

    trait TestLiteral {
        fn test(&self, expr: &dyn Expression) -> bool;
    }

    impl TestLiteral for usize {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, *self)
        }
    }
    impl TestLiteral for &usize {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, **self)
        }
    }

    impl TestLiteral for i32 {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, *self as usize)
        }
    }

    impl TestLiteral for String {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_identifier_literal(expr, self.clone())
        }
    }

    impl TestLiteral for &str {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_identifier_literal(expr, self.to_string())
        }
    }

    impl TestLiteral for bool {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_boolean_literal(expr, *self)
        }
    }
    impl TestLiteral for &bool {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_boolean_literal(expr, **self)
        }
    }

    fn test_literal_expression<T: TestLiteral>(expr: &dyn Expression, expected: T) -> bool {
        expected.test(expr)
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
    fn test_infix_expression(
        expr: &dyn Expression,
        left: &dyn TestLiteral,
        operator: &str,
        right: &dyn TestLiteral,
    ) -> bool {
        let op_exp = match expr.as_any().downcast_ref::<InfixExpression>() {
            Some(exp) => exp,
            None => {
                eprintln!("Exp is not InfixExpression got= {:?}", expr.type_id());
                return false;
            }
        };

        let left_ok = match &op_exp.left {
            Some(left_exp) => left.test(&**left_exp),
            None => {
                return false;
            }
        };

        let right_ok = match &op_exp.right {
            Some(right_exp) => right.test(&**right_exp),
            None => {
                return false;
            }
        };

        let operator_ok = op_exp.operator == operator;

        left_ok && right_ok && operator_ok
    }

    struct TestCaseInfix {
        input: String,
        left: Box<dyn TestLiteral>,
        operator: String,
        right: Box<dyn TestLiteral>,
    }

    impl TestCaseInfix {
        pub fn new<T, U>(input: &str, left: T, operator: &str, right: U) -> Self
        where
            U: TestLiteral + 'static,
            T: TestLiteral + 'static,
        {
            Self {
                input: input.to_string(),
                left: Box::new(left),
                operator: operator.to_string(),
                right: Box::new(right),
            }
        }

        pub fn run_test(&self, index: usize) {
            let mut lexer = Lexer::new(&self.input);
            let parser = Parser::new(&mut lexer);
            let mut program = Program::new(parser);
            program.parse_program();

            assert_eq!(
                program.statements.len(),
                1,
                "Test case {}: Expected 1 statement, got {}",
                index,
                program.statements.len()
            );

            let statement = &program.statements[0];
            let expr_statement = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("Test case {}: Expected ExpressionStatement", index));

            let expression = expr_statement
                .expression
                .as_ref()
                .unwrap_or_else(|| panic!("Test case {}: Expression should exist", index));

            let infix_expression = expression
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap_or_else(|| panic!("Test case {}: Expected InfixExpression", index));

            assert!(test_infix_expression(
                infix_expression,
                &*self.left,
                &self.operator,
                &*self.right
            ))
        }
    }

    #[test]
    fn test_infix_expressions() {
        let input: Vec<TestCaseInfix> = vec![
            TestCaseInfix::new("5 + 5", 5, "+", 5),
            TestCaseInfix::new("5 - 5", 5, "-", 5),
            TestCaseInfix::new("5 * 5", 5, "*", 5),
            TestCaseInfix::new("5 / 5", 5, "/", 5),
            TestCaseInfix::new("5 > 5", 5, ">", 5),
            TestCaseInfix::new("5 < 5", 5, "<", 5),
            TestCaseInfix::new("5 == 5", 5, "==", 5),
            TestCaseInfix::new("5 != 6", 5, "!=", 6),
            TestCaseInfix::new("true == true", true, "==", true),
            TestCaseInfix::new("false != true", false, "!=", true),
            TestCaseInfix::new("false == false", false, "==", false),
        ];

        for (index, value) in input.iter().enumerate() {
            value.run_test(index);
        }
    }

    struct TestOperatorPrecedence {
        input: String,
        output: String,
    }

    impl TestOperatorPrecedence {
        pub fn new(input: &str, output: &str) -> Self {
            Self {
                input: input.to_string(),
                output: output.to_string(),
            }
        }

        pub fn run_test(&self) -> bool {
            let mut lexer = Lexer::new(&self.input);
            let parser = Parser::new(&mut lexer);
            let mut p = Program::new(parser);
            p.parse_program();

            let actual = p.string();

            if actual == self.output {
                return true;
            } else {
                println!(
                    "input : {:?}, actual : {:?}, test_case: {:?}",
                    self.input, actual, self.output
                );
                return false;
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            TestOperatorPrecedence::new("-a * b", "((-a) * b)"),
            TestOperatorPrecedence::new("a + b + c", "((a + b) + c)"),
            TestOperatorPrecedence::new("a + b - c", "((a + b) - c)"),
            TestOperatorPrecedence::new("a * b * c", "((a * b) * c)"),
            TestOperatorPrecedence::new("a * b / c", "((a * b) / c)"),
            TestOperatorPrecedence::new("a + b / c", "(a + (b / c))"),
            TestOperatorPrecedence::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            TestOperatorPrecedence::new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            TestOperatorPrecedence::new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            TestOperatorPrecedence::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            TestOperatorPrecedence::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            TestOperatorPrecedence::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            TestOperatorPrecedence::new("true;", "true"),
            TestOperatorPrecedence::new("false;", "false"),
            TestOperatorPrecedence::new("3 > 5 == false", "((3 > 5) == false)"),
            TestOperatorPrecedence::new("3 < 5 == true", "((3 < 5) == true)"),
            TestOperatorPrecedence::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            TestOperatorPrecedence::new("(5 + 5) * 2", "((5 + 5) * 2)"),
            TestOperatorPrecedence::new("2 / (5 + 5)", "(2 / (5 + 5))"),
            TestOperatorPrecedence::new("-(5 + 5)", "(-(5 + 5))"),
            TestOperatorPrecedence::new("!(true == true)", "(!(true == true))"),
        ];
        for test in tests.iter() {
            assert!(test.run_test());
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
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

        let boolean_expression = expression
            .as_any()
            .downcast_ref::<BooleanExpression>()
            .expect("Expected boolean Expression");

        assert_eq!(boolean_expression.value, true);
        assert_eq!(boolean_expression.string_representation(), "true");
    }
}
