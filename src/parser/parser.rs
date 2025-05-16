use crate::ast::expressions::{
    BooleanExpression, CallExpression, ConditionalExpression, FunctionExpression,
    GroupedExpression, IdentifierExpression, InfixExpression, IntegerExpression, PrefixExpression,
};
use crate::ast::statements::{BlockStatement, ExpressionStatement, LetStatement, ReturnStatement};
use crate::ast::traits::{Expression, Node, Statement};
use crate::lexer::lexer::Lexer;
use crate::token::token::{PRECEDENCE, TokenType};
use anyhow::{Result, anyhow};
use std::mem;

#[allow(dead_code)]
pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: TokenType,
    pub next_token: TokenType,
    pub errors: Vec<anyhow::Error>,
}

impl std::fmt::Debug for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Cur token {}, Next Token, {}",
            self.cur_token.string_representation(),
            self.next_token.string_representation()
        )
    }
}

#[allow(dead_code)]
impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token: TokenType = lexer.next_token();
        let next_token: TokenType = lexer.next_token();

        // if cur_token == TokenType::EOF
        // || cur_token == TokenType::ILLEGAL
        // || next_token == TokenType::EOF
        // || next_token == TokenType::ILLEGAL
        // {
        // panic!("The lexer in the parser constructor does not have two tokens.")
        // }

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

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let cur_token = self.cur_token.clone();
        // println!("{:?}", self.next_token);

        if !self.peek_and_move(&TokenType::IDENT("x".to_string())) {
            return None;
        }

        let name = IdentifierExpression::new(self.cur_token.clone());

        if !self.peek_and_move(&TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(PRECEDENCE::LOWEST);

        if self.next_token == TokenType::SEMICOLON {
            self.next_token();
        }

        let let_statement = LetStatement::new(cur_token, name, value);

        Some(let_statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        if self.cur_token != TokenType::RETURN {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(PRECEDENCE::LOWEST);

        while !self.compare_cur_token(&TokenType::SEMICOLON) {
            if self.cur_token == TokenType::ILLEGAL || self.cur_token == TokenType::EOF {
                return None;
            }

            self.next_token();
        }

        // we need to pass the expression value into the return statment
        let return_statement = ReturnStatement::new(value);

        Some(return_statement)
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

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement::new(self.cur_token.clone());
        self.next_token();

        while !self.compare_cur_token(&TokenType::RBRACE)
            && !self.compare_cur_token(&TokenType::EOF)
        {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => block.statements.push(s),
                None => {}
            };

            self.next_token();
        }

        block
    }

    fn parse_if_expression(&mut self) -> Option<ConditionalExpression> {
        if !self.peek_and_move(&TokenType::LPAREN) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(PRECEDENCE::LOWEST);

        if !self.peek_and_move(&TokenType::RPAREN) {
            return None;
        }

        if !self.peek_and_move(&TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative: Option<BlockStatement> = if self.next_token == TokenType::ELSE {
            self.next_token();
            if !self.peek_and_move(&TokenType::LBRACE) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        match condition {
            Some(condition) => Some(ConditionalExpression::new(
                TokenType::IF,
                condition,
                consequence,
                alternative,
            )),
            None => {
                eprintln!("There is no condition in the IfConditional Struct");
                None
            }
        }
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<IdentifierExpression>> {
        let mut identifiers: Vec<IdentifierExpression> = vec![];

        if self.compare_next_token(&TokenType::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = IdentifierExpression::new(self.cur_token.clone());

        identifiers.push(ident);

        while self.compare_next_token(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = IdentifierExpression::new(self.cur_token.clone());
            identifiers.push(ident);
        }

        if !self.peek_and_move(&TokenType::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_function_expression(&mut self) -> Option<FunctionExpression> {
        // you're at fn
        let starting_token = self.cur_token.clone();

        if !self.peek_and_move(&TokenType::LPAREN) {
            return None;
        }

        let params = self.parse_function_parameters();

        if !self.peek_and_move(&TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement();

        let function_expression = FunctionExpression::new(starting_token, params?, body);

        Some(function_expression)
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut arguments = vec![];

        if self.compare_next_token(&TokenType::RPAREN) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        arguments.push(self.parse_expression(PRECEDENCE::LOWEST)?);

        while self.compare_next_token(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(PRECEDENCE::LOWEST)?);
        }

        if !self.peek_and_move(&TokenType::RPAREN) {
            return None;
        }

        Some(arguments)
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Option<Box<CallExpression>> {
        let mut call_expression = CallExpression::new(self.cur_token.clone(), function);

        if let Some(call_arguments) = self.parse_call_arguments() {
            call_expression.arguments = call_arguments;
        }
        Some(Box::new(call_expression))
    }

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

            TokenType::IF => {
                let if_expression = self.parse_if_expression();
                match if_expression {
                    Some(exp) => Some(Box::new(exp)),
                    None => None,
                }
            }

            TokenType::FUNCTION => {
                let function_expression = self.parse_function_expression();
                match function_expression {
                    Some(exp) => Some(Box::new(exp)),
                    None => None,
                }
            }
            _ => None,
        }
    }

    fn parse_expression(&mut self, precedence: PRECEDENCE) -> Option<Box<dyn Expression>> {
        let mut left = self.parse_prefix()?;

        let precedence_as_usize = precedence as i64;

        while !self.compare_next_token(&TokenType::SEMICOLON)
            && precedence_as_usize < self.get_peek_precedence() as i64
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
                TokenType::LPAREN => {
                    self.next_token();
                    self.parse_call_expression(left)?
                }
                _ => break,
            };
        }

        Some(left)
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let mut expression_statement = ExpressionStatement::new(self.cur_token.clone());

        expression_statement.expression = self.parse_expression(PRECEDENCE::LOWEST);

        if self.compare_next_token(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(expression_statement)
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            TokenType::LET => match self.parse_let_statement() {
                Some(let_statement) => return Some(Box::new(let_statement)),
                None => {
                    self.errors.push(anyhow!(
                        "There is something wrong with the let statement construction"
                    ));
                    None
                }
            },
            TokenType::RETURN => match self.parse_return_statement() {
                Some(return_statement) => return Some(Box::new(return_statement)),
                None => {
                    self.errors.push(anyhow!(
                        "There is something wrong with the return statement construction"
                    ));
                    None
                }
            },
            _ => match self.parse_expression_statement() {
                Some(expression_statement) => return Some(Box::new(expression_statement)),
                None => {
                    self.errors.push(anyhow!(
                        "There is something wrong in the construction of the expressionstatement"
                    ));

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
    fn test_let_statement_one() {
        let input = "let x = 5";

        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statement = statement
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expected an Let statemnt");

        assert_eq!(expr_statement.name.value, "x");

        if let Some(expression) = &expr_statement.value {
            let interget_expression = expression
                .as_any()
                .downcast_ref::<IntegerExpression>()
                .expect("Expected an integer expression");

            assert_eq!(interget_expression.value, 5);
        }
    }

    #[test]
    fn test_let_statement_two() {
        let input = "let y = true";

        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statement = statement
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expected an Let statemnt");

        assert_eq!(expr_statement.name.value, "y");

        if let Some(expression) = &expr_statement.value {
            let interget_expression = expression
                .as_any()
                .downcast_ref::<BooleanExpression>()
                .expect("Expected a boolean expression");

            assert_eq!(interget_expression.value, true);
        }
    }

    #[test]
    fn test_let_statement_three() {
        let input = "let x = foobar";

        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statement = statement
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expected an Let statemnt");

        assert_eq!(expr_statement.name.value, "x");

        if let Some(expression) = &expr_statement.value {
            let interget_expression = expression
                .as_any()
                .downcast_ref::<IdentifierExpression>()
                .expect("Expected an identifier expression");

            assert_eq!(interget_expression.value, "foobar");
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
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
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
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

    fn test_interget_literal(expr: &dyn Expression, value: i64) -> bool {
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

    impl TestLiteral for i64 {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, *self)
        }
    }
    impl TestLiteral for &i64 {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, **self)
        }
    }

    impl TestLiteral for i32 {
        fn test(&self, expr: &dyn Expression) -> bool {
            test_interget_literal(expr, *self as i64)
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
        let outputs: Vec<(&str, i64)> = vec![("!", 5), ("-", 15)];

        for (index, inpu) in input.iter().enumerate() {
            let lexer = Lexer::new(inpu.to_string());
            let parser = Parser::new(lexer);
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
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statement = statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression = expr_statement
            .expression
            .as_ref()
            .expect("Expression should exists");
        // testLiteralExpression
        let conditional_expression = expression
            .as_any()
            .downcast_ref::<ConditionalExpression>()
            .expect("Expected Conditional Expression");

        let infix_expression_condition = conditional_expression
            .condition
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Exprected an infix expression");

        let infix_tester = TestCaseInfix::new(&infix_expression_condition, &"x", &"<", &"y");

        assert!(infix_tester.test_infix_expression());
        //

        let consequence_statements = &conditional_expression.consequence.statements;
        assert_eq!(consequence_statements.len(), 1);

        let consequece_statement = &consequence_statements[0];
        let expr_statement_again = consequece_statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression_again = expr_statement_again
            .expression
            .as_ref()
            .expect("Expression should exists");

        let literal_expression = expression_again
            .as_any()
            .downcast_ref::<IdentifierExpression>()
            .expect("Expected Conditional Expression");

        assert!(test_identifier_literal(literal_expression, "x".to_string()));

        match &conditional_expression.alternative {
            Some(_) => {
                panic!("This should not be None")
            }
            None => {}
        }
    }

    #[test]
    fn test_if_else_condition() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();
        assert_eq!(p.statements.len(), 1);
        let statement = &p.statements[0];
        let expr_statement = statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression = expr_statement
            .expression
            .as_ref()
            .expect("Expression should exists");

        let conditional_expression = expression
            .as_any()
            .downcast_ref::<ConditionalExpression>()
            .expect("Expected Conditional Expression");

        let infix_expression_condition = conditional_expression
            .condition
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Exprected an infix expression");

        let infix_tester = TestCaseInfix::new(&infix_expression_condition, &"x", &"<", &"y");

        assert!(infix_tester.test_infix_expression());

        let consequence_statements = &conditional_expression.consequence.statements;
        assert_eq!(consequence_statements.len(), 1);

        let consequece_statement = &consequence_statements[0];
        let expr_statement_again = consequece_statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression_again = expr_statement_again
            .expression
            .as_ref()
            .expect("Expression should exists");

        let literal_expression = expression_again
            .as_any()
            .downcast_ref::<IdentifierExpression>()
            .expect("Expected Conditional Expression");

        assert!(test_identifier_literal(literal_expression, "x".to_string()));

        match &conditional_expression.alternative {
            Some(exp) => {
                let alternative_statements = &exp.statements;
                assert_eq!(alternative_statements.len(), 1);
                let alternative_statement = &exp.statements[0];
                let expr_statement_again = alternative_statement
                    .as_any()
                    .downcast_ref::<ExpressionStatement>()
                    .expect("Expected ExpressionStatement");

                let expression_again = expr_statement_again
                    .expression
                    .as_ref()
                    .expect("Expression should exists");

                let literal_expression = expression_again
                    .as_any()
                    .downcast_ref::<IdentifierExpression>()
                    .expect("Expected Conditional Expression");

                assert!(test_identifier_literal(literal_expression, "y".to_string()));
            }
            None => {
                panic!("32131223")
            }
        }
    }

    struct TestCaseInfix<'a> {
        input: &'a InfixExpression,
        left: &'a dyn TestLiteral,
        operator: &'a str,
        right: &'a dyn TestLiteral,
    }

    impl<'a> TestCaseInfix<'a> {
        pub fn new(
            input: &'a InfixExpression,
            left: &'a dyn TestLiteral,
            operator: &'a str,
            right: &'a dyn TestLiteral,
        ) -> Self {
            Self {
                input,
                left,
                operator,
                right,
            }
        }

        fn test_infix_expression(
            self,
            // left: &dyn TestLiteral,
            // operator: &str,
            // right: &dyn TestLiteral,
        ) -> bool {
            let op_exp = match self.input.as_any().downcast_ref::<InfixExpression>() {
                Some(exp) => exp,
                None => {
                    eprintln!("Exp is not InfixExpression got= {:?}", self.input);
                    return false;
                }
            };

            let left_ok = match &op_exp.left {
                Some(left_exp) => self.left.test(&**left_exp),
                None => {
                    return false;
                }
            };

            let right_ok = match &op_exp.right {
                Some(right_exp) => self.right.test(&**right_exp),
                None => {
                    return false;
                }
            };

            let operator_ok = op_exp.operator == self.operator;

            left_ok && right_ok && operator_ok
        }
    }

    #[test]
    fn test_infix_expressions() {
        let inputs: Vec<(&str, &dyn TestLiteral, &str, &dyn TestLiteral)> = vec![
            ("5 + 5", &5, "+", &5),
            ("5 - 5", &5, "-", &5),
            ("5 * 5", &5, "*", &5),
            ("5 / 5", &5, "/", &5),
            ("5 > 5", &5, ">", &5),
            ("5 == 5", &5, "==", &5),
            ("5 != 6", &5, "!=", &6),
            ("true == true", &true, "==", &true),
            ("false == false", &false, "==", &false),
            ("true != true", &true, "!=", &true),
        ];

        for input in inputs {
            let (input, left, op, right) = input;
            let lexer = Lexer::new(input.to_string());
            let parser = Parser::new(lexer);
            let mut p = Program::new(parser);
            p.parse_program();
            assert_eq!(p.statements.len(), 1);
            let statement = &p.statements[0];
            let expr_statement = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Expected ExpressionStatement");

            let expression = expr_statement
                .expression
                .as_ref()
                .expect("Expression should exists");

            let infix_expression_condition = expression
                .as_any()
                .downcast_ref::<InfixExpression>()
                .expect("Exprected an infix expression");

            let infix_tester = TestCaseInfix::new(&infix_expression_condition, left, op, right);

            assert!(infix_tester.test_infix_expression());
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
            let lexer = Lexer::new(self.input.to_string());
            let parser = Parser::new(lexer);
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
            TestOperatorPrecedence::new("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            TestOperatorPrecedence::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            TestOperatorPrecedence::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        for test in tests.iter() {
            assert!(test.run_test());
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
        let mut lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
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

    #[test]
    fn test_function_parameter_parsing() {
        let inputs = vec!["fn() {};", "fn(x) {};", "fn(x, y, z) {};"];
        let outputs = vec![vec![], vec!["x"], vec!["x", "y", "z"]];

        for (input, output) in inputs.iter().zip(outputs) {
            let lexer = Lexer::new(input.to_string());
            let parser = Parser::new(lexer);
            let mut p = Program::new(parser);
            p.parse_program();
            let statement = &p.statements[0];

            let expr_statmet = statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Expected ExpressionStatement");

            let expression = expr_statmet
                .expression
                .as_ref()
                .expect("Expression should exists");

            let function_expression = expression
                .as_any()
                .downcast_ref::<FunctionExpression>()
                .expect("Expected Function Expression");

            assert_eq!(function_expression.parameters.len(), output.len());

            for (index, ident) in output.iter().enumerate() {
                let ident_expression = function_expression.parameters[index]
                    .as_any()
                    .downcast_ref::<IdentifierExpression>()
                    .expect("Expected an Identifier Expression");

                assert!(test_identifier_literal(ident_expression, ident.to_string()));
            }
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x,y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
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

        let function_statmet = expression
            .as_any()
            .downcast_ref::<FunctionExpression>()
            .expect("Expected FunctionExpression");

        assert_eq!(function_statmet.parameters.len(), 2);

        assert!(test_identifier_literal(
            &function_statmet.parameters[0],
            "x".to_string()
        ));
        assert!(test_identifier_literal(
            &function_statmet.parameters[1],
            "y".to_string()
        ));

        assert_eq!(function_statmet.body.statements.len(), 1);

        let expr_statmet = function_statmet.body.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let expression = expr_statmet
            .expression
            .as_ref()
            .expect("Expression should exists");

        let infix_expression = expression
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Expected InfixExpression");

        let infix_test = TestCaseInfix::new(&infix_expression, &"x", &"+", &"y");
        assert!(infix_test.test_infix_expression());
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
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

        let call_expression = expression
            .as_any()
            .downcast_ref::<CallExpression>()
            .expect("Expression should be a CallExpression");

        let identifier_expression = call_expression
            .function
            .as_any()
            .downcast_ref::<IdentifierExpression>()
            .expect("FAILED!!!");

        assert!(test_identifier_literal(
            identifier_expression,
            "add".to_string()
        ));

        assert_eq!(call_expression.arguments.len(), 3);

        let first_expression = &call_expression.arguments[0];
        let second_expression = call_expression.arguments[1]
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Exprected an InfixExpression");
        let third_expression = call_expression.arguments[2]
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Exprected an InfixExpression");
        let second_test = TestCaseInfix::new(second_expression, &2, &"*", &3);
        let third_test = TestCaseInfix::new(third_expression, &4, &"+", &5);
        test_literal_expression(&**first_expression, 1);
        assert!(second_test.test_infix_expression());
        assert!(third_test.test_infix_expression());
    }
}
