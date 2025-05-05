use crate::ast::traits::Statement;
use crate::{parser::parser::Parser, token::token::TokenType};
use anyhow::{Result, anyhow};

// Program structure
#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Box<dyn Statement>>,
    parser: Parser<'a>,
    pub errors_from_parser: Vec<anyhow::Error>,
}

impl<'a> Program<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            statements: Vec::new(),
            parser,
            errors_from_parser: vec![],
        }
    }

    // This should only provide shared ownership and hence it should be chill
    fn get_root_statment(&self) -> Result<&dyn Statement> {
        self.statements
            .first()
            .map(|stmt| &**stmt)
            .ok_or_else(|| anyhow!("No statements in the Program"))
    }

    pub fn parse_program(&mut self) {
        while self.parser.cur_token != TokenType::EOF {
            let stmt = self.parser.parse_statement();
            match stmt {
                Some(s) => {
                    self.statements.push(s);
                }
                None => {}
            }
            self.parser.next_token()
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::lexer::lexer::Lexer;

    use super::*;
    #[test]
    fn test_let_statments() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";

        let mut lexer = Lexer::new(input);

        let parser = Parser::new(&mut lexer);

        let mut program = Program::new(parser);

        assert_eq!(program.statements.len(), 0);

        program.parse_program();

        assert_eq!(program.statements.len(), 3);
        let mut output_vec = vec![];
        for stmt in program.statements {
            output_vec.push(stmt.string_representation());
        }

        let test_outputs_left_side = vec!["x".to_string(), "y".to_string(), "foobar".to_string()];

        assert_eq!(test_outputs_left_side, output_vec);
    }
    #[test]
    fn test_error_let_statments() {
        let input = "let x 5;
let  = 10;
let  838383;";

        let mut lexer = Lexer::new(input);

        let parser = Parser::new(&mut lexer);

        let mut program = Program::new(parser);

        assert_eq!(program.parser.errors.len(), 0);

        program.parse_program();

        assert_eq!(program.parser.errors.len(), 3);
    }

    #[test]
    fn test_return_statemtnts() {
        let input = "return 5;
    return 10;
    return 993322;";

        let mut lexer = Lexer::new(input);

        let parser = Parser::new(&mut lexer);

        let mut program = Program::new(parser);

        assert_eq!(program.statements.len(), 0);

        program.parse_program();

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            assert_eq!(stmt.string_representation(), "return".to_string());
        }
    }
}
