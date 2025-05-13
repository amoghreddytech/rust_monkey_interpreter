use crate::ast::ast::Program;
use crate::parser::parser::Parser;
use crate::token::token::TokenType;
use crate::{lexer::lexer::Lexer, parser};
use std::io::{BufRead, Write};
use std::os::unix::process;

const PROMT: &str = ">> ";

pub fn start_repl<R: BufRead, W: Write>(input: R, mut output: W) {
    let mut scanner = input.lines();
    let mut history = Vec::new();

    write!(output, "Welcome to the Monkey REPL (Press Ctrl+D to exit)").unwrap();

    loop {
        write!(output, "{}", PROMT).unwrap();
        output.flush().expect("cannot flust the promt");

        let line = match scanner.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => {
                writeln!(output, "Error : {}", e).unwrap();
                continue;
            }
            None => break,
        };

        // skip empty input
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        match line {
            ":exit" | ":quit" => break,
            ":history" => {
                for (i, cmd) in history.iter().enumerate() {
                    writeln!(output, "{}: {}", i + 1, cmd).unwrap();
                }
                continue;
            }
            _ => {}
        }

        history.push(line.to_string());

        let mut lexer = Lexer::new(line.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        if p.errors_from_parser.len() != 0 {
            println!("{:?}", p.errors_from_parser);
            continue;
        }

        writeln!(output, "{:?}", p.string()).unwrap();
    }
}
