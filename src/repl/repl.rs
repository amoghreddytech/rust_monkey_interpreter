use crate::ast::ast::Program;
use crate::evaluator::evaluator::evaluate;
use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use std::io::{BufRead, Write};

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

        let evaluated = evaluate(&p);

        match evaluated {
            Ok(eval) => {
                let output_string = eval.inspect();
                writeln!(output, "{:?}", output_string).unwrap()
            }
            Err(e) => writeln!(output, "{}", e).unwrap(),
        }
    }
}
