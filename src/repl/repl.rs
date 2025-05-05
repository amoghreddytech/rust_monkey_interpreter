use crate::lexer::lexer::Lexer;
use crate::token::token::TokenType;
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

        let mut lexer = Lexer::new(line);
        let mut token_count = 0;

        loop {
            let token: TokenType = lexer.next_token();
            writeln!(output, "    {:?}", token).unwrap();

            token_count += 1;

            if token == TokenType::EOF {
                break;
            }
        }

        writeln!(output, "Process {} tokens", token_count - 1).unwrap();
    }
}
