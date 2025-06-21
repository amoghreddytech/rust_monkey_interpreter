use crate::evaluator::evaluator::Node;
use crate::evaluator::{evaluator::eval, object::Environment};
use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use anyhow::anyhow;
use colored::*;
use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc; // Add this

pub fn start_repl<R: BufRead, W: Write>(input: R, mut output: W) -> anyhow::Result<()> {
    // Print colorful monkey welcome banner
    writeln!(
        output,
        "{}",
        r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \     
 | |  '|  /   Y   \ |'  | |     
 | \   \  \ 0 | 0 /  /   / |    
  \ '- ,\.-"`` ``"-./, -' /     
   `'-' /_   ^ ^   _\ '-'`      
       |  \._   _./  |         
       \   \ `~` /   /         
        '._ '-=-' _.'          
           '-----'           
"#
        .bright_yellow()
        .bold()
    )
    .unwrap();
    writeln!(output, "{}", "Welcome to the Monkey REPL 🐒".green().bold()).unwrap();
    writeln!(
        output,
        "{}",
        "Type `:exit`, `:quit` or Ctrl+D to exit. Use `:history` to see past commands."
            .bright_cyan()
    )
    .unwrap();

    writeln!(output, "").unwrap();

    let mut scanner = input.lines();
    let mut history = Vec::new();
    let mut env = Rc::new(RefCell::new(Environment::new()));

    loop {
        write!(output, "{}", ">>".bright_blue()).unwrap();

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
            ":exit" | ":quit" => {
                writeln!(
                    output,
                    "{}",
                    "Exiting Monkey REPL. Goodbye 🐵!".bright_red()
                )
                .unwrap();
                break;
            }
            ":history" => {
                for (i, cmd) in history.iter().enumerate() {
                    writeln!(output, "{}: {}", (i + 1).to_string().cyan(), cmd).unwrap();
                }
                continue;
            }
            _ => {}
        }

        history.push(line.to_string());

        let lexer = Lexer::new(line.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse_program()
            .map_err(|errs| anyhow!("Parser errors : {:?}", errs))?;

        let object = eval(Node::ProgramNode(&program), Rc::clone(&env));

        if !parser.errors.is_empty() {
            for err in &parser.errors {
                writeln!(output, "{}", format!("Parser error: {}", err).red()).unwrap();
            }
            continue;
        }

        match object {
            Ok(eval) => {
                let output_string = eval.inspect();
                writeln!(output, "{:?}", output_string).unwrap()
            }
            Err(e) => writeln!(output, "{}", e).unwrap(),
        }
    }

    Ok(())
}
