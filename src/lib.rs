#![allow(warnings)]
mod ast;
// mod errors;
// mod evaluator;
mod lexer;
// mod object;
mod parser;
// pub mod repl;
mod token;

pub use lexer::lexer::Lexer;
pub use token::token::{PRECEDENCE, TokenType};
