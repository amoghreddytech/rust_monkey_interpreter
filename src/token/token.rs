use lazy_static::lazy_static;
use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // identifiers + literals
    IDENT(String),
    INT(String),

    // Operators
    ASSIGN,
    EQ,
    NOTEQ,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    // Delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

lazy_static! {
    pub static ref keywords: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("fn", TokenType::FUNCTION);
        map.insert("let", TokenType::LET);
        map.insert("true", TokenType::TRUE);
        map.insert("false", TokenType::FALSE);
        map.insert("if", TokenType::IF);
        map.insert("else", TokenType::ELSE);
        map.insert("return", TokenType::RETURN);
        map
    };
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match keywords.get(ident) {
        Some(token_type) => token_type.clone(),
        None => TokenType::IDENT(ident.to_string()),
    }
}

impl TokenType {
    pub fn string_representation(&self) -> String {
        match &self {
            TokenType::ILLEGAL => "illegal".to_string(),
            TokenType::EOF => "eof".to_string(),
            TokenType::IDENT(i) => i.clone(),
            TokenType::INT(i) => i.clone(),
            TokenType::ASSIGN => "=".to_string(),
            TokenType::EQ => "==".to_string(),
            TokenType::NOTEQ => "!=".to_string(),
            TokenType::PLUS => "+".to_string(),
            TokenType::MINUS => "-".to_string(),
            TokenType::BANG => "!".to_string(),
            TokenType::ASTERISK => "*".to_string(),
            TokenType::SLASH => "/".to_string(),
            TokenType::LT => "<".to_string(),
            TokenType::GT => ">".to_string(),
            TokenType::COMMA => ",".to_string(),
            TokenType::SEMICOLON => ";".to_string(),
            TokenType::LPAREN => "(".to_string(),
            TokenType::RPAREN => ")".to_string(),
            TokenType::LBRACE => "{".to_string(),
            TokenType::RBRACE => "}".to_string(),
            TokenType::FUNCTION => "func".to_string(),
            TokenType::LET => "let".to_string(),
            TokenType::TRUE => "true".to_string(),
            TokenType::FALSE => "false".to_string(),
            TokenType::IF => "if".to_string(),
            TokenType::ELSE => "else".to_string(),
            TokenType::RETURN => "return".to_string(),
        }
    }
}
