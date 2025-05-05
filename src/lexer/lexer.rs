use crate::token::token::{TokenType, lookup_ident};

#[derive(Debug)]
pub struct Lexer<'a> {
    // this will be the input and let's pass a refrence
    // I'm basica
    //
    // lly saying that Lexer can't outlive the refrence to input
    input: &'a str,
    // current reading position in input (point to current char)
    position: usize,
    // current reading position in input (after current char)usize,
    read_position: usize,
    // current nder examination
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        // baiscally there can never be leading whitespaces when the lexer is created so the posistions make sense.
        let input = input.trim_start();
        return Self {
            input,
            position: 0,
            read_position: 1,
            ch: input.as_bytes().get(0).copied().unwrap_or(0),
        };
    }

    fn read_char(&mut self) {
        // the char is a u8 cause it's all ascii and our monkey language only uses
        // ascii charactes.
        self.ch = self
            .input
            .as_bytes()
            .get(self.read_position)
            .copied()
            .unwrap_or(0);

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        let output = self
            .input
            .as_bytes()
            .get(self.read_position)
            .copied()
            .unwrap_or(0);

        output as char
    }

    pub fn next_token(&mut self) -> TokenType {
        self.skip_whitespace();
        let token = match self.ch {
            b'=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenType::EQ
                } else {
                    TokenType::ASSIGN
                }
            }
            b'!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenType::NOTEQ
                } else {
                    TokenType::BANG
                }
            }
            b';' => TokenType::SEMICOLON,
            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b',' => TokenType::COMMA,
            b'+' => TokenType::PLUS,
            b'-' => TokenType::MINUS,
            b'*' => TokenType::ASTERISK,
            b'/' => TokenType::SLASH,
            b'<' => TokenType::LT,
            b'>' => TokenType::GT,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,
            0 => TokenType::EOF,
            _ => {
                if self.is_letter() {
                    let tok_str = self.read_identifier();
                    return lookup_ident(tok_str);
                } else if self.is_digit() {
                    let tok_str_digits = self.read_digit();
                    return TokenType::INT(tok_str_digits.to_string());
                } else {
                    return TokenType::ILLEGAL;
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> &str {
        let pos = self.position;

        while self.is_letter() {
            self.read_char();
        }

        &self.input[pos..self.position]
    }

    fn read_digit(&mut self) -> &str {
        let pos = self.position;

        while self.is_digit() {
            self.read_char();
        }

        &self.input[pos..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.is_whitespace() {
            self.read_char();
        }
    }

    fn is_whitespace(&self) -> bool {
        let ch = self.ch as char;
        ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
    }

    fn is_letter(&self) -> bool {
        let lowercase_a = 'a' as u8;
        let uppercase_a = 'A' as u8;
        let lowercase_z = 'z' as u8;
        let uppercase_z = 'Z' as u8;

        (self.ch >= lowercase_a && self.ch <= lowercase_z)
            || (self.ch >= uppercase_a && self.ch <= uppercase_z)
            || self.ch == '_' as u8
    }

    fn is_digit(&self) -> bool {
        let zero = '0' as u8;
        let nine = '9' as u8;

        self.ch >= zero && self.ch <= nine
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lexer_easy() {
        let input = "=+(){},;";

        let output = vec![
            TokenType::ASSIGN,
            TokenType::PLUS,
            TokenType::LPAREN,
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::RBRACE,
            TokenType::COMMA,
            TokenType::SEMICOLON,
            TokenType::EOF,
        ];

        let mut l = Lexer::new(input);

        for (_, token_type) in output.iter().enumerate() {
            let tok = l.next_token();
            assert_eq!(tok, token_type.clone());
        }
    }

    #[test]
    fn test_lexer_complete() {
        let input = "    let five = 5;
let ten = 10;
let add = fn(x,y) {
    x + y;
};

let result = add(five,ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;

";
        let mut l = Lexer::new(input);
        assert_eq!(l.next_token(), TokenType::LET);
        assert_eq!(l.next_token(), TokenType::IDENT("five".to_string()));
        assert_eq!(l.next_token(), TokenType::ASSIGN);
        assert_eq!(l.next_token(), TokenType::INT("5".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::LET);
        assert_eq!(l.next_token(), TokenType::IDENT("ten".to_string()));
        assert_eq!(l.next_token(), TokenType::ASSIGN);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::LET);
        assert_eq!(l.next_token(), TokenType::IDENT("add".to_string()));
        assert_eq!(l.next_token(), TokenType::ASSIGN);
        assert_eq!(l.next_token(), TokenType::FUNCTION);
        assert_eq!(l.next_token(), TokenType::LPAREN);
        assert_eq!(l.next_token(), TokenType::IDENT("x".to_string()));
        assert_eq!(l.next_token(), TokenType::COMMA);
        assert_eq!(l.next_token(), TokenType::IDENT("y".to_string()));
        assert_eq!(l.next_token(), TokenType::RPAREN);
        assert_eq!(l.next_token(), TokenType::LBRACE);
        assert_eq!(l.next_token(), TokenType::IDENT("x".to_string()));
        assert_eq!(l.next_token(), TokenType::PLUS);
        assert_eq!(l.next_token(), TokenType::IDENT("y".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::RBRACE);
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::LET);
        assert_eq!(l.next_token(), TokenType::IDENT("result".to_string()));
        assert_eq!(l.next_token(), TokenType::ASSIGN);
        assert_eq!(l.next_token(), TokenType::IDENT("add".to_string()));
        assert_eq!(l.next_token(), TokenType::LPAREN);
        assert_eq!(l.next_token(), TokenType::IDENT("five".to_string()));
        assert_eq!(l.next_token(), TokenType::COMMA);
        assert_eq!(l.next_token(), TokenType::IDENT("ten".to_string()));
        assert_eq!(l.next_token(), TokenType::RPAREN);
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::BANG);
        assert_eq!(l.next_token(), TokenType::MINUS);
        assert_eq!(l.next_token(), TokenType::SLASH);
        assert_eq!(l.next_token(), TokenType::ASTERISK);
        assert_eq!(l.next_token(), TokenType::INT("5".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::INT("5".to_string()));
        assert_eq!(l.next_token(), TokenType::LT);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::GT);
        assert_eq!(l.next_token(), TokenType::INT("5".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::IF);
        assert_eq!(l.next_token(), TokenType::LPAREN);
        assert_eq!(l.next_token(), TokenType::INT("5".to_string()));
        assert_eq!(l.next_token(), TokenType::LT);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::RPAREN);
        assert_eq!(l.next_token(), TokenType::LBRACE);
        assert_eq!(l.next_token(), TokenType::RETURN);
        assert_eq!(l.next_token(), TokenType::TRUE);
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::RBRACE);
        assert_eq!(l.next_token(), TokenType::ELSE);
        assert_eq!(l.next_token(), TokenType::LBRACE);
        assert_eq!(l.next_token(), TokenType::RETURN);
        assert_eq!(l.next_token(), TokenType::FALSE);
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::RBRACE);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::EQ);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::INT("10".to_string()));
        assert_eq!(l.next_token(), TokenType::NOTEQ);
        assert_eq!(l.next_token(), TokenType::INT("9".to_string()));
        assert_eq!(l.next_token(), TokenType::SEMICOLON);
        assert_eq!(l.next_token(), TokenType::EOF);
    }
}
