use std::iter::Peekable;
use std::str::CharIndices;

use crate::common::Position;
use crate::frontend::token::{LocatedToken, Token};

pub struct Lexer<'a> {
    source: &'a str,
    position: Position,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(fname: &str, source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            position: Position {
                line: 1,
                column: 1,
                fname: fname.to_owned(),
            },
        }
    }

    pub fn position(&self) -> &Position {
        &self.position
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, c)) = self.chars.peek() {
            match c {
                '\n' => {
                    self.chars.next();
                    self.position.line += 1;
                    self.position.column = 1;
                }

                ' ' | '\t' | '\r' => {
                    self.chars.next();
                    self.position.column += 1;
                }

                _ => break,
            }
        }
    }

    fn scan_while(&mut self, f: impl Fn(char) -> bool) -> Option<&'a str> {
        let start = self.chars.peek()?.0;
        let end = loop {
            match self.chars.peek() {
                Some((_, c)) if f(*c) => {
                    self.chars.next();
                    self.position.column += 1;
                }

                _ => {
                    break match self.chars.peek() {
                        Some((idx, _)) => *idx,
                        None => self.source.len(),
                    }
                }
            }
        };
        Some(&self.source[start..end])
    }

    fn scan_name(&mut self) -> Option<LocatedToken<'a>> {
        let position = self.position.clone();
        match self.scan_while(|c| c.is_alphanumeric() || c == '_')? {
            "true" => Some(LocatedToken {
                position,
                token: Token::True,
            }),

            "false" => Some(LocatedToken {
                position,
                token: Token::False,
            }),

            "fn" => Some(LocatedToken {
                position,
                token: Token::Fn,
            }),

            "let" => Some(LocatedToken {
                position,
                token: Token::Let,
            }),

            "var" => Some(LocatedToken {
                position,
                token: Token::Var,
            }),

            "return" => Some(LocatedToken {
                position,
                token: Token::Return,
            }),

            "if" => Some(LocatedToken {
                position,
                token: Token::If,
            }),

            "else" => Some(LocatedToken {
                position,
                token: Token::Else,
            }),

            "i64" => Some(LocatedToken {
                position,
                token: Token::IntType,
            }),

            id => Some(LocatedToken {
                position,
                token: Token::Id(id),
            }),
        }
    }

    fn scan_integer(&mut self) -> Option<LocatedToken<'a>> {
        let position = self.position.clone();
        let lex = self.scan_while(|c| c.is_numeric())?;
        Some(LocatedToken {
            position,
            token: Token::Int(lex),
        })
    }

    fn scan_single_char(&mut self, token: Token<'a>) -> Option<LocatedToken<'a>> {
        self.chars.next();
        let position = self.position.clone();
        self.position.column += 1;
        Some(LocatedToken { token, position })
    }

    fn scan_double_char(&mut self) -> Option<LocatedToken<'a>> {
        let position = self.position.clone();
        self.position.column += 1;
        let (exp, res, fall) = match self.chars.next()?.1 {
            '>' => ('=', Token::Gte, Token::Gt),
            '<' => ('=', Token::Lte, Token::Lt),
            '!' => ('=', Token::Neq, Token::Bang),
            '=' => ('=', Token::Eq, Token::Assign),
            '-' => ('>', Token::Arrow, Token::Minus),
            _ => unreachable!(),
        };
        Some(LocatedToken {
            position,
            token: self.make_double_char(exp, res, fall)?,
        })
    }

    fn make_double_char(
        &mut self,
        exp: char,
        res: Token<'a>,
        fall: Token<'a>,
    ) -> Option<Token<'a>> {
        match self.chars.peek()?.1 {
            c if c == exp => {
                self.chars.next();
                self.position.column += 1;
                Some(res)
            }
            _ => Some(fall),
        }
    }

    fn scan_unexpected(&mut self) -> Option<LocatedToken<'a>> {
        let c = self.chars.next()?.1;
        let position = self.position.clone();
        self.position.column += 1;
        Some(LocatedToken {
            position,
            token: Token::Unexpected(c),
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LocatedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((_, c)) = self.chars.peek() {
            match c {
                c if c.is_numeric() => self.scan_integer(),
                c if c.is_alphabetic() || *c == '_' => self.scan_name(),

                '=' => self.scan_double_char(),

                '+' => self.scan_single_char(Token::Plus),
                '-' => self.scan_double_char(),
                '*' => self.scan_single_char(Token::Star),
                '/' => self.scan_single_char(Token::Slash),

                '<' => self.scan_double_char(),
                '>' => self.scan_double_char(),

                '!' => self.scan_double_char(),

                ',' => self.scan_single_char(Token::Comma),
                ';' => self.scan_single_char(Token::Semi),
                ':' => self.scan_single_char(Token::Colon),
                '(' => self.scan_single_char(Token::LParen),
                ')' => self.scan_single_char(Token::RParen),
                '{' => self.scan_single_char(Token::LBrace),
                '}' => self.scan_single_char(Token::RBrace),

                _ => self.scan_unexpected(),
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
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
        let mut lexer = Lexer::new("test", source);
        let expected = vec![
            Token::Let,
            Token::Id("five"),
            Token::Assign,
            Token::Int("5"),
            Token::Semi,
            Token::Let,
            Token::Id("ten"),
            Token::Assign,
            Token::Int("10"),
            Token::Semi,
            Token::Let,
            Token::Id("add"),
            Token::Assign,
            Token::Fn,
            Token::LParen,
            Token::Id("x"),
            Token::Comma,
            Token::Id("y"),
            Token::RParen,
            Token::LBrace,
            Token::Id("x"),
            Token::Plus,
            Token::Id("y"),
            Token::Semi,
            Token::RBrace,
            Token::Semi,
            Token::Let,
            Token::Id("result"),
            Token::Assign,
            Token::Id("add"),
            Token::LParen,
            Token::Id("five"),
            Token::Comma,
            Token::Id("ten"),
            Token::RParen,
            Token::Semi,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Star,
            Token::Int("5"),
            Token::Semi,
            Token::Int("5"),
            Token::Lt,
            Token::Int("10"),
            Token::Gt,
            Token::Int("5"),
            Token::Semi,
            Token::If,
            Token::LParen,
            Token::Int("5"),
            Token::Lt,
            Token::Int("10"),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semi,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semi,
            Token::RBrace,
            Token::Int("10"),
            Token::Eq,
            Token::Int("10"),
            Token::Semi,
            Token::Int("10"),
            Token::Neq,
            Token::Int("9"),
            Token::Semi,
        ];

        for (i, (expected, actual)) in expected.iter().zip(lexer.by_ref()).enumerate() {
            assert_eq!(*expected, actual.token, "Token {} mismatch", i);
        }
    }
}
