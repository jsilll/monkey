use std::iter::Peekable;
use std::str::CharIndices;

use crate::common::position::Position;
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
        match self.scan_while(|c| c.is_alphanumeric())? {
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
        let (res, fall) = match self.chars.next()?.1 {
            '=' => (Token::Eq, Token::Assign),
            '!' => (Token::Neq, Token::Bang),
            '<' => (Token::Lte, Token::Lt),
            '>' => (Token::Gte, Token::Gt),
            _ => unreachable!(),
        };
        Some(LocatedToken {
            position,
            token: self.make_double_char('=', res, fall)?,
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
                c if c.is_alphabetic() => self.scan_name(),

                '=' => self.scan_double_char(),

                '+' => self.scan_single_char(Token::Plus),
                '-' => self.scan_single_char(Token::Minus),
                '*' => self.scan_single_char(Token::Star),
                '/' => self.scan_single_char(Token::Slash),

                '<' => self.scan_double_char(),
                '>' => self.scan_double_char(),

                '!' => self.scan_double_char(),

                ',' => self.scan_single_char(Token::Comma),
                ';' => self.scan_single_char(Token::Semi),
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
