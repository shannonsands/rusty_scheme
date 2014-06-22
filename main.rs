use std::fmt;
use std::str;
use std::iter;
use std::from_str;

fn main() {
    run("(+ 2 3)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = Lexer::tokenize(s);
    println!("tokens: {}", tokens);
}

enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(int),
}

impl fmt::Show for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OpenParen => write!(f, "OpenParen"),
            CloseParen => write!(f, "CloseParen"),
            Identifier(ref v) => write!(f, "Identifier({})", v),
            Integer(ref v) => write!(f, "Integer({})", v),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.to_str() == other.to_str()
    }
}

struct Lexer<'a> {
  chars: iter::Peekable<char, str::Chars<'a>>,
  current: Option<char>,
  tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, &'static str> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new() };
        try!(lexer.run());
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        self.current = self.chars.next()
    }

    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(c) => Some(*c),
            None => None
        }
    }

    fn run(&mut self) -> Result<(), &'static str> {
        self.advance();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '(' => {
                            self.tokens.push(OpenParen);
                            self.advance();
                        },
                        ')' => {
                            self.tokens.push(CloseParen);
                            self.advance();
                        },
                        '+' | '-' => {
                            match self.peek() {
                                Some('0'..'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = self.parse_number();
                                    self.tokens.push(Integer(if c == '-' { -1 * val } else { val }));
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(Identifier(str::from_char(c)));
                                    self.advance();
                                }
                            }
                        },
                        '0'..'9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = self.parse_number();
                            self.tokens.push(Integer(val));
                        },
                        ' ' => self.advance(),
                        _   => return Err("unexpected character"),
                    }
                },
                None => break
            }
        };
        Ok(())
    }

    fn parse_number(&mut self) -> int {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0'..'9' => {
                            s.push_char(c);
                            self.advance();
                        },
                        _ => break
                    }
                },
                None => break
            }
        }
        from_str::from_str(s.as_slice()).unwrap()
    }
}

#[test]
fn test_simple_lexing() {
    assert_eq!(Lexer::tokenize("(+ 2 3)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(2), Integer(3), CloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(Lexer::tokenize("(+ 21 325)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(21), Integer(325), CloseParen]);
}

#[test]
fn test_subtraction() {
    assert_eq!(Lexer::tokenize("(- 7 42)").unwrap(),
               vec![OpenParen, Identifier("-".to_str()), Integer(7), Integer(42), CloseParen]);
}

#[test]
fn test_negative_integers() {
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(-8), Integer(2), Integer(-33), CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert!(Lexer::tokenize("(&&)").is_err())
}
