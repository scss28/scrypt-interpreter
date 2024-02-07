use std::fmt::Display;

use crate::{
    expression_tree::Operator, interpreter::SpannedKind, lexer::TextIterator, value::ValueKind,
};

pub type Token = SpannedKind<TokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Literal(ValueKind),
    Ident(String),
    Keyword(Keyword),
    Operator(Operator),
    ConsumerIdent(String),
    ProducerIdent(String),
    OpenParenthesis,
    ClosedParenthesis,
    OpenSquareBracket,
    ClosedSquareBracket,
    Tab,
    Colon,
    ColonEq,
    Comma,
    DoubleSlash,
    DoubleDot,
    LineBreak,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Literal(value) => write!(f, "{} literal", value.type_name()),
            TokenKind::Ident(_) => write!(f, "identifier"),
            TokenKind::Keyword(_) => write!(f, "keyword"),
            TokenKind::Operator(_) => write!(f, "operator"),
            TokenKind::ConsumerIdent(_) => write!(f, "consumer"),
            TokenKind::ProducerIdent(_) => write!(f, "producer"),
            TokenKind::OpenParenthesis => write!(f, "open parenthesis"),
            TokenKind::ClosedParenthesis => write!(f, "closed parenthesis"),
            TokenKind::Tab => write!(f, "tab"),
            TokenKind::Colon => write!(f, "colon"),
            TokenKind::ColonEq => write!(f, "colon equals"),
            TokenKind::Comma => write!(f, "comma"),
            TokenKind::DoubleSlash => write!(f, "double slash"),
            TokenKind::LineBreak => write!(f, "line break"),
            TokenKind::OpenSquareBracket => write!(f, "open square bracket"),
            TokenKind::ClosedSquareBracket => write!(f, "closed square bracket"),
            TokenKind::DoubleDot => write!(f, "double dot"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    For,
    If,
    End,
}

impl Keyword {
    fn from_str(str: &str) -> Option<Self> {
        match str {
            "for" => Some(Keyword::For),
            "if" => Some(Keyword::If),
            "end" => Some(Keyword::End),
            _ => None,
        }
    }
}

pub enum TokenParseResult {
    Some(TokenKind),
    UnrecognizedToken,
    Skipped,
}

impl TokenKind {
    pub fn parse_next(iter: &mut TextIterator) -> TokenParseResult {
        let Some(byte) = iter.next() else {
            return TokenParseResult::Skipped;
        };

        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let mut string = String::from(byte as char);
                iter.collect_into_while(&mut string, |byte| match byte {
                    b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'0'..=b'9' => true,
                    _ => false,
                });

                if string == "in" {
                    return TokenParseResult::Some(TokenKind::Operator(Operator::In));
                }

                if let Some(keyword) = Keyword::from_str(&string) {
                    return TokenParseResult::Some(TokenKind::Keyword(keyword));
                }

                match iter.peek() {
                    Some(char) if char == b'!' => {
                        iter.next(); // Skip "!"
                        return TokenParseResult::Some(TokenKind::ConsumerIdent(string));
                    }
                    Some(char) if char == b'?' => {
                        iter.next(); // Skip "?"
                        return TokenParseResult::Some(TokenKind::ProducerIdent(string));
                    }
                    _ => (),
                }

                TokenParseResult::Some(TokenKind::Ident(string))
            }
            b'0'..=b'9' => {
                let mut string = String::from(byte as char);
                iter.collect_into_while(&mut string, |byte| match byte {
                    b'0'..=b'9' => true,
                    _ => false,
                });

                let Ok(integer) = string.parse() else {
                    return TokenParseResult::UnrecognizedToken;
                };

                TokenParseResult::Some(TokenKind::Literal(ValueKind::Integer(integer)))
            }
            b':' => {
                if iter.peek() == Some(b'=') {
                    iter.next();
                    return TokenParseResult::Some(TokenKind::ColonEq);
                }

                if iter.peek() == Some(b':') {
                    iter.next();
                    return TokenParseResult::Some(TokenKind::Operator(Operator::Eq));
                }

                TokenParseResult::Some(TokenKind::Colon)
            }
            b'.' => match iter.next() {
                Some(b'.') => TokenParseResult::Some(TokenKind::DoubleDot),
                _ => TokenParseResult::UnrecognizedToken,
            },
            b',' => TokenParseResult::Some(TokenKind::Comma),
            b'+' => TokenParseResult::Some(TokenKind::Operator(Operator::Add)),
            b'-' => {
                let Ok(integer) = iter.collect_while(|byte| match byte {
                    b'0'..=b'9' => true,
                    _ => false,
                }).parse::<i32>() else {
                    return TokenParseResult::Some(TokenKind::Operator(Operator::Sub));
                };

                TokenParseResult::Some(TokenKind::Literal(ValueKind::Integer(-integer)))
            }
            b'*' => TokenParseResult::Some(TokenKind::Operator(Operator::Mul)),
            b'/' => {
                if iter.peek() == Some(b'/') {
                    iter.next();
                    return TokenParseResult::Some(TokenKind::DoubleSlash);
                }

                TokenParseResult::Some(TokenKind::Operator(Operator::Div))
            }
            b'%' => TokenParseResult::Some(TokenKind::Operator(Operator::Mod)),
            b'\"' => {
                let string = iter.collect_while(|byte| byte != b'"');
                iter.next();

                TokenParseResult::Some(TokenKind::Literal(ValueKind::String(string)))
            }
            b'(' => TokenParseResult::Some(TokenKind::OpenParenthesis),
            b')' => TokenParseResult::Some(TokenKind::ClosedParenthesis),
            b'\r' => {
                if iter.next() == Some(b'\n') {
                    return TokenParseResult::Some(TokenKind::LineBreak);
                }

                TokenParseResult::UnrecognizedToken
            }
            b'\t' => TokenParseResult::Some(TokenKind::Tab),
            b' ' => {
                for _ in 0..3 {
                    if iter.peek() != Some(b' ') {
                        return TokenParseResult::Skipped;
                    }

                    iter.next();
                }

                TokenParseResult::Some(TokenKind::Tab)
            }
            b'&' => TokenParseResult::Some(TokenKind::Operator(Operator::And)),
            b'|' => TokenParseResult::Some(TokenKind::Operator(Operator::Or)),
            b'<' => TokenParseResult::Some(TokenKind::Operator(Operator::LessThan)),
            b'>' => TokenParseResult::Some(TokenKind::Operator(Operator::GreaterThan)),
            b'[' => TokenParseResult::Some(TokenKind::OpenSquareBracket),
            b']' => TokenParseResult::Some(TokenKind::ClosedSquareBracket),
            _ => TokenParseResult::UnrecognizedToken,
        }
    }
}
