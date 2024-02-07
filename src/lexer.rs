use thiserror::Error;

use crate::{
    interpreter::SpannedKind,
    token::{Token, TokenKind, TokenParseResult},
};

pub type LexerError = SpannedKind<LexerErrorKind>;

#[derive(Error, Debug)]
pub enum LexerErrorKind {
    #[error("Unrecognized token")]
    UnrecognizedToken,
}

pub fn tokenize(bytes: &[u8]) -> Result<Vec<Token>, LexerError> {
    let mut iter = TextIterator::from(bytes);
    let mut tokens = Vec::new();
    while let Some(_) = iter.peek() {
        let start = iter.index();
        let kind = match TokenKind::parse_next(&mut iter) {
            TokenParseResult::Some(kind) => kind,
            TokenParseResult::UnrecognizedToken => {
                return Err(LexerError::new(
                    start..iter.index(),
                    LexerErrorKind::UnrecognizedToken,
                ))
            }
            TokenParseResult::Skipped => continue,
        };

        tokens.push(Token {
            kind,
            span: start..iter.index(),
        });
    }

    Ok(tokens)
}

pub struct TextIterator<'a> {
    bytes: &'a [u8],
    index: usize,
}

impl<'a> TextIterator<'a> {
    pub fn peek(&self) -> Option<u8> {
        self.bytes.get(self.index).copied()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn collect_into_while<F: Fn(u8) -> bool>(&mut self, bytes: &mut String, predicate: F) {
        while let Some(byte) = self.peek() {
            if !predicate(byte) {
                break;
            }

            bytes.push(byte as char);
            self.next();
        }
    }

    pub fn collect_while<F: Fn(u8) -> bool>(&mut self, predicate: F) -> String {
        let mut string = String::new();
        self.collect_into_while(&mut string, predicate);
        string
    }
}

impl<'a> Iterator for TextIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.bytes.get(self.index)?;
        self.index += 1;

        Some(*char)
    }
}

impl<'a> From<&'a [u8]> for TextIterator<'a> {
    fn from(value: &'a [u8]) -> Self {
        TextIterator {
            bytes: value,
            index: 0,
        }
    }
}
