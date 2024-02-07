use std::{
    fmt::Display,
    ops::{Deref, Range},
    string::FromUtf8Error,
};

use crate::{
    lexer::{self, LexerError},
    parser::{self, ParserError},
    runtime::{self, Runtime, RuntimeError},
};

pub trait Spanned {
    fn span(&self) -> Range<usize>;
}

#[derive(Debug, Clone)]
pub struct SpannedKind<K> {
    pub span: Range<usize>,
    pub kind: K,
}

impl<K> SpannedKind<K> {
    pub fn new(span: Range<usize>, kind: K) -> Self {
        Self { span, kind }
    }

    pub fn map_kind<L, F: FnOnce(K) -> L>(self, func: F) -> SpannedKind<L> {
        SpannedKind {
            span: self.span,
            kind: func(self.kind),
        }
    }
}

impl<K> Deref for SpannedKind<K> {
    type Target = K;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<K> Spanned for SpannedKind<K> {
    fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

pub enum ExecutionErrorKind {
    NonUtf8Code(FromUtf8Error),
    LexerError {
        code_text: String,
        err: LexerError,
    },
    ParserError {
        code_text: String,
        err: ParserError,
    },
    RuntimeError {
        code_text: String,
        err: RuntimeError,
    },
}

impl Display for ExecutionErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (err_type, err_message, code_text, span) = match self {
            ExecutionErrorKind::NonUtf8Code(err) => return write!(f, "{}", err),
            ExecutionErrorKind::LexerError { code_text, err } => (
                "Lexer Error",
                err.kind.to_string(),
                code_text,
                err.span.clone(),
            ),
            ExecutionErrorKind::ParserError { code_text, err } => (
                "Parser Error",
                err.kind.to_string(),
                code_text,
                err.span.clone(),
            ),
            ExecutionErrorKind::RuntimeError { code_text, err } => (
                "Runtime Error",
                err.kind.to_string(),
                code_text,
                err.span.clone(),
            ),
        };

        let (line, char) = code_text
            .bytes()
            .rev()
            .skip(code_text.len() - span.start)
            .rev()
            .fold((1, 1), |acc, byte| {
                if byte == b'\n' {
                    (acc.0 + 1, 1)
                } else {
                    (acc.0, acc.1 + 1)
                }
            });

        let start = code_text
            .bytes()
            .enumerate()
            .rev()
            .skip(code_text.len() - span.start)
            .skip_while(|(_, byte)| *byte != b'\n')
            .map(|(idx, _)| idx + 1)
            .next()
            .unwrap_or(span.start);

        let end = code_text
            .bytes()
            .enumerate()
            .skip(span.end)
            .skip_while(|(_, byte)| *byte != b'\r')
            .map(|(idx, _)| idx)
            .next()
            .unwrap_or(span.end);

        write!(
            f,
            "\n\x1b[93;1m[{}] \x1b[92m{}\x1b[0m\x1b[0m: \"\x1b[90;4m{}\x1b[93;1m{}\x1b[0m{}\" \x1b[90mline {}:{}\x1b[0m",
            err_type,
            err_message,
            &code_text[start..span.start],
            &code_text[span.clone()],
            &code_text[span.end..end],
            line,
            char
        )
    }
}

pub fn run(bytes: &[u8]) -> Result<(), ExecutionErrorKind> {
    let code_text =
        String::from_utf8(bytes.to_vec()).map_err(|err| ExecutionErrorKind::NonUtf8Code(err))?;
    let tokens = match lexer::tokenize(bytes) {
        Ok(tokens) => tokens,
        Err(err) => return Err(ExecutionErrorKind::LexerError { code_text, err }),
    };

    #[cfg(debug_assertions)]
    println!(
        "Tokens:\n{}",
        tokens
            .iter()
            .map(|token| format!(
                "\x1b[92m{:?}\x1b[0m \x1b[90m{:?}{}\x1b[0m",
                token.kind,
                token.span,
                match token.kind {
                    crate::token::TokenKind::LineBreak => "\n",
                    _ => " | ",
                }
            ))
            .collect::<String>()
            .split('\n')
            .enumerate()
            .map(|(idx, line)| format!("\x1b[90m{}.\x1b[0m {}\n", idx + 1, line))
            .collect::<String>()
    );

    let abstract_tokens = match parser::parse(tokens) {
        Ok(abstract_tokens) => abstract_tokens,
        Err(err) => return Err(ExecutionErrorKind::ParserError { code_text, err }),
    };

    runtime::evaluate(Runtime::default(), abstract_tokens)
        .map_err(|err| ExecutionErrorKind::RuntimeError { code_text, err })
}
