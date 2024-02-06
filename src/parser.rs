use crate::{
    syntax_tree::{SyntaxTree, SyntaxTreeParseError, SyntaxTreeParseResult},
    token::Token,
};

pub type ParserError = SyntaxTreeParseError;

pub fn parse<I: IntoIterator<Item = Token>>(iter: I) -> Result<Vec<SyntaxTree>, ParserError> {
    let mut iter = iter.into_iter().peekable();
    let mut tokens = Vec::new();

    loop {
        match SyntaxTree::parse_next(&mut iter) {
            SyntaxTreeParseResult::Some(token) => tokens.push(token),
            SyntaxTreeParseResult::None => break,
            SyntaxTreeParseResult::Err(err) => return Err(err),
        }
    }

    Ok(tokens)
}
