use std::{fmt::Display, iter::Peekable};

use thiserror::Error;

use crate::{
    expression_tree::{ExpressionTree, Value},
    interpreter::Spanned,
    token::{Keyword, Token, TokenKind},
};

pub enum SyntaxTree {
    VariableInit {
        ident: Ident,
        tree: ExpressionTree<RuntimeValue>,
    },
    ConsumerCall {
        ident: Ident,
        trees: Vec<ExpressionTree<RuntimeValue>>,
    },
    IgnoredProducerCall {
        ident: Ident,
        trees: Vec<ExpressionTree<RuntimeValue>>,
    },
    Loop {
        ident: Ident,
        tokens: Vec<SyntaxTree>,
    },
    If {
        tree: ExpressionTree<RuntimeValue>,
        tokens: Vec<SyntaxTree>,
    },
    End {
        tree: Option<ExpressionTree<RuntimeValue>>,
    },
}

pub type SyntaxTreeParseError = Spanned<SyntaxTreeParseErrorKind>;

pub enum SyntaxTreeParseResult {
    Some(SyntaxTree),
    None,
    Err(SyntaxTreeParseError),
}

#[derive(Debug, Error)]
pub enum SyntaxTreeParseErrorKind {
    #[error("Expected a colon")]
    ExpectedColon,
    #[error("Unexpected start of expression")]
    UnexpectedExpressionStart,
    #[error("{0}")]
    TreeParseError(ExpressionParseError),
    #[error("Expected a line break")]
    ExpectedLineBreak,
    #[error("Expected an expression after if")]
    ExpectedExpressionAfterIf,
}

#[derive(Debug)]
pub enum ExpressionParseError {
    ExpectedValueFound(Option<TokenKind>),
    ExpectedClosedParenthesis,
}

impl Display for ExpressionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionParseError::ExpectedValueFound(kind) => write!(
                f,
                "Expected a value found {}",
                kind.as_ref()
                    .map(|kind| format!("{}", kind))
                    .unwrap_or("nothing".to_owned())
            ),
            ExpressionParseError::ExpectedClosedParenthesis => {
                write!(f, "Expected a closing parenthesis")
            }
        }
    }
}

impl SyntaxTree {
    pub fn parse_next<I: Iterator<Item = Token>>(iter: &mut Peekable<I>) -> SyntaxTreeParseResult {
        fn parse_args<I: Iterator<Item = Token>>(
            iter: &mut Peekable<I>,
        ) -> Result<Vec<ExpressionTree<RuntimeValue>>, ExpressionParseError> {
            let mut args = match parse_expression_tree(iter) {
                Ok(ast) => vec![ast],
                Err(err) => match err {
                    ExpressionParseError::ExpectedValueFound(None)
                    | ExpressionParseError::ExpectedValueFound(Some(TokenKind::LineBreak)) => {
                        Vec::new()
                    }
                    _ => return Err(err),
                },
            };

            while let Some(token) = iter.peek().cloned() {
                if token.kind != TokenKind::Comma {
                    break;
                }

                iter.next();
                let Ok(ast) = parse_expression_tree(iter) else {
                    break;
                };

                args.push(ast);
            }

            Ok(args)
        }

        fn parse_expression_tree<I: Iterator<Item = Token>>(
            iter: &mut Peekable<I>,
        ) -> Result<ExpressionTree<RuntimeValue>, ExpressionParseError> {
            fn parse_value<I: Iterator<Item = Token>>(
                iter: &mut Peekable<I>,
            ) -> Result<ExpressionTree<RuntimeValue>, ExpressionParseError> {
                match iter.peek().cloned() {
                    Some(token) => match token.kind.clone() {
                        TokenKind::Literal(value) => {
                            iter.next();
                            Ok(ExpressionTree::Value(RuntimeValue::Value(Value::new(
                                token.span, value,
                            ))))
                        }
                        TokenKind::Ident(ident) => {
                            iter.next();
                            Ok(ExpressionTree::Value(RuntimeValue::Ident(Ident::new(
                                token.span, ident,
                            ))))
                        }
                        TokenKind::OpenParenthesis => {
                            iter.next();
                            Ok(ExpressionTree::new_enclosed(parse_expression_tree(iter)?))
                        }
                        TokenKind::ProducerIdent(ident) => {
                            iter.next();
                            Ok(ExpressionTree::Value(RuntimeValue::ProducerCall {
                                ident: Ident::new(token.span, ident),
                                asts: parse_args(iter)?,
                            }))
                        }
                        _ => Err(ExpressionParseError::ExpectedValueFound(Some(
                            token.kind.clone(),
                        ))),
                    },
                    None => Err(ExpressionParseError::ExpectedValueFound(None)),
                }
            }

            let left = parse_value(iter)?;
            match left {
                ExpressionTree::Enclosed(_) => match iter.next() {
                    Some(token) if token.kind == TokenKind::ClosedParenthesis => (),
                    _ => return Err(ExpressionParseError::ExpectedClosedParenthesis),
                },
                _ => (),
            }

            let Some(token) = iter.peek() else {
                return Ok(left);
            };

            let operator = match token.kind {
                TokenKind::Operator(operator) => operator,
                _ => return Ok(left),
            };
            iter.next();

            let right = parse_value(iter)?;
            match right {
                ExpressionTree::Enclosed(_) => match iter.next() {
                    Some(token) if token.kind == TokenKind::ClosedParenthesis => (),
                    _ => return Err(ExpressionParseError::ExpectedClosedParenthesis),
                },
                _ => (),
            }

            let mut tree = ExpressionTree::new_expression(left, operator, right);
            while let Some(token) = iter.peek().cloned() {
                let operator = match token.kind {
                    TokenKind::Operator(operator) => operator,
                    _ => break,
                };
                iter.next();

                let right = parse_value(iter)?;
                match right {
                    ExpressionTree::Enclosed(_) => match iter.next() {
                        Some(token) if token.kind == TokenKind::ClosedParenthesis => (),
                        _ => return Err(ExpressionParseError::ExpectedClosedParenthesis),
                    },
                    _ => (),
                }

                tree.append(operator, right);
            }

            Ok(tree)
        }

        let Some(token) = iter.next() else {
            return SyntaxTreeParseResult::None;
        };

        let token = match token.kind {
            TokenKind::Ident(ident) => {
                match iter.next() {
                    Some(token) if token.kind == TokenKind::Colon => (),
                    _ => {
                        return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                            token.span,
                            SyntaxTreeParseErrorKind::ExpectedColon,
                        ))
                    }
                }

                SyntaxTree::VariableInit {
                    ident: Ident::new(token.span.clone(), ident),
                    tree: match parse_expression_tree(iter) {
                        Ok(tree) => tree,
                        Err(err) => {
                            return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                                token.span,
                                SyntaxTreeParseErrorKind::TreeParseError(err),
                            ))
                        }
                    },
                }
            }
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::Loop => {
                    todo!()
                }
                Keyword::If => {
                    let tree = match parse_expression_tree(iter) {
                        Ok(tree) => tree,
                        Err(err) => {
                            return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                                token.span.clone(),
                                SyntaxTreeParseErrorKind::TreeParseError(err),
                            ))
                        }
                    };

                    let Some(token) = iter.next_if(|token| token.kind == TokenKind::LineBreak) else {
                        return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                            token.span,
                            SyntaxTreeParseErrorKind::ExpectedLineBreak,
                        ))
                    };

                    let Some(token) = iter.next_if(|token| token.kind == TokenKind::Tab) else {
                        return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                            token.span,
                            SyntaxTreeParseErrorKind::ExpectedExpressionAfterIf,
                        ))
                    };

                    let mut tokens = vec![match SyntaxTree::parse_next(iter) {
                        SyntaxTreeParseResult::Some(tree) => tree,
                        SyntaxTreeParseResult::None => {
                            return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                                token.span,
                                SyntaxTreeParseErrorKind::ExpectedExpressionAfterIf,
                            ))
                        }
                        err => return err,
                    }];

                    while let Some(_) = iter.next_if(|token| token.kind == TokenKind::Tab) {
                        tokens.push(match SyntaxTree::parse_next(iter) {
                            SyntaxTreeParseResult::Some(tree) => tree,
                            SyntaxTreeParseResult::None => break,
                            err => return err,
                        })
                    }

                    return SyntaxTreeParseResult::Some(SyntaxTree::If { tree, tokens });
                }
                Keyword::End => {
                    let ast = match parse_expression_tree(iter) {
                        Ok(ast) => Some(ast),
                        Err(err) => match err {
                            ExpressionParseError::ExpectedValueFound(None)
                            | ExpressionParseError::ExpectedValueFound(Some(
                                TokenKind::LineBreak,
                            )) => None,
                            _ => {
                                return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                                    token.span,
                                    SyntaxTreeParseErrorKind::TreeParseError(err),
                                ))
                            }
                        },
                    };

                    SyntaxTree::End { tree: ast }
                }
            },
            TokenKind::ConsumerIdent(ident) => SyntaxTree::ConsumerCall {
                ident: Ident::new(token.span.clone(), ident),
                trees: match parse_args(iter) {
                    Ok(trees) => trees,
                    Err(err) => {
                        return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                            token.span,
                            SyntaxTreeParseErrorKind::TreeParseError(err),
                        ))
                    }
                },
            },
            TokenKind::ProducerIdent(ident) => SyntaxTree::IgnoredProducerCall {
                ident: Ident::new(token.span.clone(), ident),
                trees: match parse_args(iter) {
                    Ok(trees) => trees,
                    Err(err) => {
                        return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                            token.span,
                            SyntaxTreeParseErrorKind::TreeParseError(err),
                        ))
                    }
                },
            },
            TokenKind::DoubleSlash => {
                while let Some(token) = iter.next() {
                    if token.kind == TokenKind::LineBreak {
                        break;
                    }
                }

                match SyntaxTree::parse_next(iter) {
                    SyntaxTreeParseResult::Some(token) => token,
                    result => return result,
                }
            }
            TokenKind::LineBreak => return SyntaxTree::parse_next(iter),
            _ => {
                return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                    token.span,
                    SyntaxTreeParseErrorKind::UnexpectedExpressionStart,
                ))
            }
        };

        match iter.next() {
            Some(token) if token.kind == TokenKind::LineBreak => (),
            None => (),
            Some(token) => {
                return SyntaxTreeParseResult::Err(SyntaxTreeParseError::new(
                    token.span,
                    SyntaxTreeParseErrorKind::ExpectedLineBreak,
                ))
            }
        }

        SyntaxTreeParseResult::Some(token)
    }
}

#[derive(Clone)]
pub enum RuntimeValue {
    Ident(Ident),
    Value(Value),
    ProducerCall {
        ident: Ident,
        asts: Vec<ExpressionTree<RuntimeValue>>,
    },
}

pub type Ident = Spanned<String>;
