use std::{collections::HashMap, fmt::Display, io::Write, process::Command};

use thiserror::Error;

use crate::{
    expression_tree::{EvaluationErrorKind, ValueKind},
    interpreter::Spanned,
    syntax_tree::SyntaxTree,
};

pub type RuntimeError = Spanned<RuntimeErrorKind>;

#[derive(Debug, Error)]
pub enum RuntimeErrorKind {
    #[error("Expected a bool expression found {0}")]
    ExpectedBoolFound(String),
    #[error("{0}")]
    EvaluationError(EvaluationErrorKind),
    #[error("{0}")]
    ConsumerCallError(ConsumerCallError),
    #[error("{0}")]
    IgnoredProducerCallError(ProducerCallError),
    #[error("Expected a string")]
    ExpectedString,
}

#[derive(Error, Debug)]
pub enum ProducerCallError {
    #[error("Incorrect parameter count, expected {expected}, found {found}")]
    IncorrectParamCount { expected: ArgCount, found: usize },
    #[error("Unknown producer")]
    UnknownProducer,
}

#[derive(Error, Debug)]
pub enum ConsumerCallError {
    #[error("Incorrect parameter count, expected {expected}, found {found}")]
    IncorrectParamCount { expected: ArgCount, found: usize },
    #[error("Unknown consumer")]
    UnknownConsumer,
}

#[derive(Debug, Clone, Copy)]
pub enum ArgCount {
    Any,
    Exact(usize),
    AtLeast(usize),
}

impl Display for ArgCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgCount::Any => write!(f, "any count"),
            ArgCount::Exact(count) => write!(f, "exactly {}", count),
            ArgCount::AtLeast(count) => write!(f, "at least {}", count),
        }
    }
}

impl ArgCount {
    fn satisfies(&self, count: usize) -> bool {
        match self {
            ArgCount::Any => true,
            ArgCount::Exact(arg_count) => *arg_count == count,
            ArgCount::AtLeast(arg_count) => *arg_count <= count,
        }
    }
}

pub struct Producer {
    arg_count: ArgCount,
    fun: fn(Vec<ValueKind>) -> ValueKind,
}

impl Producer {
    fn new(arg_count: ArgCount, fun: fn(Vec<ValueKind>) -> ValueKind) -> Self {
        Self { arg_count, fun }
    }
}

pub struct Consumer {
    arg_count: ArgCount,
    fun: fn(Vec<ValueKind>),
}

impl Consumer {
    fn new(arg_count: ArgCount, fun: fn(Vec<ValueKind>)) -> Self {
        Self { arg_count, fun }
    }
}

pub struct Runtime {
    variables: HashMap<String, ValueKind>,
    producers: HashMap<String, Producer>,
    consumers: HashMap<String, Consumer>,
}

impl Runtime {
    fn init_variable(&mut self, ident: String, value: ValueKind) {
        self.variables.insert(ident, value);
    }

    pub fn get_variable(&self, ident: &String) -> Option<ValueKind> {
        self.variables.get(ident).cloned()
    }

    pub fn call_producer(
        &self,
        ident: &String,
        args: Vec<ValueKind>,
    ) -> Result<ValueKind, ProducerCallError> {
        let Some(producer) = self.producers.get(ident) else {
            return Err(ProducerCallError::UnknownProducer);
        };

        if !producer.arg_count.satisfies(args.len()) {
            return Err(ProducerCallError::IncorrectParamCount {
                expected: producer.arg_count,
                found: args.len(),
            });
        }

        Ok((producer.fun)(args))
    }

    pub fn call_consumer(
        &self,
        ident: &String,
        args: Vec<ValueKind>,
    ) -> Result<(), ConsumerCallError> {
        let Some(consumer) = self.consumers.get(ident) else {
            return Err(ConsumerCallError::UnknownConsumer);
        };

        if !consumer.arg_count.satisfies(args.len()) {
            return Err(ConsumerCallError::IncorrectParamCount {
                expected: consumer.arg_count,
                found: args.len(),
            });
        }

        Ok((consumer.fun)(args))
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            variables: HashMap::new(),
            producers: HashMap::from([
                (
                    "readln".to_owned(),
                    Producer::new(ArgCount::Exact(0), |_| {
                        let mut string = String::new();
                        if std::io::stdin().read_line(&mut string).is_err() {
                            return ValueKind::Error(
                                "Error while reading from standard input.".to_owned(),
                            );
                        }

                        ValueKind::String(string[..(string.len() - 2)].to_owned())
                    }),
                ),
                (
                    "int".to_owned(),
                    Producer::new(ArgCount::Exact(1), |args| {
                        let value = args.into_iter().next().unwrap();
                        match value {
                            ValueKind::Integer(integer) => ValueKind::Integer(integer),
                            ValueKind::String(str) => match str.parse::<i32>() {
                                Ok(integer) => ValueKind::Integer(integer),
                                Err(err) => ValueKind::Error(err.to_string()),
                            },
                            _ => ValueKind::Error(format!("Cannot convert {} to int", value)),
                        }
                    }),
                ),
                (
                    "cmd".to_owned(),
                    Producer::new(ArgCount::AtLeast(1), |args| {
                        let mut iter = args.into_iter();
                        let name = match iter.next().unwrap() {
                            ValueKind::String(name) => name,
                            value => {
                                return ValueKind::Error(format!(
                                    "Expected a string found {}",
                                    value
                                ))
                            }
                        };

                        let mut command = Command::new(name);
                        for value in iter {
                            command.arg(match value {
                                ValueKind::String(name) => name,
                                value => {
                                    return ValueKind::Error(format!(
                                        "Expected a string found {}",
                                        value
                                    ))
                                }
                            });
                        }

                        match command.output() {
                            Ok(output) => match String::from_utf8(output.stdout) {
                                Ok(out) => ValueKind::String(out),
                                Err(err) => ValueKind::Error(err.to_string()),
                            },
                            Err(err) => ValueKind::Error(err.to_string()),
                        }
                    }),
                ),
                (
                    "typeof".to_owned(),
                    Producer::new(ArgCount::Exact(1), |args| {
                        ValueKind::String(args.into_iter().next().unwrap().type_name().to_owned())
                    }),
                ),
            ]),
            consumers: HashMap::from([
                (
                    "writeln".to_owned(),
                    Consumer::new(ArgCount::Any, |params| {
                        for value in params {
                            println!("{}", value);
                        }

                        std::io::stdout().flush().unwrap();
                    }),
                ),
                (
                    "write".to_owned(),
                    Consumer::new(ArgCount::Any, |params| {
                        for value in params {
                            print!("{}", value);
                        }

                        std::io::stdout().flush().unwrap();
                    }),
                ),
            ]),
        }
    }
}

pub fn evaluate<I: IntoIterator<Item = SyntaxTree>>(
    mut runtime: Runtime,
    tokens: I,
) -> Result<(), RuntimeError> {
    enum FlowControl {
        Continue,
        End(Option<String>),
    }
    fn evaluate_token(
        runtime: &mut Runtime,
        token: SyntaxTree,
    ) -> Result<FlowControl, RuntimeError> {
        match token {
            SyntaxTree::VariableInit { ident, tree } => {
                runtime.init_variable(
                    ident.kind,
                    tree.evaluate(&runtime)
                        .map_err(|err| {
                            err.map_kind(|kind| RuntimeErrorKind::EvaluationError(kind))
                        })?
                        .kind,
                );

                Ok(FlowControl::Continue)
            }
            SyntaxTree::ConsumerCall { ident, trees: tree } => {
                let mut args = Vec::with_capacity(tree.len());
                let (start, mut end) = (ident.span.start, ident.span.end);
                for ast in tree {
                    let value = ast.evaluate(&runtime).map_err(|err| {
                        err.map_kind(|kind| RuntimeErrorKind::EvaluationError(kind))
                    })?;

                    args.push(value.kind);
                    end = value.span.end;
                }

                runtime.call_consumer(&ident, args).map_err(|err| {
                    RuntimeError::new(start..end, RuntimeErrorKind::ConsumerCallError(err))
                })?;

                Ok(FlowControl::Continue)
            }
            SyntaxTree::Loop { ident, tokens } => todo!(),
            SyntaxTree::IgnoredProducerCall { ident, trees: tree } => {
                let mut args = Vec::with_capacity(tree.len());
                let (start, mut end) = (ident.span.start, ident.span.end);
                for ast in tree {
                    let value = ast.evaluate(&runtime).map_err(|err| {
                        err.map_kind(|kind| RuntimeErrorKind::EvaluationError(kind))
                    })?;

                    args.push(value.kind);
                    end = value.span.end;
                }

                runtime.call_producer(&ident, args).map_err(|err| {
                    RuntimeError::new(start..end, RuntimeErrorKind::IgnoredProducerCallError(err))
                })?;

                Ok(FlowControl::Continue)
            }
            SyntaxTree::If { tree, tokens } => {
                let value = tree
                    .evaluate(&runtime)
                    .map_err(|err| err.map_kind(|kind| RuntimeErrorKind::EvaluationError(kind)))?;

                if match value.kind {
                    ValueKind::Bool(bool) => bool,
                    _ => {
                        return Err(RuntimeError::new(
                            value.span.clone(),
                            RuntimeErrorKind::ExpectedBoolFound(value.type_name().to_owned()),
                        ))
                    }
                } {
                    for token in tokens {
                        match evaluate_token(runtime, token)? {
                            FlowControl::Continue => (),
                            FlowControl::End(output) => return Ok(FlowControl::End(output)),
                        }
                    }
                }

                Ok(FlowControl::Continue)
            }
            SyntaxTree::End { tree } => {
                let value = match tree {
                    Some(ast) => ast.evaluate(&runtime).map_err(|err| {
                        err.map_kind(|kind| RuntimeErrorKind::EvaluationError(kind))
                    })?,
                    None => return Ok(FlowControl::End(None)),
                };

                let ValueKind::String(string) = value.kind else {
                    return Err(RuntimeError::new(value.span, RuntimeErrorKind::ExpectedString));
                };

                Ok(FlowControl::End(Some(string)))
            }
        }
    }

    for token in tokens {
        match evaluate_token(&mut runtime, token)? {
            FlowControl::Continue => (),
            FlowControl::End(string) => {
                let Some(string) = string else {
                    break;
                };

                println!("{}", string);
                break;
            }
        }
    }

    Ok(())
}
