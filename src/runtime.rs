use std::{
    alloc::Layout,
    collections::{HashMap, VecDeque},
    fmt::Display,
    io::Write,
    process::Command,
    ptr::NonNull,
};

use thiserror::Error;

use crate::{
    interpreter::{run, SpannedKind},
    syntax_tree::SyntaxTree,
    value::{IntoValueErrorKind, RuntimeTryFrom, Value, ValueKind},
};

pub type RuntimeError = SpannedKind<RuntimeErrorKind>;

#[derive(Debug, Error)]
pub enum RuntimeErrorKind {
    #[error("Expected a bool expression found {0}")]
    ExpectedBoolFound(String),
    #[error("{0}")]
    IntoValueError(IntoValueErrorKind),
    #[error("{0}")]
    ConsumerCallError(ConsumerCallError),
    #[error("{0}")]
    IgnoredProducerCallError(ProducerCallError),
    #[error("Expected an array")]
    ExpectedArray,
}

#[derive(Error, Debug)]
pub enum ProducerCallError {
    #[error("Incorrect argument count, expected {expected}, found {found}")]
    IncorrectArgCount { expected: ArgCount, found: usize },
    #[error("Unknown producer")]
    UnknownProducer,
}

#[derive(Error, Debug)]
pub enum ConsumerCallError {
    #[error("Incorrect argument count, expected {expected}, found {found}")]
    IncorrectArgCount { expected: ArgCount, found: usize },
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

struct ScopedMap<T> {
    map: HashMap<String, T>,
    scope_idxs: VecDeque<usize>,
}

impl<T> ScopedMap<T> {
    fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity(cap),
            scope_idxs: VecDeque::new(),
        }
    }

    fn insert(&mut self, key: String, value: T) {
        self.map.insert(key, value);
    }

    fn get(&self, key: &String) -> Option<&T> {
        self.map.get(key)
    }

    fn scope_up(&mut self) {
        self.scope_idxs.push_back(self.map.len())
    }

    fn scope_down(&mut self) {
        let Some(idx) = self.scope_idxs.pop_back() else {
            return;
        };

        self.map = self.map.drain().take(idx).collect();
    }
}

impl<T, const N: usize> From<[(String, T); N]> for ScopedMap<T> {
    fn from(value: [(String, T); N]) -> Self {
        Self {
            map: HashMap::from(value),
            scope_idxs: VecDeque::new(),
        }
    }
}

pub struct Runtime {
    variables: ScopedMap<ValueKind>,
    producers: ScopedMap<Producer>,
    consumers: ScopedMap<Consumer>,
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
            return Err(ProducerCallError::IncorrectArgCount {
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
            return Err(ConsumerCallError::IncorrectArgCount {
                expected: consumer.arg_count,
                found: args.len(),
            });
        }

        Ok((consumer.fun)(args))
    }

    fn scope_up(&mut self) {
        self.variables.scope_up();
        self.consumers.scope_up();
        self.producers.scope_up();
    }

    fn scope_down(&mut self) {
        self.variables.scope_down();
        self.consumers.scope_down();
        self.producers.scope_down();
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            variables: ScopedMap::with_capacity(100),
            producers: ScopedMap::from([
                (
                    "readln".to_owned(),
                    Producer::new(ArgCount::Exact(0), |_| {
                        let mut string = String::new();
                        if std::io::stdin().read_line(&mut string).is_err() {
                            return ValueKind::Error {
                                ty: "ReadLnError".to_owned(),
                                message: "Error while reading from standard input.".to_owned(),
                            };
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
                                Err(err) => ValueKind::Error {
                                    ty: "IntegerParseError".to_owned(),
                                    message: err.to_string(),
                                },
                            },
                            _ => ValueKind::Error {
                                ty: "IntegerParseError".to_owned(),
                                message: format!("Cannot convert {} to int", value.type_name()),
                            },
                        }
                    }),
                ),
                (
                    "cmd".to_owned(),
                    Producer::new(ArgCount::AtLeast(1), |args| {
                        let mut iter = args.into_iter();
                        let value = iter.next().unwrap();
                        let name = match value {
                            ValueKind::Error { .. } => {
                                return ValueKind::Error {
                                    ty: "CmdError".to_owned(),
                                    message: format!(
                                        "Expected a non error value found ({})",
                                        value
                                    ),
                                }
                            }
                            value => value.to_string(),
                        };

                        let mut command = Command::new(name);
                        for value in iter {
                            command.arg(match value {
                                ValueKind::Error { .. } => {
                                    return ValueKind::Error {
                                        ty: "CmdError".to_owned(),
                                        message: format!(
                                            "Expected a non error value found ({})",
                                            value
                                        ),
                                    }
                                }
                                value => value.to_string(),
                            });
                        }

                        match command.output() {
                            Ok(output) => match String::from_utf8(output.stdout) {
                                Ok(out) => ValueKind::String(out),
                                Err(err) => {
                                    return ValueKind::Error {
                                        ty: "CmdError".to_owned(),
                                        message: err.to_string(),
                                    }
                                }
                            },
                            Err(err) => {
                                return ValueKind::Error {
                                    ty: "CmdError".to_owned(),
                                    message: err.to_string(),
                                }
                            }
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
            consumers: ScopedMap::from([
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
    trees: I,
) -> Result<(), RuntimeError> {
    enum FlowControl {
        Continue,
        End(Option<ValueKind>),
    }
    fn evaluate_token(
        runtime: &mut Runtime,
        token: SyntaxTree,
    ) -> Result<FlowControl, RuntimeError> {
        match token {
            SyntaxTree::VariableInit { ident, tree } => {
                runtime.init_variable(
                    ident.kind,
                    Value::runtime_try_from(tree, &runtime)
                        .map_err(|err| err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind)))?
                        .kind,
                );

                Ok(FlowControl::Continue)
            }
            SyntaxTree::ConsumerCall { ident, trees } => {
                runtime
                    .call_consumer(
                        &ident,
                        Vec::<ValueKind>::runtime_try_from(trees, &runtime).map_err(|err| {
                            err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind))
                        })?,
                    )
                    .map_err(|err| {
                        RuntimeError::new(ident.span, RuntimeErrorKind::ConsumerCallError(err))
                    })?;

                Ok(FlowControl::Continue)
            }
            SyntaxTree::For {
                ident,
                array,
                trees,
            } => {
                let array = match Value::runtime_try_from(array, &runtime)
                    .map_err(|err| err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind)))?
                {
                    Value {
                        kind: ValueKind::Array(array),
                        ..
                    } => array,
                    Value { span, .. } => {
                        return Err(RuntimeError::new(span, RuntimeErrorKind::ExpectedArray))
                    }
                };

                for element in array {
                    runtime.scope_up();
                    runtime.init_variable(ident.clone(), element);
                    for tree in trees.clone() {
                        match evaluate_token(runtime, tree)? {
                            FlowControl::Continue => (),
                            FlowControl::End(value) => return Ok(FlowControl::End(value)),
                        }
                    }
                    runtime.scope_down();
                }

                Ok(FlowControl::Continue)
            }
            SyntaxTree::IgnoredProducerCall { ident, trees } => {
                runtime
                    .call_producer(
                        &ident,
                        Vec::<ValueKind>::runtime_try_from(trees, &runtime).map_err(|err| {
                            err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind))
                        })?,
                    )
                    .map_err(|err| {
                        RuntimeError::new(
                            ident.span,
                            RuntimeErrorKind::IgnoredProducerCallError(err),
                        )
                    })?;

                Ok(FlowControl::Continue)
            }
            SyntaxTree::If {
                tree,
                trees: tokens,
            } => {
                let value = Value::runtime_try_from(tree, &runtime)
                    .map_err(|err| err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind)))?;

                if match value.kind {
                    ValueKind::Bool(bool) => bool,
                    _ => {
                        return Err(RuntimeError::new(
                            value.span.clone(),
                            RuntimeErrorKind::ExpectedBoolFound(value.type_name().to_owned()),
                        ))
                    }
                } {
                    runtime.scope_up();
                    for token in tokens {
                        match evaluate_token(runtime, token)? {
                            FlowControl::Continue => (),
                            FlowControl::End(value) => return Ok(FlowControl::End(value)),
                        }
                    }
                    runtime.scope_down();
                }

                Ok(FlowControl::Continue)
            }
            SyntaxTree::End { tree } => {
                let value = match tree {
                    Some(tree) => Value::runtime_try_from(tree, &runtime).map_err(|err| {
                        err.map_kind(|kind| RuntimeErrorKind::IntoValueError(kind))
                    })?,
                    None => return Ok(FlowControl::End(None)),
                };

                Ok(FlowControl::End(Some(value.kind)))
            }
        }
    }

    for token in trees {
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
