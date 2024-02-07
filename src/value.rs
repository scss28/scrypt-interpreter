use std::fmt::Display;

use thiserror::Error;

use crate::{
    expression_tree::{EvaluationError, ExpressionTree},
    interpreter::{Spanned, SpannedKind},
    runtime::{ProducerCallError, Runtime},
};

pub type Value = SpannedKind<ValueKind>;

#[derive(Debug, PartialEq, Clone)]
pub enum ValueKind {
    Integer(i32),
    String(String),
    Bool(bool),
    Array(Vec<ValueKind>),
    Error { ty: String, message: String },
}

impl ValueKind {
    pub fn type_name(&self) -> &'static str {
        match self {
            ValueKind::Integer(_) => "integer",
            ValueKind::String(_) => "string",
            ValueKind::Error { .. } => "error",
            ValueKind::Bool(_) => "bool",
            ValueKind::Array(_) => "array",
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Integer(integer) => write!(f, "{}", integer),
            ValueKind::String(string) => write!(f, "{}", string),
            ValueKind::Error { ty, message } => write!(f, "{}: {}", ty, message),
            ValueKind::Bool(bool) => write!(f, "{}", bool),
            ValueKind::Array(array) => {
                let mut iter = array.iter();
                match iter.next() {
                    Some(element) => write!(f, "[{}", element)?,
                    None => return write!(f, "[]"),
                }

                for element in iter {
                    write!(f, ", {}", element)?;
                }

                write!(f, "]")
            }
        }
    }
}

pub type RuntimeArray = SpannedKind<RuntimeArrayKind>;

#[derive(Clone)]
pub enum RuntimeArrayKind {
    Elements(Vec<ExpressionTree<RuntimeValue>>),
    Range {
        left: Box<ExpressionTree<RuntimeValue>>,
        right: Box<ExpressionTree<RuntimeValue>>,
    },
}

#[derive(Clone)]
pub enum RuntimeValue {
    Ident(Ident),
    Value(Value),
    Array(RuntimeArray),
    ProducerCall {
        ident: Ident,
        trees: Vec<ExpressionTree<RuntimeValue>>,
    },
}

impl Spanned for RuntimeValue {
    fn span(&self) -> std::ops::Range<usize> {
        match self {
            RuntimeValue::Ident(ident) => ident.span(),
            RuntimeValue::Value(value) => value.span(),
            RuntimeValue::Array(array) => array.span(),
            RuntimeValue::ProducerCall { ident, trees } => {
                ident.span.start
                    ..trees
                        .last()
                        .map(|tree| tree.span().end)
                        .unwrap_or(ident.span().end)
            }
        }
    }
}

pub type IntoValueError = SpannedKind<IntoValueErrorKind>;

#[derive(Debug, Error)]
pub enum IntoValueErrorKind {
    #[error("Uninitialized variable \"{0}\"")]
    UnitializedVariable(String),
    #[error("{0}")]
    ProducerCallError(ProducerCallError),
    #[error("{0}")]
    EvaluationError(EvaluationError),
    #[error("Expected an integer in a range array")]
    ExpectedIntegerInRange,
}

pub trait RuntimeTryFrom<T, E>: Sized {
    fn runtime_try_from(value: T, runtime: &Runtime) -> Result<Self, E>;
}

pub trait RuntimeTryInto<T, E> {
    fn runtime_try_into(self, runtime: &Runtime) -> Result<T, E>;
}

impl<T: RuntimeTryFrom<V, E>, E, V> RuntimeTryInto<T, E> for V {
    fn runtime_try_into(self, runtime: &Runtime) -> Result<T, E> {
        T::runtime_try_from(self, runtime)
    }
}

impl RuntimeTryFrom<RuntimeValue, IntoValueError> for Value {
    fn runtime_try_from(value: RuntimeValue, runtime: &Runtime) -> Result<Value, IntoValueError> {
        match value {
            RuntimeValue::Ident(ident) => {
                let kind = runtime.get_variable(&ident).ok_or(IntoValueError::new(
                    ident.span.clone(),
                    IntoValueErrorKind::UnitializedVariable(ident.kind),
                ))?;

                Ok(Value::new(ident.span, kind))
            }
            RuntimeValue::Value(value) => Ok(value),
            RuntimeValue::ProducerCall { ident, trees } => {
                let mut args = Vec::with_capacity(trees.len());
                let (start, mut end) = (ident.span.start, ident.span.end);
                for tree in trees {
                    let value = Value::runtime_try_from(tree, runtime)?;
                    args.push(value.kind);
                    end = value.span.end;
                }

                let kind = runtime.call_producer(&ident, args).map_err(|err| {
                    IntoValueError::new(start..end, IntoValueErrorKind::ProducerCallError(err))
                })?;

                Ok(Value::new(start..end, kind))
            }
            RuntimeValue::Array(array) => match array.kind {
                RuntimeArrayKind::Elements(elements) => Ok(Value::new(
                    array.span,
                    ValueKind::Array(Vec::<ValueKind>::runtime_try_from(elements, runtime)?),
                )),
                RuntimeArrayKind::Range { left, right } => {
                    match (
                        ValueKind::runtime_try_from(*left, runtime)?,
                        ValueKind::runtime_try_from(*right, runtime)?,
                    ) {
                        (ValueKind::Integer(left), ValueKind::Integer(right)) => Ok(Value::new(
                            array.span,
                            ValueKind::Array(
                                (left..=right)
                                    .map(|integer| ValueKind::Integer(integer))
                                    .collect(),
                            ),
                        )),
                        _ => Err(IntoValueError::new(
                            array.span,
                            IntoValueErrorKind::ExpectedIntegerInRange,
                        )),
                    }
                }
            },
        }
    }
}

impl RuntimeTryFrom<ExpressionTree<RuntimeValue>, IntoValueError> for Value {
    fn runtime_try_from(
        value: ExpressionTree<RuntimeValue>,
        runtime: &Runtime,
    ) -> Result<Value, IntoValueError> {
        match value {
            ExpressionTree::Value(runtime_value) => runtime_value.runtime_try_into(runtime),
            ExpressionTree::Expression {
                operator,
                left,
                right,
            } => {
                let left = Value::runtime_try_from(*left, runtime)?;
                let right = Value::runtime_try_from(*right, runtime)?;
                let span = left.span.start..right.span.start;
                match operator.evaluate(left.kind, right.kind) {
                    Ok(kind) => Ok(Value::new(span, kind)),
                    Err(err) => Err(IntoValueError::new(
                        span,
                        IntoValueErrorKind::EvaluationError(err),
                    )),
                }
            }
            ExpressionTree::Enclosed(node) => (*node).runtime_try_into(runtime),
        }
    }
}

impl RuntimeTryFrom<ExpressionTree<RuntimeValue>, IntoValueError> for ValueKind {
    fn runtime_try_from(
        value: ExpressionTree<RuntimeValue>,
        runtime: &Runtime,
    ) -> Result<Self, IntoValueError> {
        Ok(Value::runtime_try_from(value, runtime)?.kind)
    }
}

impl RuntimeTryFrom<Vec<ExpressionTree<RuntimeValue>>, IntoValueError> for Vec<Value> {
    fn runtime_try_from(
        value: Vec<ExpressionTree<RuntimeValue>>,
        runtime: &Runtime,
    ) -> Result<Vec<Value>, IntoValueError> {
        value
            .into_iter()
            .map(|runtime_value| Value::runtime_try_from(runtime_value, runtime))
            .collect::<Result<_, _>>()
    }
}

impl RuntimeTryFrom<Vec<ExpressionTree<RuntimeValue>>, IntoValueError> for Vec<ValueKind> {
    fn runtime_try_from(
        value: Vec<ExpressionTree<RuntimeValue>>,
        runtime: &Runtime,
    ) -> Result<Vec<ValueKind>, IntoValueError> {
        value
            .into_iter()
            .map(|runtime_value| {
                Value::runtime_try_from(runtime_value, runtime).map(|value| value.kind)
            })
            .collect::<Result<_, _>>()
    }
}

pub type Ident = SpannedKind<String>;
