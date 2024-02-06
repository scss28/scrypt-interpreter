use std::{
    fmt::{Debug, Display},
    ops::Range,
};

use thiserror::Error;

use crate::{
    interpreter::Spanned,
    runtime::{ProducerCallError, Runtime},
    syntax_tree::RuntimeValue,
};

#[derive(Clone)]
pub enum ExpressionTree<T> {
    Value(T),
    Expression {
        left: Box<ExpressionTree<T>>,
        operator: Operator,
        right: Box<ExpressionTree<T>>,
    },
    Enclosed(Box<ExpressionTree<T>>),
}

impl<T: Clone> ExpressionTree<T> {
    pub fn new_expression(
        left: ExpressionTree<T>,
        operator: Operator,
        right: ExpressionTree<T>,
    ) -> Self {
        ExpressionTree::Expression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn new_enclosed(node: ExpressionTree<T>) -> Self {
        ExpressionTree::Enclosed(Box::new(node))
    }

    pub fn append(&mut self, operator: Operator, right: ExpressionTree<T>) {
        match self {
            ExpressionTree::Value(_) | ExpressionTree::Enclosed(_) => {
                *self = ExpressionTree::new_expression(self.clone(), operator, right);
            }
            ExpressionTree::Expression {
                operator: self_operator,
                right: self_right,
                ..
            } => {
                if operator.priority() > self_operator.priority() {
                    self_right.append(operator, right);
                } else {
                    *self = ExpressionTree::new_expression(self.clone(), operator, right);
                }
            }
        }
    }
}

pub type EvaluationError = Spanned<EvaluationErrorKind>;

#[derive(Error, Debug)]
pub enum EvaluationErrorKind {
    #[error("Cannot multiply a string by a negative value")]
    MultiplyingStringByNegative,
    #[error("{0}")]
    UnsupportedOperation(String),
    #[error("Uninitialized variable \"{0}\"")]
    UnitializedVariable(String),
    #[error("{0}")]
    ProducerCallError(ProducerCallError),
}

impl ExpressionTree<RuntimeValue> {
    pub fn evaluate(self, runtime: &Runtime) -> Result<Value, EvaluationError> {
        match self {
            ExpressionTree::Value(runtime_value) => match runtime_value {
                RuntimeValue::Ident(ident) => {
                    let kind = runtime.get_variable(&ident).ok_or(EvaluationError::new(
                        ident.span.clone(),
                        EvaluationErrorKind::UnitializedVariable(ident.kind),
                    ))?;

                    Ok(Value::new(ident.span, kind))
                }
                RuntimeValue::Value(value) => Ok(value),
                RuntimeValue::ProducerCall { ident, asts } => {
                    let mut params = Vec::with_capacity(asts.len());
                    let (start, mut end) = (ident.span.start, ident.span.end);
                    for ast in asts {
                        let value = ast.evaluate(runtime)?;
                        params.push(value.kind);
                        end = value.span.end;
                    }

                    let kind = runtime.call_producer(&ident, params).map_err(|err| {
                        EvaluationError::new(
                            start..end,
                            EvaluationErrorKind::ProducerCallError(err),
                        )
                    })?;

                    Ok(Value::new(start..end, kind))
                }
            },
            ExpressionTree::Expression {
                operator,
                left,
                right,
            } => evaluate_expression(left.evaluate(runtime)?, operator, right.evaluate(runtime)?),
            ExpressionTree::Enclosed(node) => node.evaluate(runtime),
        }
    }
}

impl<T: Debug> Debug for ExpressionTree<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionTree::Value(value) => write!(f, "{:?}", value),
            ExpressionTree::Expression {
                operator,
                left,
                right,
            } => write!(f, "{:?} -> [ {:?} | {:?} ]", operator, left, right),
            ExpressionTree::Enclosed(node) => write!(f, "{:?}", node),
        }
    }
}

fn evaluate_expression(
    left: Value,
    operator: Operator,
    right: Value,
) -> Result<Value, EvaluationError> {
    let expression_span = left.span.start..right.span.end;
    let left_type = left.kind.type_name();
    let right_type = right.kind.type_name();
    let kind = match operator {
        Operator::Add => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                ValueKind::Integer(left + right)
            }
            (ValueKind::Range(left), ValueKind::Range(right)) => {
                ValueKind::Range((left.start + right.start)..(left.end + right.end))
            }
            (ValueKind::String(left), right) => ValueKind::String(format!("{}{}", left, right)),
            (left, ValueKind::String(right)) => ValueKind::String(format!("{}{}", left, right)),
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot add {} to {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::Sub => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                ValueKind::Integer(left - right)
            }
            (ValueKind::Range(left), ValueKind::Range(right)) => {
                ValueKind::Range((left.start - right.start)..(left.end - right.end))
            }
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot subtract {} from {}",
                        right_type, left_type
                    )),
                ))
            }
        },
        Operator::Mul => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                ValueKind::Integer(left * right)
            }
            (ValueKind::Integer(integer), ValueKind::String(string))
            | (ValueKind::String(string), ValueKind::Integer(integer)) => {
                if integer < 0 {
                    return Err(EvaluationError::new(
                        expression_span,
                        EvaluationErrorKind::MultiplyingStringByNegative,
                    ));
                }

                let mut output = String::with_capacity(integer as usize * string.len());
                for _ in 0..integer {
                    output += &string;
                }

                ValueKind::String(output)
            }
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot multiply {} by {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::Div => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                ValueKind::Integer(left / right)
            }
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot divide {} by {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::Mod => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                ValueKind::Integer(left % right)
            }
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot modulo {} by {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::GreaterThan => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => ValueKind::Bool(left > right),
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot compare {} by {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::LessThan => match (left.kind, right.kind) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => ValueKind::Bool(left < right),
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot compare {} by {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::And => match (left.kind, right.kind) {
            (ValueKind::Bool(left), ValueKind::Bool(right)) => ValueKind::Bool(left && right),
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot evaluate {} & {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::Or => match (left.kind, right.kind) {
            (ValueKind::Bool(left), ValueKind::Bool(right)) => ValueKind::Bool(left || right),
            _ => {
                return Err(EvaluationError::new(
                    expression_span,
                    EvaluationErrorKind::UnsupportedOperation(format!(
                        "Cannot evaluate {} | {}",
                        left_type, right_type
                    )),
                ))
            }
        },
        Operator::Eq => ValueKind::Bool(left.kind == right.kind),
    };

    Ok(Value::new(expression_span, kind))
}

pub type Value = Spanned<ValueKind>;

#[derive(Debug, PartialEq, Clone)]
pub enum ValueKind {
    Integer(i32),
    String(String),
    Range(Range<i32>),
    Bool(bool),
    Error(String),
}

impl ValueKind {
    pub fn type_name(&self) -> &'static str {
        match self {
            ValueKind::Integer(_) => "integer",
            ValueKind::String(_) => "string",
            ValueKind::Range(_) => "range",
            ValueKind::Error(_) => "error",
            ValueKind::Bool(_) => "bool",
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Integer(integer) => write!(f, "{}", integer),
            ValueKind::String(string) => write!(f, "{}", string),
            ValueKind::Range(range) => write!(f, "{:?}", range),
            ValueKind::Error(message) => write!(f, "error: {}", message),
            ValueKind::Bool(bool) => write!(f, "{}", bool),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    GreaterThan,
    LessThan,
    And,
    Or,
    Eq,
    Mod,
}

impl Operator {
    pub fn priority(&self) -> usize {
        match self {
            Operator::Add => 3,
            Operator::Sub => 3,
            Operator::Mul => 4,
            Operator::Div => 4,
            Operator::Mod => 4,
            Operator::GreaterThan => 2,
            Operator::LessThan => 2,
            Operator::Eq => 1,
            Operator::And => 0,
            Operator::Or => 0,
        }
    }
}
