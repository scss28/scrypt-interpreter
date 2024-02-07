use std::{fmt::Debug, ops::Range};

use thiserror::Error;

use crate::{interpreter::Spanned, value::ValueKind};

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

impl<T: Spanned> ExpressionTree<T> {
    pub fn span(&self) -> Range<usize> {
        match self {
            ExpressionTree::Value(spanned) => spanned.span(),
            ExpressionTree::Expression { left, right, .. } => left.span().start..right.span().end,
            ExpressionTree::Enclosed(node) => node.span(),
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
    In,
}

#[derive(Error, Debug)]
pub enum EvaluationError {
    #[error("Cannot multiply a string by a negative value")]
    MultiplyingStringByNegative,
    #[error("{0}")]
    UnsupportedOperation(String),
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
            Operator::In => 0,
        }
    }

    pub fn evaluate(
        &self,
        left: ValueKind,
        right: ValueKind,
    ) -> Result<ValueKind, EvaluationError> {
        let left_type = left.type_name();
        let right_type = right.type_name();
        let kind = match self {
            Operator::Add => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Integer(left + right)
                }
                (ValueKind::String(left), right) => ValueKind::String(format!("{}{}", left, right)),
                (left, ValueKind::String(right)) => ValueKind::String(format!("{}{}", left, right)),
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot add {} to {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::Sub => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Integer(left - right)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot subtract {} from {}",
                        right_type, left_type
                    )))
                }
            },
            Operator::Mul => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Integer(left * right)
                }
                (ValueKind::Integer(integer), ValueKind::String(string))
                | (ValueKind::String(string), ValueKind::Integer(integer)) => {
                    if integer < 0 {
                        return Err(EvaluationError::MultiplyingStringByNegative);
                    }

                    let mut output = String::with_capacity(integer as usize * string.len());
                    for _ in 0..integer {
                        output += &string;
                    }

                    ValueKind::String(output)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot multiply {} by {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::Div => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Integer(left / right)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot divide {} by {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::Mod => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Integer(left % right)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot modulo {} by {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::GreaterThan => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Bool(left > right)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot compare {} by {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::LessThan => match (left, right) {
                (ValueKind::Integer(left), ValueKind::Integer(right)) => {
                    ValueKind::Bool(left < right)
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot compare {} by {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::And => match (left, right) {
                (ValueKind::Bool(left), ValueKind::Bool(right)) => ValueKind::Bool(left && right),
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot evaluate {} & {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::Or => match (left, right) {
                (ValueKind::Bool(left), ValueKind::Bool(right)) => ValueKind::Bool(left || right),
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot evaluate {} | {}",
                        left_type, right_type
                    )))
                }
            },
            Operator::Eq => ValueKind::Bool(left == right),
            Operator::In => match (left, right) {
                (value, ValueKind::Array(elements)) => {
                    ValueKind::Bool(elements.iter().any(|element| *element == value))
                }
                _ => {
                    return Err(EvaluationError::UnsupportedOperation(format!(
                        "Cannot check if {} is in {}",
                        left_type, right_type
                    )))
                }
            },
        };

        Ok(kind)
    }
}
