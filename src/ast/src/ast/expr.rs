use std::fmt::Display;

use super::operator::{BinaryOperator, UnaryOperator};

#[derive(Debug)]
pub enum Literal {
    Boolean(bool),
    Integer(u64),
    Double(f64),
    String(String),
    Map(Vec<(String, Box<Expr>)>),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Boolean(value) => f.write_fmt(format_args!("{}", value)),
            Literal::Integer(value) => f.write_fmt(format_args!("{}", value)),
            Literal::Double(value) => f.write_fmt(format_args!("{}", value)),
            Literal::String(value) => f.write_fmt(format_args!("{}", value)),
            Literal::Map(entries) => {
                let items = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();
                f.write_fmt(format_args!("{{{}}}", items.join(", ")))
            }
            Literal::Null => f.write_str("NULL"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    /// Literal
    Literal(Literal),

    /// Binary op
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },

    /// Unary op
    UnaryOp { op: UnaryOperator, expr: Box<Expr> },

    /// Variable
    Variable(String),
}

impl Expr {
    pub(crate) fn new_literal(literal: Literal) -> Box<Expr> {
        Box::new(Expr::Literal(literal))
    }

    pub(crate) fn new_binary_op(op: BinaryOperator, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::BinaryOp {
            left: lhs,
            op,
            right: rhs,
        })
    }

    pub(crate) fn new_unary_op(op: UnaryOperator, expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::UnaryOp { op, expr })
    }

    pub(crate) fn new_variable(var: String) -> Box<Expr> {
        Box::new(Expr::Variable(var))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::BinaryOp { left, op, right } => match op {
                BinaryOperator::Plus => f.write_fmt(format_args!("{} + {}", left, right)),
                BinaryOperator::Minus => f.write_fmt(format_args!("{} - {}", left, right)),
                BinaryOperator::Multiply => f.write_fmt(format_args!("{} * {}", left, right)),
                BinaryOperator::Divide => f.write_fmt(format_args!("{} / {}", left, right)),
                BinaryOperator::Modulo => f.write_fmt(format_args!("{} % {}", left, right)),
                BinaryOperator::Gt => f.write_fmt(format_args!("{} > {}", left, right)),
                BinaryOperator::Lt => f.write_fmt(format_args!("{} < {}", left, right)),
                BinaryOperator::GtEq => f.write_fmt(format_args!("{} >= {}", left, right)),
                BinaryOperator::LtEq => f.write_fmt(format_args!("{} <= {}", left, right)),
                BinaryOperator::Eq => f.write_fmt(format_args!("{} = {}", left, right)),
                BinaryOperator::NotEq => f.write_fmt(format_args!("{} <> {}", left, right)),
                BinaryOperator::And => f.write_fmt(format_args!("{} AND {}", left, right)),
                BinaryOperator::Or => f.write_fmt(format_args!("{} OR {}", left, right)),
                BinaryOperator::Xor => f.write_fmt(format_args!("{} XOR {}", left, right)),
            },
            Expr::Literal(lit) => f.write_fmt(format_args!("{}", lit)),
            Expr::UnaryOp { op, expr } => match op {
                UnaryOperator::Minus => f.write_fmt(format_args!("-{}", expr)),
                UnaryOperator::Plus => f.write_fmt(format_args!("+{}", expr)),
                UnaryOperator::Not => f.write_fmt(format_args!("NOT {}", expr)),
            },
            Expr::Variable(var) => f.write_str(var),
        }
    }
}
