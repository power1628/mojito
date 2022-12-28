use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "NOT",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    /// Mathmatic operation
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    /// Comparison
    Gt,
    Lt,
    GtEq,
    LtEq,
    Eq,
    NotEq,
    /// Logic operation
    And,
    Or,
    Xor,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Gt => ">",
            BinaryOperator::Lt => "<",
            BinaryOperator::GtEq => ">=",
            BinaryOperator::LtEq => "<=",
            BinaryOperator::Eq => "=",
            BinaryOperator::NotEq => "<>",
            BinaryOperator::And => "AND",
            BinaryOperator::Or => "OR",
            BinaryOperator::Xor => "XOR",
        })
    }
}
