/// Parse Expression
use crate::ast::expr::Expr;
use crate::ast::expr::Literal;
use crate::ast::operator::BinaryOperator;
use crate::ast::operator::UnaryOperator;
use crate::error::Error;
#[allow(unused_imports)]
use crate::parser::Parser;
use crate::parser::Result;
use crate::parser::Rule;
use pest::iterators::Pair;
use pest::pratt_parser::PrattParser;
#[allow(unused_imports)]
use pest::Parser as P;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(Rule::Or, Left) | Op::infix(Rule::Xor, Left) | Op::infix(Rule::And, Left))
            .op(Op::infix(Rule::Eq, Left) | Op::infix(Rule::NotEq, Left) | Op::infix(Rule::Gt, Left)
             | Op::infix(Rule::Lt, Left) |   Op::infix(Rule::GtEq, Left) | Op::infix(Rule::LtEq, Left))
            .op(Op::infix(Rule::Plus, Left)|   Op::infix(Rule::Minus, Left))
            .op(Op::infix(Rule::Multiply, Left)| Op::infix(Rule::Divide, Left) | Op::infix(Rule::Modulo, Left))
            .op(Op::infix(Rule::PowerOf, Right))
            .op(Op::prefix(Rule::PositivePrefix) | Op::prefix(Rule::NegativePrefix))
    };
}

pub fn parse_expr(pair: Pair<'_, Rule>) -> Result<Box<Expr>> {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::ExpressionPrimary => parse_expression_primary(primary),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::PositivePrefix => Ok(Expr::new_unary_op(UnaryOperator::Plus, rhs?)),
            Rule::NegativePrefix => Ok(Expr::new_unary_op(UnaryOperator::Minus, rhs?)),
            Rule::NotPrefix => Ok(Expr::new_unary_op(UnaryOperator::Not, rhs?)),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Or => Ok(Expr::new_binary_op(BinaryOperator::Or, lhs?, rhs?)),
            Rule::Xor => Ok(Expr::new_binary_op(BinaryOperator::Xor, lhs?, rhs?)),
            Rule::And => Ok(Expr::new_binary_op(BinaryOperator::And, lhs?, rhs?)),
            Rule::Eq => Ok(Expr::new_binary_op(BinaryOperator::Eq, lhs?, rhs?)),
            Rule::NotEq => Ok(Expr::new_binary_op(BinaryOperator::NotEq, lhs?, rhs?)),
            Rule::Gt => Ok(Expr::new_binary_op(BinaryOperator::Gt, lhs?, rhs?)),
            Rule::Lt => Ok(Expr::new_binary_op(BinaryOperator::Lt, lhs?, rhs?)),
            Rule::GtEq => Ok(Expr::new_binary_op(BinaryOperator::GtEq, lhs?, rhs?)),
            Rule::LtEq => Ok(Expr::new_binary_op(BinaryOperator::LtEq, lhs?, rhs?)),
            Rule::Plus => Ok(Expr::new_binary_op(BinaryOperator::Plus, lhs?, rhs?)),
            Rule::Minus => Ok(Expr::new_binary_op(BinaryOperator::Minus, lhs?, rhs?)),
            Rule::Multiply => Ok(Expr::new_binary_op(BinaryOperator::Multiply, lhs?, rhs?)),
            Rule::Divide => Ok(Expr::new_binary_op(BinaryOperator::Divide, lhs?, rhs?)),
            Rule::Modulo => Ok(Expr::new_binary_op(BinaryOperator::Modulo, lhs?, rhs?)),
            _ => unreachable!(),
        })
        .parse(pair.into_inner())
}

fn parse_expression_primary(pair: Pair<'_, Rule>) -> Result<Box<Expr>> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::Atom => parse_atom(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

fn parse_atom(pair: Pair<'_, Rule>) -> Result<Box<Expr>> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::Literal => parse_literal(sub_pair).map(|v| Expr::new_literal(v)),
            Rule::Variable => Ok(Expr::new_variable(parse_variable(sub_pair)?)),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

pub(crate) fn parse_literal(pair: Pair<'_, Rule>) -> Result<Literal> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::StringLiteral => parse_string(sub_pair).map(|v| Literal::String(v)),
            Rule::NumberLiteral => parse_number(sub_pair),
            Rule::BooleanLiteral => {
                if sub_pair.as_str() == "true" {
                    Ok(Literal::Boolean(true))
                } else {
                    Ok(Literal::Boolean(false))
                }
            }
            Rule::MapLiteral => parse_map_literal(sub_pair),
            Rule::NullLiteral => Ok(Literal::Null),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

pub(crate) fn parse_variable(pair: Pair<'_, Rule>) -> Result<String> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::SymbolicName => parse_symbolic_name(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

/// TODO(pgao) EscapedSymbolicName parsing
pub(crate) fn parse_symbolic_name(pair: Pair<'_, Rule>) -> Result<String> {
    Ok(String::from(pair.as_str()))
}

/// Parse MapLiteral
pub(crate) fn parse_map_literal(pair: Pair<'_, Rule>) -> Result<Literal> {
    let mut keys = vec![];
    let mut values = vec![];
    let mut pair_iter = pair.into_inner();
    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::PropertyKeyName => keys.push(String::from(sub_pair.as_str())),
            Rule::Expression => values.push(parse_expr(sub_pair)?),
            _ => unreachable!(),
        };
    }

    let res: Vec<_> = keys.into_iter().zip(values).collect();
    Ok(Literal::Map(res))
}

fn parse_string(pair: Pair<'_, Rule>) -> Result<String> {
    let mut result = String::new();

    for component in pair.into_inner() {
        match component.as_rule() {
            Rule::CharLiteral => result.push_str(component.as_str()),
            Rule::CharEscapeSequence => result.push_str(parse_char_escape_sequence(&component)),
            Rule::UnicodeEscapeSequence => {
                let unicode = format!("\\u{}", parse_unicode_escape_sequence(&component));
                result.push_str(&unicode);
            }
            _ => unreachable!(),
        }
    }
    Ok(result)
}

fn parse_char_escape_sequence<'a>(pair: &'a Pair<'_, Rule>) -> &'a str {
    match pair.as_str() {
        "b" => "\u{0008}",
        "f" => "\u{000C}",
        "n" => "\n",
        "r" => "\r",
        "t" => "\t",
        c => c,
    }
}

// TODO(pgao): parse unicode
fn parse_unicode_escape_sequence<'a>(pair: &'a Pair<'_, Rule>) -> &'a str {
    pair.as_str()
}

/// TODO(pgao): support hex and other format numbers
fn parse_number(pair: Pair<'_, Rule>) -> Result<Literal> {
    let s = pair.as_str();
    if is_integer(s) {
        parse_integer(&pair).map(|v| Literal::Integer(v))
    } else {
        parse_double(&pair).map(|v| Literal::Double(v))
    }
}

fn parse_integer(pair: &Pair<'_, Rule>) -> Result<u64> {
    let span = (pair.as_span().start(), pair.as_span().end());
    match pair.as_str() {
        s if is_hex_literal(s) => u64::from_str_radix(&s[2..], 16).or_else(|_| {
            Err(Error::ParseError {
                span,
                msg: format!("failed to parse hex to u64, str: {:?} .", pair.to_string()),
            })
        }),
        s if is_oct_literal(s) => u64::from_str_radix(&s[2..], 8).or_else(|_| {
            Err(Error::ParseError {
                span,
                msg: format!("failed to parse hex to i64, str: {:?} .", pair.to_string()),
            })
        }),
        s => s.parse().or_else(|_| {
            Err(Error::ParseError {
                span,
                msg: format!(
                    "failed to parse integer to i64, str: {:?} .",
                    pair.to_string()
                ),
            })
        }),
    }
}

fn parse_double(pair: &Pair<'_, Rule>) -> Result<f64> {
    let span = (pair.as_span().start(), pair.as_span().end());
    let s = pair.as_str();
    s.parse().or_else(|_| {
        Err(Error::ParseError {
            span,
            msg: format!(
                "failed to parse double to f64, str: {:?} .",
                pair.to_string()
            ),
        })
    })
}

fn is_hex_literal(s: &str) -> bool {
    s.len() > 2 && (&s[..2] == "0x" || &s[..2] == "0X")
}

fn is_oct_literal(s: &str) -> bool {
    s.len() > 2 && (&s[..2] == "0o" || &s[..2] == "0O")
}

fn is_integer(s: &str) -> bool {
    !s.contains('.') && !s.contains('e') && !s.contains('E')
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_parse_atom() {
        let input = "1";

        let pairs = Parser::parse(Rule::Atom, input).unwrap();
        let res: Vec<Box<Expr>> = pairs.map(|pair| parse_atom(pair).unwrap()).collect();
        let expect = Literal::Integer(1);
        res.iter().map(|v| println!("{:#?}", *v)).count();
    }

    #[test]
    fn test_parse_expr() {
        let input = "111+222/3";
        let pair = Parser::parse(Rule::Expression, input)
            .unwrap()
            .next()
            .unwrap();
        println!("{:#?}", pair);
        let res = parse_expr(pair).unwrap();
        println!("{:#?}", res);
    }

    #[test]
    fn test_parse_map_literal() {
        let input = "{ aa: 'aa', bb: 100 }";
        let mut pairs = Parser::parse(Rule::Expression, input).unwrap();
        let pair = pairs.next().unwrap();
        let res = parse_expr(pair).unwrap();
        println!("{:#?}", res);
    }
}
