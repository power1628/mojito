/// Parse Expression
use crate::parser::Parser;
use crate::parser::Result;
use crate::parser::Rule;
use pest::pratt_parser::PrattParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

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
