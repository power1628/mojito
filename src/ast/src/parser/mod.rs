use crate::error::Error;
use pest::{iterators::Pair, Parser as P};

#[derive(Parser)]
#[grammar = "parser/cypher.pest"]
pub struct Parser;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub mod expr;
