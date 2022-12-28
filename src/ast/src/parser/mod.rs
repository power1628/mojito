use crate::error::Error;
#[allow(unused_imports)]
use pest::Parser as P;

#[derive(Parser)]
#[grammar = "parser/cypher.pest"]
pub struct Parser;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub mod expr;
pub mod pattern;
