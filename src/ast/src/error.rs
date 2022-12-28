use crate::parser::Rule;
use thiserror::Error;

/// A list of possible errors returned by pest-cypher
#[derive(Error, Debug)]
pub enum Error {
    #[error("pest parsing error")]
    PestError(#[from] pest::error::Error<Rule>),
}
