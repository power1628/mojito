/// Node/Edge Pattern
use crate::ast::expr::Literal;

/// Properties with node or edge
#[derive(Debug)]
pub enum Properties {
    MapLiteral(Literal),
}
#[derive(Debug)]
pub struct NodePattern {
    pub variable: Option<String>,
    pub labels: Option<Vec<String>>,
    pub properties: Option<Properties>,
}

#[derive(Debug)]
pub enum RelationshipDirection {
    Left,
    Right,
    Bidirectional,
    Undirected,
}

/// "[" ~ Variable? ~ RelationshipTypes? ~ Properties? ~ "]"
#[derive(Debug)]
pub struct RelationshipDetail {
    pub variable: Option<String>,
    pub types: Option<Vec<String>>,
    pub properties: Option<Properties>,
}

/// "<-" ~ RelationshipDetail ~ "-"
#[derive(Debug)]
pub struct RelationshipPattern {
    pub direction: RelationshipDirection,
    pub detail: RelationshipDetail,
}

/// PatternElementChain = { RelationshipPattern ~ NodePattern }
#[derive(Debug)]
pub struct PatternElementChain {
    pub relation: RelationshipPattern,
    pub node: NodePattern,
}

#[derive(Debug)]
pub struct RelationshipsPattern {
    pub node: NodePattern,
    pub chain: Vec<PatternElementChain>,
}

#[derive(Debug)]
pub struct PatternElement {
    pub node: NodePattern,
    pub chain: Option<Vec<PatternElementChain>>,
}

#[derive(Debug)]
pub struct PatternPart {
    pub variable: Option<String>,
    pub element: PatternElement,
}

#[derive(Debug)]
pub struct Pattern {
    pub part: Vec<PatternPart>,
}
