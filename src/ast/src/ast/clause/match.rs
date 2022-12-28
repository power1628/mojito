use crate::ast::pattern::{NodePattern, RelationshipPattern};

pub struct MatchClause {
    is_optional: bool,
    node_pattern: Vec<NodePattern>,
    edge_pattern: Vec<RelationshipPattern>,
}
