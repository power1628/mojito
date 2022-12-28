use crate::ast::pattern::NodePattern;
use crate::ast::pattern::Pattern;
use crate::ast::pattern::PatternElement;
use crate::ast::pattern::PatternElementChain;
use crate::ast::pattern::PatternPart;
use crate::ast::pattern::Properties;
use crate::ast::pattern::RelationshipDetail;
use crate::ast::pattern::RelationshipDirection;
use crate::ast::pattern::RelationshipPattern;
#[allow(unused_imports)]
use crate::parser::Parser;
use crate::parser::Result;
use crate::parser::Rule;
use pest::iterators::Pair;
#[allow(unused_imports)]
use pest::Parser as P;

use crate::parser::expr::parse_map_literal;
use crate::parser::expr::parse_variable;

pub fn parse_pattern(pair: Pair<'_, Rule>) -> Result<Pattern> {
    let mut pair_iter = pair.into_inner();

    let mut part = vec![];
    while let Some(sub_pair) = pair_iter.next() {
        part.push(parse_pattern_part(sub_pair)?);
    }

    Ok(Pattern { part })
}

fn parse_pattern_part(pair: Pair<'_, Rule>) -> Result<PatternPart> {
    let sub_pair = pair.into_inner().next().unwrap();
    match sub_pair.as_rule() {
        Rule::NamedPatternPart => parse_named_pattern_part(sub_pair),
        Rule::AnonymousPatternPart => Ok(PatternPart {
            variable: None,
            element: parse_anonmous_pattern_part(sub_pair)?,
        }),
        _ => unreachable!(),
    }
}

/// NamedPatternPart = { Variable ~ "=" ~ AnonymousPatternPart }
fn parse_named_pattern_part(pair: Pair<'_, Rule>) -> Result<PatternPart> {
    let mut pair_iter = pair.into_inner();

    let variable = parse_variable(pair_iter.next().unwrap())?;
    let element = parse_pattern_element(pair_iter.next().unwrap().into_inner().next().unwrap())?;
    Ok(PatternPart {
        variable: Some(variable),
        element,
    })
}

fn parse_anonmous_pattern_part(pair: Pair<'_, Rule>) -> Result<PatternElement> {
    parse_pattern_element(pair.into_inner().next().unwrap())
}

/// PatternElement = {
/// NodePattern  ~ PatternElementChain*
/// | ( "(" ~ PatternElement ~ ")" )
/// }
fn parse_pattern_element(pair: Pair<'_, Rule>) -> Result<PatternElement> {
    let mut pair_iter = pair.into_inner();

    let peek = pair_iter.peek().clone().unwrap();
    match peek.as_rule() {
        // NodePattern  ~ PatternElementChain*
        Rule::NodePattern => {
            let node = parse_node_pattern(pair_iter.next().unwrap())?;
            let mut chain = vec![];
            while let Some(sub_pair) = pair_iter.next() {
                chain.push(parse_pattern_element_chain(sub_pair)?);
            }
            Ok(PatternElement {
                node,
                chain: Some(chain),
            })
        }
        // | ( "(" ~ PatternElement ~ ")" )
        Rule::PatternElement => {
            let sub_pair = pair_iter.next().unwrap();
            parse_pattern_element(sub_pair)
        }
        _ => unreachable!(),
    }
}

fn parse_node_pattern(pair: Pair<'_, Rule>) -> Result<NodePattern> {
    let mut variable: Option<String> = None;
    let mut labels: Option<Vec<String>> = None;
    let mut properties: Option<Properties> = None;

    let mut pair_iter = pair.into_inner();
    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::Variable => variable = Some(parse_variable(sub_pair)?),
            Rule::NodeLabels => labels = Some(parse_node_labels(sub_pair)?),
            Rule::Properties => properties = Some(parse_properties(sub_pair)?),
            _ => unreachable!(),
        };
    }

    let pattern = NodePattern {
        variable,
        labels,
        properties,
    };

    Ok(pattern)
}

/// PatternElementChain = { RelationshipPattern ~ NodePattern }
pub fn parse_pattern_element_chain(pair: Pair<'_, Rule>) -> Result<PatternElementChain> {
    let mut rel: Option<RelationshipPattern> = None;
    let mut node: Option<NodePattern> = None;

    let mut pair_iter = pair.into_inner();
    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::RelationshipPattern => rel = Some(parse_relationship_pattern(sub_pair)?),
            Rule::NodePattern => node = Some(parse_node_pattern(sub_pair)?),
            _ => unimplemented!(),
        }
    }
    Ok(PatternElementChain {
        relation: rel.unwrap(),
        node: node.unwrap(),
    })
}

/// RelationshipPattern = {
///	BidirectionalRelationship
///	| LeftRelationship
///	| RightRelationship
///	| UndirectedRelationship
/// }
pub fn parse_relationship_pattern(pair: Pair<'_, Rule>) -> Result<RelationshipPattern> {
    if let Some(sub_pair) = pair.into_inner().next() {
        let direction = match sub_pair.as_rule() {
            Rule::LeftRelationship => RelationshipDirection::Left,
            Rule::RightRelationship => RelationshipDirection::Right,
            Rule::BidirectionalRelationship => RelationshipDirection::Bidirectional,
            Rule::UndirectedRelationship => RelationshipDirection::Undirected,
            _ => unreachable!(),
        };
        let detail = parse_relationship_inner(sub_pair)?;
        Ok(RelationshipPattern { direction, detail })
    } else {
        unreachable!()
    }
}

pub fn parse_relationship_inner(pair: Pair<'_, Rule>) -> Result<RelationshipDetail> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::RelationshipDetail => parse_relationship_detail(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

/// RelationshipDetail = {
/// 	"[" ~ Variable? ~ RelationshipTypes? ~ /*RangeLiteral? ~ */ Properties? ~
///	"]"
/// }
pub fn parse_relationship_detail(pair: Pair<'_, Rule>) -> Result<RelationshipDetail> {
    let mut detail = RelationshipDetail {
        variable: None,
        types: None,
        properties: None,
    };

    let mut pair_iter = pair.into_inner();

    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::Variable => detail.variable = Some(parse_variable(sub_pair)?),
            Rule::RelationshipTypes => detail.types = Some(parse_relationship_types(sub_pair)?),
            Rule::Properties => detail.properties = Some(parse_properties(sub_pair)?),
            _ => unreachable!(),
        }
    }
    Ok(detail)
}

/// NodeLabels = { NodeLabel+ }
pub fn parse_node_labels(pair: Pair<'_, Rule>) -> Result<Vec<String>> {
    let mut result = vec![];
    let mut pair_iter = pair.into_inner();
    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::NodeLabel => result.push(parse_node_label(sub_pair)?),
            _ => unreachable!(),
        };
    }
    Ok(result)
}

pub fn parse_properties(pair: Pair<'_, Rule>) -> Result<Properties> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::MapLiteral => Ok(Properties::MapLiteral(parse_map_literal(sub_pair)?)),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

/// NodeLabel = { ":" ~ LabelName }
pub fn parse_node_label(pair: Pair<'_, Rule>) -> Result<String> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::LabelName => parse_label_name(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

/// LabelName = { SchemaName }
pub fn parse_label_name(pair: Pair<'_, Rule>) -> Result<String> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::SchemaName => parse_schema_name(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

/// TODO(pgao): Escaped name
pub fn parse_schema_name(pair: Pair<'_, Rule>) -> Result<String> {
    Ok(String::from(pair.as_str()))
}

/// RelationshipTypes = { ":" ~ RelTypeName ~ ( "|" ~ ":" ~ RelTypeName)* }
pub fn parse_relationship_types(pair: Pair<'_, Rule>) -> Result<Vec<String>> {
    let mut res = vec![];
    let mut pair_iter = pair.into_inner();
    while let Some(sub_pair) = pair_iter.next() {
        match sub_pair.as_rule() {
            Rule::RelTypeName => res.push(parse_rel_type_name(sub_pair)?),
            _ => unreachable!(),
        };
    }
    Ok(res)
}

/// RelTypeName = { SchemaName }
pub fn parse_rel_type_name(pair: Pair<'_, Rule>) -> Result<String> {
    if let Some(sub_pair) = pair.into_inner().next() {
        match sub_pair.as_rule() {
            Rule::SchemaName => parse_schema_name(sub_pair),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_pattern() {
        let input = "(a:USER:MOVIE{name:'John'})-[e:LIKE|:RATED]-(b)";
        let mut pairs = Parser::parse(Rule::Pattern, input).unwrap();
        let pair = pairs.next().unwrap();
        let res = parse_pattern(pair).unwrap();
        println!("{:#?}", res);
    }
}
