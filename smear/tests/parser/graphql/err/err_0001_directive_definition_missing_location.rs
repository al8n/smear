


const ALL: &str = include_str!("../../../fixtures/parser/graphql/err/0001_directive_definition_missing_location.graphql");

#[cfg(feature = "graphql")]
#[test]
fn directive_definition_missing_location() {
  use smear::parser::graphql::ast::{Document, ParseStr};
  let _ = Document::<&str>::parse_str(ALL).into_result().unwrap_err();
}
