use smear_graphql::parser::ast::{raw::Document, ParseStr};


const ALL: &str = include_str!("../../fixtures/parser/err/0001_directive_definition_missing_location.graphql");

#[test]
fn directive_definition_missing_location() {
  let _ = Document::<&str>::parse_str(ALL).into_result().unwrap_err();
}
