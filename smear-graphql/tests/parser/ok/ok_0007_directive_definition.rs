use smear_graphql::parser::ast::{raw::DirectiveDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0007_directive_definition.graphql");

#[test]
fn directive_definition() {
  let definition = DirectiveDefinition::<&str>::parse_str(ALL).unwrap();
  assert_eq!(definition.name().slice(), "example");
  assert!(definition
    .locations()
    .locations()[0]
    .unwrap_executable_ref()
    .is_field())
}
