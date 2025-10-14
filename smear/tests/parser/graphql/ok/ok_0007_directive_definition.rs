
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0007_directive_definition.graphql");

#[test]
fn directive_definition() {
  use smear::parser::graphql::ast::{DirectiveDefinition, ParseStr};

  let definition = DirectiveDefinition::<&str>::parse_str(ALL).unwrap();
  assert_eq!(definition.name().source(), "example");
  assert!(definition
    .locations()
    .locations()[0]
    .unwrap_executable_ref()
    .is_field())
}

#[test]
fn graphqlx_directive_definition() {
  use smear::parser::graphqlx::ast::{DirectiveDefinition, ParseStr};

  let definition = DirectiveDefinition::<&str>::parse_str(ALL).unwrap();
  assert_eq!(definition.name(), "example");
  assert!(definition
    .locations()
    .locations()[0]
    .unwrap_executable_ref()
    .is_field())
}
