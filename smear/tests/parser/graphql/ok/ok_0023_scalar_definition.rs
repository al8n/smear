
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0023_scalar_definition.graphql");

#[test]
fn scalar_type_definition() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let definition =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = definition.definitions();
  assert_eq!(definitions.len(), 1);

  let definition = definitions
    .iter()
    .next()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_scalar_ref();

  assert_eq!(definition.name().source(), "Time");
  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().source(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}

#[test]
fn graphqlx_scalar_type_definition() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let definition =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = definition.definitions();
  assert_eq!(definitions.len(), 1);

  let definition = definitions
    .iter()
    .next()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_scalar_ref();

  assert_eq!(definition.name(), "Time");
  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
