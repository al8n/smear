use smear_graphql::parser::fast::{Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0023_scalar_definition.graphql");

#[test]
fn scalar_type_definition() {
  let definition =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = definition.definitions();
  assert_eq!(definitions.len(), 1);

  let definition = definitions
    .iter()
    .next()
    .unwrap()
    .unwrap_type_system_ref()
    .unwrap_definition_ref()
    .unwrap_type_ref()
    .unwrap_scalar_ref();

  assert_eq!(definition.name().slice(), "Time");
  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().slice(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
