
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0010_enum_type_definition.graphql");

#[cfg(feature = "graphql")]
#[test]
fn enum_type_definition() {
  use smear::parser::graphql::ast::{DescribedEnumTypeDefinition, ParseStr};

  let definition = DescribedEnumTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "Direction");
  assert!(definition.description().is_none());

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().source(), "example");
  assert!(directive.arguments().is_none());

  let variants = definition.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 4);
  let mut iter = variants.enum_value_definitions().iter();

  {
    let north = iter.next().unwrap();
    assert_eq!(north.value().source(), "NORTH");
    assert!(north.description().is_some());
    assert_eq!(
      north
        .description()
        .as_ref()
        .unwrap()
        .source().trim_matches('"'),
      "\n    description\n    "
    );
    assert!(north.directives().is_none());
  }

  {
    let east = iter.next().unwrap();
    assert_eq!(east.value().source(), "EAST");
    assert!(east.description().is_none());
    assert!(east.directives().is_none());
  }

  {
    let south = iter.next().unwrap();
    assert_eq!(south.value().source(), "SOUTH");
    assert!(south.description().is_none());
    assert!(south.directives().is_none());
  }

  {
    let west = iter.next().unwrap();
    assert_eq!(west.value().source(), "WEST");
    assert!(west.description().is_none());
    assert!(west.directives().is_none());
  }

  assert!(iter.next().is_none());
}

#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_enum_type_definition() {
  use smear::parser::graphqlx::ast::{DescribedEnumTypeDefinition, ParseStr};

  let definition = DescribedEnumTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name(), "Direction");
  assert!(definition.description().is_none());

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.path(), "example");
  assert!(directive.arguments().is_none());

  let variants = definition.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 4);
  let mut iter = variants.enum_value_definitions().iter();

  {
    let north = iter.next().unwrap();
    assert_eq!(north.value().source(), "NORTH");
    assert!(north.description().is_some());
    assert_eq!(
      north
        .description()
        .as_ref()
        .unwrap()
        .source().trim_matches('"'),
      "\n    description\n    "
    );
    assert!(north.directives().is_none());
  }

  {
    let east = iter.next().unwrap();
    assert_eq!(east.value().source(), "EAST");
    assert!(east.description().is_none());
    assert!(east.directives().is_none());
  }

  {
    let south = iter.next().unwrap();
    assert_eq!(south.value().source(), "SOUTH");
    assert!(south.description().is_none());
    assert!(south.directives().is_none());
  }

  {
    let west = iter.next().unwrap();
    assert_eq!(west.value().source(), "WEST");
    assert!(west.description().is_none());
    assert!(west.directives().is_none());
  }

  assert!(iter.next().is_none());
}
