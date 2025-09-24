use smear_graphql::parser::ast::{DescribedEnumTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0010_enum_type_definition.graphql");

#[test]
fn enum_type_definition() {
  let definition = DescribedEnumTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "Direction");
  assert!(definition.description().is_none());

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().slice(), "example");
  assert!(directive.arguments().is_none());

  let variants = definition.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 4);
  let mut iter = variants.enum_value_definitions().iter();

  {
    let north = iter.next().unwrap();
    assert_eq!(north.value().slice(), "NORTH");
    assert!(north.description().is_some());
    assert_eq!(
      north
        .description()
        .as_ref()
        .unwrap()
        .content(),
      "\n    description\n    "
    );
    assert!(north.directives().is_none());
  }

  {
    let east = iter.next().unwrap();
    assert_eq!(east.value().slice(), "EAST");
    assert!(east.description().is_none());
    assert!(east.directives().is_none());
  }

  {
    let south = iter.next().unwrap();
    assert_eq!(south.value().slice(), "SOUTH");
    assert!(south.description().is_none());
    assert!(south.directives().is_none());
  }

  {
    let west = iter.next().unwrap();
    assert_eq!(west.value().slice(), "WEST");
    assert!(west.description().is_none());
    assert!(west.directives().is_none());
  }

  assert!(iter.next().is_none());
}
