use smear_graphql::parser::fast::{EnumTypeExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0011_enum_type_extension.graphql");

#[test]
fn enum_type_extension() {
  let extension = EnumTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.name().slice(), "Direction");

  let directives = extension.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().slice(), "example");
  assert!(directive.arguments().is_none());

  let variants = extension.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 2);
  let mut iter = variants.enum_value_definitions().iter();

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
