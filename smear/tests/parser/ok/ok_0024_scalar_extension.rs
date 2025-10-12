
const ALL: &str = include_str!("../../fixtures/parser/ok/0024_scalar_extension.graphql");

#[test]
fn scalar_type_extension() {
  use smear::parser::graphql::ast::{ScalarTypeExtension, ParseStr};

  let extension = ScalarTypeExtension::<&str>::parse_str(ALL).unwrap();

  assert_eq!(extension.name().source(), "Time");
  let directives = extension.directives();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().source(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}

#[test]
fn graphqlx_scalar_type_extension() {
  use smear::parser::graphqlx::ast::{ScalarTypeExtension, ParseStr};

  let extension = ScalarTypeExtension::<&str>::parse_str(ALL).unwrap();

  assert_eq!(extension.name(), "Time");
  let directives = extension.directives();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
