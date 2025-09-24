use smear_graphql::parser::ast::{ScalarTypeExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0024_scalar_extension.graphql");

#[test]
fn scalar_type_extension() {
  let extension = ScalarTypeExtension::<&str>::parse_str(ALL).unwrap();

  assert_eq!(extension.name().slice(), "Time");
  let directives = extension.directives();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().slice(), "deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
