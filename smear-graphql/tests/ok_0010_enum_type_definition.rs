use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
enum Direction @example {
    """
    description
    """
    NORTH
    EAST
    SOUTH
    WEST
}
"###;

#[test]
fn enum_type_definition() {
  let definition = EnumTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"Direction");
  assert!(definition.description().is_none());

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().span().source(), &"example");
  assert!(directive.arguments().is_none());

  let variants = definition.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 4);
  let mut iter = variants.enum_value_definitions().iter();

  {
    let north = iter.next().unwrap();
    assert_eq!(north.value().span().source(), &"NORTH");
    assert!(north.description().is_some());
    assert_eq!(
      north
        .description()
        .as_ref()
        .unwrap()
        .content()
        .span()
        .source(),
      &"\n    description\n    "
    );
    assert!(north.directives().is_none());
  }

  {
    let east = iter.next().unwrap();
    assert_eq!(east.value().span().source(), &"EAST");
    assert!(east.description().is_none());
    assert!(east.directives().is_none());
  }

  {
    let south = iter.next().unwrap();
    assert_eq!(south.value().span().source(), &"SOUTH");
    assert!(south.description().is_none());
    assert!(south.directives().is_none());
  }

  {
    let west = iter.next().unwrap();
    assert_eq!(west.value().span().source(), &"WEST");
    assert!(west.description().is_none());
    assert!(west.directives().is_none());
  }

  assert!(iter.next().is_none());
}
