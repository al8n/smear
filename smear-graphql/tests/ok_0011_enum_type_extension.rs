use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
extend enum Direction @example {
    SOUTH
    WEST
}
"###;

#[test]
fn enum_type_extension() {
  let extension = EnumTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(extension.name().span().source(), &"Direction");

  let directives = extension.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().span().source(), &"example");
  assert!(directive.arguments().is_none());

  let variants = extension.enum_values_definition().unwrap();
  assert_eq!(variants.enum_value_definitions().len(), 2);
  let mut iter = variants.enum_value_definitions().iter();

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
