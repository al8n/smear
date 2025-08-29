use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0023_scalar_definition.graphql");

#[test]
fn scalar_type_extension() {
  let extension = ScalarTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  assert_eq!(extension.name().span().source(), &"Time");
  let directives = extension.directives();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().span().source(), &"deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
