use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
scalar Time @deprecated
"###;

#[test]
fn scalar_type_definition() {
  let definition =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = definition.content();
  assert_eq!(definitions.len(), 1);

  let definition = definitions
    .iter()
    .next()
    .unwrap()
    .unwrap_definition_ref()
    .unwrap_type_ref()
    .unwrap_scalar_ref();

  assert_eq!(definition.name().span().source(), &"Time");
  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let mut directives = directives.directives().iter();
  {
    let deprecated = directives.next().unwrap();
    assert_eq!(deprecated.name().span().source(), &"deprecated");
    assert!(deprecated.arguments().is_none());
  }
}
