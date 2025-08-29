use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0007_directive_definition.graphql");

#[test]
fn directive_definition() {
  let definition = DirectiveDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"example");
  assert!(definition
    .locations()
    .leading_location()
    .location()
    .unwrap_executable_ref()
    .is_field())
}
