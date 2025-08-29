use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0009_directive_definition_repeatable.graphql");

#[test]
fn directive_definition_repeatable() {
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
    .is_field());
  assert_eq!(definition.locations().remaining_locations().len(), 1);
  assert!(definition
    .locations()
    .remaining_locations()
    .first()
    .unwrap()
    .location()
    .unwrap_executable_ref()
    .is_mutation());
  assert!(definition.repeatable().is_some());

  let args = definition.arguments_definition().unwrap();
  let mut iter = args.input_value_definitions().iter();

  {
    let is_treat = iter.next().unwrap();
    assert_eq!(is_treat.name().span().source(), &"isTreat");
    let ty = is_treat.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Boolean");
    assert!(ty.bang().is_none());
  }

  {
    let treat_kind = iter.next().unwrap();
    assert_eq!(treat_kind.name().span().source(), &"treatKind");
    let ty = treat_kind.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"String");
    assert!(ty.bang().is_none());
  }

  assert!(iter.next().is_none());
}
