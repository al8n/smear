use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
directive @example(isTreat: Boolean, treatKind: String) on FIELD | MUTATION
"###;

const ARGS_INPUT: &str = r###"(isTreat: Boolean, treatKind: String)"###;

#[test]
fn arguments_definition() {
  let args = ArgumentsDefinition::<WithSource<&str, SimpleSpan>>::parse_str::<
    extra::Err<Simple<'_, char>>,
  >(ARGS_INPUT)
  .unwrap();
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

#[test]
fn directive_definition_with_arguments() {
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
