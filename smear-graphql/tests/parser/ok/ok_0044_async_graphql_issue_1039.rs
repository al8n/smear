use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0044_async_graphql_issue_1039.graphql");

// https://github.com/async-graphql/async-graphql/issues/1039
#[test]
fn async_graphql_issue_1039() {
  let document = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<'_, char>>,
  >(ALL)
  .unwrap();

  let definitions = document.content();

  assert_eq!(definitions.len(), 2);
  let mut iter = definitions.iter();

  {
    let fragment = iter.next().unwrap().unwrap_executable_ref().unwrap_fragment_ref();

    assert_eq!(fragment.name().span().source(), &"onboardingFull");
    assert_eq!(fragment.type_condition().name().span().source(), &"OnboardingState");

    let selections = fragment.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().span().source(), &"license");
  }

  {
    let query = iter.next().unwrap().unwrap_executable_ref().unwrap_operation_ref().unwrap_named_ref();
    assert_eq!(query.name().unwrap().span().source(), &"globalConfig");
    assert!(query.operation_type().is_query());

    let selections = query.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().span().source(), &"globalConfig");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().span().source(), &"onboarding");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.name().span().source(), &"onboardingFull");
  }
}