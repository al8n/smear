use smear_graphql::parser::ast::{Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0044_async_graphql_issue_1039.graphql");

// https://github.com/async-graphql/async-graphql/issues/1039
#[test]
fn async_graphql_issue_1039() {
  let document = Document::<&str>::parse_str(ALL).into_result().unwrap();

  let definitions = document.definitions();

  assert_eq!(definitions.len(), 2);
  let mut iter = definitions.iter();

  {
    let fragment = iter.next().unwrap().unwrap_executable_ref().unwrap_fragment_ref();

    assert_eq!(fragment.name().slice(), "onboardingFull");
    assert_eq!(fragment.type_condition().name().slice(), "OnboardingState");

    let selections = fragment.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().slice(), "license");
  }

  {
    let query = iter.next().unwrap().unwrap_executable_ref().unwrap_operation_ref().unwrap_named_ref();
    assert_eq!(query.name().unwrap().slice(), "globalConfig");
    assert!(query.operation_type().is_query());

    let selections = query.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().slice(), "globalConfig");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().slice(), "onboarding");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.name().slice(), "onboardingFull");
  }
}