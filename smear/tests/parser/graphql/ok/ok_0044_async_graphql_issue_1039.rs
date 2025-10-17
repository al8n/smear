

const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0044_async_graphql_issue_1039.graphql");

// https://github.com/async-graphql/async-graphql/issues/1039
#[test]
#[cfg(feature = "graphql")]
fn async_graphql_issue_1039() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).into_result().unwrap();

  let definitions = document.definitions();

  assert_eq!(definitions.len(), 2);
  let mut iter = definitions.iter();

  {
    let fragment = iter.next().unwrap().unwrap_definition_ref().unwrap_executable_ref().unwrap_fragment_ref();

    assert_eq!(fragment.name().source(), "onboardingFull");
    assert_eq!(fragment.type_condition().name().source(), "OnboardingState");

    let selections = fragment.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().source(), "license");
  }

  {
    let query = iter.next().unwrap().unwrap_definition_ref().unwrap_executable_ref().unwrap_operation_ref().unwrap_named_ref();
    assert_eq!(query.name().unwrap().source(), "globalConfig");
    assert!(query.operation_type().is_query());

    let selections = query.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().source(), "globalConfig");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name().source(), "onboarding");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.name().source(), "onboardingFull");
  }
}

// https://github.com/async-graphql/async-graphql/issues/1039
#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_async_graphql_issue_1039() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).into_result().unwrap();

  let definitions = document.definitions();

  assert_eq!(definitions.len(), 2);
  let mut iter = definitions.iter();

  {
    let fragment = iter.next().unwrap().unwrap_definition_ref().unwrap_executable_ref().unwrap_fragment_ref();

    assert_eq!(fragment.name(), "onboardingFull");
    assert_eq!(fragment.type_condition().path(), "OnboardingState");

    let selections = fragment.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name(), "license");
  }

  {
    let query = iter.next().unwrap().unwrap_definition_ref().unwrap_executable_ref().unwrap_operation_ref().unwrap_named_ref();
    assert_eq!(query.name().unwrap(), "globalConfig");
    assert!(query.operation_type().is_query());

    let selections = query.selection_set().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name(), "globalConfig");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(selection.name(), "onboarding");

    let selections = selection.selection_set().unwrap().selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.path(), "onboardingFull");
  }
}
