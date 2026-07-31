
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0016_operation_with_generics.graphqlx");

#[test]
fn operation_with_generics() {
  use smear::parser::graphqlx::ast::{ExecutableDocument, ParseStr};

  let document = ExecutableDocument::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  // query GetData<T>
  {
    let operation = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_operation_ref()
      .unwrap_named_ref();

    assert_eq!(operation.name().unwrap(), "GetData");
    let generics = operation.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let selection_set = operation.selection_set();
    assert_eq!(selection_set.selections().len(), 1);
  }

  // fragment<T> ItemFragment<T> on Item<T>
  {
    let fragment = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();

    assert_eq!(fragment.name(), "ItemFragment");

    let generics = fragment.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0], "T");

    let type_condition = fragment.type_condition();
    assert_eq!(type_condition.path(), "Item");
    let condition_generics = type_condition.type_generics().unwrap();
    assert_eq!(condition_generics.params_slice().len(), 1);

    let selection_set = fragment.selection_set();
    assert_eq!(selection_set.selections().len(), 2);
  }
}
