
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0022_complex_fragments.graphqlx");

#[test]
fn complex_fragments() {
  use smear::parser::graphqlx::ast::{ExecutableDocument, ParseStr};

  let document = ExecutableDocument::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 7);

  let mut iter = definitions.iter();

  // fragment<T> UserFields<T> on User<T>
  {
    let fragment = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();

    assert_eq!(fragment.name(), "UserFields");
    let generics = fragment.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);

    let type_condition = fragment.type_condition();
    assert_eq!(type_condition.path(), "User");
    assert!(type_condition.type_generics().is_some());

    let selections = fragment.selection_set().selections();
    assert_eq!(selections.len(), 4);
  }

  // fragment<K, V> ConnectionFields<K, V> on Connection<K, V>
  {
    let fragment = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();

    assert_eq!(fragment.name(), "ConnectionFields");
    let generics = fragment.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 2);
  }

  // query GetUserData<T>
  {
    let operation = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_operation_ref()
      .unwrap_named_ref();

    assert_eq!(operation.name().unwrap(), "GetUserData");
    let generics = operation.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);

    let selections = operation.selection_set().selections();
    assert_eq!(selections.len(), 2);
  }

  // fragment<T> NodeFragment<T> on Node<T> where T: Timestamped & Serializable
  {
    let fragment = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();

    assert_eq!(fragment.name(), "NodeFragment");

    let generics = fragment.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);

    // Has where clause
    let where_clause = fragment.where_clause().unwrap();
    let predicates = where_clause.predicates_slice();
    assert_eq!(predicates.len(), 1);

    let bounds = predicates[0].bounds_slice();
    assert_eq!(bounds.len(), 2);
    assert_eq!(bounds[0].path(), "Timestamped");
    assert_eq!(bounds[1].path(), "Serializable");
  }

  // query ComplexQuery<T, U>
  {
    let operation = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_operation_ref()
      .unwrap_named_ref();

    assert_eq!(operation.name().unwrap(), "ComplexQuery");
    let generics = operation.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 2);
  }

  // fragment NestedFragmentSpread on Post
  {
    let fragment = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();

    assert_eq!(fragment.name(), "NestedFragmentSpread");
    assert!(fragment.type_generics().is_none());
  }
}
