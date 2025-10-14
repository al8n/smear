
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0018_union_with_path_types_and_generics.graphqlx");

#[test]
fn union_with_path_types_and_generics() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  // union SearchResult<I> where I: ID
  {
    let union = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_union_ref();

    assert_eq!(union.name(), "SearchResult");

    let generics = union.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "I");

    let where_clause = union.where_clause().unwrap();
    let predicates = where_clause.predicates_slice();
    assert_eq!(predicates.len(), 1);
    assert_eq!(predicates[0].bounded_type().path(), "I");
    assert_eq!(predicates[0].bounds_slice()[0].path(), "ID");

    let members = union.members().unwrap().members_slice();
    assert_eq!(members.len(), 3);

    // All members should have generic type parameter
    for member in members {
      assert_eq!(member.path().segments().len(), 2);
      assert!(member.type_generics().is_some());
    }
  }

  // interface ID
  {
    let interface = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(interface.name(), "ID");
  }
}
