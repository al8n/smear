
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0006_where_clause_simple.graphqlx");

#[test]
fn where_clause_simple() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  // Repository<T> where T: Node
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Repository");

    let generics = object.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let where_clause = object.where_clause().unwrap();
    let predicates = where_clause.predicates_slice();
    assert_eq!(predicates.len(), 1);

    let predicate = &predicates[0];
    assert_eq!(predicate.bounded_type().path(), "T");
    assert_eq!(predicate.bounds_slice().len(), 1);
    assert_eq!(predicate.bounds_slice()[0].path(), "Node");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "items");
    assert_eq!(fields[1].name(), "count");
  }

  // interface Node
  {
    let interface = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(interface.name(), "Node");
    assert!(interface.type_generics().is_none());
    assert!(interface.where_clause().is_none());
  }
}
