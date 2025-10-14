
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0007_where_clause_multiple_bounds.graphqlx");

#[test]
fn where_clause_multiple_bounds() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // Storage<T> where T: Node & Timestamped
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Storage");

    let generics = object.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);

    let where_clause = object.where_clause().unwrap();
    let predicates = where_clause.predicates_slice();
    assert_eq!(predicates.len(), 1);

    let predicate = &predicates[0];
    assert_eq!(predicate.bounded_type().path(), "T");
    let bounds = predicate.bounds_slice();
    assert_eq!(bounds.len(), 2);
    assert_eq!(bounds[0].path(), "Node");
    assert_eq!(bounds[1].path(), "Timestamped");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "data");
    assert_eq!(fields[1].name(), "metadata");
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
  }

  // interface Timestamped
  {
    let interface = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(interface.name(), "Timestamped");
    let fields = interface.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "createdAt");
    assert_eq!(fields[1].name(), "updatedAt");
  }
}
