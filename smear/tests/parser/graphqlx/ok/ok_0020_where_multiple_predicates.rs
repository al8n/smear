
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0020_where_multiple_predicates.graphqlx");

#[test]
fn where_multiple_predicates() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 4);

  let mut iter = definitions.iter();

  // Container<T, U> where T: Node & Timestamped, U: Serializable
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Container");

    let generics = object.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 2);
    assert_eq!(generics.params_slice()[0].ident(), "T");
    assert_eq!(generics.params_slice()[1].ident(), "U");

    let where_clause = object.where_clause().unwrap();
    let predicates = where_clause.predicates_slice();
    assert_eq!(predicates.len(), 2);

    // T: Node & Timestamped
    {
      let predicate = &predicates[0];
      assert_eq!(predicate.bounded_type().path(), "T");
      let bounds = predicate.bounds_slice();
      assert_eq!(bounds.len(), 2);
      assert_eq!(bounds[0].path(), "Node");
      assert_eq!(bounds[1].path(), "Timestamped");
    }

    // U: Serializable
    {
      let predicate = &predicates[1];
      assert_eq!(predicate.bounded_type().path(), "U");
      let bounds = predicate.bounds_slice();
      assert_eq!(bounds.len(), 1);
      assert_eq!(bounds[0].path(), "Serializable");
    }

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "primary");
    assert_eq!(fields[1].name(), "secondary");
  }

  // interface Node
  {
    let interface = iter.next().unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(interface.name(), "Node");
  }

  // interface Timestamped
  {
    let interface = iter.next().unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(interface.name(), "Timestamped");
  }

  // interface Serializable
  {
    let interface = iter.next().unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(interface.name(), "Serializable");
  }
}
