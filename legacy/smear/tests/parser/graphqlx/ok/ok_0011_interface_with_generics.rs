
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0011_interface_with_generics.graphqlx");

#[test]
fn interface_with_generics() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // interface Collection<T>
  {
    let interface = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(interface.name(), "Collection");

    let generics = interface.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let fields = interface.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "items");
    assert_eq!(fields[1].name(), "total");
  }

  // type UserCollection implements Collection<User>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "UserCollection");

    let implements = object.implements().unwrap();
    let interfaces = implements.interfaces();
    assert_eq!(interfaces.len(), 1);

    let interface_ty = &interfaces[0];
    assert_eq!(interface_ty.path(), "Collection");
    assert!(interface_ty.type_generics().is_some());

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].name(), "items");
    assert_eq!(fields[1].name(), "total");
    assert_eq!(fields[2].name(), "activeCount");
  }

  // type User
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "User");
    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "id");
    assert_eq!(fields[1].name(), "name");
  }
}
