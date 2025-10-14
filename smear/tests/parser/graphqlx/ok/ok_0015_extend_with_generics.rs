
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0015_extend_with_generics.graphqlx");

#[test]
fn extend_with_generics() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // type Box<T>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Box");
    let generics = object.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name(), "value");
  }

  // extend type Box<T>
  {
    let extension = iter
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(extension.path(), "Box");
    let generics = extension.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let fields = extension.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name(), "metadata");
  }

  // extend type Box<String>
  {
    let extension = iter
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(extension.path(), "Box");
    let generics = extension.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    // Should be String, not a type parameter
    assert_eq!(generics.params_slice()[0].ident(), "String");

    let fields = extension.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name(), "length");
  }
}
