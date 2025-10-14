
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0004_generics_simple.graphqlx");

#[test]
fn generics_simple() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let definition = Document::<&str>::parse_str(ALL).unwrap();
  let content = definition.definitions();
  assert_eq!(content.len(), 2);

  let mut iter = content.iter();

  {
    let described = iter
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Container");
    let generics = object.type_generics().unwrap();
    let slice = generics.params_slice();
    assert_eq!(slice.len(), 1);

    let first = &slice[0];
    let ident = first.ident();
    assert_eq!(ident, "T");

    let fields = object.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 2);

    let first_field = &field_definitions[0];
    assert_eq!(first_field.name(), "value");
    let ty = first_field.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "T");
    assert!(!ty.required());

    let second_field = &field_definitions[1];
    assert_eq!(second_field.name(), "count");
    let ty = second_field.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "Int");
    assert!(!ty.required());
  }


  {
    let described = iter
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Query");
    assert!(object.type_generics().is_none());

    let fields = object.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 2);

    {
      let first_field = &field_definitions[0];
      assert_eq!(first_field.name(), "stringContainer");
      let ty = first_field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Container");
      assert!(ty.required());
      let generics = ty.type_generics().unwrap();
      assert!(!generics.params().is_empty());
      let param = generics.params_slice()[0].unwrap_path_ref();
      assert!(param.required());
      assert_eq!(param.path(), "String");
    }

    {
      let second_field = &field_definitions[1];
      assert_eq!(second_field.name(), "intContainer");
      let ty = second_field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Container");
      assert!(!ty.required());
      let generics = ty.type_generics().unwrap();
      assert!(!generics.params().is_empty());
      let param = generics.params_slice()[0].unwrap_path_ref();
      assert!(!param.required());
      assert_eq!(param.path(), "Int");
    }
  }
}
