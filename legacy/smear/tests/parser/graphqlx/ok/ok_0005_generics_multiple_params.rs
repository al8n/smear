
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0005_generics_multiple_params.graphqlx");

#[test]
fn generics_multiple_params() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // Pair<K, V>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Pair");
    let generics = object.type_generics().unwrap();
    let params = generics.params_slice();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].ident(), "K");
    assert_eq!(params[1].ident(), "V");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "key");
    assert_eq!(fields[1].name(), "value");
  }

  // Triple<A, B, C>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Triple");
    let generics = object.type_generics().unwrap();
    let params = generics.params_slice();
    assert_eq!(params.len(), 3);
    assert_eq!(params[0].ident(), "A");
    assert_eq!(params[1].ident(), "B");
    assert_eq!(params[2].ident(), "C");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].name(), "first");
    assert_eq!(fields[1].name(), "second");
    assert_eq!(fields[2].name(), "third");
  }

  // Query
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Query");
    assert!(object.type_generics().is_none());

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);

    // config: Pair<String!, Int!>!
    {
      let field = &fields[0];
      assert_eq!(field.name(), "config");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Pair");
      assert!(ty.required());
      let generics = ty.type_generics().unwrap();
      assert_eq!(generics.params_slice().len(), 2);
    }

    // data: Triple<String, Int, Boolean>
    {
      let field = &fields[1];
      assert_eq!(field.name(), "data");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Triple");
      assert!(!ty.required());
      let generics = ty.type_generics().unwrap();
      assert_eq!(generics.params_slice().len(), 3);
    }
  }
}
