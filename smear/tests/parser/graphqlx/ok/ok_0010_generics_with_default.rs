
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0010_generics_with_default.graphqlx");

#[test]
fn generics_with_default() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  // Response<T = String>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Response");

    let generics = object.type_generics().unwrap();
    let params = generics.params_slice();
    assert_eq!(params.len(), 1);

    let param = &params[0];
    assert_eq!(param.ident(), "T");
    assert!(param.default().is_some());
    let default_ty = param.default().unwrap().unwrap_path_ref();
    assert_eq!(default_ty.path(), "String");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].name(), "data");
    assert_eq!(fields[1].name(), "error");
    assert_eq!(fields[2].name(), "status");
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

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);

    // defaultResponse: Response
    assert_eq!(fields[0].name(), "defaultResponse");

    // intResponse: Response<Int>
    assert_eq!(fields[1].name(), "intResponse");
    let ty = fields[1].ty().unwrap_path_ref();
    assert!(ty.type_generics().is_some());
  }
}
