
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0014_generics_nested.graphqlx");

#[test]
fn generics_nested() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // type Result<T>
  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name(), "Result");
    let generics = object.type_generics().unwrap();
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(generics.params_slice()[0].ident(), "T");

    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name(), "value");
    assert_eq!(fields[1].name(), "success");
  }

  // type Query
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
    assert_eq!(fields.len(), 3);

    // userResult: Result<User>
    {
      let field = &fields[0];
      assert_eq!(field.name(), "userResult");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Result");
      let generics = ty.type_generics().unwrap();
      assert_eq!(generics.params_slice().len(), 1);
    }

    // listResult: Result<[User!]!>
    {
      let field = &fields[1];
      assert_eq!(field.name(), "listResult");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Result");
      let generics = ty.type_generics().unwrap();
      assert_eq!(generics.params_slice().len(), 1);
      // The param is a list type
      let param = &generics.params_slice()[0];
      assert!(param.is_list());
    }

    // optionalResult: Result<User>
    {
      let field = &fields[2];
      assert_eq!(field.name(), "optionalResult");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Result");
    }
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
  }
}
