
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0012_path_type.graphqlx");

#[test]
fn path_type() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 1);

  let mut iter = definitions.iter();

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

    // user: user::Profile
    {
      let field = &fields[0];
      assert_eq!(field.name(), "user");
      let ty = field.ty().unwrap_path_ref();
      let path = ty.path();
      assert_eq!(path, "user::Profile");
    }

    // post: blog::Post
    {
      let field = &fields[1];
      assert_eq!(field.name(), "post");
      let ty = field.ty().unwrap_path_ref();
      let path = ty.path();
      assert_eq!(path, "blog::Post");
    }
  }
}
