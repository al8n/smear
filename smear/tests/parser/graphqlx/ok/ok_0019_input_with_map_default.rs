
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0019_input_with_map_default.graphqlx");

#[test]
fn input_with_map_default() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 1);

  let input = definitions[0]
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_input_object_ref();

  assert_eq!(input.name(), "CreateUserInput");

  let fields = input.fields_definition().unwrap().input_value_definitions();
  assert_eq!(fields.len(), 2);

  // name: String!
  {
    let field = &fields[0];
    assert_eq!(field.name(), "name");
    let ty = field.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "String");
    assert!(ty.required());
  }

  // preferences: <String! => String!>!
  {
    let field = &fields[1];
    assert_eq!(field.name(), "preferences");
    let ty = field.ty().unwrap_map_ref();
    assert!(ty.required());
  }
}
