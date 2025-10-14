
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0008_map_value_simple.graphqlx");

#[test]
fn map_value_simple() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 1);

  let input = definitions[0]
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_input_object_ref();

  assert_eq!(input.name(), "ConfigInput");

  let fields = input.fields_definition().unwrap().input_value_definitions();
  assert_eq!(fields.len(), 1);

  let field = &fields[0];
  assert_eq!(field.name(), "settings");

  // Type should be <String! => String!>!
  let ty = field.ty().unwrap_map_ref();
  assert!(ty.required());
}
