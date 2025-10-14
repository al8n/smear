
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0009_map_value_nested.graphqlx");

#[test]
fn map_value_nested() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 1);

  let input = definitions[0]
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_input_object_ref();

  assert_eq!(input.name(), "AdvancedConfig");

  let fields = input.fields_definition().unwrap().input_value_definitions();
  assert_eq!(fields.len(), 1);

  let field = &fields[0];
  assert_eq!(field.name(), "database");

  // Type should be nested map: <String! => <String! => String!>!>!
  let ty = field.ty().unwrap_map_ref();
  assert!(ty.required());

  // Value type should also be a map
  let k = ty.key().unwrap_path_ref();
  assert_eq!(k.path(), "String");
  assert!(k.required());

  let v = ty.value().unwrap_map_ref();
  assert!(v.required());

  let vk = v.key().unwrap_path_ref();
  assert_eq!(vk.path(), "String");
  assert!(vk.required());

  let vv = v.value().unwrap_path_ref();
  assert_eq!(vv.path(), "String");
  assert!(vv.required());

  let default = field.default_value().unwrap();
  let map_value = default.value().unwrap_map_ref();
  let entries = map_value.entries_slice();
  assert_eq!(entries.len(), 1);
  let entry = &entries[0];
  let key = entry.key().unwrap_string_ref();
  assert_eq!(key.content(), "credentials");
  let value = entry.value().unwrap_map_ref();
  let entries = value.entries_slice();
  assert_eq!(entries.len(), 2);
  let entry = &entries[0];
  let key = entry.key().unwrap_string_ref();
  assert_eq!(key.content(), "username");
  let value = entry.value().unwrap_string_ref();
  assert_eq!(value.content(), "admin");
  let entry = &entries[1];
  let key = entry.key().unwrap_string_ref();
  assert_eq!(key.content(), "password");
  let value = entry.value().unwrap_string_ref();
  assert_eq!(value.content(), "secret");
}
