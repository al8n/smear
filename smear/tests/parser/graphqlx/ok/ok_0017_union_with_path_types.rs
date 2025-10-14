
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0017_union_with_path_types.graphqlx");

#[test]
fn union_with_path_types() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 1);

  let union = definitions[0]
    .unwrap_definition_ref()
    .unwrap_type_system_ref()
    .unwrap_type_ref()
    .unwrap_union_ref();

  assert_eq!(union.name(), "SearchResult");

  let members = union.members().unwrap().members_slice();
  assert_eq!(members.len(), 3);

  assert_eq!(members[0].path(), "user::Profile");
  assert_eq!(members[1].path(), "blog::Post");
  assert_eq!(members[2].path(), "media::Image");
}
