
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0002_import_wildcard.graphqlx");

#[test]
fn import_wildcard() {
  use smear::parser::graphqlx::ast::*;

  let definition =
    ImportDefinition::<&str>::parse_str(ALL)
      .unwrap();
  assert!(definition.clause().is_wildcard());
  assert_eq!(definition.file_path().content(), "./types.graphqlx");
}
