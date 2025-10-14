
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0001_import_named.graphqlx");

#[test]
fn import_named() {
  use smear::parser::graphqlx::ast::*;

  let definition =
    ImportDefinition::<&str>::parse_str(ALL)
      .unwrap();
  assert_eq!(definition.clause().unwrap_list_ref().len(), 2);
  assert_eq!(definition.file_path().content(), "./types.graphqlx");
}  
