
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0003_import_with_alias.graphqlx");

#[test]
fn import_with_alias() {
  use smear::parser::graphqlx::ast::*;

  let definition =
    ImportDefinition::<&str>::parse_str(ALL)
      .unwrap();
  let list = definition.clause().unwrap_list_ref();
  assert_eq!(list.len(), 2);

  {
    let first = &list[0];
    assert_eq!(first.unwrap_named_ref().name(), "User");
    assert_eq!(first.alias().unwrap(), "UserType");
  }

  {
    let second = &list[1];
    assert_eq!(second.unwrap_named_ref().name(), "Post");
    assert_eq!(second.alias().unwrap(), "BlogPost");
  }
}
