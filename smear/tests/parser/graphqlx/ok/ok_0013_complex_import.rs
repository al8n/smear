
const ALL: &str = include_str!("../../../fixtures/parser/graphqlx/ok/0013_complex_import.graphqlx");

#[test]
fn complex_import() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();
  let definitions = document.definitions();
  assert_eq!(definitions.len(), 3);

  let mut iter = definitions.iter();

  // First import: import { User, Post, * as utils } from "./types.graphqlx"
  {
    let import = iter.next().unwrap().unwrap_import_ref();
    let list = import.clause().unwrap_list_ref();
    assert_eq!(list.len(), 3);

    // User
    assert_eq!(list[0].unwrap_named_ref().name(), "User");

    // Post
    assert_eq!(list[1].unwrap_named_ref().name(), "Post");

    // * as utils
    let wildcard = list[2].unwrap_wildcard_ref();
    assert_eq!(wildcard.alias().unwrap(), "utils");

    assert_eq!(import.file_path().content(), "./types.graphqlx");
  }

  // Second import: import * as helpers from "./helpers.graphqlx"
  {
    let import = iter.next().unwrap().unwrap_import_ref();
    let wildcard = import.clause().unwrap_wildcard_ref();
    assert_eq!(wildcard.alias().unwrap(), "helpers");
    assert_eq!(import.file_path().content(), "./helpers.graphqlx");
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
  }
}
