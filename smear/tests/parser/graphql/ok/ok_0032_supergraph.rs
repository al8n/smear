

const ALL: &str = include_str!("../../../fixtures/parser/ok/0032_supergraph.graphql");

#[test]
fn supergraph() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 43);
}

#[test]
fn graphqlx_supergraph() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 43);
}

