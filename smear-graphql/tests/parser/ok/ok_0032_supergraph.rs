use smear_graphql::parser::fast::{Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0032_supergraph.graphql");

#[test]
fn supergraph() {
  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 43);
}
