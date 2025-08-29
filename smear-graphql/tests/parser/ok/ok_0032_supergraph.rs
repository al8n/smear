use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0032_supergraph.graphql");

#[test]
fn supergraph() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = document.content();
  assert_eq!(definitions.len(), 43);
}
