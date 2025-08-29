use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};


const ALL: &str = include_str!("../../fixtures/parser/err/0001_directive_definition_missing_location.graphql");

#[test]
fn directive_definition_missing_location() {
  let err = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<'_, char>>,
  >(ALL)
  .unwrap_err();
}
