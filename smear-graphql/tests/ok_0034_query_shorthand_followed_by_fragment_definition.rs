use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
{
  ...friendFields
}

fragment friendFields on User {
  id
  name
}
"###;

#[test]
fn query_shorthand_followed_by_fragment_definition() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = document.content();
  assert_eq!(definitions.len(), 1);

  let mut iter = definitions.iter();

  // {
  //   let query = iter.next().unwrap().unwrap_definition_ref().un
  // }
}
