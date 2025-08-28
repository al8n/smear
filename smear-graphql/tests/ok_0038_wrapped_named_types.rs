use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
type ObjectDef {
    a: String
    b: Int!
    c: [Int!]!
    d: [[[[[Int]]]]]
    d: [[[[[Int!]!]!]!]!]!
}

type ObjectDefTwo {
    a: String,
    b: Int!,
    c: [Int!]!,
    d: [[[[[Int]]]]],
    d: [[[[[Int!]!]!]!]!]!,
}
"###;

#[test]
fn wrapped_named_types() {
  // let values = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
  //   extra::Err<Rich<char>>,
  // >(ALL)
  // .unwrap();
}
