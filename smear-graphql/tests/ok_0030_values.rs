use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
{
    user(
        id: 4,
        size: $size
        value: "string",
        input: [ "one", 1.34 ],
        otherInput: { key: false, output: null }
        emptyList: []
        emptyObject: {}
    )
}

"###;

const INPUT_VALUES: &str = r###"
(
        id: 4,
        size: $size
        value: "string",
        input: [ "one", 1.34 ],
        otherInput: { key: false, output: null }
        emptyList: []
        emptyObject: {}
    )

"###;

#[test]
fn values() {
  let values = InputValueDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(INPUT_VALUES)
  .unwrap();
}
