use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{parse::*, ast::*};

const ALL: &str = r###""An input with no input values"
input AnInputWithoutInputValues

extend input AnInputWithoutInputValues {
  limit: Int!
}
"###;

const INPUT: &str = r###""An input with no input values"
input AnInputWithoutInputValues
"###;

#[test]
fn input_definition_without_input_values() {

  let document = InputObjectTypeDefinition::<SimpleSpan>::parse_str::<extra::Err<Simple<'_, char>>>(INPUT).unwrap();
}
