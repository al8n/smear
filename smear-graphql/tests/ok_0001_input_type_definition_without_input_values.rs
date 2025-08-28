use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###""An input with no input values"
input AnInputWithoutInputValues

extend input AnInputWithoutInputValues {
  limit: Int!
}
"###;

const INPUT: &str = r###""An input with no input values"
input AnInputWithoutInputValues
"###;

const INPUT_VALUE: &str = r###"limit: Int!"###;

#[test]
fn input_value_definition() {
  let definition = InputValueDefinition::<WithSource<&str, SimpleSpan>>::parse_str::<
    extra::Err<Simple<'_, char>>,
  >(INPUT_VALUE)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"limit");
  let ty = definition.ty().unwrap_name_ref();
  assert_eq!(ty.name().span().source(), &"Int");
  assert!(ty.bang().is_some());
}

#[test]
fn input_object_type_definition_without_input_values() {
  let definition = InputObjectTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(INPUT)
  .unwrap();
  assert_eq!(
    definition.name().span().source(),
    &"AnInputWithoutInputValues"
  );
  assert!(definition.fields().is_none());
}

// #[test]
// fn input_object_type_definition() {
//   let definition = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Simple<'_, char>>>(ALL).unwrap();
//   // assert_eq!(definition.name().span().source(), &"AnInputWithoutInputValues");
//   // assert!(definition.fields().is_none());
// }
