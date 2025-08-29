use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0033_directive_on_argument_definition.graphql");

#[test]
fn directive_on_argument_definition() {
  let values = ObjectTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  let fields = values.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 1);
  let field = &fields[0];
  assert_eq!(field.name().span().source(), &"login");

  let arguments = field
    .arguments_definition()
    .unwrap()
    .input_value_definitions();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().span().source(), &"userId");

  let directives = argument.directives().unwrap().directives();
  assert_eq!(directives.len(), 1);
  let directive = &directives[0];
  assert_eq!(directive.name().span().source(), &"deprecated");
  let arguments = directive.arguments().unwrap().arguments();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().span().source(), &"reason");
  let value = argument.value();
  assert_eq!(
    value.unwrap_string_ref().content().span().source(),
    &"Use username instead"
  );
}
