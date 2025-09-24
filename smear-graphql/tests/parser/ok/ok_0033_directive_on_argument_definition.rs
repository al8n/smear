use smear_graphql::parser::fast::{ObjectTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0033_directive_on_argument_definition.graphql");

#[test]
fn directive_on_argument_definition() {
  let values = ObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();

  let fields = values.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 1);
  let field = &fields[0];
  assert_eq!(field.name().slice(), "login");

  let arguments = field
    .arguments_definition()
    .unwrap()
    .input_value_definitions();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().slice(), "userId");

  let directives = argument.directives().unwrap().directives();
  assert_eq!(directives.len(), 1);
  let directive = &directives[0];
  assert_eq!(directive.name().slice(), "deprecated");
  let arguments = directive.arguments().unwrap().arguments();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().slice(), "reason");
  let value = argument.value();
  assert_eq!(
    value.unwrap_string_ref().content(),
    "Use username instead"
  );
}
