const ALL: &str = include_str!("../../../fixtures/parser/ok/0033_directive_on_argument_definition.graphql");

#[test]
fn directive_on_argument_definition() {
  use smear::parser::graphql::ast::{ObjectTypeDefinition, ParseStr};

  let values = ObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();

  let fields = values.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 1);
  let field = &fields[0];
  assert_eq!(field.name().source(), "login");

  let arguments = field
    .arguments_definition()
    .unwrap()
    .input_value_definitions();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().source(), "userId");

  let directives = argument.directives().unwrap().directives();
  assert_eq!(directives.len(), 1);
  let directive = &directives[0];
  assert_eq!(directive.name().source(), "deprecated");
  let arguments = directive.arguments().unwrap().arguments();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name().source(), "reason");
  let value = argument.value();
  assert_eq!(
    value.unwrap_string_ref().source().trim_matches('"'),
    "Use username instead"
  );
}

#[test]
fn graphqlx_directive_on_argument_definition() {
  use smear::parser::graphqlx::ast::{ObjectTypeDefinition, ParseStr};

  let values = ObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();

  let fields = values.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 1);
  let field = &fields[0];
  assert_eq!(field.name(), "login");

  let arguments = field
    .arguments_definition()
    .unwrap()
    .input_value_definitions();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name(), "userId");

  let directives = argument.directives().unwrap().directives();
  assert_eq!(directives.len(), 1);
  let directive = &directives[0];
  assert_eq!(directive.name(), "deprecated");
  let arguments = directive.arguments().unwrap().arguments();
  assert_eq!(arguments.len(), 1);
  let argument = &arguments[0];
  assert_eq!(argument.name(), "reason");
  let value = argument.value();
  assert_eq!(
    value.unwrap_string_ref().source().trim_matches('"'),
    "Use username instead"
  );
}
