use smear_graphql::parser::fast::{OperationDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0039_variable_with_directives.graphql");

#[test]
fn variable_with_directives() {
  let values = OperationDefinition::<&str>::parse_str(ALL).unwrap().unwrap_named();

  assert_eq!(values.name().unwrap().slice(), "getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);

  let mut iter = variable_definitions.iter();

  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().slice(),
      "input"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "Int");
    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().slice(), "deprecated");
  }
  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().slice(),
      "config"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "String");
    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(
      default_value
        .value()
        .unwrap_string_ref()
        .content(),
      "Config"
    );

    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().slice(), "tag");
    let arguments = directive.arguments().unwrap().arguments();
    assert_eq!(arguments.len(), 1);
    let argument = &arguments[0];
    assert_eq!(argument.name().slice(), "name");
    let value = argument.value().unwrap_string_ref();
    assert_eq!(value.content(), "team-customers");
  }
}
