const ALL: &str = include_str!("../../fixtures/parser/ok/0039_variable_with_directives.graphql");

#[test]
fn variable_with_directives() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL).unwrap().unwrap_named();

  assert_eq!(values.name().unwrap().source(), "getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);

  let mut iter = variable_definitions.iter();

  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().source(),
      "input"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");
    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().source(), "deprecated");
  }
  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().source(),
      "config"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "String");
    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(
      default_value
        .value()
        .unwrap_string_ref()
        .source().trim_matches('"'),
      "Config"
    );

    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().source(), "tag");
    let arguments = directive.arguments().unwrap().arguments();
    assert_eq!(arguments.len(), 1);
    let argument = &arguments[0];
    assert_eq!(argument.name().source(), "name");
    let value = argument.value().unwrap_string_ref();
    assert_eq!(value.source().trim_matches('"'), "team-customers");
  }
}

#[test]
fn graphqlx_variable_with_directives() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL).unwrap().unwrap_named();

  assert_eq!(values.name().unwrap(), "getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);

  let mut iter = variable_definitions.iter();

  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name(),
      "input"
    );
    let ty = variable_definition.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "Int");
    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name(), "deprecated");
  }
  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name(),
      "config"
    );
    let ty = variable_definition.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "String");
    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(
      default_value
        .value()
        .unwrap_string_ref()
        .source().trim_matches('"'),
      "Config"
    );

    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name(), "tag");
    let arguments = directive.arguments().unwrap().arguments();
    assert_eq!(arguments.len(), 1);
    let argument = &arguments[0];
    assert_eq!(argument.name(), "name");
    let value = argument.value().unwrap_string_ref();
    assert_eq!(value.source().trim_matches('"'), "team-customers");
  }
}

