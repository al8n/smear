const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0031_variables_with_default.graphql");

#[test]
#[cfg(feature = "graphql")]
fn variables_with_default() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().source(), "getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);
  let mut variable_definitions = variable_definitions.iter();

  {
    let variable_definition = variable_definitions.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().source(),
      "input"
    );

    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");

    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(default_value.value().unwrap_int_ref().source(), "5");
  }

  {
    let variable_definition = variable_definitions.next().unwrap();
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
  }
}

#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_variables_with_default() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap(), "getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);
  let mut variable_definitions = variable_definitions.iter();

  {
    let variable_definition = variable_definitions.next().unwrap();
    assert_eq!(
      variable_definition.variable().name(),
      "input"
    );

    let ty = variable_definition.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "Int");

    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(default_value.value().unwrap_int_ref().value_ref().unwrap_decimal(), "5");
  }

  {
    let variable_definition = variable_definitions.next().unwrap();
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
  }
}
