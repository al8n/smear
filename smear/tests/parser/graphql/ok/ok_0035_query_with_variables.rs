

const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0035_query_with_variables.graphql");

#[test]
#[cfg(feature = "graphql")]
fn query_with_variables() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().source(), "Foo");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 1);
  let variable_definition = &variable_definitions[0];
  assert_eq!(
    variable_definition.variable().name().source(),
    "bar"
  );
  let ty = variable_definition.ty().unwrap_name_ref();
  assert_eq!(ty.name().source(), "Int");
}


#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_query_with_variables() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap(), "Foo");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 1);
  let variable_definition = &variable_definitions[0];
  assert_eq!(
    variable_definition.variable().name(),
    "bar"
  );
  let ty = variable_definition.ty().unwrap_path_ref();
  assert_eq!(ty.path(), "Int");
}
