use smear_graphql::parser::ast::{raw::OperationDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0035_query_with_variables.graphql");

#[test]
fn query_with_variables() {
  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().slice(), "Foo");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 1);
  let variable_definition = &variable_definitions[0];
  assert_eq!(
    variable_definition.variable().name().slice(),
    "bar"
  );
  let ty = variable_definition.ty().unwrap_name_ref();
  assert_eq!(ty.name().slice(), "Int");
}
