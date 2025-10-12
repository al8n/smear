const ALL: &str = include_str!("../../fixtures/parser/ok/0036_parses_variable_definition_with_list_type.graphql");

#[test]
fn parses_variable_definition_with_list_type() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
    .unwrap()
    .unwrap_named();

  assert!(values.name().is_none());

  {
    let variable_definitions = values
      .variable_definitions()
      .unwrap()
      .variable_definitions();
    assert_eq!(variable_definitions.len(), 1);
    let variable_definition = &variable_definitions[0];
    assert_eq!(
      variable_definition.variable().name().source(),
      "height"
    );
    let ty = variable_definition.ty().unwrap_list_ref();
    let inner_ty = ty.ty().unwrap_name_ref();
    assert_eq!(inner_ty.name().source(), "Int");
  }

  {
    let selection_set = values.selection_set();
    let selections = selection_set.selections();
    assert_eq!(selections.len(), 2);

    let mut fields = selections.iter();

    {
      let id = fields.next().unwrap().unwrap_field_ref();
      assert_eq!(id.name().source(), "id");
    }
    {
      let trees = fields.next().unwrap().unwrap_field_ref();
      assert_eq!(trees.name().source(), "trees");
      let arguments = trees.arguments().unwrap().arguments();
      assert_eq!(arguments.len(), 1);
      let argument = &arguments[0];
      assert_eq!(argument.name().source(), "height");
      let value = argument.value().unwrap_variable_ref();
      assert_eq!(value.name().source(), "height");
    }
  }
}


#[test]
fn graphqlx_parses_variable_definition_with_list_type() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
    .unwrap()
    .unwrap_named();

  assert!(values.name().is_none());

  {
    let variable_definitions = values
      .variable_definitions()
      .unwrap()
      .variable_definitions();
    assert_eq!(variable_definitions.len(), 1);
    let variable_definition = &variable_definitions[0];
    assert_eq!(
      variable_definition.variable().name().source(),
      "height"
    );
    let ty = variable_definition.ty().unwrap_list_ref();
    let inner_ty = ty.ty().unwrap_path_ref();
    assert_eq!(inner_ty.path(), "Int");
  }

  {
    let selection_set = values.selection_set();
    let selections = selection_set.selections();
    assert_eq!(selections.len(), 2);

    let mut fields = selections.iter();

    {
      let id = fields.next().unwrap().unwrap_field_ref();
      assert_eq!(id.name(), "id");
    }
    {
      let trees = fields.next().unwrap().unwrap_field_ref();
      assert_eq!(trees.name(), "trees");
      let arguments = trees.arguments().unwrap().arguments();
      assert_eq!(arguments.len(), 1);
      let argument = &arguments[0];
      assert_eq!(argument.name(), "height");
      let value = argument.value().unwrap_variable_ref();
      assert_eq!(value.name(), "height");
    }
  }
}
