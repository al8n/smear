use smear_graphql::parser::fast::{OperationDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0021_operation_type_definition_with_arguments.graphql");

#[test]
fn operation_type_definition_with_arguments() {
  let definition = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();
  assert_eq!(definition.name().unwrap().slice(), "myQuery");
  assert_eq!(definition.operation_type().as_str(), "query");

  let args = definition.variable_definitions().unwrap();
  assert_eq!(args.variable_definitions().len(), 2);
  let mut args = args.variable_definitions().iter();
  {
    let var = args.next().unwrap();
    assert_eq!(var.variable().name().slice(), "var");
    let ty = var.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "input");
    assert!(!ty.required());
  }

  {
    let var_other = args.next().unwrap();
    assert_eq!(var_other.variable().name().slice(), "varOther");
    let ty = var_other.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "otherInput");
    assert!(!ty.required());
  }

  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 2);
  let mut selections = selection_set.clone().into_selections().into_iter();

  {
    let animal = selections.next().unwrap().unwrap_field();
    assert_eq!(animal.name().slice(), "animal");
    assert!(animal.alias().is_none());
    assert!(animal.arguments().is_none());
    assert!(animal.directives().is_none());
    assert!(animal.selection_set().is_none());
  }

  {
    let treat = selections.next().unwrap().unwrap_field();
    assert_eq!(treat.name().slice(), "treat");
    assert!(treat.alias().is_none());
    assert!(treat.arguments().is_none());
    assert!(treat.directives().is_none());
    assert!(treat.selection_set().is_none());
  }
}
