use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0022_operation_type_definition_with_arguments_and_directives.graphql");

#[test]
fn operation_type_definition_with_arguments_and_directives() {
  let definition = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();
  assert_eq!(definition.name().unwrap().source(), "myQuery");
  assert_eq!(definition.operation_type().as_str(), "query");

  {
    let args = definition.variable_definitions().unwrap();
    assert_eq!(args.variable_definitions().len(), 2);
    let mut args = args.variable_definitions().iter();
    {
      let var = args.next().unwrap();
      assert_eq!(var.variable().name().source(), "var");
      let ty = var.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "input");
      assert!(!ty.required());
    }

    {
      let var_other = args.next().unwrap();
      assert_eq!(var_other.variable().name().source(), "varOther");
      let ty = var_other.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "otherInput");
      assert!(!ty.required());
    }
  }

  {
    let directives = definition.directives().cloned().unwrap();
    assert_eq!(directives.directives().len(), 2);
    let mut directives = directives.directives().iter();

    {
      let deprecated = directives.next().unwrap();
      assert_eq!(deprecated.name().source(), "deprecated");
      assert!(deprecated.arguments().is_none());
    }

    {
      let unused = directives.next().unwrap();
      assert_eq!(unused.name().source(), "unused");
      assert!(unused.arguments().is_none());
    }
  }

  {
    let selection_set = definition.selection_set();
    assert_eq!(selection_set.selections().len(), 2);
    let mut selections = selection_set.clone().into_selections().into_iter();

    {
      let animal = selections.next().unwrap().unwrap_field();
      assert_eq!(animal.name().source(), "animal");
      assert!(animal.alias().is_none());
      assert!(animal.arguments().is_none());
      assert!(animal.directives().is_none());
      assert!(animal.selection_set().is_none());
    }

    {
      let treat = selections.next().unwrap().unwrap_field();
      assert_eq!(treat.name().source(), "treat");
      assert!(treat.alias().is_none());
      assert!(treat.arguments().is_none());
      assert!(treat.directives().is_none());
      assert!(treat.selection_set().is_none());
    }
  }
}
