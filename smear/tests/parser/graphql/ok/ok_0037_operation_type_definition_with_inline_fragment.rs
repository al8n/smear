const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0037_operation_type_definition_with_inline_fragment.graphql");

#[test]
fn operation_type_definition_with_inline_fragment() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().source(), "SomeQuery");

  {
    let variable_definitions = values
      .variable_definitions()
      .unwrap()
      .variable_definitions();
    assert_eq!(variable_definitions.len(), 2);
    let mut vars = variable_definitions.iter();

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().source(), "param1");
      let ty = var.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "String");
      assert!(ty.required());
    }

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().source(), "param2");
      let ty = var.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "String");
      assert!(ty.required());
    }
  }

  {
    let selection_set = values.selection_set();
    let selections = selection_set.selections();
    assert_eq!(selections.len(), 1);

    let items = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(items.name().source(), "item1");

    {
      let arguments = items.arguments().unwrap().arguments();
      assert_eq!(arguments.len(), 2);
      let mut args = arguments.iter();

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name().source(), "param1");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name().source(), "param1");
      }

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name().source(), "param2");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name().source(), "param2");
      }
    }

    {
      let item_selections = items.selection_set().unwrap().selections();
      assert_eq!(item_selections.len(), 2);

      let mut item_fields = item_selections.iter();

      {
        let id = item_fields.next().unwrap().unwrap_field_ref();
        assert_eq!(id.name().source(), "id");
      }

      {
        let inline_fragment = item_fields.next().unwrap().unwrap_inline_fragment_ref();
        let type_condition = inline_fragment.type_condition().unwrap();
        assert_eq!(type_condition.name().source(), "Fragment1");

        let inline_fragment_selections = inline_fragment.selection_set().selections();
        assert_eq!(inline_fragment_selections.len(), 1);

        let field3 = inline_fragment_selections
          .first()
          .unwrap()
          .unwrap_field_ref();
        assert_eq!(field3.name().source(), "field3");

        {
          let field3_selections = field3.selection_set().unwrap().selections();
          assert_eq!(field3_selections.len(), 1);

          let field4 = field3_selections.first().unwrap().unwrap_field_ref();
          assert_eq!(field4.name().source(), "field4");
        }
      }
    }
  }
}

#[test]
fn graphqlx_operation_type_definition_with_inline_fragment() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let values = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().source(), "SomeQuery");

  {
    let variable_definitions = values
      .variable_definitions()
      .unwrap()
      .variable_definitions();
    assert_eq!(variable_definitions.len(), 2);
    let mut vars = variable_definitions.iter();

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().source(), "param1");
      let ty = var.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "String");
      assert!(ty.required());
    }

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().source(), "param2");
      let ty = var.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "String");
      assert!(ty.required());
    }
  }

  {
    let selection_set = values.selection_set();
    let selections = selection_set.selections();
    assert_eq!(selections.len(), 1);

    let items = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(items.name(), "item1");

    {
      let arguments = items.arguments().unwrap().arguments();
      assert_eq!(arguments.len(), 2);
      let mut args = arguments.iter();

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name(), "param1");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name(), "param1");
      }

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name(), "param2");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name(), "param2");
      }
    }

    {
      let item_selections = items.selection_set().unwrap().selections();
      assert_eq!(item_selections.len(), 2);

      let mut item_fields = item_selections.iter();

      {
        let id = item_fields.next().unwrap().unwrap_field_ref();
        assert_eq!(id.name(), "id");
      }

      {
        let inline_fragment = item_fields.next().unwrap().unwrap_inline_fragment_ref();
        let type_condition = inline_fragment.type_condition().unwrap();
        assert_eq!(type_condition.path(), "Fragment1");

        let inline_fragment_selections = inline_fragment.selection_set().selections();
        assert_eq!(inline_fragment_selections.len(), 1);

        let field3 = inline_fragment_selections
          .first()
          .unwrap()
          .unwrap_field_ref();
        assert_eq!(field3.name(), "field3");

        {
          let field3_selections = field3.selection_set().unwrap().selections();
          assert_eq!(field3_selections.len(), 1);

          let field4 = field3_selections.first().unwrap().unwrap_field_ref();
          assert_eq!(field4.name(), "field4");
        }
      }
    }
  }
}
