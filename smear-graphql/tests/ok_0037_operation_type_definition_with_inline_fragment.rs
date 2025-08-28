use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
query SomeQuery(
  $param1: String!
  $param2: String!
) {
  item1(
    param1: $param1
    param2: $param2
  ) {
    id
    ... on Fragment1 {
      field3 {
        field4
      }
    }
  }
}
"###;

#[test]
fn opeartion_type_definition_with_inline_fragment() {
  let values = OperationDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().span().source(), &"SomeQuery");

  {
    let variable_definitions = values
      .variable_definitions()
      .unwrap()
      .variable_definitions();
    assert_eq!(variable_definitions.len(), 2);
    let mut vars = variable_definitions.iter();

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().span().source(), &"param1");
      let ty = var.ty().unwrap_name_ref();
      assert_eq!(ty.name().span().source(), &"String");
      assert!(ty.bang().is_some());
    }

    {
      let var = vars.next().unwrap();
      assert_eq!(var.variable().name().span().source(), &"param2");
      let ty = var.ty().unwrap_name_ref();
      assert_eq!(ty.name().span().source(), &"String");
      assert!(ty.bang().is_some());
    }
  }

  {
    let selection_set = values.selection_set();
    let selections = selection_set.selections();
    assert_eq!(selections.len(), 1);

    let items = selections.first().unwrap().unwrap_field_ref();
    assert_eq!(items.name().span().source(), &"item1");

    {
      let arguments = items.arguments().unwrap().arguments();
      assert_eq!(arguments.len(), 2);
      let mut args = arguments.iter();

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name().span().source(), &"param1");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name().span().source(), &"param1");
      }

      {
        let arg = args.next().unwrap();
        assert_eq!(arg.name().span().source(), &"param2");
        let value = arg.value().unwrap_variable_ref();
        assert_eq!(value.name().span().source(), &"param2");
      }
    }

    {
      let item_selections = items.selection_set().unwrap().selections();
      assert_eq!(item_selections.len(), 2);

      let mut item_fields = item_selections.iter();

      {
        let id = item_fields.next().unwrap().unwrap_field_ref();
        assert_eq!(id.name().span().source(), &"id");
      }

      {
        let inline_fragment = item_fields.next().unwrap().unwrap_inline_fragment_ref();
        let type_condition = inline_fragment.type_condition().unwrap();
        assert_eq!(type_condition.name().span().source(), &"Fragment1");

        let inline_fragment_selections = inline_fragment.selection_set().selections();
        assert_eq!(inline_fragment_selections.len(), 1);

        let field3 = inline_fragment_selections
          .first()
          .unwrap()
          .unwrap_field_ref();
        assert_eq!(field3.name().span().source(), &"field3");

        {
          let field3_selections = field3.selection_set().unwrap().selections();
          assert_eq!(field3_selections.len(), 1);

          let field4 = field3_selections.first().unwrap().unwrap_field_ref();
          assert_eq!(field4.name().span().source(), &"field4");
        }
      }
    }
  }
}
