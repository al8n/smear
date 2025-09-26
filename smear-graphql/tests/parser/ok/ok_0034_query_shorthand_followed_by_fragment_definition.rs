use smear_graphql::parser::ast::{ExecutableDocument, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0034_query_shorthand_followed_by_fragment_definition.graphql");

#[test]
fn query_shorthand_followed_by_fragment_definition() {
  let document =
    ExecutableDocument::<&str>::parse_str(ALL)
      .unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  {
    let query = iter
      .next()
      .unwrap()
      .unwrap_operation_ref()
      .unwrap_shorthand_ref();
    let selections = query.selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.name().slice(), "friendFields");
    assert!(selection.directives().is_none());
  }

  {
    let definition = iter
      .next()
      .unwrap()
      .unwrap_fragment_ref();
    assert_eq!(definition.name().slice(), "friendFields");
    let type_condition = definition.type_condition();
    assert_eq!(type_condition.name().slice(), "User");
    assert!(definition.directives().is_none());
    let selection_set = definition.selection_set();
    assert_eq!(selection_set.selections().len(), 2);
    let mut fields = selection_set.clone().into_selections().into_iter();

    {
      let id = fields.next().unwrap().unwrap_field();
      assert_eq!(id.name().slice(), "id");
      assert!(id.selection_set().is_none());
    }

    {
      let name = fields.next().unwrap().unwrap_field();
      assert_eq!(name.name().slice(), "name");
      assert!(name.selection_set().is_none());
    }
  }
}
