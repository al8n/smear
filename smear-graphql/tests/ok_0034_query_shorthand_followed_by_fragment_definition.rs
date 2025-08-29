use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
{
  ...friendFields
}

fragment friendFields on User {
  id
  name
}
"###;

#[test]
fn query_shorthand_followed_by_fragment_definition() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = document.content();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  {
    let query = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_operation_ref()
      .unwrap_shorthand_ref();
    let selections = query.selections();
    assert_eq!(selections.len(), 1);
    let selection = selections.first().unwrap().unwrap_fragment_spread_ref();
    assert_eq!(selection.name().span().source(), &"friendFields");
    assert!(selection.directives().is_none());
  }

  {
    let definition = iter
      .next()
      .unwrap()
      .unwrap_executable_ref()
      .unwrap_fragment_ref();
    assert_eq!(definition.name().span().source(), &"friendFields");
    let type_condition = definition.type_condition();
    assert_eq!(type_condition.name().span().source(), &"User");
    assert!(definition.directives().is_none());
    let selection_set = definition.selection_set();
    assert_eq!(selection_set.selections().len(), 2);
    let mut fields = selection_set.clone().into_selections().into_iter();

    {
      let id = fields.next().unwrap().unwrap_field();
      assert_eq!(id.name().span().source(), &"id");
      assert!(id.selection_set().is_none());
    }

    {
      let name = fields.next().unwrap().unwrap_field();
      assert_eq!(name.name().span().source(), &"name");
      assert!(name.selection_set().is_none());
    }
  }
}
