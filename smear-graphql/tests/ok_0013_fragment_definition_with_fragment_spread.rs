use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
fragment friendFields on User {
    id
    name
    ...standardProfilePic
}
"###;

#[test]
fn fragment_definition_with_fragment_spread() {
  let definition = FragmentDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"friendFields");

  let directives = definition.directives();
  assert!(directives.is_none());

  let type_condition = definition.type_condition();
  assert_eq!(type_condition.name().span().source(), &"User");
  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
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

  {
    let profile_pic = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(profile_pic.name().span().source(), &"standardProfilePic");
    assert!(profile_pic.directives().is_none());
  }
}
