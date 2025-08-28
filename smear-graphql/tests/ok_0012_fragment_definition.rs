use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
fragment friendFields on User @example {
    id
    name
    profilePic(size: 50)
}
"###;

const CONTENT: &str = r###"
friendFields on User @example {
    id
    name
    profilePic(size: 50)
}
"###;

#[test]
fn fragment_definition_content() {
  let definition = FragmentDefinitionContent::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<'_, char>>,
  >(CONTENT)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"friendFields");

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().span().source(), &"example");
  assert!(directive.arguments().is_none());

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
    let profile_pic = fields.next().unwrap().unwrap_field();
    assert_eq!(profile_pic.name().span().source(), &"profilePic");
    assert!(profile_pic.selection_set().is_none());

    let arguments = profile_pic.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 1);
    let argument = arguments.arguments().first().unwrap();
    assert_eq!(argument.name().span().source(), &"size");
    let value = argument.value();
    assert_eq!(value.unwrap_int_ref().span().source(), &"50");
  }
}

#[test]
fn fragment_definition() {
  let definition = FragmentDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"friendFields");

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().span().source(), &"example");
  assert!(directive.arguments().is_none());

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
    let profile_pic = fields.next().unwrap().unwrap_field();
    assert_eq!(profile_pic.name().span().source(), &"profilePic");
    assert!(profile_pic.selection_set().is_none());

    let arguments = profile_pic.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 1);
    let argument = arguments.arguments().first().unwrap();
    assert_eq!(argument.name().span().source(), &"size");
    let value = argument.value();
    assert_eq!(value.unwrap_int_ref().span().source(), &"50");
  }
}
