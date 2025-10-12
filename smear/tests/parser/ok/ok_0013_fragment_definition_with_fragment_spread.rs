
const ALL: &str = include_str!("../../fixtures/parser/ok/0013_fragment_definition_with_fragment_spread.graphql");

#[test]
fn fragment_definition_with_fragment_spread() {
  use smear::parser::graphql::ast::{FragmentDefinition, ParseStr};

  let definition = FragmentDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "friendFields");

  let directives = definition.directives();
  assert!(directives.is_none());

  let type_condition = definition.type_condition();
  assert_eq!(type_condition.name().source(), "User");
  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
  let mut fields = selection_set.clone().into_selections().into_iter();

  {
    let id = fields.next().unwrap().unwrap_field();
    assert_eq!(id.name().source(), "id");
    assert!(id.selection_set().is_none());
  }

  {
    let name = fields.next().unwrap().unwrap_field();
    assert_eq!(name.name().source(), "name");
    assert!(name.selection_set().is_none());
  }

  {
    let profile_pic = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(profile_pic.name().source(), "standardProfilePic");
    assert!(profile_pic.directives().is_none());
  }
}

#[test]
fn graphqlx_fragment_definition_with_fragment_spread() {
  use smear::parser::graphqlx::ast::{FragmentDefinition, ParseStr};

  let definition = FragmentDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name(), "friendFields");

  let directives = definition.directives();
  assert!(directives.is_none());

  let type_condition = definition.type_condition();
  assert_eq!(type_condition.path(), "User");
  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
  let mut fields = selection_set.clone().into_selections().into_iter();

  {
    let id = fields.next().unwrap().unwrap_field();
    assert_eq!(id.name(), "id");
    assert!(id.selection_set().is_none());
  }

  {
    let name = fields.next().unwrap().unwrap_field();
    assert_eq!(name.name(), "name");
    assert!(name.selection_set().is_none());
  }

  {
    let profile_pic = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(profile_pic.path(), "standardProfilePic");
    assert!(profile_pic.directives().is_none());
  }
}
