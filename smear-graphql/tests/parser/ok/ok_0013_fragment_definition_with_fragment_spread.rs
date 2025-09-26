use smear_graphql::parser::ast::{FragmentDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0013_fragment_definition_with_fragment_spread.graphql");

#[test]
fn fragment_definition_with_fragment_spread() {
  let definition = FragmentDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "friendFields");

  let directives = definition.directives();
  assert!(directives.is_none());

  let type_condition = definition.type_condition();
  assert_eq!(type_condition.name().slice(), "User");
  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
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

  {
    let profile_pic = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(profile_pic.name().slice(), "standardProfilePic");
    assert!(profile_pic.directives().is_none());
  }
}
