use smear_graphql::parser::ast::{raw::FragmentDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0012_fragment_definition.graphql");

#[test]
fn fragment_definition() {
  let definition = FragmentDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "friendFields");

  let directives = definition.directives().cloned().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().slice(), "example");
  assert!(directive.arguments().is_none());

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
    let profile_pic = fields.next().unwrap().unwrap_field();
    assert_eq!(profile_pic.name().slice(), "profilePic");
    assert!(profile_pic.selection_set().is_none());

    let arguments = profile_pic.arguments().cloned().unwrap();
    assert_eq!(arguments.arguments().len(), 1);
    let argument = arguments.arguments().first().unwrap();
    assert_eq!(argument.name().slice(), "size");
    let value = argument.value();
    assert_eq!(value.unwrap_int_ref().slice(), "50");
  }
}
