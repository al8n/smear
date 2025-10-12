
const ALL: &str = include_str!("../../fixtures/parser/ok/0002_selection_simple.graphql");

#[test]
fn selection_simple() {
  use smear::parser::graphql::ast::{SelectionSet, ParseStr};

  let selection_set = SelectionSet::<&str>::parse_str(ALL).unwrap();

  assert_eq!(selection_set.selections().len(), 2);

  let mut fields = selection_set.into_selections().into_iter();
  let pet = fields.next().unwrap().unwrap_field();
  let fave_snack = fields.next().unwrap().unwrap_field();
  assert!(fields.next().is_none());

  assert_eq!(pet.name().source(), "pet");
  assert!(pet.selection_set().is_none());
  assert_eq!(fave_snack.name().source(), "faveSnack");
  assert!(fave_snack.selection_set().is_none());
}


#[test]
fn graphqlx_selection_simple() {
  use smear::parser::graphqlx::ast::{SelectionSet, ParseStr};

  let selection_set = SelectionSet::<&str>::parse_str(ALL).unwrap();

  assert_eq!(selection_set.selections().len(), 2);

  let mut fields = selection_set.into_selections().into_iter();
  let pet = fields.next().unwrap().unwrap_field();
  let fave_snack = fields.next().unwrap().unwrap_field();
  assert!(fields.next().is_none());

  assert_eq!(pet.name(), "pet");
  assert!(pet.selection_set().is_none());
  assert_eq!(fave_snack.name(), "faveSnack");
  assert!(fave_snack.selection_set().is_none());
}
