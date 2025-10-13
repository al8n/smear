
const ALL: &str = include_str!("../../../fixtures/parser/ok/0005_selection_with_inline_fragments.graphql");

#[test]
fn selection_with_inline_fragment() {
  use smear::parser::graphql::ast::{SelectionSet, ParseStr};

  let selection_set = SelectionSet::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(selection_set.selections().len(), 3);

  let mut fields = selection_set.into_selections().into_iter();

  {
    let animal = fields.next().unwrap().unwrap_field();
    assert_eq!(animal.name().source(), "animal");
    assert!(animal.selection_set().is_none());
  }

  {
    let fave_snack = fields.next().unwrap().unwrap_field();
    assert_eq!(fave_snack.name().source(), "faveSnack");
    assert!(fave_snack.selection_set().is_none());
  }

  {
    let pet = fields.next().unwrap().unwrap_inline_fragment();
    let tc = pet.type_condition().unwrap();
    assert_eq!(tc.name().source(), "Pet");
    let spet = pet.selection_set();
    assert_eq!(spet.selections().len(), 1);

    let mut fields = spet.clone().into_selections().into_iter();
    let playmates = fields.next().unwrap().unwrap_field();
    assert_eq!(playmates.name().source(), "playmates");

    let splaymates = playmates.selection_set().cloned().unwrap();
    {
      assert_eq!(splaymates.selections().len(), 1);

      let mut fields = splaymates.into_selections().into_iter();
      let count = fields.next().unwrap().unwrap_field();
      assert_eq!(count.name().source(), "count");
      assert!(count.selection_set().is_none());
    }
  }

  assert!(fields.next().is_none());
}

#[test]
fn graphqlx_selection_with_inline_fragment() {
  use smear::parser::graphqlx::ast::{SelectionSet, ParseStr};

  let selection_set = SelectionSet::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(selection_set.selections().len(), 3);

  let mut fields = selection_set.into_selections().into_iter();

  {
    let animal = fields.next().unwrap().unwrap_field();
    assert_eq!(animal.name(), "animal");
    assert!(animal.selection_set().is_none());
  }

  {
    let fave_snack = fields.next().unwrap().unwrap_field();
    assert_eq!(fave_snack.name(), "faveSnack");
    assert!(fave_snack.selection_set().is_none());
  }

  {
    let pet = fields.next().unwrap().unwrap_inline_fragment();
    let tc = pet.type_condition().unwrap();
    assert_eq!(tc.path(), "Pet");
    let spet = pet.selection_set();
    assert_eq!(spet.selections().len(), 1);

    let mut fields = spet.clone().into_selections().into_iter();
    let playmates = fields.next().unwrap().unwrap_field();
    assert_eq!(playmates.name(), "playmates");

    let splaymates = playmates.selection_set().cloned().unwrap();
    {
      assert_eq!(splaymates.selections().len(), 1);

      let mut fields = splaymates.into_selections().into_iter();
      let count = fields.next().unwrap().unwrap_field();
      assert_eq!(count.name(), "count");
      assert!(count.selection_set().is_none());
    }
  }

  assert!(fields.next().is_none());
}
