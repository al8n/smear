use smear_graphql::parser::ast::{SelectionSet, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0005_selection_with_inline_fragments.graphql");

#[test]
fn selection_with_inline_fragment() {
  let selection_set = SelectionSet::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(selection_set.selections().len(), 3);

  let mut fields = selection_set.into_selections().into_iter();

  {
    let animal = fields.next().unwrap().unwrap_field();
    assert_eq!(animal.name().slice(), "animal");
    assert!(animal.selection_set().is_none());
  }

  {
    let fave_snack = fields.next().unwrap().unwrap_field();
    assert_eq!(fave_snack.name().slice(), "faveSnack");
    assert!(fave_snack.selection_set().is_none());
  }

  {
    let pet = fields.next().unwrap().unwrap_inline_fragment();
    let tc = pet.type_condition().unwrap();
    assert_eq!(tc.name().slice(), "Pet");
    let spet = pet.selection_set();
    assert_eq!(spet.selections().len(), 1);

    let mut fields = spet.clone().into_selections().into_iter();
    let playmates = fields.next().unwrap().unwrap_field();
    assert_eq!(playmates.name().slice(), "playmates");

    let splaymates = playmates.selection_set().cloned().unwrap();
    {
      assert_eq!(splaymates.selections().len(), 1);

      let mut fields = splaymates.into_selections().into_iter();
      let count = fields.next().unwrap().unwrap_field();
      assert_eq!(count.name().slice(), "count");
      assert!(count.selection_set().is_none());
    }
  }

  assert!(fields.next().is_none());
}
