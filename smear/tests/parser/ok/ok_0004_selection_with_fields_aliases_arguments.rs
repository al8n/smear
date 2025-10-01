use smear::parser::graphql::ast::{SelectionSet, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0004_selection_with_fields_aliases_arguments.graphql");

#[test]
fn selection_with_fields_aliases_arguments() {
  let selection_set = SelectionSet::<&str>::parse_str(ALL).unwrap();
  assert_eq!(selection_set.selections().len(), 2);

  let mut fields = selection_set.into_selections().into_iter();
  let pet = fields.next().unwrap().unwrap_field();
  let fave_snack = fields.next().unwrap().unwrap_field();
  assert!(fields.next().is_none());

  assert_eq!(pet.name().slice(), "pet");

  {
    let spet = pet.selection_set().cloned().unwrap();
    assert_eq!(spet.selections().len(), 3);

    let mut fields = spet.into_selections().into_iter();
    let name = fields.next().unwrap().unwrap_field();
    assert_eq!(name.alias().unwrap().name().slice(), "name");
    assert_eq!(name.name().slice(), "nickname");
    assert!(name.selection_set().is_none());

    let birthday = fields.next().unwrap().unwrap_field();
    assert_eq!(birthday.selection_set().unwrap().selections().len(), 2);

    {
      let mut fields = birthday
        .selection_set()
        .cloned()
        .unwrap()
        .into_selections()
        .into_iter();
      let month = fields.next().unwrap().unwrap_field();
      assert!(month.selection_set().is_none());

      let day = fields.next().unwrap().unwrap_field();
      assert!(day.selection_set().is_none());
    }

    let playmates = fields.next().unwrap().unwrap_field();
    assert_eq!(playmates.selection_set().unwrap().selections().len(), 2);

    {
      let mut fields = playmates
        .selection_set()
        .cloned()
        .unwrap()
        .into_selections()
        .into_iter();
      let name = fields.next().unwrap().unwrap_field();
      assert!(name.selection_set().is_none());

      let fave_snack = fields.next().unwrap().unwrap_field();
      assert!(fave_snack.selection_set().is_none());
    }
  }

  assert_eq!(fave_snack.name().slice(), "faveSnack");
  assert!(fave_snack.selection_set().is_none());

  let fave_snack_args = fave_snack.arguments().expect("expect arguments");
  assert_eq!(fave_snack_args.arguments().len(), 1, "expect one argument");

  let arg1 = &fave_snack_args.arguments()[0];
  assert_eq!(arg1.name().slice(), "quantity");
  assert_eq!(arg1.value().unwrap_int_ref().slice(), "4");
}
