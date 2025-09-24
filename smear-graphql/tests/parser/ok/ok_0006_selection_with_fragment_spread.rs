use smear_graphql::parser::fast::{SelectionSet, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0006_selection_with_fragment_spread.graphql");

#[test]
fn selection_with_fragment_spread() {
  let selection_set = SelectionSet::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(selection_set.selections().len(), 6);

  let mut fields = selection_set.into_selections().into_iter();

  {
    let pet = fields.next().unwrap().unwrap_field();
    assert_eq!(pet.name().slice(), "pet");
    assert!(pet.selection_set().is_none());
  }

  {
    let snack_selection = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(snack_selection.name().slice(), "snackSelection");
  }

  {
    let nap = fields.next().unwrap().unwrap_inline_fragment();
    let tc = nap.type_condition().unwrap();
    assert_eq!(tc.name().slice(), "Nap");
    let snap = nap.selection_set();
    assert_eq!(snap.selections().len(), 2);

    let mut fields = snap.clone().into_selections().into_iter();
    let cozy_location = fields.next().unwrap().unwrap_field();
    assert_eq!(cozy_location.name().slice(), "cozyLocation");
    assert!(cozy_location.selection_set().is_none());

    let duration_of_nap = fields.next().unwrap().unwrap_field();
    assert_eq!(duration_of_nap.name().slice(), "durationOfNap");
    assert!(duration_of_nap.selection_set().is_none());
  }

  {
    let snack_selection = fields.next().unwrap().unwrap_fragment_spread();
    assert_eq!(snack_selection.name().slice(), "snackSelection");
    let directives = snack_selection
      .directives()
      .expect("should have directives");
    assert_eq!(directives.directives().len(), 1);
    let deprecated = directives.directives().first().unwrap();
    assert_eq!(deprecated.name().slice(), "deprecated");
  }

  {
    let nap = fields.next().unwrap().unwrap_inline_fragment();
    let tc = nap.type_condition().unwrap();
    assert_eq!(tc.name().slice(), "Nap");
    let snap = nap.selection_set();
    assert_eq!(snap.selections().len(), 1);

    let mut fields = snap.clone().into_selections().into_iter();
    let cozy_location = fields.next().unwrap().unwrap_field();
    assert_eq!(cozy_location.name().slice(), "cozyLocation");
    assert!(cozy_location.selection_set().is_none());

    let directives = nap.directives().expect("should have directives");
    assert_eq!(directives.directives().len(), 1);
    let provides = directives.directives().first().unwrap();
    assert_eq!(provides.name().slice(), "provides");
    let args = provides.arguments().expect("should have arguments");
    assert_eq!(args.arguments().len(), 1);
    let duration = args.arguments().first().unwrap();
    assert_eq!(duration.name().slice(), "duration");
    let value = duration.value();
    assert!(value.is_string());
    assert_eq!(
      value.unwrap_string_ref().content(),
      "2 hours"
    );
  }

  {
    let anon = fields.next().unwrap().unwrap_inline_fragment();
    assert!(anon.type_condition().is_none());
    let directives = anon.directives().expect("should have directives");
    assert_eq!(directives.directives().len(), 1);
    let j = directives.directives().first().unwrap();
    assert_eq!(j.name().slice(), "J");
    let args = j.arguments().expect("should have arguments");
    assert_eq!(args.arguments().len(), 1);
    let n = args.arguments().first().unwrap();
    assert_eq!(n.name().slice(), "N");
    let value = n.value();
    assert!(value.is_int());
    assert_eq!(value.unwrap_int_ref().slice(), "0");

    let sanon = anon.selection_set();
    assert_eq!(sanon.selections().len(), 1);
    let mut fields = sanon.clone().into_selections().into_iter();
    let a = fields.next().unwrap().unwrap_field();
    assert_eq!(a.name().slice(), "a");
    assert!(a.selection_set().is_none());
  }

  assert!(fields.next().is_none());
}
