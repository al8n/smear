use smear_graphql::parser::ast::{raw::DirectiveDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0009_directive_definition_repeatable.graphql");

#[test]
fn directive_definition_repeatable() {
  let definition = DirectiveDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "example");
  let locations = definition.locations();
  let locations = locations.locations();
  assert_eq!(locations.len(), 2);
  assert!(locations[0]
    .unwrap_executable_ref()
    .is_field());
  assert!(locations[1]
    .unwrap_executable_ref()
    .is_mutation());
  assert!(definition.repeatable());

  let args = definition.arguments_definition().unwrap();
  let mut iter = args.input_value_definitions().iter();

  {
    let is_treat = iter.next().unwrap();
    assert_eq!(is_treat.name().slice(), "isTreat");
    let ty = is_treat.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "Boolean");
    assert!(!ty.required());
  }

  {
    let treat_kind = iter.next().unwrap();
    assert_eq!(treat_kind.name().slice(), "treatKind");
    let ty = treat_kind.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "String");
    assert!(!ty.required());
  }

  assert!(iter.next().is_none());
}
