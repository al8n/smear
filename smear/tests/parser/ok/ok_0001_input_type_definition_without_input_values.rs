use smear::parser::graphql::ast::{Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0001_input_type_definition_without_input_values.graphql");

#[test]
fn input_object_type_definition_without_input_values() {
  let definition =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let content = definition.definitions();
  assert_eq!(content.len(), 2);

  let mut iter = content.iter();

  {
    let described = iter
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref();
    let input = described
      .unwrap_type_ref()
      .unwrap_input_object_ref();

    assert_eq!(input.name().source(), "AnInputWithoutInputValues");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "An input with no input values"
    );
    assert!(input.fields_definition().is_none());
  }

  {
    let input = iter
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_input_object_ref();
    assert_eq!(input.name().source(), "AnInputWithoutInputValues");
    let input_values = input.fields_definition().unwrap();
    let input_value_definitions = input_values.input_value_definitions();
    assert_eq!(input_value_definitions.len(), 1);
    let input_value = &input_value_definitions[0];
    assert_eq!(input_value.name().source(), "limit");
    let ty = input_value.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");
    assert!(ty.required());
  }
}
