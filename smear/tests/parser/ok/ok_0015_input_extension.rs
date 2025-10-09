use smear::parser::graphql::ast::{InputObjectTypeExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0015_input_extension.graphql");

#[test]
fn input_object_extension() {
  let extension = InputObjectTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.name().source(), "ExampleInputObject");

  let input_fields = extension.fields_definition().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 1);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().source(), "a");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "String");
    assert!(!ty.required(), "Source: {ALL}");
  }
}
