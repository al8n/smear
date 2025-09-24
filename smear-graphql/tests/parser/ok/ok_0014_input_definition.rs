use smear_graphql::parser::ast::{DescribedInputObjectTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0014_input_definition.graphql");

#[test]
fn input_object_definition() {
  let definition = DescribedInputObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "ExampleInputObject");

  let input_fields = definition.fields_definition().cloned().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 2);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().slice(), "a");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().slice(), "String");
    assert!(!ty.required());
  }

  {
    let b = fields.next().unwrap();
    assert_eq!(b.name().slice(), "b");
    let ty = b.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "Int");
    assert!(ty.required());
  }
}
