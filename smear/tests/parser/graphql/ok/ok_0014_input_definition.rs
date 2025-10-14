
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0014_input_definition.graphql");

#[test]
fn input_object_definition() {
  use smear::parser::graphql::ast::{DescribedInputObjectTypeDefinition, ParseStr};

  let definition = DescribedInputObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "ExampleInputObject");

  let input_fields = definition.fields_definition().cloned().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 2);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().source(), "a");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "String");
    assert!(!ty.required());
  }

  {
    let b = fields.next().unwrap();
    assert_eq!(b.name().source(), "b");
    let ty = b.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");
    assert!(ty.required());
  }
}

#[test]
fn graphqlx_input_object_definition() {
  use smear::parser::graphqlx::ast::{DescribedInputObjectTypeDefinition, ParseStr};

  let definition = DescribedInputObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "ExampleInputObject");

  let input_fields = definition.fields_definition().cloned().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 2);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name(), "a");
    let ty = a.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "String");
    assert!(!ty.required());
  }

  {
    let b = fields.next().unwrap();
    assert_eq!(b.name(), "b");
    let ty = b.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "Int");
    assert!(ty.required());
  }
}
