
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0015_input_extension.graphql");

#[cfg(feature = "graphql")]
#[test]
fn input_object_extension() {
  use smear::parser::graphql::ast::{InputObjectTypeExtension, ParseStr};

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


#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_input_object_extension() {
  use smear::parser::graphqlx::ast::{InputObjectTypeExtension, ParseStr};

  let extension = InputObjectTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.path(), "ExampleInputObject");

  let input_fields = extension.fields_definition().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 1);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name(), "a");
    let ty = a.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "String");
    assert!(!ty.required(), "Source: {ALL}");
  }
}
