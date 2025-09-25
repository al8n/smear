use smear_graphql::parser::ast::{raw::Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0042_object_type_definition_without_fields.graphql");

#[test]
fn object_type_definition_without_fields() {
  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref();

    let object = described
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().slice(), "AnObjectTypeWithoutFields");
    assert_eq!(
      described.description().unwrap().content(),
      "A type with no fields"
    );
    assert!(object.fields_definition().is_none());
  }

  {
    let object = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name().slice(), "AnObjectTypeWithoutFields");
    let fields = object.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 1);
    let field = &field_definitions[0];
    assert_eq!(field.name().slice(), "id");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "ID");
    assert!(ty.required());
  }
}
