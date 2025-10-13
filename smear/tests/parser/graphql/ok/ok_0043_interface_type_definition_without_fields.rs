const ALL: &str = include_str!("../../../fixtures/parser/ok/0043_interface_type_definition_without_fields.graphql");

#[test]
fn interface_type_definition_without_fields() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    
    let interface = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(
      interface.name().source(),
      "AnInterfaceWithoutFields"
    );
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "An interface with no fields"
    );
    assert!(interface.fields_definition().is_none());
  }

  {
    let interface = types
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(
      interface.name().source(),
      "AnInterfaceWithoutFields"
    );
    let fields = interface.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 1);
    let field = &field_definitions[0];
    assert_eq!(field.name().source(), "id");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "ID");
    assert!(ty.required());
  }
}

#[test]
fn graphqlx_interface_type_definition_without_fields() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    
    let interface = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(
      interface.name().source(),
      "AnInterfaceWithoutFields"
    );
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "An interface with no fields"
    );
    assert!(interface.fields_definition().is_none());
  }

  {
    let interface = types
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(
      interface.path(),
      "AnInterfaceWithoutFields"
    );
    let fields = interface.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 1);
    let field = &field_definitions[0];
    assert_eq!(field.name().source(), "id");
    let ty = field.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "ID");
    assert!(ty.required());
  }
}
