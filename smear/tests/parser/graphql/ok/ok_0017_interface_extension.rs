
const ALL: &str = include_str!("../../../fixtures/parser/ok/0017_interface_extension.graphql");

#[test]
fn interface_object_extension() {
  use smear::parser::graphql::ast::{InterfaceTypeExtension, ParseStr};

  let extension = InterfaceTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.name().source(), "ValuedEntity");

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().source(), "skip");

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let value = fields.next().unwrap();
    assert_eq!(value.name().source(), "value");
    let ty = value.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "Int");
    assert!(!ty.required());
  }
}

#[test]
fn graphqlx_interface_object_extension() {
  use smear::parser::graphqlx::ast::{InterfaceTypeExtension, ParseStr};

  let extension = InterfaceTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.path(), "ValuedEntity");

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name(), "skip");

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let value = fields.next().unwrap();
    assert_eq!(value.name().source(), "value");
    let ty = value.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "Int");
    assert!(!ty.required());
  }
}

