use smear_graphql::parser::fast::{InterfaceTypeExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0014_input_definition.graphql");

#[test]
fn interface_object_extension() {
  let extension = InterfaceTypeExtension::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(extension.name().slice(), "ValuedEntity");

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().slice(), "skip");

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let value = fields.next().unwrap();
    assert_eq!(value.name().slice(), "value");
    let ty = value.ty().unwrap_name_ref();

    assert_eq!(ty.name().slice(), "Int");
    assert!(!ty.required());
  }
}
