use smear::parser::graphql::ast::{DescribedInterfaceTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0016_interface_definition.graphql");

#[test]
fn interface_object_definition() {
  let definition = DescribedInterfaceTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "ValuedEntity");

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().source(), "value");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "Int");
    assert!(!ty.required());
  }
}
