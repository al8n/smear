
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0016_interface_definition.graphql");

#[cfg(feature = "graphql")]
#[test]
fn interface_object_definition() {
  use smear::parser::graphql::ast::{DescribedInterfaceTypeDefinition, ParseStr};

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

#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_interface_object_definition() {
  use smear::parser::graphqlx::ast::{DescribedInterfaceTypeDefinition, ParseStr};

  let definition = DescribedInterfaceTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name(), "ValuedEntity");

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().source(), "value");
    let ty = a.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "Int");
    assert!(!ty.required());
  }
}
