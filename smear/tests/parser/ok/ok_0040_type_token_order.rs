use smear::parser::graphql::ast::{ObjectTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0040_type_token_order.graphql");

#[test]
fn type_token_order() {
  let definition = ObjectTypeDefinition::<&str>::parse_str(ALL).unwrap();

  assert_eq!(definition.name().source(), "Object");

  let mut fields = definition
    .fields_definition()
    .unwrap()
    .field_definitions()
    .iter();

  {
    let field = fields.next().unwrap();
    assert_eq!(field.name().source(), "field");
    let ty = field.ty().unwrap_list_ref();
    let inner_ty = ty.ty().unwrap_name_ref();
    assert_eq!(inner_ty.name().source(), "Int");
    assert!(inner_ty.required());
  }

  {
    let other = fields.next().unwrap();
    assert_eq!(other.name().source(), "_other");
    let ty = other.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "String");
    assert!(!ty.required());
  }

  {
    let real_field = fields.next().unwrap();
    assert_eq!(real_field.name().source(), "realField");
    let ty = real_field.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "ID");
    assert!(ty.required());
  }
}
