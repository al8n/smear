use smear_graphql::parser::ast::{raw::ObjectTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0040_type_token_order.graphql");

#[test]
fn type_token_order() {
  let definition = ObjectTypeDefinition::<&str>::parse_str(ALL).unwrap();

  assert_eq!(definition.name().slice(), "Object");

  let mut fields = definition
    .fields_definition()
    .unwrap()
    .field_definitions()
    .iter();

  {
    let field = fields.next().unwrap();
    assert_eq!(field.name().slice(), "field");
    let ty = field.ty().unwrap_list_ref();
    let inner_ty = ty.ty().unwrap_name_ref();
    assert_eq!(inner_ty.name().slice(), "Int");
    assert!(inner_ty.required());
  }

  {
    let other = fields.next().unwrap();
    assert_eq!(other.name().slice(), "_other");
    let ty = other.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "String");
    assert!(!ty.required());
  }

  {
    let real_field = fields.next().unwrap();
    assert_eq!(real_field.name().slice(), "realField");
    let ty = real_field.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "ID");
    assert!(ty.required());
  }
}
