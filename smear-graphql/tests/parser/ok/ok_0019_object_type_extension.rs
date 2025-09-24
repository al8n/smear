use smear_graphql::parser::fast::{ObjectTypeExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0014_input_definition.graphql");

#[test]
fn object_type_extension() {
  let extension = ObjectTypeExtension::<&str>::parse_str(ALL).unwrap();
  assert_eq!(extension.name().slice(), "Person");

  let impls = extension.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].slice(),
    "Human"
  );

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().slice(), "name");

    let ty = name.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "String");
    assert!(!ty.required());
  }

  {
    let age = fields.next().unwrap();
    assert_eq!(age.name().slice(), "age");
    let ty = age.ty().unwrap_name_ref();

    assert_eq!(ty.name().slice(), "Int");
    assert!(!ty.required());
  }

  {
    let picture = fields.next().unwrap();
    assert_eq!(picture.name().slice(), "picture");
    let ty = picture.ty().unwrap_name_ref();

    assert_eq!(ty.name().slice(), "Url");
    assert!(!ty.required());
  }
}
