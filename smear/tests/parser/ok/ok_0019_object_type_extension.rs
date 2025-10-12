
const ALL: &str = include_str!("../../fixtures/parser/ok/0019_object_type_extension.graphql");

#[test]
fn object_type_extension() {
  use smear::parser::graphql::ast::{ObjectTypeExtension, ParseStr};

  let extension = ObjectTypeExtension::<&str>::parse_str(ALL).unwrap();
  assert_eq!(extension.name().source(), "Person");

  let impls = extension.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].source(),
    "Human"
  );

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().source(), "name");

    let ty = name.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "String");
    assert!(!ty.required());
  }

  {
    let age = fields.next().unwrap();
    assert_eq!(age.name().source(), "age");
    let ty = age.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "Int");
    assert!(!ty.required());
  }

  {
    let picture = fields.next().unwrap();
    assert_eq!(picture.name().source(), "picture");
    let ty = picture.ty().unwrap_name_ref();

    assert_eq!(ty.name().source(), "Url");
    assert!(!ty.required());
  }
}

#[test]
fn graphqlx_object_type_extension() {
  use smear::parser::graphqlx::ast::{ObjectTypeExtension, ParseStr};

  let extension = ObjectTypeExtension::<&str>::parse_str(ALL).unwrap();
  assert_eq!(extension.path(), "Person");

  let impls = extension.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].path(),
    "Human"
  );

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().source(), "name");

    let ty = name.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "String");
    assert!(!ty.required());
  }

  {
    let age = fields.next().unwrap();
    assert_eq!(age.name().source(), "age");
    let ty = age.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "Int");
    assert!(!ty.required());
  }

  {
    let picture = fields.next().unwrap();
    assert_eq!(picture.name().source(), "picture");
    let ty = picture.ty().unwrap_path_ref();

    assert_eq!(ty.path(), "Url");
    assert!(!ty.required());
  }
}
