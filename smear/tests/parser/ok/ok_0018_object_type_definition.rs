use smear::parser::graphql::ast::{DescribedObjectTypeDefinition, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0018_object_type_definition.graphql");

#[test]
fn object_type_definition() {
  let definition = DescribedObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().slice(), "Person");
  assert_eq!(
    definition.description().unwrap().source().trim_matches('"'),
    "description of type"
  );

  let impls = definition.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].slice(),
    "Human"
  );

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().slice(), "name");
    assert_eq!(
      name.description().unwrap().source().trim_matches('"'),
      "\n    description of field\n    "
    );
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
