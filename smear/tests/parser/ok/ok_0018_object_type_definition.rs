

const ALL: &str = include_str!("../../fixtures/parser/ok/0018_object_type_definition.graphql");

#[test]
fn object_type_definition() {
  use smear::parser::graphql::ast::{DescribedObjectTypeDefinition, ParseStr};

  let definition = DescribedObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().source(), "Person");
  assert_eq!(
    definition.description().unwrap().source().trim_matches('"'),
    "description of type"
  );

  let impls = definition.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].source(),
    "Human"
  );

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().source(), "name");
    assert_eq!(
      name.description().unwrap().source().trim_matches('"'),
      "\n    description of field\n    "
    );
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
fn graphlx_object_type_definition() {
  use smear::parser::graphqlx::ast::{DescribedObjectTypeDefinition, ParseStr};

  let definition = DescribedObjectTypeDefinition::<&str>::parse_str(ALL)
  .unwrap();
  assert_eq!(definition.name().name().source(), "Person");
  assert_eq!(
    definition.description().unwrap().source().trim_matches('"'),
    "description of type"
  );

  let impls = definition.implements().unwrap();
  let ifs = impls.interfaces();
  assert_eq!(
    ifs[0].path().as_slice()[0].source(),
    "Human"
  );

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.target().field_definitions().len(), 3);
  let mut fields = fields.target().field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().source(), "name");
    assert_eq!(
      name.description().unwrap().source().trim_matches('"'),
      "\n    description of field\n    "
    );
    let ty = name.ty().unwrap_path_ref();

    assert_eq!(ty.path().as_slice()[0].source(), "String");
    assert!(!ty.required());
  }

  {
    let age = fields.next().unwrap();
    assert_eq!(age.name().source(), "age");
    let ty = age.ty().unwrap_path_ref();

    assert_eq!(ty.path().as_slice()[0].source(), "Int");
    assert!(!ty.required());
  }

  {
    let picture = fields.next().unwrap();
    assert_eq!(picture.name().source(), "picture");
    let ty = picture.ty().unwrap_path_ref();

    assert_eq!(ty.path().as_slice()[0].source(), "Url");
    assert!(!ty.required());
  }
}
