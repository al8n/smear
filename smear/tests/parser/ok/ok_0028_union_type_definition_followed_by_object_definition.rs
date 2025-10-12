

const ALL: &str = include_str!("../../fixtures/parser/ok/0028_union_type_definition_followed_by_object_definition.graphql");

#[test]
fn union_type_definition_followed_by_object_definition() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  {
    let union = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_union_ref();
    assert_eq!(union.name().source(), "SearchResult");
    let member_types = union.member_types().unwrap();
    let members = member_types.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].source(), "Photo");
    assert_eq!(members[1].source(), "Person");
  }

  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name().source(), "Error");
    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    let field = &fields[0];
    assert_eq!(field.name().source(), "code");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");
    assert!(!ty.required());
  }
}

#[test]
fn graphqlx_union_type_definition_followed_by_object_definition() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document = Document::<&str>::parse_str(ALL).unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  {
    let union = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_union_ref();
    assert_eq!(union.name().source(), "SearchResult");
    let member_types = union.members().unwrap();
    let members = member_types.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].path(), "Photo");
    assert_eq!(members[1].path(), "Person");
  }

  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name(), "Error");
    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    let field = &fields[0];
    assert_eq!(field.name(), "code");
    let ty = field.ty().unwrap_path_ref();
    assert_eq!(ty.path(), "Int");
    assert!(!ty.required());
  }
}
