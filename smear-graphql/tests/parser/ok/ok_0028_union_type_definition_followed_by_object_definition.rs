use smear_graphql::parser::ast::{raw::Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0028_union_type_definition_followed_by_object_definition.graphql");

#[test]
fn union_type_definition_followed_by_object_definition() {
  let document = Document::<&str>::parse_str(ALL).unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut iter = definitions.iter();

  {
    let union = iter
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_union_ref();
    assert_eq!(union.name().slice(), "SearchResult");
    let member_types = union.member_types().unwrap();
    let members = member_types.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].slice(), "Photo");
    assert_eq!(members[1].slice(), "Person");
  }

  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name().slice(), "Error");
    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    let field = &fields[0];
    assert_eq!(field.name().slice(), "code");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().slice(), "Int");
    assert!(!ty.required());
  }
}
