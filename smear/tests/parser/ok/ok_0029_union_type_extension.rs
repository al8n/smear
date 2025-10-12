const ALL: &str = include_str!("../../fixtures/parser/ok/0029_union_type_extension.graphql");

#[test]
fn union_type_extension() {
  use smear::parser::graphql::ast::{TypeSystemDocument, ParseStr};

  let definition = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();
  let definitions = definition.definitions();
  assert_eq!(definitions.len(), 1, "Source: {ALL}");
  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_extension_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.name().source(), "SearchResult");
    let members = definition.member_types().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].source(), "Photo");
    assert_eq!(members[1].source(), "Person");
  }
}

#[test]
fn graphqlx_union_type_extension() {
  use smear::parser::graphqlx::ast::{TypeSystemDocument, ParseStr};

  let definition = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();
  let definitions = definition.definitions();
  assert_eq!(definitions.len(), 1, "Source: {ALL}");
  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_extension_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.path(), "SearchResult");
    let members = definition.members().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].path(), "Photo");
    assert_eq!(members[1].path(), "Person");
  }
}
