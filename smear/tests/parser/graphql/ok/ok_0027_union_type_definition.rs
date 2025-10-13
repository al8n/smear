
const ALL: &str = include_str!("../../../fixtures/parser/ok/0027_union_type_definition.graphql");

#[test]
fn union_type_definition() {
  use smear::parser::graphql::ast::{TypeSystemDocument, ParseStr};

  let doc = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();

  let definitions = doc.definitions();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.name().source(), "SearchResult");

    let members = definition.member_types().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].source(), "Photo");
    assert_eq!(members[1].source(), "Person");
  }

  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();
    assert_eq!(definition.name().source(), "MultiLine");

    let members = definition.member_types().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].source(), "Photo");
    assert_eq!(members[1].source(), "Person");
  }
}

#[test]
fn graphqlx_union_type_definition() {
  use smear::parser::graphqlx::ast::{TypeSystemDocument, ParseStr};

  let doc = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();

  let definitions = doc.definitions();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.name().source(), "SearchResult");

    let members = definition.members().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].path(), "Photo");
    assert_eq!(members[1].path(), "Person");
  }

  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();
    assert_eq!(definition.name().source(), "MultiLine");

    let members = definition.members().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].path(), "Photo");
    assert_eq!(members[1].path(), "Person");
  }
}
