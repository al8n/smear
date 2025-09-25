use smear_graphql::parser::ast::{raw::TypeSystemDocument, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0027_union_type_definition.graphql");

#[test]
fn union_type_definition() {
  let doc = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();

  let definitions = doc.definitions();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.name().slice(), "SearchResult");

    let members = definition.member_types().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].slice(), "Photo");
    assert_eq!(members[1].slice(), "Person");
  }

  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();
    assert_eq!(definition.name().slice(), "MultiLine");

    let members = definition.member_types().cloned().unwrap();
    let members = members.members();
    assert_eq!(members.len(), 2);
    assert_eq!(members[0].slice(), "Photo");
    assert_eq!(members[1].slice(), "Person");
  }
}
