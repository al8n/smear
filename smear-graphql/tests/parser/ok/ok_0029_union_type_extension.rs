use smear_graphql::parser::fast::{TypeSystemDocument, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0029_union_type_extension.graphql");

#[test]
fn union_type_extension() {
  let definition = TypeSystemDocument::<&str>::parse_str(ALL).unwrap();
  let definitions = definition.definitions();
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
    let directives = definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let mut directives = directives.iter();
    {
      let directive = directives.next().unwrap();
      assert_eq!(directive.name().slice(), "deprecated");
    }
  }
}
