use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0027_union_type_definition.graphql");

#[test]
fn union_type_definition() {
  let doc = TypeSystemDocument::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  let definitions = doc.content();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter();
  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();

    assert_eq!(definition.name().span().source(), &"SearchResult");

    let members = definition.member_types().cloned().unwrap();
    let leading_member_type = members.leading_member_type();
    assert_eq!(leading_member_type.name().span().source(), &"Photo");
    let remaining_member_types = members.remaining_member_types();
    assert_eq!(remaining_member_types.len(), 1);
    let mut remaining_member_types = remaining_member_types.iter();
    {
      let member_type = remaining_member_types.next().unwrap();
      assert_eq!(member_type.name().span().source(), &"Person");
    }
  }
  {
    let definition = definitions.next().unwrap().unwrap_definition_ref();
    let definition = definition.unwrap_type_ref().unwrap_union_ref();
    assert_eq!(definition.name().span().source(), &"MultiLine");

    let members = definition.member_types().cloned().unwrap();
    let leading_member_type = members.leading_member_type();
    assert_eq!(leading_member_type.name().span().source(), &"Photo");
    let remaining_member_types = members.remaining_member_types();
    assert_eq!(remaining_member_types.len(), 1);
    let mut remaining_member_types = remaining_member_types.iter();
    {
      let member_type = remaining_member_types.next().unwrap();
      assert_eq!(member_type.name().span().source(), &"Person");
    }
  }
}
