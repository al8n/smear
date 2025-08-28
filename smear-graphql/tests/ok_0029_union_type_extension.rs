// union SearchResult = Photo | Person

// union MultiLine =
//   | Photo
//   | Person

use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const INPUT_INLINED: &str = r###"
extend union SearchResult @deprecated = Photo | Person
"###;

const INPUT_MULTILINE: &str = r###"
extend union MultiLine @deprecated =
  | Photo
  | Person
"###;

#[test]
fn union_type_extension_inlined() {
  let definition = UnionTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(INPUT_INLINED)
  .unwrap();

  assert_eq!(definition.name().span().source(), &"SearchResult");

  {
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
    let directives = definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let mut directives = directives.iter();
    {
      let directive = directives.next().unwrap();
      assert_eq!(directive.name().span().source(), &"deprecated");
    }
  }
}

#[test]
fn union_type_extension_multiline() {
  let definition = UnionTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(INPUT_MULTILINE)
  .unwrap();

  assert_eq!(definition.name().span().source(), &"MultiLine");

  {
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

  let directives = definition.directives().unwrap().directives();
  assert_eq!(directives.len(), 1);
  let mut directives = directives.iter();
  {
    let directive = directives.next().unwrap();
    assert_eq!(directive.name().span().source(), &"deprecated");
  }
}
