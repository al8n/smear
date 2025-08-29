use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
union SearchResult = Photo | Person

type Error {
    code: Int
}
"###;

#[test]
fn union_type_definition_followed_by_object_definition() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = document.content();
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
    assert_eq!(union.name().span().source(), &"SearchResult");
    let member_types = union.member_types().unwrap();
    let leading_member_type = member_types.leading_member_type();
    assert_eq!(leading_member_type.name().span().source(), &"Photo");

    let remaining_member_types = member_types.remaining_member_types();
    assert_eq!(remaining_member_types.len(), 1);
    let member_type = &remaining_member_types[0];
    assert_eq!(member_type.name().span().source(), &"Person");
  }

  {
    let object = iter
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name().span().source(), &"Error");
    let fields = object.fields_definition().unwrap().field_definitions();
    assert_eq!(fields.len(), 1);
    let field = &fields[0];
    assert_eq!(field.name().span().source(), &"code");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");
    assert_eq!(ty.bang(), None);
  }
}
