use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0014_input_definition.graphql");

#[test]
fn object_type_extension() {
  let extension = ObjectTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(extension.name().span().source(), &"Person");

  let impls = extension.implements().unwrap();
  assert_eq!(
    impls.leading_implement_interface().name().span().source(),
    &"Human"
  );

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().span().source(), &"name");

    let ty = name.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"String");
    assert!(ty.bang().is_none());
  }

  {
    let age = fields.next().unwrap();
    assert_eq!(age.name().span().source(), &"age");
    let ty = age.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_none());
  }

  {
    let picture = fields.next().unwrap();
    assert_eq!(picture.name().span().source(), &"picture");
    let ty = picture.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"Url");
    assert!(ty.bang().is_none());
  }
}
