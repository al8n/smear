use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
"description of type"
type Person implements Human {
    """
    description of field
    """
    name: String
    age: Int
    picture: Url
}
"###;

#[test]
fn object_type_definition() {
  let definition = ObjectTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"Person");
  assert_eq!(
    definition.description().unwrap().data().span().source(),
    &"description of type"
  );

  let impls = definition.implements().unwrap();
  assert_eq!(
    impls.leading_implement_interface().name().span().source(),
    &"Human"
  );

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 3);
  let mut fields = fields.field_definitions().iter();

  {
    let name = fields.next().unwrap();
    assert_eq!(name.name().span().source(), &"name");
    assert_eq!(
      name.description().unwrap().data().span().source(),
      &"\n    description of field\n    "
    );
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
