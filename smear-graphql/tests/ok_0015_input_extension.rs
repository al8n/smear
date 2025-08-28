use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
extend input ExampleInputObject @skip {
    a: String
}
"###;

#[test]
fn input_object_extension() {
  let extension = InputObjectTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(extension.name().span().source(), &"ExampleInputObject");

  let input_fields = extension.fields_definition().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 1);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().span().source(), &"a");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"String");
    assert!(ty.bang().is_none());
  }
}
