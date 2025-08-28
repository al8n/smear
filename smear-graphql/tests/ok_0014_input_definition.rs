use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
input ExampleInputObject {
    a: String
    b: Int!
}
"###;

#[test]
fn input_object_definition() {
  let definition = InputObjectTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"ExampleInputObject");

  let input_fields = definition.fields_definition().cloned().unwrap();
  assert_eq!(input_fields.input_value_definitions().len(), 2);
  let mut fields = input_fields.input_value_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().span().source(), &"a");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"String");
    assert!(ty.bang().is_none());
  }

  {
    let b = fields.next().unwrap();
    assert_eq!(b.name().span().source(), &"b");
    let ty = b.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_some());
  }
}
