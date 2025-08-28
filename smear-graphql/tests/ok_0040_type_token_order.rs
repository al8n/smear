use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
type Object {
  #(apparently you can stick a comma in there?? please dont do it!)
  field : [Int ,!] # comment
  _other:
String,,,   ,
#garbage
  realField: ID!
}
"###;

#[test]
fn type_token_order() {
  let definition = ObjectTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  assert_eq!(definition.name().span().source(), &"Object");

  let mut fields = definition
    .fields_definition()
    .unwrap()
    .field_definitions()
    .iter();

  {
    let field = fields.next().unwrap();
    assert_eq!(field.name().span().source(), &"field");
    let ty = field.ty().unwrap_list_ref();
    let inner_ty = ty.ty().unwrap_name_ref();
    assert_eq!(inner_ty.name().span().source(), &"Int");
    assert!(inner_ty.bang().is_some());
  }

  {
    let other = fields.next().unwrap();
    assert_eq!(other.name().span().source(), &"_other");
    let ty = other.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"String");
    assert!(ty.bang().is_none());
  }

  {
    let real_field = fields.next().unwrap();
    assert_eq!(real_field.name().span().source(), &"realField");
    let ty = real_field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"ID");
    assert!(ty.bang().is_some());
  }
}
