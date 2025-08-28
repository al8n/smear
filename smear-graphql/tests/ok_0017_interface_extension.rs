use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
extend interface ValuedEntity @skip {
    value: Int
}
"###;

#[test]
fn interface_object_extension() {
  let extension = InterfaceTypeExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(extension.name().span().source(), &"ValuedEntity");

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 1);
  let directive = directives.directives().first().unwrap();
  assert_eq!(directive.name().span().source(), &"skip");

  let fields = extension.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let value = fields.next().unwrap();
    assert_eq!(value.name().span().source(), &"value");
    let ty = value.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_none());
  }
}
