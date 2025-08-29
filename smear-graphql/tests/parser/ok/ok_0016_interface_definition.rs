use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = include_str!("../../fixtures/parser/ok/0014_input_definition.graphql");

#[test]
fn interface_object_definition() {
  let definition = InterfaceTypeDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Simple<'_, char>>,
  >(ALL)
  .unwrap();
  assert_eq!(definition.name().span().source(), &"ValuedEntity");

  let fields = definition.fields_definition().unwrap();
  assert_eq!(fields.field_definitions().len(), 1);
  let mut fields = fields.field_definitions().iter();

  {
    let a = fields.next().unwrap();
    assert_eq!(a.name().span().source(), &"value");
    let ty = a.ty().unwrap_name_ref();

    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_none());
  }
}
