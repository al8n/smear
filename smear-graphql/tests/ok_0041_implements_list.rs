use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
"Just one interface"
type One implements A { field: Int! }

"Several interfaces"
type Two implements A & B & C { field: Int! }

"&-prefixed"
type Three implements
  & A
  & B
  & C
{ field: Int! }
"###;

#[test]
fn implements_list() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<'_, char>>>(ALL)
      .unwrap();

  let mut types = document.content().iter();

  {
    let object = types
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().span().source(), &"One");
    assert_eq!(
      object.description().unwrap().content().span().source(),
      &"Just one interface"
    );
    let impls = object.implements().unwrap();
    let first = impls.leading_implement_interface();
    assert_eq!(first.name().span().source(), &"A");
    assert!(first.ampersand().is_none());

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().span().source(), &"field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().span().source(), &"Int");
      assert!(ty.bang().is_some());
    }
  }

  {
    let object = types
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().span().source(), &"Two");
    assert_eq!(
      object.description().unwrap().content().span().source(),
      &"Several interfaces"
    );
    let impls = object.implements().unwrap();
    let a = impls.leading_implement_interface();
    assert_eq!(a.name().span().source(), &"A");
    assert!(a.ampersand().is_none());

    let remaining = impls.remaining_implement_interfaces();
    assert_eq!(remaining.len(), 2);
    assert_eq!(remaining[0].name().span().source(), &"B");
    assert_eq!(remaining[1].name().span().source(), &"C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().span().source(), &"field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().span().source(), &"Int");
      assert!(ty.bang().is_some());
    }
  }

  {
    let object = types
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().span().source(), &"Three");
    assert_eq!(
      object.description().unwrap().content().span().source(),
      &"&-prefixed"
    );
    let impls = object.implements().unwrap();
    let a = impls.leading_implement_interface();
    assert_eq!(a.name().span().source(), &"A");
    assert!(a.ampersand().is_some());

    let remaining = impls.remaining_implement_interfaces();
    assert_eq!(remaining.len(), 2);
    assert_eq!(remaining[0].name().span().source(), &"B");
    assert_eq!(remaining[1].name().span().source(), &"C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().span().source(), &"field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().span().source(), &"Int");
      assert!(ty.bang().is_some());
    }
  }
}
