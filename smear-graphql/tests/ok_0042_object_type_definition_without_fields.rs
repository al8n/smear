use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
"A type with no fields"
type AnObjectTypeWithoutFields

extend type AnObjectTypeWithoutFields {
  id: ID!
}
"###;

#[test]
fn object_type_definition_without_fields() {
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

    assert_eq!(object.name().span().source(), &"AnObjectTypeWithoutFields");
    assert_eq!(
      object.description().unwrap().content().span().source(),
      &"A type with no fields"
    );
    assert!(object.fields_definition().is_none());
  }

  {
    let object = types
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();
    assert_eq!(object.name().span().source(), &"AnObjectTypeWithoutFields");
    let fields = object.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 1);
    let field = &field_definitions[0];
    assert_eq!(field.name().span().source(), &"id");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"ID");
    assert!(ty.bang().is_some());
  }
}
