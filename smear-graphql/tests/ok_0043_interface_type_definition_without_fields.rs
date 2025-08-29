use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
"An interface with no fields"
interface AnInterfaceWithoutFields

extend interface AnInterfaceWithoutFields {
  id: ID!
}

"###;

#[test]
fn interface_type_definition_without_fields() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<'_, char>>>(ALL)
      .unwrap();

  let mut types = document.content().iter();

  {
    let interface = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();

    assert_eq!(
      interface.name().span().source(),
      &"AnInterfaceWithoutFields"
    );
    assert_eq!(
      interface.description().unwrap().content().span().source(),
      &"An interface with no fields"
    );
    assert!(interface.fields_definition().is_none());
  }

  {
    let interface = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_interface_ref();
    assert_eq!(
      interface.name().span().source(),
      &"AnInterfaceWithoutFields"
    );
    let fields = interface.fields_definition().unwrap();
    let field_definitions = fields.field_definitions();
    assert_eq!(field_definitions.len(), 1);
    let field = &field_definitions[0];
    assert_eq!(field.name().span().source(), &"id");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"ID");
    assert!(ty.bang().is_some());
  }
}
