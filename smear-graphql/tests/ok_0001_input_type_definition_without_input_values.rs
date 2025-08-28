use chumsky::{error::Simple, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
"An input with no input values"
input AnInputWithoutInputValues

extend input AnInputWithoutInputValues {
  limit: Int!
}
"###;

#[test]
fn input_object_type_definition_without_input_values() {
  let definition =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Simple<'_, char>>>(ALL)
      .unwrap();

  let content = definition.content();
  assert_eq!(content.len(), 2);

  let mut iter = content.iter();

  {
    let input = iter
      .next()
      .unwrap()
      .unwrap_definition_ref()
      .unwrap_type_ref()
      .unwrap_input_object_ref();

    assert_eq!(input.name().span().source(), &"AnInputWithoutInputValues");
    assert_eq!(
      input.description().unwrap().content().span().source(),
      &"An input with no input values"
    );
    assert!(input.fields_definition().is_none());
  }

  {
    let input = iter
      .next()
      .unwrap()
      .unwrap_extension_ref()
      .unwrap_type_ref()
      .unwrap_input_object_ref();
    assert_eq!(input.name().span().source(), &"AnInputWithoutInputValues");
    let input_values = input.fields_definition().unwrap();
    let input_value_definitions = input_values.input_value_definitions();
    assert_eq!(input_value_definitions.len(), 1);
    let input_value = &input_value_definitions[0];
    assert_eq!(input_value.name().span().source(), &"limit");
    let ty = input_value.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_some());
  }
}
