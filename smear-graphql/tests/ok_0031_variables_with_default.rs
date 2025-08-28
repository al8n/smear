use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
query getOutput($input: Int = 5 $config: String = "Config") {
    animal
}"###;

#[test]
fn variables_with_default() {
  let values = OperationDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().span().source(), &"getOutput");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 2);
  let mut variable_definitions = variable_definitions.iter();

  {
    let variable_definition = variable_definitions.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().span().source(),
      &"input"
    );

    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");

    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(default_value.value().unwrap_int_ref().span().source(), &"5");
  }

  {
    let variable_definition = variable_definitions.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().span().source(),
      &"config"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"String");
    let default_value = variable_definition.default_value().unwrap();
    assert_eq!(
      default_value
        .value()
        .unwrap_string_ref()
        .data()
        .span()
        .source(),
      &"Config"
    );
  }
}
