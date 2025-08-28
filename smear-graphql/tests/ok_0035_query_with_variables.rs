use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
query Foo($bar: Int) {
    name
}
"###;

#[test]
fn query_with_variables() {
  let values = OperationDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap()
  .unwrap_named();

  assert_eq!(values.name().unwrap().span().source(), &"Foo");
  let variable_definitions = values
    .variable_definitions()
    .unwrap()
    .variable_definitions();
  assert_eq!(variable_definitions.len(), 1);
  let variable_definition = &variable_definitions[0];
  assert_eq!(
    variable_definition.variable().name().span().source(),
    &"bar"
  );
  let ty = variable_definition.ty().unwrap_name_ref();
  assert_eq!(ty.name().span().source(), &"Int");
}
