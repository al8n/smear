use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
query getOutput($input: Int @deprecated $config: String = "Config" @tag(name: "team-customers")) {
    animal
}
"###;

#[test]
fn variable_with_directives() {
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

  let mut iter = variable_definitions.iter();

  {
    let variable_definition = iter.next().unwrap();
    assert_eq!(
      variable_definition.variable().name().span().source(),
      &"input"
    );
    let ty = variable_definition.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");
    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().span().source(), &"deprecated");
  }
  {
    let variable_definition = iter.next().unwrap();
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
        .content()
        .span()
        .source(),
      &"Config"
    );

    let directives = variable_definition.directives().unwrap().directives();
    assert_eq!(directives.len(), 1);
    let directive = &directives[0];
    assert_eq!(directive.name().span().source(), &"tag");
    let arguments = directive.arguments().unwrap().arguments();
    assert_eq!(arguments.len(), 1);
    let argument = &arguments[0];
    assert_eq!(argument.name().span().source(), &"name");
    let value = argument.value().unwrap_string_ref();
    assert_eq!(value.content().span().source(), &"team-customers");
  }
}
