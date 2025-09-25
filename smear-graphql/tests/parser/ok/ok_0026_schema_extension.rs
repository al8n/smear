use smear_graphql::parser::ast::{raw::SchemaExtension, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0026_schema_extension.graphql");

#[test]
fn schema_extension() {
  let extension = SchemaExtension::<&str>::parse_str(ALL).unwrap();

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 2);
  let mut directives = directives.directives().iter();

  {
    let skip = directives.next().unwrap();
    assert_eq!(skip.name().slice(), "skip");
    assert!(skip.arguments().is_none());
  }

  {
    let example = directives.next().unwrap();
    assert_eq!(example.name().slice(), "example");
    assert!(example.arguments().is_none());
  }

  let operation_types = extension.root_operation_types_definition().unwrap();
  let operation_types = operation_types.root_operation_type_definitions();
  assert_eq!(operation_types.len(), 1);
  let operation_type = operation_types.first().unwrap();
  assert_eq!(operation_type.operation_type().as_str(), "query");
  assert_eq!(
    operation_type.name().slice(),
    "MyExtendedQueryType"
  );
}
