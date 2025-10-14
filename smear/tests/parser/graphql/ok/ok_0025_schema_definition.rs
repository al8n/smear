
const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0025_schema_definition.graphql");

#[test]
fn schema_definition() {
  use smear::parser::graphql::ast::{SchemaDefinition, ParseStr};

  let definition = SchemaDefinition::<&str>::parse_str(ALL).unwrap();

  let operation_types = definition
    .root_operation_types_definition()
    .root_operation_type_definitions();
  assert_eq!(operation_types.len(), 3);

  let mut operation_types = operation_types.iter();

  {
    let query = operation_types.next().unwrap();
    assert_eq!(query.operation_type().as_str(), "query");
    assert_eq!(query.name().source(), "MyQueryRootType");
  }

  {
    let mutation = operation_types.next().unwrap();
    assert_eq!(mutation.operation_type().as_str(), "mutation");
    assert_eq!(mutation.name().source(), "MyMutationRootType");
  }

  {
    let subscription = operation_types.next().unwrap();
    assert_eq!(
      subscription.operation_type().as_str(),
      "subscription"
    );
    assert_eq!(
      subscription.name().source(),
      "MySubscriptionRootType"
    );
  }
}

#[test]
fn graphqlx_schema_definition() {
  use smear::parser::graphqlx::ast::{SchemaDefinition, ParseStr};

  let definition = SchemaDefinition::<&str>::parse_str(ALL).unwrap();

  let operation_types = definition
    .root_operation_types_definition()
    .root_operation_type_definitions();
  assert_eq!(operation_types.len(), 3);

  let mut operation_types = operation_types.iter();

  {
    let query = operation_types.next().unwrap();
    assert_eq!(query.operation_type().as_str(), "query");
    assert_eq!(query.path(), "MyQueryRootType");
  }

  {
    let mutation = operation_types.next().unwrap();
    assert_eq!(mutation.operation_type().as_str(), "mutation");
    assert_eq!(mutation.path(), "MyMutationRootType");
  }

  {
    let subscription = operation_types.next().unwrap();
    assert_eq!(
      subscription.operation_type().as_str(),
      "subscription"
    );
    assert_eq!(
      subscription.path(),
      "MySubscriptionRootType"
    );
  }
}
