use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
schema {
  query: MyQueryRootType
  mutation: MyMutationRootType,
  subscription: MySubscriptionRootType
}
"###;

#[test]
fn schema_definition() {
  let definition = SchemaDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  let operation_types = definition
    .root_operation_types_definition()
    .root_operation_type_definitions();
  assert_eq!(operation_types.len(), 3);

  let mut operation_types = operation_types.iter();

  {
    let query = operation_types.next().unwrap();
    assert_eq!(query.operation_type().span().source(), &"query");
    assert_eq!(query.name().span().source(), &"MyQueryRootType");
  }

  {
    let mutation = operation_types.next().unwrap();
    assert_eq!(mutation.operation_type().span().source(), &"mutation");
    assert_eq!(mutation.name().span().source(), &"MyMutationRootType");
  }

  {
    let subscription = operation_types.next().unwrap();
    assert_eq!(
      subscription.operation_type().span().source(),
      &"subscription"
    );
    assert_eq!(
      subscription.name().span().source(),
      &"MySubscriptionRootType"
    );
  }
}
