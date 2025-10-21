use core::fmt::Debug;

use crate::cst::{DefaultInputValue, Object, ObjectField, graphql::GraphQLLanguage};

impl_graphql_node! {
  for<Value> DefaultInputValue<Value, GraphQLLanguage> => DefaultValue(DefaultInputValue::new)
  where Value: Debug
}

impl_graphql_node! {
  for<Name, Value> ObjectField<Name, Value, GraphQLLanguage> => ObjectField(ObjectField::new)
  where
    Name: Debug,
    Value: Debug
}

impl_graphql_node! {
  for<Name, Value> Object<Name, Value, GraphQLLanguage> => ObjectValue(Object::new)
  where
    Name: Debug,
    Value: Debug
}

mod list;

