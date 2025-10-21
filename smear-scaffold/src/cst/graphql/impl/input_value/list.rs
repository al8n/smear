use core::fmt::Debug;

use crate::cst::{List, graphql::GraphQLLanguage};

impl_graphql_node! {
  for<Value> List<Value, GraphQLLanguage> => ListValue(List::new)
  where Value: Debug
}
