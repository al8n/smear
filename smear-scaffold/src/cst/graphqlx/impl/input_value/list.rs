use core::fmt::Debug;

use crate::cst::{List, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Value> List<Value, GraphQLxLanguage> => ListValue(List::new)
  where Value: Debug
}
