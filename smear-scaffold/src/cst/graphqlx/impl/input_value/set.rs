use core::fmt::Debug;

use crate::cst::{Set, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Value> Set<Value, GraphQLxLanguage> => SetValue(Set::new)
  where Value: Debug
}
