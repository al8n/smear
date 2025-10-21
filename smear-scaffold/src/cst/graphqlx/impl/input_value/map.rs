use core::fmt::Debug;

use crate::cst::{Map, MapEntry, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Key, Value> MapEntry<Key, Value, GraphQLxLanguage> => MapEntry(MapEntry::new)
  where
    Key: Debug,
    Value: Debug
}

impl_graphqlx_node! {
  for<Key, Value> Map<Key, Value, GraphQLxLanguage> => MapValue(Map::new)
  where
    Key: Debug,
    Value: Debug
}
