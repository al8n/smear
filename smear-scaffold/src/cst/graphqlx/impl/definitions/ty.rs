use core::fmt::Debug;

use crate::cst::{AngleType, ListType, MapType, NamedType, SetType, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Name> NamedType<Name, GraphQLxLanguage> => NamedType(NamedType::new)
  where Name: Debug
}

impl_graphqlx_node! {
  for<Type> ListType<Type, GraphQLxLanguage> => ListType(ListType::new)
  where Type: Debug
}

impl_graphqlx_node! {
  for<Type> SetType<Type, GraphQLxLanguage> => SetType(SetType::new)
  where Type: Debug
}

impl_graphqlx_node! {
  for<KeyType, ValueType> MapType<KeyType, ValueType, GraphQLxLanguage> => MapType(MapType::new)
  where
    KeyType: Debug,
    ValueType: Debug
}

// impl_graphqlx_node! {
//   for<Type> AngleType<Type, GraphQLxLanguage> => AngleType(AngleType::new)
//   where Type: Debug
// }
