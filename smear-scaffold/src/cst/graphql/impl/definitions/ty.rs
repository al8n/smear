use core::fmt::Debug;

use crate::cst::{ListType, NamedType, graphql::GraphQLLanguage};

impl_graphql_node! {
  for<Name> NamedType<Name, GraphQLLanguage> => NamedType(NamedType::new)
  where Name: Debug
}

impl_graphql_node! {
  for<Type> ListType<Type, GraphQLLanguage> => ListType(ListType::new)
  where Type: Debug
}
