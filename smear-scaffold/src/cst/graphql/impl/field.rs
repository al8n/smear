use core::fmt::Debug;

use crate::cst::{Alias, Field, graphql::GraphQLLanguage};

impl_graphql_node! {
  for<Name> Alias<Name, GraphQLLanguage> => Alias(Alias::new)
  where Name: Debug
}

impl_graphql_node! {
  for<AliasT, Name, Arguments, Directives, SelectionSet>
    Field<AliasT, Name, Arguments, Directives, SelectionSet, GraphQLLanguage> => Field(Field::new)
  where
    AliasT: Debug,
    Name: Debug,
    Arguments: Debug,
    Directives: Debug,
    SelectionSet: Debug
}
