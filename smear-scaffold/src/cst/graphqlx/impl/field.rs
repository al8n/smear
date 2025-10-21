use core::fmt::Debug;

use crate::cst::{Alias, Field, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Name> Alias<Name, GraphQLxLanguage> => Alias(Alias::new)
  where Name: Debug
}

impl_graphqlx_node! {
  for<AliasT, Name, Arguments, Directives, SelectionSet>
    Field<AliasT, Name, Arguments, Directives, SelectionSet, GraphQLxLanguage> =>
      Field(Field::new)
  where
    AliasT: Debug,
    Name: Debug,
    Arguments: Debug,
    Directives: Debug,
    SelectionSet: Debug
}
