use core::fmt::Debug;

use crate::cst::{FragmentSpread, InlineFragment, TypeCondition, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Name> TypeCondition<Name, GraphQLxLanguage> => TypeCondition(TypeCondition::new)
  where Name: Debug
}

impl_graphqlx_node! {
  for<FragmentName, Directives>
    FragmentSpread<FragmentName, Directives, GraphQLxLanguage> =>
      FragmentSpread(FragmentSpread::new)
  where
    FragmentName: Debug,
    Directives: Debug
}

impl_graphqlx_node! {
  for<TypeCond, Directives, SelectionSet>
    InlineFragment<TypeCond, Directives, SelectionSet, GraphQLxLanguage> =>
      InlineFragment(InlineFragment::new)
  where
    TypeCond: Debug,
    Directives: Debug,
    SelectionSet: Debug
}
