use core::fmt::Debug;

use crate::cst::{FragmentName, FragmentSpread, InlineFragment, TypeCondition, graphql::GraphQLLanguage};

impl_graphql_node!{
  for<Name>
    FragmentName<Name, GraphQLLanguage> => FragmentName(FragmentName::new)
  where Name: Debug
}

impl_graphql_node! {
  for<Name> TypeCondition<Name, GraphQLLanguage> => TypeCondition(TypeCondition::new)
  where Name: Debug
}

impl_graphql_node! {
  for<FragmentName, Directives>
    FragmentSpread<FragmentName, Directives, GraphQLLanguage> => FragmentSpread(FragmentSpread::new)
  where
    FragmentName: Debug,
    Directives: Debug
}

impl_graphql_node! {
  for<TypeCond, Directives, SelectionSet>
    InlineFragment<TypeCond, Directives, SelectionSet, GraphQLLanguage> => InlineFragment(InlineFragment::new)
  where
    TypeCond: Debug,
    Directives: Debug,
    SelectionSet: Debug
}
