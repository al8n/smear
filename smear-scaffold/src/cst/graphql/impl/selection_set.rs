use core::fmt::Debug;

use crate::cst::{SelectionSet, graphql::GraphQLLanguage};

impl_graphql_node! {
  for<Selection> SelectionSet<Selection, GraphQLLanguage> => SelectionSet(SelectionSet::new)
  where Selection: Debug
}
