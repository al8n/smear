use core::fmt::Debug;

use crate::cst::{SelectionSet, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Selection> SelectionSet<Selection, GraphQLxLanguage> => SelectionSet(SelectionSet::new)
  where Selection: Debug
}
