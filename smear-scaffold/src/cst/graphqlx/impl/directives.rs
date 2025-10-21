use core::fmt::Debug;

use crate::cst::{Directive, Directives, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Name, Arguments> Directive<Name, Arguments, GraphQLxLanguage> => Directive(Directive::new)
  where
    Name: Debug,
    Arguments: Debug
}

impl_graphqlx_node! {
  for<Dir> Directives<Dir, GraphQLxLanguage> => Directives(Directives::new)
  where Dir: Debug
}
